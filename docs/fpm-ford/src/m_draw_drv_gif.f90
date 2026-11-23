! records up to $M_DRAW_GIF_MAXFRAMES (default `120`), automatically
! triggers `checkkey() /= 0` on frame 120 so your animation loop exits
! cleanly, and writes `output.gif` (or `$M_DRAW_OUTPUT`).
!
module m_draw_drv_gif
   use, intrinsic :: iso_fortran_env, only: int8, real32, output_unit, error_unit
   use m_draw_types
   use m_draw_pref
   use m_draw_drivers
   use M_pixel__writegif_animated, only: write_animated_gif
   implicit none
   private
   public :: GIF_draw_devcpy

   integer, parameter :: DEFAULT_W         = 800
   integer, parameter :: DEFAULT_H         = 600
   integer, parameter :: DEFAULT_MAXFRAMES = 120
   integer, parameter :: CMAPSIZE          = 256

   ! Active 2D framebuffer: canvas(x, y_top_down) with x in 0..W-1, y in 0..H-1
   integer, allocatable, save :: canvas(:, :)
   ! Compact 1-byte-per-pixel animation buffer: frames(max_allowed, 0:W-1, 0:H-1)
   integer(int8), allocatable, save :: frames(:, :, :)

   integer, save :: nframes            = 0
   integer, save :: max_allowed_frames = DEFAULT_MAXFRAMES
   integer, save :: img_w              = DEFAULT_W
   integer, save :: img_h              = DEFAULT_H

   integer, save :: cmap(3, 0:CMAPSIZE-1)
   integer, save :: max_col_used = 15
   integer, save :: curcol       = 7
   integer, save :: curwid       = 1
   integer, save :: curpat       = 0
   integer, save :: bg_color     = 0
   logical, save :: dirty        = .false.
   logical, save :: gif_active   = .false.
   character(len=512), save :: gif_filename = 'output.gif'

contains

   pure integer function FLIPY(y)
      integer, intent(in) :: y
      FLIPY = (img_h - 1) - y
   end function FLIPY

   ! =========================================================================
   ! INITIALIZE FRAMEBUFFER, DEFAULT PALETTE & OUTPUT FILENAME
   ! =========================================================================
   integer function GIF_init()
      integer :: prefx, prefy, prefxs, prefys, env_len, ios, alloc_stat
      character(len=512) :: env_val

      if (gif_active) then
         GIF_init = 1
         return
      end if

      gif_filename = 'output.gif'
      call get_environment_variable('M_DRAW_OUTPUT', value=env_val, length=env_len, status=ios)
      if (ios == 0 .and. env_len > 0) gif_filename = trim(env_val)

      max_allowed_frames = DEFAULT_MAXFRAMES
      call get_environment_variable('M_DRAW_GIF_MAXFRAMES', value=env_val, length=env_len, status=ios)
      if (ios == 0 .and. env_len > 0) then
         read(env_val, *, iostat=ios) max_allowed_frames
         if (ios /= 0 .or. max_allowed_frames < 1) max_allowed_frames = DEFAULT_MAXFRAMES
      end if

      if (draw_unit /= output_unit .and. draw_unit /= error_unit) then
         close(draw_unit, status='delete', iostat=ios)
         draw_unit = output_unit
      end if

      call draw_getprefposandsize(prefx, prefy, prefxs, prefys)
      if (prefxs > 0 .and. prefys > 0) then
         img_w = prefxs
         img_h = prefys
      else
         img_w = DEFAULT_W
         img_h = DEFAULT_H
      end if

      vdevice%sizeSx = img_w
      vdevice%sizeSy = img_h
      vdevice%sizeX  = min(img_w, img_h)
      vdevice%sizeY  = vdevice%sizeX
      vdevice%depth  = 8

      if (allocated(canvas)) deallocate(canvas)
      if (allocated(frames)) deallocate(frames)

      allocate(canvas(0:img_w-1, 0:img_h-1), stat=alloc_stat)
      if (alloc_stat /= 0) then
         write(error_unit, '(A)') 'm_draw_drv_gif: unable to allocate canvas'
         GIF_init = 0
         return
      end if

      allocate(frames(max_allowed_frames, 0:img_w-1, 0:img_h-1), stat=alloc_stat)
      if (alloc_stat /= 0) then
         write(error_unit, '(A, I0, A)') 'm_draw_drv_gif: unable to allocate ', &
            max_allowed_frames, ' frames; try lowering M_DRAW_GIF_MAXFRAMES'
         deallocate(canvas)
         GIF_init = 0
         return
      end if

      ! Standard VOGLE 16-color default palette
      cmap = 255
      cmap(:,  0) = [255, 255, 255] ! White
      cmap(:,  1) = [255,   0,   0] ! Red
      cmap(:,  2) = [  0, 255,   0] ! Green
      cmap(:,  3) = [255, 255,   0] ! Yellow
      cmap(:,  4) = [  0,   0, 255] ! Blue
      cmap(:,  5) = [255,   0, 255] ! Magenta
      cmap(:,  6) = [  0, 255, 255] ! Cyan
      cmap(:,  7) = [  0,   0,   0] ! Black
      cmap(:,  8) = [155,   0,   0]
      cmap(:,  9) = [  0, 155,   0]
      cmap(:, 10) = [155, 255, 255]
      cmap(:, 11) = [155, 155,   0]
      cmap(:, 12) = [  0,   0, 155]
      cmap(:, 13) = [155,   0, 155]
      cmap(:, 14) = [  0, 155, 155]
      cmap(:, 15) = [100, 100, 100]

      nframes      = 0
      max_col_used = 15
      bg_color     = 0
      curcol       = 7
      curwid       = 1
      curpat       = 0
      canvas       = bg_color
      dirty        = .false.
      gif_active   = .true.

      GIF_init = 1
   end function GIF_init

   ! =========================================================================
   ! APPEND CURRENT CANVAS FRAME TO BOUNDED ANIMATION BUFFER
   ! =========================================================================
   subroutine push_frame()
      if (.not. gif_active) return
      if (nframes >= max_allowed_frames) return

      nframes = nframes + 1
      frames(nframes, :, :) = int(canvas(:, :), kind=int8)
      dirty = .false.
   end subroutine push_frame

   ! =========================================================================
   ! WRITE ANIMATED GIF ON DEVICE EXIT
   ! =========================================================================
   integer function GIF_exit()
      character(len=64) :: delay_str
      integer :: delay_cs, env_len, ios
      integer, allocatable :: out_frames(:, :, :)

      if (.not. gif_active) then
         GIF_exit = 0
         return
      end if

      if (nframes == 0 .or. (dirty .and. nframes < max_allowed_frames)) then
         call push_frame()
      end if

      delay_cs = 4
      call get_environment_variable('M_DRAW_GIF_DELAY', value=delay_str, length=env_len, status=ios)
      if (ios == 0 .and. env_len > 0) then
         read(delay_str, *, iostat=ios) delay_cs
         if (ios /= 0 .or. delay_cs < 1) delay_cs = 4
      end if

      if (nframes > 0) then
         allocate(out_frames(nframes, 0:img_w-1, 0:img_h-1))
         out_frames = iand(int(frames(1:nframes, :, :)), 255)
         call write_animated_gif(trim(gif_filename), out_frames, &
            cmap(:, 0:max_col_used), delay=delay_cs)
         deallocate(out_frames)
      end if

      if (allocated(canvas)) deallocate(canvas)
      if (allocated(frames)) deallocate(frames)
      nframes    = 0
      gif_active = .false.
      GIF_exit   = 0
   end function GIF_exit

   ! =========================================================================
   ! SWAPBUFFERS & CHECKKEY (AUTO-STOPS LOOP ONCE MAXFRAMES IS REACHED)
   ! =========================================================================
   integer function GIF_swapb()
      call push_frame()
      GIF_swapb = 0
   end function GIF_swapb

   integer function GIF_checkkey()
      if (nframes >= max_allowed_frames) then
         GIF_checkkey = ichar('q')
      else
         GIF_checkkey = 0
      end if
   end function GIF_checkkey

   integer function GIF_backb()
      GIF_backb = 1
   end function GIF_backb

   integer function GIF_frontb()
      GIF_frontb = 0
   end function GIF_frontb

   ! =========================================================================
   ! CLEAR, COLOR, LINEWIDTH & PALETTE MAPPING
   ! =========================================================================
   integer function GIF_clear()
      bg_color = curcol
      canvas   = bg_color
      dirty    = .true.
      GIF_clear = 0
   end function GIF_clear

   integer function GIF_color(col)
      integer, intent(in) :: col
      if (col < 0) then
         curwid = abs(col)
      else
         curpat = col / CMAPSIZE
         curcol = mod(col, CMAPSIZE)
         if (curcol > max_col_used) max_col_used = curcol
      end if
      GIF_color = 0
   end function GIF_color

   integer function GIF_mapcolor(i, r, g, b)
      integer, intent(in) :: i, r, g, b
      if (i < 0 .or. i >= CMAPSIZE) then
         GIF_mapcolor = -1
         return
      end if
      cmap(1, i) = max(0, min(255, r))
      cmap(2, i) = max(0, min(255, g))
      cmap(3, i) = max(0, min(255, b))
      if (i > max_col_used) max_col_used = i
      GIF_mapcolor = 0
   end function GIF_mapcolor

   integer function GIF_setlw(width)
      integer, intent(in) :: width
      if (width >= 0) curwid = width
      GIF_setlw = 0
   end function GIF_setlw

   ! =========================================================================
   ! RASTERIZATION PRIMITIVES: BRESENHAM THICK LINES & SCANLINE POLYGON FILL
   ! =========================================================================
   subroutine plot_brush(cx, cy, rad)
      integer, intent(in) :: cx, cy, rad
      integer :: dx, dy, px, py, r2

      if (rad <= 0) then
         if (cx >= 0 .and. cx < img_w .and. cy >= 0 .and. cy < img_h) then
            canvas(cx, cy) = curcol
         end if
         return
      end if

      r2 = rad * rad
      do dy = -rad, rad
         py = cy + dy
         if (py < 0 .or. py >= img_h) cycle
         do dx = -rad, rad
            if (dx*dx + dy*dy <= r2) then
               px = cx + dx
               if (px >= 0 .and. px < img_w) canvas(px, py) = curcol
            end if
         end do
      end do
   end subroutine plot_brush

   integer function GIF_draw(x, y)
      integer, intent(in) :: x, y
      integer :: x0, y0, x1, y1, dx, dy, sx, sy, err, e2, rad

      x0 = vdevice%cpVx
      y0 = FLIPY(vdevice%cpVy)
      x1 = x
      y1 = FLIPY(y)

      rad = max(0, (vdevice%sizeX * curwid / 10000) / 2)

      dx  = abs(x1 - x0)
      dy  = abs(y1 - y0)
      sx  = merge(1, -1, x0 < x1)
      sy  = merge(1, -1, y0 < y1)
      err = dx - dy

      do
         call plot_brush(x0, y0, rad)
         if (x0 == x1 .and. y0 == y1) exit
         e2 = 2 * err
         if (e2 > -dy) then
            err = err - dy
            x0  = x0 + sx
         end if
         if (e2 < dx) then
            err = err + dx
            y0  = y0 + sy
         end if
      end do
      dirty    = .true.
      GIF_draw = 0
   end function GIF_draw
   integer function GIF_fill(n, x, y)
      integer, intent(in) :: n
      integer, intent(in) :: x(n), y(n)
      integer :: py(n), ymin, ymax, scan_y, i, j, nodes, node_x(n), tmp, k, x_start, x_end
      if (n < 3) then
         GIF_fill = 0
         return
      end if
      do i = 1, n
         py(i) = FLIPY(y(i))
      end do
      ymin = max(0, minval(py))
      ymax = min(img_h - 1, maxval(py))
      do scan_y = ymin, ymax
         nodes = 0
         j = n
         do i = 1, n
            if ((py(i) < scan_y .and. py(j) >= scan_y) .or. &
               (py(j) < scan_y .and. py(i) >= scan_y)) then
               nodes = nodes + 1
               node_x(nodes) = int(real(x(i), real32) + &
                  real(scan_y - py(i), real32) / real(py(j) - py(i), real32) * real(x(j) - x(i), real32))
            end if
            j = i
         end do
         do i = 1, nodes - 1
            do k = i + 1, nodes
               if (node_x(i) > node_x(k)) then
                  tmp = node_x(i); node_x(i) = node_x(k); node_x(k) = tmp
               end if
            end do
         end do
         do i = 1, nodes - 1, 2
            x_start = max(0, node_x(i))
            x_end   = min(img_w - 1, node_x(i + 1))
            if (x_start <= x_end) then
               canvas(x_start:x_end, scan_y) = curcol
            end if
         end do
      end do
      vdevice%cpVx = x(n)
      vdevice%cpVy = y(n)
      dirty    = .true.
      GIF_fill = 0
   end function GIF_fill
   integer function GIF_font(fontname)
      character(len=*), intent(in) :: fontname
      GIF_font = 0
   end function GIF_font
   integer function GIF_char(c)
      character(len=1), intent(in) :: c
      GIF_char = 0
   end function GIF_char
   integer function GIF_string(s)
      character(len=*), intent(in) :: s
      GIF_string = 0
   end function GIF_string
   integer function noop()
      noop = -1
   end function noop
   integer function noop_zero()
      noop_zero = 0
   end function noop_zero
   integer function noop2(x, y)
      integer, intent(out) :: x, y
      x = 0; y = 0
      noop2 = -1
   end function noop2
! =========================================================================
! REGISTER GIF DEVICE DRIVER ENTRY POINTS
! =========================================================================
   integer function GIF_draw_devcpy()
      vdevice%dev%devname   = "gif"
      vdevice%dev%large     = "large"
      vdevice%dev%small     = "small"
      vdevice%dev%Vbackb    => GIF_backb
      vdevice%dev%Vchar     => GIF_char
      vdevice%dev%Vcheckkey => GIF_checkkey
      vdevice%dev%Vclear    => GIF_clear
      vdevice%dev%Vcolor    => GIF_color
      vdevice%dev%Vdraw     => GIF_draw
      vdevice%dev%Vexit     => GIF_exit
      vdevice%dev%Vfill     => GIF_fill
      vdevice%dev%Vfont     => GIF_font
      vdevice%dev%Vfrontb   => GIF_frontb
      vdevice%dev%Vgetkey   => noop
      vdevice%dev%Vinit     => GIF_init
      vdevice%dev%Vlocator  => noop2
      vdevice%dev%Vmapcolor => GIF_mapcolor
      vdevice%dev%Vsetlw    => GIF_setlw
      vdevice%dev%Vstring   => GIF_string
      vdevice%dev%Vswapb    => GIF_swapb
      vdevice%dev%Vsync     => noop_zero
      GIF_draw_devcpy = 0
   end function GIF_draw_devcpy
end module m_draw_drv_gif
