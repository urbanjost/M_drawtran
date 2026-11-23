! A driver for an ANSI-compatible terminal using terminal escape sequences
! would be useful to explore
! 
! Building an ANSI terminal driver (m_draw_drv_ansi) is a great addition
! to M_draw because it requires zero external C libraries, works over SSH,
! and runs natively on Linux, macOS, Cygwin, and Windows Terminal.
! 
! Here are the three best ways to render vector graphics in modern
! ANSI-compatible terminals, along with a complete, zero-dependency Fortran
! driver using Unicode Braille sub-pixel rendering.
! 
! 1. Choosing a Terminal Graphics Protocol
! 
! Approach	Effective Resolution	Terminal Compatibility	Best Use Case
! Unicode Braille (U+2800–U+28FF) + 24-bit TrueColor (\e[38;2;r;g;bm)	2× horizontal × 4× vertical per character cell (e.g., 160×96 pixels on an 80×24 terminal)	Universal (Windows Terminal, Cygwin mintty, xterm, GNOME Terminal, macOS Terminal, iTerm2, tmux, SSH)	Best all-around default: crisp lines, curves, and wireframe 3D with zero external dependencies
! Half-Blocks (▀ U+2580) + TrueColor FG/BG	1× horizontal × 2× vertical per cell, with independent top/bottom pixel colors	Universal	Solid filled polygons, shaded surfaces, and color gradients where neighboring pixels differ in color
! Sixel Graphics (\ePq ... \e\\)	Full 1:1 native pixel resolution (e.g., 800×600)	High in modern terminals (Windows Terminal, mintty, xterm -ti vt340, foot, WezTerm, Konsole)	Pixel-accurate vector/raster output identical to X11/Win32 when Sixel is supported
! 
! 2. How Unicode Braille Sub-Pixel Mapping Works
! 
! Each Unicode Braille character (U+2800 + bitmask) encodes a 2×4 grid
! of sub-pixels inside a single terminal cell:
! text
! 
! Sub-pixel offsets inside one character cell:
!   (0,0) = bit 0 (0x01)   (1,0) = bit 3 (0x08)
!   (0,1) = bit 1 (0x02)   (1,1) = bit 4 (0x10)
!   (0,2) = bit 2 (0x04)   (1,2) = bit 5 (0x20)
!   (0,3) = bit 6 (0x40)   (1,3) = bit 7 (0x80)
! 
!     Maintaining an internal framebuffer fb(0:width-1, 0:height-1)
!     of packed RGB colors (0 = background) lets Vdraw (Bresenham line
!     algorithm), Vfill (scanline polygon fill), and Vstring draw at 2×4
!     sub-pixel resolution.
! 
!     On Vsync / Vswapb / Vgetkey, the driver flushes the framebuffer to
!     stdout using ANSI cursor positioning (ESC[H), 24-bit foreground colors
!     (ESC[38;2;R;G;Bm), and UTF-8 encoded Braille characters (char(226)
!     // char(160 + mask/64) // char(128 + mod(mask,64))).
! 
!     Even mouse input works via ANSI SGR mouse reporting (ESC[?1000;1006h),
!     which sends ESC[<button;col;rowM over stdin when clicked!
! 
module m_draw_drv_ansi
   use, intrinsic :: iso_fortran_env, only: real32, output_unit, input_unit
   use m_draw_types
   use m_draw_pref
   use m_draw_verror
   implicit none
   private
   public :: ansi_draw_devcpy, ansi_low_draw_devcpy

   integer, parameter :: CMAPSIZE       = 256
   integer, parameter :: DEV_ANSI_UPPER = 0  ! Black background ("ANSI")
   integer, parameter :: DEV_ANSI_LOWER = 1  ! White background ("ansi")
   character(len=1), parameter :: ESC   = char(27)

   ! Braille dot bit weights for (dx=0..1, dy=0..3)
   integer, parameter :: BRAILLE_BIT(0:1, 0:3) = reshape([ &
      1,   8, &   ! dy = 0: (0,0)=0x01, (1,0)=0x08
      2,  16, &   ! dy = 1: (0,1)=0x02, (1,1)=0x10
      4,  32, &   ! dy = 2: (0,2)=0x04, (1,2)=0x20
      64, 128  &   ! dy = 3: (0,3)=0x40, (1,3)=0x80
      ], [2, 4])

   ! Driver state
   integer, allocatable, save :: fb(:, :)         ! Packed RGB (0 = unset/bg), indexed (0:W-1, 0:H-1)
   integer, save :: cmap(3, 0:CMAPSIZE-1) = 0     ! RGB palette (0..255)
   integer, save :: cur_color_idx = 7
   integer, save :: bg_color_idx  = 0
   integer, save :: cur_lw        = 1
   integer, save :: cols = 80, rows = 24
   integer, save :: pix_w = 160, pix_h = 96
   integer, save :: back_used = 0
   integer, save :: GLOBAL_device = DEV_ANSI_UPPER
   logical, save :: dirty = .false.

contains

   pure integer function pack_rgb(r, g, b)
      integer, intent(in) :: r, g, b
      ! Bit 24 marks pixel as drawn so black (0,0,0) on white bg is distinguishable
      pack_rgb = ior(ishft(1, 24), ior(ior(iand(r, 255), ishft(iand(g, 255), 8)), ishft(iand(b, 255), 16)))
   end function pack_rgb

   subroutine set_pixel(x, y, packed)
      integer, intent(in) :: x, y, packed
      integer :: dx, dy, r
      if (cur_lw <= 1) then
         if (x >= 0 .and. x < pix_w .and. y >= 0 .and. y < pix_h) then
            fb(x, y) = packed
         end if
      else
         r = cur_lw / 2
         do dy = -r, r
            do dx = -r, r
               if (x + dx >= 0 .and. x + dx < pix_w .and. y + dy >= 0 .and. y + dy < pix_h) then
                  fb(x + dx, y + dy) = packed
               end if
            end do
         end do
      end if
   end subroutine set_pixel

   subroutine draw_bresenham(x0_in, y0_in, x1_in, y1_in, packed)
      integer, intent(in) :: x0_in, y0_in, x1_in, y1_in, packed
      integer :: x0, y0, x1, y1, dx, dy, sx, sy, err, e2

      x0 = x0_in; y0 = y0_in
      x1 = x1_in; y1 = y1_in
      dx = abs(x1 - x0)
      dy = -abs(y1 - y0)
      sx = merge(1, -1, x0 < x1)
      sy = merge(1, -1, y0 < y1)
      err = dx + dy

      do
         call set_pixel(x0, y0, packed)
         if (x0 == x1 .and. y0 == y1) exit
         e2 = 2 * err
         if (e2 >= dy) then
            err = err + dy
            x0 = x0 + sx
         end if
         if (e2 <= dx) then
            err = err + dx
            y0 = y0 + sy
         end if
      end do
   end subroutine draw_bresenham

   subroutine render_frame()
      integer :: cx, cy, dx, dy, px, py, mask, cell_rgb, p
      integer :: r, g, b, last_rgb
      character(len=3) :: utf8_braille
      character(len=32) :: esc_buf

      ! Hide cursor, move home, set background color
      r = cmap(1, bg_color_idx); g = cmap(2, bg_color_idx); b = cmap(3, bg_color_idx)
      write(output_unit, '(A,I0,A,I0,A,I0,A)', advance='no') &
         ESC // '[?25l' // ESC // '[H' // ESC // '[48;2;', r, ';', g, ';', b, 'm'

      last_rgb = -1

      do cy = 0, rows - 1
         do cx = 0, cols - 1
            mask = 0
            cell_rgb = 0
            do dy = 0, 3
               py = cy * 4 + dy
               do dx = 0, 1
                  px = cx * 2 + dx
                  p = fb(px, py)
                  if (p /= 0) then
                     mask = ior(mask, BRAILLE_BIT(dx, dy))
                     cell_rgb = iand(p, int(Z'FFFFFF'))
                  end if
               end do
            end do

            if (mask == 0) then
               write(output_unit, '(A)', advance='no') ' '
            else
               if (cell_rgb /= last_rgb) then
                  r = iand(cell_rgb, 255)
                  g = iand(ishft(cell_rgb, -8), 255)
                  b = iand(ishft(cell_rgb, -16), 255)
                  write(esc_buf, '(A,I0,A,I0,A,I0,A)') ESC // '[38;2;', r, ';', g, ';', b, 'm'
                  write(output_unit, '(A)', advance='no') trim(esc_buf)
                  last_rgb = cell_rgb
               end if
               ! UTF-8 encoding for U+2800 + mask (0xE2 0xA0..0xA3 0x80..0xBF)
               utf8_braille(1:1) = char(226)
               utf8_braille(2:2) = char(160 + ishft(mask, -6))
               utf8_braille(3:3) = char(128 + iand(mask, 63))
               write(output_unit, '(A)', advance='no') utf8_braille
            end if
         end do
         if (cy < rows - 1) write(output_unit, '(A)') ''
      end do

      ! Reset attributes and show cursor
      write(output_unit, '(A)', advance='no') ESC // '[0m' // ESC // '[?25h'
      flush(output_unit)
      dirty = .false.
   end subroutine render_frame

   integer function ANSI_init()
      integer :: prefx, prefy, prefxs, prefys, idum

      call draw_getprefposandsize(prefx, prefy, prefxs, prefys)
      ! Allow prefsize in pixels (divided by 2x4) or default 80x24 cells (160x96 subpixels)
      if (prefxs > 0 .and. prefys > 0) then
         cols = max(10, prefxs / 2)
         rows = max(5,  prefys / 4)
      else
         cols = 80
         rows = 24
      end if

      pix_w = cols * 2
      pix_h = rows * 4

      if (allocated(fb)) deallocate(fb)
      allocate(fb(0:pix_w-1, 0:pix_h-1))
      fb = 0

      vdevice%depth   = 24
      vdevice%sizeSx  = pix_w - 1
      vdevice%sizeSy  = pix_h - 1
      vdevice%sizeX   = min(pix_w, pix_h)
      vdevice%sizeY   = vdevice%sizeX
      vdevice%hheight = 4.0_real32
      vdevice%hwidth  = 2.0_real32

      if (GLOBAL_device == DEV_ANSI_UPPER) then
         idum = ANSI_mapcolor(0, 0, 0, 0)
         idum = ANSI_mapcolor(7, 255, 255, 255)
      else
         idum = ANSI_mapcolor(7, 0, 0, 0)
         idum = ANSI_mapcolor(0, 255, 255, 255)
      end if
      idum = ANSI_mapcolor(1, 255, 0, 0)
      idum = ANSI_mapcolor(2, 0, 255, 0)
      idum = ANSI_mapcolor(3, 255, 255, 0)
      idum = ANSI_mapcolor(4, 0, 0, 255)
      idum = ANSI_mapcolor(5, 255, 0, 255)
      idum = ANSI_mapcolor(6, 0, 255, 255)

      bg_color_idx  = 0
      cur_color_idx = 7
      cur_lw        = 1
      back_used     = 0

      ! Switch to alternate screen buffer & clear
      write(output_unit, '(A)', advance='no') ESC // '[?1049h' // ESC // '[2J' // ESC // '[H'
      flush(output_unit)

      ANSI_init = 1
   end function ANSI_init

   integer function ANSI_exit()
      if (dirty) call render_frame()
      ! Restore primary screen buffer & cursor
      write(output_unit, '(A)') ESC // '[0m' // ESC // '[?25h' // ESC // '[?1049l'
      flush(output_unit)
      if (allocated(fb)) deallocate(fb)
      ANSI_exit = 1
   end function ANSI_exit

   integer function ANSI_draw(x, y)
      integer, intent(in) :: x, y
      integer :: packed
      packed = pack_rgb(cmap(1, cur_color_idx), cmap(2, cur_color_idx), cmap(3, cur_color_idx))
      call draw_bresenham(vdevice%cpVx, vdevice%sizeSy - vdevice%cpVy, x, vdevice%sizeSy - y, packed)
      dirty = .true.
      if (vdevice%sync /= 0 .and. back_used == 0) call render_frame()
      ANSI_draw = UNUSED
   end function ANSI_draw

   integer function ANSI_clear()
      bg_color_idx = cur_color_idx
      fb = 0
      dirty = .true.
      if (back_used == 0) call render_frame()
      ANSI_clear = UNUSED
   end function ANSI_clear

   integer function ANSI_color(ind_in)
      integer, intent(in) :: ind_in
      integer :: ind
      if (ind_in < 0) then
         ind = merge(0, ind_in, ind_in == -1)
         cur_lw = max(1, abs(ind))
      else
         cur_color_idx = mod(ind_in, CMAPSIZE)
      end if
      ANSI_color = UNUSED
   end function ANSI_color

   integer function ANSI_setlw(ind_in)
      integer, intent(in) :: ind_in
      integer :: ind
      ind = int(real(vdevice%sizeX, real32) / 10000.0_real32 * real(ind_in, real32))
      cur_lw = max(1, abs(ind))
      ANSI_setlw = UNUSED
   end function ANSI_setlw

   integer function ANSI_mapcolor(i, r, g, b)
      integer, intent(in) :: i, r, g, b
      if (i >= CMAPSIZE .or. i < 0) then
         ANSI_mapcolor = -1
         return
      end if
      cmap(1, i) = iand(r, 255)
      cmap(2, i) = iand(g, 255)
      cmap(3, i) = iand(b, 255)
      ANSI_mapcolor = 0
   end function ANSI_mapcolor

   integer function ANSI_font(fontfile)
      character(len=*), intent(in) :: fontfile
      vdevice%hheight = 4.0_real32
      vdevice%hwidth  = 2.0_real32
      ANSI_font = 1
   end function ANSI_font

   integer function ANSI_char(c)
      character(len=1), intent(in) :: c
      integer :: cx, cy, r, g, b
      if (dirty) call render_frame()
      cx = min(cols, max(1, vdevice%cpVx / 2 + 1))
      cy = min(rows, max(1, (vdevice%sizeSy - vdevice%cpVy) / 4 + 1))
      r = cmap(1, cur_color_idx); g = cmap(2, cur_color_idx); b = cmap(3, cur_color_idx)
      write(output_unit, '(A,I0,A,I0,A,I0,A,I0,A,I0,A,A,A)', advance='no') &
         ESC // '[', cy, ';', cx, 'H' // ESC // '[38;2;', r, ';', g, ';', b, 'm', c, ESC // '[0m'
      flush(output_unit)
      ANSI_char = UNUSED
   end function ANSI_char

   integer function ANSI_string(s)
      character(len=*), intent(in) :: s
      integer :: cx, cy, r, g, b
      if (dirty) call render_frame()
      cx = min(cols, max(1, vdevice%cpVx / 2 + 1))
      cy = min(rows, max(1, (vdevice%sizeSy - vdevice%cpVy) / 4 + 1))
      r = cmap(1, cur_color_idx); g = cmap(2, cur_color_idx); b = cmap(3, cur_color_idx)
      write(output_unit, '(A,I0,A,I0,A,I0,A,I0,A,I0,A,A,A)', advance='no') &
         ESC // '[', cy, ';', cx, 'H' // ESC // '[38;2;', r, ';', g, ';', b, 'm', trim(s), ESC // '[0m'
      flush(output_unit)
      ANSI_string = UNUSED
   end function ANSI_string

   integer function ANSI_fill(n, x, y)
      integer, intent(in) :: n
      integer, intent(in) :: x(n), y(n)
      integer :: ymin, ymax, py, i, j, nodes, nodeX(MAXVERTS), swap_tmp, px, packed
      integer :: yi, yj, xi, xj
      if (n < 3) then
         ANSI_fill = UNUSED
         return
      end if
      packed = pack_rgb(cmap(1, cur_color_idx), cmap(2, cur_color_idx), cmap(3, cur_color_idx))
      ymin = max(0, minval(vdevice%sizeSy - y(1:n)))
      ymax = min(pix_h - 1, maxval(vdevice%sizeSy - y(1:n)))
      do py = ymin, ymax
         nodes = 0
         j = n
         do i = 1, n
            yi = vdevice%sizeSy - y(i)
            yj = vdevice%sizeSy - y(j)
            xi = x(i)
            xj = x(j)
            if ((yi < py .and. yj >= py) .or. (yj < py .and. yi >= py)) then
               if (nodes < MAXVERTS) then
                  nodes = nodes + 1
                  nodeX(nodes) = int(real(xi, real32) + real(py - yi, real32) / real(yj - yi, real32) * real(xj - xi, real32))
               end if
            end if
            j = i
         end do
! Sort intersection X coordinates
         do i = 1, nodes - 1
            do j = i + 1, nodes
               if (nodeX(i) > nodeX(j)) then
                  swap_tmp = nodeX(i); nodeX(i) = nodeX(j); nodeX(j) = swap_tmp
               end if
            end do
         end do
         do i = 1, nodes - 1, 2
            do px = max(0, nodeX(i)), min(pix_w - 1, nodeX(i + 1))
               fb(px, py) = packed
            end do
         end do
      end do
      vdevice%cpVx = x(n); vdevice%cpVy = y(n)
      dirty = .true.
      if (vdevice%sync /= 0 .and. back_used == 0) call render_frame()
      ANSI_fill = UNUSED
   end function ANSI_fill

   integer function ANSI_getkey()
      character(len=1) :: ch
      integer :: ios
      if (dirty) call render_frame()
! Note: Standard Fortran read blocks until Enter unless terminal is put in raw mode.
      read(input_unit, '(A1)', advance='no', iostat=ios) ch
      if (ios == 0) then
         ANSI_getkey = ichar(ch)
      else
         ANSI_getkey = 13
      end if
   end function ANSI_getkey

   integer function ANSI_checkkey()
      ANSI_checkkey = 0
   end function ANSI_checkkey

   integer function ANSI_locator(wx, wy)
      integer, intent(out) :: wx, wy
      wx = vdevice%cpVx
      wy = vdevice%cpVy
      ANSI_locator = 0
   end function ANSI_locator

   integer function ANSI_backbuf()
      back_used = 1
      ANSI_backbuf = 1
   end function ANSI_backbuf

   integer function ANSI_swapbuf()
      call render_frame()
      ANSI_swapbuf = UNUSED
   end function ANSI_swapbuf

   integer function ANSI_frontbuf()
      back_used = 0
      if (dirty) call render_frame()
      ANSI_frontbuf = UNUSED
   end function ANSI_frontbuf

   integer function ANSI_sync()
      if (dirty) call render_frame()
      ANSI_sync = UNUSED
   end function ANSI_sync

   subroutine setup_ansidev()
      vdevice%dev%large = "large"
      vdevice%dev%small = "small"
      vdevice%dev%Vbackb => ANSI_backbuf
      vdevice%dev%Vchar => ANSI_char
      vdevice%dev%Vcheckkey => ANSI_checkkey
      vdevice%dev%Vclear => ANSI_clear
      vdevice%dev%Vcolor => ANSI_color
      vdevice%dev%Vdraw => ANSI_draw
      vdevice%dev%Vexit => ANSI_exit
      vdevice%dev%Vfill => ANSI_fill
      vdevice%dev%Vfont => ANSI_font
      vdevice%dev%Vfrontb => ANSI_frontbuf
      vdevice%dev%Vgetkey => ANSI_getkey
      vdevice%dev%Vinit => ANSI_init
      vdevice%dev%Vlocator => ANSI_locator
      vdevice%dev%Vmapcolor => ANSI_mapcolor
      vdevice%dev%Vsetlw => ANSI_setlw
      vdevice%dev%Vstring => ANSI_string
      vdevice%dev%Vswapb => ANSI_swapbuf
      vdevice%dev%Vsync => ANSI_sync
   end subroutine setup_ansidev

   integer function ansi_draw_devcpy()
      call setup_ansidev()
      vdevice%dev%devname = "ANSI"
      GLOBAL_device = DEV_ANSI_UPPER
      ansi_draw_devcpy = UNUSED
   end function ansi_draw_devcpy

   integer function ansi_low_draw_devcpy()
      call setup_ansidev()
      vdevice%dev%devname = "ansi"
      GLOBAL_device = DEV_ANSI_LOWER
      ansi_low_draw_devcpy = UNUSED
   end function ansi_low_draw_devcpy

end module m_draw_drv_ansi

