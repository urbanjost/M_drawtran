module m_draw_drv_ppm
   use, intrinsic :: iso_fortran_env, only: real32, real64, output_unit, error_unit
   use m_draw_types
   use m_draw_pref
   use m_draw_drivers
   use m_draw_bitmap_fonts
   implicit none
   private
   public :: PPM_draw_devcpy, P3_draw_devcpy, P6_draw_devcpy, CHAR_draw_devcpy

   integer, parameter :: P3   = 2
   integer, parameter :: P6   = 3
   integer, parameter :: CHAR_MODE = 4

   integer, parameter :: UNDRAWN = 0
   integer, parameter :: DRAWN   = 1

   integer, parameter :: CMAPSIZE  = 8192
   integer, parameter :: CMAPDEPTH = 13

   type :: ColorTable
      integer :: r = 255, g = 255, b = 255
   end type ColorTable

   type(ColorTable), save :: coltab(CMAPSIZE)

   integer, save :: X_SIZE = 0, Y_SIZE = 0, LAST = 0
   integer, allocatable, save :: graphics_r(:), graphics_g(:), graphics_b(:)

   integer, save :: GLOBAL_driver  = 0
   integer, save :: GLOBAL_font    = 0
   integer, save :: GLOBAL_color   = 0
   integer, save :: GLOBAL_drawn   = UNDRAWN
   integer, save :: GLOBAL_rasters = 1
   integer, save :: GLOBAL_lastx   = -1111111
   integer, save :: GLOBAL_lasty   = -1111111

contains

   subroutine SET_PIXEL(x, y)
      integer, intent(in) :: x, y
      integer :: idx, c_idx
      idx = max(1, min(LAST, x * Y_SIZE + y + 1))
      c_idx = GLOBAL_color + 1
      graphics_r(idx) = coltab(c_idx)%r
      graphics_g(idx) = coltab(c_idx)%g
      graphics_b(idx) = coltab(c_idx)%b
   end subroutine SET_PIXEL

   subroutine PPM_MEMSET()
      integer :: c_idx
      if (.not. allocated(graphics_r)) return
      c_idx = GLOBAL_color + 1
      graphics_r = coltab(c_idx)%r
      graphics_g = coltab(c_idx)%g
      graphics_b = coltab(c_idx)%b
   end subroutine PPM_MEMSET

   integer function PPM_setlw(w)
      integer, intent(in) :: w
      GLOBAL_rasters = max(1, int(real(w, real32) * real(vdevice%sizeX, real32) / 10000.0_real32))
      PPM_setlw = 0
   end function PPM_setlw

   integer function PPM_color(col)
      integer, intent(in) :: col
      integer :: idum
      if (col < 0) then
         idum = PPM_setlw(abs(col))
      else
         GLOBAL_color = abs(mod(col, CMAPSIZE))
      end if
      PPM_color = 0
   end function PPM_color

   integer function PPM_mapcolor(indx, r, g, b)
      integer, intent(in) :: indx, r, g, b
      if (indx < CMAPSIZE .and. indx >= 0) then
         coltab(indx + 1)%r = abs(mod(r, CMAPSIZE))
         coltab(indx + 1)%g = abs(mod(g, CMAPSIZE))
         coltab(indx + 1)%b = abs(mod(b, CMAPSIZE))
      end if
      PPM_mapcolor = 0
   end function PPM_mapcolor

   subroutine PPM_RESIZE()
      integer :: prefx, prefy, prefxs, prefys
      call draw_getprefposandsize(prefx, prefy, prefxs, prefys)
      if (prefxs /= -1) then
         if (prefys <= 0) then
            write(error_unit, '(A, I0, A)') "*PPM_init* y size of ", prefys, " set to 400"
            prefys = 400
         end if
         vdevice%sizeSy = prefys

         if (prefxs <= 0) then
            write(error_unit, '(A, I0, A)') "*PPM_init* x size of ", prefxs, " set to 600"
            prefxs = 600
         end if
         vdevice%sizeSx = prefxs
      else
         prefx = 0; prefy = 0
         vdevice%sizeSy = 400; vdevice%sizeSx = 600
      end if

      X_SIZE = vdevice%sizeSx
      Y_SIZE = vdevice%sizeSy
      LAST   = X_SIZE * Y_SIZE
      if (allocated(graphics_r)) deallocate(graphics_r, graphics_g, graphics_b)
      allocate(graphics_r(LAST), graphics_g(LAST), graphics_b(LAST))
   end subroutine PPM_RESIZE

   integer function PPM_init()
      integer :: idum
      call PPM_RESIZE()
      vdevice%sizeX = min(vdevice%sizeSy, vdevice%sizeSx)
      vdevice%sizeY = vdevice%sizeX

      coltab = ColorTable(255, 255, 255)
      idum = PPM_mapcolor(0, 255, 255, 255)
      idum = PPM_mapcolor(1, 255,   0,   0)
      idum = PPM_mapcolor(2,   0, 255,   0)
      idum = PPM_mapcolor(3, 255, 255,   0)
      idum = PPM_mapcolor(4,   0,   0, 255)
      idum = PPM_mapcolor(5, 255,   0, 255)
      idum = PPM_mapcolor(6,   0, 255, 255)
      idum = PPM_mapcolor(7,   0,   0,   0)

      call PPM_MEMSET()
      vdevice%depth = CMAPDEPTH
      draw_unit = draw_outfile()
      GLOBAL_lastx = -1111111; GLOBAL_lasty = -1111111
      GLOBAL_drawn = UNDRAWN
      PPM_init = 1
   end function PPM_init

   subroutine PPM_DRAW_LINE(x, y)
      integer, intent(in) :: x, y
      integer :: runcount, dx, dy, xinc, yinc, xplot, yplot

      call SET_PIXEL(GLOBAL_lastx, GLOBAL_lasty)
      runcount = 0
      dx = abs(GLOBAL_lastx - x)
      xinc = 0
      if (x > GLOBAL_lastx) xinc = 1
      if (x < GLOBAL_lastx) xinc = -1

      dy = abs(GLOBAL_lasty - y)
      yinc = 0
      if (y > GLOBAL_lasty) yinc = 1
      if (y < GLOBAL_lasty) yinc = -1

      xplot = GLOBAL_lastx; yplot = GLOBAL_lasty
      if (dx > dy) then
         do while (xplot /= x)
            xplot = xplot + xinc
            runcount = runcount + dy
            if (runcount >= (dx - runcount)) then
               yplot = yplot + yinc
               runcount = runcount - dx
            end if
            call SET_PIXEL(xplot, yplot)
         end do
      else
         do while (yplot /= y)
            yplot = yplot + yinc
            runcount = runcount + dx
            if (runcount >= (dy - runcount)) then
               xplot = xplot + xinc
               runcount = runcount - dy
            end if
            call SET_PIXEL(xplot, yplot)
         end do
      end if
      GLOBAL_lastx = xplot; GLOBAL_lasty = yplot
   end subroutine PPM_DRAW_LINE

   integer function PPM_YINTERCEPT(yscan, x1, y1, x2, y2, xintercept, yprev)
      integer, intent(in)    :: yscan, x1, y1, x2, y2
      integer, intent(out)   :: xintercept
      integer, intent(inout) :: yprev
      integer :: deltay, yprevious
      real(real32) :: t

      yprevious = yprev
      yprev = y1
      deltay = y2 - y1
      if (deltay == 0) then
         yprev = yprevious
         PPM_YINTERCEPT = 0; return
      end if

      t = real(yscan - y1, real32) / real(deltay, real32)
      if (t > 0.0_real32 .and. t <= 1.0_real32) then
         xintercept = int(real(x1, real32) + t * real(x2 - x1, real32) + 0.5_real32)
         PPM_YINTERCEPT = 1; return
      else if (t == 0.0_real32) then
         xintercept = int(real(x1, real32) + t * real(x2 - x1, real32) + 0.5_real32)
         if ((yprevious <= y1 .and. y2 <= y1) .or. (yprevious >= y1 .and. y2 >= y1)) then
            PPM_YINTERCEPT = 1; return
         else
            PPM_YINTERCEPT = 0; return
         end if
      end if
      PPM_YINTERCEPT = 0
   end function PPM_YINTERCEPT

   subroutine PPM_SOLID_FILL(n, x, y)
      integer, intent(in) :: n
      integer, intent(in) :: x(n), y(n)
      integer :: i, j, yhorizontal, xint, tmp, ymin, ymax, xi(MAXVERTS), yprev
      logical :: sorted

      if (n > MAXVERTS) then
         write(error_unit, '(A, I0, A)') "*PPM_SOLID_FILL* more than ", MAXVERTS, " vertices in a polygon"
         return
      end if

      ymin = max(minval(y(1:n)), 0)
      ymax = min(maxval(y(1:n)), Y_SIZE)

      yhorizontal = ymax
      do while (yhorizontal >= ymin)
         j = 0
         yprev = y(n)
         do i = 1, n - 1
            if (PPM_YINTERCEPT(yhorizontal, x(i), y(i), x(i+1), y(i+1), xint, yprev) /= 0) then
               j = j + 1; xi(j) = xint
            end if
         end do
         if (PPM_YINTERCEPT(yhorizontal, x(n), y(n), x(1), y(1), xint, yprev) /= 0) then
            j = j + 1; xi(j) = xint
         end if

         sorted = .false.
         do while (.not. sorted)
            sorted = .true.
            do i = 1, j - 1
               if (xi(i) > xi(i+1)) then
                  tmp = xi(i); xi(i) = xi(i+1); xi(i+1) = tmp
                  sorted = .false.
               end if
            end do
         end do

         do i = 1, j - 1, 2
            GLOBAL_lastx = max(0, min(xi(i), X_SIZE))
            GLOBAL_lasty = yhorizontal
            call PPM_DRAW_LINE(max(0, min(xi(i+1), X_SIZE)), yhorizontal)
         end do
         yhorizontal = yhorizontal - 1
      end do
   end subroutine PPM_SOLID_FILL

   subroutine PPM_ENDCAP_CIRCLE(x, y)
      integer, intent(in) :: x, y
      integer, parameter :: nsegs = 15
      real(real32) :: cx, cy, dx, dy, angle, cosine, sine
      integer :: cxras(nsegs), cyras(nsegs), i

      angle  = 2.0_real32 * PI / real(nsegs, real32)
      cosine = cos(angle); sine = sin(angle)

      cx = real(x, real32) + real(GLOBAL_rasters, real32) / 2.0_real32
      cy = real(y, real32)
      cxras(1) = int(cx); cyras(1) = int(cy)
      do i = 2, nsegs
         dx = cx - real(x, real32); dy = cy - real(y, real32)
         cx = real(x, real32) + dx * cosine - dy * sine
         cy = real(y, real32) + dx * sine   + dy * cosine
         cxras(i) = int(cx); cyras(i) = int(cy)
      end do
      call PPM_SOLID_FILL(nsegs, cxras, cyras)
   end subroutine PPM_ENDCAP_CIRCLE

   integer function PPM_fill(n, x, y)
      integer, intent(in) :: n
      integer, intent(in) :: x(n), y(n)
      integer :: i

      GLOBAL_lastx = x(1); GLOBAL_lasty = y(1)
      do i = 2, n
         call PPM_DRAW_LINE(x(i), y(i))
      end do
      if (x(n) /= x(1) .or. y(n) /= y(1)) call PPM_DRAW_LINE(x(1), y(1))

      call PPM_SOLID_FILL(n, x, y)

      GLOBAL_lastx = x(n); vdevice%cpVx = x(n)
      GLOBAL_lasty = y(n); vdevice%cpVy = y(n)
      GLOBAL_drawn = DRAWN
      PPM_fill = 0
   end function PPM_fill

   integer function PPM_draw(x, y)
      integer, intent(in) :: x, y
      integer :: holdx, holdy, xwide(4), ywide(4)
      real(real32) :: cosa, sina
      real(real64) :: angle

      if (GLOBAL_lastx /= vdevice%cpVx .or. GLOBAL_lasty /= vdevice%cpVy) then
         GLOBAL_lastx = vdevice%cpVx; GLOBAL_lasty = vdevice%cpVy
      end if

      if (GLOBAL_rasters <= 1) then
         call PPM_DRAW_LINE(x, y)
      else
         if (GLOBAL_rasters >= 6) then
            holdx = GLOBAL_lastx; holdy = GLOBAL_lasty

call PPM_ENDCAP_CIRCLE(GLOBAL_lastx, GLOBAL_lasty)
call PPM_ENDCAP_CIRCLE(x, y)
GLOBAL_lastx = holdx; GLOBAL_lasty = holdy
end if
angle = atan2(real(y - GLOBAL_lasty, real64), real(x - GLOBAL_lastx, real64)) + real(PI, real64) / 2.0_real64
cosa = (real(GLOBAL_rasters, real32) / 2.0_real32) * real(cos(angle), real32)
sina = (real(GLOBAL_rasters, real32) / 2.0_real32) * real(sin(angle), real32)
xwide = [ int(real(x, real32) + cosa), int(real(GLOBAL_lastx, real32) + cosa), &
int(real(GLOBAL_lastx, real32) - cosa), int(real(x, real32) - cosa) ]
ywide = [ int(real(y, real32) + sina), int(real(GLOBAL_lasty, real32) + sina), &
int(real(GLOBAL_lasty, real32) - sina), int(real(y, real32) - sina) ]
call PPM_SOLID_FILL(4, xwide, ywide)
end if
GLOBAL_drawn = DRAWN
PPM_draw = 0
end function PPM_draw
subroutine P3_print_graphics()
integer :: x, y, idx
character(len=8) :: dt
character(len=10) :: tm
call date_and_time(date=dt, time=tm)
write(draw_unit, '(A)') "P3"
write(draw_unit, '(I0, 1X, I0)') X_SIZE, Y_SIZE
write(draw_unit, '(A)') "# TRUECOLOR 8192 entry colortable version"
write(draw_unit, '(A)') "# CREATOR: M_DRAW ppm driver; version 3.0 2019/07/10"
write(draw_unit, '(A)') "# AUTHOR: John S. Urban"
write(draw_unit, '(A, A, "-", A, "-", A, " ", A, ":", A, ":", A)') &
"# CreationDate: ", dt(1:4), dt(5:6), dt(7:8), tm(1:2), tm(3:4), tm(5:6)
write(draw_unit, '(A)') "255"
do y = Y_SIZE - 1, 0, -1
do x = 0, X_SIZE - 1
idx = Y_SIZE * x + y + 1
write(draw_unit, '(I0, 1X, I0, 1X, I0)') graphics_r(idx), graphics_g(idx), graphics_b(idx)
end do
end do
write(draw_unit, *)
GLOBAL_drawn = UNDRAWN
end subroutine P3_print_graphics
subroutine P6_print_graphics()
integer :: x, y, idx
character(len=8) :: dt
character(len=10) :: tm
call date_and_time(date=dt, time=tm)
write(draw_unit, '(A)') "P6"
write(draw_unit, '(I0, 1X, I0)') X_SIZE, Y_SIZE
write(draw_unit, '(A)') "# CREATOR: M_DRAW ppm driver; version 1.0 1997/02/02"
write(draw_unit, '(A)') "# AUTHOR: John S. Urban"
write(draw_unit, '(A, A, "-", A, "-", A, " ", A, ":", A, ":", A)') &
"# CreationDate: ", dt(1:4), dt(5:6), dt(7:8), tm(1:2), tm(3:4), tm(5:6)
write(draw_unit, '(A)') "255"
do y = Y_SIZE - 1, 0, -1
do x = 0, X_SIZE - 1
idx = Y_SIZE * x + y + 1
write(draw_unit, '(3A1)', advance='no') &
char(iand(graphics_r(idx), 255)), &
char(iand(graphics_g(idx), 255)), &
char(iand(graphics_b(idx), 255))
end do
end do
GLOBAL_drawn = UNDRAWN
end subroutine P6_print_graphics
subroutine CHAR_print_graphics()
integer :: x, y, idx, icolor
write(draw_unit, '(A, I0, ";", I0, "t")') char(27)//"[8;", Y_SIZE, X_SIZE
do y = Y_SIZE - 1, 0, -1
do x = 0, X_SIZE - 1
idx = Y_SIZE * x + y + 1
icolor = mod((graphics_r(idx) + graphics_g(idx)*2 + graphics_b(idx)*4) / 255, 8)
write(draw_unit, '(A, I0, "m ")', advance='no') char(27)//"[4", icolor
end do
write(draw_unit, *)
end do
write(draw_unit, '(A)') char(27)//"[37;40m"
GLOBAL_drawn = UNDRAWN
end subroutine CHAR_print_graphics
subroutine PPM_PRINT()
if (GLOBAL_drawn /= 0) then
select case (GLOBAL_driver)
case (P3); call P3_print_graphics()
case (P6); call P6_print_graphics()
case (CHAR_MODE); call CHAR_print_graphics()
case default
write(error_unit, '(A)') "ppm driver: UNKNOWN DRIVER NAME"
call P3_print_graphics()
end select
end if
flush(draw_unit)
end subroutine PPM_PRINT
integer function PPM_exit()
call PPM_PRINT()
if (draw_unit /= output_unit .and. draw_unit /= error_unit) then
flush(draw_unit)
close(draw_unit)
end if
if (allocated(graphics_r)) deallocate(graphics_r, graphics_g, graphics_b)
PPM_exit = 0
end function PPM_exit
integer function PPM_clear()
call PPM_PRINT()
call PPM_RESIZE()
call PPM_MEMSET()
PPM_clear = 0
end function PPM_clear
integer function PPM_font(fontname)
character(len=*), intent(in) :: fontname
if (trim(fontname) == "small") then
vdevice%hwidth = 8.00_real32; vdevice%hheight = 13.0_real32; GLOBAL_font = 0
else if (trim(fontname) == "large") then
vdevice%hwidth = 15.00_real32; vdevice%hheight = 39.00_real32; GLOBAL_font = 1
else
PPM_font = 0; return
end if
PPM_font = 1
end function PPM_font
subroutine PPM_char0(c)
character(len=1), intent(in) :: c
integer :: i, j, k, wrd
k = ichar(c) - 32 + 1
if (k < 1 .or. k > NUM_CHARS) return
do i = 1, 11
wrd = font8x13(k, i)
do j = 0, 7
if (iand(wrd, ishft(128, -j)) /= 0) then
call SET_PIXEL(j + vdevice%cpVx, vdevice%cpVy - (i - 1) + 13)
end if
end do
end do
vdevice%cpVx = vdevice%cpVx + 8
GLOBAL_lastx = vdevice%cpVx; GLOBAL_lasty = vdevice%cpVy
GLOBAL_drawn = DRAWN
end subroutine PPM_char0
subroutine PPM_char1(c)
character(len=1), intent(in) :: c
integer :: i, j, k, wrd
k = ichar(c) - 32 + 1
if (k < 1 .or. k > NUM_CHARS) return
do i = 1, 25
wrd = font13x25(k, i)
do j = 0, 12
if (iand(ishft(wrd, -j), 1) /= 0) then
call SET_PIXEL(j + vdevice%cpVx, vdevice%cpVy + (i - 1))
end if
end do
end do
vdevice%cpVx = vdevice%cpVx + 15
GLOBAL_lastx = vdevice%cpVx; GLOBAL_lasty = vdevice%cpVy
GLOBAL_drawn = DRAWN
end subroutine PPM_char1
integer function PPM_char(c)
character(len=1), intent(in) :: c
if (GLOBAL_font == 0) then
call PPM_char0(c)
else
call PPM_char1(c)
end if
PPM_char = 0
end function PPM_char
integer function PPM_string(s)
character(len=*), intent(in) :: s
integer :: i, idum
if (GLOBAL_lastx /= vdevice%cpVx .or. GLOBAL_lasty /= vdevice%cpVy) then
GLOBAL_lastx = vdevice%cpVx; GLOBAL_lasty = vdevice%cpVy
end if
do i = 1, len(s)
idum = PPM_char(s(i:i))
end do
PPM_string = 0
end function PPM_string
integer function noop(); noop = -1; end function
integer function noop2(x, y); integer, intent(out) :: x, y; x = 0; y = 0; noop2 = -1; end function
subroutine setup_ppmdev()
vdevice%dev%devname = "ppm"
vdevice%dev%large = "large"
vdevice%dev%small = "small"
vdevice%dev%Vbackb => noop
vdevice%dev%Vchar => PPM_char
vdevice%dev%Vcheckkey => noop
vdevice%dev%Vclear => PPM_clear
vdevice%dev%Vcolor => PPM_color
vdevice%dev%Vdraw => PPM_draw
vdevice%dev%Vexit => PPM_exit
vdevice%dev%Vfill => PPM_fill
vdevice%dev%Vfont => PPM_font
vdevice%dev%Vfrontb => noop
vdevice%dev%Vgetkey => noop
vdevice%dev%Vinit => PPM_init
vdevice%dev%Vlocator => noop2
vdevice%dev%Vmapcolor => PPM_mapcolor
vdevice%dev%Vsetlw => PPM_setlw
vdevice%dev%Vstring => PPM_string
vdevice%dev%Vswapb => noop
vdevice%dev%Vsync => noop
end subroutine setup_ppmdev
integer function PPM_draw_devcpy(); call setup_ppmdev(); vdevice%dev%devname = "ppm"; GLOBAL_driver = P3; PPM_draw_devcpy = 0; end function
integer function P3_draw_devcpy(); call setup_ppmdev(); vdevice%dev%devname = "p3"; GLOBAL_driver = P3; P3_draw_devcpy = 0; end function
integer function P6_draw_devcpy(); call setup_ppmdev(); vdevice%dev%devname = "p6"; GLOBAL_driver = P6; P6_draw_devcpy = 0; end function
integer function CHAR_draw_devcpy(); call setup_ppmdev(); vdevice%dev%devname = "char"; GLOBAL_driver = CHAR_MODE; CHAR_draw_devcpy = 0; end function
end module m_draw_drv_ppm
