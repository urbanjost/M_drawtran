module m_draw_drv_pbm
   use, intrinsic :: iso_fortran_env, only: real32, real64, int32, output_unit, error_unit
   use m_draw_types
   use m_draw_pref
   use m_draw_drivers
   use m_draw_bitmap_fonts
   implicit none
   private
   public :: PBM_draw_devcpy, BM_draw_devcpy, P1_draw_devcpy, &
             XBM_draw_devcpy, P4_draw_devcpy

   integer, parameter :: XBM   = 1
   integer, parameter :: P1    = 2
   integer, parameter :: P4    = 3
   integer, parameter :: BM    = 4
   integer, parameter :: CHARS = 5

   integer, parameter :: UNDRAWN = 0
   integer, parameter :: DRAWN   = 1

   integer, save :: X_SIZE = 0, Y_SIZE = 0, LAST = 0
   integer, allocatable, save :: graphics(:) ! 1-based byte array (0..255 stored in int)

   integer, save :: GLOBAL_driver     = 0
   integer, save :: GLOBAL_font       = 0
   integer, save :: GLOBAL_color      = 0
   integer, save :: GLOBAL_background = 0
   integer, save :: BITVALUE          = 0
   integer, save :: GLOBAL_drawn      = UNDRAWN
   integer, save :: GLOBAL_rasters    = 1
   integer, save :: GLOBAL_lastx      = -1111111
   integer, save :: GLOBAL_lasty      = -1111111

   character(len=1), parameter :: hexchar(16) = [ &
      '0','8','4','c','2','a','6','e','1','9','5','d','3','b','7','f' ]

contains

   subroutine SET_BIT(x, y)
      integer, intent(in) :: x, y
      integer :: idx, mask, clamped_idx
      idx = (x / 8) * Y_SIZE + y + 1
      clamped_idx = max(1, min(LAST, idx))
      mask = ishft(128, -mod(x, 8))
      if (BITVALUE /= 0) then
         graphics(clamped_idx) = ior(graphics(clamped_idx), mask)
      else
         graphics(clamped_idx) = iand(graphics(clamped_idx), not(mask))
      end if
   end subroutine SET_BIT

   logical function GET_BIT(x, y)
      integer, intent(in) :: x, y
      integer :: idx, mask
      idx = (x / 8) * Y_SIZE + y + 1
      if (idx < 1 .or. idx > LAST) then
         GET_BIT = .false.; return
      end if
      mask = ishft(128, -mod(x, 8))
      GET_BIT = (iand(graphics(idx), mask) /= 0)
   end function GET_BIT

   subroutine PBM_MEMSET()
      if (allocated(graphics)) graphics = 0
   end subroutine PBM_MEMSET

   integer function PBM_init()
      integer :: prefx, prefy, prefxs, prefys

      call draw_getprefposandsize(prefx, prefy, prefxs, prefys)
      if (prefxs /= -1) then
         if (prefys <= 0) then
            write(error_unit, '(A, I0, A)') "*PBM_init* y size of ", prefys, " set to 400"
            prefys = 400
         end if
         vdevice%sizeSy = prefys

         if (prefxs <= 0) then
            write(error_unit, '(A, I0, A)') "*PBM_init* x size of ", prefxs, " set to 600"
            prefxs = 600
         end if
         vdevice%sizeSx = prefxs
      else
         prefx = 0; prefy = 0
         vdevice%sizeSy = 400; vdevice%sizeSx = 600
      end if

      vdevice%sizeX = min(vdevice%sizeSy, vdevice%sizeSx)
      vdevice%sizeY = vdevice%sizeX
      X_SIZE = vdevice%sizeSx
      Y_SIZE = vdevice%sizeSy
      LAST   = ((X_SIZE + 7) / 8) * Y_SIZE

      if (allocated(graphics)) deallocate(graphics)
      allocate(graphics(LAST))
      call PBM_MEMSET()

      vdevice%depth = 1
      draw_unit = draw_outfile()
      GLOBAL_lastx = -1111111; GLOBAL_lasty = -1111111
      GLOBAL_drawn = UNDRAWN
      PBM_init = 1
   end function PBM_init

   subroutine PBM_DRAW_LINE(x, y)
      integer, intent(in) :: x, y
      integer :: runcount, dx, dy, xinc, yinc, xplot, yplot

      call SET_BIT(GLOBAL_lastx, GLOBAL_lasty)
      runcount = 0
      dx = abs(GLOBAL_lastx - x)
      xinc = 0
      if (x > GLOBAL_lastx) xinc = 1
      if (x < GLOBAL_lastx) xinc = -1

      dy = abs(GLOBAL_lasty - y)
      yinc = 0
      if (y > GLOBAL_lasty) yinc = 1
      if (y < GLOBAL_lasty) yinc = -1

      xplot = GLOBAL_lastx
      yplot = GLOBAL_lasty

      if (dx > dy) then
         do while (xplot /= x)
            xplot = xplot + xinc
            runcount = runcount + dy
            if (runcount >= (dx - runcount)) then
               yplot = yplot + yinc
               runcount = runcount - dx
            end if
            call SET_BIT(xplot, yplot)
         end do
      else
         do while (yplot /= y)
            yplot = yplot + yinc
            runcount = runcount + dx
            if (runcount >= (dy - runcount)) then
               xplot = xplot + xinc
               runcount = runcount - dy
            end if
            call SET_BIT(xplot, yplot)
         end do
      end if

      GLOBAL_lastx = xplot
      GLOBAL_lasty = yplot
   end subroutine PBM_DRAW_LINE

   subroutine PBM_ENDCAP_CIRCLE(x, y)
      integer, intent(in) :: x, y
      integer, parameter :: nsegs = 15
      real(real32) :: cx, cy, dx, dy, angle, cosine, sine
      integer :: cxras(nsegs), cyras(nsegs), i

      angle  = 2.0_real32 * PI / real(nsegs, real32)
      cosine = cos(angle)
      sine   = sin(angle)

      cx = real(x, real32) + real(GLOBAL_rasters, real32) / 2.0_real32
      cy = real(y, real32)
      cxras(1) = int(cx)
      cyras(1) = int(cy)
      do i = 2, nsegs
         dx = cx - real(x, real32)
         dy = cy - real(y, real32)
         cx = real(x, real32) + dx * cosine - dy * sine
         cy = real(y, real32) + dx * sine   + dy * cosine
         cxras(i) = int(cx)
         cyras(i) = int(cy)
      end do
      call PBM_SOLID_FILL(nsegs, cxras, cyras)
   end subroutine PBM_ENDCAP_CIRCLE

   integer function PBM_fill(n, x, y)
      integer, intent(in) :: n
      integer, intent(in) :: x(n), y(n)
      integer :: i

      GLOBAL_lastx = x(1); GLOBAL_lasty = y(1)
      do i = 2, n
         call PBM_DRAW_LINE(x(i), y(i))
      end do
      if (x(n) /= x(1) .or. y(n) /= y(1)) then
         call PBM_DRAW_LINE(x(1), y(1))
      end if

      call PBM_SOLID_FILL(n, x, y)

      GLOBAL_lastx = x(n); vdevice%cpVx = x(n)
      GLOBAL_lasty = y(n); vdevice%cpVy = y(n)
      GLOBAL_drawn = DRAWN
      PBM_fill = 0
   end function PBM_fill

   integer function PBM_draw(x, y)
      integer, intent(in) :: x, y
      integer :: holdx, holdy, xwide(4), ywide(4)
      real(real32) :: cosa, sina
      real(real64) :: angle

      if (GLOBAL_lastx /= vdevice%cpVx .or. GLOBAL_lasty /= vdevice%cpVy) then
         GLOBAL_lastx = vdevice%cpVx
         GLOBAL_lasty = vdevice%cpVy
      end if

      if (GLOBAL_rasters <= 1) then
         call PBM_DRAW_LINE(x, y)
      else
         if (GLOBAL_rasters >= 6) then
            holdx = GLOBAL_lastx; holdy = GLOBAL_lasty
            call PBM_ENDCAP_CIRCLE(GLOBAL_lastx, GLOBAL_lasty)
            call PBM_ENDCAP_CIRCLE(x, y)
            GLOBAL_lastx = holdx; GLOBAL_lasty = holdy
         end if
         angle = atan2(real(y - GLOBAL_lasty, real64), real(x - GLOBAL_lastx, real64)) + real(PI, real64) / 2.0_real64
         cosa = (real(GLOBAL_rasters, real32) / 2.0_real32) * real(cos(angle), real32)
         sina = (real(GLOBAL_rasters, real32) / 2.0_real32) * real(sin(angle), real32)

         xwide(1) = int(real(x, real32) + cosa)
         xwide(2) = int(real(GLOBAL_lastx, real32) + cosa)
         xwide(3) = int(real(GLOBAL_lastx, real32) - cosa)
         xwide(4) = int(real(x, real32) - cosa)

         ywide(1) = int(real(y, real32) + sina)
         ywide(2) = int(real(GLOBAL_lasty, real32) + sina)
         ywide(3) = int(real(GLOBAL_lasty, real32) - sina)
         ywide(4) = int(real(y, real32) - sina)

         call PBM_SOLID_FILL(4, xwide, ywide)
      end if
      GLOBAL_drawn = DRAWN
      PBM_draw = 0
   end function PBM_draw

   subroutine P1_print_graphics()
      integer :: x, y, bit_mask, column, idx, write_column
      character(len=8)  :: dt
      character(len=10) :: tm

      call date_and_time(date=dt, time=tm)
      write(draw_unit, '(A)') "P1"
      write(draw_unit, '(A)') "# CREATOR: M_DRAW pbm driver; version 2.0 1996/12/07"
      write(draw_unit, '(A)') "# V1: 1995, John S. Urban"
      write(draw_unit, '(A, A, "-", A, "-", A, " ", A, ":", A, ":", A)') &
         "# CreationDate: ", dt(1:4), dt(5:6), dt(7:8), tm(1:2), tm(3:4), tm(5:6)
      write(draw_unit, '(I0, 1X, I0)') X_SIZE, Y_SIZE

      write_column = 1
      do y = Y_SIZE - 1, 0, -1
         column = 1
         do x = 0, (X_SIZE + 7) / 8 - 1
            bit_mask = 128
            do while (bit_mask > 0)
               if (column > X_SIZE) exit
               idx = Y_SIZE * x + y + 1
               if (iand(graphics(idx), bit_mask) /= 0) then
                  write(draw_unit, '(A)', advance='no') "1"
               else
                  write(draw_unit, '(A)', advance='no') "0"
               end if
               column = column + 1
               write_column = write_column + 1
               if (write_column > 70) then
                  write(draw_unit, *)
                  write_column = 1
               end if
               bit_mask = ishft(bit_mask, -1)
            end do
         end do
      end do
      write(draw_unit, *)
      GLOBAL_drawn = UNDRAWN
   end subroutine P1_print_graphics

   subroutine P4_print_graphics()
      integer :: x, y, idx
      character(len=8)  :: dt
      character(len=10) :: tm

      call date_and_time(date=dt, time=tm)
      write(draw_unit, '(A)') "P4"
      write(draw_unit, '(A)') "# CREATOR: M_DRAW pbm driver; version 2.0 1996/12/07"
      write(draw_unit, '(A)') "# V1: 1995, John S. Urban"
      write(draw_unit, '(A, A, "-", A, "-", A, " ", A, ":", A, ":", A)') &
         "# CreationDate: ", dt(1:4), dt(5:6), dt(7:8), tm(1:2), tm(3:4), tm(5:6)
      write(draw_unit, '(I0, 1X, I0)') X_SIZE, Y_SIZE

      do y = Y_SIZE - 1, 0, -1
         do x = 0, (X_SIZE + 7) / 8 - 1
            idx = Y_SIZE * x + y + 1
            write(draw_unit, '(A1)', advance='no') char(iand(graphics(idx), 255))
         end do
      end do
      GLOBAL_drawn = UNDRAWN
   end subroutine P4_print_graphics

   subroutine BM_print_graphics()
      integer :: x, y
      do y = Y_SIZE - 1, 0, -1
         do x = 0, X_SIZE - 1
            if (GET_BIT(x, y)) then

write(draw_unit, '(A1)', advance='no') '#'
else
write(draw_unit, '(A1)', advance='no') '-'
end if
end do
write(draw_unit, *)
end do
GLOBAL_drawn = UNDRAWN
end subroutine BM_print_graphics
subroutine XBM_print_graphics()
integer :: x, y, print_column, nbytes, idx, item
write(draw_unit, '(A, I0)') "#define M_DRAW_width ", X_SIZE
write(draw_unit, '(A, I0)') "#define M_DRAW_height ", Y_SIZE
write(draw_unit, '(A)', advance='no') "static char M_DRAW_bits[] = {" // new_line('A') // " "
nbytes = Y_SIZE * ((X_SIZE + 7) / 8)
print_column = 1
outer: do y = Y_SIZE - 1, 0, -1
do x = 0, (X_SIZE + 7) / 8 - 1
idx = Y_SIZE * x + y + 1
if (nbytes == 1) then
write(draw_unit, '(A, Z2.2, /, A)') "0x", iand(graphics(idx), 255), "};"
exit outer
end if
item = iand(graphics(idx), 255)
write(draw_unit, '(A, A1, A1, A)', advance='no') &
"0x", hexchar(iand(item, 15) + 1), hexchar(ishft(item, -4) + 1), ","
print_column = print_column + 5
if (print_column > 75) then
write(draw_unit, '(/, A)', advance='no') " "
print_column = 1
end if
nbytes = nbytes - 1
end do
end do outer
GLOBAL_drawn = UNDRAWN
end subroutine XBM_print_graphics
subroutine PBM_PRINT()
if (GLOBAL_drawn /= 0) then
select case (GLOBAL_driver)
case (P1); call P1_print_graphics()
case (P4); call P4_print_graphics()
case (XBM); call XBM_print_graphics()
case (BM); call BM_print_graphics()
case default
write(error_unit, '(A)') "pbm driver: UNKNOWN DRIVER NAME"
call P1_print_graphics()
end select
end if
flush(draw_unit)
end subroutine PBM_PRINT
integer function PBM_exit()
call PBM_PRINT()
if (allocated(graphics)) deallocate(graphics)
if (draw_unit /= output_unit .and. draw_unit /= error_unit) then
flush(draw_unit)
close(draw_unit)
end if
PBM_exit = 0
end function PBM_exit
integer function PBM_setlw(w)
integer, intent(in) :: w
GLOBAL_rasters = max(1, w * vdevice%sizeX / 10000)
PBM_setlw = 0
end function PBM_setlw
integer function PBM_clear()
call PBM_PRINT()
call PBM_MEMSET()
GLOBAL_background = GLOBAL_color
PBM_clear = 0
end function PBM_clear
integer function PBM_font(fontname)
character(len=*), intent(in) :: fontname
if (trim(fontname) == "small") then
vdevice%hwidth = 8.00_real32; vdevice%hheight = 13.0_real32; GLOBAL_font = 0
else if (trim(fontname) == "large") then
vdevice%hwidth = 15.00_real32; vdevice%hheight = 39.00_real32; GLOBAL_font = 1
else
PBM_font = 0; return
end if
PBM_font = 1
end function PBM_font
subroutine PBM_char0(c)
character(len=1), intent(in) :: c
integer :: i, j, k, wrd
k = ichar(c) - 32 + 1
if (k < 1 .or. k > NUM_CHARS) return
do i = 1, 11
wrd = font8x13(k, i)
do j = 0, 7
if (iand(wrd, ishft(128, -j)) /= 0) then
call SET_BIT(j + vdevice%cpVx, vdevice%cpVy - (i - 1) + 13)
end if
end do
end do
vdevice%cpVx = vdevice%cpVx + 8
GLOBAL_lastx = vdevice%cpVx; GLOBAL_lasty = vdevice%cpVy
GLOBAL_drawn = DRAWN
end subroutine PBM_char0
subroutine PBM_char1(c)
character(len=1), intent(in) :: c
integer :: i, j, k, wrd
k = ichar(c) - 32 + 1
if (k < 1 .or. k > NUM_CHARS) return
do i = 1, 25
wrd = font13x25(k, i)
do j = 0, 12
if (iand(ishft(wrd, -j), 1) /= 0) then
call SET_BIT(j + vdevice%cpVx, vdevice%cpVy + (i - 1))
end if
end do
end do
vdevice%cpVx = vdevice%cpVx + 15
GLOBAL_lastx = vdevice%cpVx; GLOBAL_lasty = vdevice%cpVy
GLOBAL_drawn = DRAWN
end subroutine PBM_char1
integer function PBM_char(c)
character(len=1), intent(in) :: c
if (GLOBAL_font == 0) then
call PBM_char0(c)
else
call PBM_char1(c)
end if
PBM_char = 0
end function PBM_char
integer function PBM_string(s)
character(len=*), intent(in) :: s
integer :: i, idum
if (GLOBAL_lastx /= vdevice%cpVx .or. GLOBAL_lasty /= vdevice%cpVy) then
GLOBAL_lastx = vdevice%cpVx; GLOBAL_lasty = vdevice%cpVy
end if
do i = 1, len(s)
idum = PBM_char(s(i:i))
end do
PBM_string = 0
end function PBM_string
integer function PBM_color(col)
integer, intent(in) :: col
integer :: idum
if (col < 0) then
idum = PBM_setlw(abs(col))
else
GLOBAL_color = mod(abs(col), 8)
BITVALUE = merge(1, 0, GLOBAL_color /= GLOBAL_background)
end if
PBM_color = 0
end function PBM_color
integer function PBM_YINTERCEPT(yscan, x1, y1, x2, y2, xintercept, yprev)
integer, intent(in) :: yscan, x1, y1, x2, y2
integer, intent(out) :: xintercept
integer, intent(inout) :: yprev
integer :: deltay, yprevious
real(real32) :: t
yprevious = yprev
yprev = y1
deltay = y2 - y1
if (deltay == 0) then
yprev = yprevious
PBM_YINTERCEPT = 0; return
end if
t = real(yscan - y1, real32) / real(deltay, real32)
if (t > 0.0_real32 .and. t <= 1.0_real32) then
xintercept = int(real(x1, real32) + t * real(x2 - x1, real32) + 0.5_real32)
PBM_YINTERCEPT = 1; return
else if (t == 0.0_real32) then
xintercept = int(real(x1, real32) + t * real(x2 - x1, real32) + 0.5_real32)
if ((yprevious <= y1 .and. y2 <= y1) .or. (yprevious >= y1 .and. y2 >= y1)) then
PBM_YINTERCEPT = 1; return
else
PBM_YINTERCEPT = 0; return
end if
end if
PBM_YINTERCEPT = 0
end function PBM_YINTERCEPT
subroutine PBM_SOLID_FILL(n, x, y)
integer, intent(in) :: n
integer, intent(in) :: x(n), y(n)
integer :: i, j, yhorizontal, xint, tmp, ymin, ymax, xi(MAXVERTS), yprev
logical :: sorted
if (n > MAXVERTS) then
write(error_unit, '(A, I0, A)') "PBM_SOLID_FILL more than ", MAXVERTS, " vertices in a polygon"
return
end if
ymin = max(minval(y(1:n)), 0)
ymax = min(maxval(y(1:n)), Y_SIZE)
yhorizontal = ymax
do while (yhorizontal >= ymin)
j = 0
yprev = y(n)
do i = 1, n - 1
if (PBM_YINTERCEPT(yhorizontal, x(i), y(i), x(i+1), y(i+1), xint, yprev) /= 0) then
j = j + 1; xi(j) = xint
end if
end do
if (PBM_YINTERCEPT(yhorizontal, x(n), y(n), x(1), y(1), xint, yprev) /= 0) then
j = j + 1; xi(j) = xint
end if
if (2 * (j / 2) /= j) then
write(error_unit, '(A, I0)') "PBM_SOLID_FILL Internal error: odd number of intersection points: ", j
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
call PBM_DRAW_LINE(max(0, min(xi(i+1), X_SIZE)), yhorizontal)
end do
yhorizontal = yhorizontal - 1
end do
end subroutine PBM_SOLID_FILL
integer function noop(); noop = -1; end function
integer function noop2(x, y); integer, intent(out) :: x, y; x = 0; y = 0; noop2 = -1; end function
integer function noop4(a, b, c, d); integer, intent(in) :: a, b, c, d; noop4 = -1; end function
subroutine setup_pbmdev()
vdevice%dev%devname = "PBM"
vdevice%dev%large = "large"
vdevice%dev%small = "small"
vdevice%dev%Vbackb => noop
vdevice%dev%Vchar => PBM_char
vdevice%dev%Vcheckkey => noop
vdevice%dev%Vclear => PBM_clear
vdevice%dev%Vcolor => PBM_color
vdevice%dev%Vdraw => PBM_draw
vdevice%dev%Vexit => PBM_exit
vdevice%dev%Vfill => PBM_fill
vdevice%dev%Vfont => PBM_font
vdevice%dev%Vfrontb => noop
vdevice%dev%Vgetkey => noop
vdevice%dev%Vinit => PBM_init
vdevice%dev%Vlocator => noop2
vdevice%dev%Vmapcolor => noop4
vdevice%dev%Vsetlw => PBM_setlw
vdevice%dev%Vstring => PBM_string
vdevice%dev%Vswapb => noop
vdevice%dev%Vsync => noop
end subroutine setup_pbmdev
integer function PBM_draw_devcpy(); call setup_pbmdev(); vdevice%dev%devname = "pbm"; GLOBAL_driver = P1; PBM_draw_devcpy = 0; end function
integer function BM_draw_devcpy(); call setup_pbmdev(); vdevice%dev%devname = "bm"; GLOBAL_driver = BM; BM_draw_devcpy = 0; end function
integer function P1_draw_devcpy(); call setup_pbmdev(); vdevice%dev%devname = "p1"; GLOBAL_driver = P1; P1_draw_devcpy = 0; end function
integer function XBM_draw_devcpy(); call setup_pbmdev(); vdevice%dev%devname = "xbm"; GLOBAL_driver = XBM; XBM_draw_devcpy = 0; end function
integer function P4_draw_devcpy(); call setup_pbmdev(); vdevice%dev%devname = "p4"; GLOBAL_driver = P4; P4_draw_devcpy = 0; end function
end module m_draw_drv_pbm
