module m_draw_drv_ps
   use, intrinsic :: iso_fortran_env, only: real32, real64, output_unit, error_unit
   use m_draw_types
   use m_draw_pref
   use m_draw_drivers
   implicit none
   private
   public :: PS_draw_devcpy, PSP_draw_devcpy, PSC_draw_devcpy, &
             PPSC_draw_devcpy, PSG_draw_devcpy, PPSG_draw_devcpy

   integer, parameter :: MAXPOINTS = 1000
   integer, parameter :: CMAPSIZE  = 8192

   type :: rgb_color
      integer :: red = 255, green = 255, blue = 255
   end type rgb_color

   type(rgb_color), save :: ps_carr(CMAPSIZE)

   logical, save :: ps_first_time = .true.
   logical, save :: polyopen      = .false.
   integer, save :: drawn           = 0
   integer, save :: curcol          = 0
   integer, save :: curback         = 0
   integer, save :: curwidth        = 2
   integer, save :: curwidthwritten = -999
   integer, save :: points          = 0
   integer, save :: pslstx = -1, pslsty = -1
   integer, save :: Page_Count      = 0

   character(len=30), save :: hardfont = "Courier"

   ! 1-based grayscale map (indices 1..8 map to color indices 0..7)
   real(real32), parameter :: graymap(8) = [ &
      1.00_real32, 0.30_real32, 0.59_real32, 0.89_real32, &
      0.11_real32, 0.41_real32, 0.70_real32, 0.00_real32 ]

   integer, parameter :: helvetica_w(128) = [ &
      0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0, &
      27,33,43,55,55,88,72,28,33,33,39,58,27,33,28,28,55,55,55,55,55,55,55,55,55,55, &
      34,34,58,58,58,61,97, &
      73,73,73,73,66,61,78,72,28,55,73,61,83,72,78,66,78,72,67,61,72,66,94,66,66,61, &
      33,28,33,58,55,28, &
      55,61,55,61,55,33,61,61,28,28,55,28,88,61,61,61,61,39,55,33,61,55,78,55,55,50, &
      39,28,39,58,0 ]

contains

   subroutine PS_header(xmin, ymin, xmax, ymax, xoff, yoff)
      integer, intent(in) :: xmin, ymin, xmax, ymax, xoff, yoff
      real(real32) :: a
      character(len=8)  :: dt
      character(len=10) :: tm

      call date_and_time(date=dt, time=tm)
      a = 72.0_real32 / 300.0_real32

      write(draw_unit, '(A)') "%!PS-Adobe-1.0"
      write(draw_unit, '(A)') "%%DocumentFonts: Courier Helvetica-Bold"
      write(draw_unit, '(A)') "%%Creator: M_DRAW PostScript driver 3.0"
      write(draw_unit, '(A)') "%%Title: M_DRAW"
      write(draw_unit, '(A, 4(1X, I0))') "%%BoundingBox:", &
         int(real(xmin + xoff, real32) * a), int(real(ymin + yoff, real32) * a), &
         int(real(xmax + xoff, real32) * a), int(real(ymax + yoff, real32) * a)
      write(draw_unit, '(A)') "%%Pages: (atend)"
      write(draw_unit, '(A, A, "-", A, "-", A, " ", A, ":", A, ":", A)') &
         "%%CreationDate: ", dt(1:4), dt(5:6), dt(7:8), tm(1:2), tm(3:4), tm(5:6)
   end subroutine PS_header

   integer function PS_mapcolor(i, r, g, b)
      integer, intent(in) :: i, r, g, b
      if (i >= CMAPSIZE .or. i < 0) then
         PS_mapcolor = -1; return
      end if
      ps_carr(i + 1)%red   = r
      ps_carr(i + 1)%green = g
      ps_carr(i + 1)%blue  = b
      PS_mapcolor = 0
   end function PS_mapcolor

   integer function PS_common_init()
      integer :: idum

      write(draw_unit, '(A)') "2 setlinewidth"
      write(draw_unit, '(A)') "1 setlinejoin"
      write(draw_unit, '(A)') "1 setlinecap"
      write(draw_unit, '(A)') "/sf /Courier findfont def"
      write(draw_unit, '(A)') "/m /moveto load def"
      write(draw_unit, '(A)') "/B /moveto load def"
      write(draw_unit, '(A)') "/E /stroke load def"
      write(draw_unit, '(A)') "/d { lineto } def"
      write(draw_unit, '(A)') "/p /lineto load def"
      write(draw_unit, '(A)') "/h { sf exch scalefont setfont } def"
      write(draw_unit, '(A)') "/s /show load def"
      write(draw_unit, '(A)') "/g /setgray load def"
      write(draw_unit, '(A)') "/RJ { stringwidth neg exch neg exch rmoveto } bind def"
      write(draw_unit, '(A)') "/CJ { stringwidth 2 div neg exch 2 div neg exch rmoveto } bind def"
      write(draw_unit, '(A)') "45 h"
      write(draw_unit, '(A)') "%%EndProlog"

      drawn = 0
      ps_carr = rgb_color(255, 255, 255)

      idum = PS_mapcolor(0, 255, 255, 255)
      idum = PS_mapcolor(1, 255,   0,   0)
      idum = PS_mapcolor(2,   0, 255,   0)
      idum = PS_mapcolor(3, 255, 255,   0)
      idum = PS_mapcolor(4,   0,   0, 255)
      idum = PS_mapcolor(5, 255,   0, 255)
      idum = PS_mapcolor(6,   0, 255, 255)
      idum = PS_mapcolor(7,   0,   0,   0)
      idum = PS_mapcolor(8, 155,   0,   0)
      idum = PS_mapcolor(9,   0, 155,   0)
      idum = PS_mapcolor(10, 155, 255, 255)
      idum = PS_mapcolor(11, 155, 155,   0)
      idum = PS_mapcolor(12,   0,   0, 155)
      idum = PS_mapcolor(13, 155,   0, 155)
      idum = PS_mapcolor(14,   0, 155, 155)
      idum = PS_mapcolor(15, 100, 100, 100)

      vdevice%hwidth  = 11.0_real32 * 300.0_real32 / 72.0_real32
      vdevice%hheight = 11.0_real32 * 300.0_real32 / 72.0_real32

      polyopen   = .false.
      Page_Count = 0
      PS_common_init = 1
   end function PS_common_init

   integer function PS_init()
      integer :: prefx, prefy, prefxs, prefys
      draw_unit = draw_outfile()
      if (.not. ps_first_time) then
         PS_init = 1; return
      end if

      call draw_getprefposandsize(prefx, prefy, prefxs, prefys)
      if (prefxs /= -1) then
         vdevice%sizeSy = prefys
         vdevice%sizeSx = prefxs
         vdevice%sizeX  = min(prefys, prefxs); vdevice%sizeY = vdevice%sizeX
      else
         prefx = 150; prefy = 150
         vdevice%sizeSy = 2250
         vdevice%sizeSx = 3000
         vdevice%sizeX  = 2250; vdevice%sizeY = 2250
      end if

      call PS_header(0, 0, vdevice%sizeSy, vdevice%sizeSx, prefy, prefx)
      write(draw_unit, '(A)') "%%Orientation: Landscape"
      write(draw_unit, '(A)') "%%EndComments"
      write(draw_unit, '(A)') "%%BeginProlog"
      write(draw_unit, '(A)') "save"
      write(draw_unit, '(A, I0, 1X, I0, A)') "72 300 div dup scale 90 rotate ", prefx, 0 - vdevice%sizeSy - prefy, " translate"

      PS_init = PS_common_init()
   end function PS_init

   integer function PSP_init()
      integer :: prefx, prefy, prefxs, prefys
      draw_unit = draw_outfile()
      if (.not. ps_first_time) then
         PSP_init = 1; return
      end if

      call draw_getprefposandsize(prefx, prefy, prefxs, prefys)
      if (prefxs /= -1) then
         vdevice%sizeSy = prefys
         vdevice%sizeSx = prefxs
         vdevice%sizeX  = min(prefys, prefxs); vdevice%sizeY = vdevice%sizeX
      else
         prefx = 150; prefy = 150
         vdevice%sizeSy = 3000
         vdevice%sizeSx = 2250
         vdevice%sizeX  = 2250; vdevice%sizeY = 2250
      end if

      call PS_header(0, 0, vdevice%sizeSx, vdevice%sizeSy, prefx, prefy)
      write(draw_unit, '(A)') "%%Orientation: Portrait"
      write(draw_unit, '(A)') "%%EndComments"
      write(draw_unit, '(A)') "%%BeginProlog"
      write(draw_unit, '(A)') "save"
      write(draw_unit, '(A, I0, 1X, I0, A)') "72 300 div dup scale ", prefx, prefy, " translate"

      PSP_init = PS_common_init()
   end function PSP_init

   subroutine closeline()
      if (polyopen) then
         if (mod(points, 10) /= 0) write(draw_unit, *)
         write(draw_unit, '(A, I0)') "E %PTS=", points
         polyopen = .false.
         points = 0
      end if
   end subroutine closeline

   integer function PS_setlw(w)
      integer, intent(in) :: w
      integer :: line_width
      curwidth = w
      if (curwidthwritten /= w) then
         line_width = abs(w) - 1
         line_width = int(real(line_width, real32) * real(vdevice%sizeX, real32) / 10000.0_real32)
         line_width = max(1, line_width)
         call closeline()
         if (drawn /= 0) then
            write(draw_unit, '(I0, A)') line_width, " setlinewidth"
            curwidthwritten = w
         end if
      end if
      PS_setlw = 0
   end function PS_setlw

   recursive integer function PS_color(col)
      integer, intent(in) :: col
      integer :: idum

      call closeline()

      if (col < 0) then
         idum = PS_setlw(abs(col))
         PS_color = 0; return
      else if (drawn == 0) then
         curcol = col
         PS_color = 0; return
      else if (vdevice%depth == 1) then
         if (col == curback) then
            write(draw_unit, '(A)') " 1 g"
         else
            write(draw_unit, '(A)') " 0 g"
         end if
      else if (vdevice%depth == 3) then
         write(draw_unit, '(F4.2, A)') graymap(mod(col, 8) + 1), " g"
      else
         if (col >= CMAPSIZE) then
            PS_color = 0; return
         end if
         write(draw_unit, '(3(F0.6, 1X), A)') &
            ps_carr(col + 1)%red   / 255.0_real32, &
            ps_carr(col + 1)%green / 255.0_real32, &
            ps_carr(col + 1)%blue  / 255.0_real32, "setrgbcolor"
      end if
      curcol = col
      PS_color = 0
   end function PS_color

   recursive subroutine iffirst(ii)
      integer, intent(in) :: ii
      integer :: idum
      if (drawn == 0) then
         Page_Count = Page_Count + 1
         write(draw_unit, '(A, I0, 1X, I0)') "%%Page: ", Page_Count, Page_Count
         write(draw_unit, '(A, I0)') "save % ", ii
         drawn = 1
         idum = PS_color(curcol)
         idum = PS_setlw(curwidth)
         if (ii /= 3) idum = PS_string("")
      end if
   end subroutine iffirst

   subroutine openline()
      integer :: idum
      if (.not. polyopen) then
         idum = PS_setlw(curwidth)
         write(draw_unit, '(I0, 1X, I0, A)') vdevice%cpVx, vdevice%cpVy, " B"
         polyopen = .true.
      end if
   end subroutine openline

   integer function PS_exit()
      call closeline()
      if (drawn /= 0) then
         write(draw_unit, '(A)') "showpage"
         write(draw_unit, '(A)') "restore"
         flush(draw_unit)
      else
         drawn = 0
      end if
      write(draw_unit, '(A)') "%%Trailer"
      write(draw_unit, '(A, I0)') "%%Pages: ", Page_Count
      write(draw_unit, '(A)') "restore"
      write(draw_unit, '(A)') "%%EOF"

if (draw_unit /= output_unit .and. draw_unit /= error_unit) then
flush(draw_unit)
close(draw_unit)
end if
PS_exit = 0
end function PS_exit
integer function PS_draw(x, y)
integer, intent(in) :: x, y
call iffirst(1)
if (pslstx /= vdevice%cpVx .or. pslsty /= vdevice%cpVy .or. points >= MAXPOINTS) then
call closeline()
call openline()
points = 1
end if
call openline()
if (mod(points, 10) == 9) then
write(draw_unit, '(I0, 1X, I0, A)') x, y, " d"
else
write(draw_unit, '(I0, 1X, I0, A)', advance='no') x, y, " d "
end if
points = points + 1
pslstx = x; pslsty = y
PS_draw = 0
end function PS_draw
integer function PS_font(fontname)
character(len=*), intent(in) :: fontname
real(real32) :: rat
vdevice%attr%a%softtext = SOFTHARDWARE
if (vdevice%hwidth == 0.0_real32 .or. vdevice%hheight == 0.0_real32) then
vdevice%hwidth = 11.0_real32 * 300.0_real32 / 72.0_real32
vdevice%hheight = 11.0_real32 * 300.0_real32 / 72.0_real32
end if
if (trim(fontname) == "small") then
hardfont = "Courier"
rat = 0.60_real32
hardwidth(1:128) = 1.00_real32 * rat
else if (trim(fontname) == "large") then
hardfont = "Helvetica-Bold"
rat = 1.00_real32
hardwidth(1:128) = (real(helvetica_w(1:128), real32) / 100.0_real32) * rat
else
PS_font = 0; return
end if
PS_font = 1
end function PS_font
integer function PS_clear()
if (drawn /= 0) then
call closeline()
write(draw_unit, '(A)') "showpage restore"
end if
points = 0; drawn = 0; curback = curcol
PS_clear = 0
end function PS_clear
function stripspecial(s) result(stmp)
character(len=*), intent(in) :: s
character(len=:), allocatable :: stmp
integer :: k
character(len=1) :: c
stmp = ""
do k = 1, len(s)
c = s(k:k)
if (c == '(' .or. c == ')' .or. c == '\') then
stmp = stmp // '\' // c
else
stmp = stmp // c
end if
end do
end function stripspecial
subroutine DISPSTR(x, y, rot, s, just, szw, szh)
integer, intent(in) :: x, y, just
real(real32), intent(in) :: rot, szw, szh
character(len=*), intent(in) :: s
character(len=:), allocatable :: tmpstr
write(draw_unit, '(A, A, A, F0.6, A, F0.6, A)') &
"/", trim(hardfont), " findfont [ ", szw, " 0 0 ", szh, " 0 0] makefont setfont % scale the font"
if (len(s) == 0) return
write(draw_unit, '(A)') "gsave"
write(draw_unit, '(I0, 1X, I0, A)') x, int(real(y, real32) + 0.22_real32 * szh), " translate"
write(draw_unit, '(F0.6, A)') rot, " rotate"
write(draw_unit, '(A)') "0 0 m % Transformed text location"
tmpstr = stripspecial(s)
select case (just)
case (0)
write(draw_unit, '(A, A, A)') "(", tmpstr, ") s"
case (1)
write(draw_unit, '(A, A, A)') "(", tmpstr, ") RJ s"
case (2)
write(draw_unit, '(A, A, A)') "(", tmpstr, ") CS s"
end select
write(draw_unit, '(A)') "grestore"
end subroutine DISPSTR
recursive integer function PS_string(s)
character(len=*), intent(in) :: s
real(real32) :: rot
call closeline()
call iffirst(3)
rot = real(atan2(real(vdevice%attr%a%textsin, real64), real(vdevice%attr%a%textcos, real64)) * 180.0_real64 / PI, real32)
if (vdevice%hwidth == 0.0_real32 .or. vdevice%hheight == 0.0_real32) then
write(error_unit, '(A)') "PS_string ERROR: ZERO SIZE CHARACTERS"
vdevice%hwidth = 11.0_real32 * 300.0_real32 / 72.0_real32
vdevice%hheight = 11.0_real32 * 300.0_real32 / 72.0_real32
end if
call DISPSTR(vdevice%cpVx, vdevice%cpVy, rot, s, 0, vdevice%hwidth, vdevice%hheight)
pslstx = -1; pslsty = -1
PS_string = 0
end function PS_string
integer function PS_char(c)
character(len=1), intent(in) :: c
integer :: idum
idum = PS_string(c)
PS_char = 0
end function PS_char
integer function PS_fill(n, x, y)
integer, intent(in) :: n
integer, intent(in) :: x(n), y(n)
integer :: i
call closeline()
call iffirst(4)
write(draw_unit, '(A, I0, 1X, I0, A)') " newpath ", x(1), y(1), " m % Fill a polygon"
do i = 2, n
if (mod(i - 1, 8) == 7) then
write(draw_unit, '(1X, I0, 1X, I0, A)') x(i), y(i), " p"
else
write(draw_unit, '(1X, I0, 1X, I0, A)', advance='no') x(i), y(i), " p "
end if
end do
write(draw_unit, '(A)') "closepath"
if (vdevice%depth == 1) then
if (curcol == curback) then
write(draw_unit, '(A)') "1 g"
else
write(draw_unit, '(A)') "0 g"
end if
else if (vdevice%depth == 3) then
write(draw_unit, '(F4.2, A)') graymap(mod(curcol, 8) + 1), " g"
else if (vdevice%depth > 1 .and. curcol < CMAPSIZE) then
write(draw_unit, '(3(F0.6, 1X), A)') &
ps_carr(curcol + 1)%red / 255.0_real32, &
ps_carr(curcol + 1)%green / 255.0_real32, &
ps_carr(curcol + 1)%blue / 255.0_real32, "setrgbcolor"
end if
write(draw_unit, '(A)') "eofill"
vdevice%cpVx = x(n); vdevice%cpVy = y(n)
pslstx = -1; pslsty = -1
PS_fill = 0
end function PS_fill
integer function noop(); noop = -1; end function
integer function noop2(x, y); integer, intent(out) :: x, y; x = 0; y = 0; noop2 = -1; end function
subroutine setup_psdev()
vdevice%dev%devname = "postscript"
vdevice%dev%large = "large"
vdevice%dev%small = "small"
vdevice%dev%Vbackb => noop
vdevice%dev%Vchar => PS_char
vdevice%dev%Vcheckkey => noop
vdevice%dev%Vclear => PS_clear
vdevice%dev%Vcolor => PS_color
vdevice%dev%Vdraw => PS_draw
vdevice%dev%Vexit => PS_exit
vdevice%dev%Vfill => PS_fill
vdevice%dev%Vfont => PS_font
vdevice%dev%Vfrontb => noop
vdevice%dev%Vgetkey => noop
vdevice%dev%Vinit => PS_init
vdevice%dev%Vlocator => noop2
vdevice%dev%Vmapcolor => PS_mapcolor
vdevice%dev%Vsetlw => PS_setlw
vdevice%dev%Vstring => PS_string
vdevice%dev%Vswapb => noop
vdevice%dev%Vsync => noop
end subroutine setup_psdev
integer function PS_draw_devcpy()
call setup_psdev(); vdevice%dev%Vinit => PS_init; vdevice%depth = 1; PS_draw_devcpy = 0
end function PS_draw_devcpy
integer function PSP_draw_devcpy()
call setup_psdev(); vdevice%dev%Vinit => PSP_init; vdevice%depth = 1; PSP_draw_devcpy = 0
end function PSP_draw_devcpy
integer function PSC_draw_devcpy()
call setup_psdev(); vdevice%dev%Vinit => PS_init; vdevice%depth = 13; PSC_draw_devcpy = 0
end function PSC_draw_devcpy
integer function PPSC_draw_devcpy()
call setup_psdev(); vdevice%dev%Vinit => PSP_init; vdevice%depth = 13; PPSC_draw_devcpy = 0
end function PPSC_draw_devcpy
integer function PSG_draw_devcpy()
call setup_psdev(); vdevice%dev%Vinit => PS_init; vdevice%depth = 3; PSG_draw_devcpy = 0
end function PSG_draw_devcpy
integer function PPSG_draw_devcpy()
call setup_psdev(); vdevice%dev%Vinit => PSP_init; vdevice%depth = 3; PPSG_draw_devcpy = 0
end function PPSG_draw_devcpy
end module m_draw_drv_ps
