module m_draw_drv_pdf
   use, intrinsic :: iso_fortran_env, only: real32, real64, int64, output_unit, error_unit
   use m_draw_types
   use m_draw_pref
   use m_draw_drivers
   implicit none
   private
   public :: PDF_draw_devcpy

   integer, parameter :: PDF_YSIZE  = 792
   integer, parameter :: PDF_XSIZE  = 612
   integer, parameter :: RAS_PER_PT = 10
   integer, parameter :: CMAPSIZE   = 256

   type :: rgb_color
      integer :: red = 255, green = 255, blue = 255
   end type rgb_color

   type(rgb_color), save :: PDF_carr(CMAPSIZE)

   logical, save :: PDF_first_time = .true.
   logical, save :: PolyLineOpen   = .false.
   logical, save :: ObjectOpen     = .false.
   integer, save :: points_printed = 0
   integer, save :: drawn          = 0
   integer, save :: LAST_X = -1, LAST_Y = -1
   integer(int64), save :: stream_start = 0_int64
   integer, save :: PDF_MOVED      = 0
   integer, save :: curcol         = 0
   integer, save :: curwid         = 1
   integer, save :: curpat         = 0
   integer, save :: pgroup         = 1
   integer, save :: pplus          = 4

   real(real32), save :: page_dx     = 0.0_real32
   real(real32), save :: page_dy     = 0.0_real32
   real(real32), save :: page_angle  = 0.0_real32
   real(real32), save :: page_xscale = 1.0_real32 / RAS_PER_PT
   real(real32), save :: page_yscale = 1.0_real32 / RAS_PER_PT

   character(len=30), save :: hardfont = "/F1"

   integer, save :: num_xrefs = 0
   integer(int64), allocatable, save :: xrefs(:)

   integer, parameter :: helvetica_w(128) = [ &
        0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
        0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
      278,333,474,556,556,889,722,278,333,333,389,584,278,333,278,278, &
      556,556,556,556,556,556,556,556,556,556,333,333,584,584,584,611, &
      975,722,722,722,722,667,611,778,722,278,556,722,611,833,722,778, &
      667,778,722,667,611,722,667,944,667,667,611,333,278,333,584,556, &
      278,556,611,556,611,556,333,611,611,278,278,556,278,889,611,611, &
      611,611,389,556,333,611,556,778,556,556,500,389,280,389,584,278 ]

contains

   integer(int64) function get_stream_pos()
      integer(int64) :: pos
      flush(draw_unit)
      inquire(unit=draw_unit, pos=pos)
      if (pos > 0_int64) then
         get_stream_pos = pos - 1_int64
      else
         get_stream_pos = 0_int64
      end if
   end function get_stream_pos

   subroutine store_start_obj(id)
      integer, intent(in) :: id
      integer(int64), allocatable :: new_xrefs(:)
      integer :: delta, new_num_xrefs

      if (id > num_xrefs) then
         delta = num_xrefs / 5
         if (delta < 1000) delta = delta + 1000
         new_num_xrefs = max(id, num_xrefs + delta)
         allocate(new_xrefs(new_num_xrefs))
         new_xrefs = 0_int64
         if (allocated(xrefs)) then
            new_xrefs(1:num_xrefs) = xrefs(1:num_xrefs)
            deallocate(xrefs)
         end if
         call move_alloc(new_xrefs, xrefs)
         num_xrefs = new_num_xrefs
      end if
      xrefs(id) = get_stream_pos()
      write(draw_unit, '(I0, A)') id, " 0 obj"
   end subroutine store_start_obj

   subroutine PDF_header()
      character(len=8)  :: dt
      character(len=10) :: tm
      call date_and_time(date=dt, time=tm)

      write(draw_unit, '(A)') "%PDF-1.4"
      write(draw_unit, '(A, 4A1)') "%", char(128), char(129), char(130), char(131)
      write(draw_unit, '(A)') "% PDF: Adobe Portable Document Format"
      write(draw_unit, '(A)') "% Info Section"
      call store_start_obj(2)
      write(draw_unit, '(A)') "<</Producer(M_DRAW PDF driver 1.0 2005-04-01, John S. Urban)"
      write(draw_unit, '(A)') "  /Title(M_DRAW PDF plot)"
      write(draw_unit, '(A)') "  /Subject(graphic)"
      write(draw_unit, '(A, A, A, A, A, A, A)') &
         "  /CreationDate(D:", dt(1:8), tm(1:6), ")"
      write(draw_unit, '(A, A, A, A, A, A, A)') &
         "  /ModDate(D:", dt(1:8), tm(1:6), ")"
      write(draw_unit, '(A)') ")>>endobj"
   end subroutine PDF_header

   integer function PDF_mapcolor(i, r, g, b)
      integer, intent(in) :: i, r, g, b
      if (i >= CMAPSIZE .or. i < 0) then
         PDF_mapcolor = -1; return
      end if
      PDF_carr(i + 1)%red   = r
      PDF_carr(i + 1)%green = g
      PDF_carr(i + 1)%blue  = b
      PDF_mapcolor = 0
   end function PDF_mapcolor

   integer function PDF_init()
      integer :: prefx, prefy, prefxs, prefys, idum

      points_printed = 0; drawn = 0
      LAST_X = -1; LAST_Y = -1
      stream_start = 0_int64
      PolyLineOpen = .false.; ObjectOpen = .false.
      curcol = 0; curwid = 1; curpat = 0
      pgroup = 1; pplus = 4
      page_dx = 0.0_real32; page_dy = 0.0_real32; page_angle = 0.0_real32
      page_xscale = 1.0_real32 / RAS_PER_PT
      page_yscale = 1.0_real32 / RAS_PER_PT
      if (allocated(xrefs)) deallocate(xrefs)
      num_xrefs = 0

      draw_unit = draw_outfile()
      if (.not. PDF_first_time) then
         PDF_init = 1; return
      end if

      call PDF_header()
      call draw_getprefposandsize(prefx, prefy, prefxs, prefys)

      if (prefxs /= -1) then
         vdevice%sizeSy = prefys
         vdevice%sizeSx = prefxs
         vdevice%sizeX  = min(prefys, prefxs); vdevice%sizeY = vdevice%sizeX
      else
         vdevice%sizeSy = PDF_YSIZE * RAS_PER_PT
         vdevice%sizeSx = PDF_XSIZE * RAS_PER_PT
         vdevice%sizeX  = min(PDF_XSIZE * RAS_PER_PT, PDF_YSIZE * RAS_PER_PT)
         vdevice%sizeY  = vdevice%sizeX
      end if

      write(draw_unit, '(A, F0.6, A, F0.6, A)') &
         "% width=", real(vdevice%sizeSx, real32)/72.0_real32/RAS_PER_PT, &
         "in height=", real(vdevice%sizeSy, real32)/72.0_real32/RAS_PER_PT, "in "
      write(draw_unit, '(A, F0.6, A, F0.6, A)') &
         "% width=", real(vdevice%sizeSx, real32), "rasters height=", real(vdevice%sizeSy, real32), "rasters "
      write(draw_unit, '(A, I0)') "% create contents for page=", pgroup

      call store_start_obj((pgroup - 1) * 3 + pplus)
      write(draw_unit, '(A, I0, A)') "<< /Length ", (pgroup - 1) * 3 + pplus + 1, " 0 R>>% length follows stream"
      write(draw_unit, '(A)') "stream"
      stream_start = get_stream_pos()
      write(draw_unit, '(A)') "q"

      write(draw_unit, '(A, F0.6, 1X, F0.6, A)') "1 0 0 1 ", page_dx, page_dy, " cm % Translate"
      write(draw_unit, '(4(F0.6, 1X), A)') &
         cos(page_angle), sin(page_angle), -sin(page_angle), cos(page_angle), "0 0 cm % Rotate"
      write(draw_unit, '(F0.6, A, F0.6, A)') page_xscale, " 0 0 ", page_yscale, " 0 0 cm % Scale"
      write(draw_unit, '(A)') "1 j"
      write(draw_unit, '(A)') "1 J"
      write(draw_unit, '(I0, A)') curwid * vdevice%sizeX / 10000, " w"

      vdevice%depth = 8
      PDF_carr = rgb_color(255, 255, 255)

      idum = PDF_mapcolor(0, 255, 255, 255)
      idum = PDF_mapcolor(1, 255,   0,   0)
      idum = PDF_mapcolor(2,   0, 255,   0)
      idum = PDF_mapcolor(3, 255, 255,   0)
      idum = PDF_mapcolor(4,   0,   0, 255)
      idum = PDF_mapcolor(5, 255,   0, 255)
      idum = PDF_mapcolor(6,   0, 255, 255)
      idum = PDF_mapcolor(7,   0,   0,   0)
      idum = PDF_mapcolor(8, 155,   0,   0)
      idum = PDF_mapcolor(9,   0, 155,   0)
      idum = PDF_mapcolor(10, 155, 255, 255)
      idum = PDF_mapcolor(11, 155, 155,   0)
      idum = PDF_mapcolor(12,   0,   0, 155)
      idum = PDF_mapcolor(13, 155,   0, 155)
      idum = PDF_mapcolor(14,   0, 155, 155)
      idum = PDF_mapcolor(15, 100, 100, 100)

      PolyLineOpen = .false.; ObjectOpen = .false.
      curcol = 0; curwid = 1; curpat = 0; drawn = 0
      PDF_init = 1
   end function PDF_init

   subroutine closeline()
      if (PolyLineOpen) then
         write(draw_unit, '(A)') " S"
         PolyLineOpen = .false.
      end if
      points_printed = 0
      LAST_X = -1; LAST_Y = -1
   end subroutine closeline

   subroutine closeObject()
      if (ObjectOpen) then
         ObjectOpen = .false.
         points_printed = 0
      end if
   end subroutine closeObject

   subroutine openline()
      if (.not. ObjectOpen) ObjectOpen = .true.
      if (.not. PolyLineOpen) PolyLineOpen = .true.
   end subroutine openline

   subroutine openObject()
      if (.not. ObjectOpen) ObjectOpen = .true.
   end subroutine openObject

   integer function PDF_exit()
      integer :: i
      integer(int64) :: STARTXREF, POS

      call closeline()
      call closeObject()
      write(draw_unit, '(A)', advance='no') "Q"
      POS = get_stream_pos()
      write(draw_unit, '(/, A, /, A)') "endstream", "endobj"

      call store_start_obj((pgroup - 1) * 3 + pplus + 1)
      write(draw_unit, '(I0, A, I0, A, I0)') POS - stream_start, " endobj% size of stream, start=", stream_start, " end=", POS

      call store_start_obj((pgroup - 1) * 3 + pplus + 2)
      write(draw_unit, '(A, I0, 1X, I0, A)') "<</Type/Page/MediaBox[ 0 0 ", &
         vdevice%sizeSx / RAS_PER_PT, vdevice%sizeSy / RAS_PER_PT, " ]/Parent 3 0 R"
      write(draw_unit, '(A)') "/Resources <</ProcSet[/PDF/Text]/Font<<"
      write(draw_unit, '(A)') "   /F1<</Type/Font/Subtype/Type1/BaseFont/Courier/Encoding/WinAnsiEncoding"
      write(draw_unit, '(A)') " /FirstChar 32/LastChar 251/Widths["
      do i = 1, 13
         write(draw_unit, '(A)') " 600 600 600 600 600 600 600 600 600 600 600 600 600 600 600 600"
      end do
      write(draw_unit, '(A)') " 600 600 600 600 600 600 600 600 600 600 600 600]>>"
      write(draw_unit, '(A)') "   /F2<</Type/Font/Subtype/Type1/BaseFont/Helvetica/Encoding/WinAnsiEncoding"
      write(draw_unit, '(A)') " /FirstChar 32/LastChar 251/Widths["
      write(draw_unit, '(A)') " 278 333 474 556 556 889 722 278 333 333 389 584 278 333 278 278"
      write(draw_unit, '(A)') " 556 556 556 556 556 556 556 556 556 556 333 333 584 584 584 611"
      write(draw_unit, '(A)') " 975 722 722 722 722 667 611 778 722 278 556 722 611 833 722 778"

write(draw_unit, '(A)') " 667 778 722 667 611 722 667 944 667 667 611 333 278 333 584 556"
write(draw_unit, '(A)') " 278 556 611 556 611 556 333 611 611 278 278 556 278 889 611 611"
write(draw_unit, '(A)') " 611 611 389 556 333 611 556 778 556 556 500 389 280 389 584 278"
write(draw_unit, '(A)') " 278 278 278 278 278 278 278 278 278 278 278 278 278 278 278 278"
write(draw_unit, '(A)') " 278 278 278 278 278 278 278 278 278 278 278 278 278 278 278 278"
write(draw_unit, '(A)') " 278 333 556 556 167 556 556 556 556 238 500 556 333 333 611 611"
write(draw_unit, '(A)') " 278 556 556 556 278 278 556 350 278 500 500 556 1000 1000 278 611"
write(draw_unit, '(A)') " 278 333 333 333 333 333 333 333 333 278 333 333 278 333 333 333"
write(draw_unit, '(A)') " 1000 278 278 278 278 278 278 278 278 278 278 278 278 278 278 278"
write(draw_unit, '(A)') " 278 1000 278 370 278 278 278 278 611 778 1000 365 278 278 278 278"
write(draw_unit, '(A)') " 278 889 278 278 278 278 278 278 278 611 944 611]>>"
write(draw_unit, '(A, I0, A)') ">> >>/Contents ", (pgroup - 1) * 3 + pplus, " 0 R>> endobj"
call store_start_obj(3)
write(draw_unit, '(A, I0)') " << /Type /Pages /Count ", pgroup
write(draw_unit, '(A)') "/Kids ["
do i = 1, pgroup
write(draw_unit, '(I0, A)') (i - 1) * 3 + 6, " 0 R"
end do
write(draw_unit, '(A)') "]>>endobj"
call store_start_obj(1)
write(draw_unit, '(A)') " << /Type /Catalog /Pages 3 0 R>> endobj"
STARTXREF = get_stream_pos()
write(draw_unit, '(A)') "xref"
write(draw_unit, '(A, I0)') "0 ", (pgroup - 1) * 3 + pplus + 3
write(draw_unit, '(A)') "0000000000 65535 f "
do i = 1, pgroup * 3 + pplus - 1
write(draw_unit, '(I10.10, A)') xrefs(i), " 00000 n "
end do
write(draw_unit, '(A, I0, A)') "trailer << /Size ", pgroup * 3 + pplus, " /Root 1 0 R /Info 2 0 R>>"
write(draw_unit, '(A)') "startxref"
write(draw_unit, '(I0)') STARTXREF
write(draw_unit, '(A)') "%%EOF"
drawn = 0; points_printed = 0
flush(draw_unit)
if (draw_unit /= output_unit .and. draw_unit /= error_unit) close(draw_unit)
if (allocated(xrefs)) deallocate(xrefs)
num_xrefs = 0
PDF_exit = 0
end function PDF_exit
integer function PDF_draw(x, y)
integer, intent(in) :: x, y
if (LAST_X /= vdevice%cpVx .or. LAST_Y /= vdevice%cpVy) then
call closeline()
call openObject()
call openline()
write(draw_unit, '(I0, 1X, I0, A)', advance='no') vdevice%cpVx, vdevice%cpVy, " m"
LAST_X = vdevice%cpVx; LAST_Y = vdevice%cpVy
PDF_MOVED = 0; points_printed = 1
end if
call openline()
if (points_printed == 0) then
write(draw_unit, '(I0, 1X, I0, A)', advance='no') x, y, " m"
PDF_MOVED = 0
else
if (LAST_X /= x .or. LAST_Y /= y) PDF_MOVED = PDF_MOVED + 1
if (mod(points_printed, 8) == 7) then
write(draw_unit, '(I0, 1X, I0, A)') x, y, " l"
else
write(draw_unit, '(1X, I0, 1X, I0, A)', advance='no') x, y, " l"
end if
end if
points_printed = points_printed + 1
LAST_X = x; LAST_Y = y; drawn = 1
PDF_draw = 0
end function PDF_draw
integer function PDF_clear()
integer(int64) :: POS
call closeline()
if (drawn /= 0) then
write(draw_unit, '(A)', advance='no') "Q"
POS = get_stream_pos()
write(draw_unit, '(/, A, /, A)') "endstream", "endobj"
call store_start_obj((pgroup - 1) * 3 + pplus + 1)
write(draw_unit, '(I0, A, I0, A, I0)') POS - stream_start, " endobj% size of stream, start=", stream_start, " end=", POS
call store_start_obj((pgroup - 1) * 3 + pplus + 2)
write(draw_unit, '(A, I0, 1X, I0, A)') "<</Type/Page/MediaBox [ 0 0 ", &
vdevice%sizeSx / RAS_PER_PT, vdevice%sizeSy / RAS_PER_PT, " ] /Parent 3 0 R /Resources <</ProcSet[/PDF]>>"
write(draw_unit, '(A, I0, A)') "/Contents ", (pgroup - 1) * 3 + pplus, " 0 R>> endobj"
write(draw_unit, '(A, I0)') "% End Page ", pgroup
pgroup = pgroup + 1
write(draw_unit, '(A, I0)') "% id=Page", pgroup
call store_start_obj((pgroup - 1) * 3 + pplus)
write(draw_unit, '(A, I0, A)') "<< /Length ", (pgroup - 1) * 3 + pplus + 1, " 0 R>> stream"
stream_start = get_stream_pos()
write(draw_unit, '(A)') "q"
write(draw_unit, '(A, F0.6, 1X, F0.6, A)') "1 0 0 1 ", page_dx, page_dy, " cm % Translate"
write(draw_unit, '(4(F0.6, 1X), A)') &
cos(page_angle), sin(page_angle), -sin(page_angle), cos(page_angle), "0 0 cm % Rotate"
write(draw_unit, '(F0.6, A, F0.6, A)') page_xscale, " 0 0 ", page_yscale, " 0 0 cm % Scale"
write(draw_unit, '(A)') "1 j"
write(draw_unit, '(A)') "1 J"
write(draw_unit, '(I0, A)') curwid * vdevice%sizeX / 10000, " w"
end if
call closeObject()
drawn = 0; points_printed = 0; PDF_MOVED = 0
PDF_clear = 0
end function PDF_clear
integer function PDF_setlw(width)
integer, intent(in) :: width
call closeline()
call closeObject()
curwid = abs(width)
write(draw_unit, '(I0, A)') curwid * vdevice%sizeX / 10000, " w"
PDF_setlw = 0
end function PDF_setlw
integer function PDF_color(col)
integer, intent(in) :: col
integer :: idum
call closeline()
call closeObject()
if (col < 0) then
idum = PDF_setlw(abs(col))
else
curpat = col / CMAPSIZE
curcol = mod(col, CMAPSIZE)
write(draw_unit, '(3(F0.6, 1X), A, 3(F0.6, 1X), A)') &
PDF_carr(curcol+1)%red / 255.0_real32, &
PDF_carr(curcol+1)%green / 255.0_real32, &
PDF_carr(curcol+1)%blue / 255.0_real32, "RG ", &
PDF_carr(curcol+1)%red / 255.0_real32, &
PDF_carr(curcol+1)%green / 255.0_real32, &
PDF_carr(curcol+1)%blue / 255.0_real32, "rg"
end if
PDF_color = 0
end function PDF_color
integer function PDF_font(fontname)
character(len=*), intent(in) :: fontname
real(real32) :: rat
vdevice%attr%a%softtext = SOFTHARDWARE
if (vdevice%hwidth == 0.0_real32 .or. vdevice%hheight == 0.0_real32) then
vdevice%hwidth = real(PDF_XSIZE * RAS_PER_PT, real32)
vdevice%hheight = real(PDF_YSIZE * RAS_PER_PT, real32)
end if
if (trim(fontname) == "small") then
hardfont = "/F1"
rat = 0.072_real32
hardwidth(1:128) = (600.0_real32 / 72.0_real32) * rat
else if (trim(fontname) == "large") then
hardfont = "/F2"
rat = 0.072_real32
hardwidth(1:128) = (real(helvetica_w(1:128), real32) / 72.0_real32) * rat
else
PDF_font = 0; return
end if
PDF_font = 1
end function PDF_font
integer function PDF_string(s)
character(len=*), intent(in) :: s
integer :: i
real(real32) :: slength, rot, fudge
character(len=1) :: c
call closeline()
rot = real(atan2(real(vdevice%attr%a%textsin, real64), real(vdevice%attr%a%textcos, real64)), real32)
write(draw_unit, '(A, A, A, I0, A)', advance='no') "BT ", trim(hardfont), " ", int(vdevice%hheight), " Tf "
fudge = real(vdevice%cpVy, real32) + 0.22_real32 * vdevice%hheight
if (rot /= 0.0_real32) then
write(draw_unit, '(4(F0.6, 1X), I0, 1X, I0, A)', advance='no') &
cos(rot), sin(rot), -sin(rot), cos(rot), vdevice%cpVx, int(fudge), " Tm("
else
write(draw_unit, '(I0, 1X, I0, A)', advance='no') vdevice%cpVx, int(fudge), " Td("
end if
do i = 1, len(s)
c = s(i:i)
if (c == '(' .or. c == ')' .or. c == '\') then
write(draw_unit, '(A, A)', advance='no') '\', c
else
write(draw_unit, '(A)', advance='no') c
end if
end do
write(draw_unit, '(A)') ")Tj ET"
drawn = 1
slength = real(len(s), real32)
LAST_X = int(real(vdevice%cpVx, real32) + slength * vdevice%attr%a%textcos)
LAST_Y = int(real(vdevice%cpVy, real32) + slength * vdevice%attr%a%textsin)
PDF_string = 0
end function PDF_string
integer function PDF_char(c)
character(len=1), intent(in) :: c
integer :: idum
idum = PDF_string(c)
PDF_char = 0
end function PDF_char
integer function PDF_fill(n, x, y)
integer, intent(in) :: n
integer, intent(in) :: x(n), y(n)
integer :: i
call closeline()
call closeObject()
write(draw_unit, '(I0, 1X, I0, A)') x(1), y(1), " m"
do i = 2, n
if (mod(i - 1, 8) == 7) then
write(draw_unit, '(I0, 1X, I0, A)') x(i), y(i), " l"
else
write(draw_unit, '(I0, 1X, I0, A)', advance='no') x(i), y(i), " l "
end if
end do
write(draw_unit, '(I0, 1X, I0, A, /, A)') x(1), y(1), " l", "f*"
vdevice%cpVx = x(n); vdevice%cpVy = y(n)
LAST_X = vdevice%cpVx; LAST_Y = vdevice%cpVy
drawn = 1
PDF_fill = 0
end function PDF_fill
integer function noop(); noop = -1; end function
integer function noop2(x, y); integer, intent(out) :: x, y; x = 0; y = 0; noop2 = -1; end function
integer function PDF_draw_devcpy()
vdevice%dev%devname = "pdf"
vdevice%dev%large = "large"
vdevice%dev%small = "small"
vdevice%dev%Vbackb => noop
vdevice%dev%Vchar => PDF_char
vdevice%dev%Vcheckkey => noop
vdevice%dev%Vclear => PDF_clear
vdevice%dev%Vcolor => PDF_color
vdevice%dev%Vdraw => PDF_draw
vdevice%dev%Vexit => PDF_exit
vdevice%dev%Vfill => PDF_fill
vdevice%dev%Vfont => PDF_font
vdevice%dev%Vfrontb => noop
vdevice%dev%Vgetkey => noop
vdevice%dev%Vinit => PDF_init
vdevice%dev%Vlocator => noop2
vdevice%dev%Vmapcolor => PDF_mapcolor
vdevice%dev%Vsetlw => PDF_setlw
vdevice%dev%Vstring => PDF_string
vdevice%dev%Vswapb => noop
vdevice%dev%Vsync => noop
PDF_draw_devcpy = 0
end function PDF_draw_devcpy
end module m_draw_drv_pdf
