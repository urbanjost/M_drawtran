module m_draw_drv_vml
   use, intrinsic :: iso_fortran_env, only: real32, output_unit, error_unit
   use m_draw_types
   use m_draw_pref
   use m_draw_text
   use m_draw_drivers
   implicit none
   private
   public :: VML_draw_devcpy

   integer, parameter :: VMLXSIZE = 450
   integer, parameter :: VMLYSIZE = 650
   integer, parameter :: VMLTORAS = 20
   integer, parameter :: CMAPSIZE = 256

   type :: rgb_color
      integer :: red = 255, green = 255, blue = 255
   end type rgb_color

   type(rgb_color), save :: vml_carr(CMAPSIZE)

   logical, save :: vml_first_time = .true.
   logical, save :: lineopen       = .false.
   logical, save :: shapeopen      = .false.
   integer, save :: points         = 0
   integer, save :: drawn          = 0
   integer, save :: pslstx = -1, pslsty = -1
   integer, save :: OLDX = 0, OLDY = 0
   integer, save :: VML_MOVED      = 0
   integer, save :: curcol         = 0
   integer, save :: curwid         = 1
   integer, save :: curpat         = 0
   integer, save :: pgroup         = 1

   character(len=256), save :: fontstyle = ""

   integer, parameter :: helvetica_w(128) = [ &
      0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0, &
      27,33,43,55,55,88,72,28,33,33,39,58,27,33,28,28,55,55,55,55,55,55,55,55,55,55, &
      34,34,58,58,58,61,97, &
      73,73,73,73,66,61,78,72,28,55,73,61,83,72,78,66,78,72,67,61,72,66,94,66,66,61, &
      33,28,33,58,55,28, &
      55,61,55,61,55,33,61,61,28,28,55,28,88,61,61,61,61,39,55,33,61,55,78,55,55,50, &
      39,28,39,58,0 ]

contains

   pure integer function FLIPY(y)
      integer, intent(in) :: y
      FLIPY = vdevice%sizeSy - y
   end function FLIPY

   subroutine VML_header()
      character(len=8)  :: dt
      character(len=10) :: tm
      call date_and_time(date=dt, time=tm)

      write(draw_unit, '(A)') '<html xmlns:v="urn:schemas-microsoft-com:vml"'
      write(draw_unit, '(A)') '      xmlns:o="urn:schemas-microsoft-com:office:office">'
      write(draw_unit, '(A)') '<!--'
      write(draw_unit, '(A, A, "-", A, "-", A, " ", A, ":", A, ":", A)') &
         '  Data Creation Date: ', dt(1:4), dt(5:6), dt(7:8), tm(1:2), tm(3:4), tm(5:6)
      write(draw_unit, '(A)') '-->'
      write(draw_unit, '(A)') '<head>'
      write(draw_unit, '(A)') '<style>'
      write(draw_unit, '(A)') ' v\:* { behavior:url(#default#VML); }'
      write(draw_unit, '(A)') ' o\:* { behavior:url(#default#VML); }'
      write(draw_unit, '(A)') '</style>'
      write(draw_unit, '(A)') '<title>VML graphics</title>'
      write(draw_unit, '(A)') '<meta name="Author" content="John S. Urban">'
      write(draw_unit, '(A)') '<meta name="Description" content="@(#)M_DRAW VML Driver Version 1.0.1, March  1999">'
      write(draw_unit, '(A)') '</head>'
      write(draw_unit, '(A)') '<body topmargin="10" leftmargin="10" bgcolor="#FFFFFF" link="#000066" vlink="#666666" text="#000000">'
      write(draw_unit, '(A)') '<font face="VERDANA,ARIAL,HELVETICA" size="2">'
      write(draw_unit, '(A)') '<p>&nbsp;</p>'
   end subroutine VML_header

   integer function VML_mapcolor(i, r, g, b)
      integer, intent(in) :: i, r, g, b
      if (i >= CMAPSIZE .or. i < 0) then
         VML_mapcolor = -1; return
      end if
      vml_carr(i + 1)%red   = r
      vml_carr(i + 1)%green = g
      vml_carr(i + 1)%blue  = b
      VML_mapcolor = 0
   end function VML_mapcolor

   integer function VML_init()
      integer :: prefx, prefy, prefxs, prefys, idum

      draw_unit = draw_outfile()
      if (.not. vml_first_time) then
         VML_init = 1; return
      end if

      call VML_header()
      call draw_getprefposandsize(prefx, prefy, prefxs, prefys)

      if (prefxs /= -1) then
         vdevice%sizeSy = prefys * VMLTORAS
         vdevice%sizeSx = prefxs * VMLTORAS
         vdevice%sizeX  = min(prefys, prefxs) * VMLTORAS; vdevice%sizeY = vdevice%sizeX
      else
         vdevice%sizeSy = VMLYSIZE * VMLTORAS
         vdevice%sizeSx = VMLXSIZE * VMLTORAS
         vdevice%sizeX  = min(VMLXSIZE, VMLYSIZE) * VMLTORAS; vdevice%sizeY = vdevice%sizeX
      end if

      write(draw_unit, '(A, I0, A)') '<a name="Page', pgroup, '"></a>'
      write(draw_unit, '(A, I0, A)', advance='no') "<v:group id=""Page", pgroup, """ style='mso-position-horizontal-relative:char;"
      write(draw_unit, '(A)', advance='no') " mso-position-vertical-relative:line;"
      write(draw_unit, '(A, I0, A, I0, A)') " left:0pt;top:0pt;width:", vdevice%sizeSx/VMLTORAS, "pt;height:", vdevice%sizeSy/VMLTORAS, "pt'"
      write(draw_unit, '(A, I0, A, I0, ",", I0, A)') ' url="#Page', pgroup, '" coordsize="', vdevice%sizeSx, vdevice%sizeSy, '" >'

      vdevice%depth = 8
      vml_carr = rgb_color(255, 255, 255)

      idum = VML_mapcolor(0, 255, 255, 255)
      idum = VML_mapcolor(1, 255,   0,   0)
      idum = VML_mapcolor(2,   0, 255,   0)
      idum = VML_mapcolor(3, 255, 255,   0)
      idum = VML_mapcolor(4,   0,   0, 255)
      idum = VML_mapcolor(5, 255,   0, 255)
      idum = VML_mapcolor(6,   0, 255, 255)
      idum = VML_mapcolor(7,   0,   0,   0)
      idum = VML_mapcolor(8, 155,   0,   0)
      idum = VML_mapcolor(9,   0, 155,   0)
      idum = VML_mapcolor(10, 155, 255, 255)
      idum = VML_mapcolor(11, 155, 155,   0)
      idum = VML_mapcolor(12,   0,   0, 155)
      idum = VML_mapcolor(13, 155,   0, 155)
      idum = VML_mapcolor(14,   0, 155, 155)
      idum = VML_mapcolor(15, 100, 100, 100)

      lineopen = .false.; shapeopen = .false.
      curcol = 0; curwid = 1; curpat = 0; drawn = 0
      VML_init = 1
   end function VML_init

   subroutine closeline()
      if (lineopen) then
         if (VML_MOVED == 0) then
            write(draw_unit, '(A)', advance='no') " t -1 -1 r 1 1 "
         end if
         write(draw_unit, '(A)', advance='no') " e"
         lineopen = .false.; points = 0
      end if
   end subroutine closeline

   subroutine closeshape()
      if (shapeopen) then
         write(draw_unit, '(A)') '"><v:stroke joinstyle="round" endcap="round"/>'
         write(draw_unit, '(A)') '</v:shape>'
         shapeopen = .false.; points = 0
      end if
   end subroutine closeshape

   subroutine openline()
      if (.not. lineopen) then
         write(draw_unit, '(/, A)', advance='no') "m"
         lineopen = .true.
      end if
   end subroutine openline

   subroutine openshape()
      if (.not. shapeopen) then
         write(draw_unit, '(A)', advance='no') "<v:shape "
         write(draw_unit, '(A, I0, A, I0, A)') " style='position:absolute;top:0;left:0;width:", vdevice%sizeSx, ";height:", vdevice%sizeSy, "'"
         write(draw_unit, '(A)', advance='no') 'filled="false" stroke="true"'
         write(draw_unit, '(A, I0, A, I0, ",", I0, ",", I0, A)', advance='no') &
            ' strokeweight="', curwid * vdevice%sizeX / 10000 / VMLTORAS, &
            'pt" strokecolor="rgb(', vml_carr(curcol+1)%red, vml_carr(curcol+1)%green, vml_carr(curcol+1)%blue, ')"' // new_line('A') // 'path="nf'
         shapeopen = .true.
      end if
   end subroutine openshape

   integer function VML_exit()
      call closeline()
      call closeshape()
      write(draw_unit, '(A)') "</v:group>"
      write(draw_unit, '(A)') "</font>"
      write(draw_unit, '(A)') "</body>"
      write(draw_unit, '(A)') "</html>"
      drawn = 0; points = 0
      if (draw_unit /= output_unit .and. draw_unit /= error_unit) then
         flush(draw_unit)
         close(draw_unit)
      end if
      VML_exit = 0
   end function VML_exit

   integer function VML_draw(x, y)
      integer, intent(in) :: x, y

      if (pslstx /= vdevice%cpVx .or. pslsty /= vdevice%cpVy) then
         call closeline()
         call openshape()
         call openline()
         write(draw_unit, '(1X, I0, 1X, I0, A)', advance='no') vdevice%cpVx, FLIPY(vdevice%cpVy), " l"
         OLDX = vdevice%cpVx; OLDY = FLIPY(vdevice%cpVy)
         VML_MOVED = 0; points = 1
      end if
      call openshape()
      call openline()

      if (points == 0) then
         write(draw_unit, '(1X, I0, 1X, I0, A)', advance='no') x, FLIPY(y), " l"
         OLDX = x; OLDY = FLIPY(y); VML_MOVED = 0
      else
         if (OLDX /= x .or. OLDY /= FLIPY(y)) VML_MOVED = VML_MOVED + 1
         OLDX = x; OLDY = FLIPY(y)
         if (mod(points, 8) == 7) then
            write(draw_unit, '(I0, 1X, I0)') x, FLIPY(y)
         else
            write(draw_unit, '(1X, I0, 1X, I0)', advance='no') x, FLIPY(y)
         end if
      end if

      points = points + 1
      pslstx = x; pslsty = y; drawn = 1
      VML_draw = 0
   end function VML_draw

   integer function VML_clear()
      call closeline()
      call closeshape()
      if (drawn /= 0) then
         pgroup = pgroup + 1
         write(draw_unit, '(A)') "</v:group>"
         write(draw_unit, '(A, I0, A)') '<a name="Page', pgroup, '"></a>'
         write(draw_unit, '(A)') "<br/>"
         write(draw_unit, '(A, I0, A)', advance='no') "<v:group id=""Page", pgroup, """ style='mso-position-horizontal-relative:char;"
         write(draw_unit, '(A)', advance='no') "mso-position-vertical-relative:line;"
         write(draw_unit, '(A, I0, A, I0, A)') "left:0pt;top:0pt;width:", vdevice%sizeSx/VMLTORAS, "pt;height:", vdevice%sizeSy/VMLTORAS, "pt'"
         write(draw_unit, '(A, I0, A, I0, ",", I0, A)') ' url="#Page', pgroup, '" coordsize="', vdevice%sizeSx, vdevice%sizeSy, '" >'
      end if
      drawn = 0; points = 0
      VML_clear = 0
   end function VML_clear

   integer function VML_color(col)
      integer, intent(in) :: col
      call closeline()
      call closeshape()
      if (col < 0) then
         curwid = abs(col)
      else
         curpat = col / CMAPSIZE
         curcol = mod(col, CMAPSIZE)
      end if
      VML_color = 0
   end function VML_color

   integer function VML_setlw(width)
      integer, intent(in) :: width
      call closeline()
      call closeshape()
      if (width >= 0) curwid = width
      VML_setlw = 0
   end function VML_setlw

integer function VML_font(fontname)
character(len=*), intent(in) :: fontname
real(real32) :: rat
vdevice%attr%a%softtext = SOFTHARDWARE
if (vdevice%hwidth == 0.0_real32 .or. vdevice%hheight == 0.0_real32) then
vdevice%hwidth = 11.0_real32 * VMLTORAS; vdevice%hheight = 11.0_real32 * VMLTORAS
end if
if (trim(fontname) == "small") then
rat = 0.55_real32
hardwidth(1:128) = 1.00_real32 * rat
fontstyle = "font-family:'Times New Roman'"
else if (trim(fontname) == "large") then
rat = 1.00_real32
hardwidth(1:128) = (real(helvetica_w(1:128), real32) / 100.0_real32) * rat
fontstyle = "font-family:'Arial Black', Arial, sans-serif"
else
fontstyle = "font-family:'Arial Black', Arial, sans-serif"
VML_font = 0; return
end if
VML_font = 1
end function VML_font
integer function VML_string(s)
character(len=*), intent(in) :: s
integer :: i, uneven, x, y, ijust
real(real32) :: slen, sheight
character(len=1) :: c
call closeline()
call closeshape()
if (vdevice%hwidth == 0.0_real32 .or. vdevice%hheight == 0.0_real32) then
write(error_unit, '(A)') "VML_string ERROR: ZERO SIZE CHARACTERS"
vdevice%hwidth = 11.0_real32; vdevice%hheight = 11.0_real32
end if
slen = draw_strlength(s) * vdevice%hwidth * real(VMLTORAS, real32)
sheight = vdevice%hheight
if (FLIPY(vdevice%cpVy) == FLIPY(int(real(vdevice%cpVy, real32) + slen * vdevice%attr%a%textsin))) then
uneven = 1
else
uneven = 0
end if
ijust = int(0.50_real32 * sheight)
x = int(real(vdevice%cpVx, real32) + slen * vdevice%attr%a%textcos)
y = int(real(uneven + vdevice%cpVy, real32) + slen * vdevice%attr%a%textsin)
write(draw_unit, '(A, I0, ",", I0, A, I0, ",", I0, A)') &
'<v:line from="', vdevice%cpVx, FLIPY(vdevice%cpVy + ijust), '" to="', x, FLIPY(y + ijust), '"'
write(draw_unit, '(A, I0, A, I0, A)') " style='position:absolute;top:0;left:0;width:", vdevice%sizeSx, ";height:", vdevice%sizeSy, "'"
write(draw_unit, '(A, I0, ",", I0, ",", I0, A)') &
'filled="false" stroke="true" strokecolor="rgb(', vml_carr(curcol+1)%red, vml_carr(curcol+1)%green, vml_carr(curcol+1)%blue, ')">'
write(draw_unit, '(A, I0, ",", I0, ",", I0, A)') &
'<v:fill on="t" color="rgb(', vml_carr(curcol+1)%red, vml_carr(curcol+1)%green, vml_carr(curcol+1)%blue, ')"/>'
write(draw_unit, '(A)') '<v:path textpathok="t"></v:path>'
write(draw_unit, '(A)', advance='no') '<v:textpath on="t" fitpath ="t" string="'
do i = 1, len(s)
c = s(i:i)
select case (c)
case ('"'); write(draw_unit, '(A)', advance='no') '"'
case ('<'); write(draw_unit, '(A)', advance='no') '<'
case ('>'); write(draw_unit, '(A)', advance='no') '>'
case default; write(draw_unit, '(A)', advance='no') c
end select
end do
write(draw_unit, '(A, A, A, I0, A)') '" style="', trim(fontstyle), ';v-text-align:stretch-justify;fontsize:', int(sheight), '"/>'
write(draw_unit, '(A)') '</v:line>'
drawn = 1; pslstx = x; pslsty = y
call closeshape()
VML_string = 0
end function VML_string
integer function VML_char(c)
character(len=1), intent(in) :: c
integer :: idum
idum = VML_string(c)
VML_char = 0
end function VML_char
integer function VML_fill(n, x, y)
integer, intent(in) :: n
integer, intent(in) :: x(n), y(n)
integer :: i
call closeline()
call closeshape()
write(draw_unit, '(A, I0, A, I0, A)') "<v:shape style='position:absolute;top:0;left:0;width:", vdevice%sizeSx, ";height:", vdevice%sizeSy, "'"
write(draw_unit, '(A, I0, ",", I0, ",", I0, A, I0, ",", I0, A, I0, 1X, I0, A)') &
' stroke="false" filled="true" fillcolor="rgb(', vml_carr(curcol+1)%red, vml_carr(curcol+1)%green, vml_carr(curcol+1)%blue, &
')" coordsize="', vdevice%sizeSx, vdevice%sizeSy, '" path="m ', x(1), FLIPY(y(1)), ' l'
do i = 2, n
if (mod(i - 1, 8) == 7) then
write(draw_unit, '(1X, I0, 1X, I0)') x(i), FLIPY(y(i))
else
write(draw_unit, '(1X, I0, 1X, I0)', advance='no') x(i), FLIPY(y(i))
end if
end do
write(draw_unit, '(A)') ' x e"/></v:shape>'
vdevice%cpVx = x(n); vdevice%cpVy = y(n)
pslstx = -1; pslsty = -1; drawn = 1
VML_fill = 0
end function VML_fill
integer function noop(); noop = -1; end function
integer function noop2(x, y); integer, intent(out) :: x, y; x = 0; y = 0; noop2 = -1; end function
integer function VML_draw_devcpy()
vdevice%dev%devname = "vml"
vdevice%dev%large = "large"
vdevice%dev%small = "small"
vdevice%dev%Vbackb => noop
vdevice%dev%Vchar => VML_char
vdevice%dev%Vcheckkey => noop
vdevice%dev%Vclear => VML_clear
vdevice%dev%Vcolor => VML_color
vdevice%dev%Vdraw => VML_draw
vdevice%dev%Vexit => VML_exit
vdevice%dev%Vfill => VML_fill
vdevice%dev%Vfont => VML_font
vdevice%dev%Vfrontb => noop
vdevice%dev%Vgetkey => noop
vdevice%dev%Vinit => VML_init
vdevice%dev%Vlocator => noop2
vdevice%dev%Vmapcolor => VML_mapcolor
vdevice%dev%Vsetlw => VML_setlw
vdevice%dev%Vstring => VML_string
vdevice%dev%Vswapb => noop
vdevice%dev%Vsync => noop
VML_draw_devcpy = 0
end function VML_draw_devcpy
end module m_draw_drv_vml
