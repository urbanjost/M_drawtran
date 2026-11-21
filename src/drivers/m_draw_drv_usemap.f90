module m_draw_drv_usemap
   use, intrinsic :: iso_fortran_env, only: real32, output_unit, error_unit
   use m_draw_types
   use m_draw_pref
   use m_draw_text
   use m_draw_drivers
   implicit none
   private
   public :: USEMAP_draw_devcpy

   integer, parameter :: CMAPSIZE  = 8192
   integer, parameter :: CMAPDEPTH = 13

   type :: ColorTable
      integer :: r = 255, g = 255, b = 255
   end type ColorTable

   type(ColorTable), save :: coltab(CMAPSIZE)

   integer, save :: X_SIZE = 0, Y_SIZE = 0
   integer, save :: GLOBAL_font  = 0
   integer, save :: GLOBAL_color = 0
   integer, save :: GLOBAL_lastx = -1111111, GLOBAL_lasty = -1111111
   integer, save :: POLYGON_COUNT = 0
   integer, save :: PAGE_COUNT    = 1

contains

   pure integer function FLIPY(y)
      integer, intent(in) :: y
      FLIPY = vdevice%sizeSy - y
   end function FLIPY

   integer function USEMAP_setlw(w)
      integer, intent(in) :: w
      USEMAP_setlw = 0
   end function USEMAP_setlw

   integer function USEMAP_color(col)
      integer, intent(in) :: col
      if (col >= 0) GLOBAL_color = abs(mod(col, CMAPSIZE))
      USEMAP_color = 0
   end function USEMAP_color

   integer function USEMAP_mapcolor(indx, r, g, b)
      integer, intent(in) :: indx, r, g, b
      if (indx < CMAPSIZE .and. indx >= 0) then
         coltab(indx + 1)%r = abs(mod(r, CMAPSIZE))
         coltab(indx + 1)%g = abs(mod(g, CMAPSIZE))
         coltab(indx + 1)%b = abs(mod(b, CMAPSIZE))
      end if
      USEMAP_mapcolor = 0
   end function USEMAP_mapcolor

   integer function USEMAP_init()
      integer :: prefx, prefy, prefxs, prefys, idum

      call draw_getprefposandsize(prefx, prefy, prefxs, prefys)
      if (prefxs /= -1) then
         if (prefys <= 0) then
            write(error_unit, '(A, I0, A)') "*USEMAP_init* y size of ", prefys, " set to 400"
            prefys = 400
         end if
         vdevice%sizeSy = prefys

         if (prefxs <= 0) then
            write(error_unit, '(A, I0, A)') "*USEMAP_init* x size of ", prefxs, " set to 600"
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
      vdevice%depth = CMAPDEPTH

      draw_unit = draw_outfile()
      GLOBAL_lastx = -1111111; GLOBAL_lasty = -1111111

      coltab = ColorTable(255, 255, 255)
      idum = USEMAP_mapcolor(0, 255, 255, 255)
      idum = USEMAP_mapcolor(1, 255,   0,   0)
      idum = USEMAP_mapcolor(2,   0, 255,   0)
      idum = USEMAP_mapcolor(3, 255, 255,   0)
      idum = USEMAP_mapcolor(4,   0,   0, 255)
      idum = USEMAP_mapcolor(5, 255,   0, 255)
      idum = USEMAP_mapcolor(6,   0, 255, 255)
      idum = USEMAP_mapcolor(7,   0,   0,   0)

      write(draw_unit, '(A)') "<html>"
      write(draw_unit, '(A)') "<head>"
      write(draw_unit, '(A)') '  <meta http-equiv="Content-Language" content="en" />'
      write(draw_unit, '(A)') '  <meta http-equiv="Content-Type" content="text/html; charset=us-ascii" />'
      write(draw_unit, '(A)') '  <meta name="description" content="polygons mapped to HTML images "/>'
      write(draw_unit, '(A)') '  <meta name="author" content="John S. Urban">'
      write(draw_unit, '(A)') '  <meta name="generator" content="M_DRAW graphics library" />'
      write(draw_unit, '(A)') '  <title>Image Map</title>'
      write(draw_unit, '(A)') '  <script type="text/javascript">'
      write(draw_unit, '(A)') '    function dosomething(elementid,string){'
      write(draw_unit, '(A)') '       alert( string );'
      write(draw_unit, '(A)') '    }'
      write(draw_unit, '(A)') '  </script>'
      write(draw_unit, '(A)') '</head>'
      write(draw_unit, '(A)') '<body>'

      USEMAP_init = 1
   end function USEMAP_init

   integer function USEMAP_fill(n, x, y)
      integer, intent(in) :: n
      integer, intent(in) :: x(n), y(n)
      integer :: i

      if (POLYGON_COUNT == 0) then
         write(draw_unit, '(A, I0, A, I0, A, I0, A, I0, A, I0, A, I0, A)') &
            '<div><img id="IMAGE', PAGE_COUNT, '"  src="IMAGE', PAGE_COUNT, '.gif"  alt="Page ', PAGE_COUNT, &
            '"  usemap="#PAGE', PAGE_COUNT, '"  height="', vdevice%sizeSx, '" width="', vdevice%sizeSy, '" />'
         write(draw_unit, '(A, I0, A, I0, A)') '<map id="PAGE', PAGE_COUNT, '" name="PAGE', PAGE_COUNT, '">'
      end if

      POLYGON_COUNT = POLYGON_COUNT + 1
      GLOBAL_lastx = x(1); GLOBAL_lasty = y(1)

      write(draw_unit, '(A, I0, A, I0, 1X, I0, 1X, I0, A, I0, A)', advance='no') &
         '<area title="POLYGON: ', POLYGON_COUNT, ' color:', &
         coltab(GLOBAL_color+1)%r, coltab(GLOBAL_color+1)%g, coltab(GLOBAL_color+1)%b, &
         '" alt="POLYGON', POLYGON_COUNT, ' region" shape="poly" coords="'

      do i = 2, n
         if (i > 2) write(draw_unit, '(A)', advance='no') ","
         write(draw_unit, '(I0, ",", I0)', advance='no') x(i), FLIPY(y(i))
      end do

      if (x(n) /= x(1) .or. y(n) /= y(1)) then
         write(draw_unit, '(A, I0, ",", I0)', advance='no') ",", x(1), FLIPY(y(1))
      end if

      write(draw_unit, '(A, I0, A, I0, A)') &
         '" onClick="dosomething(', "'PAGE", PAGE_COUNT, "','clicked POLYGON", POLYGON_COUNT, "');"">"

      GLOBAL_lastx = x(n); vdevice%cpVx = x(n)
      GLOBAL_lasty = y(n); vdevice%cpVy = y(n)
      USEMAP_fill = 0
   end function USEMAP_fill

   integer function USEMAP_draw(x, y)
      integer, intent(in) :: x, y
      if (GLOBAL_lastx /= vdevice%cpVx .or. GLOBAL_lasty /= vdevice%cpVy) then
         GLOBAL_lastx = vdevice%cpVx; GLOBAL_lasty = vdevice%cpVy
      end if
      USEMAP_draw = 0
   end function USEMAP_draw

   integer function USEMAP_exit()
      if (POLYGON_COUNT /= 0) then

         write(draw_unit, '(A, I0, ",", I0, A, I0, A)') &
            '<area title="BACKGROUND" alt="background" shape="rect" coords="0,0,', &
            vdevice%sizeSy, vdevice%sizeSx, '" onclick="dosomething(', "'PAGE", PAGE_COUNT, "','you clicked BACKGROUND');"" />"
         write(draw_unit, '(A)') '</map></div>'
      end if
      write(draw_unit, '(A, /, A)') '</body>', '</html>'
      if (draw_unit /= output_unit .and. draw_unit /= error_unit) then
         flush(draw_unit)
         close(draw_unit)
      end if
      USEMAP_exit = 0
   end function USEMAP_exit

   integer function USEMAP_clear()
      if (POLYGON_COUNT /= 0) then
         write(draw_unit, '(A, I0, ",", I0, A, I0, A)') &
            '<area title="BACKGROUND" alt="background" shape="rect" coords="0,0,', &
            vdevice%sizeSy, vdevice%sizeSx, '" onclick="dosomething(', "'PAGE", PAGE_COUNT, "','you clicked BACKGROUND');"" />"
         write(draw_unit, '(A)') '</map></div>'
      end if
      PAGE_COUNT = PAGE_COUNT + 1
      POLYGON_COUNT = 0
      USEMAP_clear = 0
   end function USEMAP_clear

   integer function USEMAP_font(fontname)
      character(len=*), intent(in) :: fontname
      if (trim(fontname) == "small") then
         vdevice%hwidth = 8.00_real32; vdevice%hheight = 13.0_real32; GLOBAL_font = 0
      else if (trim(fontname) == "large") then
         vdevice%hwidth = 15.00_real32; vdevice%hheight = 39.00_real32; GLOBAL_font = 1
         call draw_font("futura.m")
      else
         USEMAP_font = 0; return
      end if
      USEMAP_font = 1
   end function USEMAP_font

   integer function USEMAP_char(c)
      character(len=1), intent(in) :: c
      if (GLOBAL_lastx /= vdevice%cpVx .or. GLOBAL_lasty /= vdevice%cpVy) then
         GLOBAL_lastx = vdevice%cpVx; GLOBAL_lasty = vdevice%cpVy
      end if
      GLOBAL_lastx = vdevice%cpVx + int(vdevice%hwidth)
      USEMAP_char = 0
   end function USEMAP_char

   integer function USEMAP_string(s)
      character(len=*), intent(in) :: s
      integer :: i
      if (GLOBAL_lastx /= vdevice%cpVx .or. GLOBAL_lasty /= vdevice%cpVy) then
         GLOBAL_lastx = vdevice%cpVx; GLOBAL_lasty = vdevice%cpVy
      end if
      do i = 1, len(s)
         GLOBAL_lastx = vdevice%cpVx + int(vdevice%hwidth)
      end do
      USEMAP_string = 0
   end function USEMAP_string

   integer function noop(); noop = -1; end function
   integer function noop2(x, y); integer, intent(out) :: x, y; x = 0; y = 0; noop2 = -1; end function

   integer function USEMAP_draw_devcpy()
      vdevice%dev%devname   = "usemap"
      vdevice%dev%large     = "large"
      vdevice%dev%small     = "small"
      vdevice%dev%Vbackb    => noop
      vdevice%dev%Vchar     => USEMAP_char
      vdevice%dev%Vcheckkey => noop
      vdevice%dev%Vclear    => USEMAP_clear
      vdevice%dev%Vcolor    => USEMAP_color
      vdevice%dev%Vdraw     => USEMAP_draw
      vdevice%dev%Vexit     => USEMAP_exit
      vdevice%dev%Vfill     => USEMAP_fill
      vdevice%dev%Vfont     => USEMAP_font
      vdevice%dev%Vfrontb   => noop
      vdevice%dev%Vgetkey   => noop
      vdevice%dev%Vinit     => USEMAP_init
      vdevice%dev%Vlocator  => noop2
      vdevice%dev%Vmapcolor => USEMAP_mapcolor
      vdevice%dev%Vsetlw    => USEMAP_setlw
      vdevice%dev%Vstring   => USEMAP_string
      vdevice%dev%Vswapb    => noop
      vdevice%dev%Vsync     => noop
      USEMAP_draw_devcpy = 0
   end function USEMAP_draw_devcpy

end module m_draw_drv_usemap
