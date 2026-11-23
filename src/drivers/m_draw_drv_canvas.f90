module m_draw_drv_canvas
   use, intrinsic :: iso_fortran_env, only: real32, output_unit, error_unit
   use m_draw_types
   use m_draw_pref
   use m_draw_text
   use m_draw_drivers
   implicit none
   private
   public :: CANVAS_draw_devcpy

   integer, parameter :: CANVASYSIZE = 5500
   integer, parameter :: CANVASXSIZE = 4250
   integer, parameter :: CANVASTORAS = 10
   integer, parameter :: CMAPSIZE    = 256

   type :: rgb_color
      integer :: red = 255, green = 255, blue = 255
   end type rgb_color

   type(rgb_color), save :: canvas_carr(CMAPSIZE)

   logical, save :: canvas_first_time = .true.
   logical, save :: PolyLineOpen      = .false.
   logical, save :: ObjectOpen        = .false.
   integer, save :: points            = 0
   integer, save :: drawn             = 0
   integer, save :: LAST_X = -1, LAST_Y = -1
   integer, save :: CANVAS_MOVED      = 0
   integer, save :: curcol            = 0
   integer, save :: curwid            = 1
   integer, save :: curpat            = 0
   integer, save :: pgroup            = 1

contains

   pure integer function FLIPY(y)
      integer, intent(in) :: y
      FLIPY = vdevice%sizeSy - y
   end function FLIPY

   subroutine CANVAS_header()
      character(len=8)  :: dt
      character(len=10) :: tm
      call date_and_time(date=dt, time=tm)

      write(draw_unit, '(A)') "<!DOCTYPE html>"
      write(draw_unit, '(A)') "<html>"
      write(draw_unit, '(A)') "<head>"
      write(draw_unit, '(A)') "<title>"
      write(draw_unit, '(A)') "  Creator: M_DRAW HTML CANVAS driver 1.2 2008-01-28"
      write(draw_unit, '(A)') "  Author: John S. Urban"
      write(draw_unit, '(A, A, "-", A, "-", A, " ", A, ":", A, ":", A)') &
         "  CreationDate: ", dt(1:4), dt(5:6), dt(7:8), tm(1:2), tm(3:4), tm(5:6)
      write(draw_unit, '(A)') "</title>"
   end subroutine CANVAS_header

   integer function CANVAS_mapcolor(i, r, g, b)
      integer, intent(in) :: i, r, g, b
      if (i >= CMAPSIZE .or. i < 0) then
         CANVAS_mapcolor = -1; return
      end if
      canvas_carr(i + 1)%red   = r
      canvas_carr(i + 1)%green = g
      canvas_carr(i + 1)%blue  = b
      CANVAS_mapcolor = 0
   end function CANVAS_mapcolor

   integer function CANVAS_init()
      integer :: prefx, prefy, prefxs, prefys, idum

      draw_unit = draw_outfile()
      if (.not. canvas_first_time) then
         CANVAS_init = 1; return
      end if

      call CANVAS_header()
      call draw_getprefposandsize(prefx, prefy, prefxs, prefys)

      if (prefxs /= -1) then
         vdevice%sizeSy = prefys
         vdevice%sizeSx = prefxs
         vdevice%sizeX  = min(prefys, prefxs); vdevice%sizeY = vdevice%sizeX
      else
         vdevice%sizeSy = CANVASYSIZE
         vdevice%sizeSx = CANVASXSIZE
         vdevice%sizeX  = min(CANVASXSIZE, CANVASYSIZE); vdevice%sizeY = vdevice%sizeX
      end if

      write(draw_unit, '(A, F0.6, A, F0.6, A, I0, 1X, I0, A)') &
         '<!-- canvas " width="', real(vdevice%sizeSx, real32)/CANVASTORAS, &
         'px" height="', real(vdevice%sizeSy, real32)/CANVASTORAS, &
         'px" viewBox="0 0 ', vdevice%sizeSx, vdevice%sizeSy, '"-->'

      write(draw_unit, '(A)') '<script type="application/x-javascript">'
      write(draw_unit, '(A)') 'function d(X,A){ // draw polyline'
      write(draw_unit, '(A)') 'X.beginPath();'
      write(draw_unit, '(A)') '   X.moveTo(A[0],A[1]);'
      write(draw_unit, '(A)') '   for(i=2; i<A.length;i=i+2){'
      write(draw_unit, '(A)') '      X.lineTo(A[i],A[i+1]);'
      write(draw_unit, '(A)') '   }'
      write(draw_unit, '(A)') '   X.stroke();'
      write(draw_unit, '(A)') '   radius=Math.max(1,X.lineWidth/2);'
      write(draw_unit, '(A)') '   if(A.length == 4){'
      write(draw_unit, '(A)') '    if( A[0] == A[2] & A[1] == A[3]){'
      write(draw_unit, '(A)') '         X.beginPath();'
      write(draw_unit, '(A)') '         X.arc(A[0],A[1],radius,0,Math.PI*2,true);'
      write(draw_unit, '(A)') '         X.fill();'
      write(draw_unit, '(A)') '      }'
      write(draw_unit, '(A)') '    }'
      write(draw_unit, '(A)') '}'
      write(draw_unit, '(A)') 'function f(X,A){ // fill polygon'
      write(draw_unit, '(A)') 'X.beginPath();'
      write(draw_unit, '(A)') '   X.moveTo(A[0],A[1]);'
      write(draw_unit, '(A)') '   for(i=2; i<A.length;i=i+2){'
      write(draw_unit, '(A)') '      X.lineTo(A[i],A[i+1]);'
      write(draw_unit, '(A)') '   }'
      write(draw_unit, '(A)') '   X.fill();'
      write(draw_unit, '(A)') '}'
      write(draw_unit, '(A)') 'function resizecanvas(canvasname,factor){'
      write(draw_unit, '(A)') '   var canvas = document.getElementById(canvasname);'
      write(draw_unit, '(A)') '   if(canvas.getContext){'
      write(draw_unit, '(A)') '       if(factor < 0 ){'
      write(draw_unit, '(A)') '          resizecanvas(canvasname,-factor/canvas.width);'
      write(draw_unit, '(A)') '       }else{'
      write(draw_unit, '(A)') '          xwidth=canvas.width*factor;'
      write(draw_unit, '(A)') '          yheight=canvas.height*factor;'
      write(draw_unit, '(A)') '          canvas.setAttribute("width", "" + xwidth);'
      write(draw_unit, '(A)') '          canvas.setAttribute("height", "" + yheight);'
      write(draw_unit, '(A)') '          canvas.width = xwidth;'
      write(draw_unit, '(A)') '          canvas.height = yheight;'
      write(draw_unit, '(A)') '       }'
      write(draw_unit, '(A)') '   }'
      write(draw_unit, '(A)') '}'
      write(draw_unit, '(A)') '</script>'
      write(draw_unit, '(A)') '<script language="JavaScript" src="resizecanvas.js" type="text/javascript"></script>'
      write(draw_unit, '(A)') '<script type="application/x-javascript">'
      write(draw_unit, '(A, I0, A)') 'function drawCanvas', pgroup, '(canvasname){'
      write(draw_unit, '(A, I0)') 'var canvas = document.getElementById(canvasname); // page ', pgroup
      write(draw_unit, '(A)') 'if(canvas.getContext){'
      write(draw_unit, '(A)') 'var X = canvas.getContext("2d");'
      write(draw_unit, '(A, I0, A)') '//<!-- id="Page', pgroup, '" -->'
      write(draw_unit, '(A)') 'X.save();'
      write(draw_unit, '(A, I0, A, I0, A)') 'X.scale(canvas.width/', vdevice%sizeSx, ',canvas.height/', vdevice%sizeSy, ');'
      write(draw_unit, '(A, I0, A)') 'X.lineWidth=', max(1, vdevice%sizeX * curwid / 10000), ';'
      write(draw_unit, '(A)') "X.fillStyle='black';"
      write(draw_unit, '(A)') "X.strokeStyle='black';"
      write(draw_unit, '(A)') "X.lineCap='round';"
      write(draw_unit, '(A)') "X.lineJoin='round';"

      vdevice%depth = 8
      canvas_carr = rgb_color(255, 255, 255)

      idum = CANVAS_mapcolor(0, 255, 255, 255)
      idum = CANVAS_mapcolor(1, 255,   0,   0)
      idum = CANVAS_mapcolor(2,   0, 255,   0)
      idum = CANVAS_mapcolor(3, 255, 255,   0)
      idum = CANVAS_mapcolor(4,   0,   0, 255)
      idum = CANVAS_mapcolor(5, 255,   0, 255)
      idum = CANVAS_mapcolor(6,   0, 255, 255)
      idum = CANVAS_mapcolor(7,   0,   0,   0)
      idum = CANVAS_mapcolor(8, 155,   0,   0)
      idum = CANVAS_mapcolor(9,   0, 155,   0)
      idum = CANVAS_mapcolor(10, 155, 255, 255)
      idum = CANVAS_mapcolor(11, 155, 155,   0)
      idum = CANVAS_mapcolor(12,   0,   0, 155)
      idum = CANVAS_mapcolor(13, 155,   0, 155)
      idum = CANVAS_mapcolor(14,   0, 155, 155)
      idum = CANVAS_mapcolor(15, 100, 100, 100)

      PolyLineOpen = .false.; ObjectOpen = .false.
      curcol = 0; curwid = 1; curpat = 0; drawn = 0
      CANVAS_init = 1
   end function CANVAS_init

   subroutine closeline()
      if (PolyLineOpen) then
         write(draw_unit, '(A)') '];d(X,A);'
         PolyLineOpen = .false.
         points = 0
      end if
   end subroutine closeline

   subroutine closeObject()
      if (ObjectOpen) then
         write(draw_unit, '(A)') '// end object'
         ObjectOpen = .false.
         points = 0
      end if
   end subroutine closeObject

   subroutine openline()
      if (.not. PolyLineOpen) PolyLineOpen = .true.
   end subroutine openline

   subroutine openObject()
      if (.not. ObjectOpen) then
         write(draw_unit, '(A)') '// start object'
         ObjectOpen = .true.
      end if
   end subroutine openObject

   integer function CANVAS_exit()
      integer :: ipages, ixtemp, iytemp

      call closeline()
      call closeObject()

      write(draw_unit, '(A)') 'X.restore();'
      write(draw_unit, '(A)') '}else{'
      write(draw_unit, '(A)') "alert('Your browser needs HTML CANVAS support to view this page');"
      write(draw_unit, '(A)') '}'
      write(draw_unit, '(A)') '} // end of page'

      write(draw_unit, '(A)') 'function drawCanvases(SCALE){'
      do ipages = 1, pgroup
         write(draw_unit, '(A, I0, A, I0, A, I0, A)') &
            'resizecanvas("canvas_id', ipages, '",SCALE);drawCanvas', ipages, '("canvas_id', ipages, '");'
      end do
      write(draw_unit, '(A)') '}'
      write(draw_unit, '(A)') '</script>'
      write(draw_unit, '(A)') '</head>'
      write(draw_unit, '(A)') '<body onload="drawCanvases(1.0);">'
      write(draw_unit, '(A)') ' <form>'

      ixtemp = vdevice%sizeSx / CANVASTORAS
      iytemp = vdevice%sizeSy / CANVASTORAS

      write(draw_unit, '(A, I0, A, I0, A)') &
         '  <input type="button" value="fit width"  onclick="drawCanvases(-', ixtemp, ');drawCanvases(window.innerWidth/', ixtemp, ')" />'
      write(draw_unit, '(A, I0, A, I0, A)') &
         '  <input type="button" value="fit height" onclick="drawCanvases(-', ixtemp, ');drawCanvases(window.innerHeight/', iytemp, ')" />'
      write(draw_unit, '(A)') '  <input type="button" value="stamps"     onclick="drawCanvases(-100)" />'
      write(draw_unit, '(A)') '  <input type="button" value="smaller"    onclick="drawCanvases(0.80)" />'
      write(draw_unit, '(A)') '  <input type="button" value="bigger"     onclick="drawCanvases(1.20)" />'
      write(draw_unit, '(A)') ' </form>'

      do ipages = 1, pgroup

write(draw_unit, '(A, I0, A)') '<canvas id="canvas_id', ipages, '"'
write(draw_unit, '(A, I0, A)') ' onclick="resizecanvas(this.id,1.1);drawCanvas', ipages, '(this.id);"'
write(draw_unit, '(A, I0, A, I0, A)') ' onkeypress="resizecanvas(this.id,', ixtemp, '/this.width);drawCanvas', ipages, '(this.id);"'
write(draw_unit, '(A, I0, A, I0, A)') ' ondblclick="resizecanvas(this.id,', ixtemp, '/this.width);drawCanvas', ipages, '(this.id);"'
write(draw_unit, '(A, I0, A)') ' width="', ixtemp, '"'
write(draw_unit, '(A, I0, A)') ' height="', iytemp, '"'
write(draw_unit, '(A)') '>The CANVAS element is not displayed by this browser'
end do
write(draw_unit, '(A)') ''
write(draw_unit, '(A)') '</body>'
write(draw_unit, '(A)') '</html>'
write(draw_unit, '(A)') '<!-- End of Document -->'

drawn = 0; points = 0
if (draw_unit /= output_unit .and. draw_unit /= error_unit) then
   flush(draw_unit)
   close(draw_unit)
end if
CANVAS_exit = 0
end function CANVAS_exit
integer function CANVAS_draw(x, y)
integer, intent(in) :: x, y
if (LAST_X /= vdevice%cpVx .or. LAST_Y /= vdevice%cpVy) then
call closeline()
call openObject()
call openline()
write(draw_unit, '(A, I0, ",", I0)', advance='no') "A=[", vdevice%cpVx, FLIPY(vdevice%cpVy)
LAST_X = vdevice%cpVx; LAST_Y = vdevice%cpVy
CANVAS_MOVED = 0; points = 1
end if
call openline()
if (points == 0) then
write(draw_unit, '(A, I0, ",", I0)', advance='no') "A=[", x, FLIPY(y)
CANVAS_MOVED = 0
else
if (LAST_X /= x .or. LAST_Y /= y) CANVAS_MOVED = CANVAS_MOVED + 1
if (mod(points, 8) == 7) then
write(draw_unit, '(A, I0, ",", I0)') ",", x, FLIPY(y)
else
write(draw_unit, '(A, I0, ",", I0)', advance='no') ",", x, FLIPY(y)
end if
end if
points = points + 1
LAST_X = x; LAST_Y = y; drawn = 1
CANVAS_draw = 0
end function CANVAS_draw
integer function CANVAS_clear()
call closeline()
call closeObject()
if (drawn /= 0) then
write(draw_unit, '(A)') 'X.restore();'
write(draw_unit, '(A)') '}else{'
write(draw_unit, '(A)') "alert('Your browser needs HTML CANVAS support to view this page');"
write(draw_unit, '(A)') '}'
write(draw_unit, '(A, I0, A)') '}// '
pgroup = pgroup + 1
write(draw_unit, '(A, I0, A)') 'function drawCanvas', pgroup, '(canvasname){'
write(draw_unit, '(A, I0)') 'var canvas = document.getElementById(canvasname); // page ', pgroup
write(draw_unit, '(A)') 'if(canvas.getContext){'
write(draw_unit, '(A)') 'var X = canvas.getContext("2d");'
write(draw_unit, '(A)') 'X.save();'
write(draw_unit, '(A, I0, A, I0, A)') 'X.scale(canvas.width/', vdevice%sizeSx, ',canvas.height/', vdevice%sizeSy, ');'
write(draw_unit, '(A)') "X.lineCap='round';"
write(draw_unit, '(A)') "X.lineJoin='round';"
write(draw_unit, '(A, I0, A)') 'X.lineWidth=', max(1, vdevice%sizeX * curwid / 10000), ';'
write(draw_unit, '(A, 3Z2.2, A)') 'X.fillStyle="#', &
canvas_carr(curcol+1)%red, canvas_carr(curcol+1)%green, canvas_carr(curcol+1)%blue, '";'
write(draw_unit, '(A, 3Z2.2, A)') 'X.strokeStyle="#', &
canvas_carr(curcol+1)%red, canvas_carr(curcol+1)%green, canvas_carr(curcol+1)%blue, '";'
end if
drawn = 0; points = 0; CANVAS_MOVED = 0
CANVAS_clear = 0
end function CANVAS_clear
integer function CANVAS_setlw(width)
integer, intent(in) :: width
call closeline()
call closeObject()
if (width >= 0) curwid = width
write(draw_unit, '(A, I0, A)') 'X.lineWidth=', max(1, vdevice%sizeX * curwid / 10000), ';'
CANVAS_setlw = 0
end function CANVAS_setlw
integer function CANVAS_color(col)
integer, intent(in) :: col
integer :: idum
call closeline()
call closeObject()
if (col < 0) then
idum = CANVAS_setlw(abs(col))
else
curpat = col / CMAPSIZE
curcol = mod(col, CMAPSIZE)
end if
write(draw_unit, '(A, 3Z2.2, A)') 'X.fillStyle="#', &
canvas_carr(curcol+1)%red, canvas_carr(curcol+1)%green, canvas_carr(curcol+1)%blue, '";'
write(draw_unit, '(A, 3Z2.2, A)') 'X.strokeStyle="#', &
canvas_carr(curcol+1)%red, canvas_carr(curcol+1)%green, canvas_carr(curcol+1)%blue, '";'
CANVAS_color = 0
end function CANVAS_color
integer function CANVAS_font(fontname)
character(len=*), intent(in) :: fontname
call draw_font("futura.m")
CANVAS_font = 0
end function CANVAS_font
integer function CANVAS_string(s)
character(len=*), intent(in) :: s
CANVAS_string = 0
end function CANVAS_string
integer function CANVAS_char(c)
character(len=1), intent(in) :: c
CANVAS_char = 0
end function CANVAS_char
integer function CANVAS_fill(n, x, y)
integer, intent(in) :: n
integer, intent(in) :: x(n), y(n)
integer :: i
call closeline()
call closeObject()
write(draw_unit, '(A)') '// Polygon'
write(draw_unit, '(A, 3Z2.2, A)') 'X.fillStyle="#', &
canvas_carr(curcol+1)%red, canvas_carr(curcol+1)%green, canvas_carr(curcol+1)%blue, '";'
write(draw_unit, '(A, 3Z2.2, A)') 'X.strokeStyle="#', &
canvas_carr(curcol+1)%red, canvas_carr(curcol+1)%green, canvas_carr(curcol+1)%blue, '";'
write(draw_unit, '(A, I0, A)') 'X.lineWidth=', max(1, vdevice%sizeX * curwid / 10000), ';'
write(draw_unit, '(A, I0, ",", I0, 1X)', advance='no') 'A=[', x(1), FLIPY(y(1))
do i = 2, n
if (mod(i - 1, 8) == 7) then
write(draw_unit, '(A, I0, ",", I0)') ',', x(i), FLIPY(y(i))
else
write(draw_unit, '(A, I0, ",", I0)', advance='no') ',', x(i), FLIPY(y(i))
end if
end do
write(draw_unit, '(A, I0, ",", I0, A)') ' ,', x(1), FLIPY(y(1)), '];f(X,A)'
vdevice%cpVx = x(n); vdevice%cpVy = y(n)
LAST_X = vdevice%cpVx; LAST_Y = vdevice%cpVy
drawn = 1
CANVAS_fill = 0
end function CANVAS_fill
integer function noop(); noop = -1; end function
integer function noop2(x, y); integer, intent(out) :: x, y; x = 0; y = 0; noop2 = -1; end function
integer function CANVAS_draw_devcpy()
vdevice%dev%devname = "canvas"
vdevice%dev%large = "large"
vdevice%dev%small = "small"
vdevice%dev%Vbackb => noop
vdevice%dev%Vchar => CANVAS_char
vdevice%dev%Vcheckkey => noop
vdevice%dev%Vclear => CANVAS_clear
vdevice%dev%Vcolor => CANVAS_color
vdevice%dev%Vdraw => CANVAS_draw
vdevice%dev%Vexit => CANVAS_exit
vdevice%dev%Vfill => CANVAS_fill
vdevice%dev%Vfont => CANVAS_font
vdevice%dev%Vfrontb => noop
vdevice%dev%Vgetkey => noop
vdevice%dev%Vinit => CANVAS_init
vdevice%dev%Vlocator => noop2
vdevice%dev%Vmapcolor => CANVAS_mapcolor
vdevice%dev%Vsetlw => CANVAS_setlw
vdevice%dev%Vstring => CANVAS_string
vdevice%dev%Vswapb => noop
vdevice%dev%Vsync => noop
CANVAS_draw_devcpy = 0
end function CANVAS_draw_devcpy
end module m_draw_drv_canvas
