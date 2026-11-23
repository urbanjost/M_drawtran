module m_draw_drv_nil
   use, intrinsic :: iso_fortran_env, only: real32
   use m_draw_types
   use m_draw_pref
   implicit none
   private
   public :: NIL_draw_devcpy

   integer, parameter :: NIL_XSIZE = 8500
   integer, parameter :: NIL_YSIZE = 11000
   integer, save :: NIL_lastx = -1, NIL_lasty = -1

contains

   integer function NIL_mapcolor(i, r, g, b)
      integer, intent(in) :: i, r, g, b
      NIL_mapcolor = 0
   end function NIL_mapcolor

   subroutine NIL_RESIZE()
      integer :: prefx, prefy, prefxs, prefys
      call draw_getprefposandsize(prefx, prefy, prefxs, prefys)
      if (prefxs /= -1) then
         vdevice%sizeSy = prefys
         vdevice%sizeSx = prefxs
         vdevice%sizeX = min(prefys, prefxs)
         vdevice%sizeY = vdevice%sizeX
      else
         vdevice%sizeSy = NIL_YSIZE
         vdevice%sizeSx = NIL_XSIZE
      end if
   end subroutine NIL_RESIZE

   integer function NIL_init()
      call NIL_RESIZE()
      vdevice%depth = 8
      vdevice%sizeX = min(NIL_XSIZE, NIL_YSIZE)
      vdevice%sizeY = vdevice%sizeX
      NIL_init = 1
   end function NIL_init

   integer function NIL_exit()
      NIL_exit = 0
   end function NIL_exit

   integer function NIL_draw(x, y)
      integer, intent(in) :: x, y
      NIL_lastx = x
      NIL_lasty = y
      NIL_draw = 0
   end function NIL_draw

   integer function NIL_clear()
      call NIL_RESIZE()
      NIL_clear = 0
   end function NIL_clear

   integer function NIL_color(col)
      integer, intent(in) :: col
      NIL_color = 0
   end function NIL_color

   integer function NIL_setlw(width)
      integer, intent(in) :: width
      NIL_setlw = 0
   end function NIL_setlw

   integer function NIL_font(font)
      character(len=*), intent(in) :: font
      if (trim(font) == "small") then
         vdevice%hwidth = 12.0_real32 / 72.0_real32 / 0.03937_real32
         vdevice%hheight = vdevice%hwidth * 2.2_real32
      else if (trim(font) == "large") then
         vdevice%hheight = 55.0_real32 / 72.0_real32 / 0.03937_real32
         vdevice%hwidth = vdevice%hheight / 4.5_real32
      else
         NIL_font = 0
         return
      end if
      NIL_font = 1
   end function NIL_font

   integer function NIL_string(s)
      character(len=*), intent(in) :: s
      NIL_lastx = -1; NIL_lasty = -1
      NIL_string = 0
   end function NIL_string

   integer function NIL_char(c)
      character(len=1), intent(in) :: c
      NIL_lastx = -1; NIL_lasty = -1
      NIL_char = 0
   end function NIL_char

   integer function NIL_fill(n, x, y)
      integer, intent(in) :: n
      integer, intent(in) :: x(n), y(n)
      vdevice%cpVx = x(n)
      vdevice%cpVy = y(n)
      NIL_lastx = -1; NIL_lasty = -1
      NIL_fill = 0
   end function NIL_fill

   integer function noop(); noop = -1; end function
   integer function noop2(x, y); integer, intent(out) :: x, y; x = 0; y = 0; noop2 = -1; end function

   integer function NIL_draw_devcpy()
      vdevice%dev%devname   = "nil"
      vdevice%dev%large     = "large"
      vdevice%dev%small     = "small"
      vdevice%dev%Vbackb    => noop
      vdevice%dev%Vchar     => NIL_char
      vdevice%dev%Vcheckkey => noop
      vdevice%dev%Vclear    => NIL_clear
      vdevice%dev%Vcolor    => NIL_color
      vdevice%dev%Vdraw     => NIL_draw
      vdevice%dev%Vexit     => NIL_exit
      vdevice%dev%Vfill     => NIL_fill
      vdevice%dev%Vfont     => NIL_font
      vdevice%dev%Vfrontb   => noop
      vdevice%dev%Vgetkey   => noop
      vdevice%dev%Vinit     => NIL_init
      vdevice%dev%Vlocator  => noop2
      vdevice%dev%Vmapcolor => NIL_mapcolor
      vdevice%dev%Vsetlw    => NIL_setlw
      vdevice%dev%Vstring   => NIL_string
      vdevice%dev%Vswapb    => noop
      vdevice%dev%Vsync     => noop
      NIL_draw_devcpy = 0
   end function NIL_draw_devcpy

end module m_draw_drv_nil
