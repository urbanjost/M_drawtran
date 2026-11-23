module m_draw_drv_fti
   use, intrinsic :: iso_fortran_env, only: real32, output_unit, error_unit
   use m_draw_types
   use m_draw_drivers
   implicit none
   private
   public :: FTI_draw_devcpy

   integer, save :: GLOBAL_color = 0
   integer, save :: GLOBAL_lastx = -1111111
   integer, save :: GLOBAL_lasty = -1111111

contains

   integer function FTI_color(col)
      integer, intent(in) :: col
      integer :: icol

      GLOBAL_color = abs(mod(col, 256))
      if (GLOBAL_color > 15) then
         icol = -GLOBAL_color
      else
         icol = GLOBAL_color
      end if

      select case (icol)
      case (0)
         write(draw_unit, '(A)') "color(iconcolor);"
      case (7)
         write(draw_unit, '(A)') "color(outlinecolor);"
      case default
         write(draw_unit, '(A, I0, A)') "color(", icol, ");"
      end select
      FTI_color = 0
   end function FTI_color

   integer function FTI_init()
      vdevice%sizeSy = 1000
      vdevice%sizeSx = 1000
      vdevice%sizeX = 1000
      vdevice%sizeY = 1000
      vdevice%depth = 1
      draw_unit = draw_outfile()
      GLOBAL_lastx = -1111111
      GLOBAL_lasty = -1111111

      write(draw_unit, '(A)') "#FTI"
      write(draw_unit, '(A)') "# CREATOR: M_DRAW FTI driver; version 1.0 1997/08/02"
      write(draw_unit, '(A)') "# Version 1: 19970802, Author: John S. Urban"
      write(draw_unit, '(A)') "color(outlinecolor);"
      FTI_init = 1
   end function FTI_init

   integer function FTI_fill(n, x, y)
      integer, intent(in) :: n
      integer, intent(in) :: x(n), y(n)
      integer :: i

      write(draw_unit, '(A)') "bgnpolygon();"
      do i = 1, n
         write(draw_unit, '(A, F0.4, A, F0.4, A)') "   vertex(", x(i)/10.0_real32, ",", y(i)/10.0_real32, ");"
      end do
      write(draw_unit, '(A)') "endpolygon();"

      GLOBAL_lastx = x(n); vdevice%cpVx = x(n)
      GLOBAL_lasty = y(n); vdevice%cpVy = y(n)
      FTI_fill = UNUSED
   end function FTI_fill

   integer function FTI_draw(x, y)
      integer, intent(in) :: x, y
      if (GLOBAL_lastx /= vdevice%cpVx .or. GLOBAL_lasty /= vdevice%cpVy) then
         GLOBAL_lastx = vdevice%cpVx
         GLOBAL_lasty = vdevice%cpVy
         write(draw_unit, '(A, F0.4, A, F0.4, A)') "move(", GLOBAL_lastx/10.0_real32, ",", GLOBAL_lasty/10.0_real32, ");"
      end if
      write(draw_unit, '(A, F0.4, A, F0.4, A)') "draw(", x/10.0_real32, ",", y/10.0_real32, ");"
      FTI_draw = UNUSED
   end function FTI_draw

   integer function FTI_exit()
      flush(draw_unit)
      if (draw_unit /= output_unit .and. draw_unit /= error_unit) then
         write(draw_unit, '(A)') "# End of FTI File"
         flush(draw_unit)
         close(draw_unit)
      end if
      FTI_exit = UNUSED
   end function FTI_exit

   integer function FTI_clear()
      write(draw_unit, '(A)') "# End of FTI Page"
      FTI_clear = UNUSED
   end function FTI_clear

   integer function FTI_font(font)
      character(len=*), intent(in) :: font
      write(error_unit, '(A)') "error: NO HARDWARE FONT"
      if (trim(font) == "small") then
         vdevice%hwidth = 97.01_real32
         vdevice%hheight = vdevice%hwidth * 2.0_real32
      else if (trim(font) == "large") then
         vdevice%hwidth = 145.5_real32
         vdevice%hheight = vdevice%hwidth * 2.0_real32
      else
         FTI_font = 0; return
      end if
      FTI_font = 1
   end function FTI_font

   integer function FTI_string(s)
      character(len=*), intent(in) :: s
      if (GLOBAL_lastx /= vdevice%cpVx .or. GLOBAL_lasty /= vdevice%cpVy) then
         GLOBAL_lastx = vdevice%cpVx
         GLOBAL_lasty = vdevice%cpVy
      end if
      write(draw_unit, '(A, A)') "# ", trim(s)
      GLOBAL_lastx = -1111111
      GLOBAL_lasty = -1111111
      FTI_string = UNUSED
   end function FTI_string

   integer function FTI_char(c)
      character(len=1), intent(in) :: c
      integer :: idum
      idum = FTI_string(c)
      FTI_char = UNUSED
   end function FTI_char

   integer function noop(); noop = -1; end function
   integer function noop1(x); integer, intent(in) :: x; noop1 = -1; end function
   integer function noop2(x, y); integer, intent(out) :: x, y; x = 0; y = 0; noop2 = -1; end function
   integer function noop4(a, b, c, d); integer, intent(in) :: a, b, c, d; noop4 = -1; end function

   integer function FTI_draw_devcpy()
      vdevice%dev%devname   = "FTI"
      vdevice%dev%large     = "large"
      vdevice%dev%small     = "small"
      vdevice%dev%Vbackb    => noop
      vdevice%dev%Vchar     => FTI_char
      vdevice%dev%Vcheckkey => noop
      vdevice%dev%Vclear    => FTI_clear
      vdevice%dev%Vcolor    => FTI_color
      vdevice%dev%Vdraw     => FTI_draw
      vdevice%dev%Vexit     => FTI_exit
      vdevice%dev%Vfill     => FTI_fill
      vdevice%dev%Vfont     => FTI_font
      vdevice%dev%Vfrontb   => noop
      vdevice%dev%Vgetkey   => noop
      vdevice%dev%Vinit     => FTI_init
      vdevice%dev%Vlocator  => noop2
      vdevice%dev%Vmapcolor => noop4
      vdevice%dev%Vsetlw    => noop1
      vdevice%dev%Vstring   => FTI_string
      vdevice%dev%Vswapb    => noop
      vdevice%dev%Vsync     => noop
      vdevice%dev%Vinit     => FTI_init
      FTI_draw_devcpy = UNUSED
   end function FTI_draw_devcpy

end module m_draw_drv_fti
