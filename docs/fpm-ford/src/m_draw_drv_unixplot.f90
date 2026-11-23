module m_draw_drv_unixplot
   use, intrinsic :: iso_fortran_env, only: real32, int16, output_unit, error_unit
   use m_draw_types
   use m_draw_drivers
   implicit none
   private
   public :: UNIXPLOT_draw_devcpy

   integer, parameter :: SPACE_SIZE = 1024
   integer, parameter :: MAXCOLOR   = 7

   integer, save :: uplot_first_time = 1
   integer, save :: drawn            = 0
   integer, save :: uplotlstx = -1, uplotlsty = -1

contains

   subroutine putpt(x, y)
      integer, intent(in) :: x, y
      integer(int16) :: sx, sy
      sx = int(x, int16)
      sy = int(y, int16)
      write(draw_unit, '(4A1)', advance='no') &
         char(iand(int(sx), 255)), char(iand(ishft(int(sx), -8), 255)), &
         char(iand(int(sy), 255)), char(iand(ishft(int(sy), -8), 255))
   end subroutine putpt

   integer function uplot_init()
      draw_unit = draw_outfile()
      if (uplot_first_time == 0) then
         uplot_init = 1; return
      end if
      write(draw_unit, '(A1)', advance='no') 's'
      call putpt(0, 0)
      call putpt(SPACE_SIZE, SPACE_SIZE)

      vdevice%sizeSx = SPACE_SIZE; vdevice%sizeSy = SPACE_SIZE
      vdevice%sizeX  = SPACE_SIZE; vdevice%sizeY  = SPACE_SIZE
      vdevice%depth  = 1
      uplot_init = 1
   end function uplot_init

   integer function uplot_exit()
      flush(draw_unit)
      if (draw_unit /= output_unit .and. draw_unit /= error_unit) close(draw_unit)
      uplot_exit = 0
   end function uplot_exit

   integer function uplot_draw(x, y)
      integer, intent(in) :: x, y
      if (uplotlstx /= vdevice%cpVx .or. uplotlsty /= vdevice%cpVy) then
         write(draw_unit, '(A1)', advance='no') 'm'
         call putpt(vdevice%cpVx, vdevice%cpVy)
      end if
      write(draw_unit, '(A1)', advance='no') 'n'
      call putpt(x, y)
      uplotlstx = x; uplotlsty = y; drawn = 1
      uplot_draw = 0
   end function uplot_draw

   integer function uplot_font(font)
      character(len=*), intent(in) :: font
      if (trim(font) == "large" .or. trim(font) == "small") then
         vdevice%hwidth = 1.0_real32
         vdevice%hheight = 1.0_real32
         uplot_font = 1
      else
         uplot_font = 0
      end if
   end function uplot_font

   integer function uplot_clear()
      if (drawn /= 0) write(draw_unit, '(A1)', advance='no') 'e'
      drawn = 0
      uplot_clear = 0
   end function uplot_clear

   integer function uplot_color(col)
      integer, intent(in) :: col
      uplot_color = 0
   end function uplot_color

   integer function uplot_char(c)
      character(len=1), intent(in) :: c
      if (uplotlstx /= vdevice%cpVx .or. uplotlsty /= vdevice%cpVy) then
         write(draw_unit, '(A1)', advance='no') 'm'
         call putpt(vdevice%cpVx, vdevice%cpVy)
      end if
      write(draw_unit, '(A1, A1)') 't', c
      drawn = 1; uplotlstx = -1; uplotlsty = -1
      uplot_char = 0
   end function uplot_char

   integer function uplot_string(s)
      character(len=*), intent(in) :: s
      if (uplotlstx /= vdevice%cpVx .or. uplotlsty /= vdevice%cpVy) then
         write(draw_unit, '(A1)', advance='no') 'm'
         call putpt(vdevice%cpVx, vdevice%cpVy)
      end if
      write(draw_unit, '(A1, A)') 't', s
      drawn = 1; uplotlstx = -1; uplotlsty = -1
      uplot_string = 0
   end function uplot_string

   integer function uplot_fill(n, x, y)
      integer, intent(in) :: n
      integer, intent(in) :: x(n), y(n)
      integer :: i

      write(draw_unit, '(A1)', advance='no') 'm'
      call putpt(x(1), y(1))
      do i = 2, n
         write(draw_unit, '(A1)', advance='no') 'n'
         call putpt(x(i), y(i))
      end do
      write(draw_unit, '(A1)', advance='no') 'n'
      call putpt(x(1), y(1))

      vdevice%cpVx = x(n); vdevice%cpVy = y(n)
      uplotlstx = -1; uplotlsty = -1
      uplot_fill = 0
   end function uplot_fill

   integer function noop(); noop = -1; end function
   integer function noop1(x); integer, intent(in) :: x; noop1 = -1; end function
   integer function noop2(x, y); integer, intent(out) :: x, y; x = 0; y = 0; noop2 = -1; end function
   integer function noop4(a, b, c, d); integer, intent(in) :: a, b, c, d; noop4 = -1; end function

   integer function UNIXPLOT_draw_devcpy()
      vdevice%dev%devname   = "unixplot"
      vdevice%dev%large     = "large"
      vdevice%dev%small     = "small"
      vdevice%dev%Vbackb    => noop
      vdevice%dev%Vchar     => uplot_char
      vdevice%dev%Vcheckkey => noop
      vdevice%dev%Vclear    => uplot_clear
      vdevice%dev%Vcolor    => uplot_color
      vdevice%dev%Vdraw     => uplot_draw
      vdevice%dev%Vexit     => uplot_exit
      vdevice%dev%Vfill     => uplot_fill
      vdevice%dev%Vfont     => uplot_font
      vdevice%dev%Vfrontb   => noop
      vdevice%dev%Vgetkey   => noop
      vdevice%dev%Vinit     => uplot_init
      vdevice%dev%Vlocator  => noop2
      vdevice%dev%Vmapcolor => noop4
      vdevice%dev%Vsetlw    => noop1
      vdevice%dev%Vstring   => uplot_string
      vdevice%dev%Vswapb    => noop
      vdevice%dev%Vsync     => noop
      UNIXPLOT_draw_devcpy = 0
   end function UNIXPLOT_draw_devcpy

end module m_draw_drv_unixplot
