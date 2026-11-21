module m_draw_drv_hpgt
   use, intrinsic :: iso_fortran_env, only: real32, input_unit, output_unit
   use, intrinsic :: iso_c_binding, only: c_int, c_char
   use m_draw_types
   use m_draw_drivers
   implicit none
   private
   public :: HPGT_draw_devcpy

   integer, parameter :: HPGT_X_SIZE = 640
   integer, parameter :: HPGT_Y_SIZE = 400
   character(len=1), parameter :: ESC = char(27)

   integer, save :: click = 0, tlstx = -1, tlsty = -1

   interface
      integer(c_int) function c_system(cmd) bind(C, name="system")
         import :: c_int, c_char
         character(kind=c_char), intent(in) :: cmd(*)
      end function c_system
   end interface

contains

   integer function HPGT_font(font)
      character(len=*), intent(in) :: font
      integer :: sz

      vdevice%hwidth = 5.0_real32; vdevice%hheight = 7.0_real32
      sz = 2
      select case (trim(font))
      case ("5x7");            sz = 1
      case ("10x14", "small"); sz = 2
      case ("15x21", "large"); sz = 3
      case ("20x28");          sz = 4
      case ("25x35");          sz = 5
      case ("30x42");          sz = 6
      case ("35x49");          sz = 7
      case ("40x56");          sz = 8
      end select

      vdevice%hwidth  = vdevice%hwidth  * real(sz, real32)
      vdevice%hheight = vdevice%hheight * real(sz, real32)
      write(draw_unit, '(A1, A, I0, A)', advance='no') ESC, "*m", sz, "m"
      tlstx = -1; tlsty = -1
      flush(draw_unit)
      HPGT_font = 1
   end function HPGT_font

   integer function HPGT_init()
      integer :: idum
      click = 0
      draw_unit = draw_outfile()
      vdevice%depth = 3

      write(draw_unit, '(A1, A)', advance='no') ESC, "*dC"
      write(draw_unit, '(A1, A)', advance='no') ESC, "*dF"
      write(draw_unit, '(A1, A)', advance='no') ESC, "*m1g"
      write(draw_unit, '(A1, A)', advance='no') ESC, "*dA"

      vdevice%sizeSx = HPGT_X_SIZE; vdevice%sizeSy = HPGT_Y_SIZE
      vdevice%sizeX  = HPGT_Y_SIZE; vdevice%sizeY  = HPGT_Y_SIZE
      tlstx = -1; tlsty = -1
      idum = HPGT_font("5x7")
      flush(draw_unit)
      HPGT_init = 1
   end function HPGT_init

   integer function HPGT_exit()
      write(draw_unit, '(A1, A, A1, A)', advance='no') ESC, "*dD", ESC, "*dE"
      HPGT_exit = 0
   end function HPGT_exit

   integer function HPGT_draw(x, y)
      integer, intent(in) :: x, y
      write(draw_unit, '(A1, A, I0, ",", I0, 1X, I0, ",", I0, A)', advance='no') &
         ESC, "*pa", vdevice%cpVx, vdevice%cpVy, x, y, "Z"
      tlstx = x; tlsty = y
      HPGT_draw = 0
   end function HPGT_draw

   integer function HPGT_getkey()
      character(len=1) :: c
      integer :: ios, idum
      flush(draw_unit)
      idum = c_system(c_char_"stty raw -echo" // char(0))
      read(input_unit, '(A1)', iostat=ios) c
      idum = c_system(c_char_"stty -raw echo" // char(0))
      HPGT_getkey = merge(-1, ichar(c), ios /= 0)
   end function HPGT_getkey

   integer function HPGT_locator(x, y)
      integer, intent(out) :: x, y
      integer :: c, ios
      character(len=50) :: buf

      x = 0; y = 0
      if (click /= 0) then
         click = 0; HPGT_locator = 0; return
      end if
      click = 1; c = -1
      if (draw_unit == output_unit) then
         write(draw_unit, '(A1, A)', advance='no') ESC, "*s4^"
         flush(draw_unit)
         read(input_unit, '(A)', iostat=ios) buf
         if (ios == 0) read(buf, *, iostat=ios) x, y, c
      end if
      HPGT_locator = ishft(1, c - ichar('1'))
   end function HPGT_locator

   integer function HPGT_clear()
      write(draw_unit, '(A1, A)', advance='no') ESC, "*dA"
      tlstx = -1; tlsty = -1
      HPGT_clear = 0
   end function HPGT_clear

   integer function HPGT_color(i_in)
      integer, intent(in) :: i_in
      integer :: i
      if (vdevice%depth == 1) then
         HPGT_color = 0; return
      end if
      i = mod(i_in, 8)
      write(draw_unit, '(A1, A, I0, A, A1, A, I0, A, A1, A, I0, A)', advance='no') &
         ESC, "*m", i, "x", ESC, "*n", i, "x", ESC, "*e", i, "x"
      flush(draw_unit)
      HPGT_color = 0
   end function HPGT_color

   integer function HPGT_char(c)
      character(len=1), intent(in) :: c
      if (tlstx /= vdevice%cpVx .or. tlsty /= vdevice%cpVy) then
         write(draw_unit, '(A1, A, I0, ",", I0, A)', advance='no') ESC, "*pa", vdevice%cpVx, vdevice%cpVy, "Z"
      end if
      write(draw_unit, '(A1, A, A1, A1)', advance='no') ESC, "*l", c, char(13)
      vdevice%cpVx = vdevice%cpVx + int(vdevice%hwidth)
      tlstx = vdevice%cpVx
      flush(draw_unit)
      HPGT_char = 0
   end function HPGT_char

   integer function HPGT_string(s)
      character(len=*), intent(in) :: s
      if (tlstx /= vdevice%cpVx .or. tlsty /= vdevice%cpVy) then
         write(draw_unit, '(A1, A, I0, ",", I0, A)', advance='no') ESC, "*pa", vdevice%cpVx, vdevice%cpVy, "Z"
      end if
      write(draw_unit, '(A1, A, A, A1)', advance='no') ESC, "*l", s, char(13)
      flush(draw_unit)
      vdevice%cpVx = vdevice%cpVx + len(s) * int(vdevice%hwidth)
      tlstx = vdevice%cpVx
      HPGT_string = 0
   end function HPGT_string

   integer function HPGT_fill(n, x, y)
      integer, intent(in) :: n
      integer, intent(in) :: x(n), y(n)
      integer :: i
      write(draw_unit, '(A1, A)', advance='no') ESC, "*pas"
      do i = 1, n
         write(draw_unit, '(I0, ",", I0, 1X)', advance='no') x(i), y(i)
      end do
      write(draw_unit, '(A1)', advance='no') 'T'
      flush(draw_unit)
      HPGT_fill = 0
   end function HPGT_fill

   integer function HPGT_sync()
      flush(draw_unit)
      HPGT_sync = 0
   end function HPGT_sync

   integer function noop(); noop = -1; end function
   integer function noop1(x); integer, intent(in) :: x; noop1 = -1; end function
   integer function noop4(a, b, c, d); integer, intent(in) :: a, b, c, d; noop4 = -1; end function

   integer function HPGT_draw_devcpy()
      vdevice%dev%devname   = "hpgt"
      vdevice%dev%large     = "large"
      vdevice%dev%small     = "small"
      vdevice%dev%Vbackb    => noop
      vdevice%dev%Vchar     => HPGT_char
      vdevice%dev%Vcheckkey => noop
      vdevice%dev%Vclear    => HPGT_clear
      vdevice%dev%Vcolor    => HPGT_color
      vdevice%dev%Vdraw     => HPGT_draw
      vdevice%dev%Vexit     => HPGT_exit
      vdevice%dev%Vfill     => HPGT_fill
      vdevice%dev%Vfont     => HPGT_font
      vdevice%dev%Vfrontb   => noop
      vdevice%dev%Vgetkey   => HPGT_getkey
      vdevice%dev%Vinit     => HPGT_init
      vdevice%dev%Vlocator  => HPGT_locator
      vdevice%dev%Vmapcolor => noop4
      vdevice%dev%Vsetlw    => noop1
      vdevice%dev%Vstring   => HPGT_string
      vdevice%dev%Vswapb    => noop
      vdevice%dev%Vsync     => HPGT_sync
      HPGT_draw_devcpy = 0
   end function HPGT_draw_devcpy

end module m_draw_drv_hpgt
