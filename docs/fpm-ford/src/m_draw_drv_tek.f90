module m_draw_drv_tek
   use, intrinsic :: iso_fortran_env, only: real32, input_unit, output_unit, error_unit
   use, intrinsic :: iso_c_binding, only: c_int, c_char
   use m_draw_types
   use m_draw_drivers
   implicit none
   private
   public :: TEK_draw_devcpy, XTEK_draw_devcpy

   integer, parameter :: MASK       = 31   ! 037 octal
   character(len=1), parameter :: BELL = char(7)
   character(len=1), parameter :: FF   = char(12)
   character(len=1), parameter :: CAN  = char(24)
   character(len=1), parameter :: SUB  = char(26)
   character(len=1), parameter :: ESC  = char(27)
   character(len=1), parameter :: GS   = char(29)
   character(len=1), parameter :: US   = char(31)
   character(len=1), parameter :: ETX  = char(3)

   integer, parameter :: TEK_X_SIZE = 1023
   integer, parameter :: TEK_Y_SIZE = 767

   integer, save :: LoYold = -1, HiYold = -1, HiXold = -1
   integer, save :: tlstx = -1, tlsty = -1
   integer, save :: click   = 0
   integer, save :: xterm   = -1
   integer, save :: PAUSE   = 2560
   integer, save :: TEKMODE = -1

   interface
      integer(c_int) function c_usleep(usec) bind(C, name="usleep")
         import :: c_int
         integer(c_int), value :: usec
      end function c_usleep

      integer(c_int) function c_system(cmd) bind(C, name="system")
         import :: c_int, c_char
         character(kind=c_char), intent(in) :: cmd(*)
      end function c_system
   end interface

contains

   subroutine XTERM_4010()
      integer :: i
      if (draw_unit /= output_unit .and. draw_unit /= error_unit) return
      if (TEKMODE == 1) return
      flush(draw_unit)
      write(draw_unit, '(6A1)', advance='no') ESC, '[', '?', '3', '8', 'h'
      flush(draw_unit)
      do i = 1, PAUSE - 1
         write(draw_unit, '(A1)', advance='no') char(0)
      end do
      flush(draw_unit)
      TEKMODE = 1
   end subroutine XTERM_4010

   subroutine XTERM_vt102()
      integer :: i
      if (draw_unit /= output_unit .and. draw_unit /= error_unit) return
      if (TEKMODE == 0) return
      flush(draw_unit)
      write(draw_unit, '(2A1)', advance='no') ESC, ETX
      flush(draw_unit)
      do i = 1, PAUSE - 1
         write(draw_unit, '(A1)', advance='no') char(0)
      end do
      flush(draw_unit)
      TEKMODE = 0
   end subroutine XTERM_vt102

   integer function TEK_init()
      character(len=64) :: varname
      integer :: env_len, ios

      vdevice%depth = 1
      draw_unit = draw_outfile()
      write(draw_unit, '(A1)', advance='no') GS

      vdevice%sizeX  = TEK_Y_SIZE; vdevice%sizeY = TEK_Y_SIZE
      vdevice%sizeSx = TEK_X_SIZE; vdevice%sizeSy = TEK_Y_SIZE
      tlstx = -1; tlsty = -1; TEKMODE = -2

      call get_environment_variable("VTEKPAUSE", value=varname, length=env_len, status=ios)
      if (ios == 0 .and. env_len > 0) then
         read(varname, *, iostat=ios) PAUSE
         if (ios /= 0) PAUSE = 2560
      else
         PAUSE = 2560
      end if
      write(error_unit, '(A, I0)') "*TEK_init* PAUSE=", PAUSE
      TEK_init = 1
   end function TEK_init

   integer function TEK_exit()
      if (xterm == 0) call XTERM_4010()
      write(draw_unit, '(2A1)', advance='no') US, CAN
      TEKMODE = -3
      if (xterm == 0) call XTERM_vt102()
      flush(draw_unit)
      if (draw_unit /= output_unit .and. draw_unit /= error_unit) close(draw_unit)
      TEKMODE = -3
      TEK_exit = UNUSED
   end function TEK_exit

   subroutine out_bytes(xin, yin)
      integer, intent(in) :: xin, yin
      integer :: x, y, HiY, LoY, HiX, LoX

      x = max(0, min(1023, xin))
      y = max(0, min(1023, yin))

      HiY = y / 32 + 32
      LoY = mod(y, 32) + 96
      HiX = x / 32 + 32
      LoX = mod(x, 32) + 64

      if (HiYold /= HiY) then
         write(draw_unit, '(A1)', advance='no') char(HiY)
         HiYold = HiY
      end if
      if (LoYold /= LoY .or. HiXold /= HiX) then
         write(draw_unit, '(A1)', advance='no') char(LoY)
         LoYold = LoY
         if (HiXold /= HiX) then
            write(draw_unit, '(A1)', advance='no') char(HiX)
            HiXold = HiX
         end if
      end if
      write(draw_unit, '(A1)', advance='no') char(LoX)
   end subroutine out_bytes

   integer function TEK_draw(x, y)
      integer, intent(in) :: x, y
      if (xterm == 0) call XTERM_4010()
      if (tlstx /= vdevice%cpVx .or. tlsty /= vdevice%cpVy) then
         write(draw_unit, '(A1)', advance='no') GS
         LoYold = -1; HiYold = -1; HiXold = -1
         call out_bytes(vdevice%cpVx, vdevice%cpVy)
      end if
      call out_bytes(x, y)
      tlstx = x; tlsty = y
      if (xterm == 0) call XTERM_vt102()
      if (draw_unit == output_unit .or. draw_unit == error_unit) flush(draw_unit)
      TEK_draw = UNUSED
   end function TEK_draw

   integer function TEK_getkey()
      character(len=1) :: c
      integer :: ios, idum

      TEKMODE = -4
      if (xterm == 0) call XTERM_4010()
      flush(draw_unit)
      idum = c_system(c_char_"stty raw -echo" // char(0))
      read(input_unit, '(A1)', iostat=ios) c
      idum = c_system(c_char_"stty -raw echo" // char(0))
      if (xterm == 0) call XTERM_vt102()
      if (ios /= 0) then
         TEK_getkey = -1
      else
         TEK_getkey = ichar(c)
      end if
   end function TEK_getkey

   integer function TEK_locator(x, y)
      integer, intent(out) :: x, y
      character(len=1) :: buf(5)
      integer :: i, ios, idum

      x = 0; y = 0
      if (click /= 0) then
         click = 0; TEK_locator = 0; return
      end if
      click = 1; TEKMODE = -5
      if (xterm == 0) call XTERM_4010()

      idum = c_system(c_char_"stty raw -echo" // char(0))
      write(draw_unit, '(3A1)', advance='no') US, ESC, SUB
      if (draw_unit == output_unit .or. draw_unit == error_unit) flush(draw_unit)

      do i = 1, 5
         read(input_unit, '(A1)', iostat=ios) buf(i)
      end do
      idum = c_system(c_char_"stty -raw echo" // char(0))

      x = ior(ishft(iand(ichar(buf(2)), MASK), 5), iand(ichar(buf(3)), MASK))
      y = ior(ishft(iand(ichar(buf(4)), MASK), 5), iand(ichar(buf(5)), MASK))

      tlstx = -1; tlsty = -1
      if (xterm == 0) call XTERM_vt102()
      TEK_locator = ishft(1, ichar(buf(1)) - ichar('1'))
   end function TEK_locator

   integer function TEK_clear()
      integer(c_int) :: idum
      if (xterm == 0) call XTERM_4010()
      write(draw_unit, '(3A1)', advance='no') US, ESC, FF
      if (draw_unit == output_unit .or. draw_unit == error_unit) flush(draw_unit)
      tlstx = -1; tlsty = -1

      if (xterm == 0) then
         TEKMODE = -6
         call XTERM_vt102()
      else if (draw_unit == output_unit .or. draw_unit == error_unit) then
         flush(draw_unit)
         idum = c_usleep(1000000_c_int)
      end if
      TEK_clear = UNUSED
   end function TEK_clear

   integer function TEK_font(font)
      character(len=*), intent(in) :: font
      if (xterm == 0) call XTERM_4010()
      if (trim(font) == "small") then
         write(draw_unit, '(2A1)', advance='no') ESC, ':'
         vdevice%hwidth = 8.0_real32; vdevice%hheight = 15.0_real32
      else if (trim(font) == "large") then
         write(draw_unit, '(2A1)', advance='no') ESC, '8'
         vdevice%hwidth = 14.0_real32; vdevice%hheight = 17.0_real32
      else
         if (xterm == 0) call XTERM_vt102()
         TEK_font = 0; return
      end if
      tlstx = -1; tlsty = -1
      if (xterm == 0) call XTERM_vt102()
      TEK_font = 1
   end function TEK_font

   integer function TEK_char(c)
      character(len=1), intent(in) :: c
      if (xterm == 0) call XTERM_4010()
      if (tlstx /= vdevice%cpVx .or. tlsty /= vdevice%cpVy) then
         write(draw_unit, '(A1)', advance='no') GS
         LoYold = -1; HiYold = -1; HiXold = -1
         call out_bytes(vdevice%cpVx, vdevice%cpVy)
      end if
      write(draw_unit, '(2A1)', advance='no') US, c
      tlstx = -1; tlsty = -1
      if (xterm == 0) call XTERM_vt102()
      if (draw_unit == output_unit .or. draw_unit == error_unit) flush(draw_unit)
      TEK_char = UNUSED
   end function TEK_char

   integer function TEK_string(s)
      character(len=*), intent(in) :: s
      if (xterm == 0) call XTERM_4010()
      if (tlstx /= vdevice%cpVx .or. tlsty /= vdevice%cpVy) then
         write(draw_unit, '(A1)', advance='no') GS
         LoYold = -1; HiYold = -1; HiXold = -1
         call out_bytes(vdevice%cpVx, vdevice%cpVy)
      end if
      write(draw_unit, '(A1, A)', advance='no') US, s
      tlstx = -1; tlsty = -1
      if (xterm == 0) call XTERM_vt102()
      if (draw_unit == output_unit .or. draw_unit == error_unit) flush(draw_unit)
      TEK_string = UNUSED
   end function TEK_string

   integer function TEK_fill(n, x, y)
      integer, intent(in) :: n
      integer, intent(in) :: x(n), y(n)
      integer :: i

      if (xterm == 0) call XTERM_4010()
      if (tlstx /= x(1) .or. tlsty /= y(1)) then
         write(draw_unit, '(A1)', advance='no') GS
         LoYold = -1; HiYold = -1; HiXold = -1
         call out_bytes(x(1), y(1))
      end if
      do i = 2, n
         call out_bytes(x(i), y(i))
      end do
      call out_bytes(x(1), y(1))

      if (draw_unit == output_unit .or. draw_unit == error_unit) flush(draw_unit)
      tlstx = x(n); vdevice%cpVx = x(n)
      tlsty = y(n); vdevice%cpVy = y(n)
      if (xterm == 0) call XTERM_vt102()
      TEK_fill = UNUSED
   end function TEK_fill

   integer function TEK_sync()
      if (draw_unit == output_unit .or. draw_unit == error_unit) flush(draw_unit)
      TEKMODE = -1
      if (xterm == 0) call XTERM_vt102()
      TEK_sync = UNUSED
   end function TEK_sync

   integer function noop(); noop = -1; end function
   integer function noop1(x); integer, intent(in) :: x; noop1 = -1; end function
   integer function noop4(a, b, c, d); integer, intent(in) :: a, b, c, d; noop4 = -1; end function

   subroutine setup_tekdev()
      vdevice%dev%devname   = "tek"
      vdevice%dev%large     = "large"
      vdevice%dev%small     = "small"

vdevice%dev%Vbackb => noop
vdevice%dev%Vchar => TEK_char
vdevice%dev%Vcheckkey => noop
vdevice%dev%Vclear => TEK_clear
vdevice%dev%Vcolor => noop1
vdevice%dev%Vdraw => TEK_draw
vdevice%dev%Vexit => TEK_exit
vdevice%dev%Vfill => TEK_fill
vdevice%dev%Vfont => TEK_font
vdevice%dev%Vfrontb => noop
vdevice%dev%Vgetkey => TEK_getkey
vdevice%dev%Vinit => TEK_init
vdevice%dev%Vlocator => TEK_locator
vdevice%dev%Vmapcolor => noop4
vdevice%dev%Vsetlw => noop1
vdevice%dev%Vstring => TEK_string
vdevice%dev%Vswapb => noop
vdevice%dev%Vsync => TEK_sync
end subroutine setup_tekdev
integer function TEK_draw_devcpy(); call setup_tekdev(); xterm = -1; TEK_draw_devcpy = UNUSED; end function
integer function XTEK_draw_devcpy(); call setup_tekdev(); xterm = 0; XTEK_draw_devcpy = UNUSED; end function
end module m_draw_drv_tek
