module m_draw_drv_hpdxy
   use, intrinsic :: iso_fortran_env, only: real32, output_unit, error_unit
   use m_draw_types
   use m_draw_pref
   use m_draw_drivers
   implicit none
   private
   public :: HPGL_A2_draw_devcpy, PCL_LAND_draw_devcpy, PCL_PORT_draw_devcpy, &
             HPGL_PORT_draw_devcpy, HPGL_LAND_draw_devcpy, DXY_draw_devcpy

   integer, save :: drawn = 0
   integer, save :: plotlstx = -1, plotlsty = -1
   integer, save :: remap(8) = [0, 1, 2, 3, 4, 5, 6, 7]

   integer, parameter :: DEV_HPGL = 1
   integer, parameter :: DEV_PCL  = 2
   integer, parameter :: DEV_DXY  = 3
   integer, save :: current_mode = DEV_HPGL

   character(len=100), save :: CLEAR_DEVICE = ""

contains

   subroutine emit_move(x, y)
      integer, intent(in) :: x, y
      select case (current_mode)
      case (DEV_HPGL, DEV_PCL)
         write(draw_unit, '(A, I0, 1X, I0, A)') "PU ", x, y, ";"
      case (DEV_DXY)
         write(draw_unit, '(A, I0, ",", I0)') "M ", x, y
      end select
   end subroutine emit_move

   subroutine emit_draw(x, y)
      integer, intent(in) :: x, y
      select case (current_mode)
      case (DEV_HPGL, DEV_PCL)
         write(draw_unit, '(A, I0, 1X, I0, A)') "PD ", x, y, ";"
      case (DEV_DXY)
         write(draw_unit, '(A, I0, ",", I0)') "D ", x, y
      end select
   end subroutine emit_draw

   subroutine emit_pen(p)
      integer, intent(in) :: p
      select case (current_mode)
      case (DEV_HPGL, DEV_PCL)
         write(draw_unit, '(A, I0, A)') "SP ", p, ";"
      case (DEV_DXY)
         write(draw_unit, '(A, I0)') "J ", p
      end select
   end subroutine emit_pen

   subroutine emit_wide(w)
      real(real32), intent(in) :: w
      select case (current_mode)
      case (DEV_HPGL, DEV_PCL)
         write(draw_unit, '(A, F0.4, A)') "PW ", w, ";"
      case (DEV_DXY)
         write(draw_unit, '(A)') "PG;"
      end select
   end subroutine emit_wide

   integer function HPGL_common_init(minx, maxx, miny, maxy)
      integer, intent(in) :: minx, maxx, miny, maxy
      vdevice%depth = 3
      draw_unit = draw_outfile()

      write(draw_unit, '(A, I0, ",", I0, ",", I0, ",", I0, A, I0, A, I0, A)') &
         char(27)//".(;"//char(27)//".I81;;17;"//char(27)//".N;19:IN;IP ", &
         minx, miny, maxx, maxy, ";SC 0,", vdevice%sizeSx, ",0,", vdevice%sizeSy, ";TR0;"

      current_mode = DEV_HPGL
      CLEAR_DEVICE = "PG;" // new_line('A')
      plotlstx = -1111111; plotlsty = -1111111
      drawn = 0
      HPGL_common_init = 1
   end function HPGL_common_init

   integer function HPGL_common_initp(minx, maxx, miny, maxy)
      integer, intent(in) :: minx, maxx, miny, maxy
      vdevice%depth = 3
      draw_unit = draw_outfile()

      write(draw_unit, '(A, I0, ",", I0, ",", I0, ",", I0, A, I0, A, I0, A)') &
         char(27)//".(;"//char(27)//".I81;;17;"//char(27)//".N;19:IN;IP ", &
         minx, miny, maxx, maxy, ";SC 0,", vdevice%sizeSx, ",0,", vdevice%sizeSy, ";RO 90;TR0;"

      current_mode = DEV_HPGL
      CLEAR_DEVICE = "PG;" // new_line('A')
      plotlstx = -1111111; plotlsty = -1111111
      drawn = 0
      HPGL_common_initp = 1
   end function HPGL_common_initp

   integer function PCL_common_init(minx, maxx, miny, maxy)
      integer, intent(in) :: minx, maxx, miny, maxy
      vdevice%depth = 3
      draw_unit = draw_outfile()

      write(CLEAR_DEVICE, '(A, I0, ",", I0, ",", I0, ",", I0, A, I0, A, I0, A)') &
         char(27)//"%1A"//char(27)//"E"//new_line('A')//char(27)//"&l2A"//char(27)//"&l1O"//char(27)//"%1B"//new_line('A')//"IP ", &
         minx, miny, maxx, maxy, ";SC 0,", vdevice%sizeSx, ",0,", vdevice%sizeSy, ";"

      write(draw_unit, '(A, A)') trim(CLEAR_DEVICE), "TR0;"
      current_mode = DEV_PCL
      plotlstx = -1111111; plotlsty = -1111111
      drawn = 0
      PCL_common_init = 1
   end function PCL_common_init

   integer function PCL_common_initp(minx, maxx, miny, maxy)
      integer, intent(in) :: minx, maxx, miny, maxy
      vdevice%depth = 3
      draw_unit = draw_outfile()

      write(CLEAR_DEVICE, '(A, I0, ",", I0, ",", I0, ",", I0, A, I0, A, I0, A)') &
         char(27)//"%1A"//char(27)//"E"//new_line('A')//char(27)//"&l2A"//char(27)//"&l1O"//char(27)//"%1B"//new_line('A')//"IP ", &
         minx, miny, maxx, maxy, ";SC 0,", vdevice%sizeSx, ",0,", vdevice%sizeSy, ";RO 90;"

      write(draw_unit, '(A, A)') trim(CLEAR_DEVICE), "TR0;"
      current_mode = DEV_PCL
      plotlstx = -1111111; plotlsty = -1111111
      drawn = 0
      PCL_common_initp = 1
   end function PCL_common_initp

   subroutine resolve_size_port(prefx, prefy)
      integer, intent(out) :: prefx, prefy
      integer :: prefxs, prefys
      call draw_getprefposandsize(prefx, prefy, prefxs, prefys)
      if (prefxs /= -1) then
         vdevice%sizeSy = prefys; vdevice%sizeSx = prefxs
         vdevice%sizeX = min(prefys, prefxs); vdevice%sizeY = vdevice%sizeX
      else
         prefx = 0; prefy = 0
         vdevice%sizeSx = 7650; vdevice%sizeSy = 9540
         vdevice%sizeX = 7650; vdevice%sizeY = 7650
      end if
   end subroutine resolve_size_port

   subroutine resolve_size_land(prefx, prefy)
      integer, intent(out) :: prefx, prefy
      integer :: prefxs, prefys
      call draw_getprefposandsize(prefx, prefy, prefxs, prefys)
      if (prefxs /= -1) then
         vdevice%sizeSy = prefys; vdevice%sizeSx = prefxs
         vdevice%sizeX = min(prefys, prefxs); vdevice%sizeY = vdevice%sizeX
      else
         prefx = 0; prefy = 0
         vdevice%sizeSy = 7650; vdevice%sizeSx = 9540
         vdevice%sizeX = 7650; vdevice%sizeY = 7650
      end if
   end subroutine resolve_size_land

   integer function HPGL_PORT_init()
      integer :: prefx, prefy
      call resolve_size_port(prefx, prefy)
      HPGL_PORT_init = HPGL_common_initp(prefx, vdevice%sizeSx + prefx, prefy, vdevice%sizeSy + prefy)
   end function HPGL_PORT_init

   integer function PCL_PORT_init()
      integer :: prefx, prefy
      call resolve_size_port(prefx, prefy)
      PCL_PORT_init = PCL_common_initp(prefx, vdevice%sizeSx + prefx, prefy, vdevice%sizeSy + prefy)
   end function PCL_PORT_init

   integer function HPGL_LAND_init()
      integer :: prefx, prefy
      call resolve_size_land(prefx, prefy)
      HPGL_LAND_init = HPGL_common_init(prefx, vdevice%sizeSx + prefx, prefy, vdevice%sizeSy + prefy)
   end function HPGL_LAND_init

   integer function PCL_LAND_init()
      integer :: prefx, prefy
      call resolve_size_land(prefx, prefy)
      PCL_LAND_init = PCL_common_init(prefx, vdevice%sizeSx + prefx, prefy, vdevice%sizeSy + prefy)
   end function PCL_LAND_init

   integer function DXY_init()
      draw_unit = draw_outfile()
      vdevice%sizeX = 1920; vdevice%sizeY = 1920
      vdevice%sizeSx = 2668; vdevice%sizeSy = 1920
      current_mode = DEV_DXY
      CLEAR_DEVICE = char(27) // ".)" // new_line('A')
      plotlstx = -1; plotlsty = -1; drawn = 0
      DXY_init = 1
   end function DXY_init

   integer function PLOT_draw(x, y)
      integer, intent(in) :: x, y
      if (plotlstx /= vdevice%cpVx .or. plotlsty /= vdevice%cpVy) then
         call emit_move(vdevice%cpVx, vdevice%cpVy)
      end if
      call emit_draw(x, y)
      plotlstx = x; plotlsty = y; drawn = 1
      PLOT_draw = UNUSED
   end function PLOT_draw

   integer function PLOT_exit()
      call emit_pen(0)
      select case (current_mode)
      case (DEV_HPGL)
         write(draw_unit, '(A)') char(27) // ".)"
      case (DEV_PCL)
         write(draw_unit, '(A)') char(27) // "%1A" // char(27) // "E"
      case (DEV_DXY)
         write(draw_unit, '(A)') " "
      end select
      flush(draw_unit)
      if (draw_unit /= output_unit .and. draw_unit /= error_unit) close(draw_unit)
      drawn = 1
      PLOT_exit = UNUSED
   end function PLOT_exit

   integer function PLOT_clear()
      if (drawn /= 0) then
         write(draw_unit, '(A)', advance='no') trim(CLEAR_DEVICE)
      end if
      drawn = 0
      PLOT_clear = UNUSED
   end function PLOT_clear

   integer function PLOT_setlw(i)
      integer, intent(in) :: i
      real(real32), save :: line_width = 0.2_real32
      if (i <= -1) then
         line_width = 0.0_real32
      else
         line_width = (real(vdevice%sizeX, real32) / 10000.0_real32) * real(abs(i), real32)
         line_width = line_width * 0.0846666_real32 / 4.0_real32
      end if
      call emit_wide(line_width)
      PLOT_setlw = UNUSED
   end function PLOT_setlw

   integer function PLOT_color(i)
      integer, intent(in) :: i
      real(real32), save :: line_width = 0.2_real32
      integer :: idum
      if (i < 0) then
         idum = PLOT_setlw(abs(i))
      else
         if (i < 8) then
            call emit_pen(remap(i + 1))
         else
            call emit_pen(i)
         end if
         call emit_wide(line_width)
      end if
      PLOT_color = UNUSED
   end function PLOT_color

   integer function HPGL_font(font)
      character(len=*), intent(in) :: font
      if (trim(font) == "small") then
         vdevice%hwidth = 97.01_real32; vdevice%hheight = vdevice%hwidth * 2.0_real32
         write(draw_unit, '(A)') "SI 0.1600 0.3200;"
      else if (trim(font) == "large") then
         vdevice%hwidth = 145.5_real32; vdevice%hheight = vdevice%hwidth * 2.0_real32
         write(draw_unit, '(A)') "SI 0.2400 0.4800;"
      else
         HPGL_font = 0; return
      end if
      HPGL_font = 1
   end function HPGL_font

   integer function PLOT_char(c)
      character(len=1), intent(in) :: c
      if (plotlstx /= vdevice%cpVx .or. plotlsty /= vdevice%cpVy) then
         call emit_move(vdevice%cpVx, vdevice%cpVy)
      end if
      if (current_mode == DEV_DXY) then
         write(draw_unit, '(A, A)') "P", c
      else
         write(draw_unit, '(A, A, A)') "LB", c, char(3)
      end if
      plotlstx = -1111111; plotlsty = -1111111; drawn = 1
      PLOT_char = UNUSED
   end function PLOT_char

   integer function PLOT_string(s)
      character(len=*), intent(in) :: s

if (plotlstx /= vdevice%cpVx .or. plotlsty /= vdevice%cpVy) then
call emit_move(vdevice%cpVx, vdevice%cpVy)
end if
if (current_mode == DEV_DXY) then
write(draw_unit, '(A, A)') "P", trim(s)
else
write(draw_unit, '(A, A, A)') "LB", trim(s), char(3)
end if
plotlstx = -1111111; plotlsty = -1111111; drawn = 1
PLOT_string = UNUSED
end function PLOT_string
integer function PLOT_fill(n, x, y)
integer, intent(in) :: n
integer, intent(in) :: x(n), y(n)
integer :: i, xclose, yclose
logical :: do_move
xclose = x(1); yclose = y(1)
if (plotlstx /= x(1) .or. plotlsty /= y(1)) call emit_move(x(1), y(1))
if (current_mode /= DEV_DXY) write(draw_unit, '(A)') "PM0"
do_move = .false.
do i = 2, n
if (x(i) == xclose .and. y(i) == yclose .and. i /= n) then
call emit_draw(x(i), y(i))
if (current_mode /= DEV_DXY) write(draw_unit, '(A)') "PM1"
do_move = .true.
else
if (do_move) then
call emit_move(x(i), y(i))
else
call emit_draw(x(i), y(i))
end if
do_move = .false.
end if
end do
if (current_mode /= DEV_DXY) write(draw_unit, '(A)') "PM2;FP0;EP"
call emit_move(x(n), y(n))
plotlstx = x(n); vdevice%cpVx = x(n)
plotlsty = y(n); vdevice%cpVy = y(n)
drawn = 1
PLOT_fill = UNUSED
end function PLOT_fill
subroutine HPGL_setpens()
character(len=128) :: env_val
integer :: env_len, ios, i
call get_environment_variable("VPENS", value=env_val, length=env_len, status=ios)
if (ios == 0 .and. env_len > 0) then
read(env_val, *, iostat=ios) remap(1:8)
else
do i = 1, 8
remap(i) = i - 1
end do
end if
end subroutine HPGL_setpens
integer function noop(); noop = -1; end function
integer function noop2(x, y); integer, intent(out) :: x, y; x = 0; y = 0; noop2 = -1; end function
integer function noop4(a, b, c, d); integer, intent(in) :: a, b, c, d; noop4 = -1; end function
subroutine setup_hpgldev()
vdevice%dev%devname = "hpgl"
vdevice%dev%large = "large"
vdevice%dev%small = "small"
vdevice%dev%Vbackb => noop
vdevice%dev%Vchar => PLOT_char
vdevice%dev%Vcheckkey => noop
vdevice%dev%Vclear => PLOT_clear
vdevice%dev%Vcolor => PLOT_color
vdevice%dev%Vdraw => PLOT_draw
vdevice%dev%Vexit => PLOT_exit
vdevice%dev%Vfill => PLOT_fill
vdevice%dev%Vfont => HPGL_font
vdevice%dev%Vfrontb => noop
vdevice%dev%Vgetkey => noop
vdevice%dev%Vinit => HPGL_LAND_init
vdevice%dev%Vlocator => noop2
vdevice%dev%Vmapcolor => noop4
vdevice%dev%Vsetlw => PLOT_setlw
vdevice%dev%Vstring => PLOT_string
vdevice%dev%Vswapb => noop
vdevice%dev%Vsync => noop
end subroutine setup_hpgldev
integer function HPGL_A2_draw_devcpy(); call setup_hpgldev(); HPGL_A2_draw_devcpy = UNUSED; end function
integer function PCL_LAND_draw_devcpy(); call setup_hpgldev(); vdevice%dev%Vinit => PCL_LAND_init; PCL_LAND_draw_devcpy = UNUSED; end function
integer function PCL_PORT_draw_devcpy(); call setup_hpgldev(); vdevice%dev%Vinit => PCL_PORT_init; PCL_PORT_draw_devcpy = UNUSED; end function
integer function HPGL_PORT_draw_devcpy()
call setup_hpgldev(); vdevice%dev%Vinit => HPGL_PORT_init; vdevice%dev%devname = "hpglport"; call HPGL_setpens(); HPGL_PORT_draw_devcpy = UNUSED
end function HPGL_PORT_draw_devcpy
integer function HPGL_LAND_draw_devcpy()
call setup_hpgldev(); vdevice%dev%Vinit => HPGL_LAND_init; vdevice%dev%devname = "hpglland"; call HPGL_setpens(); HPGL_LAND_draw_devcpy = UNUSED
end function HPGL_LAND_draw_devcpy
integer function DXY_draw_devcpy()
call setup_hpgldev(); vdevice%dev%devname = "dxy"; vdevice%dev%Vinit => DXY_init; DXY_draw_devcpy = UNUSED
end function DXY_draw_devcpy
end module m_draw_drv_hpdxy
