module m_draw_drv_pcl5
   use, intrinsic :: iso_fortran_env, only: real32, output_unit, error_unit
   use m_draw_types
   use m_draw_pref
   use m_draw_drivers
   implicit none
   private
   public :: PCL5_LAND_draw_devcpy, PCL5_PORT_draw_devcpy, &
             HPGL2_PORT_draw_devcpy, HPGL2_LAND_draw_devcpy

   integer, parameter :: CMAPSIZE = 256
   integer, save :: drawn = 0
   integer, save :: plotlstx = -1, plotlsty = -1
   logical, save :: is_pcl5_mode = .false.
   character(len=120), save :: pcl5_clear_str = ""

contains

   integer function HPGL2_mapcolor(i, r, g, b)
      integer, intent(in) :: i, r, g, b
      if (i >= CMAPSIZE .or. i < 0) then
         HPGL2_mapcolor = -1; return
      end if
      write(draw_unit, '(A, I0, ",", I0, ",", I0, ",", I0)') "PC", i, r, g, b
      HPGL2_mapcolor = UNUSED
   end function HPGL2_mapcolor

   subroutine emit_hpgl2_palette()
      integer :: idum
      write(draw_unit, '(A)') "TR0;"
      write(draw_unit, '(A)') "NP256;"
      write(draw_unit, '(A)') "CR0,255,0,255,0,255;"
      idum = HPGL2_mapcolor( 0, 255, 255, 255)
      idum = HPGL2_mapcolor( 1, 255,   0,   0)
      idum = HPGL2_mapcolor( 2,   0, 255,   0)
      idum = HPGL2_mapcolor( 3, 255, 255,   0)
      idum = HPGL2_mapcolor( 4,   0,   0, 255)
      idum = HPGL2_mapcolor( 5, 255,   0, 255)
      idum = HPGL2_mapcolor( 6,   0, 255, 255)
      idum = HPGL2_mapcolor( 7,   0,   0,   0)
      idum = HPGL2_mapcolor( 8, 155,   0,   0)
      idum = HPGL2_mapcolor( 9,   0, 155,   0)
      idum = HPGL2_mapcolor(10, 155, 255, 255)
      idum = HPGL2_mapcolor(11, 155, 155,   0)
      idum = HPGL2_mapcolor(12,   0,   0, 155)
      idum = HPGL2_mapcolor(13, 155,   0, 155)
      idum = HPGL2_mapcolor(14,   0, 155, 155)
      idum = HPGL2_mapcolor(15, 100, 100, 100)
   end subroutine emit_hpgl2_palette

   integer function HPGL2_common_init(minx, maxx, miny, maxy)
      integer, intent(in) :: minx, maxx, miny, maxy
      vdevice%depth = 8
      draw_unit = draw_outfile()
      write(draw_unit, '(A, I0, ",", I0, ",", I0, ",", I0, A, I0, A, I0, A)') &
         char(27)//".(;"//char(27)//".I81;;17;"//char(27)//".N;19:IN;IP", &
         minx, miny, maxx, maxy, ";SC0,", vdevice%sizeSx, ",0,", vdevice%sizeSy, ";"
      call emit_hpgl2_palette()
      is_pcl5_mode = .false.; plotlstx = -1111111; plotlsty = -1111111; drawn = 0
      HPGL2_common_init = UNUSED
   end function HPGL2_common_init

   integer function HPGL2_common_initp(minx, maxx, miny, maxy)
      integer, intent(in) :: minx, maxx, miny, maxy
      vdevice%depth = 8
      draw_unit = draw_outfile()
      write(draw_unit, '(A, I0, ",", I0, ",", I0, ",", I0, A, I0, A, I0, A)') &
         char(27)//".(;"//char(27)//".I81;;17;"//char(27)//".N;19:IN;IP", &
         minx, miny, maxx, maxy, ";SC0,", vdevice%sizeSx, ",0,", vdevice%sizeSy, ";RO90;"
      call emit_hpgl2_palette()
      is_pcl5_mode = .false.; plotlstx = -1111111; plotlsty = -1111111; drawn = 0
      HPGL2_common_initp = UNUSED
   end function HPGL2_common_initp

   integer function PCL5_common_init(minx, maxx, miny, maxy)
      integer, intent(in) :: minx, maxx, miny, maxy
      vdevice%depth = 8
      draw_unit = draw_outfile()
      write(pcl5_clear_str, '(A, I0, ",", I0, ",", I0, ",", I0, A, I0, A, I0, A)') &
         char(27)//"%1A"//char(27)//"E"//new_line('A')//char(27)//"&l2A"//char(27)//"&l1O"//char(27)//"%1B"//new_line('A')//"IP", &
         minx, miny, maxx, maxy, ";SC0,", vdevice%sizeSx, ",0,", vdevice%sizeSy, ";"
      write(draw_unit, '(A)') trim(pcl5_clear_str)
      write(draw_unit, '(A, /, A, /, A)') "TR0;", "NP256;", "CR0,255,0,255,0,255;"
      is_pcl5_mode = .true.; plotlstx = -1111111; plotlsty = -1111111; drawn = 0
      PCL5_common_init = UNUSED
   end function PCL5_common_init

   integer function PCL5_common_initp(minx, maxx, miny, maxy)
      integer, intent(in) :: minx, maxx, miny, maxy
      vdevice%depth = 8
      draw_unit = draw_outfile()
      write(pcl5_clear_str, '(A, I0, ",", I0, ",", I0, ",", I0, A, I0, A, I0, A)') &
         char(27)//"%1A"//char(27)//"E"//new_line('A')//char(27)//"&l2A"//char(27)//"&l1O"//char(27)//"%1B"//new_line('A')//"IP", &
         minx, miny, maxx, maxy, ";SC0,", vdevice%sizeSx, ",0,", vdevice%sizeSy, ";RO90;"
      write(draw_unit, '(A)') trim(pcl5_clear_str)
      write(draw_unit, '(A, /, A, /, A)') "TR0;", "NP256;", "CR0,255,0,255,0,255;"
      is_pcl5_mode = .true.; plotlstx = -1111111; plotlsty = -1111111; drawn = 0
      PCL5_common_initp = UNUSED
   end function PCL5_common_initp

   subroutine resolve_port(prefx, prefy)
      integer, intent(out) :: prefx, prefy
      integer :: prefxs, prefys
      call draw_getprefposandsize(prefx, prefy, prefxs, prefys)
      if (prefxs /= -1) then
         vdevice%sizeSy = prefys; vdevice%sizeSx = prefxs
         vdevice%sizeX = min(prefys, prefxs); vdevice%sizeY = vdevice%sizeX
      else
         prefx = 0; prefy = 0; vdevice%sizeSx = 7650; vdevice%sizeSy = 9540
         vdevice%sizeX = 7650; vdevice%sizeY = 7650
      end if
   end subroutine resolve_port

   subroutine resolve_land(prefx, prefy)
      integer, intent(out) :: prefx, prefy
      integer :: prefxs, prefys
      call draw_getprefposandsize(prefx, prefy, prefxs, prefys)
      if (prefxs /= -1) then
         vdevice%sizeSy = prefys; vdevice%sizeSx = prefxs
         vdevice%sizeX = min(prefys, prefxs); vdevice%sizeY = vdevice%sizeX
      else
         prefx = 0; prefy = 0; vdevice%sizeSy = 7650; vdevice%sizeSx = 9540
         vdevice%sizeX = 7650; vdevice%sizeY = 7650
      end if
   end subroutine resolve_land

   integer function HPGL2_PORT_init()
      integer :: prefx, prefy; call resolve_port(prefx, prefy)
      HPGL2_PORT_init = HPGL2_common_initp(prefx, vdevice%sizeSx + prefx, prefy, vdevice%sizeSy + prefy)
   end function HPGL2_PORT_init

   integer function PCL5_PORT_init()
      integer :: prefx, prefy; call resolve_port(prefx, prefy)
      PCL5_PORT_init = PCL5_common_initp(prefx, vdevice%sizeSx + prefx, prefy, vdevice%sizeSy + prefy)
   end function PCL5_PORT_init

   integer function HPGL2_LAND_init()
      integer :: prefx, prefy; call resolve_land(prefx, prefy)
      HPGL2_LAND_init = HPGL2_common_init(prefx, vdevice%sizeSx + prefx, prefy, vdevice%sizeSy + prefy)
   end function HPGL2_LAND_init

   integer function PCL5_LAND_init()
      integer :: prefx, prefy; call resolve_land(prefx, prefy)
      PCL5_LAND_init = PCL5_common_init(prefx, vdevice%sizeSx + prefx, prefy, vdevice%sizeSy + prefy)
   end function PCL5_LAND_init

   integer function HPGL2_draw(x, y)
      integer, intent(in) :: x, y
      if (plotlstx /= vdevice%cpVx .or. plotlsty /= vdevice%cpVy) then
         write(draw_unit, '(A, I0, ",", I0, A)') "PU", vdevice%cpVx, vdevice%cpVy, ";"
      end if
      write(draw_unit, '(A, I0, ",", I0, A)') "PD", x, y, ";"
      plotlstx = x; plotlsty = y; drawn = 1
      HPGL2_draw = UNUSED
   end function HPGL2_draw

   integer function HPGL2_exit()
      write(draw_unit, '(A)') "SP0;"
      if (is_pcl5_mode) then
         write(draw_unit, '(A)') char(27) // "%1A" // char(27) // "E"
      else
         write(draw_unit, '(A)') char(27) // ".)"
      end if
      flush(draw_unit)
      if (draw_unit /= output_unit .and. draw_unit /= error_unit) close(draw_unit)
      drawn = 1
      HPGL2_exit = UNUSED
   end function HPGL2_exit

   integer function HPGL2_clear()
      if (drawn /= 0) then
         if (is_pcl5_mode) then
            write(draw_unit, '(A)', advance='no') trim(pcl5_clear_str)
         else
            write(draw_unit, '(A)') "PG;"
         end if
      end if
      drawn = 0
      HPGL2_clear = UNUSED
   end function HPGL2_clear

   integer function HPGL2_setlw(i)
      integer, intent(in) :: i
      real(real32), save :: line_width = 0.2_real32
      if (i <= -1) then
         line_width = 0.0_real32
      else
         line_width = (real(vdevice%sizeX, real32) / 10000.0_real32) * real(abs(i), real32)
         line_width = line_width * 0.0846666_real32 / 4.0_real32
      end if
      write(draw_unit, '(A, F0.4, A)') "PW", line_width, ";"
      HPGL2_setlw = UNUSED
   end function HPGL2_setlw

   integer function HPGL2_color(i)
      integer, intent(in) :: i
      real(real32), save :: line_width = 0.2_real32
      integer :: idum
      if (i < 0) then
         idum = HPGL2_setlw(abs(i))
      else
         write(draw_unit, '(A, I0, A)') "SP", i, ";"
         write(draw_unit, '(A, F0.4, A)') "PW", line_width, ";"
      end if
      HPGL2_color = UNUSED
   end function HPGL2_color

   integer function HPGL2_font(font)
      character(len=*), intent(in) :: font
      if (trim(font) == "small") then
         vdevice%hwidth = 97.01_real32; vdevice%hheight = vdevice%hwidth * 2.0_real32
         write(draw_unit, '(A)') "SI0.1600,0.3200;"
      else if (trim(font) == "large") then
         vdevice%hwidth = 145.5_real32; vdevice%hheight = vdevice%hwidth * 2.0_real32
         write(draw_unit, '(A)') "SI0.2400,0.4800;"
      else
         HPGL2_font = 0; return
      end if
      HPGL2_font = 1
   end function HPGL2_font

   integer function HPGL2_char(c)
      character(len=1), intent(in) :: c
      if (plotlstx /= vdevice%cpVx .or. plotlsty /= vdevice%cpVy) then
         write(draw_unit, '(A, I0, ",", I0, A)') "PU", vdevice%cpVx, vdevice%cpVy, ";"
      end if
      write(draw_unit, '(A, A, A)') "LB", c, char(3)
      plotlstx = -1111111; plotlsty = -1111111; drawn = 1
      HPGL2_char = UNUSED
   end function HPGL2_char

   integer function HPGL2_string(s)
      character(len=*), intent(in) :: s
      if (plotlstx /= vdevice%cpVx .or. plotlsty /= vdevice%cpVy) then
         write(draw_unit, '(A, I0, ",", I0, A)') "PU", vdevice%cpVx, vdevice%cpVy, ";"
      end if
      write(draw_unit, '(A, A, A)') "LB", trim(s), char(3)
      plotlstx = -1111111; plotlsty = -1111111; drawn = 1
      HPGL2_string = UNUSED
   end function HPGL2_string

integer function HPGL2_fill(n, x, y)
integer, intent(in) :: n
integer, intent(in) :: x(n), y(n)
integer :: i, xclose, yclose
logical :: do_move
xclose = x(1); yclose = y(1)
if (plotlstx /= x(1) .or. plotlsty /= y(1)) then
write(draw_unit, '(A, I0, ",", I0, A)') "PU", x(1), y(1), ";"
end if
write(draw_unit, '(A)', advance='no') "PM0"
do_move = .false.
do i = 2, n
if (x(i) == xclose .and. y(i) == yclose .and. i /= n) then
write(draw_unit, '(A, I0, ",", I0, A)', advance='no') "PD", x(i), y(i), ";PM1"
do_move = .true.
else
if (do_move) then
write(draw_unit, '(A, I0, ",", I0, A)', advance='no') "PU", x(i), y(i), ";"
else
write(draw_unit, '(A, I0, ",", I0, A)', advance='no') "PD", x(i), y(i), ";"
end if
do_move = .false.
end if
end do
write(draw_unit, '(A)', advance='no') "PM2;FP0;EP"
write(draw_unit, '(A, I0, ",", I0, A)') "PU", x(n), y(n), ";"
plotlstx = x(n); vdevice%cpVx = x(n)
plotlsty = y(n); vdevice%cpVy = y(n)
drawn = 1
HPGL2_fill = UNUSED
end function HPGL2_fill
integer function noop(); noop = -1; end function
integer function noop2(x, y); integer, intent(out) :: x, y; x = 0; y = 0; noop2 = -1; end function
subroutine setup_hpgl2dev()
vdevice%dev%devname = "HPGL2"
vdevice%dev%large = "large"
vdevice%dev%small = "small"
vdevice%dev%Vbackb => noop
vdevice%dev%Vchar => HPGL2_char
vdevice%dev%Vcheckkey => noop
vdevice%dev%Vclear => HPGL2_clear
vdevice%dev%Vcolor => HPGL2_color
vdevice%dev%Vdraw => HPGL2_draw
vdevice%dev%Vexit => HPGL2_exit
vdevice%dev%Vfill => HPGL2_fill
vdevice%dev%Vfont => HPGL2_font
vdevice%dev%Vfrontb => noop
vdevice%dev%Vgetkey => noop
vdevice%dev%Vinit => HPGL2_LAND_init
vdevice%dev%Vlocator => noop2
vdevice%dev%Vmapcolor => HPGL2_mapcolor
vdevice%dev%Vsetlw => HPGL2_setlw
vdevice%dev%Vstring => HPGL2_string
vdevice%dev%Vswapb => noop
vdevice%dev%Vsync => noop
end subroutine setup_hpgl2dev
integer function PCL5_LAND_draw_devcpy(); call setup_hpgl2dev(); vdevice%dev%Vinit => PCL5_LAND_init; PCL5_LAND_draw_devcpy = UNUSED; end function
integer function PCL5_PORT_draw_devcpy(); call setup_hpgl2dev(); vdevice%dev%Vinit => PCL5_PORT_init; PCL5_PORT_draw_devcpy = UNUSED; end function
integer function HPGL2_PORT_draw_devcpy(); call setup_hpgl2dev(); vdevice%dev%Vinit => HPGL2_PORT_init; HPGL2_PORT_draw_devcpy = UNUSED; end function
integer function HPGL2_LAND_draw_devcpy(); call setup_hpgl2dev(); vdevice%dev%Vinit => HPGL2_LAND_init; HPGL2_LAND_draw_devcpy = UNUSED; end function
end module m_draw_drv_pcl5
