module m_draw_drv_vog
   use, intrinsic :: iso_fortran_env, only: real32, output_unit, error_unit
   use m_draw_types
   use m_draw_pref
   use m_draw_drivers
   implicit none
   private
   public :: VOG_draw_devcpy

   integer, parameter :: VOGXSIZE = 8500
   integer, parameter :: VOGYSIZE = 11000

   logical, save :: vog_first_time = .true.
   integer, save :: VOGlastx = -1, VOGlasty = -1
   integer, save :: pgroup = 1

contains

   subroutine VOG_header()
      character(len=8)  :: dt
      character(len=10) :: tm
      call date_and_time(date=dt, time=tm)
      write(draw_unit, '(A, A, "-", A, "-", A, " ", A, ":", A, ":", A)') &
         "#M_DRAW VOG Driver. Data Creation Date: ", &
         dt(1:4), dt(5:6), dt(7:8), tm(1:2), tm(3:4), tm(5:6)
   end subroutine VOG_header

   integer function VOG_mapcolor(i, r, g, b)
      integer, intent(in) :: i, r, g, b
      write(draw_unit, '(A, I0, 1X, I0, 1X, I0, 1X, I0)') "mapcolor ", i, r, g, b
      VOG_mapcolor = 0
   end function VOG_mapcolor

   integer function VOG_init()
      integer :: prefx, prefy, prefxs, prefys
      draw_unit = draw_outfile()
      if (.not. vog_first_time) then
         VOG_init = 1; return
      end if
      call VOG_header()
      call draw_getprefposandsize(prefx, prefy, prefxs, prefys)

      if (prefxs /= -1) then
         vdevice%sizeSy = prefys
         vdevice%sizeSx = prefxs
         vdevice%sizeX = min(prefys, prefxs)
         vdevice%sizeY = vdevice%sizeX
      else
         vdevice%sizeSy = VOGYSIZE
         vdevice%sizeSx = VOGXSIZE
         vdevice%sizeX = min(VOGXSIZE, VOGYSIZE)
         vdevice%sizeY = vdevice%sizeX
      end if

      write(draw_unit, '(A)') "#vinit vog"
      write(draw_unit, '(A, I0, A, I0)') "page 0 ", vdevice%sizeSx, " 0 ", vdevice%sizeSy
      VOG_init = 1
   end function VOG_init

   integer function VOG_exit()
      write(draw_unit, '(A)') "vflush"
      write(draw_unit, '(A)') "page 0"
      write(draw_unit, '(A)') "#vexit"
      if (draw_unit /= output_unit .and. draw_unit /= error_unit) then
         flush(draw_unit)
         close(draw_unit)
      end if
      VOG_exit = 0
   end function VOG_exit

   integer function VOG_draw(x, y)
      integer, intent(in) :: x, y
      if (VOGlastx /= vdevice%cpVx .or. VOGlasty /= vdevice%cpVy) then
         write(draw_unit, '(A, I0, 1X, I0, A)', advance='no') "move2 ", vdevice%cpVx, vdevice%cpVy, ";"
      end if
      write(draw_unit, '(A, I0, 1X, I0)') " draw2 ", x, y
      VOGlastx = x
      VOGlasty = y
      VOG_draw = 0
   end function VOG_draw

   integer function VOG_clear()
      pgroup = pgroup + 1
      write(draw_unit, '(A, /, A, I0)') "clear", "#Page ", pgroup
      VOG_clear = 0
   end function VOG_clear

   integer function VOG_color(col)
      integer, intent(in) :: col
      if (col < 0) then
         write(draw_unit, '(A, I0)') "linewidth ", abs(col)
      else
         write(draw_unit, '(A, I0)') "color ", col
      end if
      VOG_color = 0
   end function VOG_color

   integer function VOG_setlw(width)
      integer, intent(in) :: width
      if (width > 0) then
         write(draw_unit, '(A, I0)') "linewidth ", width
      end if
      VOG_setlw = 0
   end function VOG_setlw

   integer function VOG_font(font)
      character(len=*), intent(in) :: font
      if (trim(font) == "small") then
         vdevice%hwidth = 12.0_real32 / 72.0_real32 / 0.03937_real32
         vdevice%hheight = vdevice%hwidth * 2.2_real32
      else if (trim(font) == "large") then
         vdevice%hheight = 55.0_real32 / 72.0_real32 / 0.03937_real32
         vdevice%hwidth = vdevice%hheight / 4.5_real32
      else
         VOG_font = 0; return
      end if
      write(draw_unit, '(A, F12.6, 1X, F12.6, A, A, A)') &
         "textsize ", vdevice%hwidth, vdevice%hheight, ";font ", trim(font), " "
      VOG_font = 1
   end function VOG_font

   integer function VOG_string(s)
      character(len=*), intent(in) :: s
      if (VOGlastx /= vdevice%cpVx .or. VOGlasty /= vdevice%cpVy) then
         write(draw_unit, '(A, I0, 1X, I0, A)', advance='no') "move2 ", vdevice%cpVx, vdevice%cpVy, ";"
      end if
      write(draw_unit, '(A, F12.6, 1X, F12.6, A, A, A)') &
         "textsize ", vdevice%hwidth, vdevice%hheight, ";drawstr ", trim(s), " "
      VOGlastx = -1; VOGlasty = -1
      VOG_string = 0
   end function VOG_string

   integer function VOG_char(c)
      character(len=1), intent(in) :: c
      if (VOGlastx /= vdevice%cpVx .or. VOGlasty /= vdevice%cpVy) then
         write(draw_unit, '(A, I0, 1X, I0, A)', advance='no') "move2 ", vdevice%cpVx, vdevice%cpVy, ";"
      end if
      write(draw_unit, '(A, A)') "drawchar ", c
      VOGlastx = -1; VOGlasty = -1
      VOG_char = 0
   end function VOG_char

   integer function VOG_fill(n, x, y)
      integer, intent(in) :: n
      integer, intent(in) :: x(n), y(n)
      integer :: i

      write(draw_unit, '(A, I0, 1X, I0, A)') "polyfill 1;move2 ", x(1), y(1), ";makepoly;"
      do i = 2, n
         if (mod(i - 1, 8) == 7) then
            write(draw_unit, '(A, I0, 1X, I0)') "draw2 ", x(i), y(i)
         else
            write(draw_unit, '(A, I0, 1X, I0, A)', advance='no') "draw2 ", x(i), y(i), ";"
         end if
      end do
      write(draw_unit, '(A)') "closepoly;polyfill 0"
      vdevice%cpVx = x(n)
      vdevice%cpVy = y(n)
      VOGlastx = -1; VOGlasty = -1
      VOG_fill = 0
   end function VOG_fill

   integer function noop(); noop = -1; end function
   integer function noop2(x, y); integer, intent(out) :: x, y; x = 0; y = 0; noop2 = -1; end function

   integer function VOG_draw_devcpy()
      vdevice%dev%devname   = "vog"
      vdevice%dev%large     = "large"
      vdevice%dev%small     = "small"
      vdevice%dev%Vbackb    => noop
      vdevice%dev%Vchar     => VOG_char
      vdevice%dev%Vcheckkey => noop
      vdevice%dev%Vclear    => VOG_clear
      vdevice%dev%Vcolor    => VOG_color
      vdevice%dev%Vdraw     => VOG_draw
      vdevice%dev%Vexit     => VOG_exit
      vdevice%dev%Vfill     => VOG_fill
      vdevice%dev%Vfont     => VOG_font
      vdevice%dev%Vfrontb   => noop
      vdevice%dev%Vgetkey   => noop
      vdevice%dev%Vinit     => VOG_init
      vdevice%dev%Vlocator  => noop2
      vdevice%dev%Vmapcolor => VOG_mapcolor
      vdevice%dev%Vsetlw    => VOG_setlw
      vdevice%dev%Vstring   => VOG_string
      vdevice%dev%Vswapb    => noop
      vdevice%dev%Vsync     => noop
      VOG_draw_devcpy = 0
   end function VOG_draw_devcpy

end module m_draw_drv_vog
