module m_draw_drv_xfig
   use, intrinsic :: iso_fortran_env, only: real32, real64, output_unit, error_unit
   use m_draw_types
   use m_draw_pref
   use m_draw_text
   use m_draw_drivers
   implicit none
   private
   public :: XFIG_draw_devcpy

   integer, parameter :: XSPACE_SIZE = 10200
   integer, parameter :: YSPACE_SIZE = 13200
   integer, parameter :: MAXCOLOR    = 256

   type :: rgb_color
      integer :: red = 0, green = 0, blue = 0
   end type rgb_color

   integer, save :: MAXCOLOR_used = 16
   integer, save :: page = 1
   integer, save :: drawn = 0
   integer, save :: curcol = 0
   integer, save :: xfiglstx = -1, xfiglsty = -1
   integer, save :: xfigfont = 14
   integer, save :: curwid = 1

   type(rgb_color), save :: carray(MAXCOLOR)

   integer, parameter :: helvetica_w(128) = [ &
      0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0, &
      27,33,43,55,55,88,72,28,33,33,39,58,27,33,28,28,55,55,55,55,55,55,55,55,55,55, &
      34,34,58,58,58,61,97, &
      73,73,73,73,66,61,78,72,28,55,73,61,83,72,78,66,78,72,67,61,72,66,94,66,66,61, &
      33,28,33,58,55,28, &
      55,61,55,61,55,33,61,61,28,28,55,28,88,61,61,61,61,39,55,33,61,55,78,55,55,50, &
      39,28,39,58,0 ]

contains

   pure integer function FLIPY(y)
      integer, intent(in) :: y
      FLIPY = (vdevice%sizeSy * page) - y
   end function FLIPY

   integer function xfig_mapcolor(i, r, g, b)
      integer, intent(in) :: i, r, g, b
      if (i >= MAXCOLOR .or. i < 0) then
         xfig_mapcolor = -1; return
      end if
      carray(i + 1)%red   = r
      carray(i + 1)%green = g
      carray(i + 1)%blue  = b
      MAXCOLOR_used = max(MAXCOLOR_used, i + 1)

      if (drawn == 0) then
         write(draw_unit, '(A, I0, A, 3Z2.2)') "0 ", i + 32, " #", r, g, b
      else
         write(draw_unit, '(A, I0, A, 3Z2.2)') "#0 ", i + 32, " #", r, g, b
         write(error_unit, '(A)') "xfig only supports mapcolor before any drawing"
      end if
      xfig_mapcolor = 0
   end function xfig_mapcolor

   integer function xfig_init()
      integer :: prefx, prefy, prefxs, prefys, idum
      draw_unit = draw_outfile()

      call draw_getprefposandsize(prefx, prefy, prefxs, prefys)
      if (prefxs /= -1) then
         vdevice%sizeSy = prefys
         vdevice%sizeSx = prefxs
         vdevice%sizeX = min(prefys, prefxs); vdevice%sizeY = vdevice%sizeX
      else
         vdevice%sizeSy = YSPACE_SIZE
         vdevice%sizeSx = XSPACE_SIZE
         vdevice%sizeX = min(XSPACE_SIZE, YSPACE_SIZE); vdevice%sizeY = vdevice%sizeX
      end if

      write(draw_unit, '(A)') "#FIG 3.2"
      write(draw_unit, '(A)') "Portrait"
      write(draw_unit, '(A)') "Center"
      write(draw_unit, '(A)') "Inches"
      write(draw_unit, '(A)') "Letter"
      write(draw_unit, '(A)') "100"
      write(draw_unit, '(A)') "Multiple"
      write(draw_unit, '(A)') "-2"
      write(draw_unit, '(A)') "#xfig file from M_DRAW"
      write(draw_unit, '(A)') "1200 2"
      write(draw_unit, '(A, I0, 1X, I0)') "# PAGESIZE 0 0 ", vdevice%sizeX, vdevice%sizeY

      vdevice%depth = 8
      carray = rgb_color(0, 0, 0)
      drawn = 0

      idum = xfig_mapcolor(0, 255, 255, 255)
      idum = xfig_mapcolor(1, 255,   0,   0)
      idum = xfig_mapcolor(2,   0, 255,   0)
      idum = xfig_mapcolor(3, 255, 255,   0)
      idum = xfig_mapcolor(4,   0,   0, 255)
      idum = xfig_mapcolor(5, 255,   0, 255)
      idum = xfig_mapcolor(6,   0, 255, 255)
      idum = xfig_mapcolor(7,   0,   0,   0)
      idum = xfig_mapcolor(8, 155,   0,   0)
      idum = xfig_mapcolor(9,   0, 155,   0)
      idum = xfig_mapcolor(10, 155, 255, 255)
      idum = xfig_mapcolor(11, 155, 155,   0)
      idum = xfig_mapcolor(12,   0,   0, 155)
      idum = xfig_mapcolor(13, 155,   0, 155)
      idum = xfig_mapcolor(14,   0, 155, 155)
      idum = xfig_mapcolor(15, 100, 100, 100)

      MAXCOLOR_used = 16
      page = 1
      curcol = 0
      xfiglstx = -1; xfiglsty = -1
      xfig_init = 1
   end function xfig_init

   integer function xfig_exit()
      if (drawn /= 0) then
         write(draw_unit, '(A)') "-6"
      end if
      write(draw_unit, '(A)') "# EXIT"
      flush(draw_unit)
      if (draw_unit /= output_unit .and. draw_unit /= error_unit) close(draw_unit)
      xfig_exit = 0
   end function xfig_exit

   integer function xfig_draw(x, y)
      integer, intent(in) :: x, y
      integer :: L_THICKNESS, L_PEN_COLOR, L_FILL_COLOR, L_LINE_STYLE

      if (drawn == 0) then
         write(draw_unit, '(A, I0, 1X, I0, 1X, I0)') "6 0 ", FLIPY(0), vdevice%sizeX, FLIPY(vdevice%sizeY)
         drawn = 1
      end if

      L_THICKNESS  = max(1, curwid * vdevice%sizeX * 2 / 1200 / 80 / 3)
      L_PEN_COLOR  = mod(curcol, MAXCOLOR_used) + 32
      L_FILL_COLOR = L_PEN_COLOR
      L_LINE_STYLE = curcol / MAXCOLOR_used

      write(draw_unit, '(9(I0, 1X), F0.3, 6(1X, I0))') &
         2, 1, L_LINE_STYLE, L_THICKNESS, L_PEN_COLOR, L_FILL_COLOR, &
         0, -1, -1, 0.000_real32, 1, 1, -1, 0, 0, 2
      write(draw_unit, '(1X, I0, 1X, I0, 1X, I0, 1X, I0)') vdevice%cpVx, FLIPY(vdevice%cpVy), x, FLIPY(y)

      xfiglstx = x; xfiglsty = y
      xfig_draw = 0
   end function xfig_draw

   integer function xfig_font(fontname)
      character(len=*), intent(in) :: fontname
      real(real32) :: rat

      vdevice%attr%a%softtext = SOFTHARDWARE
      if (vdevice%hwidth == 0.0_real32 .or. vdevice%hheight == 0.0_real32) then
         vdevice%hwidth = 11.0_real32 * 300.0_real32 / 72.0_real32
         vdevice%hheight = 11.0_real32 * 300.0_real32 / 72.0_real32
      end if

      if (trim(fontname) == "small") then
         xfigfont = 14
         rat = 0.55_real32
         hardwidth(1:128) = 1.00_real32 * rat
      else if (trim(fontname) == "large") then
         xfigfont = 18
         rat = 0.90_real32
         hardwidth(1:128) = (real(helvetica_w(1:128), real32) / 100.0_real32) * rat
      else
         xfig_font = 0; return
      end if
      xfig_font = 1
   end function xfig_font

   integer function xfig_clear()
      if (drawn /= 0) then
         write(draw_unit, '(A)') "-6"
         write(draw_unit, '(A, I0)') "# END PAGE ", page
         page = page + 1
         write(draw_unit, '(A, I0)') "# START PAGE ", page
      end if
      drawn = 0
      xfig_clear = 0
   end function xfig_clear

   integer function xfig_setlw(rasters)
      integer, intent(in) :: rasters
      curwid = abs(rasters)
      xfig_setlw = 0
   end function xfig_setlw

   integer function xfig_color(col)
      integer, intent(in) :: col
      integer :: idum
      if (col >= MAXCOLOR) then
         xfig_color = 0; return
      else if (col < 0) then
         idum = xfig_setlw(abs(col))
         xfig_color = 0; return
      end if
      curcol = mod(col, MAXCOLOR)
      xfig_color = 0
   end function xfig_color

   integer function xfig_string(s)
      character(len=*), intent(in) :: s
      real(real32) :: ang, slen

      if (drawn == 0) then
         write(draw_unit, '(A, I0, 1X, I0, 1X, I0)') "6 0 ", FLIPY(0), vdevice%sizeX, FLIPY(vdevice%sizeY)
         drawn = 1
      end if

      if (vdevice%hwidth == 0.0_real32 .or. vdevice%hheight == 0.0_real32) then
         write(error_unit, '(A)') "*xfig_string* ERROR: ZERO SIZE CHARACTERS"
         vdevice%hwidth = 11.0_real32 * 300.0_real32 / 72.0_real32
         vdevice%hheight = 11.0_real32 * 300.0_real32 / 72.0_real32
      end if

      ang = real(atan2(real(vdevice%attr%a%textsin, real64), real(vdevice%attr%a%textcos, real64)), real32)
      if (xfigfont == 0) then
         slen = vdevice%hwidth * real(len(s), real32)
      else
         slen = draw_strlength(s)
      end if

      write(draw_unit, '(A, I0, 1X, I0, A, I0, 1X, F0.6, 1X, F0.6, 1X, I0, 1X, F0.6, 1X, F0.6, 1X, I0, 1X, I0, 1X, A, A)') &
         "4 0 ", mod(curcol, MAXCOLOR_used) + 32, mod(curcol, MAXCOLOR_used) + 32, &
         " -1 ", xfigfont, vdevice%hwidth / 1200.0_real32 * 72.0_real32, ang, &
         4, vdevice%hheight, slen, vdevice%cpVx, FLIPY(vdevice%cpVy), s, "\001"

      xfiglstx = -1; xfiglsty = -1
      xfig_string = 0
   end function xfig_string

   integer function xfig_char(c)
      character(len=1), intent(in) :: c
      integer :: idum
      idum = xfig_string(c)
      xfig_char = 0
   end function xfig_char

   integer function xfig_fill(n, x, y)
      integer, intent(in) :: n
      integer, intent(in) :: x(n), y(n)
      integer :: i, AREA_FILL, FILL_COLOR, NPOINTS

      if (drawn == 0) then
         write(draw_unit, '(A, I0, 1X, I0, 1X, I0)') "6 0 ", FLIPY(0), vdevice%sizeX, FLIPY(vdevice%sizeY)
         drawn = 1
      end if

      write(draw_unit, '(A)') "# POLYGON"
      AREA_FILL = curcol / MAXCOLOR_used
      if (AREA_FILL == 0) then
         AREA_FILL = 20
      else
         AREA_FILL = 40 + min(AREA_FILL, 22)
      end if

      FILL_COLOR = mod(curcol, MAXCOLOR_used) + 32
      NPOINTS = n + 1

      write(draw_unit, '(9(I0, 1X), F0.3, 6(1X, I0))') &
         2, 3, 0, 0, -1, FILL_COLOR, 0, -1, AREA_FILL, 0.000_real32, 1, 1, 0, 0, 0, NPOINTS

      do i = 1, n
         write(draw_unit, '(1X, I0, 1X, I0)') x(i), FLIPY(y(i))
      end do
      write(draw_unit, '(1X, I0, 1X, I0, /, A)') x(1), FLIPY(y(1)), "# END POLYGON"

      vdevice%cpVx = x(n)
      vdevice%cpVy = y(n)
      xfiglstx = -1; xfiglsty = -1
      xfig_fill = 0
   end function xfig_fill

   integer function noop(); noop = -1; end function
   integer function noop2(x, y); integer, intent(out) :: x, y; x = 0; y = 0; noop2 = -1; end function

   integer function XFIG_draw_devcpy()
      vdevice%dev%devname   = "xfig"
      vdevice%dev%large     = "large"
      vdevice%dev%small     = "small"
      vdevice%dev%Vbackb    => noop
      vdevice%dev%Vchar     => xfig_char
      vdevice%dev%Vcheckkey => noop
      vdevice%dev%Vclear    => xfig_clear
      vdevice%dev%Vcolor    => xfig_color
      vdevice%dev%Vdraw     => xfig_draw
      vdevice%dev%Vexit     => xfig_exit

vdevice%dev%Vfill => xfig_fill
vdevice%dev%Vfont => xfig_font
vdevice%dev%Vfrontb => noop
vdevice%dev%Vgetkey => noop
vdevice%dev%Vinit => xfig_init
vdevice%dev%Vlocator => noop2
vdevice%dev%Vmapcolor => xfig_mapcolor
vdevice%dev%Vsetlw => xfig_setlw
vdevice%dev%Vstring => xfig_string
vdevice%dev%Vswapb => noop
vdevice%dev%Vsync => noop
XFIG_draw_devcpy = 0
end function XFIG_draw_devcpy
end module m_draw_drv_xfig
