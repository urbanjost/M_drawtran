module m_draw_drv_cgmt
   use, intrinsic :: iso_fortran_env, only: real32, output_unit, error_unit
   use m_draw_types
   use m_draw_drivers
   implicit none
   private
   public :: CGMT_draw_devcpy

   integer, parameter :: CGMT_X_SIZE = 32767
   integer, parameter :: CGMT_Y_SIZE = 32767
   integer, parameter :: CMAPSIZE    = 256

   type :: rgb_color
      integer :: red = 0, green = 0, blue = 0
   end type rgb_color

   logical, save :: first_time = .true.
   logical, save :: drawn = .false.
   integer, save :: page_count = 1
   integer, save :: curcol = 0
   integer, save :: curwidth = 2
   integer, save :: CMAPSIZE_used = 0
   integer, save :: plotlstx = -1, plotlsty = -1

   type(rgb_color), save :: carray(CMAPSIZE)

contains

   integer function CGMT_mapcolor(i, r, g, b)
      integer, intent(in) :: i, r, g, b
      if (i >= CMAPSIZE .or. i < 0) then
         CGMT_mapcolor = -1; return
      end if
      carray(i + 1)%red   = r
      carray(i + 1)%green = g
      carray(i + 1)%blue  = b
      write(draw_unit, '(A, I0, 1X, I0, 1X, I0, 1X, I0, A)') "COLRTABLE ", i + 1, r, g, b, ";"
      CMAPSIZE_used = max(CMAPSIZE_used, i + 1)
      CGMT_mapcolor = 0
   end function CGMT_mapcolor

   integer function CGMT_init()
      integer :: i, idum
      draw_unit = draw_outfile()
      if (.not. first_time) then
         CGMT_init = 1; return
      end if

      write(draw_unit, '(A)') "BEGMF 'THIS CGM CREATED BY M_DRAW';"
      write(draw_unit, '(A)') "% Computer Graphics Metatfile %"
      write(draw_unit, '(A)') "% clear text CGM driver for M_DRAW 1.3 %"
      write(draw_unit, '(A)') "% Based on ANSI X3.122-1986 %"
      write(draw_unit, '(A)') "   MFVERSION 1;"
      write(draw_unit, '(A)') "   MFDESC 'M_DRAW CLEAR TEXT CGM';"
      write(draw_unit, '(A)') "   MFELEMLIST 'DRAWINGPLUS';"
      write(draw_unit, '(A)') "   INTEGERPREC -32768 32767;"
      write(draw_unit, '(A)') "   COLRPREC 255;"
      write(draw_unit, '(A)') "   COLRINDEXPREC 255;"
      write(draw_unit, '(A)') "   MAXCOLRINDEX 256;"
      write(draw_unit, '(A)') "   COLRVALUEEXT 0 0 0 255 255 255;"

      write(draw_unit, '(A)') "BEGMFDEFAULTS;"
      write(draw_unit, '(A)') "   VDCINTEGERPREC 0 32767;"
      write(draw_unit, '(A)') "ENDMFDEFAULTS;"

      write(draw_unit, '(A)') "BEGPIC 'PICTURE1';"
      write(draw_unit, '(A)') "   COLRMODE INDEXED;"
      write(draw_unit, '(A)') "   BACKCOLR 255 255 255;"
      write(draw_unit, '(A)') "   VDCEXT 0 0 32767 32767;"
      write(draw_unit, '(A)') "   MARKERSIZEMODE ABSTRACT;"
      write(draw_unit, '(A)') "   EDGEWIDTHMODE ABSTRACT;"
      write(draw_unit, '(A)') "   SCALEMODE ABSTRACT 0;"
      write(draw_unit, '(A)') "   LINEWIDTHMODE ABSTRACT;"
      write(draw_unit, '(A)') "BEGPICBODY;"
      write(draw_unit, '(A)') "  INTSTYLE SOLID;"
      write(draw_unit, '(A)') "  LINETYPE 1;"
      write(draw_unit, '(A)') "  LINEWIDTH 2;"
      write(draw_unit, '(A)') "  TEXTCOLR 8;"
      write(draw_unit, '(A)') "  LINECOLR 8;"
      write(draw_unit, '(A)') "  EDGECOLR 8;"
      write(draw_unit, '(A)') "  FILLCOLR 8;"

      vdevice%sizeSx = CGMT_X_SIZE
      vdevice%sizeSy = CGMT_Y_SIZE
      vdevice%sizeX  = CGMT_Y_SIZE
      vdevice%sizeY  = CGMT_Y_SIZE
      vdevice%depth  = 8

      carray = rgb_color(0, 0, 0)
      idum = CGMT_mapcolor(0, 255, 255, 255)
      idum = CGMT_mapcolor(1, 255,   0,   0)
      idum = CGMT_mapcolor(2,   0, 255,   0)
      idum = CGMT_mapcolor(3, 255, 255,   0)
      idum = CGMT_mapcolor(4,   0,   0, 255)
      idum = CGMT_mapcolor(5, 255,   0, 255)
      idum = CGMT_mapcolor(6,   0, 255, 255)
      idum = CGMT_mapcolor(7,   0,   0,   0)
      idum = CGMT_mapcolor(8, 155,   0,   0)
      idum = CGMT_mapcolor(9,   0, 155,   0)
      idum = CGMT_mapcolor(10, 155, 255, 255)
      idum = CGMT_mapcolor(11, 155, 155,   0)
      idum = CGMT_mapcolor(12,   0,   0, 155)
      idum = CGMT_mapcolor(13, 155,   0, 155)
      idum = CGMT_mapcolor(14,   0, 155, 155)
      idum = CGMT_mapcolor(15, 100, 100, 100)

      CMAPSIZE_used = 16
      write(draw_unit, '(A)') "COLRTABLE 1"
      do i = 1, CMAPSIZE_used
         write(draw_unit, '(A, I0, 1X, I0, 1X, I0)') "   ", carray(i)%red, carray(i)%green, carray(i)%blue
      end do
      write(draw_unit, '(A)') ";"

      plotlstx = -1111111; plotlsty = -1111111
      CGMT_init = 1
   end function CGMT_init

   integer function CGMT_exit()
      write(draw_unit, '(A)') "ENDPIC;"
      write(draw_unit, '(A)') "ENDMF;"
      flush(draw_unit)
      if (draw_unit /= output_unit .and. draw_unit /= error_unit) close(draw_unit)
      plotlstx = -1; plotlsty = -1
      drawn = .false.; first_time = .true.
      CGMT_exit = UNUSED
   end function CGMT_exit

   integer function CGMT_draw(x, y)
      integer, intent(in) :: x, y
      if (plotlstx /= vdevice%cpVx .or. plotlsty /= vdevice%cpVy) then
         plotlstx = vdevice%cpVx
         plotlsty = vdevice%cpVy
      end if
      write(draw_unit, '(A, I0, 1X, I0, 1X, I0, 1X, I0, A)') "LINE ", plotlstx, plotlsty, x, y, ";"
      drawn = .true.
      plotlstx = x; plotlsty = y
      CGMT_draw = UNUSED
   end function CGMT_draw

   integer function CGMT_setlw(rasters)
      integer, intent(in) :: rasters
      curwidth = max(1, abs(rasters))
      if (drawn) then
         write(draw_unit, '(A, I0, A)') "LINEWIDTH ", curwidth * vdevice%sizeX / 10000, ";"
      end if
      CGMT_setlw = UNUSED
   end function CGMT_setlw

   integer function CGMT_clear()
      integer :: i, idum
      if (drawn) then
         write(draw_unit, '(A)') "ENDPIC;"
         page_count = page_count + 1
         write(draw_unit, '(A, I0, A)') "BEGPIC 'PICTURE", page_count, "';"
         write(draw_unit, '(A)') "   COLRMODE INDEXED;"
         write(draw_unit, '(A, I0, 1X, I0, 1X, I0, A)') "   BACKCOLR ", &
            carray(curcol+1)%red, carray(curcol+1)%green, carray(curcol+1)%blue, ";"
         write(draw_unit, '(A)') "   VDCEXT 0 0 32767 32767;"
         write(draw_unit, '(A)') "   LINEWIDTHMODE ABSTRACT;"
         write(draw_unit, '(A)') "   MARKERSIZEMODE ABSTRACT;"
         write(draw_unit, '(A)') "   EDGEWIDTHMODE ABSTRACT;"
         write(draw_unit, '(A)') "   SCALEMODE ABSTRACT 0;"
         write(draw_unit, '(A)') "BEGPICBODY;"
         idum = CGMT_setlw(curwidth)
         write(draw_unit, '(A)') "COLRTABLE 1"
         do i = 1, CMAPSIZE_used
            write(draw_unit, '(A, I0, 1X, I0, 1X, I0)') "   ", carray(i)%red, carray(i)%green, carray(i)%blue
         end do
         write(draw_unit, '(A)') ";"
      end if
      drawn = .false.; plotlstx = -1; plotlsty = -1
      CGMT_clear = UNUSED
   end function CGMT_clear

   integer function CGMT_color(col)
      integer, intent(in) :: col
      integer :: idum
      if (col >= CMAPSIZE) then
         CGMT_color = 0; return
      else if (col < 0) then
         idum = CGMT_setlw(abs(col))
      else
         curcol = col
         write(draw_unit, '(A, I0, A)') "LINECOLR ", col + 1, ";"
         write(draw_unit, '(A, I0, A)') "TEXTCOLR ", col + 1, ";"
         write(draw_unit, '(A, I0, A)') "EDGECOLR ", col + 1, ";"
         write(draw_unit, '(A, I0, A)') "FILLCOLR ", col + 1, ";"
      end if
      CGMT_color = UNUSED
   end function CGMT_color

   integer function CGMT_font(fontname)
      character(len=*), intent(in) :: fontname
      if (trim(fontname) == "small" .or. trim(fontname) == "large") then
         vdevice%hwidth = 1.833_real32
         vdevice%hheight = 1.833_real32
      else
         CGMT_font = 0; return
      end if
      plotlstx = -1; plotlsty = -1
      CGMT_font = 1
   end function CGMT_font

   integer function CGMT_char(c)
      character(len=1), intent(in) :: c
      write(draw_unit, '(A, I0, 1X, I0, A, A, A)') "TEXT ", plotlstx, plotlsty, " FINAL '", c, "';"
      vdevice%cpVx = vdevice%cpVx + int(vdevice%hwidth)
      plotlstx = vdevice%cpVx
      drawn = .true.
      CGMT_char = UNUSED
   end function CGMT_char

   integer function CGMT_string(s)
      character(len=*), intent(in) :: s
      write(draw_unit, '(A, I0, 1X, I0, A, A, A)') "TEXT ", plotlstx, plotlsty, " FINAL '", trim(s), "';"
      vdevice%cpVx = vdevice%cpVx + int(vdevice%hwidth) * len(s)
      plotlstx = vdevice%cpVx
      drawn = .true.
      CGMT_string = UNUSED
   end function CGMT_string

   integer function CGMT_fill(n, x, y)
      integer, intent(in) :: n
      integer, intent(in) :: x(n), y(n)
      integer :: i

      write(draw_unit, '(A)') "INTSTYLE SOLID;"
      write(draw_unit, '(A)') "POLYGON"
      do i = 1, n
         if (mod(i - 1, 6) == 5) then
            write(draw_unit, '(I0, 1X, I0)') x(i), y(i)
         else
            write(draw_unit, '(I0, 1X, I0, 1X)', advance='no') x(i), y(i)
         end if
      end do
      write(draw_unit, '(A)') ";"
      vdevice%cpVx = x(n)
      vdevice%cpVy = y(n)
      drawn = .true.
      CGMT_fill = UNUSED
   end function CGMT_fill

   integer function CGMT_sync()
      flush(draw_unit)
      CGMT_sync = UNUSED
   end function CGMT_sync

   integer function noop(); noop = -1; end function
   integer function noop2(x, y); integer, intent(out) :: x, y; x = 0; y = 0; noop2 = -1; end function

   integer function CGMT_draw_devcpy()
      vdevice%dev%devname   = "cgmt"
      vdevice%dev%large     = "large"
      vdevice%dev%small     = "small"
      vdevice%dev%Vbackb    => noop
      vdevice%dev%Vchar     => CGMT_char
      vdevice%dev%Vcheckkey => noop
      vdevice%dev%Vclear    => CGMT_clear
      vdevice%dev%Vcolor    => CGMT_color
      vdevice%dev%Vdraw     => CGMT_draw
      vdevice%dev%Vexit     => CGMT_exit
      vdevice%dev%Vfill     => CGMT_fill
      vdevice%dev%Vfont     => CGMT_font
      vdevice%dev%Vfrontb   => noop
      vdevice%dev%Vgetkey   => noop
      vdevice%dev%Vinit     => CGMT_init
      vdevice%dev%Vlocator  => noop2
      vdevice%dev%Vmapcolor => CGMT_mapcolor
      vdevice%dev%Vsetlw    => CGMT_setlw
      vdevice%dev%Vstring   => CGMT_string

vdevice%dev%Vswapb => noop
vdevice%dev%Vsync => CGMT_sync
CGMT_draw_devcpy = UNUSED
end function CGMT_draw_devcpy
end module m_draw_drv_cgmt
