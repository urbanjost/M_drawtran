module m_draw_drv_svg
   use, intrinsic :: iso_fortran_env, only: real32, real64, output_unit, error_unit
   use m_draw_types
   use m_draw_pref
   use m_draw_drivers
   implicit none
   private
   public :: SVG_draw_devcpy

   integer, parameter :: SVGYSIZE = 5500
   integer, parameter :: SVGXSIZE = 4250
   integer, parameter :: SVGTORAS = 1000
   integer, parameter :: CMAPSIZE = 256

   type :: rgb_color
      integer :: red = 255, green = 255, blue = 255
   end type rgb_color

   type(rgb_color), save :: svg_carr(CMAPSIZE)

   logical, save :: svg_first_time = .true.
   logical, save :: PolyLineOpen   = .false.
   logical, save :: ObjectOpen     = .false.
   integer, save :: points         = 0
   integer, save :: drawn          = 0
   integer, save :: LAST_X = -1, LAST_Y = -1
   integer, save :: SVG_MOVED      = 0
   integer, save :: curcol         = 0
   integer, save :: curwid         = 1
   integer, save :: curpat         = 0
   integer, save :: pgroup         = 1

   character(len=30), save :: hardfont = "monospace"

   integer, parameter :: helvetica_w(128) = [ &
      0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0, &
      27, 53, 68, 107, 107, 171, 128, 37, 64, 64, 75, 112, 53, 64, 53, 53, &
      107, 93, 107, 107, 107, 107, 107, 107, 107, 107, 53, 53, 112, 112, 112, 107, &
      195, 128, 128, 139, 139, 128, 117, 149, 139, 53, 96, 128, 107, 160, 139, 149, &
      128, 149, 139, 128, 117, 139, 128, 181, 128, 128, 117, 53, 53, 53, 90, 107, &
      64, 107, 107, 96, 107, 107, 50, 107, 107, 43, 43, 96, 43, 160, 107, 107, &
      107, 107, 64, 96, 53, 107, 96, 139, 96, 96, 96, 64, 50, 64, 112, 144 ]

contains

   pure integer function FLIPY(y)
      integer, intent(in) :: y
      FLIPY = vdevice%sizeSy - y
   end function FLIPY

   subroutine SVG_header()
      character(len=8)  :: dt
      character(len=10) :: tm
      call date_and_time(date=dt, time=tm)

      write(draw_unit, '(A)') '<?xml version="1.0" encoding="ISO-8859-1" standalone="no"?>'
      write(draw_unit, '(A)') '<!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.1//EN"'
      write(draw_unit, '(A)') '"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd">'
      write(draw_unit, '(A)') '<!-- Creator: M_DRAW SVG driver 1.0 2005-03-25 -->'
      write(draw_unit, '(A, A, "-", A, "-", A, " ", A, ":", A, ":", A, A)') &
         '<!-- CreationDate: ', dt(1:4), dt(5:6), dt(7:8), tm(1:2), tm(3:4), tm(5:6), ' -->'
   end subroutine SVG_header

   integer function SVG_mapcolor(i, r, g, b)
      integer, intent(in) :: i, r, g, b
      if (i >= CMAPSIZE .or. i < 0) then
         SVG_mapcolor = -1; return
      end if
      svg_carr(i + 1)%red   = r
      svg_carr(i + 1)%green = g
      svg_carr(i + 1)%blue  = b
      SVG_mapcolor = 0
   end function SVG_mapcolor

   integer function SVG_init()
      integer :: prefx, prefy, prefxs, prefys, idum
      draw_unit = draw_outfile()
      if (.not. svg_first_time) then
         SVG_init = 1; return
      end if

      call SVG_header()
      call draw_getprefposandsize(prefx, prefy, prefxs, prefys)

      if (prefxs /= -1) then
         vdevice%sizeSy = prefys
         vdevice%sizeSx = prefxs
         vdevice%sizeX  = min(prefys, prefxs); vdevice%sizeY = vdevice%sizeX
      else
         vdevice%sizeSy = SVGYSIZE
         vdevice%sizeSx = SVGXSIZE
         vdevice%sizeX  = min(SVGXSIZE, SVGYSIZE); vdevice%sizeY = vdevice%sizeX
      end if

      write(draw_unit, '(A, F0.6, A, F0.6, A, I0, 1X, I0, A)') &
         '<svg xmlns="http://www.w3.org/2000/svg" width="', real(vdevice%sizeSx, real32)/SVGTORAS, &
         'in" height="', real(vdevice%sizeSy, real32)/SVGTORAS, &
         'in" viewBox="0 0 ', vdevice%sizeSx, vdevice%sizeSy, '">'

      write(draw_unit, '(A)') '<desc> M_DRAW SVG pages</desc>'
      write(draw_unit, '(A, I0, A)') &
         '<g style=" stroke-width:', max(1, vdevice%sizeX * curwid / 10000), &
         'px;stroke:black;stroke-linecap:round;stroke-linejoin:round;fill:none;fill-rule:even-odd;' // &
         'fill-opacity:1;font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;' // &
         'font-size-adjust:none;letter-spacing:normal;word-spacing:normal;text-anchor:start" xml:space="preserve">'

      write(draw_unit, '(A)') '<defs>'
      write(draw_unit, '(A)') '<path id="baseline" d="M0,0L2000000,0" style="fill:none"/>'
      write(draw_unit, '(A)') '</defs>'
      write(draw_unit, '(A, I0, A)') '<!-- id="Page', pgroup, '" -->'

      vdevice%depth = 8
      svg_carr = rgb_color(255, 255, 255)

      idum = SVG_mapcolor(0, 255, 255, 255)
      idum = SVG_mapcolor(1, 255,   0,   0)
      idum = SVG_mapcolor(2,   0, 255,   0)
      idum = SVG_mapcolor(3, 255, 255,   0)
      idum = SVG_mapcolor(4,   0,   0, 255)
      idum = SVG_mapcolor(5, 255,   0, 255)
      idum = SVG_mapcolor(6,   0, 255, 255)
      idum = SVG_mapcolor(7,   0,   0,   0)
      idum = SVG_mapcolor(8, 155,   0,   0)
      idum = SVG_mapcolor(9,   0, 155,   0)
      idum = SVG_mapcolor(10, 155, 255, 255)
      idum = SVG_mapcolor(11, 155, 155,   0)
      idum = SVG_mapcolor(12,   0,   0, 155)
      idum = SVG_mapcolor(13, 155,   0, 155)
      idum = SVG_mapcolor(14,   0, 155, 155)
      idum = SVG_mapcolor(15, 100, 100, 100)

      PolyLineOpen = .false.; ObjectOpen = .false.
      curcol = 0; curwid = 1; curpat = 0; drawn = 0
      SVG_init = 1
   end function SVG_init

   subroutine closeline()
      if (PolyLineOpen) then
         if (SVG_MOVED == 0) then
            write(draw_unit, '(A, /, A, I0, A, I0, A, I0, A)') &
               '"/>', '<circle cx="', LAST_X, '" cy="', FLIPY(LAST_Y), '" r="', &
               max(1, vdevice%sizeX * curwid / 10000 / 2), '" stroke="none"/>'
         else
            write(draw_unit, '(A)') '"/>'
         end if
         PolyLineOpen = .false.
         points = 0
      end if
   end subroutine closeline

   subroutine closeObject()
      if (ObjectOpen) then
         write(draw_unit, '(A)') '</g>'
         ObjectOpen = .false.
         points = 0
      end if
   end subroutine closeObject

   subroutine openline()
      if (.not. PolyLineOpen) then
         write(draw_unit, '(A)', advance='no') '<path d="M'
         PolyLineOpen = .true.
      end if
   end subroutine openline

   subroutine openObject()
      if (.not. ObjectOpen) then
         write(draw_unit, '(/, A, I0, A, 3Z2.2, A)') &
            '<g style="stroke-width:', max(1, vdevice%sizeX * curwid / 10000), &
            'px; stroke:#', svg_carr(curcol+1)%red, svg_carr(curcol+1)%green, svg_carr(curcol+1)%blue, '; fill:none">'
         ObjectOpen = .true.
      end if
   end subroutine openObject

   integer function SVG_exit()
      call closeline()
      call closeObject()
      write(draw_unit, '(A)') '</g>'
      write(draw_unit, '(A)') '</svg>'
      write(draw_unit, '(A)') '<!--- End of Document  -->'
      drawn = 0; points = 0
      if (draw_unit /= output_unit .and. draw_unit /= error_unit) then
         flush(draw_unit)
         close(draw_unit)
      end if
      SVG_exit = 0
   end function SVG_exit

   integer function SVG_draw(x, y)
      integer, intent(in) :: x, y

      if (LAST_X /= vdevice%cpVx .or. LAST_Y /= vdevice%cpVy) then
         call closeline()
         call openObject()
         call openline()
         write(draw_unit, '(I0, ",", I0)', advance='no') vdevice%cpVx, FLIPY(vdevice%cpVy)
         LAST_X = vdevice%cpVx; LAST_Y = vdevice%cpVy
         SVG_MOVED = 0; points = 1
      end if
      call openline()

      if (points == 0) then
         write(draw_unit, '(I0, ",", I0)', advance='no') x, FLIPY(y)
         SVG_MOVED = 0
      else
         if (LAST_X /= x .or. LAST_Y /= y) SVG_MOVED = SVG_MOVED + 1
         if (mod(points, 8) == 7) then
            write(draw_unit, '(A, I0, ",", I0)') 'L', x, FLIPY(y)
         else
            write(draw_unit, '(A, I0, ",", I0)', advance='no') ' L', x, FLIPY(y)
         end if
      end if

      points = points + 1
      LAST_X = x; LAST_Y = y; drawn = 1
      SVG_draw = 0
   end function SVG_draw

   integer function SVG_clear()
      call closeline()
      call closeObject()
      if (drawn /= 0) then
         write(draw_unit, '(A, I0, A)') '</g><!-- End Page', pgroup, ' -->'
         pgroup = pgroup + 1
         write(draw_unit, '(A, I0, A)') '<g id="Page', pgroup, '">'
      end if
      drawn = 0; points = 0; SVG_MOVED = 0
      SVG_clear = 0
   end function SVG_clear

   integer function SVG_color(col)
      integer, intent(in) :: col
      call closeline()
      call closeObject()
      if (col < 0) then
         curwid = abs(col)
      else
         curpat = col / CMAPSIZE
         curcol = mod(col, CMAPSIZE)
      end if
      SVG_color = 0
   end function SVG_color

   integer function SVG_setlw(width)
      integer, intent(in) :: width
      call closeline()
      call closeObject()
      if (width >= 0) curwid = width
      SVG_setlw = 0
   end function SVG_setlw

   integer function SVG_font(fontname)
      character(len=*), intent(in) :: fontname
      real(real32) :: rat

      vdevice%attr%a%softtext = SOFTHARDWARE
      if (vdevice%hwidth == 0.0_real32 .or. vdevice%hheight == 0.0_real32) then
         vdevice%hwidth  = 11.0_real32 * 300.0_real32 / 72.0_real32
         vdevice%hheight = 11.0_real32 * 300.0_real32 / 72.0_real32
      end if

      if (trim(fontname) == "small") then
         hardfont = "monospace"
         rat = 1.00_real32
         hardwidth(1:128) = 1.00_real32 * rat
      else if (trim(fontname) == "large") then
         hardfont = "Helvetica,serif"
         rat = 0.795_real32
         hardwidth(1:128) = (real(helvetica_w(1:128), real32) / 100.0_real32) * rat
      else
         SVG_font = 0; return
      end if
      SVG_font = 1
   end function SVG_font

   integer function SVG_string(s)
      character(len=*), intent(in) :: s
      integer :: i, idx
      real(real32) :: slength, rot
      character(len=1) :: c

      call closeline()

      rot = real(atan2(real(vdevice%attr%a%textsin, real64), real(vdevice%attr%a%textcos, real64)) * 180.0_real64 / PI, real32)
      write(draw_unit, '(A)', advance='no') '<text stroke="none" lengthAdjust="spacingAndGlyphs"'
      write(draw_unit, '(A, 3Z2.2, A)') ' fill="#', &
         svg_carr(curcol+1)%red, svg_carr(curcol+1)%green, svg_carr(curcol+1)%blue, '"'
      if (rot == 0.0_real32) then
         write(draw_unit, '(A, I0, A, I0, A, A, A, I0, A)', advance='no') &
            ' x="', vdevice%cpVx, '" y="', FLIPY(vdevice%cpVy), &
            '" font-family="', trim(hardfont), '" font-size="', int(vdevice%hheight), '" '
      else
         write(draw_unit, '(A, I0, ",", I0, A, F0.6, A, A, A, I0, A)', advance='no') &
            ' transform="translate(', vdevice%cpVx, FLIPY(vdevice%cpVy), ') rotate(', -rot, &
            ')" x="0" y="0" font-family="', trim(hardfont), '" font-size="', int(vdevice%hheight), '" '
      end if
      slength = 0.0_real32
      do i = 1, len(s)
         idx = ichar(s(i:i)) + 1
         if (idx >= 1 .and. idx <= 128) slength = slength + hardwidth(idx)
      end do
      slength = slength * vdevice%hwidth
      if (slength <= 0.0_real32) slength = vdevice%hwidth * real(len(s), real32)
      write(draw_unit, '(A, I0, A)') 'textLength="', int(slength), '">'

      do i = 1, len(s)
         c = s(i:i)
         select case (c)
         case ('&')
            write(draw_unit, '(A)', advance='no') char(38) // 'amp;'
         case ('<')
            write(draw_unit, '(A)', advance='no') char(38) // 'lt;'
         case ('>')
            write(draw_unit, '(A)', advance='no') char(38) // 'gt;'
         case ('"')
            write(draw_unit, '(A)', advance='no') char(38) // 'quot;'
         case ("'")
            write(draw_unit, '(A)', advance='no') char(38) // 'apos;'
         case default
            write(draw_unit, '(A)', advance='no') c
         end select
      end do
      write(draw_unit, '(/, A)') '</text>'
      drawn = 1
      LAST_X = int(real(vdevice%cpVx, real32) + slength * vdevice%attr%a%textcos)
      LAST_Y = int(real(vdevice%cpVy, real32) + slength * vdevice%attr%a%textsin)
   !/* debug: draw a line under text */
   !/*fprintf(draw_fp,"<line x1=\"%d\" y1=\"%d\" x2=\"%d\" y2=\"%d\"/>\n",vdevice.cpVx,FLIPY(vdevice.cpVy),LAST_X,FLIPY(LAST_Y));*/
      SVG_string = 0
   end function SVG_string

   integer function SVG_char(c)
      character(len=1), intent(in) :: c
      integer :: idum
      idum = SVG_string(c)
      SVG_char = 0
   end function SVG_char

   integer function SVG_fill(n, x, y)
      integer, intent(in) :: n
      integer, intent(in) :: x(n), y(n)
      integer :: i
      call closeline()
      call closeObject()
      write(draw_unit, '(A, I0, ",", I0, A)') '<path d="M ', x(1), FLIPY(y(1)), ' L'
      do i = 2, n
         if (mod(i - 1, 8) == 7) then
            write(draw_unit, '(I0, ",", I0)') x(i), FLIPY(y(i))
         else
            write(draw_unit, '(I0, ",", I0, 1X)', advance='no') x(i), FLIPY(y(i))
         end if
      end do
      write(draw_unit, '(A, 3Z2.2, A, I0, A, 3Z2.2, A)') &
         ' z" style="stroke:#', svg_carr(curcol+1)%red, svg_carr(curcol+1)%green, svg_carr(curcol+1)%blue, &
         ';stroke-width:', max(1, vdevice%sizeX * curwid / 10000), &
         'px;fill:#', svg_carr(curcol+1)%red, svg_carr(curcol+1)%green, svg_carr(curcol+1)%blue, '"/>'
      vdevice%cpVx = x(n); vdevice%cpVy = y(n)
      LAST_X = vdevice%cpVx; LAST_Y = vdevice%cpVy
      drawn = 1
      SVG_fill = 0
   end function SVG_fill

   integer function noop(); noop = -1; end function

   integer function noop2(x, y); integer, intent(out) :: x, y; x = 0; y = 0; noop2 = -1; end function

   integer function SVG_draw_devcpy()
      vdevice%dev%devname = "svg"
      vdevice%dev%large = "large"
      vdevice%dev%small = "small"
      vdevice%dev%Vbackb => noop
      vdevice%dev%Vchar => SVG_char
      vdevice%dev%Vcheckkey => noop
      vdevice%dev%Vclear => SVG_clear
      vdevice%dev%Vcolor => SVG_color
      vdevice%dev%Vdraw => SVG_draw
      vdevice%dev%Vexit => SVG_exit
      vdevice%dev%Vfill => SVG_fill
      vdevice%dev%Vfont => SVG_font
      vdevice%dev%Vfrontb => noop
      vdevice%dev%Vgetkey => noop
      vdevice%dev%Vinit => SVG_init
      vdevice%dev%Vlocator => noop2
      vdevice%dev%Vmapcolor => SVG_mapcolor
      vdevice%dev%Vsetlw => SVG_setlw
      vdevice%dev%Vstring => SVG_string
      vdevice%dev%Vswapb => noop
      vdevice%dev%Vsync => noop
      SVG_draw_devcpy = 0
   end function SVG_draw_devcpy

end module m_draw_drv_svg
