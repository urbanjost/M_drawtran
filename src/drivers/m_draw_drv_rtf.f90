module m_draw_drv_rtf
   use, intrinsic :: iso_fortran_env, only: real32, output_unit, error_unit
   use m_draw_types
   use m_draw_pref
   use m_draw_text
   use m_draw_drivers
   implicit none
   private
   public :: RTF_draw_devcpy

   integer, parameter :: RTFXSIZE = 12240
   integer, parameter :: RTFYSIZE = 15840
   integer, parameter :: CMAPSIZE = 256

   type :: rgb_color
      integer :: red = 255, green = 255, blue = 255
   end type rgb_color

   type(rgb_color), save :: RTF_carr(CMAPSIZE)

   logical, save :: RTF_first_time = .true.
   logical, save :: lineopen       = .false.
   logical, save :: shapeopen      = .false.
   integer, save :: drawn          = 0
   integer, save :: pslstx = -1, pslsty = -1
   integer, save :: MIN_LINE_X = 0, MAX_LINE_X = 0, MIN_LINE_Y = 0, MAX_LINE_Y = 0
   integer, save :: OLDX = 0, OLDY = 0
   integer, save :: RTF_MOVED      = 0
   integer, save :: rtf_count      = 0
   integer, save :: GATHER_COUNT   = 0
   integer, save :: curcol         = 0
   integer, save :: curwid         = 1
   integer, save :: curpat         = 0
   integer, save :: pgroup         = 1

   character(len=:), allocatable, save :: pGATHERLINE
   character(len=256), save :: fontstyle = ""

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
      FLIPY = vdevice%sizeSy - y
   end function FLIPY

   subroutine rtfline(ix1_in, iy1_in, ix2_in, iy2_in)
      integer, intent(in) :: ix1_in, iy1_in, ix2_in, iy2_in
      integer :: ix1, iy1, ix2, iy2, inw_x, inw_y, iw, ih

      ix1 = ix1_in; iy1 = iy1_in; ix2 = ix2_in; iy2 = iy2_in
      inw_x = min(ix1, ix2)
      inw_y = min(iy1, iy2)
      ix1 = ix1 - inw_x; ix2 = ix2 - inw_x
      iy1 = iy1 - inw_y; iy2 = iy2 - inw_y
      iw = abs(ix1 - ix2)
      ih = abs(iy1 - iy2)

      rtf_count = rtf_count + 1
      write(draw_unit, '(/, A)') "{\comment RTFLINE}"
      write(draw_unit, '(A, I0, A, I0, A, I0, A, I0, A, I0, A, I0, A, I0, 1X, A, I0, A, I0, A, I0, A)') &
         "{\*\do\dobxpage\dobypage\dodhgt", rtf_count, &
         "\dpline\dpptx", ix1, "\dppty", FLIPY(iy1), &
         "\dpptx", ix2, "\dppty", FLIPY(iy2), &
         "\dpx", inw_x, "\dpy", inw_y, &
         "\dpxsize", iw, "\dpysize", ih, &
         "\dplinew", 15, " \dplinecor0\dplinecog0\dplinecob0}"
   end subroutine rtfline

   subroutine RTF_header()
      character(len=8)  :: dt
      character(len=10) :: tm
      call date_and_time(date=dt, time=tm)

      write(draw_unit, '(A)') "{\rtf1\ansi"
      write(draw_unit, '(A)') "\deff0{\fonttbl {\f0 \froman Times New Roman;}}"
      write(draw_unit, '(A, I0, A, I0, 1X)', advance='no') "\paperw", RTFXSIZE, " \paperh", RTFYSIZE
      write(draw_unit, '(A, /)') "\deflang1033\plain\f0\fs20"
      write(draw_unit, '(A)') "{\*\generator @(#)M_DRAW RTF Driver Version 1.0.0, October  2007; John S. Urban}"
      write(draw_unit, '(A)') "{\info"
      write(draw_unit, '(A)') "{\comment ---------------------------------------------------------------------}"
      write(draw_unit, '(A)') "{\title M_DRAW RTF plots}"
      write(draw_unit, '(A, A, A, A, A, A, A, A, A, A, A, A, A)') &
         "{\creatim\yr", dt(1:4), "\mo", dt(5:6), "\dy", dt(7:8), &
         "\hr", tm(1:2), "\min", tm(3:4), "\sec", tm(5:6), "}"
      write(draw_unit, '(A)') "}"
      write(draw_unit, '(A)') "{\pard"
   end subroutine RTF_header

   integer function RTF_mapcolor(i, r, g, b)
      integer, intent(in) :: i, r, g, b
      if (i >= CMAPSIZE .or. i < 0) then
         RTF_mapcolor = -1; return
      end if
      RTF_carr(i + 1)%red   = r
      RTF_carr(i + 1)%green = g
      RTF_carr(i + 1)%blue  = b
      RTF_mapcolor = 0
   end function RTF_mapcolor

   integer function RTF_init()
      integer :: prefx, prefy, prefxs, prefys, idum

      draw_unit = draw_outfile()
      rtf_count = 0
      if (.not. RTF_first_time) then
         RTF_init = 1; return
      end if

      call RTF_header()
      call draw_getprefposandsize(prefx, prefy, prefxs, prefys)

      if (prefxs /= -1) then
         vdevice%sizeSy = prefys
         vdevice%sizeSx = prefxs
         vdevice%sizeX  = min(prefys, prefxs); vdevice%sizeY = vdevice%sizeX
      else
         vdevice%sizeSy = RTFYSIZE
         vdevice%sizeSx = RTFXSIZE
         vdevice%sizeX  = min(RTFXSIZE, RTFYSIZE); vdevice%sizeY = vdevice%sizeX
      end if

      vdevice%depth = 8
      RTF_carr = rgb_color(255, 255, 255)

      idum = RTF_mapcolor(0, 255, 255, 255)
      idum = RTF_mapcolor(1, 255,   0,   0)
      idum = RTF_mapcolor(2,   0, 255,   0)
      idum = RTF_mapcolor(3, 255, 255,   0)
      idum = RTF_mapcolor(4,   0,   0, 255)
      idum = RTF_mapcolor(5, 255,   0, 255)
      idum = RTF_mapcolor(6,   0, 255, 255)
      idum = RTF_mapcolor(7,   0,   0,   0)
      idum = RTF_mapcolor(8, 155,   0,   0)
      idum = RTF_mapcolor(9,   0, 155,   0)
      idum = RTF_mapcolor(10, 155, 255, 255)
      idum = RTF_mapcolor(11, 155, 155,   0)
      idum = RTF_mapcolor(12,   0,   0, 155)
      idum = RTF_mapcolor(13, 155,   0, 155)
      idum = RTF_mapcolor(14,   0, 155, 155)
      idum = RTF_mapcolor(15, 100, 100, 100)

      lineopen = .false.; shapeopen = .false.
      curcol = 0; curwid = 1; curpat = 0; drawn = 0
      RTF_init = 1
   end function RTF_init

   subroutine closeline()
      if (lineopen) then
         write(draw_unit, '(A, I0)') "\dppolycount", GATHER_COUNT
         if (allocated(pGATHERLINE)) write(draw_unit, '(A)') pGATHERLINE
         write(draw_unit, '(A, I0, A, I0)', advance='no') "\dpx0\dpy0\dpxsize", RTFXSIZE, "\dpysize", RTFYSIZE
         write(draw_unit, '(A, I0)', advance='no') "\dplinew", curwid
         write(draw_unit, '(A, I0, A, I0, A, I0, A)') &
            "\dplinecor", RTF_carr(curcol+1)%red, &
            "\dplinecog", RTF_carr(curcol+1)%green, &
            "\dplinecob", RTF_carr(curcol+1)%blue, "}"
         lineopen = .false.
         GATHER_COUNT = 0
         if (allocated(pGATHERLINE)) deallocate(pGATHERLINE)
      end if
   end subroutine closeline

   subroutine closeshape()
      if (shapeopen) then
         write(draw_unit, *)
         shapeopen = .false.
         GATHER_COUNT = 0
      end if
   end subroutine closeshape

   subroutine openline()
      if (.not. lineopen) then
         pGATHERLINE = ""
         GATHER_COUNT = 0
         rtf_count = rtf_count + 1
         write(draw_unit, '(A, I0, A)', advance='no') "{\*\do\dobxpage\dobypage\dodhgt", rtf_count, "\dppolyline"
         MIN_LINE_X = pslstx; MAX_LINE_X = pslstx
         MIN_LINE_Y = pslsty; MAX_LINE_Y = pslsty
         lineopen = .true.
      end if
   end subroutine openline

   subroutine openshape()
      if (.not. shapeopen) shapeopen = .true.
   end subroutine openshape

   integer function RTF_exit()
      call closeline()
      call closeshape()
      write(draw_unit, '(A, /, A)') "\par}", "}"
      drawn = 0; GATHER_COUNT = 0
      if (draw_unit /= output_unit .and. draw_unit /= error_unit) then
         flush(draw_unit)
         close(draw_unit)
      end if
      RTF_exit = 0
   end function RTF_exit

   integer function RTF_draw(x, y)
      integer, intent(in) :: x, y
      character(len=100) :: pt_str

      if (pslstx /= vdevice%cpVx .or. pslsty /= vdevice%cpVy) then
         call closeline()
         call openshape()
         call openline()
         write(pt_str, '(A, I0, A, I0)') "\dpptx", vdevice%cpVx, "\dppty", FLIPY(vdevice%cpVy)
         pGATHERLINE = trim(pt_str)
         OLDX = vdevice%cpVx; OLDY = FLIPY(vdevice%cpVy)
         RTF_MOVED = 0; GATHER_COUNT = 1
      end if
      call openshape()
      call openline()

      if (GATHER_COUNT == 0) then
         write(draw_unit, '(A)', advance='no') "{\comment RTF_draw to RTFLINE}"
         call rtfline(x, y, x + 1, y + 1)
         OLDX = x; OLDY = FLIPY(y); RTF_MOVED = 0
      else
         if (OLDX /= x .or. OLDY /= FLIPY(y)) RTF_MOVED = RTF_MOVED + 1
         OLDX = x; OLDY = FLIPY(y)
         write(pt_str, '(A, I0, A, I0)') "\dpptx", x, "\dppty", FLIPY(y)
         if (mod(GATHER_COUNT, 6) == 5) then
            pGATHERLINE = pGATHERLINE // new_line('A') // trim(pt_str)
         else
            pGATHERLINE = pGATHERLINE // " " // trim(pt_str)
         end if
      end if

      GATHER_COUNT = GATHER_COUNT + 1
      pslstx = x; pslsty = y; drawn = 1
      RTF_draw = 0
   end function RTF_draw

   integer function RTF_clear()
      call closeline()
      call closeshape()
      if (drawn /= 0) then
         pgroup = pgroup + 1
         write(draw_unit, '(A)') "\page\par}"
         write(draw_unit, '(A)') "{\pard"
      end if
      drawn = 0; GATHER_COUNT = 0
      RTF_clear = 0
   end function RTF_clear

   integer function RTF_color(col)
      integer, intent(in) :: col
      call closeline()
      call closeshape()
      if (col < 0) then
         curwid = abs(col)
      else
         curpat = col / CMAPSIZE
         curcol = mod(col, CMAPSIZE)
      end if
      RTF_color = 0
   end function RTF_color

   integer function RTF_setlw(width)
      integer, intent(in) :: width
      call closeline()
      call closeshape()
      if (width >= 0) curwid = width
      RTF_setlw = 0
   end function RTF_setlw

   integer function RTF_font(fontname)
      character(len=*), intent(in) :: fontname
      real(real32) :: rat

      vdevice%attr%a%softtext = SOFTHARDWARE
      if (vdevice%hwidth == 0.0_real32 .or. vdevice%hheight == 0.0_real32) then
         vdevice%hwidth = 11.0_real32; vdevice%hheight = 11.0_real32
      end if

      if (trim(fontname) == "small") then
         rat = 0.55_real32
         hardwidth(1:128) = 1.00_real32 * rat
         fontstyle = "font-family:'Times New Roman'"
      else if (trim(fontname) == "large") then
         rat = 1.00_real32
         hardwidth(1:128) = (real(helvetica_w(1:128), real32) / 100.0_real32) * rat
         fontstyle = "font-family:'Arial Black', Arial, sans-serif"
      else
         fontstyle = "font-family:'Arial Black', Arial, sans-serif"
         RTF_font = 0; return
      end if
      RTF_font = 1
   end function RTF_font

   subroutine esc(s)
      character(len=*), intent(in) :: s
      integer :: i, ch
      character(len=1) :: c
      do i = 1, len(s)
         c = s(i:i)
         ch = ichar(c)
         select case (ch)
          case (9)
            write(draw_unit, '(A)', advance='no') "\tab "
          case (12)
            write(draw_unit, '(A)', advance='no') "\page"
          case (92) ! '\\'
            write(draw_unit, '(A)', advance='no') "\\"
          case (123) ! '{'
            write(draw_unit, '(A)', advance='no') "\{"
          case (125) ! '}'
            write(draw_unit, '(A)', advance='no') "\}"
          case (10)
            write(draw_unit, '(/, A)', advance='no') "\line "
          case default
            if (ch < 32 .or. ch >= 128) then
               write(draw_unit, '(A, Z2.2)', advance='no') "'", ch
            else
               write(draw_unit, '(A)', advance='no') c
            end if
         end select
      end do
      write(draw_unit, *)
   end subroutine esc

   integer function RTF_string(s)
      character(len=*), intent(in) :: s
      real(real32) :: slen, sheight
      integer :: uneven, x, y
      integer, parameter :: First_Line_Height = 100
      call closeline()
      call closeshape()
      if (vdevice%hwidth == 0.0_real32 .or. vdevice%hheight == 0.0_real32) then
         write(error_unit, '(A)') "RTF_string ERROR: ZERO SIZE CHARACTERS"
         vdevice%hwidth = 11.0_real32; vdevice%hheight = 11.0_real32
      end if
      slen = draw_strlength(s) * vdevice%hwidth
      sheight = vdevice%hheight
      if (FLIPY(vdevice%cpVy) == FLIPY(int(real(vdevice%cpVy, real32) + slen * vdevice%attr%a%textsin))) then
         uneven = 1
      else
         uneven = 0
      end if
      x = int(real(vdevice%cpVx, real32) + slen * vdevice%attr%a%textcos)
      y = int(real(uneven + vdevice%cpVy, real32) + slen * vdevice%attr%a%textsin)
      write(draw_unit, '(A, I0, ",", I0, ",", I0, ")")') &
         "//rgb(", RTF_carr(curcol+1)%red, RTF_carr(curcol+1)%green, RTF_carr(curcol+1)%blue
      write(draw_unit, '(/, A)') "{\comment RTF_string}"
      write(draw_unit, '(A)', advance='no') "{\pard \pvpg\phpg"
      write(draw_unit, '(A, I0, A, I0)', advance='no') "\posx", OLDX, " \posy", OLDY - First_Line_Height
      write(draw_unit, '(A, I0, A, I0)', advance='no') "\absw", OLDX, " \absh-", OLDY + First_Line_Height
      write(draw_unit, '(A)', advance='no') "\f0\fs20"
      call esc(s)
      write(draw_unit, '(A)') "{\par"
      call esc(s)
      drawn = 1; pslstx = x; pslsty = y
      call closeshape()
      RTF_string = 0
   end function RTF_string

   integer function RTF_char(c)
      character(len=1), intent(in) :: c
      integer :: idum
      idum = RTF_string(c)
      RTF_char = 0
   end function RTF_char

   integer function RTF_fill(n, x, y)
      integer, intent(in) :: n
      integer, intent(in) :: x(n), y(n)
      integer :: i
      call closeline()
      call closeshape()
      rtf_count = rtf_count + 1
      write(draw_unit, '(A, I0, A, I0)') "{*\do\dobxpage\dobypage\dodhgt", rtf_count, "\dppolygon\dppolycount", n
      do i = 1, n
         if (mod(i - 1, 8) == 7) then
            write(draw_unit, '(A, I0, A, I0)') "\dpptx", x(i), "\dppty", FLIPY(y(i))
         else
            write(draw_unit, '(A, I0, A, I0, 1X)', advance='no') "\dpptx", x(i), "\dppty", FLIPY(y(i))
         end if
      end do
      write(draw_unit, '(/, A, I0, A, I0)') "\dpx0\dpy0\dpxsize", RTFXSIZE, "\dpysize", RTFYSIZE
      write(draw_unit, '(A, I0, A, I0, A, I0)', advance='no') &
         "\dpfillbgcr", RTF_carr(curcol+1)%red, "\dpfillbgcg", RTF_carr(curcol+1)%green, "\dpfillbgcb", RTF_carr(curcol+1)%blue
      write(draw_unit, '(A, I0, A, I0, A, I0)', advance='no') &
         "\dpfillfgcr", RTF_carr(curcol+1)%red, "\dpfillfgcg", RTF_carr(curcol+1)%green, "\dpfillfgcb", RTF_carr(curcol+1)%blue
      write(draw_unit, '(A, I0, A, I0, A, I0)') &
         "\dplinew1\dplinecor", RTF_carr(curcol+1)%red, "\dplinecog", RTF_carr(curcol+1)%green, "\dplinecob", RTF_carr(curcol+1)%blue
      write(draw_unit, '(A)') "\dpfillpat1"
      write(draw_unit, '(A)') "}"
      vdevice%cpVx = x(n); vdevice%cpVy = y(n)
      pslstx = -1; pslsty = -1; drawn = 1
      RTF_fill = 0
   end function RTF_fill

   integer function noop(); noop = -1; end function

   integer function noop2(x, y); integer, intent(out) :: x, y; x = 0; y = 0; noop2 = -1; end function

   integer function RTF_draw_devcpy()
      vdevice%dev%devname = "rtf"
      vdevice%dev%large = "large"
      vdevice%dev%small = "small"
      vdevice%dev%Vbackb => noop
      vdevice%dev%Vchar => RTF_char
      vdevice%dev%Vcheckkey => noop
      vdevice%dev%Vclear => RTF_clear
      vdevice%dev%Vcolor => RTF_color
      vdevice%dev%Vdraw => RTF_draw
      vdevice%dev%Vexit => RTF_exit
      vdevice%dev%Vfill => RTF_fill
      vdevice%dev%Vfont => RTF_font
      vdevice%dev%Vfrontb => noop
      vdevice%dev%Vgetkey => noop
      vdevice%dev%Vinit => RTF_init
      vdevice%dev%Vlocator => noop2
      vdevice%dev%Vmapcolor => RTF_mapcolor
      vdevice%dev%Vsetlw => RTF_setlw
      vdevice%dev%Vstring => RTF_string
      vdevice%dev%Vswapb => noop
      vdevice%dev%Vsync => noop
      RTF_draw_devcpy = 0
   end function RTF_draw_devcpy

end module m_draw_drv_rtf
