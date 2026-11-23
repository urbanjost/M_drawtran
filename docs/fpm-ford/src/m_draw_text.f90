module m_draw_text
   use, intrinsic :: iso_fortran_env, only: real32, real64, int8, error_unit
   use m_draw_types
   use m_draw_verror
   use m_draw_matrix
   use m_draw_mapping
   use m_draw_move
   use m_draw_draw
   use m_draw_attr
   implicit none
   private
   public :: draw_font, draw_numchars, draw_hershfont, draw_getcharsize, &
      draw_drawchar, draw_textsize, draw_getfontwidth, draw_getfontheight, &
      draw_getfontdec, draw_getfontasc, draw_getfontsize, draw_drawstr, &
      draw_strlength, draw_boxtext, draw_boxfit, draw_centertext, &
      draw_textjustify, draw_xcentertext, draw_ycentertext, &
      draw_leftjustify, draw_rightjustify, draw_topjustify, &
      draw_bottomjustify, draw_fixedwidth, draw_textang, &
      draw_textslant, draw_textweight

      interface draw_centertext
         module procedure draw_centertext_integer
         module procedure draw_centertext_logical
      end interface draw_centertext

      interface draw_fixedwidth
         module procedure draw_fixedwidth_integer
         module procedure draw_fixedwidth_logical
      end interface draw_fixedwidth

      interface draw_drawchar
         module procedure draw_drawchar_integer
         module procedure draw_drawchar_character
      end interface draw_drawchar

   real(real32), save :: SCSIZEX = 1.0_real32, SCSIZEY = 1.0_real32
   integer, save :: Loaded = 0, nchars = 0

   type :: font_table_t
      integer(int8), allocatable :: p(:)
      integer, allocatable       :: ind(:)
      integer :: as = 0, dec = 0, mw = 0
   end type font_table_t

   type(font_table_t), save :: ftab
   real(real32), parameter :: btab_x(5) = [0.0_real32, 0.5_real32, 0.0_real32, 1.0_real32, 1.0_real32]
   real(real32), parameter :: btab_y(5) = [0.0_real32, 0.5_real32, 1.0_real32, 1.0_real32, 0.0_real32]

   interface
      function draw_newtokens(num) result(toks)
         import :: Token
         integer, intent(in) :: num
         type(Token), pointer :: toks(:)
      end function draw_newtokens
   end interface

contains

   pure integer function xcoord(val)
      integer, intent(in) :: val
      xcoord = val - ichar('R')
   end function xcoord

   pure integer function ycoord(val)
      integer, intent(in) :: val
      ycoord = ichar('R') - val
   end function ycoord

   pure integer function skew_x(x, y)
      integer, intent(in) :: x, y
      skew_x = int(real(x, real32) + vdevice%attr%a%skew * real(y, real32))
   end function skew_x

   pure function basename(path) result(base)
      character(len=*), intent(in) :: path
      character(len=:), allocatable :: base
      integer :: idx
      idx = max(index(path, '/', back=.true.), index(path, '\', back=.true.))
      if (idx > 0 .and. idx < len(path)) then
         base = path(idx+1:)
      else
         base = path
      end if
   end function basename

!>
!!
!!##NAME
!!    font(3f) - [M_draw:TEXT] Set the current font by name
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!         subroutine font(fontname)
!!         character(len=*),intent(in) :: fontname
!!
!!##DESCRIPTION
!!    Set the current font.
!!
!!    M_draw supports hardware and software fonts. The software fonts are based
!!    on the character set digitized by Dr Allen V. Hershey while working at
!!    the U. S. National Bureau of Standards. Exactly what hardware fonts are
!!    supported depends on the device, but it is guaranteed that the names
!!    "large" and "small" will result in something readable. For X11 displays
!!    the default large and small fonts used by the program can be overridden
!!    by placing the following defaults in the ~/.Xdefaults file:
!!
!!      draw.smallfont: X11-font-name
!!      draw.largefont: X11-font-name
!!
!!    It is noted here that hardware text is always assumed to be drawn
!!    parallel to the (x, y) plane, using whatever the current z coordinate
!!    is. The following software fonts are supported:
!!
!!       astrology       cursive         cyrillic        futura.l
!!       futura.m        gothic.eng      gothic.ger      gothic.ita
!!       greek           markers         math.low        math.upp
!!       meteorology     music           script          symbolic
!!       times.g         times.i         times.ib        times.r
!!       times.rb        japanese
!!
!!    A markers font "markers" is also provided for doing markers - you need
!!    to have centertext mode on for this to give sensible results when placing
!!    the markers.
!!
!!    If the environment variable "M_DRAW_FONTPATH" is set M_draw looks for the software
!!    fonts in the directory given by this value.
!!
!!    WHEN ASKED FOR NON-EXISTENT FONT NAMES, FONT(3f) STOPS THE PROGRAM.
!!
!!##EXAMPLE
!!
!!   Sample Program:
!!
!!    program demo_font
!!    use :: M_draw
!!    use M_draw,    only  : D_BLACK,   D_WHITE
!!    use M_draw,    only  : D_RED,     D_GREEN,    D_BLUE
!!    use M_draw,    only  : D_YELLOW,  D_MAGENTA,  D_CYAN
!!    implicit none
!!    real    :: left
!!    real    :: baseline=80.0
!!    integer :: icolor=1
!!    integer :: ipaws
!!       !! set up drawing surface
!!       call prefsize(400, 400)
!!       call vinit(' ') ! start graphics using device $M_DRAW_DEVICE
!!       call page(-100.0, 100.0, -100.0, 100.0)
!!       call color(D_WHITE)
!!       call clear()
!!       call textsize(10.0, 10.0)
!!       !! place a vertical line along the edge
!!       call color(D_RED)
!!       call move2(-90.0, -90.0)
!!       call draw2(-90.0, 90.0)
!!       !! make a centered title at top a bit bolder and bigger
!!       call xcentertext()
!!       call textsize(13.0, 13.0)
!!       call linewidth(90)
!!       left=0
!!       call nextline('Font Samples')
!!       !! print the font samples
!!       left=-90
!!       call linewidth(40)
!!       call textsize(10.0, 10.0)
!!       call centertext(.false.)
!!       icolor=icolor-1
!!       call nextline('DEFAULT (ie. futura.l)')
!!       icolor=icolor-1
!!       call nextline('now call font(3f) ...')
!!       call nextline('A nice SIMPLEX font, futura.l')
!!       call nextline('A COMPLEX font, times.r')
!!       call nextline('ITALIC letters,  times.i')
!!       call nextline('DUPLEX is in between, futura.m')
!!       ipaws=getkey()
!!       call vexit()
!!    contains
!!    subroutine nextline(string)
!!    character(len=*) :: string
!!    !! reduce some duplicate code; very specific to this EXAMPLE
!!       integer :: iend
!!       iend=index(string,',')  ! if comma, assume font name found
!!       write(*,*)'FONT=',string(iend+1:),iend
!!       if(iend.ne.0)call font(trim(adjustl(string(iend+1:)))) ! change font
!!       icolor=icolor+1         ! set pen color
!!       call color(icolor)
!!       baseline=baseline-20    ! move down before drawing line
!!       call move2(left, baseline)
!!       call drawstr(string)    ! draw string
!!    end subroutine nextline
!!
!!    end program demo_font
   recursive subroutine draw_font(name)
      character(len=*), intent(in) :: name
      type(Token), pointer :: tok(:)
      character(len=:), allocatable :: leaf

      if (vdevice%initialized == 0) call draw_verror("font: draw not initialized")

      if (vdevice%inobject /= 0) then
         tok => draw_newtokens(2)
         tok(1)%i = OBJ_FONT
         tok(2)%s = trim(name)
         return
      end if

      leaf = basename(trim(name))
      if (leaf == trim(vdevice%attr%a%font)) return

      vdevice%attr%a%softtext = REALHARDWARE

      if (draw_hershfont(trim(name)) /= 0) then
         vdevice%attr%a%softtext = HERSHEY
         return
      else if (trim(name) == "large") then
         if (vdevice%dev%Vfont(trim(vdevice%dev%large)) == 0) call draw_verror("font: unable to open large font")
      else if (trim(name) == "small") then
         if (vdevice%dev%Vfont(trim(vdevice%dev%small)) == 0) call draw_verror("font: unable to open small font")
      else if (vdevice%dev%Vfont(trim(name)) == 0) then
         write(error_unit, '(A)') "font: unable to open fontfile " // trim(name)
         call draw_font("large")
         return
      end if

      if (vdevice%attr%a%softtext == SOFTHARDWARE) then
         call draw_textsize(vdevice%attr%a%fontwidth, vdevice%attr%a%fontheight)
      end if
      vdevice%attr%a%font = leaf
   end subroutine draw_font

!>
!!##NAME
!!    numchars(3f) - [M_draw:TEXT] Return number of characters in the current SOFTWARE font.
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!         integer function numchars()
!!
!!##DESCRIPTION
!!    Return the number of characters in the current font. Applicable only to
!!    software fonts.
   integer function draw_numchars()
      draw_numchars = merge(nchars, 128, vdevice%attr%a%softtext == HERSHEY)
   end function draw_numchars

   integer function draw_hershfont(fontname)
      character(len=*), intent(in) :: fontname
      integer :: u, ios, i, nvects, n, pos_idx, env_len
      character(len=512) :: path, flib
      logical :: exists

      call get_environment_variable("M_DRAW_FONTPATH", value=flib, length=env_len, status=ios)
      if (ios /= 0 .or. env_len == 0) then
         path = FONTLIB // trim(fontname)
      else
         path = trim(flib) // "/" // trim(fontname)
      end if

      inquire(file=trim(path), exist=exists)
      if (.not. exists) then
         path = trim(fontname)
         inquire(file=trim(path), exist=exists)
         if (.not. exists) then
            draw_hershfont = 0
            return
         end if
      end if

      open(newunit=u, file=trim(path), access='stream', form='unformatted', status='old', action='read', iostat=ios)
      if (ios /= 0) then; draw_hershfont = 0; return; end if

      read(u, iostat=ios) nchars, nvects, ftab%as, ftab%dec, n
      if (ios /= 0) then; close(u); draw_hershfont = 0; return; end if
      ftab%mw = int(real(n, real32) + vdevice%attr%a%skew * real(ftab%as, real32))

      if (Loaded /= 0) then
         if (allocated(ftab%p)) deallocate(ftab%p)
         if (allocated(ftab%ind)) deallocate(ftab%ind)
         Loaded = 0
      end if

      allocate(ftab%ind(nchars + 1), ftab%p(2 * nvects))
      pos_idx = 1
      do i = 1, nchars
         read(u, iostat=ios) n
         if (ios /= 0) then; close(u); draw_hershfont = 0; return; end if
         if (n > 0) then
            read(u, iostat=ios) ftab%p(pos_idx : pos_idx + n - 1)
            if (ios /= 0) then; close(u); draw_hershfont = 0; return; end if
         end if
         ftab%ind(i) = pos_idx
         pos_idx = pos_idx + n
      end do
      ftab%ind(nchars + 1) = pos_idx
      close(u)

      Loaded = 1
      vdevice%attr%a%softtext = Loaded
      vdevice%attr%a%font = basename(trim(fontname))
      draw_hershfont = 1
   end function draw_hershfont
!>
!!##NAME
!!    getcharsize(3f) - [M_draw:TEXT] Get the width and height of a character.
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!         subroutine getcharsize(c, width, height)
!!         character*1 c
!!         real width, height
!!
!!##DESCRIPTION
!!    Get the width and height of a character. At the moment the height
!!    returned is always that of the difference between the maximum descender
!!    and ascender.
   subroutine draw_getcharsize(c, width, height)
      character(len=1), intent(in) :: c
      real(real32), intent(out)    :: width, height
      integer :: i, ii, idx_start
      real(real32) :: a, b
      real(real32), parameter :: fudge = 0.0123_real32

      if (vdevice%initialized == 0) call draw_verror("getcharsize: draw not initialized")

      if (vdevice%attr%a%softtext == HERSHEY) then
         if (Loaded == 0) call draw_verror("getcharsize: no software font loaded")
         height = real(ftab%as - ftab%dec, real32) * SCSIZEY
         if (vdevice%attr%a%fixedwidth /= 0) then
            width = real(ftab%mw, real32) * SCSIZEX
         else
            ii = max(1, min(nchars, ichar(c) - 32 + 1))
            idx_start = ftab%ind(ii)
            width = real(int(ftab%p(idx_start + 1)) - int(ftab%p(idx_start)), real32) * SCSIZEX
         end if
      else if (vdevice%attr%a%softtext == SOFTHARDWARE) then
         i = ichar(c) + 1
         if (i >= 1 .and. i <= 128) then
            width = hardwidth(i) * draw_getfontwidth()
            height = draw_getfontheight()
         else
            height = 0.0_real32; width = 0.0_real32
         end if
      else if (vdevice%attr%a%softtext == REALHARDWARE) then
         call draw_VtoWxy(vdevice%hwidth + fudge, vdevice%hheight + fudge, width, height)
         call draw_VtoWxy(fudge, fudge, a, b)
         height = height - b; width = width - a
      end if
   end subroutine draw_getcharsize

!>
!!##NAME
!!    drawchar(3f) - [M_draw:TEXT] Draw the character c and update current position.
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!         subroutine drawchar(ch)
!!         character(len=1),intent(in) :: ch
!!
!!##DESCRIPTION
!!
!!    Draw the character c at the current position. The current graphics
!!    position represents the bottom left hand corner of the character space.
!!
!!    Uses current line color and thickness and text justification mode.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_drawchar
!!    !
!!    !      display all the hershey fonts and demonstrate textang
!!    !
!!
!!    use M_draw, only: vinit, vsetflush, color, clear, font, vexit, vflush
!!    use M_draw, only: ortho2, textang, boxtext, rect, textsize, getkey
!!    use M_draw,    only  : D_BLACK,   D_WHITE
!!    use M_draw,    only  : D_RED,     D_GREEN,    D_BLUE
!!    use M_draw,    only  : D_YELLOW,  D_MAGENTA,  D_CYAN
!!    use M_draw
!!    implicit none
!!
!!    character(len=40)   :: str1, str2, str3, str4, fonts(22)
!!    character(len=100)  :: buf
!!    character(len=1)    :: c
!!    integer             :: i, ios
!!    data fonts/ 'astrology', 'cursive',    'futura.l',               &
!!    &      'futura.m',  'gothic.eng', 'gothic.ger',             &
!!    &      'gothic.ita','greek',      'japanese',    'markers', &
!!    &      'math.low',  'math.upp',   'meteorology', 'music',   &
!!    &      'cyrillic',  'script',     'symbolic',    'times.g', &
!!    &      'times.ib',  'times.i',    'times.r',     'times.rb' /
!!
!!    data str1/ 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' /
!!    data str2/ 'abcdefghijklmnopqrstuvwxyz' /
!!    data str3/ '1234567890+-=!@#$%^&*(){}[]' /
!!    data str4/ '<>,./?~`\|_BONK,blark' /
!!    integer :: idum
!!    print*,'Enter output device:'
!!    read(*,'(a)',iostat=ios)buf
!!    if(ios.ne.0)buf=' '
!!    call prefsize(1000,1000)
!!    call vinit(buf)
!!    call vsetflush(.false.)
!!    call linewidth(20)
!!    call clear()
!!    ! define the world space
!!    call ortho2(-14.0, 14.0, -14.0, 14.0)
!!    do i = 1, 22
!!       ! do the title
!!       call textang(0.0)   ! reset text angle so title is straight
!!       call color(D_CYAN)
!!       call font('futura.m')
!!       write(buf, '(''This is Hershey font '',a)') fonts(i)
!!       !call printattribs('before')
!!       call boxtext(-11.0, 12.0, 20.0, 1.0, buf)
!!       !call printattribs('after')
!!       call rect(-11.0, 12.0, 9.0, 13.0) ! draw a box around the title
!!       call font(fonts(i))               ! grab a font from the table
!!       call color(D_BLUE)
!!       ! show the outer ring
!!       call textsize(1.5, 1.5)
!!       call ShowCircularText(11.0, str1)
!!       ! show the second ring
!!       call textsize(1.3, 1.3)
!!       call ShowCircularText(8.5, str2)
!!       ! show the third ring
!!       call textsize(1.1, 1.1)
!!       call ShowCircularText(7.0, str3)
!!       ! show the inside ring
!!       call textsize(0.9, 0.9)
!!       call ShowCircularText(5.0, str4)
!!       call vflush()
!!
!!       idum= getkey()
!!       select case(idum)
!!        case(:-1,ichar('q'),ichar('Q'))
!!          exit
!!       end select
!!
!!       call color(D_BLACK)
!!       call clear()
!!    enddo
!!    call vexit()
!!    contains
!!
!!    subroutine ShowCircularText(r, str)  ! show a ring of text
!!       use M_draw, only : move2, textang, drawchar
!!
!!       real              :: r
!!       character(len=*)  :: str
!!       real              :: i, inc, x, y, a
!!       integer           :: j, i10
!!       character(len=1)  :: c
!!       real,parameter    :: pi = 3.1415926535
!!
!!       j = 1
!!       inc = 360.0 / len_trim(str)
!!
!!       i=0.0
!!       do i10 = 1,len_trim(str)
!!          !
!!          ! calculate the next drawing position
!!          c = str(j:j)
!!          x = r * cos(i * pi / 180.0)
!!          y = r * sin(i * pi / 180.0)
!!          call move2(x, y)
!!          !
!!          ! calculate angle for next character
!!          a = 90.0 + i
!!          !
!!          ! set the orientation of the next character
!!          call textang(a)
!!          !
!!          ! draw the character
!!          call drawchar(c)
!!          j = j + 1
!!          i=i+inc
!!       enddo
!!
!!    end subroutine ShowCircularText
!!
!!    end program demo_drawchar
   subroutine draw_drawchar_integer(c)
      integer, intent(in) :: c
      integer :: p_idx, e_idx, idx_start, move_flag, b, i, x, y, xt, yt, sync
      real(real32) :: xp, yp, tmp, xsave, ysave, tcos, tsin
      type(Token), pointer :: pr(:)

      if (vdevice%inobject /= 0) then
         pr => draw_newtokens(2)
         pr(1)%i = OBJ_DRAWCHAR
         pr(2)%i = c
         return
      end if

      if (vdevice%attr%a%softtext == SOFTHARDWARE) then
         if (vdevice%cpVvalid == 0) call draw_actual_move()
         call draw_drawhardchar(char(c))
         if (vdevice%attr%a%fixedwidth /= 0) then
            call draw_rmove(draw_getfontwidth(), 0.0_real32, 0.0_real32)
         else
            i = max(1, min(128, c + 1))
            call draw_rmove(hardwidth(i) * draw_getfontwidth(), 0.0_real32, 0.0_real32)
         end if
         return
      end if

      if (vdevice%attr%a%softtext /= HERSHEY) then
         if (vdevice%cpVvalid == 0) call draw_actual_move()
         call draw_drawhardchar(char(c))
         call draw_rmove(draw_getfontwidth(), 0.0_real32, 0.0_real32)
         return
      end if

      if (Loaded == 0) call draw_verror("drawchar: no font loaded")

      sync = vdevice%sync
      if (sync /= 0) vdevice%sync = 0

      tcos = vdevice%attr%a%textcos
      tsin = vdevice%attr%a%textsin
      i = max(1, min(nchars, c - 32 + 1))

      xsave = vdevice%cpW(D_X)
      ysave = vdevice%cpW(D_Y)
      idx_start = ftab%ind(i)

      xt = merge(-ftab%mw / 2, xcoord(int(ftab%p(idx_start))), vdevice%attr%a%fixedwidth /= 0)
      yt = ftab%dec

      if (iand(vdevice%attr%a%justify, D_XCENTERED) /= 0) then
         xt = 0
      else if (iand(vdevice%attr%a%justify, D_RIGHT) /= 0) then
         xt = merge(ftab%mw / 2, -xcoord(int(ftab%p(idx_start))), vdevice%attr%a%fixedwidth /= 0)
      end if

      if (iand(vdevice%attr%a%justify, D_YCENTERED) /= 0) then
         yt = 0
      else if (iand(vdevice%attr%a%justify, D_TOP) /= 0) then
         yt = -ftab%dec
      end if

      e_idx = ftab%ind(i + 1)
      b = 1
      do
         move_flag = 1
         p_idx = idx_start + 2
         do while (p_idx < e_idx)
            x = xcoord(int(ftab%p(p_idx)))
            y = ycoord(int(ftab%p(p_idx + 1)))
            p_idx = p_idx + 2
            x = skew_x(x, y)
            if (x /= -50) then
               xp = (btab_x(b) + real(x - xt, real32)) * SCSIZEX
               yp = (btab_y(b) + real(y - yt, real32)) * SCSIZEY
               tmp = xp
               xp = tcos * tmp - tsin * yp + xsave
               yp = tsin * tmp + tcos * yp + ysave

               if (move_flag /= 0) then
                  move_flag = 0
                  call draw_move(xp, yp, vdevice%cpW(D_Z))
               else
                  call draw_draw(xp, yp, vdevice%cpW(D_Z))
               end if
            else
               move_flag = 1
            end if
         end do
         b = b + 1
         if (b > vdevice%attr%a%bold) exit
      end do
      tmp = merge(real(ftab%mw, real32), &
         real(int(ftab%p(idx_start + 1)) - int(ftab%p(idx_start)), real32), &
         vdevice%attr%a%fixedwidth /= 0) * SCSIZEX
      call draw_move(xsave + tcos * tmp, ysave + tsin * tmp, vdevice%cpW(D_Z))
      if (sync /= 0) then
         vdevice%sync = 1
         sync = vdevice%dev%Vsync()
      end if
   end subroutine draw_drawchar_integer

   subroutine draw_drawchar_character(letter)
   character(len=1),intent(in) :: letter
      call draw_drawchar_integer(ichar(letter))
   end subroutine draw_drawchar_character

   subroutine draw_drawhardchar(c)
      character(len=1), intent(in) :: c
      integer :: idum
      if (vdevice%clipoff == 0) then
         if (vdevice%cpVx - int(vdevice%hwidth) > vdevice%maxVx) return
         if (vdevice%cpVx < vdevice%minVx) return
         if (vdevice%cpVy - int(vdevice%hheight) > vdevice%maxVy) return
         if (vdevice%cpVy < vdevice%minVy) return
      end if
      idum = vdevice%dev%Vchar(c)
   end subroutine draw_drawhardchar

!>
!!##NAME
!!    textsize(3f) - [M_draw:TEXT] Set text size of a character in the current SOFTWARE font in world units.
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!         subroutine textsize(width, height)
!!         real,intent(in) :: width
!!         real,intent(in) :: height
!!
!!##DESCRIPTION
!!
!! Set the maximum size of a character in the current font. Width and height
!! are values in world units. This applies to software text, but may not apply
!! to hardware fonts depending upon the output device. This must
!! be done after the font being scaled is loaded. To keep text of different
!! sizes aligned along the same baseline not that you typically need to
!! subtract the descender height from the Y position
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_textsize
!!    use M_draw
!!    implicit none
!!    integer :: i,ii
!!    integer :: ipaws
!!       !! set up long bar as plotting area
!!       call prefsize(900,150)
!!       call vinit(' ') ! start graphics using device $M_DRAW_DEVICE
!!       call ortho2(-30.0, 30.0, -5.0, 5.0)
!!       call font('times.r')
!!
!!       call move2(-23.0,-4.5)
!!       call color(D_WHITE)
!!       call textsize(2.0,2.0)
!!       call move2(-27.5,-3.0)
!!       call draw2( 27.5,-3.0)
!!       call move2(-27.5,-3.0)
!!
!!       do i=1,7
!!          ii=nint((i*20)*0.30)
!!          call linewidth(nint(ii*2.35))
!!          call textsize(real(i),real(i))
!!          call color(D_MAGENTA)
!!          call drawstr('aA')
!!       enddo
!!
!!       ipaws=getkey()
!!
!!       call vexit()
!!
!!    end program demo_textsize
   subroutine draw_textsize(width, height)
      real(real32), intent(in) :: width, height
      real(real32) :: a, p1W(4), p2W(4), r1(4), r2(4)
      type(Token), pointer :: tok(:)
      if (vdevice%initialized == 0) call draw_verror("textsize: draw not initialized")
      if (Loaded == 0) call draw_verror("textsize: no font loaded")
      if (vdevice%inobject /= 0) then
         tok => draw_newtokens(3)
         tok(1)%i = OBJ_TEXTSIZE; tok(2)%f = width; tok(3)%f = height
         return
      end if
      vdevice%attr%a%fontwidth = width
      vdevice%attr%a%fontheight = height
      if (vdevice%attr%a%softtext == SOFTHARDWARE) then
         p1W = [width, height, 0.0_real32, 1.0_real32]
         p2W = [0.0_real32, 0.0_real32, 0.0_real32, 1.0_real32]
         call draw_multvector(r1, p1W, vdevice%transmat%m)
         call draw_multvector(r2, p2W, vdevice%transmat%m)
         vdevice%hwidth = real(abs(draw_WtoVx(r1) - draw_WtoVx(r2)), real32)
         vdevice%hheight = real(abs(draw_WtoVy(r1) - draw_WtoVy(r2)), real32)
         return
      else if (vdevice%attr%a%softtext == REALHARDWARE) then
         return
      end if
      a = real(max(ftab%mw, ftab%as - ftab%dec), real32)
      SCSIZEX = width / a
      SCSIZEY = height / a
   end subroutine draw_textsize

   real(real32) function draw_getfontwidth()
      real(real32) :: a, b, c, d
      real(real32), parameter :: fudge = 0.00123_real32
      if (vdevice%initialized == 0) call draw_verror("getfontwidth: draw not initialized")
      if (vdevice%attr%a%softtext == HERSHEY) then
         if (Loaded == 0) call draw_verror("getfontwidth: No font loaded")
         draw_getfontwidth = SCSIZEX * real(max(ftab%mw, ftab%as - ftab%dec), real32)
      else if (vdevice%attr%a%softtext == SOFTHARDWARE) then
         draw_getfontwidth = vdevice%attr%a%fontwidth
      else
         call draw_VtoWxy(vdevice%hwidth + fudge, vdevice%hheight + fudge, c, d)
         call draw_VtoWxy(fudge, fudge, a, b)
         draw_getfontwidth = c - a
      end if
   end function draw_getfontwidth

   real(real32) function draw_getfontheight()
      real(real32) :: a, b, c, d
      real(real32), parameter :: fudge = 0.0123_real32
      if (vdevice%initialized == 0) call draw_verror("getfontheight: draw not initialized")
      if (vdevice%attr%a%softtext == HERSHEY) then
         if (Loaded == 0) call draw_verror("getfontheight: No font loaded")
         draw_getfontheight = SCSIZEY * real(max(ftab%mw, ftab%as - ftab%dec), real32)
      else if (vdevice%attr%a%softtext == SOFTHARDWARE) then
         draw_getfontheight = vdevice%attr%a%fontheight
      else
         call draw_VtoWxy(vdevice%hwidth + fudge, vdevice%hheight + fudge, c, d)
         call draw_VtoWxy(fudge, fudge, a, b)
         draw_getfontheight = d - b
      end if
   end function draw_getfontheight

!>
!!##NAME
!!    getfontdec(3f) - [M_draw:TEXT] Return size of maximum font descender
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!         real function getfontdec()
!!##DESCRIPTION
!!    Get the descender size of a character in a font.
   real(real32) function draw_getfontdec()
      real(real32) :: a, b, c, d
      real(real32), parameter :: fudge = 0.0123_real32
      if (vdevice%initialized == 0) call draw_verror("getfontdec: draw not initialized")
      if (vdevice%attr%a%softtext == HERSHEY) then
         if (Loaded == 0) call draw_verror("getfontdec: No font loaded")
         draw_getfontdec = SCSIZEY * real(ftab%dec, real32)
      else
         call draw_VtoWxy(vdevice%hwidth + fudge, vdevice%hheight + fudge, c, d)
         call draw_VtoWxy(fudge, fudge, a, b)
         draw_getfontdec = -(d - b) * 0.28_real32
      end if
   end function draw_getfontdec

   real(real32) function draw_getfontasc()
      real(real32) :: a, b, c, d
      real(real32), parameter :: fudge = 0.0123_real32
      if (vdevice%initialized == 0) call draw_verror("getfontasc: draw not initialized")
      if (vdevice%attr%a%softtext == HERSHEY) then
         if (Loaded == 0) call draw_verror("getfontasc: No font loaded")
         draw_getfontasc = SCSIZEY * real(ftab%as, real32)
      else
         call draw_VtoWxy(vdevice%hwidth + fudge, vdevice%hheight + fudge, c, d)
         call draw_VtoWxy(fudge, fudge, a, b)
         draw_getfontasc = -(d - b) * 0.72_real32
      end if
   end function draw_getfontasc

!>
!!##NAME
!!    getfontsize(3f) - [M_draw:TEXT] Get maximum width and height of a character in a font.
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!         subroutine getfontsize(width, height)
!!         real width, height
!!
!!##DESCRIPTION
!!
!!    Get the maximum width and height of a character in a font.
   subroutine draw_getfontsize(cw, ch)
      real(real32), intent(out) :: cw, ch
      cw = draw_getfontwidth()
      ch = draw_getfontheight()
   end subroutine draw_getfontsize

   subroutine draw_drawhstr(string)
      character(len=*), intent(in) :: string
      integer :: i, k, sync, oldClipoff, oldJustify
      logical :: NeedClip
      real(real32) :: p(4), q(4)
      if (vdevice%initialized == 0) call draw_verror("drawhstr: not initialized")
      oldJustify = vdevice%attr%a%justify
      vdevice%attr%a%justify = D_LEFT
      oldClipoff = vdevice%clipoff
      if (oldClipoff == 0) then
         q = [vdevice%cpW(D_X), vdevice%cpW(D_Y), vdevice%cpW(D_Z), 1.0_real32]
         call draw_multvector(p, q, vdevice%transmat%m)
         NeedClip = any(p(4) + p(1:3) < 0.0_real32 .or. p(4) - p(1:3) < 0.0_real32)
         if (.not. NeedClip) then
            q(1) = q(1) + draw_strlength(string)
            q(2) = q(2) + draw_getfontheight()
            call draw_multvector(p, q, vdevice%transmat%m)
            NeedClip = any(p(4) + p(1:3) < 0.0_real32 .or. p(4) - p(1:3) < 0.0_real32)
         end if
         if (.not. NeedClip) vdevice%clipoff = 1
      end if
      sync = vdevice%sync
      if (sync /= 0) vdevice%sync = 0
      do k = 1, len(string)
         call draw_drawchar(ichar(string(k:k)))
      end do
      if (sync /= 0) then
         vdevice%sync = 1
         sync = vdevice%dev%Vsync()
      end if
      vdevice%clipoff = oldClipoff
      vdevice%attr%a%justify = oldJustify
   end subroutine draw_drawhstr
!>
!!##NAME
!!    drawstr(3f) - [M_draw:TEXT] Draw the text in string at the current position.
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!         subroutine drawstr(str)
!!         character(len=*),intent(in) :: str
!!
!!##DESCRIPTION
!!    Draw a text string at the current position. Uses current line color
!!    and thickness and text centering mode.
!!
!!##EXAMPLE
!!
!!  Sample program:
!!
!!       program demo_drawstr
!!       use M_draw
!!       implicit none
!!       integer :: idum
!!       call vinit('')
!!       ! by default the drawing surface is
!!       ! a square ranging from -1 to 1 in both
!!       ! the X and Y axis
!!
!!       call color(D_BLACK)    ! set current color to black
!!       call clear()           ! clear to current color
!!
!!       ! SET COMMON TEXT ATTRIBUTES
!!       call color(D_GREEN)    ! we want to draw in green
!!       call font('futura.m')  ! set font
!!       call textsize(0.1,0.1) ! font size
!!
!!       ! DRAW A STRING
!!       call move2(-1.0, 0.0)
!!       call drawstr('Hello')  ! draw string at current position
!!       ! note that current position is now at end of this string
!!
!!       ! CHANGE SOME TEXT ATTRIBUTES AGAIN
!!       call linewidth(20)     ! set line width
!!       call color(D_RED)      ! change color
!!       call textang(45.0)     ! change text angle
!!
!!       call drawstr(' World!')! draw string at current position
!!       idum=getkey()          ! pause
!!
!!       call vexit()           !  wrap up and exit graphics mode
!!
!!       end program demo_drawstr
   subroutine draw_drawstr(string)
      character(len=*), intent(in) :: string
      real(real32) :: sl, width, height, cx, cy, tcos, tsin, x1, y1, x2, y2
      real(real64) :: tangle, tangle2
      integer :: k, idum
      type(Token), pointer :: tok(:)
      if (vdevice%initialized == 0) call draw_verror("drawstr: draw not initialized")
      if (vdevice%inobject /= 0) then
         tok => draw_newtokens(2)
         tok(1)%i = OBJ_DRAWSTR; tok(2)%s = string
         return
      end if
      sl = real(len(string), real32)
      tcos = vdevice%attr%a%textcos; tsin = vdevice%attr%a%textsin
      height = draw_getfontheight(); width = draw_strlength(string)
      if (iand(vdevice%attr%a%justify, D_XCENTERED) /= 0) then
         width = width / 2.0_real32
      else if (iand(vdevice%attr%a%justify, D_RIGHT) == 0) then
         width = 0.0_real32
      end if
      if (iand(vdevice%attr%a%justify, D_YCENTERED) /= 0) then
         height = height / 2.0_real32
      else if (iand(vdevice%attr%a%justify, D_TOP) == 0) then
         height = 0.0_real32
      end if
      cx = vdevice%cpW(D_X) + height * tsin - width * tcos
      cy = vdevice%cpW(D_Y) - height * tcos - width * tsin
      call draw_move(cx, cy, vdevice%cpW(D_Z))
      if (vdevice%attr%a%softtext == HERSHEY) then
         call draw_drawhstr(string)
      else if (vdevice%attr%a%softtext == SOFTHARDWARE) then
         call draw_move2(cx, cy); call draw_actual_move()
         x1 = real(vdevice%cpVx, real32); y1 = real(vdevice%cpVy, real32)
         call draw_move2(cx + draw_strlength(string), cy); call draw_actual_move()
         x2 = real(vdevice%cpVx, real32); y2 = real(vdevice%cpVy, real32)
         tangle = atan2(real(y2 - y1, real64), real(x2 - x1, real64))
         tangle2 = atan2(real(vdevice%attr%a%textsin, real64), real(vdevice%attr%a%textcos, real64))
         vdevice%attr%a%textcos = real(cos(tangle + tangle2), real32)
         vdevice%attr%a%textsin = real(sin(tangle + tangle2), real32)
         call draw_move2(cx, cy); call draw_actual_move()
         if (vdevice%attr%a%fixedwidth /= 0) then
            do k = 1, len(string)
               call draw_drawhardchar(string(k:k))
               vdevice%cpVx = vdevice%cpVx + int(vdevice%hwidth)
            end do
         else
            idum = vdevice%dev%Vstring(string)
         end if
         if (vdevice%clipoff == 0) then
            call draw_move(cx + draw_strlength(string), cy, vdevice%cpW(D_Z))
            call draw_actual_move()
            vdevice%attr%a%textcos = tcos; vdevice%attr%a%textsin = tsin
         end if
      else
         call draw_actual_move()
         if (vdevice%clipoff /= 0 .or. &
            (vdevice%cpVx > vdevice%minVx .and. &
            vdevice%cpVx + int(sl * (vdevice%hwidth - 1.0_real32)) < vdevice%maxVx .and. &
            vdevice%cpVy - int(vdevice%hheight) < vdevice%maxVy .and. &
            vdevice%cpVy > vdevice%minVy)) then
            idum = vdevice%dev%Vstring(string)
         else
            do k = 1, len(string)
               call draw_drawhardchar(string(k:k))
               vdevice%cpVx = vdevice%cpVx + int(vdevice%hwidth)
            end do
         end if
         call draw_move(cx + draw_getfontwidth() * sl, cy, vdevice%cpW(D_Z))
      end if
   end subroutine draw_drawstr

   integer function draw_istrlength(s)
      character(len=*), intent(in) :: s
      integer :: i, k, len_acc, idx_start
      if (vdevice%attr%a%fixedwidth /= 0) then
         draw_istrlength = len(s) * ftab%mw
      else
         len_acc = 0
         do k = 1, len(s)
            i = ichar(s(k:k)) - 32 + 1
            if (i < 1 .or. i > nchars) i = nchars
            idx_start = ftab%ind(i)
            len_acc = len_acc + (int(ftab%p(idx_start + 1)) - int(ftab%p(idx_start)))
         end do
         draw_istrlength = len_acc + int(0.5_real32 * vdevice%attr%a%skew * real(ftab%as, real32))
      end if
   end function draw_istrlength

!>
!!##NAME
!!    strlength(3f) - [M_draw:TEXT] return length of string
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!
!!       real function strlength(string)
!!       character(len=*),intent(in) :: string
!!
!!##DESCRIPTION
!!    Return the length of the string "STRING" in world units.
!!
!!##RETURNS
!!    STRLENGTH  length of string using current font size
!!
!!##EXAMPLE
!!
!!   Sample Program:
!!
!!    program demo_strlength
!!    use :: M_draw
!!    use M_draw,    only  : D_BLACK,   D_WHITE
!!    use M_draw,    only  : D_RED,     D_GREEN,    D_BLUE
!!    use M_draw,    only  : D_YELLOW,  D_MAGENTA,  D_CYAN
!!    implicit none
!!    real    :: left
!!    real    :: baseline
!!    integer :: ipaws
!!    integer :: icolor=0
!!    real    :: texth=10.0
!!       !! set up drawing surface
!!       call prefsize(800, 400)
!!       call vinit(' ') ! start graphics using device $M_DRAW_DEVICE
!!       call page(-100.0, 300.0, -100.0, 100.0)
!!       call color(D_WHITE)
!!       call clear()
!!       call linewidth(40)
!!       call textsize(texth, texth)
!!       call xcentertext()
!!       call color(D_RED)
!!
!!       baseline=85.0
!!       call move2(0.0,baseline)
!!       call drawstr('If I Can Stop One Heart')
!!       baseline= baseline-texth*1.20
!!       call move2(0.0,baseline)
!!       call drawstr('by Emily Dickinson')
!!       call centertext(.false.)
!!
!!       texth=8.5
!!       baseline=baseline-texth*1.50
!!       call textsize(texth, texth)
!!       left=-90.0
!!
!!       call nextline('If I can stop one heart from breaking,')
!!       call nextline('I shall not live in vain;')
!!       call nextline('If I can ease one life the aching,')
!!       call nextline('Or cool one pain,')
!!       call nextline('Or help one fainting robin')
!!       call nextline('Unto his nest again,')
!!       call nextline('I shall not live in vain.')
!!
!!       ipaws=getkey()
!!       call vexit()
!!    contains
!!    subroutine nextline(string)
!!    character(len=*) :: string
!!    real :: xx
!!    !! reduce some duplicate code; very specific to this EXAMPLE
!!       call color(icolor)
!!       baseline=baseline-texth*1.5    ! move down before drawing line
!!          call polyfill(.true.)
!!             call makepoly()
!!                xx=strlength(string)
!!                call rect(left,baseline-texth*0.3,left+xx,baseline+texth)
!!             call closepoly()
!!          call polyfill(.false.)
!!       call color(D_WHITE)
!!       call move2(left, baseline)
!!       call drawstr(string)    ! draw string
!!       icolor=icolor+1         ! set pen color
!!    end subroutine nextline
!!
!!    end program demo_strlength
   real(real32) function draw_strlength(s)
      character(len=*), intent(in) :: s
      integer :: i, k
      real(real32) :: len_acc
      if (vdevice%initialized == 0) call draw_verror("strlength: draw not initialized")
      if (vdevice%attr%a%softtext == HERSHEY) then
         draw_strlength = real(draw_istrlength(s), real32) * SCSIZEX
      else if (vdevice%attr%a%softtext == SOFTHARDWARE) then
         if (vdevice%attr%a%fixedwidth /= 0) then
            draw_strlength = real(len(s), real32) * draw_getfontwidth()
         else
            len_acc = 0.0_real32
            do k = 1, len(s)
               i = ichar(s(k:k)) + 1
               if (i >= 1 .and. i <= 128) len_acc = len_acc + hardwidth(i)
            end do
            draw_strlength = len_acc * draw_getfontwidth()
         end if
      else
         draw_strlength = real(len(s), real32) * draw_getfontwidth()
      end if
   end function draw_strlength

!>
!!##NAME
!!    boxtext(3f) - [M_draw:TEXT] stretch and draw the SOFTWARE string s so that it fits in the imaginary box
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!         subroutine boxtext(x, y, l, h, s)
!!         real x, y, l, h, s
!!
!!##DESCRIPTION
!!    Draw the string S so that it fits in the imaginary box defined with
!!    bottom left hand corner at (x, y), length l, and height h. This only
!!    applies to software text. The textsize is temporarily resized so that
!!    the string fills the specified box.
!!##OPTIONS
!!    X,Y   position of lower left corner of box in world coordinates
!!    L     length of box in world coordinates
!!    H     height of box in world coordinates
!!    S     string to stretch into box and draw
!!
!!##EXAMPLE
!!
!!   Simple program:
!!
!!     program demo_boxtext
!!     use M_draw,     only : vinit,vexit,prefsize,vgetdev,clear,page
!!     use M_draw,     only : centertext,polyfill,font,linewidth,color
!!     use M_draw,     only : getkey
!!     use M_draw,     only : color,rect,boxtext
!!     use M_draw,     only : D_BLACK,   D_WHITE
!!     use M_draw,     only : D_RED,     D_GREEN,    D_BLUE
!!     use M_draw,     only : D_YELLOW,  D_MAGENTA,  D_CYAN
!!     implicit none
!!     real              :: x1=0.0,    x2=40.0,    y1=0.0,    y2=7.0
!!     real              :: xmin=1.0,  xmax=39.0,  ymin=1.0,  ymax=6.0
!!     integer           :: idum
!!        call prefsize(int(x2-x1)*25,int(y2-y1)*25)
!!        call vinit(' ')
!!        call page(x1,x2,y1,y2)
!!        call centertext(.true.)
!!        call font("times.rb")
!!        call color(D_GREEN)
!!        call clear()
!!        call linewidth(200)
!!        call color(D_CYAN)
!!        call polyfill(.false.)
!!        call rect(xmin,ymin,xmax,ymax)
!!        call color(D_WHITE)
!!        call polyfill(.true.)
!!        call rect(xmin,ymin,xmax,ymax)
!!        call color(D_BLACK)
!!        call boxtext(xmin,ymin,xmax-xmin,ymax-ymin,"This text is in the box")
!!        idum=getkey()
!!        call vexit()
!!     end program demo_boxtext
   subroutine draw_boxtext(x, y, l, h, s)
      real(real32), intent(in) :: x, y, l, h
      character(len=*), intent(in) :: s
      real(real32) :: oscsizex, oscsizey
      type(Token), pointer :: tok(:)
      if (vdevice%initialized == 0) call draw_verror("boxtext: draw not initialized")
      if (vdevice%attr%a%softtext /= HERSHEY) call draw_verror("boxtext: need a hershey vector font loaded")
      if (vdevice%inobject /= 0) then
         tok => draw_newtokens(6)
         tok(1)%i = OBJ_BOXTEXT; tok(2)%f = x; tok(3)%f = y; tok(4)%f = l; tok(5)%f = h; tok(6)%s = s
         return
      end if
      oscsizex = SCSIZEX; oscsizey = SCSIZEY
      if (len(s) /= 0) then
         SCSIZEX = l / real(draw_istrlength(s), real32)
         SCSIZEY = h / real(ftab%as - ftab%dec, real32)
         call draw_move(x, y, vdevice%cpW(D_Z))
         call draw_pushattributes()
         call draw_centertext(0)
         call draw_drawstr(s)
         call draw_popattributes()
      end if
      SCSIZEX = oscsizex; SCSIZEY = oscsizey
   end subroutine draw_boxtext

!>
!!##NAME
!!    boxfit(3f) - [M_draw:TEXT] resize the SOFTWARE text size so it fits in a box
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!         subroutine boxfit(l, h, nchars)
!!         real l, h
!!         integer nchars
!!
!!##DESCRIPTION
!!    Set scale for text so that a string of the biggest characters in
!!    the font will fit in a box l by h. l and h are real values in world
!!    dimensions. This only applies to software text.
      subroutine draw_boxfit(l, h, num_chars)
         real(real32), intent(in) :: l, h
         integer, intent(in) :: num_chars
         if (vdevice%initialized == 0) call draw_verror("boxfit: draw not initialized")
         if (vdevice%attr%a%softtext /= HERSHEY) call draw_verror("boxfit: cannot rescale hardware font")
         SCSIZEX = l / real(num_chars * ftab%mw, real32)
         SCSIZEY = h / real(ftab%as - ftab%dec, real32)
      end subroutine draw_boxfit

!>
!!##NAME
!!    centertext(3f) - [M_draw:TEXT] Turns centertext mode on or off for SOFTWARE fonts.
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!         subroutine centertext(onoff)
!!         logical,intent(in) :: onoff
!!
!!##DESCRIPTION
!!
!!    Turns centertext text on or off. .TRUE. is on. This centers
!!    strings and chars. This routine only affects software text.
!!
!!##OPTIONS
!!    ONOFF  set centering mode on or off
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_centertext
!!    use :: M_draw
!!    implicit none
!!    real :: x1, y1, r, ang, xx, yy
!!    integer :: i, j, ipaws
!!    !! set up drawing environment
!!    call prefsize(600,600)
!!    call vinit(' ') ! start graphics using device $M_DRAW_DEVICE
!!    call ortho2(-300.0,300.0,-300.0,300.0)
!!    call textsize(8.0,8.0)
!!    call linewidth(30)
!!    call color(D_BLACK)
!!    call clear()
!!
!!    x1=-150
!!    y1=-150
!!    do j=1,4
!!       select case(j)
!!       case(1);  call  xcentertext();        x1=-150;  y1=-150;  r=100
!!       case(2);  call  ycentertext();        x1=+150;  y1=-150;  r= 30
!!       case(3);  call  centertext(.true.);   x1=-150;  y1=+150;  r=100
!!       case(4);  call  centertext(.false.);  x1=+150;  y1=+150;  r= 30
!!       end select
!!       !! draw radial lines
!!       call color(D_RED)
!!       do i=1,80
!!          call move2(x1,y1)
!!          call draw2(x1+150.0*cosd(i*12.0), y1+150.0*sind(i*12.0))
!!       enddo
!!
!!       !! draw rotated text
!!       call color(D_GREEN)
!!       do i=1,30
!!          ang=i*12.0
!!          xx=x1+r*cosd(ang)
!!          yy=y1+r*sind(ang)
!!          call move2(xx,yy)
!!          call textang(ang)
!!          call color(D_WHITE)
!!          call drawstr('This is angled text')
!!          call color(D_RED)
!!       enddo
!!    enddo
!!
!!    ipaws=getkey()
!!
!!    call vexit()
!!
!!    end program demo_centertext
      subroutine draw_centertext_integer(onoff)
         integer, intent(in) :: onoff
         type(Token), pointer :: tok(:)
         if (vdevice%inobject /= 0) then
            tok => draw_newtokens(2); tok(1)%i = OBJ_CENTERTEXT; tok(2)%i = onoff; return
         end if
         vdevice%attr%a%justify = merge(ior(D_XCENTERED, D_YCENTERED), ior(D_LEFT, D_BOTTOM), onoff /= 0)
      end subroutine draw_centertext_integer

   subroutine draw_centertext_logical(onoff)
      logical, intent(in) :: onoff
      call draw_centertext_integer(merge(1,0,onoff))
   end subroutine draw_centertext_logical

!>
!!##NAME
!!    textjustify(3f) - [M_draw:TEXT] general text justification (C only)
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!        subroutine textjustify(val)
!!        character(kind=c_short) :: ival
!!        character(kind=c_char) :: val
!!
!!##DESCRIPTION
!!    General (direct) control of text justification. The value of val is made
!!    up of the logical OR of the following predefined constants in draw.h
!!    (FOR C and Fortran only). D_LEFT, D_RIGHT, D_XCENTERED, D_TOP, D_BOTTOM,
!!    D_YCENTERED. Centering takes priority, as does RIGHT and TOP justification
!!    (if you were silly enough to set it to D_LEFT|D_RIGHT for EXAMPLE that
!!    is). A value of 0 (zero) (in all languages) resets the textjustification
!!    to the default.
!!
!!        ! from Fortran, use IANY() to OR the array of options, and CHAR()
!!        ! to convert the integer result to a C_CHAR type. KIND C_CHAR is
!!        ! defined by loading the intrinsic module for C bindings
!!        ! (ie. "USE ISO_C_BINDING").
!!        ival=iany([D_XCENTERED,D_YCENTERED])
!!        val=char(ival)
!!        call textjustify(val)
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_textjustify
!!    use M_draw
!!    use M_draw,    only  : D_BLACK,   D_WHITE
!!    use M_draw,    only  : D_RED,     D_GREEN,    D_BLUE
!!    use M_draw,    only  : D_YELLOW,  D_MAGENTA,  D_CYAN
!!    use iso_c_binding
!!    implicit none
!!    real :: x1=-20.0, x2=20.0, y1=-20.0, y2=20.0
!!       call prefsize(int(x2-x1)*30,int(y2-y1)*30)
!!       !!call voutput('|ppmtogif >textjustify.3M_draw.gif')
!!       !!call vinit('p6')
!!       call vinit(' ')
!!       call page(x1,x2,y1,y2)
!!       call clear()
!!       call textsize(0.9, 1.4)
!!       call font("times.rb")
!!       call linewidth(20)
!!       call seejustify( "right|top",           &
!!               iany([d_right,d_top]),           -10.0, -10.0 )
!!       call seejustify( "right|ycentered",     &
!!               iany([d_right,d_ycentered]),     -10.0,   0.0 )
!!       call seejustify( "right|bottom",        &
!!               iany([d_right,d_bottom]),        -10.0, +10.0 )
!!       call seejustify( "xcentered|top",       &
!!               iany([d_xcentered,d_top]),         0.0, -10.0 )
!!       call seejustify( "xcentered|ycentered", &
!!               iany([d_xcentered,d_ycentered]),   0.0,   0.0 )
!!       call seejustify( "xcentered|bottom",    &
!!               iany([d_xcentered,d_bottom]),      0.0, +10.0 )
!!       call seejustify( "left|top",            &
!!               iany([d_left,d_top]),            +10.0, -10.0 )
!!       call seejustify( "left|ycentered",      &
!!               iany([d_left,d_ycentered]),      +10.0,   0.0 )
!!       call seejustify( "left|bottom",         &
!!               iany([d_left,d_bottom]),         +10.0, +10.0 )
!!       call vexit()
!!    contains
!!       subroutine seejustify(string,justify,x,y)
!!          implicit none
!!          real                    :: x, y
!!          real                    :: height, width
!!          integer(kind=c_short)   :: justify
!!          character(len=*)        :: string
!!          character(kind=c_char)  :: byte
!!          call color(D_RED)
!!          call move2(x-1.0,y); call draw2(x+1.0,y)
!!          call move2(x,y-1.0); call draw2(x,y+1.0)
!!          call circle(x,y,5.0)
!!          call color(D_BLUE)
!!          call move2(x,y)
!!          byte=char(justify)
!!          call textjustify(byte)
!!          call drawstr(string)
!!          call color(D_WHITE)
!!          call rmove2(-strlength(string),0.0)
!!          call rdraw2(+strlength(string),0.0)
!!          call getfontsize(width, height)
!!          call rmove2(0.0,height)
!!          call rmove2(-strlength(string),0.0)
!!          call rdraw2(+strlength(string),0.0)
!!       end subroutine seejustify
!!    end program demo_textjustify

      subroutine draw_textjustify(val)
         integer, intent(in) :: val
         type(Token), pointer :: tok(:)
         if (vdevice%inobject /= 0) then
            tok => draw_newtokens(2); tok(1)%i = OBJ_TEXTJUSTIFY; tok(2)%i = val; return
         end if
         vdevice%attr%a%justify = val
      end subroutine draw_textjustify
!>
!!##NAME
!!    xcentertext(3f) - [M_draw:TEXT] set text centering mode on in X direction
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!        subroutine xcentertext()
!!
!!##DESCRIPTION
!!    Set text centering mode on in X direction. Y justification is
!!    turned off.
!!
!!    Centers text in the X direction. The text string will begin at a
!!    point to the notional left of the current position and finish at a
!!    point to the right of the current position. Left justification and
!!    Right justification are turned off.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_xcentertext
!!    use M_draw
!!    implicit none
!!    real    :: x1, x2, y1, y2
!!    real    :: scl, ax, bx
!!    integer :: key
!!       call prefsize(1200,120)
!!       call vinit(' ')
!!       x1=0; x2=40; y1=0; y2=4; scl=1.9
!!       call page(x1,x2,y1,y2)
!!       call textsize(0.9*scl,1.4*scl)
!!       call font("times.rb")
!!       call linewidth(200)
!!       AX=(x1+x2)/2+4; BX=y1+1.3
!!       call move2(AX,BX)
!!       call xcentertext()
!!       call color(D_GREEN)
!!       call drawstr("xcentertext()")
!!       call color(D_RED)
!!       call move2(AX-1.0,BX)
!!       call draw2(AX+1.0,BX)
!!       call move2(AX,BX-1.0)
!!       call draw2(AX,BX+1.0)
!!       call vflush()
!!       key=getkey()
!!       call vexit()
!!    end program demo_xcentertext
      subroutine draw_xcentertext()
         type(Token), pointer :: tok(:)
         if (vdevice%inobject /= 0) then
            tok => draw_newtokens(1); tok(1)%i = OBJ_XCENTERTEXT; return
         end if
         vdevice%attr%a%justify = iand(ior(vdevice%attr%a%justify, D_XCENTERED), not(ior(D_LEFT, D_RIGHT)))
      end subroutine draw_xcentertext
!>
!!##NAME
!!    ycentertext(3f) - [M_draw:TEXT] center text in the Y direction
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!        subroutine ycentertext()
!!
!!##DESCRIPTION
!!    Centers text in the Y direction. The text string will so that its
!!    center line is aligned with the current y position. Top justification
!!    and Bottom justification are turned off.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_ycentertext
!!    use M_draw
!!    implicit none
!!    real    :: x1, x2, y1, y2
!!    real    :: scl, ax, bx
!!    integer :: key
!!       call prefsize(1200,120)
!!       call vinit(' ')
!!       call color(D_BLACK)
!!       call clear()
!!       x1=0; x2=40; y1=0; y2=4; scl=1.9
!!       call page(x1,x2,y1,y2)
!!       call textsize(0.9*scl,1.4*scl)
!!       call font("times.rb")
!!       call linewidth(200)
!!       AX=(x1+x2)/2.0; BX=(y1+y2)/2.0
!!       call move2(AX,BX)
!!       call ycentertext()
!!       call color(D_MAGENTA)
!!       call drawstr("ycentertext()")
!!       call color(D_CYAN)
!!       call move2(AX-1.0,BX)
!!       call draw2(AX+1.0,BX)
!!       call move2(AX,BX-1.0)
!!       call draw2(AX,BX+1.0)
!!       call vflush()
!!       key=getkey()
!!       call vexit()
!!    end program demo_ycentertext
!!
      subroutine draw_ycentertext()
         type(Token), pointer :: tok(:)
         if (vdevice%inobject /= 0) then
            tok => draw_newtokens(1); tok(1)%i = OBJ_YCENTERTEXT; return
         end if
         vdevice%attr%a%justify = iand(ior(vdevice%attr%a%justify, D_YCENTERED), not(ior(D_TOP, D_BOTTOM)))
      end subroutine draw_ycentertext
!>
!!##NAME
!!    leftjustify(3f) - [M_draw:TEXT] left justify text
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!         subroutine leftjustify()
!!
!!##DESCRIPTION
!!    Left justifies text. The text string will begin at the current position
!!    and extend to the notional right. Right justification and X centering
!!    are turned off.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_leftjustify
!!    use M_draw
!!    use M_draw,    only  : D_BLACK,   D_WHITE
!!    use M_draw,    only  : D_RED,     D_GREEN,    D_BLUE
!!    use M_draw,    only  : D_YELLOW,  D_MAGENTA,  D_CYAN
!!    implicit none
!!    real    :: x1, x2, y1, y2
!!    real    :: scl, ax, bx
!!    integer :: key
!!       call prefsize(1200,120)
!!       call vinit(' ')
!!       x1=0; x2=40; y1=0; y2=4; scl=1.9
!!       call page(x1,x2,y1,y2)
!!       call textsize(0.9*scl,1.4*scl)
!!       call font("times.rb")
!!       call linewidth(200)
!!       AX=(x1+x2)/2+1; BX=y1+1.3
!!       call move2(AX,BX)
!!       call leftjustify()
!!       call color(D_GREEN)
!!       call drawstr("leftjustify()")
!!       call color(D_RED)
!!       call move2(AX-1.0,BX)
!!       call draw2(AX+1.0,BX)
!!       call move2(AX,BX-1.0)
!!       call draw2(AX,BX+1.0)
!!       call vflush()
!!       key=getkey()
!!       call vexit()
!!    end program demo_leftjustify
      subroutine draw_leftjustify()
         type(Token), pointer :: tok(:)
         if (vdevice%inobject /= 0) then
            tok => draw_newtokens(1); tok(1)%i = OBJ_LEFTJUSTIFY; return
         end if
         vdevice%attr%a%justify = iand(ior(vdevice%attr%a%justify, D_LEFT), not(ior(D_RIGHT, D_XCENTERED)))
      end subroutine draw_leftjustify

!>
!!##NAME
!!    rightjustify(3f) - [M_draw:TEXT] right justify text
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!        subroutine rightjustify()
!!
!!##DESCRIPTION
!!
!!    Right justifies text. The text string will begin at a point to the
!!    notional left of the current position and finish at the current
!!    position. Left justification and X centering are turned off.
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_rightjustify
!!    use M_draw
!!    use M_draw,    only  : D_BLACK,   D_WHITE
!!    use M_draw,    only  : D_RED,     D_GREEN,    D_BLUE
!!    use M_draw,    only  : D_YELLOW,  D_MAGENTA,  D_CYAN
!!    implicit none
!!    real    :: x1, x2, y1, y2
!!    real    :: scl, ax, bx
!!    integer :: key
!!       call prefsize(1200,120)
!!       call vinit(' ')
!!       x1=0; x2=40; y1=0; y2=4; scl=1.9
!!       call page(x1,x2,y1,y2)
!!       call textsize(0.9*scl,1.4*scl)
!!       call font("times.rb")
!!       call linewidth(200)
!!       AX=(x1+x2)/2+6; BX=y1+1.3
!!       call move2(AX,BX)
!!       call rightjustify()
!!       call color(D_GREEN)
!!       call drawstr("rightjustify()")
!!       call color(D_RED)
!!       call move2(AX-1.0,BX)
!!       call draw2(AX+1.0,BX)
!!       call move2(AX,BX-1.0)
!!       call draw2(AX,BX+1.0)
!!       call vflush()
!!       key=getkey()
!!       call vexit()
!!    end program demo_rightjustify
      subroutine draw_rightjustify()
         type(Token), pointer :: tok(:)
         if (vdevice%inobject /= 0) then
            tok => draw_newtokens(1); tok(1)%i = OBJ_RIGHTJUSTIFY; return
         end if
         vdevice%attr%a%justify = iand(ior(vdevice%attr%a%justify, D_RIGHT), not(ior(D_LEFT, D_XCENTERED)))
      end subroutine draw_rightjustify

!>
!!##NAME
!!    topjustify(3f) - [M_draw:TEXT] top justify text
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!         subroutine topjustify()
!!
!!##DESCRIPTION
!!    Top justifies text. The text string will be drawn with its upper edge
!!    aligned with the current Y position. Bottom justification and Y centering
!!    are turned off.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_topjustify
!!    use M_draw
!!    implicit none
!!    real    :: x1, x2, y1, y2
!!    real    :: scl, ax, bx
!!    integer :: key
!!       call prefsize(1200,120)
!!       call vinit(' ')
!!       x1=0; x2=40; y1=0; y2=4; scl=1.9
!!       call page(x1,x2,y1,y2)
!!       call textsize(0.9*scl,1.4*scl)
!!       call font("times.rb")
!!       call linewidth(200)
!!       AX=(x1+x2)/2+0.3; BX=y1+3.3
!!       call move2(AX,BX)
!!       call topjustify()
!!       call color(D_BLUE)
!!       call drawstr("topjustify()")
!!       call color(D_RED)
!!       call move2(AX-1.0,BX)
!!       call draw2(AX+1.0,BX)
!!       call move2(AX,BX-1.0)
!!       call draw2(AX,BX+1.0)
!!       call vflush()
!!       key=getkey()
!!       call vexit()
!!    end program demo_topjustify
      subroutine draw_topjustify()
         type(Token), pointer :: tok(:)
         if (vdevice%inobject /= 0) then
            tok => draw_newtokens(1); tok(1)%i = OBJ_TOPJUSTIFY; return
         end if
         vdevice%attr%a%justify = iand(ior(vdevice%attr%a%justify, D_TOP), not(ior(D_BOTTOM, D_YCENTERED)))
      end subroutine draw_topjustify

!>
!!##NAME
!!    bottomjustify(3f) - [M_draw:TEXT] bottom justify text
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!        subroutine bottomjustify()
!!
!!##DESCRIPTION
!!
!!    Bottom justifies text. The text string will be drawn with the lower
!!    edge aligned with the current Y position. Top justification and Y
!!    centering are turned off.
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_bottomjustify
!!    use M_draw
!!    implicit none
!!    real    :: x1, x2, y1, y2
!!    real    :: scl, ax, bx
!!    integer :: key
!!       call prefsize(1200,120)
!!       call vinit(' ')
!!       x1=0; x2=40; y1=0; y2=4; scl=1.9
!!       call page(x1,x2,y1,y2)
!!       call textsize(0.9*scl,1.4*scl)
!!       call font("times.rb")
!!       call linewidth(200)
!!       AX=(x1+x2)/2+1; BX=y1+1.3
!!       call move2(AX,BX)
!!       call bottomjustify()
!!       call color(D_BLUE)
!!       call drawstr("bottomjustify()")
!!       call color(D_RED)
!!       call move2(AX-1.0,BX)
!!       call draw2(AX+1.0,BX)
!!       call move2(AX,BX-1.0)
!!       call draw2(AX,BX+1.0)
!!       call vflush()
!!       key=getkey()
!!       call vexit()
!!    end program demo_bottomjustify
      subroutine draw_bottomjustify()
         type(Token), pointer :: tok(:)
         if (vdevice%inobject /= 0) then
            tok => draw_newtokens(1); tok(1)%i = OBJ_BOTTOMJUSTIFY; return
         end if
         vdevice%attr%a%justify = iand(ior(vdevice%attr%a%justify, D_BOTTOM), not(ior(D_TOP, D_YCENTERED)))
      end subroutine draw_bottomjustify

!>
!!##NAME
!!    fixedwidth(3f) - [M_draw:TEXT] Turns fixedwidth mode on or off for SOFTWARE fonts.
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!         subroutine fixedwidth(onoff)
!!         logical onoff
!!##DESCRIPTION
!!
!!    Turns fixedwidth text on or off. .TRUE. causes all text to
!!    be printed with a fixed width for each character. Otherwise, the text
!!    is spaced proportionally, where each character has a unique width less
!!    than or equal to the current fixed font width. This routine only affects
!!    software text.
!!
!!    The default at program initialization is fixedwidth(.false.)
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_fixedwidth
!!       use M_draw
!!       implicit none
!!       real,parameter :: x1=0.0,  x2=40.0,  y1=0.0,  y2=4.0
!!       real,parameter :: scl=3*0.7
!!       integer :: idum
!!    ! set up display
!!       call prefsize(1000,100)
!!       call vinit(' ')
!!       call page(x1,x2,y1,y2)
!!    ! set font appearance
!!       call linewidth(200)
!!       call font("times.rb")
!!    ! draw a string using proportional and fixed spacing
!!       call move2(x1+0.3,y1+0.4)
!!       call textsize(0.8*scl,1.2*scl)
!!       call color(1)
!!       call fixedwidth(.false.)
!!       call drawstr("fixedwidth(.false.)")
!!       call textsize(0.6*scl,1.2*scl)
!!       call color(2)
!!       call fixedwidth(.true.)
!!       call drawstr(" fixedwidth(.true.)")
!!    ! wrap up
!!       idum=getkey()
!!       call vexit()
!!    end program demo_fixedwidth
      subroutine draw_fixedwidth_integer(onoff)
         integer, intent(in) :: onoff
         type(Token), pointer :: tok(:)
         if (vdevice%inobject /= 0) then
            tok => draw_newtokens(2); tok(1)%i = OBJ_FIXEDWIDTH; tok(2)%i = onoff; return
         end if
         vdevice%attr%a%fixedwidth = onoff
      end subroutine draw_fixedwidth_integer

   subroutine draw_fixedwidth_logical(onoff)
      logical, intent(in) :: onoff
      call draw_fixedwidth_integer(merge(1,0,onoff))
   end subroutine draw_fixedwidth_logical

!>
!!##NAME
!!    textang(3f) - [M_draw:TEXT] Set the SOFTWARE text angle.
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!         subroutine textang(ang)
!!         real,intent(in) :: ang
!!
!!##DESCRIPTION
!!    Set the text angle. This angles strings and chars. This routine only
!!    affects software text. Angle is in degrees
!!
!!##OPTIONS
!!    ANG   The angle in degrees to draw text with when using drawstr(3f).
!!          Angles are measured counterclockwise with zero degrees at the horizontal
!!          line to the right of the original.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_textang
!!    use :: M_draw
!!    implicit none
!!    integer :: i, ipaws
!!
!!    !! set up drawing environment
!!    call prefsize(600,600)
!!    call vinit(' ') ! start graphics using device $M_DRAW_DEVICE
!!    call ortho2(-100.0,100.0,-100.0,100.0)
!!    call textsize(7.0,7.0)
!!    call linewidth(20)
!!    call color(D_BLACK)
!!    call clear()
!!
!!    do i=1,30
!!       !! draw radial lines
!!       call color(D_RED)
!!       call move2(0.0,0.0)
!!       call draw2(100.0*cosd(i*12.0),100.0*sind(i*12.0))
!!       !! draw rotated text
!!       call color(D_WHITE)
!!       call move2(30.0*cosd(i*12.0),30.0*sind(i*12.0))
!!       call textang(i*12.0)
!!       call drawstr('angled text')
!!    enddo
!!
!!    ipaws=getkey()
!!
!!    call vexit()
!!
!!    end program demo_textang
      subroutine draw_textang(ang)
         real(real32), intent(in) :: ang
         type(Token), pointer :: tok(:)
         if (vdevice%initialized == 0) call draw_verror("textang: draw not initialized")
         if (vdevice%inobject /= 0) then
            tok => draw_newtokens(3); tok(1)%i = OBJ_TEXTANG; tok(2)%f = cos(ang * D2R); tok(3)%f = sin(ang * D2R); return
         end if
         vdevice%attr%a%textcos = cos(ang * D2R)
         vdevice%attr%a%textsin = sin(ang * D2R)
      end subroutine draw_textang

!>
!!##NAME
!!    textslant(3f) - [M_draw:TEXT] Defines the obliqueness of the fonts.
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!        subroutine textslant(var)
!!        real var
!!
!!##DESCRIPTION
!!
!!    Defines the obliqueness of the fonts. This is a simplistic method that
!!    allows you to generate italicized versions of the software fonts. The x-
!!    values of the software font coordinates after the current textsize()
!!    values are applied are multiplied by (1+val).
!!
!!    Note that this means the same value tilts the characters less the taller
!!    the characters are relative to their width.
!!
!!    Generally, practical values are generally between -1 and 1 times the
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_textslant
!!    use M_draw
!!    implicit none
!!    real    :: x1, x2, y1, y2
!!    real    :: scl
!!    integer :: key
!!       call prefsize(1200,300)
!!       call vinit(' ')
!!       call color(D_BLACK)
!!       call clear()
!!       x1=0.0; x2=40.0; y1=0.0; y2=10.0; scl=3*0.7
!!       call page(x1,x2,y1,y2)
!!       call font("times.rb")
!!       call linewidth(180)
!!       call textsize(0.8*scl,1.2*scl)
!!       call move2( x1+.3,y1+.4)
!!       call color(D_RED)
!!       call textslant(0.0);  call drawstr("textslant(0.0); ")
!!       !
!!       call color(D_GREEN)
!!       call textslant(-1.0); call drawstr(" textslant(-1.0);")
!!       !
!!       call color(D_BLUE)
!!       call textslant(1.0);  call drawstr(" textslant(1.0);")
!!       !
!!       call textsize(0.8*scl,1.2*3*scl)
!!       call move2(x1+.3,y1+3+.4)
!!       call color(D_MAGENTA)
!!       call textslant(1.0); call drawstr(" textslant(1.0);")
!!       !
!!       call textsize(0.8*scl,1.2*scl)
!!       call color(D_CYAN)
!!       call textslant(0.3); call drawstr(" textslant(0.3);")
!!       !
!!       call color(D_WHITE)
!!       call textslant(0.5); call drawstr(" textslant(0.5);")
!!       !
!!       call vflush()
!!       key=getkey()
!!       call vexit()
!!    end program demo_textslant
      subroutine draw_textslant(val)
         real(real32), intent(in) :: val
         type(Token), pointer :: tok(:)
         if (vdevice%inobject /= 0) then
            tok => draw_newtokens(2); tok(1)%i = OBJ_TEXTSLANT; tok(2)%f = val; return
         end if
         vdevice%attr%a%skew = val
      end subroutine draw_textslant

!>
!!##NAME
!!    textweight(3f) - [M_draw:TEXT] Defines the weight of the fonts.
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!        subroutine textweight(ival)
!!        integer ival
!!##DESCRIPTION
!!
!!     Defines the weight of the fonts. Currently, the predefined constants
!!     in C and Fortran are D_NORMAL and D_BOLD; which correspond to 0
!!     and 1. This is not the same as using linethickess to change the
!!     appearance of a software font. The font is redrawn multiple times
!!     with a slight offset to create the bold appearance.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_textweight
!!    use M_draw
!!    implicit none
!!    real,parameter :: w=40.0
!!    integer        :: key
!!       call prefsize(600,600)
!!       call vinit(' ')
!!       call color(D_BLACK)
!!       call clear()
!!       call color(D_YELLOW)
!!       call page(-w,w,-w,w)
!!       call font("times.rb")
!!       call linewidth(180)
!!       call textsize(15.0,15.0)
!!       call centertext(.true.)
!!       call linewidth(0);call color(D_BLUE)
!!       call move2(0.0, W/2.0)
!!
!!       call textweight(0)
!!       call drawstr('NORMAL')
!!
!!       call linewidth(0);call color(D_MAGENTA)
!!       call move2(0.0, 0.0-W/2.0)
!!
!!       call textweight(1)
!!       call drawstr('BOLD')
!!
!!       call vflush()
!!       key=getkey()
!!       call vexit()
!!    end program demo_textweight
      subroutine draw_textweight(val)
         integer, intent(in) :: val
         vdevice%attr%a%bold = merge(5, 0, val /= 0)
      end subroutine draw_textweight

      subroutine draw_actual_move()
         real(real32) :: v2(4)
         call draw_multvector(v2, vdevice%cpW, vdevice%transmat%m)
         vdevice%cpVvalid = 0
         vdevice%cpVx = draw_WtoVx(v2)
         vdevice%cpVy = draw_WtoVy(v2)
         call draw_copyvector(vdevice%cpWtrans, v2)
      end subroutine draw_actual_move
   end module m_draw_text
