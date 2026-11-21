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

      subroutine draw_boxfit(l, h, num_chars)
         real(real32), intent(in) :: l, h
         integer, intent(in) :: num_chars
         if (vdevice%initialized == 0) call draw_verror("boxfit: draw not initialized")
         if (vdevice%attr%a%softtext /= HERSHEY) call draw_verror("boxfit: cannot rescale hardware font")
         SCSIZEX = l / real(num_chars * ftab%mw, real32)
         SCSIZEY = h / real(ftab%as - ftab%dec, real32)
      end subroutine draw_boxfit

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

      subroutine draw_textjustify(val)
         integer, intent(in) :: val
         type(Token), pointer :: tok(:)
         if (vdevice%inobject /= 0) then
            tok => draw_newtokens(2); tok(1)%i = OBJ_TEXTJUSTIFY; tok(2)%i = val; return
         end if
         vdevice%attr%a%justify = val
      end subroutine draw_textjustify
      subroutine draw_xcentertext()
         type(Token), pointer :: tok(:)
         if (vdevice%inobject /= 0) then
            tok => draw_newtokens(1); tok(1)%i = OBJ_XCENTERTEXT; return
         end if
         vdevice%attr%a%justify = iand(ior(vdevice%attr%a%justify, D_XCENTERED), not(ior(D_LEFT, D_RIGHT)))
      end subroutine draw_xcentertext
      subroutine draw_ycentertext()
         type(Token), pointer :: tok(:)
         if (vdevice%inobject /= 0) then
            tok => draw_newtokens(1); tok(1)%i = OBJ_YCENTERTEXT; return
         end if
         vdevice%attr%a%justify = iand(ior(vdevice%attr%a%justify, D_YCENTERED), not(ior(D_TOP, D_BOTTOM)))
      end subroutine draw_ycentertext
      subroutine draw_leftjustify()
         type(Token), pointer :: tok(:)
         if (vdevice%inobject /= 0) then
            tok => draw_newtokens(1); tok(1)%i = OBJ_LEFTJUSTIFY; return
         end if
         vdevice%attr%a%justify = iand(ior(vdevice%attr%a%justify, D_LEFT), not(ior(D_RIGHT, D_XCENTERED)))
      end subroutine draw_leftjustify
      subroutine draw_rightjustify()
         type(Token), pointer :: tok(:)
         if (vdevice%inobject /= 0) then
            tok => draw_newtokens(1); tok(1)%i = OBJ_RIGHTJUSTIFY; return
         end if
         vdevice%attr%a%justify = iand(ior(vdevice%attr%a%justify, D_RIGHT), not(ior(D_LEFT, D_XCENTERED)))
      end subroutine draw_rightjustify
      subroutine draw_topjustify()
         type(Token), pointer :: tok(:)
         if (vdevice%inobject /= 0) then
            tok => draw_newtokens(1); tok(1)%i = OBJ_TOPJUSTIFY; return
         end if
         vdevice%attr%a%justify = iand(ior(vdevice%attr%a%justify, D_TOP), not(ior(D_BOTTOM, D_YCENTERED)))
      end subroutine draw_topjustify
      subroutine draw_bottomjustify()
         type(Token), pointer :: tok(:)
         if (vdevice%inobject /= 0) then
            tok => draw_newtokens(1); tok(1)%i = OBJ_BOTTOMJUSTIFY; return
         end if
         vdevice%attr%a%justify = iand(ior(vdevice%attr%a%justify, D_BOTTOM), not(ior(D_TOP, D_YCENTERED)))
      end subroutine draw_bottomjustify

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
      subroutine draw_textslant(val)
         real(real32), intent(in) :: val
         type(Token), pointer :: tok(:)
         if (vdevice%inobject /= 0) then
            tok => draw_newtokens(2); tok(1)%i = OBJ_TEXTSLANT; tok(2)%f = val; return
         end if
         vdevice%attr%a%skew = val
      end subroutine draw_textslant
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
