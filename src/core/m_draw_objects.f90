module m_draw_objects
   use, intrinsic :: iso_fortran_env, only: real32, error_unit
   use m_draw_types
   use m_draw_verror
   use m_draw_newtokens
   use m_draw_matrix
   use m_draw_tensor
   use m_draw_move
   use m_draw_draw
   use m_draw_arcs
   use m_draw_curves
   use m_draw_patches
   use m_draw_polygons
   use m_draw_rect
   use m_draw_text
   use m_draw_attr
   use m_draw_viewp
   use m_draw_buffer
   use m_draw_trans
   implicit none
   private
   public :: draw_makeobj, draw_closeobj, draw_delobj, draw_genobj, &
      draw_getopenobj, draw_loadobj, draw_saveobj, draw_callobj, draw_isobj

   type :: Object
      integer :: obno = 0
      type(TokList), pointer :: tlist => null()
      type(Object), pointer  :: next  => null()
   end type Object

   type :: ObjectPtr
      type(Object), pointer :: ptr => null()
   end type ObjectPtr

   type(ObjectPtr), save :: object_table(MAXENTS)
   integer, save :: obno = -1, omax = 0

   interface
      subroutine draw_color(i)
         integer, intent(in) :: i
      end subroutine draw_color

      subroutine draw_linewidth(w)
         integer, intent(in) :: w
      end subroutine draw_linewidth

      subroutine draw_mapcolor(i, r, g, b)
         use, intrinsic :: iso_fortran_env, only: int16
         integer, intent(in) :: i
         integer(int16), intent(in) :: r, g, b
      end subroutine draw_mapcolor
   end interface

contains

   pure integer function obj_hash(n)
      integer, intent(in) :: n
      obj_hash = mod(abs(n), MAXENTS) + 1
   end function obj_hash

   subroutine draw_makeobj(n)
      integer, intent(in) :: n
      type(Object), pointer :: o
      integer :: idx

      if (vdevice%initialized == 0) call draw_verror("makeobj: draw not initialized")

      idx = obj_hash(n)
      o => object_table(idx)%ptr
      do while (associated(o))
         if (o%obno == n) then
            call draw_delobj(n)
            exit
         end if
         o => o%next
      end do

      obno = n
      vdevice%tokens => null()
      vdevice%inobject = 1
      if (omax <= n) omax = n + 1
   end subroutine draw_makeobj

   subroutine draw_closeobj()
      type(Object), pointer :: o
      integer :: idx

      if (vdevice%inobject == 0) call draw_verror("closeobj: not in an object")

      vdevice%inobject = 0
      idx = obj_hash(obno)

      allocate(o)
      o%obno = obno
      o%tlist => vdevice%tokens
      o%next => object_table(idx)%ptr
      object_table(idx)%ptr => o
      obno = -1
   end subroutine draw_closeobj

   subroutine draw_delobj(n)
      integer, intent(in) :: n
      type(Object), pointer :: o, lo
      type(TokList), pointer :: tl, ntl
      integer :: idx

      idx = obj_hash(n)
      lo => null()
      o  => object_table(idx)%ptr
      do while (associated(o))
         if (o%obno == n) exit
         lo => o
         o  => o%next
      end do

      if (associated(o)) then
         tl => o%tlist
         do while (associated(tl))
            ntl => tl%next
            if (allocated(tl%toks)) deallocate(tl%toks)
            deallocate(tl)
            tl => ntl
         end do
         if (.not. associated(lo)) then
            object_table(idx)%ptr => o%next
         else
            lo%next => o%next
         end if
         deallocate(o)
      end if
   end subroutine draw_delobj

   integer function draw_genobj()
      draw_genobj = omax
      omax = omax + 1
   end function draw_genobj

   integer function draw_getopenobj()
      draw_getopenobj = obno
   end function draw_getopenobj

   subroutine draw_loadobj(n, file)
      integer, intent(in) :: n
      character(len=*), intent(in) :: file
      integer :: u, ios, idx, k, slen
      type(TokList), pointer :: tl
      type(Object), pointer :: o

      open(newunit=u, file=trim(file), access='stream', form='unformatted', status='old', action='read', iostat=ios)
      if (ios /= 0) then
         write(error_unit, '(A, A)') "loadobject: unable to open object file ", trim(file)
         return
      end if

      idx = obj_hash(n)
      allocate(o, tl)
      o%obno = n
      o%next => object_table(idx)%ptr
      object_table(idx)%ptr => o
      o%tlist => tl
      tl%next => null()

      read(u) tl%count
      allocate(tl%toks(tl%count))
      do k = 1, tl%count
         read(u) tl%toks(k)%i, tl%toks(k)%f, slen
         if (slen > 0) then
            allocate(character(len=slen) :: tl%toks(k)%s)
            read(u) tl%toks(k)%s
         end if
      end do
      close(u)
   end subroutine draw_loadobj

   subroutine draw_saveobj(n, file)
      integer, intent(in) :: n
      character(len=*), intent(in) :: file
      integer :: u, ios, idx, count, k, slen
      type(Object), pointer :: o
      type(TokList), pointer :: tl

      open(newunit=u, file=trim(file), access='stream', form='unformatted', status='replace', action='write', iostat=ios)
      if (ios /= 0) then
         write(error_unit, '(A, A)') "saveobject: unable to open object file ", trim(file)
         return
      end if

      idx = obj_hash(n)
      o => object_table(idx)%ptr
      do while (associated(o))
         if (o%obno == n) exit
         o => o%next
      end do
      if (.not. associated(o)) then
         close(u); return
      end if

      count = 0
      tl => o%tlist
      do while (associated(tl))
         count = count + tl%count
         tl => tl%next
      end do

      write(u) count
      tl => o%tlist
      do while (associated(tl))
         do k = 1, tl%count
            if (allocated(tl%toks(k)%s)) then
               slen = len(tl%toks(k)%s)
            else
               slen = 0
            end if
            write(u) tl%toks(k)%i, tl%toks(k)%f, slen
            if (slen > 0) write(u) tl%toks(k)%s
         end do
         tl => tl%next
      end do
      close(u)
   end subroutine draw_saveobj

   subroutine draw_doarc(x, y, xoff, yoff, cosine, sine, nsegs)
      real(real32), intent(in) :: x, y, xoff, yoff, cosine, sine
      integer, intent(in) :: nsegs
      real(real32) :: cx, cy, dx, dy
      integer :: i

      cx = x + xoff; cy = y + yoff
      call draw_move2(cx, cy)
      do i = 1, nsegs
         dx = cx - x; dy = cy - y
         cx = x + dx * cosine - dy * sine
         cy = y + dx * sine + dy * cosine
         call draw_draw2(cx, cy)
      end do
   end subroutine draw_doarc

   recursive subroutine draw_callobj(n)
      integer, intent(in) :: n
      type(Object), pointer :: o
      type(TokList), pointer :: tl
      real(real32) :: prod(4,4), tmpmat(4,4), S_ten(4,4,4)
      integer :: sync, i, j, k, p_idx, inpoly, idx, idum
      real(real32) :: cx, cy, cz
      type(Token), pointer :: t(:)

      if (vdevice%initialized == 0) call draw_verror("callobj: draw not initialized")

      if (vdevice%inobject /= 0) then
         t => draw_newtokens(2)
         t(1)%i = OBJ_CALLOBJ; t(2)%i = n
         return
      end if

      idx = obj_hash(n)
      o => object_table(idx)%ptr
      do while (associated(o))
         if (o%obno == n) exit
         o => o%next
      end do
      if (.not. associated(o)) return

      sync = vdevice%sync
      if (sync /= 0) vdevice%sync = 0

      tl => o%tlist
      do while (associated(tl))
         k = 1
         do while (k <= tl%count)
            select case (tl%toks(k)%i)
             case (OBJ_ARC)
               call draw_doarc(tl%toks(k+1)%f, tl%toks(k+2)%f, tl%toks(k+3)%f, &
                  tl%toks(k+4)%f, tl%toks(k+5)%f, tl%toks(k+6)%f, tl%toks(k+7)%i)
               k = k + 8
             case (OBJ_BACKBUFFER)
               idum = draw_backbuffer(); k = k + 1
             case (OBJ_BACKFACING)
               call draw_backface(tl%toks(k+1)%i); k = k + 2
             case (OBJ_BOXTEXT)
               call draw_boxtext(tl%toks(k+1)%f, tl%toks(k+2)%f, tl%toks(k+3)%f, &
                  tl%toks(k+4)%f, tl%toks(k+5)%s)
               k = k + 6
             case (OBJ_CALLOBJ)
               call draw_callobj(tl%toks(k+1)%i); k = k + 2
             case (OBJ_TEXTSLANT)
               call draw_textslant(tl%toks(k+1)%f); k = k + 2
             case (OBJ_CENTERTEXT)
               call draw_centertext(tl%toks(k+1)%i); k = k + 2
             case (OBJ_TEXTJUSTIFY)
               call draw_textjustify(tl%toks(k+1)%i); k = k + 2
             case (OBJ_RIGHTJUSTIFY)
               call draw_rightjustify(); k = k + 1
             case (OBJ_LEFTJUSTIFY)
               call draw_leftjustify(); k = k + 1
             case (OBJ_XCENTERTEXT)
               call draw_xcentertext(); k = k + 1
             case (OBJ_YCENTERTEXT)
               call draw_ycentertext(); k = k + 1
             case (OBJ_TOPJUSTIFY)
               call draw_topjustify(); k = k + 1
             case (OBJ_BOTTOMJUSTIFY)
               call draw_bottomjustify(); k = k + 1
             case (OBJ_CIRCLE)
               inpoly = vdevice%inpolygon
               if ((vdevice%attr%a%fill /= 0 .or. vdevice%attr%a%hatch /= 0) .and. inpoly == 0) call draw_makepoly()
               call draw_doarc(tl%toks(k+1)%f, tl%toks(k+2)%f, tl%toks(k+3)%f, &
                  0.0_real32, tl%toks(k+4)%f, tl%toks(k+5)%f, tl%toks(k+6)%i)
               if ((vdevice%attr%a%fill /= 0 .or. vdevice%attr%a%hatch /= 0) .and. inpoly == 0) then
                  call draw_closepoly()
               else
                  call draw_draw2(tl%toks(k+1)%f + tl%toks(k+3)%f, tl%toks(k+2)%f)
               end if
               k = k + 7
             case (OBJ_CLEAR)
               idum = vdevice%dev%Vclear(); k = k + 1
             case (OBJ_COLOR)
               call draw_color(tl%toks(k+1)%i); k = k + 2
             case (OBJ_LINEWIDTH)
               call draw_linewidth(tl%toks(k+1)%i); k = k + 2
             case (OBJ_DRAW)
               call draw_draw(tl%toks(k+1)%f, tl%toks(k+2)%f, tl%toks(k+3)%f); k = k + 4
             case (OBJ_DASHCODE)
               call draw_dashcode(tl%toks(k+1)%f); k = k + 2
             case (OBJ_DRAWCHAR)

               call draw_drawchar(tl%toks(k+1)%i); k = k + 2
             case (OBJ_DRAWSTR)
               call draw_drawstr(tl%toks(k+1)%s); k = k + 2
             case (OBJ_LINESTYLE)
               call draw_linestyle(tl%toks(k+1)%s); k = k + 2
             case (OBJ_FIXEDWIDTH)
               call draw_fixedwidth(tl%toks(k+1)%i); k = k + 2
             case (OBJ_FONT)
               call draw_font(tl%toks(k+1)%s); k = k + 2
             case (OBJ_HATCHANG)
               vdevice%attr%a%hatchcos = tl%toks(k+1)%f
               vdevice%attr%a%hatchsin = tl%toks(k+2)%f
               k = k + 3
             case (OBJ_HATCHPITCH)
               vdevice%attr%a%hatchpitch = tl%toks(k+1)%f; k = k + 2
             case (OBJ_LOADMATRIX)
               p_idx = k + 1
               do i = 1, 4
                  do j = 1, 4
                     vdevice%transmat%m(i,j) = tl%toks(p_idx)%f
                     p_idx = p_idx + 1
                  end do
               end do
               vdevice%cpVvalid = 0
               k = k + 17
             case (OBJ_MAPCOLOR)
               call draw_mapcolor(tl%toks(k+1)%i, int(tl%toks(k+2)%i, 2), &
                  int(tl%toks(k+3)%i, 2), int(tl%toks(k+4)%i, 2))
               k = k + 5
             case (OBJ_MOVE)
               call draw_move(tl%toks(k+1)%f, tl%toks(k+2)%f, tl%toks(k+3)%f); k = k + 4
             case (OBJ_MULTMATRIX)
               p_idx = k + 1
               do i = 1, 4
                  do j = 1, 4
                     tmpmat(i,j) = tl%toks(p_idx)%f
                     p_idx = p_idx + 1
                  end do
               end do
               call draw_mult4x4(prod, tmpmat, vdevice%transmat%m)
               call draw_loadmatrix(prod)
               k = k + 17
             case (OBJ_POLY)
               call draw_polyobj(tl%toks(k+1)%i, tl%toks(k+2 : k + 1 + 3*tl%toks(k+1)%i))
               k = k + 2 + 3 * tl%toks(k+1)%i
             case (OBJ_POLYFILL)
               call draw_polyfill(tl%toks(k+1)%i); k = k + 2
             case (OBJ_POLYHATCH)
               call draw_polyhatch(tl%toks(k+1)%i); k = k + 2
             case (OBJ_POPATTRIBUTES)
               call draw_popattributes(); k = k + 1
             case (OBJ_POPMATRIX)
               call draw_popmatrix(); k = k + 1
             case (OBJ_POPVIEWPORT)
               call draw_popviewport(); k = k + 1
             case (OBJ_PUSHATTRIBUTES)
               call draw_pushattributes(); k = k + 1
             case (OBJ_PUSHMATRIX)
               call draw_pushmatrix(); k = k + 1
             case (OBJ_PUSHVIEWPORT)
               call draw_pushviewport(); k = k + 1
             case (OBJ_RCURVE)
               i = tl%toks(k+1)%i
               cx = tl%toks(k+2)%f; cy = tl%toks(k+3)%f; cz = tl%toks(k+4)%f
               p_idx = k + 5
               do j = 1, 4
                  do idum = 1, 4
                     tmpmat(j, idum) = tl%toks(p_idx)%f
                     p_idx = p_idx + 1
                  end do
               end do
               call draw_mult4x4(prod, tmpmat, vdevice%transmat%m)
               call draw_drcurve(i, prod)
               vdevice%cpW(D_X) = cx; vdevice%cpW(D_Y) = cy; vdevice%cpW(D_Z) = cz
               k = k + 21
             case (OBJ_RPATCH)
               cx = tl%toks(k+1)%f; cy = tl%toks(k+2)%f; cz = tl%toks(k+3)%f
               p_idx = k + 10
               do i = 1, 4
                  do j = 1, 4
                     S_ten(1,i,j) = tl%toks(p_idx)%f; S_ten(2,i,j) = tl%toks(p_idx+1)%f
                     S_ten(3,i,j) = tl%toks(p_idx+2)%f; S_ten(4,i,j) = tl%toks(p_idx+3)%f
                     p_idx = p_idx + 4
                  end do
               end do
               call draw_transformtensor(S_ten, vdevice%transmat%m)
               call draw_drpatch(S_ten, tl%toks(k+4)%i, tl%toks(k+5)%i, tl%toks(k+6)%i, &
                  tl%toks(k+7)%i, tl%toks(k+8)%i, tl%toks(k+9)%i)
               vdevice%cpW(D_X) = cx; vdevice%cpW(D_Y) = cy; vdevice%cpW(D_Z) = cz
               k = p_idx
             case (OBJ_SECTOR)
               inpoly = vdevice%inpolygon
               if ((vdevice%attr%a%fill /= 0 .or. vdevice%attr%a%hatch /= 0) .and. inpoly == 0) call draw_makepoly()
               call draw_doarc(tl%toks(k+1)%f, tl%toks(k+2)%f, tl%toks(k+3)%f, &
                  tl%toks(k+4)%f, tl%toks(k+5)%f, tl%toks(k+6)%f, tl%toks(k+7)%i)
               call draw_draw(tl%toks(k+1)%f, tl%toks(k+2)%f, 0.0_real32)
               if ((vdevice%attr%a%fill /= 0 .or. vdevice%attr%a%hatch /= 0) .and. inpoly == 0) then
                  call draw_closepoly()
               else
                  call draw_draw2(tl%toks(k+1)%f + tl%toks(k+3)%f, tl%toks(k+2)%f + tl%toks(k+4)%f)
               end if
               k = k + 8
             case (OBJ_TEXTANG)
               vdevice%attr%a%textcos = tl%toks(k+1)%f; vdevice%attr%a%textsin = tl%toks(k+2)%f
               k = k + 3
             case (OBJ_TEXTSIZE)
               call draw_textsize(tl%toks(k+1)%f, tl%toks(k+2)%f); k = k + 3
             case (OBJ_VIEWPORT)
               call draw_viewport(tl%toks(k+1)%f, tl%toks(k+2)%f, tl%toks(k+3)%f, tl%toks(k+4)%f); k = k + 5
             case (OBJ_TRANSLATE)
               call draw_translate(tl%toks(k+1)%f, tl%toks(k+2)%f, tl%toks(k+3)%f); k = k + 4
             case (OBJ_SCALE)
               vdevice%transmat%m(1,1:4) = vdevice%transmat%m(1,1:4) * tl%toks(k+1)%f
               vdevice%transmat%m(2,1:4) = vdevice%transmat%m(2,1:4) * tl%toks(k+2)%f
               vdevice%transmat%m(3,1:4) = vdevice%transmat%m(3,1:4) * tl%toks(k+3)%f
               k = k + 4
             case (OBJ_ROTATE)
               call draw_rotate(tl%toks(k+1)%f, char(tl%toks(k+2)%i)); k = k + 3
             case (OBJ_VFLUSH)
               k = k + 1
             case (OBJ_RECT)
               call draw_rect(tl%toks(k+1)%f, tl%toks(k+2)%f, tl%toks(k+3)%f, tl%toks(k+4)%f); k = k + 5
             case default
               write(error_unit, '(A, I0)') "UNEXPECTED OBJECT ", tl%toks(k)%i
               call draw_verror("draw: internal error in callobj")
            end select
         end do
         tl => tl%next
      end do
      if (sync /= 0) then
         vdevice%sync = 1
         idum = vdevice%dev%Vsync()
      end if
   end subroutine draw_callobj

   function draw_isobj(n)
      integer, intent(in) :: n
      type(Object), pointer :: o
      logical          :: draw_isobj
      o => object_table(obj_hash(n))%ptr
      do while (associated(o))
         if (o%obno == n) then
            draw_isobj = .true.; return
         end if
         o => o%next
      end do
      draw_isobj = .false.
   end function draw_isobj

end module m_draw_objects
