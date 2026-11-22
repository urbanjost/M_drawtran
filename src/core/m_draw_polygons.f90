module m_draw_polygons
   use, intrinsic :: iso_fortran_env, only: real32, int64, error_unit
   use m_draw_types
   use m_draw_verror
   use m_draw_matrix
   use m_draw_mapping
   use m_draw_move
   use m_draw_draw
   implicit none
   private
   public :: draw_polyfill, draw_polyhatch, draw_hatchang, draw_hatchpitch, &
      draw_backfacedir, draw_backface, draw_polyobj, draw_poly2, &
      draw_poly, draw_pmove, draw_pdraw, draw_makepoly, draw_closepoly

   interface draw_polyfill
      module procedure draw_polyfill_integer
      module procedure draw_polyfill_logical
   end interface draw_polyfill

   interface draw_polyhatch
      module procedure draw_polyhatch_integer
      module procedure draw_polyhatch_logical
   end interface draw_polyhatch

   interface draw_backface
      module procedure draw_backface_integer
      module procedure draw_backface_logical
   end interface draw_backface

   interface draw_backfacedir
      module procedure draw_backfacedir_integer
      module procedure draw_backfacedir_logical
   end interface draw_backfacedir

   real(real32), save :: F(6, 4) = 0.0_real32
   real(real32), save :: S(6, 4) = 0.0_real32
   real(real32), save :: I_pt(4) = 0.0_real32
   real(real32), save :: p(MAXVERTS, 4) = 0.0_real32
   real(real32), save :: newp(3, MAXVERTS) = 0.0_real32

   integer, save :: nout = 0
   integer, save :: first(6) = 1
   integer, save :: np = 0
   integer, save :: ip1(MAXVERTS) = 0
   integer, save :: ip2(MAXVERTS) = 0

   ! Orientation of backfacing polygons (in screen coords): 1 = clockwise, 0 = anticlockwise
   integer, save :: clockwise = 1

   interface
      function draw_newtokens(num) result(toks)
         import :: Token
         integer, intent(in) :: num
         type(Token), pointer :: toks(:)
      end function draw_newtokens
   end interface

contains

   subroutine draw_polyfill_integer(onoff)
      integer, intent(in) :: onoff
      type(Token), pointer :: tok(:)

      if (vdevice%inobject /= 0) then
         tok => draw_newtokens(2)
         tok(1)%i = OBJ_POLYFILL
         tok(2)%i = onoff
         return
      end if

      vdevice%attr%a%fill = onoff
      vdevice%attr%a%hatch = 0
   end subroutine draw_polyfill_integer

   subroutine draw_polyfill_logical(onoff)
      logical, intent(in) :: onoff
      call draw_polyfill_integer(merge(1,0,onoff))
   end subroutine draw_polyfill_logical

   subroutine draw_polyhatch_integer(onoff)
      integer, intent(in) :: onoff
      type(Token), pointer :: tok(:)

      if (vdevice%inobject /= 0) then
         tok => draw_newtokens(2)
         tok(1)%i = OBJ_POLYHATCH
         tok(2)%i = onoff
         return
      end if

      vdevice%attr%a%hatch = onoff
      vdevice%attr%a%fill = 0
   end subroutine draw_polyhatch_integer

   subroutine draw_polyhatch_logical(onoff)
      logical, intent(in) :: onoff
      call draw_polyhatch_integer(merge(1,0,onoff))
   end subroutine draw_polyhatch_logical

   subroutine draw_hatchang(a)
      real(real32), intent(in) :: a
      type(Token), pointer :: tok(:)

      if (vdevice%initialized == 0) then
         call draw_verror("hatchang - draw not initialized")
      end if

      if (vdevice%inobject /= 0) then
         tok => draw_newtokens(3)
         tok(1)%i = OBJ_HATCHANG
         tok(2)%f = cos(a * D2R)
         tok(3)%f = sin(a * D2R)
         return
      end if

      vdevice%attr%a%hatchcos = cos(a * D2R)
      vdevice%attr%a%hatchsin = sin(a * D2R)
   end subroutine draw_hatchang

   subroutine draw_hatchpitch(a)
      real(real32), intent(in) :: a
      type(Token), pointer :: tok(:)

      if (vdevice%initialized == 0) then
         call draw_verror("hatchpitch - draw not initialized")
      end if

      if (vdevice%inobject /= 0) then
         tok => draw_newtokens(2)
         tok(1)%i = OBJ_HATCHPITCH
         tok(2)%f = a
         return
      end if

      vdevice%attr%a%hatchpitch = a
   end subroutine draw_hatchpitch

   subroutine draw_hatch(n, poly_pts)
      integer, intent(in)          :: n
      real(real32), intent(inout)  :: poly_pts(3, n)
      real(real32) :: yl, xint, y1, y2, x1, x2, tmp
      real(real32) :: ymax, ymin
      real(real32) :: xi(256)
      real(real32) :: z, s_ang, c_ang, pitch
      integer      :: sync, i, j
      logical      :: sorted

      ymax = -1.0e30_real32
      ymin =  1.0e30_real32

      c_ang = vdevice%attr%a%hatchcos
      s_ang = vdevice%attr%a%hatchsin
      pitch = vdevice%attr%a%hatchpitch

      sync = vdevice%sync
      if (sync /= 0) then
         vdevice%sync = 0
      end if

      ! Rotate by the x-hatch angle
      do i = 1, n
         tmp = poly_pts(1, i)
         poly_pts(1, i) = poly_pts(1, i) * c_ang - poly_pts(2, i) * s_ang
         poly_pts(2, i) = tmp * s_ang + poly_pts(2, i) * c_ang
         ymax = max(ymax, poly_pts(2, i))
         ymin = min(ymin, poly_pts(2, i))
      end do

      ! For each y value, get a list of X intersections
      yl = ymax - pitch
      do while (yl > ymin)
         j = 0
         do i = 1, n - 1
            if (draw_yintersect(yl, poly_pts(1, i), poly_pts(2, i), &
               poly_pts(1, i+1), poly_pts(2, i+1), xint) /= 0) then
               j = j + 1
               xi(j) = xint
            end if
         end do
         ! Last edge
         if (draw_yintersect(yl, poly_pts(1, n), poly_pts(2, n), &
            poly_pts(1, 1), poly_pts(2, 1), xint) /= 0) then
            j = j + 1
            xi(j) = xint
         end if

         ! Sort the X intersections
         sorted = .false.
         do while (.not. sorted)
            sorted = .true.
            do i = 1, j - 1
               if (xi(i) > xi(i+1)) then
                  tmp = xi(i)
                  xi(i) = xi(i+1)
                  xi(i+1) = tmp
                  sorted = .false.
               end if
            end do
         end do

         ! Draw the hatch lines (rotated back)
         z = poly_pts(3, 1)
         do i = 1, j - 1, 2
            y1 = yl * c_ang - xi(i) * s_ang
            y2 = yl * c_ang - xi(i+1) * s_ang
            x1 = xi(i) * c_ang + yl * s_ang
            x2 = xi(i+1) * c_ang + yl * s_ang
            call draw_move(x1, y1, z)
            call draw_draw(x2, y2, z)
         end do
         yl = yl - pitch
      end do

      ! Rotate vertices back and draw outline
      do i = 1, n
         tmp = poly_pts(1, i)
         poly_pts(1, i) = tmp * c_ang + poly_pts(2, i) * s_ang
         poly_pts(2, i) = poly_pts(2, i) * c_ang - tmp * s_ang
      end do

      call draw_move(poly_pts(1, 1), poly_pts(2, 1), poly_pts(3, 1))
      do i = 2, n
         call draw_draw(poly_pts(1, i), poly_pts(2, i), poly_pts(3, i))
      end do
      call draw_draw(poly_pts(1, 1), poly_pts(2, 1), poly_pts(3, 1))

      if (sync /= 0) then
         vdevice%sync = 1
         sync = vdevice%dev%Vsync()
      end if
   end subroutine draw_hatch

   integer function draw_yintersect(y, x1, y1, x2, y2, xint)
      real(real32), intent(in)  :: y, x1, y1, x2, y2
      real(real32), intent(out) :: xint
      real(real32) :: t, a

      a = y2 - y1
      if (abs(a) >= 0.00001_real32) then
         t = (y - y1) / a
         if (t >= 0.0_real32 .and. t <= 1.0_real32) then
            xint = x1 + t * (x2 - x1)
            draw_yintersect = 1
            return
         end if
      end if
      draw_yintersect = 0
   end function draw_yintersect

   subroutine draw_backfacedir_integer(cdir)
      integer, intent(in) :: cdir
      clockwise = cdir
   end subroutine draw_backfacedir_integer

   subroutine draw_backfacedir_logical(cdir)
      logical, intent(in) :: cdir
      call draw_backfacedir_integer(merge(1,0,cdir))
   end subroutine draw_backfacedir_logical

   subroutine draw_backface_integer(onoff)
      integer, intent(in) :: onoff
      vdevice%attr%a%backface = onoff
   end subroutine draw_backface_integer

   subroutine draw_backface_logical(onoff)
      logical, intent(in) :: onoff
      call draw_backface_integer(merge(1,0,onoff))
   end subroutine draw_backface_logical

   integer function draw_dopoly(n)
      integer, intent(in) :: n
      integer :: i, idum

      if (n > MAXVERTS) then
         write(error_unit, '(A, I0)') &
            "dopoly: can't hatch or fill a polygon with more than ", MAXVERTS
         draw_dopoly = 0
         return
      end if

      if (vdevice%clipoff == 0) then
         call draw_polyclip(n)
      else
         nout = n
         do i = 1, n
            ip1(i) = draw_WtoVx(p(i, 1:4))
            ip2(i) = draw_WtoVy(p(i, 1:4))
         end do
      end if

      if (vdevice%attr%a%backface /= 0 .and. draw_checkbacki() /= 0) then
         draw_dopoly = 0
         return
      end if

      if (vdevice%attr%a%fill /= 0) then
         if (nout > 2) then
            idum = vdevice%dev%Vfill(nout, ip1(1:nout), ip2(1:nout))
         end if
      else
         vdevice%cpVx = ip1(1)
         vdevice%cpVy = ip2(1)
         vdevice%cpVvalid = 0
         call draw_polyoutline(nout, ip1(1:nout), ip2(1:nout))
      end if

      draw_dopoly = 1
   end function draw_dopoly

   subroutine draw_polyoutline(n, ipx, ipy)
      integer, intent(in) :: n
      integer, intent(in) :: ipx(n), ipy(n)
      integer :: i, idum

      if (n > 2) then
         do i = 2, n
            idum = vdevice%dev%Vdraw(ipx(i), ipy(i))
            vdevice%cpVx = ipx(i)
            vdevice%cpVy = ipy(i)
         end do
         idum = vdevice%dev%Vdraw(ipx(1), ipy(1))
         vdevice%cpVx = ipx(1)
         vdevice%cpVy = ipy(1)
      end if
   end subroutine draw_polyoutline

   subroutine draw_polyobj(n, dp)
      integer, intent(in) :: n
      type(Token), intent(in) :: dp(:)
      integer :: b, i, j
      real(real32) :: vect(4), result(4), tmp_pts(3, n)

      j = 0
      do i = 1, n
         vect(D_X) = dp(j + D_X)%f
         vect(D_Y) = dp(j + D_Y)%f
         vect(D_Z) = dp(j + D_Z)%f
         vect(D_W) = 1.0_real32
         tmp_pts(1:3, i) = vect(1:3)
         call draw_multvector(result, vect, vdevice%transmat%m)
         p(i, 1:4) = result(1:4)
         j = j + 3
      end do

      b = draw_dopoly(n)
      if (b /= 0 .and. vdevice%attr%a%hatch /= 0) then
         call draw_hatch(n, tmp_pts)
      end if

      vdevice%cpW(D_X) = dp(D_X)%f
      vdevice%cpW(D_Y) = dp(D_Y)%f
      vdevice%cpW(D_Z) = dp(D_Z)%f
   end subroutine draw_polyobj

   subroutine draw_poly2(n, dp)
      integer, intent(in) :: n
      real(real32), intent(in) :: dp(2, n)
      integer :: i
      real(real32) :: np_local(3, MAXVERTS)

      if (vdevice%initialized == 0) then
         call draw_verror("poly2: draw not initialized")
      end if

      do i = 1, n
         np_local(D_X, i) = dp(D_X, i)
         np_local(D_Y, i) = dp(D_Y, i)
         np_local(D_Z, i) = 0.0_real32
      end do
      call draw_poly(n, np_local(1:3, 1:n))
   end subroutine draw_poly2

   subroutine draw_poly(n, dp)
      integer, intent(in)      :: n
      real(real32), intent(in) :: dp(3, n)   ! <-- Changed from dp(n, 3)
      integer                  :: sync, b, i, j
      real(real32)             :: vect(4), result(4), dp_copy(3, n) ! <-- Changed from (n, 3)
      type(Token), pointer     :: tok(:)

      if (vdevice%initialized == 0) then
         call draw_verror("poly: draw not initialized")
      end if

      if (vdevice%inobject /= 0) then
         tok => draw_newtokens(2 + 3 * n)

         tok(1)%i = OBJ_POLY
         tok(2)%i = n
         j = 2
         do i = 1, n
            tok(j + D_X)%f = dp(D_X, i)      ! <-- Flipped indices
            tok(j + D_Y)%f = dp(D_Y, i)      ! <-- Flipped indices
            tok(j + D_Z)%f = dp(D_Z, i)      ! <-- Flipped indices
            j = j + 3
         end do
         return
      end if

      do i = 1, n
         vect(D_X) = dp(D_X, i)              ! <-- Flipped indices
         vect(D_Y) = dp(D_Y, i)              ! <-- Flipped indices
         vect(D_Z) = dp(D_Z, i)              ! <-- Flipped indices
         vect(D_W) = 1.0_real32
         call draw_multvector(result, vect, vdevice%transmat%m)
         p(i, 1:4) = result(1:4)             ! Note: verify if p is p(MAX, 4) or p(4, MAX)
      end do

      sync = vdevice%sync
      if (sync /= 0) then
         vdevice%sync = 0
      end if

      b = draw_dopoly(n)
      if (b /= 0 .and. vdevice%attr%a%hatch /= 0) then
         dp_copy = dp
         call draw_hatch(n, dp_copy)
      end if

      if (sync /= 0) then
         vdevice%sync = 1
         sync = vdevice%dev%Vsync()
      end if

      vdevice%cpW(D_X) = dp(D_X, 1)          ! <-- Flipped indices
      vdevice%cpW(D_Y) = dp(D_Y, 1)          ! <-- Flipped indices
      vdevice%cpW(D_Z) = dp(D_Z, 1)          ! <-- Flipped indices
   end subroutine draw_poly
   subroutine draw_pmove(x, y, z)
      real(real32), intent(in) :: x, y, z
      np = 1
      p(np, D_X) = x
      p(np, D_Y) = y
      p(np, D_Z) = z
      p(np, D_W) = 1.0_real32
   end subroutine draw_pmove

   subroutine draw_pdraw(x, y, z)
      real(real32), intent(in) :: x, y, z
      character(len=100) :: buf
      np = np + 1
      if (np > MAXVERTS) then
         write(buf, '(A, I0, A)') &
            "pdraw: can't draw a polygon with more than ", MAXVERTS, " vertices"
         call draw_verror(buf)
      end if
      p(np, D_X) = x
      p(np, D_Y) = y
      p(np, D_Z) = z
      p(np, D_W) = 1.0_real32
   end subroutine draw_pdraw

   subroutine draw_makepoly()
      vdevice%inpolygon = 1
      vdevice%pmove => draw_pmove
      vdevice%pdraw => draw_pdraw
      np = 1
      p(np, D_X) = vdevice%cpW(D_X)
      p(np, D_Y) = vdevice%cpW(D_Y)
      p(np, D_Z) = vdevice%cpW(D_Z)
      p(np, D_W) = 1.0_real32
   end subroutine draw_makepoly

   subroutine draw_closepoly()
      real(real32) :: lstx, lsty, lstz
      real(real32) :: result(4)
      integer :: sync, b, i, j
      type(Token), pointer :: tok(:)
      if (vdevice%initialized == 0) then
         call draw_verror("closepoly: draw not initialized")
      end if
      vdevice%inpolygon = 0
      if (vdevice%inobject /= 0) then
         tok => draw_newtokens(2 + 3 * np)
         tok(1)%i = OBJ_POLY
         tok(2)%i = np
         j = 2
         do i = 1, np
            tok(j + D_X)%f = p(i, D_X)
            tok(j + D_Y)%f = p(i, D_Y)
            tok(j + D_Z)%f = p(i, D_Z)
            j = j + 3
         end do
         return
      end if
      lstx = p(np, D_X)
      lsty = p(np, D_Y)
      lstz = p(np, D_Z)
      if (vdevice%attr%a%hatch /= 0) then
         do i = 1, np
            newp(1:3, i) = p(i, 1:3)
         end do
      end if
      do i = 1, np
         call draw_multvector(result, p(i, 1:4), vdevice%transmat%m)
         p(i, 1:4) = result(1:4)
      end do
      sync = vdevice%sync
      if (sync /= 0) then
         vdevice%sync = 0
      end if
      b = draw_dopoly(np)
      if (b /= 0 .and. vdevice%attr%a%hatch /= 0) then
         call draw_hatch(np, newp(1:3, 1:np))
      end if
      if (sync /= 0) then
         vdevice%sync = 1
         sync = vdevice%dev%Vsync()
      end if
      vdevice%cpW(D_X) = lstx
      vdevice%cpW(D_Y) = lsty
      vdevice%cpW(D_Z) = lstz
   end subroutine draw_closepoly

   integer function draw_checkbacki()
      integer(int64) :: z
      integer :: x1, x2, y1, y2
      x1 = ip1(2) - ip1(1)
      x2 = ip1(3) - ip1(2)
      y1 = ip2(2) - ip2(1)
      y2 = ip2(3) - ip2(2)
      z = int(x1, int64) * int(y2, int64) - int(y1, int64) * int(x2, int64)
      if (clockwise /= 0) then
         draw_checkbacki = merge(1, 0, z <= 0_int64)
      else
         draw_checkbacki = merge(1, 0, z > 0_int64)
      end if
   end function draw_checkbacki

! Sutherland-Hodgman 3D homogeneous polygon clipper (sides 1..6)
   subroutine draw_polyclip(n)
      integer, intent(in) :: n
      integer :: i
      nout = 0
      first(1:6) = 1
      do i = 1, n
         call draw_shclip(p(i, 1:4), 1)
      end do
      call draw_shclose(1)
   end subroutine draw_polyclip

   recursive subroutine draw_shclip(pt, side)
      real(real32), intent(in) :: pt(4)
      integer, intent(in) :: side
      real(real32) :: P_loc(4)
      if (side == 7) then
         nout = nout + 1
         ip1(nout) = draw_WtoVx(pt)
         ip2(nout) = draw_WtoVy(pt)
      else
         call draw_copyvector(P_loc, pt)
         if (first(side) /= 0) then
            first(side) = 0
            call draw_copyvector(F(side, 1:4), P_loc)
         else if (draw_intersect(side, I_pt, P_loc) /= 0) then
            call draw_shclip(I_pt, side + 1)
         end if
         call draw_copyvector(S(side, 1:4), P_loc)
         if (draw_visible(side) /= 0) then
            call draw_shclip(S(side, 1:4), side + 1)
         end if
      end if
   end subroutine draw_shclip

   recursive subroutine draw_shclose(side)
      integer, intent(in) :: side
      if (side <= 6) then
         if (draw_intersect(side, I_pt, F(side, 1:4)) /= 0) then
            call draw_shclip(I_pt, side + 1)
         end if
         call draw_shclose(side + 1)
         first(side) = 1
      end if
   end subroutine draw_shclose

   integer function draw_intersect(side, I_out, pt)
      integer, intent(in) :: side
      real(real32), intent(out) :: I_out(4)
      real(real32), intent(in) :: pt(4)
      real(real32) :: wc1, wc2, a
      wc1 = 0.0_real32
      wc2 = 0.0_real32
      select case (side)
       case (1) ! x - left
         wc1 = pt(4) + pt(1)
         wc2 = S(side, 4) + S(side, 1)
       case (2) ! x - right
         wc1 = pt(4) - pt(1)
         wc2 = S(side, 4) - S(side, 1)
       case (3) ! y - bottom
         wc1 = pt(4) + pt(2)
         wc2 = S(side, 4) + S(side, 2)
       case (4) ! y - top
         wc1 = pt(4) - pt(2)
         wc2 = S(side, 4) - S(side, 2)
       case (5) ! z - near
         wc1 = pt(4) + pt(3)
         wc2 = S(side, 4) + S(side, 3)
       case (6) ! z - far
         wc1 = pt(4) - pt(3)
         wc2 = S(side, 4) - S(side, 3)
       case default
         call draw_verror("intersect: ridiculous side value")
      end select
      if (wc1 * wc2 < 0.0_real32) then
         a = wc1 / (wc1 - wc2)
         if (a < 0.0_real32 .or. a > 1.0_real32) then
            draw_intersect = 0
            return
         else
            I_out(1:4) = pt(1:4) + a * (S(side, 1:4) - pt(1:4))
            draw_intersect = 1
            return
         end if
      end if
      draw_intersect = 0
   end function draw_intersect

   integer function draw_visible(side)
      integer, intent(in) :: side
      real(real32) :: wc
      wc = 0.0_real32
      select case (side)
       case (1)
         wc = S(side, 4) + S(side, 1)
       case (2)
         wc = S(side, 4) - S(side, 1)
       case (3)
         wc = S(side, 4) + S(side, 2)
       case (4)
         wc = S(side, 4) - S(side, 2)
       case (5)
         wc = S(side, 4) + S(side, 3)
       case (6)
         wc = S(side, 4) - S(side, 3)
       case default
         call draw_verror("visible: ridiculous side value")
      end select
      draw_visible = merge(1, 0, wc >= 0.0_real32)
   end function draw_visible

end module m_draw_polygons
