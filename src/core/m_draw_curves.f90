module m_draw_curves
   use, intrinsic :: iso_fortran_env, only: real32
   use m_draw_types
   use m_draw_verror
   use m_draw_matrix
   use m_draw_mapping
   use m_draw_clip
   implicit none
   private
   public :: draw_curvebasis, draw_curveprecision, draw_rcurve, &
             draw_curve, draw_drcurve, draw_curven

   integer, save :: nsegs = 15
   logical, save :: make_prec_called = .false.
   real(real32), save :: basismatrix(4,4) = 0.0_real32
   real(real32), save :: e(4,4) = 0.0_real32

   interface
      function draw_newtokens(num) result(toks)
         import :: Token
         integer, intent(in) :: num
         type(Token), pointer :: toks(:)
      end function draw_newtokens
   end interface

contains

   subroutine draw_curvebasis(basis)
      real(real32), intent(in) :: basis(4,4)
      call draw_copymatrix(basismatrix, basis)
   end subroutine draw_curvebasis

   subroutine draw_make_prec()
      real(real32) :: n2, n3

      call draw_identmatrix(e)
      n2 = real(nsegs * nsegs, real32)
      n3 = real(nsegs, real32) * n2

      e(1,1) = 0.0_real32; e(3,3) = 0.0_real32; e(4,4) = 0.0_real32
      e(2,1) = 1.0_real32 / n3
      e(2,2) = 1.0_real32 / n2
      e(3,1) = 6.0_real32 / n3
      e(4,1) = 6.0_real32 / n3
      e(3,2) = 2.0_real32 / n2
      e(2,3) = 1.0_real32 / real(nsegs, real32)
      e(1,4) = 1.0_real32

      make_prec_called = .true.
   end subroutine draw_make_prec

   subroutine draw_curveprecision(nsegments)
      integer, intent(in) :: nsegments
      if (nsegments > 0) then
         nsegs = nsegments
      else
         call draw_verror("curveprecision: number of segments <= 0")
      end if
      call draw_make_prec()
   end subroutine draw_curveprecision

   subroutine draw_rcurve(geom)
      real(real32), intent(in) :: geom(4,4)
      real(real32) :: d(4,4), tmp(4,4)
      real(real32) :: xlast, ylast, zlast
      type(Token), pointer :: tok(:)
      integer :: i, j, k

      if (vdevice%initialized == 0) call draw_verror("rcurve: draw not initialized")
      if (.not. make_prec_called) call draw_make_prec()

      call draw_mult4x4(d, basismatrix, geom)

      xlast = sum(d(1:4, 1))
      ylast = sum(d(1:4, 2))
      zlast = sum(d(1:4, 3))

      call draw_mult4x4(tmp, e, d)

      if (vdevice%inobject /= 0) then
         tok => draw_newtokens(21)
         tok(1)%i = OBJ_RCURVE
         tok(2)%i = nsegs
         tok(3)%f = xlast
         tok(4)%f = ylast
         tok(5)%f = zlast
         k = 6
         do i = 1, 4
            do j = 1, 4
               tok(k)%f = tmp(i, j)
               k = k + 1
            end do
         end do
         return
      end if

      call draw_mult4x4(d, tmp, vdevice%transmat%m)
      call draw_drcurve(nsegs, d)

      vdevice%cpW(D_X) = xlast
      vdevice%cpW(D_Y) = ylast
      vdevice%cpW(D_Z) = zlast
   end subroutine draw_rcurve

   subroutine draw_curve(geom)
      real(real32), intent(in) :: geom(4,3)
      real(real32) :: tmp(4,4)

      if (vdevice%initialized == 0) call draw_verror("curve: draw not initialized")

      tmp(1:4, 1:3) = geom(1:4, 1:3)
      tmp(1:4, 4) = 1.0_real32

      call draw_rcurve(tmp)
   end subroutine draw_curve

   subroutine draw_drcurve(n, r)
      integer, intent(in) :: n
      real(real32), intent(inout) :: r(4,4)
      integer :: it, vx, vy, sync, idum
      real(real32) :: r_row(4)

      sync = vdevice%sync
      if (sync /= 0) vdevice%sync = 0

      vdevice%cpVvalid = 0
      if (vdevice%clipoff /= 0) then
         r_row = r(1, 1:4)
         vdevice%cpVx = draw_WtoVx(r_row)
         vdevice%cpVy = draw_WtoVy(r_row)
      end if

      do it = 1, n
         vdevice%cpWtrans(1:4) = r(1, 1:4)

         r(1, 1:4) = r(1, 1:4) + r(2, 1:4)
         r(2, 1:4) = r(2, 1:4) + r(3, 1:4)
         r(3, 1:4) = r(3, 1:4) + r(4, 1:4)

         r_row = r(1, 1:4)
         if (vdevice%clipoff /= 0) then
            vx = draw_WtoVx(r_row)
            vy = draw_WtoVy(r_row)
            idum = vdevice%dev%Vdraw(vx, vy)
            vdevice%cpVx = vx
            vdevice%cpVy = vy
            vdevice%cpVvalid = 0
         else
            if (vdevice%cpVvalid /= 0) then
               call draw_quickclip(vdevice%cpWtrans, r_row)
            else
               call draw_clip(vdevice%cpWtrans, r_row)
            end if
            r(1, 1:4) = r_row
         end if
      end do

      vdevice%cpWtrans(1:4) = r(1, 1:4)

      if (sync /= 0) then
         vdevice%sync = 1
         idum = vdevice%dev%Vsync()
      end if
   end subroutine draw_drcurve

   subroutine draw_curven(n, geom)
      integer, intent(in) :: n
      real(real32), intent(in) :: geom(n, 3)
      integer :: i

      if (vdevice%initialized == 0) call draw_verror("curven: draw not initialized")
      if (n < 4) call draw_verror("curven: not enough points in geometry matrix")

      do i = 1, n - 3
         call draw_curve(geom(i:i+3, 1:3))
      end do
   end subroutine draw_curven

end module m_draw_curves
