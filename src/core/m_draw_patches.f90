module m_draw_patches
   use, intrinsic :: iso_fortran_env, only: real32
   use m_draw_types
   use m_draw_verror
   use m_draw_matrix
   use m_draw_tensor
   use m_draw_curves
   implicit none
   private
   public :: draw_patchbasis, draw_patchprecision, draw_patchcurves, &
             draw_patch, draw_rpatch, draw_transformtensor, draw_drpatch

   integer, save :: ntsegs = 10, tsegs = 10, nusegs = 10, usegs = 10
   integer, save :: ntcurves = 10, nucurves = 10
   integer, save :: ntiter = 1, nuiter = 1
   logical, save :: makeprec_called = .false.

   real(real32), save :: tbasis(4,4) = 0.0_real32, ubasis(4,4) = 0.0_real32
   real(real32), save :: et(4,4) = 0.0_real32, eu(4,4) = 0.0_real32
   real(real32), parameter :: ones(4,4) = 1.0_real32

   interface
      function draw_newtokens(num) result(toks)
         import :: Token
         integer, intent(in) :: num
         type(Token), pointer :: toks(:)
      end function draw_newtokens
   end interface

contains

   subroutine draw_patchbasis(tb, ub)
      real(real32), intent(in) :: tb(4,4), ub(4,4)
      if (vdevice%initialized == 0) call draw_verror("patchbasis: draw not initialized")
      call draw_copymatrix(tbasis, tb)
      call draw_copytranspose(ubasis, ub)
   end subroutine draw_patchbasis

   subroutine draw_patchprecision(tseg, useg)
      integer, intent(in) :: tseg, useg
      if (vdevice%initialized == 0) call draw_verror("patchprecision: draw not initialized")
      if (tseg > 0 .and. useg > 0) then
         tsegs = tseg
         usegs = useg
      else
         call draw_verror("patchprecision: number of segments <= 0")
      end if
      call draw_makeprec()
   end subroutine draw_patchprecision

   subroutine draw_patchcurves(nt, nu)
      integer, intent(in) :: nt, nu
      if (vdevice%initialized == 0) call draw_verror("patchcurves: draw not initialized")
      if (nt > 0 .and. nu > 0) then
         ntcurves = nt
         nucurves = nu
      else
         call draw_verror("patchcurves: number of patch curves <= 0")
      end if
      call draw_makeprec()
   end subroutine draw_patchcurves

   subroutine draw_makeprec()
      real(real32) :: n2, n3

      ntsegs = tsegs
      ntiter = ntsegs / (nucurves - 1)
      if (ntsegs > ntiter * (nucurves - 1)) then
         ntiter = ntiter + 1
         ntsegs = ntiter * (nucurves - 1)
      end if

      nusegs = usegs
      nuiter = nusegs / (ntcurves - 1)
      if (nusegs > nuiter * (ntcurves - 1)) then
         nuiter = nuiter + 1
         nusegs = nuiter * (ntcurves - 1)
      end if

      call draw_identmatrix(et)
      n2 = real(ntsegs * ntsegs, real32)
      n3 = real(ntsegs, real32) * n2

      et(1,1) = 0.0_real32; et(3,3) = 0.0_real32; et(4,4) = 0.0_real32
      et(2,1) = 1.0_real32 / n3
      et(2,2) = 1.0_real32 / n2
      et(3,1) = 6.0_real32 / n3
      et(4,1) = 6.0_real32 / n3
      et(3,2) = 2.0_real32 / n2
      et(2,3) = 1.0_real32 / real(ntsegs, real32)
      et(1,4) = 1.0_real32

      call draw_identmatrix(eu)
      n2 = real(nusegs * nusegs, real32)
      n3 = real(nusegs, real32) * n2

      eu(1,1) = 0.0_real32; eu(3,3) = 0.0_real32; eu(4,4) = 0.0_real32
      eu(1,2) = 1.0_real32 / n3
      eu(2,2) = 1.0_real32 / n2
      eu(1,3) = 6.0_real32 / n3
      eu(1,4) = 6.0_real32 / n3
      eu(2,3) = 2.0_real32 / n2
      eu(3,2) = 1.0_real32 / real(nusegs, real32)
      eu(4,1) = 1.0_real32

      makeprec_called = .true.
   end subroutine draw_makeprec

   subroutine draw_patch(geomx, geomy, geomz)
      real(real32), intent(in) :: geomx(4,4), geomy(4,4), geomz(4,4)
      call draw_rpatch(geomx, geomy, geomz, ones)
   end subroutine draw_patch

   subroutine draw_rpatch(geomx, geomy, geomz, geomw)
      real(real32), intent(in) :: geomx(4,4), geomy(4,4), geomz(4,4), geomw(4,4)
      real(real32) :: S(4,4,4), R(4,4,4)
      real(real32) :: tmp(4,4), tmp2(4,4)
      real(real32) :: xlast, ylast, zlast
      integer :: i, j, k, sync, idum
      type(Token), pointer :: tok(:)

      if (vdevice%initialized == 0) call draw_verror("patch: draw not initialized")
      if (.not. makeprec_called) call draw_makeprec()

      call draw_mult4x4(tmp, et, tbasis)
      call draw_mult4x4(tmp2, ubasis, eu)

      S(1, 1:4, 1:4) = geomx(1:4, 1:4)
      S(2, 1:4, 1:4) = geomy(1:4, 1:4)
      S(3, 1:4, 1:4) = geomz(1:4, 1:4)
      S(4, 1:4, 1:4) = geomw(1:4, 1:4)

      call draw_premulttensor(R, tbasis, S)
      call draw_multtensor(S, ubasis, R)

      xlast = sum(S(1, 1:4, 1:4))
      ylast = sum(S(2, 1:4, 1:4))
      zlast = sum(S(3, 1:4, 1:4))

      call draw_premulttensor(R, et, S)
      call draw_multtensor(S, eu, R)

      if (vdevice%inobject /= 0) then
         tok => draw_newtokens(74)
         tok(1)%i = OBJ_RPATCH
         tok(2)%f = xlast
         tok(3)%f = ylast
         tok(4)%f = zlast
         tok(5)%i = ntcurves
         tok(6)%i = nucurves
         tok(7)%i = ntsegs
         tok(8)%i = nusegs
         tok(9)%i = ntiter
         tok(10)%i = nuiter

         k = 11
         do i = 1, 4
            do j = 1, 4
               tok(k)%f   = S(1, i, j)
               tok(k+1)%f = S(2, i, j)
               tok(k+2)%f = S(3, i, j)
               tok(k+3)%f = S(4, i, j)
               k = k + 4
            end do
         end do
         return
      end if

      call draw_transformtensor(S, vdevice%transmat%m)

      sync = vdevice%sync
      if (sync /= 0) vdevice%sync = 0

      call draw_drpatch(S, ntcurves, nucurves, ntsegs, nusegs, ntiter, nuiter)

      if (sync /= 0) then
         vdevice%sync = 1
         idum = vdevice%dev%Vsync()
      end if

      vdevice%cpW(D_X) = xlast
      vdevice%cpW(D_Y) = ylast
      vdevice%cpW(D_Z) = zlast
   end subroutine draw_rpatch

   subroutine draw_transformtensor(S, m)
      real(real32), intent(inout) :: S(4,4,4)
      real(real32), intent(in) :: m(4,4)
      real(real32) :: tmp(4,4), tmp2(4,4)
      integer :: i

      do i = 1, 4
         call draw_extract(tmp, S, i)
         call draw_mult4x4(tmp2, tmp, m)
         call draw_replace(S, tmp2, i)
      end do
   end subroutine draw_transformtensor

   subroutine draw_replace(a, b, k)
      real(real32), intent(inout) :: a(4,4,4)
      real(real32), intent(in) :: b(4,4)
      integer, intent(in) :: k
      integer :: i, j

      do i = 1, 4
         do j = 1, 4
            a(j, i, k) = b(i, j)
         end do
      end do
   end subroutine draw_replace

   subroutine draw_drpatch(R, ntc, nuc, nts, nus, nti, nui)
      real(real32), intent(inout) :: R(4,4,4)
      integer, intent(in) :: ntc, nuc, nts, nus, nti, nui
      real(real32) :: S(4,4,4), tmp(4,4)
      integer :: i

      call draw_copytensortrans(S, R)

      do i = 1, ntc
         call draw_extract(tmp, R, 1)
         call draw_drcurve(nts, tmp)
         call draw_iterate(R, nui)
      end do

      do i = 1, nuc
         call draw_extract(tmp, S, 1)
         call draw_drcurve(nus, tmp)
         call draw_iterate(S, nti)
      end do
   end subroutine draw_drpatch

   subroutine draw_iterate(R, n)
      real(real32), intent(inout) :: R(4,4,4)
      integer, intent(in) :: n
      integer :: it, i, j, k

      do it = 1, n
         do i = 1, 4
            do j = 1, 4
               do k = 1, 3
                  R(i, j, k) = R(i, j, k) + R(i, j, k+1)
               end do
            end do
         end do
      end do
   end subroutine draw_iterate

   subroutine draw_extract(b, a, k)
      real(real32), intent(out) :: b(4,4)
      real(real32), intent(in) :: a(4,4,4)
      integer, intent(in) :: k
      integer :: i, j

      do i = 1, 4
         do j = 1, 4
            b(i, j) = a(j, i, k)
         end do
      end do
   end subroutine draw_extract

end module m_draw_patches
