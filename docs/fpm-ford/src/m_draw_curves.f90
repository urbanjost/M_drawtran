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
!>
!!##NAME
!!    curvebasis(3f) - [M_draw:CURVE] Define a basis matrix for a curve.
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!          subroutine curvebasis(basis)
!!          real,intent(in) :: basis(4,4)
!!##DESCRIPTION
!!    Define a basis matrix for a curve.
   subroutine draw_curvebasis(basis)
      real(real32), intent(in) :: basis(4,4)
      ! Note: If m_draw_patches calls draw_curvebasis with an already-transposed
      ! matrix, use draw_copymatrix here and call draw_copytranspose in the
      ! public curvebasis() wrapper instead.
      call draw_copytranspose(basismatrix, basis)
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

!>
!!##NAME
!!    curveprecision(3f) - [M_draw:CURVE] Define number of line segments used to draw a curve.
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!          subroutine curveprecision(nsegs)
!!          integer nsegs
!!##DESCRIPTION
!!    Define the number of line segments used to draw a curve.
!>
!!##NAME
!!    curveprecision(3f) - [M_draw:CURVE] Define number of line segments used to draw a curve.
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!          subroutine curveprecision(nsegs)
!!          integer nsegs
!!##DESCRIPTION
!!    Define the number of line segments used to draw a curve.
   subroutine draw_curveprecision(nsegments)
      integer, intent(in) :: nsegments
      if (nsegments > 0) then
         nsegs = nsegments
      else
         call draw_verror("curveprecision: number of segments <= 0")
      end if
      call draw_make_prec()
   end subroutine draw_curveprecision

!>
!!##NAME
!!    rcurve(3f) - [M_draw:CURVE] Draw a rational curve.
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!          subroutine rcurve(geom)
!!          real geom(4,4)
!!
!!##DESCRIPTION
!!    Draw a rational curve.
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

!>
!!##NAME
!!    curve(3f) - [M_draw:CURVE] Draw a curve.
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine curve(geom)
!!    real geom(3,4)
!!
!!##DESCRIPTION
!!    Draw a curve.
!!
!!##EXAMPLE
!!
!!   Sample Program:
!!
!!    program demo_curve
!!    !
!!    !      using curves
!!    !
!!    use M_draw
!!    implicit none
!!
!!    integer i, idum, ios
!!    character(len=50) :: buf
!!    real bezier(4, 4), cardinal(4, 4), bspline(4, 4)
!!    real geom1(3, 4), geom2(3, 6)
!!    !
!!    ! curve basis types
!!    !
!!    data bezier /                                            &
!!    &          -1.0,   3.0,    -3.0,   1.0,                  &
!!    &          3.0,    -6.0,   3.0,    0.0,                  &
!!    &          -3.0,   3.0,    0.0,    0.0,                  &
!!    &          1.0,    0.0,    0.0,    0.0                   &
!!    &  /
!!
!!    data cardinal /                                          &
!!    &          -0.5,   1.5,    -1.5,   0.5,                  &
!!    &          1.0,    -2.5,   2.0,    -0.5,                 &
!!    &          -0.5,   0.0,    0.5,    0.0,                  &
!!    &          0.0,    1.0,    0.0,    0.0                   &
!!    &  /
!!
!!    data bspline /                                           &
!!    &          -0.166666,     0.5,     -0.5,     0.166666,   &
!!    &           0.5,         -1.0,      0.5,     0.0,        &
!!    &          -0.5,          0.0,      0.5,     0.0,        &
!!    &           0.166666,     0.666666, 0.166666, 0.0        &
!!    &  /
!!
!!    !
!!    ! Geometry matrix to demonstrate basic spline segments
!!    !
!!    data geom1 /                      &
!!    &           -180.0, 10.0, 0.0,    &
!!    &           -100.0, 110.0, 0.0,   &
!!    &           -100.0, -90.0, 0.0,   &
!!    &           0.0, 50.0, 0.0        &
!!    &  /
!!
!!    !
!!    ! Geometry matrix to demonstrate overlapping control points to
!!    ! produce continuous (Well, except for the bezier ones) curves
!!    ! from spline segments
!!    !
!!    data geom2 /                      &
!!    &          200.0, 480.0, 0.0,     &
!!    &          380.0, 180.0, 0.0,     &
!!    &          250.0, 430.0, 0.0,     &
!!    &          100.0, 130.0, 0.0,     &
!!    &          50.0,  280.0, 0.0,     &
!!    &          150.0, 380.0, 0.0      &
!!    &  /
!!
!!
!!    print*,'Enter output device:'
!!    read(*,'(a)',iostat=ios) buf
!!    if(ios.ne.0)buf=' '
!!
!!    call vinit(buf)
!!
!!    call ortho2(-200.0, 400.0, -100.0, 500.0)
!!
!!    call color(D_BLACK)
!!    call clear()
!!
!!    call color(D_YELLOW)
!!
!!    call textsize(10.0, 10.0)
!!    !
!!    ! label the control points in geom1
!!    !
!!    do i = 1, 4
!!       call move2(geom1(1, i), geom1(2, i))
!!       write(buf, '(i1)')i
!!       call drawstr(buf)
!!    enddo
!!    !
!!    ! label the control points in geom2
!!    !
!!    do i = 1, 6
!!       call move2(geom2(1, i), geom2(2, i))
!!       write(buf, '(i1)')i
!!       call drawstr(buf)
!!    enddo
!!    !
!!    ! scale the current font so that 30 of the largest characters
!!    ! in the current font will fit in a region 300 world units wide,
!!    ! 20 high.
!!    !
!!    call boxfit(300.0, 20.0, 30)
!!
!!    !
!!    ! set the number of line segments appearing in each curve to 20
!!    !
!!    call curveprecision(20)
!!
!!    !
!!    ! copy the bezier basis matrix into the curve basis matrix.
!!    !
!!    call curvebasis(bezier)
!!
!!    call color(D_RED)
!!
!!    !
!!    ! draw a curve using the current basis matrix (bezier in this case)
!!    ! and the control points in geom1
!!    !
!!    call curve(geom1)
!!
!!    call move2(70.0, 60.0)
!!    call drawstr('Bezier Curve Segment')
!!
!!    call move2(-190.0, 450.0)
!!    call drawstr('Three overlapping Bezier Curves')
!!
!!    !
!!    ! curven draws overlapping curve segments according to geom2, the
!!    ! number of curve segments drawn is three less than the number of
!!    ! points passed, assuming there are a least four points in the
!!    ! geometry matrix (in this case geom2). This call will draw 3
!!    ! overlapping curve segments in the current basis matrix - still
!!    ! bezier.
!!    !
!!    call curven(6, geom2)
!!
!!    idum=getkey()
!!    !
!!    ! load in the cardinal basis matrix
!!    !
!!    call curvebasis(cardinal)
!!
!!    call color(D_MAGENTA)
!!
!!    call move2(70.0, 10.0)
!!    call drawstr('Cardinal Curve Segment')
!!    !
!!    ! plot out a curve segment using the cardinal basis matrix
!!    !
!!    call curve(geom1)
!!
!!    call move2(-190.0, 400.0)
!!    call drawstr('Three overlapping Cardinal Curves')
!!    !
!!    ! now draw a bunch of them again.
!!    !
!!    call curven(6, geom2)
!!
!!    idum=getkey()
!!    !
!!    ! change the basis matrix again
!!    !
!!    call curvebasis(bspline)
!!
!!    call color(D_GREEN)
!!
!!    call move2(70.0, -40.0)
!!    call drawstr('Bspline Curve Segment')
!!    !
!!    ! now draw our curve segment in the new basis...
!!    !
!!    call curve(geom1)
!!
!!    call move2(-190.0, 350.0)
!!    call drawstr('Three overlapping Bspline Curves')
!!    !
!!    ! ...and do some overlapping ones
!!    !
!!    call curven(6, geom2)
!!
!!    idum=getkey()
!!
!!    call vexit()
!!
!!    end program demo_curve
   subroutine draw_curve(geom)
      real(real32), intent(in) :: geom(3,4)
      real(real32) :: tmp(4,4)

      if (vdevice%initialized == 0) call draw_verror("curve: draw not initialized")

      tmp(1:4, 1:3) = transpose(geom(1:3, 1:4))
      tmp(1:4, 4)   = 1.0_real32

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
         end if
      end do

      vdevice%cpWtrans(1:4) = r(1, 1:4)

      if (sync /= 0) then
         vdevice%sync = 1
         idum = vdevice%dev%Vsync()
      end if
   end subroutine draw_drcurve

!>
!!##NAME
!!    curven(3f) - [M_draw:CURVE] Draw n-3 overlapping curve segments. Note: n must be at least 4.
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!          subroutine curven(n, geom)
!!          integer n
!!          real geom(3,n)
!!
!!##DESCRIPTION
!!    Draw n-3 overlapping curve segments. Note: n must be at least 4.
   subroutine draw_curven(n, geom)
      integer, intent(in) :: n
      real(real32), intent(in) :: geom(3, n)
      integer :: i

      if (vdevice%initialized == 0) call draw_verror("curven: draw not initialized")
      if (n < 4) call draw_verror("curven: not enough points in geometry matrix")

      do i = 1, n - 3
         call draw_curve(geom(1:3, i:i+3))
      end do
   end subroutine draw_curven

end module m_draw_curves
