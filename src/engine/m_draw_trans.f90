module m_draw_trans
   use, intrinsic :: iso_fortran_env, only: real32
   use m_draw_types
   use m_draw_verror
   implicit none
   private
   public :: draw_translate, draw_rotate, draw_scale

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
!!    translate(3f) - [M_draw:TRANSFORMATION] Set up a translation.
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!       subroutine translate(x, y, z)
!!       real x, y, z
!!
!!##DESCRIPTION
!!
!!    Set up a translation.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_translate
!!    !
!!    !      a demonstration of objects
!!    !
!!    use M_draw
!!    implicit none
!!
!!    integer SPHERE
!!    real RADIUS
!!    parameter (RADIUS = 10.0)
!!    parameter(SPHERE = 1)
!!    character(len=50) :: device
!!    integer           :: ios, idum
!!
!!    print*,'Enter output device:'
!!    read(*,'(a)',iostat=ios) device
!!    if(ios.ne.0)device=' '
!!
!!    call vinit(device)
!!
!!    call vsetflush(.false.)
!!    !
!!    ! set up our viewing transformation
!!    !
!!    call perspective(90.0, 1.0, 0.001, 500.0)
!!    call lookat(13.0, 13.0, 8.0, 0.0, 0.0, 0.0, 0.0)
!!
!!    call color(D_BLACK)
!!    call clear()
!!    !
!!    ! Call a routine to make the sphere object
!!    !
!!    call makesphere()
!!
!!    !
!!    ! Now draw the sphere object scaled down. We use the pushmatrix
!!    ! and the popmatrix to preserve the transformation matrix so
!!    ! that only this sphere is drawn scaled. The callobj then enables
!!    ! us to draw the sphere we generated with makeobj in makesphere.
!!    !
!!    call color(D_CYAN)
!!
!!    call pushmatrix()
!!    call scale(0.5, 0.5, 0.5)
!!    call callobj(SPHERE)
!!    call popmatrix()
!!    !
!!    ! now we draw the same sphere translated, with a different
!!    ! scale and color.
!!    !
!!    call color(D_WHITE)
!!
!!    call pushmatrix()
!!    call translate(0.0, (-1.4) * RADIUS, 1.4 * RADIUS)
!!    call scale(0.3, 0.3, 0.3)
!!    call callobj(SPHERE)
!!    call popmatrix()
!!    !
!!    ! and maybe a few more times....
!!    !
!!    call color(D_RED)
!!
!!    call pushmatrix()
!!    call translate(0.0, RADIUS, 0.7 * RADIUS)
!!    call scale(0.2, 0.2, 0.2)
!!    call callobj(SPHERE)
!!    call popmatrix()
!!
!!    call color(D_GREEN)
!!
!!    call pushmatrix()
!!    call translate(0.0, 1.5 * RADIUS, -RADIUS)
!!    call scale(0.15, 0.15, 0.15)
!!    call callobj(SPHERE)
!!    call popmatrix()
!!
!!    call color(D_YELLOW)
!!
!!    call pushmatrix()
!!    call translate(0.0, -RADIUS, -RADIUS)
!!    call scale(0.12, 0.12, 0.12)
!!    call callobj(SPHERE)
!!    call popmatrix()
!!
!!    call color(D_BLUE)
!!
!!    call pushmatrix()
!!    call translate(0.0, (-2.0)*RADIUS, -RADIUS)
!!    call scale(0.3, 0.3, 0.3)
!!    call callobj(SPHERE)
!!    call popmatrix()
!!
!!    idum=getkey()
!!
!!    call vexit()
!!    contains
!!    subroutine makesphere
!!    !
!!    !        make a sphere object
!!    !
!!    integer SPHERE
!!    integer ii
!!    integer ia
!!    real i, r, z, a, RADIUS, PI
!!    parameter (PI = 3.1415926535, RADIUS = 10.0, SPHERE = 1)
!!
!!    call makeobj(SPHERE)
!!    !
!!    ! create the latitudinal rings
!!    !
!!    do ii = 0, 180, 20
!!       call pushmatrix()
!!       i=real(ii)
!!       call rotate(i, 'y')
!!       call circle(0.0, 0.0, RADIUS)
!!       call popmatrix()
!!    enddo
!!    !
!!    ! create the longitudinal rings
!!    !
!!    call pushmatrix()
!!    call rotate(90.0, 'x')
!!    do ia = -90, 90, 20
!!       a=ia
!!       r = RADIUS * cos(a * PI / 180.0)
!!       z = RADIUS * sin(a * PI / 180.0)
!!       call pushmatrix()
!!       call translate(0.0, 0.0, -z)
!!       call circle(0.0, 0.0, r)
!!       call popmatrix()
!!    enddo
!!    call popmatrix()
!!
!!    call closeobj()
!!
!!    end subroutine makesphere
!!
!!    end program demo_translate
   subroutine draw_translate(x, y, z)
      real(real32), intent(in) :: x, y, z
      type(Token), pointer :: tok(:)
      integer :: j

      if (vdevice%initialized == 0) then
         call draw_verror("translate: draw not initialized")
      end if

      if (vdevice%inobject /= 0) then
         tok => draw_newtokens(4)
         tok(1)%i = OBJ_TRANSLATE
         tok(2)%f = x
         tok(3)%f = y
         tok(4)%f = z
         return
      end if

      do j = 1, 4
         vdevice%transmat%m(4, j) = vdevice%transmat%m(4, j) + &
            x * vdevice%transmat%m(1, j) + &
            y * vdevice%transmat%m(2, j) + &
            z * vdevice%transmat%m(3, j)
      end do
   end subroutine draw_translate

!>
!!##NAME
!!    rotate(3f) - [M_draw:TRANSFORMATION] Set up a rotation in axis axis where axis is one of 'x','y', or 'z'.
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!          subroutine rotate(angle, axis)
!!          real angle
!!          character axis
!!##DESCRIPTION
!!
!!    Set up a rotation of coordinate system along specified axis, relative
!!    to current coordinate system.
!!
!!##OPTIONS
!!    angle  Angle in degrees to rotate coordinate system, with clockwise
!!           angles being positive.
!!
!!    axis   Which axis to perform rotation about. Allowed values are 'x',
!!           'y', and 'z'.
!!
!!##EXAMPLE
!!
!!   Sample usage
!!
!!       program demo_rotate
!!       use M_draw
!!       implicit none
!!       real :: x, y, size
!!       integer :: idum
!!
!!       ! set up display
!!          call prefsize(300, 300)
!!          call prefposition(200, 10)
!!          call vinit('X11')
!!
!!          SIZE = 1.2
!!          X = -0.75
!!          Y = 0.75
!!          call ortho2(-SIZE, SIZE, -SIZE, SIZE)
!!          call color(3)
!!          call clear()
!!       ! create an object to repeatedly draw
!!          call makeobj(1)
!!            call polyfill(.true.)
!!            call color(1)
!!            call rect(0.0, 0.0, X, Y)
!!            call polyfill(.false.)
!!            call linewidth(200)
!!            call color(2)
!!            call rect(0.0, 0.0, X, Y)
!!          call closeobj()
!!       ! draw object, rotating coordinate system between instantiations
!!          call callobj(1)
!!          call rotate(45.0, 'z')
!!          call callobj(1)
!!          call rotate(45.0, 'z')
!!          call callobj(1)
!!          call circle(0.0, 0.0, X/3)
!!          idum = getkey()
!!          call vexit()
!!
!!       end program demo_rotate
!!
!!##SEE ALSO
!!     translate, pushmatrix, popmatrix, scale
   subroutine draw_rotate(r, axis)
      real(real32), intent(in) :: r
      character(len=1), intent(in) :: axis
      type(Token), pointer :: tok(:)
      real(real32) :: costheta, sintheta, tmp
      integer :: j

      if (vdevice%initialized == 0) then
         call draw_verror("rotate: draw not initialized")
      end if

      if (vdevice%inobject /= 0) then
         tok => draw_newtokens(3)
         tok(1)%i = OBJ_ROTATE
         tok(2)%f = r
         tok(3)%i = ichar(axis)
         return
      end if

      costheta = cos(D2R * r)
      sintheta = sin(D2R * r)

      select case (axis)
      case ('x', 'X')
         do j = 1, 4
            tmp = vdevice%transmat%m(2, j)
            vdevice%transmat%m(2, j) = costheta * tmp + sintheta * vdevice%transmat%m(3, j)
            vdevice%transmat%m(3, j) = costheta * vdevice%transmat%m(3, j) - sintheta * tmp
         end do
      case ('y', 'Y')
         do j = 1, 4
            tmp = vdevice%transmat%m(1, j)
            vdevice%transmat%m(1, j) = costheta * tmp - sintheta * vdevice%transmat%m(3, j)
            vdevice%transmat%m(3, j) = sintheta * tmp + costheta * vdevice%transmat%m(3, j)
         end do
      case ('z', 'Z')
         do j = 1, 4
            tmp = vdevice%transmat%m(1, j)
            vdevice%transmat%m(1, j) = costheta * tmp + sintheta * vdevice%transmat%m(2, j)
            vdevice%transmat%m(2, j) = costheta * vdevice%transmat%m(2, j) - sintheta * tmp
         end do
      case default
         call draw_verror("rotate: illegal axis of rotation")
      end select
   end subroutine draw_rotate

!>
!!##NAME
!!    scale(3f) - [M_draw:TRANSFORMATION] Set up scaling factors in x, y, and z axis.
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine scale(x, y, z)
!!
!!     real,intent(in) ::  x, y, z
!!##DESCRIPTION
!!    Set up scaling factors in x, y, and z axis. The scaling is applied relative
!!    to the current transformation matrix; ie. the scaling is accumulative.
!!
!!##OPTIONS
!!    x  scaling factor to apply in X direction to current transformation matrix
!!    y  scaling factor to apply in Y direction to current transformation matrix
!!    x  scaling factor to apply in Z direction to current transformation matrix
!!##EXAMPLE
!!
!!
!!  Sample program
!!
!!       program demo_scale
!!       use M_draw
!!       implicit none
!!       real :: size, x, y
!!       integer :: idum
!!       ! set up display
!!          call prefsize(300, 300)
!!          call prefposition(200, 10)
!!          call vinit('X11')
!!          SIZE = 1.2
!!          X = -0.75
!!          Y = 0.75
!!          call color(3)
!!          call clear()
!!          call color(2)
!!          call ortho2(-SIZE, SIZE, -SIZE, SIZE)
!!       ! create an object to repeatedly draw
!!          call makeobj(1)
!!            call polyfill(.true.)
!!            call color(1)
!!            call rect(0.0, 0.0, X, Y)
!!            call polyfill(.false.)
!!            call linewidth(200)
!!            call color(2)
!!            call rect(0.0, 0.0, X, Y)
!!          call closeobj()
!!       ! draw object, scaling coordinate system between instantiations
!!          call pushmatrix()
!!            call scale(1.1, 2.0, 0.0)
!!            call callobj(1)
!!            ! scaling accumulates
!!            call scale(0.5, 0.5, 0.0)
!!            call callobj(1)
!!            ! circles appear as ellipses in this coordinate system
!!            call circle(0.0, 0.0, X/3.0)
!!          ! return back to saved coordinate system
!!          call popmatrix()
!!          ! now a circle is a circle again
!!          call color(5)
!!          call circle(0.0, 0.0, X/3.0)
!!          idum = getkey()
!!          call vexit()
!!       end program demo_scale
!!
!!##SEE ALSO
!!     rotate, translate, pushmatrix, popmatrix
    subroutine draw_scale(x, y, z)
       real(real32), intent(in) :: x, y, z
       type(Token), pointer :: tok(:)

       if (vdevice%initialized == 0) call draw_verror("scale: draw not initialized")

       if (vdevice%inobject /= 0) then
          tok => draw_newtokens(4)
          tok(1)%i = OBJ_SCALE
          tok(2)%f = x
          tok(3)%f = y
          tok(4)%f = z
          return
       end if

       vdevice%transmat%m(1, 1:4) = vdevice%transmat%m(1, 1:4) * x
       vdevice%transmat%m(2, 1:4) = vdevice%transmat%m(2, 1:4) * y
       vdevice%transmat%m(3, 1:4) = vdevice%transmat%m(3, 1:4) * z
    end subroutine draw_scale

end module m_draw_trans
