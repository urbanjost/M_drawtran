module m_draw_viewing
   use, intrinsic :: iso_fortran_env, only: real32
   use m_draw_types
   use m_draw_verror
   use m_draw_matrix
   use m_draw_trans
   implicit none
   private
   public :: draw_polarview, draw_up, draw_lookat, draw_perspective, &
             draw_window, draw_ortho, draw_ortho2

   real(real32), save :: Vup_x = 0.0_real32, Vup_y = 1.0_real32, Vup_z = 0.0_real32

contains
!>
!!##NAME
!!    polarview(3f) - [M_draw:VIEWPORT] Specify the viewer's position in polar coordinates
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!          subroutine polarview(dist, azim, inc, twist)
!!          real dist, azim, inc, twist
!!##DESCRIPTION
!!
!! Specify the viewer's position in polar coordinates by giving the distance
!! from the viewpoint to the world origin, the azimuthal angle in the x-y
!! plane, measured from the y-axis, the incidence angle in the y-z plane,
!! measured from the z-axis, and the twist angle about the line of sight.

   subroutine draw_polarview(dist, azim, inc, twist)
      real(real32), intent(in) :: dist, azim, inc, twist

      if (vdevice%initialized == 0) call draw_verror("polarview: draw not initialized")

      call draw_translate(0.0_real32, 0.0_real32, -dist)
      call draw_rotate(-twist, 'z')
      call draw_rotate(-inc, 'x')
      call draw_rotate(-azim, 'z')
   end subroutine draw_polarview

!>
!!##NAME
!!    up(3f) - [M_draw:VIEWPORT] Specify the world up.
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!          subroutine up(x, y, z)
!!          real x, y, z
!!
!!##DESCRIPTION
!! Specify the world up. This can be used to prevent lookat's sometimes
!! annoying habit of turning everything upside down due to the line of
!! sight crossing the appropriate axis.
   subroutine draw_up(x, y, z)
      real(real32), intent(in) :: x, y, z
      Vup_x = x
      Vup_y = y
      Vup_z = z
   end subroutine draw_up

!>
!!##NAME
!!    lookat(3f) - [M_draw:VIEWPORT] Specify the viewer's position
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!          subroutine lookat(vx, vy, vz, px, py, pz, twist)
!!          real vx, vy, vz, px, py, pz, twist
!!
!!##DESCRIPTION
!!    Specify the viewer's position by giving a viewpoint and a reference
!!    point in world coordinates. A twist about the line of sight may also
!!    be given.
!!
!!##EXAMPLE
!!
!!  Sample program:
!!
!!      program demo_lookat
!!      ! Demonstrate a rotating translating tetrahedron, and doublebuffering
!!      use M_draw
!!      implicit none
!!      !
!!      integer TETRAHEDRON
!!      parameter (TETRAHEDRON = 1)
!!      !
!!      real R, tx, tz, rotval, drotval, zeye
!!      integer i
!!      logical back, backdir, fill
!!      character(len=50) :: device
!!      integer :: ios
!!      !
!!      call prefsize(300, 300)
!!      !
!!      print*,'Enter output device:'
!!      read(*,'(a)',iostat=ios)device
!!      if(ios.ne.0)device=' '
!!      !
!!      back = .true.
!!      backdir = .true.
!!      fill = .true.
!!      !
!!      call vinit(device)
!!      !
!!      ! Make the tetrahedral object
!!      !
!!      call maketheobject()
!!      !
!!      rotval = 0.0
!!      drotval = 5.0
!!      zeye = 5.0
!!      !
!!      R = 1.6
!!      !
!!      tx = 0.0
!!      tz = R
!!      !
!!      !all polyfill(fill)
!!      call backface(back)
!!      call backfacedir(backdir)
!!      call clipping(.false.)
!!      !
!!      ! set up a perspective projection with a field of view of
!!      ! 40.0 degrees, aspect ratio of 1.0, near clipping plane 0.1,
!!      ! and the far clipping plane at 1000.0.
!!      !
!!      call perspective(40.0, 1.0, 0.001, 15.0)
!!      call lookat(0.0, 0.0, zeye, 0.0, 0.0, 0.0, 0.0)
!!      !
!!      ! Setup drawing into the backbuffer....
!!      !
!!      if (backbuffer().lt.0) then
!!         call vexit()
!!         write(*,*)'Device can''t support doublebuffering'
!!         stop
!!      endif
!!      !
!!      ! we loop back here ad-nauseam until someone hits a non-interpreted key
!!      !
!!      INFINITE: do
!!      !
!!         rotval = 0.0
!!      !
!!         do i = 0, int(359.0 / drotval)
!!      !
!!            call color(D_BLACK)
!!            call clear()
!!      !
!!      !  Rotate the whole scene...(this accumulates - hence
!!      !  drotval)
!!      !
!!            call rotate(drotval * 0.1, 'x')
!!            call rotate(drotval * 0.1, 'z')
!!      !
!!            call color(D_RED)
!!            call pushmatrix()
!!            call polyfill(.false.)
!!            call rotate(90.0, 'x')
!!            call circle(0.0, 0.0, R)
!!            call polyfill(fill)
!!            call popmatrix()
!!      !
!!            call color(D_BLUE)
!!            call move(0.0, 0.0, 0.0)
!!            call draw(tx, 0.0, tz)
!!      !
!!      ! Remember! The order of the transformations is
!!      ! the reverse of what is specified here in between
!!      ! the pushmatrix and the popmatrix. These ones don't
!!      ! accumulate because of the push and pop.
!!      !
!!            call pushmatrix()
!!               call translate(tx, 0.0, tz)
!!               call rotate(rotval, 'x')
!!               call rotate(rotval, 'y')
!!               call rotate(rotval, 'z')
!!               call scale(0.4, 0.4, 0.4)
!!               call callobj(TETRAHEDRON)
!!            call popmatrix()
!!      !
!!            tz = R * cos(rotval * 3.1415926535 / 180)
!!            tx = R * sin(rotval * 3.1415926535 / 180)
!!      !
!!            call swapbuffers()
!!      !
!!            select case(char(checkkey()))
!!            case('f')
!!                      fill = .not. fill
!!                      call polyfill(fill)
!!            case('b')
!!                      back = .not. back
!!                      call backface(back)
!!            case('d')
!!                      backdir = .not. backdir
!!                      call backfacedir(backdir)
!!            case(char(0))
!!            case default
!!                      call vexit()
!!                      stop
!!             end select
!!      !
!!            rotval = rotval + drotval
!!      !
!!            call microsleep(30000)
!!      !
!!      enddo
!!      !
!!      enddo INFINITE
!!      !
!!      contains
!!      !
!!      ! maketheobject
!!      !
!!      !       generate a tetrahedron object as a series of move draws
!!      !
!!      subroutine maketheobject()
!!
!!      integer TETRAHEDRON, NSIDES, NFACES, NPNTS
!!      parameter (TETRAHEDRON = 1, NSIDES = 3, NFACES = 4, NPNTS = 4)
!!      integer colface(NFACES)
!!      real pnts(3, NPNTS)
!!      integer faces(NSIDES, NFACES)
!!      integer i, j
!!      real x, y, z
!!
!!      data pnts/               &
!!           &  -0.5, 0.866, -0.667,     &
!!           &  -0.5, -0.866, -0.667,    &
!!           &   1.0, 0.0, -0.667,       &
!!           &   0.0, 0.0, 1.334/
!!
!!      data colface/D_GREEN, D_YELLOW, D_CYAN, D_MAGENTA/
!!
!!      data faces/   &
!!           &  3, 2, 1,      &
!!           &  1, 2, 4,      &
!!           &  2, 3, 4,      &
!!           &  3, 1, 4/
!!
!!      call makeobj(TETRAHEDRON)
!!
!!      do i = 1, NFACES
!!         call makepoly()
!!         call color(colface(i))
!!         x = pnts(1, faces(1, i))
!!         y = pnts(2, faces(1, i))
!!         z = pnts(3, faces(1, i))
!!         call move(x, y, z)
!!         do j = 2, NSIDES
!!            x = pnts(1, faces(j,i))
!!            y = pnts(2, faces(j,i))
!!            z = pnts(3, faces(j,i))
!!            call draw(x, y, z)
!!         enddo
!!         call closepoly()
!!       enddo
!!
!!       call closeobj()
!!
!!       end subroutine maketheobject
!!
!!       subroutine microsleep(waittime)
!!       use,intrinsic       :: iso_c_binding, only: c_int
!!       integer,intent(in)  :: waittime
!!       integer(kind=c_int) :: status
!!
!!       interface
!!          function c_usleep(seconds) bind (C,name="usleep")
!!             import
!!             ! should be unsigned int (not available in Fortran).
!!             ! OK until highest bit gets set.
!!             integer(c_int)       :: c_usleep
!!             integer(c_int), intent(in), VALUE :: seconds
!!          end function c_usleep
!!       end interface
!!
!!          if(waittime>0)then
!!             status=c_usleep(int(waittime,kind=c_int))
!!          endif
!!       end subroutine microsleep
!!
!!      end program demo_lookat
!!
   subroutine draw_lookat(vx, vy, vz, px, py, pz, twist)
      real(real32), intent(in) :: vx, vy, vz, px, py, pz, twist
      real(real32) :: dv, dx, dy, dz, mat(4,4)
      real(real32) :: rx, ry, rz, ux, uy, uz, rlen

      if (vdevice%initialized == 0) call draw_verror("lookat: draw not initialized")

      call draw_rotate(-twist, 'z')

      dx = px - vx
      dy = py - vy
      dz = pz - vz
      dv = sqrt(dx*dx + dy*dy + dz*dz)
      if (dv == 0.0_real32) call draw_verror("lookat: viewing point equals reference point")

      ! Forward vector (into screen is -Z in right-handed view coords)
      dx = dx / dv; dy = dy / dv; dz = dz / dv

      ! Right vector = forward x up
      rx = dy * Vup_z - dz * Vup_y
      ry = dz * Vup_x - dx * Vup_z
      rz = dx * Vup_y - dy * Vup_x
      rlen = sqrt(rx*rx + ry*ry + rz*rz)
      if (rlen == 0.0_real32) call draw_verror("lookat: up vector parallel to line of sight")
      rx = rx / rlen; ry = ry / rlen; rz = rz / rlen

      ! Recomputed orthogonal Up vector = right x forward
      ux = ry * dz - rz * dy
      uy = rz * dx - rx * dz
      uz = rx * dy - ry * dx

      call draw_identmatrix(mat)
      mat(1, 1) = rx;  mat(1, 2) = ux;  mat(1, 3) = -dx
      mat(2, 1) = ry;  mat(2, 2) = uy;  mat(2, 3) = -dy
      mat(3, 1) = rz;  mat(3, 2) = uz;  mat(3, 3) = -dz

      call draw_multmatrix(mat)
      call draw_translate(-vx, -vy, -vz)
   end subroutine draw_lookat

!>
!!##NAME
!!    perspective(3f) - [M_draw:PROJECTION] Specify perspective viewing pyramid
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!          subroutine perspective(fov, aspect, near, far)
!!          real fov, aspect, near, far
!!##DESCRIPTION
!!    Specify a perspective viewing pyramid in world coordinates by giving
!!    a field of view, aspect ratio and the distance from the eye of the
!!    near and far clipping plane.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_perspective
!!    !
!!    !     Shows various combinations of viewing and projection transformations
!!    use M_draw
!!    implicit none
!!    !
!!    character(len=50) :: device
!!    integer :: ios, idum
!!    !
!!       print*,'Enter output device:'
!!       read(*,'(a)',iostat=ios)device
!!       if(ios.ne.0)device=' '
!!    !
!!       call vinit(device)
!!    !
!!       call color(D_BLACK)
!!       call clear()
!!    !
!!    ! we want to draw just within the boundaries of the screen
!!    !
!!       call viewport(-0.9, 0.9, -0.9, 0.9)
!!    !
!!    ! set the world size
!!    !
!!       call ortho2(-5.0, 5.0, -5.0, 5.0)
!!    !
!!    ! draw a boundary frame
!!    !
!!       call color(D_RED)
!!       call rect(-5.0, -5.0, 5.0, 5.0)
!!    !
!!    ! set up a perspective projection with a field of view of
!!    ! 40.0 degrees, aspect ratio of 1.0, near clipping plane 0.1,
!!    ! and the far clipping plane at 1000.0.
!!    !
!!       call perspective(40.0, 1.0, 0.1, 1000.0)
!!    !
!!    ! we want the drawing to be done with our eye point at (5.0, 8.0, 5.0)
!!    ! looking towards (0.0, 0.0, 0.0). The last parameter gives a twist
!!    ! in degrees around the line of sight, in this case zero.
!!    !
!!       call lookat(5.0, 8.0, 5.0, 0.0, 0.0, 0.0, 0.0)
!!    !
!!       call drawtetra()
!!    !
!!    ! set the text size
!!    !
!!       call textsize(0.6, 0.9)
!!    !
!!       call move2(-4.5, -4.5)
!!       call drawstr('perspective/lookat')
!!    !
!!       idum=getkey()
!!    !
!!    ! window can also be used to give a perspective projection. Its
!!    ! arguments are 6 clipping planes, left, right, bottom, top, near,
!!    ! and far.
!!    !
!!       call window(-5.0, 5.0, -5.0, 5.0, -5.0, 5.0)
!!    !
!!    ! as window replaces the current transformation matrix we must
!!    ! specify our viewpoint again.
!!    !
!!       call lookat(5.0, 8.0, 5.0, 0.0, 0.0, 0.0, 0.0)
!!    !
!!       call color(D_BLACK)
!!       call clear()
!!    !
!!       call color(D_GREEN)
!!       call rect(-5.0, -5.0, 5.0, 5.0)
!!    !
!!       call drawtetra()
!!    !
!!       call textsize(0.6, 0.9)
!!       call move2(-4.5,-4.5)
!!       call drawstr('window/lookat')
!!    !
!!       idum=getkey()
!!    !
!!    ! set up our original perspective projection again.
!!    !
!!       call perspective(40.0, 1.0, 0.1, 1000.0)
!!    !
!!    ! polarview also specifies our viewpoint, but, unlike lookat, in polar
!!    ! coordinates. Its arguments are the distance from the world origin, an
!!    ! azimuthal angle in the x-y plane measured from the y axis, an
!!    ! incidence angle in the y-z plane measured from the z axis, and a
!!    ! twist around the line of sight.
!!    !
!!       call polarview(15.0, 30.0, 30.0, 30.0)
!!    !
!!       call color(D_BLACK)
!!       call clear()
!!    !
!!       call color(D_MAGENTA)
!!       call rect(-5.0, -5.0, 5.0, 5.0)
!!    !
!!       call drawtetra()
!!    !
!!       call move2(-4.5,-4.5)
!!       call textsize(0.6, 0.9)
!!       call drawstr('perspective/polarview')
!!    !
!!       idum=getkey()
!!    !
!!    ! once more with window for comparison
!!    !
!!       call window(-4.0, 4.0, -4.0, 4.0, -4.0, 4.0)
!!       call polarview(6.0, 20.0, -30.0, 70.0)
!!    !
!!       call color(D_BLACK)
!!       call clear()
!!    !
!!       call color(D_YELLOW)
!!       call rect(-5.0, -5.0, 5.0, 5.0)
!!    !
!!       call drawtetra()
!!    !
!!       call move2(-4.5,-4.5)
!!       call textsize(0.6, 0.9)
!!       call drawstr('window/polarview')
!!    !
!!       idum=getkey()
!!    !
!!       call vexit()
!!    !
!!    contains
!!    !
!!    subroutine drawtetra()
!!    !
!!    ! generate a tetrahedron as a series of move draws
!!    !
!!       call move(-0.5,  0.866, -0.5)
!!       call draw(-0.5, -0.866, -0.5)
!!       call draw( 1.0,  0.0,   -0.5)
!!       call draw(-0.5,  0.866, -0.5)
!!       call draw( 0.0,  0.0,    1.5)
!!       call draw(-0.5, -0.866, -0.5)
!!       call move( 1.0,  0.0,   -0.5)
!!       call draw( 0.0,  0.0,    1.5)
!!    !
!!    !    Label the vertices.
!!    !
!!       call color(D_WHITE)
!!       call textsize(0.3, 0.5)
!!       call move(-0.5,  0.866, -0.5)
!!       call drawchar('a')
!!       call move(-0.5, -0.866, -0.5)
!!       call drawchar('b')
!!       call move( 1.0,  0.0,   -0.5)
!!       call drawchar('c')
!!       call move( 0.0,  0.0,    1.5)
!!       call drawchar('d')
!!    !
!!    end subroutine drawtetra
!!    !
!!    end program demo_perspective
   subroutine draw_perspective(fov, aspect, hither, yon)
      real(real32), intent(in) :: fov, aspect, hither, yon
      real(real32) :: mat(4,4), cotfov

      if (vdevice%initialized == 0) call draw_verror("perspective: draw not initialized")
      if (fov <= 0.0_real32 .or. fov >= 180.0_real32) call draw_verror("perspective: bad fov")
      if (aspect == 0.0_real32) call draw_verror("perspective: zero aspect ratio")
      if ((yon - hither) == 0.0_real32) call draw_verror("perspective: hither == yon")

      cotfov = 1.0_real32 / tan(0.5_real32 * D2R * fov)

      mat = 0.0_real32
      mat(1, 1) = cotfov / aspect
      mat(2, 2) = cotfov
      mat(3, 3) = -(yon + hither) / (yon - hither)
      mat(3, 4) = -1.0_real32
      mat(4, 3) = -2.0_real32 * yon * hither / (yon - hither)

      call draw_loadmatrix(mat)
   end subroutine draw_perspective

!>
!!##NAME
!!    window(3f) - [M_draw:PROJECTION] Specify a perspective viewing pyramid
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!          subroutine window(left, right, bot, top, near, far)
!!          real left, right, bot, top, near, far
!!##DESCRIPTION
!!
!!    Specify a perspective viewing pyramid in world coordinates by giving
!!    the rectangle closest to the eye (ie. at the near clipping plane)
!!    and the distances to the near and far clipping planes.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_window
!!    use M_draw
!!    implicit none
!!    integer :: idum, ios
!!    integer,parameter :: &
!!      CUBE=1,       &
!!      TOPLEFT=2,    &
!!      TOPRIGHT=3,   &
!!      BOTTOMLEFT=4, &
!!      BOTTOMRIGHT=5
!!    character(len=20) :: device
!!    print*,'Enter device name:'
!!    read(*,'(A)',iostat=ios)device
!!    if(ios.ne.0)device=' '
!!    call vinit(device)
!!    call pushviewport
!!    call textsize(0.5, 0.9)
!!    call font('futura.m')
!!    call color(D_BLACK)
!!    call clear
!!    ! make an object that represents the cube
!!    call makecube
!!    ! set up an object which draws in the top left of the screen.
!!    call makeobj(TOPLEFT)
!!    call viewport(-1.0, 0.0, 0.0, 1.0)
!!    call ortho2(-5.0, 5.0, -5.0, 5.0)
!!    call color(D_RED)
!!    call rect(-5.0, -5.0, 5.0, 5.0)
!!    call perspective(40.0, 1.0, 0.1, 1000.0)
!!    call lookat(5.0, 8.0, 5.0, 0.0, 0.0, 0.0, 0.0)
!!    ! Call an object within another object
!!    call callobj(CUBE)
!!    call color(D_GREEN)
!!    call move2(-4.5, -4.5)
!!    call drawstr('perspective/lookat')
!!    call closeobj
!!    ! now set up one which draws in the top right of the screen
!!    call makeobj(TOPRIGHT)
!!    call viewport(0.0, 1.0, 0.0, 1.0)
!!    call ortho2(-5.0, 5.0, -5.0, 5.0)
!!    call color(D_GREEN)
!!    call rect(-5.0, -5.0, 5.0, 5.0)
!!    call window(-5.0, 5.0, -5.0, 5.0, -5.0, 5.0)
!!    call lookat(5.0, 8.0, 5.0, 0.0, 0.0, 0.0, 0.0)
!!    call callobj(CUBE)
!!    call color(D_RED)
!!    call move2(-4.5, -4.5)
!!    call drawstr('window/lookat')
!!    call closeobj
!!    ! try the bottom left
!!    call makeobj(BOTTOMLEFT)
!!    call viewport(-1.0, 0.0, -1.0, 0.0)
!!    call ortho2(-5.0, 5.0, -5.0, 5.0)
!!    call color(D_MAGENTA)
!!    call rect(-5.0, -5.0, 5.0, 5.0)
!!    call perspective(40.0, 1.0, 0.1, 1000.0)
!!    call polarview(15.0, 30.0, 30.0, 30.0)
!!    call callobj(CUBE)
!!    call color(D_YELLOW)
!!    call move2(-4.5, -4.5)
!!    call drawstr('perspective/polarview')
!!    call closeobj
!!    ! and the bottom right
!!    call makeobj(BOTTOMRIGHT)
!!    call viewport(0.0, 1.0, -1.0, 0.0)
!!    call ortho2(-5.0, 5.0, -5.0, 5.0)
!!    call color(D_CYAN)
!!    call rect(-5.0, -5.0, 5.0, 5.0)
!!    call window(-5.0, 5.0, -5.0, 5.0, -5.0, 5.0)
!!    call polarview(8.0, -18.0, -3.0, 18.0)
!!    call callobj(CUBE)
!!    call color(D_BLUE)
!!    call move2(-4.5, -4.5)
!!    call drawstr('window/polarview')
!!    call closeobj
!!    ! now draw them
!!    call callobj(TOPLEFT)
!!    call callobj(TOPRIGHT)
!!    call callobj(BOTTOMLEFT)
!!    call callobj(BOTTOMRIGHT)
!!    idum=getkey()
!!    call vexit
!!    !=====================================================================
!!    contains
!!    !=====================================================================
!!    subroutine makecube
!!    ! set up a cube
!!    integer CUBE
!!    parameter (CUBE = 1)
!!    call makeobj(CUBE)
!!    ! The border around the cube
!!    call rect(-5.0, -5.0, 10.0, 10.0)
!!    ! Make the cube from 4 squares
!!    call pushmatrix()
!!    call side()
!!    call rotate(90.0, 'x')
!!    call side()
!!    call rotate(90.0, 'x')
!!    call side()
!!    call rotate(90.0, 'x')
!!    call side()
!!    call popmatrix()
!!    call closeobj()
!!    end subroutine makecube
!!    !=====================================================================
!!    subroutine side
!!    ! define a face for the cube
!!    call pushmatrix
!!    call translate(0.0, 0.0, 1.0)
!!    call rect(-1.0, -1.0, 1.0, 1.0)
!!    call popmatrix
!!    end subroutine side
!!    !=====================================================================
!!    end program demo_window
   subroutine draw_window(left, right, bottom, top, hither, yon)
      real(real32), intent(in) :: left, right, bottom, top, hither, yon
      real(real32) :: mat(4,4)

      if (vdevice%initialized == 0) call draw_verror("window: draw not initialized")
      if ((right - left) == 0.0_real32) call draw_verror("window: left == right")
      if ((top - bottom) == 0.0_real32) call draw_verror("window: bottom == top")
      if ((yon - hither) == 0.0_real32) call draw_verror("window: hither == yon")

      mat = 0.0_real32
      mat(1, 1) = 2.0_real32 * hither / (right - left)
      mat(2, 2) = 2.0_real32 * hither / (top - bottom)
      mat(3, 1) = (right + left) / (right - left)
      mat(3, 2) = (top + bottom) / (top - bottom)
      mat(3, 3) = -(yon + hither) / (yon - hither)
      mat(3, 4) = -1.0_real32
      mat(4, 3) = -2.0_real32 * yon * hither / (yon - hither)

      call draw_loadmatrix(mat)
   end subroutine draw_window

!>
!!##NAME
!!    ortho(3f) - [M_draw:PROJECTION] Define x,y,z clipping planes.
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!          subroutine ortho(left, right, bottom, top, near_d, far_d)
!!          real left, right, bottom, top, near_d, far_d
!!##DESCRIPTION
!!
!!    Define x (left, right), y (bottom, top), and z (near, far) clipping
!!    planes. The near and far clipping planes are actually specified
!!    as distances along the line of sight. These distances can also be
!!    negative. The actual location of the clipping planes is z = -near_d
!!    and z = -far_d.
   subroutine draw_ortho(left, right, bottom, top, hither, yon)
      real(real32), intent(in) :: left, right, bottom, top, hither, yon
      real(real32) :: mat(4,4)

      if (vdevice%initialized == 0) call draw_verror("ortho: draw not initialized")
      if ((right - left) == 0.0_real32) call draw_verror("ortho: left == right")
      if ((top - bottom) == 0.0_real32) call draw_verror("ortho: bottom == top")
      if ((yon - hither) == 0.0_real32) call draw_verror("ortho: hither == yon")

      call draw_identmatrix(mat)
      mat(1, 1) = 2.0_real32 / (right - left)
      mat(2, 2) = 2.0_real32 / (top - bottom)
      mat(3, 3) = -2.0_real32 / (yon - hither)
      mat(4, 1) = -(right + left) / (right - left)
      mat(4, 2) = -(top + bottom) / (top - bottom)
      mat(4, 3) = -(yon + hither) / (yon - hither)

      call draw_loadmatrix(mat)
   end subroutine draw_ortho

!>
!!##NAME
!!    ortho2(3f) - [M_draw:PROJECTION] define the area of the virtual world coordinates to map to the viewport
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!          subroutine ortho2(left, right, bottom, top)
!!          real,intent(in) :: left, right, bottom, top
!!##DESCRIPTION
!!
!!    Defines the section of the virtual world coordinates to map to
!!    the viewport. That is, Define x (left, right), and y (bottom, top)
!!    clipping planes.
!!
!!    All the projection routines define a new transformation
!!    matrix, and consequently the world units. Parallel projections are
!!    defined by ortho2.
   subroutine draw_ortho2(left, right, bottom, top)
      real(real32), intent(in) :: left, right, bottom, top
      call draw_ortho(left, right, bottom, top, -1.0_real32, 1.0_real32)
   end subroutine draw_ortho2

end module m_draw_viewing
