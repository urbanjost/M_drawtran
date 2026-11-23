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

!>
!!##NAME
!!    backface(3f) - [M_draw:POLYGONS] Turns on culling of backfacing polygons.
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!       subroutine backface(onoff)
!!       logical onoff
!!
!!##DESCRIPTION
!!    Turns on culling of backfacing polygons. A polygon is backfacing if
!!    its orientation in *screen* coords is clockwise, unless a call
!!    to backfacedir is made.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_backface
!!    ! demonstrate double buffering and what happens
!!    ! when you hit clipping plane
!!    !
!!    ! Specifying an extra argument turns on the polygon filling.
!!    !
!!    use M_draw
!!    implicit none
!!    character(len=30) :: device
!!    character(len=1)  :: c
!!    real    r, t, dr, dt
!!    integer nplanes
!!    logical fill, back, backdir
!!    integer :: ios
!!    integer :: ipaws
!!
!!    print*,'Enter output device:'
!!    read(*,'(a)',iostat=ios)device
!!    if(ios.ne.0)device=' '
!!
!!    dr = 0.3
!!    dt = 0.2
!!    print*,'Enter delta in degrees (0.333 is typical):'
!!    read(*,*,iostat=ios) dr
!!    if(ios.ne.0)dr=0.333
!!    write(*,*)'DR=',dr
!!
!!    call prefsize(300, 300)
!!
!!    call vinit(device)
!!
!!    nplanes = getdepth()
!!
!!    fill = .true.
!!    back = .true.
!!    backdir = .true.
!!
!!    call polyfill(fill)
!!    call backface(back)
!!    call backfacedir(backdir)
!!
!!    call color(D_BLACK)
!!    call clear()
!!
!!    call window(-1.5, 1.5, -1.5, 1.5, 9.0, -5.0)
!!    call lookat(0.0, 0.0, 12.0, 0.0, 0.0, 0.0, 0.0)
!!
!!    t = 0.0
!!    r = 30.0
!!    !
!!    ! Setup drawing into the backbuffer....
!!    !
!!    if (backbuffer().lt.0) then
!!       write(*,*)'Device can''t support doublebuffering'
!!       ! draw a single view so at least the cube is displayed
!!       call frontbuffer()
!!       call color(D_BLACK)
!!       call clear()
!!       call translate(0.0, 0.0, t)
!!       call rotate(r, 'y')
!!       call rotate(r, 'z')
!!       call rotate(r, 'x')
!!       call color(D_WHITE)
!!       call polyfill(.true.)
!!       call drawcube(nplanes)
!!       call color(D_BLACK)
!!       call polyfill(.false.)
!!       call drawcube(nplanes)
!!       ipaws=getkey()
!!       call vexit()
!!       stop
!!
!!    endif
!!
!!    do
!!       if(r.ge.360) r = 0.0
!!       call color(D_BLACK)
!!       call clear()
!!
!!       call pushmatrix()
!!
!!       call translate(0.0, 0.0, t)
!!       call rotate(r, 'y')
!!       call rotate(r, 'z')
!!       call rotate(r, 'x')
!!       call color(D_WHITE)
!!
!!       call drawcube(nplanes)
!!
!!       if (nplanes .eq. 1 .and. fill) then
!!          call polyfill(.false.)
!!          call color(D_BLACK)
!!          call drawcube(nplanes)
!!          call polyfill(fill)
!!       endif
!!
!!       call popmatrix()
!!
!!       t = t + dt
!!       if (t.gt.3.0 .or. t.lt.-18.0) dt = -dt
!!
!!       call swapbuffers()
!!
!!       c = char(checkkey())
!!       if (c .eq. 'f') then
!!          fill = .not. fill
!!          call polyfill(fill)
!!       elseif (c .eq. 'b') then
!!          back = .not. back
!!          call backface(back)
!!       elseif (c .eq. 'd') then
!!          backdir = .not. backdir
!!          call backfacedir(backdir)
!!       elseif (c .ne. char(0)) then
!!          call vexit()
!!          stop
!!       endif
!!
!!       r = r + dr
!!    enddo
!!
!!    contains
!!    ! this routine draws the cube, using colours if available
!!    !
!!    subroutine drawcube(nplanes)
!!       integer nplanes
!!
!!       real carray(3, 8)
!!       data carray/            &
!!       &   -1.0,  -1.0,   1.0,  &
!!       &    1.0,  -1.0,   1.0,  &
!!       &    1.0,   1.0,   1.0,  &
!!       &   -1.0,   1.0,   1.0,  &
!!       &   -1.0,  -1.0,  -1.0,  &
!!       &    1.0,  -1.0,  -1.0,  &
!!       &    1.0,   1.0,  -1.0,  &
!!       &   -1.0,   1.0,  -1.0/
!!       save carray
!!
!!       if (nplanes.gt.1) call color(D_RED)
!!
!!       call makepoly()
!!       call move(carray(1,1), carray(2,1), carray(3,1))
!!       call draw(carray(1,2), carray(2,2), carray(3,2))
!!       call draw(carray(1,3), carray(2,3), carray(3,3))
!!       call draw(carray(1,4), carray(2,4), carray(3,4))
!!       call draw(carray(1,1), carray(2,1), carray(3,1))
!!       call closepoly()
!!
!!       if (nplanes.gt.1) call color(D_GREEN)
!!
!!       call makepoly()
!!       call move(carray(1,6), carray(2,6), carray(3,6))
!!       call draw(carray(1,5), carray(2,5), carray(3,5))
!!       call draw(carray(1,8), carray(2,8), carray(3,8))
!!       call draw(carray(1,7), carray(2,7), carray(3,7))
!!       call draw(carray(1,6), carray(2,6), carray(3,6))
!!       call closepoly()
!!
!!       if (nplanes.gt.1) call color(D_YELLOW)
!!
!!       call makepoly()
!!       call move(carray(1,2), carray(2,2), carray(3,2))
!!       call draw(carray(1,6), carray(2,6), carray(3,6))
!!       call draw(carray(1,7), carray(2,7), carray(3,7))
!!       call draw(carray(1,3), carray(2,3), carray(3,3))
!!       call draw(carray(1,2), carray(2,2), carray(3,2))
!!       call closepoly()
!!
!!       if (nplanes.gt.1) call color(D_BLUE)
!!
!!       call makepoly()
!!       call move(carray(1,1), carray(2,1), carray(3,1))
!!       call draw(carray(1,4), carray(2,4), carray(3,4))
!!       call draw(carray(1,8), carray(2,8), carray(3,8))
!!       call draw(carray(1,5), carray(2,5), carray(3,5))
!!       call draw(carray(1,1), carray(2,1), carray(3,1))
!!       call closepoly()
!!
!!       if (nplanes.gt.1) call color(D_MAGENTA)
!!
!!       call makepoly()
!!       call move(carray(1,3), carray(2,3), carray(3,3))
!!       call draw(carray(1,7), carray(2,7), carray(3,7))
!!       call draw(carray(1,8), carray(2,8), carray(3,8))
!!       call draw(carray(1,4), carray(2,4), carray(3,4))
!!       call draw(carray(1,3), carray(2,3), carray(3,3))
!!       call closepoly()
!!
!!       if (nplanes.gt.1) call color(D_CYAN)
!!
!!       call makepoly()
!!       call move(carray(1,1), carray(2,1), carray(3,1))
!!       call draw(carray(1,5), carray(2,5), carray(3,5))
!!       call draw(carray(1,6), carray(2,6), carray(3,6))
!!       call draw(carray(1,2), carray(2,2), carray(3,2))
!!       call draw(carray(1,1), carray(2,1), carray(3,1))
!!       call closepoly()
!!
!!    end subroutine drawcube
!!
!!    end program demo_backface
!!
!! !
   interface draw_backface
      module procedure draw_backface_integer
      module procedure draw_backface_logical
   end interface draw_backface

!>
!!##NAME
!!    backfacedir(3f) - [M_draw:POLYGONS] Sets backfacing direction to clockwise or anti-clockwise
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!       subroutine backfacedir(clockwise)
!!       integer,intent(in) :: clockwise
!!
!!##DESCRIPTION
!!    Sets the backfacing direction to clockwise or anti-clockwise depending
!!    on whether clockwise is 1 or 0. 1 = clockwise (in screen coords)
!!    0 = anticlockwise.
!!
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

!>
!!##NAME
!!      polyfill(3f) - [M_draw:POLYGONS] Set the polygon fill flag
!!      (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!       subroutine polyfill(onoff)
!!       logical onoff
!!
!!##DESCRIPTION
!!    Set the polygon fill flag. This will always turn off hatching. A
!!    LOGICAL .true. turns polyfill on.
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_polyfill
!!    ! Using polygons, hatching, and filling.
!!       use M_draw, only : vinit, color, clear, ortho, boxtext, vexit
!!       use M_draw, only : polyfill
!!       use M_draw,    only  : D_BLACK,   D_WHITE
!!       use M_draw,    only  : D_RED,     D_GREEN,    D_BLUE
!!       use M_draw,    only  : D_YELLOW,  D_MAGENTA,  D_CYAN
!!       implicit none
!!       integer           :: ios
!!       character(len=50) :: device
!!       print*,'Enter output device:'
!!       read(*,'(a)',iostat=ios)device
!!       if(ios.ne.0)device=' '
!!       call vinit(device)
!!    ! clear to black
!!       call color(D_BLACK)
!!       call clear()
!!    ! world coordinates are now in the range -10 to 10
!!    ! in x, y, and z. Note that positive z is towards us.
!!       call ortho(-10.0, 10.0, -10.0, 10.0, 10.0, -10.0)
!!       call color(D_YELLOW)
!!    ! write out the string "Polygon from poly()" in the
!!    ! starting at (-8.0, -4.0) and scaled to be 4.0 units long,
!!    ! 0.5 units high.
!!       call boxtext(-8.0, -4.0, 4.0, 0.5, 'Polygon from poly()')
!!       call color(D_GREEN)
!!    ! write out a scaled string starting at (0.0, 6.0)
!!       call boxtext(0.0, 6.0, 4.0, 0.5, 'Polygon from move()/ draw()')
!!       call color(D_MAGENTA)
!!    ! write out a scaled string starting at (0.0, 6.0)
!!       call boxtext(3.5, -3.5, 1.9, 0.5, 'Sector')
!!    ! turn on polygon filling - this automatically turns off hatching
!!       call polyfill(.true.)
!!    ! draw some polygons with filling
!!       call my_drawpoly()
!!       call vexit()
!!    contains
!!    subroutine my_drawpoly()
!!       use M_draw, only : color, move, draw, closepoly, sector, getkey
!!       use M_draw, only : poly, closepoly, sector, makepoly
!!       use M_draw,    only  : D_BLACK,   D_WHITE
!!       use M_draw,    only  : D_RED,     D_GREEN,    D_BLUE
!!       use M_draw,    only  : D_YELLOW,  D_MAGENTA,  D_CYAN
!!       real parray(3,4)                   ! An array of points for a polygon
!!       integer :: idum
!!       data parray/ -8.0, -8.0, 0.0,  &
!!       & -5.0, -8.0, 0.0,  &
!!       & -5.0, -5.0, 0.0,  &
!!       & -8.0, -5.0, 0.0 /
!!       call color(D_YELLOW)
!!    ! Draw a polygon using poly, parray is our array of
!!    ! points and 4 is the number of points in it.
!!       call poly(4, parray)
!!       call color(D_GREEN)
!!    ! Draw a 5 sided figure by using move, draw and closepoly.
!!       call makepoly()
!!       call move(0.0, 0.0, 0.0)
!!       call draw(3.0, 0.0, 0.0)
!!       call draw(3.0, 4.0, 0.0)
!!       call draw(-1.0, 5.0, 0.0)
!!       call draw(-2.0, 2.0, 0.0)
!!       call closepoly()
!!       call color(D_MAGENTA)
!!    ! draw a sector representing a 1/4 circle
!!       call sector(1.5, -7.0, 3.0, 0.0, 90.0)
!!       idum=getkey()
!!    end subroutine my_drawpoly
!!    end program demo_polyfill

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

!>
!!##NAME
!!    polyhatch(3f) - [M_draw:POLYGONS] Set the polygon hatch flag
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!     subroutine polyhatch(onoff)
!!     logical onoff
!!##DESCRIPTION
!!
!!    Set the polygon hatch flag. This will always turn off fill. A
!!    LOGICAL .true. turns polyhatch on. Note that hatched polygons
!!    must initially be defined parallel to the X-Y plane.
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_polyhatch
!!    use M_draw
!!    use M_draw, only: D_BLACK, D_WHITE
!!    use M_draw, only: D_RED, D_GREEN, D_BLUE
!!    use M_draw, only: D_YELLOW, D_MAGENTA, D_CYAN
!!    implicit none
!!    integer :: key
!!    real :: N = 11.0
!!       call prefsize(600*10/6, 200*10/6)
!!       call vinit(' ')
!!       call page(-15.0, 15.0, -5.0, 5.0)
!!       call linewidth(100)
!!       call color(D_BLACK)
!!       call clear()
!!       call color(D_RED)
!!       call spirograph(-10.0, 0.0, N, 1.0, N, 5.0, 1000, 0.0, 0.0, 0)
!!       call polyhatch(.true.) ! turn on polygon hatching
!!       call hatchang(45.0)
!!       call hatchpitch(0.3)
!!       call color(D_GREEN)
!!       call spirograph(10.0, 0.0, N, 1.0, N, 5.0, 1000, 0.0, 0.0, 2)
!!       call vflush()
!!       key = getkey()
!!       call vexit()
!!    contains
!!    subroutine spirograph(xc,yc,sun,planet0,offset0,rad,ilines,ang,angs,ifill)
!!    real, parameter :: PI=3.14159265358979323846264338327950288419716939937510
!!    ! center of curve
!!    real, intent(in)    :: xc, yc
!!    ! radii of sun, planet, and planet offset
!!    real, intent(in)    :: sun, planet0, offset0
!!    ! radius to fit the shape to (no fit if radius is 0)
!!    real, intent(in)    :: rad
!!    ! number of points to sample along curve
!!    integer, intent(in) :: ilines
!!    ! angle to rotate the shape by, to orientate it.
!!    real, intent(in)    :: ang
!!    ! angle to start sampling points at; ccw is +; 0 is East
!!    real, intent(in)    :: angs
!!    ! 1 make a filled polygon, 2 make a hatched polygon
!!    integer, intent(in) :: ifill
!!    real                :: ang1, con1, con2, factor, offset, planet
!!    real                :: r, sunr, u, xpoin, xpoin1, ypoin, ypoin1
!!    integer             :: i10
!!       sunr = sun
!!       offset = offset0
!!       planet = planet0
!!       if (ilines  ==  0) return
!!       if (planet  ==  0.0) return
!!       if (sunr  ==  0.0) return
!!       if (rad  /=  0 .and. sunr - planet + offset  /=  0) then
!!          factor = rad/(sunr - planet + offset)
!!          sunr = factor*sunr
!!          planet = factor*planet
!!          offset = factor*offset
!!       end if
!!       u = 0.0 + ang
!!       con1 = PI*2.*(sunr/planet)/real(ilines)
!!       con2 = (1.0 - planet/sunr)*u
!!       xpoin1 = (sunr - planet)*cos(planet*u/sunr) + offset*cos(con2)
!!       ypoin1 = (sunr - planet)*sin(planet*u/sunr) - offset*sin(con2)
!!       ang1 = atan2(ypoin1, xpoin1) + angs
!!       r = sqrt(xpoin1**2 + ypoin1**2)
!!       xpoin1 = r*cos(ang1) + xc
!!       ypoin1 = r*sin(ang1) + yc
!!       select case (ifill)
!!       case (0)
!!       case (1)
!!          call polyfill(.true.)
!!          call makepoly()
!!       case (2)
!!          call polyhatch(.true.)
!!          call makepoly()
!!       case (3:)
!!          call makepoly()
!!       case default
!!       end select
!!       call move2(xpoin1, ypoin1)
!!       do i10 = 1, ilines
!!          u = con1*i10 + ang
!!          con2 = (1.0 - planet/sunr)*u
!!          if (con2  >=  2**24) con2 = amod(con2, PI)
!!          xpoin = (sunr - planet)*cos(planet*u/sunr) + offset*cos(con2)
!!          ypoin = (sunr - planet)*sin(planet*u/sunr) - offset*sin(con2)
!!          ang1 = atan2(ypoin, xpoin) + angs
!!          r = sqrt(xpoin**2 + ypoin**2)
!!          xpoin = r*cos(ang1) + xc
!!          ypoin = r*sin(ang1) + yc
!!          call draw2(xpoin, ypoin)
!!       end do
!!       if (ifill  >  0) then
!!          call closepoly()
!!          call polyfill(.false.)
!!       end if
!!    end subroutine spirograph
!!    end program demo_polyhatch
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

!>
!!##NAME
!!    hatchang(3f) - [M_draw:POLYGONS] Set the angle of the hatch lines.
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!       subroutine hatchang(angle)
!!       real,intent(in) :: angle
!!
!!##DESCRIPTION
!!
!!    Set the angle of the hatch lines. The angle is in degrees. Zero degrees
!!    is on the negative X axis. Positive values are counterclockwise. The
!!    value is 0 at program initialization. The last value set is retained
!!    even if hatching is not active or is turned on and off.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!       program demo_hatchang
!!       use M_draw
!!       implicit none
!!       real :: b
!!       integer :: idum
!!          call prefsize(1000,200)
!!          call vinit(' ')
!!          b = 0.4
!!          call page(-25.0 - b, 25.0 + b, -5.0 - b, 5.0 + b)
!!          call color(0)
!!          call clear()
!!          call textsize(0.6, 0.7)
!!          call font('futura.l')
!!          call centertext(.true.)
!!          call leftjustify()
!!          call linewidth(50)
!!          call polyhatch(.true.)
!!          call hatchpitch(1.0/2.0)
!!          ! draw circles with hatching
!!          call drawc(ang=  90.1, col=7, x=-20.0, label='90 degrees')
!!          call drawc(ang=  45.0, col=2, x=-10.0, label='45 degrees')
!!          call drawc(ang=   0.0, col=6, x=  0.0, label='0 degrees')
!!          call drawc(ang= -45.0, col=5, x= 10.0, label='-45 degrees')
!!          call drawc(ang= -90.0, col=4, x= 20.0, label='-90 degrees')
!!
!!          call linewidth(130)
!!          call move2(0.0, 0.0)
!!          call draw2(-5.0, 0.0)
!!          call move2(-5.0, 0.0)
!!          call draw2(-4.4, 0.3)
!!          call move2(-5.0, 0.0)
!!          call draw2(-4.4, - 0.3)
!!          call rightjustify()
!!          call linewidth(60)
!!          call move2(-5.0,0.0)
!!          call drawstr('0 degrees')
!!          idum = getkey()
!!          call vexit()
!!       contains
!!          subroutine drawc(ang,col,x,label)
!!          real,intent(in) :: ang, x
!!          integer,intent(in) :: col
!!          character(len=*),intent(in) :: label
!!          real :: y
!!          y=0.0
!!          call linewidth(90)
!!          call hatchang(ang)
!!          call color(col)
!!          call circle(X, Y, 5.0)
!!          y = -4.9
!!          call move2(X - 4.9, Y)
!!          call color(7)
!!          call linewidth(60)
!!          call drawstr(label)
!!          end subroutine drawc
!!
!!       end program demo_hatchang
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

!>
!!##NAME
!!    hatchpitch(3f) - [M_draw:POLYGONS] Set the distance between hatch lines.
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!       subroutine hatchpitch(pitch)
!!       real,intent(in) :: pitch
!!
!!##DESCRIPTION
!!    Set the distance between hatch lines. The distance is measured in
!!    window units (as opposed to viewport or device units).
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!       program demo_hatchpitch
!!       use M_draw
!!       implicit none
!!       real :: b
!!       integer :: idum
!!
!!          call prefsize(1000, 200)
!!          call vinit(' ')
!!          b = 0.1
!!          call page(-25.0 - b, 25.0 + b, -5.0 - b, 5.0 + b)
!!          call color(0)
!!          call clear()
!!          call textsize(0.5, 0.6)
!!          call font('futura.l')
!!          call leftjustify()
!!          call circleprecision(3)
!!          ! draw circles with hatching
!!          call linewidth(150)
!!          call polyhatch(.true.)
!!          call hatchang(30.0)
!!
!!          call try(pitch=1.0/3.0, col=7, X=-20.0, label='1/3')
!!          call try(pitch=1.0/2.0, col=2, X=-10.0, label='1/2')
!!          call try(pitch=1.0,     col=6, X= -0.0, label='1')
!!          call try(pitch=2.0,     col=5, X= 10.0, label='2')
!!          call try(pitch=3.0,     col=4, X= 20.0, label='3')
!!          idum = getkey()
!!          call vexit()
!!       contains
!!          subroutine try(pitch, col, x, label)
!!             real, intent(in) :: pitch
!!             integer, intent(in) :: col
!!             real, intent(in) :: x
!!             character(len=*), intent(in) :: label
!!             call hatchpitch(pitch)
!!             call color(col)
!!             call circle(X, 0.0, 5.0)
!!             call move2(X - 4.9,-4.9)
!!             call color(7)
!!             call drawstr(label)
!!          end subroutine try
!!
!!       end program demo_hatchpitch
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

!>
!!##NAME
!!    poly2(3f) - [M_draw:POLYGONS] Construct an (x, y) polygon from an array of points
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!       subroutine poly2(n, points)
!!       integer,intent(in) :: n
!!       real,intent(in) :: points(2, n)
!!
!!##DESCRIPTION
!!    Construct an (x, y) polygon from an array of points provided by the user.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!      program demo_poly2
!!      use M_draw
!!      implicit none
!!      integer :: i,j
!!      integer :: ipaws, icolor
!!      real    :: xx,yy
!!         call prefsize(512,512)
!!         call vinit(' ') ! start graphics using device $M_DRAW_DEVICE
!!         call ortho2(0.0,256.0,0.0,256.0)
!!         call linewidth(1)
!!         call polyfill(.true.)
!!         ! step thru a series of rectangular cells
!!         icolor=0
!!         xx=0.0
!!         do i=1,16
!!            yy=0.0
!!            do j=1,16
!!               yy=yy+16.0
!!               icolor=icolor+1
!!               call setcolor(icolor,xx,yy)
!!            enddo
!!            xx=xx+16.0
!!         enddo
!!         ipaws=getkey()
!!         call vexit()
!!      contains
!!
!!      subroutine setcolor(iset,xx,yy)
!!      integer,intent(in) :: iset
!!      real,intent(in)    :: xx,yy
!!      real    :: points(2,100)
!!      integer :: red, green, blue
!!      if(iset.gt.255)return
!!      ! determine coordinates of next square
!!      points(1:2,1)=[xx,      yy      ]
!!      points(1:2,2)=[xx,      yy+16.0 ]
!!      points(1:2,3)=[xx+16.0, yy+16.0 ]
!!      points(1:2,4)=[xx+16.0, yy      ]
!!      points(1:2,5)=[xx,      yy      ]
!!      ! get some RGB values to try
!!      red=irand(0,255)
!!      green=irand(0,255)
!!      blue=irand(0,255)
!!      ! set a color number to the new RGB values
!!      call mapcolor(icolor, red, green, blue)
!!      ! set to the new color
!!      call color(icolor)
!!      ! fill the rectangle in that color
!!      call poly2(5,points)
!!      end subroutine setcolor
!!
!!      function irand(first,last) result(rand_int)
!!      use, intrinsic :: iso_fortran_env, only : dp=>real64
!!      integer,intent(in) :: first
!!      integer,intent(in) :: last
!!      integer              :: rand_int
!!      real(kind=dp)        :: rand_val
!!         call random_number(rand_val)
!!         rand_int = first + FLOOR((last+1-first)*rand_val)
!!      end function irand
!!
!!      end program demo_poly2
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

!>
!!##NAME
!!    poly(3f) - [M_draw:POLYGONS] Construct a polygon from an array of points
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!       subroutine poly(n, points)
!!       integer,intent(in) :: n
!!       real,intent(in) :: points(3, n)
!!
!!##DESCRIPTION
!!
!!    Construct a polygon from an array of points provided by the user.
!!
!!##EXAMPLE
!!
!!  Sample program:
!!
!!    program demo_poly
!!    use M_draw
!!    implicit none
!!    ! Using polygons, hatching, and filling.
!!    integer           :: ios
!!    character(len=50) :: device
!!       print*,'Enter output device:'
!!       read(*,'(a)',iostat=ios)device
!!       if(ios.ne.0)then
!!          call prefsize(1000,1000)
!!          device=' '
!!       endif
!!       call vinit(device)
!!    ! clear to black
!!       call color(D_BLACK)
!!       call clear()
!!    ! world coordinates are now in the range -10 to 10
!!    ! in x, y, and z. Note that positive z is towards us.
!!       call ortho(-10.0, 10.0, -10.0, 10.0, 10.0, -10.0)
!!       call color(D_YELLOW)
!!    ! write out the string "Polygon from poly()" in the
!!    ! starting at (-8.0, -4.0) and scaled to be 4.0 units long,
!!    ! 0.5 units high.
!!       call boxtext(-8.0, -4.0, 4.0, 0.5, 'Polygon from poly()')
!!       call color(D_GREEN)
!!    ! write out a scaled string starting at (0.0, 6.0)
!!       call boxtext(0.0, 6.0, 4.0, 0.5, 'Polygon from move()/ draw()')
!!       call color(D_MAGENTA)
!!    ! write out a scaled string starting at (0.0, 6.0)
!!       call boxtext(3.5, -3.5, 1.9, 0.5, 'Sector')
!!    ! draw some polygons
!!       call drawpoly()
!!    ! turn on polygon hatching
!!       call polyhatch(.true.)
!!       call hatchang(45.0)
!!       call hatchpitch(0.3)
!!    !  Rotate 20 degrees around x and 30 around y
!!       call rotate(20.0, 'x')
!!       call rotate(30.0, 'y')
!!    ! draw some polygons with hatching
!!       call drawpoly()
!!    ! turn on polygon filling - this automatically turns off hatching
!!       call polyfill(.true.)
!!    !  Do another set of rotations.
!!       call rotate(20.0, 'x')
!!       call rotate(30.0, 'y')
!!    ! draw some polygons with filling
!!       call drawpoly()
!!       call vexit()
!!    contains
!!    subroutine drawpoly()
!!       integer :: idum
!!       real parray(3,4)                   ! An array of points for a polygon
!!       data parray/ -8.0, -8.0, 0.0,  &
!!                  & -5.0, -8.0, 0.0,  &
!!                  & -5.0, -5.0, 0.0,  &
!!                  & -8.0, -5.0, 0.0 /
!!       call color(D_YELLOW)
!!    ! Draw a polygon using poly, parray is our array of
!!    ! points and 4 is the number of points in it.
!!       call poly(4, parray)
!!       call color(D_GREEN)
!!    ! Draw a 5 sided figure by using move, draw and closepoly.
!!       call makepoly()
!!          call move(0.0, 0.0, 0.0)
!!          call draw(3.0, 0.0, 0.0)
!!          call draw(3.0, 4.0, 0.0)
!!          call draw(-1.0, 5.0, 0.0)
!!          call draw(-2.0, 2.0, 0.0)
!!       call closepoly()
!!       call color(D_MAGENTA)
!!    ! draw a sector representing a 1/4 circle
!!       call sector(1.5, -7.0, 3.0, 0.0, 90.0)
!!       idum=getkey()
!!    end subroutine drawpoly
!!
!!    end program demo_poly
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

!>
!!##NAME
!!    makepoly(3f) - [M_draw:POLYGONS] opens polygon constructed by a series of move-draws and closed by closepoly
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!       subroutine makepoly()
!!
!!##DESCRIPTION
!!    makepoly(3f)  opens up a polygon which will then be constructed by a series
!!    of move-draws and closed by a closepoly.
!!
!!##EXAMPLE
!!
!!  Sample program:
!!
!!    program demo_makepoly
!!    use :: M_draw
!!    implicit none
!!    integer,parameter :: wide=640, tall=640
!!    integer :: rows, xoff, yoff, box_sz
!!    integer :: i20, i30, ncols, nrows, ilines
!!    real    :: bottom, left, sun_radius, planet_radius, planet_offset
!!    integer :: ipaws
!!       call prefsize(wide,tall)
!!       call vinit(' ') ! start graphics using device $M_DRAW_DEVICE
!!       call ortho2(0.0, real(wide), 0.0, real(tall) )
!!       ! really slows down pbm driver because all lines are polygons
!!       ! call linewidth(3)
!!       call color(D_WHITE)
!!       call clear()
!!       call color(D_BLACK)
!!       rows=1
!!       ! size of biggest box to use and get specified number of rows
!!       box_sz=MIN(wide,tall)/rows
!!       ! number of rows of objects to draw
!!       nrows = tall/box_sz
!!       ! number of columns of objects to draw
!!       ncols = wide/box_sz
!!       ! initial x offset to begin row at to center drawings
!!       xoff = (wide - ncols * box_sz)/2
!!       ! initial x offset to begin column at to center drawings
!!       yoff = (tall - nrows * box_sz)/2
!!       sun_radius = 148
!!       planet_radius = 1
!!       do ilines = 1, 300
!!          do i20 = 1, ncols
!!             left = (i20-1)*box_sz+xoff
!!             do i30 = 1, nrows
!!                bottom = (i30-1)*box_sz+yoff
!!                call color(D_BLACK)
!!             call makepoly()
!!                call rect(left,bottom,left+box_sz,bottom+box_sz)
!!             call closepoly()
!!                planet_offset= sun_radius
!!                   call color(mod(ilines,15)+1)
!!                   call hypoc(left + box_sz/2.0, bottom + box_sz/2.0, &
!!                & sun_radius, planet_radius, planet_offset, &
!!                & box_sz/2.0, ilines,  &
!!                & 0.0, 0.0, 1)
!!             enddo
!!          enddo
!!          ipaws=getkey()
!!       enddo
!!       call vexit()
!!    contains
!!    !
!!    !  Make shapes using hypocycloidal curves.
!!    !
!!    subroutine hypoc(xc,yc,sun,planet0,offset0,radius,ilines,ang,angs,ifill)
!!    use M_draw
!!    implicit none
!!    real,parameter  :: PI= 3.14159265358979323846264338327950288419716939937510
!!    real,intent(in) :: xc, yc      ! center of curve
!!    ! radii of sun, planet, and planet offset
!!    real,intent(in) :: sun,planet0,offset0
!!    real,intent(in)    :: radius
!!    integer,intent(in) :: ilines
!!    ! radius to fit the shape to (no fit if radius is 0)
!!    real,intent(in)    :: ang
!!    ! number of points to sample along curve
!!    real,intent(in)    :: angs
!!    ! angle to rotate the shape by, to orientate it.
!!    integer,intent(in) :: ifill
!!    ! angle to start sampling points at; ccw is +; 0 is East
!!    integer            :: i10
!!    ! 1 make a filled polygon, 2 make a hatched polygon
!!    real               :: ang1, con1, con2, factor
!!    real               :: offset, planet, r, sunr, u
!!    real               :: xpoin, xpoin1, ypoin, ypoin1
!!       sunr=sun
!!       offset=offset0
!!       planet=planet0
!!       if(ilines.eq.0.0) return
!!       if(planet.eq.0.0) return
!!       if(sunr.eq.0.0)   return
!!       if(radius.ne.0.and.sunr-planet+offset.ne.0)then
!!          factor=radius/(sunr-planet+offset)
!!          sunr=factor*sunr
!!          planet=factor*planet
!!          offset=factor*offset
!!       endif
!!       u=0.0+ang
!!       con1=PI*2.*(sunr/planet)/real(ilines)
!!       con2=(1.0-planet/sunr)*u
!!       xpoin1=(sunr-planet)*cos(planet*u/sunr)+offset*cos(con2)
!!       ypoin1=(sunr-planet)*sin(planet*u/sunr)-offset*sin(con2)
!!       ang1=atan2(ypoin1,xpoin1)+angs
!!       r=sqrt(xpoin1**2+ypoin1**2)
!!       xpoin1=r*cos(ang1)+xc
!!       ypoin1=r*sin(ang1)+yc
!!       select case(ifill)
!!       case(:0)
!!       case(1:)
!!          call makepoly()
!!       end select
!!       call move2(xpoin1,ypoin1)
!!       do i10=1,ilines
!!          u=con1*i10+ang
!!          con2=(1.0-planet/sunr)*u
!!          if(con2.ge.2**24) con2=amod(con2,PI)
!!          xpoin=(sunr-planet)*cos(planet*u/sunr)+offset*cos(con2)
!!          ypoin=(sunr-planet)*sin(planet*u/sunr)-offset*sin(con2)
!!          ang1=atan2(ypoin,xpoin)+angs
!!          r=sqrt(xpoin**2+ypoin**2)
!!          xpoin=r*cos(ang1)+xc
!!          ypoin=r*sin(ang1)+yc
!!          call draw2(xpoin,ypoin)
!!       enddo
!!       call draw2(xpoin1,ypoin1)
!!       if(ifill.gt.0)then
!!         call closepoly()
!!       endif
!!    end subroutine hypoc
!!    end program demo_makepoly
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

!>
!!##NAME
!!    closepoly(3f) - [M_draw:POLYGONS] Terminates a polygon opened by makepoly(3f)
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!       subroutine closepoly()
!!
!!##DESCRIPTION
!!
!!    Terminates a polygon opened by makepoly(3f).
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
