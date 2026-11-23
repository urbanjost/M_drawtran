module m_draw_arcs
   use, intrinsic :: iso_fortran_env, only: real32, real64
   use m_draw_types
   use m_draw_verror
   use m_draw_move
   use m_draw_draw
   implicit none
   private
   public :: draw_arcprecision, draw_circleprecision, draw_arc, draw_sector, draw_circle

   integer, save :: nsegs = 32

   interface
      function draw_newtokens(num) result(toks)
         import :: Token
         integer, intent(in) :: num
         type(Token), pointer :: toks(:)
      end function draw_newtokens

      subroutine draw_makepoly()
      end subroutine draw_makepoly

      subroutine draw_closepoly()
      end subroutine draw_closepoly
   end interface

contains

   subroutine draw_arcprecision(noseg)
      integer, intent(in) :: noseg
      nsegs = noseg
   end subroutine draw_arcprecision

!>
!!##NAME
!!     circleprecision(3f) - [M_draw:ARCS] Set number of line segments used to approximate a circle
!!     (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!         subroutine circleprecision(nsegs)
!!         integer,intent(in) :: nsegs
!!
!!    Set the number of line segments making up a circle. Default is
!!    currently 32. The number of segments in an arc or sector is calculated
!!    from the variable "nsegs" according to the span of the arc or sector.
!!
!!##OPTIONS
!!    NSEGS   number of line segments making up a circle
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_circleprecision
!!    use M_draw
!!    use M_draw,    only  : D_BLACK,   D_WHITE
!!    use M_draw,    only  : D_RED,     D_GREEN,    D_BLUE
!!    use M_draw,    only  : D_YELLOW,  D_MAGENTA,  D_CYAN
!!    implicit none
!!    real    :: b=0.5
!!    real    :: y1,y2,ym,x1,x2
!!    real    :: width=50.0/8.0,width2
!!    integer,parameter :: ivals(*)=[3,5,7,10,20,30,60,100]
!!    integer :: i
!!    integer :: ipaws
!!       !! set up long bar as plotting area
!!       call prefsize(1000,200)
!!       call vinit(' ') ! start graphics using device $M_DRAW_DEVICE
!!       call page(-25.0-b, 25.0+b, -5.0-b, 5.0+b)
!!       call textsize( 2.5/2.0, 3.0/2.0)
!!       call font('futura.l')
!!       call centertext(.true.)
!!       call linewidth(30)
!!       call color(D_GREEN)
!!       y1=-5
!!       y2=5
!!       ym=0
!!       x1=-25+.05*width
!!       ! draw colored rectangle and a circle and label center of circle repeat
!!       width2=width*0.95
!!       do i=1,size(ivals)
!!          x2=x1+width2
!!          call move2((x1+x2)/2.0,ym)
!!          call circleprecision(ivals(i))
!!          call print(ivals(i))     ! convert number to string and draw it
!!          call circle((x1+x2)/2.0, ym, (x2-x1)/2.10)
!!          x1=x1+width
!!       enddo
!!       ipaws=getkey()
!!       call vexit()
!!    end program demo_circleprecision
!!
!!##IMAGE
!!    circles are drawn with various circle precision values.
   subroutine draw_circleprecision(noseg)
      integer, intent(in) :: noseg
      nsegs = noseg
   end subroutine draw_circleprecision

!>
!!##NAME
!!     arc(3f) - [M_draw:ARCS] Draw an arc in world units.
!!     (LICENSE:PD)
!!##SYNOPSIS
!!
!!         subroutine arc(x, y, radius, startang, endang)
!!         real,intent(in) :: x
!!         real,intent(in) :: y
!!         real,intent(in) :: radius
!!         real,intent(in) :: startang
!!         real,intent(in) :: endang
!!##DESCRIPTION
!!
!!    Draw an arc. x, y, and radius are values in world units
!!    using current line width and color
!!
!!    Angles are in degrees, positive measured counterclockwise from the
!!    +X axis. The current position after the arc is drawn is at the end
!!    of the arc.
!!
!!##OPTIONS
!!    X,Y        Coordinates for the center of the circle
!!    RADIUS     Radius of the circle
!!    STARTANG   Start angle
!!    ENDANG     End angle
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_arc
!!       use M_draw
!!       use M_draw,    only  : D_BLACK,   D_WHITE
!!       use M_draw,    only  : D_RED,     D_GREEN,    D_BLUE
!!       use M_draw,    only  : D_YELLOW,  D_MAGENTA,  D_CYAN
!!       implicit none
!!       integer        :: icolor
!!       real           :: b=0.5
!!       real           :: R=4.9
!!       real           :: X, Y, A, YY
!!       integer        :: key
!!       call prefsize(1000,200)
!!       call vinit(' ')
!!       call color(D_BLACK)
!!       call clear()
!!       call color(D_YELLOW)
!!       call page(-25.0-b, 25.0+b, -5.0-b, 5.0+b)
!!       call textsize(1.0,1.4)
!!       call font("futura.l")
!!       call centertext(.true.)
!!       ! draw arcs with various start and end angles
!!
!!       X=-20.0; Y=0.0; A=0.0; B=30.0;  icolor=7; YY=4
!!       call drawit("0 to 30 deg.")
!!
!!       X=-10.0; YY=-4; A=0.0; B=-45.0; icolor=1
!!       call drawit("0 to -45 deg.")
!!
!!       X=0.0; YY=-4; A=100.0; B=200.0; icolor=2
!!       call drawit("100 to 200 deg.")
!!
!!       X=10.0; YY=-4; A=-30.0; B=30.0;icolor=5
!!       call drawit("-30 to 30 deg.")
!!
!!       YY=4; X=20.0; A=45.0; B=-45.0; icolor=6
!!       call drawit("45 to -45 deg.")
!!
!!       call vflush()
!!       key=getkey()
!!       call vexit()
!!    contains
!!       subroutine drawit(label)
!!          character(len=*) :: label
!!          call linewidth(150)
!!          call color(icolor)
!!          call arc(X,Y,R,A,B)
!!          call draw2(X,Y)
!!          call move2(X,YY)
!!          call linewidth(50)
!!          call color(D_WHITE)
!!          call drawstr(label)
!!       end subroutine drawit
!!
!!    end program demo_arc
   subroutine draw_arc(x, y, radius, startang, endang)
      real(real32), intent(in) :: x, y, radius, startang, endang
      type(Token), pointer :: t(:)
      real(real32) :: cx, cy, dx, dy
      real(real32) :: deltang, cosine, sine, angle
      integer :: sync, i, numsegs, idum

      if (vdevice%initialized == 0) call draw_verror("arc: draw not initialized")

      sync = vdevice%sync
      if (sync /= 0) vdevice%sync = D_FALSE

      angle = startang * D2R
      numsegs = int(abs(endang - startang) / 360.0_real32 * nsegs + 0.5_real32)
      deltang = (endang - startang) * D2R / numsegs
      cosine = real(cos(real(deltang, real64)), real32)
      sine = real(sin(real(deltang, real64)), real32)

      if (vdevice%inobject /= 0) then
         t => draw_newtokens(8)
         t(1)%i = OBJ_ARC
         t(2)%f = x
         t(3)%f = y
         t(4)%f = radius * real(cos(real(angle, real64)), real32)
         t(5)%f = radius * real(sin(real(angle, real64)), real32)
         t(6)%f = cosine
         t(7)%f = sine
         t(8)%i = numsegs
         return
      end if

      cx = x + radius * real(cos(real(angle, real64)), real32)
      cy = y + radius * real(sin(real(angle, real64)), real32)
      call draw_move2(cx, cy)

      do i = 1, numsegs
         dx = cx - x
         dy = cy - y
         cx = x + dx * cosine - dy * sine
         cy = y + dx * sine + dy * cosine
         call draw_draw2(cx, cy)
      end do

      if (sync /= 0) then
         vdevice%sync = D_TRUE
         idum = vdevice%dev%Vsync()
      end if
   end subroutine draw_arc

!>
!!##NAME
!!    sector(3f) - [M_draw:ARCS] Draw a sector. Note: sectors are polygons.
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!         subroutine sector(x, y, radius, startang, endang)
!!         REAL x, y, radius, startang, endang
!!
!!##DESCRIPTION
!!
!!    Draw a sector. x, y, and radius are values in world units. Note: sectors
!!    are regarded as polygons, so if polyfill or polyhatch has been called
!!    with 1, the sectors will be filled or hatched accordingly.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_sector
!!    use M_draw
!!    use M_draw,    only  : D_BLACK,   D_WHITE
!!    use M_draw,    only  : D_RED,     D_GREEN,    D_BLUE
!!    use M_draw,    only  : D_YELLOW,  D_MAGENTA,  D_CYAN
!!    implicit none
!!    real           :: b=0.5
!!    real           :: R=4.9
!!    real           :: X, Y, A
!!    integer        :: key
!!       call prefsize(1000,200)
!!       call vinit(' ')
!!       call color(D_BLACK)
!!       call clear()
!!       call color(D_YELLOW)
!!       call page(-25.0-b, 25.0+b, -5.0-b, 5.0+b)
!!       call textsize(1.0,1.4)
!!       call font("futura.l")
!!       call centertext(.true.)
!!
!!       ! draw sectors with various start and end angles
!!
!!       call linewidth(150)
!!       call color(D_WHITE)
!!       X=-20; Y=0; R=5.0; A=0; B=30
!!       call sector(X,Y,R,A,B)
!!       call move2(X, 4.0)
!!       call linewidth(50)
!!       call drawstr("0 to 30 deg.")
!!
!!       call linewidth(150)
!!       call color(D_RED)
!!       X=-10; Y=0; R=5; A=0; B=-45
!!       call sector(X,Y,R,A,B)
!!       call move2(X,-4.0)
!!       call linewidth(50)
!!       call drawstr("0 to -45 deg.")
!!
!!       call polyfill(.true.)
!!
!!       call linewidth(150)
!!       call color(D_GREEN)
!!       X=-0; Y=0; R=5; A=100; B=200
!!       call sector(X,Y,R,A,B)
!!       call move2(X,-4.0)
!!       call linewidth(50)
!!       call drawstr("100 to 200 deg.")
!!
!!       call polyhatch(.true.)
!!       call hatchpitch(1.0/2.0)
!!       call hatchang(90.0)
!!
!!       call linewidth(150)
!!       call color(D_MAGENTA)
!!       X=10; Y=0; R=5; A=-30; B=30
!!       call sector(X,Y,R,A,B)
!!       call move2(X,-4.0)
!!       call linewidth(50)
!!       call drawstr("-30 to 30 deg.")
!!
!!       call hatchang(30.0)
!!       call linewidth(150)
!!       call color(D_CYAN)
!!       X=20; Y=0; R=5; A=45; B=-45
!!       call sector(X,Y,R,A,B)
!!       call move2(X, 4.0)
!!       call linewidth(50)
!!       call drawstr(" 45 to -45 deg.")
!!
!!       key=getkey()
!!       call vexit()
!!    end program demo_sector
   subroutine draw_sector(x, y, radius, startang, endang)
      real(real32), intent(in) :: x, y, radius, startang, endang
      type(Token), pointer :: t(:)
      real(real32) :: cx, cy, dx, dy
      real(real32) :: deltang, cosine, sine, angle
      integer :: sync, i, numsegs, inpoly

      if (vdevice%initialized == 0) call draw_verror("segment: draw not initialized")

      angle = startang * D2R
      numsegs = int(abs(endang - startang) / 360.0_real32 * nsegs + 0.5_real32)
      deltang = (endang - startang) * D2R / numsegs
      cosine = real(cos(real(deltang, real64)), real32)
      sine = real(sin(real(deltang, real64)), real32)

      if (vdevice%inobject /= 0) then
         t => draw_newtokens(8)
         t(1)%i = OBJ_SECTOR
         t(2)%f = x
         t(3)%f = y
         t(4)%f = radius * real(cos(real(angle, real64)), real32)
         t(5)%f = radius * real(sin(real(angle, real64)), real32)
         t(6)%f = cosine
         t(7)%f = sine
         t(8)%i = numsegs
         return
      end if

      sync = vdevice%sync
      if (sync /= 0) vdevice%sync = D_FALSE

      inpoly = vdevice%inpolygon
      if ((vdevice%attr%a%fill /= 0 .or. vdevice%attr%a%hatch /= 0) .and. inpoly == 0) then
         call draw_makepoly()
      end if

      call draw_move2(x, y)
      cx = x + radius * real(cos(real(angle, real64)), real32)
      cy = y + radius * real(sin(real(angle, real64)), real32)
      call draw_draw2(cx, cy)

      do i = 1, numsegs
         dx = cx - x
         dy = cy - y
         cx = x + dx * cosine - dy * sine
         cy = y + dx * sine + dy * cosine
         call draw_draw2(cx, cy)
      end do

      if ((vdevice%attr%a%fill /= 0 .or. vdevice%attr%a%hatch /= 0) .and. inpoly == 0) then
         call draw_closepoly()
      else
         if (sync /= 0) vdevice%sync = D_TRUE
         call draw_draw2(x, y)
      end if
   end subroutine draw_sector

!>
!!##NAME
!!    circle(3f) - [M_draw:ARCS] Draw a circle.
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!         subroutine circle(x, y, radius)
!!         real,intent(in) :: x
!!         real,intent(in) :: y
!!         real,intent(in) :: radius
!!
!!##DESCRIPTION
!!
!!    Draw a circle. x, y, and radius are values in world units.
!!
!!    Draw a circle using current line width and color
!!
!!    NOTE
!!
!!    circles are regarded as polygons, so if polyfill or polyhatch has been
!!    called with .true., the circle will be filled or hatched accordingly.
!!
!!##OPTIONS
!!    X,Y        Coordinates for the center of the circle
!!    RADIUS     Radius of the circle
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_circle
!!       use M_draw
!!       use M_draw,    only  : D_BLACK,   D_WHITE
!!       use M_draw,    only  : D_RED,     D_GREEN,    D_BLUE
!!       use M_draw,    only  : D_YELLOW,  D_MAGENTA,  D_CYAN
!!       implicit none
!!       real :: b=0.5
!!       real :: R=5
!!       integer :: ipaws
!!    ! set up drawing surface
!!       call prefsize(1000,200)
!!       call vinit(' ') ! start graphics using device $M_DRAW_DEVICE
!!       call page(-25.0-b,25.0+b,-R-b,R+b)
!!       call linewidth(200)
!!       call color(D_CYAN)
!!       call clear()
!!    ! draw circles with hatching and fill
!!       call color(D_WHITE)
!!       call circle(-20.0,0.0, R)
!!       call color(D_RED)
!!       call circle(-10.0,0.0, R)
!!       call polyfill(.true.)
!!       call color(D_GREEN)
!!       call circle(0.0,0.0,R)
!!       call linewidth(20)
!!       call polyhatch(.true.)
!!       call hatchpitch(1.0/2.0)
!!       call color(D_MAGENTA)
!!       call circle(10.0,0.0,R)
!!    ! layer calls to get a filled crosshatched circle
!!    ! first a solid colored circle
!!       call color(D_YELLOW)
!!       call polyfill(.true.)
!!       call circle(20.0,0.0,R)
!!
!!    ! draw hatch lines at 45 degrees
!!       call color(D_GREEN)
!!       call linewidth(80)
!!       call polyhatch(.true.)
!!       call hatchpitch(2.0/3.0)
!!       call hatchang(45.0)
!!       call circle(20.0,0.0,R)
!!    ! draw hatch lines at -45 degrees
!!       call hatchang(-45.0)
!!       call circle(20.0,0.0,R)
!!
!!    ! outline circle with a thick border
!!       call color(D_WHITE)
!!       call linewidth(160)
!!       call polyhatch(.false.)
!!       call circle(20.0,0.0,R)
!!
!!       ipaws=getkey()
!!    ! exit graphics mode
!!       call vexit()
!!    end program demo_circle
   subroutine draw_circle(x, y, radius)
      real(real32), intent(in) :: x, y, radius
      type(Token), pointer :: t(:)
      real(real32) :: cx, cy, dx, dy
      real(real32) :: angle, cosine, sine
      integer :: sync, i, inpoly, idum
      logical :: need_to_close

      if (vdevice%initialized == 0) call draw_verror("circle: draw not initialized")

      angle = 2.0_real32 * PI / nsegs
      cosine = real(cos(real(angle, real64)), real32)
      sine = real(sin(real(angle, real64)), real32)

      if (vdevice%inobject /= 0) then
         t => draw_newtokens(7)
         t(1)%i = OBJ_CIRCLE
         t(2)%f = x
         t(3)%f = y
         t(4)%f = radius
         t(5)%f = cosine
         t(6)%f = sine
         t(7)%i = nsegs
         return
      end if

      sync = vdevice%sync
      if (sync /= 0) vdevice%sync = D_FALSE

      cx = x + radius
      cy = y
      inpoly = vdevice%inpolygon

      if ((vdevice%attr%a%fill /= 0 .or. vdevice%attr%a%hatch /= 0) .and. inpoly == 0) then
         call draw_makepoly()
         need_to_close = .true.
      else
         need_to_close = .false.
      end if

      call draw_move2(cx, cy)
      do i = 1, nsegs - 1
         dx = cx - x
         dy = cy - y
         cx = x + dx * cosine - dy * sine
         cy = y + dx * sine + dy * cosine
         call draw_draw2(cx, cy)
      end do

      if (need_to_close) then
         call draw_closepoly()
      else
         call draw_draw2(x + radius, y)
      end if

      if (sync /= 0) then
         vdevice%sync = D_TRUE
         idum = vdevice%dev%Vsync()
      end if
   end subroutine draw_circle

end module m_draw_arcs
