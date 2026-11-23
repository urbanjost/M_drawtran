module m_draw_draw
   use, intrinsic :: iso_fortran_env, only: real32
   use m_draw_types
   use m_draw_verror
   use m_draw_matrix
   use m_draw_mapping
   use m_draw_clip
   implicit none
   private
   public :: draw_dashcode, draw_linestyle, draw_dashline, draw_draw, &
             draw_draw2, draw_rdraw, draw_rdraw2, draw_sdraw2, draw_rsdraw2

   logical, save :: do_dash = .true.

   interface
      function draw_newtokens(num) result(toks)
         import :: Token
         integer, intent(in) :: num
         type(Token), pointer :: toks(:)
      end function draw_newtokens
   end interface

contains

   logical function is_near(a, b)
      real(real32), intent(in) :: a, b
      is_near = (((a - b) / a) < 0.0001_real32)
   end function is_near

   logical function checkdash(line)
      type(Attribute), intent(inout) :: line
      line%dash_pos = line%dash_pos + 1
      if (line%dash_pos > len(line%style)) then
         line%dash_pos = 1
      end if
      checkdash = (line%style(line%dash_pos:line%dash_pos) /= '0')
   end function checkdash

!>
!!##NAME
!!    dashcode(3f) - [M_draw:LINESTYLE] set dash pattern length
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!         subroutine dashcode(dashlen)
!!         real dashlen
!!
!!##DESCRIPTION
!!
!!    Set the current dash length (in world units) to be dashlen.
!!
!!##IMAGE
!!
!!    The sample graphic shows a line segment being drawn using the same
!!    linestyle except the dashcode is being changed. Note that the dashcode
!!    is in world units,
!!
!!##EXAMPLE
!!
!!  Sample program:
!!
!!   program demo_dashcode
!!   use M_draw
!!   implicit none
!!   integer        :: icolor
!!   integer        :: ikey
!!   real,parameter :: b=0.5
!!   real           :: x, y
!!   real           :: dcode
!!      call prefsize(1000,200)
!!      call vinit(' ')
!!      call color(D_BLACK)
!!      call clear()
!!      call page(-25.0-b, 25.0+b, -5.0-b, 5.0+b)
!!      call textsize(2.0, 2.4)
!!      call font("futura.m")
!!      call ycentertext()
!!      icolor=7; dcode=0.010/5.0; X=-23.0; Y=3.5  ; call line()
!!      icolor=1; dcode=0.025/5.0; X=-23.0; Y=-0.0 ; call line()
!!      icolor=2; dcode=0.030/5.0; X=-23.0; Y=-3.5 ; call line()
!!      icolor=4; dcode=0.050/5.0; X=0.0;   Y=3.5  ; call line()
!!      icolor=5; dcode=0.075/5.0; X=0.0;   Y=-0.0 ; call line()
!!      icolor=6; dcode=0.100/5.0; X=0.0;   Y=-3.5 ; call line()
!!      call linewidth(200)
!!      call color(D_BLUE)
!!      call move2(-25.0, -5.0)
!!      call draw2(-25.0,  5.0)
!!      call draw2(25.0,   5.0)
!!      call draw2(25.0,  -5.0)
!!      call draw2(-25.0, -5.0)
!!      ikey=getkey()
!!      call vexit()
!!   contains
!!   subroutine line
!!   character(len=6) :: string
!!      call linestyle('11100100')
!!      call dashcode(dcode)
!!      call color(icolor)
!!      call linewidth(70)
!!      call move2(X,Y)
!!      call rdraw2(10.0, 0.0)
!!      call rmove2(3.0, 0.0)
!!      call linestyle('')
!!      call linewidth(180)
!!      call color(D_WHITE)
!!      write(string,'(f6.3)')dcode
!!      call drawstr (string)
!!   end subroutine line
!!   end program demo_dashcode
   subroutine draw_dashcode(d_in)
      real(real32), intent(in) :: d_in
      real(real32) :: d
      type(Token), pointer :: tok(:)

      d = abs(d_in)
      if (d == 0.0_real32) d = 0.1_real32

      if (vdevice%inobject /= 0) then
         tok => draw_newtokens(2)
         tok(1)%i = OBJ_DASHCODE
         tok(2)%f = d
         return
      end if

      vdevice%attr%a%dash = d
   end subroutine draw_dashcode

!>
!!##NAME
!!     linestyle(3f) - [M_draw:LINESTYLE] set the line dash pattern
!!     (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine linestyle(style)
!!    character *(*) style
!!
!!##DESCRIPTION
!!
!!     Set the current linestyle to style. Linestyles are specified by giving
!!     a nominal length of a single dash and a character string consisting
!!     of 1's and 0's (zeros) that specify when to draw a dash and when not
!!     to draw a dash. "1" is for a dash , "0" is for a gap. Linestyles will
!!     follow curves and "go around" corners.
!!
!!     To reset to a solid line style, enter a linestyle of " ". If a
!!     linestyle is set or reset, the accumulated information as to where
!!     on a curve (or line) a dash is to be draw is also reset.
!!
!!     The sample program shows a line segment being drawn using the same
!!     dashcode (to specify dash length) except the linestyle is being
!!     changed. Note that the dashcode is in world units.
!!
!!##EXAMPLE
!!
!!  Sample program:
!!
!!     program demo_linestyle
!!     ! A program showing basic line styles.
!!     use M_draw
!!     implicit none
!!     character(len=40) :: device
!!     integer           :: ios, idum, i
!!
!!        print*,'Enter output device: '
!!        read(*,'(a)',iostat=ios)device
!!        if(ios.ne.0)device=' '
!!
!!        call prefsize(1000,1000)
!!        call vinit(device)
!!        call vsetflush(.true.)
!!        call up(0.0, 1.0, 0.0)
!!        call perspective(90.0, 1.0, 0.3, 3.0)
!!        call translate(0.0, 0.0, -1.3)
!!        call linewidth(30)
!!
!!        call drawscene()
!!        call rotate(-30.0, 'y')
!!        call rotate(-30.0, 'x')
!!        call drawscene()
!!
!!        call vexit()
!!        contains
!!
!!        subroutine drawscene()
!!
!!           call color(D_BLACK)
!!           call clear()
!!
!!           call color(D_GREEN)
!!           call dashcode(0.03)
!!
!!           call linestyle(' ')
!!           call xcentertext()
!!           call move2(-0.45, 0.9)
!!           call drawstr('Linestyle: "10"')
!!           call move2(-0.45, 0.7)
!!           call drawstr('Linestyle: "110"')
!!           call move2(-0.45, 0.5)
!!           call drawstr('Linestyle: "111010"')
!!           call move2(-0.45, 0.3)
!!           call drawstr('Linestyle: "0001"')
!!
!!           call linestyle('10')
!!           call move2(-0.9, 0.9)
!!           call draw2( 0.0, 0.9)
!!           call circle(0.6, 0.6, 0.4)
!!
!!           call drawbox(0.9)
!!           call drawsine(0.9)
!!
!!           call color(D_RED)
!!           call linestyle('110')
!!           call move2(-0.9, 0.7)
!!           call draw2( 0.0, 0.7)
!!           call circle(0.6, 0.6, 0.3)
!!           call drawbox(0.7)
!!           call drawsine(0.7)
!!
!!           call color(D_CYAN)
!!           call linestyle('111010')
!!           call move2(-0.9, 0.5)
!!           call draw2( 0.0, 0.5)
!!           call circle(0.6, 0.6, 0.2)
!!           call drawbox(0.5)
!!           call drawsine(0.5)
!!
!!           call color(D_YELLOW)
!!           call linestyle('0001')
!!           call move2(-0.9, 0.3)
!!           call draw2( 0.0, 0.3)
!!           call circle(0.6, 0.6, 0.1)
!!           call drawbox(0.3)
!!           call drawsine(0.3)
!!
!!           idum=getkey()
!!
!!           end subroutine drawscene
!!
!!           subroutine drawbox(scl)
!!           real :: scl
!!
!!           call pushmatrix()
!!
!!           call rotate(30.0, 'x')
!!           call rotate(60.0, 'y')
!!           call translate(-0.7, -1.2, 0.0)
!!           call scale(scl, scl, scl)
!!
!!           call move(0.0, 0.0, 0.0)
!!
!!           call draw(1.0, 0.0, 0.0)
!!           call draw(1.0, 1.0, 0.0)
!!           call draw(0.0, 1.0, 0.0)
!!           call draw(0.0, 0.0, 0.0)
!!
!!           call draw(0.0, 0.0, -1.0)
!!           call draw(1.0, 0.0, -1.0)
!!           call draw(1.0, 1.0, -1.0)
!!           call draw(0.0, 1.0, -1.0)
!!           call draw(0.0, 0.0, -1.0)
!!
!!           call move(0.0, 1.0, -1.0)
!!           call draw(0.0, 1.0, 0.0)
!!
!!           call move(1.0, 1.0, 0.0)
!!           call draw(1.0, 1.0, -1.0)
!!
!!           call move(1.0, 0.0, 0.0)
!!           call draw(1.0, 0.0, -1.0)
!!
!!           call popmatrix()
!!
!!           end subroutine drawbox
!!
!!           subroutine drawsine(s)
!!           real    s, RAD, AMP
!!           parameter(RAD = 0.5, AMP = 0.04)
!!           real    a, x, y, z
!!
!!           call pushmatrix()
!!
!!           call translate(RAD + 0.2, -0.5, 0.0)
!!           call scale(s, s, s)
!!
!!           call move(RAD, 0.0, 0.0)
!!           a=0.0
!!           do i = 0, 2*314, 2
!!              x = RAD * cos(a)
!!              z = RAD * sin(a)
!!              y = AMP * sin(a * 6.0)
!!
!!              call draw(x, y, z)
!!              a=a+0.02
!!           enddo
!!           call popmatrix()
!!           end subroutine drawsine
!!     end program demo_linestyle
   subroutine draw_linestyle(l)
      character(len=*), intent(in) :: l
      type(Attribute), pointer :: line
      type(Token), pointer :: tok(:)
      integer :: i

      if (vdevice%inobject /= 0) then
         tok => draw_newtokens(2)
         tok(1)%i = OBJ_LINESTYLE
         tok(2)%s = l
         return
      end if

      line => vdevice%attr%a

      if (len(l) == 0) then
         if (allocated(line%style)) deallocate(line%style)
      else
         do i = 1, len(l)
            if (l(i:i) == '0' .or. l(i:i) == ' ') then
               line%style = l
               line%dash_pos = 1
               line%adist = 0.0_real32
               do_dash = (line%style(1:1) /= '0')
               return
            end if
         end do
         if (allocated(line%style)) deallocate(line%style)
      end if
   end subroutine draw_linestyle

   subroutine draw_dashline(p0, p1)
      real(real32), intent(inout) :: p0(4), p1(4)
      integer :: vx, vy, sync, idum
      real(real32) :: dx, dy, dz, dw, dist, ldist, tdist
      real(real32) :: pd(4)
      type(Attribute), pointer :: line

      line => vdevice%attr%a
      if (line%dash == 0.0_real32) then
         vx = draw_WtoVx(p1)
         vy = draw_WtoVy(p1)
         idum = vdevice%dev%Vdraw(vx, vy)
         vdevice%cpVx = vx
         vdevice%cpVy = vy
         return
      end if

      dx = p1(D_X) - p0(D_X)
      dy = p1(D_Y) - p0(D_Y)
      dz = p1(D_Z) - p0(D_Z)
      dw = p1(D_W) - p0(D_W)
      ldist = sqrt(dx*dx + dy*dy + dz*dz + dw*dw)

      if (ldist <= (line%dash - line%adist)) then
         if (is_near(line%dash, line%adist)) then
            line%adist = 0.0_real32
            do_dash = checkdash(line)
         end if
         line%adist = line%adist + ldist

         vx = draw_WtoVx(p1)
         vy = draw_WtoVy(p1)
         if (do_dash) idum = vdevice%dev%Vdraw(vx, vy)

         vdevice%cpVx = vx
         vdevice%cpVy = vy
         return
      else
         sync = vdevice%sync
         if (sync /= 0) vdevice%sync = 0

         tdist = 0.0_real32
         call draw_copyvector(pd, p0)

         if (line%adist > 0.0_real32) then
            tdist = line%dash - line%adist
            if (is_near(line%dash, line%adist)) then
               line%adist = 0.0_real32
               do_dash = checkdash(line)
            end if
            line%adist = line%adist + tdist

            dist = tdist / ldist
            pd(D_X) = pd(D_X) + dx * dist
            pd(D_Y) = pd(D_Y) + dy * dist
            pd(D_Z) = pd(D_Z) + dz * dist
            pd(D_W) = pd(D_W) + dw * dist
            vx = draw_WtoVx(pd)
            vy = draw_WtoVy(pd)

            if (do_dash) idum = vdevice%dev%Vdraw(vx, vy)
            vdevice%cpVx = vx
            vdevice%cpVy = vy
         end if

         dx = dx * (line%dash / ldist)
         dy = dy * (line%dash / ldist)
         dz = dz * (line%dash / ldist)
         dw = dw * (line%dash / ldist)
         dist = line%dash

         do while (tdist <= ldist - dist)
            if (is_near(line%dash, line%adist)) then
               line%adist = 0.0_real32
               do_dash = checkdash(line)
            end if

            pd(D_X) = pd(D_X) + dx
            pd(D_Y) = pd(D_Y) + dy
            pd(D_Z) = pd(D_Z) + dz
            pd(D_W) = pd(D_W) + dw

            vx = draw_WtoVx(pd)
            vy = draw_WtoVy(pd)

            line%adist = line%adist + dist
            tdist = tdist + dist

            if (do_dash) idum = vdevice%dev%Vdraw(vx, vy)
            vdevice%cpVx = vx
            vdevice%cpVy = vy
         end do

         if (is_near(line%dash, line%adist)) then
            line%adist = 0.0_real32
            do_dash = checkdash(line)
         end if

         dx = p1(D_X) - pd(D_X)
         dy = p1(D_Y) - pd(D_Y)
         dz = p1(D_Z) - pd(D_Z)
         dw = p1(D_W) - pd(D_W)
         dist = sqrt(dx*dx + dy*dy + dz*dz + dw*dw)

         line%adist = line%adist + dist
         vx = draw_WtoVx(p1)
         vy = draw_WtoVy(p1)

         if (do_dash) idum = vdevice%dev%Vdraw(vx, vy)
         vdevice%cpVx = vx
         vdevice%cpVy = vy
      end if

      if (sync /= 0) then
         vdevice%sync = 1
         idum = vdevice%dev%Vsync()
      end if
   end subroutine draw_dashline

!>
!!##NAME
!!    draw(3f) - [M_draw:DRAW] Draw from current graphics position to (x, y, z)
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!          subroutine draw(x, y, z)
!!          real x, y, z
!!##DESCRIPTION
!!
!!    Draw from current graphics position to (x, y, z). (x, y, z) is a
!!    point in world coordinates.
   subroutine draw_draw(x, y, z)
      real(real32), intent(in) :: x, y, z
      type(Token), pointer :: tok(:)
      integer :: vx, vy, idum
      real(real32) :: res(4)

      if (vdevice%initialized == 0) then
         call draw_verror("draw: draw not initialized")
      end if

      if (vdevice%inpolygon /= 0) then
         call vdevice%pdraw(x, y, z)
         vdevice%cpW(D_X) = x
         vdevice%cpW(D_Y) = y
         vdevice%cpW(D_Z) = z
         vdevice%cpVvalid = 0
         return
      end if

      if (vdevice%inobject /= 0) then
         tok => draw_newtokens(4)
         tok(1)%i = OBJ_DRAW
         tok(2)%f = x
         tok(3)%f = y
         tok(4)%f = z

         vdevice%cpW(D_X) = x
         vdevice%cpW(D_Y) = y
         vdevice%cpW(D_Z) = z
         vdevice%cpVvalid = 0
         return
      end if

      if (vdevice%cpVvalid == 0) then
         call draw_multvector(vdevice%cpWtrans, vdevice%cpW, vdevice%transmat%m)
      end if

      vdevice%cpW(D_X) = x
      vdevice%cpW(D_Y) = y
      vdevice%cpW(D_Z) = z
      call draw_multvector(res, vdevice%cpW, vdevice%transmat%m)

      if (vdevice%clipoff /= 0) then
         vx = draw_WtoVx(res)
         vy = draw_WtoVy(res)

         if (allocated(vdevice%attr%a%style)) then
            call draw_dashline(vdevice%cpWtrans, res)
            vdevice%cpVvalid = 0
            return
         end if

         idum = vdevice%dev%Vdraw(vx, vy)
         vdevice%cpVx = vx
         vdevice%cpVy = vy
         vdevice%cpVvalid = 0
      else
         if (vdevice%cpVvalid /= 0) then
            call draw_quickclip(vdevice%cpWtrans, res)
         else
            call draw_clip(vdevice%cpWtrans, res)
         end if
      end if

      vdevice%cpWtrans = res
   end subroutine draw_draw

!>
!!##NAME
!!    draw2(3f) - [M_draw:DRAW] Draw from current graphics position to given point (x, y)
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!     subroutine draw2(x, y)
!!     real,intent(in) :: x, y
!!
!!##DESCRIPTION
!!    Draw from current position to specified point using current
!!    color and line width. Updates current position to new point.
!!    (x, y) is a point in world coordinates.
!!
!!##OPTIONS
!!    X  new X position
!!    Y  new Y position
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_draw2
!!    use M_draw, only: prefsize, vinit, ortho2, clear, getkey
!!    use M_draw, only: move2, draw2, vexit, color, linewidth
!!    use M_draw, only: D_BLACK, D_MAGENTA! , &
!!    !        D_RED, D_GREEN, D_BLUE, D_YELLOW, D_WHITE, D_CYAN
!!    !
!!    ! The Archimedean spiral is the locus of points corresponding
!!    ! to the locations over time of a point moving away from a
!!    ! fixed point with a constant speed along a line which rotates
!!    ! with constant angular velocity.
!!    !    r=A+B*theta
!!    ! Changing the parameter A will turn the spiral,
!!    ! while B controls the distance between successive turnings.
!!    !
!!    implicit none
!!    integer        :: i
!!    real           :: x, y, radius, theta
!!    real, parameter :: rotate = 0.0, gap = 2.0
!!    integer        :: ipaws
!!
!!       call prefsize(400, 400)
!!       call vinit(' ') ! start graphics using device $M_DRAW_DEVICE
!!       call ortho2(-150.0, 150.0, -150.0, 150.0)
!!       call color(D_MAGENTA)
!!       call clear()
!!       call move2(0.0, 0.0)
!!       call color(D_BLACK)
!!       call linewidth(40)
!!       do i = 0, 360*10, 5
!!          theta = d2r(real(i))
!!          ! equation in polar coordinates
!!          radius = rotate + gap*theta
!!          ! convert polar coordinates to cartesian
!!          call polar_to_cartesian(radius, theta, x, y)
!!          ! draw from current position to end of next segment
!!          call draw2(x, y)
!!       end do
!!       ipaws = getkey()
!!       ! exit graphics mode
!!       call vexit()
!!    contains
!!    !
!!    elemental real function d2r(degrees)
!!    real, intent(in) :: degrees
!!    real, parameter  :: Deg_Per_Rad = 57.2957795130823208767981548
!!       d2r = degrees/Deg_Per_Rad
!!    end function d2r
!!    !
!!    subroutine polar_to_cartesian(radius, inclination, x, y)
!!    real, intent(in) :: radius, inclination
!!    real, intent(out)  :: x, y
!!       if (radius == 0) then
!!          x = 0.0
!!          y = 0.0
!!       else
!!          x = radius*cos(inclination)
!!          y = radius*sin(inclination)
!!       end if
!!    end subroutine polar_to_cartesian
!!    !
!!    end program demo_draw2
   subroutine draw_draw2(x, y)
      real(real32), intent(in) :: x, y
      if (vdevice%initialized == 0) call draw_verror("draw2: draw not initialized")
      call draw_draw(x, y, 0.0_real32)
   end subroutine draw_draw2

!>
!!##NAME
!!    rdraw(3f) - [M_draw:DRAW] Relative draw
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!     subroutine rdraw(deltax, deltay, deltaz)
!!     real deltax, deltay, deltaz
!!
!!##DESCRIPTION
!!    Relative draw. deltax, deltay, and deltaz are offsets in world units.
   subroutine draw_rdraw(dx, dy, dz)
      real(real32), intent(in) :: dx, dy, dz
      if (vdevice%initialized == 0) call draw_verror("rdraw: draw not initialized")
      call draw_draw(vdevice%cpW(D_X) + dx, vdevice%cpW(D_Y) + dy, vdevice%cpW(D_Z) + dz)
   end subroutine draw_rdraw

!>
!!##NAME
!!    rdraw2(3f) - [M_draw:DRAW] Relative draw from current position to given point
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!          subroutine rdraw2(deltax, deltay)
!!          real,intent(in) :: deltax, deltay
!!
!!##DESCRIPTION
!!    Relative draw from current position to specified point using current
!!    color and line width. Updates current position to new point.
!!    (x, y) is a point in world coordinates.
!!
!!##OPTIONS
!!    deltax and deltay are offsets in world units.
!!
!!    X  new X position
!!    Y  new Y position
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!      program demo_rdraw2
!!      use M_draw, only: vinit, prefsize, ortho2,linewidth,getkey
!!      use M_draw, only: clear, move2, rdraw2, vexit,color
!!      use M_draw,    only  : D_BLACK,   D_WHITE
!!      use M_draw,    only  : D_RED,     D_GREEN,    D_BLUE
!!      use M_draw,    only  : D_YELLOW,  D_MAGENTA,  D_CYAN
!!      implicit none
!!      integer :: ipaws
!!
!!      call prefsize(200,200)
!!      call vinit(' ') ! start graphics using device $M_DRAW_DEVICE
!!      call ortho2(-55.0, 55.0, -55.0, 55.0)
!!      call linewidth(400)
!!      call color(D_WHITE)
!!      call clear()
!!
!!      call color(D_RED)
!!      call move2(-50.0,0.0)
!!      call square(50.0)
!!
!!      call linewidth(200)
!!      call color(D_GREEN)
!!      call move2(  0.0,-50.0)
!!      call square(50.0)
!!
!!      ipaws=getkey()
!!      call vexit()
!!
!!      contains
!!
!!      subroutine square(side)
!!      real,intent(in) :: side
!!      call rdraw2( side,   0.0)
!!      call rdraw2(  0.0,  side)
!!      call rdraw2(-side,   0.0)
!!      call rdraw2(  0.0, -side)
!!      end subroutine square
!!
!!      end program demo_rdraw2
   subroutine draw_rdraw2(dx, dy)
      real(real32), intent(in) :: dx, dy
      if (vdevice%initialized == 0) call draw_verror("rdraw2: draw not initialized")
      call draw_draw(vdevice%cpW(D_X) + dx, vdevice%cpW(D_Y) + dy, 0.0_real32)
   end subroutine draw_rdraw2

!>
!!##NAME
!!    sdraw2(3f) - [M_draw:DRAW] Draw in screen coordinates (-1.0 to 1.0).
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!          subroutine sdraw2(x, y)
!!          real x, y
!!##DESCRIPTION
!!    Draw in screen coordinates (-1.0 to 1.0).
   subroutine draw_sdraw2(xs, ys)
      real(real32), intent(in) :: xs, ys
      integer :: nx, ny, idum
      if (vdevice%initialized == 0) call draw_verror("sdraw2: draw not initialized")

      nx = int((xs / 2.0_real32 + 0.5_real32) * vdevice%sizeX)
      ny = int((0.5_real32 + ys / 2.0_real32) * vdevice%sizeY)

      idum = vdevice%dev%Vdraw(nx, ny)
      vdevice%cpVx = nx
      vdevice%cpVy = ny
      vdevice%cpVvalid = 0
   end subroutine draw_sdraw2

!>
!!##NAME
!!    rsdraw2(3f) - [M_draw:DRAW] Relative draw in screen units (-1.0 to 1.0).
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!          subroutine rsdraw2(deltax, deltay)
!!          real deltax, deltay
!!
!!##DESCRIPTION
!!    Relative sdraw2. delatx and deltay are in screen units (-1.0 to 1.0).
   subroutine draw_rsdraw2(dxs, dys)
      real(real32), intent(in) :: dxs, dys
      integer :: ndx, ndy, idum
      if (vdevice%initialized == 0) call draw_verror("rsdraw2: draw not initialized")

      ndx = int(dxs * vdevice%sizeSx / 2.0_real32)
      ndy = int(dys * vdevice%sizeSy / 2.0_real32)

      idum = vdevice%dev%Vdraw(vdevice%cpVx + ndx, vdevice%cpVy + ndy)
      vdevice%cpVx = vdevice%cpVx + ndx
      vdevice%cpVy = vdevice%cpVy + ndy
   end subroutine draw_rsdraw2

end module m_draw_draw
