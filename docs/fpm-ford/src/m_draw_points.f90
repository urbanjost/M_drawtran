module m_draw_points
   use, intrinsic :: iso_fortran_env, only: real32
   use m_draw_types
   use m_draw_verror
   use m_draw_move
   use m_draw_draw
   implicit none
   private
   public :: draw_point, draw_point2, draw_spoint2

contains

!>
!!##NAME
!!    point(3f) - [M_draw:POINT] Draw a point at x, y, z
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!         subroutine point(x, y, z)
!!         real,intent(in) :: x, y, z
!!##DESCRIPTION
!!    Draw a point at x, y, z. The size of the point is not affected by
!!    perspective. Draw a polygon if you want perspective to be applied.
!!    The size of the point is controlled by the current linewidth.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_point
!!    use M_draw
!!    use M_draw,    only  : D_BLACK,   D_WHITE
!!    use M_draw,    only  : D_RED,     D_GREEN,    D_BLUE
!!    use M_draw,    only  : D_YELLOW,  D_MAGENTA,  D_CYAN
!!    implicit none
!!    integer :: i
!!    integer :: ikey
!!       call prefsize(1000,200)
!!       call vinit(' ')
!!       call color(D_BLACK)
!!       call clear()
!!       call page(-25.0, 25.0, -5.0, 5.0)
!!
!!       ! draw points using various linewidths and colors
!!       do i=1,300
!!          call randpoint()
!!       enddo
!!
!!       ikey=getkey()
!!       call vexit()
!!
!!    contains
!!       subroutine randpoint()
!!          real :: r1, r2
!!          call random_number(r1)
!!          call random_number(r2)
!!          call linewidth(int(r1*500+200))
!!          call color(nint(r2*7))
!!          call point(r1*50.0-25.0,r2*10.0-5.0,0.0)
!!       end subroutine randpoint
!!
!!    end program demo_point
   subroutine draw_point(x, y, z)
      real(real32), intent(in) :: x, y, z
      if (vdevice%initialized == 0) call draw_verror("point: draw not initialized")
      call draw_move(x, y, z)
      call draw_draw(x, y, z)
   end subroutine draw_point

!>
!!##NAME
!!    point2(3f) - [M_draw:POINT] Draw a point at x, y.
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!         subroutine point2(x, y)
!!         real,intent(in) :: x, y
!!
!!##DESCRIPTION
!!    Draw a point at x, y. Points are device-dependent and may not appear
!!    at all. Generally points are drawn with the current color as a circle
!!    with a diameter equal to the current linewidth.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_point2
!!    use :: M_draw
!!    implicit none
!!    integer :: i
!!    integer :: ipaws
!!    call prefsize(300,300)
!!    call vinit(' ') ! start graphics using device $M_DRAW_DEVICE
!!    call ortho2(0.0, 20.0, 0.0, 20.0)
!!    call color(D_MAGENTA)
!!    do i=1,20
!!       call linewidth(20*i)
!!       call point2(real(i),real(i))
!!    enddo
!!    ipaws=getkey()
!!    call vexit()
!!    end program demo_point2
   subroutine draw_point2(x, y)
      real(real32), intent(in) :: x, y
      call draw_move(x, y, 0.0_real32)
      call draw_draw(x, y, 0.0_real32)
   end subroutine draw_point2

   subroutine draw_spoint2(xs, ys)
      real(real32), intent(in) :: xs, ys
      integer :: idum
      if (vdevice%initialized == 0) call draw_verror("spoint2: draw not initialized")

      vdevice%cpVx = int((xs + 0.5_real32) * (vdevice%maxVx - vdevice%minVx))
      vdevice%cpVy = int((ys + 0.5_real32) * (vdevice%maxVy - vdevice%minVy))
      idum = vdevice%dev%Vdraw(vdevice%cpVx, vdevice%cpVy)
   end subroutine draw_spoint2

end module m_draw_points
