module m_draw_getgp
   use, intrinsic :: iso_fortran_env, only: real32
   use m_draw_types
   use m_draw_matrix
   use m_draw_mapping
   implicit none
   private
   public :: draw_getgp, draw_getgp2, draw_getgpt, draw_sgetgp2

contains

!>
!!##NAME
!!    getgp(3f) - [M_draw:POSITION] Gets the current graphics position
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!          subroutine getgp(x, y, z)
!!          real x, y, z
!!##DESCRIPTION
!!
!!    Gets the current graphics position in world coords.
   subroutine draw_getgp(x, y, z)
      real(real32), intent(out) :: x, y, z
      x = vdevice%cpW(D_X)
      y = vdevice%cpW(D_Y)
      z = vdevice%cpW(D_Z)
   end subroutine draw_getgp

!>
!!##NAME
!!    getgp2(3f) - [M_draw:POSITION] Gets the current graphics position in world coordinates
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!          subroutine getgp2(x, y)
!!          real,intent(out) :: x,y
!!##DESCRIPTION
!!
!!    Gets the current graphics position in world coords.
!!
!!##RETURNS
!!    X  X coordinate of current position
!!    Y  Y coordinate of current position
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!      program demo_getgp2
!!      use M_draw
!!      implicit none
!!      real :: X,Y
!!      call prefsize(20,20)
!!      call vinit(' ') ! start graphics using device $M_DRAW_DEVICE
!!      call ortho2(-100.0,100.0,-100.0,100.0)
!!      call move2(0.0,0.0)
!!      call draw2(96.5,98.333)
!!
!!      call getgp2(X,Y)
!!      write(*,*)'CURRENT POSITION (X,Y)=',X,Y
!!
!!      call vexit()
!!      end program demo_getgp2
!!
!!   Results
!!
!!    CURRENT POSITION (X,Y)=   96.5000000       98.3330002
   subroutine draw_getgp2(x, y)
      real(real32), intent(out) :: x, y
      x = vdevice%cpW(D_X)
      y = vdevice%cpW(D_Y)
   end subroutine draw_getgp2

!>
!!##NAME
!!    getgpt(3f) - [M_draw:POSITION] Gets the current transformed graphics position in world coords.
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!          subroutine getgpt(x, y, z, w)
!!          real x, y, z, w
!!##DESCRIPTION
!!
!!    Gets the current transformed graphics position in world coords.
   subroutine draw_getgpt(x, y, z, w)
      real(real32), intent(out) :: x, y, z, w
      call draw_multvector(vdevice%cpWtrans, vdevice%cpW, vdevice%transmat%m)
      x = vdevice%cpWtrans(D_X)
      y = vdevice%cpWtrans(D_Y)
      z = vdevice%cpWtrans(D_Z)
      w = vdevice%cpWtrans(D_W)
   end subroutine draw_getgpt

!>
!!##NAME
!!    sgetgp2(3f) - [M_draw:POSITION] Gets the current screen graphics position in screen coords (-1 to 1)
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!          subroutine sgetgp2(x, y)
!!          real x, y
!!
!!##DESCRIPTION
!!    Gets the current screen graphics position in screen coords (-1 to 1)
   subroutine draw_sgetgp2(x, y)
      real(real32), intent(out) :: x, y
      real(real32) :: sx, sy

      sx = real(vdevice%maxVx - vdevice%minVx, real32)
      sy = real(vdevice%maxVy - vdevice%minVy, real32)

      call draw_multvector(vdevice%cpWtrans, vdevice%cpW, vdevice%transmat%m)
      x = 2.0_real32 * real(draw_WtoVx(vdevice%cpWtrans), real32) / sx - 1.0_real32
      y = 2.0_real32 * real(draw_WtoVy(vdevice%cpWtrans), real32) / sy - 1.0_real32
   end subroutine draw_sgetgp2

end module m_draw_getgp
