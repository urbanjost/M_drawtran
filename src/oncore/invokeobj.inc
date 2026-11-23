!----------------------------------------------------------------------------------------------------------------------------------!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!----------------------------------------------------------------------------------------------------------------------------------!
!>
!!##NAME
!!    invokeobj(3f) - [M_draw] invoke object with specified transformations
!!    (LICENSE:MIT)
!!
!!##SYNOPSIS
!!
!!    subroutine invokeobj(xt,yt,zt,xs,ys,zs,xr,yr,zr,iobject)
!!
!!     real,intent(in)    :: xt,yt,zt
!!     real,intent(in)    :: xs,ys,zs
!!     real,intent(in)    :: xr,yr,zr
!!     integer,intent(in) :: iobject
!!
!!##DESCRIPTION
!!    save and restore the coordinate system while invoking an object with
!!    specified translation, rotation, and scaling.
!!
!!##OPTIONS
!!    xt,yt,zt    linear transforms
!!    xs,ys,zs    scaling
!!    xr,yr,zr    rotation in degrees
!!    iobject     object to invoke
!!##EXAMPLES
!!
!!  Sample program
!!
!!    program demo_invokeobj
!!    use M_engine
!!    implicit none
!!    real :: a, angle, step
!!    integer :: i, idum
!!    ! set window size
!!       call draw_prefsize(700,700)
!!       call draw_prefposition( 0, 0)
!!       call draw_vinit ('X11')
!!       a=1.0
!!    ! make an object to draw ( a disk with an arrow on it)
!!       call draw_makeobj(12345)
!!       call draw_polyfill(.TRUE.)
!!       call draw_color( 5)
!!       call draw_circle( 0.0, 0.0, a)
!!       call draw_color( 3)
!!       call draw_makepoly()
!!       call draw_move2( 0.00*a, 0.80*a)
!!       call draw_draw2( 0.50*a, 0.30*a)
!!       call draw_draw2( 0.20*a, 0.30*a)
!!       call draw_draw2( 0.20*a,-0.80*a)
!!       call draw_draw2(-0.20*a,-0.80*a)
!!       call draw_draw2(-0.20*a, 0.30*a)
!!       call draw_draw2(-0.50*a, 0.30*a)
!!       call draw_draw2( 0.00*a, 0.80*a)
!!       call draw_closepoly()
!!       call draw_polyfill(.FALSE.)
!!       call draw_color(7)
!!       call draw_linewidth(20)
!!       call draw_circleprecision(200)
!!       call draw_circle( 0.0, 0.0, a)
!!       call draw_vflush()
!!       call draw_closeobj()
!!    ! draw the disk invoking different rotation
!!       ANGLE=0.0
!!       STEP=0.4
!!       idum=draw_backbuffer()
!!       idum=-1
!!       if(idum.ne.-1)then
!!          do i=1,int(360/STEP*10)
!!             idum=draw_backbuffer()
!!             call draw_clear()
!!             call draw_invokeobj(0.0,0.0,0.0,1.0,1.0,1.0,ANGLE,ANGLE,ANGLE,12345)
!!             ANGLE=ANGLE+STEP
!!             call draw_swapbuffers()
!!          enddo
!!       else
!!          ANGLE=45.0
!!          call draw_invokeobj(0.0,0.0,0.0,1.0,1.0,1.0,ANGLE,ANGLE,ANGLE,12345)
!!          idum=draw_getkey()
!!       endif
!!       call draw_vexit()
!!    end program demo_invokeobj
!!##AUTHO
!!    John S. Urban
!!##LICENSE
!!    MIT
subroutine invokeobj(xt,yt,zt,xs,ys,zs,xr,yr,zr,iobject)

! ident_3="@(#) M_draw invokeobj(3f) invoke object with specified transformation applied and then restored"

real,intent(in)    :: xt,yt,zt  ! linear transforms
real,intent(in)    :: xs,ys,zs  ! scaling
real,intent(in)    :: xr,yr,zr  ! rotation
integer,intent(in) :: iobject

   call draw_pushmatrix()
   call draw_translate(xt,yt,zt)
   call draw_scale(xs,ys,zs)
   call draw_rotate(xr,'x')
   call draw_rotate(yr,'y')
   call draw_rotate(zr,'z')
   call draw_callobj(iobject)
   call draw_popmatrix()

end subroutine invokeobj
