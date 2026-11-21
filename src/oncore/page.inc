!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    page(3f) - [M_pixel] define the area of the virtual world coordinates
!!               to map to the viewport
!!    (LICENSE:MIT)
!!
!!##SYNOPSIS
!!
!!  definition:
!!
!!    subroutine page(left, right, bottom, top)
!!    real,intent(in) :: left, right, bottom, top
!!
!!##DESCRIPTION
!!    Defines the section of the virtual world coordinates to map to
!!    the viewport. Automatically use the largest viewport that provides
!!    one-to-one correspondence between the window and the viewport.
!!
!!##EXAMPLES
!!
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    MIT
subroutine page(xsmall,xlarge,ysmall,ylarge)
!use M_journal, only : journal

! ident_25="@(#) M_pixel page(3f) given a window size find and set to largest accommodating viewport"

real,intent(in) :: xsmall
real,intent(in) :: xlarge
real,intent(in) :: ysmall
real,intent(in) :: ylarge

real :: rps
real :: spr
real :: tryx
real :: tryy
real :: vhigh
real :: vwide
real :: xdelta
real :: xmax
real :: xmin
real :: xsplit
real :: ydelta
real :: ymax
real :: ymin
real :: ysplit

!
!     given a window size, and assuming a one-to-one correspondence of window
!     units (ie. an "x-unit" is as long as a "y-unit"), find the largest area
!     on the display surface that has the same aspect ratio, and set the
!     viewport to it.
!     assumes that the screen rasters are square.
!
      call draw_getdisplaysize(vwide,vhigh) !get screen size in terms of raster units
!
!     the default viewport is in "screen units", and goes from top-left of 0,0
!     to bottom-right of vwide,vhigh
!     all new viewports are defined in terms of this original viewport.
!
      rps=1.0                          ! number of rasters per screen unit
      spr=1.0                          ! number of screen units per raster
      tryx=vwide                       ! make as wide as display as a trial fit
      if(xlarge-xsmall.ne.0.0)then
         tryy=vwide*(ylarge-ysmall)/(xlarge-xsmall) ! calculate required height
      else                             ! ERROR: do something desperate
         call journal('*P_page* window has a zero X dimension')
         tryy=vhigh
      endif
      if(tryy.gt.vhigh)then ! if required height too great, fit with y maximized
         tryy=vhigh
         if(ylarge-ysmall.ne.0.0)then
            tryx=vhigh*(xlarge-xsmall)/(ylarge-ysmall)
         else                          ! ERROR: do something desperate
            call journal('*P_page* window has a zero Y dimension')
            tryx=vwide
         endif
      endif
!
!   tryx and tryy are now the required viewport in raster units. The raster
!   units now need converted to screen units to be used in viewport procedure
!
!   some explanation of physical viewport units is required:
!   assuming maximizing the required aspect ratio in the available drawing area,
!   and that the original viewport "origin" 0,0 stays in its original position,
!   and that the original -1,1,-1,1 viewport is the largest square that can fit
!   on the display, bottom left justified.
!   the screen coordinate system is a right-handed Cartesian coordinate system
!   with positive x to the viewer's right, positive y up.
!
!   at this point,
!    vwide=width in rasters of entire display
!    vhigh=height in rasters of entire display
!   assuming a square raster
!     tryx is desired width in rasters
!     tryy is desired height in rasters
!
      xdelta=tryx-2.0*rps  ! need this many more rasters in x direction from 1,1
      ydelta=tryy-2.0*rps  ! need this many more rasters in y direction from 1,1
      ! to center (to left bottom justify, make xsplit and ysplit 0)
      xsplit=(vwide-tryx)/2.0
      ysplit=(vhigh-tryy)/2.0
      xmax=1+xdelta*spr+xsplit*spr
      ymax=1+ydelta*spr+ysplit*spr
      xmin=-1+xsplit*spr
      ymin=-1+ysplit*spr
      !if(P_debug)then
      !   write(*,*)'max. display area is', vwide, ' by ',vhigh,' rasters'
      !   write(*,*)'shape is ',xsmall,xlarge,ysmall,ylarge
      !   write(*,*)'attempting to get a viewport of ',tryx,' by ',tryy
      !   write(*,*)'needed more rasters, ',xdelta,' by ',ydelta
      !   write(*,*)'resulted in viewport ',xmin,xmax,ymin,ymax
      !endif
      if(xmin.ne.xmax.and.ymin.ne.ymax)then
         call draw_viewport(xmin,xmax,ymax,ymin)
      else
       call journal('*P_page* window has zero dimension,no viewport set')
      endif
      if(xsmall.ne.xlarge.and.ysmall.ne.ylarge)then
         call draw_ortho2(xsmall,xlarge,ysmall,ylarge)
      else    ! ERROR: do something desperate
        call journal('*P_page* window has zero dimension, no window set')
      endif
contains

subroutine journal(message)
character(len=*),intent(in) :: message
   write(*,'(a)')trim(message)
end subroutine journal

end subroutine page
