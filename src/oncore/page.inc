!----------------------------------------------------------------------------------------------------------------------------------!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!----------------------------------------------------------------------------------------------------------------------------------!
!>
!!##NAME
!!    page(3f) - [M_draw] set window into largest viewport available
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine page(xsmall,xlarge,ysmall,ylarge)
!!    real, intent=(in) :: xsmall
!!    real, intent=(in) :: xlarge
!!    real, intent=(in) :: ysmall
!!    real, intent=(in) :: ylarge
!!
!!    subroutine page(xsize,ysize,icolor)
!!    real, intent=(in)    :: xsize
!!    real, intent=(in)    :: ysize
!!    integer, intent=(in) :: icolor
!!
!!##DESCRIPTION
!!    FORM SUBROUTINE PAGE(XSMALL,XLARGE,YSMALL,YLARGE)
!!
!!    Set the window to the rectangle defined by the corner points
!!    <xsmall,ysmall> and <xlarge,ylarge>.
!!
!!    Also, given the window size, and assuming a one-to-one correspondence
!!    of window units (ie. an "x-unit" is as long as a "y-unit"), find the
!!    largest area on the display surface that has the same aspect ratio,
!!    and set the viewport to it.
!!
!!    FORM SUBROUTINE PAGE(XSIZE,YSIZE,ICOLOR)
!!
!!    Size the window to the rectangle defined by the corner points
!!    <0.0,0.0> and <xsize,ysize> and the viewport to the largest centered
!!    area that has the same aspect ratio, and set the background color to
!!    the value mapped to color ICOLOR.
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!    program demo_page
!!    use M_draw
!!    use M_draw,    only  : D_BLACK,   D_WHITE
!!    use M_draw,    only  : D_RED,     D_GREEN,    D_BLUE
!!    use M_draw,    only  : D_YELLOW,  D_MAGENTA,  D_CYAN
!!    implicit none
!!    integer :: ipaws
!!    real,parameter :: radius=25.0
!!       call prefsize(600,600)
!!       call vinit(' ') ! start graphics using device $M_draw_DEVICE
!!       call page(-radius, radius, -radius, radius)
!!       call linewidth(200)
!!       call clear()
!!       call color(D_BLUE)
!!       call move2(-radius, -radius)
!!       call draw2(-radius, radius)
!!       call draw2(radius, radius)
!!       call draw2(radius, -radius)
!!       call draw2(-radius, -radius)
!!       call color(D_CYAN)
!!       call circle(0.0,0.0,radius)
!!       call vflush()
!!       ipaws=getkey()
!!       call vexit()
!!    end program demo_page
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    MIT
subroutine biggest_ortho2(xsmall,xlarge,ysmall,ylarge)

! ident_4="@(#) M_draw page(3f) given a window size find and set to largest accommodating viewport"

real,intent(in)  :: xsmall
real,intent(in)  :: xlarge
real,intent(in)  :: ysmall
real,intent(in)  :: ylarge
real             :: rps
real             :: spr
real             :: tryx
real             :: tryy
real             :: vhigh
real             :: vwide
real             :: xdelta
real             :: xmax
real             :: xmin
real             :: xsplit
real             :: ydelta
real             :: ymax
real             :: ymin
real             :: ysplit
!----------------------------------------------------------------------------------------------------------------------------------!
      call draw_getdisplaysize(vwide,vhigh) !get screen size in terms of raster units
!----------------------------------------------------------------------------------------------------------------------------------!
!     the default viewport is in "screen units", and goes from -1,-1 to 1,1
!     all new viewports are defined in terms of this original viewport, which
!     is 2 units wide and 2 units tall.
!----------------------------------------------------------------------------------------------------------------------------------!
      rps=min(vwide,vhigh)/2.0         ! number of rasters per screen unit
      spr=2.0/min(vwide,vhigh)         ! number of screen units per raster
      tryx=vwide                       ! make as wide as display as a trial fit
      if(xlarge-xsmall.ne.0.0)then
         tryy=vwide*(ylarge-ysmall)/(xlarge-xsmall) ! calculate required height
      else                             ! ERROR: do something desperate
         call journal('*page* window has a zero X dimension')
         tryy=vhigh
      endif
      if(tryy.gt.vhigh)then ! if required height too great, fit with y maximized
         tryy=vhigh
         if(ylarge-ysmall.ne.0.0)then
            tryx=vhigh*(xlarge-xsmall)/(ylarge-ysmall)
         else                          ! ERROR: do something desperate
            call journal('*page* window has a zero Y dimension')
            tryx=vwide
         endif
      endif
!----------------------------------------------------------------------------------------------------------------------------------!
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
!----------------------------------------------------------------------------------------------------------------------------------!
      xdelta=tryx-2.0*rps  ! need this many more rasters in x direction from 1,1
      ydelta=tryy-2.0*rps  ! need this many more rasters in y direction from 1,1
      ! to center (to left bottom justify, make xsplit and ysplit 0)
      xsplit=(vwide-tryx)/2.0
      ysplit=(vhigh-tryy)/2.0
      xmax=1+xdelta*spr+xsplit*spr
      ymax=1+ydelta*spr+ysplit*spr
      xmin=-1+xsplit*spr
      ymin=-1+ysplit*spr
!----------------------------------------------------------------------------------------------------------------------------------!
      write(*,*)'max. display area is', vwide, ' by ',vhigh,' rasters'
      write(*,*)'shape is ',xsmall,xlarge,ysmall,ylarge
      write(*,*)'attempting to get a viewport of ',tryx,' by ',tryy
      write(*,*)'needed more rasters, ',xdelta,' by ',ydelta
      write(*,*)'resulted in viewport ',-1,xmax,-1,ymax
!----------------------------------------------------------------------------------------------------------------------------------!
      if(xmin.ne.xmax.and.ymin.ne.ymax)then
         call draw_viewport(xmin,xmax,ymin,ymax)
      else
         call journal('*page* window has zero dimension,no viewport set')
      endif
!     to prevent clipping lines that are right on edge of window fudge a bit
      !bugx=.001*(xlarge-xsmall)
      !bugy=.001*(ylarge-ysmall)
      !xsmall1=xsmall-bugx
      !xlarge1=xlarge+bugx
      !ysmall1=ysmall-bugy
      !ylarge1=ylarge+bugy
      !call draw_ortho2(xsmall1,xlarge1,ysmall1,ylarge1)
      if(xsmall.ne.xlarge.and.ysmall.ne.ylarge)then
         call draw_ortho2(xsmall,xlarge,ysmall,ylarge)
      else    ! ERROR: do something desperate
         call journal('*page* window has zero dimension, no window set')
      endif
contains

subroutine journal(string)
character(len=*),intent(in) :: string
   write(*,'(a)')string
end subroutine journal

end subroutine biggest_ortho2
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!       page_rri(3fp) - [M_draw] - new world window with viewport set to largest area with same aspect ratio
!!       (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   subroutine page_rri(xsize,ysize,icolor)
!!
!!    integer,intent(in)          :: xsize
!!    integer,intent(in)          :: ysize
!!    integer,intent(in)          :: icolor
!!
!!##DESCRIPTION
!!    Given a horizontal size and vertical size and color set the window to
!!    the rectangle defined by the corner points <0.0,0.0> and <xsize,ysize>
!!    and set the viewport to the largest viewport on the display with the
!!    same aspect ratio and start a new page with the specified background
!!    color if background color is supported on the device.
!!
!!##OPTIONS
!!    XSIZE    X size of requested window
!!    YSIZE    Y size of requested window
!!    ICOLOR   Color to set background.
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!      program demo_page_rri
!!      use M_draw
!!      use M_drawplus, only : polyline2
!!      implicit none
!!      integer :: ipaws
!!      call prefsize(300,300)
!!      call vinit(' ')
!!      call page(8.5,11.0,3)
!!      call color(2)
!!      call linewidth(100)
!!      call circle(8.5/0.0,11.0/0.0,8.5/2.0)
!!      call color(1)
!!      call polyline2([0.0,0,0,8.5,11.0]
!!      call polyline2([8.5,0,0,0.0,11.0]
!!      ipaws=getkey()
!!      call vexit()
!!      end
!!      program demo_page_rri
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    MIT

subroutine page_rri(xsize,ysize,icolor)
real,intent(in)             :: xsize
real,intent(in)             :: ysize
integer                     :: icolor
   call page(0.0,xsize,0.0,ysize)
   call draw_pushattributes()
      call draw_color(icolor)
      call draw_clear()
   call draw_popattributes()
end subroutine page_rri

