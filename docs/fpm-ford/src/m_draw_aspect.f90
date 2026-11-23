module m_draw_aspect
   use, intrinsic :: iso_fortran_env, only: real32
   use m_draw_types
   use m_draw_mapping
   implicit none
   private
   public :: draw_getaspect, draw_getdisplaysize, draw_getfactors, &
             draw_expandviewport, draw_unexpandviewport

contains
!>
!!##NAME
!!    getaspect(3f) - [M_draw:ASPECT] Returns the ratio height over width of the display device.
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!          real function getaspect()
!!
!!##DESCRIPTION
!!    Returns the ratio height over width of the display device.
   real(real32) function draw_getaspect()
      draw_getaspect = real(vdevice%sizeSy, real32) / real(vdevice%sizeSx, real32)
   end function draw_getaspect

!>
!!##NAME
!!    getdisplaysize(3f) - [M_draw:ASPECT] Returns width and height of device in device units
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!          subroutine getdisplaysize(w, h)
!!          real,intent(in) :: w, h
!!
!!##DESCRIPTION
!!    Returns the width and height of the device in device units in w and h
!!    respectively.
   subroutine draw_getdisplaysize(x, y)
      real(real32), intent(out) :: x, y
      x = real(vdevice%sizeSx, real32)
      y = real(vdevice%sizeSy, real32)
   end subroutine draw_getdisplaysize

!>
!!##NAME
!!    getfactors(3f) - [M_draw:ASPECT] Returns width over min(width of device, height of device) and height over min(width of device, height of device).
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!          subroutine getfactors(w, h)
!!          real w, h
!!##DESCRIPTION
!!    Returns wfact as the width over min(width of device, height of device)
!!    and hfact as the height over min(width of device, height of
!!    device).
   subroutine draw_getfactors(x, y)
      real(real32), intent(out) :: x, y
      x = real(vdevice%sizeSx, real32) / real(vdevice%sizeX, real32)
      y = real(vdevice%sizeSy, real32) / real(vdevice%sizeY, real32)
   end subroutine draw_getfactors

!>
!!##NAME
!!    expandviewport(3f) - [M_draw:VIEWPORT] use the entire device viewport
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!         subroutine expandviewport()
!!
!!##DESCRIPTION
!!
!!    When M_draw does viewport calculations, it will normally begin by using
!!    the largest square it can fit onto the actual display device. This
!!    call says to use the whole device... however you must then take
!!    into account any distortion that will occur due to the non-square
!!    mapping. Thus, a viewport of (-1.0, 1.0, -1.0, 1.0) will map into
!!    the whole display device.
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_expandviewport
!!    use M_draw
!!    use M_draw,    only  : D_BLACK,   D_WHITE
!!    use M_draw,    only  : D_RED,     D_GREEN,    D_BLUE
!!    use M_draw,    only  : D_YELLOW,  D_MAGENTA,  D_CYAN
!!    implicit none
!!    integer :: ipaws
!!
!!    !! set up graphics area
!!    call prefsize(1000,200)
!!    call vinit(' ') ! start graphics using device $M_DRAW_DEVICE
!!
!!    !! draw box that fills default world coordinate window
!!    call color(D_RED)
!!    call polyfill(.true.)
!!    call rect(-1.0, -1.0, 1.0, 1.0)
!!    call color(D_GREEN)
!!    call circle(0.0,0.0,1.0)
!!
!!    call expandviewport()
!!    !! the meaning of viewport numbers has now changed, but the
!!    !! viewport itself has not. Now <-1,-1> <1,1> defines the
!!    !! entire display area, where before it defined the largest square
!!    !! that would fit on the display
!!    call viewport(-1.0,1.0,-1.0,1.0)
!!    !! draw box that fills default world coordinate window again
!!
!!    !! instead of a square and circle, the mapping now
!!    !! produces an ellipse and rectangle unless this
!!    !! device has a square display
!!    call polyhatch(.true.)
!!    call hatchpitch(0.1)
!!    call hatchang(30.0)
!!    call linewidth(20)
!!    call color(D_CYAN)
!!    call rect(-1.0, -1.0, 1.0, 1.0)
!!    call color(D_YELLOW)
!!    call hatchang(120.0)
!!    call circle(0.0,0.0,1.0)
!!
!!    !! border
!!    call linewidth(100)
!!    call color(D_BLUE)
!!    call move2(-1.0, -1.0)
!!    call draw2(-1.0, 1.0)
!!    call draw2(1.0, 1.0)
!!    call draw2(1.0, -1.0)
!!    call draw2(-1.0, -1.0)
!!
!!    call vflush(); ipaws=getkey() !! pause
!!    call vexit()                  !! wrap up graphics
!!
!!    end program demo_expandviewport
   subroutine draw_expandviewport()
      vdevice%attr%a%exvp = 1
      vdevice%sizeX = vdevice%sizeSx
      vdevice%sizeY = vdevice%sizeSy
      call draw_CalcW2Vcoeffs()
   end subroutine draw_expandviewport

!>
!!##NAME
!!    unexpandviewport(3f) - [M_draw:VIEWPORT] undo expandviewport(3f)
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!         subroutine unexpandviewport()
!!##DESCRIPTION
!!
!!    Does the reverse of expandviewport. Basically, it returns M_draw to
!!    using the largest square of the device for its viewport calculations.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_unexpandviewport
!!    use M_draw
!!    use M_draw,    only  : D_BLACK,   D_WHITE
!!    use M_draw,    only  : D_RED,     D_GREEN,    D_BLUE
!!    use M_draw,    only  : D_YELLOW,  D_MAGENTA,  D_CYAN
!!    implicit none
!!    integer :: ipaws
!!
!!    !! set up graphics area
!!    call prefsize(1000,200)
!!    call vinit(' ') ! start graphics using device $M_DRAW_DEVICE
!!
!!    !! draw circle that fills default world coordinate window
!!    call polyfill(.true.)
!!    call color(D_GREEN)
!!    call circle(0.0,0.0,1.0)
!!    ipaws=getkey() !! pause
!!
!!    !! set new scales for viewport so <-1,-1> and <1,1> are at
!!    !! corners of display instead of corners of largest square
!!    !! that can fit on display
!!    call expandviewport()
!!    call viewport(-1.0,1.0,-1.0,1.0)
!!
!!    !! draw circle that fills default world coordinate window again
!!    !! instead of a circle, the mapping now produces an ellipse unless
!!    !! this device has a square display
!!    call polyhatch(.true.)
!!    call hatchpitch(0.1)
!!    call hatchang(30.0)
!!    call linewidth(40)
!!    call color(D_CYAN)
!!    call circle(0.0,0.0,1.0)
!!    ipaws=getkey() !! pause
!!
!!    !! set new scales for viewport so <-1,-1> and <1,1> are at
!!    !! corners of largest square that fits on display
!!    call unexpandviewport()
!!    !! actually change to the new viewport
!!    call viewport(-1.0,1.0,-1.0,1.0)
!!
!!    !! now the same circle should draw where the original one did
!!    call color(D_BLACK)
!!    call hatchang(120.0)
!!    call linewidth(40)
!!    call circle(0.0,0.0,1.0)
!!    ipaws=getkey() !! pause
!!
!!    call vexit()                  !! wrap up graphics
!!
!!    end program demo_unexpandviewport
   subroutine draw_unexpandviewport()
      vdevice%attr%a%exvp = 0
      vdevice%sizeY = min(vdevice%sizeSx, vdevice%sizeSy)
      vdevice%sizeX = vdevice%sizeY
      call draw_CalcW2Vcoeffs()
   end subroutine draw_unexpandviewport

end module m_draw_aspect
