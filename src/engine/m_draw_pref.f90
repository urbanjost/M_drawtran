module m_draw_pref
   use, intrinsic :: iso_fortran_env, only: error_unit
   implicit none
   private
   public :: draw_prefposition, draw_prefsize, draw_getprefposandsize

   integer, save :: px = -1, py = -1, pxs = -1, pys = -1

contains

!>
!!##NAME
!!    prefposition(3f) - [M_draw:WINDOW_SETUP] Specify preferred position of window
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine prefposition(x, y)
!!
!!     integer,intent(in) :: x, y
!!
!!##DESCRIPTION
!!    Specify the preferred position of the window opened by the *next*
!!    vinit in raster units or units of resolution, depending on the output
!!    device. The physical size of the units are generally device-specific --
!!    For X11 Windows and PPM pixmaps the values would represent rasters.
!!    For vector output devices the number of "rasters" per inch varies.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!      program demo_prefposition
!!      use M_draw, only    : prefsize, vinit, ortho2, clear, getkey
!!      use M_draw, only    : prefposition, move2, draw2, vexit, color
!!      use M_draw,    only  : D_BLACK,   D_WHITE
!!      use M_draw,    only  : D_RED,     D_GREEN,    D_BLUE
!!      use M_draw,    only  : D_YELLOW,  D_MAGENTA,  D_CYAN
!!      implicit none
!!      integer :: ipaws
!!
!!      call prefsize(60,40)
!!      call prefposition(100,100)
!!
!!      call vinit(' ')         ! start graphics using device $M_DRAW_DEVICE
!!      call ortho2(-300.0,300.0,-200.0,200.0)
!!      call color(D_BLACK)
!!      call clear()
!!      call color(D_RED)
!!      call move2(-300.0,-200.0)
!!      call draw2(300.0,200.0)
!!      call move2(300.0,-200.0)
!!      call draw2(-300.0,200.0)
!!      ipaws=getkey()
!!      call vexit()
!!
!!      end program demo_prefposition
   subroutine draw_prefposition(x, y)
      integer, intent(in) :: x, y

      if (x < -1) then
         write(error_unit, '(A, I0)') 'prefposition: bad x value ', x
         px = -1
         py = -1
      end if
      if (y < -1) then
         write(error_unit, '(A, I0)') 'prefposition: bad y value ', y
         px = -1
         py = -1
      end if
      px = x
      py = y
   end subroutine draw_prefposition

!>
!!##NAME
!!    prefsize(3f) - [M_draw:WINDOW_SETUP] Specify preferred width and
!!    height of window in physical units
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!         subroutine prefsize(width, height)
!!         integer,intent(in) :: width, height
!!
!!##DESCRIPTION
!!
!!    Specify the preferred width and height of the device output surface
!!    opened by the *next* vinit(3f).
!!
!!##OPTIONS
!!    WIDTH   width of device to create when vinit(3f) is called
!!    HEIGHT  height of device to create when vinit(3f) is called
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!      program demo_prefsize
!!      use M_draw, only: prefsize, vinit, ortho2, clear, getkey
!!      use M_draw, only: move2, draw2, vexit, color
!!      use M_draw,    only  : D_BLACK,   D_WHITE
!!      use M_draw,    only  : D_RED,     D_GREEN,    D_BLUE
!!      use M_draw,    only  : D_YELLOW,  D_MAGENTA,  D_CYAN
!!      implicit none
!!      integer :: ipaws
!!         ! make first file with one size
!!         call prefsize(60*2,40*2)
!!         call vinit(' ') ! start graphics using device $M_DRAW_DEVICE
!!         call picture()
!!         ipaws=getkey()
!!         call vexit()
!!
!!         ! make second file with another size
!!         call prefsize(60*3,40*3)
!!         call vinit(' ')
!!         call picture()
!!         ipaws=getkey()
!!         call vexit()
!!      contains
!!      subroutine picture
!!         call ortho2(-300.0,300.0,-200.0,200.0)
!!         call color(D_BLACK)
!!         call clear()
!!         call color(D_RED)
!!         call move2(-300.0,-200.0)
!!         call draw2(300.0,200.0)
!!         call move2(300.0,-200.0)
!!         call draw2(-300.0,200.0)
!!      end subroutine picture
!!      end program demo_prefsize
   subroutine draw_prefsize(x, y)
      integer, intent(in) :: x, y

      if (x < -1 .or. x == 0) then
         write(error_unit, '(A, I0)') 'prefsize: bad x value ', x
         px = -1
         py = -1
      end if
      if (y < -1 .or. y == 0) then
         write(error_unit, '(A, I0)') 'prefsize: bad y value ', y
         px = -1
         py = -1
      end if
      pxs = x
      pys = y
   end subroutine draw_prefsize

   subroutine draw_getprefposandsize(x, y, xs, ys)
      integer, intent(out) :: x, y, xs, ys

      x = px
      y = py
      xs = pxs
      ys = pys
   end subroutine draw_getprefposandsize

end module m_draw_pref
