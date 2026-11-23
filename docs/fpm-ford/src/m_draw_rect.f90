module m_draw_rect
   use, intrinsic :: iso_fortran_env, only: real32
   use m_draw_types
   use m_draw_verror
   use m_draw_move
   use m_draw_draw
   implicit none
   private
   public :: draw_rect, draw_srect

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

!>
!!##NAME
!!    rect(3f) - [M_draw:POLYGONS] Draw a rectangle given two corners
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!       subroutine rect(x1, y1, x2, y2)
!!       real,intent(in) :: x1,y1,x2,y2
!!
!!##DESCRIPTION
!!    Draw rectangle given two opposite corners.
!!
!!    Note: rectangles are regarded as polygons, so if
!!    polyfill or polyhatch has been called with .TRUE., the rectangle will
!!    be filled or hatched accordingly.
!!
!!##OPTIONS
!!   Given
!!
!!       x1,y1 ############ x2,y1
!!             #          #
!!             #          #
!!             #          #
!!       x1,y2 ############ x2,y2
!!
!!    X1,Y1  coordinates of a corner of the rectangle
!!    X2,Y2  coordinates of corner point opposite first point
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_rect
!!    use M_draw
!!    use M_draw,    only  : D_BLACK,   D_WHITE
!!    use M_draw,    only  : D_RED,     D_GREEN,    D_BLUE
!!    use M_draw,    only  : D_YELLOW,  D_MAGENTA,  D_CYAN
!!    implicit none
!!    integer :: ipaws
!!    real    :: b=0.2
!!
!!    !! set up graphics area
!!    call prefsize(1000,200)
!!    call vinit(' ') ! start graphics using device $M_DRAW_DEVICE
!!    call page(-25.0-b, 25.0+b, -5.0-b, 5.0+b)
!!
!!    call linewidth(150)
!!    call color(D_RED)
!!    call rect(-24.0, -4.0, -12.0, 4.0)
!!    call polyfill(.true.)
!!    call color(D_GREEN)
!!    call rect(-10.0, -4.0, -2.0, 4.0)
!!    call polyhatch(.true.)
!!    call hatchpitch(0.4)
!!    call hatchang(30.0)
!!    call linewidth(20)
!!    call color(D_BLUE)
!!    call rect(0.0, -4.0, 20.0, 3.0)
!!    call linewidth(200)
!!    call color(D_BLUE)
!!    call move2(-25.0, -5.0)
!!    call draw2(-25.0, 5.0)
!!    call draw2(25.0, 5.0)
!!    call draw2(25.0, -5.0)
!!    call draw2(-25.0, -5.0)
!!
!!    !! pause
!!    call vflush()
!!    ipaws=getkey()
!!
!!    !! wrap up graphics
!!    call vexit()
!!
!!    end program demo_rect
   subroutine draw_rect(x1, y1, x2, y2)
      real(real32), intent(in) :: x1, y1, x2, y2
      logical                  :: started
      integer                  :: sync, idum
      type(Token), pointer     :: tok(:)

      if (vdevice%initialized == 0) call draw_verror("rect: draw not initialized")

      if (vdevice%inobject /= 0) then
         tok => draw_newtokens(5)
         tok(1)%i = OBJ_RECT
         tok(2)%f = x1
         tok(3)%f = y1
         tok(4)%f = x2
         tok(5)%f = y2
         return
      end if

      if ((vdevice%attr%a%fill /= 0 .or. vdevice%attr%a%hatch /= 0) .and. vdevice%inpolygon == 0) then
         call draw_makepoly()
         started = .true.
      else
         started = .false.
      end if

      sync = vdevice%sync
      if (sync /= 0) vdevice%sync = 0

      call draw_move2(x1, y1)    
      call draw_draw2(x2, y1)
      call draw_draw2(x2, y2)   
      call draw_draw2(x1, y2)

      if (started) then
         call draw_closepoly()
      else
         call draw_draw2(x1, y1)
      end if

      if (sync /= 0) then
         vdevice%sync = 1
         idum = vdevice%dev%Vsync()
      end if
   end subroutine draw_rect

   subroutine draw_srect(x1, y1, x2, y2)
      real(real32), intent(in) :: x1, y1, x2, y2
      integer :: sync, idum

      if (vdevice%initialized == 0) call draw_verror("rect: draw not initialized")

      sync = vdevice%sync
      if (sync /= 0) vdevice%sync = 0

      call draw_smove2(x1, y1)
      call draw_sdraw2(x2, y1)
      call draw_sdraw2(x2, y2)
      call draw_sdraw2(x1, y2)
      call draw_sdraw2(x1, y1)

      if (sync /= 0) then
         vdevice%sync = 1
         idum = vdevice%dev%Vsync()
      end if
   end subroutine draw_srect

end module m_draw_rect
