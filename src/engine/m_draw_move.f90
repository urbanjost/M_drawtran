module m_draw_move
   use, intrinsic :: iso_fortran_env, only: real32
   use m_draw_types
   use m_draw_verror
   use m_draw_matrix
   use m_draw_mapping
   implicit none
   private
   public :: draw_move, draw_move2, draw_rmove, draw_rmove2, draw_smove2, draw_rsmove2

   interface
      function draw_newtokens(num) result(toks)
         import :: Token
         integer, intent(in) :: num
         type(Token), pointer :: toks(:)
      end function draw_newtokens
   end interface

contains

!>
!!##NAME
!!    move(3f) - [M_draw:MOVE] Move current graphics position to (x, y, z)
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!          subroutine move(x, y, z)
!!          real x, y, z
!!##DESCRIPTION
!!
!!    Move current graphics position to (x, y, z). (x, y, z) is a point in
!!    world coordinates.
   subroutine draw_move(x, y, z)
      real(real32), intent(in) :: x, y, z
      type(Token), pointer :: p(:)

      if (vdevice%initialized == 0) then
         call draw_verror("move: draw not initialized")
      end if

      vdevice%cpW(D_X) = x
      vdevice%cpW(D_Y) = y
      vdevice%cpW(D_Z) = z
      vdevice%cpVvalid = 0

      if (vdevice%inpolygon /= 0) then
         call vdevice%pmove(x, y, z)
         return
      end if

      if (vdevice%inobject /= 0) then
         p => draw_newtokens(4)
         p(1)%i = OBJ_MOVE
         p(2)%f = x
         p(3)%f = y
         p(4)%f = z
         return
      end if

      if (vdevice%clipoff /= 0) then
         call draw_multvector(vdevice%cpWtrans, vdevice%cpW, vdevice%transmat%m)
         vdevice%cpVx = draw_WtoVx(vdevice%cpWtrans)
         vdevice%cpVy = draw_WtoVy(vdevice%cpWtrans)
      end if
   end subroutine draw_move

!>
!!##NAME
!!    move2(3f) - [M_draw:MOVE] Move graphics position to point (x, y)
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!          subroutine move2(x, y)
!!          real,intent(in) :: x, y
!!
!!##DESCRIPTION
!!
!!    Update current position.
!!    Move graphics position to point (x, y). (x, y) is a point in world
!!    coordinates.
!!
!!##OPTIONS
!!    X  new X position
!!    Y  new Y position
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!      program demo_move2
!!      use M_draw, only : prefsize, vinit, ortho2, clear, getkey
!!      use M_draw, only : move2, draw2, vexit, color
!!      use M_draw,    only  : D_BLACK,   D_WHITE
!!      use M_draw,    only  : D_RED,     D_GREEN,    D_BLUE
!!      use M_draw,    only  : D_YELLOW,  D_MAGENTA,  D_CYAN
!!      implicit none
!!      integer :: ipaws
!!      call prefsize(60,40)
!!      call vinit(' ') ! start graphics using device $M_DRAW_DEVICE
!!      call ortho2(-300.0,300.0,-200.0,200.0)
!!      call color(D_BLACK)
!!      call clear()
!!      call color(D_WHITE)
!!      call move2(-300.0,-200.0)
!!      call draw2(300.0,200.0)
!!      call move2(300.0,-200.0)
!!      call draw2(-300.0,200.0)
!!      ipaws=getkey()
!!      call vexit()
!!      end program demo_move2
   subroutine draw_move2(x, y)
      real(real32), intent(in) :: x, y
      if (vdevice%initialized == 0) then
         call draw_verror("move2: draw not initialized")
      end if
      call draw_move(x, y, 0.0_real32)
   end subroutine draw_move2

!>
!!##NAME
!!    rmove(3f) - [M_draw:MOVE] Relative move
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!          subroutine rmove(deltax, deltay, deltaz)
!!          real deltax, deltay, deltaz
!!
!!##DESCRIPTION
!!
!!    Relative move. deltax, deltay, and deltaz are offsets in world units.
   subroutine draw_rmove(dx, dy, dz)
      real(real32), intent(in) :: dx, dy, dz
      if (vdevice%initialized == 0) then
         call draw_verror("rmove: draw not initialized")
      end if
      call draw_move(vdevice%cpW(D_X) + dx, vdevice%cpW(D_Y) + dy, vdevice%cpW(D_Z) + dz)
   end subroutine draw_rmove

!>
!!##NAME
!!    rmove2(3f) - [M_draw:MOVE] Relative move in world units.
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!     subroutine rmove2(deltax, deltay)
!!     real,intent(in) :: deltax, deltay
!!
!!##DESCRIPTION
!!    Update current position.
!!    Relative move2. deltax and deltay are offsets in world units.
!!
!!##OPTIONS
!!    X  new X position
!!    Y  new Y position
!!##EXAMPLE
!!
!!   Sample program:
!!
!!      program demo_rmove2
!!      use M_draw, only: prefsize, vinit, ortho2, clear, getkey
!!      use M_draw, only: move2, rmove2, rdraw2, vexit
!!      use M_draw, only: linewidth
!!      implicit none
!!      integer :: ipaws
!!      integer :: i
!!      call prefsize(500,500)
!!      call vinit(' ') ! start graphics using device $M_DRAW_DEVICE
!!      call ortho2(-110.0,110.0,-110.0,110.0)
!!      call move2(-100.0,-100.0)
!!      call linewidth(70)
!!      do i=1,20
!!         call rmove2(10.0, 0.0)
!!         call rdraw2( 0.0,10.0)
!!      enddo
!!      ipaws=getkey()
!!      call vexit()
!!      end program demo_rmove2
   subroutine draw_rmove2(dx, dy)
      real(real32), intent(in) :: dx, dy
      if (vdevice%initialized == 0) then
         call draw_verror("rmove2: draw not initialized")
      end if
      call draw_move(vdevice%cpW(D_X) + dx, vdevice%cpW(D_Y) + dy, 0.0_real32)
   end subroutine draw_rmove2

!>
!!##NAME
!!    smove2(3f) - [M_draw:MOVE] Move current graphics position in screen coordinates (-1.0 to 1.0).
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!          subroutine smove2(x, y)
!!          real x, y
!!
!!##DESCRIPTION
!!    Move current graphics position in screen coordinates (-1.0 to 1.0).
   subroutine draw_smove2(xs, ys)
      real(real32), intent(in) :: xs, ys
      if (vdevice%initialized == 0) then
         call draw_verror("smove2: draw not initialized")
      end if
      vdevice%cpVx = int((xs / 2.0_real32 + 0.5_real32) * vdevice%sizeX)
      vdevice%cpVy = int((0.5_real32 + ys / 2.0_real32) * vdevice%sizeY)
   end subroutine draw_smove2

!>
!!##NAME
!!    rsmove2(3f) - [M_draw:MOVE] Relative move in screen units (-1.0 to 1.0).
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!          subroutine rsmove2(deltax, deltay)
!!          real deltax, deltay
!!
!!##DESCRIPTION
!!
!!    Relative smove2. deltax, and deltay are offsets in screen units
!!    (-1.0 to 1.0).
   subroutine draw_rsmove2(dxs, dys)
      real(real32), intent(in) :: dxs, dys
      if (vdevice%initialized == 0) then
         call draw_verror("rsmove2: draw not initialized")
      end if
      vdevice%cpVx = vdevice%cpVx + int(dxs / 2.0_real32 * vdevice%sizeX)
      vdevice%cpVy = vdevice%cpVy + int(dys / 2.0_real32 * vdevice%sizeY)
   end subroutine draw_rsmove2

end module m_draw_move
