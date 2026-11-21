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

   subroutine draw_move2(x, y)
      real(real32), intent(in) :: x, y
      if (vdevice%initialized == 0) then
         call draw_verror("move2: draw not initialized")
      end if
      call draw_move(x, y, 0.0_real32)
   end subroutine draw_move2

   subroutine draw_rmove(dx, dy, dz)
      real(real32), intent(in) :: dx, dy, dz
      if (vdevice%initialized == 0) then
         call draw_verror("rmove: draw not initialized")
      end if
      call draw_move(vdevice%cpW(D_X) + dx, vdevice%cpW(D_Y) + dy, vdevice%cpW(D_Z) + dz)
   end subroutine draw_rmove

   subroutine draw_rmove2(dx, dy)
      real(real32), intent(in) :: dx, dy
      if (vdevice%initialized == 0) then
         call draw_verror("rmove2: draw not initialized")
      end if
      call draw_move(vdevice%cpW(D_X) + dx, vdevice%cpW(D_Y) + dy, 0.0_real32)
   end subroutine draw_rmove2

   subroutine draw_smove2(xs, ys)
      real(real32), intent(in) :: xs, ys
      if (vdevice%initialized == 0) then
         call draw_verror("smove2: draw not initialized")
      end if
      vdevice%cpVx = int((xs / 2.0_real32 + 0.5_real32) * vdevice%sizeX)
      vdevice%cpVy = int((0.5_real32 + ys / 2.0_real32) * vdevice%sizeY)
   end subroutine draw_smove2

   subroutine draw_rsmove2(dxs, dys)
      real(real32), intent(in) :: dxs, dys
      if (vdevice%initialized == 0) then
         call draw_verror("rsmove2: draw not initialized")
      end if
      vdevice%cpVx = vdevice%cpVx + int(dxs / 2.0_real32 * vdevice%sizeX)
      vdevice%cpVy = vdevice%cpVy + int(dys / 2.0_real32 * vdevice%sizeY)
   end subroutine draw_rsmove2

end module m_draw_move
