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

   subroutine draw_point(x, y, z)
      real(real32), intent(in) :: x, y, z
      if (vdevice%initialized == 0) call draw_verror("point: draw not initialized")
      call draw_move(x, y, z)
      call draw_draw(x, y, z)
   end subroutine draw_point

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
