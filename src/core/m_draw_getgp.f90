module m_draw_getgp
   use, intrinsic :: iso_fortran_env, only: real32
   use m_draw_types
   use m_draw_matrix
   use m_draw_mapping
   implicit none
   private
   public :: draw_getgp, draw_getgp2, draw_getgpt, draw_sgetgp2

contains

   subroutine draw_getgp(x, y, z)
      real(real32), intent(out) :: x, y, z
      x = vdevice%cpW(D_X)
      y = vdevice%cpW(D_Y)
      z = vdevice%cpW(D_Z)
   end subroutine draw_getgp

   subroutine draw_getgp2(x, y)
      real(real32), intent(out) :: x, y
      x = vdevice%cpW(D_X)
      y = vdevice%cpW(D_Y)
   end subroutine draw_getgp2

   subroutine draw_getgpt(x, y, z, w)
      real(real32), intent(out) :: x, y, z, w
      call draw_multvector(vdevice%cpWtrans, vdevice%cpW, vdevice%transmat%m)
      x = vdevice%cpWtrans(D_X)
      y = vdevice%cpWtrans(D_Y)
      z = vdevice%cpWtrans(D_Z)
      w = vdevice%cpWtrans(D_W)
   end subroutine draw_getgpt

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
