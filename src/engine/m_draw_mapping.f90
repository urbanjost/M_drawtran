
module m_draw_mapping
   use, intrinsic :: iso_fortran_env, only: real32
   use m_draw_types
   implicit none
   private
   public :: draw_VtoWxy, draw_CalcW2Vcoeffs, draw_WtoVx, draw_WtoVy

   real(real32), save :: Vcx = 0.0_real32, Vcy = 0.0_real32
   real(real32), save :: Vsx = 1.0_real32, Vsy = 1.0_real32

contains

   subroutine draw_VtoWxy(xs, ys, xw, yw)
      real(real32), intent(in)  :: xs, ys
      real(real32), intent(out) :: xw, yw
      real(real32) :: d, a1, a2, b1, b2, c1, c2, A, B

      A = (xs - Vcx) / Vsx
      B = (ys - Vcy) / Vsy

      a1 = vdevice%transmat%m(1,1) - vdevice%transmat%m(1,4) * A
      a2 = vdevice%transmat%m(1,2) - vdevice%transmat%m(1,4) * B

      b1 = vdevice%transmat%m(2,1) - vdevice%transmat%m(2,4) * A
      b2 = vdevice%transmat%m(2,2) - vdevice%transmat%m(2,4) * B

      c1 = vdevice%transmat%m(4,4) * A - vdevice%transmat%m(4,1)
      c2 = vdevice%transmat%m(4,4) * B - vdevice%transmat%m(4,2)

      d = (a2 * b1 - b2 * a1)

      if (d /= 0.0_real32) then
         xw = (b1 * c2 - c1 * b2) / d
         yw = (c1 * a2 - a1 * c2) / d
      else
         xw = A
         yw = B
      end if

      if (xw == 0.0_real32) xw = A
      if (yw == 0.0_real32) yw = B
   end subroutine draw_VtoWxy

   subroutine draw_CalcW2Vcoeffs()
      Vcx = real(vdevice%maxVx + vdevice%minVx, real32) * 0.5_real32
      Vcy = real(vdevice%maxVy + vdevice%minVy, real32) * 0.5_real32

      Vsx = real(vdevice%maxVx - vdevice%minVx, real32) * 0.5_real32
      Vsy = real(vdevice%maxVy - vdevice%minVy, real32) * 0.5_real32
   end subroutine draw_CalcW2Vcoeffs

   integer function draw_WtoVx(p)
      real(real32), intent(in) :: p(4)
      draw_WtoVx = int(p(1) * Vsx / p(4) + Vcx + 0.5_real32)
   end function draw_WtoVx

   integer function draw_WtoVy(p)
      real(real32), intent(in) :: p(4)
      draw_WtoVy = int(p(2) * Vsy / p(4) + Vcy + 0.5_real32)
   end function draw_WtoVy

end module m_draw_mapping
