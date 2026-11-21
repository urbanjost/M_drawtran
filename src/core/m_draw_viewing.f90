module m_draw_viewing
   use, intrinsic :: iso_fortran_env, only: real32
   use m_draw_types
   use m_draw_verror
   use m_draw_matrix
   use m_draw_trans
   implicit none
   private
   public :: draw_polarview, draw_up, draw_lookat, draw_perspective, &
             draw_window, draw_ortho, draw_ortho2

   real(real32), save :: Vup_x = 0.0_real32, Vup_y = 1.0_real32, Vup_z = 0.0_real32

contains

   subroutine draw_polarview(dist, azim, inc, twist)
      real(real32), intent(in) :: dist, azim, inc, twist

      if (vdevice%initialized == 0) call draw_verror("polarview: draw not initialized")

      call draw_translate(0.0_real32, 0.0_real32, -dist)
      call draw_rotate(-twist, 'z')
      call draw_rotate(-inc, 'x')
      call draw_rotate(-azim, 'z')
   end subroutine draw_polarview

   subroutine draw_up(x, y, z)
      real(real32), intent(in) :: x, y, z
      Vup_x = x
      Vup_y = y
      Vup_z = z
   end subroutine draw_up

   subroutine draw_lookat(vx, vy, vz, px, py, pz, twist)
      real(real32), intent(in) :: vx, vy, vz, px, py, pz, twist
      real(real32) :: dv, dx, dy, dz, mat(4,4)
      real(real32) :: rx, ry, rz, ux, uy, uz, rlen

      if (vdevice%initialized == 0) call draw_verror("lookat: draw not initialized")

      call draw_rotate(-twist, 'z')

      dx = px - vx
      dy = py - vy
      dz = pz - vz
      dv = sqrt(dx*dx + dy*dy + dz*dz)
      if (dv == 0.0_real32) call draw_verror("lookat: viewing point equals reference point")

      ! Forward vector (into screen is -Z in right-handed view coords)
      dx = dx / dv; dy = dy / dv; dz = dz / dv

      ! Right vector = forward x up
      rx = dy * Vup_z - dz * Vup_y
      ry = dz * Vup_x - dx * Vup_z
      rz = dx * Vup_y - dy * Vup_x
      rlen = sqrt(rx*rx + ry*ry + rz*rz)
      if (rlen == 0.0_real32) call draw_verror("lookat: up vector parallel to line of sight")
      rx = rx / rlen; ry = ry / rlen; rz = rz / rlen

      ! Recomputed orthogonal Up vector = right x forward
      ux = ry * dz - rz * dy
      uy = rz * dx - rx * dz
      uz = rx * dy - ry * dx

      call draw_identmatrix(mat)
      mat(1, 1) = rx;  mat(1, 2) = ux;  mat(1, 3) = -dx
      mat(2, 1) = ry;  mat(2, 2) = uy;  mat(2, 3) = -dy
      mat(3, 1) = rz;  mat(3, 2) = uz;  mat(3, 3) = -dz

      call draw_multmatrix(mat)
      call draw_translate(-vx, -vy, -vz)
   end subroutine draw_lookat

   subroutine draw_perspective(fov, aspect, hither, yon)
      real(real32), intent(in) :: fov, aspect, hither, yon
      real(real32) :: mat(4,4), cotfov

      if (vdevice%initialized == 0) call draw_verror("perspective: draw not initialized")
      if (fov <= 0.0_real32 .or. fov >= 180.0_real32) call draw_verror("perspective: bad fov")
      if (aspect == 0.0_real32) call draw_verror("perspective: zero aspect ratio")
      if ((yon - hither) == 0.0_real32) call draw_verror("perspective: hither == yon")

      cotfov = 1.0_real32 / tan(0.5_real32 * D2R * fov)

      mat = 0.0_real32
      mat(1, 1) = cotfov / aspect
      mat(2, 2) = cotfov
      mat(3, 3) = -(yon + hither) / (yon - hither)
      mat(3, 4) = -1.0_real32
      mat(4, 3) = -2.0_real32 * yon * hither / (yon - hither)

      call draw_loadmatrix(mat)
   end subroutine draw_perspective

   subroutine draw_window(left, right, bottom, top, hither, yon)
      real(real32), intent(in) :: left, right, bottom, top, hither, yon
      real(real32) :: mat(4,4)

      if (vdevice%initialized == 0) call draw_verror("window: draw not initialized")
      if ((right - left) == 0.0_real32) call draw_verror("window: left == right")
      if ((top - bottom) == 0.0_real32) call draw_verror("window: bottom == top")
      if ((yon - hither) == 0.0_real32) call draw_verror("window: hither == yon")

      mat = 0.0_real32
      mat(1, 1) = 2.0_real32 * hither / (right - left)
      mat(2, 2) = 2.0_real32 * hither / (top - bottom)
      mat(3, 1) = (right + left) / (right - left)
      mat(3, 2) = (top + bottom) / (top - bottom)
      mat(3, 3) = -(yon + hither) / (yon - hither)
      mat(3, 4) = -1.0_real32
      mat(4, 3) = -2.0_real32 * yon * hither / (yon - hither)

      call draw_loadmatrix(mat)
   end subroutine draw_window

   subroutine draw_ortho(left, right, bottom, top, hither, yon)
      real(real32), intent(in) :: left, right, bottom, top, hither, yon
      real(real32) :: mat(4,4)

      if (vdevice%initialized == 0) call draw_verror("ortho: draw not initialized")
      if ((right - left) == 0.0_real32) call draw_verror("ortho: left == right")
      if ((top - bottom) == 0.0_real32) call draw_verror("ortho: bottom == top")
      if ((yon - hither) == 0.0_real32) call draw_verror("ortho: hither == yon")

      call draw_identmatrix(mat)
      mat(1, 1) = 2.0_real32 / (right - left)
      mat(2, 2) = 2.0_real32 / (top - bottom)
      mat(3, 3) = -2.0_real32 / (yon - hither)
      mat(4, 1) = -(right + left) / (right - left)
      mat(4, 2) = -(top + bottom) / (top - bottom)
      mat(4, 3) = -(yon + hither) / (yon - hither)

      call draw_loadmatrix(mat)
   end subroutine draw_ortho

   subroutine draw_ortho2(left, right, bottom, top)
      real(real32), intent(in) :: left, right, bottom, top
      call draw_ortho(left, right, bottom, top, -1.0_real32, 1.0_real32)
   end subroutine draw_ortho2

end module m_draw_viewing
