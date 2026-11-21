module m_draw_aspect
   use, intrinsic :: iso_fortran_env, only: real32
   use m_draw_types
   use m_draw_mapping
   implicit none
   private
   public :: draw_getaspect, draw_getdisplaysize, draw_getfactors, &
             draw_expandviewport, draw_unexpandviewport

contains

   real(real32) function draw_getaspect()
      draw_getaspect = real(vdevice%sizeSy, real32) / real(vdevice%sizeSx, real32)
   end function draw_getaspect

   subroutine draw_getdisplaysize(x, y)
      real(real32), intent(out) :: x, y
      x = real(vdevice%sizeSx, real32)
      y = real(vdevice%sizeSy, real32)
   end subroutine draw_getdisplaysize

   subroutine draw_getfactors(x, y)
      real(real32), intent(out) :: x, y
      x = real(vdevice%sizeSx, real32) / real(vdevice%sizeX, real32)
      y = real(vdevice%sizeSy, real32) / real(vdevice%sizeY, real32)
   end subroutine draw_getfactors

   subroutine draw_expandviewport()
      vdevice%attr%a%exvp = 1
      vdevice%sizeX = vdevice%sizeSx
      vdevice%sizeY = vdevice%sizeSy
      call draw_CalcW2Vcoeffs()
   end subroutine draw_expandviewport

   subroutine draw_unexpandviewport()
      vdevice%attr%a%exvp = 0
      vdevice%sizeY = min(vdevice%sizeSx, vdevice%sizeSy)
      vdevice%sizeX = vdevice%sizeY
      call draw_CalcW2Vcoeffs()
   end subroutine draw_unexpandviewport

end module m_draw_aspect
