module m_draw_buffer
   use, intrinsic :: iso_fortran_env, only: error_unit
   use m_draw_types
   implicit none
   private
   public :: draw_backbuffer, draw_frontbuffer, draw_swapbuffers

contains

!>
!!##NAME
!!    backbuffer(3f) - [M_draw:DOUBLE_BUFFERING] Draw in the backbuffer. Returns -1 if the device is not up to it.
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!          integer function backbuffer()
!!
!!##DESCRIPTION
!!    Make M_draw draw in the backbuffer. Returns -1 if the device is not up to it.
   integer function draw_backbuffer()
      vdevice%attr%a%inbackbuffer = 1
      vdevice%sync = 0
      draw_backbuffer = vdevice%dev%Vbackb()
   end function draw_backbuffer

!>
!!##NAME
!!    frontbuffer(3f) - [M_draw:DOUBLE_BUFFERING] Draw in the front buffer. This will always work.
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!          subroutine frontbuffer()
!!
!!##DESCRIPTION
!!
!!    Make M_draw draw in the front buffer. This will always work.
   subroutine draw_frontbuffer()
      integer :: idum
      idum = vdevice%dev%Vfrontb()
      vdevice%attr%a%inbackbuffer = 0
      vdevice%sync = 1
   end subroutine draw_frontbuffer

!>
!!##NAME
!!    swapbuffers(3f) - [M_draw:DOUBLE_BUFFERING] Swap the front and back buffers.
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!          subroutine swapbuffers()
!!
!!##DESCRIPTION
!!    Swap the front and back buffers.
   integer function draw_swapbuffers()
      if (vdevice%attr%a%inbackbuffer /= 1) then
         write(error_unit, '(A)') "swapbuffers: double buffering not initialized."
         draw_swapbuffers = -1
         return
      end if
      draw_swapbuffers = vdevice%dev%Vswapb()
   end function draw_swapbuffers

end module m_draw_buffer
