module m_draw_verror
   use, intrinsic :: iso_fortran_env, only: error_unit
   use m_draw_types
   implicit none
   private
   public :: draw_verror

   interface
      subroutine draw_vexit()
      end subroutine draw_vexit
   end interface

contains

   subroutine draw_verror(str)
      character(len=*), intent(in) :: str

      write(error_unit, '(A)') trim(str)
      if (vdevice%initialized /= 0) then
         call draw_vexit()
      end if
      error stop 1
   end subroutine draw_verror

end module m_draw_verror
