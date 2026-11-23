module m_draw_valloc
   use, intrinsic :: iso_fortran_env, only: int64
   use m_draw_verror
   implicit none
   private
   public :: draw_vallocate_bytes, draw_vfree

contains

   subroutine draw_vallocate_bytes(buf, num_bytes, from)
      character(len=:), allocatable, intent(out) :: buf
      integer(int64), intent(in) :: num_bytes
      character(len=*), intent(in) :: from
      integer :: istat
      character(len=120) :: msg

      if (num_bytes <= 0_int64) then
         allocate(character(len=0) :: buf)
         return
      end if

      allocate(character(len=int(num_bytes)) :: buf, stat=istat)
      if (istat /= 0) then
         write(msg, '(A, I0, A, A)') "*vallocate*: request for ", num_bytes, &
            " bytes failed from ", trim(from)
         call draw_verror(msg)
      end if
   end subroutine draw_vallocate_bytes

   subroutine draw_vfree(buf, from)
      character(len=:), allocatable, intent(inout) :: buf
      character(len=*), intent(in) :: from
      if (allocated(buf)) deallocate(buf)
   end subroutine draw_vfree

end module m_draw_valloc
