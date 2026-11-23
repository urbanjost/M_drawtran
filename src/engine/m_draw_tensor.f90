module m_draw_tensor
   use, intrinsic :: iso_fortran_env, only: real32
   implicit none
   private
   public :: draw_premulttensor, draw_multtensor, draw_copytensor, draw_copytensortrans

contains

   subroutine draw_premulttensor(c, a, b)
      real(real32), intent(out) :: c(4,4,4)
      real(real32), intent(in)  :: a(4,4), b(4,4,4)
      integer :: i, j, k
      real(real32) :: x1, x2, x3, x4

      do i = 1, 4
         do j = 1, 4
            x1 = 0.0_real32; x2 = 0.0_real32; x3 = 0.0_real32; x4 = 0.0_real32
            do k = 1, 4
               x1 = x1 + a(i, k) * b(1, k, j)
               x2 = x2 + a(i, k) * b(2, k, j)
               x3 = x3 + a(i, k) * b(3, k, j)
               x4 = x4 + a(i, k) * b(4, k, j)
            end do
            c(1, i, j) = x1
            c(2, i, j) = x2
            c(3, i, j) = x3
            c(4, i, j) = x4
         end do
      end do
   end subroutine draw_premulttensor

   subroutine draw_multtensor(c, a, b)
      real(real32), intent(out) :: c(4,4,4)
      real(real32), intent(in)  :: a(4,4), b(4,4,4)
      integer :: i, j, k
      real(real32) :: x1, x2, x3, x4

      do i = 1, 4
         do j = 1, 4
            x1 = 0.0_real32; x2 = 0.0_real32; x3 = 0.0_real32; x4 = 0.0_real32
            do k = 1, 4
               x1 = x1 + b(1, i, k) * a(k, j)
               x2 = x2 + b(2, i, k) * a(k, j)
               x3 = x3 + b(3, i, k) * a(k, j)
               x4 = x4 + b(4, i, k) * a(k, j)
            end do
            c(1, i, j) = x1
            c(2, i, j) = x2
            c(3, i, j) = x3
            c(4, i, j) = x4
         end do
      end do
   end subroutine draw_multtensor

   subroutine draw_copytensor(b, a)
      real(real32), intent(out) :: b(4,4,4)
      real(real32), intent(in)  :: a(4,4,4)
      b = a
   end subroutine draw_copytensor

   subroutine draw_copytensortrans(b, a)
      real(real32), intent(out) :: b(4,4,4)
      real(real32), intent(in)  :: a(4,4,4)
      integer :: i, j, k
      do i = 1, 4
         do j = 1, 4
            do k = 1, 4
               b(i, j, k) = a(i, k, j)
            end do
         end do
      end do
   end subroutine draw_copytensortrans

end module m_draw_tensor
