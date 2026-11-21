module m_draw_pref
   use, intrinsic :: iso_fortran_env, only: error_unit
   implicit none
   private
   public :: draw_prefposition, draw_prefsize, draw_getprefposandsize

   integer, save :: px = -1, py = -1, pxs = -1, pys = -1

contains

   subroutine draw_prefposition(x, y)
      integer, intent(in) :: x, y

      if (x < -1) then
         write(error_unit, '(A, I0)') 'prefposition: bad x value ', x
         px = -1
         py = -1
      end if
      if (y < -1) then
         write(error_unit, '(A, I0)') 'prefposition: bad y value ', y
         px = -1
         py = -1
      end if
      px = x
      py = y
   end subroutine draw_prefposition

   subroutine draw_prefsize(x, y)
      integer, intent(in) :: x, y

      if (x < -1 .or. x == 0) then
         write(error_unit, '(A, I0)') 'prefsize: bad x value ', x
         px = -1
         py = -1
      end if
      if (y < -1 .or. y == 0) then
         write(error_unit, '(A, I0)') 'prefsize: bad y value ', y
         px = -1
         py = -1
      end if
      pxs = x
      pys = y
   end subroutine draw_prefsize

   subroutine draw_getprefposandsize(x, y, xs, ys)
      integer, intent(out) :: x, y, xs, ys

      x = px
      y = py
      xs = pxs
      ys = pys
   end subroutine draw_getprefposandsize

end module m_draw_pref
