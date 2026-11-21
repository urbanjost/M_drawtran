module m_draw_trans
   use, intrinsic :: iso_fortran_env, only: real32
   use m_draw_types
   use m_draw_verror
   implicit none
   private
   public :: draw_translate, draw_rotate, draw_scale

   interface
      function draw_newtokens(num) result(toks)
         import :: Token
         integer, intent(in) :: num
         type(Token), pointer :: toks(:)
      end function draw_newtokens
   end interface

contains

   subroutine draw_translate(x, y, z)
      real(real32), intent(in) :: x, y, z
      type(Token), pointer :: tok(:)
      integer :: j

      if (vdevice%initialized == 0) then
         call draw_verror("translate: draw not initialized")
      end if

      if (vdevice%inobject /= 0) then
         tok => draw_newtokens(4)
         tok(1)%i = OBJ_TRANSLATE
         tok(2)%f = x
         tok(3)%f = y
         tok(4)%f = z
         return
      end if

      do j = 1, 4
         vdevice%transmat%m(4, j) = vdevice%transmat%m(4, j) + &
            x * vdevice%transmat%m(1, j) + &
            y * vdevice%transmat%m(2, j) + &
            z * vdevice%transmat%m(3, j)
      end do
   end subroutine draw_translate

   subroutine draw_rotate(r, axis)
      real(real32), intent(in) :: r
      character(len=1), intent(in) :: axis
      type(Token), pointer :: tok(:)
      real(real32) :: costheta, sintheta, tmp
      integer :: j

      if (vdevice%initialized == 0) then
         call draw_verror("rotate: draw not initialized")
      end if

      if (vdevice%inobject /= 0) then
         tok => draw_newtokens(3)
         tok(1)%i = OBJ_ROTATE
         tok(2)%f = r
         tok(3)%i = ichar(axis)
         return
      end if

      costheta = cos(D2R * r)
      sintheta = sin(D2R * r)

      select case (axis)
      case ('x', 'X')
         do j = 1, 4
            tmp = vdevice%transmat%m(2, j)
            vdevice%transmat%m(2, j) = costheta * tmp + sintheta * vdevice%transmat%m(3, j)
            vdevice%transmat%m(3, j) = costheta * vdevice%transmat%m(3, j) - sintheta * tmp
         end do
      case ('y', 'Y')
         do j = 1, 4
            tmp = vdevice%transmat%m(1, j)
            vdevice%transmat%m(1, j) = costheta * tmp - sintheta * vdevice%transmat%m(3, j)
            vdevice%transmat%m(3, j) = sintheta * tmp + costheta * vdevice%transmat%m(3, j)
         end do
      case ('z', 'Z')
         do j = 1, 4
            tmp = vdevice%transmat%m(1, j)
            vdevice%transmat%m(1, j) = costheta * tmp + sintheta * vdevice%transmat%m(2, j)
            vdevice%transmat%m(2, j) = costheta * vdevice%transmat%m(2, j) - sintheta * tmp
         end do
      case default
         call draw_verror("rotate: illegal axis of rotation")
      end select
   end subroutine draw_rotate

    subroutine draw_scale(x, y, z)
       real(real32), intent(in) :: x, y, z
       type(Token), pointer :: tok(:)

       if (vdevice%initialized == 0) call draw_verror("scale: draw not initialized")

       if (vdevice%inobject /= 0) then
          tok => draw_newtokens(4)
          tok(1)%i = OBJ_SCALE
          tok(2)%f = x
          tok(3)%f = y
          tok(4)%f = z
          return
       end if

       vdevice%transmat%m(1, 1:4) = vdevice%transmat%m(1, 1:4) * x
       vdevice%transmat%m(2, 1:4) = vdevice%transmat%m(2, 1:4) * y
       vdevice%transmat%m(3, 1:4) = vdevice%transmat%m(3, 1:4) * z
    end subroutine draw_scale

end module m_draw_trans
