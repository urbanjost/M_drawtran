module m_draw_matrix
   use, intrinsic :: iso_fortran_env, only: real32, output_unit
   use m_draw_types
   use m_draw_verror
   implicit none
   private
   public :: draw_copyvector, draw_copymatrix, draw_copytranspose, &
             draw_getmatrix, draw_pushmatrix, draw_popmatrix, &
             draw_loadmatrix, draw_mult4x4, draw_multmatrix, &
             draw_identmatrix, draw_multvector, draw_premultvector, &
             draw_printmat, draw_printvect

   type(Mstack), pointer, save :: msfree => null()

   interface
      function draw_newtokens(num) result(toks)
         import :: Token
         integer, intent(in) :: num
         type(Token), pointer :: toks(:)
      end function draw_newtokens
   end interface

contains

   subroutine draw_copyvector(a, b)
      real(real32), intent(out) :: a(4)
      real(real32), intent(in)  :: b(4)
      a = b
   end subroutine draw_copyvector

   subroutine draw_copymatrix(a, b)
      real(real32), intent(out) :: a(4,4)
      real(real32), intent(in)  :: b(4,4)
      a = b
   end subroutine draw_copymatrix

   subroutine draw_copytranspose(a, b)
      real(real32), intent(out) :: a(4,4)
      real(real32), intent(in)  :: b(4,4)
      a = transpose(b)
   end subroutine draw_copytranspose

   subroutine draw_getmatrix(m)
      real(real32), intent(out) :: m(4,4)
      call draw_copymatrix(m, vdevice%transmat%m)
   end subroutine draw_getmatrix

   subroutine draw_pushmatrix()
      type(Mstack), pointer :: tmpmat
      type(Token), pointer :: p(:)

      if (vdevice%inobject /= 0) then
         p => draw_newtokens(1)
         p(1)%i = OBJ_PUSHMATRIX
         return
      end if

      if (associated(msfree)) then
         tmpmat => vdevice%transmat
         vdevice%transmat => msfree
         msfree => msfree%back
         vdevice%transmat%back => tmpmat
         call draw_copymatrix(vdevice%transmat%m, tmpmat%m)
      else
         allocate(tmpmat)
         tmpmat%back => vdevice%transmat
         call draw_copymatrix(tmpmat%m, vdevice%transmat%m)
         vdevice%transmat => tmpmat
      end if
   end subroutine draw_pushmatrix

   subroutine draw_popmatrix()
      type(Token), pointer :: p(:)
      type(Mstack), pointer :: oldtop

      if (vdevice%inobject /= 0) then
         p => draw_newtokens(1)
         p(1)%i = OBJ_POPMATRIX
         return
      end if

      if (.not. associated(vdevice%transmat%back)) then
         call draw_verror("popmatrix: matrix stack empty")
      else
         oldtop => vdevice%transmat
         vdevice%transmat => vdevice%transmat%back
         oldtop%back => msfree
         msfree => oldtop
      end if

      vdevice%cpVvalid = 0
   end subroutine draw_popmatrix

   subroutine draw_loadmatrix(mat)
      real(real32), intent(in) :: mat(4,4)
      integer :: i, j, k
      type(Token), pointer :: p(:)

      if (vdevice%initialized == 0) then
         call draw_verror("loadmatrix: draw not initialized")
      end if

      if (vdevice%inobject /= 0) then
         p => draw_newtokens(17)
         p(1)%i = OBJ_LOADMATRIX
         k = 2
         do i = 1, 4
            do j = 1, 4
               p(k)%f = mat(i, j)
               k = k + 1
            end do
         end do
         return
      end if

      vdevice%transmat%m = mat
      vdevice%cpVvalid = 0
   end subroutine draw_loadmatrix

   subroutine draw_mult4x4(a, b, c)
      real(real32), intent(out) :: a(4,4)
      real(real32), intent(in)  :: b(4,4), c(4,4)
      a = matmul(b, c)
   end subroutine draw_mult4x4

   subroutine draw_multmatrix(mat)
      real(real32), intent(in) :: mat(4,4)
      real(real32) :: prod(4,4)
      type(Token), pointer :: p(:)
      integer :: i, j, k

      if (vdevice%inobject /= 0) then
         p => draw_newtokens(17)
         p(1)%i = OBJ_MULTMATRIX
         k = 2
         do i = 1, 4
            do j = 1, 4
               p(k)%f = mat(i, j)
               k = k + 1
            end do
         end do
         return
      end if

      call draw_mult4x4(prod, mat, vdevice%transmat%m)
      call draw_loadmatrix(prod)
   end subroutine draw_multmatrix

   subroutine draw_identmatrix(a)
      real(real32), intent(out) :: a(4,4)
      integer :: i
      a = 0.0_real32
      do i = 1, 4
         a(i, i) = 1.0_real32
      end do
   end subroutine draw_identmatrix

   subroutine draw_multvector(v, a, b)
      real(real32), intent(out) :: v(4)
      real(real32), intent(in)  :: a(4), b(4,4)
      v(1) = a(1)*b(1,1) + a(2)*b(2,1) + a(3)*b(3,1) + a(4)*b(4,1)
      v(2) = a(1)*b(1,2) + a(2)*b(2,2) + a(3)*b(3,2) + a(4)*b(4,2)
      v(3) = a(1)*b(1,3) + a(2)*b(2,3) + a(3)*b(3,3) + a(4)*b(4,3)
      v(4) = a(1)*b(1,4) + a(2)*b(2,4) + a(3)*b(3,4) + a(4)*b(4,4)
   end subroutine draw_multvector

   subroutine draw_premultvector(v, a, b)
      real(real32), intent(out) :: v(4)
      real(real32), intent(in)  :: a(4), b(4,4)
      v(1) = a(1)*b(1,1) + a(2)*b(1,2) + a(3)*b(1,3) + a(4)*b(1,4)
      v(2) = a(1)*b(2,1) + a(2)*b(2,2) + a(3)*b(2,3) + a(4)*b(2,4)
      v(3) = a(1)*b(3,1) + a(2)*b(3,2) + a(3)*b(3,3) + a(4)*b(3,4)
      v(4) = a(1)*b(4,1) + a(2)*b(4,2) + a(3)*b(4,3) + a(4)*b(4,4)
   end subroutine draw_premultvector

   subroutine draw_printmat(s, m)
      character(len=*), intent(in) :: s
      real(real32), intent(in) :: m(4,4)
      integer :: i
      write(output_unit, '(A)') trim(s)
      do i = 1, 4
         write(output_unit, '(4F12.6)') m(i, 1:4)
      end do
   end subroutine draw_printmat

   subroutine draw_printvect(s, v)
      character(len=*), intent(in) :: s
      real(real32), intent(in) :: v(4)
      write(output_unit, '(A, 1X, 4F12.6)') trim(s), v(1:4)
   end subroutine draw_printvect

end module m_draw_matrix
