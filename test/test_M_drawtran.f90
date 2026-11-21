! Automated Unit Test Suite (test_M_drawtran.f90)

!   This program tests 1-based matrix/tensor math, transformation
!   stacks, attribute push/pop, coordinate mapping, display-list objects
!   (makeobj/saveobj/loadobj/callobj), and file generation across multiple
!   drivers (nil, svg, ps, pdf, ppm).

program test_mdraw_unit
   use, intrinsic :: iso_fortran_env, only: real32, int16, error_unit
   use m_core
   implicit none

   integer :: failures = 0

   write(*, '(A)') "=== Running M_DRAW Fortran Unit Tests ==="

   call test_matrix_and_tensor(failures)
   call test_stacks_and_viewport(failures)
   call test_objects_and_io(failures)
   call test_file_drivers(failures)

   if (failures == 0) then
      write(*, '(A)') "ALL TESTS PASSED."
   else
      write(error_unit, '(A, I0, A)') "FAILED: ", failures, " test(s) failed."
      error stop 1
   end if

contains

   subroutine assert_near(actual, expected, tol, label, fail_count)
      real(real32), intent(in) :: actual, expected, tol
      character(len=*), intent(in) :: label
      integer, intent(inout) :: fail_count
      if (abs(actual - expected) > tol) then
         write(error_unit, '(A, A, A, F12.6, A, F12.6)') &
            "[FAIL] ", label, ": got ", actual, ", expected ", expected
         fail_count = fail_count + 1
      end if
   end subroutine assert_near

   subroutine assert_int(actual, expected, label, fail_count)
      integer, intent(in) :: actual, expected
      character(len=*), intent(in) :: label
      integer, intent(inout) :: fail_count
      if (actual /= expected) then
         write(error_unit, '(A, A, A, I0, A, I0)') &
            "[FAIL] ", label, ": got ", actual, ", expected ", expected
         fail_count = fail_count + 1
      end if
   end subroutine assert_int

   subroutine test_matrix_and_tensor(fail_count)
      integer, intent(inout) :: fail_count
      real(real32) :: m1(4,4), m2(4,4), m3(4,4)
      real(real32) :: v(4), res(4)
      real(real32) :: t1(4,4,4), t2(4,4,4)
      integer :: i, j, k

      write(*, '(A)') "-- Testing 1-based Matrix & Tensor Math --"
      call draw_identmatrix(m1)
      call assert_near(m1(1,1), 1.0_real32, 1.0e-6_real32, "ident(1,1)", fail_count)
      call assert_near(m1(4,4), 1.0_real32, 1.0e-6_real32, "ident(4,4)", fail_count)
      call assert_near(m1(1,4), 0.0_real32, 1.0e-6_real32, "ident(1,4)", fail_count)

      ! Verify row-vector * matrix multiplication (1-based)
      m2 = m1
      m2(4, 1:3) = [10.0_real32, 20.0_real32, 30.0_real32] ! translation row
      v = [1.0_real32, 2.0_real32, 3.0_real32, 1.0_real32]
      call draw_multvector(res, v, m2)
      call assert_near(res(1), 11.0_real32, 1.0e-6_real32, "multvector X", fail_count)
      call assert_near(res(2), 22.0_real32, 1.0e-6_real32, "multvector Y", fail_count)
      call assert_near(res(3), 33.0_real32, 1.0e-6_real32, "multvector Z", fail_count)
      call assert_near(res(4),  1.0_real32, 1.0e-6_real32, "multvector W", fail_count)

      ! Verify tensor transpose copy
      do i = 1, 4
         do j = 1, 4
            do k = 1, 4
               t1(i, j, k) = real(100*i + 10*j + k, real32)
            end do
         end do
      end do
      call draw_copytensortrans(t2, t1)
      call assert_near(t2(2, 3, 4), t1(2, 4, 3), 1.0e-6_real32, "copytensortrans(2,3,4)", fail_count)

      call draw_copytranspose(m3, m2)
      call assert_near(m3(1, 4), 10.0_real32, 1.0e-6_real32, "copytranspose(1,4)", fail_count)
   end subroutine test_matrix_and_tensor

   subroutine test_stacks_and_viewport(fail_count)
      integer, intent(inout) :: fail_count
      real(real32) :: gx, gy, gz, l, r, b, t

      write(*, '(A)') "-- Testing Stacks, Graphics Position & Viewports --"
      call draw_prefsize(800, 600)
      call draw_vinit("nil")

      call assert_near(draw_getaspect(), 600.0_real32 / 800.0_real32, 1.0e-5_real32, "getaspect", fail_count)

      ! Graphics position tracking
      call draw_move(1.5_real32, -2.5_real32, 3.0_real32)
      call draw_rmove(0.5_real32, 1.5_real32, -1.0_real32)
      call draw_getgp(gx, gy, gz)
      call assert_near(gx,  2.0_real32, 1.0e-6_real32, "getgp X", fail_count)
      call assert_near(gy, -1.0_real32, 1.0e-6_real32, "getgp Y", fail_count)
      call assert_near(gz,  2.0_real32, 1.0e-6_real32, "getgp Z", fail_count)

      ! Viewport push/pop
      call draw_viewport(-0.5_real32, 0.5_real32, -0.25_real32, 0.75_real32)
      call draw_pushviewport()
      call draw_viewport(-1.0_real32, 1.0_real32, -1.0_real32, 1.0_real32)
      call draw_popviewport()
      call draw_getviewport(l, r, b, t)
      call assert_near(l, -0.5_real32, 1.0e-6_real32, "viewport left", fail_count)
      call assert_near(r,  0.5_real32, 1.0e-6_real32, "viewport right", fail_count)
      call assert_near(b, -0.25_real32, 1.0e-6_real32, "viewport bottom", fail_count)
      call assert_near(t,  0.75_real32, 1.0e-6_real32, "viewport top", fail_count)

      ! Attribute push/pop
      call draw_color(RED)
      call draw_pushattributes()
      call draw_color(BLUE)
      call assert_int(vdevice%attr%a%color, BLUE, "pushed color", fail_count)
      call draw_popattributes()
      call assert_int(vdevice%attr%a%color, RED, "popped color", fail_count)

      call draw_vexit()
   end subroutine test_stacks_and_viewport

   subroutine test_objects_and_io(fail_count)
      integer, intent(inout) :: fail_count
      integer :: obj_id
      real(real32) :: gx, gy
      logical :: exists

      write(*, '(A)') "-- Testing Display-List Objects & Serialization --"
      call draw_vinit("nil")

      obj_id = draw_genobj()
      call draw_makeobj(obj_id)
      call draw_move2(0.0_real32, 0.0_real32)
      call draw_draw2(0.5_real32, 0.25_real32)
      call draw_circle(0.1_real32, 0.1_real32, 0.2_real32)
      call draw_closeobj()

      call assert_int(draw_isobj(obj_id), 1, "isobj before save", fail_count)

      call draw_saveobj(obj_id, "test_obj.bin")
      inquire(file="test_obj.bin", exist=exists)
      call assert_int(merge(1, 0, exists), 1, "saveobj file exists", fail_count)

      call draw_delobj(obj_id)
      call assert_int(draw_isobj(obj_id), 0, "isobj after del", fail_count)

      call draw_loadobj(obj_id, "test_obj.bin")
      call assert_int(draw_isobj(obj_id), 1, "isobj after load", fail_count)

      call draw_callobj(obj_id)
      call draw_getgp2(gx, gy)
      ! Circle ends at (x + radius, y) = (0.3, 0.1)
      call assert_near(gx, 0.3_real32, 1.0e-5_real32, "callobj final gp X", fail_count)
      call assert_near(gy, 0.1_real32, 1.0e-5_real32, "callobj final gp Y", fail_count)

      call draw_delobj(obj_id)
      call draw_vexit()
   end subroutine test_objects_and_io

   subroutine test_file_drivers(fail_count)
      integer, intent(inout) :: fail_count
      character(len=16), parameter :: drivers(5) = [ &
         "svg             ", &
         "ps              ", &
         "pdf             ", &
         "p3              ", &
         "canvas          " ]
      character(len=32) :: fname
      integer :: i, sz
      logical :: exists
      real(real32) :: tri(3, 2)

      write(*, '(A)') "-- Testing File Output Drivers --"
      tri(1, :) = [-0.5_real32, -0.5_real32]
      tri(2, :) = [ 0.5_real32, -0.5_real32]
      tri(3, :) = [ 0.0_real32,  0.5_real32]

      do i = 1, size(drivers)
         fname = "unit_out_" // trim(drivers(i)) // ".dat"
         call draw_voutput(trim(fname))
         call draw_vinit(trim(drivers(i)))
         call draw_ortho2(-1.0_real32, 1.0_real32, -1.0_real32, 1.0_real32)
         call draw_color(RED)
         call draw_polyfill(1)
         call draw_poly2(3, tri)
         call draw_polyfill(0)
         call draw_color(BLUE)
         call draw_rect(-0.8_real32, -0.8_real32, 0.8_real32, 0.8_real32)
         call draw_vexit()

         inquire(file=trim(fname), exist=exists, size=sz)
         call assert_int(merge(1, 0, exists .and. sz > 0), 1, "driver output: " // trim(drivers(i)), fail_count)
      end do
   end subroutine test_file_drivers

end program test_mdraw_unit
