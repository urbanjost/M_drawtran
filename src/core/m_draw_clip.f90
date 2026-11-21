module m_draw_clip
   use, intrinsic :: iso_fortran_env, only: real32
   use m_draw_types
   use m_draw_mapping
   implicit none
   private
   public :: draw_MakeEdgeCoords, draw_clip, draw_quickclip, draw_clipping

   interface draw_clipping
      module procedure draw_clipping_integer
      module procedure draw_clipping_logical
   end interface draw_clipping

   integer, parameter :: planes(6) = [1, 2, 4, 8, 16, 32]
   real(real32), save :: wc(2, 6) = 0.0_real32

   interface
      subroutine draw_dashline(p0, p1)
         import :: real32
         real(real32), intent(inout) :: p0(4), p1(4)
      end subroutine draw_dashline
   end interface

contains

   integer function draw_MakeEdgeCoords(i, p)
      integer, intent(in) :: i
      real(real32), intent(in) :: p(4)
      integer :: j, k

      k = 0
      wc(i, 1) = p(D_W) + p(D_X)
      wc(i, 2) = p(D_W) - p(D_X)
      wc(i, 3) = p(D_W) + p(D_Y)
      wc(i, 4) = p(D_W) - p(D_Y)
      wc(i, 5) = p(D_W) + p(D_Z)
      wc(i, 6) = p(D_W) - p(D_Z)

      do j = 1, 6
         if (wc(i, j) < 0.0_real32) then
            k = ior(k, planes(j))
         end if
      end do
      draw_MakeEdgeCoords = k
   end function draw_MakeEdgeCoords

   subroutine draw_clip(p0, p1)
      real(real32), intent(inout) :: p0(4), p1(4)
      real(real32) :: t, t1, t2, dx, dy, dz, dw
      integer :: vx, vy, c1, c2, i, idum

      c1 = draw_MakeEdgeCoords(1, p0)
      c2 = draw_MakeEdgeCoords(2, p1)

      if (iand(c1, c2) == 0) then
         t1 = 0.0_real32
         t2 = 1.0_real32
         do i = 1, 6
            if (wc(1, i) < 0.0_real32 .or. wc(2, i) < 0.0_real32) then
               t = wc(1, i) / (wc(1, i) - wc(2, i))
               if (wc(1, i) < 0.0_real32) then
                  if (t > t1) t1 = t
               else if (t < t2) then
                  t2 = t
               end if
            end if
         end do

         if (t2 >= t1) then
            vdevice%cpVvalid = 1
            dx = p1(D_X) - p0(D_X)
            dy = p1(D_Y) - p0(D_Y)
            dz = p1(D_Z) - p0(D_Z)
            dw = p1(D_W) - p0(D_W)
            if (t2 /= 1.0_real32) then
               p1(D_X) = p0(D_X) + t2 * dx
               p1(D_Y) = p0(D_Y) + t2 * dy
               p1(D_Z) = p0(D_Z) + t2 * dz
               p1(D_W) = p0(D_W) + t2 * dw
               vdevice%cpVvalid = 0
            end if
            if (t1 /= 0.0_real32) then
               p0(D_X) = p0(D_X) + t1 * dx
               p0(D_Y) = p0(D_Y) + t1 * dy
               p0(D_Z) = p0(D_Z) + t1 * dz
               p0(D_W) = p0(D_W) + t1 * dw
            end if
            vdevice%cpVx = draw_WtoVx(p0)
            vdevice%cpVy = draw_WtoVy(p0)

            if (allocated(vdevice%attr%a%style)) then
               call draw_dashline(p0, p1)
               return
            end if

            vx = draw_WtoVx(p1)
            vy = draw_WtoVy(p1)
            idum = vdevice%dev%Vdraw(vx, vy)
            vdevice%cpVx = vx
            vdevice%cpVy = vy
         end if
      end if
   end subroutine draw_clip

   subroutine draw_quickclip(p0, p1)
      real(real32), intent(inout) :: p0(4), p1(4)
      real(real32) :: t, t1
      integer :: vx, vy, i, idum

      t1 = 1.0_real32

      wc(1, 1) = p0(D_W) + p0(D_X)
      wc(1, 2) = p0(D_W) - p0(D_X)
      wc(1, 3) = p0(D_W) + p0(D_Y)
      wc(1, 4) = p0(D_W) - p0(D_Y)
      wc(1, 5) = p0(D_W) + p0(D_Z)
      wc(1, 6) = p0(D_W) - p0(D_Z)
      wc(2, 1) = p1(D_W) + p1(D_X)
      wc(2, 2) = p1(D_W) - p1(D_X)
      wc(2, 3) = p1(D_W) + p1(D_Y)
      wc(2, 4) = p1(D_W) - p1(D_Y)
      wc(2, 5) = p1(D_W) + p1(D_Z)
      wc(2, 6) = p1(D_W) - p1(D_Z)

      do i = 1, 6
         if (wc(1, i) >= 0.0_real32 .and. wc(2, i) < 0.0_real32) then
            t = wc(1, i) / (wc(1, i) - wc(2, i))
            if (t < t1) t1 = t
         end if
      end do

      vdevice%cpVvalid = 1
      if (t1 /= 1.0_real32) then
         p1(D_X) = p0(D_X) + t1 * (p1(D_X) - p0(D_X))
         p1(D_Y) = p0(D_Y) + t1 * (p1(D_Y) - p0(D_Y))
         p1(D_Z) = p0(D_Z) + t1 * (p1(D_Z) - p0(D_Z))
         p1(D_W) = p0(D_W) + t1 * (p1(D_W) - p0(D_W))
         vdevice%cpVvalid = 0
      end if

      if (allocated(vdevice%attr%a%style)) then
         call draw_dashline(p0, p1)
         return
      end if

      vx = draw_WtoVx(p1)
      vy = draw_WtoVy(p1)
      idum = vdevice%dev%Vdraw(vx, vy)
      vdevice%cpVx = vx
      vdevice%cpVy = vy
   end subroutine draw_quickclip

   subroutine draw_clipping_integer(onoff)
      integer, intent(in) :: onoff
      if (onoff == 0) then
         vdevice%clipoff = 1
      else
         vdevice%clipoff = 0
      end if
   end subroutine draw_clipping_integer

   subroutine draw_clipping_logical(onoff)
      logical, intent(in) :: onoff
      call draw_clipping_integer(merge(1,0,onoff))
   end subroutine draw_clipping_logical

end module m_draw_clip
