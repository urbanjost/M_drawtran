module m_draw_draw
   use, intrinsic :: iso_fortran_env, only: real32
   use m_draw_types
   use m_draw_verror
   use m_draw_matrix
   use m_draw_mapping
   use m_draw_clip
   implicit none
   private
   public :: draw_dashcode, draw_linestyle, draw_dashline, draw_draw, &
             draw_draw2, draw_rdraw, draw_rdraw2, draw_sdraw2, draw_rsdraw2

   logical, save :: do_dash = .true.

   interface
      function draw_newtokens(num) result(toks)
         import :: Token
         integer, intent(in) :: num
         type(Token), pointer :: toks(:)
      end function draw_newtokens
   end interface

contains

   logical function is_near(a, b)
      real(real32), intent(in) :: a, b
      is_near = (((a - b) / a) < 0.0001_real32)
   end function is_near

   logical function checkdash(line)
      type(Attribute), intent(inout) :: line
      line%dash_pos = line%dash_pos + 1
      if (line%dash_pos > len(line%style)) then
         line%dash_pos = 1
      end if
      checkdash = (line%style(line%dash_pos:line%dash_pos) /= '0')
   end function checkdash

   subroutine draw_dashcode(d_in)
      real(real32), intent(in) :: d_in
      real(real32) :: d
      type(Token), pointer :: tok(:)

      d = abs(d_in)
      if (d == 0.0_real32) d = 0.1_real32

      if (vdevice%inobject /= 0) then
         tok => draw_newtokens(2)
         tok(1)%i = OBJ_DASHCODE
         tok(2)%f = d
         return
      end if

      vdevice%attr%a%dash = d
   end subroutine draw_dashcode

   subroutine draw_linestyle(l)
      character(len=*), intent(in) :: l
      type(Attribute), pointer :: line
      type(Token), pointer :: tok(:)
      integer :: i

      if (vdevice%inobject /= 0) then
         tok => draw_newtokens(2)
         tok(1)%i = OBJ_LINESTYLE
         tok(2)%s = l
         return
      end if

      line => vdevice%attr%a

      if (len(l) == 0) then
         if (allocated(line%style)) deallocate(line%style)
      else
         do i = 1, len(l)
            if (l(i:i) == '0' .or. l(i:i) == ' ') then
               line%style = l
               line%dash_pos = 1
               line%adist = 0.0_real32
               do_dash = (line%style(1:1) /= '0')
               return
            end if
         end do
         if (allocated(line%style)) deallocate(line%style)
      end if
   end subroutine draw_linestyle

   subroutine draw_dashline(p0, p1)
      real(real32), intent(inout) :: p0(4), p1(4)
      integer :: vx, vy, sync, idum
      real(real32) :: dx, dy, dz, dw, dist, ldist, tdist
      real(real32) :: pd(4)
      type(Attribute), pointer :: line

      line => vdevice%attr%a
      if (line%dash == 0.0_real32) then
         vx = draw_WtoVx(p1)
         vy = draw_WtoVy(p1)
         idum = vdevice%dev%Vdraw(vx, vy)
         vdevice%cpVx = vx
         vdevice%cpVy = vy
         return
      end if

      dx = p1(D_X) - p0(D_X)
      dy = p1(D_Y) - p0(D_Y)
      dz = p1(D_Z) - p0(D_Z)
      dw = p1(D_W) - p0(D_W)
      ldist = sqrt(dx*dx + dy*dy + dz*dz + dw*dw)

      if (ldist <= (line%dash - line%adist)) then
         if (is_near(line%dash, line%adist)) then
            line%adist = 0.0_real32
            do_dash = checkdash(line)
         end if
         line%adist = line%adist + ldist

         vx = draw_WtoVx(p1)
         vy = draw_WtoVy(p1)
         if (do_dash) idum = vdevice%dev%Vdraw(vx, vy)

         vdevice%cpVx = vx
         vdevice%cpVy = vy
         return
      else
         sync = vdevice%sync
         if (sync /= 0) vdevice%sync = 0

         tdist = 0.0_real32
         call draw_copyvector(pd, p0)

         if (line%adist > 0.0_real32) then
            tdist = line%dash - line%adist
            if (is_near(line%dash, line%adist)) then
               line%adist = 0.0_real32
               do_dash = checkdash(line)
            end if
            line%adist = line%adist + tdist

            dist = tdist / ldist
            pd(D_X) = pd(D_X) + dx * dist
            pd(D_Y) = pd(D_Y) + dy * dist
            pd(D_Z) = pd(D_Z) + dz * dist
            pd(D_W) = pd(D_W) + dw * dist
            vx = draw_WtoVx(pd)
            vy = draw_WtoVy(pd)

            if (do_dash) idum = vdevice%dev%Vdraw(vx, vy)
            vdevice%cpVx = vx
            vdevice%cpVy = vy
         end if

         dx = dx * (line%dash / ldist)
         dy = dy * (line%dash / ldist)
         dz = dz * (line%dash / ldist)
         dw = dw * (line%dash / ldist)
         dist = line%dash

         do while (tdist <= ldist - dist)
            if (is_near(line%dash, line%adist)) then
               line%adist = 0.0_real32
               do_dash = checkdash(line)
            end if

            pd(D_X) = pd(D_X) + dx
            pd(D_Y) = pd(D_Y) + dy
            pd(D_Z) = pd(D_Z) + dz
            pd(D_W) = pd(D_W) + dw

            vx = draw_WtoVx(pd)
            vy = draw_WtoVy(pd)

            line%adist = line%adist + dist
            tdist = tdist + dist

            if (do_dash) idum = vdevice%dev%Vdraw(vx, vy)
            vdevice%cpVx = vx
            vdevice%cpVy = vy
         end do

         if (is_near(line%dash, line%adist)) then
            line%adist = 0.0_real32
            do_dash = checkdash(line)
         end if

         dx = p1(D_X) - pd(D_X)
         dy = p1(D_Y) - pd(D_Y)
         dz = p1(D_Z) - pd(D_Z)
         dw = p1(D_W) - pd(D_W)
         dist = sqrt(dx*dx + dy*dy + dz*dz + dw*dw)

         line%adist = line%adist + dist
         vx = draw_WtoVx(p1)
         vy = draw_WtoVy(p1)

         if (do_dash) idum = vdevice%dev%Vdraw(vx, vy)
         vdevice%cpVx = vx
         vdevice%cpVy = vy
      end if

      if (sync /= 0) then
         vdevice%sync = 1
         idum = vdevice%dev%Vsync()
      end if
   end subroutine draw_dashline

   subroutine draw_draw(x, y, z)
      real(real32), intent(in) :: x, y, z
      type(Token), pointer :: tok(:)
      integer :: vx, vy, idum
      real(real32) :: res(4)

      if (vdevice%initialized == 0) then
         call draw_verror("draw: draw not initialized")
      end if

      if (vdevice%inpolygon /= 0) then
         call vdevice%pdraw(x, y, z)
         vdevice%cpW(D_X) = x
         vdevice%cpW(D_Y) = y
         vdevice%cpW(D_Z) = z
         vdevice%cpVvalid = 0
         return
      end if

      if (vdevice%inobject /= 0) then
         tok => draw_newtokens(4)
         tok(1)%i = OBJ_DRAW
         tok(2)%f = x
         tok(3)%f = y
         tok(4)%f = z

         vdevice%cpW(D_X) = x
         vdevice%cpW(D_Y) = y
         vdevice%cpW(D_Z) = z
         vdevice%cpVvalid = 0
         return
      end if

      if (vdevice%cpVvalid == 0) then
         call draw_multvector(vdevice%cpWtrans, vdevice%cpW, vdevice%transmat%m)
      end if

      vdevice%cpW(D_X) = x
      vdevice%cpW(D_Y) = y
      vdevice%cpW(D_Z) = z
      call draw_multvector(res, vdevice%cpW, vdevice%transmat%m)

      if (vdevice%clipoff /= 0) then
         vx = draw_WtoVx(res)
         vy = draw_WtoVy(res)

         if (allocated(vdevice%attr%a%style)) then
            call draw_dashline(vdevice%cpWtrans, res)
            vdevice%cpVvalid = 0
            return
         end if

         idum = vdevice%dev%Vdraw(vx, vy)
         vdevice%cpVx = vx
         vdevice%cpVy = vy
         vdevice%cpVvalid = 0
      else
         if (vdevice%cpVvalid /= 0) then
            call draw_quickclip(vdevice%cpWtrans, res)
         else
            call draw_clip(vdevice%cpWtrans, res)
         end if
      end if

      vdevice%cpWtrans = res
   end subroutine draw_draw

   subroutine draw_draw2(x, y)
      real(real32), intent(in) :: x, y
      if (vdevice%initialized == 0) call draw_verror("draw2: draw not initialized")
      call draw_draw(x, y, 0.0_real32)
   end subroutine draw_draw2

   subroutine draw_rdraw(dx, dy, dz)
      real(real32), intent(in) :: dx, dy, dz
      if (vdevice%initialized == 0) call draw_verror("rdraw: draw not initialized")
      call draw_draw(vdevice%cpW(D_X) + dx, vdevice%cpW(D_Y) + dy, vdevice%cpW(D_Z) + dz)
   end subroutine draw_rdraw

   subroutine draw_rdraw2(dx, dy)
      real(real32), intent(in) :: dx, dy
      if (vdevice%initialized == 0) call draw_verror("rdraw2: draw not initialized")
      call draw_draw(vdevice%cpW(D_X) + dx, vdevice%cpW(D_Y) + dy, 0.0_real32)
   end subroutine draw_rdraw2

   subroutine draw_sdraw2(xs, ys)
      real(real32), intent(in) :: xs, ys
      integer :: nx, ny, idum
      if (vdevice%initialized == 0) call draw_verror("sdraw2: draw not initialized")

      nx = int((xs / 2.0_real32 + 0.5_real32) * vdevice%sizeX)
      ny = int((0.5_real32 + ys / 2.0_real32) * vdevice%sizeY)

      idum = vdevice%dev%Vdraw(nx, ny)
      vdevice%cpVx = nx
      vdevice%cpVy = ny
      vdevice%cpVvalid = 0
   end subroutine draw_sdraw2

   subroutine draw_rsdraw2(dxs, dys)
      real(real32), intent(in) :: dxs, dys
      integer :: ndx, ndy, idum
      if (vdevice%initialized == 0) call draw_verror("rsdraw2: draw not initialized")

      ndx = int(dxs * vdevice%sizeSx / 2.0_real32)
      ndy = int(dys * vdevice%sizeSy / 2.0_real32)

      idum = vdevice%dev%Vdraw(vdevice%cpVx + ndx, vdevice%cpVy + ndy)
      vdevice%cpVx = vdevice%cpVx + ndx
      vdevice%cpVy = vdevice%cpVy + ndy
   end subroutine draw_rsdraw2

end module m_draw_draw
