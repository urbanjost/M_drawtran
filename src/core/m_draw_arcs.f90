module m_draw_arcs
   use, intrinsic :: iso_fortran_env, only: real32, real64
   use m_draw_types
   use m_draw_verror
   use m_draw_move
   use m_draw_draw
   implicit none
   private
   public :: draw_arcprecision, draw_circleprecision, draw_arc, draw_sector, draw_circle

   integer, save :: nsegs = 32

   interface
      function draw_newtokens(num) result(toks)
         import :: Token
         integer, intent(in) :: num
         type(Token), pointer :: toks(:)
      end function draw_newtokens

      subroutine draw_makepoly()
      end subroutine draw_makepoly

      subroutine draw_closepoly()
      end subroutine draw_closepoly
   end interface

contains

   subroutine draw_arcprecision(noseg)
      integer, intent(in) :: noseg
      nsegs = noseg
   end subroutine draw_arcprecision

   subroutine draw_circleprecision(noseg)
      integer, intent(in) :: noseg
      nsegs = noseg
   end subroutine draw_circleprecision

   subroutine draw_arc(x, y, radius, startang, endang)
      real(real32), intent(in) :: x, y, radius, startang, endang
      type(Token), pointer :: t(:)
      real(real32) :: cx, cy, dx, dy
      real(real32) :: deltang, cosine, sine, angle
      integer :: sync, i, numsegs, idum

      if (vdevice%initialized == 0) call draw_verror("arc: draw not initialized")

      sync = vdevice%sync
      if (sync /= 0) vdevice%sync = D_FALSE

      angle = startang * D2R
      numsegs = int(abs(endang - startang) / 360.0_real32 * nsegs + 0.5_real32)
      deltang = (endang - startang) * D2R / numsegs
      cosine = real(cos(real(deltang, real64)), real32)
      sine = real(sin(real(deltang, real64)), real32)

      if (vdevice%inobject /= 0) then
         t => draw_newtokens(8)
         t(1)%i = OBJ_ARC
         t(2)%f = x
         t(3)%f = y
         t(4)%f = radius * real(cos(real(angle, real64)), real32)
         t(5)%f = radius * real(sin(real(angle, real64)), real32)
         t(6)%f = cosine
         t(7)%f = sine
         t(8)%i = numsegs
         return
      end if

      cx = x + radius * real(cos(real(angle, real64)), real32)
      cy = y + radius * real(sin(real(angle, real64)), real32)
      call draw_move2(cx, cy)

      do i = 1, numsegs
         dx = cx - x
         dy = cy - y
         cx = x + dx * cosine - dy * sine
         cy = y + dx * sine + dy * cosine
         call draw_draw2(cx, cy)
      end do

      if (sync /= 0) then
         vdevice%sync = D_TRUE
         idum = vdevice%dev%Vsync()
      end if
   end subroutine draw_arc

   subroutine draw_sector(x, y, radius, startang, endang)
      real(real32), intent(in) :: x, y, radius, startang, endang
      type(Token), pointer :: t(:)
      real(real32) :: cx, cy, dx, dy
      real(real32) :: deltang, cosine, sine, angle
      integer :: sync, i, numsegs, inpoly

      if (vdevice%initialized == 0) call draw_verror("segment: draw not initialized")

      angle = startang * D2R
      numsegs = int(abs(endang - startang) / 360.0_real32 * nsegs + 0.5_real32)
      deltang = (endang - startang) * D2R / numsegs
      cosine = real(cos(real(deltang, real64)), real32)
      sine = real(sin(real(deltang, real64)), real32)

      if (vdevice%inobject /= 0) then
         t => draw_newtokens(8)
         t(1)%i = OBJ_SECTOR
         t(2)%f = x
         t(3)%f = y
         t(4)%f = radius * real(cos(real(angle, real64)), real32)
         t(5)%f = radius * real(sin(real(angle, real64)), real32)
         t(6)%f = cosine
         t(7)%f = sine
         t(8)%i = numsegs
         return
      end if

      sync = vdevice%sync
      if (sync /= 0) vdevice%sync = D_FALSE

      inpoly = vdevice%inpolygon
      if ((vdevice%attr%a%fill /= 0 .or. vdevice%attr%a%hatch /= 0) .and. inpoly == 0) then
         call draw_makepoly()
      end if

      call draw_move2(x, y)
      cx = x + radius * real(cos(real(angle, real64)), real32)
      cy = y + radius * real(sin(real(angle, real64)), real32)
      call draw_draw2(cx, cy)

      do i = 1, numsegs
         dx = cx - x
         dy = cy - y
         cx = x + dx * cosine - dy * sine
         cy = y + dx * sine + dy * cosine
         call draw_draw2(cx, cy)
      end do

      if ((vdevice%attr%a%fill /= 0 .or. vdevice%attr%a%hatch /= 0) .and. inpoly == 0) then
         call draw_closepoly()
      else
         if (sync /= 0) vdevice%sync = D_TRUE
         call draw_draw2(x, y)
      end if
   end subroutine draw_sector

   subroutine draw_circle(x, y, radius)
      real(real32), intent(in) :: x, y, radius
      type(Token), pointer :: t(:)
      real(real32) :: cx, cy, dx, dy
      real(real32) :: angle, cosine, sine
      integer :: sync, i, inpoly, idum
      logical :: need_to_close

      if (vdevice%initialized == 0) call draw_verror("circle: draw not initialized")

      angle = 2.0_real32 * PI / nsegs
      cosine = real(cos(real(angle, real64)), real32)
      sine = real(sin(real(angle, real64)), real32)

      if (vdevice%inobject /= 0) then
         t => draw_newtokens(7)
         t(1)%i = OBJ_CIRCLE
         t(2)%f = x
         t(3)%f = y
         t(4)%f = radius
         t(5)%f = cosine
         t(6)%f = sine
         t(7)%i = nsegs
         return
      end if

      sync = vdevice%sync
      if (sync /= 0) vdevice%sync = D_FALSE

      cx = x + radius
      cy = y
      inpoly = vdevice%inpolygon

      if ((vdevice%attr%a%fill /= 0 .or. vdevice%attr%a%hatch /= 0) .and. inpoly == 0) then
         call draw_makepoly()
         need_to_close = .true.
      else
         need_to_close = .false.
      end if

      call draw_move2(cx, cy)
      do i = 1, nsegs - 1
         dx = cx - x
         dy = cy - y
         cx = x + dx * cosine - dy * sine
         cy = y + dx * sine + dy * cosine
         call draw_draw2(cx, cy)
      end do

      if (need_to_close) then
         call draw_closepoly()
      else
         call draw_draw2(x + radius, y)
      end if

      if (sync /= 0) then
         vdevice%sync = D_TRUE
         idum = vdevice%dev%Vsync()
      end if
   end subroutine draw_circle

end module m_draw_arcs
