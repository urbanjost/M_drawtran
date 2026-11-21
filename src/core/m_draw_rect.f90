module m_draw_rect
   use, intrinsic :: iso_fortran_env, only: real32
   use m_draw_types
   use m_draw_verror
   use m_draw_move
   use m_draw_draw
   implicit none
   private
   public :: draw_rect, draw_srect

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

   subroutine draw_rect(x1, y1, x2, y2)
      real(real32), intent(in) :: x1, y1, x2, y2
      logical :: started
      integer :: sync, idum
      type(Token), pointer :: tok(:)

      if (vdevice%initialized == 0) call draw_verror("rect: draw not initialized")

      if (vdevice%inobject /= 0) then
         tok => draw_newtokens(5)
         tok(1)%i = OBJ_RECT
         tok(2)%f = x1
         tok(3)%f = y1
         tok(4)%f = x2
         tok(5)%f = y2
         return
      end if

      if ((vdevice%attr%a%fill /= 0 .or. vdevice%attr%a%hatch /= 0) .and. vdevice%inpolygon == 0) then
         call draw_makepoly()
         started = .true.
      else
         started = .false.
      end if

      sync = vdevice%sync
      if (sync /= 0) vdevice%sync = 0

      call draw_move2(x1, y1)
      call draw_draw2(x2, y1)
      call draw_draw2(x2, y2)
      call draw_draw2(x1, y2)

      if (started) then
         call draw_closepoly()
      else
         call draw_draw2(x1, y1)
      end if

      if (sync /= 0) then
         vdevice%sync = 1
         idum = vdevice%dev%Vsync()
      end if
   end subroutine draw_rect

   subroutine draw_srect(x1, y1, x2, y2)
      real(real32), intent(in) :: x1, y1, x2, y2
      integer :: sync, idum

      if (vdevice%initialized == 0) call draw_verror("rect: draw not initialized")

      sync = vdevice%sync
      if (sync /= 0) vdevice%sync = 0

      call draw_smove2(x1, y1)
      call draw_sdraw2(x2, y1)
      call draw_sdraw2(x2, y2)
      call draw_sdraw2(x1, y2)
      call draw_sdraw2(x1, y1)

      if (sync /= 0) then
         vdevice%sync = 1
         idum = vdevice%dev%Vsync()
      end if
   end subroutine draw_srect

end module m_draw_rect
