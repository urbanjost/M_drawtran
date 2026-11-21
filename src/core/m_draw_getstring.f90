module m_draw_getstring
   use, intrinsic :: iso_fortran_env, only: real32
   use m_draw_types
   use m_draw_move
   use m_draw_text
   implicit none
   private
   public :: draw_getstring

   integer, parameter :: EOT = 4, BS = 8, DEL = 127, CR = 13

   interface
      integer function draw_getkey()
      end function draw_getkey

      subroutine draw_color(i)
         integer, intent(in) :: i
      end subroutine draw_color
   end interface

contains

   integer function draw_getstring(bcol, s)
      integer, intent(in) :: bcol
      character(len=*), intent(out) :: s
      integer :: i, nc, pos, col, slen
      real(real32) :: cw, ch

      s = ""
      slen = len(s)
      pos = 0
      nc = draw_numchars() + 32
      col = vdevice%attr%a%color

      do
         i = draw_getkey()
         if (i == CR) exit
         select case (i)
         case (-1, EOT)
            draw_getstring = pos
            return
         case (BS, DEL)
            if (pos >= 1) then
               call draw_getcharsize(s(pos:pos), cw, ch)
               call draw_rmove(-cw, 0.0_real32, 0.0_real32)
               call draw_color(bcol)
               call draw_drawchar(ichar(s(pos:pos)))
               call draw_rmove(-cw, 0.0_real32, 0.0_real32)
               call draw_color(col)
               s(pos:pos) = ' '
               pos = pos - 1
            end if
         case default
            if (i > nc .or. i < 32) cycle
            if (pos < slen) then
               pos = pos + 1
               s(pos:pos) = char(i)
               call draw_drawchar(i)
            end if
         end select
      end do

      draw_getstring = pos
   end function draw_getstring

end module m_draw_getstring
