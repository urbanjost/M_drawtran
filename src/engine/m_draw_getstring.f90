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

!>
!!##NAME
!!    getstring(3f) - [M_draw:INTERACTIVE] Read in a string, echoing it in current font
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!          integer function getstring(bcol, string)
!!          integer bcol
!!          character *(*) string
!!##DESCRIPTION
!!
!!    Read in a string, echoing it in the current font, using the current
!!    color and the current transformation.
!!
!!    getstring(3f) interprets the Backspace key (ASCII 8) and the Del
!!    key (ASCII 127) as erasing characters. An EOT (ASCII 4) or a Carriage
!!    return (ASCII 13) will terminate input.
!!
!!##OPTIONS
!!    BCOL  is the background color which is used for erasing characters
!!          after a backspace or a delete key is received.
!!##RETURNS
!!          getstring(3f) returns the number of characters read. Getstring
!!          does not check for overflow in the input buffer string.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_getstring
!!    use M_draw
!!    implicit none
!!    ! reading a string from graphic input with getstring(3f)
!!    character(len=128) :: buf(10)
!!    character(len=20)  :: dev
!!    character(len=20)  :: fname
!!    integer            :: ios, i, n
!!    real               :: shft, tsize, y
!!
!!    print*, 'Enter device:'
!!    read (*, '(a)',iostat=ios) dev
!!    if(ios.ne.0)dev=' '
!!
!!    print*, 'Enter a font name:'
!!    read (*, '(a)',iostat=ios) fname
!!    if(ios.ne.0)fname='futura.l'
!!    if(fname.eq.'')fname='futura.l'
!!
!!    call vinit(dev)
!!
!!    call font(fname)
!!
!!    call clipping(.false.)
!!
!!    shft=0.14
!!    call window(-1.0-shft, 1.0-shft, -1.0, 1.0, 1.0, -1.0)
!!    call lookat(0.0-shft, 0.0-shft, 1.0, 0.0, 0.0, 0.0, 0.0)
!!
!!    call rotate(30.0, 'x')
!!    call rotate(30.0, 'z')
!!    call rotate(60.0, 'y')
!!
!!    call color(D_BLACK)
!!    call clear()
!!    call color(D_YELLOW)
!!
!!    tsize=0.25
!!    call textsize(tsize/2.5, tsize)
!!    call linewidth(100)
!!    call rect(-0.5, -0.5, 0.5, 0.5)
!!
!!    y=0.5
!!
!!    call linewidth(40)
!!    call move2(-0.5, y)
!!    call color(D_GREEN)
!!    call drawstr('getstring(3f) demo')
!!
!!    write(*,*)'Enter 10 lines up to 128 characters long.'
!!    write(*,*)'Program ends on blank line.'
!!
!!    do n=1,10
!!       y=y-tsize
!!       call move2(-0.5, y)
!!       i = getstring(D_BLACK, buf(n))
!!       write(*,'(/,a,i0,a,i0)',advance='no')'N=',n,' I=',i
!!       if(n.ge.1 .and. n.le.size(buf))write(*,*)' BUF=',trim(buf(n)(:i))
!!       buf(n)(min(128,i+1):)=' '
!!       if(i.le.0)exit ! exit on empty line
!!    enddo
!!
!!    call vexit()
!!
!!    do i = 1, n-1
!!       write(*, '(1x, ''Line'',i3,'' was: '', a)') i, buf(i)
!!    enddo
!!
!!    end program demo_getstring
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
