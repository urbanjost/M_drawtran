!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    print(3f) - [M_draw] call DRAWSTR(3f) with up to nine arbitrary intrinsic scalar values
!!
!!##SYNOPSIS
!!
!!    subroutine print(g1, g2, g3, g4, g5, g6, g7, g8, g9)
!!
!!    class(*),intent(in),optional :: g1 ,g2 ,g3 ,g4 ,g5, g6 ,g7 ,g8 ,g9
!!
!!##DESCRIPTION
!!    Call DRAWSTR(3f) with up to nine intrinsic scalar values. They will be composed into a
!!    string with spaces between the argument values. Trailing spaces of strings
!!    are trimmed unless the entire variable is blank.
!!##OPTIONS
!!    g1-g9  intrinsic scalar values to plot at the current point.
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_print
!!    use M_draw
!!    implicit none
!!    real :: angle
!!    integer :: idum
!!    character(len=*),parameter :: space='       '
!!       ! set window size
!!       call prefsize(700,700)
!!       call prefposition( 0, 0)
!!       call vinit('X11')
!!       call page(-5.0,5.0,-5.0,5.0)
!!       call textsize(0.3,0.3)
!!       call color(D_BLUE)
!!       angle=0.0
!!       call turn()
!!       call print(space,'a logical such as ',.true.)
!!       call turn()
!!       call print(space,'a real value',3.1416)
!!       call turn()
!!       call print(space,'double precision',7890.123456d0)
!!       call turn()
!!       call print(space,'integer ',1234)
!!       call turn()
!!       call print(space,'lots of stuff',1234,.false.,cmplx(20.0,30.0))
!!       idum=getkey()
!!       call vexit()
!!    contains
!!       subroutine turn()
!!          call move2(-4.0,-3.5)
!!          call textang(angle)
!!          angle=angle+15.0
!!       end subroutine turn
!!    end program demo_print
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    MIT
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine print(g1, g2, g3, g4, g5, g6, g7, g8, g9)
class(*),intent(in),optional :: g1 ,g2 ,g3 ,g4 ,g5, g6 ,g7 ,g8 ,g9
   call draw_drawstr(str(g1, g2, g3, g4, g5, g6, g7, g8, g9))
end subroutine print
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
function str(generic0, generic1, generic2, generic3, generic4, generic5, generic6, generic7, generic8, generic9, &
                  & generica, genericb, genericc, genericd, generice, genericf, genericg, generich, generici, genericj, &
                  & sep)
implicit none

! ident_2="@(#)M_draw::str(3fp): writes a message to a string composed of any standard scalar types"

class(*),intent(in),optional  :: generic0, generic1, generic2, generic3, generic4
class(*),intent(in),optional  :: generic5, generic6, generic7, generic8, generic9
class(*),intent(in),optional  :: generica, genericb, genericc, genericd, generice
class(*),intent(in),optional  :: genericf, genericg, generich, generici, genericj
character(len=:),allocatable  :: str
character(len=4096)           :: line
integer                       :: istart
integer                       :: increment
character(len=*),intent(in),optional :: sep
character(len=:),allocatable  :: sep_local
   if(present(sep))then
      increment=len(sep)+1
      sep_local=sep
   else
      increment=2
      sep_local=' '
   endif

   istart=1
   line=''
   if(present(generic0))call print_generic(generic0)
   if(present(generic1))call print_generic(generic1)
   if(present(generic2))call print_generic(generic2)
   if(present(generic3))call print_generic(generic3)
   if(present(generic4))call print_generic(generic4)
   if(present(generic5))call print_generic(generic5)
   if(present(generic6))call print_generic(generic6)
   if(present(generic7))call print_generic(generic7)
   if(present(generic8))call print_generic(generic8)
   if(present(generic9))call print_generic(generic9)
   if(present(generica))call print_generic(generica)
   if(present(genericb))call print_generic(genericb)
   if(present(genericc))call print_generic(genericc)
   if(present(genericd))call print_generic(genericd)
   if(present(generice))call print_generic(generice)
   if(present(genericf))call print_generic(genericf)
   if(present(genericg))call print_generic(genericg)
   if(present(generich))call print_generic(generich)
   if(present(generici))call print_generic(generici)
   if(present(genericj))call print_generic(genericj)
   str=trim(line)
contains

subroutine print_generic(generic)
!use, intrinsic :: iso_fortran_env, only : int8, int16, int32, biggest=>int64, real32, real64, dp=>real128
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64, real32, real64, real128
class(*),intent(in) :: generic
   select type(generic)
      type is (integer(kind=int8));     write(line(istart:),'(i0)') generic
      type is (integer(kind=int16));    write(line(istart:),'(i0)') generic
      type is (integer(kind=int32));    write(line(istart:),'(i0)') generic
      type is (integer(kind=int64));    write(line(istart:),'(i0)') generic
      type is (real(kind=real32));      write(line(istart:),'(1pg0)') generic
      type is (real(kind=real64));      write(line(istart:),'(1pg0)') generic
      type is (real(kind=real128));     write(line(istart:),'(1pg0)') generic
      type is (logical);                write(line(istart:),'(l1)') generic
      type is (character(len=*));       write(line(istart:),'(a)') trim(generic)
      type is (complex);                write(line(istart:),'("(",1pg0,",",1pg0,")")') generic
   end select
   istart=len_trim(line)+increment
   line=trim(line)//sep_local
end subroutine print_generic

end function str
