!----------------------------------------------------------------------------------------------------------------------------------!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!----------------------------------------------------------------------------------------------------------------------------------!
!>
!!##NAME
!!    pop(3f) - [M_draw] call popviewport(), popmatrix(), popattributes()
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!    subroutine pop()
!!##DESCRIPTION
!!    call popviewport(), popmatrix(), popattributes()
!!##EXAMPLES
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    MIT
subroutine pop()

! ident_5="@(#) M_draw pop(3f) call popviewport() popmatrix() popattributes()"

   call draw_popviewport()
   call draw_popmatrix()
   call draw_popattributes()
end subroutine pop
