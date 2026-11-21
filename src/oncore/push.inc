!----------------------------------------------------------------------------------------------------------------------------------!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!----------------------------------------------------------------------------------------------------------------------------------!
!>
!!##NAME
!!    push(3f) - [M_draw] call pushviewport(), pushmatrix(), pushattributes()
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!     subroutine push()
!!##DESCRIPTION
!!    call pushattributes(), pushmatrix(), pushviewport()
!!##EXAMPLES
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    MIT
subroutine push()

! ident_6="@(#) M_draw push(3f) call pushattributes() pushmatrix() pushviewport()"

   call draw_pushattributes()
   call draw_pushmatrix()
   call draw_pushviewport()
end subroutine push
