module m_draw_attr
   use, intrinsic :: iso_fortran_env, only: real32, output_unit, error_unit
   use m_draw_types
   use m_draw_verror
   use m_draw_aspect
   use m_draw_buffer
   implicit none
   private
   public :: draw_printattribs, draw_getattribs, draw_printlogicals, &
             draw_printvdevice, draw_pushattributes, draw_popattributes

   type(Astack), pointer, save :: asfree => null()

   interface
      function draw_newtokens(num) result(toks)
         import :: Token
         integer, intent(in) :: num
         type(Token), pointer :: toks(:)
      end function draw_newtokens

      subroutine draw_font(name)
         character(len=*), intent(in) :: name
      end subroutine draw_font

      subroutine draw_color(i)
         integer, intent(in) :: i
      end subroutine draw_color

      subroutine draw_textsize(width, height)
         import :: real32
         real(real32), intent(in) :: width, height
      end subroutine draw_textsize
   end interface

contains

   subroutine draw_copyattributes(a, b)
      type(Attribute), intent(inout) :: a
      type(Attribute), intent(in)    :: b

      if (allocated(a%style)) deallocate(a%style)
      if (allocated(b%style)) a%style = b%style

      a%dash_pos     = b%dash_pos
      a%dash         = b%dash
      a%adist        = b%adist
      a%color        = b%color
      a%fill         = b%fill
      a%hatch        = b%hatch
      a%inbackbuffer = b%inbackbuffer
      a%textcos      = b%textcos
      a%textsin      = b%textsin
      a%hatchcos     = b%hatchcos
      a%hatchsin     = b%hatchsin
      a%hatchpitch   = b%hatchpitch
      a%justify      = b%justify
      a%skew         = b%skew
      a%bold         = b%bold
      a%fixedwidth   = b%fixedwidth
      a%fontwidth    = b%fontwidth
      a%fontheight   = b%fontheight
      a%softtext     = b%softtext
      a%exvp         = b%exvp
      a%backface     = b%backface
      a%font         = b%font
   end subroutine draw_copyattributes

   subroutine draw_printattribs(s)
      character(len=*), intent(in) :: s

      write(output_unit, '(A)') trim(s)
      write(output_unit, '(A, I0)')    'color        = ', vdevice%attr%a%color
      write(output_unit, '(A, I0)')    'fill         = ', vdevice%attr%a%fill
      write(output_unit, '(A, I0)')    'backface     = ', vdevice%attr%a%backface
      write(output_unit, '(A, I0)')    'hatch        = ', vdevice%attr%a%hatch
      write(output_unit, '(A, F12.6)') 'hatchcos     = ', vdevice%attr%a%hatchcos
      write(output_unit, '(A, F12.6)') 'hatchsin     = ', vdevice%attr%a%hatchsin
      write(output_unit, '(A, F12.6)') 'hatchpitch   = ', vdevice%attr%a%hatchpitch
      write(output_unit, '(A, A)')     'font         = ', trim(vdevice%attr%a%font)
      write(output_unit, '(A, I0)')    'fixedwidth   = ', vdevice%attr%a%fixedwidth
      write(output_unit, '(A, I0)')    'justify      = ', vdevice%attr%a%justify
      write(output_unit, '(A, F12.6)') 'skew         = ', vdevice%attr%a%skew
      write(output_unit, '(A, F12.6)') 'fontwidth    = ', vdevice%attr%a%fontwidth
      write(output_unit, '(A, F12.6)') 'fontheight   = ', vdevice%attr%a%fontheight
      write(output_unit, '(A, F12.6)') 'textcos      = ', vdevice%attr%a%textcos
      write(output_unit, '(A, F12.6)') 'textsin      = ', vdevice%attr%a%textsin
      write(output_unit, '(A, I0)')    'exvp         = ', vdevice%attr%a%exvp
      write(output_unit, '(A, I0)')    'softtext     = ', vdevice%attr%a%softtext
      write(output_unit, '(A, I0)')    'inbackbuffer = ', vdevice%attr%a%inbackbuffer
      write(output_unit, '(A, F12.6)') 'adist        = ', vdevice%attr%a%adist
      write(output_unit, '(A, F12.6)') 'dash         = ', vdevice%attr%a%dash
      write(output_unit, '(A, I0)')    'bold         = ', vdevice%attr%a%bold

      if (allocated(vdevice%attr%a%style)) then
         write(output_unit, '(A, A)') 'dashp        = ', vdevice%attr%a%style(vdevice%attr%a%dash_pos:)
         write(output_unit, '(A, A)') 'linestyle    = ', vdevice%attr%a%style
      else
         write(output_unit, '(A)') 'dashp        = (NULL)'
         write(output_unit, '(A)') 'linestyle    = (NULL)'
      end if
   end subroutine draw_printattribs

   subroutine draw_getattribs(s)
      character(len=*), intent(in) :: s

      if (len(s) == 0) return
      select case (s(1:1))
      case ('b')
         write(output_unit, '(A, I0)')    'backface   = ', vdevice%attr%a%backface
      case ('c')
         write(output_unit, '(A, I0)')    'color      = ', vdevice%attr%a%color
      case ('e')
         write(output_unit, '(A, I0)')    'exvp       = ', vdevice%attr%a%exvp
      case ('f')
         if (len(s) >= 2) then
            select case (s(2:2))
            case ('i')
               write(output_unit, '(A, I0)')    'fixedwidth = ', vdevice%attr%a%fixedwidth
               write(output_unit, '(A, I0)')    'fill       = ', vdevice%attr%a%fill
            case ('o')
               write(output_unit, '(A, A)')     'font       = ', trim(vdevice%attr%a%font)
               write(output_unit, '(A, F12.6)') 'fontheight = ', vdevice%attr%a%fontheight
               write(output_unit, '(A, F12.6)') 'fontwidth  = ', vdevice%attr%a%fontwidth
            end select
         end if
      case ('h')
         write(output_unit, '(A, I0)')    'hatch      = ', vdevice%attr%a%hatch
         write(output_unit, '(A, F12.6)') 'hatchcos   = ', vdevice%attr%a%hatchcos
         write(output_unit, '(A, F12.6)') 'hatchpitch = ', vdevice%attr%a%hatchpitch
         write(output_unit, '(A, F12.6)') 'hatchsin   = ', vdevice%attr%a%hatchsin
      case ('j')
         write(output_unit, '(A, I0)')    'justify    = ', vdevice%attr%a%justify
      case ('s')
         write(output_unit, '(A, F12.6)') 'skew       = ', vdevice%attr%a%skew
         write(output_unit, '(A, I0)')    'softtext   = ', vdevice%attr%a%softtext
      case ('t')
         write(output_unit, '(A, F12.6)') 'textcos    = ', vdevice%attr%a%textcos
         write(output_unit, '(A, F12.6)') 'textsin    = ', vdevice%attr%a%textsin
      case default
         write(output_unit, '(A, A)')     'unknown string=', trim(s)
      end select
   end subroutine draw_getattribs

   subroutine draw_printlogicals(s)
      character(len=*), intent(in) :: s

      write(output_unit, '(A)') trim(s)
      write(output_unit, '(A, I0)') 'initialized     = ', vdevice%initialized
      write(output_unit, '(A, I0)') 'writestoprocess = ', vdevice%writestoprocess
      write(output_unit, '(A, I0)') 'clipoff         = ', vdevice%clipoff
      write(output_unit, '(A, I0)') 'inobject        = ', vdevice%inobject
      write(output_unit, '(A, I0)') 'inpolygon       = ', vdevice%inpolygon
      write(output_unit, '(A, I0)') 'upset           = ', vdevice%upset
      write(output_unit, '(A, I0)') 'cpVvalid        = ', vdevice%cpVvalid
      write(output_unit, '(A, I0)') 'sync            = ', vdevice%sync
      write(output_unit, '(A, I0)') 'clipplanes      = ', vdevice%clipplanes
   end subroutine draw_printlogicals

   subroutine draw_printvdevice(s)
      character(len=*), intent(in) :: s
      write(output_unit, '(A)') trim(s)
      call draw_printlogicals('logicals:')
      call draw_printattribs('attributes:')
   end subroutine draw_printvdevice

!>
!!##NAME
!!    pushattributes(3f) - [M_draw:ATTRIBUTE_STACK] Save the current attributes on the attribute stack.
!!    (LICENSE:PD)
!!
!!
!!##SYNOPSIS
!!
!!          subroutine pushattributes()
!!
!!##DESCRIPTION
!!
!!    Save the current attributes on the attribute stack.
   subroutine draw_pushattributes()
      type(Astack), pointer :: nattr
      type(Token), pointer :: p(:)

      if (vdevice%initialized == 0) call draw_verror("pushattributes:  draw not initialized")

      if (vdevice%inobject /= 0) then
         p => draw_newtokens(1)
         p(1)%i = OBJ_PUSHATTRIBUTES
         return
      end if

      if (associated(asfree)) then
         nattr => vdevice%attr
         vdevice%attr => asfree
         asfree => asfree%back
         vdevice%attr%back => nattr
         call draw_copyattributes(vdevice%attr%a, nattr%a)
      else
         allocate(nattr)
         nattr%back => vdevice%attr
         call draw_copyattributes(nattr%a, vdevice%attr%a)
         vdevice%attr => nattr
      end if
   end subroutine draw_pushattributes

!>
!!##NAME
!!    popattributes(3f) - [M_draw:ATTRIBUTE_STACK] Restore attributes to what they were at last pushattributes().
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!          subroutine popattributes()
!!
!!##DESCRIPTION
!!
!!    Restore the attributes to what they were at the last pushattributes().
   subroutine draw_popattributes()
      type(Astack), pointer :: nattr
      type(Token), pointer :: p(:)
      type(Attribute) :: blank_attr
      integer :: idum

      if (vdevice%initialized == 0) call draw_verror("popattributes: draw not initialized")

      if (vdevice%inobject /= 0) then
         p => draw_newtokens(1)
         p(1)%i = OBJ_POPATTRIBUTES
         return
      end if

      if (.not. associated(vdevice%attr%back)) then
         write(error_unit, '(A)') "popattributes: attribute stack is empty"
         return
      else
         nattr => vdevice%attr
         call draw_font(trim(vdevice%attr%back%a%font))
         vdevice%attr => vdevice%attr%back
         nattr%back => asfree
         asfree => nattr
         if (allocated(nattr%a%style)) deallocate(nattr%a%style)
         nattr%a = blank_attr
      end if

      call draw_color(vdevice%attr%a%color)
      if (vdevice%attr%a%inbackbuffer /= 0) idum = draw_backbuffer()
      if (vdevice%attr%a%softtext == HERSHEY) call draw_textsize(vdevice%attr%a%fontwidth, vdevice%attr%a%fontheight)
      if (vdevice%attr%a%exvp /= 0) call draw_expandviewport()
   end subroutine draw_popattributes

end module m_draw_attr
