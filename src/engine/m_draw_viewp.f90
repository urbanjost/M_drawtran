module m_draw_viewp
   use, intrinsic :: iso_fortran_env, only: real32
   use m_draw_types
   use m_draw_verror
   use m_draw_mapping
   implicit none
   private
   public :: draw_pushviewport, draw_popviewport, draw_viewport, draw_getviewport

   type(Vstack), pointer, save :: vsfree => null()

   interface
      function draw_newtokens(num) result(toks)
         import :: Token
         integer, intent(in) :: num
         type(Token), pointer :: toks(:)
      end function draw_newtokens
   end interface

contains

!>
!!##NAME
!!    pushviewport(3f) - [M_draw:VIEWPORT] Save current viewport
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!          subroutine pushviewport()
!!##DESCRIPTION
!!
!!    Save current viewport.
   subroutine draw_pushviewport()
      type(Vstack), pointer :: nvport
      type(Token), pointer :: tok(:)

      if (vdevice%initialized == 0) then
         call draw_verror("pushviewport: draw not initialized")
      end if

      if (vdevice%inobject /= 0) then
         tok => draw_newtokens(1)
         tok(1)%i = OBJ_PUSHVIEWPORT
         return
      end if

      if (associated(vsfree)) then
         nvport => vdevice%viewport
         vdevice%viewport => vsfree
         vsfree => vsfree%back
         vdevice%viewport%back => nvport
         vdevice%viewport%v = nvport%v
      else
         allocate(nvport)
         nvport%back => vdevice%viewport
         nvport%v = vdevice%viewport%v
         vdevice%viewport => nvport
      end if
   end subroutine draw_pushviewport

!>
!!##NAME
!!    popviewport(3f) - [M_draw:VIEWPORT] Retrieve last viewport
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!          subroutine popviewport()
!!
!!##DESCRIPTION
!!
!!    Retrieve last viewport.
   subroutine draw_popviewport()
      type(Token), pointer :: tok(:)
      type(Vstack), pointer :: nvport

      if (vdevice%initialized == 0) then
         call draw_verror("popviewport: draw not initialized")
      end if

      if (vdevice%inobject /= 0) then
         tok => draw_newtokens(1)
         tok(1)%i = OBJ_POPVIEWPORT
         return
      end if

      if (.not. associated(vdevice%viewport%back)) then
         call draw_verror("popviewport: viewport stack underflow")
      else
         nvport => vdevice%viewport
         vdevice%viewport => vdevice%viewport%back
         nvport%back => vsfree
         vsfree => nvport
      end if

      vdevice%maxVx = int(vdevice%viewport%v%right * vdevice%sizeX)
      vdevice%maxVy = int(vdevice%viewport%v%top * vdevice%sizeY)
      vdevice%minVx = int(vdevice%viewport%v%left * vdevice%sizeX)
      vdevice%minVy = int(vdevice%viewport%v%bottom * vdevice%sizeY)

      call draw_CalcW2Vcoeffs()
   end subroutine draw_popviewport

!>
!!##NAME
!!    viewport(3f) - [M_draw:VIEWPORT] Specify which part of screen to draw in
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!          subroutine viewport(left, right, bottom, top)
!!          real,intent(in) :: left, right, bottom, top
!!##DESCRIPTION
!!    Specify which part of the screen to draw in. Left, right, bottom,
!!    and top are real values in screen coordinates (0:n,0:m).
!!
!!    If a device has been declared to be 600 x 400
!!
!!         o-----> X                         (right=600,top=0)
!!         | #------------------------------------#
!!         | |                                    |
!!         | |                                    |
!!         V |                                    |
!!         Y |                                    |
!!           #------------------------------------#
!!      (left=0,bottom=400)
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_viewport
!!    !
!!    ! using non-square viewports,
!!    ! the associated distortion -- and how to fix it
!!    !
!!    use M_draw
!!    implicit none
!!
!!    character(len=50)  :: device
!!    character(len=120) :: buf
!!    real    xfact, yfact
!!    integer :: ios, idum
!!
!!    print*,'Enter output device:'
!!    read(*,'(a)',iostat=ios)device
!!    if(ios.ne.0)device=' '
!!
!!    call vinit(device)
!!
!!    call color(D_BLACK)
!!    call clear()
!!    !
!!    ! Make the viewport the same size as the screen/window.
!!    !
!!    call getfactors(xfact, yfact)
!!    call viewport(-1.0, xfact, -1.0, yfact)
!!    !
!!    ! Draw a square. (Looks like a rectangle, if the viewport
!!    ! wasn't "accidentally" square)
!!    !
!!    call color(D_RED)
!!    call rect(-0.5, -0.5, 0.5, 0.5)
!!    !
!!    ! Tell them what it is.
!!    !
!!    call move2(-1.0, 0.9)
!!    write(buf,&
!!    & '(''Distorted square (viewport(-1, '', F7.3, '', -1, '', F7.3, ''))'')')&
!!    & xfact, yfact
!!    call drawstr(buf)
!!
!!    idum=getkey()
!!    !
!!    ! Fix up the distortion (The actual formula to fix
!!    ! the distortion is (viewport.xmax * (1 + xfact) / 2.0),
!!    ! and similar for the y axis.
!!    !
!!    call ortho2(-1.0, xfact, -1.0, yfact)
!!    !
!!    ! Draw another square (Really is square this time)
!!    !
!!    call color(D_YELLOW)
!!    call rect(-0.5, -0.5, 0.5, 0.5)
!!    !
!!    ! Tell them what it is.
!!    !
!!    call move2(-1.0, -0.9)
!!    write(buf,&
!!    & '(''Fixed up square with ortho2(-1, '', F7.3, '', -1, '', F7.3, '')'')')&
!!    & xfact, yfact
!!    call drawstr(buf)
!!
!!    idum=getkey()
!!    !
!!    ! Do it with world coords going from 0 - 5, 0 - 5.
!!    ! Reset square viewport.
!!    !
!!    call color(D_BLACK)
!!    call clear()
!!
!!    call viewport(-1.0, 1.0, -1.0, 1.0)
!!    call ortho2(0.0, 5.0, 0.0, 5.0)
!!    call textsize(0.1, 0.1)
!!    !
!!    ! Square from 1 to 3. (Really is square)
!!    !
!!    call color(D_GREEN)
!!    call rect(1.0, 1.0, 3.0, 3.0)
!!
!!    call move2(0.0, 4.5)
!!    call drawstr('Square from 0 - 3, 0 - 3')
!!
!!    idum=getkey()
!!    !
!!    ! Distort it with a non-square viewport.
!!    !
!!    call viewport(-1.0, xfact, -1.0, yfact)
!!
!!    call color(D_BLUE)
!!    call rect(1.0, 1.0, 3.0, 3.0)
!!
!!    call move2(0.0, 0.5)
!!    call drawstr('Distorted square from 0 - 3, 0 - 3')
!!
!!    idum=getkey()
!!    !
!!    ! Fix the distortion.
!!    !
!!    call ortho2(0.0, 5.0 * (1.0 + xfact) / 2.0, 0.0, 5.0 * (1.0 + yfact) / 2.0)
!!
!!    call color(D_MAGENTA)
!!    call rect(1.0, 1.0, 3.0, 3.0)
!!
!!    call move2(0.0, 2.5)
!!    call drawstr('Fixed up  square from 0 - 3, 0 - 3')
!!
!!    idum=getkey()
!!
!!    call vexit()
!!
!!    end program demo_viewport
   subroutine draw_viewport(xlow, xhigh, ylow, yhigh)
      real(real32), intent(in) :: xlow, xhigh, ylow, yhigh
      real(real32) :: xl, xh, yl, yh
      type(Token), pointer :: tok(:)
      character(len=80) :: buf

      if (vdevice%initialized == 0) then
         call draw_verror("viewport: draw not initialized")
      end if

      if (xlow >= xhigh) then
         write(buf, '(A, F5.2, A, F5.2, A)') "viewport: xleft(", xlow, ") >= xright(", xhigh, ")"
         call draw_verror(buf)
      end if
      if (ylow >= yhigh) then
         write(buf, '(A, F5.2, A, F5.2, A)') "viewport: ybottom(", ylow, ") >= ytop(", yhigh, ")"
         call draw_verror(buf)
      end if

      if (vdevice%inobject /= 0) then
         tok => draw_newtokens(5)
         tok(1)%i = OBJ_VIEWPORT
         tok(2)%f = xlow
         tok(3)%f = xhigh
         tok(4)%f = ylow
         tok(5)%f = yhigh
         return
      end if

      xl = xlow / 2.0_real32 + 0.5_real32
      xh = xhigh / 2.0_real32 + 0.5_real32
      yl = ylow / 2.0_real32 + 0.5_real32
      yh = yhigh / 2.0_real32 + 0.5_real32

      vdevice%viewport%v%left = xl
      vdevice%viewport%v%right = xh
      vdevice%viewport%v%bottom = yl
      vdevice%viewport%v%top = yh

      vdevice%maxVx = int(xh * vdevice%sizeX)
      vdevice%maxVy = int(yh * vdevice%sizeY)
      vdevice%minVx = int(xl * vdevice%sizeX)
      vdevice%minVy = int(yl * vdevice%sizeY)

      call draw_CalcW2Vcoeffs()
   end subroutine draw_viewport

!>
!!##NAME
!!    getviewport(3f) - [M_draw:VIEWPORT] Returns limits of current viewport in screen coordinates
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!          subroutine getviewport(left, right, bottom, top)
!!          real,intent(out) :: left
!!          real,intent(out) :: right
!!          real,intent(out) :: bottom
!!          real,intent(out) :: top
!!
!!##DESCRIPTION
!!
!! Returns the left, right, bottom and top limits of the current viewport
!! in screen coordinates (-1.0 to 1.0).
!!
!!    If a device has been declared to be real :: array(600,400)
!!
!!         o-----> X                         (right=600,top=0)
!!         | #------------------------------------#
!!         | |                                    |
!!         | |                                    |
!!         V |                                    |
!!         Y |                                    |
!!           #------------------------------------#
!!      (left=0,bottom=400)
!!
!!##OPTIONS
!!    LEFT     value for left side
!!    RIGHT    value for right side
!!    BOTTOM   value for bottom side
!!    TOP      value for top side
   subroutine draw_getviewport(left, right, bottom, top)
      real(real32), intent(out) :: left, right, bottom, top

      left = (vdevice%viewport%v%left - 0.5_real32) * 2.0_real32
      right = (vdevice%viewport%v%right - 0.5_real32) * 2.0_real32
      bottom = (vdevice%viewport%v%bottom - 0.5_real32) * 2.0_real32
      top = (vdevice%viewport%v%top - 0.5_real32) * 2.0_real32
   end subroutine draw_getviewport

end module m_draw_viewp
