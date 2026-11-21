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

   subroutine draw_getviewport(left, right, bottom, top)
      real(real32), intent(out) :: left, right, bottom, top

      left = (vdevice%viewport%v%left - 0.5_real32) * 2.0_real32
      right = (vdevice%viewport%v%right - 0.5_real32) * 2.0_real32
      bottom = (vdevice%viewport%v%bottom - 0.5_real32) * 2.0_real32
      top = (vdevice%viewport%v%top - 0.5_real32) * 2.0_real32
   end subroutine draw_getviewport

end module m_draw_viewp
