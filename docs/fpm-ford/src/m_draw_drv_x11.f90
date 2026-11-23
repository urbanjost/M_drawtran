module m_draw_drv_x11
   use, intrinsic :: iso_fortran_env, only: real32, error_unit
   use, intrinsic :: iso_c_binding
   use m_draw_types
   use m_draw_pref
   use m_draw_verror
   implicit none
   private
   public :: X11_draw_devcpy, x11_low_draw_devcpy

   integer, parameter :: CMAPSIZE = 256
   integer, parameter :: DEV_X11_UPPER   = 0
   integer, parameter :: DEV_X11_LOWER   = 1

   integer(c_long), parameter :: KeyPressMask      = 1_c_long
   integer(c_long), parameter :: ButtonPressMask   = 4_c_long
   integer(c_long), parameter :: ButtonReleaseMask = 8_c_long
   integer(c_long), parameter :: ExposureMask      = 32768_c_long
   integer(c_long), parameter :: EV_MASK           = ior(ior(KeyPressMask, ButtonReleaseMask), ior(ExposureMask, ButtonPressMask))

   integer(c_int), parameter :: KeyPress         = 2
   integer(c_int), parameter :: Expose           = 12
   integer(c_int), parameter :: MapNotify        = 19
   integer(c_int), parameter :: LineSolid        = 0
   integer(c_int), parameter :: CapRound         = 2
   integer(c_int), parameter :: JoinRound        = 1
   integer(c_int), parameter :: Nonconvex        = 1
   integer(c_int), parameter :: CoordModeOrigin  = 0
   integer(c_int), parameter :: RevertToParent   = 2
   integer(c_long), parameter :: CurrentTime     = 0_c_long

   type, bind(C) :: XPoint
      integer(c_short) :: x, y
   end type XPoint

   type, bind(C) :: XColor
      integer(c_long)        :: pixel
      integer(c_short)       :: red, green, blue
      character(kind=c_char) :: flags, pad
   end type XColor

   type, bind(C) :: XWindowAttributes
      integer(c_int) :: x, y, width, height, border_width, depth
      type(c_ptr)    :: visual
      integer(c_long) :: root
      integer(c_int) :: class, bit_gravity, win_gravity, backing_store
      integer(c_long) :: backing_planes, backing_pixel
      integer(c_int) :: save_under
      integer(c_long) :: colormap
      integer(c_int) :: map_installed, map_state
      integer(c_long) :: all_event_masks, your_event_mask, do_not_propagate_mask
      integer(c_int) :: override_redirect
      type(c_ptr)    :: screen
   end type XWindowAttributes

   type(c_ptr), save     :: display     = c_null_ptr
   integer(c_long), save :: winder      = 0_c_long
   integer(c_int), save  :: screen      = 0
   integer(c_long), save :: carray(CMAPSIZE) = 0_c_long
   integer(c_long), save :: colormap    = 0_c_long
   integer(c_long), save :: theDrawable = 0_c_long
   type(c_ptr), save     :: theGC       = c_null_ptr
   integer(c_long), save :: bbuf        = 0_c_long
   integer, save         :: back_used   = 0
   type(c_ptr), save     :: font_id     = c_null_ptr
   integer(c_long), save :: colour      = 0_c_long
   integer, save         :: GLOBAL_device = DEV_X11_UPPER
   integer(c_long), save :: event_buf(24) = 0_c_long

   interface
      type(c_ptr) function XOpenDisplay(name) bind(C, name="XOpenDisplay")
         import :: c_ptr
         type(c_ptr), value :: name
      end function XOpenDisplay

      subroutine XCloseDisplay(disp) bind(C, name="XCloseDisplay")
         import :: c_ptr
         type(c_ptr), value :: disp
      end subroutine XCloseDisplay

      integer(c_long) function XDefaultRootWindow(disp) bind(C, name="XDefaultRootWindow")
         import :: c_ptr, c_long
         type(c_ptr), value :: disp
      end function XDefaultRootWindow

      integer(c_int) function XDefaultScreen(disp) bind(C, name="XDefaultScreen")
         import :: c_ptr, c_int
         type(c_ptr), value :: disp
      end function XDefaultScreen

      integer(c_long) function XDefaultColormap(disp, scr) bind(C, name="XDefaultColormap")
         import :: c_ptr, c_int, c_long
         type(c_ptr), value :: disp
         integer(c_int), value :: scr
      end function XDefaultColormap

      integer(c_int) function XDefaultDepth(disp, scr) bind(C, name="XDefaultDepth")
         import :: c_ptr, c_int
         type(c_ptr), value :: disp
         integer(c_int), value :: scr
      end function XDefaultDepth

      type(c_ptr) function XDefaultGC(disp, scr) bind(C, name="XDefaultGC")
         import :: c_ptr, c_int
         type(c_ptr), value :: disp
         integer(c_int), value :: scr
      end function XDefaultGC

      integer(c_long) function XBlackPixel(disp, scr) bind(C, name="XBlackPixel")
         import :: c_ptr, c_int, c_long
         type(c_ptr), value :: disp
         integer(c_int), value :: scr
      end function XBlackPixel

      integer(c_long) function XWhitePixel(disp, scr) bind(C, name="XWhitePixel")
         import :: c_ptr, c_int, c_long
         type(c_ptr), value :: disp
         integer(c_int), value :: scr
      end function XWhitePixel

      integer(c_long) function XCreateSimpleWindow(disp, parent, x, y, w, h, bw, border, bg) bind(C, name="XCreateSimpleWindow")
         import :: c_ptr, c_int, c_long
         type(c_ptr), value :: disp
         integer(c_long), value :: parent, border, bg
         integer(c_int), value :: x, y, w, h, bw
      end function XCreateSimpleWindow

      subroutine XDestroyWindow(disp, win) bind(C, name="XDestroyWindow")
         import :: c_ptr, c_long
         type(c_ptr), value :: disp
         integer(c_long), value :: win
      end subroutine XDestroyWindow

      subroutine XStoreName(disp, win, name) bind(C, name="XStoreName")
         import :: c_ptr, c_long, c_char
         type(c_ptr), value :: disp
         integer(c_long), value :: win
         character(kind=c_char), intent(in) :: name(*)
      end subroutine XStoreName

      subroutine XSelectInput(disp, win, mask) bind(C, name="XSelectInput")
         import :: c_ptr, c_long
         type(c_ptr), value :: disp
         integer(c_long), value :: win, mask
      end subroutine XSelectInput

      subroutine XMapRaised(disp, win) bind(C, name="XMapRaised")
         import :: c_ptr, c_long
         type(c_ptr), value :: disp
         integer(c_long), value :: win
      end subroutine XMapRaised

      subroutine XFlush(disp) bind(C, name="XFlush")
         import :: c_ptr
         type(c_ptr), value :: disp
      end subroutine XFlush

      subroutine XSync(disp, discard) bind(C, name="XSync")
         import :: c_ptr, c_int
         type(c_ptr), value :: disp
         integer(c_int), value :: discard
      end subroutine XSync

      subroutine XNextEvent(disp, ev) bind(C, name="XNextEvent")
         import :: c_ptr, c_long
         type(c_ptr), value :: disp
         integer(c_long), intent(out) :: ev(*)
      end subroutine XNextEvent

      integer(c_int) function XCheckWindowEvent(disp, win, mask, ev) bind(C, name="XCheckWindowEvent")
         import :: c_ptr, c_long, c_int
         type(c_ptr), value :: disp
         integer(c_long), value :: win, mask
         integer(c_long), intent(out) :: ev(*)
      end function XCheckWindowEvent

      integer(c_int) function XLookupString(ev, buf, len, keysym, status) bind(C, name="XLookupString")
         import :: c_ptr, c_long, c_int, c_char
         integer(c_long), intent(in) :: ev(*)
         character(kind=c_char), intent(out) :: buf(*)
         integer(c_int), value :: len
         type(c_ptr), value :: keysym, status
      end function XLookupString

      subroutine XSetInputFocus(disp, focus, revert_to, time) bind(C, name="XSetInputFocus")
         import :: c_ptr, c_long, c_int
         type(c_ptr), value :: disp
         integer(c_long), value :: focus, time
         integer(c_int), value :: revert_to
      end subroutine XSetInputFocus

      integer(c_int) function XGetWindowAttributes(disp, win, attr) bind(C, name="XGetWindowAttributes")
         import :: c_ptr, c_long, c_int, XWindowAttributes
         type(c_ptr), value :: disp
         integer(c_long), value :: win
         type(XWindowAttributes), intent(out) :: attr
      end function XGetWindowAttributes

      integer(c_int) function XQueryPointer(disp, win, root_ret, child_ret, rx, ry, wx, wy, mask) bind(C, name="XQueryPointer")
         import :: c_ptr, c_long, c_int
         type(c_ptr), value :: disp
         integer(c_long), value :: win
         integer(c_long), intent(out) :: root_ret, child_ret
         integer(c_int), intent(out) :: rx, ry, wx, wy, mask
      end function XQueryPointer

      subroutine XDrawLine(disp, d, gc, x1, y1, x2, y2) bind(C, name="XDrawLine")
         import :: c_ptr, c_long, c_int
         type(c_ptr), value :: disp, gc
         integer(c_long), value :: d
         integer(c_int), value :: x1, y1, x2, y2
      end subroutine XDrawLine

      subroutine XFillRectangle(disp, d, gc, x, y, w, h) bind(C, name="XFillRectangle")
         import :: c_ptr, c_long, c_int
         type(c_ptr), value :: disp, gc
         integer(c_long), value :: d
         integer(c_int), value :: x, y, w, h
      end subroutine XFillRectangle

      subroutine XFillPolygon(disp, d, gc, pts, npts, shape, mode) bind(C, name="XFillPolygon")
         import :: c_ptr, c_long, c_int, XPoint
         type(c_ptr), value :: disp, gc
         integer(c_long), value :: d
         type(XPoint), intent(in) :: pts(*)
         integer(c_int), value :: npts, shape, mode
      end subroutine XFillPolygon

      subroutine XDrawString(disp, d, gc, x, y, str, length) bind(C, name="XDrawString")
         import :: c_ptr, c_long, c_int, c_char
         type(c_ptr), value :: disp, gc
         integer(c_long), value :: d
         integer(c_int), value :: x, y, length
         character(kind=c_char), intent(in) :: str(*)
      end subroutine XDrawString

      subroutine XSetForeground(disp, gc, fg) bind(C, name="XSetForeground")
         import :: c_ptr, c_long
         type(c_ptr), value :: disp, gc
         integer(c_long), value :: fg
      end subroutine XSetForeground

      subroutine XSetBackground(disp, gc, bg) bind(C, name="XSetBackground")
         import :: c_ptr, c_long
         type(c_ptr), value :: disp, gc
         integer(c_long), value :: bg
      end subroutine XSetBackground

      subroutine XSetLineAttributes(disp, gc, lw, ls, cap, join) bind(C, name="XSetLineAttributes")
         import :: c_ptr, c_int
         type(c_ptr), value :: disp, gc
         integer(c_int), value :: lw, ls, cap, join
      end subroutine XSetLineAttributes
      integer(c_int) function XAllocColor(disp, cmap, scr_in_out) bind(C, name="XAllocColor")
         import :: c_ptr, c_long, c_int, XColor
         type(c_ptr), value :: disp
         integer(c_long), value :: cmap
         type(XColor), intent(inout) :: scr_in_out
      end function XAllocColor
      type(c_ptr) function XLoadQueryFont(disp, name) bind(C, name="XLoadQueryFont")
         import :: c_ptr, c_char
         type(c_ptr), value :: disp
         character(kind=c_char), intent(in) :: name(*)
      end function XLoadQueryFont
      subroutine XFreeFont(disp, fstruct) bind(C, name="XFreeFont")
         import :: c_ptr
         type(c_ptr), value :: disp, fstruct
      end subroutine XFreeFont
      integer(c_long) function XLoadFont(disp, name) bind(C, name="XLoadFont")
         import :: c_ptr, c_long, c_char
         type(c_ptr), value :: disp
         character(kind=c_char), intent(in) :: name(*)
      end function XLoadFont
      subroutine XSetFont(disp, gc, font) bind(C, name="XSetFont")
         import :: c_ptr, c_long
         type(c_ptr), value :: disp, gc
         integer(c_long), value :: font
      end subroutine XSetFont
      integer(c_long) function XCreatePixmap(disp, d, w, h, depth) bind(C, name="XCreatePixmap")
         import :: c_ptr, c_long, c_int
         type(c_ptr), value :: disp
         integer(c_long), value :: d
         integer(c_int), value :: w, h, depth
      end function XCreatePixmap
      subroutine XFreePixmap(disp, pixmap) bind(C, name="XFreePixmap")
         import :: c_ptr, c_long
         type(c_ptr), value :: disp
         integer(c_long), value :: pixmap
      end subroutine XFreePixmap
      subroutine XCopyArea(disp, src, dest, gc, sx, sy, w, h, dx, dy) bind(C, name="XCopyArea")
         import :: c_ptr, c_long, c_int
         type(c_ptr), value :: disp, gc
         integer(c_long), value :: src, dest
         integer(c_int), value :: sx, sy, w, h, dx, dy
      end subroutine XCopyArea
   end interface
contains
   subroutine X11_NEW()
      integer :: idum
      idum = X11_checkkey()
      call XFlush(display)
      idum = X11_backbuf()
      idum = X11_clear()
      idum = X11_swapbuf()
      idum = X11_frontbuf()
      call XSetBackground(display, theGC, carray(8))
      call XFlush(display)
   end subroutine X11_NEW
   integer function X11_RESIZE()
      type(XWindowAttributes) :: retWinAttr
      integer :: idum
      if (XGetWindowAttributes(display, winder, retWinAttr) == 0) then
         write(error_unit, '(A)') "X11_RESIZE: failed to get window attributes."
         error stop 1
      end if
      if (retWinAttr%width /= vdevice%sizeSx .or. retWinAttr%height /= vdevice%sizeSy) then
         vdevice%sizeSx = retWinAttr%width
         vdevice%sizeSy = retWinAttr%height
         if (back_used /= 0) then
            back_used = 0
            idum = X11_backbuf()
         end if
         call XFlush(display)
      end if
      X11_RESIZE = UNUSED
   end function X11_RESIZE

   integer function X11_init()
      integer :: i, x, y, w, h, bw, prefx, prefy, prefxs, prefys, idum, env_len, ios
      character(len=64) :: varname
      call get_environment_variable("PLTDEVICE", value=varname, length=env_len, status=ios)
      if (ios /= 0 .or. env_len == 0) varname = "UNDEFINED"
      if (trim(varname) == "NOCLOSE") then
         write(error_unit, '(A)') "M_draw: returning to X11 window"
      else
         display = XOpenDisplay(c_null_ptr)
         if (.not. c_associated(display)) then
            write(error_unit, '(A)') "M_draw: X11_init: can't connect to X server"
            error stop 1
         end if
         winder = XDefaultRootWindow(display)
         screen = XDefaultScreen(display)
         colormap = XDefaultColormap(display, screen)
      end if
      vdevice%depth = XDefaultDepth(display, screen)

      if (vdevice%depth == 1) then
         if (GLOBAL_device == DEV_X11_UPPER) then
            carray(1) = XBlackPixel(display, screen)
            carray(2:CMAPSIZE) = XWhitePixel(display, screen)
         else
            carray(1) = XWhitePixel(display, screen)
            carray(2:CMAPSIZE) = XBlackPixel(display, screen)
         end if
      else
         if (GLOBAL_device == DEV_X11_UPPER) then
            idum = X11_mapcolor(0, 0, 0, 0)
            idum = X11_mapcolor(7, 255, 255, 255)
         else
            idum = X11_mapcolor(7, 0, 0, 0)
            idum = X11_mapcolor(0, 255, 255, 255)
         end if

         idum = X11_mapcolor(1, 255, 0, 0)
         idum = X11_mapcolor(2, 0, 255, 0)
         idum = X11_mapcolor(3, 255, 255, 0)
         idum = X11_mapcolor(4, 0, 0, 255)
         idum = X11_mapcolor(5, 255, 0, 255)
         idum = X11_mapcolor(6, 0, 255, 255)
      end if
      call draw_getprefposandsize(prefx, prefy, prefxs, prefys)
      x = merge(prefx, 0, prefx > -1)
      y = merge(prefy, 0, prefy > -1)
      w = merge(prefxs, 500, prefxs > -1)
      h = merge(prefys, 500, prefys > -1)
      bw = 4
      if (trim(varname) /= "NOCLOSE") then
         winder = XCreateSimpleWindow(display, winder, x, y, w, h, bw, carray(2), carray(8))
         call XStoreName(display, winder, c_char_"M_draw" // char(0))
         theGC = XDefaultGC(display, screen)
         theDrawable = winder
      end if
      call XSelectInput(display, winder, EV_MASK)
      call XMapRaised(display, winder)
      call XFlush(display)
      do
         call XNextEvent(display, event_buf)
         if (int(event_buf(1), c_int) == Expose .or. int(event_buf(1), c_int) == MapNotify) exit
      end do
      if (prefx == -1 .and. prefxs == -1) then
         call XSetInputFocus(display, winder, RevertToParent, CurrentTime)
      end if
      idum = X11_RESIZE()
      vdevice%sizeX = min(h, w); vdevice%sizeY = vdevice%sizeX
      if (trim(varname) /= "NOCLOSE") then
         if (back_used /= 0) then
            back_used = 0
            idum = X11_backbuf()
         end if
      else
         call X11_NEW()
      end if
      X11_init = 1
   end function X11_init

   integer function X11_exit()
      character(len=64) :: varname
      integer :: env_len, ios
      call get_environment_variable("PLTDEVICE", value=varname, length=env_len, status=ios)
      if (ios == 0 .and. trim(varname) == "NOCLOSE") then
         write(error_unit, '(A)') "M_draw planning on returning to X11 window"
         X11_exit = 1; return
      end if
      call XSync(display, 0)
      if (back_used /= 0) then
         call XFreePixmap(display, bbuf)
         bbuf = 0_c_long; back_used = 0
      end if
      if (c_associated(font_id)) then
         call XFreeFont(display, font_id)
         font_id = c_null_ptr
      end if
      call XDestroyWindow(display, winder)
      call XSync(display, 0)
      call XFlush(display)
      call XCloseDisplay(display)
      display = c_null_ptr; winder = 0_c_long
      X11_exit = 1
   end function X11_exit
   integer function X11_draw(x, y)
      integer, intent(in) :: x, y
      call XDrawLine(display, theDrawable, theGC, vdevice%cpVx, vdevice%sizeSy - vdevice%cpVy, x, vdevice%sizeSy - y)
      if (vdevice%sync /= 0) call XSync(display, 0)
      X11_draw = UNUSED
   end function X11_draw
   integer function X11_getkey()
      character(kind=c_char) :: c(2)
      do
         call XNextEvent(display, event_buf)
         if (int(event_buf(1), c_int) == KeyPress) then
            if (XLookupString(event_buf, c, 1, c_null_ptr, c_null_ptr) > 0) then
               X11_getkey = ichar(c(1)); return
            else
               X11_getkey = 0; return
            end if
         end if
      end do
   end function X11_getkey
   integer function X11_checkkey()
      character(kind=c_char) :: c(2)
      if (XCheckWindowEvent(display, winder, KeyPressMask, event_buf) == 0) then
         X11_checkkey = 0; return
      end if
      if (int(event_buf(1), c_int) == KeyPress) then
         if (XLookupString(event_buf, c, 1, c_null_ptr, c_null_ptr) > 0) then
            X11_checkkey = ichar(c(1)); return
         end if
      end if
      X11_checkkey = 0
   end function X11_checkkey
!   integer function X11_locator(wx, wy)
!      integer, intent(out) :: wx, wy
!      integer(c_long) :: rootw, childw
!      integer(c_int) :: rx, ry, mask, idum
!      idum = XQueryPointer(display, winder, rootw, childw, rx, ry, wx, wy, mask)
!      wy = vdevice%sizeSy - wy
!      X11_locator = ishft(mask, -8)
!   end function X11_locator

    integer function X11_locator(wx, wy)
       integer, intent(out) :: wx, wy
       integer(c_long) :: rootw, childw
       integer(c_int) :: rx, ry, mask, idum
       idum = XQueryPointer(display, winder, rootw, childw, rx, ry, wx, wy, mask)
       wy = vdevice%sizeSy - wy
       X11_locator = iand(ishft(mask, -8), 7)
    end function X11_locator

   integer function X11_clear()
      integer :: idum
      idum = X11_RESIZE()
      call XSync(display, 0)
      call XSetBackground(display, theGC, colour)
      call XFillRectangle(display, theDrawable, theGC, 0, 0, vdevice%sizeSx + 1, vdevice%sizeSy + 1)
      call XFlush(display)
      X11_clear = UNUSED
   end function X11_clear

   integer function X11_color(ind_in)
      integer, intent(in) :: ind_in
      integer :: ind, lw
      if (ind_in < 0) then
         ind = merge(0, ind_in, ind_in == -1)
         lw = abs(ind)
         call XSetLineAttributes(display, theGC, lw, LineSolid, CapRound, JoinRound)
      else
         colour = carray(mod(ind_in, CMAPSIZE) + 1)
         call XSetForeground(display, theGC, colour)
      end if
      X11_color = UNUSED
   end function X11_color

   integer function X11_setlw(ind_in)
      integer, intent(in) :: ind_in
      integer :: ind, lw
      ind = int(real(vdevice%sizeX, real32) / 10000.0_real32 * real(ind_in, real32))
      if (ind <= -1) ind = 0
      lw = abs(ind)
      call XSetLineAttributes(display, theGC, lw, LineSolid, CapRound, JoinRound)
      X11_setlw = UNUSED
   end function X11_setlw

   integer function X11_mapcolor(i, r, g, b)
      integer, intent(in) :: i, r, g, b
      integer :: stat
      type(XColor) :: tmp
      if (i >= CMAPSIZE .or. i < 0) then
         X11_mapcolor = -1; return
      end if

      if (vdevice%depth == 1) then
         if (GLOBAL_device == DEV_X11_UPPER) then
            if (i == 0 .and. (r /= 0 .or. g /= 0 .or. b /= 0)) then
               carray(i + 1) = XWhitePixel(display, screen)
            else if (i /= 0 .and. r == 0 .and. g == 0 .and. b == 0) then
               carray(i + 1) = XBlackPixel(display, screen)
            end if
         else
            if (i == 0 .and. (r /= 255 .or. g /= 255 .or. b /= 255)) then
               carray(i + 1) = XBlackPixel(display, screen)
            else if (i /= 0 .and. r == 255 .and. g == 255 .and. b == 255) then
               carray(i + 1) = XWhitePixel(display, screen)
            end if
         end if
      else
         tmp%red   = int(real(r, real32) / 255.0_real32 * 65535.0_real32, c_short)
         tmp%green = int(real(g, real32) / 255.0_real32 * 65535.0_real32, c_short)
         tmp%blue  = int(real(b, real32) / 255.0_real32 * 65535.0_real32, c_short)
         tmp%flags = c_null_char
         tmp%pad   = c_null_char
         tmp%pixel = int(i, c_long)

         stat = XAllocColor(display, colormap, tmp)
         if (stat == 0) then
            write(error_unit, '(A, I0)') "XAllocColor failed (status = ", stat
         else
            carray(i + 1) = tmp%pixel
         end if
      end if
      if (vdevice%sync /= 0) call XFlush(display)
      X11_mapcolor = 0
   end function X11_mapcolor

   integer function X11_font(fontfile)
      character(len=*), intent(in) :: fontfile
      character(len=128) :: fname
      integer(c_long) :: f_handle
      if (c_associated(font_id)) then
         call XFreeFont(display, font_id)
         font_id = c_null_ptr
      end if
      if (trim(fontfile) == "small" .or. trim(fontfile) == "large") then
         fname = "fixed"
      else
         fname = trim(fontfile)
      end if
      font_id = XLoadQueryFont(display, trim(fname) // char(0))
      if (.not. c_associated(font_id)) then
         write(error_unit, '(A, A)') "M_draw X11 couldn't open font ", trim(fname)
         X11_font = 0; return
      end if
      vdevice%hheight = 13.0_real32
      vdevice%hwidth = 8.0_real32
      f_handle = XLoadFont(display, trim(fname) // char(0))
      call XSetFont(display, theGC, f_handle)
      X11_font = 1
   end function X11_font

   integer function X11_char(c)
      character(len=1), intent(in) :: c
      call XDrawString(display, theDrawable, theGC, vdevice%cpVx, vdevice%sizeSy - vdevice%cpVy, c // char(0), 1)
      if (vdevice%sync /= 0) call XFlush(display)
      X11_char = UNUSED
   end function X11_char

   integer function X11_string(s)
      character(len=*), intent(in) :: s
      call XDrawString(display, theDrawable, theGC, vdevice%cpVx, vdevice%sizeSy - vdevice%cpVy, s // char(0), len(s))
      if (vdevice%sync /= 0) call XFlush(display)
      X11_string = UNUSED
   end function X11_string

   integer function X11_fill(n, x, y)
      integer, intent(in) :: n
      integer, intent(in) :: x(n), y(n)
      type(XPoint) :: plist(MAXVERTS)
      integer :: i
      character(len=128) :: buf
      if (n > MAXVERTS) then
         write(buf, '(A, I0, A)') "M_draw: more than ", MAXVERTS, " points in a polygon"
         call draw_verror(buf)
      end if
      do i = 1, n
         plist(i)%x = int(x(i), c_short)
         plist(i)%y = int(vdevice%sizeSy - y(i), c_short)
      end do
      call XFillPolygon(display, theDrawable, theGC, plist, n, Nonconvex, CoordModeOrigin)
      vdevice%cpVx = x(n); vdevice%cpVy = y(n)
      if (vdevice%sync /= 0) call XFlush(display)
      X11_fill = UNUSED
   end function X11_fill

   integer function X11_backbuf()
      if (back_used == 0) then
         bbuf = XCreatePixmap(display, winder, vdevice%sizeSx + 1, vdevice%sizeSy + 1, vdevice%depth)
      end if
      theDrawable = bbuf
      back_used = 1
      X11_backbuf = 1
   end function X11_backbuf

   integer function X11_swapbuf()
      call XCopyArea(display, theDrawable, winder, theGC, 0, 0, vdevice%sizeSx + 1, vdevice%sizeSy + 1, 0, 0)
      call XSync(display, 0)
      X11_swapbuf = UNUSED
   end function X11_swapbuf

   integer function X11_frontbuf()
      theDrawable = winder
      X11_frontbuf = UNUSED
   end function X11_frontbuf
   integer function X11_sync()
      call XSync(display, 0)
      X11_sync = UNUSED
   end function X11_sync

   subroutine setup_x11dev()
      vdevice%dev%large = "large"
      vdevice%dev%small = "small"
      vdevice%dev%Vbackb => X11_backbuf
      vdevice%dev%Vchar => X11_char
      vdevice%dev%Vcheckkey => X11_checkkey
      vdevice%dev%Vclear => X11_clear
      vdevice%dev%Vcolor => X11_color
      vdevice%dev%Vdraw => X11_draw
      vdevice%dev%Vexit => X11_exit
      vdevice%dev%Vfill => X11_fill
      vdevice%dev%Vfont => X11_font
      vdevice%dev%Vfrontb => X11_frontbuf
      vdevice%dev%Vgetkey => X11_getkey
      vdevice%dev%Vinit => X11_init
      vdevice%dev%Vlocator => X11_locator
      vdevice%dev%Vmapcolor => X11_mapcolor
      vdevice%dev%Vsetlw => X11_setlw
      vdevice%dev%Vstring => X11_string
      vdevice%dev%Vswapb => X11_swapbuf
      vdevice%dev%Vsync => X11_sync
   end subroutine setup_x11dev

   integer function X11_draw_devcpy()
      call setup_x11dev()
      vdevice%dev%devname = "X11"
      GLOBAL_device = DEV_X11_UPPER
      X11_draw_devcpy = UNUSED
   end function X11_draw_devcpy

   integer function x11_low_draw_devcpy()
      call setup_x11dev()
      vdevice%dev%devname = "x11"
      GLOBAL_device = DEV_X11_LOWER
      x11_low_draw_devcpy = UNUSED
   end function x11_low_draw_devcpy
end module m_draw_drv_x11
