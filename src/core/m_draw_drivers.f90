module m_draw_drivers
   use, intrinsic :: iso_fortran_env, only: real32, int16, output_unit, error_unit
   use m_draw_types
   use m_draw_verror
   use m_draw_pref
   use m_draw_matrix
   use m_draw_mapping
   use m_draw_viewp
   use m_draw_aspect
   use m_draw_attr
   use m_draw_move
   use m_draw_text
   use m_draw_newtokens
   implicit none
   private
   public :: draw_voutput, draw_outfile, draw_vinit, draw_pushdev, draw_popdev, &
      draw_vnewdev, draw_vgetdev, draw_getkey, draw_checkkey, &
      draw_locator, draw_slocator, draw_clear, draw_vexit, draw_color, &
      draw_mapcolor, draw_getdepth, draw_vsetflush, draw_vflush, draw_linewidth

   integer, save :: allocated_stacks = 0
   integer, save :: badcount = 0

   integer, parameter :: DEVSTACK = 8
   type(Device), save :: vdevstk(DEVSTACK)
   integer, save :: vdevindx = 0

   ! External driver registration hooks (implemented in driver modules)
   interface
      integer function drv_X11_devcpy()
      end function drv_X11_devcpy
      integer function drv_x11_low_devcpy()
      end function drv_x11_low_devcpy
      integer function drv_TEK_devcpy()
      end function drv_TEK_devcpy
      integer function drv_XTEK_devcpy()
      end function drv_XTEK_devcpy
      integer function drv_PS_devcpy()
      end function drv_PS_devcpy
      integer function drv_PSP_devcpy()
      end function drv_PSP_devcpy
      integer function drv_PPSC_devcpy()
      end function drv_PPSC_devcpy
      integer function drv_PPSG_devcpy()
      end function drv_PPSG_devcpy
      integer function drv_PSC_devcpy()
      end function drv_PSC_devcpy
      integer function drv_PSG_devcpy()
      end function drv_PSG_devcpy
      integer function drv_PCL_PORT_devcpy()
      end function drv_PCL_PORT_devcpy
      integer function drv_PCL_LAND_devcpy()
      end function drv_PCL_LAND_devcpy
      integer function drv_PCL5_PORT_devcpy()
      end function drv_PCL5_PORT_devcpy
      integer function drv_PCL5_LAND_devcpy()
      end function drv_PCL5_LAND_devcpy
      integer function drv_HPGL_LAND_devcpy()
      end function drv_HPGL_LAND_devcpy
      integer function drv_HPGL_PORT_devcpy()
      end function drv_HPGL_PORT_devcpy
      integer function drv_HPGL2_LAND_devcpy()
      end function drv_HPGL2_LAND_devcpy
      integer function drv_HPGL2_PORT_devcpy()
      end function drv_HPGL2_PORT_devcpy
      integer function drv_DXY_devcpy()
      end function drv_DXY_devcpy
      integer function drv_CGMT_devcpy()
      end function drv_CGMT_devcpy
      integer function drv_UNIXPLOT_devcpy()
      end function drv_UNIXPLOT_devcpy
      integer function drv_VOG_devcpy()
      end function drv_VOG_devcpy
      integer function drv_GNU_devcpy()
      end function drv_GNU_devcpy
      integer function drv_SVG_devcpy()
      end function drv_SVG_devcpy
      integer function drv_XFIG_devcpy()
      end function drv_XFIG_devcpy
      integer function drv_CANVAS_devcpy()
      end function drv_CANVAS_devcpy
      integer function drv_USEMAP_devcpy()
      end function drv_USEMAP_devcpy
      integer function drv_MIF_devcpy()
      end function drv_MIF_devcpy
      integer function drv_MIF4_devcpy()
      end function drv_MIF4_devcpy
      integer function drv_VML_devcpy()
      end function drv_VML_devcpy
      integer function drv_FTI_devcpy()
      end function drv_FTI_devcpy
      integer function drv_PDF_devcpy()
      end function drv_PDF_devcpy
      integer function drv_RTF_devcpy()
      end function drv_RTF_devcpy
      integer function drv_PBM_devcpy()
      end function drv_PBM_devcpy
      integer function drv_P1_devcpy()
      end function drv_P1_devcpy
      integer function drv_P4_devcpy()
      end function drv_P4_devcpy
      integer function drv_PPM_devcpy()
      end function drv_PPM_devcpy
      integer function drv_P3_devcpy()
      end function drv_P3_devcpy
      integer function drv_P6_devcpy()
      end function drv_P6_devcpy
      integer function drv_CHAR_devcpy()
      end function drv_CHAR_devcpy
      integer function drv_BM_devcpy()
      end function drv_BM_devcpy
      integer function drv_XBM_devcpy()
      end function drv_XBM_devcpy
      integer function drv_NIL_devcpy()
      end function drv_NIL_devcpy
   end interface

   interface draw_vsetflush
      module procedure draw_vsetflush_integer
      module procedure draw_vsetflush_logical
   end interface draw_vsetflush

contains

   subroutine draw_voutput(path)
      character(len=*), intent(in) :: path
      integer :: u, ios

      if (trim(path) == "-") then
         draw_unit = output_unit
      else if (trim(path) == "+") then
         draw_unit = error_unit
      else if (len_trim(path) > 0 .and. path(1:1) == "|") then
         ! Pure Fortran fallback for pipe syntax: write to scratch or M_DRAW_OUTPUT
         open(newunit=u, file="M_DRAW_OUTPUT", status="unknown", position="append", action="write", iostat=ios)
         if (ios /= 0) then
            draw_unit = output_unit
         else
            draw_unit = u
            vdevice%writestoprocess = 1
         end if
      else
         open(newunit=u, file=trim(path), status="replace", action="write", iostat=ios)
         if (ios /= 0) then
            write(error_unit, '(A, A, A)') "voutput: could not open [", trim(path), "]; appending to M_DRAW_OUTPUT"
            open(newunit=u, file="M_DRAW_OUTPUT", status="unknown", position="append", action="write", iostat=ios)
            if (ios /= 0) then
               write(error_unit, '(A)') "voutput: could not open M_DRAW_OUTPUT; using stdout."
               draw_unit = output_unit
            else
               draw_unit = u
            end if
         else
            draw_unit = u
         end if
      end if
   end subroutine draw_voutput

   integer function draw_outfile()
      logical :: is_open
      if (draw_unit == output_unit .or. draw_unit == error_unit) then
         draw_outfile = draw_unit
      else
         inquire(unit=draw_unit, opened=is_open)
         if (.not. is_open) draw_unit = output_unit
         draw_outfile = draw_unit
      end if
   end function draw_outfile

   subroutine draw_getdev(dev)
      character(len=*), intent(in) :: dev
      character(len=132) :: dtmp, env_dev, sdevice
      integer :: xx, yy, xxo, yyo, ifound, env_len, ios, idum

      dtmp = ""
      if (len_trim(dev) == 0) then
         call get_environment_variable("M_DRAW_DEVICE", value=env_dev, length=env_len, status=ios)
         if (ios == 0 .and. env_len > 0) then
            dtmp = trim(env_dev)
         end if
      else
         dtmp = trim(dev)
      end if

      sdevice = ""
      ifound = 0
      read(dtmp, *, iostat=ios) sdevice
      if (ios == 0) ifound = 1
      read(dtmp, *, iostat=ios) sdevice, xx
      if (ios == 0) ifound = 2
      read(dtmp, *, iostat=ios) sdevice, xx, yy
      if (ios == 0) ifound = 3
      read(dtmp, *, iostat=ios) sdevice, xx, yy, xxo
      if (ios == 0) ifound = 4
      read(dtmp, *, iostat=ios) sdevice, xx, yy, xxo, yyo
      if (ios == 0) ifound = 5

      if (ifound == 2) then
         call draw_prefsize(xx, xx)
      else if (ifound == 3) then
         call draw_prefsize(xx, yy)
      else if (ifound == 4) then
         call draw_prefsize(xx, yy); call draw_prefposition(xxo, xxo)
      else if (ifound >= 5) then
         call draw_prefsize(xx, yy); call draw_prefposition(xxo, yyo)
      end if

      if (sdevice(1:3) == "nil") then; idum = drv_NIL_devcpy()
      else if (sdevice(1:3) == "X11") then; idum = drv_X11_devcpy()
      else if (sdevice(1:3) == "x11") then; idum = drv_x11_low_devcpy()
      else if (sdevice(1:3) == "rtf") then; idum = drv_RTF_devcpy()
      else if (sdevice(1:3) == "svg") then; idum = drv_SVG_devcpy()
      else if (sdevice(1:4) == "xfig") then; idum = drv_XFIG_devcpy()
      else if (sdevice(1:6) == "canvas") then; idum = drv_CANVAS_devcpy()
      else if (sdevice(1:6) == "usemap") then; idum = drv_USEMAP_devcpy()
      else if (sdevice(1:10) == "postscript") then; idum = drv_PS_devcpy()
      else if (sdevice(1:11) == "ppostscript") then; idum = drv_PSP_devcpy()
      else if (sdevice(1:4) == "ppsm") then; idum = drv_PSP_devcpy()
      else if (sdevice(1:4) == "ppsc") then; idum = drv_PPSC_devcpy()
      else if (sdevice(1:4) == "ppsg") then; idum = drv_PPSG_devcpy()
      else if (sdevice(1:3) == "pps") then; idum = drv_PSP_devcpy()
      else if (sdevice(1:3) == "psm") then; idum = drv_PS_devcpy()
      else if (sdevice(1:3) == "psc") then; idum = drv_PSC_devcpy()
      else if (sdevice(1:3) == "psg") then; idum = drv_PSG_devcpy()
      else if (sdevice(1:2) == "ps") then; idum = drv_PS_devcpy()
      else if (sdevice(1:7) == "pclport") then; idum = drv_PCL_PORT_devcpy()
      else if (sdevice(1:7) == "pclland") then; idum = drv_PCL_LAND_devcpy()
      else if (sdevice(1:8) == "hpglland") then; idum = drv_HPGL_LAND_devcpy()
      else if (sdevice(1:8) == "hpglport") then; idum = drv_HPGL_PORT_devcpy()
      else if (sdevice(1:4) == "hpgl") then; idum = drv_HPGL_LAND_devcpy()
      else if (sdevice(1:8) == "pcl5port") then; idum = drv_PCL5_PORT_devcpy()
      else if (sdevice(1:8) == "pcl5land") then; idum = drv_PCL5_LAND_devcpy()
      else if (sdevice(1:9) == "hpgl2land") then; idum = drv_HPGL2_LAND_devcpy()
      else if (sdevice(1:9) == "hpgl2port") then; idum = drv_HPGL2_PORT_devcpy()
      else if (sdevice(1:3) == "dxy") then; idum = drv_DXY_devcpy()
      else if (sdevice(1:3) == "tek") then; idum = drv_TEK_devcpy()
      else if (sdevice(1:4) == "xtek") then; idum = drv_XTEK_devcpy()
      else if (sdevice(1:4) == "cgmt") then; idum = drv_CGMT_devcpy()
      else if (sdevice(1:4) == "mif4") then; idum = drv_MIF4_devcpy()
      else if (sdevice(1:3) == "mif") then; idum = drv_MIF_devcpy()
      else if (sdevice(1:3) == "pdf") then; idum = drv_PDF_devcpy()
      else if (sdevice(1:8) == "unixplot") then; idum = drv_UNIXPLOT_devcpy()
      else if (sdevice(1:3) == "vml") then; idum = drv_VML_devcpy()
      else if (sdevice(1:3) == "vog") then; idum = drv_VOG_devcpy()

      else if (sdevice(1:3) == "gnu") then; idum = drv_GNU_devcpy()
      else if (sdevice(1:3) == "fti") then; idum = drv_FTI_devcpy()
      else if (sdevice(1:3) == "pbm") then; idum = drv_PBM_devcpy()
      else if (sdevice(1:2) == "p1") then; idum = drv_P1_devcpy()
      else if (sdevice(1:2) == "p4") then; idum = drv_P4_devcpy()
      else if (sdevice(1:3) == "xbm") then; idum = drv_XBM_devcpy()
      else if (sdevice(1:2) == "bm") then; idum = drv_BM_devcpy()
      else if (sdevice(1:4) == "char") then; idum = drv_CHAR_devcpy()
      else if (sdevice(1:3) == "ppm") then; idum = drv_PPM_devcpy()
      else if (sdevice(1:2) == "p3") then; idum = drv_P3_devcpy()
      else if (sdevice(1:2) == "p6") then; idum = drv_P6_devcpy()
      else
         write(error_unit, '(A, A, A)') "draw: ", trim(sdevice), " is an invalid device type; setting to nil"
         badcount = badcount + 1
         if (badcount > 30) error stop 1
         idum = drv_NIL_devcpy()
      end if
   end subroutine draw_getdev

   subroutine draw_vinit(device)
      character(len=*), intent(in) :: device
      character(len=512) :: env_val
      integer :: env_len, ios
      logical :: is_open
      inquire(unit=draw_unit, opened=is_open)
      if (.not. is_open) then
         draw_unit = output_unit
         call get_environment_variable("M_DRAW_OUTPUT", value=env_val, length=env_len, status=ios)
         if (ios == 0 .and. env_len > 0) call draw_voutput(trim(env_val))
      end if
      call draw_getdev(device)
      if (vdevice%initialized /= 0) call draw_vexit()
      if (allocated_stacks == 0) then
         allocated_stacks = 1
         allocate(vdevice%transmat, vdevice%attr, vdevice%viewport)
         vdevice%transmat%back => null()
         vdevice%attr%back => null()
         vdevice%viewport%back => null()
      end if
      vdevice%clipoff = 0
      vdevice%sync = 1
      vdevice%upset = 0
      vdevice%cpW(D_W) = 1.0_real32
      if (allocated(vdevice%attr%a%style)) deallocate(vdevice%attr%a%style)
      vdevice%attr%a%dash_pos = 1
      vdevice%attr%a%fill = 0
      vdevice%attr%a%hatch = 0
      vdevice%attr%a%backface = 0
      vdevice%attr%a%color = 0
      vdevice%attr%a%justify = ior(D_LEFT, D_BOTTOM)
      vdevice%attr%a%bold = 0
      vdevice%attr%a%inbackbuffer = 0
      vdevice%attr%a%exvp = 0
      vdevice%attr%a%softtext = 0
      vdevice%attr%a%fixedwidth = 0
      vdevice%attr%a%skew = 0.0_real32
      vdevice%attr%a%hatchcos = 1.0_real32
      vdevice%attr%a%hatchsin = 0.0_real32
      vdevice%attr%a%hatchpitch = 0.1_real32
      vdevice%attr%a%textcos = 1.0_real32
      vdevice%attr%a%textsin = 0.0_real32
      vdevice%attr%a%dash = 0.0_real32
      vdevice%attr%a%adist = 0.0_real32
      vdevice%attr%a%font = ""
      if (vdevice%dev%Vinit() /= 0) then
         vdevice%initialized = 1
         vdevice%writestoprocess = 1
         vdevice%inobject = 0
         vdevice%inpolygon = 0
         call get_environment_variable("VEXPANDVP", length=env_len, status=ios)
         if (ios == 0) call draw_expandviewport()
         call draw_viewport(-1.0_real32, 1.0_real32, -1.0_real32, 1.0_real32)
         call draw_identmatrix(vdevice%transmat%m)
         call draw_move(0.0_real32, 0.0_real32, 0.0_real32)
         if (draw_hershfont("futura.l") == 0) then
            call draw_font("small")
         else
            call draw_textsize(0.05_real32, 0.05_real32)
         end if
      else
         write(error_unit, '(A)') "draw: error while setting up device"
         error stop 1
      end if
   end subroutine draw_vinit

   subroutine draw_pushdev(device)
      character(len=*), intent(in) :: device
      integer :: idum
      call draw_pushattributes()
      call draw_pushviewport()
      if (vdevindx < DEVSTACK) then
         vdevindx = vdevindx + 1
         vdevstk(vdevindx) = vdevice
      else
         call draw_verror("draw: pushdev: Device stack overflow")
      end if
      vdevice%initialized = 0
      call draw_getdev(device)
      idum = vdevice%dev%Vinit()
      vdevice%initialized = 1
      call draw_popviewport()
      call draw_popattributes()
   end subroutine draw_pushdev

   subroutine draw_popdev()
      integer :: idum
      call draw_pushattributes()
      call draw_pushviewport()
      idum = vdevice%dev%Vexit()
      if (vdevindx > 0) then
         vdevice = vdevstk(vdevindx)
         vdevindx = vdevindx - 1
      else
         call draw_verror("draw: popdev: Device stack underflow")
      end if
      call draw_popviewport()
      call draw_popattributes()
   end subroutine draw_popdev

   subroutine draw_vnewdev(device)
      character(len=*), intent(in) :: device
      integer :: idum
      if (vdevice%initialized == 0) call draw_verror("vnewdev: draw not initialized")
      call draw_pushviewport()
      idum = vdevice%dev%Vexit()
      vdevice%initialized = 0
      call draw_getdev(device)
      idum = vdevice%dev%Vinit()
      vdevice%initialized = 1
      if (trim(vdevice%attr%a%font) == "small") then
         if (vdevice%dev%Vfont(trim(vdevice%dev%small)) == 0) call draw_verror("font: unable to open small font")
      else if (trim(vdevice%attr%a%font) == "large") then
         if (vdevice%dev%Vfont(trim(vdevice%dev%large)) == 0) call draw_verror("font: unable to open large font")
      end if
      call draw_popviewport()
   end subroutine draw_vnewdev

   subroutine draw_vgetdev(buf)
      character(len=*), intent(out) :: buf
      if (len_trim(vdevice%dev%devname) > 0) then
         buf = trim(vdevice%dev%devname)
      else
         buf = "(no device)"
      end if
   end subroutine draw_vgetdev

   integer function draw_getkey()
      if (vdevice%initialized == 0) call draw_verror("getkey: draw not initialized")
      draw_getkey = vdevice%dev%Vgetkey()
   end function draw_getkey

   integer function draw_checkkey()
      if (vdevice%initialized == 0) call draw_verror("checkkey: draw not initialized")
      draw_checkkey = vdevice%dev%Vcheckkey()
   end function draw_checkkey

   integer function draw_locator(wx, wy)
      real(real32), intent(out) :: wx, wy
      integer :: a, b, c
      if (vdevice%initialized == 0) call draw_verror("locator: draw not initialized")
      c = vdevice%dev%Vlocator(a, b)
      call draw_VtoWxy(real(a, real32), real(b, real32), wx, wy)
      draw_locator = c
   end function draw_locator

   integer function draw_slocator(wx, wy)
      real(real32), intent(out) :: wx, wy
      integer :: a, b, c
      if (vdevice%initialized == 0) call draw_verror("slocator: draw not initialized")
      c = vdevice%dev%Vlocator(a, b)
      wx = real(a, real32) / (0.5_real32 * real(vdevice%sizeX, real32)) - 1.0_real32
      wy = real(b, real32) / (0.5_real32 * real(vdevice%sizeY, real32)) - 1.0_real32
      draw_slocator = c
   end function draw_slocator

   subroutine draw_clear()
      type(Token), pointer :: tok(:)
      integer :: idum
      if (vdevice%initialized == 0) call draw_verror("clear: draw not initialized")
      if (vdevice%inobject /= 0) then
         tok => draw_newtokens(1); tok(1)%i = OBJ_CLEAR; return
      end if
      idum = vdevice%dev%Vclear()
   end subroutine draw_clear

   subroutine draw_vexit()
      integer :: idum
      if (vdevice%initialized == 0) call draw_verror("vexit: draw not initialized")
      idum = vdevice%dev%Vexit()
      vdevice%initialized = 0
      draw_unit = output_unit
      badcount = 0
   end subroutine draw_vexit

   subroutine draw_color(i)
      integer, intent(in) :: i
      type(Token), pointer :: tok(:)
      integer :: idum
      if (vdevice%initialized == 0) call draw_verror("color: draw not initialized")
      if (vdevice%inobject /= 0) then
         tok => draw_newtokens(2); tok(1)%i = OBJ_COLOR; tok(2)%i = i; return
      end if
      vdevice%attr%a%color = i
      idum = vdevice%dev%Vcolor(i)
   end subroutine draw_color

   subroutine draw_mapcolor(i, r, g, b)
      integer, intent(in) :: i
      integer(int16), intent(in) :: r, g, b
      type(Token), pointer :: tok(:)
      integer :: idum
      if (vdevice%initialized == 0) call draw_verror("mapcolor: draw not initialized")
      if (vdevice%inobject /= 0) then
         tok => draw_newtokens(5)
         tok(1)%i = OBJ_MAPCOLOR; tok(2)%i = i; tok(3)%i = r; tok(4)%i = g; tok(5)%i = b
         return
      end if
      idum = vdevice%dev%Vmapcolor(i, int(r), int(g), int(b))
   end subroutine draw_mapcolor

   integer function draw_getdepth()
      if (vdevice%initialized == 0) call draw_verror("getdepth: draw not initialized")
      draw_getdepth = vdevice%depth
   end function draw_getdepth

   subroutine draw_vsetflush_integer(yn)
      integer, intent(in) :: yn
      vdevice%sync = yn
   end subroutine draw_vsetflush_integer

   subroutine draw_vsetflush_logical(yn)
      logical, intent(in) :: yn
      vdevice%sync = merge(1,0,yn)
   end subroutine draw_vsetflush_logical

   subroutine draw_vflush()
      type(Token), pointer :: tok(:)
      integer :: idum
      if (vdevice%initialized == 0) call draw_verror("vflush: draw not initialized")
      if (vdevice%inobject /= 0) then
         tok => draw_newtokens(1); tok(1)%i = OBJ_VFLUSH; return
      end if
      idum = vdevice%dev%Vsync()
   end subroutine draw_vflush

   subroutine draw_linewidth(w)
      integer, intent(in) :: w
      type(Token), pointer :: tok(:)
      integer :: idum
      if (vdevice%initialized == 0) call draw_verror("linewidth: draw not initialized")
      if (vdevice%inobject /= 0) then
         tok => draw_newtokens(2); tok(1)%i = OBJ_LINEWIDTH; tok(2)%i = w; return
      end if
      idum = vdevice%dev%Vsetlw(w)
   end subroutine draw_linewidth

end module m_draw_drivers
