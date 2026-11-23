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

      integer function drv_ansi_devcpy()
      end function drv_ansi_devcpy

      integer function drv_SIXEL_devcpy()
      end function drv_SIXEL_devcpy

      integer function drv_sixel_low_devcpy()
      end function drv_sixel_low_devcpy

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

!>
!!##NAME
!!    voutput(3f) - [M_draw:DEVICE] Redirect output from *next* vinit to file
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!          subroutine voutput(path)
!!          character*(*) path
!!##DESCRIPTION
!!
!!       Redirect output from *next* vinit() to file given by path. This routine
!!       only applies to device drivers that write to stdout e.g. PostScript and
!!       hpgl.
!!
!!       The special file names are
!!
!!         * -  is standard output
!!         * +  is standard error
!!         * |  command will create a pipe to "command"
!!
!!       If the open of the file fails, an attempt is made to append to file
!!       "M_DRAW_OUTPUT". If this fails, standard output is used.
!!
!!       When vinit() is called if voutput() has not been called then the
!!       environment variable M_DRAW_OUTPUT is checked and if it is defined and not a
!!       null string then voutput() is called with the M_DRAW_OUTPUT variable's value.
!!
!!       A common use of the |command option is to automatically call programs
!!       that convert PPM files to other common pixmap formats or converts the GNU
!!       metafile to other formats (typically via the GNU plotutils plot program).
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_voutput
!!    use M_draw
!!    implicit none
!!    ! want a 400x400 raster output
!!    call prefsize(400,400)
!!    ! convert PPM to a GIF file using ppmtogif(1)
!!    call voutput('|ppmtogif >voutput.3m_draw.gif')
!!    ! start graphics using PPM device
!!    call vinit('p6')
!!    ! draw a filled circle
!!    call color(D_RED)
!!    call polyfill(.true.)
!!    call circle(0.0,0.0,1.0)
!!    !
!!    call vexit()
!!    end program demo_voutput
!>
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
      else if (sdevice(1:5) == "SIXEL") then; idum = drv_SIXEL_devcpy()
      else if (sdevice(1:5) == "sixel") then; idum = drv_sixel_low_devcpy()
      else if (sdevice(1:4) == "ansi") then; idum = drv_ansi_devcpy()
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

!>
!!##NAME
!!    vinit(3f) - [M_draw:DEVICE] Initialise device
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!          subroutine vinit(device)
!!          character(len=*),intent(in) :: device
!!
!!##DESCRIPTION
!!    Initialise the device.
!!
!!
!!  Note 1 : Currently available devices are:
!!
!!        INTERACTIVE DEVICES
!!
!!        PC   - native MSW PC driver; only tested from CygWin
!!        X11  - X11 windows (Black background)
!!        x11  - X11 windows (White background)
!!        tek  - tektronix 4010 and compatibles
!!        xtek - X11 xterm Tektronix 4010 emulator
!!
!!        PRINTERS AND PLOTTERS
!!
!!           PostScript:
!!              [p]psm or [p]postscript - monochrome PostScript
!!              [p]psg - grayscale PostScript
!!              [p]psc - color PostScript
!!           HPGL:
!!              hpgl - HP Graphics language and compatibles
!!              [p]hpgl2 - HPGL level 2 (obeys prefsize calls)
!!           PCL:
!!              [p]pclland  - monochrome PCL5 (obeys prefsize calls)
!!              [p]pclport  - monochrome PCL5 (obeys prefsize calls)
!!              pcl5land (color PCL5 landscape)
!!              pcl5port (color PCL5 portrait)
!!
!!        PIXMAPS (COLOR) AND BITMAPS (MONOCHROME)
!!
!!        char     An ASCII file that can be displayed to most
!!                 xterm(1) terminal emulators that support
!!                 color
!!        p1/pbm   Poskanzer (pbmplus/netplus) portable
!!                 ASCII bitmap file
!!        p3/ppm   Poskanzer portable ASCII pixmap file
!!        p4       Poskanzer portable binary bitmap file
!!        p6       Poskanzer portable binary pixmap file
!!        xbm      X11 bitmap file
!!        bm       bitmap format for atobm(1)
!!
!!        PRODUCT INPUT FILES
!!
!!        mif        FrameMaker Interchange Format 3.0 (MIF) files
!!                   (16 colors)
!!        mif4       FrameMaker Interchange Format 4.0 (MIF) files
!!                   (user-definable colors, but breaks a MIF rule)
!!        xfig       X11 xfig(1) figure utility
!!
!!        METAFILES WITH POST_PROCESSORS/CONVERTERS
!!
!!        cgmt      a clear-text CGM (Computer Graphics Metafile)
!!        gnu       GNU plot(1) metafile
!!        pdf       Adobe Public Document Format
!!        unixplot  Unix plot(1) metafile
!!
!!        BROWSER FILES
!!
!!        canvas    HTML5 CANVAS graphics element file
!!        svg       Scalable Vector Graphics
!!        usemap    HTML image map
!!        vml       Microsoft Vector Markup Language
!!
!!        FILES
!!        vog       M_draw low level call record (debug)
!!
!!        OTHER
!!        fti       SGI vector-based icons
!!        null      no output
!!
!!      :- Drivers I've dropped but code is there for
!!      ---------------------------------------------
!!
!!        grwin - (minGW GRwin PC interface)
!!        decX11 - the decstation window manager
!!        dxy - roland DXY plotter language
!!        sun - Sun workstations running sunview
!!        next - NeXTStep and other NeXT platforms
!!        apollo - Apollo workstations
!!
!!      :- Drivers I've dropped but are in the original distribution
!!      ------------------------------------------------------------
!!
!!        hercules - IBM PC hercules graphics card
!!        cga - IBM PC cga graphics card
!!        ega - IBM PC ega graphics card
!!        vga - IBM PC vga graphics card
!!        sigma - IBM PC sigma graphics card.
!!        mswin - IBM PC Microsoft Windows.
!!
!!     Note 2 : If device is a NULL or a null string the value
!!          of the environment variable "M_DRAW_DEVICE" is taken as the
!!          device type to be opened. The format of the variable is
!!
!!             M_DRAW_DEVICE [ xsize [ ysize [ xoffset [ yoffset ] ] ]
!!
!!          That is, if present xsize and ysize will be used
!!          in a call to prefsize(3c), and xoffset and yoffset will
!!          be used in a call to preposition(3c).
!!
!!     Note 3 : after vinit() it is wise to explicitly clear the screen.
!!     e.g.:
!!
!!          call color(D_BLACK)
!!          call clear()
!!
!!##NOTES
!!
!! gnu
!!
!!    The GNU plotutils package includes a program called plot(1) that
!!    can read in the gnu metafile and render images on an X11 display, PNG
!!    (Portable Network Graphics) format, portable anymap format (PBM/PGM/PPM),
!!    a pseudo-GIF format that does not use LZW encoding, the new XML-based
!!    Scalable Vector Graphics format, the format used by Adobe Illustrator,
!!    Postscript or Encapsulated Postscript (EPS) that can be edited with
!!    idraw(1), CGM format (by default, confirming to the WebCGM profile),
!!    the format used by the xfig(1) drawing editor, the Hewlett-Packard PCL 5
!!    printer language, the Hewlett-Packard Graphics Language, ReGIS graphics
!!    format (which can be displayed by the dxterm(1) terminal emulator or
!!    by a VT330 or VT340 terminal), Tektronix format (which can be displayed
!!    by the xterm(1) terminal emulator), and device-independent GNU metafile
!!    format itself.
!!
!! pdf
!!
!!    Popular PDF readers are the Adobe PDF viewer, which is often callable from
!!    Web browsers; the GhostScript-based gv(1) utility; or the xpdf program.
!!
!!        The xpdf(1) software , related utilities ( pdftops(1), pdftotext(1),
!!        pdfinfo(1), pdffonts(1), pdftoppm(1), pdfimages(1), xpdfrc (5))
!!        and documentation are copyright 1996-2004 Glyph & Cog, LLC. at
!!        http://www.foolabs.com/xpdf/
!!
!!    The GhostScript-based tools can convert PDF files to PostScript as
!!    well as view the files.
!!
!! cgmt
!!
!!    The ralcgm(1) and gplot(1) packages are two very complete CGM viewers.
!!
!!    ppm,pbm (and p1,p3,p4,p6)
!!
!!      * p1/pbm - Poskanzer (pbmplus/netplus) portable ASCII bitmap file
!!      * p3/ppm - Poskanzer portable ASCII pixmap file
!!      * p4 - Poskanzer portable binary bitmap file
!!      * p6 - Poskanzer portable binary pixmap file
!!
!!    The NetPBM package is available for almost every platform and lets
!!    you convert the Poskanzer portable pixmap (PPM) files to just about
!!    any pixmap or bitmap format, including PNG, JPEG, GIF/PseudoGIF, BPM,
!!    ..... Other popular pixmap products such as ImageMagick, gv, ... can
!!    read PPM files, convert them, and often edit them.
!!
!!##HTML:
!!
!!    The vml, canvas, svg, and usemap drivers are primarily used to generate
!!    graphics for inclusion in HTML documents. Browsers such as Opera, Safari,
!!    Foxfire, and Chrome can easily incorporate graphics generated using the
!!    SVG (Scalable Vector Graphics) format or the HTML5 CANVAS element.
!!
!! usemap
!!
!!    This driver writes out the edges of any polygon in a format that can be
!!    used with an HTML image map; if the same sizes are used a plot generated
!!    with the ppm driver; you will have clickable regions in your pixmap when
!!    converted to a GIF image.
!!
!!    If the polygons overlap you need to reverse the order of the polygon
!!    definitions in the output file. The numeric field in the<AREA> titles
!!    should help.
!!
!! vml
!!
!!    The VML format can be read in by any MicroSoft Office 2000+ product and
!!    MicroSoft's web browser Internet Explorer. If the plots contain more than
!!    about 9766 vectors MicroSoft Word starts choking (still true in 2005),
!!    but otherwise this is a very nice way to generate input for MicroSoft
!!    products.
!!
!!    I generally use this on a machine running MicroSoft Windows by installing
!!    CygWin with the X11 options (and ralcgm, the GhostScript software,
!!    the GNU plotutils packages and netpbm).
!!
!! xfig
!!
!!    The xfig(1) command can be used to edit graphics generated with the
!!    M_draw graphics library; and to convert the xfig(1)-format file to
!!    many other output formats. If you are generating pixmaps with the PPM
!!    driver and want to use them as image maps in your HTML documents the
!!    usemap driver can be used.
!!
!!    If you have xfig(1) installed, you will find that calling fig2dev(1)
!!    allows you to generate many output formats from a single file,
!!    including LaTex and encapsulated PostScript.
!!
!!    xfig(1) is an X11 Windows application that can be used to interactively
!!    edit figures. The HELP utility of xfig(1) provides a description of the
!!    xfig(1) file format (as well as a user guide and many other documents).
!!
!!    Unfortunately, the manual indicates the user defined colors must
!!    be defined before any other Fig objects. By default, 16 colors are
!!    defined. If undefined colors are used they are assigned a dash pattern
!!    or a fill pattern to help distinguish them. Use of hardware dash and
!!    M_draw software dash could get confusing.
!!
!!    Also, in the current driver version all lines are drawn as a series
!!    of move-draw vectors, which can make the files relatively very large.
!!
!!    multiple pages appear to only work with the PostScript and PDF drivers
!!    of xfig(1); and even then pages must be all positive numbers from left
!!    to right and top to bottom, printing all pages in a rectangular area.
!!
!!    Alternatively, could use depth to keep up to 999 pages separate
!!
!!##EXAMPLE
!!
!!  Sample program:
!!
!!       program demo_vinit
!!       use M_draw
!!       use ISO_C_BINDING
!!       implicit none
!!       integer :: ios, idum
!!       character(len=50) :: device
!!
!!       ! read in device name and start graphics mode
!!       print*,'Enter output device:'
!!       read(*,'(a)',iostat=ios)device
!!       if(ios.ne.0)device=' '
!!       call vinit(device)
!!       ! by default the drawing surface is
!!       ! a square ranging from -1 to 1 in both
!!       ! the X and Y axis
!!
!!       ! set font to large hardware font
!!       call font('large')
!!
!!       ! set current color to black
!!       call color(D_BLACK)
!!
!!       ! clear to current color
!!       call clear()
!!
!!       ! we want to draw in green
!!       call color(D_GREEN)
!!
!!       ! draw a horizontal line at y = 0
!!       call move2(-1.0, 0.0)
!!       call draw2(1.0, 0.0)
!!
!!       ! pause for some input
!!       idum=getkey()
!!
!!       ! draw a line along x = 0
!!       call move2(0.0, 0.0)
!!       call draw2(0.0, 1.0)
!!
!!       ! move to the middle of the screen
!!       call move2(0.0, 0.0)
!!
!!       ! draw 'Hello' starting at the origin
!!       call drawstr('Hello')
!!
!!       ! pause again
!!       idum=getkey()
!!
!!       !  wrap up and exit graphics mode
!!       call vexit()
!!
!!       end program demo_vinit
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

!>
!!##NAME
!!    pushdev(3f) - [M_draw:DEVICE] push current device onto a stack
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!          subroutine pushdev(device)
!!          character *(*) device
!!
!!##DESCRIPTION
!!    Initialize a new device without changing attributes, viewport etc,
!!    but save the previously initialised device on a stack.
!!
!!    Note, this is intended to completely change the device, it will not work
!!    if you pushdev() the same device that you are already running. (This will
!!    be fixed at a later date).
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

!>
!!##NAME
!!    popdev(3f) - [M_draw:DEVICE] pop device from stack created by pushdev.
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!           subroutine popdev()
!!
!!##DESCRIPTION
!!
!!    Pops a device off the device stack and reinstates the previously pushed
!!    device.
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

!>
!!##NAME
!!    vnewdev(3f) - [M_draw:DEVICE] Reinitialize to use new device without changing
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!          subroutine vnewdev(device)
!!          character *(*) device
!!##DESCRIPTION
!!
!!    Reinitialize M_draw to use a new device without changing attributes,
!!    viewport etc. (eg. window and viewport specifications)
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

!>
!!##NAME
!!    vgetdev(3f) - [M_draw:DEVICE] Get name of current device
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!          subroutine vgetdev(device)
!!          character *(*) device
!!##DESCRIPTION
!!
!!    Gets the name of the current M_draw device. The C version of the routine
!!    also returns a pointer to its argument.
   subroutine draw_vgetdev(buf)
      character(len=*), intent(out) :: buf
      if (len_trim(vdevice%dev%devname) > 0) then
         buf = trim(vdevice%dev%devname)
      else
         buf = "(no device)"
      end if
   end subroutine draw_vgetdev

!>
!!##NAME
!!    getkey(3f) - [M_draw:INTERACTIVE] Return ASCII ordinal of next key typed
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!          integer function getkey()
!!
!!##DESCRIPTION
!!
!!    Return the ASCII ordinal of the next key typed at the keyboard. If the
!!    device has no keyboard getkey(3) returns -1.
!!##EXAMPLE
!!
!!   Sample program:
!!
!!      program demo_getkey
!!      use :: M_draw
!!      implicit none
!!      integer :: ichar
!!      !! set up drawing environment
!!      call prefsize(600,600)
!!      call voutput('+')
!!      call vinit(' ') ! start graphics using device $M_DRAW_DEVICE
!!      call ortho2(-300.0,300.0,-300.0,300.0)
!!      call textsize(500.0,500.0)
!!      call linewidth(130)
!!      call centertext(.true.)
!!      call color(D_BLACK)
!!      call clear()
!!      write(*,*)'press any character to see it displayed in the default font'
!!      write(*,*)'Enter "q" to quit'
!!      do
!!        ichar=getkey()
!!        if(ichar.lt.0)then
!!           write(*,*)'this device does not support getkey'
!!           exit
!!        elseif(ichar.ne.0)then
!!           call color(D_BLACK)
!!           call clear()
!!           call color(D_BLUE)
!!           call move2(0.0,0.0)
!!           call drawstr(char(ichar))
!!        endif
!!        if(char(ichar).eq.'q')then
!!           write(*,*)'press any key to exit'
!!           ichar=getkey()
!!           exit
!!        endif
!!      enddo
!!      call vexit()
!!      end program demo_getkey
   integer function draw_getkey()
      if (vdevice%initialized == 0) call draw_verror("getkey: draw not initialized")
      draw_getkey = vdevice%dev%Vgetkey()
   end function draw_getkey

!>
!!##NAME
!!    checkkey(3f) - [M_draw:INTERACTIVE] Returns zero if no key is pressed or ASCII ordinal
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!          integer function checkkey()
!!
!!##DESCRIPTION
!!
!!    Returns zero if no key is pressed or the ASCII ordinal of the key
!!    that was pressed.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_checkkey
!!    use :: M_draw
!!    implicit none
!!    integer :: ichar
!!    !! set up drawing environment
!!    call prefsize(600,600)
!!    call vinit(' ') ! start graphics using device $M_DRAW_DEVICE
!!    call ortho2(-300.0,300.0,-300.0,300.0)
!!    call textsize(500.0,500.0)
!!    call linewidth(130)
!!    call centertext(.true.)
!!    call color(D_BLACK)
!!    call clear()
!!    write(*,*)'press any character to see it displayed in the default font'
!!    write(*,*)'Enter "q" to quit'
!!    do
!!      ichar=checkkey()
!!      if(ichar.lt.0)then
!!         write(*,*)'this device does not support checkkey'
!!         exit
!!      elseif(ichar.ne.0)then
!!         call color(D_BLACK)
!!         call clear()
!!         call color(D_BLUE)
!!         call move2(0.0,0.0)
!!         call drawstr(char(ichar))
!!      endif
!!      if(char(ichar).eq.'q')then
!!         write(*,*)'press any key to exit'
!!         ichar=getkey()
!!         exit
!!      endif
!!    enddo
!!    call vexit()
!!    end program demo_checkkey
   integer function draw_checkkey()
      if (vdevice%initialized == 0) call draw_verror("checkkey: draw not initialized")
      draw_checkkey = vdevice%dev%Vcheckkey()
   end function draw_checkkey

!>
!!##NAME
!!    locator(3f) - [M_draw:INTERACTIVE] Find out where cursor is
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!          integer function locator(xaddr, yaddr)
!!          real xaddr, yaddr
!!##DESCRIPTION
!!
!!    Find out where the cursor is. XADDR and YADDR are set to the current
!!    location in world coordinates. The function returns a bit pattern
!!    which indicates which buttons are being held down -- eg. if mouse
!!    buttons 1 and 3 are down locator returns binary 101 (decimal 7). The
!!    function returns -1 if the device has no locator capability. Note:
!!    if doing 3-D transformations XADDR and YADDR may not make a lot of
!!    sense. In that case use slocator.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!       program demo_locator !     track a cube with the locator
!!       use M_draw
!!       implicit none
!!       real :: trans, sc, tdir, scal, x, y
!!       integer :: idum, nplanes
!!       parameter(TRANS=20.0, SC=0.1)
!!       integer, parameter :: FACE=1, FILLED=2, OUTLINE=3
!!       character(len=10)  :: device
!!       character(len=1)   :: but
!!       logical            :: back, fill, hatch
!!       integer            :: ios
!!          write(*,*)'x,y,z = translate'
!!          write(*,*)'s     = apply scale'
!!          write(*,*)'+,-   = change scale/translate direction'
!!          write(*,*)'f,h   = fill, hatch'
!!          write(*,*)'b     = toggle backface'
!!          write(*,*)'q     = quit'
!!
!!          print*, 'Enter output device:'
!!          read(*, '(a)',iostat=ios) device
!!          if(ios.ne.0) device=' '
!!
!!          call prefposition(50, 50)
!!          call prefsize(500, 500)
!!
!!          call vinit(device)
!!
!!          call window(-800.0, 800.0, -800.0, 800.0, -800.0, 800.0)
!!          call lookat(0.0, 0.0, 1500.0, 0.0, 0.0, 0.0, 0.0)
!!
!!          tdir = TRANS
!!          scal = SC
!!
!!          !
!!          ! Start with a very ordinary filled cube like in the original demo...
!!          !
!!          call polyhatch(.false.)
!!          call hatchang(45.0)
!!          call hatchpitch(40.0)
!!          call polyfill(.true.)
!!
!!          fill = .true.
!!          hatch = .false.
!!          back = .true.
!!
!!          call makeobj(FACE)
!!          call makepoly()
!!          call rect(-200.0, -200.0, 200.0, 200.0)
!!          call closepoly()
!!          call closeobj()
!!
!!          call makecube(FILLED)
!!
!!          nplanes = getdepth()
!!          if (nplanes .eq. 1) call makecube(OUTLINE)
!!
!!          call backface(back)
!!          !
!!          ! Setup drawing into the backbuffer....
!!          !
!!          if (backbuffer().lt.0) then
!!             call vexit()
!!             write(*,*)'Device can''t support doublebuffering'
!!             stop
!!          endif
!!
!!          INFINITE: do
!!             idum = slocator(x, y)
!!             call pushmatrix()
!!             call rotate(100.0 * x, 'y')
!!             call rotate(100.0 * y, 'x')
!!             call color(D_BLACK)
!!             call clear()
!!             call callobj(FILLED)
!!             if (nplanes .eq. 1 .and. (fill .or. hatch)) call callobj(OUTLINE)
!!             call popmatrix()
!!             call swapbuffers()
!!
!!             but = char(checkkey())
!!             select case(but)
!!              case('x')
!!                call translate(tdir, 0.0, 0.0)
!!              case('y')
!!                call translate(0.0, tdir, 0.0)
!!              case('z')
!!                call translate(0.0, 0.0, tdir)
!!              case('s')
!!                call scale(scal, scal, scal)
!!              case('f')
!!                fill = .not. fill
!!                hatch = .false.
!!                call polyfill(fill)
!!              case('h')
!!                hatch = .not. hatch
!!                fill = .false.
!!                call polyhatch(hatch)
!!              case('b')
!!                back = .not. back
!!                call backface(back)
!!              case('-')
!!                tdir = -tdir
!!                if (scal .lt. 1.0) then
!!                   scal = 1.0 + SC
!!                else
!!                   scal = 1.0 - SC
!!                endif
!!              case('+')
!!                tdir = TRANS
!!              case('q',char(27))
!!                call vexit()
!!                stop
!!             end select
!!          enddo INFINITE
!!       contains
!!
!!       subroutine makecube(obj)
!!       integer obj
!!
!!          call makeobj(obj)
!!          if (obj .eq. OUTLINE) then
!!             call pushattributes()
!!             call color(D_BLACK)
!!             call polyfill(.false.)
!!             call polyhatch(.false.)
!!          endif
!!
!!          call pushmatrix()
!!          call translate(0.0, 0.0, 200.0)
!!          if (obj .eq. FILLED) call color(D_RED)
!!          call callobj(FACE)
!!          call popmatrix()
!!
!!          call pushmatrix()
!!          call translate(200.0, 0.0, 0.0)
!!          call rotate(90.0, 'y')
!!          if (obj .eq. FILLED) call color(D_GREEN)
!!          call callobj(FACE)
!!          call popmatrix()
!!
!!          call pushmatrix()
!!          call translate(0.0, 0.0, -200.0)
!!          call rotate(180.0, 'y')
!!          if (obj .eq. FILLED) call color(D_BLUE)
!!          call callobj(FACE)
!!          call popmatrix()
!!
!!          call pushmatrix()
!!          call translate(-200.0, 0.0, 0.0)
!!          call rotate(-90.0, 'y')
!!          if (obj .eq. FILLED) call color(D_CYAN)
!!          call callobj(FACE)
!!          call popmatrix()
!!
!!          call pushmatrix()
!!          call translate(0.0, 200.0, 0.0)
!!          call rotate(-90.0, 'x')
!!          if (obj .eq. FILLED) call color(D_MAGENTA)
!!          call callobj(FACE)
!!          call popmatrix()
!!
!!          call pushmatrix()
!!          call translate(0.0, -200.0, 0.0)
!!          call rotate(90.0, 'x')
!!          if (obj .eq. FILLED) call color(D_YELLOW)
!!          call callobj(FACE)
!!          call popmatrix()
!!
!!          if (obj .eq. OUTLINE) call popattributes()
!!
!!          call closeobj()
!!
!!       end subroutine makecube
!!
!!       end program demo_locator
   integer function draw_locator(wx, wy)
      real(real32), intent(out) :: wx, wy
      integer :: a, b, c
      if (vdevice%initialized == 0) call draw_verror("locator: draw not initialized")
      c = vdevice%dev%Vlocator(a, b)
      call draw_VtoWxy(real(a, real32), real(b, real32), wx, wy)
      draw_locator = c
   end function draw_locator

!>
!!##NAME
!!    slocator(3f) - [M_draw:INTERACTIVE] Find out where cursor is in screen coordinates
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!          integer function slocator(xaddr, yaddr)
!!          real xaddr, yaddr
!!##DESCRIPTION
!!
!!    Find out where the cursor is. xaddr and yaddr are set to the current
!!    location in screen coordinates. The return value of the function is
!!    set up in the same way as with locator. If the device has no locator
!!    device slocator returns -1.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!     !
!!     !      a routine to demonstrate using locator.
!!     !
!!     program demo_slocator
!!
!!     use M_draw
!!    implicit none
!!
!!     character(len=20) :: dev
!!     integer bt
!!     real x, y, sx, sy
!!     logical act, curpnt
!!     integer :: ios
!!
!!     print*,'Enter device name:'
!!     read(*,'(a)',iostat=ios) dev
!!     if(ios.ne.0)dev=' '
!!     call vinit(dev)
!!
!!     call color(D_BLACK)
!!     call clear()
!!
!!     call color(D_BLUE)
!!     !
!!     !       draw some axes
!!     !
!!     call move2(0.0, 1.0)
!!     call draw2(0.0, -1.0)
!!
!!     call move2(1.0, 0.0)
!!     call draw2(-1.0, 0.0)
!!
!!     call color(D_GREEN)
!!
!!     act = .false.
!!     curpnt = .false.
!!     !
!!     !       locator returns whether a mouse button has been
!!     !       pressed or not. In a device such as the tektronix
!!     !       where you have to wait for a keypress to get the
!!     !       position of the crosshairs locator returns 0
!!     !       automatically on every second call. A return value
!!     !       of 2 indicates the second mouse button has been pressed.
!!     !       A return value of 1 indicates the first mouse button has
!!     !       been pressed. We wait for the locator to return zero so
!!     !       that we know the mouse button has been released.
!!     !
!!     write(*,*)' click two points to create a line segment'
!!     write(*,*)' button 2 exits'
!!
!!     INFINITE: do
!!        bt = slocator(x, y)
!!        !! write(*,*)'slocator returned ',bt,' and coordinates ',x,y
!!        if (bt .eq. -1) then
!!           call vexit()
!!           print*,'No locator device found'
!!           stop
!!        elseif (bt .eq. 2) then
!!           call vexit()
!!           stop
!!        elseif (bt .eq. 0) then
!!           act = .true.
!!        elseif (act) then
!!           act = .false.
!!           if (bt .eq. 1) then
!!              if (curpnt) then
!!                 call move2(sx, sy)
!!                 call draw2(x, y)
!!                 curpnt = .false.
!!              else
!!                 curpnt = .true.
!!              endif
!!
!!              sx = x
!!              sy = y
!!           endif
!!        endif
!!     enddo INFINITE
!!
!!     end program demo_slocator
   integer function draw_slocator(wx, wy)
      real(real32), intent(out) :: wx, wy
      integer :: a, b, c
      if (vdevice%initialized == 0) call draw_verror("slocator: draw not initialized")
      c = vdevice%dev%Vlocator(a, b)
      wx = real(a, real32) / (0.5_real32 * real(vdevice%sizeX, real32)) - 1.0_real32
      wy = real(b, real32) / (0.5_real32 * real(vdevice%sizeY, real32)) - 1.0_real32
      draw_slocator = c
   end function draw_slocator

!>
!!##NAME
!!    clear(3f) - [M_draw:COLOR] Clears screen to current color
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine clear()
!!
!!##DESCRIPTION
!!
!!    Clears the screen to the current color. Causes a new page to begin
!!    on file-based devices that support multiple pages (Currently, the
!!    pixmap devices do not support multiple pages).
!!
!!##EXAMPLE
!!
!!  Sample program:
!!
!!      program demo_clear
!!      use M_draw, only  : prefsize, vinit, ortho2, clear, getkey
!!      use M_draw, only  : vexit, color, circle, polyfill
!!      use M_draw, only  : D_BLACK,   D_WHITE
!!      use M_draw, only  : D_RED,     D_GREEN,    D_BLUE
!!      use M_draw, only  : D_YELLOW,  D_MAGENTA,  D_CYAN
!!      implicit none
!!      integer :: ipaws
!!
!!      call prefsize(300,300)
!!      call vinit(' ') ! start graphics using device $M_DRAW_DEVICE
!!      ipaws=getkey()
!!      call ortho2(-100.0,100.0,-100.0,100.0)
!!
!!      call color(D_BLACK)               ! set current  color
!!      call clear()                ! clear background to current color
!!      call color(D_RED)               ! set color to draw with
!!      call circle(0.0,0.0,50.0)
!!      ipaws=getkey()              ! pause for a keypress on interactive devices
!!
!!      call color(D_GREEN)               ! make a second page
!!      call clear()
!!      call polyfill(.true.)
!!      call color(D_YELLOW)
!!      call circle(0.0,0.0,50.0)
!!      ipaws=getkey()
!!
!!      call vexit()
!!
!!      end program demo_clear
   subroutine draw_clear()
      type(Token), pointer :: tok(:)
      integer :: idum
      if (vdevice%initialized == 0) call draw_verror("clear: draw not initialized")
      if (vdevice%inobject /= 0) then
         tok => draw_newtokens(1); tok(1)%i = OBJ_CLEAR; return
      end if
      idum = vdevice%dev%Vclear()
   end subroutine draw_clear

!>
!!##NAME
!!    vexit(3f) - [M_draw:DEVICE] Reset window/terminal and exit graphics mode. Must be last routine called.
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!          subroutine vexit()
!!
!!##DESCRIPTION
!!
!!    Reset the window/terminal and terminate graphics mode. Required to
!!    properly close out most output devices. Must be called before any
!!    second call to vinit(3f). Must be the last M_draw routine called.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!      program demo_vexit
!!      use M_draw, only: vinit, vexit, voutput, circle, linewidth, color
!!      use M_draw,    only  : D_BLACK,   D_WHITE
!!      use M_draw,    only  : D_RED,     D_GREEN,    D_BLUE
!!      use M_draw,    only  : D_YELLOW,  D_MAGENTA,  D_CYAN
!!      implicit none
!!      integer :: i
!!
!!      ! assuming you have the NetPBM package installed
!!      ! set up output to create a GIF file called one.gif
!!      call voutput('|ppmtogif >vexit.3m_draw.gif')
!!
!!      call vinit('p6') ! start graphics
!!      ! default window is -1 <= x <= 1, -1 <= y <= 1
!!      ! default viewport is left bottom justified square
!!      ! so essentially you have a square drawing surface
!!      ! with the point <0,0> at the center of the square
!!      ! with X and Y ranging from -1 to 1.
!!
!!      call color(D_RED)
!!      call linewidth(100)
!!      ! this loop draws outside the current window
!!      ! but by default clipping is on
!!      do i=1,30
!!         call circle((real(i)-1)*0.04,0.0,1.0/real(i))
!!      enddo
!!
!!      call vexit() ! exited graphics so can start again
!!
!!      ! start second graphics session with new output
!!      ! device and output file
!!      call voutput('|ppmtogif >vexit2.3m_draw.gif')
!!      call vinit('p6')
!!
!!      do i=10,1,-1
!!         call color(i)
!!         call circle(0.0,0.0,1.0/real(i))
!!      enddo
!!
!!      call vexit()
!!
!!      end program demo_vexit
   subroutine draw_vexit()
      integer :: idum
      if (vdevice%initialized == 0) call draw_verror("vexit: draw not initialized")
      idum = vdevice%dev%Vexit()
      vdevice%initialized = 0
      draw_unit = output_unit
      badcount = 0
   end subroutine draw_vexit

!>
!!##NAME
!!    color(3f) - [M_draw:COLOR] Set current color
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!         subroutine color(col)
!!         integer,intent(in) :: col
!!
!!##DESCRIPTION
!!
!! Set the current color. The standard colors are as follows:
!!
!!       d_black  =  0  d_red      =  1  d_green  =  2  d_yellow  =  3
!!       d_blue   =  4  d_magenta  =  5  d_cyan   =  6  d_white   =  7
!!
!!##OPTION
!!     COL  A color number from 0 to 255. To define additional
!!          colors see mapcolor(3f).
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!     program demo_color
!!     use M_draw
!!     implicit none
!!     real    :: b=0.5
!!     real    :: y1,y2,ym,x1,x2
!!     real    :: width=50.0/8.0,width2
!!     integer :: i
!!     integer :: ipaws
!!        !! set up long bar as plotting area
!!        call prefsize(1000,200)
!!        call vinit(' ') ! start graphics using device $M_DRAW_DEVICE
!!        call page(-25.0-b, 25.0+b, -5.0-b, 5.0+b)
!!        call textsize( 3.5, 4.0)
!!        call font('futura.m')
!!        call centertext(.true.)
!!        call linewidth(90)
!!        y1=-5
!!        y2=5
!!        ym=0
!!        x1=-25+.05*width
!!        ! draw colored rectangle and a circle and label center of circle
!!        ! and repeat from colors 0 to 7.
!!        width2=width*0.95
!!        call linewidth(40)
!!        do i=0,7
!!           call color(i)
!!           x2=x1+width2
!!           call polyfill(.true.)
!!           call makepoly()
!!           call rect(x1,y1,x2,y2)
!!           call closepoly()
!!           call color(mod(i+1,7)+1)
!!           call move2((x1+x2)/2.0,ym)
!!           call print(i)     ! convert number to string and draw it
!!           call polyfill(.false.)
!!           call circle((x1+x2)/2.0, ym, (x2-x1)/2.10)
!!           x1=x1+width
!!        enddo
!!        ipaws=getkey()
!!        call vexit()
!!     end program demo_color
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

!>
!!##NAME
!!    mapcolor(3f) - [M_draw:COLOR] set a color index using RGB values
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!         subroutine mapcolor(indx, red, green, blue)
!!         integer,intent(in) :: indx, red, green, blue
!!
!!##DESCRIPTION
!!
!!    Set the color map index indx to the color represented by (red, green,
!!    blue). If the device has no color map this call does nothing.
!!
!!    rgb values are in the range of 0 to 255.
!!
!!##OPTIONS
!!    INDX    color index number, in range 0 to 255
!!    RED     red component of color being defined, in range 0 to 255
!!    GREEN   green component of color being defined, in range 0 to 255
!!    BLUE    blue component of color being defined, in range 0 to 255
!!
!!##EXAMPLE
!!
!!  Color wheel EXAMPLE:
!!
!!    program demo_mapcolor
!!    !   good program to exercise color tables, and look at differences
!!    !   when actual output device has a color table that is dynamic,
!!    !   or only has a small color table (a frame in this program takes
!!    !   at least SLICES*RINGS colors to produce accurately).
!!    use M_draw
!!    implicit none
!!    real                 :: lightstep
!!    integer              :: ii, iframe
!!    integer, parameter    :: SLICES = 30
!!    integer, parameter    :: RINGS = 8
!!    real                 :: LIGHTNESS
!!    integer, parameter    :: BOX = 500
!!    integer              :: ipaws
!!    integer              :: istart, iend
!!    character(len=20)    :: device
!!       call prefsize(BOX, BOX)
!!       call vinit(' ') ! start graphics using device $M_DRAW_DEVICE
!!       call polyfill(.true.)
!!       call color(D_BLACK)
!!       call clear()
!!       call color(D_WHITE)
!!       call page(-110.0/2.0+5, 85.0/2.0+5, -110.0/2.0, 110.0/2.0)
!!       LIGHTNESS = 100.0
!!       lightstep = -5
!!
!!       call vgetdev(device)
!!       select case (device)
!!       case ('p6', 'p3', 'ppm') ! just do one wheel
!!          istart = 10
!!          iend = 10
!!          LIGHTNESS = 50.0
!!       case default           ! do many lightnesses
!!          istart = 1
!!          iend = 19
!!       end select
!!
!!       do ii = istart, iend
!!          iframe = ii
!!          call color(D_BLACK)
!!          call clear()
!!          call color(D_WHITE)
!!          call wheel()
!!          LIGHTNESS = LIGHTNESS + LIGHTSTEP
!!          ipaws = getkey()
!!       end do
!!       call vexit()
!!    contains
!!    !
!!    subroutine wheel() ! draw an entire wheel
!!    character(len=40) :: inline
!!    real              :: hue_val
!!    integer           :: ii
!!       call textang(0.0)
!!       call color(D_WHITE)
!!       call textsize(5.0, 6.0)
!!       call font('times.r')
!!       call move2(0.0, 103.0/2.0)
!!       call centertext(.true.)
!!       call linewidth(30)
!!       call drawstr('COLOR WHEEL')
!!       call linewidth(0)
!!       call textsize(2.5, 2.5)
!!       call font('futura.l')
!!       call move2(0.0, 90.0/2.0)
!!       write (inline, '("lightness=",f6.2)') LIGHTNESS
!!       call linewidth(30)
!!       call drawstr(inline)
!!       call linewidth(0)
!!       call textsize(1.5, 1.5)
!!       hue_val = 0
!!       do ii = SLICES, 1, -1
!!          call slice(hue_val)
!!       end do
!!       call centertext(.false.)
!!    end subroutine wheel
!!    !
!!    subroutine slice(hue_val) ! draw a slice
!!    integer           :: buffer
!!    real              :: hue_val, ang_inc
!!    character(len=40) :: inline
!!    real              :: step
!!    real              :: X1, X2, X3, X4
!!    real              :: Y1, Y2, Y3, Y4
!!    !
!!    integer           :: maxcolors, current_color
!!    integer           :: ir, ig, ib
!!    real              :: r, g, b
!!    real              :: saturation
!!    !
!!    integer           :: status
!!    integer           :: icount
!!    real              :: angle1, angle2
!!    real              :: radius1, radius2, radius3, radius4
!!    !
!!    integer, save      :: color_count = 0
!!       !
!!       buffer = 8
!!       ANG_INC = 360.0/SLICES
!!       angle1 = hue_val - ANG_INC/2
!!       angle2 = angle1 + ANG_INC
!!       saturation = 100
!!       radius1 = 32
!!       radius3 = radius1 + 4
!!       radius4 = radius1 + 7
!!       ! draw tic from wheel to start of angle label
!!       call color(D_WHITE)
!!       call linewidth(40)
!!       call move2(radius1*cosd(hue_val), radius1*sind(hue_val))
!!       call draw2(radius3*cosd(hue_val), radius3*sind(hue_val))
!!       ! draw degree label at tic
!!       call textang(hue_val)
!!       call move2(radius4*cosd(hue_val), radius4*sind(hue_val))
!!       write (inline, '(i0)') nint(hue_val)
!!       call linewidth(20)
!!       call drawstr(inline)
!!       call linewidth(0)
!!       step = radius1/real(RINGS)
!!       radius2 = radius1 - step
!!       ! draw a chunk in a slice
!!       MAXCOLORS = (256) - buffer
!!       do icount = RINGS + 1, 2, -1
!!          ! add buffer to leave base colors alone
!!          CURRENT_COLOR = MOD(color_count, MAXCOLORS) + buffer
!!          color_count = color_count + 1
!!          ! fancy mapcolor
!!          call hlsrgb(hue_val, LIGHTNESS, saturation, r, g, b, status)
!!          ir = int(r*255.0/100.0 + 0.50)
!!          ig = int(g*255.0/100.0 + 0.50)
!!          ib = int(b*255.0/100.0 + 0.50)
!!          call mapcolor(CURRENT_COLOR, ir, ig, ib)
!!          call color(CURRENT_COLOR)
!!          !
!!          X1 = cosd(angle1)*radius2
!!          Y1 = sind(angle1)*radius2
!!          X2 = cosd(angle1)*radius1
!!          Y2 = sind(angle1)*radius1
!!          !
!!          X3 = cosd(angle2)*radius2
!!          Y3 = sind(angle2)*radius2
!!          X4 = cosd(angle2)*radius1
!!          Y4 = sind(angle2)*radius1
!!          !
!!          call makepoly()
!!          call move2(X1, Y1)
!!          call draw2(X2, Y2)
!!          call draw2(X4, Y4)
!!          call draw2(X3, Y3)
!!          call closepoly()
!!          !
!!          saturation = saturation - 100.0/RINGS
!!          radius1 = radius2
!!          radius2 = radius1 - step
!!       end do
!!       hue_val = hue_val + ANG_INC
!!    end subroutine slice
!!    !
!!    subroutine hlsrgb(H, L, S, R, G, B, status)
!!    ! convert HLS(hue,lightness,saturation) values to RGB components
!!    !     given  : hue as a value of 0 to 360 degrees.
!!    !     .        lightness and saturation each as a value of 0 to 100.
!!    !     desired: r, g, and b each as a value of 0 to 100.
!!    !
!!    real, intent(in)   :: H, L, S
!!    real, intent(out)  :: R, G, B
!!    integer           :: status
!!    real              :: hue, lightness, saturation
!!    real              :: clr1, clr2
!!       ! passively report on bad input values
!!       if (h < 0.0 .or. h > 360.0) status = 1
!!       if (l < 0.0 .or. l > 100.0) status = 1
!!       if (s < 0.0 .or. s > 100.0) status = 1
!!       hue = H
!!       lightness = L/100.0
!!       saturation = S/100.0
!!       if (saturation == 0.0) then
!!          R = lightness
!!          G = lightness
!!          B = lightness
!!       end if
!!       if (lightness <= 0.50) then
!!          clr2 = lightness*(1.0 + saturation)
!!       else
!!          clr2 = lightness + saturation - lightness*saturation
!!       end if
!!       clr1 = 2.0*lightness - clr2
!!       R = rgbval(clr1, clr2, hue + 120.0)*100.0
!!       G = rgbval(clr1, clr2, hue)*100.0
!!       B = rgbval(clr1, clr2, hue - 120.0)*100.0
!!    end subroutine hlsrgb
!!    real function rgbval(clr1,clr2,h)
!!    ! rgbval(3fp): ensure a value is in the appropriate range and quadrant
!!    real    :: clr1,clr2
!!    real    :: h
!!    real    :: h2
!!       h2=h
!!       do
!!          if(h2 > 360.0 ) then
!!             h2=h2-360.0
!!             cycle
!!          endif
!!          exit
!!       enddo
!!       do
!!          if( h2  <  0.0 ) then
!!             h2=h2+360.0
!!             cycle
!!          endif
!!          exit
!!       enddo
!!       if(h2 < 60.0 ) then
!!          rgbval=clr1+(clr2-clr1)*h2/60.0
!!       else if(h2 < 180.0) then
!!          rgbval=clr2
!!       else if(h2 < 240.0) then
!!          rgbval=clr1+(clr2-clr1)*(240.0-h2)/60.0
!!       else
!!          rgbval=clr1
!!       endif
!!    end function rgbval
!!    end program demo_mapcolor
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

!>
!!##NAME
!!    getdepth(3f) - [M_draw:DEVICE] Return number of bit planes (color planes)
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!          integer function getdepth()
!!
!!##DESCRIPTION
!!    Returns the number of bit planes (or color planes) for a particular
!!    device. The number of colors displayable by the device is then
!!    2**(nplanes); ie. if nplanes=1,then there are two colors (black and
!!    white).
   integer function draw_getdepth()
      if (vdevice%initialized == 0) call draw_verror("getdepth: draw not initialized")
      draw_getdepth = vdevice%depth
   end function draw_getdepth

!>
!!##NAME
!!    vsetflush(3f) - [M_draw:FLUSHING] Set global flushing status
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!          subroutine vsetflush(yesno)
!!          logical yesno
!!
!!##DESCRIPTION
!!    Set global flushing status. If yesno = 0 (.false.) then don't do any
!!    flushing (except in swapbuffers(), or vflush()). If yesno = 1
!!    (.true.) then do the flushing as described above.
   subroutine draw_vsetflush_logical(yn)
      logical, intent(in) :: yn
      vdevice%sync = merge(1,0,yn)
   end subroutine draw_vsetflush_logical

!>
!!##NAME
!!    vflush(3f) - [M_draw:FLUSHING] Call device flush or syncronisation routine
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!          subroutine vflush()
!!
!!##DESCRIPTION
!!
!!    Call the device flush or syncronisation routine. This forces a flush.
   subroutine draw_vflush()
      type(Token), pointer :: tok(:)
      integer :: idum
      if (vdevice%initialized == 0) call draw_verror("vflush: draw not initialized")
      if (vdevice%inobject /= 0) then
         tok => draw_newtokens(1); tok(1)%i = OBJ_VFLUSH; return
      end if
      idum = vdevice%dev%Vsync()
   end subroutine draw_vflush

   subroutine draw_vsetflush_integer(yn)
      integer, intent(in) :: yn
      vdevice%sync = yn
   end subroutine draw_vsetflush_integer

!>
!!##NAME
!!    linewidth(3f) - [M_draw:LINESTYLE] set line width in rasters
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!         subroutine linewidth(iwidth)
!!         integer iwidth
!!##DESCRIPTION
!!
!!    Set the current line width in units of 1/10,000 of the X size of the
!!    display surface
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_linewidth
!!    use M_draw,     only : prefsize, vinit, clear, getkey, drawstr, page
!!    use M_draw,     only : textsize, ycentertext, rdraw2, rmove2
!!    use M_draw,     only : move2, draw2, vexit, color, linewidth, font
!!    use M_draw,     only : D_BLACK,   D_WHITE
!!    use M_draw,     only : D_RED,     D_GREEN,    D_BLUE
!!    use M_draw,     only : D_YELLOW,  D_MAGENTA,  D_CYAN
!!    implicit none
!!    real    :: b=0.5
!!    integer :: ipaws
!!    call prefsize(1000,200)
!!    call vinit(' ') ! start graphics using device $M_DRAW_DEVICE
!!    call page(-25.0-b, 25.0+b, -5.0-b, 5.0+b)
!!    call color(D_YELLOW)
!!    call clear()
!!    call textsize(2.4,3.0)
!!    call font("futura.m")
!!    call ycentertext()
!!    ! draw circles with different precisions and label center of circles
!!    call color(D_WHITE)
!!    call linewidth(0)
!!    call move2(-20.0, -3.5)
!!    call rdraw2(10.0,0.0)
!!    call rmove2(3.0,0.0)
!!    call linewidth(180)
!!    call color(D_WHITE)
!!    call drawstr("0")
!!    call color(D_RED)
!!    call linewidth(100)
!!    call move2(-20.0,0.0)
!!    call rdraw2(10.0,0.0)
!!    call rmove2(3.0,0.0)
!!    call linewidth(180)
!!    call color(D_WHITE)
!!    call drawstr("100")
!!    call color(D_GREEN)
!!    call linewidth(200)
!!    call move2(-20.0,3.5)
!!    call rdraw2(10.0,0.0)
!!    call rmove2(3.0,0.0)
!!    call linewidth(180)
!!    call color(D_WHITE)
!!    call drawstr("200")
!!    call color(D_BLUE)
!!    call linewidth(300)
!!    call move2(3.0,-3.5)
!!    call rdraw2(10.0,0.0)
!!    call rmove2(3.0,0.0)
!!    call linewidth(180)
!!    call color(D_WHITE)
!!    call drawstr("300")
!!    call color(D_MAGENTA)
!!    call linewidth(400)
!!    call move2(3.0,0.0)
!!    call rdraw2(10.0,0.0)
!!    call rmove2(3.0,0.0)
!!    call linewidth(180)
!!    call color(D_WHITE)
!!    call drawstr("400")
!!    call color(D_CYAN)
!!    call linewidth(500)
!!    call move2(3.0,3.5)
!!    call rdraw2(10.0,0.0)
!!    call rmove2(3.0,0.0)
!!    call linewidth(180)
!!    call color(D_WHITE)
!!    call drawstr("500")
!!    call linewidth(200)
!!    call color(D_BLUE)
!!    call move2(-25.0,-5.0)
!!    call draw2(-25.0,5.0)
!!    call draw2(25.0,5.0)
!!    call draw2(25.0,-5.0)
!!    call draw2(-25.0,-5.0)
!!    ipaws=getkey()
!!    ! exit graphics mode
!!    call vexit()
!!    end program demo_linewidth
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
