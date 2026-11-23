!>
!!##NAME
!!    M_draw(3fm) - [M_draw::INTRO] The M_draw graphics library
!!    (LICENSE:PD)
!!
!!##DESCRIPTION
!!
!!    M_draw is a portable public-domain device-independent graphics library
!!    intended for being called from Fortran that is based on VOGLE (from
!!    the University of Melbourne) that is loosely based on the Silicon
!!    Graphics Iris GL library. It was also partly inspired by the DIGS
!!    library developed at the U.S. Naval Academy under the guidance of
!!    Prof David Rogers.
!!
!!    Many output devices are available:
!!
!!      * FrameMaker MIF 3.0 (Maker Interchange File) driver.
!!      * Adobe PDF driver.
!!      * HTML5 Canvas driver.
!!      * SVG driver.
!!      * A PCL5/HPGL2 driver that supports prefsize() calls.
!!      * Monochrome PBM (Poskanzer bitmap P1 and P4 formats) and X11
!!        bitmap driver.
!!      * Color PBM (Poskanzer pixmap P3 and P6 formats). If you have the
!!        pbmplus package you can use it to make M_draw appear to write
!!        any format pbmplus writes (assuming your system supports the
!!        popen(3c) function).
!!      * A clear-text CGM (Computer Graphics Metafile) driver.
!!      * A different (color) PostScript driver.
!!      * A driver for Microsoft VML (Vector Markup Language)
!!
!!    M_draw is intended to produce simple graphics composed of line
!!    drawings and polygon fills in two and three dimensions. It handles
!!    circles, curves, arcs, patches, polygons, and software text in a device
!!    independent fashion. Simple hidden line removal is also available via
!!    polygon backfacing. Access to hardware text and double buffering of
!!    drawings depends on the driver.
!!
!!    M_draw is callable from C and Fortran, and Pascal; but M_draw is only
!!    supported in Fortran (the C components are being converted to Fortran).
!!
!!    The original VOGLE source's ownership statement
!!
!!       This software is public domain and may be used for any purpose
!!       commercial or otherwise. It is offered without any guarantee
!!       as to its suitability for any purpose or as to the sanity of
!!       its writers. The authors do ask that the source is passed on to
!!       anyone that requests a copy, and that people who get copies don't
!!       go round claiming they wrote it. Use at your own risk.
!!
!!##LIBRARY FUNCTION DESCRIPTIONS
!!
!!    DEVICE ROUTINES
!!    vinit(device)                    Initialise device
!!    vexit()                          Reset window/terminal (must be last routine called)
!!    voutput(path)                    Redirect output from *next* vinit to file
!!    vnewdev(device)                  Reinitialize to use new device without changing
!!    vgetdev(device)                  Get name of current device
!!    pushdev(device)                  push current device onto a stack
!!    popdev(device)                   pop device from stack created by pushdev.
!!    getdepth()                       Return number of bit planes (color planes)
!!
!!    ROUTINES FOR SETTING UP WINDOWS
!!    prefposition(x, y)               Specify preferred position of window
!!    prefsize(width, height)          Specify preferred width and height of window
!!
!!    Some devices are basically window oriented - like sunview and X11. You
!!    can give M_draw some information on the window that it will use with these
!!    routines. These can make your code very device independent. Both routines
!!    take arguments which are in device space. (0, 0) is the top left hand
!!    corner in device space. To have any effect these routines must be called
!!    before vinit. For the X11 device, an entry may be made in your .Xdefaults
!!    file or loaded in with the xrdb(1) command:
!!
!!       xrdb <<\end_of_file
!!       ! X11 Windows fonts to use for "small" and "large" fonts
!!       M_draw*smallfont: fixed
!!       M_draw*largefont: 9x15
!!       ! title on decoration bar for the window
!!       M_draw*title: My M_draw program
!!       ! window geometry and position,
!!       ! overridden by prefsize(3c) and prefposition(3c)
!!       M_draw.Geometry: =500x500-10+20
!!       end_of_file
!!
!!    (where you specify your geometry as you please).
!!
!!    CLIPPING ROUTINES
!!    clipping(onoff)                  Turn clipping on or off
!!
!!    COLOR ROUTINES
!!    clear()                          Clears screen to current color
!!    color(col)                       Set current color
!!    mapcolor(indx, red, green, blue) Set color map index
!!
!!    INTERACTIVE ROUTINES
!!    getkey()                         Return ASCII ordinal of next key typed
!!    checkkey()                       Returns zero if no key is pressed or ASCII ordinal
!!    getstring(bcol, string)          Read in a string, echoing it in current font
!!    locator(xaddr, yaddr)            Find out where cursor is
!!    slocator(xaddr, yaddr)           Find out where cursor is in screen coordinates
!!
!!    FLUSHING
!!    vsetflush(yesno)                 Set global flushing status
!!    vflush()                         Call device flush or syncronisation routine
!!
!!    On some devices (particularly X11) considerable speedups in display
!!    can be achieved by not flushing each graphics primitive call to the
!!    actual display until necessary. M_draw automatically delays flushing in
!!    the following cases:
!!
!!      * Within a callobj() call.
!!      * Within curves and patches.
!!      * Within Hershey software text.
!!      * When double buffering (the flush is only done within swapbuffers).
!!
!!    There are two user routines that can be used to control flushing.
!!
!!    VIEWPORT ROUTINES
!!    viewport(left, right, bottom, top)     Specify which part of screen to draw in
!!    pushviewport()                         Save current viewport
!!    popviewport()                          Retrieve last viewport
!!    getviewport(left, right, bottom,top)   Returns limits of current viewport in screen coordinates
!!    expandviewport()                       use the entire device viewport
!!    unexpandviewport()                     undo expandviewport(3f)
!!
!!    Viewpoint routines alter the current transformation matrix.
!!
!!    GETTING THE ASPECT DETAILS
!!    getaspect()                      Returns the ratio height over width of the display device.
!!    getfactors(wfact, hfact)         Returns width over min(width of device, height of device) and height over min(width of
!!                                     device, height of device).
!!    getdisplaysize(w, h)             Returns width and height of device in device units
!!
!!    Often the screen is not perfectly square and it would be nice to use
!!    the extra space without having to turn clipping off. The following
!!    routines are provided to get the values needed to adjust the calls
!!    to viewport, etc as needed.
!!
!!    ATTRIBUTE STACK ROUTINES
!!    pushattributes()                 Save the current attributes on the attribute stack.
!!    popattributes()                  Restore attributes to what they were at last pushattributes().
!!
!!    The attribute stack contains details such as current color, filling,
!!    hatching, centered, fixedwidth, text height, text width, and the
!!    current font. If you need to prevent object calls from changing these,
!!    use pushattributes before the call and popattributes after.
!!
!!    PROJECTION ROUTINES
!!    ortho(left, right, bottom, top,near,far)    Define x,y,z clipping planes.
!!    ortho2(left, right, bottom, top)            Define x and y clipping planes.
!!    perspective(fov, aspect, near, far)         Specify perspective viewing pyramid
!!    window(left, right, bot, top, near,far)     Specify a perspective viewing pyramid
!!
!!    PROJECTION AND VIEWPORT ROUTINES
!!    subroutine page (left, right, bottom, top)  create a window of the specified size and then
!!                                                set the viewport to the largest viewport with
!!                                                that aspect so that output acts much like a
!!                                                page of paper of the specified size without
!!                                                distortion.
!!
!!    All the projection routines define a new transformation matrix, and
!!    consequently the world units. Parallel projections are defined by
!!    ortho or ortho2. Perspective projections can be defined by perspective
!!    and window.
!!
!!    MATRIX STACK ROUTINES
!!    pushmatrix()                     Save the current transformation matrix on the matrix stack.
!!    popmatrix()                      Reinstall the last matrix pushed
!!
!!    VIEWPOINT ROUTINES
!!    polarview(dist, azim, inc, twist)      Specify the viewer's position in polar coordinates
!!    up(x, y, z)                            Specify the world up.
!!    lookat(vx, vy, vz, px, py, pz,twist)   Specify the viewer's position
!!
!!    MOVE ROUTINES
!!    move(x, y, z)                    Move current graphics position to (x, y, z)
!!    rmove(deltax, deltay, deltaz)    Relative move
!!    move2(x, y)                      Move graphics position to point (x, y)
!!    rmove2(deltax, deltay)           Relative move in world units.
!!    smove2(x, y)                     Move current graphics position in screen coordinates (-1.0 to 1.0).
!!    rsmove2(deltax, deltay)          Relative move in screen units (-1.0 to 1.0).
!!
!!    LINESTYLE ROUTINES
!!    linewidth()                      set line width in rasters
!!    dashcode()                       set dash pattern length
!!    linestyle()                      set the line dash pattern
!!
!!    Linestyles are specified by giving a nominal length of a single
!!    dash and a character string consisting of 1's and 0's (zeros) that
!!    specify when to draw a dash and when not to draw a dash. Linestyles
!!    will follow curves and "go around" corners. If a linestyle is set or
!!    reset, the accumulated information as to where on a curve (or line)
!!    a dash is to be draw is also reset.
!!
!!    For EXAMPLE, with a nominal view of -1 to 1, setting the dash length
!!    to 0.5, and the linestyle to '11010' would draw a line(or curve) with
!!    a 1.0 unit solid part, followed by a 0.5 unit blank part followed by
!!    a 0.5 unit solid part followed by a 0.5 unit blank part. The linestyle
!!    would then repeat itself.
!!
!!    The dash sizes are affected by the current viewport/transformation
!!    scaling factors, meaning that in perspective, the dashes look smaller
!!    the farther away they are.
!!
!!    DRAW ROUTINES
!!    draw(x, y, z)                    Draw from current graphics position to (x, y, z)
!!    rdraw(deltax, deltay, deltaz)    Relative draw
!!    draw2(x, y)                      Draw from current graphics position to point (x, y)
!!    rdraw2(deltax,deltay)            Relative draw
!!    sdraw2(x, y)                     Draw in screen coordinates (-1.0 to 1.0).
!!    rsdraw2(deltax, deltay)          Relative draw in screen units (-1.0 to 1.0).
!!
!!    ARCS AND CIRCLES
!!    circleprecision(nsegs)                  Set number of line segments in a circle. Default is 32.
!!    arc(x, y, radius, startang, endang)     Draw an arc in world units.
!!    sector(x, y, radius, startang,endang)   Draw a sector. Note: sectors are polygons.
!!    circle(x, y, radius)                    Draw a circle. Note: circles are polygons.
!!
!!    When creating arcs and sectors note that angles are
!!    measured in degrees; where zero(0) is the positive X axis in a
!!    right-handed Cartesian coordinate system and positive angles sweep
!!    counterclockwise. If filling sectors or circles (As described in the
!!    section on polygons) hatch pitch is measured in world coordinates
!!    and is initially set to 0.1. The initial hatch angle is zero(0).
!!
!!    CURVE ROUTINES
!!    curvebasis(basis)           Define a basis matrix for a curve.
!!    curveprecision(nsegs)       Define number of line segments used to draw a curve.
!!    rcurve(geom)                Draw a rational curve.
!!    curve(geom)                 Draw a curve.
!!    curven(n, geom)             Draw n - 3 overlapping curve segments. Note: n must be at least 4.
!!
!!    RECTANGLES AND GENERAL POLYGON ROUTINES
!!    rect(x1, y1, x2, y2)        Draw a rectangle.
!!    polyfill(onoff)             Set the polygon fill flag
!!    polyhatch(onoff)            Set the polygon hatch flag
!!    hatchang(angle)             Set the angle of the hatch lines.
!!    hatchpitch(pitch)           Set the distance between hatch lines.
!!    poly2(n, points)            Construct an (x, y) polygon from an array of points
!!    poly(n, points)             Construct a polygon from an array of points
!!    makepoly()                  opens polygon constructed by a series of move-draws and closed by closepoly
!!    closepoly()                 Terminates a polygon opened by makepoly.
!!    backface(onoff)             Turns on culling of backfacing polygons.
!!    backfacedir(clockwise)      Sets backfacing direction to clockwise or anti-clockwise
!!
!!    A polygon is composed of a number of coplanar line segments connected
!!    end to end to form a closed shape.
!!
!!    In M_draw curves are estimated by a series of line segments, and thus
!!    may be included easily into polygons.
!!
!!    Regular    A polygon with all sides and interior angles the same. Regular
!!               polygons are always convex. See Regular Polygons
!!    Irregular  Each side may a different length, each angle may be a different
!!               measure. The opposite of a regular polygon. See Irregular Polygons
!!    Convex     All interior angles less than 180 ,and all vertices 'point
!!               outwards' away from the interior. The opposite of concave. Regular
!!               polygons are always convex. See Convex Polygons
!!    Concave    One or more interior angles greater than 180 . Some vertices
!!               push 'inwards' towards the interior of the polygon. The opposite
!!               of convex.
!!    Self-intersecting or Crossed  A polygon where one or more sides crosses back over another side,
!!                                  creating multiple smaller polygons. Most of the properties and
!!                                  theorems concerning polygons do not apply to this shape. It is
!!                                  best considered as several separate polygons. A polygon that in
!!                                  not self-intersecting in this way is called a simple polygon.
!!
!!    TEXT ROUTINES
!!    font(fontname)                   set the current font
!!    numchars()                       return number of characters in the current SOFTWARE font.
!!    textsize(width, height)          set maximum size of a character in the current SOFTWARE font.
!!    textang(ang)                     set the SOFTWARE text angle.
!!    fixedwidth(onoff)                turns fixedwidth mode on or off for SOFTWARE fonts.
!!    getcharsize(c, width, height)    get the width and height of a character.
!!    getfontdec()                     return size of maximum font descender
!!    getfontsize(width, height)       get maximum width and height of a character in a font.
!!    drawchar(c)                      draw the character c and update current position.
!!    drawstr(str)                     draw the text in string at the current position.
!!    strlength(str)                   return the length of the string s
!!    boxtext(x, y, l, h, s)           stretch and draw the SOFTWARE string s so that it fits in the imaginary box
!!    boxfit(x, y, l, h, s)            resize the SOFTWARE text size so it fits in a box
!!
!!    textjustify(val)                 general text justification
!!
!!    leftjustify()                    left justify text
!!    rightjustify()                   right justify text
!!    topjustify()                     top justify text
!!    bottomjustify()                  bottom justify text
!!
!!    centertext(onoff)                turns centertext mode on or off for SOFTWARE fonts.
!!    xcentertext()                    center text in the X direction
!!    ycentertext()                    center text in the Y direction
!!
!!    textslant()                      defines the obliqueness of the fonts.
!!    textweight()                     defines the weight of the fonts.
!!
!!    M_draw supports hardware and software fonts. The software fonts are based
!!    on the character set digitized by Dr Allen V. Hershey while working at
!!    the U. S. National Bureau of Standards. Exactly what hardware fonts are
!!    supported depends on the device, but it is guaranteed that the names
!!    "large" and "small" will result in something readable. For X11 displays
!!    the default large and small fonts used by the program can be overridden
!!    by placing the following defaults in the ~/.Xdefaults file:
!!
!!      draw.smallfont: X11-font-name
!!      draw.largefont: X11-font-name
!!
!!    It is noted here that hardware text is always assumed to be drawn
!!    parallel to the (x, y) plane, using whatever the current z coordinate
!!    is. The following software fonts are supported:
!!
!!       astrology       cursive         cyrillic        futura.l
!!       futura.m        gothic.eng      gothic.ger      gothic.ita
!!       greek           markers         math.low        math.upp
!!       meteorology     music           script          symbolic
!!       times.g         times.i         times.ib        times.r
!!       times.rb        japanese
!!
!!    A markers font "markers" is also provided for doing markers - you need
!!    to have centertext mode on for this to give sensible results when placing
!!    the markers.
!!
!!    If the environment variable "M_DRAW_FONTPATH" is set M_draw looks for the software
!!    fonts in the directory given by this value.
!!
!!    the default font is futura.l
!!
!!    TRANSFORMATION ROUTINES
!!    translate(x, y, z)          Set up a translation.
!!    scale(x, y, z)              Set up scaling factors in x, y, and z axis.
!!    rotate(angle, axis)         Set up a rotation in axis axis where axis is one of 'x','y', or 'z'.
!!
!!    All transformations are cumulative, so if you rotate something and
!!    then do a translate you are translating relative to the rotated
!!    axes. If you need to preserve the current transformation matrix use
!!    pushmatrix(), do the drawing, and then call popmatrix() to get back
!!    where you were before.
!!
!!    When doing transformations, ensure your objects remain in the viewing
!!    volume or they will be clipped. See routines such as ortho(3) for
!!    more information.
!!
!!    PATCH ROUTINES
!!    patchbasis(tbasis, ubasis)  Define the t and u basis matrices of a patch.
!!    patchprecision(tseg, useg)  Set minimum number of line segments making up curves in a patch.
!!    patchcurves(nt, nu)         Set the number of curves making up a patch.
!!    rpatch(gx, gy, gz, gw)      Draws a rational patch in the current basis, according to the geometry matrices gx, gy, gz, and gw.
!!    patch(gx, gy, gz)           Draws a patch in the current basis, according to the geometry matrices gx, gy, and gz.
!!
!!    POINT ROUTINES
!!    point(x, y, z)              Draw a point at x, y, z
!!    point2(x, y)                Draw a point at x, y.
!!
!!    points are drawn with the current color and linewidth. Points are
!!    currently device-specific and may appear as circles, squares, or
!!    not at all; as they are generated by a zero-length vector using the
!!    hardware line style.
!!
!!    OBJECT ROUTINES
!!    makeobj(n)                  Commence the object number n.
!!    closeobj()                  Close the current object.
!!    genobj()                    Returns a unique object identifier.
!!    getopenobj()                Return the number of the current object.
!!    callobj(n)                  Draw object number n.
!!    isobj(n)                    Returns non-zero if there is an object of number n.
!!    delobj(n)                   Delete the object number n.
!!    loadobj(n, filename)        Load the object in the file filename as object number n.
!!    saveobj(n, filename)        Save object number n into file filename. Does NOT save objects called inside object n.
!!
!!    invokeobj(xt,yt,zt,xs,ys,zs,xr,yr,zr,iobject)  push environment and do a transformation and then pop environment
!!
!!    Objects are graphical entities created by the drawing routines called
!!    between makeobj and closeobj. Objects may be called from within other
!!    objects. When an object is created most of the calculations required
!!    by the drawing routines called within it are done up to where the
!!    calculations involve the current transformation matrix. So if you need to
!!    draw the same thing several times on the screen but in different places
!!    it is faster to use objects than to call the appropriate drawing routines
!!    each time. Objects also have the advantage of being savable to a file,
!!    from where they can be reloaded for later reuse. Routines which draw
!!    or move in screen coordinates, or change device, cannot be included
!!    in objects.
!!
!!    DOUBLE BUFFERING
!!    backbuffer()                Draw in the backbuffer. Returns -1 if the device is not up to it.
!!    frontbuffer()               Draw in the front buffer. This will always work.
!!    swapbuffers()               Swap the front and back buffers.
!!
!!    Where possible M_draw allows for front and back buffers to enable
!!    things like animation and smooth updating of the screen. The routine
!!    backbuffer is used to initialise double buffering.
!!
!!    POSITION ROUTINES
!!    getgp(x, y, z)              Gets the current graphics position
!!    getgpt(x, y, z, w)          Gets the current transformed graphics position in world coords.
!!    getgp2(x, y)                Gets the current graphics position
!!    sgetgp2(x, y)               Gets the current screen graphics position in screen coords (-1 to 1)
!!
!!    GENERAL STACK ROUTINES
!!    subroutine pop()            pop
!!    subroutine push()           push
!!
!!    CONSTANTS
!!
!!     D_XCENTERED=   1
!!     D_YCENTERED=   2
!!     D_LEFT=        4 ! The default
!!     D_RIGHT=       8
!!     D_TOP=        16
!!     D_BOTTOM=     32 ! The default
!!     
!!     D_NORMAL=      0 ! The default
!!     D_BOLD=        1
!!     
!!     D_THICK=       1
!!     D_THIN=        0 ! The default
!!     
!!     D_BLACK        0
!!     D_RED          1
!!     D_GREEN        2
!!     D_YELLOW       3
!!     D_BLUE         4
!!     D_MAGENTA      5
!!     D_CYAN         6
!!     D_WHITE        7
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_M_draw
!!    use M_draw
!!    use M_draw,    only  : D_BLACK,   D_WHITE
!!    use M_draw,    only  : D_RED,     D_GREEN,    D_BLUE
!!    use M_draw,    only  : D_YELLOW,  D_MAGENTA,  D_CYAN
!!    implicit none
!!    integer  :: ipaws
!!    real     :: x1, y1
!!    integer  :: icolor
!!    integer  :: i,j
!!
!!       ! initialize image
!!       call prefsize(400,400)  ! set size before starting
!!       call vinit(' ')         ! start graphics using device $M_DRAW_DEVICE
!!       call textsize(10.0,10.0)
!!       call mapcolor( 0,   255,255,255 )  !white
!!       call mapcolor( 1,   255,  0,  0 )  !red
!!       call mapcolor( 2,     0,255,  0 )  !green
!!       call mapcolor( 3,   255,255,  0 )  !yellow
!!       call mapcolor( 4,     0,  0,255 )  !blue
!!       call mapcolor( 5,   255,  0,255 )  !magenta
!!       call mapcolor( 6,     0,255,255 )  !cyan
!!       call mapcolor( 7,     0,  0,  0 )  !black
!!       call mapcolor( 8,     0,155,  0 )
!!       call mapcolor( 9,   155,155,155 )
!!       call mapcolor(10,   155,255,255 )
!!       call mapcolor(11,   155,155,  0 )
!!       call mapcolor(12,     0,  0,155 )
!!       call mapcolor(13,   155,  0,155 )
!!       call mapcolor(14,     0,155,155 )
!!       call mapcolor(15,   100,100,100 )
!!       call mapcolor(16,   155,100,100 )
!!       call color(D_BLACK)
!!       call clear()            ! clear to color 0
!!       call color(D_WHITE)
!!
!!       ! map area of virtual world to specified device area
!!       ! notice Y-axis for viewport is zero at TOP
!!       ! define the virtual world area we want to work in
!!       call page(0.0,  400.0,    0.0, 400.0)
!!       ! the drawing routines use these world units
!!
!!       ! put some colored boxes into pixmap by address
!!       ! so show how the pixel map can be edited easily with
!!       ! other routines that can manipulate a pixel array.
!!       ! The P_pixel array was created when vinit(3f) was called
!!       call polyfill(.true.)
!!       icolor=1
!!       do i=0,3
!!          do j=0,3
!!             x1=j*100.0
!!             y1=i*100.0
!!             icolor=icolor+1
!!             call color(icolor)
!!             call rect(x1,y1,x1+100.0,y1+100.0)
!!          enddo
!!       enddo
!!       call polyfill(.false.)
!!
!!       ! draw polar grids
!!       call linewidth(100)
!!
!!       call linewidth(100)
!!       call color(14)
!!       call target(200.0,200.0,200.0)
!!
!!       call linewidth(75)
!!       call color(0)
!!       call target(100.0,200.0,50.0)
!!
!!       ! draw some lines
!!       call color(D_RED)
!!       call linewidth(200)
!!       call line(1.0,1.0,400.0,400.0)
!!
!!       call color(D_BLUE)
!!       call linewidth(250)
!!       call line(350.0,200.0,350.0,300.0)
!!
!!       ! print some text
!!       call color(1)
!!       call linewidth(125)
!!       call font('futura.l')
!!       call hershey(40.0, 40.0,35.0,'Hello World',0.0)
!!       call color(7)
!!       call linewidth(25)
!!       call hershey(40.0, 80.0,35.0,'Hello World',0.0)
!!       call linewidth(100)
!!       call hershey(40.0,120.0,35.0,'Hello World',30.0)
!!
!!       call hershey(  40.0,350.0,35.0,'Hello World',0.0)
!!       call font('futura.m')
!!       call hershey(  40.0,310.0,35.0,'Hello World',0.0)
!!       call font('times.r')
!!       call hershey( 350.0,400.0,35.0,'Hello World',90.0)
!!       call linewidth(50)
!!       call font('times.i')
!!       call hershey(200.0,120.0,15.0,'Hello World',20.0)
!!
!!       ipaws=getkey()
!!       call vexit()
!!
!!       contains
!!
!!       subroutine target(xc,yc,rc)
!!       real     :: xc,yc,rc
!!       integer  :: i
!!       real     :: x,y
!!          do i=0,360,10
!!             x=rc*cosd(real(i))
!!             y=rc*sind(real(i))
!!             call line(xc,yc,xc+x,yc+y)
!!          enddo
!!          do i=1,int(rc),10
!!             call circle(xc,yc,real(i))
!!          enddo
!!       end subroutine target
!!
!!       subroutine line(x1,y1,x2,y2)
!!       real,intent(in) :: x1,y1,x2,y2
!!       call move2(x1,y1)
!!       call draw2(x2,y2)
!!       end subroutine line
!!
!!       subroutine hershey(x,y,height,itext,theta)
!!       real,intent(in)               :: x,y
!!       real,intent(in)               :: height
!!       character(len=*),intent(in)   :: itext
!!       real,intent(in)               :: theta
!!       call move2(x,y)
!!       call textang(theta)
!!       call textsize(height,height)
!!       call drawstr(itext)
!!       end subroutine hershey
!!    end program demo_M_draw
!!
!!##BUGS
!!
!!    Polygon hatching will give unexpected results unless the polygon is
!!    initially defined in the X-Y plane.
!!
!!    Double buffering isn't supported on all devices.
!!
!!    We don't recommend the use of the smove/sdraw routines.
!!
!!    The yobbarays may be turned on or they may be turned off.
!!
!!    When creating an object, current position and text size are not
!!    actually changed so almost any query routine to get position or font
!!    size or whatever will not work properly.
!!
!!    If call vexit(3f) and then call vinit(3f) everything should probably
!!    be reset to initial values at program startup, such as linewidth,
!!    current font, and color. It is currently left up to the output device
!!    initialization routine. It should not be. To minimize the issue,
!!    call all the push* routines after a vinit(3f) and call all the pop*
!!    routines before vexit(3f).
!!
!!    Exactly what attributes should and should not be reset with a
!!    vnewdev(3f) is questionable.
module M_draw

use M_engine, only : D_BLACK, D_RED, D_GREEN, D_YELLOW, D_BLUE, D_MAGENTA, D_CYAN, D_WHITE
use M_engine, only : D_XCENTERED, D_YCENTERED, D_LEFT, D_RIGHT, D_TOP, D_BOTTOM

use M_oncore, only : page
use M_oncore, only : print
use M_oncore, only : pop
use M_oncore, only : push
use M_oncore, only : invokeobj

!use M_engine, only : CalcW2Vcoeffs      => draw_CalcW2Vcoeffs
!use M_engine, only : VtoWxy             => draw_VtoWxy
!use M_engine, only : WtoVx              => draw_WtoVx
!use M_engine, only : WtoVy              => draw_WtoVy

use M_engine, only : arc                => draw_arc
use M_engine, only : arcprecision       => draw_arcprecision
use M_engine, only : backbuffer         => draw_backbuffer
use M_engine, only : backface           => draw_backface
use M_engine, only : backfacedir        => draw_backfacedir
use M_engine, only : bottomjustify      => draw_bottomjustify
use M_engine, only : boxfit             => draw_boxfit
use M_engine, only : boxtext            => draw_boxtext
use M_engine, only : callobj            => draw_callobj
use M_engine, only : centertext         => draw_centertext
use M_engine, only : checkkey           => draw_checkkey
use M_engine, only : circle             => draw_circle
use M_engine, only : circleprecision    => draw_circleprecision
use M_engine, only : clear              => draw_clear
use M_engine, only : clip               => draw_clip
use M_engine, only : clipping           => draw_clipping
use M_engine, only : closeobj           => draw_closeobj
use M_engine, only : closepoly          => draw_closepoly
use M_engine, only : color              => draw_color
use M_engine, only : copymatrix         => draw_copymatrix
use M_engine, only : copytensor         => draw_copytensor
use M_engine, only : copytensortrans    => draw_copytensortrans
use M_engine, only : copytranspose      => draw_copytranspose
use M_engine, only : copyvector         => draw_copyvector
use M_engine, only : curve              => draw_curve
use M_engine, only : curvebasis         => draw_curvebasis
use M_engine, only : curven             => draw_curven
use M_engine, only : curveprecision     => draw_curveprecision
use M_engine, only : dashcode           => draw_dashcode
use M_engine, only : dashline           => draw_dashline
use M_engine, only : delobj             => draw_delobj
use M_engine, only : draw               => draw_draw
use M_engine, only : draw2              => draw_draw2
use M_engine, only : drawchar           => draw_drawchar
use M_engine, only : drawstr            => draw_drawstr
use M_engine, only : drcurve            => draw_drcurve
use M_engine, only : drpatch            => draw_drpatch
use M_engine, only : expandviewport     => draw_expandviewport
use M_engine, only : fixedwidth         => draw_fixedwidth
use M_engine, only : font               => draw_font
use M_engine, only : frontbuffer        => draw_frontbuffer
use M_engine, only : genobj             => draw_genobj
use M_engine, only : getaspect          => draw_getaspect
use M_engine, only : getcharsize        => draw_getcharsize
use M_engine, only : getdepth           => draw_getdepth
use M_engine, only : getdisplaysize     => draw_getdisplaysize
use M_engine, only : getfactors         => draw_getfactors
use M_engine, only : getfontasc         => draw_getfontasc
use M_engine, only : getfontdec         => draw_getfontdec
use M_engine, only : getfontheight      => draw_getfontheight
use M_engine, only : getfontsize        => draw_getfontsize
use M_engine, only : getfontwidth       => draw_getfontwidth
use M_engine, only : getgp              => draw_getgp
use M_engine, only : getgp2             => draw_getgp2
use M_engine, only : getgpt             => draw_getgpt
use M_engine, only : getkey             => draw_getkey
use M_engine, only : getmatrix          => draw_getmatrix
use M_engine, only : getopenobj         => draw_getopenobj
!use M_engine, only : getplanes          => draw_getplanes
use M_engine, only : getprefposandsize  => draw_getprefposandsize
use M_engine, only : getstring          => draw_getstring
use M_engine, only : getviewport        => draw_getviewport
use M_engine, only : hatchang           => draw_hatchang
use M_engine, only : hatchpitch         => draw_hatchpitch
use M_engine, only : hershfont          => draw_hershfont
use M_engine, only : identmatrix        => draw_identmatrix
use M_engine, only : isobj              => draw_isobj
use M_engine, only : leftjustify        => draw_leftjustify
use M_engine, only : linestyle          => draw_linestyle
use M_engine, only : linewidth          => draw_linewidth
use M_engine, only : loadmatrix         => draw_loadmatrix
use M_engine, only : loadobj            => draw_loadobj
use M_engine, only : locator            => draw_locator
use M_engine, only : lookat             => draw_lookat
use M_engine, only : makeobj            => draw_makeobj
use M_engine, only : makepoly           => draw_makepoly
use M_engine, only : mapcolor_int16     => draw_mapcolor
use M_engine, only : move               => draw_move
use M_engine, only : move2              => draw_move2
use M_engine, only : mult4x4            => draw_mult4x4
use M_engine, only : multmatrix         => draw_multmatrix
use M_engine, only : multtensor         => draw_multtensor
use M_engine, only : multvector         => draw_multvector
use M_engine, only : newtokens          => draw_newtokens
use M_engine, only : numchars           => draw_numchars
use M_engine, only : ortho              => draw_ortho
use M_engine, only : ortho2             => draw_ortho2
use M_engine, only : patch              => draw_patch
use M_engine, only : patchbasis         => draw_patchbasis
use M_engine, only : patchcurves        => draw_patchcurves
use M_engine, only : patchprecision     => draw_patchprecision
use M_engine, only : pdraw              => draw_pdraw
use M_engine, only : perspective        => draw_perspective
use M_engine, only : pmove              => draw_pmove
use M_engine, only : point              => draw_point
use M_engine, only : point2             => draw_point2
use M_engine, only : polarview          => draw_polarview
use M_engine, only : poly               => draw_poly
use M_engine, only : poly2              => draw_poly2
use M_engine, only : polyfill           => draw_polyfill
use M_engine, only : polyhatch          => draw_polyhatch
use M_engine, only : polyobj            => draw_polyobj
use M_engine, only : popattributes      => draw_popattributes
use M_engine, only : popdev             => draw_popdev
use M_engine, only : popmatrix          => draw_popmatrix
use M_engine, only : popviewport        => draw_popviewport
use M_engine, only : prefposition       => draw_prefposition
use M_engine, only : prefsize           => draw_prefsize
use M_engine, only : premulttensor      => draw_premulttensor
use M_engine, only : premultvector      => draw_premultvector
use M_engine, only : printattribs       => draw_printattribs
use M_engine, only : printmat           => draw_printmat
use M_engine, only : printvdevice       => draw_printvdevice
use M_engine, only : printvect          => draw_printvect
use M_engine, only : pushattributes     => draw_pushattributes
use M_engine, only : pushdev            => draw_pushdev
use M_engine, only : pushmatrix         => draw_pushmatrix
use M_engine, only : pushviewport       => draw_pushviewport
use M_engine, only : quickclip          => draw_quickclip
use M_engine, only : rcurve             => draw_rcurve
use M_engine, only : rdraw              => draw_rdraw
use M_engine, only : rdraw2             => draw_rdraw2
use M_engine, only : rect               => draw_rect
use M_engine, only : rightjustify       => draw_rightjustify
use M_engine, only : rmove              => draw_rmove
use M_engine, only : rmove2             => draw_rmove2
use M_engine, only : rotate             => draw_rotate
use M_engine, only : rpatch             => draw_rpatch
use M_engine, only : rsdraw2            => draw_rsdraw2
use M_engine, only : rsmove2            => draw_rsmove2
use M_engine, only : saveobj            => draw_saveobj
use M_engine, only : scale              => draw_scale
use M_engine, only : sdraw2             => draw_sdraw2
use M_engine, only : sector             => draw_sector
use M_engine, only : sgetgp2            => draw_sgetgp2
use M_engine, only : slocator           => draw_slocator
use M_engine, only : smove2             => draw_smove2
use M_engine, only : spoint2            => draw_spoint2
use M_engine, only : srect              => draw_srect
use M_engine, only : strlength          => draw_strlength
use M_engine, only : swapbuffers        => draw_swapbuffers
use M_engine, only : textang            => draw_textang
use M_engine, only : textjustify        => draw_textjustify
use M_engine, only : textsize           => draw_textsize
use M_engine, only : textslant          => draw_textslant
use M_engine, only : textweight         => draw_textweight
use M_engine, only : topjustify         => draw_topjustify
use M_engine, only : transformtensor    => draw_transformtensor
use M_engine, only : translate          => draw_translate
use M_engine, only : unexpandviewport   => draw_unexpandviewport
use M_engine, only : up                 => draw_up
!use M_engine, only : vallocate          => draw_vallocate
use M_engine, only : verror             => draw_verror
use M_engine, only : vexit              => draw_vexit
use M_engine, only : vflush             => draw_vflush
use M_engine, only : vfree              => draw_vfree
use M_engine, only : vgetdev            => draw_vgetdev
use M_engine, only : viewport           => draw_viewport
use M_engine, only : vinit              => draw_vinit
use M_engine, only : vnewdev            => draw_vnewdev
use M_engine, only : voutput            => draw_voutput
use M_engine, only : vsetflush          => draw_vsetflush
use M_engine, only : window             => draw_window
use M_engine, only : xcentertext        => draw_xcentertext
use M_engine, only : ycentertext        => draw_ycentertext
!use M_engine, only : yobbarays          => draw_yobbarays
private

!private :: CalcW2Vcoeffs
!private :: VtoWxy
!private :: WtoVx
!private :: WtoVy

public ::  arc
public ::  arcprecision
public ::  backbuffer
public ::  backface
public ::  backfacedir
public ::  bottomjustify
public ::  boxfit
public ::  boxtext
public ::  callobj
public ::  centertext
public ::  checkkey
public ::  circle
public ::  circleprecision
public ::  clear
public ::  clip
public ::  clipping
public ::  closeobj
public ::  closepoly
public ::  color
public ::  copymatrix
public ::  copytensor
public ::  copytensortrans
public ::  copytranspose
public ::  copyvector
public ::  curve
public ::  curvebasis
public ::  curven
public ::  curveprecision
public ::  dashcode
public ::  dashline
public ::  delobj
public ::  draw
public ::  draw2
public ::  drawchar
public ::  drawstr
public ::  drcurve
public ::  drpatch
public ::  expandviewport
public ::  fixedwidth
public ::  font
public ::  frontbuffer
public ::  genobj
public ::  getaspect
public ::  getcharsize
public ::  getdepth
public ::  getdisplaysize
public ::  getfactors
public ::  getfontasc
public ::  getfontdec
public ::  getfontheight
public ::  getfontsize
public ::  getfontwidth
public ::  getgp
public ::  getgp2
public ::  getgpt
public ::  getkey
public ::  getmatrix
public ::  getopenobj
public ::  getprefposandsize
public ::  getstring
public ::  getviewport
public ::  hatchang
public ::  hatchpitch
public ::  hershfont
public ::  identmatrix
public ::  isobj
public ::  leftjustify
public ::  linestyle
public ::  linewidth
public ::  loadmatrix
public ::  loadobj
public ::  locator
public ::  lookat
public ::  makeobj
public ::  makepoly
public ::  mapcolor
public ::  move
public ::  move2
public ::  mult4x4
public ::  multmatrix
public ::  multtensor
public ::  multvector
public ::  newtokens
public ::  numchars
public ::  ortho
public ::  ortho2
public ::  page
public ::  pop
public ::  push
public ::  invokeobj
public ::  print
public ::  patch
public ::  patchbasis
public ::  patchcurves
public ::  patchprecision
public ::  pdraw
public ::  perspective
public ::  pmove
public ::  point
public ::  point2
public ::  polarview
public ::  poly
public ::  poly2
public ::  polyfill
public ::  polyhatch
public ::  polyobj
public ::  popattributes
public ::  popdev
public ::  popmatrix
public ::  popviewport
public ::  prefposition
public ::  prefsize
public ::  premulttensor
public ::  premultvector
public ::  printattribs
public ::  printmat
public ::  printvdevice
public ::  printvect
public ::  pushattributes
public ::  pushdev
public ::  pushmatrix
public ::  pushviewport
public ::  quickclip
public ::  rcurve
public ::  rdraw
public ::  rdraw2
public ::  rect
public ::  rightjustify
public ::  rmove
public ::  rmove2
public ::  rotate
public ::  rpatch
public ::  rsdraw2
public ::  rsmove2
public ::  saveobj
public ::  scale
public ::  sdraw2
public ::  sector
public ::  sgetgp2
public ::  slocator
public ::  smove2
public ::  spoint2
public ::  srect
public ::  strlength
public ::  swapbuffers
public ::  textang
public ::  textjustify
public ::  textsize
public ::  textslant
public ::  textweight
public ::  topjustify
public ::  transformtensor
public ::  translate
public ::  unexpandviewport
public ::  up
public ::  verror
public ::  vexit
public ::  vflush
public ::  vfree
public ::  vgetdev
public ::  viewport
public ::  vinit
public ::  vnewdev
public ::  voutput
public ::  vsetflush
public ::  window
public ::  xcentertext
public ::  ycentertext

public ::  D_BLACK, D_RED, D_GREEN, D_YELLOW, D_BLUE, D_MAGENTA, D_CYAN, D_WHITE

! Hershey text justification
public  :: D_XCENTERED, D_YCENTERED, D_LEFT, D_RIGHT, D_TOP, D_BOTTOM

interface mapcolor
  procedure mapcolor_int16
  module procedure mapcolor_int32
end interface mapcolor

contains
subroutine mapcolor_int32(i, r, g, b)
   use, intrinsic :: iso_fortran_env, only: int16
   integer, intent(in) :: i
   integer, intent(in) :: r, g, b
   call mapcolor_int16(i,int(r,kind=int16),int(g,kind=int16),int(b,kind=int16))
end subroutine mapcolor_int32
end module M_draw
