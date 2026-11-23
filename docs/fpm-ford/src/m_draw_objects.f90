module m_draw_objects
   use, intrinsic :: iso_fortran_env, only: real32, error_unit
   use m_draw_types
   use m_draw_verror
   use m_draw_newtokens
   use m_draw_matrix
   use m_draw_tensor
   use m_draw_move
   use m_draw_draw
   use m_draw_arcs
   use m_draw_curves
   use m_draw_patches
   use m_draw_polygons
   use m_draw_rect
   use m_draw_text
   use m_draw_attr
   use m_draw_viewp
   use m_draw_buffer
   use m_draw_trans
   implicit none
   private
   public :: draw_makeobj, draw_closeobj, draw_delobj, draw_genobj, &
      draw_getopenobj, draw_loadobj, draw_saveobj, draw_callobj, draw_isobj

   type :: Object
      integer :: obno = 0
      type(TokList), pointer :: tlist => null()
      type(Object), pointer  :: next  => null()
   end type Object

   type :: ObjectPtr
      type(Object), pointer :: ptr => null()
   end type ObjectPtr

   type(ObjectPtr), save :: object_table(MAXENTS)
   integer, save :: obno = -1, omax = 0

   interface
      subroutine draw_color(i)
         integer, intent(in) :: i
      end subroutine draw_color

      subroutine draw_linewidth(w)
         integer, intent(in) :: w
      end subroutine draw_linewidth

      subroutine draw_mapcolor(i, r, g, b)
         use, intrinsic :: iso_fortran_env, only: int16
         integer, intent(in) :: i
         integer(int16), intent(in) :: r, g, b
      end subroutine draw_mapcolor
   end interface

contains

   pure integer function obj_hash(n)
      integer, intent(in) :: n
      obj_hash = mod(abs(n), MAXENTS) + 1
   end function obj_hash

!>
!!##NAME
!!    makeobj(3f) - [M_draw:OBJECT] Commence the object number n.
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!         subroutine makeobj(n)
!!         integer n
!!
!!##DESCRIPTION
!!    Commence the object number n.
   subroutine draw_makeobj(n)
      integer, intent(in) :: n
      type(Object), pointer :: o
      integer :: idx

      if (vdevice%initialized == 0) call draw_verror("makeobj: draw not initialized")

      idx = obj_hash(n)
      o => object_table(idx)%ptr
      do while (associated(o))
         if (o%obno == n) then
            call draw_delobj(n)
            exit
         end if
         o => o%next
      end do

      obno = n
      vdevice%tokens => null()
      vdevice%inobject = 1
      if (omax <= n) omax = n + 1
   end subroutine draw_makeobj

!>
!!##NAME
!!    closeobj(3f) - [M_draw:OBJECT] Close the current object.
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!         subroutine closeobj()
!!##DESCRIPTION
!!
!!    Close the current object.
   subroutine draw_closeobj()
      type(Object), pointer :: o
      integer :: idx

      if (vdevice%inobject == 0) call draw_verror("closeobj: not in an object")

      vdevice%inobject = 0
      idx = obj_hash(obno)

      allocate(o)
      o%obno = obno
      o%tlist => vdevice%tokens
      o%next => object_table(idx)%ptr
      object_table(idx)%ptr => o
      obno = -1
   end subroutine draw_closeobj

!>
!!##NAME
!!    delobj(3f) - [M_draw:OBJECT] Delete the object number n.
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!         subroutine delobj(n)
!!         integer n
!!##DESCRIPTION
!!    Delete the object number n.
   subroutine draw_delobj(n)
      integer, intent(in) :: n
      type(Object), pointer :: o, lo
      type(TokList), pointer :: tl, ntl
      integer :: idx

      idx = obj_hash(n)
      lo => null()
      o  => object_table(idx)%ptr
      do while (associated(o))
         if (o%obno == n) exit
         lo => o
         o  => o%next
      end do

      if (associated(o)) then
         tl => o%tlist
         do while (associated(tl))
            ntl => tl%next
            if (allocated(tl%toks)) deallocate(tl%toks)
            deallocate(tl)
            tl => ntl
         end do
         if (.not. associated(lo)) then
            object_table(idx)%ptr => o%next
         else
            lo%next => o%next
         end if
         deallocate(o)
      end if
   end subroutine draw_delobj

!>
!!##NAME
!!    genobj(3f) - [M_draw:OBJECT] Returns a unique object identifier.
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!         integer function genobj()
!!##DESCRIPTION
!!    Returns a unique object identifier.
   integer function draw_genobj()
      draw_genobj = omax
      omax = omax + 1
   end function draw_genobj

!>
!!##NAME
!!    getopenobj(3f) - [M_draw:OBJECT] Return the number of the current object.
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!         integer function getopenobj()
!!
!!##DESCRIPTION
!!    Return the number of the current object.
   integer function draw_getopenobj()
      draw_getopenobj = obno
   end function draw_getopenobj

!>
!!##NAME
!!    loadobj(3f) - [M_draw:OBJECT] Load the object in the file filename as object number n.
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!         subroutine loadobj(n, filename)
!!         integer n
!!         character*(*) filename
!!##DESCRIPTION
!!    Load the object in the file "filename" as object number n.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_loadobj
!!    ! test some object-related procedures
!!    use M_draw
!!    implicit none
!!    integer                      :: env_len, idum
!!    character(len=:),allocatable :: env
!!       !------------------------------------------------------------
!!       ! make an object file, which would normally be
!!       ! made by a separate program
!!       call voutput('+')
!!       call vinit('nil')   ! set up device
!!       call makeobj(3)
!!          call polyfill(.true.)
!!          call color(D_GREEN)
!!          call circle(0.0,0.0,4.0)
!!       call closeobj()
!!       call saveobj(3,"circle.obj")
!!       call vexit()
!!       !------------------------------------------------------------
!!       ! CURRENTLY:
!!       ! with multiple vinit(3f) calls the environment variable is
!!       ! not used without explicitly using it
!!       call get_environment_variable('M_DRAW_OUTPUT',LENGTH=env_len)
!!       if(env_len.ne.0)then
!!          allocate(character(len=env_len) :: env)
!!          call get_environment_variable('M_DRAW_OUTPUT',env)
!!          call voutput(env)
!!       endif
!!       !------------------------------------------------------------
!!       call prefsize(300, 300)
!!       call prefposition(100, 100)
!!       call vinit(' ')   ! set up device
!!       call ortho2(-5.1,5.3,-5.2,5.4)
!!       call color(D_WHITE)  ! set current color
!!       call clear()   ! clear screen to current color
!!       call color(D_BLUE)
!!       call move2(-5.0,-5.0)
!!       call draw2(5.0,5.0)
!!       call move2(-5.0,5.0)
!!       call draw2(5.0,-5.0)
!!       call loadobj(3,"circle.obj")
!!       call callobj(3)
!!       call translate(1.0,0.0,0.0)
!!       call translate(0.0,1.0,0.0)
!!       idum=getkey() ! wait for some input
!!       call vexit()  !  set the screen back to its original state
!!       !------------------------------------------------------------
!!    end program demo_loadobj
   subroutine draw_loadobj(n, file)
      integer, intent(in) :: n
      character(len=*), intent(in) :: file
      integer :: u, ios, idx, k, slen
      type(TokList), pointer :: tl
      type(Object), pointer :: o

      open(newunit=u, file=trim(file), access='stream', form='unformatted', status='old', action='read', iostat=ios)
      if (ios /= 0) then
         write(error_unit, '(A, A)') "loadobject: unable to open object file ", trim(file)
         return
      end if

      idx = obj_hash(n)
      allocate(o, tl)
      o%obno = n
      o%next => object_table(idx)%ptr
      object_table(idx)%ptr => o
      o%tlist => tl
      tl%next => null()

      read(u) tl%count
      allocate(tl%toks(tl%count))
      do k = 1, tl%count
         read(u) tl%toks(k)%i, tl%toks(k)%f, slen
         if (slen > 0) then
            allocate(character(len=slen) :: tl%toks(k)%s)
            read(u) tl%toks(k)%s
         end if
      end do
      close(u)
   end subroutine draw_loadobj

!>
!!##NAME
!!    saveobj(3f) - [M_draw:OBJECT] Save object number n into file filename. Does NOT save objects called inside object n.
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!         subroutine saveobj(n, filename)
!!         integer,intent(in) :: n
!!         character(len=*),intent(in) :: filename
!!
!!##DESCRIPTION
!!    Save the object number n into the file filename. This call does not
!!    save objects called inside object n.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_saveobj
!!    ! create object in file "circle.obj" for use with loadobj(3f)
!!    use M_draw
!!    implicit none
!!    character(len=:),allocatable :: env
!!    integer :: ipaws
!!    integer :: env_len
!!       call voutput('+')            ! ignore $M_DRAW_OUTPUT
!!       call vinit('nil')            ! start graphics
!!       call push()
!!       !
!!       call makeobj(3)              ! create an object
!!          call polyfill(.true.)
!!          call color(D_GREEN)
!!          call circle(0.0,0.0,10.0)
!!          call polyfill(.false.)
!!          call color(D_BLUE)
!!          call linewidth(100)
!!          call circle(0.0,0.0,4.0)
!!       call closeobj()
!!       !
!!       call saveobj(3,"circle.obj") ! save object to file
!!       call pop()
!!       call vexit()                 ! exit graphics
!!       !
!!       !-------  now this could be a separate program to use object
!!       !
!!       !------------------------------------------------------------
!!       ! CURRENTLY:
!!       ! with multiple vinit(3f) calls the environment variable is
!!       ! not used without explicitly using it
!!       call get_environment_variable('M_DRAW_OUTPUT',LENGTH=env_len)
!!       if(env_len.ne.0)then
!!          allocate(character(len=env_len) :: env)
!!          call get_environment_variable('M_DRAW_OUTPUT',env)
!!          call voutput(env)
!!       endif
!!       !------------------------------------------------------------
!!       call vinit(' ')  ! set up device
!!       call page(-100.0,100.0,-100.0,100.0)
!!       call loadobj(100,"circle.obj")
!!       ! translate x,y,z scale x,y,z rotate x,y,z object
!!       call invokeobj(   0.0,  0.0, 0.0,1.0,1.0,1.0,0.0,0.0,0.0, 100)
!!       call invokeobj( -20.0,-20.0, 0.0,1.0,2.0,1.0,0.0,0.0,0.0, 100)
!!       call invokeobj(  30.0, 40.0, 0.0,2.0,2.0,1.0,0.0,0.0,0.0, 100)
!!       ipaws=getkey()
!!       call vexit() ! set the screen back to its original state
!!       !
!!    end program demo_saveobj
   subroutine draw_saveobj(n, file)
      integer, intent(in) :: n
      character(len=*), intent(in) :: file
      integer :: u, ios, idx, count, k, slen
      type(Object), pointer :: o
      type(TokList), pointer :: tl

      open(newunit=u, file=trim(file), access='stream', form='unformatted', status='replace', action='write', iostat=ios)
      if (ios /= 0) then
         write(error_unit, '(A, A)') "saveobject: unable to open object file ", trim(file)
         return
      end if

      idx = obj_hash(n)
      o => object_table(idx)%ptr
      do while (associated(o))
         if (o%obno == n) exit
         o => o%next
      end do
      if (.not. associated(o)) then
         close(u); return
      end if

      count = 0
      tl => o%tlist
      do while (associated(tl))
         count = count + tl%count
         tl => tl%next
      end do

      write(u) count
      tl => o%tlist
      do while (associated(tl))
         do k = 1, tl%count
            if (allocated(tl%toks(k)%s)) then
               slen = len(tl%toks(k)%s)
            else
               slen = 0
            end if
            write(u) tl%toks(k)%i, tl%toks(k)%f, slen
            if (slen > 0) write(u) tl%toks(k)%s
         end do
         tl => tl%next
      end do
      close(u)
   end subroutine draw_saveobj

   subroutine draw_doarc(x, y, xoff, yoff, cosine, sine, nsegs)
      real(real32), intent(in) :: x, y, xoff, yoff, cosine, sine
      integer, intent(in) :: nsegs
      real(real32) :: cx, cy, dx, dy
      integer :: i

      cx = x + xoff; cy = y + yoff
      call draw_move2(cx, cy)
      do i = 1, nsegs
         dx = cx - x; dy = cy - y
         cx = x + dx * cosine - dy * sine
         cy = y + dx * sine + dy * cosine
         call draw_draw2(cx, cy)
      end do
   end subroutine draw_doarc

!>
!!##NAME
!!    callobj(3f) - [M_draw:OBJECT] Draw object number n.
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!         subroutine callobj(n)
!!         integer n
!!##DESCRIPTION
!!    Draw object number n.
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_callobj
!!       use M_draw
!!       implicit none
!!       integer :: ipaws
!!       integer :: ix, iy
!!       real    :: x, y
!!       integer :: icolor
!!
!!       ! set up graphics area
!!       call prefsize(680,680)
!!       call vinit(' ') ! start graphics using device $M_DRAW_DEVICE
!!       !! Background color
!!       !call color(D_WHITE)
!!       !! Page setup
!!       !call page(0.0,2124.0,0.0,2124.0)
!!       !call color(D_BLACK)
!!
!!       call makeobj(1111)
!!       call circleprecision(300)
!!       call curveprecision(300)
!!
!!       icolor=0
!!       call mapcolor(icolor, 0, 0, 0); icolor=icolor+1
!!       call mapcolor(icolor, 0, 0, 255); icolor=icolor+1
!!       call mapcolor(icolor, 0, 255, 0); icolor=icolor+1
!!       call mapcolor(icolor, 0, 255, 255); icolor=icolor+1
!!       call mapcolor(icolor, 255, 0, 0); icolor=icolor+1
!!       call mapcolor(icolor, 255, 0, 255); icolor=icolor+1
!!       call mapcolor(icolor, 255, 255, 0); icolor=icolor+1
!!       call mapcolor(icolor, 255, 255, 255); icolor=icolor+1
!!       call mapcolor(icolor, 0, 0, 142); icolor=icolor+1
!!       call mapcolor(icolor, 0, 0, 175); icolor=icolor+1
!!       call mapcolor(icolor, 0, 0, 209); icolor=icolor+1
!!       call mapcolor(icolor, 135, 206, 255); icolor=icolor+1
!!       call mapcolor(icolor, 0, 142, 0); icolor=icolor+1
!!       call mapcolor(icolor, 0, 175, 0); icolor=icolor+1
!!       call mapcolor(icolor, 0, 209, 0); icolor=icolor+1
!!       call mapcolor(icolor, 0, 142, 142); icolor=icolor+1
!!       call mapcolor(icolor, 0, 175, 175); icolor=icolor+1
!!       call mapcolor(icolor, 0, 209, 209); icolor=icolor+1
!!       call mapcolor(icolor, 142, 0, 0); icolor=icolor+1
!!       call mapcolor(icolor, 175, 0, 0); icolor=icolor+1
!!       call mapcolor(icolor, 209, 0, 0); icolor=icolor+1
!!       call mapcolor(icolor, 142, 0, 142); icolor=icolor+1
!!       call mapcolor(icolor, 175, 0, 175); icolor=icolor+1
!!       call mapcolor(icolor, 209, 0, 209); icolor=icolor+1
!!       call mapcolor(icolor, 127, 48, 0); icolor=icolor+1
!!       call mapcolor(icolor, 160, 63, 0); icolor=icolor+1
!!       call mapcolor(icolor, 191, 96, 0); icolor=icolor+1
!!       call mapcolor(icolor, 255, 127, 127); icolor=icolor+1
!!       call mapcolor(icolor, 255, 160, 160); icolor=icolor+1
!!       call mapcolor(icolor, 255, 191, 191); icolor=icolor+1
!!       call mapcolor(icolor, 255, 224, 224); icolor=icolor+1
!!       call mapcolor(icolor, 255, 214, 0); icolor=icolor+1
!!       call mapcolor(icolor, 64, 64, 64); icolor=icolor+1
!!       call mapcolor(icolor, 128, 128, 128); icolor=icolor+1
!!       call mapcolor(icolor, 192, 192, 192); icolor=icolor+1
!!       call mapcolor(icolor, 224, 224, 224); icolor=icolor+1
!!       call mapcolor(icolor, 255, 255, 255); icolor=icolor+1
!!
!!       call polyfill(.true.)
!!
!!       ! Lower Pedestal Box
!!       call color(34-1)
!!       call rect(612.0, 537.0, 1512.0, 462.0)
!!       call polyfill(.false.)
!!       call rasters(5)
!!       call color(37-1)
!!       call rect(612.0, 537.0, 1512.0, 462.0)
!!       call polyfill(.true.)
!!       call rasters(1)
!!       ! Keyboard Surface
!!       call color(36-1)
!!       call makepoly()
!!       IX=237
!!       IY=462
!!       X=real(IX)
!!       Y=real(IY)
!!       call move2(X,Y)
!!       call pline([IX,IY,1887,462,2112,87,2112,12,12,12,12,87,IX,IY])
!!
!!       call closepoly()
!!       ! Upper Pedestal Box
!!       call color(34-1)
!!       call rect(687.0, 612.0, 1437.0, 537.0)
!!       call polyfill(.false.)
!!       call rasters(5)
!!       call color(37-1)
!!       call rect(687.0, 612.0, 1437.0, 537.0)
!!       call polyfill(.true.)
!!       call rasters(1)
!!
!!       ! Monitor Box
!!       call color(36-1)
!!       call rect(162.0, 2112.0, 1962.0, 612.0)
!!
!!       ! Main QWERTY area
!!       !edgewidth(0)
!!
!!       call color(37-1)
!!       call makepoly()
!!       call pline([312,387,162,162,1512,162,1437,387,312,387])
!!       call closepoly()
!!
!!       ! Numeric Keypad Area
!!       call makepoly()
!!       call pline([1812,387,1512,387,1587,162,1962,162,1812,387])
!!       call closepoly()
!!
!!       ! Shade Keyboard Front
!!       call color(35-1)
!!       call makepoly()
!!       call pline([12,87,12,12,2112,12,2112,87])
!!
!!       call closepoly()
!!       ! U-Shaped Edge of Keyboard Front for definition
!!       call rasters(5)
!!       call color(1-1)
!!       call pline([12,87,12,12,2112,12,2112,87])
!!
!!       ! Glass Tube/Viewing Panel
!!       call color(33-1)
!!       call rect(387.0, 1962.0, 1737.0, 762.0)
!!       ! Upper Edge of Recess
!!       call color(34-1)
!!       call makepoly()
!!       call pline([ &
!!       & 387,1887,388,1887,391,1887,396,1888,403,1888,413, &
!!       & 1889,427,1890,443,1892,463,1893,484,1895, &
!!       & 508,1896,535,1898,562,1900,592,1902,622,1904,654, &
!!       & 1906,687,1907,722,1909,759,1911,797,1912, &
!!       & 838,1913,881,1915,927,1916,975,1916,1025,1917,1077, &
!!       & 1917,1133,1917,1187,1916,1238,1915,1285,1914, &
!!       & 1330,1913,1371,1911,1410,1910,1446,1908,1481,1906, &
!!       & 1514,1904,1545,1902,1575,1900,1603,1898,1629,1896, &
!!       & 1653,1894,1674,1892,1692,1891,1707,1890,1719,1889, &
!!       & 1727,1888,1733,1887,1736,1887,1737,1887,1738,1888, &
!!       & 1741,1891,1748,1898,1760,1910,1775,1925,1789,1939, &
!!       & 1801,1951,1808,1958,1811,1961,1812,1962,1810,1962, &
!!       & 1807,1962,1800,1962,1789,1962,1773,1962,1752,1962, &
!!       & 1725,1962,1692,1962,1653,1962,1607,1962,1555,1962, &
!!       & 1498,1962,1434,1962,1366,1962,1294,1962,1218,1962, &
!!       & 1141,1962,1062,1962,983,1962,906,1962,830,1962, &
!!       & 758,1962,690,1962,626,1962,569,1962,517,1962,471, &
!!       & 1962,432,1962,399,1962,372,1962,351,1962, &
!!       & 335,1962,324,1962,317,1962,314,1962,312,1962,313, &
!!       & 1961,316,1958,323,1951,335,1939,350,1924, &
!!       & 364,1910,376,1898,383,1891,386,1888,387,1887])
!!       call closepoly()
!!
!!       ! Raised front of QWERTY area
!!       call rect(162.0, 162.0, 1512.0, 135.0)
!!
!!       ! Left Control Keys in QWERTY
!!       call color(35-1)
!!       call makepoly()
!!       call pline([284,387,162,162,387,162,418,237,366, &
!!               & 237,396,312,348,312,387,387,284,387])
!!       call closepoly()
!!
!!       ! Right Control Keys in QWERTY
!!       call makepoly()
!!       call pline([1512,162,1287,162,1287,238,1336,237,1328, &
!!               & 312,1362,312,1373,312,1358,387,1451,387,1512,162])
!!       call closepoly()
!!
!!       ! Numeric Keypad Special Keys -- Just Top?
!!       call makepoly()
!!       call pline([1962,162,1893,162,1812,342,1526,342,1511,387, &
!!               & 1843,387,1962,162])
!!       call closepoly()
!!
!!       ! Raised Front of Numeric Keypad
!!       call color(34-1)
!!       call rect(1587.0,162.0,1962.0,136.0)
!!
!!       ! Left Raised Edge of Numeric Keypad
!!       call makepoly()
!!       call pline([1587,161,1586,134,1510,365,1511,386,1587,161])
!!       call closepoly()
!!
!!       ! Right Edge of Tube Recess
!!       call color(35-1)
!!       call makepoly()
!!       call pline([ &
!!       & 1812,762,1811,763,1808,766,1801,773,1789,785, &
!!       & 1774,800,1760,814,1748,826,1741,833,1738,836, &
!!       & 1737,837,1737,839,1737,843,1737,851,1737,863, &
!!       & 1737,881,1737,905,1737,934,1737,970,1737,1012, &
!!       & 1737,1060,1737,1113,1737,1171,1737,1232,1737,1297, &
!!       & 1737,1362,1737,1427,1737,1492,1737,1553,1737,1611, &
!!       & 1737,1664,1737,1712,1737,1754,1737,1790,1737,1819, &
!!       & 1737,1843,1737,1861,1737,1873,1737,1881,1737,1885, &
!!       & 1737,1887,1738,1888,1741,1891,1748,1898,1760,1910, &
!!       & 1775,1925,1789,1939,1801,1951,1808,1958,1811,1961, &
!!       & 1812,1962,1812,1960,1812,1956,1812,1948,1812,1937, &
!!       & 1812,1919,1812,1897,1812,1868,1812,1833,1812,1791, &
!!       & 1812,1744,1812,1690,1812,1631,1812,1568,1812,1502, &
!!       & 1812,1432,1812,1362,1812,1292,1812,1222,1812,1156, &
!!       & 1812,1093,1812,1034,1812,980,1812,933,1812,891, &
!!       & 1812,856,1812,827,1812,805,1812,787,1812,776, &
!!       & 1812,768,1812,764,1812,762])
!!       call closepoly()
!!
!!       ! Left Edge of Tube Recess
!!       call makepoly()
!!       call pline([ &
!!       & 312,762,313,763,316,766,323,773,335,785, &
!!       & 350,800,364,814,376,826,383,833,386,836, &
!!       & 387,837,387,839,387,843,387,851,387,863, &
!!       & 387,881,387,905,387,934,387,970,387,1012, &
!!       & 387,1060,387,1113,387,1171,387,1232,387,1297, &
!!       & 387,1362,387,1427,387,1492,387,1553,387,1611, &
!!       & 387,1664,387,1712,387,1754,387,1790,387,1819, &
!!       & 387,1843,387,1861,387,1873,387,1881,387,1885, &
!!       & 387,1887,386,1888,383,1891,376,1898,364,1910, &
!!       & 349,1925,335,1939,323,1951,316,1958,313,1961, &
!!       & 312,1962,312,1960,312,1956,312,1948,312,1937, &
!!       & 312,1919,312,1897,312,1868,312,1833,312,1791, &
!!       & 312,1744,312,1690,312,1631,312,1568,312,1502, &
!!       & 312,1432,312,1362,312,1292,312,1222,312,1156, &
!!       & 312,1093,312,1034,312,980,312,933,312,891, &
!!       & 312,856,312,827,312,805,312,787,312,776, &
!!       & 312,768,312,764,312,762])
!!
!!       call closepoly()
!!
!!       ! Bottom Edge of Tube Recess
!!       call color(37-1)
!!       call makepoly()
!!       call pline([ &
!!       & 387,837,388,837,391,837,396,836,403,836, &
!!       & 413,835,427,834,443,832,463,831,484,829, &
!!       & 508,828,535,826,562,824,592,822,622,820, &
!!       & 654,818,687,817,722,815,759,813,797,812, &
!!       & 838,811,881,809,927,808,975,808,1025,807, &
!!       & 1077,807,1133,807,1187,808,1238,809,1285,810, &
!!       & 1330,811,1371,813,1410,814,1446,816,1481, &
!!       & 818,1514,820,1545,822,1575,824,1603,826,1629,828, &
!!       & 1653,830,1674,832,1692,833,1707,834,1719, &
!!       & 835,1727,836,1733,837,1736,837,1737,837,1738,836, &
!!       & 1741,833,1748,826,1760,814,1775,799,1789,785, &
!!       & 1801,773,1808,766,1811,763,1812,762,1810,762, &
!!       & 1807,762,1800,762,1789,762,1773,762,1752,762, &
!!       & 1725,762,1692,762,1653,762,1607,762,1555,762, &
!!       & 1498,762,1434,762,1366,762,1294,762,1218,762, &
!!       & 1141,762,1062,762,983,762,906,762,830,762, &
!!       & 758,762,690,762,626,762,569,762,517,762,471, &
!!       & 762,432,762,399,762,372,762,351,762, &
!!       & 335,762,324,762,317,762,314,762,312,762,313, &
!!       & 763,316,766,323,773,335,785,350,800, &
!!       & 364,814,376,826,383,833,386,836,387,837])
!!       call closepoly()
!!
!!       ! < of X11
!!       call color(8-1)
!!       call makepoly()
!!       call pline([1034,1233,927,1233,743,1463,987, &
!!               & 1769,1034,1769,819,1493,1034,1233])
!!       call closepoly()
!!
!!       call rasters(5)
!!       call pline([1034,1233,927,1233,743,1463,987, &
!!               & 1769,1034,1769,819,1493,1034,1233])
!!       call rasters(1)
!!
!!       ! > of X11
!!       call color(8-1)
!!       call makepoly()
!!       call pline( [483,1769,591,1769,774,1540,530, &
!!               & 1233,483,1233, 698,1509,483,1769])
!!       call closepoly()
!!
!!       call rasters(5)
!!       call pline( [483,1769,591,1769,774,1540,530, &
!!               & 1233,483,1233,698,1509,483,1769])
!!       call rasters(1)
!!
!!       ! End of Picture %
!!
!!       call closeobj()
!!
!!       call makeobj(2222)
!!       call call_obj(1111,xt=-1012.0, yt=-1012.0)
!!       call closeobj()
!!
!!       !!call call_obj(2222)
!!
!!       ! make a big page and call object with various transformations
!!       call color(D_WHITE)
!!       call page( 0.0, 20000.0, 0.0, 20000.0)
!!       call clear()
!!       call color(D_BLACK)
!!
!!       call call_obj(1111, xs=4.0, ys=4.0)
!!       call call_obj(2222, xs=2.0, ys=2.0)
!!       call call_obj(2222, xs=2.0, ys=2.0, xt=10000.0, yt=10000.0)
!!       call call_obj(2222, xs=2.0, ys=2.0, xt=4000.0, yt=10000.0, zr=45.0)
!!       call call_obj(2222, xs=2.0, ys=2.0, yt=3000.0, xt=12000.0)
!!       call call_obj(2222, xs=1.4, ys=2.0, xt=13000.0, yt=16000.0, zr=180.0)
!!       call call_obj(2222, xs=1.0, ys=1.0, xt=16000.0, yt=16000.0, zr=0.0)
!!       call call_obj(2222, xs=4.0, ys=2.0, xt=6000.0, yt=17000.0)
!!       call call_obj(2222, xs=1.0, ys=1.0, xt=16000.0, yt=4000.0, zr=0.0)
!!       call call_obj(2222, xs=1.0, ys=1.0, xt=18000.0, yt=5500.0, zr=-30.0)
!!       call call_obj(2222, xs=1.0, ys=1.0, xt=16000.0, yt=7000.0, zr=30.0)
!!       call call_obj(2222, xs=1.0, ys=1.0, xt=18000.0, yt=9500.0, zr=-50.0)
!!       call call_obj(2222, xs=1.0, ys=1.0, xt=16000.0, yt=11000.0, zr=70.0)
!!
!!       ! this does not work as expected
!!       !call call_obj(2222, xs=2.0,, ys=2.0,, xt=16000.0, yt=13000.0, &
!!       ! & xr=10.0, yr=10.0 )
!!       !call call_obj(1111, xs=2.0,, ys=2.0,, xt=16000.0, yt=13000.0, &
!!       ! & xr=0.10, yr=0.10 )
!!
!!       ! pause
!!       call vflush()
!!       ipaws=getkey()
!!
!!       ! wrap up graphics
!!       call vexit()
!!
!!       contains
!!       subroutine pline(iarr)
!!          integer,intent(in) :: iarr(:)
!!          integer            :: i
!!          ! assuming nice data in x,y pairs
!!          call move2(real(iarr(1)),real(iarr(2)))
!!          do i=3,size(iarr),2
!!             call draw2(real(iarr(i)),real(iarr(i+1)))
!!          enddo
!!       end subroutine pline
!!
!!       subroutine call_obj(iobj,xt,yt,zt,xs,ys,zs,xr,yr,zr)
!!          ! DEFAULT call_obj(iobj,0.0,0.0,0.0, 1.0,1.0,1.0, 0.0,0.0.0.0)
!!          integer,intent(in) :: iobj
!!          real,optional :: xt,yt,zt
!!          real,optional :: xs,ys,zs
!!          real,optional :: xr,yr,zr
!!          real          :: xt_l,yt_l,zt_l
!!          real          :: xs_l,ys_l,zs_l
!!          real          :: xr_l,yr_l,zr_l
!!          if(present(xt))then; xt_l=xt; else; xt_l=0.0; endif
!!          if(present(yt))then; yt_l=yt; else; yt_l=0.0; endif
!!          if(present(zt))then; zt_l=zt; else; zt_l=0.0; endif
!!
!!          if(present(xs))then; xs_l=xs; else; xs_l=1.0; endif
!!          if(present(ys))then; ys_l=ys; else; ys_l=1.0; endif
!!          if(present(zs))then; zs_l=zs; else; zs_l=1.0; endif
!!
!!          if(present(xr))then; xr_l=xr; else; xr_l=0.0; endif
!!          if(present(yr))then; yr_l=yr; else; yr_l=0.0; endif
!!          if(present(zr))then; zr_l=zr; else; zr_l=0.0; endif
!!
!!          ! call an object using specified scaling, translation, and rotation
!!          ! and then restore coordinate space
!!          call pushmatrix()
!!             call translate(xt_l,yt_l,zt_l)
!!             call scale(xs_l,ys_l,zs_l)
!!             call rotate(xr_l,"x")
!!             call rotate(yr_l,"y")
!!             call rotate(zr_l,"z")
!!             call callobj(iobj)
!!          call popmatrix()
!!       end subroutine call_obj
!!
!!       subroutine rasters(iwidth)
!!          integer,intent(in) :: iwidth
!!          call linewidth(iwidth*5)
!!       end subroutine rasters
!!    end program demo_callobj
   recursive subroutine draw_callobj(n)
      integer, intent(in) :: n
      type(Object), pointer :: o
      type(TokList), pointer :: tl
      real(real32) :: prod(4,4), tmpmat(4,4), S_ten(4,4,4)
      integer :: sync, i, j, k, p_idx, inpoly, idx, idum
      real(real32) :: cx, cy, cz
      type(Token), pointer :: t(:)

      if (vdevice%initialized == 0) call draw_verror("callobj: draw not initialized")

      if (vdevice%inobject /= 0) then
         t => draw_newtokens(2)
         t(1)%i = OBJ_CALLOBJ; t(2)%i = n
         return
      end if

      idx = obj_hash(n)
      o => object_table(idx)%ptr
      do while (associated(o))
         if (o%obno == n) exit
         o => o%next
      end do
      if (.not. associated(o)) return

      sync = vdevice%sync
      if (sync /= 0) vdevice%sync = 0

      tl => o%tlist
      do while (associated(tl))
         k = 1
         do while (k <= tl%count)
            select case (tl%toks(k)%i)
             case (OBJ_ARC)
               call draw_doarc(tl%toks(k+1)%f, tl%toks(k+2)%f, tl%toks(k+3)%f, &
                  tl%toks(k+4)%f, tl%toks(k+5)%f, tl%toks(k+6)%f, tl%toks(k+7)%i)
               k = k + 8
             case (OBJ_BACKBUFFER)
               idum = draw_backbuffer(); k = k + 1
             case (OBJ_BACKFACING)
               call draw_backface(tl%toks(k+1)%i); k = k + 2
             case (OBJ_BOXTEXT)
               call draw_boxtext(tl%toks(k+1)%f, tl%toks(k+2)%f, tl%toks(k+3)%f, &
                  tl%toks(k+4)%f, tl%toks(k+5)%s)
               k = k + 6
             case (OBJ_CALLOBJ)
               call draw_callobj(tl%toks(k+1)%i); k = k + 2
             case (OBJ_TEXTSLANT)
               call draw_textslant(tl%toks(k+1)%f); k = k + 2
             case (OBJ_CENTERTEXT)
               call draw_centertext(tl%toks(k+1)%i); k = k + 2
             case (OBJ_TEXTJUSTIFY)
               call draw_textjustify(tl%toks(k+1)%i); k = k + 2
             case (OBJ_RIGHTJUSTIFY)
               call draw_rightjustify(); k = k + 1
             case (OBJ_LEFTJUSTIFY)
               call draw_leftjustify(); k = k + 1
             case (OBJ_XCENTERTEXT)
               call draw_xcentertext(); k = k + 1
             case (OBJ_YCENTERTEXT)
               call draw_ycentertext(); k = k + 1
             case (OBJ_TOPJUSTIFY)
               call draw_topjustify(); k = k + 1
             case (OBJ_BOTTOMJUSTIFY)
               call draw_bottomjustify(); k = k + 1
             case (OBJ_CIRCLE)
               inpoly = vdevice%inpolygon
               if ((vdevice%attr%a%fill /= 0 .or. vdevice%attr%a%hatch /= 0) .and. inpoly == 0) call draw_makepoly()
               call draw_doarc(tl%toks(k+1)%f, tl%toks(k+2)%f, tl%toks(k+3)%f, &
                  0.0_real32, tl%toks(k+4)%f, tl%toks(k+5)%f, tl%toks(k+6)%i)
               if ((vdevice%attr%a%fill /= 0 .or. vdevice%attr%a%hatch /= 0) .and. inpoly == 0) then
                  call draw_closepoly()
               else
                  call draw_draw2(tl%toks(k+1)%f + tl%toks(k+3)%f, tl%toks(k+2)%f)
               end if
               k = k + 7
             case (OBJ_CLEAR)
               idum = vdevice%dev%Vclear(); k = k + 1
             case (OBJ_COLOR)
               call draw_color(tl%toks(k+1)%i); k = k + 2
             case (OBJ_LINEWIDTH)
               call draw_linewidth(tl%toks(k+1)%i); k = k + 2
             case (OBJ_DRAW)
               call draw_draw(tl%toks(k+1)%f, tl%toks(k+2)%f, tl%toks(k+3)%f); k = k + 4
             case (OBJ_DASHCODE)
               call draw_dashcode(tl%toks(k+1)%f); k = k + 2
             case (OBJ_DRAWCHAR)

               call draw_drawchar(tl%toks(k+1)%i); k = k + 2
             case (OBJ_DRAWSTR)
               call draw_drawstr(tl%toks(k+1)%s); k = k + 2
             case (OBJ_LINESTYLE)
               call draw_linestyle(tl%toks(k+1)%s); k = k + 2
             case (OBJ_FIXEDWIDTH)
               call draw_fixedwidth(tl%toks(k+1)%i); k = k + 2
             case (OBJ_FONT)
               call draw_font(tl%toks(k+1)%s); k = k + 2
             case (OBJ_HATCHANG)
               vdevice%attr%a%hatchcos = tl%toks(k+1)%f
               vdevice%attr%a%hatchsin = tl%toks(k+2)%f
               k = k + 3
             case (OBJ_HATCHPITCH)
               vdevice%attr%a%hatchpitch = tl%toks(k+1)%f; k = k + 2
             case (OBJ_LOADMATRIX)
               p_idx = k + 1
               do i = 1, 4
                  do j = 1, 4
                     vdevice%transmat%m(i,j) = tl%toks(p_idx)%f
                     p_idx = p_idx + 1
                  end do
               end do
               vdevice%cpVvalid = 0
               k = k + 17
             case (OBJ_MAPCOLOR)
               call draw_mapcolor(tl%toks(k+1)%i, int(tl%toks(k+2)%i, 2), &
                  int(tl%toks(k+3)%i, 2), int(tl%toks(k+4)%i, 2))
               k = k + 5
             case (OBJ_MOVE)
               call draw_move(tl%toks(k+1)%f, tl%toks(k+2)%f, tl%toks(k+3)%f); k = k + 4
             case (OBJ_MULTMATRIX)
               p_idx = k + 1
               do i = 1, 4
                  do j = 1, 4
                     tmpmat(i,j) = tl%toks(p_idx)%f
                     p_idx = p_idx + 1
                  end do
               end do
               call draw_mult4x4(prod, tmpmat, vdevice%transmat%m)
               call draw_loadmatrix(prod)
               k = k + 17
             case (OBJ_POLY)
               call draw_polyobj(tl%toks(k+1)%i, tl%toks(k+2 : k + 1 + 3*tl%toks(k+1)%i))
               k = k + 2 + 3 * tl%toks(k+1)%i
             case (OBJ_POLYFILL)
               call draw_polyfill(tl%toks(k+1)%i); k = k + 2
             case (OBJ_POLYHATCH)
               call draw_polyhatch(tl%toks(k+1)%i); k = k + 2
             case (OBJ_POPATTRIBUTES)
               call draw_popattributes(); k = k + 1
             case (OBJ_POPMATRIX)
               call draw_popmatrix(); k = k + 1
             case (OBJ_POPVIEWPORT)
               call draw_popviewport(); k = k + 1
             case (OBJ_PUSHATTRIBUTES)
               call draw_pushattributes(); k = k + 1
             case (OBJ_PUSHMATRIX)
               call draw_pushmatrix(); k = k + 1
             case (OBJ_PUSHVIEWPORT)
               call draw_pushviewport(); k = k + 1
             case (OBJ_RCURVE)
               i = tl%toks(k+1)%i
               cx = tl%toks(k+2)%f; cy = tl%toks(k+3)%f; cz = tl%toks(k+4)%f
               p_idx = k + 5
               do j = 1, 4
                  do idum = 1, 4
                     tmpmat(j, idum) = tl%toks(p_idx)%f
                     p_idx = p_idx + 1
                  end do
               end do
               call draw_mult4x4(prod, tmpmat, vdevice%transmat%m)
               call draw_drcurve(i, prod)
               vdevice%cpW(D_X) = cx; vdevice%cpW(D_Y) = cy; vdevice%cpW(D_Z) = cz
               k = k + 21
             case (OBJ_RPATCH)
               cx = tl%toks(k+1)%f; cy = tl%toks(k+2)%f; cz = tl%toks(k+3)%f
               p_idx = k + 10
               do i = 1, 4
                  do j = 1, 4
                     S_ten(1,i,j) = tl%toks(p_idx)%f; S_ten(2,i,j) = tl%toks(p_idx+1)%f
                     S_ten(3,i,j) = tl%toks(p_idx+2)%f; S_ten(4,i,j) = tl%toks(p_idx+3)%f
                     p_idx = p_idx + 4
                  end do
               end do
               call draw_transformtensor(S_ten, vdevice%transmat%m)
               call draw_drpatch(S_ten, tl%toks(k+4)%i, tl%toks(k+5)%i, tl%toks(k+6)%i, &
                  tl%toks(k+7)%i, tl%toks(k+8)%i, tl%toks(k+9)%i)
               vdevice%cpW(D_X) = cx; vdevice%cpW(D_Y) = cy; vdevice%cpW(D_Z) = cz
               k = p_idx
             case (OBJ_SECTOR)
               inpoly = vdevice%inpolygon
               if ((vdevice%attr%a%fill /= 0 .or. vdevice%attr%a%hatch /= 0) .and. inpoly == 0) call draw_makepoly()
               call draw_doarc(tl%toks(k+1)%f, tl%toks(k+2)%f, tl%toks(k+3)%f, &
                  tl%toks(k+4)%f, tl%toks(k+5)%f, tl%toks(k+6)%f, tl%toks(k+7)%i)
               call draw_draw(tl%toks(k+1)%f, tl%toks(k+2)%f, 0.0_real32)
               if ((vdevice%attr%a%fill /= 0 .or. vdevice%attr%a%hatch /= 0) .and. inpoly == 0) then
                  call draw_closepoly()
               else
                  call draw_draw2(tl%toks(k+1)%f + tl%toks(k+3)%f, tl%toks(k+2)%f + tl%toks(k+4)%f)
               end if
               k = k + 8
             case (OBJ_TEXTANG)
               vdevice%attr%a%textcos = tl%toks(k+1)%f; vdevice%attr%a%textsin = tl%toks(k+2)%f
               k = k + 3
             case (OBJ_TEXTSIZE)
               call draw_textsize(tl%toks(k+1)%f, tl%toks(k+2)%f); k = k + 3
             case (OBJ_VIEWPORT)
               call draw_viewport(tl%toks(k+1)%f, tl%toks(k+2)%f, tl%toks(k+3)%f, tl%toks(k+4)%f); k = k + 5
             case (OBJ_TRANSLATE)
               call draw_translate(tl%toks(k+1)%f, tl%toks(k+2)%f, tl%toks(k+3)%f); k = k + 4
             case (OBJ_SCALE)
               vdevice%transmat%m(1,1:4) = vdevice%transmat%m(1,1:4) * tl%toks(k+1)%f
               vdevice%transmat%m(2,1:4) = vdevice%transmat%m(2,1:4) * tl%toks(k+2)%f
               vdevice%transmat%m(3,1:4) = vdevice%transmat%m(3,1:4) * tl%toks(k+3)%f
               k = k + 4
             case (OBJ_ROTATE)
               call draw_rotate(tl%toks(k+1)%f, char(tl%toks(k+2)%i)); k = k + 3
             case (OBJ_VFLUSH)
               k = k + 1
             case (OBJ_RECT)
               call draw_rect(tl%toks(k+1)%f, tl%toks(k+2)%f, tl%toks(k+3)%f, tl%toks(k+4)%f); k = k + 5
             case default
               write(error_unit, '(A, I0)') "UNEXPECTED OBJECT ", tl%toks(k)%i
               call draw_verror("draw: internal error in callobj")
            end select
         end do
         tl => tl%next
      end do
      if (sync /= 0) then
         vdevice%sync = 1
         idum = vdevice%dev%Vsync()
      end if
   end subroutine draw_callobj

!>
!!##NAME
!!    isobj(3f) - [M_draw:OBJECT] Returns .FALSE. if there is an object of number n.
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!         logical function isobj(n)
!!         integer :: n
!!
!!
!!##DESCRIPTION
!!    Returns .FALSE. if there is an object of number n.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_isobj
!!    use M_draw
!!    implicit none
!!    integer :: idum
!!       call prefsize(300, 300)
!!       call prefposition(100, 100)
!!       call vinit(' ')   ! set up device
!!       call ortho2(-5.0,5.0,-5.0,5.0)
!!       call color(D_WHITE)  ! set current color
!!       call clear()   ! clear screen to current color
!!
!!       call makeobj(3)  ! create a simple object
!!          call polyfill(.true.)
!!          call color(D_GREEN)
!!          call circle(0.0,0.0,4.0)
!!       call closeobj()
!!
!!       if(isobj(3))then
!!          write(*,*)' 3 is an object (CORRECT)'
!!          call callobj(3)
!!       else
!!          write(*,*)' 3 is not an object (ERROR)'
!!       endif
!!
!!       if(isobj(4))then
!!          write(*,*)' 4 is an object (ERROR)'
!!       else
!!          write(*,*)' 4 is not an object (CORRECT)'
!!       endif
!!
!!       call callobj(4) ! note: calling a non-existent object is a no-op
!!
!!       idum=getkey()! wait for some input
!!       call vexit()!  set the screen back to its original state
!!
!!    end program demo_isobj
!!
!!   Expected output:
!!
!!      3 is an object (CORRECT)
!!      4 is not an object (CORRECT)
   function draw_isobj(n)
      integer, intent(in) :: n
      type(Object), pointer :: o
      logical          :: draw_isobj
      o => object_table(obj_hash(n))%ptr
      do while (associated(o))
         if (o%obno == n) then
            draw_isobj = .true.; return
         end if
         o => o%next
      end do
      draw_isobj = .false.
   end function draw_isobj

end module m_draw_objects
