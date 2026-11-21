module m_draw_types
   use, intrinsic :: iso_fortran_env, only: int32, int64, real32, error_unit, output_unit
   implicit none
   private

   ! Constants and pseudo-void return
   integer, parameter, public :: UNUSED = -999

   character(len=*), parameter, public :: FONTLIB = "/usr/share/hershey/"

   integer, parameter, public :: REALHARDWARE = 0
   integer, parameter, public :: HERSHEY      = 1
   integer, parameter, public :: SOFTHARDWARE = 2

   integer, parameter, public :: D_TRUE  = 1
   integer, parameter, public :: D_FALSE = 0

!   integer, parameter, public :: D_BLACK   = 0
!   integer, parameter, public :: D_RED     = 1
!   integer, parameter, public :: D_GREEN   = 2
!   integer, parameter, public :: D_YELLOW  = 3
!   integer, parameter, public :: D_BLUE    = 4
!   integer, parameter, public :: D_MAGENTA = 5
!   integer, parameter, public :: D_CYAN    = 6
!   integer, parameter, public :: D_WHITE   = 7
!
!   integer, parameter, public :: BLACK   = 0
!   integer, parameter, public :: RED     = 1
!   integer, parameter, public :: GREEN   = 2
!   integer, parameter, public :: YELLOW  = 3
!   integer, parameter, public :: BLUE    = 4
!   integer, parameter, public :: MAGENTA = 5
!   integer, parameter, public :: CYAN    = 6
!   integer, parameter, public :: WHITE   = 7
 
   ! Hershey text justification
   integer, parameter, public :: D_XCENTERED = 1
   integer, parameter, public :: D_YCENTERED = 2
   integer, parameter, public :: D_LEFT      = 4
   integer, parameter, public :: D_RIGHT     = 8
   integer, parameter, public :: D_TOP       = 16
   integer, parameter, public :: D_BOTTOM    = 32

   ! Text weights & line thickness
   integer, parameter, public :: D_NORMAL = 0
   integer, parameter, public :: D_BOLD   = 1
   integer, parameter, public :: D_THIN   = 0
   integer, parameter, public :: D_THICK  = 1

   ! Math constants
   real(real32), parameter, public :: PI  = 3.14159265358979323846_real32
   real(real32), parameter, public :: D2R = PI / 180.0_real32

   ! Limits
   integer, parameter, public :: MAXVERTS    = 99999
   integer, parameter, public :: FONTNAMELEN = 256
   integer, parameter, public :: MAXENTS     = 10100
   integer, parameter, public :: MAXTOKS     = 1000

   ! 1-based Vector indices (converted from 0..3)
   integer, parameter, public :: D_X = 1
   integer, parameter, public :: D_Y = 2
   integer, parameter, public :: D_Z = 3
   integer, parameter, public :: D_W = 4

   ! Object opcodes
   integer, parameter, public :: OBJ_ARC            = 1
   integer, parameter, public :: OBJ_BOXTEXT        = 2
   integer, parameter, public :: OBJ_CALLOBJ        = 3
   integer, parameter, public :: OBJ_CENTERTEXT     = 4
   integer, parameter, public :: OBJ_CIRCLE         = 5
   integer, parameter, public :: OBJ_CLEAR          = 6
   integer, parameter, public :: OBJ_COLOR          = 7
   integer, parameter, public :: OBJ_DRAW           = 8
   integer, parameter, public :: OBJ_DRAWCHAR       = 9
   integer, parameter, public :: OBJ_DRAWSTR        = 10
   integer, parameter, public :: OBJ_FIXEDWIDTH     = 11
   integer, parameter, public :: OBJ_FONT           = 12
   integer, parameter, public :: OBJ_HATCHANG       = 13
   integer, parameter, public :: OBJ_HATCHPITCH     = 14
   integer, parameter, public :: OBJ_LOADMATRIX     = 15
   integer, parameter, public :: OBJ_MAPCOLOR       = 16
   integer, parameter, public :: OBJ_MOVE           = 17
   integer, parameter, public :: OBJ_MULTMATRIX     = 18
   integer, parameter, public :: OBJ_POLY           = 19
   integer, parameter, public :: OBJ_POLYFILL       = 20
   integer, parameter, public :: OBJ_POLYHATCH      = 21
   integer, parameter, public :: OBJ_POPATTRIBUTES  = 22
   integer, parameter, public :: OBJ_POPMATRIX      = 23
   integer, parameter, public :: OBJ_POPVIEWPORT    = 24
   integer, parameter, public :: OBJ_PUSHATTRIBUTES = 25
   integer, parameter, public :: OBJ_PUSHMATRIX     = 26
   integer, parameter, public :: OBJ_PUSHVIEWPORT   = 27
   integer, parameter, public :: OBJ_RCURVE         = 28
   integer, parameter, public :: OBJ_RPATCH         = 29
   integer, parameter, public :: OBJ_SECTOR         = 30
   integer, parameter, public :: OBJ_TEXTANG        = 31
   integer, parameter, public :: OBJ_TEXTSIZE       = 32
   integer, parameter, public :: OBJ_VIEWPORT       = 33
   integer, parameter, public :: OBJ_BACKBUFFER     = 34
   integer, parameter, public :: OBJ_FRONTBUFFER    = 35
   integer, parameter, public :: OBJ_SWAPBUFFER     = 36
   integer, parameter, public :: OBJ_BACKFACING     = 37
   integer, parameter, public :: OBJ_TRANSLATE      = 38
   integer, parameter, public :: OBJ_ROTATE         = 39
   integer, parameter, public :: OBJ_SCALE          = 40
   integer, parameter, public :: OBJ_VFLUSH         = 41
   integer, parameter, public :: OBJ_RECT           = 42
   integer, parameter, public :: OBJ_LINEWIDTH      = 43
   integer, parameter, public :: OBJ_XCENTERTEXT    = 44
   integer, parameter, public :: OBJ_YCENTERTEXT    = 45
   integer, parameter, public :: OBJ_BOTTOMJUSTIFY  = 46
   integer, parameter, public :: OBJ_TOPJUSTIFY     = 47
   integer, parameter, public :: OBJ_TEXTJUSTIFY    = 48
   integer, parameter, public :: OBJ_RIGHTJUSTIFY   = 49
   integer, parameter, public :: OBJ_LEFTJUSTIFY    = 50
   integer, parameter, public :: OBJ_TEXTSLANT      = 51
   integer, parameter, public :: OBJ_DASHCODE       = 52
   integer, parameter, public :: OBJ_LINESTYLE      = 53

   ! Token representation (replaces C union)
   type, public :: Token
      integer :: i = 0
      real(real32) :: f = 0.0_real32
      character(len=:), allocatable :: s
   end type Token

   type, public :: TokList
      integer :: count = 0
      type(Token), allocatable :: toks(:)
      type(TokList), pointer :: next => null()
   end type TokList

   type, public :: Attribute
      character(len=:), allocatable :: style
      integer :: dash_pos = 1
      integer :: fill = 0
      integer :: hatch = 0
      integer :: backface = 0
      integer :: justify = 0
      integer :: inbackbuffer = 0
      integer :: exvp = 0
      integer :: softtext = 0
      integer :: fixedwidth = 0
      integer :: color = 0
      integer :: bold = 0
      real(real32) :: fontheight = 0.0_real32
      real(real32) :: fontwidth = 0.0_real32
      real(real32) :: skew = 0.0_real32
      real(real32) :: hatchcos = 1.0_real32
      real(real32) :: hatchsin = 0.0_real32
      real(real32) :: hatchpitch = 0.1_real32
      real(real32) :: textcos = 1.0_real32
      real(real32) :: textsin = 0.0_real32
      real(real32) :: dash = 0.0_real32
      real(real32) :: adist = 0.0_real32
      character(len=FONTNAMELEN) :: font = ""
   end type Attribute

   type, public :: View_port
      real(real32) :: left = 0.0_real32
      real(real32) :: right = 1.0_real32
      real(real32) :: bottom = 0.0_real32
      real(real32) :: top = 1.0_real32
   end type View_port

   type, public :: Mstack
      real(real32) :: m(4,4) = 0.0_real32
      type(Mstack), pointer :: back => null()
   end type Mstack

   type, public :: Astack
      type(Attribute) :: a
      type(Astack), pointer :: back => null()
   end type Astack

   type, public :: Vstack
      type(View_port) :: v
      type(Vstack), pointer :: back => null()
   end type Vstack

   ! Abstract interfaces for device driver procedure pointers
   abstract interface
      integer function dev_void_fn()
      end function dev_void_fn

      integer function dev_char_fn(c)
         character(len=1), intent(in) :: c
      end function dev_char_fn

      integer function dev_int_fn(i)
         integer, intent(in) :: i
      end function dev_int_fn

      integer function dev_int2_fn(x, y)
         integer, intent(in) :: x, y
      end function dev_int2_fn

      integer function dev_fill_fn(n, x, y)
         integer, intent(in) :: n
         integer, intent(in) :: x(n), y(n)
      end function dev_fill_fn

      integer function dev_str_fn(s)
         character(len=*), intent(in) :: s
      end function dev_str_fn

      integer function dev_loc_fn(x, y)
         integer, intent(out) :: x, y
      end function dev_loc_fn

      integer function dev_mapcolor_fn(i, r, g, b)
         integer, intent(in) :: i, r, g, b
      end function dev_mapcolor_fn

      subroutine poly_move_draw_fn(x, y, z)
         import :: real32
         real(real32), intent(in) :: x, y, z
      end subroutine poly_move_draw_fn
   end interface

   type, public :: DevEntry
      character(len=32) :: devname = ""
      character(len=32) :: large = "large"
      character(len=32) :: small = "small"
      procedure(dev_void_fn), pointer, nopass :: Vbackb => null()
      procedure(dev_char_fn), pointer, nopass :: Vchar => null()
      procedure(dev_void_fn), pointer, nopass :: Vcheckkey => null()
      procedure(dev_void_fn), pointer, nopass :: Vclear => null()
      procedure(dev_int_fn), pointer, nopass :: Vcolor => null()
      procedure(dev_int2_fn), pointer, nopass :: Vdraw => null()
      procedure(dev_void_fn), pointer, nopass :: Vexit => null()
      procedure(dev_fill_fn), pointer, nopass :: Vfill => null()
      procedure(dev_str_fn), pointer, nopass :: Vfont => null()
      procedure(dev_void_fn), pointer, nopass :: Vfrontb => null()
      procedure(dev_void_fn), pointer, nopass :: Vgetkey => null()
      procedure(dev_void_fn), pointer, nopass :: Vinit => null()
      procedure(dev_loc_fn), pointer, nopass :: Vlocator => null()
      procedure(dev_mapcolor_fn), pointer, nopass :: Vmapcolor => null()
      procedure(dev_int_fn), pointer, nopass :: Vsetlw => null()
      procedure(dev_str_fn), pointer, nopass :: Vstring => null()
      procedure(dev_void_fn), pointer, nopass :: Vswapb => null()
      procedure(dev_void_fn), pointer, nopass :: Vsync => null()
   end type DevEntry

   type, public :: Device

integer :: initialized = 0
integer :: writestoprocess = 0
integer :: clipoff = 0
integer :: inobject = 0
integer :: inpolygon = 0
integer :: upset = 0
integer :: cpVvalid = 0
integer :: sync = 1
integer :: clipplanes = 0
procedure(poly_move_draw_fn), pointer, nopass :: pmove => null()
procedure(poly_move_draw_fn), pointer, nopass :: pdraw => null()
type(TokList), pointer :: tokens => null()
type(Mstack), pointer :: transmat => null()
type(Astack), pointer :: attr => null()
type(Vstack), pointer :: viewport => null()
real(real32) :: hheight = 0.0_real32, hwidth = 0.0_real32
real(real32) :: hheightd = 0.0_real32, hwidthd = 0.0_real32
real(real32) :: cpW(4) = [0.0_real32, 0.0_real32, 0.0_real32, 1.0_real32]
real(real32) :: cpWtrans(4) = 0.0_real32
real(real32) :: upvector(4) = [0.0_real32, 1.0_real32, 0.0_real32, 0.0_real32]
integer :: depth = 0
integer :: maxVx = 0, minVx = 0
integer :: maxVy = 0, minVy = 0
integer :: sizeX = 0, sizeY = 0
integer :: sizeSx = 0, sizeSy = 0
integer :: cpVx = 0, cpVy = 0
type(DevEntry) :: dev
end type Device
! Global shared state
type(Device), public, save :: vdevice
integer, public, save :: draw_unit = output_unit
real(real32), public, save :: hardwidth(128) = 0.0_real32
end module m_draw_types
