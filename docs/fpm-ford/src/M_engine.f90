module M_engine

use m_draw_move
use m_draw_draw
use m_draw_points
use m_draw_rect
use m_draw_arcs
use m_draw_curves
use m_draw_polygons

use m_draw_aspect
use m_draw_attr
use m_draw_buffer
use m_draw_clip
use m_draw_getgp
use m_draw_mapping
use m_draw_matrix
use m_draw_newtokens
use m_draw_objects
use m_draw_patches
use m_draw_pref
use m_draw_tensor
use m_draw_trans
use m_draw_types
use m_draw_valloc
use m_draw_verror
use m_draw_viewing
use m_draw_viewp

use m_draw_getstring
use m_draw_text
use m_draw_bitmap_fonts

use m_draw_drivers

use m_draw_drv_canvas
use m_draw_drv_cgmt
use m_draw_drv_fti
use m_draw_drv_gnu
use m_draw_drv_hpdxy
use m_draw_drv_hpgt
use m_draw_drv_mif
use m_draw_drv_nil
use m_draw_drv_pbm
use m_draw_drv_pcl5
use m_draw_drv_pdf
use m_draw_drv_ppm
use m_draw_drv_ps
use m_draw_drv_rtf
use m_draw_drv_svg
use m_draw_drv_tek
use m_draw_drv_unixplot
use m_draw_drv_usemap
use m_draw_drv_vml
use m_draw_drv_vog
use m_draw_drv_x11
use m_draw_drv_ansi
use m_draw_drv_sixel
use m_draw_drv_xfig

integer, parameter, public :: D_BLACK   = 0
integer, parameter, public :: D_RED     = 1
integer, parameter, public :: D_GREEN   = 2
integer, parameter, public :: D_YELLOW  = 3
integer, parameter, public :: D_BLUE    = 4
integer, parameter, public :: D_MAGENTA = 5
integer, parameter, public :: D_CYAN    = 6
integer, parameter, public :: D_WHITE   = 7

! Hershey text justification
public  ::  D_XCENTERED,  D_YCENTERED  ,  D_LEFT  ,  D_RIGHT  ,  D_TOP  ,  D_BOTTOM
end module M_engine
