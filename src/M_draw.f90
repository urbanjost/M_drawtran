module M_draw
use M_core

use M_oncore, only : page
use M_oncore, only : print
use M_oncore, only : pop
use M_oncore, only : push
use M_oncore, only : invokeobj

use M_core, only : CalcW2Vcoeffs      => draw_CalcW2Vcoeffs
use M_core, only : VtoWxy             => draw_VtoWxy
use M_core, only : WtoVx              => draw_WtoVx
use M_core, only : WtoVy              => draw_WtoVy
use M_core, only : arc                => draw_arc
use M_core, only : arcprecision       => draw_arcprecision
use M_core, only : backbuffer         => draw_backbuffer
use M_core, only : backface           => draw_backface
use M_core, only : backfacedir        => draw_backfacedir
use M_core, only : bottomjustify      => draw_bottomjustify
use M_core, only : boxfit             => draw_boxfit
use M_core, only : boxtext            => draw_boxtext
use M_core, only : callobj            => draw_callobj
use M_core, only : centertext         => draw_centertext
use M_core, only : checkkey           => draw_checkkey
use M_core, only : circle             => draw_circle
use M_core, only : circleprecision    => draw_circleprecision
use M_core, only : clear              => draw_clear
use M_core, only : clip               => draw_clip
use M_core, only : clipping           => draw_clipping
use M_core, only : closeobj           => draw_closeobj
use M_core, only : closepoly          => draw_closepoly
use M_core, only : color              => draw_color
use M_core, only : copymatrix         => draw_copymatrix
use M_core, only : copytensor         => draw_copytensor
use M_core, only : copytensortrans    => draw_copytensortrans
use M_core, only : copytranspose      => draw_copytranspose
use M_core, only : copyvector         => draw_copyvector
use M_core, only : curve              => draw_curve
use M_core, only : curvebasis         => draw_curvebasis
use M_core, only : curven             => draw_curven
use M_core, only : curveprecision     => draw_curveprecision
use M_core, only : dashcode           => draw_dashcode
use M_core, only : dashline           => draw_dashline
use M_core, only : delobj             => draw_delobj
use M_core, only : draw               => draw_draw
use M_core, only : draw2              => draw_draw2
use M_core, only : drawchar           => draw_drawchar
use M_core, only : drawstr            => draw_drawstr
use M_core, only : drcurve            => draw_drcurve
use M_core, only : drpatch            => draw_drpatch
use M_core, only : expandviewport     => draw_expandviewport
use M_core, only : fixedwidth         => draw_fixedwidth
use M_core, only : font               => draw_font
use M_core, only : frontbuffer        => draw_frontbuffer
use M_core, only : genobj             => draw_genobj
use M_core, only : getaspect          => draw_getaspect
use M_core, only : getcharsize        => draw_getcharsize
use M_core, only : getdepth           => draw_getdepth
use M_core, only : getdisplaysize     => draw_getdisplaysize
use M_core, only : getfactors         => draw_getfactors
use M_core, only : getfontasc         => draw_getfontasc
use M_core, only : getfontdec         => draw_getfontdec
use M_core, only : getfontheight      => draw_getfontheight
use M_core, only : getfontsize        => draw_getfontsize
use M_core, only : getfontwidth       => draw_getfontwidth
use M_core, only : getgp              => draw_getgp
use M_core, only : getgp2             => draw_getgp2
use M_core, only : getgpt             => draw_getgpt
use M_core, only : getkey             => draw_getkey
use M_core, only : getmatrix          => draw_getmatrix
use M_core, only : getopenobj         => draw_getopenobj
!use M_core, only : getplanes          => draw_getplanes
use M_core, only : getprefposandsize  => draw_getprefposandsize
use M_core, only : getstring          => draw_getstring
use M_core, only : getviewport        => draw_getviewport
use M_core, only : hatchang           => draw_hatchang
use M_core, only : hatchpitch         => draw_hatchpitch
use M_core, only : hershfont          => draw_hershfont
use M_core, only : identmatrix        => draw_identmatrix
use M_core, only : isobj              => draw_isobj
use M_core, only : leftjustify        => draw_leftjustify
use M_core, only : linestyle          => draw_linestyle
use M_core, only : linewidth          => draw_linewidth
use M_core, only : loadmatrix         => draw_loadmatrix
use M_core, only : loadobj            => draw_loadobj
use M_core, only : locator            => draw_locator
use M_core, only : lookat             => draw_lookat
use M_core, only : makeobj            => draw_makeobj
use M_core, only : makepoly           => draw_makepoly
use M_core, only : mapcolor_int16     => draw_mapcolor
use M_core, only : move               => draw_move
use M_core, only : move2              => draw_move2
use M_core, only : mult4x4            => draw_mult4x4
use M_core, only : multmatrix         => draw_multmatrix
use M_core, only : multtensor         => draw_multtensor
use M_core, only : multvector         => draw_multvector
use M_core, only : newtokens          => draw_newtokens
use M_core, only : numchars           => draw_numchars
use M_core, only : ortho              => draw_ortho
use M_core, only : ortho2             => draw_ortho2
use M_core, only : patch              => draw_patch
use M_core, only : patchbasis         => draw_patchbasis
use M_core, only : patchcurves        => draw_patchcurves
use M_core, only : patchprecision     => draw_patchprecision
use M_core, only : pdraw              => draw_pdraw
use M_core, only : perspective        => draw_perspective
use M_core, only : pmove              => draw_pmove
use M_core, only : point              => draw_point
use M_core, only : point2             => draw_point2
use M_core, only : polarview          => draw_polarview
use M_core, only : poly               => draw_poly
use M_core, only : poly2              => draw_poly2
use M_core, only : polyfill           => draw_polyfill
use M_core, only : polyhatch          => draw_polyhatch
use M_core, only : polyobj            => draw_polyobj
use M_core, only : popattributes      => draw_popattributes
use M_core, only : popdev             => draw_popdev
use M_core, only : popmatrix          => draw_popmatrix
use M_core, only : popviewport        => draw_popviewport
use M_core, only : prefposition       => draw_prefposition
use M_core, only : prefsize           => draw_prefsize
use M_core, only : premulttensor      => draw_premulttensor
use M_core, only : premultvector      => draw_premultvector
use M_core, only : printattribs       => draw_printattribs
use M_core, only : printmat           => draw_printmat
use M_core, only : printvdevice       => draw_printvdevice
use M_core, only : printvect          => draw_printvect
use M_core, only : pushattributes     => draw_pushattributes
use M_core, only : pushdev            => draw_pushdev
use M_core, only : pushmatrix         => draw_pushmatrix
use M_core, only : pushviewport       => draw_pushviewport
use M_core, only : quickclip          => draw_quickclip
use M_core, only : rcurve             => draw_rcurve
use M_core, only : rdraw              => draw_rdraw
use M_core, only : rdraw2             => draw_rdraw2
use M_core, only : rect               => draw_rect
use M_core, only : rightjustify       => draw_rightjustify
use M_core, only : rmove              => draw_rmove
use M_core, only : rmove2             => draw_rmove2
use M_core, only : rotate             => draw_rotate
use M_core, only : rpatch             => draw_rpatch
use M_core, only : rsdraw2            => draw_rsdraw2
use M_core, only : rsmove2            => draw_rsmove2
use M_core, only : saveobj            => draw_saveobj
use M_core, only : scale              => draw_scale
use M_core, only : sdraw2             => draw_sdraw2
use M_core, only : sector             => draw_sector
use M_core, only : sgetgp2            => draw_sgetgp2
use M_core, only : slocator           => draw_slocator
use M_core, only : smove2             => draw_smove2
use M_core, only : spoint2            => draw_spoint2
use M_core, only : srect              => draw_srect
use M_core, only : strlength          => draw_strlength
use M_core, only : swapbuffers        => draw_swapbuffers
use M_core, only : textang            => draw_textang
use M_core, only : textjustify        => draw_textjustify
use M_core, only : textsize           => draw_textsize
use M_core, only : textslant          => draw_textslant
use M_core, only : textweight         => draw_textweight
use M_core, only : topjustify         => draw_topjustify
use M_core, only : transformtensor    => draw_transformtensor
use M_core, only : translate          => draw_translate
use M_core, only : unexpandviewport   => draw_unexpandviewport
use M_core, only : up                 => draw_up
!use M_core, only : vallocate          => draw_vallocate
use M_core, only : verror             => draw_verror
use M_core, only : vexit              => draw_vexit
use M_core, only : vflush             => draw_vflush
use M_core, only : vfree              => draw_vfree
use M_core, only : vgetdev            => draw_vgetdev
use M_core, only : viewport           => draw_viewport
use M_core, only : vinit              => draw_vinit
use M_core, only : vnewdev            => draw_vnewdev
use M_core, only : voutput            => draw_voutput
use M_core, only : vsetflush          => draw_vsetflush
use M_core, only : window             => draw_window
use M_core, only : xcentertext        => draw_xcentertext
use M_core, only : ycentertext        => draw_ycentertext
!use M_core, only : yobbarays          => draw_yobbarays
private

private :: CalcW2Vcoeffs
private :: VtoWxy
private :: WtoVx
private :: WtoVy

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
