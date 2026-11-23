module M_oncore
use M_engine
private
public :: page
public :: print
public :: pop
public :: push
public :: invokeobj

interface page
   module procedure :: biggest_ortho2
   module procedure :: page_rri
end interface page

contains
include "page.inc"
include "print.inc"
include "pop.inc"
include "push.inc"
include "invokeobj.inc"
end module M_oncore
