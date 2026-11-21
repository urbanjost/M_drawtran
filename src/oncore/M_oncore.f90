module M_oncore
use M_core
private
public :: page
public :: print
public :: pop
public :: push
public :: invokeobj
contains
include "page.inc"
include "print.inc"
include "pop.inc"
include "push.inc"
include "invokeobj.inc"
end module M_oncore
