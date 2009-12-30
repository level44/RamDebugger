#
# $Id: static.tcl,v 1.1.1.1 2001-06-07 15:03:42 ramsan Exp $
#
#  DESCRIPTION
#      This file contains a procedure written in Tcl that supports static 
#      variables.
#
#+ static - static variables support
#
#    This procedure supports static variables for a procedure whole through
#    Tcl code.  This procedure is based on code from the authors of the
#    tclX extension.
#
# REQUIREMENTS
#    None.
#
# RETURNS
#    Nothing.
#
# EXAMPLE
#    static {foo 10} bar
#
# CAVEATES
#    None.
#
proc static {args} {
    global staticvars
    set procName [lindex [info level -1] 0]
    foreach varPair $args {
	set varName [lindex $varPair 0]
	if {[llength $varPair] != 1} {
	    set varValue [lrange $varPair 1 end]
	} else {
	    set varValue {}
	}
	if {! [info exists staticvars($procName:$varName)]} {
	    set staticvars($procName:$varName) $varValue
	}
	uplevel 1 "upvar #0 staticvars($procName:$varName) $varName"
    }
}
