#
# TCL Library
#

#
# $Id: errors.tcl,v 1.1.1.1 2001-06-07 15:03:42 ramsan Exp $
#
# Procedures for unimplemented procedures and error messages used by
# TkCVS.
#

proc notyet {} {

  set mess "This is not implemented yet.\nPlease try again in a later version."

  set title {Not Implemented Yet!}
  tk_dialog .notyet $title "$mess" error 0 Dismiss
}

proc cvsok {mess} {
#
# Sometimes cancel is meaningless, we just want
# and acknowlegement
#
  set title {Acknowledge!}
  tk_dialog .confirm $title $mess warning 0 OK
}

proc cvsconfirm {mess} {

  set title {Confirm!}
  tk_dialog .confirm $title $mess warning 0 OK Cancel
}

proc cvsfail {mess} {
  set title {TkCVS Warning!}
  tk_dialog .warn $title $mess warning 0 Dismiss
}

proc cvserror {mess} {
#
# Suggest reserving this for fatal errors that
# warrant an exit from tkcvs -dr
#
  set title {TkCVS Error!}
  tk_dialog .notyet $title $mess error 0 {Bye Bye}
  exit 1
}

