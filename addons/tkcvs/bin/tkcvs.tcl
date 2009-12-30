#!/bin/sh
#-*-tcl-*-
# the next line restarts using wish \
exec wish "$0" -- ${1+"$@"}

set ScriptBin [file normalize [file dirname [info script]]]

set comms [list tkButtonInvoke tkTextSelectTo tkEntryInsert tkEntryBackspace \
    tk_textCut tk_textCopy tk_textPaste tk_focusNext tk_focusPrev tkTextClosestGap \
    tkTextAutoScan tkCancelRepeat]

foreach i $comms {
    auto_load $i
    if {![llength [info commands $i]]} {
	tk::unsupported::ExposePrivateCommand $i
    }
}
::tk::unsupported::ExposePrivateVariable *

#ramsan
# rename puts puts_base
# proc puts { args } {
#     if { [llength $args] == 2 } {
#         eval [concat puts_base $args]
#     } else {
#         tk_messageBox -message puts--$args -type ok
#     }
# }


#
# $Id: tkcvs.tcl,v 1.6 2009-03-25 23:47:52 ramsan Exp $
#
# TkCVS Main program -- A Tk interface to CVS.
#
# Uses a structured modules file -- see the manpage for more details.
#
# Author:  Del (del@babel.dialix.oz.au)
#

#


set TclExe [info nameofexecutable]
if {$tcl_platform(platform) == "windows"} {
  set TclExe [file attributes $TclExe -shortname]
}

#set TclRoot C:/tcltk/TkCvs/lib     ;# 2 b replaced by install skript
set TclRoot [file join [file dirname $ScriptBin] lib]

set TCDIR [file join $TclRoot tkcvs]
set GFDIR [file join $TclRoot tkcvs bitmaps]

#
# Constants for user configuration
# Change this variable to point to your bitmap directory.
set cvscfg(bitmapdir) "$GFDIR"
set cvscfg(editorargs) {}

set auto_path [linsert $auto_path 0 $TCDIR]

set cvscfg(allfiles) 0
set cvscfg(checkrecursive) {}
set cvscfg(startwindow) "workdir"

#   09-Jan-2000  lcs
#   Determine the "working mode".  This should be project specific later,
#   but for now it is controlled by this variable and the user's personal
#   configuration file.
#    -  Locking:        The traditional locking.
#    -  Concurrent:     The name sais it.
#
set cvscfg(workmode) "concurrent"

set cvscfg(usage) "Usage: tkcvs \[-dir directory\] \[-root cvsroot\] \[-win workdir|module\]"

set maxdirs 15
set dirlist {}
set totaldirs 0

# Read in defaults
if {[file exists [file join $TCDIR tkcvs_def.tcl]]} {
  source [file join $TCDIR tkcvs_def.tcl]
}
if {[file exists ~/.tkcvs]} {
  source ~/.tkcvs
}

#
# Global option to check out all files read-only if work mode is locking.
# Doing this after reading ~./tkcvs allows override by the user specific
# configuration file.
# 12-Jan-2000  lcs
#
if {$cvscfg(workmode) == "locking"} {
  set cvscfg(co_file_mode) "-r"
} else {
  set cvscfg(co_file_mode) ""
}


# Set some defaults
if { ! [info exists cvscfg(guifont)] } {
  set cvscfg(guifont) {Helvetica -12 bold}
}
if { ! [info exists cvscfg(listboxfont)] } {
  set cvscfg(listboxfont) {Helvetica -12 normal}
}
# Initialize logging (classes are CFTD)
if { ! [info exists cvscfg(log_classes)] } {
  set cvscfg(log_classes) "C"
}
foreach class [split $cvscfg(log_classes) {}] {
  set logclass($class) $class
}
if { ! [info exists cvscfg(logging)] } {
  set cvscfg(logging) 0
}
if {$cvscfg(logging)} {
  gen_log:init
}

#
# Command line options
#
for {set i 0} {$i < [llength $argv]} {incr i} {
  set arg [lindex $argv $i]
  set val [lindex $argv [expr {$i+1}]]
  switch -glob -- $arg {
    -dir {
      set dir $val; incr i
      cd $val
    }
    -root {
      set cvscfg(cvsroot) $val; incr i
    }
    -win {
      set cvscfg(startwindow) $val; incr i
    }
    -help {
      puts $cvscfg(usage)
      exit 0
    }
    {} {
	# nothing
    }
    default {
      puts arg=$arg--$cvscfg(usage)
      exit 1
    }
  }
}

if { ! [info exists cvscfg(cvsroot)] } {
  if { ! [info exists env(CVSROOT)] } {
    puts "warning: your \$CVSROOT environment variable is not set."
    set cvscfg(cvsroot) ""
  } else {
    set cvscfg(cvsroot) $env(CVSROOT)
  }
}
 
if {![info exists cvscfg(ignore_file_filter)]} {
  set cvscfg(ignore_file_filter) ""
}
# Remember what the setting was.  We'll have to restore it after
# leaving a directory with a .cvsignore file.
set cvscfg(default_ignore_filter) $cvscfg(ignore_file_filter)

if {![info exists cvscfg(papersize)]} {
  set cvscfg(papersize) "A4"
}
if {$cvscfg(papersize) == "A4"} {
  set cvscfg(ystart) 770
  set cvscfg(yend) 60
  set cvscfg(xstart) 25
  set cvscfg(xend) 580
} else {
  set cvscfg(ystart) 700
  set cvscfg(yend) 60
  set cvscfg(xstart) 25
  set cvscfg(xend) 580
}

set incvs 0

#
# Read in the bitmaps
#
image create photo Fileview -format gif -file [file join $cvscfg(bitmapdir) fileview.gif]
image create photo Fileedit -format gif -file [file join $cvscfg(bitmapdir) fileedit.gif]
image create photo Delete -format gif -file [file join $cvscfg(bitmapdir) delete.gif]
image create photo Clear -format gif -file [file join $cvscfg(bitmapdir) clear.gif]
image create photo Refresh -format gif -file [file join $cvscfg(bitmapdir) refresh.gif]
image create photo Branches -format gif -file [file join $cvscfg(bitmapdir) branch.gif]
image create photo Add -format gif -file [file join $cvscfg(bitmapdir) add.gif]
image create photo Remove -format gif -file [file join $cvscfg(bitmapdir) remove.gif]
image create photo Diff -format gif -file [file join $cvscfg(bitmapdir) diff.gif]
image create photo Checkin -format gif -file [file join $cvscfg(bitmapdir) checkin.gif]
image create photo Checkout -format gif -file [file join $cvscfg(bitmapdir) checkout.gif]
image create photo Modules -format gif -file [file join $cvscfg(bitmapdir) modules.gif]
image create photo Import -format gif -file [file join $cvscfg(bitmapdir) import.gif]

image create photo Export -format gif -file [file join $cvscfg(bitmapdir) export.gif]
image create photo Files -format gif -file [file join $cvscfg(bitmapdir) files.gif]
image create photo Patches -format gif -file [file join $cvscfg(bitmapdir) patches.gif]
image create photo Patchfile -format gif -file [file join $cvscfg(bitmapdir) patchfile.gif]
image create photo Tag -format gif -file [file join $cvscfg(bitmapdir) tag.gif]
image create photo Tags -format gif -file [file join $cvscfg(bitmapdir) tags.gif]
image create photo Branchtag -format gif -file [file join $cvscfg(bitmapdir) branchtag.gif]
image create photo Mergebranch -format gif -file [file join $cvscfg(bitmapdir) mergebranch.gif]
image create photo Mergediff -format gif -file [file join $cvscfg(bitmapdir) mergediff.gif]
image create photo Conflict -format gif -file [file join $cvscfg(bitmapdir) conflict.gif]
image create photo Who -format gif -file [file join $cvscfg(bitmapdir) who.gif]

image create photo dir -format gif -file [file join $cvscfg(bitmapdir) dir.gif]
image create photo mdir -format gif -file [file join $cvscfg(bitmapdir) mdir.gif]
image create photo mod -format gif -file [file join $cvscfg(bitmapdir) mod.gif]
image create photo adir -format gif -file [file join $cvscfg(bitmapdir) adir.gif]
image create photo amod -format gif -file [file join $cvscfg(bitmapdir) amod.gif]

foreach c [lsort [array names cvscfg]] {
  gen_log:log D "cvscfg($c) $cvscfg($c)"
}

# Create a window
workdir_setup
if {$cvscfg(startwindow) == "module"} {
  modbrowse_run $cvscfg(cvsroot)
  wm iconify .
}

