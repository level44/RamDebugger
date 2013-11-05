#!/bin/sh
# use -*-Tcl-*- \
    exec tclsh "$0" "$@"

package require Tk

cd $env(HOME)

if { [winfo screenheight .] < 1024 } {
    set g1 700x845+0+0
    set g2 654x525+715+0
#     set g1 720x845+0+0
#     set g2 750x600-0+0
} else {
    set g1 699x970+0+0
    set g2 550x600-0+0
}

set big_icons 0
set zero_one_two 2
set foreground 0

while { [string match "-*" [lindex $argv 0]] } {
    switch -- [lindex $argv 0] {
	"-0" { set zero_one_two 0 }
	"-1" { set zero_one_two 1 }
	"-2" { set zero_one_two 2 }
	"-b" { set big_icons 1 }
	"-f" { set foreground 1 }
	default {
	    puts "$argv0 ?-0? ?-1? ?-2? ?-b? ?-f? ?-h? ?filename?"
	    exit
	}
    }
    set argv [lrange $argv 1 end]
}

set wish [info nameofexecutable]

if { $zero_one_two == 2 } {
    exec $wish $env(HOME)/mytcltk/RamDebugger/RamDebugger.tcl -rgeometry $g2 \
	-big_icons $big_icons -prefs_group win2 -check_remotes 0 &
}

if { $zero_one_two == 0 } {
    set g1 ""
}

if { !$foreground } {
    exec $wish $env(HOME)/mytcltk/RamDebugger/RamDebugger.tcl -rgeometry $g1 \
	-big_icons $big_icons {*}$argv &
} else {
    exec $wish $env(HOME)/mytcltk/RamDebugger/RamDebugger.tcl -rgeometry $g1 \
	-big_icons $big_icons {*}$argv
}

exit
