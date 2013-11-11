#!/bin/sh
# use -*-Tcl-*- \
    exec tclsh "$0" "$@"

package require Tk

cd $env(HOME)

lassign [list 0 0 [winfo screenwidth .] [winfo screenheight .]] x0 y0 x1 y1
set err [catch { package require twapi }]
if { !$err } {
    lassign [twapi::get_desktop_workarea] x0 y0 x1 y1
}
set width [expr {$x1-$x0}]
set height [expr {$y1-$y0}]

if { $height < 1024 } {
    set g1 700x845+$x0+$y0
    set g2 660x516+[expr {$x0+715}]+$y0
} else {
    set g1 [expr {$width-550-25}]x970+$x0+$y0
    set g2 550x600-0+$y0
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
