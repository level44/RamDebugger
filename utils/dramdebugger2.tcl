
set g1 720x845+0+0
set g2 750x600-0+0
set big_icons 0

if { [lindex $argv 0] eq "-b" } {
    set big_icons 1
    set argv [lrange $argv 1 end]
}

set wish [info nameofexecutable]

exec $wish $env(HOME)/mytcltk/RamDebugger/RamDebugger.tcl -rgeometry $g1 -big_icons $big_icons {*}$argv &
exec $wish $env(HOME)/mytcltk/RamDebugger/RamDebugger.tcl -rgeometry $g2 -big_icons $big_icons -prefs_group win2 &
exit
