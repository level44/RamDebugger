


proc LoadRamDebugger { dir } {

    if { [interp exists ramdebugger] } { interp delete ramdebugger }
    interp create ramdebugger
    interp alias ramdebugger master "" eval
    ramdebugger eval [list load {} Tk]
    ramdebugger eval { set argc 0 ; set argv "" }
    ramdebugger eval [list set argv0 [file join $dir RamDebugger.tcl]]
    ramdebugger eval [list source [file join $dir RamDebugger.tcl]]
    package provide RamDebugger 2.0
    update idletasks
}
if {![package vsatisfies [package provide Tcl] 8.3]} {return}
package ifneeded RamDebugger 2.0 [list LoadRamDebugger $dir]
