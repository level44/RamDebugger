
set Version 3.1

proc LoadRamDebugger { dir version } {

    if { [interp exists ramdebugger] } { interp delete ramdebugger }
    interp create ramdebugger
    interp alias ramdebugger master "" eval
    ramdebugger eval [list load {} Tk]
    ramdebugger eval { set argc 0 ; set argv "" }
    ramdebugger eval [list set auto_path $::auto_path]
    ramdebugger eval [list set argv0 [file join $dir RamDebugger.tcl]]
    ramdebugger eval [list source [file join $dir RamDebugger.tcl]]
    package provide RamDebugger $version
    update idletasks
}
if {![package vsatisfies [package provide Tcl] 8.3]} {return}
package ifneeded RamDebugger $Version [list LoadRamDebugger $dir $Version]
