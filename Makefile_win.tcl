

proc zipfile { zipname directory files } {

    set cwd [pwd]
    file delete $zipname
    cd $directory
    .t ins end zipping... ; update
    set comm "exec \"$::ZIP\" \"[file join $cwd $zipname]\" -r $files"
    eval $comm
    .t ins end "done\n" ; update
    cd $cwd
}

proc CheckHiddenFiles { directory files } {

    set cwd [pwd]
    cd $directory

    set fail 0
    for { set i 0 } { $i < [llength $files] } { incr i } {
	set file [lindex $files $i]
	if { [file isdir $file] } {
	    eval lappend files [glob -nocomplain -dir $file *]
	}
	foreach j [glob -nocomplain -dir $file -types hidden *] {
	    .t ins end "Hidden file: $j\n" ; .t see end ; update
	    incr fail
	    file attributes $j -hidden 0
	}
	if { [file attributes $file -hidden] || [file attributes $file -system] } {
	    .t ins end "Hidden or system file: $file\n" ; .t see end ; update
	    incr fail
	    file attributes $file -hidden 0 -system 0
	}
    }
    cd $cwd
    if { $fail } {
	error "There are hidden or system files"
    }
}

proc MustCompile { file args } {

    if { ![file exists $file] } { return 1 }
    foreach i $args {
	if { [file mtime $i] > [file mtime $file] } {
	    return 1
	}
    }
    return 0
}

proc Execute { args } {
    .t ins end $args... ; update
    eval [concat exec $args]
    .t ins end "done\n"
}

set FREEWRAP /tcltk/freewrap44/freewrap.exe
set FREEWRAPTCLSH /utils/freewrapTCLSH
set TEXI2HTML {perl "/Gid Project/info/html-version/texi2html" \
    -split_node -menu}
set ZIP /utils/zip.exe

set ZIPFILES [list RamDebugger/RamDebugger.tcl RamDebugger/license.terms RamDebugger/Readme \
    RamDebugger/addons RamDebugger/scripts RamDebugger/Examples RamDebugger/help]

pack [text .t -width 80 -height 8] -fill both -expand 1

if { [MustCompile help/RamDebugger/RamDebugger_toc.html RamDebugger.texinfo] } {
    set oldcwd [pwd]
    cd help/01RamDebugger
    eval [concat Execute $TEXI2HTML ../../RamDebugger.texinfo]
    cd $oldcwd
}

CheckHiddenFiles .. $ZIPFILES
zipfile RamDebugger2.7.zip .. $ZIPFILES

update
after 500
exit
