

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

proc Rename { tofile fromfile_tcl } {

    if { $::tcl_platform(platform) == "windows" } {
	set from [file root $fromfile_tcl].exe
    } else {
	set from [file root $fromfile_tcl]
    }
    file rename -force $from $from.back
    file rename -force $from.back $tofile
}

set FREEWRAP /tcltk/freewrap53b/freewrap.exe
set FREEWRAPTCLSH /utils/freewrapTCLSH
set TEXI2HTML {perl "/Gid Project/info/html-version/texi2html" \
    -split_node -menu}
set ZIP /utils/zip.exe

set ZIPFILES [list RamDebugger/RamDebugger.tcl RamDebugger/license.terms RamDebugger/Readme \
    RamDebugger/addons RamDebugger/scripts RamDebugger/Examples RamDebugger/help]

set files [list RamDebugger.tcl]

pack [text .t -width 70 -height 4]

if { [eval MustCompile RamDebugger.exe $files] } {
    file delete RamDebugger.exe
    eval [concat Execute $FREEWRAP -e $files]
}

update
after 500
exit
