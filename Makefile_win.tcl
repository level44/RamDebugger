

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

set FREEWRAP /tcltk/freewrap44/freewrap.exe
set FREEWRAPTCLSH /utils/freewrapTCLSH
set TEXI2HTML {perl "/Gid Project/info/html-version/texi2html" \
    -split_node -menu}
set ZIP /utils/zip.exe

set ZIPFILES [list RamDebugger/RamDebugger.tcl RamDebugger/license.terms RamDebugger/Readme \
    RamDebugger/addons RamDebugger/Examples RamDebugger/help]

pack [text .t -width 70 -height 4]

if { [MustCompile help/RamDebugger/RamDebugger_toc.html RamDebugger.texinfo] } {
    set oldcwd [pwd]
    cd help/01RamDebugger
    eval [concat Execute $TEXI2HTML ../../RamDebugger.texinfo]
    cd $oldcwd
}

zipfile RamDebugger1.0.zip .. $ZIPFILES

update
after 500
exit
