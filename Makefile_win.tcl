

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

pack [text .t -width 70 -height 4]

if { [MustCompile help/RamDebugger/RamDebugger_toc.html RamDebugger.texinfo] } {
    set oldcwd [pwd]
    cd help/RamDebugger
    eval [concat Execute $TEXI2HTML ../../RamDebugger.texinfo]
    cd $oldcwd
}


update
after 500
exit
