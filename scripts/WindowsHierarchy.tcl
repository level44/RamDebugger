
################################################################################
#   Windows Hierarchy
################################################################################


proc RamDebugger::DisplayWindowsHierarchyInfo { canvas w x y } {
    variable TextMotionAfterId
    
    after cancel $TextMotionAfterId
    set TextMotionAfterId ""
    if { [winfo exists $canvas.help] } { destroy $canvas.help }

    if { $w != "" } {
	set TextMotionAfterId [after 100 RamDebugger::DisplayWindowsHierarchyInfoDo \
		$canvas $w $x $y]
    }
}

proc RamDebugger::DisplayWindowsHierarchyInfoDo { canvas w x y } {
    variable TextMotionAfterId

    set TextMotionAfterId ""
    set comm {
	set retval "WIDGET\n\n"
	if { [winfo class WIDGET] == "Toplevel" } {
	    append retval "TITLE: [wm title WIDGET]\n"
	}
	if { [winfo class WIDGET] == "Label" || [winfo class WIDGET] == "Button" || \
	   [winfo class WIDGET] == "Radiobutton" || [winfo class WIDGET] == "Checkbutton" } {
	    if { [WIDGET cget -text] != "" } {
		append retval "LABEL: [WIDGET cget -text]\n"
	    }
	}
	foreach "cols rows" [grid size WIDGET] break
	if { $cols > 0 } {
	    append retval "GRID MASTER\n"
	    for { set i 0 } { $i < $rows } { incr i } {
		for { set j 0 } { $j < $cols } { incr j } {
		    set slave [grid slaves WIDGET -row $i -column $j]
		    if { $slave != "" } {
		        append retval "    $i,$j $slave [grid info $slave]\n"
		    }
		}
	    }
	    append retval "  ROWS\n"
	    for { set i 0 } { $i < $rows } { incr i } {
		append retval "    $i [grid rowconfigure WIDGET $i]\n" 
	    }
	    append retval "  COLUMNS\n"
	    for { set j 0 } { $j < $cols } { incr j } {
		append retval "    $j [grid columnconfigure WIDGET $j]\n" 
	    }
	}
	set info [grid info WIDGET]
	if { $info != "" } {
	    append retval "GRID SLAVE\n$info\n"
	}
	if { [pack slaves WIDGET] != "" } {
	    append retval "PACK MASTER\n"
	    foreach i [pack slaves WIDGET] {
		append retval "    $i [pack info $i]\n"
	    }
	}
	if { ![catch [list pack info WIDGET] info] } {
	    append retval "PACK SLAVE\n$info\n"
	}
	append retval "OPTIONS\n"
	set retval_in ""
	foreach i [WIDGET configure] {
	    append retval_in "[lindex $i 0] [WIDGET cget [lindex $i 0]] "
	    while { [string length $retval_in] > 80 } {
		append retval [string range $retval_in 0 79]\n
		set retval_in [string range $retval_in 80 end]
	    }
	    if { [string length $retval_in] > 40 } {
		append retval $retval_in\n
		set retval_in ""
	    }
	}
	if { [string index $retval_in end] != "\n" } {
	    append retval_in \n
	}
	append retval $retval_in
	append retval "SIZES\n"
	append retval "    width=[winfo width WIDGET] reqwidth=[winfo reqwidth WIDGET]\n"
	append retval "    height=[winfo height WIDGET] reqheight=[winfo reqheight WIDGET]\n"
	EVAL
    }
    if { $DialogWinHier::user(type) == "ramdebugger" } {
	set retcomm "RamDebugger::DisplayWindowsHierarchyInfoDo2 $canvas $w $x $y \[set retval]"
    } else {
	set retcomm "RDC::SendDev \[list RamDebugger::DisplayWindowsHierarchyInfoDo2 $canvas \
		$w $x $y \[set retval]]"
    }
    set comm [string map [list EVAL $retcomm] $comm]
    set comm [string map [list WIDGET $w] $comm]
    if { $DialogWinHier::user(type) == "ramdebugger" } {
	eval $comm
    } else {
	EvalRemote $comm
    }
}

proc RamDebugger::DisplayWindowsHierarchyInfoDo2 { canvas w x y res } {

    set w $canvas.help
    if { [winfo exists $w] } { destroy $w }
    toplevel $w
    wm overrideredirect $w 1
    wm transient $w $canvas

    pack [label $w.l -bg white -justify left -anchor w]
    $w.l conf -bd 1 -relief solid
    $w.l conf -text $res
    bind $w <Motion> "destroy $w"

    wm withdraw $w
    update idletasks

    incr x 5
    incr y 5

    if { $x+[winfo reqwidth $w]+5 > [winfo screenwidth $w] } {
	set x [expr [winfo screenwidth $w]-[winfo reqwidth $w]-5]
    }
    if { $x < 5 } { set x 5 }

    if { $y+[winfo reqheight $w]+5 > [winfo screenheight $w] } {
	set y [expr [winfo screenheight $w]-[winfo reqheight $w]-5]
    }
    if { $y < 0 } { set y 5 }

    wm geom $w +$x+$y
    wm deiconify $w

    focus $w.l
    bind $w.l <Control-c> "clipboard clear; clipboard append [list $res]"

}

proc RamDebugger::DisplayWindowsHierarchyDoDraw { canvas list x y linespace } {

    set maxwidth 0
    foreach "w info rest" $list {
	foreach i $info {
	    set width [font measure NormalFont $i]
	    if { $width > $maxwidth } { set maxwidth $width }
	}
    }
    set maxwidth [expr $maxwidth+10]

    set maxx [expr $x+$maxwidth]
    set maxy $y
    set ty $y
    set num 0
    foreach "w info rest" $list {
	if { $rest != "-" } { 
	    foreach "newx newy" [DisplayWindowsHierarchyDoDraw $canvas $rest [expr $x+$maxwidth] \
		$maxy $linespace] break
	    if { $newx > $maxx } { set maxx $newx}
	    set ty [expr ($maxy+$newy)/2-$linespace/2]
	    set maxy $newy

	    set width 0
	    foreach i $info {
		set widthi [font measure NormalFont $i]
		if { $widthi > $width } { set width $widthi }
	    }

	    $canvas create line [expr $x+$width+2] [expr $ty+$linespace/2] \
		[expr $x+$maxwidth-5] [expr $ty+$linespace/2] -width 2 -fill red -tags "items $w"
	    incr maxy 12
	}
	set inum 0
	foreach i $info {
	    if { $i != "" } {
		switch $inum {
		    0 { set color black }
		    1 { set color blue }
		    2 { set color green }
		}
		$canvas create text $x $ty -text $i -tags "items $w" -font NormalFont -anchor nw \
		    -fill $color
		set ty [expr $ty+$linespace]
	    }
	    incr inum
	}
	$canvas bind $w <Enter> "RamDebugger::DisplayWindowsHierarchyInfo $canvas $w %X %Y"
	$canvas bind $w <Leave> "RamDebugger::DisplayWindowsHierarchyInfo $canvas {} %X %Y"

	if { $ty > $maxy } {
	    set maxy $ty
	} else { set ty $maxy }

	incr num
    }
    if { $num > 1 } {
	$canvas create line [expr $x-5] $y [expr $x-1] [expr $y-4] -width 2 -fill red -tags items
	$canvas create line [expr $x-5] $y [expr $x-5] $maxy -width 2 -fill red -tags items
	$canvas create line [expr $x-5] $maxy [expr $x-1] [expr $maxy+4] -width 2 -fill red -tags items
    }
    return [list $maxx $maxy]
}

proc RamDebugger::DisplayWindowsHierarchyDo { what { res "" } } {
    variable remoteserver

    if { $what == "do" } {
	set comm {
	    namespace eval ::RDC {}
	    proc ::RDC::WindowsHierarchy { w } {
		set info [list "[winfo name $w] [winfo class $w]"]
		if { [winfo class $w] == "Label" || [winfo class $w] == "Button" || \
		    [winfo class $w] == "Radiobutton" || [winfo class $w] == "Checkbutton" } {
		    if { [$w cget -text] != "" } {
		        lappend info [$w cget -text]
		    }
		} elseif { [winfo class $w] == "Toplevel" && [wm title $w] != "" } {
		    lappend info "" [wm title $w]
		}
		set retval [list $w $info]
		set retval_in ""
		foreach i [winfo children $w] {
		    eval lappend retval_in [RDC::WindowsHierarchy $i]
		}
		if { $retval_in == "" } {
		    lappend retval -
		} else { lappend retval $retval_in }
		return $retval
	    }
	    EVAL
	}
	if { $DialogWinHier::user(type) == "ramdebugger" } {
	    set retcomm "RamDebugger::DisplayWindowsHierarchyDo res \[::RDC::WindowsHierarchy .]"
	} else {
	    set retcomm "RDC::SendDev \[list RamDebugger::DisplayWindowsHierarchyDo res \
		        \[::RDC::WindowsHierarchy .]]"
	}
	set comm [string map [list EVAL $retcomm] $comm]

	if { [info exists RamDebugger::DisplayWindowsHierarchyFindLastId] } {
	    unset RamDebugger::DisplayWindowsHierarchyFindLastId
	}

	if { $DialogWinHier::user(type) == "ramdebugger" } {
	    eval $comm
	} else {
	    if { $remoteserver == "" } {
		after 100 [list WarnWin "Error: there is no debugged application" \
		    $DialogWinHier::user(canvas)]
		$DialogWinHier::user(canvas) delete items
		return
	    }
	    EvalRemote $comm
	}
    } else {
	$DialogWinHier::user(canvas) delete items
	set linespace [expr [font metrics NormalFont -linespace]+1]

	foreach "newx newy" [DisplayWindowsHierarchyDoDraw $DialogWinHier::user(canvas) $res \
	    5 5 $linespace] break

	$DialogWinHier::user(canvas) conf -scrollregion [list 0 0 $newx $newy]
    }
}

proc RamDebugger::DisplayWindowsHierarchyFind {} {
    variable DisplayWindowsHierarchyFindLastId

    set canvas $DialogWinHier::user(canvas)

    if { ![info exists DisplayWindowsHierarchyFindLastId] } {
	set DisplayWindowsHierarchyFindLastId -1
    }
    set found 0
    foreach i [$canvas find all] {
	if { $DisplayWindowsHierarchyFindLastId > -1 && $i <= $DisplayWindowsHierarchyFindLastId } {
	    continue
	}
	if { [$canvas type $i] == "text" } {
	    set text [$canvas itemcget $i -text]
	    if { [string match -nocase *$DialogWinHier::user(find)* $text] } {
		set found 1
		set DisplayWindowsHierarchyFindLastId $i
		break
	    }
	}
    }
    if { $found } {
	$canvas select from $i 0
	$canvas select to $i end
	foreach "x y" [$canvas coords $i] break
	foreach "- - width height" [$canvas cget -scrollregion] break
	
	foreach "f1 f2" [$canvas xview] break
	set xs [expr $x/double($width)-($f2-$f1)/2.0]
	if { $xs < 0 } { set xs 0 }
	$canvas xview moveto $xs

	foreach "f1 f2" [$canvas yview] break
	set ys [expr $y/double($height)-($f2-$f1)/2.0]
	if { $ys < 0 } { set ys 0 }
	$canvas yview moveto $ys
    } else {
	unset RamDebugger::DisplayWindowsHierarchyFindLastId
	bell
    }
}

proc RamDebugger::DisplayWindowsHierarchy {} {
    variable text

    CopyNamespace ::DialogWin ::DialogWinHier

    set f [DialogWinHier::Init $text "Windows hierarchy" separator "" -]
    set w [winfo toplevel $f]

    radiobutton $f.r1 -text "In debugged app" -variable DialogWinHier::user(type) -value debug \
       -command "RamDebugger::DisplayWindowsHierarchyDo do" -grid "0 w"
    radiobutton $f.r2 -text "In RamDebugger" -variable DialogWinHier::user(type) -value ramdebugger \
       -command "RamDebugger::DisplayWindowsHierarchyDo do" -grid "1 w"

    frame $f.f -grid "0 2"
    label $f.f.l -text "Find:" -grid 0
    entry $f.f.e -textvar DialogWinHier::user(find) -grid 1

    tkTabToWindow $f.f.e

    button $f.f.b1 -text Go -width 5 -grid "2 px3 py3" -command \
       "RamDebugger::DisplayWindowsHierarchyFind"
    bind $f.f.e <Return> "$f.f.b1 invoke"

    set DialogWinHier::user(type) debug

    set sw [ScrolledWindow $f.lf -relief sunken -borderwidth 0 -grid "0 2"]
    set DialogWinHier::user(canvas) [canvas $sw.t -width 600 -height 400 -bg white -bd 0 ]
    $sw setwidget $DialogWinHier::user(canvas)

    bind $DialogWinHier::user(canvas) <2> {
	%W scan mark %x %y
	set tkPriv(x) %x
	set tkPriv(y) %y
	set tkPriv(mouseMoved) 0
    }
    bind $DialogWinHier::user(canvas) <B2-Motion> {
	if {(%x != $tkPriv(x)) || (%y != $tkPriv(y))} {
	    set tkPriv(mouseMoved) 1
	}
	if {$tkPriv(mouseMoved)} {
	    %W scan dragto %x %y
	}
    }

    supergrid::go $f

    DisplayWindowsHierarchyDo do

    set action [DialogWinHier::CreateWindow]
    DialogWinHier::DestroyWindow
    namespace delete ::DialogWinHier
}
