
################################################################################
#   Windows Hierarchy
################################################################################


proc RamDebugger::DisplayWindowsHierarchyInfo { w canvas widget x y } {
    variable TextMotionAfterId
    
    after cancel $TextMotionAfterId
    set TextMotionAfterId ""
    if { [winfo exists $canvas.help] } { destroy $canvas.help }

    if { $widget != "" } {
	set TextMotionAfterId [after 1000 RamDebugger::DisplayWindowsHierarchyInfoDo \
		$w $canvas $widget $x $y]
    }
}

proc RamDebugger::DisplayWindowsHierarchyInfoDo { w canvas widget x y } {
    variable TextMotionAfterId

    if { abs($x-[winfo pointerx .])> 6 || abs($y-[winfo pointery .])> 6 } { return }

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
	if { ![catch [list WIDGET panes] info] } {
	    append retval "PANEDWINDOW MASTER\n"
	    foreach i [WIDGET panes] {
		append retval "    $i [WIDGET paneconfigure $i]\n"
	    }
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
	append retval "BINDTAGS\n[bindtags WIDGET]\n"
	EVAL
    }
    if { $DialogWinTop::user($w,type) == "ramdebugger" } {
	set retcomm "RamDebugger::DisplayWindowsHierarchyInfoDo2 $canvas $x $y \
		\[set retval]"
    } else {
	set retcomm "RDC::SendDev \[list RamDebugger::DisplayWindowsHierarchyInfoDo2 $canvas \
		$x $y \[set retval]]"
    }
    set comm [string map [list EVAL $retcomm] $comm]
    set comm [string map [list WIDGET $widget] $comm]
    if { $DialogWinTop::user($w,type) == "ramdebugger" } {
	eval $comm
    } else {
	EvalRemote $comm
    }
}

proc RamDebugger::DisplayWindowsHierarchyInfoDo2 { canvas x y res } {

    set w $canvas.help
    if { [winfo exists $w] } { destroy $w }
    toplevel $w
    wm overrideredirect $w 1
    wm transient $w $canvas

    $w configure -highlightthicknes 1 -highlightbackground grey \
	-highlightcolor grey
    pack [label $w.l -fg black -justify left -anchor w -bg grey95]
    $w.l conf -bd 0

    append res "\nPress Ctrl-x to copy widget name to clipboard. Ctrl-c to copy all"

    $w.l conf -text $res

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

    focus -force $w.l

    wm geom $w +$x+$y
    wm deiconify $w
    update
    bind $w <Motion> "destroy $w"

    set widgetname [lindex [split $res \n] 0]
    bind $w.l <Control-x> "clipboard clear; [list clipboard append $widgetname]"
    bind $w.l <Control-c> "clipboard clear; [list clipboard append $res]"

}

proc RamDebugger::DisplayWindowsHierarchyDoDraw { w canvas list x y linespace } {

    set maxwidth 0
    foreach "widget info rest" $list {
	foreach i $info {
	    set width [font measure WindowsHierarchyFont $i]
	    if { $width > $maxwidth } { set maxwidth $width }
	}
    }
    set maxwidth [expr $maxwidth+10]

    set maxx [expr $x+$maxwidth]
    set maxy $y
    set ty $y
    set num 0
    foreach "widget info rest" $list {
	if { $rest != "-" } { 
	    foreach "newx newy" [DisplayWindowsHierarchyDoDraw $w $canvas $rest \
		[expr $x+$maxwidth] $maxy $linespace] break
	    if { $newx > $maxx } { set maxx $newx}
	    set ty [expr ($maxy+$newy)/2-$linespace/2]
	    set maxy $newy

	    set width 0
	    foreach i $info {
		set widthi [font measure WindowsHierarchyFont $i]
		if { $widthi > $width } { set width $widthi }
	    }

	    $canvas create line [expr $x+$width+2] [expr $ty+$linespace/2] \
		[expr $x+$maxwidth-5] [expr $ty+$linespace/2] -width 2 -fill red \
		-tags "items $widget"
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
		$canvas create text $x $ty -text $i -tags "items $widget" -font WindowsHierarchyFont \
		    -anchor nw \
		    -fill $color
		set ty [expr $ty+$linespace]
	    }
	    incr inum
	}
#         $canvas bind $widget <Enter> "RamDebugger::DisplayWindowsHierarchyInfo $w \
#                  $canvas $widget %X %Y"
	$canvas bind $widget <Motion> "RamDebugger::DisplayWindowsHierarchyInfo $w \
		 $canvas $widget %X %Y"

	#$canvas bind $widget <Leave> "RamDebugger::DisplayWindowsHierarchyInfo $w $canvas {} %X %Y"

#         $canvas bind $widget <ButtonRelease-3> {
#             set menu %W.menu
#             catch { destroy $menu }
#             menu $menu
#             $menu add command -label "Draw red" -command "RamDebugger::DisplayWindowsHierarchyInfo $w $canvas {} %X %Y"
#         }
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

proc RamDebugger::DisplayWindowsHierarchyDo { w what { res "" } } {
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
		    if { [regexp {^#BWidget} [winfo name $i]] } { continue }
		    eval lappend retval_in [RDC::WindowsHierarchy $i]
		}
		if { $retval_in == "" } {
		    lappend retval -
		} else { lappend retval $retval_in }
		return $retval
	    }
	    EVAL
	}
	if { $DialogWinTop::user($w,type) == "ramdebugger" } {
	    set retcomm "RamDebugger::DisplayWindowsHierarchyDo $w res \
		     \[::RDC::WindowsHierarchy .]"
	} else {
	    set retcomm "RDC::SendDev \[list RamDebugger::DisplayWindowsHierarchyDo $w res \
		        \[::RDC::WindowsHierarchy .]]"
	}
	set comm [string map [list EVAL $retcomm] $comm]

	if { [info exists RamDebugger::DisplayWindowsHierarchyFindLastId] } {
	    unset RamDebugger::DisplayWindowsHierarchyFindLastId
	}

	if { $DialogWinTop::user($w,type) == "ramdebugger" } {
	    eval $comm
	} else {
	    if { $remoteserver == "" } {
		after 100 [list WarnWin "Error: there is no debugged application" \
		    $DialogWinTop::user($w,canvas)]
		$DialogWinTop::user($w,canvas) delete items
		return
	    }
	    EvalRemote $comm
	}
    } else {
	$DialogWinTop::user($w,canvas) delete items
	if { $DialogWinTop::user($w,fontsize) < 1 } {
	    bell
	    set DialogWinTop::user($w,fontsize) 1
	}
	if { [lsearch [font names] WindowsHierarchyFont] == -1 } {
	    font create WindowsHierarchyFont -size $DialogWinTop::user($w,fontsize)
	} else {
	    font configure WindowsHierarchyFont -size $DialogWinTop::user($w,fontsize)
	}
	set linespace [expr [font metrics WindowsHierarchyFont -linespace]+0]

	foreach "newx newy" [DisplayWindowsHierarchyDoDraw $w $DialogWinTop::user($w,canvas) $res \
	    5 5 $linespace] break

	$DialogWinTop::user($w,canvas) conf -scrollregion [list 0 0 $newx $newy]
    }
}

proc RamDebugger::DisplayWindowsHierarchyFind { w } {
    variable DisplayWindowsHierarchyFindLastId

    set canvas $DialogWinTop::user($w,canvas)

    if { ![info exists DisplayWindowsHierarchyFindLastId] } {
	set DisplayWindowsHierarchyFindLastId -1
    }
    set found 0
    foreach i [$canvas find all] {
	if { $DisplayWindowsHierarchyFindLastId > -1 && \
		 $i <= $DisplayWindowsHierarchyFindLastId } {
	    continue
	}
	if { [$canvas type $i] == "text" } {
	    set text [$canvas itemcget $i -text]
	    if { [string match -nocase *$DialogWinTop::user(find)* $text] } {
		set found 1
		set DisplayWindowsHierarchyFindLastId $i
		break
	    }
	}
	set tags [$canvas gettags $i]
	if { [string match *.* $DialogWinTop::user(find)] && \
		 [lsearch $tags $DialogWinTop::user(find)] != -1 } {
	    set found 1
	    set DisplayWindowsHierarchyFindLastId $i
	    break
	}
    }
    if { $found } {
	catch {
	    $canvas select from $i 0
	    $canvas select to $i end
	}
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

proc RamDebugger::DisplayWindowsPickWindow { w button } {
    variable remoteserver

    set l [winfo parent $button].__l
    set comm {
	set widgets [list .]
	set curr 0
	while { $curr < [llength $widgets] } {
	    set wi [lindex $widgets $curr]
	    bindtags $wi [concat givemeyourname [bindtags $wi]]
	    eval lappend widgets [winfo children $wi]
	    incr curr
	}
	bind givemeyourname <1> EVAL
    }
    if { $DialogWinTop::user($w,type) == "ramdebugger" } {
	set retcomm [list RamDebugger::DisplayWindowsPickWindowDo $w $button %W]
    } else {
	set retcomm [list RDC::SendDev [list RamDebugger::DisplayWindowsPickWindowDo \
		                            $w $l %W]]
    }
    set comm [string map [list EVAL [list $retcomm]] $comm]
    
    if { $DialogWinTop::user($w,type) == "ramdebugger" } {
	eval $comm
    } else {
	if { $remoteserver == "" } {
	    after 100 [list WarnWin "Error: there is no debugged application" \
		           $DialogWinTop::user($w,canvas)]
	    $DialogWinTop::user($w,canvas) delete items
	    return
	}
	EvalRemote $comm
    }
    label $l -text "Pick a widget to find its name" -height 2
    place $l -in [winfo parent $button] -x [winfo x $button] -y [winfo y $button] -anchor nw
    bind $l <1> "[list RamDebugger::DisplayWindowsPickWindowDo $w $l ""] ; break"
}

proc RamDebugger::DisplayWindowsPickWindowDo { w l widget } {
    variable remoteserver

    destroy $l

    set comm {
	set widgets [list .]
	set curr 0
	while { $curr < [llength $widgets] } {
	    set wi [lindex $widgets $curr]
	    if { [lindex [bindtags $wi] 0] == "givemeyourname" } {
		bindtags $wi [lrange [bindtags $wi] 1 end]
	    }
	    eval lappend widgets [winfo children $wi]
	    incr curr
	}
	bind givemeyourname <1> ""
    }
    if { $DialogWinTop::user($w,type) == "ramdebugger" } {
	eval $comm
    } else {
	if { $remoteserver == "" } {
	    after 100 [list WarnWin "Error: there is no debugged application" \
		           $DialogWinTop::user($w,canvas)]
	    $DialogWinTop::user($w,canvas) delete items
	    return
	}
	EvalRemote $comm
    }
    
    if { $widget == "" } { return } 

    raise $w
    set DisplayWindowsHierarchyFindLastId -1
    set DialogWinTop::user(find) $widget
    DisplayWindowsHierarchyFind $w
} 

proc RamDebugger::DisplayWindowsHierarchyCancel { f } {
    destroy [winfo toplevel $f]
}

proc RamDebugger::DisplayWindowsHierarchy {} {
    variable text

    set commands [list "RamDebugger::DisplayWindowsHierarchyCancel"]

    set f [DialogWinTop::Init $text "Windows hierarchy" separator $commands "" -]
    set w [winfo toplevel $f]
    wm withdraw $w

    radiobutton $f.r1 -text "In debugged app" -variable DialogWinTop::user($w,type) -value debug \
       -command "RamDebugger::DisplayWindowsHierarchyDo $w do" -grid "0 w"
    radiobutton $f.r2 -text "In RamDebugger" -variable DialogWinTop::user($w,type) -value ramdebugger \
       -command "RamDebugger::DisplayWindowsHierarchyDo $w do" -grid "1 w"

    if { ![info exists DialogWinTop::user($w,fontsize)] } {
	set DialogWinTop::user($w,fontsize) [font actual NormalFont -size]
    }

    set bbox [ButtonBox $f.font -spacing 0 -padx 1 -pady 1 -homogeneous 1 -grid "2 nwwe px5 py3" \
	    -spacing 2]
    $bbox add -image viewmag-16 \
	 -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1 -padx 1 -pady 1 \
	 -helptext "Decrease display size" \
	 -command "[list incr DialogWinTop::user($w,fontsize) -1] ;\
		    [list RamDebugger::DisplayWindowsHierarchyDo $w do]"
    $bbox add -image viewmag+16 \
	 -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1 -padx 1 -pady 1 \
	 -helptext "Increase display size" \
	 -command "[list incr DialogWinTop::user($w,fontsize) 1] ;\
		    [list RamDebugger::DisplayWindowsHierarchyDo $w do]"

    frame $f.f -grid "0 3"
    label $f.f.l -text "Find:" -grid 0
    entry $f.f.e -textvar DialogWinTop::user(find) -grid 1

    tkTabToWindow $f.f.e

    button $f.f.b01 -text Go -width 5 -grid "2 px3 py3" -command \
       "RamDebugger::DisplayWindowsHierarchyFind $w"
    bind $f.f.e <Return> "$f.f.b01 invoke"

    button $f.f.b1 -text "Pick the window" -grid "0 2 w px3 py3" -command \
	[list RamDebugger::DisplayWindowsPickWindow $w $f.f.b1]

    set DialogWinTop::user($w,type) debug

    set sw [ScrolledWindow $f.lf -relief sunken -borderwidth 0 -grid "0 3"]
    set DialogWinTop::user($w,canvas) [canvas $sw.t -width 600 -height 400 -bg white -bd 0 ]
    $sw setwidget $DialogWinTop::user($w,canvas)

    bind $DialogWinTop::user($w,canvas) <2> {
	%W scan mark %x %y
	set tkPriv(x) %x
	set tkPriv(y) %y
	set tkPriv(mouseMoved) 0
    }
    bind $DialogWinTop::user($w,canvas) <B2-Motion> {
	if {(%x != $tkPriv(x)) || (%y != $tkPriv(y))} {
	    set tkPriv(mouseMoved) 1
	}
	if {$tkPriv(mouseMoved)} {
	    %W scan dragto %x %y
	}
    }

    supergrid::go $f

    DisplayWindowsHierarchyDo $w do
    update idletasks
    wm deiconify $w
    DialogWinTop::CreateWindow $f

}
