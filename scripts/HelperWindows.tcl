

################################################################################
# DisplayVar
################################################################################

namespace eval RamDebugger {}

proc RamDebugger::DisplayVar { X Y x y } {
    variable text
    variable remoteserverType
    variable debuggerstate

    if { $debuggerstate != "debug" } { return }

    if { abs($X-[winfo pointerx $text])> 3 || abs($Y-[winfo pointery $text])> 3 } {
	return
    }
    set var [GetSelOrWordInIndex @$x,$y]
    if { $var == "" } { return }

    if { $remoteserverType == "gdb" } {
	set comm "$var"
    } else {
	set comm {
	    if { [::array exists {VAR}] } {
		::set ::RDC::retval [::list array [array get {VAR}]]
	    } elseif { [::info exists {VAR}] } {
		::set ::RDC::retval [::list variable [set {VAR}]]
	    } else {
		::set ::RDC::errorInfo $::errorInfo
		::set ::RDC::err [::catch {::expr {VAR}} ::RDC::val]
		if { !$::RDC::err } {
		    ::set ::RDC::retval [::list expr $::RDC::val]
		} else {
		    ::set ::RDC::retval [::list error {variable or expr 'VAR' does not exist}]
		    ::set ::errorInfo $::RDC::errorInfo
		}
	    }
	    ::set ::RDC::retval
	}
	set comm [string map [list VAR [string trim $var]] $comm]
    }
    # catch is here for strange situations, like when changing source file
    catch {
	set res [reval -handler [list RamDebugger::DisplayVar2 $var $X $Y $x $y] $comm]
    }
}

proc RamDebugger::DisplayVar2 { var X Y x y res } {
    variable text
    variable remoteserverType

    if { $remoteserverType == "gdb" } {
	lset res 1 [list expr [lindex $res 1]]
    }

    if { [lindex $res 0] == 0 && [lindex $res 1 0] ne "error" } {
	set w $text.help
	if { [winfo exists $w] } { destroy $w }
	toplevel $w
	wm overrideredirect $w 1
	wm transient $w $text
	wm geom $w +$X+$Y
	$w configure -highlightthicknes 1 -highlightbackground grey \
	    -highlightcolor grey
	pack [label $w.l -fg black -bg grey95 -wraplength 400 -justify left]
	#$w.l conf -bd 1 -relief solid
	set val [lindex $res 1 1]
	if { [string length $val] > 500 } {
	    set val [string range $val 0 496]...
	}
	$w.l conf -text "$var=$val"
	raise $w
    }
}

################################################################################
# DisplayVarWindow
################################################################################

proc RamDebugger::DisplayVarWindowEval { what w { res "" } } {
    variable remoteserver
    variable remoteserverType
    variable options

    if { $what == "do" } {
	if { [string trim [$w give_uservar_value expression]] == "" } {
	    $w set_uservar_value type ""
	    return
	}
	set var [$w give_uservar_value expression]

	if { $remoteserver == "" } {
	    WarnWin [_ "Debugger is not active"]
	    return
	}

	set ipos [lsearch -exact $options(old_expressions) $var]
	if { $ipos != -1 } {
	    set options(old_expressions) [lreplace $options(old_expressions) $ipos $ipos]
	}
	set options(old_expressions) [linsert [lrange $options(old_expressions) 0 20] 0 $var]

	[$w give_uservar_value combo] configure -values $options(old_expressions)

	if { $remoteserverType == "gdb" } {
	    if { ![regexp {\[([0-9]+):([0-9]+)\]} $var {} ini1 end1] } {
		set ini1 1
		set end1 1
	    }
	    if { ![regexp {\[([0-9]+)::([0-9]+)\]} $var {} ini2 end2] } {
		set ini2 1
		set end2 1
	    }
	    set remoteserver [lreplace $remoteserver 2 2 [list getdata \
		[list RamDebugger::DisplayVarWindowEval res $w]]]

	    set comm ""
	    set isinit 0
	    for { set i1 $ini1 } { $i1 <= $end1 } { incr i1 } {
		regsub {\[([0-9]+):([0-9]+)\]} $var \[$i1\] varn
		for { set i2 $ini2 } { $i2 <= $end2 } { incr i2 } {
		    regsub {\[([0-9]+)::([0-9]+)\]} $varn \[$i2\] varn
		    if { !$isinit } {
		        if { $ini1 != $end1 || $ini2 != $end2 } {
		            append comm "printf \"MULTIPLE RESULT\\n\"\n"
		        }
		        append comm "whatis $varn\n"
		        set isinit 1
		    }
		    append comm "printf \"\\n$varn=\"\noutput $varn\nprintf \"\\n\"\n"
		}
	    }
	    append comm "printf \"FINISHED GETDATA\\n\""
	    EvalRemote $comm
	    return
	} else {
	    set comm {
		if { [::array exists {VAR}] } {
		    ::list array [::array get {VAR}]
		} elseif { [::info exists {VAR}] } {
		    ::list variable [::set {VAR}]
		} else {
		    ::list expression [::expr {VAR}]
		}
	    }
	    set comm [string map [list VAR [string trim $var]] $comm]
	}
	reval -handler [list RamDebugger::DisplayVarWindowEval res $w] $comm
    } else {
	set var [$w give_uservar_value expression]
	[$w give_uservar_value textv] configure -state normal
	[$w give_uservar_value textv] delete 1.0 end

	if { $remoteserverType == "gdb" } {
	    if { [regexp {^(\s*MULTIPLE RESULT\s*type\s+=\s+char\s)(.*)} $res {} ini rest] } {
		set res $ini
		append res "   \""
		foreach "i c" [regexp -all -inline {'(.[^']*|')'\n} $rest] {
		    append res "$c"
		}
		append res "\"\n$rest"
	    }
	    set res [list 0 [list variable $res]]
	}
	switch [lindex $res 0] {
	    0 {
		[$w give_uservar_value textv] configure -fg black
		$w set_uservar_value type [lindex [lindex $res 1] 0]
		if { [$w give_uservar_value type] eq "array" } {
		    $w set_uservar_value type array
		    foreach "name val" [lindex [lindex $res 1] 1] {
		        [$w give_uservar_value textv] insert end "${var}($name) = $val\n"
		    }
		} elseif { [$w give_uservar_value aslist] eq "list" } {
		    $w set_uservar_value type list
		    if { [catch { set list [lrange [lindex [lindex $res 1] 1] 0 end] }] } {
		        [$w give_uservar_value textv] insert end [_ "Error: variable is not a list"]
		    } else {
		        set ipos 0
		        foreach i $list {
		            [$w give_uservar_value textv] insert end "$ipos = $i\n"
		            incr ipos
		        }
		    }
		}  elseif { [$w give_uservar_value aslist] eq "dict" } {
		    $w set_uservar_value type dict
		    set dict [lindex [lindex $res 1] 1]
		    if { [catch { dict info $dict }] } {
		        [$w give_uservar_value textv] insert end [_ "Error: variable is not a dict"]
		    } else {
		        foreach key [lsort -dictionary [dict keys $dict]] {
		            [$w give_uservar_value textv] insert end "$key = [dict get $dict $key]\n"
		        }
		    }
		} else {
		    $w set_uservar_value type ""
		    [$w give_uservar_value textv] insert end [lindex [lindex $res 1] 1]
		}
	    }
	    1 {
		$w set_uservar_value type error
		[$w give_uservar_value textv] configure -fg red
		[$w give_uservar_value textv] insert end [lindex $res 1]
	    }
	}
	[$w give_uservar_value textv] see end
	[$w give_uservar_value textv] configure -state disabled
    }
}

proc RamDebugger::GetSelOrWordInIndex { args } {
    variable text
    
    set optional {
	{ -return_range boolean 0 }
    }
    set compulsory "idx"
    parse_args $optional $compulsory $args

    
    set range [$text tag ranges sel]
    if { $range != "" && [$text compare [lindex $range 0] <= $idx] && \
	[$text compare [lindex $range 1] >= $idx] } {
	if { $return_range } {
	    return $range
	} else {
	    return [$text get {*}$range]
	}
    } else {
	if { $idx != "" } {
	    set var ""
	    set idx0 $idx
	    set char [$text get $idx0]
	    while { [string is wordchar $char] } {
		#  || $char == "(" || $char == ")"
		set var $char$var
		set idx0 [$text index $idx0-1c]
		if { [$text compare $idx0 <= 1.0] } { break }
		set char [$text get $idx0]
	    }
	    set idx1 [$text index $idx+1c]
	    set char [$text get $idx1]
	    while { [string is wordchar $char] } {
		#  || $char == "(" || $char == ")"
		append var $char
		set idx1 [$text index $idx1+1c]
		if { [$text compare $idx1 >= end-1c] } { break }
		set char [$text get $idx1]
	    }
	    if { ![regexp {[^()]*\([^)]+\)} $var] } {
		set var [string trimright $var "()"]
	    }
	} else { set var "" }
    }
    if { $return_range } {
	return [list [$text index "$idx0+1c"] [$text index "$idx1-1c"]]
    } else {
	return $var
    }
}

proc RamDebugger::ToggleTransientWinAll { mainwindow } {
    variable varwindows
    
    foreach i $varwindows {
	ToggleTransientWin [winfo toplevel $i] $mainwindow
    }
}

proc RamDebugger::ToggleTransientWin { w mainwindow } {
    variable options

    if { [info exists options(TransientVarWindow)] && $options(TransientVarWindow) } {
	wm transient $w $mainwindow
    } else {
	wm transient $w ""
    }
    AddAlwaysOnTopFlag $w $mainwindow
}

proc RamDebugger::AddAlwaysOnTopFlag { w mainwindow } {
    variable options

    catch { destroy $w._topmenu }
    menu $w._topmenu -tearoff 0
    $w conf -menu $w._topmenu
    menu $w._topmenu.system -tearoff 0
    $w._topmenu add cascade -menu $w._topmenu.system
    $w._topmenu.system add checkbutton -label [_ "Always on top"] -var \
	RamDebugger::options(TransientVarWindow) -command \
	[list RamDebugger::ToggleTransientWinAll $mainwindow]
}

proc RamDebugger::InvokeAllDisplayVarWindows {} {
    variable varwindows

    if { ![info exists varwindows] } { return }
    foreach i $varwindows {
	set w [winfo toplevel $i]
	catch { $w invokeok }
    }
}

proc RamDebugger::DisplayVarWindow_contextual { text x y } {
    destroy $text.menu
    set menu [menu $text.menu]
    $menu add command -label [_ "Copy"] -command \
	[list RamDebugger::DisplayVarWindow_contextual_do $text copy]
    $menu add separator
    $menu add command -label [_ "Select all"] -command \
	[list RamDebugger::DisplayVarWindow_contextual_do $text selectall]
    tk_popup $menu $x $y
}

proc RamDebugger::DisplayVarWindow_contextual_do { text what } {

    switch $what {
	copy {
	    event generate $text <<Copy>>
	}
	selectall {
	    $text tag add sel 1.0 end-1c
	    focus $text
	}
    }
}

proc RamDebugger::DisplayVarWindow { mainwindow { var "" } } {
    variable text
    variable options
    variable varwindows
    
    if { $var eq "" } {
	set var [GetSelOrWordInIndex insert]
    }

    set w [dialogwin_snit $text.%AUTO% -title [_ "View expression or variable"] -okname \
	    [_ Eval] -cancelname [_ Close] -grab 0 -callback [list RamDebugger::DisplayVarWindowDo]]
    set f [$w giveframe]
 
    lappend varwindows $f
    bind $f <Destroy> {
	set ipos [lsearch -exact $RamDebugger::varwindows %W]
	set RamDebugger::varwindows [lreplace $RamDebugger::varwindows $ipos $ipos]
    }
    ToggleTransientWin $w $mainwindow

    ttk::label $f.l1 -text [_ "Expression:"]

    tooltip::tooltip $f.l1 [_ {
	Examples of possible expressions in TCL:

	*  variablename: Enter the name of a variable 
	*  $variablename+4: Enter a expression like the ones accepted by expr
	*  [lindex $variablename 2]: Enter any command between brackets
	*  [set variablename 6]: modify variable
	
	Examples of possible expressions in C++:
	
	*  variablename: Enter the name of a variable 
	*  $variablename+4: Enter any expression that gdb accepts
	*  $variablename[4:2][6::8]: One extension to the gdb expressions. Permmits to
	   print part of a string or vector
    }]

    if { ![info exists options(old_expressions)] } {
	set options(old_expressions) ""
    }

    $w set_uservar_value combo [ttk::combobox $f.e1 -textvariable [$w give_uservar expression] \
	-values $options(old_expressions)]

    $w set_uservar_value expression $var
    
    set sw [ScrolledWindow $f.lf -relief sunken -borderwidth 0]
    $w set_uservar_value textv [text $sw.text -background white -wrap word -width 40 -height 10 \
		                       -exportselection 0 -font FixedFont -highlightthickness 0]

    bind $f.l1 <1> [list $w invokeok]
    bind $sw.text <3> [list RamDebugger::DisplayVarWindow_contextual %W %X %Y]


    $sw setwidget $sw.text

    if { $::tcl_platform(platform) != "windows" } {
	$sw.text conf -exportselection 1
    }

    label $f.l2 -textvar [$w give_uservar type] -bg [CCColorActivo [$w  cget -bg]]
    ttk::radiobutton $f.cb1 -text [_ "As var"] -variable [$w give_uservar aslist] \
	-command [list $w invokeok] -value "var"
    ttk::radiobutton $f.cb2 -text [_ "As list"] -variable [$w give_uservar aslist] \
	-command [list $w invokeok] -value "list"
    ttk::radiobutton $f.cb3 -text [_ "As dict"] -variable [$w give_uservar aslist] \
	-command [list $w invokeok] -value "dict"

    $w set_uservar_value aslist var
    
    grid $f.l1 $f.e1 - - -sticky w -pady 2
    grid $f.lf - - - -sticky nsew
    grid $f.l2 $f.cb1 $f.cb2 $f.cb3 -sticky w
    
    grid configure $f.l2 -sticky ew
    grid configure $f.e1 -sticky ew
    grid columnconfigure $f 3 -weight 1
    grid rowconfigure $f 1 -weight 1

    tk::TabToWindow $f.e1
    bind [$w give_uservar_value combo] <Return> "[list $w invokeok] ; break"
    [$w give_uservar_value textv] configure -state disabled
    bind [$w give_uservar_value textv] <1> [list focus [$w give_uservar_value textv]]

    $w createwindow
    $w invokeok
}

proc RamDebugger::DisplayVarWindowDo { w } {
    if { [$w giveaction] < 1 } {
	destroy $w
	return
    }
    DisplayVarWindowEval do $w
}

################################################################################
# DisplayBreakpoints
################################################################################


proc RamDebugger::DisplayBreakpointsWindowSetCond { w } {

    set curr [[$w give_uservar_value list] curselection]
    if { [llength $curr] != 1 } { return }

    $w set_uservar_value cond [lindex [[$w give_uservar_value list] get $curr] 4]
}

proc RamDebugger::DisplayBreakpointsWindow {} {
    variable text
    variable breakpoints
    variable currentfile
    
    set w [dialogwin_snit $text.%AUTO% -title [_ "Breakpoints window"] -okname \
	    [_ "Apply Cond"] -cancelname [_ Close] -morebuttons [list \
		[_ Delete] [_ "Delete all"] [_ View] [_ En/Dis] [_ Trace]]]
    set f [$w giveframe]

    set help [_ {Examples of conditions:
	$i > $j+1
	[string match *.tcl $file]
    }]

    ttk::label $f.l1 -text [_ "Condition:"]
    tooltip::tooltip $f.l1 $help
    ttk::entry $f.e1 -textvariable [$w give_uservar cond ""] -width 80

    set sw [ScrolledWindow $f.lf -relief sunken -borderwidth 0]

    $w set_uservar_value list [tablelist::tablelist $sw.lb -width 55\
		  -exportselection 0 \
		  -columns [list \
		                4  [_ Num]        left \
		                6  [_ En/dis]     center \
		                20 [_ File]        right \
		                5  [_ Line] left \
		                12 [_ Condition] left \
		                40 [_ Path] left \
		               ] \
		  -labelcommand tablelist::sortByColumn \
		  -background white \
		  -selectbackground navy -selectforeground white \
		  -stretch all -selectmode extended \
		  -highlightthickness 0]
    
    $sw.lb columnconfigure 0 -sortmode integer
    $sw.lb columnconfigure 2 -sortmode dictionary
    $sw.lb columnconfigure 3 -sortmode integer
    $sw.lb columnconfigure 5 -sortmode dictionary
 

    $sw setwidget $sw.lb
    
    grid $f.l1 $f.e1 -sticky w -padx 2 -pady 2
    grid $sw   -     -sticky nsew -padx 2 -pady 2
    
    grid configure $f.e1 -sticky ew
    grid columnconfigure $f 1 -weight 1
    grid rowconfigure $f 1 -weight 1

    focus $sw.lb
    bind [$sw.lb bodypath] <Double-1> {
	set w [winfo toplevel %W]
	focus [$w give_uservar_value list]
	RamDebugger::DisplayBreakpointsWindowSetCond $w
    }
    bind [$sw.lb bodypath] <ButtonPress-3> \
	    [bind TablelistBody <ButtonPress-1>]

    bind [$sw.lb bodypath] <ButtonRelease-3> {
	set w [winfo toplevel %W]
	catch { destroy %W.menu }
	set menu [menu %W.menu]
	$menu add command -label [_ "Apply condition"] -command [list $w invokeok]
	$menu add command -label [_ "View"] -command [list $w invokebutton 4]
	$menu add command -label [_ "Enable/disable"] -command [list $w invokebutton 5]
	$menu add command -label [_ "Trace"] -command [list $w invokebutton 6]
	$menu add separator
	$menu add command -label [_ "Delete"] -command [list $w invokebutton 2]
	tk_popup $menu %X %Y
    }

    set nowline [scan [$text index insert] %d]
    foreach i $breakpoints {
	foreach "num endis file line cond" $i break
	if { $file ne "" } {
	    set tail [file tail $file]
	    set dir [file dirname $file]
	} else { foreach "tail dir" [list "" ""] break }
	$sw.lb insert end [list $num $endis $tail $line $cond \
		$dir]
	if { [AreFilesEqual $file $currentfile] && $line == $nowline } {
	    $sw.lb selection set end
	    $sw.lb see end
	}
    }

    set action [$w createwindow]
    while 1 {
	switch $action {
	    0 {
		destroy $w
		return
	    }
	    1 {
		set curr [[$w give_uservar_value list] curselection]
		if { [llength $curr] != 1 } {
		    WarnWin [_ "Select just one breakpoint before applying condition"] $w
		} else {
		    set val [[$w give_uservar_value list] get $curr]
		    rcond [lindex $val 0] [$w give_uservar_value cond]
		    [$w give_uservar_value list] delete $curr
		    [$w give_uservar_value list] insert $curr [lreplace $val 4 4 \
		            [$w give_uservar_value cond]]
		    set file [file join [lindex $val 5 ] [lindex $val 2]]
		    set line [lindex $val 3]
		    if { $file == $currentfile } {
		        $text mark set insert $line.0
		        $text see $line.0
		    }
		}
	    }
	    2 {
		foreach i [[$w give_uservar_value list] curselection] {
		    set ent [[$w give_uservar_value list] get $i]
		    set num [lindex $ent 0]
		    set file [file join [lindex $ent 5 ] [lindex $ent 2]]
		    set line [lindex $ent 3]
		    if { $file == $currentfile } {
		        UpdateArrowAndBreak $line 0 ""
		    }
		    rdel $num
		}
		[$w give_uservar_value list] delete 0 end
		foreach i $breakpoints {
		    foreach "num endis file line cond" $i break
		    [$w give_uservar_value list] insert end [list $num $endis [file tail $file] $line $cond \
		            [file dirname $file]]
		}
	    }
	    3 {
		set ret [snit_messageBox -default ok -icon warning -message \
		        [_ "Are you sure to delete all breakpoints?"] -parent $w \
		        -title [_ "delete all breakpoints"] -type okcancel]
		if { $ret == "ok" } {
		    [$w give_uservar_value list] delete 0 end
		    foreach i $breakpoints {
		        set num [lindex $i 0]
		        set file [lindex $i 2]
		        set line [lindex $i 3]
		        if { $file == $currentfile } {
		            UpdateArrowAndBreak $line 0 ""
		        }
		        rdel $num
		    }
		}
	    }
	    4 {
		set curr [[$w give_uservar_value list] curselection]
		if { [llength $curr] != 1 } {
		    WarnWin [_ "Select just one breakpoint in order to see the file"] $w
		    return
		}
		set val [[$w give_uservar_value list] get $curr]
		set file [file join [lindex $val 5] [lindex $val 2]]
		set line [lindex $val 3]
		if { $file != $currentfile } {
		    OpenFileF $file
		}
		$text mark set insert $line.0
		$text see $line.0
	    }
	    5 {
		foreach i [[$w give_uservar_value list] curselection] {
		    set val [[$w give_uservar_value list] get $i]
		    renabledisable [lindex $val 0]
		    [$w give_uservar_value list] delete $i
		    if { [lindex $val 1] } {
		        set enabledisable 0
		    } else { set enabledisable 1 }
		    [$w give_uservar_value list] insert $i [lreplace $val 1 1 $enabledisable]
		    [$w give_uservar_value list] selection set $i
		    set file [file join [lindex $val 5 ] [lindex $val 2]]
		    set line [lindex $val 3]
		    if { $file == $currentfile } {
		        UpdateArrowAndBreak $line "" "" 0
		        $text mark set insert $line.0
		        $text see $line.0
		    }
		}
	    }
	    6 {
		set txt [_ "A trace is a breakpoint that is applied to every line.\
		           It should be used with a condition.\
		           to trace changes in a variable, write as cond '%s'.\
		           Proceed?" "variable varname"]
		set ret [snit_messageBox -type okcancel -message $txt -parent $w]
		if { $ret eq "ok" } {
		    foreach i [[$w give_uservar_value list] curselection] {
		        set val [[$w give_uservar_value list] get $i]
		        set file [file join [lindex $val 5 ] [lindex $val 2]]
		        set line [lindex $val 3]
		        rbreaktotrace [lindex $val 0]
		        set val [lreplace $val 2 3 "" ""]
		        set val [lreplace $val 5 5 ""]
		        [$w give_uservar_value list] insert $i $val
		        if { $file == $currentfile } {
		            UpdateArrowAndBreak $line "" "" 0
		        }
		    }
		}
	    }
	}
	set action [$w waitforwindow]
    }
}

################################################################################
# fonts
################################################################################


proc RamDebugger::CreateModifyFonts {} {
    variable options

    if { [lsearch [font names] NormalFont] == -1 } { font create NormalFont }
    if { [lsearch [font names] FixedFont] == -1 } { font create FixedFont }
    if { [lsearch [font names] HelpFont] == -1 } { font create HelpFont }

    eval font configure NormalFont $options(NormalFont)
    eval font configure FixedFont $options(FixedFont)
    eval font configure HelpFont $options(HelpFont)

    option add *font NormalFont

}

# proc RamDebugger::UpdateFont { w but fontname } {
# 
#     set newfont [SelectFont $w.fonts -type dialog -parent $w -sampletext \
#         "RamDebugger is nice" -title "Select font" -font $fontname]
#     if { $newfont == "" } { return }
#     set list [list family size weight slant underline overstrike]
#     foreach $list $newfont break
# 
#     if { $weight == "" } { set weight normal }
#     if { $slant == "" } { set slant roman }
# 
#     set comm "font configure"
#     lappend comm $fontname
#     foreach i [lrange $list 0 3] {
#         lappend comm -$i [set $i]
#     }
#     if { [info exists underline] && $underline != "" } {
#         lappend comm -underline 1
#     } else { lappend comm -underline 0 }
# 
#     if { [info exists overstrike] && $overstrike != "" } {
#         lappend comm -overstrike 1
#     } else { lappend comm -overstrike 0 }
# 
#     eval $comm
# 
#     regexp {^[^:]+} [$but cget -text] type
#     set btext "$type: [font conf $fontname -family] [font conf $fontname -size] "
#     append btext "[font conf $fontname -weight] [font conf $fontname -slant]"
#     $but configure -text $btext
# }

################################################################################
# Preferences
################################################################################

proc RamDebugger::PreferencesAddDelProcs { w listbox what } {
    variable options
    
    switch $what {
	add {
	    set proc [string trim [$w give_uservar_value nonInstrumentingProc]]
	    if { $proc eq "" } {
		WarnWin [_ "Write a proc in order to add it to list"]
		return
	    }
	    if { [lsearch -exact [$w give_uservar_value nonInstrumentingProcs] $proc] == -1 } {
		set l [$w give_uservar_value nonInstrumentingProcs]
		lappend l $proc
		$w set_uservar_value nonInstrumentingProcs $l
	    }
	}
	delete {
	    set num 0
	    foreach i [$listbox curselection] {
		set ipos [expr $i-$num]
		$listbox delete $ipos
		incr num
	    }
	    $listbox selection clear 0 end
	}
    }
}

proc RamDebugger::PreferencesAddDelDirectories { listbox what } {
    variable options

    switch $what {
	add {
	    set dir [RamDebugger::filenormalize [tk_chooseDirectory -initialdir $options(defaultdir) \
		-parent $listbox \
		-title "Add directories" -mustexist 1]]
	    if { $dir == "" } { return }
	    $listbox insert end $dir
	}
	delete {
	    set num 0
	    foreach i [$listbox curselection] {
		set ipos [expr $i-$num]
		$listbox delete $ipos
		incr num
	    }
	    $listbox selection clear 0 end
	}
	up {
	    set sel [$listbox curselection]
	    if { [llength $sel] != 1 } {
		WarnWin "Select just one directory name to increase its precedence" $listbox
		return
	    }
	    set dir [$listbox get $sel]
	    $listbox delete $sel
	    $listbox insert [expr $sel-1] $dir
	    $listbox selection set [expr $sel-1]
	}
    }
}

proc RamDebugger::_giveopposite_color { col } {

    foreach "r g b" [winfo rgb . $col] break

    foreach i "r g b" {
	set $i [expr {65535-[set $i]}]
    }
    return [format "#%04x%04x%04x" $r $g $b]
}

proc RamDebugger::_chooseColor { w button what } {

    set col [tk_chooseColor -initialcolor [$w give_uservar_value colors,$what] \
	    -parent $button -title "Choose color"]
    if { $col ne "" } {
	$w set_uservar_value colors,$what $col
	$button configure -background $col -text $col \
	    -foreground [_giveopposite_color $col]
    }
}

proc RamDebugger::_update_pref_font { w label fontname } {
    
    set font ""
    foreach i [list family size weight slant underline] {
	lappend font -$i [$w give_uservar_value font_$i,$fontname]
    }
    $label configure -font $font
}

proc RamDebugger::PreferencesWindow {} {
    variable text
    variable text_secondary
    variable options
    variable options_def
    variable iswince
    
    set w [dialogwin_snit $text.%AUTO% -title [_ "Preferences window"] -morebuttons \
	    [list [_ Apply] [_ Defaults]]]
    set f [$w giveframe]

    set nb [ttk::notebook $f.nb]
    
    set f1 [ttk::labelframe $nb.f1 -text [_ Debugging]]
    $nb add $f1 -text [_ Debugging] -sticky nsew

    ttk::checkbutton $f1.cb0 -text "Automatically check remote files" -variable \
	[$w give_uservar CheckRemotes]
    tooltip::tooltip $f1.cb0 [string trim {
	    this variable is only used on windows. It can be:
	    0: Only check remote programs on demand (useful if not making remote debugging, the
	       start up is faster)
	    1: Register as remote and check remote programs on start up. The start up can be slower
		but is better when making remote debugging
	    }]
    $w set_uservar_value CheckRemotes $options(CheckRemotes)

    ttk::checkbutton $f1.cb1 -text [_ "Confirm restart debugging"] -variable \
	[$w give_uservar ConfirmStartDebugging]
    tooltip::tooltip $f1.cb1 [_ "\
	    If this option is set, a confirmation window will be displayed\n\
	    when restarting the execution of the debugger"]
    
    $w set_uservar_value ConfirmStartDebugging $options(ConfirmStartDebugging)

    ttk::checkbutton $f1.cb2 -text [_ "Confirm modify variable"] -variable \
	[$w give_uservar ConfirmModifyVariable]
    tooltip::tooltip $f1.cb2 [_ "\
	    If this option is set, a confirmation window will be displayed\n\
	    before changing the value of a variable in the debugged program\n\
	    by pressing return in the 'User defined variables' or in the\n\
	    'Local variables'"]
    
    $w set_uservar_value ConfirmModifyVariable $options(ConfirmModifyVariable)

    ttk::checkbutton $f1.cb25 -text [_ "Use browser to open files"] -variable \
	[$w give_uservar openfile_browser]
    tooltip::tooltip $f1.cb25 [_ "\
	    If this option is set, files names are chosen with the default\n\
	    browser. If not, file names are chosen inline. This last option\n\
	    permmits to use remote protocols like ssh,..."]
    
    $w set_uservar_value openfile_browser $options(openfile_browser)

    ttk::label $f1.isf -text [_ "Instrument sourced files"]
    ttk::menubutton $f1.cb3 -textvariable [$w give_uservar instrument_source_p] -menu \
	$f1.cb3.m
    menu $f1.cb3.m -tearoff 0
    foreach i [list auto autoprint always never ask] j [list [_ Auto] [_ "Auto print"] \
	    [_ Always] [_ Never] [_ Ask]] {
	set cmd1 [list $w set_uservar_value instrument_source $i]
	set cmd2 [list $w set_uservar_value instrument_source_p $j]
	$f1.cb3.m add command -label $j -command "$cmd1 ; $cmd2"
	if { $options(instrument_source) eq $i } {
	    $w set_uservar_value instrument_source_p $j
	}
    }
    tooltip::tooltip $f1.isf [_ "\
	    This variable controls what to do when the debugged program tries\n\
	    to source a file. Depending on the option chosen, the source file\n\
	    will be instrumented or not.\n\
	    If 'auto' is chosen, only the already instrumented files will be sent\n\
	    instrumented"]
    
    if { [string match *ask* $options(instrument_source)] } {
	$w set_uservar_value instrument_source ask
	$w set_uservar_value instrument_source_p [_ Ask]
    } else {
	$w set_uservar_value instrument_source $options(instrument_source)
    }

    ttk::checkbutton $f1.cb23 -text [_ "Instrument proc last line"] -variable \
	[$w give_uservar instrument_proc_last_line]
    tooltip::tooltip $f1.cb23 [_ "\
	    If this option is set, it is possible to put stops in the last line\n\
	    of one proc. It can make the debugged program fail if the proc wants\n\
	    to return a value without using command return. A typical example of\n\
	    failure are the procs that finish using 'set a 23' instead of 'return 23'"]

    $w set_uservar_value instrument_proc_last_line $options(instrument_proc_last_line)

    ttk::label $f1.l15 -text [_ "Local debugging type:"]
    tooltip::tooltip $f1.l15 [_ "When debugging locally, choose here if the debugged script\n\
	    is only TCL or also TK"]
    ttk::frame $f1.rads
    ttk::radiobutton $f1.rads.r1 -text "TCL" -variable [$w give_uservar LocalDebuggingType] \
	-value tcl
    ttk::radiobutton $f1.rads.r2 -text "TK" -variable [$w give_uservar LocalDebuggingType] \
	-value tk
    grid $f1.rads.r1 $f1.rads.r2 -sticky w
    
    $w set_uservar_value LocalDebuggingType $options(LocalDebuggingType)

    ttk::label $f1.l2 -text [_ "Indent size TCL:"]
    tooltip::tooltip $f1.l2 [_ "Size used when indenting TCL with key: <Tab>"]

    spinbox $f1.sb -from 0 -to 10 -increment 1 -textvariable [$w give_uservar indentsizeTCL] \
       -width 4
    $w set_uservar_value indentsizeTCL $options(indentsizeTCL)

    ttk::label $f1.l3 -text [_ "Indent size c++:"]
    tooltip::tooltip $f1.l3 [_ "Size used when indenting c++ with key: <Tab>"]

    spinbox $f1.sb2 -from 0 -to 10 -increment 1 -textvariable [$w give_uservar indentsizeC++] \
       -width 4
    $w set_uservar_value indentsizeC++ $options(indentsizeC++)

    ttk::checkbutton $f1.cb12 -text [_ "Compile fast instrumenter"] -variable \
	[$w give_uservar CompileFastInstrumenter]
    tooltip::tooltip $f1.cb1 [_ "\
      If this option is set, RamDebugger tries to compile automatically the fast C++ instrumented code"]

    $w set_uservar_value CompileFastInstrumenter $options(CompileFastInstrumenter)

    if { $::tcl_platform(platform) == "windows" } {
	grid $f1.cb0 - -sticky w
    }
    grid $f1.cb1 - -sticky w
    grid $f1.cb2 - -sticky w
    grid $f1.cb25 - -sticky w
    grid $f1.isf $f1.cb3 -sticky w
    grid $f1.cb23 - -sticky w
    grid $f1.l15 $f1.rads -sticky w
    grid $f1.l2 $f1.sb -sticky w
    grid $f1.l3 $f1.sb2 -sticky w
    grid $f1.cb12 - -sticky w
    
    grid configure $f1.l2 $f1.l3 -padx "20 0"
    
    grid rowconfigure $f1 10 -weight 1
    
    set f2 [ttk::labelframe $nb.f2 -text [_ Fonts]]
    $nb add $f2 -text [_ Fonts] -sticky nsew
    
    set families [lsort -dictionary [font families]]
    set sizes [list 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24]
    set irow 0
    foreach "but type fontname" [list b1 {GUI font} NormalFont b2 {Text font} \
	    FixedFont b3 {Help font} HelpFont] {
	label $f2.l$but -text $type: -font $fontname -anchor w
	ttk::combobox $f2.cbf$irow -textvariable [$w give_uservar font_family,$fontname] \
	    -values $families -width 8 -state readonly
	ttk::combobox $f2.cbs$irow -textvariable [$w give_uservar font_size,$fontname] \
	    -values $sizes -width 2
	ttk::checkbutton $f2.cbb$irow -image format-text-bold-16 \
	    -variable [$w give_uservar font_weight,$fontname] \
	    -style Toolbutton -onvalue bold -offvalue normal
	ttk::checkbutton $f2.cbsl$irow -image format-text-italic-16 \
	    -variable [$w give_uservar font_slant,$fontname] \
	    -style Toolbutton -onvalue italic -offvalue roman
	ttk::checkbutton $f2.cbu$irow -image format-text-underline-16 \
	    -variable [$w give_uservar font_underline,$fontname] \
	    -style Toolbutton
	
	grid $f2.l$but $f2.cbf$irow $f2.cbs$irow $f2.cbb$irow $f2.cbsl$irow \
	    $f2.cbu$irow -sticky w
	grid configure $f2.cbf$irow -sticky ew
	grid columnconfigure $f2 1 -weight 1
	
	foreach i [list family size weight slant underline] {
	    $w set_uservar_value font_$i,$fontname [font configure $fontname -$i]
	    set cmd [list RamDebugger::_update_pref_font $w $f2.l$but $fontname]
	    $w add_trace_to_uservar font_$i,$fontname $cmd
	}
	incr irow
    }
    grid rowconfigure $f2 $irow -weight 1
    
    set fco [ttk::labelframe $nb.fco -text [_ Colors]]
    $nb add $fco -text [_ Colors] -sticky nsew

    set irow 0
    foreach i [list foreground background commands defines procnames \
	    quotestrings set comments varnames] {
	ttk::label $fco.l$i -text $i
	$w set_uservar_value colors,$i $options(colors,$i)
	button $fco.b$i -width 15 -background $options(colors,$i) \
	    -text $options(colors,$i) \
	    -foreground [_giveopposite_color $options(colors,$i)] \
	    -command [list RamDebugger::_chooseColor $w $fco.b$i $i] \
	    -anchor center
	grid $fco.l$i $fco.b$i -sticky w -padx 3 -pady 2
	incr irow
    }
    grid rowconfigure $fco $irow -weight 1
 
    set fde [ttk::labelframe $nb.fde -text [_ Extensions]]
    $nb add $fde -text [_ Extensions] -sticky nsew

    ttk::label $fde.ll -text [_ "Choose extensions for every file type:"]
    grid $fde.ll - -sticky w -pady 3
    set ic 1
    foreach "type extsdefaultlist" [list TCL [list ".tcl" ".tcl .tk *"] \
	    C/C++ [list ".c .cpp .cc .h"] \
	    XML [list ".xml .html .htm"] \
	    "GiD BAS file" .bas \
	    "GiD data files" [list ".prb .mat .cnd"]] {
	ttk::label $fde.l$ic -text $type:
	if { ![info exists options(extensions,$type)] } {
	    $w set_uservar_value extensions,$type $options_def(extensions,$type)
	} else {
	    $w set_uservar_value extensions,$type $options(extensions,$type)
	}
	ttk::combobox $fde.cb$ic -textvariable [$w give_uservar extensions,$type] -values \
	    $extsdefaultlist
	grid $fde.l$ic $fde.cb$ic -sticky w -pady 2
	grid configure $fde.cb$ic -sticky ew
	incr ic
    }
    grid rowconfigure $fde $ic -weight 1

    set fd [ttk::labelframe $nb.fd -text [_ Directories]]
    $nb add $fd -text [_ Directories] -sticky nsew

    set sw [ScrolledWindow $fd.lf -relief sunken -borderwidth 0]
    listbox $sw.lb -listvariable DialogWin::user(executable_dirs) -selectmode extended
    $sw setwidget $sw.lb
    
    ttk::frame $fd.bbox1
    set icol 0
    foreach "img cmd help" [list \
	    folderopen16 add [_ "Add include directory"] \
	    actcross16 delete [_ "Delete include directory"] \
	    playeject16 up [_ "Increase directory priority"] \
	    ] {
	ttk::button $fd.bbox1.b$icol -image $img -style Toolbutton -command \
	    [list RamDebugger::PreferencesAddDelDirectories $sw.lb $cmd]
	tooltip::tooltip $fd.bbox1.b$icol $help
	grid $fd.bbox1.b$icol -sticky w -row 0 -column $icol
	incr icol
    }
    grid columnconfigure $fd.bbox1 $icol -weight 1

    $w set_uservar_value executable_dirs $options(executable_dirs)

    set tt [_ "Include here all directories where RamDebugger should find executables\n\
	    This is primary useful in Windows to describe where mingw is installed"]
    tooltip::tooltip $sw.lb $tt

    grid $fd.lf -sticky nsew
    grid $fd.bbox1 -sticky w
    
    grid columnconfigure $fd 0 -weight 1
    grid rowconfigure $fd 0 -weight 1

    set lb [ttk::labelframe $nb.lb -text [_ "Auto save revisions"]]
    $nb add $lb -text [_ "Auto save"] -sticky nsew

    ttk::checkbutton $lb.c1 -text [_ "Perform auto save revisions"] -variable \
	[$w give_uservar AutoSaveRevisions]
    ttk::label $lb.l1 -text [_ "Auto save time"]:
    spinbox $lb.cb1 -textvariable [$w give_uservar AutoSaveRevisions_time] \
	-from 0 -to 10000 -increment 1 -width 4
    ttk::label $lb.l2 -text [_ "seconds"]
    tooltip::tooltip $lb.l1 [_ "Time in seconds before performing an auto-save"]

    ttk::label $lb.l3 -text [_ "Auto save idle time"]:
    spinbox $lb.cb2 -textvariable [$w give_uservar AutoSaveRevisions_idletime] \
	-from 0 -to 10000 -increment 1 -width 4
    ttk::label $lb.l4 -text [_ "seconds"]
    set tt [_ "Time in seconds without user activity before performing an auto-save"]
    tooltip::tooltip $lb.l3 $tt
    
    set d [dict create 1 "$lb.l1 $lb.cb1 $lb.l2 $lb.l3 $lb.cb2 $lb.l4"]
    $w enable_disable_on_key AutoSaveRevisions $d

    if { [info exists options(AutoSaveRevisions)] } {
	$w set_uservar_value AutoSaveRevisions_time $options(AutoSaveRevisions_time)
	$w set_uservar_value AutoSaveRevisions_idletime $options(AutoSaveRevisions_idletime)
	$w set_uservar_value AutoSaveRevisions $options(AutoSaveRevisions)
    } else {
	$w set_uservar_value AutoSaveRevisions 1
    }

    grid $lb.c1 - - -sticky nw
    grid $lb.l1 $lb.cb1 $lb.l2 -sticky nw
    grid $lb.l3 $lb.cb2 $lb.l4 -sticky nw

    grid configure $lb.cb1 $lb.cb2 -sticky new
    grid configure $lb.l1 $lb.l3 -padx "10 0"
    grid rowconfigure $lb 2 -weight 1

    set fdi [ttk::labelframe $nb.fdi -text [_ "Non instrument procs"]]
    $nb add $fdi -text [_ "Instrument"] -sticky nsew

    set sw [ScrolledWindow $fdi.lf -relief sunken -borderwidth 0]
    listbox $sw.lb -listvariable [$w give_uservar nonInstrumentingProcs] \
	-selectmode extended
    $sw setwidget $sw.lb
    
    ttk::frame $fdi.bbox1
    set icol 0
    foreach "img cmd help" [list \
	    folderopen16 add [_ "Add proc"] \
	    actcross16 delete [_ "Delete proc"] \
	    ] {
	ttk::button $fdi.bbox1.b$icol -image $img -style Toolbutton -command \
	    [list RamDebugger::PreferencesAddDelProcs $w $sw.lb $cmd]
	tooltip::tooltip $fdi.bbox1.b$icol $help
	grid $fdi.bbox1.b$icol -sticky w -row 0 -column $icol
	incr icol
    }
    grid columnconfigure $fdi.bbox1 $icol -weight 1

    ttk::label $fdi.l1 -text [_ "Proc"]:
    ttk::entry $fdi.e1 -textvariable [$w give_uservar nonInstrumentingProc]

    bind $fdi.e1 <Return> "RamDebugger::PreferencesAddDelProcs $w $sw.lb add"

    grid $fdi.lf - -sticky nsew
    grid $fdi.bbox1 - -sticky nw
    grid $fdi.l1 $fdi.e1 -sticky nw -pady 3 -padx 3
    grid configure $fdi.e1 -sticky new
    grid columnconfigure $fdi 1 -weight 1
    grid rowconfigure $fdi 0 -weight 1

    $w set_uservar_value nonInstrumentingProc ""
    $w set_uservar_value nonInstrumentingProcs $options(nonInstrumentingProcs)

    set tt [_ "The procs with this name will not be instrumented"]
    tooltip::tooltip $sw.lb $tt
    tooltip::tooltip $fdi.l1 $tt

    grid $nb -sticky nsew
    grid columnconfigure $f 0 -weight 1
    grid rowconfigure $f 0 -weight 1

    focus $f1.cb1

    set action [$w createwindow]
    while 1 {
	switch $action {
	    0 {
		destroy $w
		return
	    }
	    1 - 2 {
		foreach i [list indentsizeTCL indentsizeC++ AutoSaveRevisions \
		        AutoSaveRevisions_time AutoSaveRevisions_idletime] {
		    set $i [$w give_uservar_value $i]
		}
		set good 1
		if { ![string is integer -strict $indentsizeTCL] || \
		    $indentsizeTCL < 0 || $indentsizeTCL > 10 } {
		    WarnWin [_ "Error: indent size must be between 0 and 10"] $w
		    set good 0
		} elseif { ![string is integer -strict ${indentsizeC++}] || \
		    ${indentsizeC++} < 0 || ${indentsizeC++} > 10 } {
		    WarnWin [_ "Error: indent size must be between 0 and 10"] $w
		    set good 0
		}
		if { $good && $AutoSaveRevisions != 0 } {
		    if { ![string is double -strict $AutoSaveRevisions_time] || \
		             $AutoSaveRevisions_time < 0 } {
		        WarnWin [_ "Error: Auto save revisions time must be a number"] $w
		        set good 0
		    } elseif { ![string is double -strict \
		                     $AutoSaveRevisions_idletime] || \
		                   $AutoSaveRevisions_idletime < 0 } {
		        WarnWin [_ "Error: Auto save revisions idle time must be a number"] $w
		        set good 0
		    }
		}
		if { $good } {
		    foreach i [list indentsizeTCL indentsizeC++ ConfirmStartDebugging \
		            ConfirmModifyVariable openfile_browser instrument_source instrument_proc_last_line \
		            LocalDebuggingType AutoSaveRevisions AutoSaveRevisions_time \
		            AutoSaveRevisions_idletime CompileFastInstrumenter \
		            nonInstrumentingProcs] {
		        set options($i) [$w give_uservar_value $i]
		    }
		    foreach i [list foreground background commands defines procnames \
		            quotestrings set comments varnames] {
		        set options(colors,$i) [$w give_uservar_value colors,$i]
		    }
		    foreach "but type fontname" [list b1 {GUI font} NormalFont b2 {Text font} \
		            FixedFont b3 {Help font} HelpFont] {
		        set font ""
		        foreach i [list family size weight slant underline] {
		            lappend font -$i [$w give_uservar_value font_$i,$fontname]
		        }
		        set options($fontname) $font
		    }
		    CreateModifyFonts

		    if { [info exists options(CheckRemotes)] } {
		        set options(CheckRemotes) [$w give_uservar_value CheckRemotes]
		    }

		    foreach i [array names options_def extensions,*] {
		        set options($i) [$w give_uservar_value $i]
		    }
		    set options(executable_dirs) [$w give_uservar_value executable_dirs]
		    UpdateExecDirs
		    RamDebugger::CVS::ManageAutoSave
		    if { $options(CompileFastInstrumenter) == 1 } {
		        Instrumenter::TryCompileFastInstrumenter 1
		    }
		    ApplyColorPrefs $text
		    if { [info exists text_secondary] && [winfo exists text_secondary] } {
		        ApplyColorPrefs $text_secondary
		    }
		    
		    if { $action == 1 } {
		        destroy $w
		    }
		    
		    RamDebugger::SavePreferences 1
		    
		    if { $action == 1 } {
		        return
		    }
		}
	    }
	    3 {
		foreach i [list indentsizeTCL indentsizeC++ ConfirmStartDebugging \
		               ConfirmModifyVariable openfile_browser instrument_source instrument_proc_last_line \
		               LocalDebuggingType CheckRemotes NormalFont FixedFont HelpFont \
		               executable_dirs AutoSaveRevisions AutoSaveRevisions_time \
		              AutoSaveRevisions_idletime] {
		    if { [info exists options_def($i)] } {
		        set options($i) $options_def($i)
		        $w set_uservar_value $i $options_def($i)
		    }
		}
		foreach i [list foreground background commands defines procnames \
		        quotestrings set comments varnames] {
		    set options(colors,$i) $options_def(colors,$i)
		    $w set_uservar_value colors,$i $options_def(colors,$i)
		    $fco.b$i configure -background $options(colors,$i) \
		        -text $options(colors,$i) -foreground \
		        [_giveopposite_color $options(colors,$i)]
		}
		CreateModifyFonts
		foreach "but type fontname" [list b1 {GUI font} NormalFont b2 {Text font} \
		        FixedFont b3 {Help font} HelpFont] {
		    foreach i [list family size weight slant underline] {
		        $w set_uservar_value font_$i,$fontname [font configure $fontname -$i]
		    }
		}
		ApplyColorPrefs $text
		if { [info exists text_secondary] && [winfo exists text_secondary] } {
		    ApplyColorPrefs $text_secondary
		}

#                 foreach "but fontname" [list $f2.b1 NormalFont $f2.b2 FixedFont $f2.b3 HelpFont] {
#                     regexp {^[^:]+} [$but cget -text] type
#                     set btext "$type: [font conf $fontname -family] [font conf $fontname -size] "
#                     append btext "[font conf $fontname -weight] [font conf $fontname -slant]"
#                     $but configure -text $btext
#                 }

		foreach i [array names options_def extensions,*] {
		    set options($i) $options_def($i)
		}
		RamDebugger::CVS::ManageAutoSave
	    }
	}
	set action [$w waitforwindow]
    }
}

################################################################################
# DisplayTimes
################################################################################


proc RamDebugger::DisplayTimesWindowStart { { f "" } } {
    rtime -start
    #WarnWin "Starting to measure times. Use 'File->Debug on->currentfile' to proceed" $f
}

proc RamDebugger::DisplayTimesWindowReport { f } {
    variable text

    set f [DialogWin::Init $text "Timing report" separator "" -]
    set w [winfo toplevel $f]

    ComboBox $f.cb1 -textvariable DialogWin::user(units) -editable 0 \
       -modifycmd {
	   $DialogWin::user(text) delete 1.0 end
	   $DialogWin::user(text) ins end [RamDebugger::rtime -display $DialogWin::user(units)]
       } -width 10 -values [list microsec  milisec  sec  min] -grid "0 w"
    set DialogWin::user(units) sec
    button $f.b1 -text "Clear time table" -command {
	$DialogWin::user(text) delete 1.0 end
	RamDebugger::rtime -cleartimes
    } -grid "1 w"

    set sw [ScrolledWindow $f.lf -relief sunken -borderwidth 0 -grid "0 2"]
    text $sw.t
    $sw setwidget $sw.t

    set DialogWin::user(text) $sw.t

    bind $sw.t <1> "focus $sw.t"
    
    supergrid::go $f

    focus $sw.t

    $sw.t ins end [rtime -display sec]

    set action [DialogWin::CreateWindow]
    DialogWin::DestroyWindow
}

proc RamDebugger::DisplayTimesWindowCancel { f } {

    destroy [winfo toplevel $f]
}

proc RamDebugger::DisplayTimesWindowStop { { f "" } } {

    rtime -stop
}

proc RamDebugger::DisplayTimesPickSelection { w } {
    variable text

    if { [catch {
	set DialogWinTop::user($w,lineini) [scan [$text index sel.first] %d]
	set DialogWinTop::user($w,lineend) [scan [$text index "sel.last-1c"] %d]
    }]} {
	set DialogWinTop::user($w,lineini) [scan [$text index insert] %d]
	set DialogWinTop::user($w,lineend) [scan [$text index insert] %d]
	#WarnWin "It is necessary to select some text in main window" $w
    }
}

proc RamDebugger::DisplayTimesDrawSelection { w } {
    variable text

    if { [catch {
	$text tag remove sel 1.0 end
	$text tag add sel "$DialogWinTop::user($w,lineini).0 linestart" \
	   "$DialogWinTop::user($w,lineend).0 lineend"
	$text see $DialogWinTop::user($w,lineini).0
    }] } {
	WarnWin "Lines are not correct" $w
    }
}

proc RamDebugger::ModifyTimingBlock { w entry what } {
    variable TimeMeasureData

    set idx [$DialogWinTop::user($w,list) curselection]

    switch $what {
	create {
	    set err [catch [list rtime -add $DialogWinTop::user($w,name) \
		$DialogWinTop::user($w,lineini) $DialogWinTop::user($w,lineend)] errstring]
	    if { $err } {
		WarnWin [lindex [split $errstring \n] 0]
		return $err
	    } else {
		set i 1
		while 1 {
		    set found 0
		    foreach j $TimeMeasureData {
		        if { [lindex $j 0] == "Block $i" } {
		            set found 1
		            break
		        }
		    }
		    if { !$found } { break }
		    incr i
		}
		set DialogWinTop::user($w,name) "Block $i"
		set DialogWinTop::user($w,lineini) ""
		set DialogWinTop::user($w,lineend) ""
		tkTabToWindow $entry
		ModifyTimingBlock $w $entry update
		return 0
	    }
	}
	edit {
	    if { [llength $idx] != 1 } {
		WarnWin "Error: it is necessary to select just one block in list"
		return 1
	    }
	    set err [ModifyTimingBlock $w $entry delete]
	    if { !$err } { ModifyTimingBlock $w $entry create }
	}
	delete {
	    if { [llength $idx] == 0 } {
		WarnWin "Error: it is necessary to select at least one block in list"
		return 1
	    }
	    set selnames ""
	    foreach i $idx {
		lappend selnames [lindex [$DialogWinTop::user($w,list) get $i] 0]
	    }
	    foreach selname $selnames {
		set err [catch [list rtime -delete $selname] errstring]
		if { $err } {
		    WarnWin [lindex [split $errstring \n] 0]
		    return $err
		}
	    }
	    ModifyTimingBlock $w $entry update
	    return 0
	}
	updatecurrent {
	    if { [llength $idx] == 0 } {
		WarnWin "Error: it is necessary to select at least one block in list"
		return 1
	    }
	    set init 1
	    foreach i $idx {
		set l1 [lindex [$DialogWinTop::user($w,list) get $i] 2]
		set l2 [lindex [$DialogWinTop::user($w,list) get $i] 3]
		if { $init || $l1 < $DialogWinTop::user($w,lineini) } {
		    set DialogWinTop::user($w,lineini) $l1
		}
		if { $init || $l2 > $DialogWinTop::user($w,lineend) } {
		    set DialogWinTop::user($w,lineend) $l2
		}
		set init 0
	    }
	    if { [llength $idx] == 1 } {
		set DialogWinTop::user($w,name) [lindex [$DialogWinTop::user($w,list) get $idx] 0]
	    } else {
		set i 1
		while 1 {
		    set found 0
		    foreach j $TimeMeasureData {
		        if { [lindex $j 0] == "Block group $i" } {
		            set found 1
		            break
		        }
		    }
		    if { !$found } { break }
		    incr i
		}
		set DialogWinTop::user($w,name) "Block group $i"
	    }
	    tkTabToWindow $entry
	}
	update {
	    $DialogWinTop::user($w,list) delete 0 end
	    foreach i $TimeMeasureData {
		$DialogWinTop::user($w,list) insert end [lrange $i 0 3]
	    }
	}
    }
}

proc RamDebugger::DisplayTimesWindowMenu { w x y } {

    catch { destroy $w.debugtime }
    menu $w.debugtime

    $w.debugtime add radiobutton -label "Normal debug" \
	-command "RamDebugger::DisplayTimesWindowStop" -variable RamDebugger::debuggerstate \
	-value debug
    $w.debugtime add radiobutton -label "Time debug" -command \
	"RamDebugger::DisplayTimesWindowStart" -variable RamDebugger::debuggerstate \
	-value time
    $w.debugtime add separator
    $w.debugtime add command -label "Time window..." -command RamDebugger::DisplayTimesWindow

    tk_popup $w.debugtime $x $y
}

proc RamDebugger::DisplayTimesWindow {} {
    variable text
    variable TimeMeasureData

    set commands [list RamDebugger::DisplayTimesWindowStart RamDebugger::DisplayTimesWindowStop \
	    RamDebugger::DisplayTimesWindowReport RamDebugger::DisplayTimesWindowCancel]
    set f [DialogWinTop::Init $text "Timing control" separator $commands \
	       [list Stop Report] Start]
    set w [winfo toplevel $f]

    TitleFrame $f.f1 -text [_ "current block"] -grid "0 new"
    set f1 [$f.f1 getframe]

    label $f1.l1 -text "Name:" -grid "0 e"
    entry $f1.e1 -textvariable DialogWinTop::user($w,name) -grid "1 ew"
    label $f1.l2 -text "Initial line:" -grid "0 e"
    entry $f1.e2 -textvariable DialogWinTop::user($w,lineini) -grid "1 ew py2" -width 8
    label $f1.l3 -text "End line:" -grid "0 e"
    entry $f1.e3 -textvariable DialogWinTop::user($w,lineend) -grid "1 ew py2" -width 8
    
    set bbox [ButtonBox $f1.bbox1 -spacing 0 -padx 1 -pady 1 -homogeneous 1 -grid "0 2 w"]
    $bbox add -text "Pick selection" \
	 -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1 -padx 1 -pady 1 \
	 -helptext [_ "Gets the selection from the text window"] \
	 -command "RamDebugger::DisplayTimesPickSelection $w"
    $bbox add -text "Draw selection" \
	 -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1 -padx 1 -pady 1 \
	 -helptext [_ "Selects current block in text"] \
	 -command "RamDebugger::DisplayTimesDrawSelection $w"

    TitleFrame $f.f2 -text [_ "blocks"] -grid "0 nsew"
    set f2 [$f.f2 getframe]

    set sw [ScrolledWindow $f2.lf -relief sunken -borderwidth 0]
    
    set DialogWinTop::user($w,list) [tablelist::tablelist $sw.lb -width 55 \
	-exportselection 0 \
	-columns [list \
	    10 "Name"        left \
	    10 "File"        right \
	     7 "Initial line"        right \
	     7 "End line"  right \
	   ] \
	-labelcommand tablelist::sortByColumn \
	-background white \
	-selectbackground navy -selectforeground white \
	-stretch "0 1" -selectmode extended \
	-highlightthickness 0]

    $sw setwidget $DialogWinTop::user($w,list)

    bind [$DialogWinTop::user($w,list) bodypath] <Double-1> \
       "RamDebugger::ModifyTimingBlock $w $f1.e1 updatecurrent"

    menu $w.popup
    $w.popup add command -label "View" -command \
	"RamDebugger::ModifyTimingBlock $w $f1.e1 updatecurrent"
    $w.popup add separator
    $w.popup add command -label "Update" -command \
	"RamDebugger::ModifyTimingBlock $w $f1.e1 edit"
    $w.popup add command -label "Delete" -command \
	"RamDebugger::ModifyTimingBlock $w $f1.e1 delete"

    bind [$DialogWinTop::user($w,list) bodypath] <ButtonPress-3> "tk_popup $w.popup %X %Y"

    set bbox [ButtonBox $f2.bbox1 -spacing 0 -padx 1 -pady 1 -homogeneous 1]
    $bbox add -image acttick16 \
	 -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1 -padx 1 -pady 1 \
	 -helptext [_ "Create new block"] \
	 -command "RamDebugger::ModifyTimingBlock $w $f1.e1 create"
    $bbox add -image edit16 \
	 -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1 -padx 1 -pady 1 \
	 -helptext [_ "Update selected block"] \
	 -command "RamDebugger::ModifyTimingBlock $w $f1.e1 edit"
    $bbox add -image actcross16 \
	 -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1 -padx 1 -pady 1 \
	 -helptext [_ "Delete selected block"] \
	 -command "RamDebugger::ModifyTimingBlock $w $f1.e1 delete"

    grid $f2.lf -sticky nsew
    grid $f2.bbox1 -sticky w
    grid columnconfigure $f2 0 -weight 1
    grid rowconfigure $f2 0 -weight 1


    supergrid::go $f1
    supergrid::go $f

    bind [winfo toplevel $f] <Return> "DialogWinTop::InvokeOK $f"

    foreach i "$f1.e1 $f1.e2 $f1.e3" {
	bind $i <Return> "$bbox invoke 0; break"
    }

    ModifyTimingBlock $w $f1.e1 update

    set i 1
    while 1 {
	set found 0
	foreach j $TimeMeasureData {
	    if { [lindex $j 0] == "Block $i" } {
		set found 1
		break
	    }
	}
	if { !$found } { break }
	incr i
    }
    set DialogWinTop::user($w,name) "Block $i"

    tkTabToWindow $f1.e1

    DialogWinTop::CreateWindow $f
    #DialogWinTop::InvokeOK $f
}

################################################################################
# About
################################################################################


proc RamDebugger::AboutWindow {} {
    variable text
    
    set par [winfo toplevel $text]
    set w $par.about
    toplevel $w
    wm protocol $w WM_DELETE_WINDOW {
	  # nothing
    }
    
    label $w.l -text RamDebugger -font "-family {new century schoolbook} -size 24 -weight bold" \
	    -fg \#d3513d -grid 0

    set tt "Author: Ramon Ribó (RAMSAN)\n"
    append tt "ramsan@compassis.com\nhttp://www.gidhome.com/ramsan\n"
    append tt "http://www2.compassis.com/ramdebugger"

    text $w.l2 -grid "0 px20 py10" -bd 0 -bg [$w cget -bg] -width 10 -height 4 \
	    -highlightthickness 0 
    $w.l2 ins end $tt
    $w.l2 conf -state disabled
    bind $w.l2 <1> "focus $w.l2"
    canvas $w.c -height 50 -grid 0 -bg white -highlightthickness 0
    ScrolledWindow $w.lf -relief sunken -borderwidth 0 -grid 0

    tablelist::tablelist $w.lf.lb -width 55\
		  -exportselection 0 \
		  -columns [list \
		                10 Package        left \
		                10 Author        left \
		                10 License right\
		                4  Mod left \
		               ] \
		  -labelcommand tablelist::sortByColumn \
		  -background white \
		  -stretch 1 -selectmode extended \
		  -highlightthickness 0


    $w.lf setwidget $w.lf.lb

    frame $w.buts -height 35 -bg [CCColorActivo [$w  cget -bg]] -grid 0

    supergrid::go $w

    button $w.close -text Close -width 10
    place $w.close -in $w.buts -anchor n -y 3
    
    wm withdraw $w
    update idletasks
    set x [expr [winfo x $par]+[winfo width $par]/2-[winfo reqwidth $w]/2]
    set y [expr [winfo y $par]+[winfo height $par]/2-[winfo reqheight $w]/2]
    wm geom $w +$x+$y
    wm deiconify $w
    wm transient $w $par

    bind $w.close <Enter> "RamDebugger::MotionInAbout $w.close ; break"
    bind $w.close <ButtonPress-1> "WarnWin [list {Congratulations, you got it!!!}] ; destroy $w; break"


    $w.c create text 0 0 -anchor n -font "-family {new century schoolbook} -size 16 -weight bold"\
	    -fill \#d3513d -text "Version $RamDebugger::Version" -tags text
    RamDebugger::AboutMoveCanvas $w.c 0


    set data {
	tkcon "Jeffrey Hobbs" BSD SLIGHTLY
	comm "Open Group Res. Ins." BSD YES
	BWidgets "Unifix" BSD YES
	dialogwin  RAMSAN BSD NO
	helpviewer RAMSAN BSD NO
	supergrid  RAMSAN BSD NO
	supertext  "Bryan Oakley" "FREE SOFT" YES
	tablelist  "Csaba Nemethi" "FREE SOFT" NO
	"visual regexp" "Laurent Riesterer" GPL NO
	Tkhtml  "D. Richard Hipp" LGPL NO
	tcllib Many BSD NO
	icons "Adrian Davis" BSD NO
	tkdnd "George Petasis" BSD NO
	tkdiff "John M. Klassa" GPL NO
	tkcvs  "DEL" GPL YES
    }
    foreach "pack author lic mod" $data {
	$w.lf.lb insert end [list $pack $author $lic $mod]
    }


}

proc RamDebugger::AboutMoveCanvas { c t } {

    if { ![winfo exists $c] } { return }
    set w [winfo width $c]
    set h [winfo height $c]

    set x [expr $w/2*(1.0-sin($t))]
    set y [expr $h/2*(cos($t)+1)]

    $c coords text $x $y

    set t [expr $t+.2]
    after 100 [list RamDebugger::AboutMoveCanvas $c $t]
}

proc RamDebugger::MotionInAbout { but } {

    if { ![winfo exists $but] } { return }

    set w [winfo toplevel $but]

    while 1 {
	if { ![winfo exists $but] } { return }
	set x [expr [winfo pointerx $w]-[winfo rootx $w]]
	set y [expr [winfo pointery $w]-[winfo rooty $w]]
	
	set x1 [expr [winfo rootx $but]-[winfo rootx $w]]
	set y1 [expr [winfo rooty $but]-[winfo rooty $w]]
	set x2 [expr $x1+[winfo width $but]]
	set y2 [expr $y1+[winfo height $but]]

	if { $x < $x1-2 || $x > $x2+2 || $y < $y1-2 || $y > $y2+2 } { return }
	
	set xmax [winfo width $w]
	set ymax [winfo height $w]
	
	if { $x < ($x1+$x2)/2 } {
	    set newx [expr $x1+int((rand()-.2)*5.1)]
	} else { set newx [expr $x1+int((rand()-.8)*5.1)] }
	
	if { $newx < 0 } { set newx 0 }
	if { $newx > $xmax-[winfo width $but] } { set newx [expr $xmax-[winfo width $but]] }
	
	if { $y < ($y1+$y2)/2 } {
	    set newy [expr $y1+int((rand()-.2)*5.1)]
	} else { set newy [expr $y1+int((rand()-.8)*5.1)] }
	
	
	if { $newy < 0 } { set newy 0 }
	if { $newy > $ymax-[winfo height $but] } { set newy [expr $ymax-[winfo height $but]] }
	place $but -in $w -x $newx -y $newy -anchor nw
	update
    }
}

################################################################################
# GotoLine
################################################################################


proc RamDebugger::GotoLine {} {
    variable text
    variable text_secondary

    if { [info exists text_secondary] && [focus -lastfor $text] eq $text_secondary } {
	set active_text $text_secondary
    } else { set active_text $text }

    set w [dialogwin_snit $active_text._ask -title [_ "Goto line"]]
    set f [$w giveframe]

    ttk::label $f.l -text [_ "Go to line:"]
    spinbox $f.sb -from 1 -to 10000 -increment 1 -textvariable [$w give_uservar line] \
	    -width 8

    ttk::checkbutton $f.cb1 -text [_ "Relative to current line"] -variable \
	[$w give_uservar relative]
    
    grid $f.l $f.sb -sticky w -padx 5 -pady 3
    grid $f.cb1 - -sticky w
    
    grid $f.sb -sticky ew
    grid columnconfigure $f 1 -weight 1
    grid rowconfigure $f 2 -weight 1
  
    $w set_uservar_value relative 0

    tk::TabToWindow $f.sb
    bind $w <Return> [list $w invokeok]
    set action [$w createwindow]
    while 1 {
	switch $action {
	    0 {
		destroy $w
		return
	    }
	    1 {
		set line [$w give_uservar_value line]
		set good 1
		if { ![string is integer -strict $line] } {
		    snit_messageBox -message [_ "Line number must be a positive number"] \
		        -parent $w
		    set good 0
		}
		if { $good && [$w give_uservar_value relative] } {
		    set insline [scan [$active_text index insert] %d]
		    set lastline [scan [$active_text index end-1c] %d]
		    set line [expr $insline+$line]
		    if { $line < 1 || $line > $lastline } {
		        snit_messageBox -message [_ "Trying to go to line %s. Out of limits" \
		                $line] -parent $w
		        set good 0
		    }
		}
		if { $good } {
		    set lastline [scan [$active_text index end-1c] %d]
		    if { $line < 1 } { set line 1 }
		    if { $line > $lastline } { set line $lastline }
		    $active_text mark set insert $line.0
		    $active_text see $line.0
		    focus $active_text
		    destroy $w
		    return
		}
	    }
	}
	set action [$w waitforwindow]
    }
}

################################################################################
# DebugCplusPlus
################################################################################

proc RamDebugger::DebugCplusPlusWindow { { tryautomatic 0 } } {
    variable text
    variable options

    if { ![info exists options(debugcplusplus)] } {
	set options(debugcplusplus) ""
    }

    set exes ""
    set dirs ""
    set args ""
    foreach "exe dir arg" $options(debugcplusplus) {
	lappend exes $exe
	lappend dirs $dir
	lappend args $arg
    }

    set exe ""
    foreach "exe dir arg" [cproject::GiveDebugData] break

    if { $tryautomatic && $exe != "" } {
	set found 0
	set ipos 0
	foreach "exe_in dir_in arg_in" $options(debugcplusplus) {
	    if { $exe == $exe_in && $dir == $dir_in && $arg == $arg_in } {
		set found 1
		break
	    }
	    incr ipos 3
	}
	if { $found && $ipos != 0 } {
	    set options(debugcplusplus) [lreplace $options(debugcplusplus) $ipos \
		    [expr $ipos+2]]
	}
	if { !$found || $ipos != 0 } {
	    set options(debugcplusplus) [linsert $options(debugcplusplus) 0 \
		    $exe $dir $arg]
	}

	rdebug -debugcplusplus [list $exe $dir $arg]
	return
    }
    if { $tryautomatic && $options(debugcplusplus) != "" } {
	rdebug -debugcplusplus [lrange $options(debugcplusplus) 0 2]
	return
    }

    set f [DialogWin::Init $text "Debug c++" separator ""]
    set w [winfo toplevel $f]
    
    label $f.l -text "Program to debug:" -grid "0 e px3 py5"
    ComboBox $f.cb1 -textvariable DialogWin::user(executable) -width 40 -grid 1 -values \
	    $exes
    Button $f.b1 -image [Bitmap::get file] -width 16 -grid 2 -relief link


    label $f.l2 -text "Directory:" -grid "0 e px3 py5"
    ComboBox $f.cb2 -textvariable DialogWin::user(directory) -width 40 -grid "1" -values \
	    $dirs
    Button $f.b2 -image [Bitmap::get folder] -grid 2 -relief link

    label $f.l3 -text "Arguments:" -grid "0 e"
    ComboBox $f.cb3 -textvariable DialogWin::user(arguments) -width 40 -grid "1 2" -values \
	    $args


    set comm {
	set DialogWin::user(executable) [tk_getOpenFile -filetypes {{{All Files} *}} \
		-initialdir $RamDebugger::options(defaultdir) -initialfile \
		$DialogWin::user(executable) -parent PARENT -title "Debug executable"]
    }
    set comm [string map [list PARENT $w] $comm]
    $f.b1 configure -command $comm

    set comm {
	set initial $RamDebugger::options(defaultdir)
	catch { set initial [file dirname $DialogWin::user(executable)] }
	set DialogWin::user(directory) [RamDebugger::filenormalize [tk_chooseDirectory \
	    -initialdir $initial -parent PARENT \
	    -title "Debug directory" -mustexist 1]]
    }
    set comm [string map [list PARENT $w] $comm]
    $f.b2 configure -command $comm

    if { [info exists options(debugcplusplus)] } {
	set DialogWin::user(executable) [lindex $options(debugcplusplus) 0]
	set DialogWin::user(directory) [lindex $options(debugcplusplus) 1]
	set DialogWin::user(arguments) [lindex $options(debugcplusplus) 2]
    }
    tkTabToWindow $f.cb1

    bind $w <Return> "DialogWin::InvokeOK"

    supergrid::go $f

    set action [DialogWin::CreateWindow]
    while 1 {
	switch $action {
	    0 {
		DialogWin::DestroyWindow
		return
	    }
	    1 {
		set found 0
		set ipos 0
		foreach "exe dir args" $options(debugcplusplus) {
		    if { $exe == $DialogWin::user(executable) && \
		        $dir == $DialogWin::user(directory) && \
		        $args == $DialogWin::user(arguments) } {
		        set found 1
		        break
		    }
		    incr ipos 3
		}
		if { $found && $ipos != 0 } {
		    set options(debugcplusplus) [lreplace $options(debugcplusplus) $ipos \
		            [expr $ipos+2]]
		}
		if { !$found || $ipos != 0 } {
		    set options(debugcplusplus) [linsert $options(debugcplusplus) 0 \
		        $DialogWin::user(executable) $DialogWin::user(directory) \
		        $DialogWin::user(arguments)]
		}

		rdebug -debugcplusplus [list $DialogWin::user(executable) \
		        $DialogWin::user(directory) $DialogWin::user(arguments)]
		DialogWin::DestroyWindow
		return
	    }
	}
	set action [DialogWin::WaitForWindow]
    }
}


################################################################################
# DebugCplusPlus
################################################################################

proc RamDebugger::DebugCurrentFileArgsWindow {} {
    variable text
    variable options
    variable currentfile

    if { $currentfile == "" } {
	WarnWin "Error. there is no current file"
	return
    }
    set filetype [GiveFileType $currentfile]
    if { $filetype != "TCL" } {
	WarnWin "Current file is not TCL"
	return
    }

    if { ![info exists options(currentfileargs5)] } {
	set options(currentfileargs5) ""
    }

    set dir [file dirname $currentfile]
    set arg ""
    set curr_as ""
    set currs ""
    set currs_as ""
    set dirs ""
    set args ""
    set tcl_or_tk auto
    set tcl_or_tks ""
    
    foreach "curr curr_as_in dir_in arg_in tcl_or_tk_in" $options(currentfileargs5) {
	if { $curr == $currentfile } {
	    set curr_as $curr_as_in
	    set dir $dir_in
	    set arg $arg_in
	    set tcl_or_tk $tcl_or_tk_in
	}
	lappend currs $curr
	lappend currs_as $curr_as
	lappend dirs $dir_in
	lappend args $arg_in
	lappend tcl_or_tks $tcl_or_tk_in
    }
    
    set w [dialogwin_snit $text._ask -title [_ "TCL Execution arguments"]]
    set f [$text._ask giveframe]

    ttk::label $f.l -text [_ "Current file to debug:"]

    ttk::combobox $f.cb1 -textvariable [$w give_uservar curr] -width 60 -values \
	    $currs
    ttk::button $f.b1 -image fileopen16 -width 16 -style Toolbutton

    ttk::label $f.lb -text [_ "File to debug as:"]
    ttk::combobox $f.cb1b -textvariable [$w give_uservar curr_as] -width 40 -values \
	$currs_as
    tooltip::tooltip $f.cb1b [_ "Select another TCL file to execute when 'Current file' is open"]
    ttk::button $f.b1b -image fileopen16 -width 16 -style Toolbutton

    ttk::label $f.l2 -text [_ "Directory:"]
    ttk::combobox $f.cb2 -textvariable [$w give_uservar directory] -width 40 -values \
	    $dirs
    ttk::button $f.b2 -image folderopen16 -style Toolbutton

    ttk::label $f.l3 -text [_ "Arguments:"]
    ttk::combobox $f.cb3 -textvariable [$w give_uservar arguments] -width 40 -values \
	    $args

    ttk::label $f.l4 -text [_ "File type:"]
    ttk::frame $f.f1
    ttk::radiobutton $f.f1.r1 -text [_ "Auto"] -variable [$w give_uservar tcl_or_tk] -value auto
    ttk::radiobutton $f.f1.r2 -text [_ "Tcl"] -variable [$w give_uservar tcl_or_tk] -value tcl
    ttk::radiobutton $f.f1.r3 -text [_ "Tk"] -variable [$w give_uservar tcl_or_tk] -value tk
 
    grid $f.f1.r1 $f.f1.r2 $f.f1.r3 -sticky w

    grid $f.l $f.cb1 $f.b1 -sticky w
    grid $f.lb $f.cb1b $f.b1b -sticky w
    grid $f.l2 $f.cb2 $f.b2 -sticky w
    grid $f.l3 $f.cb3 -sticky w
    grid $f.l4 $f.f1 - -sticky w
    
    grid $f.cb1 $f.cb1b $f.cb2 $f.cb3 $f.f1 -sticky ew
    grid columnconfigure $f 1 -weight 1
    grid rowconfigure $f 5 -weight 1
  
    $w set_uservar_value curr $currentfile
    $w set_uservar_value curr_as $curr_as
    $w set_uservar_value directory $dir
    $w set_uservar_value arguments $arg
    $w set_uservar_value tcl_or_tk $tcl_or_tk

    set comm {
	set w %PARENT%
	set initial $RamDebugger::options(defaultdir)
	catch { set initial [file dirname [$w give_uservar_value curr]] }
	set curr [tk_getOpenFile -filetypes {{{TCL Scripts} {.tcl} } {{All Files} *}} \
		     -initialdir $initial -parent $w -title [_ "Debug TCL file"]]
	if { $curr != "" } { $w set_uservar_value curr $curr }
    }
    set comm [string map [list %PARENT% $w] $comm]
    $f.b1 configure -command $comm

    set comm {
	set w %PARENT%
	set initial $RamDebugger::options(defaultdir)
	catch { set initial [file dirname [$w give_uservar_value curr_as]] }
	set curr_as [tk_getOpenFile -filetypes {{{TCL Scripts} {.tcl} } {{All Files} *}} \
	       -initialdir $initial -parent $w -title [_ "Debug as TCL file"]]
	if { $curr_as != "" } { $w set_uservar_value curr_as $curr_as }
    }
    set comm [string map [list %PARENT% $w] $comm]
    $f.b1b configure -command $comm

    set comm {
	set w %PARENT%
	set initial $RamDebugger::options(defaultdir)
	catch { set initial [file dirname [$w give_uservar_value curr]] }
	$w set_uservar_value directory [RamDebugger::filenormalize [tk_chooseDirectory \
	    -initialdir $initial -parent $w \
	    -title [_ "Debug directory"] -mustexist 1]]
    }
    set comm [string map [list %PARENT% $w] $comm]
    $f.b2 configure -command $comm

    tk::TabToWindow $f.cb1
    bind $w <Return> [list $w invokeok]
    set action [$w createwindow]

    while 1 {
	switch $action {
	    0 {
		destroy $w
		return
	    }
	    1 - 2 {
		if { $action == 2 } {
		    $w set_uservar_value curr_as ""
		    $w set_uservar_value directory ""
		    $w set_uservar_value arguments ""
		    $w set_uservar_value tcl_or_tk auto
		}
		set ipos 0
		foreach "curr curr_as dir args tcl_or_tk" $options(currentfileargs5) {
		    if { $curr == [$w give_uservar_value curr] } {
		        set options(currentfileargs5) [lreplace $options(currentfileargs5) $ipos \
		            [expr $ipos+4]]
		        break
		    }
		    incr ipos 5
		}
		if { [$w give_uservar_value directory] ne "" || 
		    [$w give_uservar_value arguments] ne "" ||
		    [$w give_uservar_value curr_as] ne "" ||
		    [$w give_uservar_value tcl_or_tk] ne "auto" } {
		    lappend options(currentfileargs5) [$w give_uservar_value curr] \
		        [$w give_uservar_value curr_as] [$w give_uservar_value directory] \
		        [$w give_uservar_value arguments] [$w give_uservar_value tcl_or_tk]
		}
		destroy $w
		return
	    }
	}
	set action [$w waitforwindow]
    }
}


################################################################################
# Compile
################################################################################

proc RamDebugger::Compile { name } {
    variable text
    variable mainframe
    variable MainDir

    $mainframe setmenustate debugentry disabled

    set dir [file dirname $name]

    set pwd [pwd]
    cd $dir

    set filetype [GiveFileType $currentfile]
    if { $filetype == "C/C++" } {
	if { [auto_execok gcc] == "" } {
	    $mainframe setmenustate debug normal
	    cd $pwd

	    set ret [DialogWin::messageBox -default yes -icon question -message \
		"Could not find command 'gcc'. Do you want to see the help?" -parent $text \
		-title "Command not found" -type yesno]
	    if { $ret == "yes" } {
		ViewHelpForWord "Debugging c++"
		#RamDebugger::ViewHelpFile "01RamDebugger/RamDebugger_12.html"
	    }
	    return
	}
	set comm "gcc -c -I$dir $name"
	set commS "gcc -c -I$dir $name"
    } elseif { [regexp {Makefil.*[^~]$} $name] } {
	$mainframe setmenustate debug normal
	if { [auto_execok make] == "" } {
	    cd $pwd
	    set ret [DialogWin::messageBox -default yes -icon question -message \
		"Could not find command 'make'. Do you want to see the help?" -parent $text \
		-title "Command not found" -type yesno]
	    if { $ret == "yes" } {
		ViewHelpForWord "Debugging c++"
		#RamDebugger::ViewHelpFile "01RamDebugger/RamDebugger_12.html"
	    }
	    return
	}
	set comm "make -f $name"
	set commS "make -f $name"
    }

#     if {$::tcl_platform(platform) == "windows" && \
#         [info exists ::env(COMSPEC)]} {
#         set comm  "[file join $::env(COMSPEC)] /c $comm"
#     }

    set cat cat
#     if { $::tcl_platform(platform) == "windows" } {
#             set cat [file join $MainDir addons cat.exe]
#     } else { set cat cat }

    set fin [open "|$comm |& $cat" r]
    fconfigure $fin -blocking 0
    fileevent $fin readable [list RamDebugger::CompileFeedback $fin $name]
    cd $pwd
    
    TextCompClear
    TextCompRaise
    TextCompInsert "[string repeat - 20]Compiling [file tail $name][string repeat - 20]\n"
    TextCompInsert "--> $commS\n"
    update
}

proc RamDebugger::CompileFeedback { fin name } {
    variable text
    variable mainframe

    if { [eof $fin] } {
	if { $name != "" } {
	    TextCompInsert "End compilation of [file tail $name]\n"
	    TextCompRaise
	}

	set err [catch { close $fin } errstring]
	$mainframe setmenustate debugentry normal
	if { $err } {
	    TextCompInsert $errstring\n
	    TextCompRaise
	    set cproject::compilationstatus 1
	} else {
	    set cproject::compilationstatus 0
	}
	return
    }
    gets $fin aa

    if { $aa != "" } {
	TextCompInsert $aa\n
	update
    }
}

################################################################################
# SearchInFiles
################################################################################

proc RamDebugger::FindFilesWithPattern { dir patternlist recurse } {

    set retval [eval glob -nocomplain -dir [list $dir] -types f -- $patternlist]
    if { $recurse } {
	foreach i [glob -nocomplain -dir $dir -type d *] {
	    set retval [concat $retval [FindFilesWithPattern $i $patternlist $recurse]]
	}
    }
    return $retval
}

proc RamDebugger::SearchInFilesDo { w } {
    variable options
    variable MainDir
    variable searchstring
    
    if { [$w giveaction] < 1 || $searchstring eq "" } {
	destroy $w
	return
    }

    set grep grep
#     if { $::tcl_platform(platform) == "windows" } {
#         set grep [file join $MainDir addons grep.exe]
#     } else { set grep grep }
    
    set comm [list $grep -ns]
    switch -- $::RamDebugger::searchmode {
	-exact { lappend comm -F }
	-regexp { lappend comm -E }
    }
    if { !$::RamDebugger::searchcase } {
	lappend comm -i
    }
    lappend comm $RamDebugger::searchstring

    set patternlist [regexp -inline -all {[^\s,;]+} $RamDebugger::searchextensions]

    WaitState 1
    set files [FindFilesWithPattern $RamDebugger::searchdir $patternlist \
	    $RamDebugger::searchrecursedirs]

    if { [llength $files] == 0 } {
	WaitState 0
	TextOutClear
	TextOutInsert "No files found"
	TextOutRaise
	return
    }

    set toctree [$w give_uservar_value toctree]
    $toctree item delete all
    set result ""
    set ifile 0
    while { $ifile < [llength $files] } {
	set comm2 $comm
	eval lappend comm2 [lrange $files $ifile [expr {$ifile+50}]]
	incr ifile 50
	set fin [open "|$comm2" r]
	while { [gets $fin line] > -1 } {
	    append result $line\n
	    set ret [regexp {^(.+):(\d+):(.*)} $line {} path linenum txt]
	    if { $ret } {
		set file [file tail $path]
		set path [file dirname $path]
	    } else {
		foreach "file path linenum" [list "" "" ""] break
		set txt $line
	    }
	    $toctree insert end [list $file $linenum $txt $path]
	}
	set result [string trim $result]\n
	set err [catch { close $fin } errstring]
    }
    if { [string trim $result] == "" } { set result "Nothing found" }

    TextOutClear
    TextOutInsert $result
    TextOutRaise
    
    foreach "i j search" [list texts RamDebugger::searchstring lsearch \
	    exts RamDebugger::searchextensions lsearchfile \
	    dirs RamDebugger::searchdir lsearchfile] {
	set ipos [$search $options(SearchInFiles,$i) [set $j]]
	if { $ipos != -1 } {
	    set options(SearchInFiles,$i) [lreplace $options(SearchInFiles,$i) $ipos $ipos]
	}
	set options(SearchInFiles,$i) [linsert $options(SearchInFiles,$i) 0 [set $j]]
	if { [llength $options(SearchInFiles,$i)] > 6 } {
	    set options(SearchInFiles,$i) [lreplace $options(SearchInFiles,$i) 6 end]
	}
    }
    WaitState 0
}

proc RamDebugger::SearchInFilesGo { w tree ids } {
    variable options
    variable currentfile
    variable text
    
    if { $ids eq "" } { return }
    foreach "file line txt path" [$tree item text [lindex $ids 0]] break
    set file [file join $path $file]
    if { ![file exists $file] && [file exists [file join $options(defaultdir) \
	$file]] } {
	set file [file join $options(defaultdir) $file]
    }
    
    if { [file exists $file] } {
	set file [filenormalize $file]
	if { $file ne $currentfile } {
	    OpenFileF $file
	}
	$text see $line.0
	$text mark set insert $line.0
	focus $text
    } 
}

proc RamDebugger::SearchInFiles {} {
    variable options
    variable text

    set txt [GetSelOrWordInIndex insert]
    
    set w [dialogwin_snit $text.%AUTO% -title [_ "Search in files"] \
	    -grab 0 -transient 1 -callback [namespace code SearchInFilesDo] \
	    -cancelname [_ Close]]
    set f [$w giveframe]

    if { ![info exists options(SearchInFiles,texts)] } {
	set options(SearchInFiles,texts) ""
	set options(SearchInFiles,exts) ""
	set options(SearchInFiles,dirs) ""
    }

    ttk::label $f.l1 -text [_ "Text:"]
    ttk::combobox $f.e1 -textvariable ::RamDebugger::searchstring \
	    -values $options(SearchInFiles,texts) -width 40

    if { [string trim $txt] != "" } {
	set ::RamDebugger::searchstring $txt
    } else { set ::RamDebugger::searchstring [lindex $options(SearchInFiles,texts) 0] }

    ttk::label $f.l2 -text [_ "File ext:"]

    set values $options(SearchInFiles,exts)
    foreach i [list "*.tcl" "*.h,*.cc,*.c"] {
	if { [lsearch -exact $values $i] == -1 } { lappend values $i }
    }
    foreach i [array names options(extensions,*)] {
	if { [lsearch -exact $values $options($i)] == -1 } { lappend values $options($i) }
    }

    ttk::combobox $f.e2 -textvariable ::RamDebugger::searchextensions \
	    -values $values

    set ::RamDebugger::searchextensions [lindex [$f.e2 cget -values] 0]
    
    set dir $options(defaultdir)
    set ipos [lsearch -exact $options(SearchInFiles,dirs) $dir]
    if { $ipos != -1 } {
	set options(SearchInFiles,dirs) [lreplace $options(SearchInFiles,dirs) $ipos $ipos]
    }
    set options(SearchInFiles,dirs) [linsert $options(SearchInFiles,dirs) 0 $dir]

    ttk::label $f.l3 -text [_ "Directory:"]
    ttk::combobox $f.e3 -textvariable ::RamDebugger::searchdir \
	    -values $options(SearchInFiles,dirs) -width 70
    ttk::button $f.b3 -image fileopen16 -style Toolbutton


    set comm {
	set initial $::RamDebugger::searchdir
	set ::RamDebugger::searchdir [RamDebugger::filenormalize [tk_chooseDirectory   \
	    -initialdir $initial -parent PARENT \
	    -title [_ "Search directory"] -mustexist 1]]
    }
    set comm [string map [list PARENT $w] $comm]
    $f.b3 configure -command $comm

    if { ![info exists ::RamDebugger::searchdir] || $::RamDebugger::searchdir == "" } {
	set ::RamDebugger::searchdir $options(defaultdir)
    }

    set f1 [ttk::frame $f.f1]
    set f2 [frame $f1.f2 -bd 1 -relief ridge]
    ttk::radiobutton $f2.r1 -text Exact -variable ::RamDebugger::searchmode \
	-value -exact
    ttk::radiobutton $f2.r2 -text Regexp -variable ::RamDebugger::searchmode \
	-value -regexp

    grid $f2.r1 -sticky w
    grid $f2.r2 -sticky w

    set f3 [ttk::frame $f1.f3]
    ttk::checkbutton $f3.cb1 -text [_ "Consider case"] -variable ::RamDebugger::searchcase
    ttk::checkbutton $f3.cb2 -text [_ "Recurse dirs"] -variable ::RamDebugger::searchrecursedirs

    grid $f3.cb1 -sticky w
    grid $f3.cb2 -sticky w
    
    grid $f2 $f3 -sticky w -padx 3
    
    set columns [list \
	    [list 20 [_ "File"] left item 0] \
	    [list  5 [_ "Line"] left text 0] \
	    [list 50 [_ "Text"] left text 0] \
	    [list 35 [_ "Path"] left text 0] \
	]

    $w set_uservar_value toctree [fulltktree $f.toctree \
	    -selecthandler2 [list RamDebugger::SearchInFilesGo $w] \
	    -columns $columns -expand 0 \
	    -selectmode extended -showheader 1 -showlines 0  \
	    -indent 0 -sensitive_cols all]
    
    
    grid $f.l1 $f.e1 - -sticky w -padx 3 -pady 1
    grid $f.l2 $f.e2 - -sticky w -padx 3 -pady 1
    grid $f.l3 $f.e3 $f.b3 -sticky w -padx 3 -pady 1
    grid $f1     -     -  -sticky w
    grid $f.toctree - - -sticky nsew -padx 3 -pady 3
    
    grid configure $f.e1 $f.e2 $f.e3 -sticky ew
    grid columnconfigure $f 1 -weight 1
    grid rowconfigure $f 4 -weight 1


    set ::RamDebugger::searchmode -exact
    set ::RamDebugger::searchcase 0
    set ::RamDebugger::searchrecursedirs 1

    tk::TabToWindow $f.e1
    bind $w <Return> [list $w invokeok]
    
    set action [$w createwindow]
}

################################################################################
# Search
################################################################################

proc RamDebugger::SearchWindow_autoclose { { force "" } } {
    variable options
    variable SearchToolbar
    variable mainframe
    variable text

    if { $force eq "" && (![info exists options(SearchToolbar_autoclose)] ||
	!$options(SearchToolbar_autoclose)) } { return }

    if { [info exists SearchToolbar] && [lindex $SearchToolbar 0] } {
	$mainframe showtoolbar 1 0
	lset SearchToolbar 0 0
	if { [focus] ne $text } { focus $text }
    }
}

proc RamDebugger::SearchWindow { { replace 0 } }  {
    variable text
    variable options
    variable text_secondary
    variable mainframe
    variable SearchToolbar
    variable searchFromBegin
    variable iswince
    
    set istoplevel 0

    if { ![info exists options(SearchToolbar_autoclose)] } {
	set options(SearchToolbar_autoclose) 1
    }

    if { [info exists SearchToolbar] } {
	set f [$mainframe gettoolbar 1]
	set ::RamDebugger::searchstring [GetSelOrWordInIndex insert]
	if { $replace != [lindex $SearchToolbar 1] } {
	    #nothing
	} elseif { [lindex $SearchToolbar 0] && $options(SearchToolbar_autoclose) } {
	    $mainframe showtoolbar 1 0
	    focus $text
	    set SearchToolbar [list 0 $replace]
	    return
	} else {
	    tkTabToWindow $f.e1
	    $mainframe showtoolbar 1 1
	    set SearchToolbar [list 1 $replace]
	    return
	}
    } else {
	$mainframe showtoolbar 1 1
    }

    if { !$replace && [info exists text_secondary] && [focus -lastfor $text] eq \
	$text_secondary } {
	set active_text $text_secondary
    } else { set active_text $text }


    if { ![info exists options(old_searchs)] } {
	set options(old_searchs) ""
    }
    if { ![info exists options(old_replaces)] } {
	set options(old_replaces) ""
    }

#     if { ![info exists searchFromBegin] } {
#         set searchFromBegin 1
#     }
    set searchFromBegin 1

    if { $replace && $searchFromBegin } {
	set searchFromBegin 0
    }

    set ::RamDebugger::replacestring ""

    set w [winfo toplevel $active_text]
    if { !$replace } {
	set commands [list "RamDebugger::Search $w begin 0" \
		          "RamDebugger::SearchReplace $w cancel"]
	set morebuttons ""
	set OKname ""
	set title "Search"
    } else {
	set commands [list "RamDebugger::SearchReplace $w beginreplace" \
		          "RamDebugger::SearchReplace $w replace" \
		          "RamDebugger::SearchReplace $w replaceall"  \
		          "RamDebugger::SearchReplace $w cancel $istoplevel"]
	set morebuttons [list "Replace" "Replace all"]
	set OKname "Search"
	set title "Replace"
    }
    
    if { $istoplevel } {
	set f [DialogWinTop::Init $active_text $title separator $commands $morebuttons $OKname]
    } else {
	if { [info exists ::RamDebugger::SearchToolbar] } {
	    $mainframe showtoolbar 1 1
	    set f [$mainframe gettoolbar 1]
	    eval destroy [winfo children $f]
	} else {
	    set f [$mainframe gettoolbar 1]
	    #set f [$mainframe addtoolbar]
	}
    }
    set ::RamDebugger::SearchToolbar [list 1 $replace]

    ttk::label $f.l1 -text "Search:"
    ttk::combobox $f.e1 -textvariable ::RamDebugger::searchstring -values $options(old_searchs)

    # to avoid problems with paste, that sometimes pastes too to the main window
    bind $f.e1 "[bind [winfo class $f.e1] $f.e1]; break"
    
    set cmd "$f.e1 configure -values \$RamDebugger::options(old_searchs)"
    trace add variable ::RamDebugger::options(old_searchs) write "$cmd ;#"
    bind $f.e1 <Destroy> [list trace remove variable \
	    ::RamDebugger::options(old_searchs) write "$cmd ;#"]

    set f2 [frame $f.f2 -bd 1 -relief ridge]
    radiobutton $f2.r1 -text Exact -variable ::RamDebugger::searchmode \
	-value -exact
    radiobutton $f2.r2 -text Regexp -variable ::RamDebugger::searchmode \
	-value -regexp

    grid $f2.r1 -sticky w
    grid $f2.r2 -sticky w

    set f25 [frame $f.f25 -bd 1 -relief ridge]
    radiobutton $f25.r1 -text Forward -variable ::RamDebugger::SearchType \
	-value -forwards
    radiobutton $f25.r2 -text Backward -variable ::RamDebugger::SearchType \
	-value -backwards

    grid $f25.r1 -sticky w
    grid $f25.r2 -sticky w

    set f3 [frame $f.f3]
    checkbutton $f3.cb1 -text "Consider case" -variable ::RamDebugger::searchcase
    checkbutton $f3.cb2 -text "From beginning" -variable ::RamDebugger::searchFromBegin

    grid $f3.cb1 $f3.cb2 -sticky w
 
    ttk::button $f.r09 -image find-16 -style Toolbutton -command \
	[list RamDebugger::Search $w begin 0 $f]

    ttk::radiobutton $f.r1 -image navup16 -variable ::RamDebugger::SearchType \
	-value -backwards -style Toolbutton
    ttk::radiobutton $f.r2 -image navdown16 -variable ::RamDebugger::SearchType \
	-value -forwards -style Toolbutton
    ttk::separator $f.sp1 -orient vertical
    ttk::checkbutton $f.cb1 -image navhome16 -variable ::RamDebugger::searchFromBegin \
	-style Toolbutton
    ttk::checkbutton $f.cb2 -image uppercase_lowercase -variable ::RamDebugger::searchcase\
	-style Toolbutton
    ttk::checkbutton $f.cb3 -text Regexp -variable ::RamDebugger::searchmode \
	-onvalue -regexp -offvalue -exact -style Toolbutton

    set helps [list \
	    $f.r09 "Search" \
	    $f.r1 "Search backwards (PgUp)" \
	    $f.r2 "Search forwards (PgDn)" \
	    $f.cb1 "From beginning (Home)" \
	    $f.cb2 "Consider case" \
	    $f.cb3 "Rexexp mode"]
    foreach "widget help" $helps {
	tooltip::tooltip $widget $help
    }

    if { $replace } {
	ttk::label $f.l11 -text "Replace:"
	ttk::combobox $f.e11 -textvariable ::RamDebugger::replacestring \
	    -values $options(old_replaces)
	raise $f.e11 $f2.r1
	frame $f.buts
	
	set ic 0
	foreach "txt cmd help" [list "Skip" beginreplace "Search next (Shift-Return)" \
		"Replace" replace "Replace (Return)" \
		"Replace all" replaceall "Replace all"] {
	    ttk::button $f.buts.b[incr ic] -text $txt -width 9 \
		-style Toolbutton -command \
		[list RamDebugger::SearchReplace $w $cmd]
	    tooltip::tooltip $f.buts.b$ic $help
	}
	grid $f.buts.b1 $f.buts.b2 $f.buts.b3 -sticky nw -padx 2
    }

    if { !$istoplevel } {
	if { !$iswince } {
	    grid $f.l1 $f.e1 $f.r09 $f.r1 $f.r2 $f.sp1 $f.cb1 $f.cb2 $f.cb3 -sticky w -padx 2
	} else {
	    grid $f.e1 $f.r09 $f.r1 $f.r2 $f.sp1 $f.cb1 $f.cb2 $f.cb3 -sticky w -padx 0
	}
	grid $f.e1 -sticky ew

	if { [winfo exists $f.l11] } {
	    if { !$iswince } {
		grid $f.l11 $f.e11 $f.buts - - - - - - -sticky w -padx 2 -pady 0
	    } else {
		grid $f.e11 $f.buts - - - - - - -sticky w -padx 0 -pady 0
		$f.cb1 configure -width 6
		$f.cb2 configure -width 6
		grid configure $f.buts.b1 $f.buts.b2 $f.buts.b3 -sticky nw -padx 0
	    }
	    grid configure $f.e11 -sticky ew
	}
	if { $iswince } {
	    set col 0
	} else {
	    set col 1
	}
	grid $f.sp1 -sticky nsw
	grid columnconfigure $f $col -weight 1
    } else {
	grid $f.l1 $f.e1    - -sticky nw -padx 3 -pady 3
	if { [winfo exists $f.l11] } {
	    grid $f.l11 $f.e11    - -sticky nw -padx 3 -pady 3
	    grid configure $f.e11 -sticky new
	}
	grid $f2     -   $f25 -sticky nw -padx 3
	grid $f3     -     -   -sticky nw
	grid configure $f.e1 -sticky new
	grid columnconfigure $f 2 -weight 1
    }
    set ::RamDebugger::searchstring [GetSelOrWordInIndex insert]

    set ::RamDebugger::searchmode -exact
    set ::RamDebugger::searchcase 0
    set ::RamDebugger::SearchType -forwards

    bind $f.l1 <3> [list RamDebugger::SearchReplace $w contextual $replace %X %Y]
    bind $f.e1 <3> [list RamDebugger::SearchReplace $w contextual $replace %X %Y]

    bind $f.e1 <Home> "[list set ::RamDebugger::searchFromBegin 1] ; break"
    bind $f.e1 <End> "[list set ::RamDebugger::searchFromBegin 0] ; break"
    bind $f.e1 <Prior> "[list set ::RamDebugger::SearchType -backwards] ; break"
    bind $f.e1 <Next> "[list set ::RamDebugger::SearchType -forwards] ; break"

    tkTabToWindow $f.e1
    if { !$replace } {
	bind $f.l1 <1> "RamDebugger::Search $w begin 0 $f"
	bind $f.e1 <Return> "RamDebugger::Search $w begin 0 $f"
	bind $f.e1 <Escape> [list RamDebugger::SearchWindow_autoclose force]
    } else {
	bind $f.l1 <1> "RamDebugger::SearchReplace $w replace"
	bind $f.e1 <Return> "RamDebugger::SearchReplace $w replace"
	bind $f.e1 <Shift-Return> "RamDebugger::SearchReplace $w beginreplace ; break"
	bind $f.e11 <Return> "RamDebugger::SearchReplace $w replace ; break"
	bind $f.e11 <Shift-Return> "RamDebugger::SearchReplace $w beginreplace ; break"
	bind $f.l11 <3> [list RamDebugger::SearchReplace $w contextual $replace %X %Y]
	bind $f.e11 <3> [list RamDebugger::SearchReplace $w contextual $replace %X %Y]
	bind $f.e11 <Home> "[list set ::RamDebugger::searchFromBegin 1] ; break"
	bind $f.e11 <End> "[list set ::RamDebugger::searchFromBegin 0] ; break"
	bind $f.e11 <Prior> "[list set ::RamDebugger::SearchType -backwards] ; break"
	bind $f.e11 <Next> "[list set ::RamDebugger::SearchType -forwards] ; break"
	bind $f.e1  <Escape> [list RamDebugger::SearchWindow_autoclose force]
	bind $f.e11 <Escape> [list RamDebugger::SearchWindow_autoclose force]
    }

    if { $istoplevel } {
	bind [winfo toplevel $f] <Destroy> "$active_text tag remove search 1.0 end"
	DialogWinTop::CreateWindow $f
    } else {
	bind $f <Destroy> "[list $active_text tag remove search 1.0 end]
	    [list unset ::RamDebugger::SearchToolbar]"
    }
}

proc RamDebugger::SearchReplace { w what args } {
    variable text
    variable searchstring
    variable replacestring
    variable searchmode
    variable searchcase

    switch $what {
	beginreplace {
	    Search $w $what
	} 
	replace {
	    if { [llength [$text tag ranges search]] == 0 } {
		Search $w beginreplace
	    } else {
		foreach "idx1 idx2" [$text tag ranges search] break
		set txt $replacestring
		if { $searchmode == "-regexp" } {
		    if { $searchcase } {
		        regsub $searchstring [$text get $idx1 $idx2] \
		            $replacestring txt
		    } else {
		        regsub -nocase $searchstring [$text get $idx1 $idx2] \
		            $replacestring txt
		    }
		} elseif { !$searchcase && \
		    [string tolower $searchstring] eq $searchstring && \
		    [string tolower $replacestring] eq $replacestring } {
		    
		    set otxt [$text get $idx1 $idx2]
		    switch -- $otxt \
		        [string tolower $otxt] { set txt [string tolower $txt] } \
		        [string toupper $otxt] { set txt [string toupper $txt] } \
		        [string totitle $otxt] { set txt [string totitle $txt] }
		}
		$text delete $idx1 $idx2
		$text insert $idx1 $txt
		Search $w beginreplace
	    }
	}
	replaceall {
	    set inum 0
	    while 1 {
		if { [llength [$text tag ranges search]] == 0 } {
		    catch {Search $w beginreplace 1}
		}
		if { [llength [$text tag ranges search]] == 0 } { return }
		foreach "idx1 idx2" [$text tag ranges search] break
		set txt $replacestring
		if { $searchmode == "-regexp" } {
		    if { $searchcase } {
		        regsub $searchstring [$text get $idx1 $idx2] \
		            $replacestring txt
		    } else {
		        regsub -nocase $searchstring [$text get $idx1 $idx2] \
		            $replacestring txt
		    }
		} elseif { !$searchcase && \
		    [string tolower $searchstring] eq $searchstring && \
		    [string tolower $replacestring] eq $replacestring } {

		    set otxt [$text get $idx1 $idx2]
		    switch -- $otxt \
		        [string tolower $otxt] { set txt [string tolower $txt] } \
		        [string toupper $otxt] { set txt [string toupper $txt] } \
		        [string totitle $otxt] { set txt [string totitle $txt] }
		}
		$text delete $idx1 $idx2
		$text insert $idx1 $txt
		incr inum
		catch {Search $w beginreplace 1}
	    }
	    SetMessage "Replaced $inum words"
	}
	cancel {
	    foreach "istoplevel f" $args break
	    if { $istoplevel } {
		destroy [winfo toplevel $f]
	    } else {
		destroy $f
	    }
	    return
	}
	contextual {
	    foreach "replace x y" $args break
	    set menu $w._menu
	    catch { destroy $menu }
	    menu $menu -tearoff 0
	    if { !$replace } {
		$menu add command -label Replace -command \
		    [list RamDebugger::SearchWindow 1]
	    } else {
		$menu add command -label Search -command \
		    [list RamDebugger::SearchWindow]
	    }
	    $menu add separator
	    $menu add radio -label Backward -variable ::RamDebugger::SearchType \
		-value -backwards
	    $menu add radio -label Forward -variable ::RamDebugger::SearchType \
		-value -forwards
	    $menu add separator
	    $menu add check -label "From beginning" -variable \
		::RamDebugger::searchFromBegin
	    $menu add check -label "Consider case" -variable \
		::RamDebugger::searchcase
	    $menu add check -label "Regexp mode" -variable \
		::RamDebugger::searchmode -onvalue -regexp -offvalue -exact
	    $menu add separator
	    $menu add check -label "Auto close toolbar" -variable \
		::RamDebugger::options(SearchToolbar_autoclose) \
		-command RamDebugger::SearchWindow_autoclose
	    $menu add separator
	    if { !$replace } {
		$menu add command -label Close -command \
		    [list RamDebugger::SearchWindow]
	    } else {
		$menu add command -label Close -command \
		    [list RamDebugger::SearchWindow 1]
	    }
	    tk_popup $menu $x $y
	}
    }
}

proc RamDebugger::inline_replace { w search_entry } {
    variable text
    
    set focus [focus]
    set grab [grab current]
    
    set x [dict get [place info $search_entry] -x]
    set x1 [expr {$x+[winfo width $search_entry]+2}]
    destroy $w.replace
    entry $w.replace -width 25 -textvariable RamDebugger::replacestring -relief solid -bd 1
    place $w.replace -in $w -x $x1 -rely 1 -y -1 -anchor sw
    set err [catch { clipboard get } data]
    if { !$err && [string length $data] < 20 } {
	$w.replace delete 0 end
	$w.replace insert end $data
	$w.replace selection range 0 end
    }
    focus $w.replace
    grab $w.replace
    
    bind $w.replace <Return> [list RamDebugger::inline_replace_end $w $focus $grab $search_entry accept]
    bind $w.replace <Control-i> [list RamDebugger::inline_replace_end $w $focus $grab $search_entry accept]
    bind $w.replace <Escape> [list RamDebugger::inline_replace_end $w $focus $grab $search_entry end]
    bind $w.replace <1> "[list RamDebugger::inline_replace_end $w $focus $grab $search_entry end];break"
    
    set cmd1 "[list RamDebugger::inline_replace_end $w $focus $grab $search_entry accept];"
    append cmd1 "[list RamDebugger::textPaste_insert_after $text];"
    set cmd2 "RamDebugger::Search $w iforward"
    set cmd3 "RamDebugger::Search $w iforward_all"
    bind $w.replace <Control-j> "$cmd1; $cmd2; break"
    bind $w.replace <Control-J> "$cmd1; $cmd3; break"

    set msg [_ "Press <Return> or Ctrl+I to continue search and replace\nCtrl+J to replace\nCtrl+Shift+J to replace all"]
    tooltip::tooltip $w.replace $msg
    
    label $w.searchl2 -text $msg -justify left -bd 1 -relief solid
    set x2 [expr {$x1+[winfo reqwidth $w.replace]+2}]
    place $w.searchl2 -in $w -x $x2 -rely 1 -y -1 -anchor sw
    bind $w.replace <Destroy> [list destroy $w.searchl2]
}

proc RamDebugger::inline_replace_end { w focus grab search_entry what } {
    variable text
    
    if { $what eq "accept" } {
	clipboard clear
	clipboard append $RamDebugger::replacestring
	
	set cmd1 "[list RamDebugger::textPaste_insert_after $text];RamDebugger::Search $w iforward"
	set cmd2 "[list RamDebugger::textPaste_insert_after $text];RamDebugger::Search $w iforward_all"
	bind $search_entry <Control-j> "$cmd1; break"
	bind $search_entry <Control-J> "$cmd2; break"
    }
    destroy $w.replace
    if { $focus ne "" } { focus -force $focus }
    if { $grab ne "" } { grab $grab }
}

proc RamDebugger::textPaste_insert_after { w } {

    set err [catch {::tk::GetSelection $w CLIPBOARD} sel]
    if { $err } { return }
    
    set oldSeparator [$w cget -autoseparators]
    if {$oldSeparator} {
	$w configure -autoseparators 0
	$w edit separator
    }
    catch { $w delete sel.first sel.last }

    set d [expr {[string length $sel]-[string length $::RamDebugger::searchstring]}]
    set RamDebugger::SearchPos [$w index "insert+${d}c"]
    $w insert insert $sel
    if {$oldSeparator} {
	$w edit separator
	$w configure -autoseparators 1
    }
}

proc RamDebugger::Search_get_selection { active_text } {
    if { [$active_text tag ranges sel] ne "" } {
	set txt [$active_text get {*}[$active_text tag ranges sel]]
    } else {
	set err [catch { clipboard get } txt]
	if { $err } { set txt "" }
    }
    set save_traces [trace info variable RamDebugger::searchstring]
    foreach i $save_traces {
	trace remove variable RamDebugger::searchstring {*}$i
    }
    set RamDebugger::searchstring $txt
    set RamDebugger::Lastsearchstring $txt
    
    foreach i $save_traces {
	trace add variable RamDebugger::searchstring {*}$i
    }
}

proc RamDebugger::Search_goHome { active_text } {
    
    $active_text mark set insert 1.0
    $active_text see 1.0
    
    set ::RamDebugger::SearchIni 1.0
    set ::RamDebugger::SearchPos 1.0
#     if { [info exists ::RamDebugger::Lastsearchstring] } {
#         set ::RamDebugger::lastwascreation $::RamDebugger::Lastsearchstring
#     } else { set ::RamDebugger::lastwascreation "" }
    set ::RamDebugger::Lastsearchstring $RamDebugger::searchstring
}    

proc RamDebugger::Search { w what { raiseerror 0 } {f "" } } {
    variable text
    variable options
    variable text_secondary

    if { [info exists text_secondary] && [focus -lastfor $text] eq $text_secondary } {
	set active_text $text_secondary
    } else { set active_text $text }

    if { [string match begin* $what] } {
	if { $what == "begin" && ![info exists ::RamDebugger::SearchToolbar] } {
	    destroy [winfo toplevel $f]
	}
	if { $::RamDebugger::searchstring == "" } {
	    return
	}
	set ipos [lsearch -exact $options(old_searchs) $::RamDebugger::searchstring]
	if { $ipos != -1 } {
	    set options(old_searchs) [lreplace $options(old_searchs) $ipos $ipos]
	}
	set options(old_searchs) [linsert [lrange $options(old_searchs) 0 6] 0 \
		$::RamDebugger::searchstring]
	if { $::RamDebugger::replacestring != "" } {
	    set ipos [lsearch -exact $options(old_replaces) $::RamDebugger::replacestring]
	    if { $ipos != -1 } {
		set options(old_replaces) [lreplace $options(old_replaces) $ipos $ipos]
	    }
	    set options(old_replaces) [linsert [lrange $options(old_replaces) 0 6] 0 \
		    $::RamDebugger::replacestring]
	}        
	if { $::RamDebugger::searchFromBegin } {
	    if { $::RamDebugger::SearchType == "-forwards" } {
		set idx 1.0
	    } else { set idx [$active_text index end] }
	} else {
	    set idx [$active_text index insert]
	}
	if { [info exists ::RamDebugger::SearchToolbar] } {
	    set ::RamDebugger::searchFromBegin 0
	}
	if { $what == "beginreplace" } {
	    set ::RamDebugger::searchFromBegin 0
	}
	set ::RamDebugger::SearchIni $idx
	set ::RamDebugger::SearchPos $idx
	set ::RamDebugger::Lastsearchstring ""
    } elseif { $what != "any" } {
	if { ![winfo exists $w.search] } {
	    entry $w.search -width 25 -textvariable RamDebugger::searchstring -relief solid -bd 1
	    place $w.search -in $w -x 2 -rely 1 -y -1 -anchor sw

	    set msg1 [_ "Press Ctrl+J to replace"]
	    set msg2 [_ "Press Ctrl+C to use selection for search"]
	    set msg3 [_ "Press Home to search from beginning"]
	    tooltip::tooltip $w.search $msg1\n$msg2\n$msg3

	    focus $active_text
	    bindtags $active_text [linsert [bindtags $active_text] 0 $w.search]
	    #bind $w.search <FocusOut> "destroy $w.search ; break"
	    bind $w.search <Escape> "destroy $w.search ; break"
	    bind $w.search <KeyPress> [list if { ![string equal "%A" ""] && ([string is print %A] || \
		[string is space %A]) } \
		"$w.search icursor end; tkEntryInsert $w.search %A ; break" else \
		"destroy $w.search ; break"]
	    bind $w.search <Delete> "$w.search icursor end; $w.search delete insert ; break"
	    bind $w.search <BackSpace> "$w.search icursor end; tkEntryBackspace $w.search ; break"
	    bind $w.search <1> "destroy $w.search"
	    bind $w.search <3> "destroy $w.search"
	    foreach i [list F1 F2 F5 F6 F9 F10 F11] {
		bind $w.search <$i> "destroy $w.search"
	    }
	    bind $w.search <Return> "destroy $w.search ; break"
	    bind $w.search <Control-i> "RamDebugger::Search $w iforward ; break"
	    bind $w.search <Control-r> "RamDebugger::Search $w ibackward ; break"
	    bind $w.search <Control-g> "RamDebugger::Search $w stop ; break"
	    bind $w.search <Control-c> "RamDebugger::Search_get_selection $active_text; break"
	    bind $w.search <Home> "RamDebugger::Search_goHome $active_text; break"
	    
	    if { $active_text eq $text } {
		bind $w.search <Control-j> "RamDebugger::inline_replace $w $w.search; break"
		
		label $w.searchl1 -text $msg1
		set x1 [expr {2+[winfo reqwidth $w.search]+2}]
		place $w.searchl1 -in $w -x $x1 -rely 1 -y -1 -anchor sw
		after 2000 [list catch [list destroy $w.searchl1]]
	    }
	    set ::RamDebugger::searchstring ""
	    trace var RamDebugger::searchstring w "[list RamDebugger::Search $w {}];#"
	    bind $w.search <Destroy> [list trace vdelete RamDebugger::searchstring w \
		"[list RamDebugger::Search $w {}];#"]
	    bind $w.search <Destroy> "+ [list destroy $w.searchl1]"
	    bind $w.search <Destroy> "+ [list bindtags $active_text [lreplace [bindtags $active_text] 0 0]] ; break"
	    foreach i [bind Text] {
		if { [lsearch -exact [list <Motion>] $i] != -1 } { continue }
		if { [bind $w.search $i] == "" } {
		    if { [string match *nothing* [bind Text $i]] } {
		        bind $w.search $i [bind Text $i]
		    } else {
		        bind $w.search $i "destroy $w.search" }
		}
	    }
	    if { $what eq "iforward_get_insert" } {
		set range [GetSelOrWordInIndex -return_range 1 insert]
		$active_text mark set insert [lindex $range 0]
	    }
	    set idx [$active_text index insert]
	    
	    if { $idx == "" } { set idx 1.0 }
	    set ::RamDebugger::SearchIni $idx
	    set ::RamDebugger::SearchPos $idx
	    set ::RamDebugger::searchcase -1
	    set ::RamDebugger::searchmode -exact
	    
	    if { $what eq "iforward_get_insert" } {
		set ::RamDebugger::Lastsearchstring ""
		set ::RamDebugger::SearchType "-forwards"
		set ::RamDebugger::searchstring [GetSelOrWordInIndex insert]
		return
	    } elseif { [info exists ::RamDebugger::Lastsearchstring] } {
		set ::RamDebugger::lastwascreation $::RamDebugger::Lastsearchstring
	    } else {
		set ::RamDebugger::lastwascreation ""
	    }
	    set ::RamDebugger::Lastsearchstring ""
	} else {
	    if { $::RamDebugger::searchstring == "" && $::RamDebugger::lastwascreation != "" } {
		set ::RamDebugger::searchstring $::RamDebugger::lastwascreation
		set ::RamDebugger::lastwascreation ""
		return ;# the trace will continue the search
	    }
	    set ::RamDebugger::lastwascreation ""
	}
    }
    switch $what {
	iforward - iforward_all {
	    set ::RamDebugger::SearchType -forwards
	}
	ibackward {
	    set ::RamDebugger::SearchType -backwards
	}
	any {
	    if { ![info exists ::RamDebugger::Lastsearchstring] } {
		WarnWin "Before using 'Continue search', use 'Search'" $w
		return
	    }
	}
	stop {
	    destroy $w.search
	    return
	}
    }
    if { $RamDebugger::searchstring != "" || $RamDebugger::Lastsearchstring != "" } {
	if { $RamDebugger::SearchType == "-forwards" } {
	    set stopindex end
	} else {
	    set stopindex 1.0
	}
	if { $RamDebugger::searchstring == $RamDebugger::Lastsearchstring } {
	    set len [string length $RamDebugger::searchstring]
	    if { $RamDebugger::SearchType == "-forwards" } {
		set idx [$active_text index $RamDebugger::SearchPos+${len}c]
	    } else {
		set idx [$active_text index $RamDebugger::SearchPos-${len}c]
	    }
	} elseif { [string length $RamDebugger::searchstring] < \
		       [string length $RamDebugger::Lastsearchstring] } {
	    set idx $RamDebugger::SearchIni
	} else { set idx $RamDebugger::SearchPos }

	set search_options $RamDebugger::SearchType
	lappend search_options $::RamDebugger::searchmode
	lappend search_options -count ::len

	if { $::RamDebugger::searchcase == 1 } {
	    # nothing
	} elseif { $::RamDebugger::searchcase == 0 } {
	    lappend search_options -nocase
	} elseif { $RamDebugger::searchstring == [string tolower $RamDebugger::searchstring] } {
	    lappend search_options -nocase
	}
	lappend search_options --
	set idx [eval $active_text search $search_options [list $RamDebugger::searchstring] \
		     $idx $stopindex]
	if { $idx == "" } {
	    if { $what eq "iforward_all" } {
		destroy $w.search
		return
	    }
	    if { $raiseerror } {
		error "Search not found"
	    }
	    SetMessage "Search not found"
	    bell
	    if { [winfo exists $w.search] } {
		set bg [$w.search cget -background]
		if { $bg ne "red" } {
		    $w.search configure -bg red
		    after 1000 [list catch [list $w.search configure -bg $bg]]
		}
	    }

	    if { $RamDebugger::SearchType == "-forwards" } {
		set RamDebugger::SearchPos 1.0
	    } else {
		set RamDebugger::SearchPos end
	    }
	} else {
	    set RamDebugger::SearchPos $idx
	    #set len [string length $RamDebugger::searchstring]
	    if { $RamDebugger::SearchType == "-forwards" } {
		set idx2 [$active_text index $RamDebugger::SearchPos+${::len}c]
	    } else {
		set idx2 $RamDebugger::SearchPos
		set RamDebugger::SearchPos [$active_text index $RamDebugger::SearchPos+${::len}c]
	    }
	    if { $active_text eq $text } {
		set ed [$active_text cget -editable]
		$active_text conf -editable 1
	    } else {
		set state [$text_secondary cget -state]
		$text_secondary configure -state normal
	    }
	    $active_text tag remove sel 1.0 end
	    $active_text tag remove search 1.0 end
	    if { $RamDebugger::SearchType == "-forwards" } {
		set idxA $RamDebugger::SearchPos
		set idxB $idx2
	     } else {
		set idxB $RamDebugger::SearchPos
		set idxA $idx2
	    }
	    $active_text tag add sel $idxA $idxB
	    #if { $what == "beginreplace" } 
		$active_text tag add search $idxA $idxB
		$active_text tag conf search -background [$active_text tag cget sel -background] \
		    -foreground [$active_text tag cget sel -foreground]
	    #
	    if { $active_text eq $text } {
		$active_text conf -editable $ed
	    } else {
		$text_secondary configure -state $state
	    }
	    $active_text mark set insert $idx2
	    $active_text see $RamDebugger::SearchPos
	    
	    if { $what eq "iforward_all" } {
		tk_textPaste $text
		after idle [list RamDebugger::Search $w iforward_all]
	    }
	}
	set RamDebugger::Lastsearchstring $RamDebugger::searchstring
    }
}

################################################################################
# OpenProgram OpenConsole
################################################################################

proc RamDebugger::OpenProgram { what args } {
    variable MainDir
    variable currentfile
    
    set argv $args
    switch $what {
	visualregexp { set file [file join $MainDir addons visualregexp visual_regexp.tcl] }
	tkcvs {
	    set file [file join $MainDir addons tkcvs bin tkcvs.tcl]
	    if {  [llength $args] == 0 && [file isdirectory [file dirname $currentfile]] } {
		lappend argv -dir [file dirname $currentfile]
	    }
	}
	tkdiff { set file [file join $MainDir addons tkcvs bin tkdiff.tcl] }
    }
    if { ![interp exists $what] } {
	interp create $what
    }
    interp alias $what exit_interp "" interp delete $what
    $what eval [list proc exit { args } "destroy . ; exit_interp"]
    interp alias $what puts "" RamDebugger::_OpenProgram_puts
    $what eval [list load {} Tk]
    $what eval [list set argc [llength $argv]]
    $what eval [list set argv $argv]
    $what eval [list source $file]
}

proc RamDebugger::_OpenProgram_puts { args } {
    variable textOUT
    
    lassign [list stdout "" 1] channelId string hasnewline
    switch [llength $args] {
	1 { lassign $args string }
	2 { lassign $args channelId string }
	3 { lassign $args channelId string hasnewline }
	default {
	    error "error in RamDebugger::_OpenProgram_puts"
	}
    } 
    if { [info exists textOUT] && [winfo exists $textOUT] } {
	RamDebugger::RecieveOutputFromProgram $channelId $string $hasnewline
    } elseif { $channelId eq "stderr" } {
	#tk_messageBox -message $string
    }
}

proc RamDebugger::revalforTkcon { comm } {
    variable remoteserver

    if { $remoteserver == "" } {
	catch { tkcon_puts stderr "There is no current debugged program" }
	::tkcon::Attach {}
	set ::tkcon::PRIV(appname) ""
	set ::tkcon::attachdebugged 0
	::tkcon::Prompt \n [::tkcon::CmdGet $::tkcon::PRIV(console)]
	return [uplevel #0 $comm]
    }
    return [EvalRemoteAndReturn $comm]
}

proc RamDebugger::OpenConsole {} {
    variable MainDir
    variable textOUT

    set tkcon [file join $MainDir addons tkcon tkcon.tcl]

    if { $tkcon == "" } {
	WarnWin "Could not find tkcon"
	return
    }

    catch { font delete tkconfixed }
    catch { namespace delete ::tkcon }

    set tkconprefs {
	if { $::tcl_platform(platform) == "windows" } {
	    tkcon font "MS Sans Serif" 8
	} else {
	   tkcon font "new century schoolbook" 12
	}
	# Keep 50 commands in history
	set ::tkcon::OPT(history)  50
	set ::tkcon::OPT(buffer) 5000
	
	# Use a pink prompt
	set ::tkcon::COLOR(prompt) red
	# set tkcon(autoload) "Tk"
	set ::tkcon::OPT(cols)  80
	set ::tkcon::OPT(rows) 24
	set ::tkcon::OPT(gc-delay) 0
	#set tkcon(cols) 60
	#set tkcon(rows) 18
	catch [list namespace import RamDebugger::*]

	set menubar $::tkcon::PRIV(menubar)
	$menubar insert [$menubar index end] cascade -label "RamDebugger" -underline 0 \
	    -menu $menubar.ramm
	set m $menubar.ramm
	menu $m
	$m add check -label "Attach to debugged program" -variable ::tkcon::attachdebugged \
	    -command {
		if { !$::tkcon::attachdebugged } {
		    ::tkcon::Attach {}
		    set ::tkcon::PRIV(appname) ""
		    ::tkcon::Prompt \n [::tkcon::CmdGet $::tkcon::PRIV(console)]
		} else {
		    interp alias {} ::tkcon::EvalAttached {} RamDebugger::revalforTkcon
		    set ::tkcon::PRIV(appname) "RamDebugger"
		    ::tkcon::Prompt \n [::tkcon::CmdGet $::tkcon::PRIV(console)]
		    RamDebugger::revalforTkcon ""
		}
	    }
	puts "Welcome to Ramdebugger inside tkcon. Use 'rhelp' for help"
    }

    if { [info exists textOUT] && [winfo exists $textOUT] } {
	set channel stdout
	set line [scan [$textOUT index end-1c] %d]
	if { $line > 50 } {
	    set idx0 [expr {$line-50}].0
	    append tkconprefs [list puts -nonewline $channel "---last lines---"]\n
	} else { set idx0 1.0 }
	foreach "k v i" [$textOUT dump -tag -text 1.0 end-1c] {
	    switch $k {
		tagon { if { $v eq "red" } { set channel stderr } }
		tagoff { if { $v eq "red" } { set channel stdout } }
		text {
		    if { [$textOUT compare $i > $idx0] } {
		        append tkconprefs [list puts -nonewline $channel $v]\n
		    }
		}
	    }
	}
    }
    append tkconprefs [list puts "Welcome to Ramdebugger inside tkcon. Use 'rhelp' for help"]\n

    if { [winfo exists .tkcon] } { destroy .tkcon }

    set argv [list -rcfile "" -exec "" -root .tkcon -eval $tkconprefs]

    uplevel \#0 [list source $tkcon]
    uplevel \#0 ::tkcon::Init $argv
    proc ::tkcon::FinalExit { args } { destroy .tkcon }
}

################################################################################
# DoinstrumentThisfile
################################################################################

proc RamDebugger::DoinstrumentThisfile { file } {
    variable text
    variable options
    variable WindowFilesList

    if { ![info exists options(instrument_source)] } {
	set options(instrument_source) auto
    }
    if { [string match auto* $options(instrument_source)] } {
	set file [filenormalize $file]
	if { [lsearch -exact $WindowFilesList $file] != -1 } {
	    return 1
	} elseif { $options(instrument_source) eq "autoprint" } {
	    return 2
	} else { return 0 }
    } elseif { $options(instrument_source) == "always" } {
	return 1
    } elseif { $options(instrument_source) == "never" } {
	return 0
    }

    set w [dialogwin_snit $text._ask -title "Debugged program source" -okname - -cancelname OK]
    set f [$w giveframe]

    label $f.l1 -text "The debugged program is trying to source file:" -grid "0 nw"
    set fname $file
    if { [string length $fname] > 60 } {
	set fname ...[string range $fname end-57 end]
    }
    label $f.l2 -text $fname -width 60 -grid "0 nw px8"
    label $f.l3 -text "What do you want to do?" -grid "0 nw"

    set f1 [frame $f.f1 -grid 0]

    radiobutton $f1.r1 -text "Instrument this file" -grid "0 w" -var [$w give_uservar opt] \
	-value thisfile
    radiobutton $f1.r2 -text "Instrument all files" -grid "0 w" -var [$w give_uservar opt] \
	-value always
    radiobutton $f1.r3 -text "Do not instrument this file" -grid "0 w" -var [$w give_uservar opt] \
	-value thisfileno
    radiobutton $f1.r4 -text "Do not instrument any file" -grid "0 w" -var [$w give_uservar opt] \
	-value never
    radiobutton $f1.r5 -text "Instrument only load files (auto)" -grid "0 w" -var \
	[$w give_uservar opt] -value auto
    radiobutton $f1.r6 -text "... and print source" -grid "0 w" -var [$w give_uservar opt] \
	-value autoprint

    if { $options(instrument_source) == "ask_yes" } {
	$w set_uservar_value opt thisfile
    } else { $w set_uservar_value opt thisfileno }

    label $f.l4 -text "Check preferences to change these options" -grid "0 nw"

    supergrid::go $f

    bind $w <Return> [list $w invokecancel]
    $w focuscancel

    set action [$w createwindow]
    set opt [$w give_uservar_value opt]
    destroy $w


    switch $opt {
	thisfile {
	    set options(instrument_source) ask_yes
	    return 1
	}
	thisfileno {
	    set options(instrument_source) ask_no
	    return 0
	}
    }
    
    set options(instrument_source) $opt

    if { [string match auto* $options(instrument_source)] } {
	set file [filenormalize $file]
	if { [lsearch -exact $WindowFilesList $file] != -1 || [GiveInstFile $file 1 P] != "" } {
	    return 1
	} elseif { $options(instrument_source) eq "autoprint" } {
	    return 2
	} else { return 0 }
    } elseif { $options(instrument_source) == "always" } {
	return 1
    } elseif { $options(instrument_source) == "never" } {
	return 0
    }

}

################################################################################
# Position stack
################################################################################


proc RamDebugger::DisplayPositionsStack { args } {
    variable options
    variable text
    variable text_secondary
    variable currentfile
    variable currentfile_secondary

    if { [info exists DialogWinTop::user(list)] && [winfo exits $DialogWinTop::user(list)] } {
	raise [winfo toplevel $DialogWinTop::user(list)]
	DisplayPositionsStackDo refresh
	return
    }

    if { [info exists text_secondary] && [focus -lastfor $text] eq $text_secondary } {
	set curr_text $text_secondary
    } else {
	set curr_text $text
    }
    set nowline [scan [$curr_text index insert] %d]
    foreach "curr_text nowline" $args break

    if { [info exists text_secondary] && $curr_text eq $text_secondary } {
	set file $currentfile_secondary
    } else {
	set curr_text $text
	set file $currentfile
    }
    set DialogWinTop::user(curr_text) $curr_text

    set commands [list "RamDebugger::DisplayPositionsStackDo delete" \
	    "RamDebugger::DisplayPositionsStackDo up" \
	    "RamDebugger::DisplayPositionsStackDo down" \
	    "RamDebugger::DisplayPositionsStackDo go" \
	    "RamDebugger::DisplayPositionsStackDo cancel"]

    set f [DialogWinTop::Init $curr_text "Positions stack window" separator $commands \
	    [list Up Down View] Delete Close]
    set w [winfo toplevel $f]

    set sw [ScrolledWindow $f.lf -relief sunken -borderwidth 0 -grid "0 2"]
    
    set DialogWinTop::user(list) [tablelist::tablelist $sw.lb -width 55\
		  -exportselection 0 \
		  -columns [list \
		                20 File        right \
		                5  Line left \
		                20 Context left \
		                30 Directory left \
		                ] \
		  -labelcommand tablelist::sortByColumn \
		  -background white \
		  -selectbackground navy -selectforeground white \
		  -stretch "2 3" -selectmode extended \
		  -highlightthickness 0]

    $sw setwidget $DialogWinTop::user(list)
    
    supergrid::go $f
    
    focus $DialogWinTop::user(list)
    bind [$DialogWinTop::user(list) bodypath] <Button-1> {
	focus [$DialogWinTop::user(list) bodypath]
    }
    bind [$DialogWinTop::user(list) bodypath] <Double-1> {
	RamDebugger::DisplayPositionsStackDo go
    }
    bind [$DialogWinTop::user(list) bodypath] <Return> {
	RamDebugger::DisplayPositionsStackDo go
    }
    bind [$DialogWinTop::user(list) bodypath] <ButtonPress-3> \
	    [bind TablelistBody <ButtonPress-1>]

    bind [$DialogWinTop::user(list) bodypath] <KeyPress-Delete> {
	RamDebugger::DisplayPositionsStackDo delete
    }

    bind [$DialogWinTop::user(list) bodypath] <ButtonPress-3> {
	set tablelist::win [winfo parent %W]
	set tablelist::x [expr {%x + [winfo x %W]}]
	set tablelist::y [expr {%y + [winfo y %W]}]
	set cell [$tablelist::win nearestcell $tablelist::x $tablelist::y]
	foreach {row col} [ split $cell ,] break
	if { ![$tablelist::win selection includes $row] } {
	    $tablelist::win selection clear 0 end
	    $tablelist::win selection set $row
	}
	RamDebugger::DisplayPositionsStackDo contextual %X %Y
    }
    
    foreach i $options(saved_positions_stack) {
	foreach "file_in line context" $i break
	$DialogWinTop::user(list) insert end [list [file tail $file_in] $line \
		$context [file dirname $file_in]]
	if { [AreFilesEqual $file $file_in] && $line == $nowline } {
	    $DialogWinTop::user(list) selection set end
	}
    }
    $DialogWinTop::user(list) see end
    DialogWinTop::CreateWindow $f
}

proc RamDebugger::DisplayPositionsStackDo { what args } {
    variable options
    variable text
    variable currentfile
    variable currentfile_secondary
    variable text_secondary

    set w [winfo toplevel $DialogWinTop::user(list)]
    set curr_text $DialogWinTop::user(curr_text)
    
    switch $what {
	cancel {
	    unset DialogWinTop::user(list) DialogWinTop::user(curr_text)
	    destroy $w
	    return
	}
	delete {
	    set curr [$DialogWinTop::user(list) curselection]
	    if { [llength $curr] == 0 } {
		WarnWin "Select one or more positions to delete in the stack" $w
	    } else {
		set ns ""
		set ipos 0
		foreach i $options(saved_positions_stack) {
		    if { [lsearch $curr $ipos] == -1 } {
		        lappend ns $i
		    }
		    incr ipos
		}
		set options(saved_positions_stack) $ns
		ManagePositionsImages
	    }
	}
	contextual {
	    catch { destroy $w.menu }
	    set menu [menu $w.menu]
	    $menu add command -label "Up" -command "RamDebugger::DisplayPositionsStackDo up"
	    $menu add command -label "Down" -command "RamDebugger::DisplayPositionsStackDo down"
	    $menu add command -label "View" -command "RamDebugger::DisplayPositionsStackDo go"
	    $menu add separator
	    $menu add command -label "Delete" -command "RamDebugger::DisplayPositionsStackDo delete"
	    foreach "x y" $args break
	    tk_popup $menu $x $y
	    return
	}
	up {
	    set curr [$DialogWinTop::user(list) curselection]
	    if { [llength $curr] == 0 } {
		WarnWin "Select one or more positions to move up in the stack" $w
	    } else {
		if { [lindex $curr 0] > 0 } {
		    set tomove [lrange $options(saved_positions_stack) [lindex $curr 0] [ lindex $curr end]]
		    set options(saved_positions_stack) [lreplace $options(saved_positions_stack) [lindex $curr 0] \
		                                 [lindex $curr end]]
		    set options(saved_positions_stack) [eval linsert [list $options(saved_positions_stack)] \
		                                 [expr {[lindex $curr 0]-1}] $tomove]
		}
	    }
	}
	down {
	    set curr [$DialogWinTop::user(list) curselection]
	    if { [llength $curr] == 0 } {
		WarnWin "Select one or more positions to move down in the stack" $w
	    } else {
		if { [lindex $curr end] < [llength  $options(saved_positions_stack)]  } {
		    set tomove [lrange $options(saved_positions_stack) [lindex $curr 0] [ lindex $curr end]]
		    set options(saved_positions_stack) [lreplace $options(saved_positions_stack) [lindex $curr 0] \
		                                 [lindex $curr end]]
		    set options(saved_positions_stack) [eval linsert [list $options(saved_positions_stack)] \
		                                 [expr {[lindex $curr 0]+1}] $tomove]
		}
	    }
	}
	go {
	    set curr [$DialogWinTop::user(list) curselection]
	    if { [llength $curr] != 1 } {
		WarnWin "Select just one position in the stack to go to it" $w
	    } else {
		foreach "file line -" [lindex $options(saved_positions_stack) $curr] break

		if { $curr_text eq $text || ![info exists text_secondary] } {
		    set active_file $currentfile
		} else { set active_file $currentfile_secondary }

		if { ![AreFilesEqual $file $active_file] } {
		    if { $curr_text eq $text || ![info exists text_secondary] } {
		        RamDebugger::OpenFileF $file
		    } else {
		        OpenFileSecondary $file
		    }
		}
		$curr_text mark set insert $line.0
		$curr_text see $line.0
		SetMessage "Gone to position in line $line"
	    }
	}
	refresh {
	    # nothing
	}
    }
    $DialogWinTop::user(list) delete 0 end
    foreach i $options(saved_positions_stack) {
	foreach "file line context" $i break
	$DialogWinTop::user(list) insert end [list [file tail $file] $line \
		$context [file dirname $file]]
    }
    catch { $DialogWinTop::user(list) selection set [lindex $curr 0] }
}

if { [info commands lreverse] eq "" } {
    proc lreverse { L } {
	set res {}
	set i [llength $L]
	#while {[incr i -1]>=0} {lappend res [lindex $L $i]}
	while {$i} {lappend res [lindex $L [incr i -1]]} ;# rmax
	return $res
    }
}

# what can be save or go or clean
proc RamDebugger::PositionsStack { what args } {
    variable options
    variable text
    variable text_secondary
    variable currentfile
    variable currentfile_secondary

    if { [info exists text_secondary] && [focus -lastfor $text] eq $text_secondary } {
	set curr_text $text_secondary
    } else {
	set curr_text $text
    }

    set line [scan [$curr_text index insert] %d]
    foreach "curr_text line" $args break

    if { [info exists text_secondary] && $curr_text eq $text_secondary } {
	set file $currentfile_secondary
    } else {
	set curr_text $text
	set file $currentfile
    }
    
    switch $what {
	save {
	    set ipos 0
	    foreach i $options(saved_positions_stack) {
		if { $file eq [lindex $i 0] && $line == [lindex $i 1] } {
		    return [eval PositionsStack clean $args]
		}
		incr ipos
	    }
	    set len [llength $options(saved_positions_stack)]
	    if { $len > 14 } {
		set options(saved_positions_stack) [lrange \
		        $options(saved_positions_stack) [expr {$len-14}] end]
	    }
	    set idx $line.0
	    set procname ""
	    regexp -all -line {^\s*(?:proc|method)\s+(\S+)} [$text get \
		    "$idx-200l linestart" "$idx lineend"] {} procname
	    lappend options(saved_positions_stack) [list $file $line $procname]
	    SetMessage "Saved position in line $line"
	    catch { RamDebugger::DisplayPositionsStackDo refresh }
	    ManagePositionsImages
	}
	goto {
	    set file_new [lindex $args 2]
	    set found 0
	    foreach i [lreverse $options(saved_positions_stack)] {
		if { $file_new eq [lindex $i 0] && $line == [lindex $i 1] } {
		    set found 1
		    break
		}
	    }
	    if { !$found } {
		WarnWin "Position not valid"
		return
	    }
	    if { ![AreFilesEqual $file_new $file] } {
		RamDebugger::OpenFileF $file_new
	    }
	    $curr_text mark set insert $line.0
	    $curr_text see $line.0
	    SetMessage "Gone to position in line $line"
	}
	go {
	    set ipos [expr {[llength $options(saved_positions_stack)]-1}]
	    set found 0
	    foreach i [lreverse $options(saved_positions_stack)] {
		if { $file eq [lindex $i 0] && $line == [lindex $i 1] } {
		    set found 1
		    break
		}
		incr ipos -1
	    }
	    if { $found } {
		incr ipos -1
		if { $ipos < 0 } { set ipos end }
	    } else { set ipos end }
	    set file_new ""
	    foreach "file_new line -" [lindex $options(saved_positions_stack) $ipos] break
	    if { $file_new eq "" } {
		bell
		SetMessage "Stack is void"
		return
	    }
	    if { ![AreFilesEqual $file_new $file] } {
		RamDebugger::OpenFileF $file_new
	    }
	    $curr_text mark set insert $line.0
	    $curr_text see $line.0
	    SetMessage "Gone to position in line $line"
	}
	clean {
	    set ipos [expr {[llength $options(saved_positions_stack)]-1}]
	    set found 0
	    foreach i [lreverse $options(saved_positions_stack)] {
		if { $file eq [lindex $i 0] && $line == [lindex $i 1] } {
		    set found 1
		    break
		}
		incr ipos -1
	    }
	    if { !$found } {
		WarnWin "There is no saved position at current line"
		return
	    }
	    set options(saved_positions_stack) [lreplace $options(saved_positions_stack) \
		    $ipos $ipos]
	    SetMessage "Clean position at current line"
	    catch { RamDebugger::DisplayPositionsStackDo refresh }
	    ManagePositionsImages
	}
    }
}

################################################################################
# Macros
################################################################################


proc RamDebugger::MacrosDo { what { f "" } } {
    variable text
    variable options

    switch $what {
	edit {
	    if { $f != "" } { destroy [winfo toplevel $f] }
	    OpenFileF *Macros*
	}
	execute {
	    set w [winfo toplevel $f]
	    set idx [$DialogWinTop::user($w,list) curselection]
	    if { [llength $idx] != 1 } {
		WarnWin "It is necessary to select one macro in order to execute it"
		return
	    }
	    set macro [lindex [$DialogWinTop::user($w,list) get $idx] 0]
	    #tk_messageBox -message [array get ::env]
	    RamDebugger::Macros::$macro $text
	}
	default {
	    set ret [DialogWinTop::messageBox -default ok -icon warning -message \
		         "Are you sure to delete all your macros and load default macros?" -parent $f \
		         -title "delete all macros and update" -type okcancel]
	    if { $ret == "ok" } {
		catch { unset options(MacrosDocument) }
		AddActiveMacrosToMenu $Macros::mainframe $Macros::menu
		destroy [winfo toplevel $f]
	    }
	}
	cancel {
	    destroy [winfo toplevel $f]
	}
    }
}

proc RamDebugger::Macros { parent } {

    set commands [list "RamDebugger::MacrosDo execute" "RamDebugger::MacrosDo edit" \
		     "RamDebugger::MacrosDo default" "RamDebugger::MacrosDo cancel"]
    set f [DialogWinTop::Init $parent "Macros" separator $commands \
	       [list Edit Default] Execute]
    set w [winfo toplevel $f]

    TitleFrame $f.f2 -text "Defined macros" -grid 0
    set f2 [$f.f2 getframe]

    set sw [ScrolledWindow $f2.lf -relief sunken -borderwidth 0 -grid "0 ewns"]
    
    set DialogWinTop::user($w,list) [tablelist::tablelist $sw.lb -width 70 \
	-exportselection 0 \
	-columns [list \
	    16 "Name"        left \
	    9 "Accelerator"        center \
	    7 "In menu"         center \
	    50 "Description"        left \
	   ] \
	-labelcommand tablelist::sortByColumn \
	-background white \
	-selectbackground navy -selectforeground white \
	-stretch "0 1 3" -selectmode extended \
	-highlightthickness 0]

    $sw setwidget $DialogWinTop::user($w,list)

    bind [$DialogWinTop::user($w,list) bodypath] <Double-1> \
       "RamDebugger::MacrosDo execute $f"

    supergrid::go $f2
    supergrid::go $f

    bind [winfo toplevel $f] <Return> "DialogWinTop::InvokeOK $f"


    foreach i [info commands Macros::*] {
	set i [namespace tail $i]
	if { [info exists Macros::macrodata($i,accelerator)] } {
	    set acc $Macros::macrodata($i,accelerator)
	} else { set acc "" }
	if { [info exists Macros::macrodata($i,inmenu)] && $Macros::macrodata($i,inmenu) == 1 } {
	    set inmenu $Macros::macrodata($i,inmenu)
	} else { set inmenu "" }
	if { [info exists Macros::macrodata($i,help)] } {
	    set help $Macros::macrodata($i,help)
	} else { set help "" }

	$DialogWinTop::user($w,list) insert end [list $i $acc $inmenu $help]
    }
    DialogWinTop::CreateWindow $f
}

proc RamDebugger::_AddActiveMacrosToMenu { mainframe menu } {
    variable text

    if { [$menu index end] > 0 } { $menu delete 1 end }

    namespace eval Macros {
	eval $RamDebugger::options(MacrosDocument)
    }

    set commands ""
    foreach i [array names Macros::macrodata *,inmenu] {
	if { $Macros::macrodata($i) == 1 } {
	    regexp {^[^,]*} $i comm
	    lappend commands $comm
	}
    }
    foreach i [array names Macros::macrodata *,accelerator] {
	if { $Macros::macrodata($i) != "" } {
	    regexp {(.*),accelerator} $i {} name
	    bind all $Macros::macrodata($i) [list RamDebugger::Macros::$name $text]
	    bind $text $Macros::macrodata($i) "[list RamDebugger::Macros::$name $text];break"

	}
    }
    if { [llength $commands] } {
	$menu add separator
	DynamicHelp::register $menu menu [$mainframe cget -textvariable]
	foreach i $commands {
	    $menu add command -label $i -command [list RamDebugger::Macros::$i $text]
	    if { [info exists Macros::macrodata($i,accelerator)] && \
		     $Macros::macrodata($i,accelerator) != "" } {
		set acclabel [string trim $Macros::macrodata($i,accelerator) " <>"]
		regsub -all Control $acclabel Ctrl acclabel
		regsub -all { } $acclabel + acclabel
		regsub {><} $acclabel { } acclabel
		$menu entryconfigure end -acc $acclabel
		#bind all $Macros::macrodata($i,accelerator) [list $menu invoke [$menu index end]]
	    }
	    if { [info exists Macros::macrodata($i,help)] && $Macros::macrodata($i,help) != "" } {
		DynamicHelp::register $menu menuentry [$menu index end] $Macros::macrodata($i,help)
	    }
	}
    }
}

proc RamDebugger::AddActiveMacrosToMenu { mainframe menu } {
    variable options
    variable MainDir
    variable text

    if { ![info exists options(MacrosDocument)] } {
	set file [file join $MainDir scripts Macros_default.tcl]
	set fin [open $file r]
	set header [read $fin 256]
	if { [regexp -- {-\*-\s*coding:\s*utf-8\s*;\s*-\*-} $header] } {
	    fconfigure $fin -encoding utf-8
	}
	seek $fin 0
	set options(MacrosDocument) [read $fin]
	close $fin
    }
    catch { namespace delete Macros }
    namespace eval Macros {}
    set Macros::menu $menu
    set Macros::mainframe $mainframe

    if { [catch {_AddActiveMacrosToMenu $mainframe $menu} errstring] } {
	WarnWin "There is an error when trying to use Macros ($::errorInfo). Correct it please"
    }

}

proc RamDebugger::GiveMacrosDocument {} {
    variable options
    variable MainDir

    if { ![info exists options(MacrosDocument)] } {
	set file [file join $MainDir scripts Macros_default.tcl]
	set fin [open $file r]
	set header [read $fin 256]
	if { [regexp -- {-\*-\s*coding:\s*utf-8\s*;\s*-\*-} $header] } {
	    fconfigure $fin -encoding utf-8
	}
	seek $fin 0
	set options(MacrosDocument) [read $fin]
	close $fin
    }
    return $options(MacrosDocument)
}

proc RamDebugger::SaveMacrosDocument { data } {
    variable options

    set options(MacrosDocument) $data
    AddActiveMacrosToMenu $Macros::mainframe $Macros::menu

}

################################################################################
#    LOC
################################################################################

proc RamDebugger::UpdateProgramNameInLOC { f } {

    set DialogWin::user(dirs) ""
    set DialogWin::user(patterns) ""
    $DialogWin::user(listbox) delete 0 end
    foreach i $DialogWin::user(programs) {
	if { [lindex $i 0] == $DialogWin::user(programname) } {
	    eval $DialogWin::user(listbox) insert end [lindex $i 1]
	    set DialogWin::user(patterns) [lindex $i 2]
	    break
	}
    }
}

proc RamDebugger::AddDirToLOC {} {
    variable MainDir

    set dir [tk_chooseDirectory -initialdir $MainDir -parent $DialogWin::user(listbox) \
	    -title [_ "Select directory"] -mustexist 1]
    if { $dir == "" } { return }
    $DialogWin::user(listbox) insert end $dir
}

proc RamDebugger::DelDirFromLOC {} {
    set numdel 0
    foreach i [$DialogWin::user(listbox) curselection] {
	$DialogWin::user(listbox) delete $i
	incr numdel
    }
    if { $numdel == 0 } {
	WarnWin [_ "Select one directory to erase from list"]
    }
}

proc RamDebugger::CountLOCInFiles { parent } {
    variable options

    set f [DialogWin::Init $parent "Count LOC in files" separator]
    
    if { [catch {set DialogWin::user(programs) $options(CountLOCInFilesProgram)}] || \
	$DialogWin::user(programs) == "" } {
	if { ![info exists options(defaultdir)] } { set options(defaultdir) [pwd] }
	set DialogWin::user(programs) [list [list "RamDebugger" [list $options(defaultdir)] .tcl]]
    }
    
    set programnames ""
    foreach i $DialogWin::user(programs) {
	lappend programnames [lindex $i 0]
    }

    
    label $f.lp -text [_ "Project"]: -grid 0
    set combo [ComboBox $f.c \
	-textvariable DialogWin::user(programname) \
	-values $programnames -width 20 -helptext [_ "Enter the name of the program"] \
	-grid 1]

    label $f.l -text [_ "Select directories names:"] -grid "0 2 w"
    
    set sw [ScrolledWindow $f.lf -relief sunken -borderwidth 0 -grid "0 2"]
    set listbox [listbox $sw.ls -selectmode extended]
    set DialogWin::user(listbox) $listbox
    $sw setwidget $listbox

    set bbox [ButtonBox $f.bbox1 -spacing 0 -padx 1 -pady 1 -grid "0 2 wn"]
    $bbox add -image filenew16 \
	-highlightthickness 0 -takefocus 0 -relief link -borderwidth 1 -padx 1 -pady 1 \
	-helptext [_ "Add a directory to the list"] -command "RamDebugger::AddDirToLOC"
    $bbox add -image actcross16 \
	-highlightthickness 0 -takefocus 0 -relief link -borderwidth 1 -padx 1 -pady 1 \
	-helptext [_ "Delete dir from the list"] -command "RamDebugger::DelDirFromLOC"

    label $f.l2 -text [_ "Enter patterns (ex: .tcl .cc):"] -grid "0 2 w"
    entry $f.e2 -textvar DialogWin::user(patterns) -width 80 -grid "0 2 px3"

    trace var DialogWin::user(programname) w "RamDebugger::UpdateProgramNameInLOC $f ;#"

    set DialogWin::user(programname) [lindex $programnames 0]

    supergrid::go $f

    set action [DialogWin::CreateWindow]

    while 1 {
	if { $action == 0 } {
	    trace vdelete DialogWin::user(programname) w \
		"RamDebugger::UpdateProgramNameInLOC $f ;#"
	    DialogWin::DestroyWindow
	    return
	}
	if { $DialogWin::user(programname) == "" } {
	    tk_messageBox -icon error -message "Error. Program name cannot be void" \
		    -type ok
	} else {
	    break
	}
	set action [DialogWin::WaitForWindow]
    }
    trace vdelete DialogWin::user(programname) w "RamDebugger::UpdateProgramNameInLOC $f ;#"

    set dirs [$listbox get 0 end]
    set ipos [lsearch -exact $programnames $DialogWin::user(programname)]
    if { $ipos != -1 } {
	set DialogWin::user(programs) [lreplace $DialogWin::user(programs) $ipos $ipos]
    }
    set DialogWin::user(programs) [linsert $DialogWin::user(programs) 0 \
	[list $DialogWin::user(programname) $dirs $DialogWin::user(patterns)]]
    set options(CountLOCInFilesProgram) $DialogWin::user(programs)

    DialogWin::DestroyWindow

    WaitState 1

    CountLOCInFilesDo $parent $DialogWin::user(programname) $dirs \
	    $DialogWin::user(patterns)
    WaitState 0
}

proc RamDebugger::CountLOCInFilesCancel { f } {
    destroy [winfo toplevel $f]
}

proc RamDebugger::CountLOCInFilesDo { parent program dirs patterns } {

    set files ""
    foreach i $dirs {
	foreach j $patterns {
	    foreach k [glob -nocomplain -directory $i *$j] {
		lappend files $k
	    }
	}
    }
    set numfiles [llength $files]
    set ifiles 0
    set LOC 0
    set LOCnoBlank 0
    set LOCnoCommments 0

    ProgressVar 0
    foreach i $files {
	ProgressVar [expr {int($ifiles*100/$numfiles)}]
	set fin [open $i r]
	set header [read $fin 256]
	if { [regexp -- {-\*-\s*coding:\s*utf-8\s*;\s*-\*-} $header] } {
	    fconfigure $fin -encoding utf-8
	}
	seek $fin 0
	set txt [read $fin]
	close $fin

	set numlines [llength [split $txt "\n"]]
	set numblank [regexp -all -line {^\s*$} $txt]
	# comments are only an approximation to full comment lines of type: # or // or /* */
	set numcomments [regexp -all -line {^\s*(#|//)} $txt]
	incr numcomments [regexp -all -lineanchor {^\s*/\*.*?\*/\s*$} $txt]

	incr ifiles
	incr LOC $numlines
	incr LOCnoBlank [expr {$numlines-$numblank}]
	incr LOCnoCommments [expr {$numlines-$numblank-$numcomments}]

	set dir [file dirname $i]
	if { ![info exists ifiles_D($dir)] } {
	    set ifiles_D($dir) 0
	    set LOC_D($dir) 0
	    set LOCnoBlank_D($dir) 0
	    set LOCnoCommments_D($dir) 0
	}
	incr ifiles_D($dir)
	incr LOC_D($dir) $numlines
	incr LOCnoBlank_D($dir) [expr {$numlines-$numblank}]
	incr LOCnoCommments_D($dir) [expr {$numlines-$numblank-$numcomments}]

    }
    update
    ProgressVar 100
    
    set commands [list RamDebugger::CountLOCInFilesCancel]
    set f [DialogWinTop::Init $parent "LOC info" separator $commands \
	    "" - Close]
    set w [winfo toplevel $f]
    
    set sw [ScrolledWindow $f.lf -relief sunken -borderwidth 0 -grid "0 2"]
    text $sw.text -background white -wrap word -width 80 -height 35 \
	-exportselection 0 -font FixedFont -highlightthickness 0
    $sw setwidget $sw.text
    
    if { $::tcl_platform(platform) != "windows" } {
	$sw.text conf -exportselection 1
    }
    supergrid::go $f


    $sw.text insert end "Number of lines of code for program '$program'\n\n"
    $sw.text insert end "Number of files: $numfiles\n"
    $sw.text insert end "LOC: $LOC\n"
    $sw.text insert end "LOC (no blank lines): $LOCnoBlank\n"
    $sw.text insert end "LOC (no blank, no comments): $LOCnoCommments\n"

    $sw.text insert end "\nNumber of lines of code per directory\n\n"
    foreach dir [array names ifiles_D] {
	$sw.text insert end "Directory: $dir\n"
	$sw.text insert end "Number of files: $ifiles_D($dir)\n"
	$sw.text insert end "LOC: $LOC_D($dir)\n"
	$sw.text insert end "LOC (no blank lines): $LOCnoBlank_D($dir)\n"
	$sw.text insert end "LOC (no blank, no comments): $LOCnoCommments_D($dir)\n\n"
    }

    $sw.text conf -state disabled
    bind $sw.text <1> "focus $sw.text"
    bind [winfo toplevel $f] <Return> "DialogWinTop::InvokeCancel $f"

    DialogWinTop::CreateWindow $f
}












