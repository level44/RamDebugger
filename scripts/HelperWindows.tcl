

################################################################################
# DisplayVar
################################################################################

proc RamDebugger::DisplayVar { X Y x y } {
    variable text
    variable remoteserverType
    variable debuggerstate

    if { $debuggerstate != "debug" } { return }

    if { $X != [winfo pointerx $text] || $Y != [winfo pointery $text] } {
	return
    }
    set var [GetSelOrWordInIndex @$x,$y]
    if { $var == "" } { return }

    if { $remoteserverType == "gdb" } {
	set comm "$var"
    } else {
	set comm {
	    if { [array exists {VAR}] } {
		set ::RDC::retval [list array [array get {VAR}]]
	    } elseif { [info exists {VAR}] } {
		set ::RDC::retval [list variable [set {VAR}]]
	    } else {
		set ::RDC::errorInfo $::errorInfo
		set ::RDC::err [catch {expr {VAR}} ::RDC::val]
		if { !$::RDC::err } {
		    set ::RDC::retval [list expr $::RDC::val]
		} else {
		    set ::RDC::retval [list error {variable or expr 'VAR' does not exist}]
		    set ::errorInfo $::RDC::errorInfo
		}
	    }
	    set ::RDC::retval
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
    }
}

################################################################################
# DisplayVarWindow
################################################################################

proc RamDebugger::DisplayVarWindowEval { what f { res "" } } {
    variable remoteserver
    variable remoteserverType
    variable options

    set w [winfo toplevel $f]

    if { $what == "do" } {
	if { [string trim $DialogWinTop::user($w,expression)] == "" } {
	    set DialogWinTop::user($w,type) ""
	    return
	}
	set var $DialogWinTop::user($w,expression)

	if { $remoteserver == "" } {
	    WarnWin "Debugger is not active"
	    return
	}

	set ipos [lsearch -exact $options(old_expressions) $var]
	if { $ipos != -1 } {
	    set options(old_expressions) [lreplace $options(old_expressions) $ipos $ipos]
	}
	set options(old_expressions) [linsert [lrange $options(old_expressions) 0 20] 0 $var]

	$DialogWinTop::user($w,combo) configure -values $options(old_expressions)

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
		"RamDebugger::DisplayVarWindowEval res $f"]]

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
		if { [array exists {VAR}] } {
		    list array [array get {VAR}]
		} elseif { [info exists {VAR}] } {
		    list variable [set {VAR}]
		} else {
		    list expression [expr {VAR}]
		}
	    }
	    set comm [string map [list VAR [string trim $var]] $comm]
	}
	reval -handler [list RamDebugger::DisplayVarWindowEval res $f] $comm
    } else {
	set var $DialogWinTop::user($w,expression)
	$DialogWinTop::user($w,textv) conf -state normal
	$DialogWinTop::user($w,textv) delete 1.0 end

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
		$DialogWinTop::user($w,textv) conf -fg black
		set DialogWinTop::user($w,type) [lindex [lindex $res 1] 0]
		if { $DialogWinTop::user($w,type) == "array" } {
		    foreach "name val" [lindex [lindex $res 1] 1] {
		        $DialogWinTop::user($w,textv) insert end "${var}($name) = $val\n"
		    }
		} elseif { $DialogWinTop::user($w,aslist) } {
		    set DialogWinTop::user($w,type) list
		    if { [catch { set list [lrange [lindex [lindex $res 1] 1] 0 end] }] } {
		        $DialogWinTop::user($w,textv) insert end "Error: variable is not a list"
		    } else {
		        set ipos 0
		        foreach i $list {
		            $DialogWinTop::user($w,textv) insert end "$ipos = $i\n"
		            incr ipos
		        }
		    }
		} else {
		    $DialogWinTop::user($w,textv) insert end [lindex [lindex $res 1] 1]
		}
	    }
	    1 {
		set DialogWinTop::user($w,type) error
		$DialogWinTop::user($w,textv) conf -fg red
		$DialogWinTop::user($w,textv) insert end [lindex $res 1]
	    }
	}
	$DialogWinTop::user($w,textv) see end
	$DialogWinTop::user($w,textv) conf -state disabled
    }
}

proc RamDebugger::DisplayVarWindowCancel { f } {
    destroy [winfo toplevel $f]
}

proc RamDebugger::GetSelOrWordInIndex { idx } {
    variable text
    
    set range [$text tag ranges sel]
    if { $range != "" && [$text compare [lindex $range 0] <= $idx] && \
	[$text compare [lindex $range 1] >= $idx] } {
	return [eval $text get $range]
    } else {
	if { $idx != "" } {
	    set var ""
	    set idx0 $idx
	    set char [$text get $idx0]
	    while { [string is wordchar $char] || $char == "(" || $char == ")" } {
		set var $char$var
		set idx0 [$text index $idx0-1c]
		if { [$text compare $idx0 <= 1.0] } { break }
		set char [$text get $idx0]
	    }
	    set idx1 [$text index $idx+1c]
	    set char [$text get $idx1]
	    while { [string is wordchar $char] || $char == "(" || $char == ")" } {
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
    return $var
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
    $w._topmenu.system add checkbutton -label "Always on top" -var \
	RamDebugger::options(TransientVarWindow) -command \
	"RamDebugger::ToggleTransientWinAll $mainwindow"
}

proc RamDebugger::InvokeAllDisplayVarWindows {} {
    variable varwindows

    if { ![info exists varwindows] } { return }
    foreach i $varwindows {
	catch { DialogWinTop::InvokeOK $i }
    }
}

proc RamDebugger::DisplayVarWindow { mainwindow { var "" } } {
    variable text
    variable options
    variable varwindows
    
    if { $var eq "" } {
	set var [GetSelOrWordInIndex insert]
    }

    set commands [list "RamDebugger::DisplayVarWindowEval do" RamDebugger::DisplayVarWindowCancel]
    set f [DialogWinTop::Init $text "View expression or variable" separator $commands \
	       "" Eval Close]
    set w [winfo toplevel $f]

    lappend varwindows $f
    bind $f <Destroy> {
	set ipos [lsearch -exact $RamDebugger::varwindows %W]
	set RamDebugger::varwindows [lreplace $RamDebugger::varwindows $ipos $ipos]
    }
    ToggleTransientWin $w $mainwindow

    Label $f.l1 -text "Expression:" -grid "0 w px3"

    $f.l1 configure -helptext {
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
    }

    if { ![info exists options(old_expressions)] } {
	set options(old_expressions) ""
    }

    set DialogWinTop::user($w,combo) [ComboBox $f.e1 -textvariable DialogWinTop::user($w,expression) \
	-values $options(old_expressions) -grid "1 ew px3"]

    set DialogWinTop::user($w,expression) $var
    
    set sw [ScrolledWindow $f.lf -relief sunken -borderwidth 0 -grid "0 2"]
    set DialogWinTop::user($w,textv) [text $sw.text -background white -wrap word -width 40 -height 10 \
		                       -exportselection 0 -font FixedFont -highlightthickness 0]
    $sw setwidget $DialogWinTop::user($w,textv)

    if { $::tcl_platform(platform) != "windows" } {
	$sw.text conf -exportselection 1
    }

    label $f.l2 -textvar DialogWinTop::user($w,type) -grid "0 w" -bg [CCColorActivo [$w  cget -bg]]
    checkbutton $f.cb1 -text "As list" -variable DialogWinTop::user($w,aslist) -grid "1 wwe" \
       -command "DialogWinTop::InvokeOK $f"
    set DialogWinTop::user($w,aslist) 0
    supergrid::go $f

    tkTabToWindow $f.e1
    $DialogWinTop::user($w,combo) bind <Return> "DialogWinTop::InvokeOK $f ; break"
    $DialogWinTop::user($w,textv) conf -state disabled
    bind $DialogWinTop::user($w,textv) <1> "focus $DialogWinTop::user($w,textv)"

    DialogWinTop::CreateWindow $f
    DialogWinTop::InvokeOK $f
}

################################################################################
# DisplayBreakpoints
################################################################################


proc RamDebugger::DisplayBreakpointsWindowSetCond {} {

    set curr [$DialogWinBreak::user(list) curselection]
    if { [llength $curr] != 1 } { return }

    set DialogWinBreak::user(cond) [lindex [$DialogWinBreak::user(list) get $curr] 4]
}

proc RamDebugger::DisplayBreakpointsWindow {} {
    variable text
    variable breakpoints
    variable currentfile
    
    CopyNamespace ::DialogWin ::DialogWinBreak

    set f [DialogWinBreak::Init $text "Breakpoints window" separator [list Delete \
	    "Delete all" View En/Dis] "Apply Cond" Close]
    set w [winfo toplevel $f]

    set help {Examples of conditions:
	$i > $j+1
	[string match *.tcl $file]
    }

    Label $f.l1 -text "Condition:" -helptext $help -grid 0
    entry $f.e1 -textvariable DialogWinBreak::user(cond) -width 80 -grid "1 px3 py3"

    set sw [ScrolledWindow $f.lf -relief sunken -borderwidth 0 -grid "0 2"]
    
    set DialogWinBreak::user(list) [tablelist::tablelist $sw.lb -width 55\
		  -exportselection 0 \
		  -columns [list \
		                4  Num        left \
		                6  En/dis     center \
		                20 File        right \
		                5  Line left \
		                20 Condition left \
		                40 Path left
		               ] \
		  -labelcommand tablelist::sortByColumn \
		  -background white \
		  -selectbackground navy -selectforeground white \
		  -stretch 1 -selectmode extended \
		  -highlightthickness 0]

    $sw setwidget $DialogWinBreak::user(list)

    supergrid::go $f

    focus $DialogWinBreak::user(list)
    bind [$DialogWinBreak::user(list) bodypath] <Double-1> {
	focus $DialogWinBreak::user(list)
	RamDebugger::DisplayBreakpointsWindowSetCond
    }
    bind [$DialogWinBreak::user(list) bodypath] <ButtonPress-3> \
	    [bind TablelistBody <ButtonPress-1>]

    bind [$DialogWinBreak::user(list) bodypath] <ButtonRelease-3> {
	catch { destroy %W.menu }
	set menu [menu %W.menu]
	$menu add command -label "Apply condition" -command "DialogWinBreak::InvokeOK"
	$menu add command -label "View" -command "DialogWinBreak::InvokeButton 4"
	$menu add command -label "Enable/disable" -command "DialogWinBreak::InvokeButton 5"
	$menu add separator
	$menu add command -label "Delete" -command "DialogWinBreak::InvokeButton 2"
	tk_popup $menu %X %Y
    }

    set nowline [scan [$text index insert] %d]
    foreach i $breakpoints {
	foreach "num endis file line cond" $i break
	$DialogWinBreak::user(list) insert end [list $num $endis [file tail $file] $line $cond \
		[file dirname $file]]
	if { [AreFilesEqual $file $currentfile] && $line == $nowline } {
	    $DialogWinBreak::user(list) selection set end
	    $DialogWinBreak::user(list) see end
	}
    }

    set action [DialogWinBreak::CreateWindow]
    while 1 {
	switch $action {
	    0 {
		DialogWinBreak::DestroyWindow
		namespace delete ::DialogWinBreak
		return
	    }
	    1 {
		set curr [$DialogWinBreak::user(list) curselection]
		if { [llength $curr] != 1 } {
		    WarnWin "Select just one breakpoint before applying condition" $w
		} else {
		    set val [$DialogWinBreak::user(list) get $curr]
		    rcond [lindex $val 0] $DialogWinBreak::user(cond)
		    $DialogWinBreak::user(list) delete $curr
		    $DialogWinBreak::user(list) insert $curr [lreplace $val 4 4 \
		            $DialogWinBreak::user(cond)]
		    set file [file join [lindex $val 5 ] [lindex $val 2]]
		    set line [lindex $val 3]
		    if { $file == $currentfile } {
		        $text mark set insert $line.0
		        $text see $line.0
		    }
		}
	    }
	    2 {
		foreach i [$DialogWinBreak::user(list) curselection] {
		    set ent [$DialogWinBreak::user(list) get $i]
		    set num [lindex $ent 0]
		    set file [file join [lindex $ent 5 ] [lindex $ent 2]]
		    set line [lindex $ent 3]
		    if { $file == $currentfile } {
		        UpdateArrowAndBreak $line 0 ""
		    }
		    rdel $num
		}
		$DialogWinBreak::user(list) delete 0 end
		foreach i $breakpoints {
		    foreach "num endis file line cond" $i break
		    $DialogWinBreak::user(list) insert end [list $num $endis [file tail $file] $line $cond \
		            [file dirname $file]]
		}
	    }
	    3 {
		set ret [DialogWinBreak::messageBox -default ok -icon warning -message \
		             "Are you sure to delete all breakpoints?" -parent $f \
		             -title "delete all breakpoints" -type okcancel]
		if { $ret == "ok" } {
		    $DialogWinBreak::user(list) delete 0 end
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
		set curr [$DialogWinBreak::user(list) curselection]
		if { [llength $curr] != 1 } {
		    WarnWin "Select just one breakpoint in order to see the file" $w
		    return
		}
		set val [$DialogWinBreak::user(list) get $curr]
		set file [file join [lindex $val 5] [lindex $val 2]]
		set line [lindex $val 3]
		if { $file != $currentfile } {
		    OpenFileF $file
		}
		$text mark set insert $line.0
		$text see $line.0
	    }
	    5 {
		foreach i [$DialogWinBreak::user(list) curselection] {
		    set val [$DialogWinBreak::user(list) get $i]
		    renabledisable [lindex $val 0]
		    $DialogWinBreak::user(list) delete $i
		    if { [lindex $val 1] } {
		        set enabledisable 0
		    } else { set enabledisable 1 }
		    $DialogWinBreak::user(list) insert $i [lreplace $val 1 1 $enabledisable]
		    $DialogWinBreak::user(list) selection set $i
		    set file [file join [lindex $val 5 ] [lindex $val 2]]
		    set line [lindex $val 3]
		    if { $file == $currentfile } {
		        UpdateArrowAndBreak $line "" "" 0
		        $text mark set insert $line.0
		        $text see $line.0
		    }
		}
	    }
	}
	set action [DialogWinBreak::WaitForWindow]
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

proc RamDebugger::UpdateFont { w but fontname } {

    set newfont [SelectFont $w.fonts -type dialog -parent $w -sampletext \
	"RamDebugger is nice" -title "Select font"]
    if { $newfont == "" } { return }
    set list [list family size weight slant underline overstrike]
    foreach $list $newfont break

    if { $weight == "" } { set weight normal }
    if { $slant == "" } { set slant roman }

    set comm "font configure"
    lappend comm $fontname
    foreach i [lrange $list 0 3] {
	lappend comm -$i [set $i]
    }
    if { [info exists underline] && $underline != "" } {
	lappend comm -underline 1
    } else { lappend comm -underline 0 }

    if { [info exists overstrike] && $overstrike != "" } {
	lappend comm -overstrike 1
    } else { lappend comm -overstrike 0 }

    eval $comm

    regexp {^[^:]+} [$but cget -text] type
    set btext "$type: [font conf $fontname -family] [font conf $fontname -size] "
    append btext "[font conf $fontname -weight] [font conf $fontname -slant]"
    $but configure -text $btext
}

################################################################################
# Preferences
################################################################################


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

proc RamDebugger::PreferencesWindow {} {
    variable text
    variable options
    variable options_def
    
    set fb [DialogWin::Init $text "Preferences window" separator [list Apply Defaults]]
    set w [winfo toplevel $fb]

    set notebook [NoteBook $fb.nb -homogeneous 1 -bd 1 -internalborderwidth 3  \
	-grid "0 px3 py3"]
    
    set f [$fb.nb insert end basic -text "Basic"]

    TitleFrame $f.f1 -text [_ debugging] -grid "0 nsew"
    set f1 [$f.f1 getframe]

    if { $::tcl_platform(platform) == "windows" } {
	checkbutton $f1.cb0 -text "Automatically check remote files" -variable \
	DialogWin::user(CheckRemotes) -grid "0 3 w"
	DynamicHelp::register $f1.cb0 balloon [string trim {
	    this variable is only used on windows. It can be:
	    0: Only check remote programs on demand (useful if not making remote debugging, the
	       start up is faster)
	    1: Register as remote and check remote programs on start up. The start up can be slower
		but is better when making remote debugging
	}]
	set DialogWin::user(CheckRemotes) $options(CheckRemotes)
    }

    checkbutton $f1.cb1 -text "Confirm restart debugging" -variable \
       DialogWin::user(ConfirmStartDebugging) -grid "0 3 w"
    DynamicHelp::register $f1.cb1 balloon "\
	If this option is set, a confirmation window will be displayed\n\
	when restarting the execution of the debugger"
    
    set DialogWin::user(ConfirmStartDebugging) $options(ConfirmStartDebugging)

    checkbutton $f1.cb2 -text "Confirm modify variable" -variable \
       DialogWin::user(ConfirmModifyVariable) -grid "0 3 w"
    DynamicHelp::register $f1.cb2 balloon "\
	If this option is set, a confirmation window will be displayed\n\
	before changing the value of a variable in the debugged program\n\
	by pressing return in the 'User defined variables' or in the\n\
	'Local variables'"
    
    set DialogWin::user(ConfirmModifyVariable) $options(ConfirmModifyVariable)

    label $f1.isf -text "Instrument sourced files" -grid "0 e"
    tk_optionMenu $f1.cb3 DialogWin::user(instrument_source) auto autoprint always never ask
    $f1.cb3 conf -width 10
    supergrid::gridinfo $f1.cb3 "1 w"
    DynamicHelp::register $f1.isf balloon "\
	This variable controls what to do when the debugged program tries\n\
	to source a file. Depending on the option chosen, the source file\n\
	will be instrumented or not.\n\
	If 'auto' is chosen, only the already instrumented files will be sent\n\
	instrumented"
    
    set DialogWin::user(instrument_source) $options(instrument_source)
    if { [string match *ask* $DialogWin::user(instrument_source)] } {
	set DialogWin::user(instrument_source) ask
    }

    checkbutton $f1.cb23 -text "Instrument proc last line" -variable \
       DialogWin::user(instrument_proc_last_line) -grid "0 3 w"
    DynamicHelp::register $f1.cb23 balloon "\
	If this option is set, it is possible to put stops in the last line\n\
	of one proc. It can make the debugged program fail if the proc wants\n\
	to return a value without using command return. A typical example of\n\
	failure are the procs that finish using 'set a 23' instead of 'return 23'"

    set DialogWin::user(instrument_proc_last_line) $options(instrument_proc_last_line)

    set helptext "When debugging locally, choose here if the debugged script is only TCL "
    append helptext "or also TK"
    Label $f1.l15 -text "Local debugging type:" -helptext $helptext -grid "0 e"
    frame $f1.rads -grid "1 2 w"
    radiobutton $f1.rads.r1 -text "TCL" -variable DialogWin::user(LocalDebuggingType) -value tcl \
	    -grid "0 w"
    radiobutton $f1.rads.r2 -text "TK" -variable DialogWin::user(LocalDebuggingType) -value tk \
	    -grid "1 w"

    set DialogWin::user(LocalDebuggingType) $options(LocalDebuggingType)

    Label $f1.l2 -text "Indent size TCL:" -helptext "Size used when indenting TCL with key: <Tab>" \
       -grid "0 e"
    SpinBox $f1.sb -range "0 10 1" -textvariable DialogWin::user(indentsizeTCL) \
       -width 4 -grid "1 2 px3"
    set DialogWin::user(indentsizeTCL) $options(indentsizeTCL)

    Label $f1.l3 -text "Indent size c++:" -helptext "Size used when indenting c++ with key: <Tab>" \
       -grid "0 e"
    SpinBox $f1.sb2 -range "0 10 1" -textvariable DialogWin::user(indentsizeC++) \
       -width 4 -grid "1 2 px3"
    set DialogWin::user(indentsizeC++) $options(indentsizeC++)

    TitleFrame $f.f2 -text [_ fonts] -grid "0 nsew"
    set f2 [$f.f2 getframe]
    
    foreach "but type fontname" [list $f2.b1 {GUI font} NormalFont $f2.b2 {Text font} FixedFont \
	$f2.b3 {Help font} HelpFont] {
	set btext "$type: [font conf $fontname -family] [font conf $fontname -size] "
	append btext "[font conf $fontname -weight] [font conf $fontname -slant]"
	Button $but -text $btext -font $fontname -relief link -command \
	    "RamDebugger::UpdateFont $w $but $fontname" -grid "0 w"
    }

    set fde [$fb.nb insert end extensions -text "Extensions"]

    TitleFrame $fde.f1 -text "extensions" -grid "0 nsew"
    set fde1 [$fde.f1 getframe]

    label $fde1.ll -text "Choose extensions for every file type:" -grid "0 2 w"
    set ic 0
    foreach "type extsdefaultlist" [list TCL [list ".tcl" ".tcl .tk *"] \
		                            C/C++ [list ".c .cpp .cc .h"] \
		                            "GiD BAS file" .bas \
		                            "GiD data files" [list ".prb .mat .cnd"]] {
	label $fde1.l$ic -text $type: -grid "0 e"
	if { ![info exists options(extensions,$type)] } {
	    set DialogWin::user(extensions,$type) $options_def(extensions,$type)
	} else {
	    set DialogWin::user(extensions,$type) $options(extensions,$type)
	}
	ComboBox $fde1.cb$ic -textvariable DialogWin::user(extensions,$type) -values \
	    $extsdefaultlist -grid 1
	incr ic
    }
    supergrid::go $fde
    supergrid::go $fde1

    set fd [$fb.nb insert end directories -text "Directories"]

    TitleFrame $fd.f1 -text "executable directories" -grid "0 nsew"
    set fd1 [$fd.f1 getframe]

    set sw [ScrolledWindow $fd1.lf -relief sunken -borderwidth 0]
    listbox $sw.lb -listvariable DialogWin::user(executable_dirs) -selectmode extended
    $sw setwidget $sw.lb

    set bbox [ButtonBox $fd1.bbox1 -spacing 0 -padx 1 -pady 1 -homogeneous 1]
    $bbox add -image folderopen16 \
	 -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1 -padx 1 -pady 1 \
	 -helptext [_ "Add include directory"] \
	 -command "RamDebugger::PreferencesAddDelDirectories $sw.lb add"
    $bbox add -image actcross16 \
	 -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1 -padx 1 -pady 1 \
	 -helptext [_ "Delete include directory"] \
	 -command "RamDebugger::PreferencesAddDelDirectories $sw.lb delete"
    $bbox add -image playeject16 \
	 -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1 -padx 1 -pady 1 \
	 -helptext [_ "Increase directory priority"] \
	 -command "RamDebugger::PreferencesAddDelDirectories $sw.lb up"

    grid $fd1.lf -sticky nsew
    grid $fd1.bbox1 -sticky nw
    grid columnconfigure $fd1 0 -weight 1
    grid rowconfigure $fd1 0 -weight 1

    set DialogWin::user(executable_dirs) $options(executable_dirs)

    set tt "Include here all directories where RamDebugger should find executables\n"
    append tt "This is primary useful in Windows to describe where mingw is installed"
    DynamicHelp::register $sw.lb balloon $tt

    supergrid::go $f1
    supergrid::go $f2
    supergrid::go $f
    supergrid::go $fd


    set fas [$fb.nb insert end autosave -text "Auto save"]

    set lb [labelframe $fas.l1 -text "auto save revisions"]

    checkbutton $lb.c1 -text "Perform auto save revisions" -variable \
	DialogWin::user(AutoSaveRevisions)
    label $lb.l1 -text "Auto save time"
    spinbox $lb.cb1 -textvariable DialogWin::user(AutoSaveRevisions_time) \
	-from 0 -to 10000 -increment 1
    label $lb.l2 -text "seconds"
    DynamicHelp::register $lb.l1 balloon "Time in seconds before performing an auto-save"

    label $lb.l3 -text "Auto save idle time"
    spinbox $lb.cb2 -textvariable DialogWin::user(AutoSaveRevisions_idletime) \
	-from 0 -to 10000 -increment 1
    label $lb.l4 -text "seconds"
    set tt "Time in seconds without user activity before performing an auto-save"
    DynamicHelp::register $lb.l3 balloon $tt

    set cmd "switch \$DialogWin::user(AutoSaveRevisions) 1 { $lb.cb1 configure -state normal ;"
    append cmd "$lb.cb2 configure -state normal } 0 { $lb.cb1 configure -state disabled ;"
    append cmd "$lb.cb2 configure -state disabled }"

    trace add variable DialogWin::user(AutoSaveRevisions) write "$cmd ;#"
    bind $lb.cb1 <Destroy> [list trace remove variable DialogWin::user(AutoSaveRevisions) \
		                write "$cmd ;#"]

    if { [info exists options(AutoSaveRevisions)] } {
	set DialogWin::user(AutoSaveRevisions_time) $options(AutoSaveRevisions_time)
	set DialogWin::user(AutoSaveRevisions_idletime) $options(AutoSaveRevisions_idletime)
	set DialogWin::user(AutoSaveRevisions) $options(AutoSaveRevisions)
    }

    grid $lb.c1 - - -sticky nw
    grid $lb.l1 $lb.cb1 $lb.l2 -sticky nw
    grid $lb.l3 $lb.cb2 $lb.l4 -sticky nw

    grid configure $lb.cb1 $lb.cb2 -sticky new
    grid configure $lb.l1 $lb.l3 -padx "10 0"
    grid columnconfigure $lb 1 -weight 1
    grid rowconfigure $lb 2 -weight 1

    grid $lb -sticky nsew
    grid columnconfigure $fas 0 -weight 1
    grid rowconfigure $fas 0 -weight 1


    $fb.nb compute_size
    $fb.nb raise basic

    supergrid::go $fb

    focus $f1.cb1

    set action [DialogWin::CreateWindow "" "" 200]
    while 1 {
	switch $action {
	    0 {
		DialogWin::DestroyWindow
		return
	    }
	    1 - 2 {
		set good 1
		if { ![string is integer -strict $DialogWin::user(indentsizeTCL)] || \
		    $DialogWin::user(indentsizeTCL) < 0 || $DialogWin::user(indentsizeTCL) > 10 } {
		    WarnWin "Error: indent size must be between 0 and 10" $w
		    set good 0
		} elseif { ![string is integer -strict $DialogWin::user(indentsizeC++)] || \
		    $DialogWin::user(indentsizeC++) < 0 || $DialogWin::user(indentsizeC++) > 10 } {
		    WarnWin "Error: indent size must be between 0 and 10" $w
		    set good 0
		}
		if { $good && $DialogWin::user(AutoSaveRevisions) } {
		    if { ![string is double -strict $DialogWin::user(AutoSaveRevisions_time)] || \
		             $DialogWin::user(AutoSaveRevisions_time) < 0 } {
		        WarnWin "Error: Auto save revisions time must be a number" $w
		        set good 0
		    } elseif { ![string is double -strict \
		                     $DialogWin::user(AutoSaveRevisions_idletime)] || \
		                   $DialogWin::user(AutoSaveRevisions_idletime) < 0 } {
		        WarnWin "Error: Auto save revisions idle time must be a number" $w
		        set good 0
		    }
		}
		if { $good } {
		    foreach i [list indentsizeTCL indentsizeC++ ConfirmStartDebugging \
		                   ConfirmModifyVariable instrument_source instrument_proc_last_line \
		                   LocalDebuggingType AutoSaveRevisions AutoSaveRevisions_time \
		                   AutoSaveRevisions_idletime] {
		        set options($i) $DialogWin::user($i)
		    }
		    if { [info exists options(CheckRemotes)] } {
		        set options(CheckRemotes) $DialogWin::user(CheckRemotes)
		    }

		    foreach i [array names options_def extensions,*] {
		        set options($i) $DialogWin::user($i)
		    }

		    set options(executable_dirs) $DialogWin::user(executable_dirs)
		    UpdateExecDirs
		    RamDebugger::CVS::ManageAutoSave
		    if { $action == 1 } {
		        DialogWin::DestroyWindow
		        return
		    }
		}
	    }
	    3 {
		foreach i [list indentsizeTCL indentsizeC++ ConfirmStartDebugging \
		               ConfirmModifyVariable instrument_source instrument_proc_last_line \
		               LocalDebuggingType CheckRemotes NormalFont FixedFont HelpFont \
		               executable_dirs AutoSaveRevisions AutoSaveRevisions_time \
		              AutoSaveRevisions_idletime] {
		    if { [info exists options_def($i)] } {
		        set options($i) $options_def($i)
		        set DialogWin::user($i) $options_def($i)
		    }
		}
		CreateModifyFonts

		foreach "but fontname" [list $f2.b1 NormalFont $f2.b2 FixedFont $f2.b3 HelpFont] {
		    regexp {^[^:]+} [$but cget -text] type
		    set btext "$type: [font conf $fontname -family] [font conf $fontname -size] "
		    append btext "[font conf $fontname -weight] [font conf $fontname -slant]"
		    $but configure -text $btext
		}

		foreach i [array names options_def extensions,*] {
		    set options($i) $options_def($i)
		}
		RamDebugger::CVS::ManageAutoSave
	    }
	}
	set action [DialogWin::WaitForWindow]
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
    append tt "ramsan@cimne.upc.es\nhttp://gid.cimne.upc.es/ramsan\n"
    append tt "http://gid.cimne.com/RamDebugger"

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

    set f [DialogWin::Init $active_text "Goto line" separator ""]
    set w [winfo toplevel $f]

    label $f.l -text "Go to line:" -grid "0 px3 py5"
    SpinBox $f.sb -range "1 1000 1" -textvariable DialogWin::user(line) \
	    -width 8 -grid "1 px3"

    checkbutton $f.cb1 -text "Relative to current line" -variable DialogWin::user(relative) \
	    -grid "0 2 w"

    set DialogWin::user(relative) 0

    tkTabToWindow $f.sb

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
		set line $DialogWin::user(line)
		set good 1
		if { ![string is integer -strict $line] } {
		    WarnWin "Line number must be a positive number" $w
		    set good 0
		}
		if { $good && $DialogWin::user(relative) } {
		    set insline [scan [$active_text index insert] %d]
		    set lastline [scan [$active_text index end-1c] %d]
		    set line [expr $insline+$line]
		    if { $line < 1 || $line > $lastline } {
		        WarnWin "Trying to go to line $line. Out of limits" $w
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
		    DialogWin::DestroyWindow
		    return
		}
	    }
	}
	set action [DialogWin::WaitForWindow]
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

    set f [DialogWin::Init $text "TCL Execution arguments" separator [list Clear]]
    set w [winfo toplevel $f]
    
    label $f.l -text "Currentfile to debug:" -grid "0 e px3 py5"

    ComboBox $f.cb1 -textvariable DialogWin::user(curr) -width 60 -grid 1 -values \
	    $currs
    Button $f.b1 -image [Bitmap::get file] -width 16 -grid 2 -relief link

    label $f.lb -text "File to debug as:" -grid "0 e px3 py5"
    ComboBox $f.cb1b -textvariable DialogWin::user(curr_as) -width 40 -grid 1 -values \
	$currs_as -helptext "Select another TCL file to execute when 'Current file' is open"
    Button $f.b1b -image [Bitmap::get file] -width 16 -grid 2 -relief link

    label $f.l2 -text "Directory:" -grid "0 e px3 py5"
    ComboBox $f.cb2 -textvariable DialogWin::user(directory) -width 40 -grid "1" -values \
	    $dirs
    Button $f.b2 -image [Bitmap::get folder] -grid 2 -relief link

    label $f.l3 -text "Arguments:" -grid "0 e"
    ComboBox $f.cb3 -textvariable DialogWin::user(arguments) -width 40 -grid "1 2" -values \
	    $args

    label $f.l4 -text "File type:" -grid "0 e"
    frame $f.f1 -grid "1 2 w"
    radiobutton $f.f1.r1 -text "Auto" -variable DialogWin::user(tcl_or_tk) -value auto -grid 0
    radiobutton $f.f1.r2 -text "Tcl" -variable DialogWin::user(tcl_or_tk) -value tcl -grid 1
    radiobutton $f.f1.r3 -text "Tk" -variable DialogWin::user(tcl_or_tk) -value tk -grid 2


    set DialogWin::user(curr) $currentfile
    set DialogWin::user(curr_as) $curr_as
    set DialogWin::user(directory) $dir
    set DialogWin::user(arguments) $arg
    set DialogWin::user(tcl_or_tk) $tcl_or_tk

    set comm {
	set initial $RamDebugger::options(defaultdir)
	catch { set initial [file dirname $DialogWin::user(curr)] }
	set curr [tk_getOpenFile -filetypes {{{TCL Scripts} {.tcl} } {{All Files} *}} \
		     -initialdir $initial -parent PARENT -title "Debug TCL file"]
	if { $curr != "" } { set DialogWin::user(curr) $curr }
    }
    set comm [string map [list PARENT $w] $comm]
    $f.b1 configure -command $comm

    set comm {
	set initial $RamDebugger::options(defaultdir)
	catch { set initial [file dirname $DialogWin::user(curr_as)] }
	set curr_as [tk_getOpenFile -filetypes {{{TCL Scripts} {.tcl} } {{All Files} *}} \
		         -initialdir $initial -parent PARENT -title "Debug as TCL file"]
	if { $curr_as != "" } { set DialogWin::user(curr_as) $curr_as }
    }
    set comm [string map [list PARENT $w] $comm]
    $f.b1b configure -command $comm

    set comm {
	set initial $RamDebugger::options(defaultdir)
	catch { set initial [file dirname $DialogWin::user(curr)] }
	set DialogWin::user(directory) [RamDebugger::filenormalize [tk_chooseDirectory \
	    -initialdir $initial -parent PARENT \
	    -title "Debug directory" -mustexist 1]]
    }
    set comm [string map [list PARENT $w] $comm]
    $f.b2 configure -command $comm

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
	    1 - 2 {
		if { $action == 2 } {
		    set DialogWin::user(curr_as) ""
		    set DialogWin::user(directory) ""
		    set DialogWin::user(arguments) ""
		    set DialogWin::user(tcl_or_tk) auto
		}
		set ipos 0
		foreach "curr curr_as dir args tcl_or_tk" $options(currentfileargs5) {
		    if { $curr == $DialogWin::user(curr) } {
		        set options(currentfileargs5) [lreplace $options(currentfileargs5) $ipos \
		            [expr $ipos+4]]
		        break
		    }
		    incr ipos 5
		}
		if { $DialogWin::user(directory) != "" || $DialogWin::user(arguments) != "" || \
		     $DialogWin::user(curr_as) != "" || $DialogWin::user(tcl_or_tk) != "auto" } {
		    lappend options(currentfileargs5) $DialogWin::user(curr) \
		        $DialogWin::user(curr_as) $DialogWin::user(directory) \
		        $DialogWin::user(arguments) $DialogWin::user(tcl_or_tk)
		}
		DialogWin::DestroyWindow
		return
	    }
	}
	set action [DialogWin::WaitForWindow]
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

    if { $::tcl_platform(platform) == "windows" } {
	    set cat [file join $MainDir addons cat.exe]
    } else { set cat cat }

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

proc RamDebugger::SearchInFilesDo {} {
    variable options
    variable MainDir
    
     if { $::tcl_platform(platform) == "windows" } {
	 set grep [file join $MainDir addons grep.exe]
     } else { set grep grep }

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

    set result ""
    set ifile 0
    while { $ifile < [llength $files] } {
	set comm2 $comm
	eval lappend comm2 [lrange $files $ifile [expr {$ifile+50}]]
	incr ifile 50
	set fin [open "|$comm2" r]
	while { ![eof $fin] } {
	    append result [gets $fin]\n
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

proc RamDebugger::SearchInFiles {} {
    variable options
    variable text

    set txt [GetSelOrWordInIndex insert]
    
    set f [DialogWin::Init $text "Search in files" separator]
    set w [winfo toplevel $f]

    if { ![info exists options(SearchInFiles,texts)] } {
	set options(SearchInFiles,texts) ""
	set options(SearchInFiles,exts) ""
	set options(SearchInFiles,dirs) ""
    }

    label $f.l1 -text "Text:" -grid 0
    ComboBox $f.e1 -textvariable ::RamDebugger::searchstring -grid "1 2 px3 py3" \
	    -values $options(SearchInFiles,texts) -width 40

    if { [string trim $txt] != "" } {
	set ::RamDebugger::searchstring $txt
    } else { set ::RamDebugger::searchstring [lindex $options(SearchInFiles,texts) 0] }

    label $f.l2 -text "File ext:" -grid 0

    set values $options(SearchInFiles,exts)
    foreach i [list "*.tcl" "*.h,*.cc,*.c"] {
	if { [lsearch -exact $values $i] == -1 } { lappend values $i }
    }
    foreach i [array names options(extensions,*)] {
	if { [lsearch -exact $values $options($i)] == -1 } { lappend values $options($i) }
    }

    ComboBox $f.e2 -textvariable ::RamDebugger::searchextensions -grid "1 2 px3 py3" \
	    -values $values]

    set ::RamDebugger::searchextensions [lindex [$f.e2 cget -values] 0]

    label $f.l3 -text "Directory:" -grid 0
    ComboBox $f.e3 -textvariable ::RamDebugger::searchdir -grid "1 1 px3 py3" \
	    -values $options(SearchInFiles,dirs) -width 70
    Button $f.b3 -image [Bitmap::get folder] -relief link -grid "2" 


    set comm {
	set initial $::RamDebugger::searchdir
	set ::RamDebugger::searchdir [RamDebugger::filenormalize [tk_chooseDirectory   \
	    -initialdir $initial -parent PARENT \
	    -title "Search directory" -mustexist 1]]
    }
    set comm [string map [list PARENT $w] $comm]
    $f.b3 configure -command $comm

    if { ![info exists ::RamDebugger::searchdir] || $::RamDebugger::searchdir == "" } {
	set ::RamDebugger::searchdir $options(defaultdir)
    }

    set f1 [frame $f.f1 -bd 0 -grid "0 3 w"]
    set f2 [frame $f1.f2 -bd 1 -relief ridge -grid "0 w px3"]
    radiobutton $f2.r1 -text Exact -variable ::RamDebugger::searchmode \
	-value -exact -grid "0 w"
    radiobutton $f2.r2 -text Regexp -variable ::RamDebugger::searchmode \
	-value -regexp -grid "0 w"

    set f3 [frame $f1.f3 -grid "1 w"]
    checkbutton $f3.cb1 -text "Consider case" -variable ::RamDebugger::searchcase \
	-grid 0
    checkbutton $f3.cb2 -text "Recurse dirs" -variable ::RamDebugger::searchrecursedirs \
	-grid 0
   
    supergrid::go $f


    set ::RamDebugger::searchmode -exact
    set ::RamDebugger::searchcase 0
    set ::RamDebugger::searchrecursedirs 1

    tkTabToWindow $f.e1
    bind $w <Return> "DialogWin::InvokeOK"
    
    set action [DialogWin::CreateWindow]
    switch $action {
	0 {
	    DialogWin::DestroyWindow
	    return
	}
	1 {
	    if { $::RamDebugger::searchstring == "" } {
		DialogWin::DestroyWindow
		return
	    }
	    DialogWin::DestroyWindow
	    update
	    SearchInFilesDo
	    return
	}
    }
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

    set istoplevel 0

    if { [info exists SearchToolbar] } {
	set f [$mainframe gettoolbar 1]
	set ::RamDebugger::searchstring [GetSelOrWordInIndex insert]
	if { $replace != [lindex $SearchToolbar 1] } {
	    #nothing
	} elseif { [lindex $SearchToolbar 0] } {
	    $mainframe showtoolbar 1 0
	    focus $text
	    set SearchToolbar [list 0 $replace]
	    return
	} else {
	    $mainframe showtoolbar 1 1
	    tkTabToWindow $f.e1
	    set SearchToolbar [list 1 $replace]
	    return
	}
    }
    if { ![info exists options(SearchToolbar_autoclose)] } {
	set options(SearchToolbar_autoclose) 1
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
	    set f [$mainframe addtoolbar]
	}
    }
    set ::RamDebugger::SearchToolbar [list 1 $replace]

    label $f.l1 -text "Search:"
    ComboBox $f.e1 -textvariable ::RamDebugger::searchstring -values $options(old_searchs)

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

    radiobutton $f.r1 -image navup16 -variable ::RamDebugger::SearchType \
	-value -backwards -indicatoron 0 -bd 1
    radiobutton $f.r2 -image navdown16 -variable ::RamDebugger::SearchType \
	-value -forwards -indicatoron 0 -bd 1
    Separator $f.sp1 -orient vertical
    checkbutton $f.cb1 -image navhome16 -variable ::RamDebugger::searchFromBegin \
	-indicatoron 0 -bd 1
    checkbutton $f.cb2 -image uppercase_lowercase -variable ::RamDebugger::searchcase\
	 -indicatoron 0 -bd 1
    checkbutton $f.cb3 -text Regexp -variable ::RamDebugger::searchmode \
	-onvalue -regexp -offvalue -exact

    set helps [list \
	    $f.r1 "Search backwards (PgUp)" \
	    $f.r2 "Search forwards (PgDn)" \
	    $f.cb1 "From beginning (Home)" \
	    $f.cb2 "Consider case" \
	    $f.cb3 "Rexexp mode"]
    foreach "widget help" $helps {
	DynamicHelp::register $widget balloon $help
    }

    if { $replace } {
	label $f.l11 -text "Replace:"
	ComboBox $f.e11 -textvariable ::RamDebugger::replacestring \
	    -values $options(old_replaces)
	raise $f.e11 $f2.r1
	frame $f.buts
	
	set ic 0
	foreach "txt cmd help" [list "Skip" beginreplace "Search next (Shift-Return)" \
		"Replace" replace "Replace (Return)" \
		"Replace all" replaceall "Replace all"] {
	    button $f.buts.b[incr ic] -text $txt -padx 0 -pady 0 -width 9 \
		-relief flat -overrelief raised -bd 1 -command \
		[list RamDebugger::SearchReplace $w $cmd]
	    DynamicHelp::register $f.buts.b$ic balloon $help
	}
	grid $f.buts.b1 $f.buts.b2 $f.buts.b3 -sticky nw -padx 2
    }

    if { !$istoplevel } {
	grid $f.l1 $f.e1 $f.r1 $f.r2 $f.sp1 $f.cb1 $f.cb2 $f.cb3 -sticky nw -padx 2
	if { [winfo exists $f.l11] } {
	    grid $f.l11 $f.e11 $f.buts - - - - - -sticky nw -padx 2 -pady 1
	    grid configure $f.e11 -sticky new
	}
	grid $f.e1 -sticky new
	grid $f.sp1 -sticky nsw
	grid columnconfigure $f 1 -weight 1
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
    $f.e1 bind <3> [list RamDebugger::SearchReplace $w contextual $replace %X %Y]

    $f.e1 bind <Home> "[list set ::RamDebugger::searchFromBegin 1] ; break"
    $f.e1 bind <End> "[list set ::RamDebugger::searchFromBegin 0] ; break"
    $f.e1 bind <Prior> "[list set ::RamDebugger::SearchType -backwards] ; break"
    $f.e1 bind <Next> "[list set ::RamDebugger::SearchType -forwards] ; break"

    tkTabToWindow $f.e1
    if { !$replace } {
	$f.e1 bind <Return> "RamDebugger::Search $w begin 0 $f"
	$f.e1 bind <Escape> [list RamDebugger::SearchWindow_autoclose force]
    } else {
	$f.e1 bind <Return> "RamDebugger::SearchReplace $w replace"
	$f.e1 bind <Shift-Return> "RamDebugger::SearchReplace $w beginreplace ; break"
	$f.e11 bind <Return> "RamDebugger::SearchReplace $w replace ; break"
	$f.e11 bind <Shift-Return> "RamDebugger::SearchReplace $w beginreplace ; break"
	bind $f.l11 <3> [list RamDebugger::SearchReplace $w contextual $replace %X %Y]
	$f.e11 bind <3> [list RamDebugger::SearchReplace $w contextual $replace %X %Y]
	$f.e11 bind <Home> "[list set ::RamDebugger::searchFromBegin 1] ; break"
	$f.e11 bind <End> "[list set ::RamDebugger::searchFromBegin 0] ; break"
	$f.e11 bind <Prior> "[list set ::RamDebugger::SearchType -backwards] ; break"
	$f.e11 bind <Next> "[list set ::RamDebugger::SearchType -forwards] ; break"
	$f.e1 bind <Escape> [list RamDebugger::SearchWindow_autoclose force]
	$f.e11 bind <Escape> [list RamDebugger::SearchWindow_autoclose force]
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
		::RamDebugger::options(SearchToolbar_autoclose)
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
	    entry $w.search -width 25 -textvariable RamDebugger::searchstring
	    place $w.search -in $w -x 0 -rely 1 -y -1 -anchor sw

	    focus $active_text
	    bindtags $active_text [linsert [bindtags $active_text] 0 $w.search]
	    bind $w.search <FocusOut> "destroy $w.search ; break"
	    bind $w.search <Escape> "destroy $w.search ; break"
	    bind $w.search <KeyPress> [list if { ![string equal "%A" ""] && ([string is print %A] || \
		[string is space %A]) } \
		"$w.search icursor end; tkEntryInsert $w.search %A ; break" else \
		"destroy $w.search ; break"]
	    bind $w.search <Delete> "$w.search icursor end; $w.search delete insert ; break"
	    bind $w.search <BackSpace> "$w.search icursor end; tkEntryBackspace $w.search ; break"
	    bind $w.search <1> "destroy $w.search"
	    bind $w.search <3> "destroy $w.search"
	    foreach i [list F2 F5 F9 F10 F11] {
		bind $w.search <$i> "destroy $w.search"
	    }
	    bind $w.search <Return> "destroy $w.search ; break"
	    bind $w.search <Control-i> "RamDebugger::Search $w iforward ; break"
	    bind $w.search <Control-r> "RamDebugger::Search $w ibackward ; break"
	    bind $w.search <Control-g> "RamDebugger::Search $w stop ; break"

	    set ::RamDebugger::searchstring ""
	    trace var RamDebugger::searchstring w "[list RamDebugger::Search $w {}];#"
	    bind $w.search <Destroy> [list trace vdelete RamDebugger::searchstring w \
		"[list RamDebugger::Search $w {}];#"]
	    bind $w.search <Destroy> "+ [list bindtags $active_text [lreplace [bindtags $active_text] 0 0]] ; break"
	    foreach i [bind Text] {
		if { [bind $w.search $i] == "" } {
		    if { [string match *nothing* [bind Text $i]] } {
		        bind $w.search $i [bind Text $i]
		    } else {
		        bind $w.search $i "destroy $w.search" }
		}
	    }
	    set idx [$active_text index insert]
	    if { $idx == "" } { set idx 1.0 }
	    set ::RamDebugger::SearchIni $idx
	    set ::RamDebugger::SearchPos $idx
	    set ::RamDebugger::searchcase -1
	    set ::RamDebugger::searchmode -exact
	    if { [info exists ::RamDebugger::Lastsearchstring] } {
		set ::RamDebugger::lastwascreation $::RamDebugger::Lastsearchstring
	    } else { set ::RamDebugger::lastwascreation "" }
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
	iforward {
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
	    if { $raiseerror } {
		error "Search not found"
	    }
	    SetMessage "Search not found"
	    bell
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
	}
	set RamDebugger::Lastsearchstring $RamDebugger::searchstring

    }
}

################################################################################
# OpenProgram OpenConsole
################################################################################

proc RamDebugger::OpenProgram { what } {
    variable MainDir

    switch $what {
	visualregexp { set file [file join $MainDir addons visualregexp visual_regexp.tcl] }
	tkcvs { set file [file join $MainDir addons tkcvs bin tkcvs.tcl] }
	tkdiff { set file [file join $MainDir addons tkdiff.tcl] }
    }
#tkdiff { set file [file join $MainDir addons tkcvs bin tkdiff.tcl] }
    if { [interp exists $what] } { interp delete $what }
    interp create $what
    interp alias $what exit_interp "" interp delete $what
    $what eval [list proc exit { args } "destroy . ; exit_interp"]
    $what eval [list load {} Tk]
    $what eval { set argc 0 ; set argv "" }
    $what eval [list source $file]
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
	foreach "k v i" [$textOUT dump -tag -text 1.0 end-1c] {
	    switch $k {
		tagon { if { $v eq "red" } { set channel stderr } }
		tagoff { if { $v eq "red" } { set channel stdout } }
		text {
		    append tkconprefs [list puts -nonewline $channel $v]\n
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
	$w set_uservar_value opt
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
	    if { [llength $options(saved_positions_stack)] > 14 } {
		set options(saved_positions_stack) [lrange \
		        $options(saved_positions_stack) 0 13]
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
	    set ipos 0
	    set found 0
	    foreach i $options(saved_positions_stack) {
		if { $file_new eq [lindex $i 0] && $line == [lindex $i 1] } {
		    set found 1
		    break
		}
		incr ipos
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
	    set ipos 0
	    set found 0
	    foreach i $options(saved_positions_stack) {
		if { $file eq [lindex $i 0] && $line == [lindex $i 1] } {
		    set found 1
		    break
		}
		incr ipos
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
	    set ipos 0
	    set found 0
	    foreach i $options(saved_positions_stack) {
		if { $file eq [lindex $i 0] && $line == [lindex $i 1] } {
		    set found 1
		    break
		}
		incr ipos
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
		bind all $Macros::macrodata($i,accelerator) [list $menu invoke [$menu index end]]
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
    entry $f.e2 -textvar DialogWin::user(patterns) -width 30 -grid "0 2 px3"

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
    text $sw.text -background white -wrap word -width 80 -height 40 \
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





















