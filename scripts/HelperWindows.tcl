

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
		array get {VAR}
	    } elseif { [info exists {VAR}] } {
		set {VAR}
	    } else {
		expr {VAR}
	    }
	}
	set comm [string map [list VAR [string trim $var]] $comm]
	#set comm "if { \[array exists $var] } { array get $var } else { set $var }"
    }
    # catch is here for strange situations, like when changing source file
    catch {
	set res [reval -handler [list RamDebugger::DisplayVar2 $var $X $Y $x $y] $comm]
    }
}

proc RamDebugger::DisplayVar2 { var X Y x y res } {
    variable text

    if { [lindex $res 0] == 0 } {
	set w $text.help
	if { [winfo exists $w] } { destroy $w }
	toplevel $w
	wm overrideredirect $w 1
	wm transient $w $text
	wm geom $w +$X+$Y
	pack [label $w.l -bg white]
	$w.l conf -bd 1 -relief solid
	set val [lindex $res 1]
	if { [string length $val] > 50 } {
	    set val [string range $val 0 46]...
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

	set ipos [lsearch $options(old_expressions) $var]
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
	$DialogWinTop::user($w,textv) del 1.0 end

	if { $remoteserverType == "gdb" } {
	    if { [regexp {^(\s*MULTIPLE RESULT\s*type\s+=\s+char\s)(.*)} $res {} ini rest] } {
		set res $ini
		append res "   \""
		foreach "i c" [regexp -all -inline {'(.[^\)]*)'\n} $rest] {
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
	    while { [string is wordchar [$text get $idx0]] } {
		set var [$text get $idx0]$var
		set idx0 [$text index $idx0-1c]
		if { [$text compare $idx0 <= 1.0] } { break }
	    }
	    set idx1 [$text index $idx+1c]
	    while { [string is wordchar [$text get $idx1]] } {
		append var [$text get $idx1]
		set idx1 [$text index $idx1+1c]
		if { [$text compare $idx1 >= end-1c] } { break }
	    }
	} else { set var "" }
    }
    return $var
}

proc RamDebugger::DisplayVarWindow {} {
    variable text
    variable options
    
    set var [GetSelOrWordInIndex insert]

    set commands [list "RamDebugger::DisplayVarWindowEval do" RamDebugger::DisplayVarWindowCancel]
    set f [DialogWinTop::Init $text "View expression or variable" separator $commands \
	       "" Eval Close]
    set w [winfo toplevel $f]

    Label $f.l1 -text "Expression:" -grid "0 px3"

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
	-values $options(old_expressions) -grid "1 px3"]

    set DialogWinTop::user($w,expression) $var
    
    set sw [ScrolledWindow $f.lf -relief sunken -borderwidth 0 -grid "0 2"]
    set DialogWinTop::user($w,textv) [text $sw.text -background white -wrap word -width 40 -height 10 \
		                       -exportselection 0 -font FixedFont -highlightthickness 0]
    $sw setwidget $DialogWinTop::user($w,textv)

    if { $::tcl_platform(platform) != "windows" } {
	$sw.text conf -exportselection 1
    }

    label $f.l2 -textvar DialogWinTop::user($w,type) -grid "0 w" -bg [CCColorActivo [$w  cget -bg]]
    checkbutton $f.cb1 -text "As list" -variable DialogWinTop::user($w,aslist) -grid "1 w" \
       -command "DialogWinTop::InvokeOK $f"
    set DialogWinTop::user($w,aslist) 0
    supergrid::go $f

    tkTabToWindow $f.e1
    bind [winfo toplevel $f] <Return> "DialogWinTop::InvokeOK $f"
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

    set DialogWinBreak::user(cond) [lindex [$DialogWinBreak::user(list) get $curr] 3]
}

proc RamDebugger::DisplayBreakpointsWindow {} {
    variable text
    variable breakpoints
    variable currentfile
    
    CopyNamespace ::DialogWin ::DialogWinBreak

    set f [DialogWinBreak::Init $text "Breakpoints window" separator [list Delete \
	    "Delete all" View] "Apply Cond" Close]
    set w [winfo toplevel $f]

    label $f.l1 -text "Condition:" -grid 0
    entry $f.e1 -textvariable DialogWinBreak::user(cond) -width 80 -grid "1 px3 py3"

    set sw [ScrolledWindow $f.lf -relief sunken -borderwidth 0 -grid "0 2"]
    
    set DialogWinBreak::user(list) [tablelist::tablelist $sw.lb -width 55\
		  -exportselection 0 \
		  -columns [list \
		                4  Num        left \
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
	$menu add separator
	$menu add command -label "Delete" -command "DialogWinBreak::InvokeButton 2"
	tk_popup $menu %X %Y
    }

    foreach i $breakpoints {
	foreach "num file line cond" $i break
	$DialogWinBreak::user(list) insert end [list $num [file tail $file] $line $cond \
		[file dirname $file]]
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
		    $DialogWinBreak::user(list) insert $curr [lreplace $val 3 3 \
		            $DialogWinBreak::user(cond)]
		    set file [file join [lindex $val 4 ] [lindex $val 1]]
		    set line [lindex $val 2]
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
		    set file [file join [lindex $ent 4 ] [lindex $ent 1]]
		    set line [lindex $ent 2]
		    if { $file == $currentfile } {
		        UpdateArrowAndBreak $line 0 ""
		    }
		    rdel $num
		}
		$DialogWinBreak::user(list) del 0 end
		foreach i $breakpoints {
		    foreach "num file line cond" $i break
		    $DialogWinBreak::user(list) insert end [list $num [file tail $file] $line $cond \
		            [file dirname $file]]
		}
	    }
	    3 {
		set ret [DialogWinBreak::messageBox -default ok -icon warning -message \
		             "Are you sure to delete all breakpoints?" -parent $f \
		             -title "delete all breakpoints" -type okcancel]
		if { $ret == "ok" } {
		    $DialogWinBreak::user(list) del 0 end
		    foreach i $breakpoints {
		        set num [lindex $i 0]
		        set file [lindex $i 1]
		        set line [lindex $i 2]
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
		set file [file join [lindex $val 4 ] [lindex $val 1]]
		set line [lindex $val 2]
		if { $file != $currentfile } {
		    OpenFileF $file
		}
		$text mark set insert $line.0
		$text see $line.0
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

    TitleFrame $f.f1 -text [_ debugging] -grid 0
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

    checkbutton $f1.cb1 -text "Confirm start debugging" -variable \
       DialogWin::user(ConfirmStartDebugging) -grid "0 3 w"
    DynamicHelp::register $f1.cb1 balloon "\
	If this option is set, a confirmation window will be displayed\n\
	when starting the execution of the debugger"
    
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
    tk_optionMenu $f1.cb3 DialogWin::user(instrument_source) auto always never ask
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

    set helptext "When debugging locally, choose here if the debugged script is only TCL "
    append helptext "or also TK"
    Label $f1.l15 -text "Local debugging type:" -helptext $helptext -grid "0 e"
    radiobutton $f1.r1 -text "TCL" -variable DialogWin::user(LocalDebuggingType) -value tcl \
	    -grid "1 w"
    radiobutton $f1.r2 -text "TK" -variable DialogWin::user(LocalDebuggingType) -value tk \
	    -grid "2 w"

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

    TitleFrame $f.f2 -text [_ fonts] -grid 0
    set f2 [$f.f2 getframe]
    
    foreach "but type fontname" [list $f2.b1 {GUI font} NormalFont $f2.b2 {Text font} FixedFont \
	$f2.b3 {Help font} HelpFont] {
	set btext "$type: [font conf $fontname -family] [font conf $fontname -size] "
	append btext "[font conf $fontname -weight] [font conf $fontname -slant]"
	Button $but -text $btext -font $fontname -relief link -command \
	    "RamDebugger::UpdateFont $w $but $fontname" -grid "0 w"
    }

    set fd [$fb.nb insert end directories -text "Directories"]

    TitleFrame $fd.f1 -text "executable directories" -grid 0
    set fd1 [$fd.f1 getframe]

    set sw [ScrolledWindow $fd1.lf -relief sunken -borderwidth 0 -grid "0"]
    listbox $sw.lb -listvariable DialogWin::user(executable_dirs) -selectmode extended
    $sw setwidget $sw.lb

    set bbox [ButtonBox $fd1.bbox1 -spacing 0 -padx 1 -pady 1 -homogeneous 1 -grid "0 wn"]
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

    set DialogWin::user(executable_dirs) $options(executable_dirs)

    set tt "Include here all directories where RamDebugger should find executables\n"
    append tt "This is primary useful in Windows to describe where mingw is installed"
    DynamicHelp::register $sw.lb balloon $tt

    supergrid::go $f1
    supergrid::go $f2
    supergrid::go $f
    supergrid::go $fd
    supergrid::go $fd1

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
		if { ![string is integer -strict $DialogWin::user(indentsizeTCL)] || \
		    $DialogWin::user(indentsizeTCL) < 0 || $DialogWin::user(indentsizeTCL) > 10 } {
		    WarnWin "Error: indent size must be between 0 and 10" $w
		} elseif { ![string is integer -strict $DialogWin::user(indentsizeC++)] || \
		    $DialogWin::user(indentsizeC++) < 0 || $DialogWin::user(indentsizeC++) > 10 } {
		    WarnWin "Error: indent size must be between 0 and 10" $w
		} else {
		    set options(indentsizeTCL) $DialogWin::user(indentsizeTCL)
		    set options(indentsizeC++) $DialogWin::user(indentsizeC++)
		    set options(ConfirmStartDebugging) $DialogWin::user(ConfirmStartDebugging)
		    set options(ConfirmModifyVariable) $DialogWin::user(ConfirmModifyVariable)
		    set options(instrument_source) $DialogWin::user(instrument_source)
		    set options(LocalDebuggingType) $DialogWin::user(LocalDebuggingType)
		    if { [info exists options(CheckRemotes)] } {
		        set options(CheckRemotes) $DialogWin::user(CheckRemotes)
		    }
		    set options(executable_dirs) $DialogWin::user(executable_dirs)
		    UpdateExecDirs

		    if { $action == 1 } {
		        DialogWin::DestroyWindow
		        return
		    }
		}
	    }
	    3 {
		foreach i [list indentsizeTCL indentsizeC++ ConfirmStartDebugging \
		        ConfirmModifyVariable instrument_source LocalDebuggingType \
		        CheckRemotes NormalFont FixedFont HelpFont executable_dirs] {
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
	    }
	}
	set action [DialogWin::WaitForWindow]
    }
}

################################################################################
# DisplayTimes
################################################################################


proc RamDebugger::DisplayTimesWindowStart { f } {
    rtime -start
    #WarnWin "Starting to measure times. Use 'File->Debug on->currentfile' to proceed" $f
}

proc RamDebugger::DisplayTimesWindowReport { f } {
    variable text

    set f [DialogWin::Init $text "Timing report" separator "" -]
    set w [winfo toplevel $f]

    ComboBox $f.cb1 -textvariable DialogWin::user(units) -editable 0 \
       -modifycmd {
	   $DialogWin::user(text) del 1.0 end
	   $DialogWin::user(text) ins end [RamDebugger::rtime -display $DialogWin::user(units)]
       } -width 10 -values [list microsec  milisec  sec  min] -grid "0 w"
    set DialogWin::user(units) sec

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

    rtime -stop
    destroy [winfo toplevel $f]
}

proc RamDebugger::DisplayTimesPickSelection { w } {
    variable text

    if { [catch {
	set DialogWinTop::user($w,lineini) [scan [$text index sel.first] %d]
	set DialogWinTop::user($w,lineend) [scan [$text index sel.last] %d]
    }]} {
	set DialogWinTop::user($w,lineini) [scan [$text index insert] %d]
	set DialogWinTop::user($w,lineend) [scan [$text index insert] %d]
	#WarnWin "It is necessary to select some text in main window" $w
    }
}

proc RamDebugger::DisplayTimesDrawSelection { w } {
    variable text

    if { [catch {
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
	    if { [llength $idx] != 1 } {
		WarnWin "Error: it is necessary to select one block in list"
		return 1
	    }
	    set DialogWinTop::user($w,name) [lindex [$DialogWinTop::user($w,list) get $idx] 0]
	    set DialogWinTop::user($w,lineini) [lindex [$DialogWinTop::user($w,list) get $idx] 2]
	    set DialogWinTop::user($w,lineend) [lindex [$DialogWinTop::user($w,list) get $idx] 3]
	}
	update {
	    $DialogWinTop::user($w,list) del 0 end
	    foreach i $TimeMeasureData {
		$DialogWinTop::user($w,list) insert end [lrange $i 0 3]
	    }
	}
    }
}

proc RamDebugger::DisplayTimesWindow {} {
    variable text
    variable TimeMeasureData

    set commands [list RamDebugger::DisplayTimesWindowStart RamDebugger::DisplayTimesWindowReport \
	RamDebugger::DisplayTimesWindowCancel]
    set f [DialogWinTop::Init $text "Timing control" separator $commands \
	       [list Report] Start]
    set w [winfo toplevel $f]

    TitleFrame $f.f1 -text [_ "current block"] -grid "0 ew"
    set f1 [$f.f1 getframe]

    label $f1.l1 -text "Name" -grid 0
    entry $f1.e1 -textvariable DialogWinTop::user($w,name) -grid "1 ew"
    label $f1.l2 -text "Initial line:" -grid "0 e"
    entry $f1.e2 -textvariable DialogWinTop::user($w,lineini) -grid "1 w py2" -width 8
    label $f1.l3 -text "End line:" -grid "0 e"
    entry $f1.e3 -textvariable DialogWinTop::user($w,lineend) -grid "1 w py2" -width 8
    
    set bbox [ButtonBox $f1.bbox1 -spacing 0 -padx 1 -pady 1 -homogeneous 1 -grid "0 2 w"]
    $bbox add -text "Pick selection" \
	 -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1 -padx 1 -pady 1 \
	 -helptext [_ "Gets the selection from the text window"] \
	 -command "RamDebugger::DisplayTimesPickSelection $w"
    $bbox add -text "Draw selection" \
	 -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1 -padx 1 -pady 1 \
	 -helptext [_ "Selects current block in text"] \
	 -command "RamDebugger::DisplayTimesDrawSelection $w"

    TitleFrame $f.f2 -text [_ "blocks"] -grid 0
    set f2 [$f.f2 getframe]

    set sw [ScrolledWindow $f2.lf -relief sunken -borderwidth 0 -grid "0 ewns"]
    
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

    set bbox [ButtonBox $f2.bbox1 -spacing 0 -padx 1 -pady 1 -homogeneous 1 -grid "0 w"]
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

    supergrid::go $f1
    supergrid::go $f2
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
    DialogWinTop::InvokeOK $f
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
    append tt "http://gid.cimne.upc.es/RamDebugger"

    text $w.l2 -grid "0 px20 py10" -bd 0 -bg [$w cget -bg] -width 10 -height 4
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
		  -selectbackground navy -selectforeground white \
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
	    -fill \#d3513d -text "Version 2.0" -tags text
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

    set f [DialogWin::Init $text "Goto line" separator ""]
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
		    set insline [scan [$text index insert] %d]
		    set lastline [scan [$text index end-1c] %d]
		    set line [expr $insline+$line]
		    if { $line < 1 || $line > $lastline } {
		        WarnWin "Trying to go to line $line. Out of limits" $w
		        set good 0
		    }
		}
		if { $good } {
		    set lastline [scan [$text index end-1c] %d]
		    if { $line < 1 } { set line 1 }
		    if { $line > $lastline } { set line $lastline }
		    $text mark set insert $line.0
		    $text see $line.0
		    focus $text
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
    if { [regexp {\.(h|c|cc)$} $currentfile] } {
	WarnWin "Current file is not TCL"
	return
    }

    if { ![info exists options(currentfileargs)] } {
	set options(currentfileargs) ""
    }

    set dir [file dirname $currentfile]
    set arg ""
    set currs ""
    set dirs ""
    set args ""
    foreach "curr dir_in arg_in" $options(currentfileargs) {
	if { $curr == $currentfile } {
	    set dir $dir_in
	    set arg $arg_in
	}
	lappend currs $curr
	lappend dirs $dirs
	lappend args $args
    }

    set f [DialogWin::Init $text "TCL Execution arguments" separator [list Clear]]
    set w [winfo toplevel $f]
    
    label $f.l -text "Currentfile to debug:" -grid "0 e px3 py5"

    ComboBox $f.cb1 -textvariable DialogWin::user(curr) -width 40 -grid 1 -values \
	    $currs
    Button $f.b1 -image [Bitmap::get file] -width 16 -grid 2 -relief link

    label $f.l2 -text "Directory:" -grid "0 e px3 py5"
    ComboBox $f.cb2 -textvariable DialogWin::user(directory) -width 40 -grid "1" -values \
	    $dirs
    Button $f.b2 -image [Bitmap::get folder] -grid 2 -relief link

    label $f.l3 -text "Arguments:" -grid "0 e"
    ComboBox $f.cb3 -textvariable DialogWin::user(arguments) -width 40 -grid "1 2" -values \
	    $args

    set DialogWin::user(curr) $currentfile
    set DialogWin::user(directory) $dir
    set DialogWin::user(arguments) $arg

    set comm {
	set initial $RamDebugger::options(defaultdir)
	catch { set initial [file dirname $DialogWin::user(curr)] }
	set DialogWin::user(curr) [tk_getOpenFile -filetypes {{{TCL Scripts} {.tcl} }{{All Files} *}} \
		-initialdir $initial -parent PARENT -title "Debug TCL file"]
    }
    set comm [string map [list PARENT $w] $comm]
    $f.b1 configure -command $comm

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
		    set DialogWin::user(directory) ""
		    set DialogWin::user(arguments) ""
		}
		set ipos 0
		foreach "curr dir args" $options(currentfileargs) {
		    if { $curr == $DialogWin::user(curr) } {
		        set options(currentfileargs) [lreplace $options(currentfileargs) $ipos \
		            [expr $ipos+2]]
		        break
		    }
		    incr ipos 3
		}
		if { $DialogWin::user(directory) != "" || $DialogWin::user(arguments) != "" } {
		    lappend options(currentfileargs) $DialogWin::user(curr) \
		       $DialogWin::user(directory) $DialogWin::user(arguments)
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

    if { [regexp {\.(c|cc)$} $name] } {
	if { [auto_execok gcc] == "" } {
	    $mainframe setmenustate debug normal
	    cd $pwd

	    set ret [DialogWin::messageBox -default yes -icon question -message \
		"Could not find command 'gcc'. Do you want to see the help?" -parent $text \
		-title "Command not found" -type yesno]
	    if { $ret == "yes" } {
		RamDebugger::ViewHelpFile "01RamDebugger/RamDebugger_12.html"
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
		RamDebugger::ViewHelpFile "01RamDebugger/RamDebugger_12.html"
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

    eval lappend comm $files

    set fin [open "|$comm" r]

    set result ""
    while { ![eof $fin] } {
	append result [gets $fin]\n
    }
    set err [catch { close $fin } errstring]
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
	    -values $options(SearchInFiles,texts) -width 30

    if { [string trim $txt] != "" } {
	set ::RamDebugger::searchstring $txt
    } else { set ::RamDebugger::searchstring [lindex $options(SearchInFiles,texts) 0] }

    label $f.l2 -text "File ext:" -grid 0
    ComboBox $f.e2 -textvariable ::RamDebugger::searchextensions -grid "1 2 px3 py3" \
	    -values [concat $options(SearchInFiles,exts) [list "*.tcl" "*.h,*.cc,*.c"]]

    set ::RamDebugger::searchextensions [lindex [$f.e2 cget -values] 0]

    label $f.l3 -text "Directory:" -grid 0
    ComboBox $f.e3 -textvariable ::RamDebugger::searchdir -grid "1 1 px3 py3" \
	    -values $options(SearchInFiles,dirs)
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

proc RamDebugger::SearchWindow {} {
    variable text
    variable options

    if { ![info exists options(old_searchs)] } {
	set options(old_searchs) ""
    }
    if { ![info exists ::RamDebugger::searchFromBegin] } {
	set ::RamDebugger::searchFromBegin 1
    }
	
    set f [DialogWin::Init $text "Search" separator]
    set w [winfo toplevel $f]

    label $f.l1 -text "Text:" -grid 0
    ComboBox $f.e1 -textvariable ::RamDebugger::searchstring -values $options(old_searchs) \
	-grid "1 2 px3 py3"

    set f2 [frame $f.f2 -bd 1 -relief ridge -grid "0 2 w px3"]
    radiobutton $f2.r1 -text Exact -variable ::RamDebugger::searchmode \
	-value -exact -grid "0 w"
    radiobutton $f2.r2 -text Regexp -variable ::RamDebugger::searchmode \
	-value -regexp -grid "0 w"

    set f25 [frame $f.f25 -bd 1 -relief ridge -grid "2 w px3"]
    radiobutton $f25.r1 -text Forward -variable ::RamDebugger::SearchType \
	-value -forwards -grid "0 w"
    radiobutton $f25.r2 -text Backward -variable ::RamDebugger::SearchType \
	-value -backwards -grid "0 w"

    set f3 [frame $f.f3 -grid "0 3 w"]
    checkbutton $f3.cb1 -text "Consider case" -variable ::RamDebugger::searchcase \
	-grid 0
    checkbutton $f3.cb2 -text "From beginning" -variable ::RamDebugger::searchFromBegin \
	-grid 1
   
    supergrid::go $f

    set ::RamDebugger::searchstring [GetSelOrWordInIndex insert]

    set ::RamDebugger::searchmode -exact
    set ::RamDebugger::searchcase 0
    set ::RamDebugger::SearchType -forwards

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
	    set ipos [lsearch $options(old_searchs) $::RamDebugger::searchstring]
	    if { $ipos != -1 } {
		set options(old_searchs) [lreplace $options(old_searchs) $ipos $ipos]
	    }
	    set options(old_searchs) [linsert [lrange $options(old_searchs) 0 6] 0 \
		$::RamDebugger::searchstring]

	    if { $::RamDebugger::searchFromBegin } {
		if { $::RamDebugger::SearchType == "-forwards" } {
		    set idx 1.0
		} else { set idx [$text index end] }
	    } else {
		set idx [$text index insert]
	    }
	    set ::RamDebugger::SearchIni $idx
	    set ::RamDebugger::SearchPos $idx
	    set ::RamDebugger::Lastsearchstring ""
	    Search [winfo toplevel $text] any
	    DialogWin::DestroyWindow
	}
    }
}

proc RamDebugger::Search { w { what {} } } {
    variable text

    if { $what != "any" } {
	if { ![winfo exists $w.search] } {
	    entry $w.search -width 25 -textvariable RamDebugger::searchstring
	    place $w.search -in $w -x 0 -rely 1 -y -1 -anchor sw

	    focus $text
	    bindtags $text [linsert [bindtags $text] 0 $w.search]
	    bind $w.search <FocusOut> "destroy $w.search ; break"
	    bind $w.search <Escape> "destroy $w.search ; break"
	    bind $w.search <KeyPress> [list if { [string is wordchar %A] || [string is punct %A] \
		|| [string is space %A] } \
		"$w.search icursor end; tkEntryInsert $w.search %A ; break" else \
		"destroy $w.search ; break"]
	    bind $w.search <Delete> "$w.search icursor end; $w.search delete insert ; break"
	    bind $w.search <BackSpace> "$w.search icursor end; tkEntryBackspace $w.search ; break"
	    bind $w.search <1> "destroy $w.search ; break"
	    bind $w.search <3> "destroy $w.search ; break"
	    bind $w.search <Return> "destroy $w.search ; break"
	    bind $w.search <Control-i> "RamDebugger::Search $w iforward ; break"
	    bind $w.search <Control-r> "RamDebugger::Search $w ibackward ; break"
	    bind $w.search <Control-g> "RamDebugger::Search $w stop ; break"

	    set ::RamDebugger::searchstring ""
	    trace var RamDebugger::searchstring w "RamDebugger::Search $w ;#"
	    bind $w.search <Destroy> [list trace vdelete RamDebugger::searchstring w \
		"RamDebugger::Search $w ;#"]
	    bind $w.search <Destroy> "+ [list bindtags $text [lreplace [bindtags $text] 0 0]] ; break"
	    foreach i [bind Text] {
		if { [bind $w.search $i] == "" } {
		    if { [string match *nothing* [bind Text $i]] } {
			bind $w.search $i [bind Text $i]
		    } else {
			bind $w.search $i "destroy $w.search" }
		}
	    }
	    set idx [$text index insert]
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
		set idx [$text index $RamDebugger::SearchPos+${len}c]
	    } else {
		set idx [$text index $RamDebugger::SearchPos-${len}c]
	    }
	} elseif { [string length $RamDebugger::searchstring] < \
		       [string length $RamDebugger::Lastsearchstring] } {
	    set idx $RamDebugger::SearchIni
	} else { set idx $RamDebugger::SearchPos }

	set options $RamDebugger::SearchType
	lappend options $::RamDebugger::searchmode

	if { $::RamDebugger::searchcase == 1 } {
	    # nothing
	} elseif { $::RamDebugger::searchcase == 0 } {
	    lappend options -nocase
	} elseif { $RamDebugger::searchstring == [string tolower $RamDebugger::searchstring] } {
	    lappend options -nocase
	}
	lappend options --

	set idx [eval $text search $options [list $RamDebugger::searchstring] \
		     $idx $stopindex]
	if { $idx == "" } {
	    SetMessage "Search not found"
	    bell
	    if { $RamDebugger::SearchType == "-forwards" } {
		set RamDebugger::SearchPos 1.0
	    } else {
		set RamDebugger::SearchPos end
	    }
	} else {
	    set RamDebugger::SearchPos $idx
	    set len [string length $RamDebugger::searchstring]
	    if { $RamDebugger::SearchType == "-forwards" } {
		set idx2 [$text index $RamDebugger::SearchPos+${len}c]
	    } else {
		set idx2 $RamDebugger::SearchPos
		set RamDebugger::SearchPos [$text index $RamDebugger::SearchPos+${len}c]
	    }
	    set ed [$text cget -editable]
	     $text conf -editable 1
	    $text tag remove sel 1.0 end
	    if { $RamDebugger::SearchType == "-forwards" } {
		$text tag add sel $RamDebugger::SearchPos $idx2
	    } else { $text tag add sel $idx2 $RamDebugger::SearchPos }
	     $text conf -editable $ed
	    $text mark set insert $idx2
	    $text see $RamDebugger::SearchPos
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
	visualregexp { set file [file join $MainDir addons visual_regexp-2.2 visual_regexp.tcl] }
	tkcvs { set file [file join $MainDir addons tkcvs bin tkcvs.tcl] }
	tkdiff { set file [file join $MainDir addons tkcvs bin tkdiff.tcl] }
    }
    if { [interp exists $what] } { interp delete $what }
    interp create $what
    interp alias $what exit "" interp delete $what
    $what eval [list load {} Tk]
    $what eval { set argc 0 ; set argv "" }
    $what eval [list source $file]
}

proc RamDebugger::OpenConsole {} {
    variable MainDir

    set tkcon [file join $MainDir addons tkcon tkcon.tcl]

    if { $tkcon == "" } {
	WarnWin "Could not find tkcon"
	return
    }

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
	puts "Welcome to Ramdebugger inside tkcon. Use 'rhelp' for help"
    }
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
    if { $options(instrument_source) == "auto" } {
	set file [filenormalize $file]
	if { [lsearch $WindowFilesList $file] != -1 } {
	    return 1
	} else { return 0 }
    } elseif { $options(instrument_source) == "always" } {
	return 1
    } elseif { $options(instrument_source) == "never" } {
	return 0
    }

    set f [DialogWin::Init $text "Debugged program source" separator "" - OK]
    set w [winfo toplevel $f]

    label $f.l1 -text "The debugged program is trying to source file:" -grid "0 nw"
    set fname $file
    if { [string length $fname] > 60 } {
	set fname ...[string range $fname end-57 end]
    }
    label $f.l2 -text $fname -width 60 -grid "0 nw px8"
    label $f.l3 -text "What do you want to do?" -grid "0 nw"

    set f1 [frame $f.f1 -grid 0]

    radiobutton $f1.r1 -text "Instrument this file" -grid "0 w" -var DialogWin::user(opt) \
	-value thisfile
    radiobutton $f1.r2 -text "Instrument all files" -grid "0 w" -var DialogWin::user(opt) \
	-value always
    radiobutton $f1.r3 -text "Do not instrument this file" -grid "0 w" -var DialogWin::user(opt) \
	-value thisfileno
    radiobutton $f1.r4 -text "Do not instrument any file" -grid "0 w" -var DialogWin::user(opt) \
	-value never
    radiobutton $f1.r5 -text "Instrument only load files (auto)" -grid "0 w" -var DialogWin::user(opt) \
	-value auto

    if { $options(instrument_source) == "ask_yes" } {
	set DialogWin::user(opt) thisfile
    } else { set DialogWin::user(opt) thisfileno }

    label $f.l4 -text "Check preferences to change these options" -grid "0 nw"

    supergrid::go $f

    bind $w <Return> DialogWin::InvokeCancel
    DialogWin::FocusCancel

    set action [DialogWin::CreateWindow]
    DialogWin::DestroyWindow

    switch $DialogWin::user(opt) {
	thisfile {
	    set options(instrument_source) ask_yes
	    return 1
	}
	thisfileno {
	    set options(instrument_source) ask_no
	    return 0
	}
    }
    
    set options(instrument_source) $DialogWin::user(opt)

    if { $options(instrument_source) == "auto" } {
	set file [filenormalize $file]
	if { [lsearch $WindowFilesList $file] != -1 || [GiveInstFile $file 1 P] != "" } {
	    return 1
	} else { return 0 }
    } elseif { $options(instrument_source) == "always" } {
	return 1
    } elseif { $options(instrument_source) == "never" } {
	return 0
    }

}