
################################################################################
# This is the macros document, you can add your own macros here


# The format is the following:
#
# if macro name is inside variable 'macroname', data to fill is the following:
#
# set macrodata($macroname,inmenu) 1  .- can be 0 or 1. If 1, it appears in 
#                                        RamDebugger menu
# set macrodata($macroname,accelerator) <Control-u> .- Enter one accelerator that
#                                                      will be applied globally
# set macrodata($macroname,help) text .- Enter the description of the macro
#
# proc $macroname { w } { ... } .- Argument w is the path of the editor text widget
################################################################################




################################################################################
#    proc toupper
################################################################################

set "macrodata(To upper,inmenu)" 1
set "macrodata(To upper,accelerator)" "<Control-u>"
set "macrodata(To upper,help)" "This commands converts to uppercase the editor selection"

proc "To upper" { w } {

    set range [$w tag nextrange sel 1.0 end]
    if { $range == "" } { bell; return }

    set txt [eval $w get $range]
    eval $w delete $range
    $w insert [lindex $range 0] [string toupper $txt]
    eval $w tag add sel $range
}

################################################################################
#    proc tolower
################################################################################

set "macrodata(To lower,inmenu)" 1
set "macrodata(To lower,accelerator)" ""
set "macrodata(To lower,help)" "This commands converts to lowercase the editor selection"

proc "To lower" { w } {

    set range [$w tag nextrange sel 1.0 end]
    if { $range == "" } { bell; return }

    set txt [eval $w get $range]
    eval $w delete $range
    $w insert [lindex $range 0] [string tolower $txt]
    eval $w tag add sel $range
}

################################################################################
#    proc insert rectangular text
################################################################################

set "macrodata(Insert rectangular text,inmenu)" 1
set "macrodata(Insert rectangular text,accelerator)" ""
set "macrodata(Insert rectangular text,help)" "Inserts text from cliboard to every row of the selection"

proc "Insert rectangular text" { w } {

    set txt [clipboard get]
    set range [$w tag nextrange sel 1.0 end]
    if { $range == "" } { bell; return }
    scan [lindex $range 0] "%d.%d" l1 c1
    scan [lindex $range 1] "%d.%d" l2 c2
    if { $c2 < $c1 } { set tmp $c1 ; set c1 $c2 ; set c2 $tmp }
    if { $l2 < $l1 } { set tmp $l1 ; set l1 $l2 ; set l2 $tmp }

    for { set i $l1 } { $i <= $l2 } { incr i } {
	$w delete $i.$c1 $i.$c2
	$w insert $i.$c1 $txt
    }
}

################################################################################
#    proc Kill rectangular text
################################################################################

set "macrodata(Kill rectangular text,inmenu)" 1
set "macrodata(Kill rectangular text,accelerator)" ""
set "macrodata(Kill rectangular text,help)" "Kills all text contained in the rectangular part of the selection"

proc "Kill rectangular text" { w } {

    set range [$w tag nextrange sel 1.0 end]
    if { $range == "" } { bell; return }
    scan [lindex $range 0] "%d.%d" l1 c1
    scan [lindex $range 1] "%d.%d" l2 c2
    if { $c2 < $c1 } { set tmp $c1 ; set c1 $c2 ; set c2 $tmp }
    if { $l2 < $l1 } { set tmp $l1 ; set l1 $l2 ; set l2 $tmp }

    for { set i $l1 } { $i <= $l2 } { incr i } {
	$w delete $i.$c1 $i.$c2
    }
}


################################################################################
#    proc Macro regsub
################################################################################

set "macrodata(Macro regsub,inmenu)" 1
set "macrodata(Macro regsub,accelerator)" ""
set "macrodata(Macro regsub,help)" "This commands applies a user-defined regsub to the selected text"

proc "Macro regsub" { w } {

    set range [$w tag nextrange sel 1.0 end]
    if { $range == "" } { 
	WarnWin "Select a region to modify"
	return
    }

    set f [DialogWin::Init $w "Macro regsub" separator ""]
    set wf [winfo toplevel $f]

    label $f.l -text "Enter a regsub to be applied to variable 'sel':" -grid "0 px3 py5"
    text $f.t -wrap word -width 80 -height 4 -grid 0
    $f.t insert end {regsub -all {text1} $sel {text2} sel}
    $f.t tag add sel 1.0 end-1c
    tkTabToWindow $f.t

    bind $wf <Return> "DialogWin::InvokeOK"

    supergrid::go $f

    set action [DialogWin::CreateWindow]
    set reg [$f.t get 1.0 end-1c]
    DialogWin::DestroyWindow
    if { $action == 0 } { return }

    set sel [eval $w get $range]
    set err [catch { eval $reg } errstring]
    if { $err } {
	WarnWin "Error applying regsub: $errstring"
	return
    }
    eval $w delete $range
    $w insert [lindex $range 0] $sel
    eval $w tag add sel $range
}

################################################################################
#    proc Comment header
################################################################################

set "macrodata(Comment header,inmenu)" 1
set "macrodata(Comment header,accelerator)" ""
set "macrodata(Comment header,help)" "This commands inserts a comment menu for TCL"

proc "Comment header" { w } {
    $w mark set insert "insert linestart"
    $w insert insert "[string repeat # 80]\n"
    set idx [$w index insert]
    $w insert insert "#    Comment\n"
    $w insert insert "[string repeat # 80]\n"
    $w tag add sel "$idx+5c" "$idx+12c"
    $w mark set insert $idx+12c
}

################################################################################
#    proc Go to proc
################################################################################

set "macrodata(Go to proc,inmenu)" 1
set "macrodata(Go to proc,accelerator)" "<Control-G>"
set "macrodata(Go to proc,help)" "This commands permmits to select a proc to go"

proc "Go to proc" { w } {

    set procs ""
    set numline 1
    foreach line [split [$w get 1.0 end-1c] \n] {
	set types {proc|method|constructor|onconfigure|snit::type|snit::widget|snit::widgetadaptor}

	if { [regexp "^\\s*(?:::)?($types)\\s+(\[\\w:]+)" $line {} type name] } {
	    set namespace ""
	    regexp {(.*)::([^:]+)} $name {} namespace name
	    lappend procs [list $name $namespace $type $numline]
	}
	incr numline
    }
    
    set wg $w.g
    destroy $wg
    dialogwin_snit $wg -title "Go to proc"
    set f [$wg giveframe]
    set sw [ScrolledWindow $f.lf -relief sunken -borderwidth 0 -grid "0 2"]
    
    tablelist::tablelist $sw.lb -width 60\
	    -height 35 -exportselection 0 \
	-columns [list \
		20  "Proc name"        left \
		14  "Proc namespace"   left \
		10  "Proc type"        center \
		5  "line"     right \
		] \
	    -labelcommand tablelist::sortByColumn \
	    -background white \
	    -selectbackground navy -selectforeground white \
	    -stretch "" -selectmode browse \
	-highlightthickness 0
    
    $sw setwidget $sw.lb
    $sw.lb columnconfigure 0 -sortmode dictionary
    $sw.lb columnconfigure 1 -sortmode dictionary
    $sw.lb columnconfigure 2 -sortmode dictionary
    $sw.lb columnconfigure 3 -sortmode integer

    foreach i $procs {
	$sw.lb insert end $i
	if { [string match *snit* [lindex $i 2]] } {
	    $sw.lb rowconfigure end -background orange
	}
    }
    $sw.lb sortbycolumn 0
    $sw.lb selection set 0
    $sw.lb activate 0

    bind [$sw.lb bodypath] <Double-1> [list $wg invokeok]
    bind [$sw.lb bodypath] <Return> [list $wg invokeok]

    bind [$sw.lb bodypath] <KeyPress> {
	set w [winfo parent %W]
	set idx [$w index active]
	if { [string is wordchar -strict %A] } {
	    if { ![info exists ::searchstring] } { set ::searchstring "" }
	    if { ![string equal $::searchstring %A] } {
		append ::searchstring %A
	    }
	    set found 0
	    for { set i [expr {$idx+1}] } { $i < [$w index end] } { incr i } {
		if { [string match -nocase $::searchstring* [lindex [$w get $i] 0]] } {
		    set found 1
		    break
		}
	    }
	    if { !$found } {
		for { set i 0 } { $i < $idx } { incr i } {
		    if { [string match -nocase $::searchstring* [lindex [$w get $i] 0]] } {
		        set found 1
		        break
		    }
		}
	    }
	    if { $found } {
		$w selection clear 0 end
		$w selection set $i
		$w activate $i
		$w see $i
	    }
	    after 500 unset -nocomplain ::searchstring
	}
    }
    supergrid::go $f
    focus $sw.lb

    set action [$wg createwindow]

    while 1 {
	switch -- $action {
	    -1 - 0 {
		destroy $wg
		return
	    }
	    1 {
		set curr [$sw.lb curselection]
		if { [llength $curr] == 1 } {
		    set line [lindex [$sw.lb get $curr] 3]
	$w mark set insert $line.0
	$w see $line.0
	focus $w
		    destroy $wg
		    return
		} else {
		    tk_messageBox -message "Select one function in order to go to it"
		}
	    }
	}
	set action [$wg waitforwindow]
    }
}

################################################################################
#    proc Mark translation strings
################################################################################

set "macrodata(Mark translation strings,inmenu)" 0
# recomended: <F8>
set "macrodata(Mark translation strings,accelerator)" ""
set "macrodata(Mark translation strings,help)" "Mark translation strings for GiD"

proc "Mark translation strings" { w } {

    set trans_cmd "="
    set rex {(?:-text|-label|-helptext)\s+([^\[$\s]\S+|\"[^\"]+\"|{[^\}]+})}
    set rex_before "(\[\[\]\\s*$trans_cmd\\s+\$|-command\\s+\$|^\\s*bind\\s+)"
    set rex_cmd {(-command\s+$|^\s*bind\s+)}

    destroy $w._t
    set top [toplevel $w._t]
    wm overrideredirect $top 1
    label $top.l -bg white -fg black -bd 1 -relief raised -width 25
    set labeltext(trans) "Y modify string\nS modify and stop\nN, F8 do not modify\nESC stop"
    set labeltext(err) "Y, N, F8 continue\nESC stop"

    pack $top.l
    bind $top.l <KeyPress> {set Mark_translation_strings_var %K; break}
    bind $top.l <ButtonPress-1> {set Mark_translation_strings_var Escape}
    focus $top.l
    wm withdraw $top
    $w tag configure markerrstrings -background red -foreground black
    $w tag configure marktransstrings -background orange1 -foreground black

    while 1 {
	set type_idx1 trans
	set type_idx2 trans
	set idx1 insert
	while 1 {
	    set idx1 [$w search -regexp -count ::len1 $rex $idx1 end]
	    if { $idx1 eq "" } { break }
	    set before [$w get "$idx1 linestart" $idx1]
	    set curr [$w get $idx1 "$idx1+${::len1}c"]
	    regexp -indices $rex $curr {} s1
	    if { ![regexp $rex_before $before] } {
		set idx1_end "$idx1+[expr {[lindex $s1 1]+1}]c"
		set idx1 "$idx1+[lindex $s1 0]c"
		break
	    }
	    set idx1 "$idx1+${::len1}c"
	}
	set idx2 insert
	while 1 {
	    set ret [$w tag nextrange grey $idx2]
	    if { $ret eq "" } {
		set idx2 ""
		break
	    }
	    foreach "idx2 idx2_end" $ret break
	    set txt [$w get $idx2 $idx2_end]
	    set txt_before [$w get "$idx2 linestart" $idx2]
	    if { [string length $txt] > 2 } {
		if { [regexp $rex_before $txt_before] } {
		    if { ![regexp -- $rex_cmd $txt_before] && \
		        [regexp {[^\\]\$} $txt] } {
		        set type_idx2 err
		        break
		    }
		} else { break }
	    }
	    set idx2 $idx2_end
	}
	if { $idx1 eq "" && $idx2 eq "" } {
	    bell
	    break
	}
	if { $idx1 ne "" && ($idx2 eq "" || [$w compare $idx1 < $idx2]) } {
	    foreach "ini end type" [list $idx1 $idx1_end $type_idx1] break
	} else {
	    foreach "ini end type" [list $idx2 $idx2_end $type_idx2] break
	}
	if { $type eq "trans" } {
	    $w tag add marktransstrings $ini $end
	} else {
	    $w tag add markerrstrings $ini $end
	}
	$top.l configure -text $labeltext($type)
	$w see $ini
	update idletasks
	foreach "tx ty tw th" [$w bbox $ini] break
	wm deiconify $top
	set tx [expr {$tx+[winfo rootx $w]}]
	set ty [expr {$ty+$th+[winfo rooty $w]+10}]
	wm geometry $top +$tx+$ty
	grab $top.l
	focus -force $top.l
	vwait Mark_translation_strings_var
	set stopafter 0
	switch -- $::Mark_translation_strings_var {
	    y - Y {
		if { $type eq "trans" } {
		    set what change
		} else { set what cont }
	    }
	    s - S {
		if { $type eq "trans" } {
		    set what change
		} else { set what cont }
		set stopafter 1
	    }
	    n - N - F8 { set what cont }
	    default { set what end }
	}
	switch $what {
	    change {
		set data [$w get $ini $end]
		regsub -all {([^\\])\$(\w+|{[^\}]+})} $data {\1%s} data_new
		set data_new "\[$trans_cmd $data_new"
		set rex_var {[^\\]\$(\w+|{[^\}]+})}
		foreach "- v" [regexp -inline -all $rex_var $data] {
		    append data_new " \$$v"
		}
		append data_new "\]"
		$w delete $ini $end
		$w insert $ini $data_new
		$w mark set insert "$ini+[string length $data_new]c"
		if { $stopafter } { break }
	    }
	    cont {
		$w tag remove marktransstrings $ini $end
		$w tag remove markerrstrings 1.0 end
		$w mark set insert $end
	    }
	    end {
		$w mark set insert $ini
		break
	    }
	}
	wm withdraw $top
    }
    destroy $top
    $w tag remove marktransstrings 1.0 end
    $w tag remove markerrstrings 1.0 end
    focus $w
}

################################################################################
#    proc Convert GiD help Strings
################################################################################

set "macrodata(Convert GiD help Strings,inmenu)" 0
# recommended: <Control-F8>
set "macrodata(Convert GiD help Strings,accelerator)" ""
set "macrodata(Convert GiD help Strings,help)" "Convert GiD help Strings"

proc "Convert GiD help Strings" { w } {

    set trans_cmd "="
    set range [$w tag nextrange sel 1.0 end]
    if { $range == "" } { 
	WarnWin "Select a region to modify"
	return
    }
    set data [eval $w get $range]

    regsub -line -all {^(\s*.)\"} $data {\1} data
    regsub -line -all {\s*\"\s*\\$} $data {\\} data

    eval $w delete $range
    $w insert [lindex $range 0] "\[$trans_cmd $data\]"
}














