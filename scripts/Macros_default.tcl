
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
    
    set f [DialogWin::Init $w "Go to proc" separator]
    set sw [ScrolledWindow $f.lf -relief sunken -borderwidth 0 -grid "0 2"]
    
    set DialogWin::user(list) [tablelist::tablelist $sw.lb -width 55\
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
	    -highlightthickness 0]
    
    $sw setwidget $DialogWin::user(list)
    $DialogWin::user(list) columnconfigure 0 -sortmode dictionary
    $DialogWin::user(list) columnconfigure 1 -sortmode dictionary
    $DialogWin::user(list) columnconfigure 2 -sortmode dictionary
    $DialogWin::user(list) columnconfigure 3 -sortmode integer

    foreach i $procs {
	$DialogWin::user(list) insert end $i
	if { [string match *snit* [lindex $i 2]] } {
	    $DialogWin::user(list) rowconfigure end -background orange
	}
    }
    $DialogWin::user(list) selection set 0
    $DialogWin::user(list) activate 0

    bind [$DialogWin::user(list) bodypath] <Double-1> {
	DialogWin::InvokeOK
    }
    bind [$DialogWin::user(list) bodypath] <Return> {
	DialogWin::InvokeOK
    }
    bind [$DialogWin::user(list) bodypath] <KeyPress> {
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

    set action [DialogWin::CreateWindow]

    if { $action == 1 } {
	set curr [$DialogWin::user(list) curselection]
	if { [llength $curr] != 1 } { return }
	set line [lindex [$DialogWin::user(list) get $curr] 2]
	$w mark set insert $line.0
	$w see $line.0
	focus $w
    }
    DialogWin::DestroyWindow
}
