
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
    if { $c2 < $c1 } {
	set tmp $c1
	set c1 $c2
	set c2 $tmp
    }

    for { set i $l1 } { $i <= $l2 } { incr i } {
	$w delete $i.$c1 $i.$c2
	$w insert $i.$c1 $txt
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
