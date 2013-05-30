
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
set "macrodata(To upper,accelerator)" "<Control-u><Control-u>"
set "macrodata(To upper,help)" "This commands converts to uppercase the editor selection"

proc "To upper" { w } {

    set range [$w tag nextrange sel 1.0 end]
    if { $range eq "" } { bell; return }

    set txt [$w get {*}$range]
    $w delete {*}$range
    $w insert [lindex $range 0] [string toupper $txt]
    $w tag add sel {*}$range
    mc::set_is_modified
}

################################################################################
#    proc tolower
################################################################################

set "macrodata(To lower,inmenu)" 1
set "macrodata(To lower,accelerator)" "<Control-u><Control-l>"
set "macrodata(To lower,help)" "This commands converts to lowercase the editor selection"

proc "To lower" { w } {

    set range [$w tag nextrange sel 1.0 end]
    if { $range eq "" } { bell; return }

    set txt [$w get {*}$range]
    $w delete {*}$range
    $w insert [lindex $range 0] [string tolower $txt]
    $w tag add sel {*}$range
    mc::set_is_modified
}

################################################################################
#    proc totitle
################################################################################

set "macrodata(To title,inmenu)" 1
set "macrodata(To title,accelerator)" "<Control-u><Control-t>"
set "macrodata(To title,help)" "This commands converts to lowercase the editor selection (first letter uppercase)"

proc "To title" { w } {

    set range [$w tag nextrange sel 1.0 end]
    if { $range eq "" } { bell; return }

    set txt [$w get {*}$range]
    $w delete {*}$range
    $w insert [lindex $range 0] [string totitle $txt]
    $w tag add sel {*}$range
    mc::set_is_modified
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
    mc::set_is_modified
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
    mc::set_is_modified
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
    mc::set_is_modified
}

################################################################################
#    proc Comment header
################################################################################

set "macrodata(Comment header,inmenu)" 1
set "macrodata(Comment header,accelerator)" ""
set "macrodata(Comment header,help)" "This commands inserts a comment menu for TCL"

proc "Comment header" { w } {
    
    switch [mc::give_active_file_type] {
	"C/C++" { lassign [list "//" ""] prefix suffix }
	"XML" { lassign [list "<!-- " " -->"] prefix suffix }
	default { lassign [list "#" ""] prefix suffix }
    }
    set options_def(extensions,C/C++) ".c .cpp .cc .h"
    set options_def(extensions,XML) ".xml .spd .xsl .xslt .svg (xml)*"

    
    $w mark set insert "insert linestart"
    $w insert insert "$prefix[string repeat # 80]$suffix\n"
    set idx [$w index insert]
    $w insert insert "$prefix    Comment$suffix\n"
    $w insert insert "$prefix[string repeat # 80]$suffix\n"
    $w tag add sel "$idx+[expr {4+[string length $prefix]}]c" "$idx+[expr {11+[string length $prefix]}]c"
    $w mark set insert $idx+[expr {11+[string length $prefix]}]c
    mc::set_is_modified
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
    mc::set_is_modified
}

################################################################################
#    proc Background color region
################################################################################

set "macrodata(Background color region,inmenu)" 1
set "macrodata(Background color region,accelerator)" "<Control-u><Control-b>"
set "macrodata(Background color region,help)" "Apply color to the selected text region background (To unapply, use function without selection)"

proc "Background color region" { w } {

    set range [$w tag nextrange sel 1.0 end]
    if { $range == "" } {
	if { [lsearch [$w tag names insert] background_color_*] == -1 } {
	    WarnWin "Select a region to apply color or put the cursor in a region to unapply"
	    return
	}
	dialogwin_snit $w._ask -title "Unapply background color" \
	    -entrytext "Do you want to unapply background color to current region?" \
	    -morebuttons [list "Unapply all"]
	set action [$w._ask createwindow]
	destroy $w._ask
	if { $action <= 0 } {  return }
	if { $action == 1 } {
	    foreach tag [$w tag names insert] {
		if { ![string match background_color_* $tag] } { continue }
		foreach "idx1 idx2" [$w tag ranges $tag] {
		    if { [$w compare insert >= $idx1] && \
		        [$w compare insert < $idx2] } {
		        $w tag remove $tag $idx1 $idx2
		    }
		}
	    }
	} elseif { $action == 2 } {
	    foreach tag [$w tag names] {
		if { ![string match background_color_* $tag] } { continue }
		$w tag delete $tag
	    }
	}
	return
    }
    set color [tk_chooseColor -initialcolor grey20]
    if { $color eq "" } { return }

    $w tag add background_color_$color [lindex $range 0] [lindex $range 1]
    $w tag configure background_color_$color -background $color
    $w tag lower background_color_$color sel
}

################################################################################
#    proc Toggle debug lines
################################################################################

proc activate_deactivate_debug_lines { w activate_deactivate_toggle } {
    
    lassign [$w tag ranges sel] idx idx_end
    if { $idx eq "" } {
	lassign [list "insert linestart" "insert lineend"] idx idx_end
    }
    lassign "0 0" num_changes num_found
    while { [set idx [$w search -regexp -count ::count {\m(printf_\w+)} $idx $idx_end]] ne "" } {
	set name [$w get $idx "$idx + $::count c"]
	set replace 1
	switch $activate_deactivate_toggle {
	    activate {
		if { ![regexp {(.*)_OFF} $name] } { set replace 0 }
	    }
	    deactivate {
		if { [regexp {(.*)_OFF} $name] } { set replace 0 }
	    }
	    toggle {
		# nothing
	    }
	    default {
		error "error in activate_deactivate_debug_lines"
	    }
	}
	if { $replace } {
	if { ![regexp {(.*)_OFF} $name {} name] } {
	    set name ${name}_OFF
	}
	$w replace $idx "$idx + $::count c" $name
	incr num_changes
    }
	set idx [$w index "$idx + $::count c"]
	incr num_found
    }
    if { $num_found == 0 } {
	tk_messageBox -message "Current line should contain a command similar to \"printf_debug...\""
    }
    mc::set_is_modified
}

set "macrodata(Activate debug lines,inmenu)" 1
# recommended: <Control-Shift-F8>
set "macrodata(Activate debug lines,accelerator)" "<Control-Shift-F8>"
set "macrodata(Activate debug lines,help)" "convert C functions: printf_debug to printf_debug_OFF and viceversa"

proc "Activate debug lines" { w } {

    set wg $w.g
    destroy $wg
    dialogwin_snit $wg -title [_ "Enter action"] -entrylabel [_ "Action"]: \
	-entrytype noneditable_entry -entryvalues [list activate deactivate toggle] \
	-entrydefault activate
    set action [$wg createwindow]
    set txt [$wg giveentryvalue]
    destroy $wg
    if { $action <= 0 } {  return }
    activate_deactivate_debug_lines $w $txt
}

set "macrodata(Toggle debug lines,inmenu)" 1
# recommended: <Shift-F8>
set "macrodata(Toggle debug lines,accelerator)" "<Shift-F8>"
set "macrodata(Toggle debug lines,help)" "convert C functions: printf_debug to printf_debug_OFF and viceversa"

proc "Toggle debug lines" { w } {
    activate_deactivate_debug_lines $w toggle
}

################################################################################
#    proc Debug GiD Post
################################################################################

# package require commR
# set tkcon_commr_id [commR::register tkcon 1]

proc ddp { args } {
    global debug_commr_id
    
    if { ![info exists debug_commr_id] } {
	package require commR
	set debug_commr_id [commR::register debug_GiDPost 1]
    }
    for { set i 12350 } { $i < 12360 } { incr i } {
	set err [catch { commR::comm send $i [list commR::givename $debug_commr_id] } ret]
	if { !$err && $ret eq "GiDPost" } {
	    break
	}
    }
    commR::comm send $i {*}$args
}

proc send_draw_point_to_gidpost { txt } {
    if { ![regexp {([-+.\deE]+),([-+.\deE]+),([-+.\deE]+)} $txt {} px py pz] } { continue }
    ddp [list draw_post::draw_signal_point 0 [list $px $py $pz]]
}

proc send_draw_box_to_gidpost { txt } {
    if { ![regexp {p0=(\S+)\s+L=(\S+)} $txt {} p0 L] } { continue }
    
    set p0 [split $p0 ","]
    set L [split $L ","]
    if { [llength $L] == 1 } {
	set L [lrepeat 3 $L]
    }
    set entList ""
    foreach i [list point ariste face] {
	set rex [format {idx_%s[^=]*=(\d+)} $i]
	if { [regexp $rex $txt {} num] } {
	    lappend entList $i $num
	}
    }
    ddp [list draw_post::draw_point_line_indicator -draw_arrow_to_center 0 box_axes $p0 $L $entList]
}
    
set "macrodata(Debug box GiD Post,inmenu)" 0
# recommended: <Control-u><F1>
set "macrodata(Debug box GiD Post,accelerator)" ""
set "macrodata(Debug box GiD Post,help)" "Draw a debug box in an open GiDPost"

proc "Debug box GiD Post" { w } {

    set range [$w tag nextrange sel 1.0 end]
    if { $range ne "" } {
	set txt [$w get {*}$range]
    } else {
       set txt ""
    }
    if { $range eq "" || [regexp {idx_point|idx_ariste|idx_face} $txt] } {
	set wg $w.g
	destroy $wg
	dialogwin_snit $wg -title [_ "Enter variable"] -entrylabel [_ "Enter variable name"]: \
	    -entrytype entry -entryvalues [list this] -entrydefault "this $txt"
	set action [$wg createwindow]
	set txt [string trim [$wg giveentryvalue]]
	destroy $wg
	if { $action <= 0 } {  return }
    }
    lassign [split $txt] var1 var2
    if { $var2 ne "" } {
	set format "\"$var2=%d\",$var2"   
    } else {
	set format "\"\""
    }
    set ev [RamDebugger::reval $var1]
    if { [regexp {PointAristeFace} $ev] } {
	set ev [RamDebugger::reval "bprintf_debug_paf(\"DEBUG PAF\",*$var1,$format)"]
    } elseif { [regexp {mesh_tree_full_leave} $ev] } {
	switch -- $var2 {
	    "idx_point" { set cmd "bprintf_debug_boxP(\"DEBUG BOX\",$var1,$var2)" }
	    "idx_ariste" { set cmd "bprintf_debug_boxA(\"DEBUG BOX\",$var1,$var2)" }
	    "idx_face" { set cmd "bprintf_debug_boxF(\"DEBUG BOX\",$var1,$var2)" }
	    "default" {   set cmd "bprintf_debug_box(\"DEBUG BOX\",$var1,\"\")" }
	}
	set ev [RamDebugger::reval $cmd]
    } else {
	return
    }
    send_draw_box_to_gidpost $ev
}

set "macrodata(Debug point GiD Post,inmenu)" 0
# recommended: <Control-u><F2>
set "macrodata(Debug point GiD Post,accelerator)" ""
set "macrodata(Debug point GiD Post,help)" "Draw a debug point in an open GiDPost"

proc "Debug point GiD Post" { w } {

    set range [$w tag nextrange sel 1.0 end]
    if { $range eq "" } {
	set wg $w.g
	destroy $wg
	dialogwin_snit $wg -title [_ "Enter variable"] -entrylabel [_ "Enter variable name"]: \
	    -entrytype entry -entryvalues [list this]
	set action [$wg createwindow]
	set txt [$wg giveentryvalue]
	destroy $wg
	if { $action <= 0 } {  return }
    } else {
	set txt [$w get {*}$range]
    }
    set ev [RamDebugger::reval $txt]
    if { [regexp {PointAristeFace} $ev] } {
	set ev [RamDebugger::reval "bprintf_debug_paf(\"DEBUG PAF\",*$txt,\"\")"]
    } elseif { [regexp {mesh_tree_full_leave} $ev] } {
	set ev [RamDebugger::reval "bprintf_debug_box(\"DEBUG BOX\",$txt,\"\")"]
    } else {
	return
    }
    send_draw_point_to_gidpost $ev
}

################################################################################
#    proc Open comment file
################################################################################

set "macrodata(Open comment file,inmenu)" 1
set "macrodata(Open comment file,accelerator)" <Control-u><Control-o>
set "macrodata(Open comment file,help)" "Get the text under the cursor and try to open this file name"

proc "Open comment file" { w } {

    set line1 [$w get "insert linestart" insert]
    set line2 [$w get insert "insert lineend"]
    
    lassign "" txt1 txt2
    if { ![regexp {"([^"]*)$} $line1 {} txt1] || ![regexp {^([^"]*)"} $line2 {} txt2] } {
	regexp {\S*$} $line1 txt1
	regexp {^\S*} $line2 txt2
    }
    set file "$txt1$txt2"
    if { $file eq "" } {
	WarnWin "there is no file name under the cursor"
	return
    }
    set page ""
    regexp {([^#]*)#(.*)} $file {} file page
    
    set currentfile [mc::give_currentfile]
    
    if { ![file exists $file] && [file exists [file join [file dirname $currentfile] $file]] } {
	set file [file join [file dirname $currentfile] $file]
    }
    if { ![file exists $file] } {
	set pwd [pwd]
	cd [file dirname $currentfile]
	set err [catch { exec fossil info } ret]
	cd $pwd
	if { !$err && [regexp -line {local-root:\s+(.*)} $ret {} dir] } {
	    if { [file exists [file join [string trim $dir] $file]] } {
		set file [file join [string trim $dir] $file]
	    }
	}
    }
    if { ![file exists $file] } {
	WarnWin "'$file' does not exist"
	return
    }
    if { [string tolower [file extension $file]] eq ".wnl" && $page ne "" } {
	foreach exe [list lognoter dlognoter] {
	    if { [auto_execok $exe] ne "" } {
		set err [catch { mc::execute exec $exe -notebookname $file -page $page & } ret]
		if { $err } {
		    WarnWin "Failed executing '$exe -notebookname $file -page $page' ($ret)"
		}
		return
	    }
	}
	WarnWin "Failed executing lognoter"
    } else {
	set err [catch { mc::execute start $file } ret]
	if { $err } {
	    WarnWin "Failed executing '$file' ($ret)"
	}
    }
}




