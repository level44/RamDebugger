
package require treectrl
package require snit
package require autoscroll
package require tooltip
catch { package require compass_utils }

#kike: add catch because GiD has img::png internal, starting ramdebugger can't load img::png as external package
catch {package require img::png}

package provide fulltktree 1.22

if { [info commands ttk::style] eq "" } {
    interp alias "" ttk::style "" style
}

namespace eval ::fulltktree_top {   
    if { [info script] != "" } {
    variable topdir [file dirname [info script]]   
    } else {
	#to be debugged by ramdebugger inside GiD
	variable topdir [file join $::GIDDEFAULTTCL fulltktree]
    }
}
source [file join $::fulltktree_top::topdir fulltktree_bindings.tcl]

# for tclIndex
proc fulltktree { args } {}

#  -editbeginhandler can return:
# 0: no edition; 1,entry: edit with entry; combo "editable values dict": edit with combo;
# combotree  "editable values_tree"; anywidget widget
# -columns: list with this information for each column:
#              <width> <name> <justify> <type> <is_editable> <expand>
#
#              <justify> tktreectrl standard options (left, right, center)
#              <type> now can be: item imagetext text text_r image
#              <is_editable> boolean
#              <expand> boolean

snit::widget fulltktree {
    option -button1handler "" ;# returns also identify information
    option -button1presshandler "" ;# returns also identify information
    option -selecthandler "" ;# button-1 or return (if not selecthandler2)
    option -selecthandler2 "" ;# double button or return
    option -returnhandler "" ;# if not null, previous options do not react on return
    option -contextualhandler ""
    option -contextualhandler_menu ""
    option -contextualhandler_fmenu "" ;# like previous but adding more arguments at the callback
    option -editbeginhandler "" ;# if returns 0, the edition does not begin (optional)
    option -editaccepthandler ""
    option -editrejecthandler ""
    option -deletehandler ""
    option -draghandler ""
    option -columns ""
    option -expand 0
    option -compass_background 0
    option -selectioncolors "" ; # "backgroundcolor foregroundcolor"
    option -selectmode extended
    option -sensitive_cols "" ;# can be a list of column indexes or all
    option -sort_type_cols ""; # can be a list of sort types: dictionary, integer real
    option -item_image ""
    option -folder_image ""
    option -buttonpress_open_close 0
    option -has_sizegrip 0
    option -spreadsheet_mode 0
    option -keypress_search_active 1
    option -have_search_button 0 ;# 0, 1 or automatic
    option -have_vscrollbar automatic ;# 0, 1 or automatic
    option -recursive_sort 0
    option -takefocus 0 ;# option used by the tab standard bindings
    option -arrow_image_expand 1

    variable tree
    variable vscrollbar
    variable hscrollbar
    variable searchbutton

    typevariable SystemHighlight
    typevariable SystemHighlightText
    typevariable SystemFont

    variable searchstring ""
    variable searchstring_reached_end
    variable search_full_words 0

    variable sortcolumn
    variable itemStyle
    variable last_active_item ""
    variable last_end_item ""
    
    variable editcolumns ""
    variable selecthandler_active 1
    variable accept_edit_item_last_accepted ""

    variable uservar
    typevariable type_uservar
    
    variable popup_help_dict ""

    delegate method * to tree
    delegate method _selection to tree as selection
    delegate option * to tree
    delegate option -borderwidth to hull
    delegate option -bd to hull
    delegate option -relief to hull

    constructor { args } {
	$self createimages_colors
	
	set height [font metrics $fulltktree::SystemFont -linespace]
	if {$height < 18} { set height 18 }
	
	install tree as treectrl $win.t -highlightthickness 0 -borderwidth 0 \
	    -xscrollincrement 20 -showheader 1 -indent 19 \
	    -font $fulltktree::SystemFont -minitemheight $height -selectmode extended -showroot no \
	    -showrootbutton no -showbuttons yes -showlines yes \
	    -scrollmargin 16 -xscrolldelay "500 50" -yscrolldelay "500 50" \
	    -yscrollincrement $height

	catch { $win.t configure -usetheme 1 }
	
	catch { $hull configure -bg [cu::give_widget_background [winfo parent $win]] }

	install vscrollbar as ttk::scrollbar $win.sv -orient vertical -command [list $win.t yview] -takefocus 0
	install hscrollbar as ttk::scrollbar $win.sh -orient horizontal -command [list $win.t xview] -takefocus 0
	if { $options(-arrow_image_expand) } {

	set err [catch {
		$tree configure -openbuttonimage mac-collapse \
		    -closedbuttonimage mac-expand
	    }]
	if { $err } {
	    $tree configure -buttonimage [list mac-collapse open mac-expand !open ]
	}
	}
	autoscroll::autoscroll $win.sh
	if { $options(-have_vscrollbar) eq "automatic" } {
	    autoscroll::autoscroll $win.sv
	}
	bind $tree <Configure> [mymethod check_scroll_idle]
	
	#$self configure -background white

	$tree state define disabled
	$tree state define emphasis
	$tree state define readonly

	bind $win <FocusIn> [mymethod focus -only_if_win %W]
	grid $win.t $win.sv -sticky ns -row 2
	grid $win.sh -sticky ew -row 3
	grid configure $win.t -sticky nsew
	grid columnconfigure $win 0 -weight 1
	grid rowconfigure $win 2 -weight 1
	
	$tree element create e_folder_image image -image \
	    {folder-open {open} folder-closed {}}

	$tree element create e_item_image image -image appbook16
	$tree element create e_image image
	$tree element create e_image_r image
	
	set bfont [concat [font actual [$tree cget -font]] [list -weight bold]]
	$tree element create e_text_sel text -lines 1 \
	    -fill [list grey disabled grey !enabled green readonly \
		$fulltktree::SystemHighlightText {selected focus} \
		] -font [list $bfont emphasis]
	$tree element create e_rect rect -fill \
	    [list $fulltktree::SystemHighlight {selected focus} gray90 {selected !focus}] \
	    -showfocus yes -open we
	$tree element create e_hidden text -lines 1 -format " " -datatype string
	$tree element create e_selmarker_up rect 
	$tree element create e_selmarker_down rect
	if { [package vcompare [package present treectrl] 2.1] >= 0 } {
	    $tree element create e_window window
	}
	$tree element create e_rect_disabled rect -fill grey \
	    -showfocus no -width 20 -height 20
	
################################################################################
#    style folder
################################################################################
	
	set S [$tree style create folder -orient horizontal]
	$tree style elements $S [list e_rect e_folder_image e_text_sel \
		e_selmarker_up e_selmarker_down]
	$tree style layout $S e_folder_image -expand ns
	$tree style layout $S e_text_sel -padx {4 0} -squeeze x -expand ns \
	    -iexpand ns
	if { [package vcompare [package present treectrl] 2.1] >= 0 } {
	    $tree style layout $S e_text_sel -iexpand nsx -sticky w
	}
	$tree style layout $S e_rect -union [list e_text_sel] -iexpand nswe -ipadx 2
	$tree style layout $S e_selmarker_up -detach 1
	    
	if { [package vcompare [package present treectrl] 2.1] >= 0 } {
	    $tree style layout $S e_selmarker_up -height 2 -sticky nwe -iexpand x
	}
	$tree style layout $S e_selmarker_down -detach 1 \
	     -expand n
	if { [package vcompare [package present treectrl] 2.1] >= 0 } {
	    $tree style layout $S e_selmarker_down -height 2 -sticky swe -iexpand x
	}
	
################################################################################
#    style item
################################################################################

	set S [$tree style create item -orient horizontal]
	$tree style elements $S [list e_rect e_item_image e_text_sel \
		e_selmarker_up e_selmarker_down]
	$tree style layout $S e_item_image -expand ns
	$tree style layout $S e_text_sel -padx {4 0} -squeeze x -expand ns \
	    -iexpand ns
	if { [package vcompare [package present treectrl] 2.1] >= 0 } {
	    $tree style layout $S e_text_sel -iexpand nsx -sticky w
	}
	$tree style layout $S e_rect -union [list e_text_sel] -iexpand nswe -ipadx 2
       
	$tree style layout $S e_selmarker_up -detach 1

	if { [package vcompare [package present treectrl] 2.1] >= 0 } {
	    $tree style layout $S e_selmarker_up -height 2 -sticky nwe -iexpand x
	}
	$tree style layout $S e_selmarker_down -detach 1 \
	    -expand n

	if { [package vcompare [package present treectrl] 2.1] >= 0 } {
	    $tree style layout $S e_selmarker_down -height 2 -sticky swe -iexpand x
	}

################################################################################
#    style imagetext
################################################################################

	set S [$tree style create imagetext -orient horizontal]
	$tree style elements $S [list e_rect e_image e_text_sel \
		e_selmarker_up e_selmarker_down]
	$tree style layout $S e_image -expand ns
	$tree style layout $S e_text_sel -padx {4 0} -squeeze x -expand ns \
	    -iexpand ns
	if { [package vcompare [package present treectrl] 2.1] >= 0 } {
	    $tree style layout $S e_text_sel -iexpand nsx -sticky w
	}
	$tree style layout $S e_rect -union [list e_text_sel] -iexpand nswe -ipadx 2

	$tree style layout $S e_selmarker_up -detach 1
	    
	if { [package vcompare [package present treectrl] 2.1] >= 0 } {
	    $tree style layout $S e_selmarker_up -height 2 -sticky nwe -iexpand x
	}
	$tree style layout $S e_selmarker_down -detach 1 \
	    -expand n
	if { [package vcompare [package present treectrl] 2.1] >= 0 } {
	    $tree style layout $S e_selmarker_down -height 2 -sticky swe -iexpand x
	}

################################################################################
#    style imagetextimage
################################################################################

	set S [$tree style create imagetextimage -orient horizontal]
	$tree style elements $S [list e_rect e_image e_text_sel \
		e_image_r e_selmarker_up e_selmarker_down]
	$tree style layout $S e_image -expand ns
	$tree style layout $S e_image_r -expand ns -padx {2 2}
	$tree style layout $S e_text_sel -padx {4 0} -squeeze x -expand ns \
	    -iexpand ns
	if { [package vcompare [package present treectrl] 2.1] >= 0 } {
	    $tree style layout $S e_text_sel -iexpand nsx -sticky w
	}
	$tree style layout $S e_rect -union [list e_text_sel] -iexpand nswe -ipadx 2

	$tree style layout $S e_selmarker_up -detach 1
	    
	if { [package vcompare [package present treectrl] 2.1] >= 0 } {
	    $tree style layout $S e_selmarker_up -height 2 -sticky nwe -iexpand x
	}
	$tree style layout $S e_selmarker_down -detach 1 \
	    -expand n
	if { [package vcompare [package present treectrl] 2.1] >= 0 } {
	    $tree style layout $S e_selmarker_down -height 2 -sticky swe -iexpand x
	}


################################################################################
#    style text
################################################################################

	set S [$tree style create text -orient horizontal]
	$tree style elements $S [list e_rect e_text_sel \
		e_selmarker_up e_selmarker_down]
	$tree style layout $S e_text_sel -padx {2 0} -squeeze x -expand ns \
	    -iexpand ns
	if { [package vcompare [package present treectrl] 2.1] >= 0 } {
	    $tree style layout $S e_text_sel -iexpand nsx -sticky w
	}
	$tree style layout $S e_rect -union [list e_text_sel] -iexpand nswe -ipadx 2

	$tree style layout $S e_selmarker_up -detach 1           
	if { [package vcompare [package present treectrl] 2.1] >= 0 } {
	    $tree style layout $S e_selmarker_up -height 2 -sticky nwe -iexpand x
	}
	$tree style layout $S e_selmarker_down -detach 1 \
	    -expand n
	if { [package vcompare [package present treectrl] 2.1] >= 0 } {
	    $tree style layout $S e_selmarker_down -height 2 -sticky swe -iexpand x
	}
 
################################################################################
#    style text_r
################################################################################

	set S [$tree style create text_r -orient horizontal]
	$tree style elements $S [list e_rect e_text_sel \
		e_selmarker_up e_selmarker_down]
	$tree style layout $S e_text_sel -padx {2 0} -squeeze x -expand ns \
	    -iexpand ns
	if { [package vcompare [package present treectrl] 2.1] >= 0 } {
	    $tree style layout $S e_text_sel -iexpand nsx -sticky e
	}
	
	$tree style layout $S e_rect -union [list e_text_sel] -iexpand nswe -ipadx 2
 
	$tree style layout $S e_selmarker_up -detach 1           
	if { [package vcompare [package present treectrl] 2.1] >= 0 } {
	    $tree style layout $S e_selmarker_up -height 2 -sticky nwe -iexpand x
	}
	$tree style layout $S e_selmarker_down -detach 1 \
	    -expand n
	if { [package vcompare [package present treectrl] 2.1] >= 0 } {
	    $tree style layout $S e_selmarker_down -height 2 -sticky swe -iexpand x
	}
     
################################################################################
#    style image
################################################################################

	set S [$tree style create image -orient horizontal]
	$tree style elements $S [list e_rect e_image e_text_sel e_hidden \
		e_selmarker_up e_selmarker_down]
	$tree style layout $S e_image -expand ns
	$tree style layout $S e_text_sel -padx 0 -squeeze x -expand ns
	$tree style layout $S e_rect -union [list e_image] -iexpand nswe -ipadx 2
	$tree style layout $S e_hidden -padx 0 -squeeze x -expand nswe

	$tree style layout $S e_selmarker_up -detach 1           
	if { [package vcompare [package present treectrl] 2.1] >= 0 } {
	    $tree style layout $S e_selmarker_up -height 2 -sticky nwe -iexpand x
	}
	$tree style layout $S e_selmarker_down -detach 1 \
	    -expand n
	if { [package vcompare [package present treectrl] 2.1] >= 0 } {
	    $tree style layout $S e_selmarker_down -height 2 -sticky swe -iexpand x
	}

################################################################################
#    style window
################################################################################

	set S [$tree style create window -orient horizontal]
	$tree style elements $S [list e_rect \
		e_selmarker_up e_selmarker_down]
	if { [package vcompare [package present treectrl] 2.1] >= 0 } {
	    $tree style elements $S [list e_rect e_window \
		e_selmarker_up e_selmarker_down]
	    #$tree style layout $S e_window -iexpand xynsew -sticky nsew -squeeze xy -padx 0 -pady 0
	    $tree style layout $S e_window -iexpand xy -sticky nsew -squeeze xy -padx 0 -pady 1
	    $tree style layout $S e_rect -union [list e_window] -iexpand nswe -ipadx 2
	} else {
	    $tree style layout $S e_rect -iexpand nswe -ipadx 2
	}
 
	$tree style layout $S e_selmarker_up -detach 1           
	if { [package vcompare [package present treectrl] 2.1] >= 0 } {
	    $tree style layout $S e_selmarker_up -height 2 -sticky nwe -iexpand x
	}
	$tree style layout $S e_selmarker_down -detach 1 \
	    -expand n
	if { [package vcompare [package present treectrl] 2.1] >= 0 } {
	    $tree style layout $S e_selmarker_down -height 2 -sticky swe -iexpand x
	}
       
################################################################################
#    style full_disabled
################################################################################

	set S [$tree style create full_disabled -orient horizontal]
	$tree style elements $S [list e_rect_disabled \
		e_selmarker_up e_selmarker_down]
	$tree style layout $S e_rect_disabled -iexpand xy -ipadx 2 -ipady 2
	
	ttk::style layout TSizegripWhite [ttk::style layout TSizegrip]
	ttk::style configure TSizegripWhite -background white
 
	$tree style layout $S e_selmarker_up -detach 1           
	if { [package vcompare [package present treectrl] 2.1] >= 0 } {
	    $tree style layout $S e_selmarker_up -height 2 -sticky nwe -iexpand x
	}
	$tree style layout $S e_selmarker_down -detach 1 \
	    -expand n
	if { [package vcompare [package present treectrl] 2.1] >= 0 } {
	    $tree style layout $S e_selmarker_down -height 2 -sticky swe -iexpand x
	}

	$self createbindings
	$self configurelist $args

	return $win
    }
    onconfigure -selectioncolors { value } {
	set options(-selectioncolors) $value
	
	set colList [$tree element cget e_text_sel -fill]
	foreach ipos [lsearch -all -regexp $colList {selected}] {
	    lset colList [expr {$ipos-1}] [lindex $value 1]
	}
	$tree element configure e_text_sel -fill $colList
	set colList [$tree element cget e_rect -fill]
	foreach ipos [lsearch -all -regexp $colList {selected}] {
	    lset colList [expr {$ipos-1}] [lindex $value 0]
	}
	$tree element configure e_rect -fill $colList
    }
    onconfigure -have_vscrollbar { value } {
	set options(-have_vscrollbar) $value

	if { $options(-have_vscrollbar) == 0 } {
	    grid remove $win.sv
	} elseif { $options(-have_vscrollbar) == 1 } {
	    grid $win.sv -row 2 -column 1 -sticky ns
	} else {
	    grid $win.sv -row 2 -column 1 -sticky ns
	    autoscroll::autoscroll $win.sv
	}
    }
    onconfigure -have_search_button { value } {
	set options(-have_search_button) $value

	if { $options(-have_search_button) != 0 } {
	    set searchbutton [button $win.search -image search13x14 -bd 0 -highlightthickness 0 -padx 0 \
		    -command [mymethod toggle_search_label]]
	    bindtags $win.search [lreplace [bindtags $win.search] 2 2]
#             bind $win.search <ButtonPress-1> "[list %W state pressed]; break"
#             bind $win.search <ButtonRelease-1> "[bind [winfo class $win.search] <ButtonRelease-1>]; break"
	    set info [grid info $win.sv]
	    
	    tooltip::tooltip $win.search [_ "Search in tree"]
	    
	    grid $win.t -row 2 -column 0 -rowspan 2
	    grid $searchbutton -row 2 -column 1 -rowspan 1
	    grid $win.sv -row 3 -column 1 -sticky nse
	    grid $win.sh -row 4 -column 0
	    grid rowconfigure $win 0 -weight 0
	    grid rowconfigure $win 1 -weight 0
	    grid rowconfigure $win 2 -weight 0
	    grid rowconfigure $win 3 -weight 1

	    if { $info eq "" } {
		grid remove $searchbutton
	    } else {
		bind $win.sv <Configure> [mymethod chech_grid_searchbutton]
		bind $win.sv <Unmap> [mymethod chech_grid_searchbutton]
	    }
	} elseif { [info exists searchbutton] } {
	    destroy $searchbutton
	    unset searchbutton
	    
	    grid $win.t -row 2 -column 0 -rowspan 1
	    grid $win.sv -row 2 -column 1
	    grid $win.sh -row 3 -column 0
	    grid rowconfigure $win 0 -weight 0
	    grid rowconfigure $win 1 -weight 0
	    grid rowconfigure $win 2 -weight 1
	    grid rowconfigure $win 3 -weight 0
	}
    }
    method set_selecthandler_active { v } {
	set ret $selecthandler_active
	set selecthandler_active $v
	return $ret
    }
    method get_selecthandler_active {} {
	return $selecthandler_active
    }
    method chech_grid_searchbutton {} {
	set info [grid info $win.sv]
	if { $info eq "" } {
	    grid remove $searchbutton
	}
    }
    method hasfocus {} {
	if { [focus -lastfor $tree] eq $tree } {
	    return 1
	} else {
	    return 0
	}
    }
    method focus { args } {
	
	set optional {
	    { -only_if_win w "" }
	}
	set compulsory ""
	parse_args $optional $compulsory $args

	if { $only_if_win ne "" && $only_if_win ne $win } { return }
	if { [string match $win.* [focus]] } { return }
	if { [winfo exists $tree] } {
	    focus $tree
	}
    }
    method givetreectrl {} {
	return $tree
    }
    method createbindings {} {
	
	bindtags $tree [list $win $tree FullTreeCtrl TreeCtrl [winfo toplevel $tree] all]

	bind TreeCtrl <KeyPress-Left> {
	    #TreeCtrl::SetActiveItem %W [TreeCtrl::LeftRight %W -1]
	    %W collapse [%W index active]
	    if { [%W index "active parent"] != 0 } {
		%W activate "active parent"
		%W selection clear all
		%W selection add active
		%W selection anchor active
	    }
	}
	bind TreeCtrl <KeyPress-Right> {
	    #TreeCtrl::SetActiveItem %W [TreeCtrl::LeftRight %W 1]
	    if { [%W item numchildren [%W index active]] } {
		%W expand [%W index active]
	    }
	}
#         bind TreeCtrl <Double-ButtonPress-1> {+
#             set id [%W identify %x %y]
#             if {[lindex $id 0] eq "item"} {
#                 %W toggle [%W index active]
#             }
#         }

	bind $tree <Motion> [mymethod manage_popup %x %y]
	bind $tree <Leave> [mymethod manage_popup %x %y]

	$tree notify bind $vscrollbar <Scroll-y> { %W set %l %u }
	bind $vscrollbar <ButtonPress-1> [list focus $tree]

	$tree notify bind $hscrollbar <Scroll-x> { %W set %l %u }
	bind $hscrollbar <ButtonPress-1> [list focus $tree]

	$tree notify install event Header
	$tree notify install detail Header invoke

	$tree notify install event Drag
	$tree notify install detail Drag begin
	$tree notify install detail Drag end
	$tree notify install detail Drag receive

	$tree notify install event Edit
	$tree notify install detail Edit accept
	$tree notify install detail Edit end

	#$tree notify install event ActiveItem
   

#         $tree notify bind DontDelete <Selection> [mymethod select]
#         $tree notify bind DontDelete <Selection> {
#             if {%c == 1} {
#                 set selection [%T selection get]
#                 #xmlwidget::DisplayNode %T [lindex $selection 0]
#             }
#         }

	bind $tree <KeyPress> [mymethod keypress %K %A]
	bind $tree <Return> "[mymethod execute_select_return]; break"
	bind $tree <space> "[mymethod execute_select_return]; break"
	
	foreach i [list <ButtonPress-1> <Control-ButtonPress-1> <Shift-ButtonPress-1>] {
	    set cmd "[mymethod manage_popup %x %y]"
	    if { $i eq "<ButtonPress-1>" } {
		append cmd "; [mymethod execute_select_press %x %y]; break"
	    } else {
		append cmd "; [bind FullTreeCtrl $i]"
	    }
	    bind $tree $i $cmd
	}
	bind $tree <ButtonRelease-1> "[bind FullTreeCtrl <ButtonRelease-1>];
		[mymethod execute_select %x %y] ; break"
	bind $tree <<ContextualPress>> "[mymethod popup_help_cancel] ; [mymethod contextual %x %y %X %Y]"
	bind $tree <Double-ButtonRelease-1> [mymethod execute_select_double1 %x %y]

	bind $tree <F2> {
	    if { [[winfo parent %W] is_special_folder active] } { return }
	    set self [winfo parent %W]
	    $self edit_item active
	}
	bind $tree <Delete> {
	     if { [[winfo parent %W] is_special_folder active] } { return }
	    set ch [[winfo parent %W] cget -deletehandler]
	    if { $ch ne "" } {
		set ids [%W selection get]
		uplevel #0 $ch [list [winfo parent %W] $ids]
	    }
	}
	bind $tree <FocusIn> {
	    if { [llength [%W selection get]] == 0 && [%W index last] != 0 } {
		catch {
		    %W selection add "root firstchild visible"
		    %W selection anchor "root firstchild visible"
		    %W activate "root firstchild visible"
		}
	    }
	}
	bind $tree <Control-c> [mymethod clipboard_copy]

	$tree notify bind $tree <Edit-accept> { 
	    set self [winfo parent %W]
	    $self accept_edit_item [%T index %I] %C %t
	}
	$tree notify bind $tree <Edit-end> { 
	    set self [winfo parent %W]
	    $self end_edit_item [%T index %I] %C
	}
	$tree notify bind $tree <Drag-receive> {
	    set self [winfo parent %W]
	    $self end_drop %I %l %x %y
	}
	$tree notify bind $tree <ActiveItem> {
	    set self [winfo parent %W]
	    after idle [list catch [list $self active_item_changed %c]]
	}

	set sortcolumn ""
	$tree notify bind $tree <Header-invoke> [mymethod header_invoke %C]


#         bind $tree <Delete> {
#             set id [%W selection get]
#             if { $id == "" } { return }
#             #xmlwidget::AddxmlElement %W $id "" delete
#         }
	#bind $tree <<Cut>> [list xmlwidget::CutOrCopy $tree cut]
	#bind $tree <<Copy>> [list xmlwidget::CutOrCopy $tree copy]

    }
    onconfigure -compass_background {value} {
	set options(-compass_background) $value

	if { [package vcompare [package present treectrl] 2.1] < 0 } { return }
	
	# not used any more by now
	return

	if { $value } {
	    if { [lsearch -exact [image names] fulltktree::compass_background] == -1 } {
		image create photo fulltktree::compass_background -file \
		    [file join $::fulltktree_top::topdir compas_logo_small.gif]
	    }
	    $tree configure -backgroundimage fulltktree::compass_background
	} else {
	    $tree configure -backgroundimage ""
	}
    }
    onconfigure -selectmode {value} {
	set options(-selectmode) $value
	$tree configure -selectmode $value

#         if { $value eq "single" || $value eq "browse" } {
#             if { [package vcompare [package present treectrl] 2.1] >= 0 } {
#                 $tree style layout item e_text_sel -iexpand nsx
#             } else {
#                 $tree style layout item e_text_sel -iexpand ns
#             }
#             if { [package vcompare [package present treectrl] 2.1] >= 0 } {
#                 $tree style layout folder e_text_sel -iexpand nsx
#             } else {
#                 $tree style layout folder e_text_sel -iexpand ns
#             }
#             if { [package vcompare [package present treectrl] 2.1] >= 0 } {
#                 $tree style layout text e_text_sel -iexpand nsx
#             } else {
#                 $tree style layout text e_text_sel -iexpand ns
#             }
#         } else {
#             $tree style layout item e_text_sel -iexpand ns
#             $tree style layout folder e_text_sel -iexpand ns
#             $tree style layout text e_text_sel -iexpand ns
#         }
	$self _apply_columns_values
    }
    onconfigure -expand {value} {
	set options(-expand) $value

	if { $options(-expand) } {
	    grid remove $win.sh
	} else {
	    grid $win.sh
	}
	
	set w0 [font measure [$self cget -font] 0]
	set idx 0
	foreach col $options(-columns) {
	    lassign $col width name justify type is_editable expand
	    set wi [expr {round($w0*$width)}]
	    if { $expand == 1 || ($options(-expand) && $expand != 0) } {
		$tree column configure c$idx -minwidth $wi \
		    -width "" -expand 1 -squeeze 1 -weight $wi
	    } else {
		$tree column configure c$idx -width $wi \
		    -expand 0 -squeeze 0
	    }
	    incr idx
	}
    }
    onconfigure -columns {value} {
	set options(-columns) $value

	set err [catch { $tree column delete all }]
	if { $err } {
	    # old version
	    set n [$tree numcolumns]
	    for { set i 0 } { $i < $n } { incr i } {
		$tree column delete 0
	    }
	}
	set background white
	catch { set background [cu::give_widget_background [winfo parent $win]] }
	
	set w0 [font measure [$self cget -font] 0]
	set itemStyle ""
	set idx 0
	foreach col $options(-columns) {
	    lassign $col width name justify type is_editable expand
	    set wi [expr {round($w0*$width)}]
	    $tree column create -text $name -width $wi \
		-tag c$idx -justify $justify -borderwidth 1 \
		-background $background
	    if { $expand == 1 || ($options(-expand) && $expand != 0) } {
		$tree column configure c$idx -minwidth $wi \
		    -width "" -expand 1 -squeeze 1 -weight $wi
	    }
	    if { $type eq "text" && $justify eq "right" } {
		set type text_r
		lset options(-columns) $idx 3 $type
	    }
	    lappend itemStyle $idx $type
	    incr idx
	}
	$tree column configure tail -borderwidth 1 -background $background
	$tree configure -treecolumn 0

	$self _apply_columns_values
    }
    onconfigure -editaccepthandler {value} {
	set options(-editaccepthandler) $value
	$self _apply_columns_values
    }
    onconfigure -draghandler {value} {
	set options(-draghandler) $value
	$self _apply_columns_values
    }
    onconfigure -sensitive_cols {value} {
	set options(-sensitive_cols) $value
	$self _apply_columns_values
    }
    onconfigure -item_image {value} {
	set options(-item_image) $value
	$tree element configure e_item_image -image $value
    }
    onconfigure -folder_image {value} {
	set options(-folder_image) $value
	$tree element configure e_folder_image -image $value
    }
    onconfigure -has_sizegrip {value} {
	set options(-has_sizegrip) $value
	
	if { $options(-has_sizegrip) } {
	    ttk::sizegrip $win.grip -style TSizegripWhite
	    place $win.grip -relx 1 -rely 1 -anchor se
	    if { $options(-have_vscrollbar) != 0 } {
		grid configure $vscrollbar -pady "0 15"
		bind $vscrollbar <Map> +[list after 100 [list catch [list raise $win.grip]]]
	    }
	    bind $win.grip <ButtonPress-1> +[mymethod _move_sizegrip start $win.grip]
	    bind $win.grip <ButtonRelease-1> +[mymethod _move_sizegrip end $win.grip]
	    bind $win.grip <B1-Motion> +[mymethod _move_sizegrip motion $win.grip %X %Y]
	}
    }
    onconfigure -arrow_image_expand {value} {
	set options(-arrow_image_expand) $value
	
	if { $options(-arrow_image_expand)} {
		set err [catch {
		$tree configure -openbuttonimage mac-collapse \
		    -closedbuttonimage mac-expand
	    }]
	    if { $err } {
		$tree configure -buttonimage [list mac-collapse open mac-expand !open ]
	    }
	} else {
	    set err [catch {
	    $tree configure -openbuttonimage "" \
		    -closedbuttonimage ""
	    }]
	    if { $err } {
		$tree configure -buttonimage [list "" open "" !open ]
	    }
	}
    }
    method give_top_frame { args } {

	set optional {
	    { -if_exists "" 0 }
	}
	set compulsory ""
	parse_args $optional $compulsory $args

	if { ![winfo exists $win.topframe] } {
	    if { $if_exists } {
		return ""
	    }
	    grid [ttk::frame $win.topframe] -row 0 -column 0 -sticky nsew
	}
	return $win.topframe
    }
    variable check_scroll_idle_after ""
    method check_scroll_idle {} {
	after cancel $check_scroll_idle_after
	set check_scroll_idle_after [after idle [list catch [mymethod check_scroll]]]
    }
    method check_scroll {} {
	
	after cancel $check_scroll_idle_after
	set check_scroll_idle_after ""
	
	if { [winfo height $tree] == 1 } { return }
	
	lassign [$tree yview] f0 f1
	if { $f0 == $f1 } { return }
	if { $f0 != 0 || $f1 != 1 } {
	    #$tree yview moveto 0
	    if { [info exists searchbutton] } {
		grid $searchbutton -row 2 -column 1
		grid $win.sv -row 3 -column 1 -sticky nsw
	    } else {
		grid $win.sv -row 2 -column 1
	    }
	    set last_end_item [$tree index end]
	} elseif { [$tree index end] ne $last_end_item } {
#             if { [info exists searchbutton] } {
#                 grid remove $searchbutton
#             }
	    #grid remove $win.sv
	    set last_end_item [$tree index end]
	}
    }

    method _move_sizegrip { what w args } {
	switch $what {
	    start {
		$tree notify bind $vscrollbar <Scroll-y> ""
	    }
	    motion {
		lassign $args X Y
		set toplevel [winfo toplevel $tree]
		set x [winfo rootx $toplevel]
		set y [winfo rooty $toplevel]
		if { $X < $x || $Y < $y } {
		    if { $X < $x } { set x $X }
		    if { $Y < $y } { set y $Y }
		    wm geometry $toplevel +$x+$y
		    ttk::sizegrip::Press $w $X $Y
		}
	    }
	    end {
		$tree notify bind $vscrollbar <Scroll-y> { %W set %l %u }
		if { [lindex [$tree yview] 0] != 0 || [lindex [$tree yview] 1] != 1 } {
		    $vscrollbar set {*}[$tree yview]
		}
	    }
	}
    }
    variable manage_popup_id
    variable manage_popup_active 1
    method manage_popup { x y } {
	if { !$manage_popup_active } { return }

	if { [info exists manage_popup_id] } {
	    after cancel $manage_popup_id
	    unset -nocomplain manage_popup_id
	}
	set identify [$tree identify $x $y]
	lassign $identify NN item CN column EN elem
	if { $NN eq "item" && ($CN ne "column" || $EN ne "elem") } {
	    return
	} elseif { $NN ni "item header" } {
	    return
	}
	set manage_popup_id [after 600 [list catch [mymethod popup_help $identify]]]
    }
    method add_state { state color } {
	
	$tree state define $state
	set colorList [linsert [$tree element cget e_text_sel -fill] 0 $color $state]
	$tree element configure e_text_sel -fill $colorList
    }
    method popup_enter_help { args } {
	
	set optional {
	    { -command commandList "" }
	}
	set compulsory "item text"
	parse_args $optional $compulsory $args

	dict set popup_help_dict $item text $text
	if { $command ne "" } {
	    dict set popup_help_dict $item command $command
	}
    }
    method popup_help_deactivate {} {
	if { [info exists manage_popup_id] } {
	    after cancel $manage_popup_id
	    unset -nocomplain manage_popup_id
	}
	set manage_popup_active 0
    }
    method popup_help_reactivate {} {
	set manage_popup_active 1
    }
    method popup_help_cancel {} {
       
	if { !$manage_popup_active } { return }
	
	if { [info exists manage_popup_id] } {
	    after cancel $manage_popup_id
	    unset -nocomplain manage_popup_id
	}
	$self popup_help_deactivate
	after 600 [mymethod popup_help_reactivate]
    }
    method popup_help { identify } {
	unset -nocomplain manage_popup_id

	lassign $identify NN id CN column EN elem

	set grab [grab current $tree]
	if { $grab ne "" } {
	    if { [string match "$tree.*" $grab] } {
		return
	    }
	    if { [winfo toplevel $grab] ne [winfo toplevel $self] } {
		return
	    }
	}
	if { $NN eq "item" } {
	    if { [$tree item id $id] eq "" } { return }
	    
	    catch { lassign [$tree item bbox $id $column $elem] x1 y1 x2 y2 }
	    if { ![info exists x1] } { return }
	    #if { $x1 >= 0 && $x2 < [winfo width $tree] } { return }
	    
	    if { [dict exists $popup_help_dict $id command] } {
		set w [uplevel #0 [dict get $popup_help_dict $id command] [list $id [dict get $popup_help_dict $id text]]]
		if { [winfo exists $w] } {
		    set manage_popup_active 0
		    bind $w <Destroy> +[list set [varname manage_popup_active] 1]
		}
		return
	    } elseif { [dict exists $popup_help_dict $id text] } {
		set text [dict get $popup_help_dict $id text]
	    } else {
		set err [catch { $tree item element cget $id $column $elem -text } text]
		if { $err } { return }
		set width [font measure [$tree cget -font] $text]
		if { $x1 >= 0 && $x1+$width <= [winfo width $tree] } { return }
	    }
	} else {
	    catch { lassign [$tree column bbox $id] x1 y1 x2 y2 }
	    if { ![info exists x1] } { return }
	    set err [catch { $tree column cget $id -text } text]
	    if { $err } { return }
	    set width [font measure [$tree cget -font] $text]
	    if { $x1 >= 0 && $width <= $x2-$x1 } { return }
	}
	set x [expr {[winfo pointerx $tree]+15}]
	set y [expr {[winfo pointery $tree]+0}]

	set manage_popup_active 0
	set w [$self popup_help_do $tree $text $x $y]
	bind $w <Destroy> [list set [varname manage_popup_active] 1]
    }
    method _popup_help_activate_motion { l } {
	if { ![winfo exists $l] } { return }
	set w [winfo toplevel $l]
	lassign [winfo pointerxy $l] x y
	bind $l <Motion> [mymethod _popup_help_has_motion $l $x $y %X %Y]
    }
    method _popup_help_has_motion { l x_old y_old x y } {
	set d [expr {sqrt(($x-$x_old)**2+($y-$y_old)**2)}]
	if { $d < 3 } { return }
	set w [winfo toplevel $l]
	destroy $w
    }
    method _popup_help_button_event { w event X Y } {
	destroy $w
	if { $event ne "" } {
	    set win [winfo containing $X $Y]
	    if { $win eq "" } { return }
	    set x [expr {$X-[winfo rootx $win]}]
	    set y [expr {$Y-[winfo rooty $win]}]
	    event generate $win $event -x $x -y $y -rootx $X -rooty $Y    
	}
    }
    method is_popup_help_active { wp } {
	return [winfo exists $wp.tooltip]
    }
    method popup_help_do { wp text x y { minimum_time 0 } } {
	destroy $wp.tooltip
	
	set w [toplevel $wp.tooltip -bg black]
	
	label $w.l -text $text -highlightthickness 1 -bg white -fg black \
	    -justify left -wraplength 300
	pack $w.l -padx 1 -pady 1
	#focus $w.l
	grab $w.l
	incr y 24
	
	if { $y+[winfo reqheight $w.l] > [winfo screenheight $wp]-40 } {
	    set y [expr {[winfo screenheight $wp]-[winfo reqheight $w.l]-40}]
	    if { $y < 0 } { set y 0 }
	}
	wm overrideredirect $w 1
	wm geometry $w +${x}+$y
	bind $w.l <ButtonPress-1> [mymethod _popup_help_button_event \
		$w <ButtonPress-1> %X %Y]
	bind $w.l <<ContextualPress>> [mymethod _popup_help_button_event \
		$w <<ContextualPress>> %X %Y]
	after $minimum_time [mymethod _popup_help_activate_motion $w.l]
	bind $w.l <KeyPress> [mymethod _popup_help_button_event \
		$w "" "" ""]
	
	bind $w.l <ButtonPress-4> "[mymethod _popup_help_button_event \
		$w "" "" ""];break"
	bind $w.l <ButtonPress-5> "[mymethod _popup_help_button_event \
		$w "" "" ""];break"
	bind $w.l <MouseWheel> "[mymethod _popup_help_button_event \
		$w "" "" ""];break"
	
	return $w
    }
    method selection { what args } {
#         if { $what eq "set" } {
#             set err [catch { $self _selection add {*}$args } ret]
#             return $ret
#         }
	return [$self _selection $what {*}$args]
    }
    method _apply_columns_values {} {

	set idx 0
	foreach "dragimage editable sensitive" [list "" "" ""] break
	foreach col $options(-columns) {
	    if { $options(-sensitive_cols) ne "" } {
		if { $options(-sensitive_cols) eq "all" || \
		    [lsearch -integer $options(-sensitive_cols) $idx] != -1 } {
		    set sensitive_off 0
		} else {
		    set sensitive_off 1
		}
	    } else {
		set sensitive_off 0
	    }
	    lassign $col width name justify type is_editable expand
	    if { $is_editable } { lappend editcolumns $idx }
	    switch $type {
		item {
		    if { $is_editable } {
		        lappend editable [list $idx $type e_text_sel]
		        lappend editable [list $idx folder e_text_sel]
		    }
		    if { !$sensitive_off } {
		        lappend sensitive [list $idx $type e_item_image e_text_sel \
		                e_selmarker_up e_selmarker_down]
		        lappend sensitive [list $idx folder e_folder_image e_text_sel \
		                e_selmarker_up e_selmarker_down]
		        if { $options(-selectmode) ne "single" && $options(-selectmode) ne "browse" } {
		            set sensitive_off 1
		        }
		    }
		    if { $options(-draghandler) ne "" } {
		        lappend dragimage [list $idx $type e_item_image e_text_sel]
		        lappend dragimage [list $idx folder e_folder_image e_text_sel]
		    }
		}
		imagetext {
		    if { $is_editable } {
		        lappend editable [list $idx $type e_text_sel]
		    }
		    if { !$sensitive_off } {
		        lappend sensitive [list $idx $type e_image e_text_sel]
		        if { $idx == 0 } {
		            lappend sensitive [list $idx folder e_folder_image e_text_sel \
		                    e_selmarker_up e_selmarker_down]   
		        }
		        if { $options(-selectmode) ne "single" && $options(-selectmode) ne "browse" } {
		            set sensitive_off 1
		        }
		    }
		    if { $options(-draghandler) ne "" } {
		        lappend dragimage [list $idx $type e_image e_text_sel]
		    }
		}
		text - text_r {
		    if { $is_editable } {
		        lappend editable [list $idx $type e_text_sel]
		    }
		    if { !$sensitive_off } {
		        lappend sensitive [list $idx $type e_text_sel]
		        if { $idx == 0 } {
		            lappend sensitive [list $idx folder e_folder_image e_text_sel \
		                    e_selmarker_up e_selmarker_down]   
		        }
		        if { $options(-selectmode) ne "single" && $options(-selectmode) ne "browse" } {
		            set sensitive_off 1
		        }
		    }
		    if { $options(-draghandler) ne "" } {
		        lappend dragimage [list $idx $type e_text_sel]
		    }
		}
		image {
		    if { $is_editable } {
		        lappend editable [list $idx $type e_image]
		    }
		    if { !$sensitive_off } {
		        lappend sensitive [list $idx $type e_image]
		    }
		    if { $options(-draghandler) ne "" } {
		        lappend dragimage [list $idx $type e_image]
		    }
		}
	    }
	    incr idx
	}
	TreeCtrl::SetEditable $tree $editable
	TreeCtrl::SetDragImage $tree $dragimage
	TreeCtrl::SetSensitive $tree $sensitive
    }
   method insert { index list { parent_or_sibling root } } {

	set item [$tree item create]
	$tree item style set $item {*}$itemStyle

	set desc ""
	set idx 0
	foreach col $options(-columns) {
	    lassign $col - - justify type is_editable expand
	    switch $type {
		imagetext {
		    lassign [lindex $list $idx] img txt
		    # dirty trick to avoid a crash in tktreectrl
		    if { $txt eq "" } { set txt " " }
		    
		    switch [llength $img] {
		        0 { set fimg "" }
		        1 {
		            set img [lindex $img 0]
		            set selimg ${img}::selected
		            if { [info commands $selimg] eq "" } {
		                image create photo $selimg
		                $selimg copy $img
		                imagetint $selimg $fulltktree::SystemHighlight 128
		            }
		            set fimg [list $selimg {selected} $img {}]
		        }
		        default {
		            set fimg $img
		        }
		    }
		    lappend desc [list [list e_image -image $fimg] \
		            [list e_text_sel -text $txt -justify $justify]]
		}
		text -  text_r - item {
		    set txt [lindex $list $idx]
		    if { $txt eq "" } { set txt " " }
		    lappend desc [list [list e_text_sel -text $txt -justify $justify]]
		}
		image {
		    lassign  [lindex $list $idx] img txt
		    if { $txt eq "" } { set txt " " }
		    lappend desc [list [list e_image -image $img] \
		            [list e_hidden -data $txt]]
		}
	    }
	    incr idx
	}
	$tree item complex $item {*}$desc
	switch $index {
	    end - child {
		$tree item lastchild $parent_or_sibling $item
	    }
	    prev {
		$tree item prevsibling $parent_or_sibling $item
	    }
	    next {
		$tree item nextsibling $parent_or_sibling $item
	    }
	    default {
		error "error: index can be: end (or child), prev or next"
	    }
	}
	if { ($index eq "end" || $index eq "child") && [$tree index $parent_or_sibling] != 0 } {
	    set ipos -1
	    foreach style [list item imagetext text text_r] {
		set ipos [lsearch -exact $itemStyle $style]
		if { $ipos != -1 } {
		    break
		}
	    }
	    if { $ipos != -1 } {
		set col [lindex $itemStyle [expr {$ipos-1}]]
		if { $style eq "imagetext" && [$tree item image $parent_or_sibling $col] ne "" } {
		    # if they have an image, do not get converted to folder style
#                     $tree item style map $parent_or_sibling $col folder \
#                         [list e_text_sel e_text_sel e_image e_folder_image]
		} elseif { "e_text_sel" in [$tree item style elements $parent_or_sibling $col] } {
		    $tree item style map $parent_or_sibling $col folder \
		        [list e_text_sel e_text_sel]
		}
	    }
	    $tree item configure $parent_or_sibling -button 1
	}
	#set ::TreeCtrl::Priv(DirCnt,$tree) $item
	set ::TreeCtrl::Priv(DirCnt,$tree) end

	$self check_scroll_idle
	return $item
    }
    method convert_to_folder { item } {
	set idx 0
	foreach col $options(-columns) {
	    set type [lindex $col 3]
	    if { $type eq "item" } { break }
	    incr idx
	}        
	$tree item style map $item $idx folder [list e_text_sel e_text_sel]
	$tree item collapse $item
	if { [llength [$tree item children $item]] } {
	    $tree item configure $item -button 1
	}
    }
    method is_folder { item } {
	set idx 0
	foreach col $options(-columns) {
	    set type [lindex $col 3]
	    if { $type eq "item" } { break }
	    incr idx
	}        
	if { $idx < [llength $options(-columns)] } {
	    if { [$tree item style set $item $idx] eq "folder" } {
		return 1
	    }
	}
	return 0
    }
    method sort_column { args } {
	set optional {}
	parse_args -compulsory_min 0 "" "column order" $args
	if { $column eq "" } {
	    if { $sortcolumn eq "" } {
		set order ""
	    } else {
		if {[$tree column cget $sortcolumn -arrow] eq "down"} {
		    set order decreasing
		} else {
		    set order increasing
		}
	    }
	    return [list $sortcolumn $order]
	} else {
	    if { $sortcolumn ne "" } {
		$tree column configure $sortcolumn -arrow none -itembackground {}
	    }
	    set sortcolumn $column
	    switch $order {
		increasing { set arrow up }
		decreasing { set arrow down }
	    }
	    $tree column configure $sortcolumn -arrow $arrow -itembackground #F7F7F7
	    
	    set type [lindex $options(-columns) $sortcolumn 3]
	    $self sort_column_do root $type $order
	}
    }
    method sort_column_do { parent ptype order } {
	
	set first_last_opts ""
	set items [$self item children $parent]
	set len [llength $items]
	foreach first_last [list first last] {
	    switch $first_last {
		"first" {
		    set max [expr {($len>6)?5:$len-2}]
		    set range [rangeF $max 0 -1] }
		"last" { 
		    set min [expr {($len-6>0)?$len-6:1}]
		    if { $min < $max } {
		        set min $max
		    }
		    set range [range $min $len] }
	    }
	    foreach i $range {
		if { $i < 0 || $i >= $len } { continue }
		set item [lindex $items $i]
		set i_type [$self item style set $item $sortcolumn]
		if { $i_type eq $ptype } { continue }
		if { [string match *text* $i_type] && [string match *text* $ptype] } { continue }
		if { $i_type eq "folder" } { continue }
		switch $first_last {
		    "first" { set item [lindex $items [expr {$i+1}]] }
		    "last" { set item [lindex $items [expr {$i-1}]] }
		}
		lappend first_last_opts -$first_last $item
		break
	    }
	}
	set sort_type [lindex $options(-sort_type_cols) $sortcolumn]
	if { $sort_type eq "" } {
	    set sort_type dictionary
	}
	if { [llength $sort_type] == 2 && [lindex $sort_type 0] eq "command" } {                
	    $self item sort $parent -$order -column $sortcolumn -command [list [lindex $sort_type 1] \
		    $self $sortcolumn] {*}$first_last_opts         
	} else {
	    if { $ptype eq "image" } {
		$self item sort $parent -$order -column $sortcolumn -element e_hidden -$sort_type {*}$first_last_opts
	    } else {
		$self item sort $parent -$order -column $sortcolumn -$sort_type {*}$first_last_opts
	    }
	}
	if { $options(-recursive_sort) } {
	    foreach i [$tree item children $parent] {
		$self sort_column_do $i $ptype $order
	    }
	}
    }
    method header_invoke { col } {
	
	if { $sortcolumn ne "" && [$tree column id $sortcolumn] eq ""  } {
	    #sortcolumn is wrong
	    set sortcolumn ""
	}
	if { $sortcolumn eq "" } {
	    set are_equal 0
	} elseif { [package vcompare [package present treectrl] 2.1] >= 0 } {
	    set are_equal [$tree column compare $col == $sortcolumn]
	} else {
	    set are_equal [expr {$col == $sortcolumn}]
	}
	if { $sortcolumn eq "" } {
	    set order increasing
	    set arrow up
	    set sortcolumn $col
	} elseif { $are_equal } {
	    if {[$tree column cget $sortcolumn -arrow] eq "down"} {
		set order increasing
		set arrow up
	    } else {
		set order decreasing
		set arrow down
	    }
	} else {
	    if {[$tree column cget $sortcolumn -arrow] eq "down"} {
		set order decreasing
		set arrow down
	    } else {
		set order increasing
		set arrow up
	    }
	    $tree column configure $sortcolumn -arrow none -itembackground {}
	    set sortcolumn $col
	}
	$tree column configure $col -arrow $arrow -itembackground #F7F7F7

	set type [lindex $options(-columns) $sortcolumn 3]
	$self sort_column_do root $type $order
    }
    variable contextual_save
    method contextual { x y X Y } {
	lassign [$tree identify $x $y] type id

	focus $tree
	
	if { $options(-contextualhandler_menu) ne "" || $options(-contextualhandler_fmenu) ne "" } {
	    set contextual_save [list [$tree index active]]
	    set selecthandler_active 0
	} else {
	    set contextual_save ""
	}
	if { $type eq "item" && $id ne "" } {
	    lappend contextual_save [$tree selection get]
	    if { ![$tree selection includes $id] } {
		$tree selection clear all
		$tree selection add $id
	    }
	    $tree activate $id
	} else {
	    $tree selection clear all
	    lappend contextual_save ""   
	}
	if { $options(-contextualhandler_menu) ne "" || $options(-contextualhandler_fmenu) ne "" } {
	    $self popup_help_deactivate
	    update idletasks
	    set selecthandler_active 1
	    destroy $tree.menu
	    menu $tree.menu -tearoff 0
	    if { $options(-contextualhandler_fmenu) ne "" } {
		set ch $options(-contextualhandler_fmenu)   
		lappend ch $self $tree.menu $id [$tree selection get] $x $y
	    } else {
		set ch $options(-contextualhandler_menu)
		lappend ch $self $tree.menu $id [$tree selection get]
	    }
	    set ret [uplevel #0 $ch]

	    if { $::tcl_platform(platform) ne "windows" } {
		bind $tree.menu <Unmap> [mymethod contextual_end]
	    }

	    if { $ret ne "" && [string is digit $ret] } {
		tk_popup $tree.menu $X $Y $ret
	    } else {
		tk_popup $tree.menu $X $Y
	    }

	    if { $::tcl_platform(platform) eq "windows" } {
		$self contextual_end
	    }
	}
	set ch $options(-contextualhandler)
	if { $ch ne "" } {
	    uplevel #0 $ch [list $self [$tree selection get] $X $Y]
	}
    }
    method contextual_end {} {
	lassign $contextual_save active selection
	set selecthandler_active 0
	$tree activate $active
	$tree selection anchor $active
	
	$tree selection clear all
	foreach id $selection {
	    $tree selection add $id
	}
	set selecthandler_active 1
	$self popup_help_reactivate
    }
    method set_search_string { txt } {
	if { ![winfo exists $win.search_label] } {
	    $self toggle_search_label
	}
	set searchstring $txt
    }
    method is_search_label_open {} {
	if { ![winfo exists $win.search_label] } {
	    return 0
	} else {
	    return 1
	}
    }
    method open_search_label {} {
	if { ![winfo exists $win.search_label] } {
	    $self toggle_search_label
	}
    }
    method close_search_label {} {
	if { [winfo exists $win.search_label] } {
	    $self toggle_search_label
	}
    }
    method toggle_search_label { args } {
	
	set optional {
	    { -clear boolean 1 }
	}
	set compulsory ""
	parse_args $optional $compulsory $args

	if { [winfo exists $win.search_label] } {
	    if { $clear } { set searchstring "" }
	    $self search_text_label
	    destroy $win.search_label
	    focus $tree
	} else {
	    ttk::entry $win.search_label -textvariable [myvar searchstring]
	    grid $win.search_label -row 1 -column 0 -sticky ew -padx 2 -pady 2
	    
	    lower $win.search_label $tree
	    
	    focus $win.search_label
	    $win.search_label icursor end
	    bind $win.search_label <Escape> "[mymethod toggle_search_label]; break"
	    
	    if { $clear } { set searchstring "" }
	    $self search_text_label
	    
	    cu::add_contextual_menu_to_entry $win.search_label init [_ "Full words"] "" \
		[mymethod toggle_search_full_words]
	    
	    set cmd [mymethod search_text_label_idle]
	    trace add variable [myvar searchstring] write "$cmd ;#"
	    bind $win.search_label <Destroy> [list trace remove variable [myvar searchstring] \
		    write "$cmd ;#"]
	}
    }
    method toggle_search_full_words {} {
	if { $search_full_words } {
	    set search_full_words 0
	} else {
	    set search_full_words 1
	}
	$self search_text_label
    }
    variable search_text_after_id
    method keypress { key char } {
	if { $options(-spreadsheet_mode) } {
	    if { ![string is print -strict $char] } { return }
	    $self edit_item active "" $char
	} elseif { [winfo exists $win.search_label] || $options(-have_search_button) eq "automatic" } {
	    if { ![string is print -strict $char] && $key ne "BackSpace" } { return }
	    if { $key eq "BackSpace" } {
		set searchstring [string range $searchstring 0 end-1]
	    } else {
		append searchstring $char
	    }
	    if { ![winfo exists $win.search_label] } { $self toggle_search_label -clear 0 }
	} elseif { $options(-keypress_search_active) } {
	    if { ![string is print -strict $char] } { return }
	    $self search_text $char
	}
    }
    method search_text_label_idle {} {
	if { [info exists search_text_after_id] } {
	    after cancel $search_text_after_id
	}
	set search_text_after_id [after 500 [mymethod search_text_label]]
    }
    proc glob_map { args } {
	set map {
	    ? \\?
	    * \\*
	    \[ \\\[
	    \] \\\]
	    \\ \\\\
	    \{ \\\{
	    \} \\\}
	}
	return [string map $map [join $args ""]]
    }
    method search_text_label {} {

	if { [info exists search_text_after_id] } {
	    after cancel $search_text_after_id
	    unset search_text_after_id
	}
	set is_selected 0
	set selected0 [lindex [$tree selection get] 0]
	$tree selection clear all

	lassign [list "" and] wordList op
	foreach i [regexp -inline -all {"[^"]*"|\S+} $searchstring] {
	    set i [string trim $i {"}]
	    if { [string length $i] < 2 } { continue }
	    if { [string equal -nocase $i or] } {
		set op or
	    } elseif { [string equal -nocase $i and] } {
		set op and
	    } else {
		lappend wordList $i
	    }
	}
	if { [llength $wordList] == 0 && [string length $searchstring] > 0 } {
	    set wordList [list [string trim $searchstring]]
	}
	set num 0
	set item [$tree item id "first next"]
	while { $item ne "" } {
	    if { [$tree item parent $item] != 0 && [$tree item cget [$tree item parent $item] -visible] } {
		set found 1
	    } elseif { $searchstring eq "" } {
		set found 1
	    } else {
		set itemList [cu::string::compare_list [$tree item text $item]]
		set found 0
		foreach i $wordList {
		    set pattern [glob_map [cu::string::compare_string $i]]
		    if { !$search_full_words } {
		        set pattern "*$pattern*"
		    }
		    if { [lsearch -glob -nocase $itemList $pattern] != -1 } {
		        incr found
		        if { $op eq "or" } { break }
		    } else {
		        if { $op eq "and" } {
		            set found 0
		            break 
		        }
		    }
		}
	    }
	    if { $found } {
		$tree item configure $item -visible 1
		incr num
		foreach i [lrange [$tree item ancestors $item] 0 end-1] {
		    $tree item configure $i -visible 1
		    if { $searchstring ne "" } {
		        $tree item expand $i
		    }
		}
		if { !$is_selected || $item eq $selected0 } {
		    $tree activate $item
		    $tree selection clear all
		    $tree selection add $item
		    $tree selection anchor $item
		    set is_selected 1
		}
	    } else {
		$tree item configure $item -visible 0
	    }
	    set item [$tree item id "$item next"]
	}
	set item [lindex [$tree selection get] 0]
	if { $item ne "" } {
	    $tree see $item
	}
	
	if { $num>1 && [winfo exists $win.search_label] && [focus] ne "$win.search_label" } {
	    focus $win.search_label
	    $win.search_label icursor end
	} elseif { $num==1 } {
	    focus $tree
	}
    }
    method search_text { char } {
	
	if { [info exists search_text_after_id] } {
	    after cancel $search_text_after_id
	    unset search_text_after_id
	}
	if { [$tree index last] == 0 } { return }
	if { $char eq "\t" } { return }
	if { [string is wordchar -strict $char] || [string is punct -strict $char] \
		 || [string is space -strict $char] } {
	    if { ![info exists searchstring] || [string index $searchstring end] != $char } {
		append searchstring $char
	    }
	    if { [info exists searchstring_reached_end] && $searchstring_reached_end ne "" \
		&& $searchstring_reached_end eq $searchstring } {
		set id "first visible"
	    } elseif { [$tree compare active == "active bottom"] } {
		set id "first visible"
	    } else { set id "active below" }
	    set err [catch { $tree index $id } id]
	    if { $err } { return }
	    lassign "" id_found id_found2
	    while { $id ne "" } {
		set txt [$tree item text $id 0]
		if { [string match -nocase $searchstring* $txt] } {
		    set id_found $id
		    break
		} elseif { [string match -nocase *$searchstring* $txt] && $id_found2 eq "" } {
		    set id_found2 $id
		}
		set id [$tree index "$id below"]
	    }
	    if { $id_found eq "" && $id_found2 ne "" } {
		if { [string length $searchstring] < 2 } { return }
		set id_found $id_found2
	    }
	    if { $id_found eq "" } {
		bell
		set searchstring_reached_end $searchstring
		set searchstring ""
		after 300 [list set [varname searchstring_reached_end] ""]
	    } else {
		$tree activate $id_found
		$tree see $id_found
		$tree selection clear all
		$tree selection add active
		$tree selection anchor active
		set search_text_after_id [after 300 [list set [varname searchstring] ""]]
	    }
	}
    }
    method activate_select_item { id } {
	foreach i [$tree item ancestors $id] {
	    $tree expand $i
	}
	$tree see $id
	
	$tree activate $id
	$tree selection clear all
	$tree selection add $id
	$tree selection anchor active
	set ::TreeCtrl::Priv(prev) ""
    }
    method activate_select { name_path { col "" } } {

	if { $col eq "" } {
	    if { [package vcompare [package present treectrl] 2.1] >= 0 } {
		set range [range 0 [$tree column count]]
	    } else {
		set range [range 0 [$tree numcolumns]]
	    }
	} else { set range $col }
	foreach col $range {
	    foreach el [$tree item style elements 1 $col] {
		if { $el eq "e_text_sel" } { break }
	    }
	}
	set parent 0
	foreach i $name_path {
	    set found 0
	    foreach id [$tree item children $parent] {
		if { [$tree item text $id $col] eq $i } {
		    if { $i ne [lindex $name_path end] } {
		        $tree expand $id
		    }
		    set parent $id
		    set found 1
		    break
		}
	    }
	    if { !$found } {
		error "error: '$name_path' not found in tree"
	    }
	}
	$tree activate $id
	$tree selection clear all
	$tree selection add $id
	$tree selection anchor active
    }
    method _correct_xview { item col max_width } {
	$tree see $item
	lassign [$tree item bbox $item $col] x0 - x1 -
	if { $x0 eq "" } { return }
	if { $max_width ne "" && $x1-$x0 > $max_width } {
	    set x1 [expr {$x0+$max_width}]
	}
	if { $x0 < 0 || $x1 > [winfo width $tree] } {
	    set x0c [$tree canvasx $x0]
	    set tw [expr {[winfo width $tree]/([lindex [$tree xview] 1]-[lindex [$tree xview] 0])}]
	    $tree xview moveto [expr {$x0c/$tw}]
	    update
	}
    }
    method edit_item { item { col "" } { char "" } } {
	if { $options(-editaccepthandler) eq "" } { return 0 }

	if { [package vcompare [package present treectrl] 2.1] >= 0 } {
	    set nc [$tree column count]
	} else {
	    set nc [$tree numcolumns]
	}
	if { $col eq "" } {
	    set range [range 0 $nc]
	} elseif { [string match next* $col] } {
	    if { [lindex $col 1] ne "" } {
		set range [range [expr {[lindex $col 1]+1}] $nc]
	    } else {
		set range [range 0 $nc]
	    }
	} elseif { [string match prev* $col] } {
	    if { [lindex $col 1] ne "" } {
		set range [rangeF [expr {[lindex $col 1]-1}] 0 -1]
	    } else {
		set range [rangeF [expr {$nc-1}] 0 -1]
	    }
	} elseif { [string match above* $col] } {
	    set err [catch { $tree item id "$item above" } item]
	    if { $err } { return }
	    set range [lindex $col 1]
	} elseif { [string match below* $col] } {
	    set err [catch { $tree item id "$item below" } item]
	    if { $err || $item eq "" } { return }
	    set range [lindex $col 1]
	} else { set range $col }

	foreach icol $range {
	    if { [lsearch -integer $editcolumns $icol] == -1 } { continue }
	    if { [$tree column cget $icol -visible] == 0 } { continue }
	    if { [catch { $tree item style elements $item $icol }] } { continue }
	    foreach el [$tree item style elements $item $icol] {
		if { $el eq "e_text_sel" || ( [$tree item style set active $icol] eq "image" && $el eq "e_image") } {
		    if { $options(-editbeginhandler) ne "" } {
		        set ret [uplevel #0 $options(-editbeginhandler) \
		                [list $tree $item $icol]]
		        if { $ret == 0 } { break }
		    } else {
		        set ret 1
		    }
	       
		    $self _correct_xview $item $icol 30
		    
		    switch [lindex $ret 0] {
		        1 - entry {
		            set text "-"
		            if {[llength $ret] > 1 } {
		                set text [lindex $ret 1]
		            }
		            #set w [::TreeCtrl::EntryOpen $tree $item $icol e_text_sel]
		            set w [::TreeCtrl::EntryExpanderOpen $tree $item $icol e_text_sel $text]
		        }
		        text {
		            set text "-"
		            if {[llength $ret] > 1 } {
		                set text [lindex $ret 1]
		            }
		            set w [::TreeCtrl::TextExpanderOpen $tree $item $icol e_text_sel 500 $text]
		        }
		        combo {
		            lassign [lrange $ret 1 end] editable values dict
		            set w [::TreeCtrl::ComboOpen $tree $item $icol e_text_sel \
		                    $editable $values -NONE- $dict]
		        }
		        combotree {
		            foreach "editable values_tree" [lrange $ret 1 end] break
		            set w [::TreeCtrl::ComboTreeOpen $tree $item $icol e_text_sel \
		                    $editable $values_tree]
		        }
		        anywidget {
		            set widget [lindex $ret 1]
		            set w [::TreeCtrl::AnyWidgetOpen $tree $item $icol $el $widget]
		        }
		        default {
		            set w ""
		        }
		    }
		    if { $w ne "" } {
		        set max_width [winfo width $w]
		        $self _correct_xview $item $icol $max_width
		        
		        bind $w <KeyPress-Tab> "[mymethod edit_item_tab $item $w $icol next];break"
		        bind $w <<PrevWindow>> "[mymethod edit_item_tab $item $w $icol prev];break"
		        foreach i [list Right Left Up Down] {
		            bind $w <KeyPress-$i> [mymethod edit_item_tab_arrows $item $w $icol $i]
		        }
		        if { $char ne "" } {
		            switch [winfo class $w] {
		                Entry - TEntry {
		                    catch { ttk::entry::Insert $w $char }
		                }
		                Text {
		                    catch { tk::TextInsert $w $char }
		                }
		                default {
		                    catch { $w insert insert $char }
		                }
		            }
		        }
		    }
		    return 1
		}
	    }
	}
	if { [string match next* $col] } {
	    set item [$tree item id "$item below"]
	    if { $item ne "" } {
		$self activate $item
		return [$self edit_item $item "next"]
	    }
	}
	if { [string match prev* $col] } {
	    set item [$tree item id "$item above"]
	    if { $item ne "" } {
		return [$self edit_item $item "prev"]
	    }
	}
	return 0
    }
    method edit_item_tab_arrows { item w icol where } {
	switch $where {
	    Right {
		if { ![$w selection present] && [$w index insert] == [$w index end] } {
		    $self edit_item_tab $item $w $icol next
		    return -code break
		}
	    }
	    Left {
		if { [$w index insert] == [$w index end] && [$w selection present] && 
		    [$w index sel.last]-[$w index sel.first] == [$w index end] } {
		    $w selection clear
		    $w icursor 0
		    return -code break
		} elseif { ![$w selection present] && [$w index insert] == 0 } {
		    $self edit_item_tab $item $w $icol prev
		    return -code break
		}
	    }
	    Up {
		$self edit_item_tab $item $w $icol above
		return -code break
	    }
	    Down {
		$self edit_item_tab $item $w $icol below
		return -code break
	    }
	}
    }
    method edit_item_tab { item w icol where } {
	TreeCtrl::WidgetClose $tree 1
	#catch { focus $TreeCtrl::Priv(entry,$tree,focus) }
	after idle [mymethod edit_item $item [list $where $icol]]
    }
    method edit_item_cancel {} {
	TreeCtrl::WidgetClose $tree 0
    }
    method accept_edit_item { item col text } {
	
	set accept_edit_item_last_accepted [list $item $col]
	
	if { $options(-editaccepthandler) eq "" } { return }
	if { [lsearch -integer $editcolumns $col] == -1 } { return }
	uplevel #0 $options(-editaccepthandler) [list $tree $item $col $text]
    }
    method end_edit_item { item col } {
	
	if { $accept_edit_item_last_accepted eq [list $item $col] } {
	    set accept_edit_item_last_accepted ""
	    return
	}
	if { $options(-editrejecthandler) eq "" } { return }
	if { [lsearch -integer $editcolumns $col] == -1 } { return }
	uplevel #0 $options(-editrejecthandler) [list $tree $item $col]
    }
    method end_drop { receiving_item dragged_items x y } {
	if { $options(-draghandler) ne "" } {
	    lassign [$tree item bbox $receiving_item] - y1 - y2
	    if { $y < [expr {.667*$y1+.333*$y2}] } {
		set where prev
	    } elseif { $y < [expr {.333*$y1+.667*$y2}] } {
		set where center
	    } else {
		set where next
	    }
	    if { $where eq "next" && [$tree item numchildren $receiving_item] &&
		[$tree item state get $receiving_item open] } {
		set receiving_item [$tree item firstchild $receiving_item]
		set relative_pos prev
	    }
	    uplevel #0 $options(-draghandler) [list $tree \
		    $receiving_item $dragged_items $where]
	}
    }
    variable execute_select_pointer ""
    method execute_select_press { x y } {
	set TreeCtrl::Priv(selectMode) set
	TreeCtrl::FullTkTreeButton1 $tree $x $y
	set execute_select_pointer [list $x $y]
	
	set identify [$tree identify $x $y]
	if { $identify ne "" && $options(-button1presshandler) ne "" } {
	    set ids [$tree selection get]
	    uplevel #0 $options(-button1presshandler) [list $tree $ids $identify $x $y]
	}
    }
    variable execute_select_pressed ""
    method execute_select { { x "" } { y "" } } {

	set ids [$tree selection get]
	set id [lindex $ids 0]

	if { $id eq "" } { 
	    set identify [$tree identify $x $y]
	    if { [lindex $identify 0] == "item" } {
		set id [lindex $identify 1]
		$tree selection add $id
		set ids [$tree selection get]
		set id [lindex $ids 0]
	    }
	    if { $id eq "" } { 
		return 
	    }
	}
	
	if { [list $x $y] ne $execute_select_pointer } {
	    return
	}
	set identify ""
	if { [llength $ids] == 1 } {
	    if { $x ne "" } {
		set identify [$tree identify $x $y]
		if { [lindex $identify 0] ne "item" || [lindex $identify 2] ne "column" } {
		    return
		}
	    }
	    if { $identify ne "" && $execute_select_pressed == $id } {
		set execute_select_pressed ""
		set ch [$self cget -editaccepthandler]
		if { $ch ne "" } {
		    set col [lindex $identify 3]
		    set found 0
		    foreach elem [$tree item style elements active $col] {
		        if { $elem ne "e_hidden" && [$tree element type $elem] eq "text" } {
		            set found 1
		            break
		        }
		    }
		    if { !$found && [lindex $options(-columns) $col 4] } {
		        foreach elem [$tree item style elements active $col] {
		            if { [$tree element type $elem] eq "image" } {
		                set found 1
		                break
		            }
		        }
		    }
		    if { $found } {
		        lassign [$tree item bbox active $col $elem] x1 y1 x2 y2
		        if { [$tree element type $elem] eq "text" } {
		            set font [$tree item element perstate active $col $elem -font]
		            if {$font eq ""} { set font [$tree cget -font] }
		            set text [$tree item element cget active $col $elem -text]
		            set dx [font measure $font $text]
		        } else {
		            set dx [expr {$x2-$x1}]
		        }
		        if { $x >= $x1 && $x <= $x1+$dx } {
		            set ret [$self edit_item active $col]
		            if { $ret } { return }
		        }
		    }
		}
	    } elseif { ![$self is_special_folder $id] } {
		set execute_select_pressed $id
		after 900 [list catch [list set [varname execute_select_pressed] ""]]
	    }
	    set ret 1
	    if { $identify ne "" && $options(-button1handler) ne "" } {
		set ret [uplevel #0 $options(-button1handler) [list $tree $ids $identify $x $y]]
	    }
	    if { $ret != 0 && $options(-buttonpress_open_close) } {
		if { [info exists execute_select_after_after_id] } {
		    after cancel $execute_select_after_after_id
		    unset execute_select_after_after_id
		    return
		}
		set execute_select_after_after_id [after 400 [list catch [mymethod execute_select_after $id]]]
	    }
	}
	if { 0&& $options(-selecthandler2) ne "" } {
	    if { $identify eq "" } {
		uplevel #0 $options(-selecthandler2) [list $tree $ids]
	    }
	} elseif { $selecthandler_active && $options(-selecthandler) ne "" && $options(-selecthandler2) eq "" } {
	    uplevel #0 $options(-selecthandler) [list $tree $ids]
	}
    }
    variable execute_select_after_after_id
    method execute_select_after { id } {

	if { [info exists execute_select_after_after_id] } {
	    after cancel $execute_select_after_after_id
	    unset execute_select_after_after_id
	}
	set isopen [$tree item state get $id open]
	if { $isopen } {
	    $tree collapse -recurse $id
	} elseif { [$tree item numchildren $id] } {
	    $tree collapse all
	    foreach i [$tree item ancestors $id] { $tree expand $i }
	    if { [$tree item numchildren $id] } {
		$tree expand -recurse $id
	    }
	}
    }
    method active_item_changed { item } {
	
	if { [$tree item state get $item disabled] } {
	    set dir next
	    if { $last_active_item ne "" && $last_active_item > $item } {
		set dir prev
	    }
	    catch { TreeCtrl::SetActiveItem $tree "$item $dir visible" }
	    return
	}
	set ids [$tree selection get]
	if { $ids eq "" } {
	    set ids $item
	}
	if { $selecthandler_active && $options(-selecthandler) ne "" && $options(-selecthandler2) ne "" } {
	    uplevel #0 $options(-selecthandler) [list $tree $ids]
	}
	set last_active_item $item
    }
    method execute_select_return {} {
	set ids [$tree selection get]

	if { $options(-returnhandler) ne "" } {
	    uplevel #0 $options(-returnhandler) [list $tree $ids]
	} elseif { $options(-selecthandler2) ne "" } {
	    uplevel #0 $options(-selecthandler2) [list $tree $ids]
	} elseif { $selecthandler_active && $options(-selecthandler) ne "" } {
	    uplevel #0 $options(-selecthandler) [list $tree $ids]
	}
    }
    method execute_select_double1 { x y } {
	
	set id [$tree identify $x $y]
	if { [lindex $id 0] ne "item" || [lindex $id 2] ne "column"} { return }
	set ids [$tree selection get]
	set id0 [lindex $ids 0]
	if { $id0 eq "" } { return }
	
	set ch [$self cget -editaccepthandler]
	
	if { $options(-selecthandler2) ne "" } {
	    set execute_select_pressed ""
	    after idle $options(-selecthandler2) [list $tree $ids]
	    return
	} elseif { $ch ne "" } {
	    set ret [$self edit_item $id0 [dict get $id column]]
	    if { $ret } { return }
	}
	if { $selecthandler_active && $options(-selecthandler) ne "" } {
	    uplevel #0 $options(-selecthandler) [list $tree $ids]
	}
    }
    method give_item_path_text { name_path { col "" } } {

	if { $col eq "" } {
	    if { [package vcompare [package present treectrl] 2.1] >= 0 } {
		set range [range 0 [$tree column count]]
	    } else {
		set range [range 0 [$tree numcolumns]]
	    }
	} else { set range $col }
	foreach col $range {
	    foreach el [$tree item style elements 1 $col] {
		if { $el eq "e_text_sel" } { break }
	    }
	}
	set parent 0
	foreach i $name_path {
	    set found 0
	    foreach id [$tree item children $parent] {
		if { [$tree item text $id $col] eq $i } {
		    set parent $id
		    set found 1
		    break
		}
	    }
	    if { !$found } { return "" }
	}
	return $id
    }
    method item_path_text { id { col "" } } {

	if { $col eq "" } {
	    if { [package vcompare [package present treectrl] 2.1] >= 0 } {
		set range [range 0 [$tree column count]]
	    } else {
		set range [range 0 [$tree numcolumns]]
	    }
	} else { set range $col }
	foreach col $range {
	    foreach el [$tree item style elements $id $col] {
		if { $el eq "e_text_sel" } { break }
	    }
	}

	set path ""
	while { $id != 0 } {
	    set txt [$tree item text $id $col]
	    set path [linsert $path 0 $txt]
	    set id [$tree item parent $id]
	}
	return $path
    }
#     method set_special_folder { id } {
#         set name [$tree item text $id 0]
#         set description [$tree item text $id 1]
#         $tree item configure $id -button yes
#         $tree item style set $id 0 s1-magenta 1 s3
#         $tree item complex $id \
#             [list [list e3 -text $name]] \
#             [list [list e6 -text $description]]
#     }
#     method get_special_folder {} {
#         foreach id [$tree item children 0] {
#             if {[$tree item style set $id 0] eq "s1-magenta" } {
#                 return $id
#             }
#         }
#         return ""
#     }
     method is_special_folder { id } {
	return 0
#         if {[$tree item style set $id 0] eq "s1-magenta" } {
#             return 1
#         }
#         return 0
    }
    method exists_uservar { key } {
	return [info exists uservar($key)]
    }
    method give_uservar { args } {
	switch -- [llength $args] {
	    1 {
		#nothing
	    }
	    2 {
		set  uservar([lindex $args 0]) [lindex $args 1]
	    }
	    default {
		error "error in give_uservar"
	    }
	}
	return [varname uservar([lindex $args 0])]
    }
    method give_unique_item_window_name { item } {
	lassign [list $tree.item$item $tree.item$item 0] w0 w idx
	while { [winfo exists $w] } {
	    set w $w0$idx
	    incr idx
	}
	return $w
    }
    method set_item_window_set_window { item w column } {
	$tree item style set $item $column window
	$tree item element configure $item $column e_window -destroy 1 \
	    -window $w
	return $w
    }
    method item_window_configure { item widget_cmd args } {
	
	lassign [list $tree.item$item $tree.item$item 0] w0 w idx
	while { [winfo exists $w] } {
	    set w $w0$idx
	    incr idx
	}
	if { $widget_cmd ne "" } {
	    set txt [$tree item text $item 0]
	    set img [lindex [$tree item image $item 0] end-1]

	    $widget_cmd $w {*}$args
	    
	    set classes [list Button Checkbutton TButton TCheckbutton]
	    if { ![dict exists $args -text] && ![dict exists $args -image] && [winfo class $w] in $classes } {
		$w configure -text $txt -image $img -compound left
	    }
	}
	$tree item style set $item 0 window
	$tree item element configure $item 0 e_window -destroy 1 \
	    -window $w
	return $w
    }
    method item_window_cget { args } {
	set optional {
	    { -column col 0 }
	}
	set compulsory "item"
	parse_args $optional $compulsory $args

	if { [$self item style set $item $column] ne "window" } { return "" }        
	return [$self item element cget $item $column e_window -window]
    }
    method item_window_configure_button { args } {
	
	set optional {
	    { -text text "" }
	    { -image image "" }
	    { -compound compound "" }
	    { -font font "" }
	    { -command cmd "" }
	    { -column col 0 }
	}
	set compulsory "item"
	parse_args $optional $compulsory $args

	if { $font eq "" } {
	    set font [font actual [$tree cget -font]]
	    set size [dict get $font -size]
	    if { $size > 0 } {
		set size [expr {$size-2}]
	    } else {
		set size [expr {$size+2}]
	    }
	    set font [list {*}$font -underline 1 -size $size]
	    set compoundL [expr {($compound eq "")?"":[list -compound $compound]}]
	}
	lassign [list $tree.item$item $tree.item$item 0] b0 b idx
	while { [winfo exists $b] } {
	    set b $b0$idx
	    incr idx
	}
	button $b -font $font -text $text \
	    -image $image {*}$compoundL \
	    -foreground blue -background white -bd 0 -cursor hand2 \
	    -highlightthickness 0 \
	    -command $command
	$tree item style set $item $column window
	$tree item element configure $item $column e_window -destroy 1 \
	    -window $b
	return $b
    }
    method set_uservar_value { key newvalue } {
	set uservar($key) $newvalue
    }
    method give_uservar_value { args } {
	set key [lindex $args 0]
	switch -- [llength $args] {
	    1 {
		return $uservar($key)
	    }
	    2 {
		if { [info exists uservar($key)] } {
		    return $uservar($key)
		} else {
		    return [lindex $args 1]
		}
	    }
	    default {
		error "error in give_uservar_value"
	    }
	}
    }
    typemethod exists_uservar { key } {
	return [info exists type_uservar($key)]
    }
    typemethod give_uservar { args } {
	switch -- [llength $args] {
	    1 {
		#nothing
	    }
	    2 {
		set type_uservar([lindex $args 0]) [lindex $args 1]
	    }
	    default {
		error "error in give_uservar"
	    }
	}
	return [typevarname type_uservar([lindex $args 0])]
    }
    typemethod set_uservar_value { key newvalue } {
	set type_uservar($key) $newvalue
    }
    typemethod give_uservar_value { args } {
	set key [lindex $args 0]
	switch -- [llength $args] {
	    1 {
		return $type_uservar($key)
	    }
	    2 {
		if { [info exists type_uservar($key)] } {
		    return $type_uservar($key)
		} else {
		    return [lindex $args 1]
		}
	    }
	    default {
		error "error in give_uservar_value"
	    }
	}
    }
    method createimages_colors {} {

	set w [listbox .listbox]
	set fulltktree::SystemHighlight [$w cget -selectbackground]
	set fulltktree::SystemHighlightText [$w cget -selectforeground]
	set fulltktree::SystemFont [$w cget -font]
	destroy $w

	if { [info command search13x14] ne "" } { return }
	
	package require img::png

	image create photo mac-collapse  -data {
	    R0lGODlhEAAQALIAAAAAAAAAMwAAZgAAmQAAzAAA/wAzAAAzMyH5BAUAAAYA
	    LAAAAAAQABAAggAAAGZmzIiIiLu7u5mZ/8zM/////wAAAAMlaLrc/jDKSRm4
	    OAMHiv8EIAwcYRKBSD6AmY4S8K4xXNFVru9SAgAh/oBUaGlzIGFuaW1hdGVk
	    IEdJRiBmaWxlIHdhcyBjb25zdHJ1Y3RlZCB1c2luZyBVbGVhZCBHSUYgQW5p
	    bWF0b3IgTGl0ZSwgdmlzaXQgdXMgYXQgaHR0cDovL3d3dy51bGVhZC5jb20g
	    dG8gZmluZCBvdXQgbW9yZS4BVVNTUENNVAAh/wtQSUFOWUdJRjIuMAdJbWFn
	    ZQEBADs=
	}
	image create photo mac-expand -data {
	    R0lGODlhEAAQALIAAAAAAAAAMwAAZgAAmQAAzAAA/wAzAAAzMyH5BAUAAAYA
	    LAAAAAAQABAAggAAAGZmzIiIiLu7u5mZ/8zM/////wAAAAMnaLrc/lCB6MCk
	    C5SLNeGR93UFQQRgVaLCEBasG35tB9Qdjhny7vsJACH+gFRoaXMgYW5pbWF0
	    ZWQgR0lGIGZpbGUgd2FzIGNvbnN0cnVjdGVkIHVzaW5nIFVsZWFkIEdJRiBB
	    bmltYXRvciBMaXRlLCB2aXNpdCB1cyBhdCBodHRwOi8vd3d3LnVsZWFkLmNv
	    bSB0byBmaW5kIG91dCBtb3JlLgFVU1NQQ01UACH/C1BJQU5ZR0lGMi4wB0lt
	    YWdlAQEAOw==
	}
	image create photo appbook16 -data {
	    R0lGODlhEAAQAIQAAPwCBAQCBDyKhDSChGSinFSWlEySjCx+fHSqrGSipESO
	    jCR6dKTGxISytIy6vFSalBxydAQeHHyurAxubARmZCR+fBx2dDyKjPz+/MzK
	    zLTS1IyOjAAAAAAAAAAAAAAAACH5BAEAAAAALAAAAAAQABAAAAVkICCOZGmK
	    QXCWqTCoa0oUxnDAZIrsSaEMCxwgwGggHI3E47eA4AKRogQxcy0mFFhgEW3M
	    CoOKBZsdUrhFxSUMyT7P3bAlhcnk4BoHvb4RBuABGHwpJn+BGX1CLAGJKzmK
	    jpF+IQAh/mhDcmVhdGVkIGJ5IEJNUFRvR0lGIFBybyB2ZXJzaW9uIDIuNQ0K
	    qSBEZXZlbENvciAxOTk3LDE5OTguIEFsbCByaWdodHMgcmVzZXJ2ZWQuDQpo
	    dHRwOi8vd3d3LmRldmVsY29yLmNvbQA7
	}

	image create photo folder-open -data {
	    R0lGODlhEAANANIAAAAAAISEhMbGxv//AP////8AAAAAAAAAACH5BAkZAAUA
	    LAAAAAAQAA0AAAM8WBrM+rAEQmmIb4qxBWnNQnCkV32ARHRlGQBgDA7vdN6v
	    UK8tC78qlrCWmvRKsJTquHkpZTKAsiCtWq0JAAA7
	}
	image create photo folder-closed -data {
	    R0lGODlhDwANANIAAAAAAISEhMbGxv//AP////8AAAAAAAAAACH5BAkZAAUA
	    LAAAAAAPAA0AAAMzWBXM+jCIMUWAT9JtmwtEKI5hAIBcOplgpVIs8bqxa8On
	    fNP5zsWzDctD9AAKgKRyuUwAAAA7
	} -format GIF
	image create photo folder-magenta-open -data {
	    R0lGODlhEAANAMQAANnZ2YSEhP77/vno/P33/sSkytt/esSSzcOAz9l2gtp6ftyDdcSjyt6I
	    cYWChYp5jYaAh49vlf///8W1yPz2/vLS+P//////////////////////////////////////
	    /yH5BAEAAAAALAAAAAAQAA0AAAWNICCCgSiCgDiSgTAQgxCQZCAUxmEUAhiIIwAEg4EkiFIQ
	    AzEIAQAEQmEcxrEwYMMshQMEQeA80AM90ANFywMEkjAQAzGAxEA8wOQAgTQth3EYh3EsQBAA
	    QEAdCQImSIIkBwQEABAMBpIgCZIgSgEEAAA4FWgcxmEcxrE8AAAAgfNAD/RAD/Q4ICCOZDmG
	    ADs=
	}
	image create photo folder-magenta-closed -data {
	    R0lGODlhDwANAMQAANnZ2YWChYp5jYaAh8Skytt/esSSzYSEhMW1yNyDdfz2/vLR+Prt/Pvy
	    /fno/P33/tl2gsOAz///////////////////////////////////////////////////////
	    /yH5BAEAAAAALAAAAAAPAA0AAAWKIAAEwiAEoiiKQEAURkEEoigCB5IYhZGACHiII3AoC7Mw
	    S+M8zuMEwOEkRmEURmEUoJEIwKEYUARFUARFkDEAh1NEUARFUASBUSEAh2JAERRBERRBxgAc
	    ThFBERRBEQRGhQAcigFFUARFUAQZA3A4iVEYhVEYBWgkAnAEwiAMwiAMwiAEICCOZAmEADs=
	}
	image create photo search13x14 -data {
	    iVBORw0KGgoAAAANSUhEUgAAAA0AAAAOCAYAAAD0f5bSAAAAAXNSR0IArs4c
	    6QAAAAZiS0dEAP8A/wD/oL2nkwAAAAlwSFlzAAALEwAACxMBAJqcGAAAAAd0
	    SU1FB9oJGBAnHDUuxbIAAAAZdEVYdENvbW1lbnQAQ3JlYXRlZCB3aXRoIEdJ
	    TVBXgQ4XAAABj0lEQVQoz3WSsWsUQRSHvw1bTHHFFocsmOLAFGcRWLCxDChy
	    oMKJKURzkDL/R7CSVGlSWFxjt4nXBCwstjONcGCKLXIwheAUi75iA69YfBZr
	    lEn0BwPDDB+/7zGTmJnxO5+/XV5tCSI8vns74R9JzMyOv6zYGo/I0v7Qt7AM
	    wtm5R1vhcPYggteACAAYDQDncM4hCk/331kErX5cRgCAAgwcoQNRkGt6a6I3
	    nR0gXX/hRZAgHLw5sEhPuhjyHdRBe0AhtHFX6puAaM5o6HADkBYWQfEXniCK
	    iKDXbNLnm3eS159W9sGDS0G7Xsk3SvAebQJ5UwPFXwggBKFu9M/AThX/NaCt
	    gj+j2MjjJoDDZ/eSzf1jCyLkqvgmII1A69nKYHd7wqJcxO8E8GKoTDNl1Nbc
	    TwOTXJls5Hhfc3Q0Z7o9ZfZqZlc/AoDT96eWDTOkERQlz3K0U6qqYv52zrgo
	    2Nvb7RvNDDOjPCmt+9ndWO331nZe7tj6rXV7+OiJlSelRU31Rc3/slwuqT5W
	    jIuCXyL8683Gqp6ZAAAAAElFTkSuQmCC
	}
    }
    method clipboard_copy {} {
	set cols [$self cget -columns]
	set dataList ""
	set lineList ""
	foreach i $cols {
	    lappend lineList [lindex $i 1]
	}
	lappend dataList [join $lineList \t]
	set ncols [llength [$self cget -columns]]

	set items [lsort -integer [$tree selection get]]
	if { ![llength $items] } {
	    set items [$tree item children 0]
	}
	foreach item $items {
	    set lineList ""
	    for { set col 0 } { $col < $ncols } { incr col } {
		lappend lineList [$tree item text $item $col]
	    }
	    lappend dataList [join $lineList \t]
	}
	clipboard clear
	clipboard append [join $dataList \n]
    }
}

proc range {from to {step 1}} {
    set res ""
    if { $step > 0 } {
	for { set i $from } { $i < $to } { set i [expr {$i+$step}] } {
	    lappend res $i
	}
    } else {
	for { set i $from } { $i > $to } { set i [expr {$i+$step}] } {
	    lappend res $i
	}
    }
    return $res
}

proc rangeF {from to {step 1}} {
    set res ""
    if { $step > 0 } {
	for { set i $from } { $i <= $to } { set i [expr {$i+$step}] } {
	    lappend res $i
	}
    } else {
	for { set i $from } { $i >= $to } { set i [expr {$i+$step}] } {
	    lappend res $i
	}
    }
    return $res
}






