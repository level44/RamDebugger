


namespace eval cproject {
    variable project ""
    variable group All
    variable groupbefore ""
    variable groups All
    variable links
    variable scripttabs
    variable debugreleasebefore ""
    variable debugrelease debug
    variable files ""

    variable thisdataC
    variable dataC
    variable thisdataL
    variable dataL
    variable thisdataE
    variable dataE

    variable compilationstatus
}

################################################################################
#   project creation/edition
################################################################################

proc cproject::Init { w } {
    variable project

    trace var ::cproject::group w "cproject::SetGroupActive;#"
    trace var ::cproject::debugrelease w "cproject::SetDebugReleaseActive;#"

    if { [info exists RamDebugger::options(recentprojects)] && \
	    [llength $RamDebugger::options(recentprojects)] > 0 } {
	set project [lindex $RamDebugger::options(recentprojects) 0]
	catch { OpenProject $w 0 }
    }
}

proc cproject::synctoUI {} {
    variable thisdataC
    variable dataC
    variable thisdataL
    variable dataL
    variable thisdataE
    variable dataE
    variable thisdataS
    variable dataS
    variable group
    variable debugrelease

    foreach i [array names dataC $group,$debugrelease,*] {
	regexp {^[^,]+,[^,]+,(.*)} $i {} prop
	set thisdataC($prop) $dataC($i)
    }
    foreach i [array names dataL $debugrelease,*] {
	regexp {^[^,]+,(.*)} $i {} prop
	set thisdataL($prop) $dataL($i)
    }
    foreach i [array names dataS $debugrelease,*] {
	regexp {^[^,]+,(.*)} $i {} prop
	set thisdataS($prop) $dataS($i)
    }
    foreach i [array names dataE $debugrelease,*] {
	regexp {^[^,]+,(.*)} $i {} prop
	set thisdataE($prop) $dataE($i)
    }
}

proc cproject::syncfromUI {} {
    variable thisdataC
    variable dataC
    variable thisdataL
    variable dataL
    variable thisdataS
    variable dataS
    variable thisdataE
    variable dataE
    variable groupbefore
    variable debugreleasebefore

    if { $debugreleasebefore == "" } { return }

    foreach i [array names dataC $groupbefore,$debugreleasebefore,*] {
	regexp {^[^,]+,[^,]+,(.*)} $i {} prop

	if { $groupbefore == "All" || $debugreleasebefore == "both" } {
	    TransferDataToLowerGroups $groupbefore $debugreleasebefore $prop $dataC($i) \
		    $thisdataC($prop) dataC
	}
	set dataC($i) $thisdataC($prop)
    }
    foreach i [array names dataL $debugreleasebefore,*] {
	regexp {^[^,]+,(.*)} $i {} prop
	if { $debugreleasebefore == "both" } {
	    TransferDataToLowerGroups "" $debugreleasebefore $prop $dataL($i) $thisdataL($prop) \
		dataL
	}
	set dataL($i) $thisdataL($prop)
    }
    foreach i [array names dataS $debugreleasebefore,*] {
	regexp {^[^,]+,(.*)} $i {} prop
	if { ![info exists thisdataS($prop)] } { continue }
	if { $debugreleasebefore == "both" } {
	    TransferDataToLowerGroups "" $debugreleasebefore $prop $dataS($i) $thisdataS($prop) \
		dataS
	}
	set dataS($i) $thisdataS($prop)
    }
    foreach i [array names dataE $debugreleasebefore,*] {
	regexp {^[^,]+,(.*)} $i {} prop
	if { $debugreleasebefore == "both" } {
	    TransferDataToLowerGroups "" $debugreleasebefore $prop $dataE($i) $thisdataE($prop) \
		dataE
	}
	set dataE($i) $thisdataE($prop)
    }
}

proc cproject::TransferDataToLowerGroups { gr dr prop olddata newdata dataname } {
    variable dataC
    variable dataL
    variable dataS
    variable dataE

    upvar 0 $dataname data

    set toadd ""
    set todel ""

    if { ![string match *dirs $prop] } {
	set olddataL [regexp -inline -all {[^\s;]+} $olddata]
	set newdataL [regexp -inline -all {[^\s;]+} $newdata]
	set searchcmd lsearch
    } else {
	set olddataL $olddata
	set newdataL $newdata
	set searchcmd RamDebugger::lsearchfile
    }

    foreach i $newdataL {
	if { [$searchcmd $olddataL $i] == -1 } { lappend toadd $i }
    }
    foreach i $olddataL {
	if { [$searchcmd $newdataL $i] == -1 } { lappend todel $i }
    }
    foreach i [array names data *,$prop] {
	switch $dataname {
	    dataC { regexp {^([^,]+),([^,]+),(.*)} $i {} gr_in dr_in prop_in }
	    dataL - dataS - dataE {
		set gr_in ""
		regexp {^([^,]+),(.*)} $i {} dr_in prop_in
	    }
	}
	if { $gr_in == $gr && $dr_in == $dr } { continue }

	if { $gr == "All" && $dr == "both" } {
	    # nothing
	} elseif { $gr == "All" } {
	    if { $dr != $dr_in } { continue }
	} elseif { $dr == "both" } {
	    if { $gr != $gr_in } { continue }
	} else { continue }

	if { $dataname == "dataS" } {
	    regsub -all {(?n)^\s*\#.*$} $data($i) "" res
	    if { [string trim $res] == "" } {
		set data($i) $newdata
	    }
	    continue
	}

	if { ![string match *dirs $prop] } {
	    set dataLocal [regexp -inline -all {[^\s;]+} $data($i)]
	} else {
	    set dataLocal $data($i)
	}
	foreach j $toadd {
	    if { [$searchcmd $dataLocal $j] == -1 } {
		if { ![string match *dirs $prop] } {
		    append data($i) " $j"
		} else {
		    lappend data($i) $j
		}
	    }
	}
	foreach j $todel {
	    if { [$searchcmd $dataLocal $j] != -1 } {
		if { ![string match *dirs $prop] } {
		    set ipos [$searchcmd $dataLocal $j]
		    set data($i) [join [lreplace $dataLocal $ipos $ipos]]
		} else {
		    set ipos [$searchcmd $dataLocal $j]
		    set data($i) [lreplace $data($i) $ipos $ipos]
		}
	    }
	}
	if { ![string match *dirs $prop] } {
	    set data($i) [string trim $data($i)]
	}
    }
}

proc cproject::SaveProjectC { w } {
    variable project
    variable group
    variable groups
    variable links
    variable scripttabs
    variable debugrelease
    variable files
    variable thisdataC
    variable dataC
    variable thisdataL
    variable dataL
    variable thisdataS
    variable dataS
    variable thisdataE
    variable dataE

    if { $project == "" } { return }

    syncfromUI

    set err [catch { open $project w } fout]
    if { $err } {
	WarnWin "Could not open file '$project' to save ($fout)" $w
	return
    }
    foreach i [list groups group links scripttabs debugrelease files thisdataC dataC thisdataL \
	    dataL thisdataS dataS thisdataE dataE ] {
	if { [array exists $i] } {
	    puts $fout [list array set $i [array get $i]]
	} else {
	    puts $fout [list set $i [set $i]]
	}
    }
    close $fout

    if { [RamDebugger::lsearchfile $RamDebugger::options(recentprojects) $project] != -1 } {
	set ipos [RamDebugger::lsearchfile $RamDebugger::options(recentprojects) $project]
	set RamDebugger::options(recentprojects) [lreplace $RamDebugger::options(recentprojects) \
	    $ipos $ipos]
    }
    set RamDebugger::options(recentprojects) [linsert $RamDebugger::options(recentprojects) \
	    0 $project]
}


proc cproject::UpdateComboValues { combo varname } {
    if { ![winfo exists $combo] } { return }
    $combo configure -values [set $varname]
}

proc cproject::NewProject { w } {
    variable project
    variable groupbefore
    variable group
    variable groups
    variable links
    variable scripttabs
    variable debugrelease
    variable files
    variable thisdataC
    variable dataC
    variable thisdataL
    variable dataL
    variable thisdataE
    variable dataE


    set types {
	{{Project files}      {.prj}   }
	{{All Files}        *          }
    }
    set dir $RamDebugger::options(defaultdir)

    set file [tk_getSaveFile -filetypes $types -initialdir $dir -parent $w \
	-title "New project" -defaultextension .prj]
    if { $file == "" } { return }

    set RamDebugger::options(defaultdir) [file dirname $file]

    set project $file
    set debugreleasebefore ""
    set groups All
    set links Link
    set scripttabs Script
    set files ""

    NewData

    set debugrelease debug
    set group All
}

proc cproject::NewData {} {
    variable project
    variable thisdataC
    variable dataC
    variable thisdataL
    variable dataL
    variable thisdataS
    variable dataS
    variable thisdataE
    variable dataE
    variable groups
    variable links
    variable scripttabs

    foreach i [list C L S E] {
	catch { unset data$i }
	catch { unset thisdata$i }
    }
    foreach i [list debug release both] {
	foreach group $groups {
	    set dataC($group,$i,includedirs) .
	    set dataC($group,$i,defines) ""
	    set dataC($group,$i,compiler) "gcc"
	    switch $i {
		debug {
		    set dataC($group,$i,flags) "-c -g"
		}
		release {
		    set dataC($group,$i,flags) "-c -O3"
		}
		both {
		    set dataC($group,$i,flags) "-c"
		}
	    }
	}
	foreach link $links {
	    set dataL($i,$link,librariesdirs) .
	    set dataL($i,$link,linkgroups) All
	    set dataL($i,$link,libraries) "libc libm"
	    set dataL($i,$link,linker) "gcc"
	    set dataL($i,$link,linkflags) ""
	    set dataL($i,$link,linkexe) [file root [file tail $project]]
	    
	    if { $i != "both" } {
		set dataE($i,execdir) [file join [file dirname $project] [file root $project]_$i]
		set dataE($i,exe) [file root [file tail $project]]
	    } else {
		set dataE($i,execdir) ""
		set dataE($i,exe) ""
		
	    }
	    set dataE($i,exeargs) ""
	}
	foreach scripttab $scripttabs {
	    set dataS($i,$scripttab,script) help
	    set dataS($i,$scripttab,executetime) "No automatic"
	}
    }
}

proc cproject::OpenProject { w { ask 1 } } {
    variable project
    variable groupbefore
    variable group
    variable groups
    variable links
    variable scripttabs
    variable debugrelease
    variable files
    variable dataC
    variable dataL
    variable dataE
    variable debugreleasebefore

    if { $ask } {
#          set ret [DialogWin::messageBox -default ok -icon warning -message \
#              "Are you sure to discard all project data?" -parent $w \
#              -title "discard data" -type okcancel]
#          if { $ret == "cancel" } { return }

	set types {
	    {{Project files}      {.prj}   }
	    {{All Files}        *          }
	}
	
	set dir $RamDebugger::options(defaultdir)
	
	set file [tk_getOpenFile -filetypes $types -initialdir $dir -parent $w \
	    -title "Open existing project" -defaultextension .prj]
	if { $file == "" } { return }
    } else { set file $project }

    set project $file

    trace vdelete ::cproject::group w "cproject::SetGroupActive;#"
    trace vdelete ::cproject::debugrelease w "cproject::SetDebugReleaseActive;#"
    trace vdelete ::cproject::links w "UpdateLinktabs ;#"
    trace vdelete ::cproject::scripttabs w "UpdateScripttabs ;#"
    
    set err [catch {
	if { [interp exists cproject_tmp] } { interp delete cproject_tmp }
	interp create cproject_tmp
	cproject_tmp eval [list source $file]
	set groups [cproject_tmp eval set groups]
	if { [catch { set links [cproject_tmp eval set links] }]} {
	    set links Link
	}
	if { [catch { set scripttabs [cproject_tmp eval set scripttabs] }]} {
	    set scripttabs Script
	}
	interp delete cproject_tmp
	# trick because NewData needs to have groups and links defined
	NewData
	source $file
    } errstring]
    
    if { $err } {
	WarnWin "Error opening project '$file' ($errstring)" $w
	return
    }
    if { [array exists data] } {
	# upgrade old versions
	set links Link
	foreach i [array names data] {
	    if { [regexp {(librariesdirs|libraries|linkflags)$} $i] } {
		regexp {^([^,]+),(.*)} $i {} dr r 
		set dataL($dr,Link,$r) $data($i)
	    } elseif { [regexp {(execdir|exe|exeargs)$} $i] } {
		set dataE($i) $data($i)
	    } else { set dataC($i) $data($i) }
	}
    }
    # to activate the trace
    set groupbefore ""
    set debugreleasebefore ""

    trace var ::cproject::group w "cproject::SetGroupActive;#"
    trace var ::cproject::debugrelease w "cproject::SetDebugReleaseActive;#"
    trace var ::cproject::links w "UpdateLinktabs ;#"
    trace var ::cproject::scripttabs w "UpdateScripttabs ;#"
   
    set links $links
    set scripttabs $scripttabs

    set group $group

    set groupbefore $group
    set debugreleasebefore $debugrelease

    if { [RamDebugger::lsearchfile $RamDebugger::options(recentprojects) $project] != -1 } {
	set ipos [RamDebugger::lsearchfile $RamDebugger::options(recentprojects) $project]
	set RamDebugger::options(recentprojects) [lreplace $RamDebugger::options(recentprojects) \
		$ipos $ipos]
    }
    set RamDebugger::options(recentprojects) [linsert $RamDebugger::options(recentprojects) \
	    0 $project]
}

proc cproject::SaveProject { w } {
    variable project
    set types {
	{{Project files}      {.prj}   }
	{{All Files}        *          }
    }
    set dir $RamDebugger::options(defaultdir)

    set file [tk_getSaveFile -filetypes $types -initialdir $dir -parent $w \
	-title "Save project" -defaultextension .prj]
    if { $file == "" } { return }

    #set RamDebugger::options(defaultdir) [file dirname $file]
    
    set project $file
    SaveProjectC $w
}

proc cproject::SetGroupActive {} {
    variable groupbefore
    variable group
    variable debugrelease
    variable thisdata
    variable data

    set dir [IsProjectNameOk]

    syncfromUI

    set groupbefore $group
    synctoUI
}

proc cproject::SetDebugReleaseActive {} {
    variable debugreleasebefore
    variable debugrelease
    variable thisdata
    variable data

    set dir [IsProjectNameOk]
    syncfromUI
    set debugreleasebefore $debugrelease
    synctoUI
}

proc cproject::CreateModifyGroup { w what } {
    variable group
    variable groups
    variable files
    variable dataC
    variable groupbefore

    set dir [IsProjectNameOk]
    syncfromUI

    if { $what == "delete" } {
	if { $group == "All" } {
	    WarnWin "Group 'All' cannot be deleted" $w
	    return
	}
	set ret [DialogWin::messageBox -default ok -icon warning -message \
	    "Are you sure to delete group '$group'?" -parent $w \
	    -title "delete group" -type okcancel]
	if { $ret == "cancel" } { return }
	
	for { set i 0 } { $i < [llength $files] } { incr i } {
	    foreach "file type group_in path" [lindex $files $i] break
	    if { $group == $group_in } {
		set files [lreplace $files $i $i [list $file $type All $path]]
	    }
	}
	set ipos [lsearch $groups $group]
	set groups [lreplace $groups $ipos $ipos]
	foreach i [array names dataC $group,*] {
	    unset dataC($i)
	}
	set groupbefore ""
	set group All
	return
    }

    CopyNamespace ::DialogWin ::DialogWinCR

    switch $what {
	create {
	    set title "New group"
	    set label "Enter new group name"
	}
	rename {
	    set title "Rename group"
	    set label "Enter new name for group '$group'"
	}
    }

    set f [DialogWinCR::Init $w $title separator ""]
    set w [winfo toplevel $f]

    label $f.l -text $label -grid "0 px3 py3"
    entry $f.e -textvariable DialogWinCR::user(name) -grid "0 px10 py3" -width 30

    set DialogWinCR::user(name) $group
    tkTabToWindow $f.e

    supergrid::go $f

    bind $w <Return> "DialogWinCR::InvokeOK"

    set action [DialogWinCR::CreateWindow]
    while 1 {
	switch $action {
	    0 {
		DialogWinCR::DestroyWindow
		namespace delete ::DialogWinCR
		return
	    }
	    1 {
		if { [string trim $DialogWinCR::user(name)] == "" } {
		    WarnWin "Group name cannot be void" $w
		} elseif { [lsearch $groups $DialogWinCR::user(name)] != -1 } {
		    WarnWin "Group name already exists" $w
		} elseif { ![string is wordchar $DialogWinCR::user(name)] } {
		    WarnWin "Group name is not OK" $w
		} else {
		    set newname $DialogWinCR::user(name)
		    DialogWinCR::DestroyWindow
		    namespace delete ::DialogWinCR
		    break
		}
	    }
	}
	set action [DialogWinCR::WaitForWindow]
    }

    if { $what == "rename" } {
	for { set i 0 } { $i < [llength $files] } { incr i } {
	    foreach "file type group_in path" [lindex $files $i] break
	    if { $group == $group_in } {
		set files [lreplace $files $i $i [list $file $type $newname $path]]
	    }
	}
	set ipos [lsearch $groups $group]
	set groups [lreplace $groups $ipos $ipos $newname]
	foreach i [array names dataC $group,*] {
	    regexp {,(.*)} $i {} rest
	    set dataC($newname,$rest) $dataC($i)
	    unset dataC($i)
	}
    } else {
	lappend groups $newname
	foreach i [array names dataC All,*] {
	    regexp {,(.*)} $i {} rest
	    set dataC($newname,$rest) $dataC($i)
	}
    }
    set groupbefore ""
    set group $newname

}

proc cproject::CreateDo { what f } {

    set w [winfo toplevel $f]
    switch $what {
	Ok {
	    SaveProjectC $w
	    destroy $w
	}
	Apply {
	    SaveProjectC $w
	}
	Cancel { destroy $w }
    }
}

proc cproject::Create { par } {
    variable notebook

    if { ![info exists RamDebugger::options(recentprojects)] } {
	set RamDebugger::options(recentprojects) ""
    }

    set commands [list "cproject::CreateDo Ok" "cproject::CreateDo Apply" \
		      "cproject::CreateDo Cancel"]

    set f [DialogWinTop::Init $par "C++ compilation project" separator $commands [list Apply]]
    set w [winfo toplevel $f]

    set f1 [frame $f.f1 -grid "0 n"]
    Label $f1.l1 -text "Project:" -grid 0 -helptext \
       "A project includes all the compilation information. Create a project before entering data"
    ComboBox $f1.cb1 -textvariable cproject::project -grid "1 3" -width 100 -editable 0 \
	 -values $RamDebugger::options(recentprojects) -modifycmd "cproject::OpenProject $w 0"

    focus $f1.cb1

    set bbox [ButtonBox $f1.bbox1 -spacing 0 -padx 1 -pady 1 -homogeneous 1 -grid "4 w"]
    $bbox add -image filenew16 \
	 -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1 -padx 1 -pady 1 \
	 -helptext [_ "Create new project"] \
	 -command "cproject::NewProject $w"
    $bbox add -image fileopen16 \
	 -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1 -padx 1 -pady 1 \
	 -helptext [_ "Open existing project"] \
	 -command "cproject::OpenProject $w"
    $bbox add -image filesave16 \
	 -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1 -padx 1 -pady 1 \
	 -helptext [_ "Save as project"] \
	 -command "cproject::SaveProject $w"
 
    Label $f1.l2 -text "Group:" -grid "0 py3" -helptext \
       "A group is a set of files with common compilation options. The special group 'all'\
	always exists and affects all files"
    ComboBox $f1.cb2 -textvariable cproject::group -grid 1 -values $cproject::groups \
       -editable 0

    trace var cproject::groups w "cproject::UpdateComboValues $f1.cb2 cproject::groups ;#"

    set bbox [ButtonBox $f1.bbox2 -spacing 0 -padx 1 -pady 1 -homogeneous 1 -grid "2 w"]
    $bbox add -image acttick16 \
	 -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1 -padx 1 -pady 1 \
	 -helptext [_ "Create new group"] \
	 -command "cproject::CreateModifyGroup $w create"
    $bbox add -image edit16 \
	 -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1 -padx 1 -pady 1 \
	 -helptext [_ "Rename group"] \
	 -command "cproject::CreateModifyGroup $w rename"
    $bbox add -image actcross16 \
	 -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1 -padx 1 -pady 1 \
	 -helptext [_ "Delete group"] \
	 -command "cproject::CreateModifyGroup $w delete"

    frame $f1.f1 -grid "3 2 px3 py3 ew" -bd 2 -relief raised
    radiobutton $f1.f1.r1 -text Debug -variable cproject::debugrelease -value debug \
	    -grid 0
    radiobutton $f1.f1.r2 -text Release -variable cproject::debugrelease -value release \
	    -grid "1"
    radiobutton $f1.f1.r3 -text Both -variable cproject::debugrelease -value both \
	    -grid "2"

    set pw [PanedWindow $f.pw -side top -pad 0 -weights available -grid 0 -activator line]

    foreach "weight1 weight2" [RamDebugger::ManagePanes $pw h "2 3"] break

    set pane1 [$pw add -weight $weight1]

    set sw [ScrolledWindow $pane1.lf -relief sunken -borderwidth 0 -grid "0"]
    set DialogWinTop::user($w,list) [tablelist::tablelist $sw.lb -width 55 -height 20\
	    -exportselection 0 \
	    -columns [list \
	    14 File   left \
	    5  Type center \
	    11 Group right \
	    15 Path left \
	    ] \
	    -labelcommand tablelist::sortByColumn \
	    -background white \
	    -selectbackground navy -selectforeground white \
	    -stretch 1 -selectmode extended \
	    -highlightthickness 0 \
	    -listvariable cproject::files]
    
    $sw setwidget $DialogWinTop::user($w,list)

    bind [$sw.lb bodypath] <1> "focus $sw.lb"

    set bbox [ButtonBox $pane1.bbox1 -spacing 0 -padx 1 -pady 1 -homogeneous 1 -grid "0 wn py3"]
    $bbox add -image fileopen16 \
	 -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1 -padx 1 -pady 1 \
	 -helptext [_ "Add file to project"] \
	 -command "cproject::AddModFiles $sw.lb file"
    $bbox add -image folderopen16 \
	 -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1 -padx 1 -pady 1 \
	 -helptext [_ "Add files from directory to project"] \
	 -command "cproject::AddModFiles $sw.lb dir"
    $bbox add -image edit16 \
	 -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1 -padx 1 -pady 1 \
	 -helptext [_ "Assign selected files to active group"] \
	 -command "cproject::AddModFiles $sw.lb edit"
    $bbox add -image actcross16 \
	 -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1 -padx 1 -pady 1 \
	 -helptext [_ "Delete files from project"] \
	 -command "cproject::AddModFiles $sw.lb delete"
    $bbox add -image acttick16 \
	 -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1 -padx 1 -pady 1 \
	 -helptext [_ "View file"] \
	 -command "cproject::AddModFiles $sw.lb view"

    bind [$DialogWinTop::user($w,list) bodypath] <ButtonPress-3> \
	[bind TablelistBody <ButtonPress-1>]

    bind [$DialogWinTop::user($w,list) bodypath] <ButtonRelease-3> {
	catch { destroy %W.menu }
	set menu [menu %W.menu]
	set lb [winfo parent %W]
	
	$menu add command -label "Assign group" -command "cproject::AddModFiles $lb edit"
	$menu add command -label "View file" -command "cproject::AddModFiles $lb view"
	$menu add separator
	$menu add command -label "Delete from project" -command "cproject::AddModFiles $lb delete"
	tk_popup $menu %X %Y
    }

    set pane2 [$pw add -weight $weight2]

    set notebook [NoteBook $pane2.nb -homogeneous 1 -bd 1 -internalborderwidth 3  \
	-grid "0 px3 py3"]

    set f21 [$pane2.nb insert end compilation -text "Compilation"]

    TitleFrame $f21.f1 -text "include directories" -grid 0
    set f121 [$f21.f1 getframe]

    set sw [ScrolledWindow $f121.lf -relief sunken -borderwidth 0]
    listbox $sw.lb -listvariable cproject::thisdataC(includedirs)
    $sw setwidget $sw.lb

    set bbox [ButtonBox $f121.bbox1 -spacing 0 -padx 1 -pady 1 -homogeneous 1]
    $bbox add -image folderopen16 \
	 -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1 -padx 1 -pady 1 \
	 -helptext [_ "Add include directory"] \
	 -command "cproject::AddDelDirectories $sw.lb add"
    $bbox add -image actcross16 \
	 -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1 -padx 1 -pady 1 \
	 -helptext [_ "Delete include directory"] \
	 -command "cproject::AddDelDirectories $sw.lb delete"

    grid $f121.lf -sticky nsew
    grid $bbox -sticky nw
    grid rowconfigure $f121 0 -weight 1
    grid columnconfigure $f121 0 -weight 1
    
    TitleFrame $f21.f15 -text "compiler" -grid "0 n"
    set f1215 [$f21.f15 getframe]

    set values [list "" gcc g++]
    ComboBox $f1215.cb -textvariable cproject::thisdataC(compiler) -values $values \
	-grid "0 w" -width 10

    TitleFrame $f21.f2 -text "defines" -grid "0 n"
    set f122 [$f21.f2 getframe]

    entry $f122.e -grid 0 -textvariable cproject::thisdataC(defines)

    TitleFrame $f21.f3 -text "additional compile flags" -grid "0 n"
    set f123 [$f21.f3 getframe]

    entry $f123.e -grid 0 -textvariable cproject::thisdataC(flags)

    set f23 [$pane2.nb insert end execute -text "Execute"]

    TitleFrame $f23.f1 -text "executable file" -grid "0 n"
    set f321 [$f23.f1 getframe]

    entry $f321.e -grid 0 -textvariable cproject::thisdataE(exe)
    Button $f321.b1 -image [Bitmap::get file] -width 16 -grid 1 -relief link

    TitleFrame $f23.f2 -text "working directory" -grid "0 n"
    set f322 [$f23.f2 getframe]

    entry $f322.e -grid 0 -textvariable cproject::thisdataE(execdir)
    Button $f322.b1 -image [Bitmap::get folder] -width 16 -grid 1 -relief link

    TitleFrame $f23.f3 -text "arguments" -grid "0 n"
    set f323 [$f23.f3 getframe]

    entry $f323.e -grid 0 -textvariable cproject::thisdataE(exeargs)

    set comm {
	set cproject::thisdataE(exe) [tk_getOpenFile -filetypes {{{All Files} *}} \
		-initialdir $RamDebugger::options(defaultdir) -initialfile \
		[file tail $cproject::thisdataE(exe)] -parent PARENT -title "Executable file"]
    }
    set comm [string map [list PARENT $w] $comm]
    $f321.b1 configure -command $comm

    set comm {
	set initial $RamDebugger::options(defaultdir)
	catch { set initial [file dirname $cproject::thisdataE(exe)] }
	set cproject::thisdataE(execdir) [RamDebugger::filenormalize [tk_chooseDirectory   \
	    -initialdir $initial -parent PARENT \
	    -title "Working directory" -mustexist 1]]
    }
    set comm [string map [list PARENT $w] $comm]
    $f322.b1 configure -command $comm


    $pane2.nb compute_size
    $pane2.nb raise compilation
 
    supergrid::go $pane1
    supergrid::go $f1215
    supergrid::go $f122
    supergrid::go $f123
    supergrid::go $f21
    supergrid::go $f321
    supergrid::go $f322
    supergrid::go $f323
    supergrid::go $f23
    supergrid::go $pane2
    supergrid::go $f

    UpdateLinktabs
    # if it exists from before, it will be deleted
    trace vdelete ::cproject::links w "UpdateLinktabs ;#"
    trace var cproject::links w "UpdateLinktabs ;#"


    UpdateScripttabs
    # if it exists from before, it will be deleted
    trace vdelete ::cproject::scripttabs w "UpdateScripttabs ;#"
    trace var cproject::scripttabs w "UpdateScripttabs ;#"


    bind $w <Return> "DialogWinTop::InvokeOK $f"
    
    DialogWinTop::CreateWindow $f "" "" 500
}

proc cproject::UpdateLinktabs {} {
    variable notebook
    variable links

    if { ![info exists notebook] || ![winfo exists $notebook] } { return }
    if { ![info exists links] } { set links Link }

    set pages [$notebook pages 1 end-1]

    foreach i $pages {
	if { [string match Link* $i] } {
	    $notebook delete $i
	}
    }
    foreach i $links {
	regsub -all {\W} $i {X} page
	set f [$notebook insert end-1 $page -text $i]
	AddLinkTab $f $i
    }
}

proc cproject::AddGroupInLinkGroups { but entry } {
    variable groups

    set menu $but.menu
    catch { destroy $menu }

    menu $menu
    foreach i $groups {
	set comm {
	    set str [ENTRY get]
	    append str " GROUP"
	    ENTRY del 0 end
	    ENTRY insert end [string trim $str]
	}
	set comm [string map [list ENTRY $entry GROUP $i] $comm]
	$menu add command -label $i -command $comm
    }
    tk_popup $menu [winfo rootx $but] [winfo rooty $but]
}

proc cproject::AddLinkTab { f link } {

    TitleFrame $f.f0 -text "link groups"
    set f0 [$f.f0 getframe]
    entry $f0.e -grid 0 -textvariable cproject::thisdataL($link,linkgroups)

    Label $f0.b1 -image acttick16 \
	 -highlightthickness 0 -takefocus 0 -relief flat -borderwidth 1 -padx 1 -pady 1 \
	 -helptext [_ "Add group"] -grid 1
    bind $f0.b1 <ButtonPress-1> "cproject::AddGroupInLinkGroups $f0.b1 $f0.e" 

    TitleFrame $f.f1 -text "libraries directories"
    set f1 [$f.f1 getframe]

    set sw [ScrolledWindow $f1.lf -relief sunken -borderwidth 0]
    listbox $sw.lb -listvariable cproject::thisdataL($link,librariesdirs)
    $sw setwidget $sw.lb

    set bbox [ButtonBox $f1.bbox1 -spacing 0 -padx 1 -pady 1 -homogeneous 1]
    $bbox add -image folderopen16 \
	 -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1 -padx 1 -pady 1 \
	 -helptext [_ "Add link directories"] \
	 -command "cproject::AddDelDirectories $sw.lb add"
    $bbox add -image actcross16 \
	 -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1 -padx 1 -pady 1 \
	 -helptext [_ "Delete link directories"] \
	 -command "cproject::AddDelDirectories $sw.lb delete"

    grid $f1.lf -sticky nsew
    grid $bbox -sticky nw
    grid rowconfigure $f1 0 -weight 1
    grid columnconfigure $f1 0 -weight 1

    TitleFrame $f.f2 -text "libraries"
    set f2 [$f.f2 getframe]

    set values [list gcc g++ ar]
    if { $::tcl_platform(platform) == "windows" } {
	lappend values windres
    }
    ComboBox $f2.cb -textvariable cproject::thisdataL($link,linker) -values $values \
	-grid "0 w" -width 7

    entry $f2.e -grid 1 -textvariable cproject::thisdataL($link,libraries)

    TitleFrame $f.f3 -text "additional link flags"
    set f3 [$f.f3 getframe]
    entry $f3.e -grid 0 -textvariable cproject::thisdataL($link,linkflags)

    TitleFrame $f.f4 -text "output name"
    set f4 [$f.f4 getframe]
    entry $f4.e -grid 0 -textvariable cproject::thisdataL($link,linkexe)

    set bbox [ButtonBox $f.bbox -spacing 0 -padx 1 -pady 1 -homogeneous 1 -grid "0 nw"]
    $bbox add -image acttick16 \
	 -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1 -padx 1 -pady 1 \
	 -helptext [_ "Create new link tab"] \
	 -command [list cproject::CreateDeleteLinkTab $link create]
    $bbox add -image actcross16 \
	 -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1 -padx 1 -pady 1 \
	 -helptext [_ "Delete link tab"] \
	 -command [list cproject::CreateDeleteLinkTab $link delete]
    $bbox add -image edit16 \
	 -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1 -padx 1 -pady 1 \
	 -helptext [_ "Rename link tab"] \
	 -command [list cproject::CreateDeleteLinkTab $link rename]

    grid $f.f0 -sticky new
    grid $f.f1 -sticky nsew
    grid $f.f2 -sticky new
    grid $f.f3 -sticky new
    grid $f.f4 -sticky new
    grid $f.bbox -sticky nw

    grid rowconfigure $f 1 -weight 1
    grid columnconfigure $f 0 -weight 1


    supergrid::go $f0
    supergrid::go $f2
    supergrid::go $f3
    supergrid::go $f4
}

proc cproject::CreateDeleteLinkTab { currentlink what } {
    variable links
    variable dataL
    variable notebook

    syncfromUI

    switch $what {
	create {
	    set num [expr [llength $links]+1]
	    set newlink Link$num
	    foreach i [array names dataL *,$currentlink,*] {
		regexp {([^,]+),[^,]+,([^,]+)} $i {} dr v
		set dataL($dr,$newlink,$v) $dataL($i)
	    }
	    lappend links $newlink
	    $notebook raise $newlink
	}
	delete {
	    if { [llength $links] == 1 } {
		WarnWin "Error: There must be at least one link tab" $notebook
		return
	    }
	    set ret [DialogWin::messageBox -default ok -icon warning -message \
		"Are you sure to delete link tab '$currentlink'?" -parent $notebook \
		-title "delete link tab" -type okcancel]
	    if { $ret == "cancel" } { return }
	    
	    set delpos [lsearch $links $currentlink]

	    foreach i [array names dataL *,$currentlink,*] {
		unset dataL($i)
	    }
	    set links [lreplace $links $delpos $delpos]
	    if { $delpos >= [llength $links] } { set delpos 0 }
	    $notebook raise [lindex $links $delpos]
	}
	rename {
	    CopyNamespace ::DialogWin ::DialogWinCR
	    set f [DialogWinCR::Init $notebook "Enter link name" separator ""]
	    set w [winfo toplevel $f]
	    
	    label $f.l -text "Enter new name for link tab '$currentlink'" -grid "0 px3 py3"
	    entry $f.e -textvariable DialogWinCR::user(name) -grid "0 px10 py3" -width 30
	    
	    set DialogWinCR::user(name) $currentlink
	    tkTabToWindow $f.e
	    supergrid::go $f
	    bind $w <Return> "DialogWinCR::InvokeOK"

	    set action [DialogWinCR::CreateWindow]
	    while 1 {
		switch $action {
		    0 {
		        DialogWinCR::DestroyWindow
		        namespace delete ::DialogWinCR
		        return
		    }
		    1 {
		        set newlink [string trim $DialogWinCR::user(name)]
		        if { $newlink == "" } {
		            set newlink "Link"
		        } elseif { ![string match "Link *" $newlink] } {
		            set newlink "Link $newlink"
		        }
		        if { $newlink == "" } {
		            WarnWin "Link tab name cannot be void" $w
		        } elseif { [lsearch $links $newlink] != -1 } {
		            WarnWin "Link tab name '$newlink' already exists" $w
		        } else {
		            DialogWinCR::DestroyWindow
		            namespace delete ::DialogWinCR
		            break
		        }
		    }
		}
		set action [DialogWinCR::WaitForWindow]
	    }
	    set pos [lsearch $links $currentlink]
	    foreach i [array names dataL *,$currentlink,*] {
		regexp {([^,]+),[^,]+,([^,]+)} $i {} dr v
		set dataL($dr,$newlink,$v) $dataL($i)
		unset dataL($i)
	    }
	    set links [lreplace $links $pos $pos $newlink]
	}
    }
    synctoUI
}


proc cproject::UpdateScripttabs {} {
    variable notebook
    variable scripttabs

    if { ![info exists notebook] || ![winfo exists $notebook] } { return }
    if { ![info exists scripttabs] } { set scripttabs Script }

    set pages [$notebook pages 1 end-1]

    foreach i $pages {
	if { [string match Script* $i] } {
	    $notebook delete $i
	}
    }
    foreach i $scripttabs {
	regsub -all {\W} $i {X} page
	set f [$notebook insert end-1 $page -text $i]
	AddScripttab $f $i
    }
}

proc cproject::ScriptTabColorize { txt command args } {
    
    if { [regexp {^(ins|del)} $command] } {
	RamDebugger::ColorizeSlow $txt
    }
}

proc cproject::AddScripttab { f scripttab } {
    variable links
    variable debugrelease

    TitleFrame $f.f0 -text "contents" -grid "0 news"
    set f0 [$f.f0 getframe]

    set helptext ""
    append helptext "# It is possible to include a TCL script here\n"
    append helptext "# it will be executed either automatically or\n"
    append helptext "# manually, depending on configuration. In this\n"
    append helptext "# latter case, it is possible to use it as a NOTES\n"
    append helptext "# storage place\n\n"
    append helptext "# AVAILABLE VARIABLES\n"
    append helptext "# \$ProjectDir: the directory where the project is\n"
    append helptext "# \$ObjectsDir: the directory where the object files are\n"

    set sw [ScrolledWindow $f0.lf -relief sunken -borderwidth 0 -grid "0 nsew"]
    supertext::text $sw.text -wrap none -syncvar cproject::thisdataS($scripttab,script) \
	    -height 4 -bg white -postproc "cproject::ScriptTabColorize $sw.text"
    $sw setwidget $sw.text

    bind $sw.text <Return> "[bind Text <Return>] ;break"

    foreach i [array names cproject::dataS *,script] {
	if { $cproject::dataS($i) == "help" } {
	    set cproject::dataS($i) $helptext
	}
    }
    foreach i [array names cproject::dataS $debugrelease,*,script] {
	regexp {^[^,]+,(.*)} $i {} prop
	set cproject::thisdataS($prop) $cproject::dataS($i)
    }
    if { $cproject::thisdataS($scripttab,script) == "help" } {
	set cproject::thisdataS($scripttab,script) $helptext
    }

    set bbox [ButtonBox $f0.bbox1 -spacing 0 -padx 1 -pady 1 -homogeneous 1 -grid "0 wn"]
    $bbox add -image acttick16 \
	 -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1 -padx 1 -pady 1 \
	 -helptext [_ "Clear text and add help"] \
	 -command "[list $sw.text del 1.0 end] ; [list $sw.text ins end $helptext]"
    $bbox add -image actcross16 \
	 -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1 -padx 1 -pady 1 \
	 -helptext [_ "Clear text"] \
	 -command [list $sw.text del 1.0 end]

    TitleFrame $f.f1 -text "execute when" -grid "0 n"
    set f1 [$f.f1 getframe]

    set values [list "Before compile" "After compile"]
    foreach i $links {
	lappend values "After $i"
    }
    lappend values --- "No automatic"

    label $f1.l -text "Execute script:" -grid 0
    ComboBox $f1.cb -editable 0 -textvariable cproject::thisdataS($scripttab,executetime) \
	    -values $values -grid 1

    button $f1.b -text "Execute now" -width 15 -grid "0 2 py3" -command \
	    "cproject::syncfromUI; cproject::EvalScript $f \$cproject::debugrelease \
	    [list $scripttab] 1"
    
    set bbox [ButtonBox $f.bbox -spacing 0 -padx 1 -pady 1 -homogeneous 1 -grid "0 nw"]
    $bbox add -image acttick16 \
	 -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1 -padx 1 -pady 1 \
	 -helptext [_ "Create new script tab"] \
	 -command [list cproject::CreateDeleteScriptTab $scripttab create]
    $bbox add -image actcross16 \
	 -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1 -padx 1 -pady 1 \
	 -helptext [_ "Delete script tab"] \
	 -command [list cproject::CreateDeleteScriptTab $scripttab delete]
    $bbox add -image edit16 \
	 -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1 -padx 1 -pady 1 \
	 -helptext [_ "Rename script tab"] \
	 -command [list cproject::CreateDeleteScriptTab $scripttab rename]


    supergrid::go $f0
    supergrid::go $f1
    supergrid::go $f
}

proc cproject::CreateDeleteScriptTab { currentscripttab what } {
    variable scripttabs
    variable dataS
    variable notebook

    syncfromUI

    switch $what {
	create {
	    set num [expr [llength $scripttabs]+1]
	    set newscripttab Script$num
	    foreach i [array names dataS *,$currentscripttab,*] {
		regexp {([^,]+),[^,]+,([^,]+)} $i {} dr v
		set dataS($dr,$newscripttab,$v) $dataS($i)
	    }
	    lappend scripttabs $newscripttab
	    $notebook raise $newscripttab
	}
	delete {
	    if { [llength $scripttabs] == 1 } {
		WarnWin "Error: There must be at least one Script tab" $notebook
		return
	    }
	    set ret [DialogWin::messageBox -default ok -icon warning -message \
		"Are you sure to delete script tab '$currentscripttab'?" -parent $notebook \
		-title "delete script tab" -type okcancel]
	    if { $ret == "cancel" } { return }
	    
	    set delpos [lsearch $scripttabs $currentscripttab]

	    foreach i [array names dataS *,$currentscripttab,*] {
		unset dataS($i)
	    }
	    set scripttabs [lreplace $scripttabs $delpos $delpos]
	    if { $delpos >= [llength $scripttabs] } { set delpos 0 }
	    $notebook raise [lindex $$scripttabsd $delpos]
	}
	rename {
	    CopyNamespace ::DialogWin ::DialogWinCR
	    set f [DialogWinCR::Init $notebook "Enter script tab name" separator ""]
	    set w [winfo toplevel $f]
	    
	    label $f.l -text "Enter new name for script tab '$currentscripttab'" -grid "0 px3 py3"
	    entry $f.e -textvariable DialogWinCR::user(name) -grid "0 px10 py3" -width 30
	    
	    set DialogWinCR::user(name) $currentscripttab
	    tkTabToWindow $f.e
	    supergrid::go $f
	    bind $w <Return> "DialogWinCR::InvokeOK"

	    set action [DialogWinCR::CreateWindow]
	    while 1 {
		switch $action {
		    0 {
		        DialogWinCR::DestroyWindow
		        namespace delete ::DialogWinCR
		        return
		    }
		    1 {
		        set newscripttab [string trim $DialogWinCR::user(name)]
		        if { $newscripttab == "" } {
		            set newscripttab "Script"
		        } elseif { ![string match "Script *" $newscripttab] } {
		            set newscripttab "Script $newscripttab"
		        }
		        if { $newscripttab == "" } {
		            WarnWin "Script tab name cannot be void" $w
		        } elseif { [lsearch $scripttabs $newscripttab] != -1 } {
		            WarnWin "Script tab name '$newscripttab' already exists" $w
		        } else {
		            DialogWinCR::DestroyWindow
		            namespace delete ::DialogWinCR
		            break
		        }
		    }
		}
		set action [DialogWinCR::WaitForWindow]
	    }
	    set pos [lsearch $scripttabs $currentscripttab]
	    foreach i [array names dataS *,$currentscripttab,*] {
		regexp {([^,]+),[^,]+,([^,]+)} $i {} dr v
		set dataS($dr,$newscripttab,$v) $dataS($i)
		unset dataS($i)
	    }
	    set scripttabs [lreplace $scripttabs $pos $pos $newscripttab]
	}
    }
    synctoUI
}

proc cproject::AreFilesEqual { file1 file2 } {
    
    if { $::tcl_platform(platform) == "windows" } {
	return [string equal -nocase $file1 $file2]
    } else {
	return [string equal $file1 $file2]
    }
}

proc cproject::ConvertToRelative { dir file } {

    set list1 [file split $dir]
    set list2 [file split $file]

    for { set i 0 } { $i < [llength $list2] } { incr i } {
	if { ![AreFilesEqual [lindex $list1 $i] [lindex $list2 $i]] } {
	    break
	}
    }
    set listres ""
    for { set j $i } { $j < [llength $list1] } { incr j } {
	lappend listres ..
    }
    for { set j $i } { $j < [llength $list2] } { incr j } {
	lappend listres [lindex $list2 $j]
    }
    if { $listres == "" } { return . }
    return [eval file join $listres]
}

proc cproject::IsProjectNameOk {} {
    variable project
    variable notebook

    if { [info exists notebook] && [winfo exists notebook] } {
	set w [winfo toplevel $notebook]
    } else { set w . }

    if { [string trim $project] == "" } {
	WarnWin "Define a project name before entering data" $w
	return -code return
    }
    if { [file pathtype $project] != "absolute" } {
	set project [file join [pwd] $project]
	if { ![file isdir [file dirname $project]] } {
	    WarnWin "Project pathname is not correct" $w
	    return -code return
	}
    }
    return [file dirname $project]
}

proc cproject::AddModFiles { listbox what } {
    variable project
    variable files
    variable group

    set projectdir [IsProjectNameOk]

    switch $what {
	"view" {
	    if { [llength [$listbox curselection]] != 1 } {
		WarnWin "Error: Select just one file to see it" $listbox
		return
	    }
	    foreach "file_in type group_in path" [lindex $files [$listbox curselection]] break
	    set file [file join [file dirname $project] $path $file_in]
	    RamDebugger::OpenFileF [RamDebugger::filenormalize $file]
	}
	"file" {
	    set types {
		{{C Source Files} {.c .cc .h} }
		{{All Files} * }
	    }
	    set file [tk_getOpenFile -filetypes $types -initialdir $projectdir -parent $listbox \
		-title "Insert file into project"]
	    if { $file == "" } { return }
	    set file [ConvertToRelative $projectdir $file]

	    foreach i $files {
		foreach "file_in type group_in path" $i break
		if { [AreFilesEqual $file $file_in] } {
		    WarnWin "Error: file '$file' is already in the project" $listbox
		    return
		}
	    }
	    lappend files [list [file tail $file] [string trimleft [file ext $file] .] $group \
		[file dirname $file]]
	}
	"dir" {
	    set dir [RamDebugger::filenormalize [tk_chooseDirectory -initialdir $projectdir \
		-parent $listbox \
		-title "Insert files from directory into project" -mustexist 1]]
	    if { $dir == "" } { return }
	    
	    set fileslist ""
	    foreach i $files {
		foreach "file_in type group_in path" $i break
		lappend fileslist $file_in
	    }
	    set num 0
	    foreach i [glob -nocomplain -dir $dir *.c *.cc] {
		set file [ConvertToRelative $projectdir $i]
		if { [RamDebugger::lsearchfile $fileslist $file] != -1 } { continue }
		lappend files [list [file tail $file] [string trimleft [file ext $file] .] $group \
		     [file dirname $file]]
		incr num
	    }
	    WarnWin "Inserted $num new files" $listbox
	}
	edit {
	    set num 0
	    set numdiff 0
	    foreach i [$listbox curselection] {
		foreach "file_in type group_in path" [lindex $files $i] break
		if { $group != $group_in } { incr numdiff }
		set files [lreplace $files $i $i [list $file_in $type $group $path]]
		incr num
	    }
	    WarnWin "Replaced group to $num files ($numdiff new)"
	}
	delete {
	    set num 0
	    foreach i [$listbox curselection] {
		set ipos [expr $i-$num]
		set files [lreplace $files $ipos $ipos]
		incr num
	    }
	    $listbox selection clear 0 end
	    WarnWin "Deleted from project $num files"
	}
    }
}

proc cproject::AddDelDirectories { listbox what } {

    set projectdir [IsProjectNameOk]

    set dir [IsProjectNameOk]
    switch $what {
	add {
	    set dir [RamDebugger::filenormalize [tk_chooseDirectory -initialdir $dir \
		    -parent $listbox -title "Add directories" -mustexist 1]]
	    if { $dir == "" } { return }
	    $listbox insert end [ConvertToRelative $projectdir $dir]
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


################################################################################
#   execution
################################################################################


proc cproject::GiveDebugData {} {
    variable project
    variable dataE

    set dr $RamDebugger::options(debugrelease)

    if { [info exists dataE($dr,exe)] } {
	set objdir [file root $project]_$dr
	set exe [file join $objdir $dataE($dr,exe)]
	return [list $exe $dataE($dr,execdir) \
		$dataE($dr,exeargs)]
    }
    return ""
}


proc cproject::TryToFindPath { file } {
    variable project
    variable files

    set last_path ""

    set base_dir [file dirname $project]

    if { [file exists [file join $base_dir $file]] } {
	return [file join $base_dir $file]
    }

    foreach i $files {
	foreach "file_in type group_in path" $i break
	if { $path == $last_path } { continue }

	if { [file exists [file join $base_dir $path $file]] } {
	    return [file join $base_dir $path $file]
	}
	set last_path $path
    }
    return ""
}

proc cproject::ScanHeaders { file } {

    set fin [open $file r]
    set aa [read $fin]
    close $fin

    set headers ""
    foreach "- header" [regexp -inline -all {\#include\s+\"([^\"]+)\"} $aa] {
	set file [file join [file dirname $file] $header]
	if { [file exist $file] } {
	    lappend headers [file join [file dirname $file] $header]
	}
    }
    return $headers
}

proc cproject::CleanCompiledFiles { w } {
    variable project
    variable files
    variable dataC
    variable dataL
    variable dataE

    RamDebugger::SetMessage "Cleaning compilation files..."
    RamDebugger::WaitState 1

    if { $project == "" } {
	if { [info exists RamDebugger::options(recentprojects)] && \
		[llength $RamDebugger::options(recentprojects)] > 0 } {
	    set project [lindex $RamDebugger::options(recentprojects) 0]
	    set err [catch { cproject::OpenProject $w 0 }]
	    if { $err } { set project "" }
	}
	if { $project == "" } {
	    cproject::Create $w
	    return
	}
    }

    set dr $RamDebugger::options(debugrelease)

    if { $dr == "both" } {
	WarnWin "error: program must be in debug or in release mode"
	RamDebugger::WaitState 0
	return
    }
    set objdir [file join [file dirname $project] [file root $project]_$dr]

    foreach i [glob -nocomplain -dir $objdir *] {
	file delete $i
    }
    RamDebugger::TextCompClear
    RamDebugger::TextCompRaise
    RamDebugger::TextCompInsert "Compilation files deleted"

    RamDebugger::WaitState 0
    RamDebugger::SetMessage "Cleaning compilation files...done"
}

proc cproject::printfilename { filename } {
    regsub -all " " $filename {\\ } filename
    return $filename
}

proc cproject::TouchFiles { w } {
    variable project
    variable files
    variable dataC
    variable dataL
    variable dataE

    set dr $RamDebugger::options(debugrelease)

    RamDebugger::SetMessage "Actualizing date for compilation files..."
    RamDebugger::WaitState 1

    if { $project == "" } {
	if { [info exists RamDebugger::options(recentprojects)] && \
		[llength $RamDebugger::options(recentprojects)] > 0 } {
	    set project [lindex $RamDebugger::options(recentprojects) 0]
	    set err [catch { cproject::OpenProject $w 0 }]
	    if { $err } { set project "" }
	}
	if { $project == "" } {
	    cproject::Create $w
	    return
	}
    }
    if { $dr == "both" } {
	WarnWin "error: program must be in debug or in release mode"
	RamDebugger::WaitState 0
	return
    }
    set objdir [file join [file dirname $project] [file root $project]_$dr]

    set time [clock seconds]
    foreach i [glob -nocomplain -dir $objdir *] {
	file mtime $i $time
    }

    RamDebugger::TextCompClear
    RamDebugger::TextCompRaise
    RamDebugger::TextCompInsert "Actualized date for compilation files"

    RamDebugger::WaitState 0
    RamDebugger::SetMessage "Actualizing date for compilation files...done"
}

proc cproject::CompileAll { w } {

    RamDebugger::TextCompClear
    foreach i [list debug release] {
	set retval [CompileDo $w $i 1 ""]
	if { $retval == -1 } { break }
    }
}

proc cproject::Compile { w { unique_file "" } } {

    set dr $RamDebugger::options(debugrelease)

    RamDebugger::TextCompClear
    CompileDo $w $dr 0 $unique_file
}

proc cproject::CompileNoStop { w } {

    set dr $RamDebugger::options(debugrelease)

    RamDebugger::TextCompClear
    CompileDo $w $dr 1 ""
}

proc cproject::CompileDo { w debrel nostop { unique_file "" } } {
    variable project
    variable files
    variable dataC
    variable dataL
    variable dataE
    variable links
    variable compilationstatus

    if { $project == "" } {
	if { [info exists RamDebugger::options(recentprojects)] && \
		[llength $RamDebugger::options(recentprojects)] > 0 } {
	    set project [lindex $RamDebugger::options(recentprojects) 0]
	    set err [catch { cproject::OpenProject $w 0 }]
	    if { $err } { set project "" }
	}
	if { $project == "" } {
	    cproject::Create $w
	    return -1
	}
    }

    if { $debrel != "debug" && $debrel != "release" } {
	WarnWin "error: program must be in debug or in release mode"
	return -1
    }
    if { [auto_execok gcc] == "" } {
	set ret [DialogWin::messageBox -default yes -icon question -message \
	    "Could not find command 'gcc'. Do you want to see the help?" -parent $w \
	    -title "Command not found" -type yesno]
	if { $ret == "yes" } {
	    RamDebugger::ViewHelpForWord "Debugging c++"
	    #RamDebugger::ViewHelpFile "01RamDebugger/RamDebugger_12.html"
	}
	return -1
    }
    $RamDebugger::mainframe setmenustate debugentry disabled
    $RamDebugger::mainframe setmenustate c++entry disabled
    $RamDebugger::mainframe setmenustate activeconfiguration disabled

    set menu [$RamDebugger::mainframe getmenu c++]
    $menu add separator
    $menu add command -label "Stop compiling" -command \
       "set ::cproject::compilationstatus 2"

    set pwd [pwd]
    cd [file dirname $project]

    set objdir [file tail [file root $project]]_$debrel
    if { ![file exists $objdir] } { file mkdir $objdir }

    set cproject::compilationstatus -1

    set catch_err [catch {
	if { $unique_file != "" } {
	    set found 0
	    set unique_file [RamDebugger::filenormalize $unique_file]
	    foreach i $files {
		foreach "file_in type group_in path" $i break
		set file_in2 [RamDebugger::filenormalize [file join [file dirname $project] \
		        $path $file_in]]
		if { [string equal $file_in2 $unique_file] } {
		    set compfiles [list $i]
		    set found 1
		    break
		}
	    }
	    if { !$found } {
		WarnWin "error: file '$unique_file' is not included in the compile project"
		set cproject::compilationstatus 1
		set compfiles ""
	    }
	    set forcecompile 1
	    set project_short "$file_in $debrel"
	} else {
	    set compfiles $files
	    set forcecompile 0
	    set project_short "[file root [file tail $project]] $debrel"
	}
	
	RamDebugger::TextCompRaise
	RamDebugger::TextCompInsert "[string repeat - 20]Compiling $project_short"
	RamDebugger::TextCompInsert "[string repeat - 20]\n"
	update

	set make [file join $objdir Makefile.ramdebugger]
	if { $unique_file != "" } { append make 1 }

	set fout [open $make w]
	
	puts -nonewline  $fout "\n# Makefile  -*- makefile -*- "
	puts $fout "Created: [clock format [clock seconds] -format {%Y-%m-%d %H:%M:%S}]"
	puts $fout "\n[string repeat # 80]"
	puts $fout "# Makefile automatically made by RamDebugger"
	puts $fout "#     execute it from the upper directory"
	puts $fout "[string repeat # 80]\n"

	if { $unique_file == "" } {
	    puts -nonewline $fout "all: "
	    foreach link $links {
		puts -nonewline $fout "[file join $objdir $dataL($debrel,$link,linkexe)] "
	    }
	    puts $fout "\n"
	}

	foreach i $compfiles {
	    foreach "file_in type group_in path" $i break

	    if { [string trim $dataC($group_in,$debrel,compiler)] == "" } { continue }

	    set file [file join $path $file_in]
	    set objfile [file join $objdir [file root $file_in].o]
	    
	    if { $forcecompile && [file exists $objfile] } {
		file delete $objfile
	    }
	    set dependencies [ScanHeaders $file]
	    puts -nonewline $fout "[printfilename $objfile]: [printfilename $file]"
	    foreach i $dependencies {
		puts -nonewline $fout " [printfilename $i]"
	    }
	    puts -nonewline $fout "\n\t$dataC($group_in,$debrel,compiler) "
	    foreach j $dataC($group_in,$debrel,flags) {
		puts -nonewline $fout "$j "
	    }
	    foreach j $dataC($group_in,$debrel,includedirs) {
		if { [string first " " $j] == -1 } {
		    puts -nonewline $fout "-I$j "
		} else {
		    puts -nonewline $fout "-I\"$j\" "
		}
	    }
	    foreach j $dataC($group_in,$debrel,defines) {
		puts -nonewline $fout "-D$j "
	    }
	    puts -nonewline $fout "\\\n\t\t-o [printfilename $objfile] "
	    puts $fout "[printfilename $file]\n"
	}
	if { $unique_file == "" } {
	    foreach link $links {
		set objfiles ""
		foreach i $files {
		    foreach "file_in type group_in path" $i break
		    if { [lsearch $dataL($debrel,$link,linkgroups) $group_in] != -1 || \
		            [lsearch $dataL($debrel,$link,linkgroups) All] != -1 } {
		        if { [file ext $file_in] == ".rc" } {
		            lappend objfiles [file join $path $file_in]
		        } else {
		            lappend objfiles [file join $objdir [file root $file_in].o]
		        }
		    }
		}
		if { [string trim $dataL($debrel,$link,linkexe)] == "" } {
		    set tt "WARNING: no output name for linking target '$link'. "
		    append tt "assuming 'program_$link.exe'\n"
		    RamDebugger::TextCompInsertRed $tt
		    set outputname [file join $objdir program_$link.exe]
		} else {
		    set outputname [file join $objdir $dataL($debrel,$link,linkexe)]
		}

		set target [string toupper OBJFILES_$link]
		set string "$target = "
		foreach i $objfiles {
		    append string "$i "
		    if { [string length $string] > 70 } {
		        puts $fout "$string \\"
		        set string ""
		    }
		}
		puts $fout "$string\n"

		puts $fout "$outputname: \$($target)"
		puts -nonewline $fout "\t$dataL($debrel,$link,linker) "
		foreach j $dataL($debrel,$link,linkflags) {
		    puts -nonewline $fout  "$j "
		}
		puts -nonewline $fout "-o $outputname \$($target) "
		foreach j $dataL($debrel,$link,librariesdirs) {
		    if { $dataL($debrel,$link,linker) != "windres" } {
		        if { [string first " " $j] == -1 } {
		            puts -nonewline $fout "-L$j "
		        } else {
		            puts -nonewline $fout "-L\"$j\" "
		        }
		    } else {
		        if { [string first " " $j] == -1 } {
		            puts -nonewline $fout "--include $j "
		        } else {
		            puts -nonewline $fout "--include \"$j\" "
		        }
		    }
		}
		if { $dataL($debrel,$link,linker) != "windres" } {
		    puts -nonewline $fout "-L$objdir "
		}
		foreach j $dataL($debrel,$link,libraries) {
		    if { [regexp {^lib(.*)(\.a|\.lib|\.so)$} $j {} j2] } {
		        puts -nonewline $fout "-l$j2 "
		    } elseif { [regexp {^lib(.*)} $j {} j2] } {
		        puts -nonewline $fout "-l$j2 "
		    } elseif { [file exists $j] } {
		        puts -nonewline $fout "$j "
		    } elseif { [regexp {(?i)(.*)\.lib} $j {} j2] } {
		        puts -nonewline $fout "-l$j2 "
		    } else {
		        puts -nonewline $fout "[file join $objdir $j] "
		    }
		}
		puts $fout "\n"
	    }
	}
	close $fout

	if { $::tcl_platform(platform) == "windows" } {
	    set comm ""
	    #set comm [auto_execok start]
	    #lappend comm  /w /m
	    #lappend comm  /WAIT /MIN
	} else { set comm "" }

	lappend comm make
	if { $nostop } { lappend comm -k }
	lappend comm -f $make

	if { $nostop } {
	    RamDebugger::TextCompInsert "make -k -f $make\n"
	} else {
	    RamDebugger::TextCompInsert "make -f $make\n"
	}
	set fin [open "|$comm |& cat" r]
	
	fconfigure $fin -blocking 0
	fileevent $fin readable [list cproject::CompileFeedback $fin]
	
	vwait cproject::compilationstatus
	
	if { $compilationstatus == 2 } {
	    if { $::tcl_platform(platform) == "windows" } {
		# maybe it kills also other compilations from other RamDebugger's or manual make's
		# but that's live and that's windows
		foreach i [split [exec tlist] \n] {
		    if { [string match -nocase "*make.exe*" $i] } {
		        catch { exec kill /f [scan $i %d] }
		    }
		}
	    }
	    catch { close $fin }
	}
    } catch_string]
	
    if { $catch_err } {
	WarnWin $catch_string
	set compilationstatus 1
    }

    switch -- $compilationstatus {
	-1 {
	    RamDebugger::TextCompInsert "Project '$project_short' is up to date"
	}
	0 {
	    RamDebugger::TextCompInsert "[string repeat - 20]Ending compilation of "
	    RamDebugger::TextCompInsert "$project_short"
	    RamDebugger::TextCompInsert "[string repeat - 20]\n"
	}
	1 {
	    RamDebugger::TextCompInsert "[string repeat - 20]Failing compilation of $project_short"
	    RamDebugger::TextCompInsert "[string repeat - 20]\n"
	    update
	}
	2 {
	    RamDebugger::TextCompInsert "[string repeat - 20]compilation of $project_short stopped "
	    RamDebugger::TextCompInsert "at user demand[string repeat - 20]\n"
	    
	    update
	}
    }
    cd $pwd

    $menu delete end
    $menu delete end
    $RamDebugger::mainframe setmenustate c++entry normal
    $RamDebugger::mainframe setmenustate debugentry normal
    $RamDebugger::mainframe setmenustate activeconfiguration normal

    if { $compilationstatus == 2 } {
	return -1
    } else { return 0 }
}

proc cproject::CompileFeedback { fin} {
    variable compilationstatus

    if { [eof $fin] } {
	set err [catch { close $fin } errstring]
	if { $err } {
	    RamDebugger::TextCompInsert $errstring\n
	    set compilationstatus 1
	} else {
	    set compilationstatus 0
	}
	return
    }
    gets $fin aa

    if { $aa != "" } {
	RamDebugger::TextCompInsert $aa\n
	update
    }
}

proc cproject::EvalScript { w debrel scripttab { show 0 } } {
    variable project
    variable dataS

    set script $dataS($debrel,$scripttab,script)
    regsub -all {(?n)^\s*\#.*$} $script "" res
    if { [string trim $res] == "" } {
	if { $show } {
	    WarnWin "Nothing to execute in Script '$scripttab'" $w
	}
	return
    }

    set pwd [pwd]
    cd [file dirname $project]

    set ProjectDir [file dirname $project]
    set ObjectsDir [file tail [file root $project]]_$debrel

    rename ::puts ::___puts
    proc ::puts args {
	set argsN $args
	set hasnewline 1
	if { [lindex $argsN 0] == "-nonewline" } {
	    set hasnewline 0
	    set argsN [lrange $argsN 1 end]
	}
	set channelId stdout
	if { [llength $argsN] == 2 } {
	    set channelId [lindex $argsN 0]
	    set argsN [lrange $argsN 1 end]
	}
	if { [llength $argsN] == 1 && [regexp {stdout|stderr} $channelId] } {
	    RamDebugger::TextCompInsert [lindex $argsN 0]
	    if { $hasnewline } { RamDebugger::TextCompInsert \n }
	} else {
	    uplevel ___puts $args
	}
    }

    set err [catch $script errstring]

    rename ::puts ""
    rename ::___puts ::puts

    if { $err } {
	RamDebugger::TextCompRaise
	RamDebugger::TextCompInsertRed "-----Error executing script '$scripttab'-----\n"
	RamDebugger::TextCompInsertRed $::errorInfo\n
	if { $show } {
	    WarnWin "error executing script '$scripttab' ($errstring)" $w
	}
    } else {
	RamDebugger::TextCompRaise
	RamDebugger::TextCompInsert "Executed script '$scripttab'. Result: \n$errstring\n"
	if { $show } {
	    WarnWin "Executed script '$scripttab'. Result: $errstring" $w
	}
    }
    cd $pwd
}


