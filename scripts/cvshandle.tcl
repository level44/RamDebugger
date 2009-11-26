
package require textutil
package require base64
package require sha1

namespace eval RamDebugger::CVS {
    variable cvsrootdir
    variable cvsworkdir
    variable null
    variable lasttimeautosave ""
    variable autosave_after ""
    variable autosaveidle_after ""
}

proc RamDebugger::CVS::Init {} {
    variable cvsrootdir
    variable cvsworkdir
    variable null

    if { [auto_execok cvs] eq "" } {
	error "error: It is necessary to have program 'cvs' in the path"
    }

    unset -nocomplain ::env(CVSROOT)
    unset -nocomplain ::env(CVS_RSH)

    set cvsrootdir [file join $RamDebugger::AppDataDir cvsroot]
    set cvsworkdir  [file join $RamDebugger::AppDataDir cvswork]
    if { $::tcl_platform(platform) eq "windows" } {
	set null NUL:
    } else {
	set null /dev/null
    }
    if { ![file exists cvsworkdir] } {
	file mkdir $cvsworkdir
    }
    if { ![file exists $cvsrootdir] } {
	set pwd [pwd]
	cd $cvsworkdir
	exec cvs -d :local:$cvsrootdir init
	exec cvs -d :local:$cvsrootdir import -m "" cvswork RamDebugger start
	cd ..
	exec cvs -d :local:$cvsrootdir checkout cvswork 2> $null
	    
#             cd cvswork
#             exec cvs -d :local:$cvsrootdir checkout CVSROOT 2> $null
#             set fout [open [file join CVSROOT modules] a]
#             puts $fout "\n#M\tcvswork\tSupport for RamDebugger file changes"
#             puts $fout "cvswork RamDebugger/cvswork"
#             close $fout
#             cd CVSROOT
#             exec cvs commit -m "" modules
#             cd ..
#             file delete -force CVSROOT

	cd $pwd
    }
}

proc RamDebugger::CVS::SetUserActivity {} {
    variable autosaveidle_after

    if { $autosaveidle_after ne "" } {
	after cancel $autosaveidle_after
	set time [expr {int($RamDebugger::options(AutoSaveRevisions_idletime)*1000)}]
	set autosaveidle_after [after $time RamDebugger::CVS::_ManageAutoSaveDo]
    }
}

proc RamDebugger::CVS::ManageAutoSave {} {
    variable lasttimeautosave
    variable autosave_after
    variable autosaveidle_after

    after cancel $autosave_after
    after cancel $autosaveidle_after
    set autosaveidle_after ""

    if { ![info exists RamDebugger::options(AutoSaveRevisions)] || \
	     $RamDebugger::options(AutoSaveRevisions) == 0 } {
	return
    }
    set now [clock seconds]
    if { $lasttimeautosave eq "" } { set lasttimeautosave $now }
    if { $now-$lasttimeautosave < $RamDebugger::options(AutoSaveRevisions_time) } {
	set time [expr {int(($RamDebugger::options(AutoSaveRevisions_time)-$now+\
		                 $lasttimeautosave)*1000)}]
	set $autosave_after [after $time RamDebugger::CVS::ManageAutoSave]
    } else {
	set time [expr {int($RamDebugger::options(AutoSaveRevisions_idletime)*1000)}]
	set autosaveidle_after [after $time RamDebugger::CVS::_ManageAutoSaveDo]
    }
}

proc RamDebugger::CVS::_ManageAutoSaveDo {} {
    variable lasttimeautosave
    variable autosaveidle_after
    variable autosave_warning

    set autosaveidle_after ""

    if { ![winfo exists $RamDebugger::text] } { return }

    set needsautosave 0
    if { $RamDebugger::currentfileIsModified } { set needsautosave 1 }
    if { ![regexp {^\*.*\*$} $RamDebugger::currentfile] && [file exists $RamDebugger::currentfile] &&\
	     [clock seconds]-[file mtime $RamDebugger::currentfile] < \
	     2*$RamDebugger::options(AutoSaveRevisions_time) } {
	set needsautosave 1
    }
    if { !$needsautosave } {
	set lasttimeautosave ""
	ManageAutoSave
    } else {
	set err [catch {SaveRevision 1} errstring]
	if { $err } {
	    if { ![info exists autosave_warning] } {
		WarnWin "Failed auto saving revisions. Feature disconnected for this session. Reason: $errstring"
		set autosave_warning 1
	    }
	} else {
	    set lasttimeautosave [clock seconds]
	    ManageAutoSave
	}
    }
    indicator_update
}

proc RamDebugger::CVS::SaveRevision { { raiseerror 0 } } {
    variable cvsworkdir
    variable null

    RamDebugger::WaitState 1

    set map [list > "" < ""]
    set file [string map $map $RamDebugger::currentfile]
    
    if { [regexp {^\*.*\*$} $file] } {
	set file [string trim $file *]
	set lfile $file.[sha1::sha1 $file]
    } else {
	set file [file normalize $file]
	set lfile [file tail $file].[sha1::sha1 $file]
    }
    RamDebugger::SetMessage "Saving revision for file '$file'..."

    set err [catch { Init } errstring]
    if { $err } {
	RamDebugger::WaitState 0
	RamDebugger::SetMessage ""
	if { $raiseerror } { error $errstring }
	WarnWin $errstring
	return
    }

    set map [list "\n[string repeat { } 16]" "\n\t\t" "\n[string repeat { } 8]" "\n\t"]
    set data [string map $map [$RamDebugger::text get 1.0 end-1c]]
    RamDebugger::_savefile_only [file join $cvsworkdir $lfile] $data

    set pwd [pwd]
    cd $cvsworkdir
    set err [catch { exec cvs log -R $lfile }]
    if { $err } {
	set err [catch { exec cvs add -ko -m $file $lfile 2> $null } errstring]
	if { $err } {
	    RamDebugger::WaitState 0
	    RamDebugger::SetMessage ""
	    if { $raiseerror } { error $errstring }
	    WarnWin $errstring
	    return
	}
    }
    exec cvs commit -m "" $lfile
    file delete $lfile
    cd $pwd
    RamDebugger::WaitState 0
    RamDebugger::SetMessage "Saved revision for file '$file'"
}

proc RamDebugger::CVS::OpenRevisions { { file "" } } {
    variable cvsworkdir

    RamDebugger::WaitState 1
    
    if { $file eq "" } { set file $RamDebugger::currentfile }
    set map [list > "" < ""]
    set file [string map $map $file]
    
    if { [regexp {^\*.*\*$} $file] } {
	set file [string trim $file *]
	set lfile $file.[sha1::sha1 $file]
    } else {
	set file [file normalize $file]
	set lfile [file tail $file].[sha1::sha1 $file]
    }
    set err [catch { Init } errstring]
    if { $err } {
	RamDebugger::WaitState 0
	WarnWin $errstring
	return
    }

    set pwd [pwd]
    cd $cvsworkdir
    set err [catch { exec cvs log $lfile } retval]
    cd $pwd
    if { $err } {
	RamDebugger::WaitState 0
	WarnWin [_ "File '%s' has no revisions" $file]
	return
    }
    RamDebugger::WaitState 0

    set w $RamDebugger::text._openrev
    dialogwin_snit $w -title [_ "Choose revision"] -entrytext \
	[_ "Choose a revision for file '%s'" $file] -morebuttons [list [_ "Differences"]]
    set f [$w giveframe]

    set list ""
    foreach i [lrange [textutil::splitx $retval {--------+}] 1 end] {
	regexp -line {^revision\s+(\S+)} $i {} revision
	regexp -line {date:\s+([^;]+)} $i {} date
	regexp -line {author:\s+([^;]+)} $i {} author
	set lines ""
	regexp -line {lines:\s+([^;]+)} $i {} lines
	lappend list [list $revision $date $author $lines]
    }
    set columns [list \
	    [list  6 [_ "Rev"] left text 0] \
	    [list 20 [_ "Date"] left text 0] \
	    [list  8 [_ "Author"] center text 0] \
	    [list  5 [_ "Lines"] left text 0] \
	]
    fulltktree $f.lf -width 400 \
	-columns $columns -expand 0 \
	-selectmode extended -showheader 1 -showlines 0  \
	-indent 0 -sensitive_cols all \
	-selecthandler2 "[list $w invokeok];#"

    set num 0
    foreach i $list {
	$f.lf insert end $i
	incr num
    }
    if { $num } {
	$f.lf selection add 1
	$f.lf activate 1
    }
    focus $f.lf

    grid $f.lf -stick nsew
    grid rowconfigure $f 1 -weight 1
    grid columnconfigure $f 0 -weight 1

    set action [$w createwindow]
    while 1 {
	if { $action <= 0 } {
	    destroy $w
	    return
	}
	set selecteditems ""
	foreach i [$f.lf selection get] {
	    lappend selecteditems [$f.lf item text $i]
	}
	if { $action == 1 } {
	    if { [llength $selecteditems] != 1  } {
		WarnWin [_ "Select one revision in order to visualize it"] $w
		destroy $w
		return
	    }
	    set revision [lindex $selecteditems 0 0]
	    cd $cvsworkdir
	    set data [exec cvs -Q update -p -r $revision $lfile]
	    cd $pwd
	    RamDebugger::OpenFileSaveHandler *[file tail $file].$revision* $data ""
	    destroy $w
	    return
	} elseif { [llength $selecteditems] < 1 || [llength $selecteditems] > 2 } {
	    WarnWin [_ "Select one or two revisions in order to visualize the differences"] $w
	} else {
	    cd $cvsworkdir
	    set deletefiles ""
	    if { [llength $selecteditems] == 1 } {
		set revision [lindex $selecteditems 0 0]
		
		set currentfileL $RamDebugger::currentfile
		set map [list > "" < ""]
		set currentfileL [string map $map $currentfileL]
    
		if { [regexp {^\*.*\*$} $currentfileL] } {
		    set currentfileL [string trim $currentfileL *]
		} else {
		    set currentfileL [file normalize $currentfileL]
		}
		if { $file eq $currentfileL } {
		    set map [list "\n[string repeat { } 16]" "\n\t\t" "\n[string repeat { } 8]" "\n\t"]
		    set data [string map $map [$RamDebugger::text get 1.0 end-1c]]
		    set file1 [file tail $file]
		    RamDebugger::_savefile_only $file1 $data
		    lappend deletefiles [file join $cvsworkdir $file1]
		} else {
		    set file1 $file
		}
		set file2 [file tail $file].$revision
		exec cvs -Q update -p -r $revision $lfile > $file2
		lappend deletefiles [file join $cvsworkdir $file2]
	    } else {
		set r1 [lindex $selecteditems 0 0]
		set r2 [lindex $selecteditems 1 0]
		set file1 [file tail $file].$r1
		exec cvs -Q update -p -r $r1 $lfile > $file1
		set file2 [file tail $file].$r2
		exec cvs -Q update -p -r $r2 $lfile > $file2
		lappend deletefiles [file join $cvsworkdir $file1] \
		    [file join $cvsworkdir $file2]
	    }
	    set ex ""
	    set interp diff
	    while { [interp exists $interp] } {
		if { $ex eq "" } { set ex 2} else { incr ex }
		set interp diff$ex
	    }
	    interp create $interp
	    $interp eval package require Tk
	    interp alias $interp exit_interp "" interp delete $interp
	    set cmd "file delete $deletefiles ; exit_interp"
	    $interp eval [list proc exit { args } $cmd]
	    $interp eval [list cd $cvsworkdir]
	    $interp eval [list set argc 2]
	    $interp eval [list set argv [list [file join $cvsworkdir $file1] \
		        [file join $cvsworkdir $file2]]]
	    $interp eval [list source [file join $RamDebugger::MainDir addons tkcvs bin tkdiff.tcl]]
	    cd $pwd
	}
	set action [$w waitforwindow]
    }
}

proc RamDebugger::CVS::_showallfiles_update {} {
    variable cvsrootdir
    variable cvsworkdir
    variable null

    package require fileutil

    set pwd [pwd]
    cd $cvsworkdir
    set err [catch { exec cvs -Q log -t } retcvslog]
    cd $pwd

    set list ""
    set totalsize 0
    foreach i [textutil::splitx $retcvslog {=======+}] {
	set file ""
	if { [string trim $i] eq "" } { continue }
	regexp -line {^RCS file:\s+(.*)} $i {} rcsfile
	regexp -line {^description:\s+(.*)} $i {} file
	set file [string trim $file]
	if { $file eq "" } { continue }
	set size [file size $rcsfile]
	incr totalsize $size
	set size_show [format "%.3g KB" [expr {$size/1024.0}]]
	if { [file exists $file] } {
	    set current_size [file size $file]
	    set current_size_show [format "%.3g KB" [expr {$current_size/1024.0}]]
	} else { set current_size_show "" }
	set dirname [file dirname $file]
	if { $dirname eq "." } { set dirname "" }
	lappend list [list [file tail $file] $dirname $size_show $current_size_show]
    }

    set totalsize_show [format "%.3g MB" [expr {$totalsize/1024.0/1024.0}]]
    if { $totalsize_show < 1 } {
	set totalsize_show [format "%.3g KB" [expr {$totalsize/1024.0}]]
    }
    return [list $list $totalsize_show $retcvslog]
}

proc RamDebugger::CVS::ShowAllFiles {} {
    variable cvsrootdir
    variable cvsworkdir
    variable null

    RamDebugger::WaitState 1
    set err [catch { Init } errstring]
    if { $err } {
	RamDebugger::WaitState 0
	WarnWin $errstring
	return
    }

    set w $RamDebugger::text._openrev
    destroy $w
    dialogwin_snit $w -title [_ "Choose revision file"] -entrytext \
	[_ "Choose a revision file to check its revisions or to remove revisions history"] \
	-morebuttons [list [_ "Remove..."] [_ "Purge..."]] -okname [_ "Revisions"]
    set f [$w giveframe]

    lassign [_showallfiles_update] list totalsize_show retcvslog

    ttk::label $f.lsize -text [_ "Total size of revision storage: %s" $totalsize_show]
    
    set columns [list \
	    [list 15 [_ "File"] left text 0] \
	    [list 50 [_ "Path"] left text 0] \
	    [list 15 [_ "Storage size"] left text 0] \
	    [list 10 [_ "File size"] left text 0] \
	]
    fulltktree $f.lf -width 400 \
	-columns $columns -expand 0 \
	-selectmode extended -showheader 1 -showlines 0  \
	-indent 0 -sensitive_cols all \
	-selecthandler2 "[list $w invokeok];#"

    set num 0
    foreach i $list {
	$f.lf insert end $i
	incr num
    }
    if { $num } {
	$f.lf selection add 1
	$f.lf activate 1
    }
    focus $f.lf

    grid $f.lsize -sticky w
    grid $f.lf -stick nsew
    grid rowconfigure $f 2 -weight 1
    grid columnconfigure $f 0 -weight 1

    RamDebugger::WaitState 0
    set action [$w createwindow]
    while 1 {
	if { $action <= 0 } {
	    destroy $w
	    return
	}
	set selecteditems ""
	foreach i [$f.lf selection get] {
	    lappend selecteditems [$f.lf item text $i]
	}
	if { $action == 1 } {
	    if { [llength $selecteditems] != 1  } {
		WarnWin [_ "Select one file in order to visualize its revisions"]
	    } else {
		destroy $w
		OpenRevisions [file join [lindex $selecteditems 0 1] \
		        [lindex $selecteditems 0 0]]
		return
	    }
	} else {
	    set isgood 1
	    if { [llength $selecteditems] == 0  } {
		if { $action == 2 } {
		    WarnWin [_ "Select one or more files in order to remove the revisions"]
		} else {
		    WarnWin [_ "Select one or more files in order to purge the revisions"]
		}
		set isgood 0
	    } else {
		set len [llength $selecteditems]
		if { $action == 2 } {
		    set title [_ "Remove revisions"]
		    set txt [_ "Are you user to remove revision history for the %s selected files?" $len]
		} else {
		    set title [_ "Purge revisions"]
		    set txt [_ "Are you user to purge revision history for the %s selected files?" $len]
		}
		set ret [snit_messageBox -icon question -title $title -type okcancel \
		        -default ok -parent $w -message $txt]
		if { $ret ne "ok" } { set isgood 0 }
	    }
	    if { $isgood } {
		RamDebugger::WaitState 1
		set pwd [pwd]
		cd $cvsworkdir
		
		foreach i [textutil::splitx $retcvslog {=======+}] {
		    if { [string trim $i] eq "" } { continue }
		    regexp -line {^RCS file:\s+(.*)} $i {} rcsfile
		    regexp -line {^description:\s+(.*)} $i {} file
		    set file [string trim $file]
		    if { $file eq "" } {
		        set lfile [string range [file tail $rcsfile] 0 end-2]
		        exec cvs remove $lfile 2> $null
		        exec cvs commit -m "" $lfile
		        file delete [file join $cvsrootdir cvswork Attic [file tail $rcsfile]]
		        continue
		    }
		    set size [file size $rcsfile]
		    set size_show [format "%.3g KB" [expr {$size/1024.0}]]
		    if { [file exists $file] } {
		        set current_size [file size $file]
		        set current_size_show [format "%.3g KB" [expr {$current_size/1024.0}]]
		    } else { set current_size_show "" }
		    set dirname [file dirname $file]
		    if { $dirname eq "." } { set dirname "" }
		    set key [list [file tail $file] $dirname $size_show $current_size_show]
		    if { [lsearch -exact $selecteditems $key] != -1 } {
		        set lfile [string range [file tail $rcsfile] 0 end-2]
		        if { $action == 3 } {
		            set data [exec cvs -Q update -p $lfile]
		        }
		        exec cvs remove $lfile 2> $null
		        exec cvs commit -m "" $lfile
		        file delete [file join $cvsrootdir cvswork Attic [file tail $rcsfile]]
		        
		        if { $action == 3 } {
		            RamDebugger::_savefile_only [file join $cvsworkdir $lfile] $data
		            exec cvs add -ko -m $file $lfile 2> $null
		            exec cvs commit -m "" $lfile
		            file delete $lfile
		        }
		    }
		}
		cd $pwd
		lassign [_showallfiles_update] list totalsize_show retcvslog
		$f.lsize configure -text [_ "Total size of revision storage: %s" $totalsize_show]
		$sw.lb delete 0 end
		$sw.lb insertlist end $list
		RamDebugger::WaitState 0
	    }
	}
	set action [$w waitforwindow]
    }
}

################################################################################
#    CVS indicator
################################################################################

proc RamDebugger::CVS::indicator_init { f } {
    variable cvs_indicator_frame

    if { [auto_execok cvs] eq "" && [auto_execok fossil] eq "" } { return }

    set cvs_indicator_frame $f
    ttk::label $f.l1 -text VCS:
    ttk::label $f.l2 -width 3
    ttk::label $f.l3 -width 3
    
    tooltip::tooltip $f.l1 [_ "Version control system (CVS or fossil)"]
    
    foreach i [list 1 2 3] {
	#bind $f.l$i <1> [list RamDebugger::OpenProgram tkcvs]
	bind $f.l$i <1> [list RamDebugger::CVS::update_recursive $cvs_indicator_frame last]
	bind $f.l$i <<Contextual>> [list RamDebugger::CVS::indicator_menu $cvs_indicator_frame %X %Y]
    }
    grid $f.l1 $f.l2 $f.l3 -sticky w
}

proc RamDebugger::CVS::indicator_menu { cvs_indicator_frame x y } {

    destroy $cvs_indicator_frame.menu
    set menu [menu $cvs_indicator_frame.menu -tearoff 0]
    $menu add command -label [_ "Open"] -command \
	[list RamDebugger::CVS::update_recursive $cvs_indicator_frame last]
    $menu add command -label [_ "Open - current directory"] -command \
	[list RamDebugger::CVS::update_recursive $cvs_indicator_frame current]
    tk_popup $menu $x $y
}

proc RamDebugger::CVS::indicator_update {} {
    variable cvs_indicator_fileid
    variable fossil_indicator_fileid
    variable cvs_indicator_data
    variable fossil_indicator_data
    variable cvs_indicator_frame
    
    set currentfile $RamDebugger::currentfile
    
    if { ![info exists cvs_indicator_frame] } { return }
    
    set f $cvs_indicator_frame
    if { [regexp {^\*.*\*$} $currentfile] } {
	$f.l2 configure -image ""
	tooltip::tooltip $f.l2 [_ "No CVS or fossil information"]
	$f.l3 configure -image ""
	tooltip::tooltip $f.l3 [_ "No CVS or fossil information"]
	return
    }
    if { [info exists cvs_indicator_fileid] } {
	catch { close $cvs_indicator_fileid }
	unset cvs_indicator_fileid
    }
    if { [info exists fossil_indicator_fileid] } {
	catch { close $fossil_indicator_fileid }
	unset fossil_indicator_fileid
    }
    foreach i [list 1 2 3] {
	raise $f.l$i
    }
    set pwd [pwd]
    cd [file dirname $currentfile]
    set cvs_indicator_data ""
    if { [auto_execok cvs] ne "" && [file isdirectory CVS] } {
	set cvs_indicator_fileid [open "|cvs -n update" r]
	fconfigure $cvs_indicator_fileid -blocking 0
	fileevent $cvs_indicator_fileid readable [list RamDebugger::CVS::indicator_update_do cvs]
    }
    set  fossil_indicator_data ""
    if { [auto_execok fossil] ne "" && [catch { exec fossil info }] == 0 } {
	set fossil_indicator_fileid [open "|fossil changes" r]
	fconfigure $fossil_indicator_fileid -blocking 0
	fileevent $fossil_indicator_fileid readable \
	    [list RamDebugger::CVS::indicator_update_do fossil]
    }
    cd $pwd
}

proc RamDebugger::CVS::indicator_update_do { cvs_or_fossil } {
    variable cvs_indicator_fileid
    variable fossil_indicator_fileid
    variable cvs_indicator_data
    variable fossil_indicator_data
    variable cvs_indicator_frame
    
    set f $cvs_indicator_frame
    set currentfile $RamDebugger::currentfile
    set cfile [file tail $currentfile]
    set cdir [file tail [file dirname $currentfile]]
   
    if { $cvs_or_fossil eq "cvs" } {
	append cvs_indicator_data "[gets $cvs_indicator_fileid]\n"
    } else {
	append fossil_indicator_data "[gets $fossil_indicator_fileid]\n"
    }
    if { [info exists cvs_indicator_fileid] && [eof $cvs_indicator_fileid] } {
	catch { close $cvs_indicator_fileid }
	unset cvs_indicator_fileid
    }
    if { [info exists fossil_indicator_fileid] && [eof $fossil_indicator_fileid] } {
	catch { close $fossil_indicator_fileid }
	unset fossil_indicator_fileid
    }
    if { [info exists cvs_indicator_fileid] ||
	 [info exists fossil_indicator_fileid] } { return }

    set files ""
    set currentfile_mode ""
    foreach line [split $cvs_indicator_data \n] {
	if { ![regexp {^\s*(\w)\s+(\S.*)} $line {} mode file] } { continue }
	if { $file eq $cfile } {
	    set currentfile_mode $mode
	}
	lappend files $line
    }
    foreach line [split $fossil_indicator_data \n] {
	if { ![regexp {(\w+)\s+(.*)} $line {} mode file] } { continue }
	if { [file tail $file] eq $cfile } {
	    set currentfile_mode $mode
	}
	lappend files $line
    }
    switch -- $currentfile_mode {
	"" {
	    $f.l2 configure -image ""
	    tooltip::tooltip $f.l2 [_ "CVS up to date for current file '%s'" $cfile]
	}
	M - EDITED {
	    $f.l2 configure -image up-16
	    tooltip::tooltip $f.l2 [_ "It is necessary to COMMIT current file '%s'" $cfile]
	}
	default {
	    $f.l2 configure -image down-16
	    tooltip::tooltip $f.l2 [_ "CVS or fossil is NOT up to date for current file '%s'" $cfile]
	}
    }
    if { [llength $files] > 10 } {
	set files [lrange $files 0 9]
    }
    if { [llength $files] > 0 } {
	$f.l3 configure -image reload-16
	tooltip::tooltip $f.l3 [join $files \n]
    } else {
	$f.l3 configure -image ""
	tooltip::tooltip $f.l3 [_ "CVS or fossil up to date for current directory '%s'" $cdir]
    }
}

################################################################################
#    proc CVS update recursive
################################################################################

proc RamDebugger::CVS::update_recursive { wp current_or_last } {
    
    if { [file isdirectory [file dirname $RamDebugger::currentfile]] } {
	set directory [file dirname $RamDebugger::currentfile]
    } else {
	set directory ""
    }
    set script ""
    foreach cmd [list update_recursive_do0 select_directory messages_menu clear_entry insert_in_entry \
	    update_recursive_do1 update_recursive_accept update_recursive_cmd] {
	set full_cmd RamDebugger::CVS::$cmd
	append script "[list proc $cmd [info_fullargs $full_cmd] [info body $full_cmd]]\n"
    }
    append script "[list namespace eval RamDebugger ""]\n"
    append script "[list set RamDebugger::MainDir $RamDebugger::MainDir]\n"
    append script "[list lappend ::auto_path {*}$::auto_path]\n"
    append script "[list update_recursive_do0 $directory $current_or_last]\n"
    
    set try_threaded 1
    
    if { $try_threaded eq "debug" } {
	uplevel #0 $script
    } elseif { $try_threaded && $::tcl_platform(os) ne "Darwin" && $::tcl_platform(threaded) } {
	package require Thread
	append script "thread::wait\n"
	thread::create $script
    } else {
	if { ![interp exists update_recursive_intp] } {
	    interp create update_recursive_intp
	}
	update_recursive_intp eval $script
    }
}

proc RamDebugger::CVS::update_recursive_do0 { directory current_or_last } {

    package require dialogwin
    package require tooltip
    #package require compass_utils

    wm withdraw .
    
    destroy ._ask
    set w [dialogwin_snit ._ask -title [_ "VCS management"] \
	    -okname [_ View] -morebuttons [list [_ "Update"]] \
	    -cancelname [_ Close] -grab 0 -callback [list update_recursive_do1]]
    set f [$w giveframe]
    
    ttk::label $f.l0 -text [_ "Select origin directory for CVS or fossil update recursive, then use the contextual menu on the files:"] \
	-wraplength 400 -justify left

    set dict [cu::get_program_preferences -valueName cvs_update_recursive RamDebugger]
    
    set directories [dict_getd $dict directories ""]
    set dir [lindex $directories 0]
    if { $dir eq "" || $current_or_last eq "current" } {
	set dir $directory
    }
    if { $directory ne "" } {
	set directories [linsert0 $directories $directory]
    }
    $w set_uservar_value directories $directories
    $w set_uservar_value messages  [dict_getd $dict messages ""]
    
    if { [info command RamDebugger::CVS::document-open-16] eq "" } {
	image create photo RamDebugger::CVS::document-open-16 -data {
	    R0lGODlhEAAQAPYAAAAAAFVXU1lbV11fW0VdeWFiX2FjX2NlYWdpZWpsaG1vaz9ghj5giTRlpENr
	    nVNxllh2m2F6mmOVzGSWzGWXzGWYzWaYzGiYzWiZzWmZzWqazWuazWubzWqazmybzm2czm6czm6d
	    znCdz3CeznKfz3ai0Hym0paXlZaZkpqcmKampqeppKmrqYGp1IWs1Yiu1omu1omv14qv14uw14yx
	    2JS225u73Zq63py73abC4K3H467H477S6MDAv8bGxcnJyczMys7OztjY18bY6/X19P///8zMzAAA
	    AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
	    AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
	    AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACH5
	    BAEAAEYALAAAAAAQABAAAAeigAEKg4MICAVGiYpGCkGOjz4HAYuJCT0nJykrm5ycKEYIPyoqK0Wm
	    p6crRgaXJ6VEsLFERaoDoqRFREC7u7O1ra+ysLRGArevvELDqgE9EQ3Q0dLQDgE/DUM82tvcQw3N
	    DTs04+TkMDnf1zozNDMyLy4tJiUkON89EDYjIiEgHx4cOmTAUOMbixgcNmjIcMFCBQoTJNx4QIDB
	    tIvQFlDauDEQADs=
	}
	image create photo RamDebugger::CVS::edit-clear-16 -data {
	    R0lGODlhEAAQAPZvAHhIAHlJAHtKAHxLAKsbDaEkAKA2ALg4HYBOAYRSAYZSA4dkAIppAJx/AK1C
	    E7tKKKZpCqlrCqttC7BpF7JyDLJ2C7x6D8F9EMhtM5qFAJyIAJyLAJ2MAJ6NAJ6NAZ+NAJ+NAZ+O
	    AJ+OAZ+OBJ+PBaGIAaCOAaCPAaCPBaGQCKKQCaWUEaubGq6eG7GgHbulKb6uMsCwL8W0Msa1MMa2
	    M8W1Ns29Qcy9Q82+RdexYtrCA9vDBNzEB9zFENzGENzGFd/KJuPLEeXNFOfQGO7XI/PbKvbeL/vk
	    N/zlPP3mPM/BSdLDT9jIT9bIVtzNWN3PXuTSSeTTTOXUTu3aRubWVuHTW+fXW+fYX+LUZ+fZbeja
	    bOzcaOrccuvdd+ved/bYYfvlRP3pUv3qX/PjZf3rYf3qY/3rbP3sa/3sbf3uffvqhP3vhPjqiPvt
	    i/3viv///8zMzAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACH5
	    BAEAAHAALAAAAAAQABAAAAexgAADAgBwhoeIhgIQEoSJjwIUFxECDBsbj4YBChYXCRmXmXAAAggV
	    JTAumKJwCxo2XhMGJxyriBseTFsYBDlcNyoitnAbHS8PB19lZmlsOCQhocQFDmpna2hhYGNdSi0c
	    hw0xbmJkYUlFQz5QWCCHGyttSEdGREI7OlQ0IYgmT1NDgvDQoSOKkxHDNrDQ0oPgDytNUAwzFKLG
	    FSBSssjoMNFQsSVVZqT40PEdhw4cEwUCADs=
	}
	image create photo RamDebugger::CVS::list-add-16 -data {
	    R0lGODlhEAAQAPQAAAAAADRlpH2m13+o14Oq2Iat2ZCz2pK02pS225W325++4LTM5bXM5rbM5rbN
	    5rfO5rvR57zR58DT6MzMzAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACH5BAEA
	    ABMALAAAAAAQABAAAAU94CSOZGmeqBisQToGz9O6EyzTdTyb7Kr3pIAEEonFHIzGrrZIIA6GAmEg
	    UCx7gUIBi4Jtcd5lVwdm4c7nEAA7
	}
    }
    ttk::label $f.l1 -text [_ "Directory"]:
    cu::combobox $f.e1 -textvariable [$w give_uservar dir ""] -valuesvariable \
	[$w give_uservar directories] -width 60
    ttk::button $f.b1 -image RamDebugger::CVS::document-open-16 \
	-command [namespace code [list select_directory $w]] \
	-style Toolbutton
    
    tooltip::tooltip $f.b1 [_ "Select the directory under version control"]
    
    ttk::label $f.l2 -text [_ "Commit messages"]:
    cu::multiline_entry $f.e2 -textvariable [$w give_uservar message ""] -valuesvariable \
	[$w give_uservar messages] -width 60
    bind $f.e2 <Return> "[bind Text <Return>] ; break"
    
    ttk::button $f.b2 -image RamDebugger::CVS::edit-clear-16 \
	-command [namespace code [list clear_entry $w $f.e2]] \
	-style Toolbutton
    
    tooltip::tooltip $f.b2 [_ "Clear the messages entry"]
    
    ttk::menubutton $f.b3 -image RamDebugger::CVS::list-add-16 \
	-menu $f.b3.m -style Toolbutton
    menu $f.b3.m -tearoff 0 -postcommand [namespace code [list messages_menu $w $f.b3.m  $f.e2]] 
    
    package require fulltktree
    set columns [list [list 100 [_ "line"] left item 0]]
    fulltktree $f.toctree -height 350 \
	-columns $columns -expand 0 \
	-selectmode extended -showheader 1 -showlines 0  \
	-indent 0 -sensitive_cols all \
	-contextualhandler_menu [list "update_recursive_cmd" $w contextual]
    $w set_uservar_value tree $f.toctree
    
    grid $f.l0    -         -      -sticky w -padx 2 -pady 2
    grid $f.l1 $f.e1 $f.b1 -sticky w -padx 2 -pady 2
    grid $f.l2 $f.e2 $f.b2 -sticky w -padx 2 -pady 2
    grid   ^         ^   $f.b3 -sticky w -padx 2 -pady 2
    grid $f.toctree - - -sticky nsew
    grid configure $f.e1 $f.e2 -sticky ew
    grid columnconfigure $f 1 -weight 1
    grid rowconfigure $f 4 -weight 1
    
    $w set_uservar_value dir $dir
    $w set_uservar_value message ""
    
    bind $w <Control-d> [list "update_recursive_cmd" $w open_program tkdiff $f.toctree ""]
    bind $w <Control-i> [list "update_recursive_cmd" $w commit $f.toctree ""]
    bind $f.e2 <Control-i> "[bind $w <Control-i>];break"

    tk::TabToWindow $f.e1
    bind $w <Return> [list $w invokeok]
    $w createwindow
}

proc RamDebugger::CVS::messages_menu { w menu entry } {
    
    $menu delete 0 end
    $menu add command -label [_ "Clear message"] -image RamDebugger::CVS::edit-clear-16 \
	-compound left -command [namespace code [list clear_entry $w $entry]] 

    set tree [$w give_uservar_value tree]
    set files ""
    foreach item [$tree selection get] {
	if { [regexp {^[MA]\s(\S+)} [$tree item text $item 0] {} file] } { 
	    lappend files $file
	} elseif { [regexp {(\w{2,})\s+(.*)} [$tree item text $item 0] {} mode file] && $mode ne "UNCHANGED" } {
	    lappend files $file
	}
    }
    if { [llength $files] } {
	$menu add separator
	set insertedList ""
	foreach file $files {
	    for { set i 0 } { $i < [llength [file split $file]] } { incr i } {
		set f [file join {*}[lrange [file split $file] 0 $i]]
		if { $f in $insertedList } { continue }
		set txt "$f: "
		$menu add command -label [_ "Insert '%s'" $txt] -command  \
		    [namespace code [list insert_in_entry $w $entry $txt]] 
		lappend insertedList $f
	    }
	}
    }
    set pwd [pwd]
    cd [$w give_uservar_value dir]
    set err [catch { exec fossil timeline -n 2000 -t t } data]
    cd $pwd
    if { $err } { set data "" }
    lassign "" date full_line lineList
    foreach line [split $data \n] {
	if { [regexp {^===\s*(\S+)\s*===} $line {} date] } {
	    # nothing
	} elseif { [regexp {^\S+\s+(.*)} $line {} l] } {
	    if { $full_line ne "" } {
		lappend lineList [list $date $full_line]
	    }
	    set full_line $l
	} else {
	    append full_line " [string trim $line]"
	}
    }
    if { $full_line ne "" } {
	lappend lineList [list $date $full_line]
    }
    set has_sep 0
    foreach i $lineList {
	lassign $i date txt
	if { [regexp {New ticket\s*(\[\w+\])\s+<i>(.*)</i>} $txt {} ticket message] } {
	    if { !$has_sep } {
		$menu add separator
		set has_sep 1
	    }
	    set txt1 [string range "$ticket $message" 0 100]...
	    set txt2 "$ticket $message"
	    $menu add command -label [_ "Insert ticket '%s'" $txt1] -command  \
		[namespace code [list insert_in_entry $w $entry $txt2]]
	}
    }    
}

proc RamDebugger::CVS::select_directory { w } {
    set dir [tk_chooseDirectory -initialdir [$w give_uservar_value dir] \
	    -mustexist 1 -parent $w -title [_ "Select origin directory"]]
    if { $dir eq "" } { return }
    $w set_uservar_value dir $dir
}

proc RamDebugger::CVS::clear_entry { w entry } {
    $entry delete 1.0 end
    focus $entry
}

proc RamDebugger::CVS::insert_in_entry { w entry txt } {
    tk::TextInsert $entry $txt
    focus $entry
}

proc RamDebugger::CVS::update_recursive_do1 { w } {

    set action [$w giveaction]

    if { $action < 1 } {
	destroy $w
	return
    } elseif { $action == 1 } {
	set what view
    } else {
	set what update
    }
    set dir [$w give_uservar_value dir]
    $w set_uservar_value directories [linsert0 [$w give_uservar_value directories] $dir]
    set dict [cu::get_program_preferences -valueName cvs_update_recursive RamDebugger]
    dict set dict directories [$w give_uservar_value directories]
    cu::store_program_preferences -valueName cvs_update_recursive RamDebugger $dict
    set tree [$w give_uservar_value tree]
    $tree item delete all
    update_recursive_accept $w $what $dir $tree 0
}

proc RamDebugger::CVS::update_recursive_accept { w what dir tree itemP { item "" } } {
    
    if { $item ne "" } {
	foreach i [$tree item children $item] { $tree item delete $i }
    }
    set olddir [pwd]
    cd $dir

    set has_vcs 0
    if { [file exists [file join $dir CVS]] } {
	if { $what eq "view" } {
	    set err [catch { exec cvs -n -q update 2>@1 } ret]
	} else {
	    set err [catch { exec cvs -q update 2>@1 } ret]
	}
	foreach line [split $ret \n] {
	    if { ![winfo exists $tree] } {
		cd $olddir
		return
	    }
	    if { $line eq "cvs server: WARNING: global `-l' option ignored." } { continue }
	    if { $item eq "" } {
		set item [$tree insert end [list $dir] $itemP]
	    }
	    set i [$tree insert end [list "$line"] $item]
	    if { ![regexp {^[A-Z]\s|^cvs} $line] } {
		$tree item configure $i -visible 0
	    }
	    update
	}
	set has_vcs 1
    }
    if { [auto_execok fossil] ne "" && [catch { exec fossil info } info] == 0 } {
	regexp -line {^local-root:\s*(.*)} $info {} dirLocal
	set dirLocal [string trimright $dirLocal /]
	
	cd $dirLocal
	if { $item eq "" || [$tree item text $item 0] ne $dirLocal } {
	    set item [$tree insert end [list $dirLocal] $itemP]
	}
	if { $what eq "view" &&  [update_recursive_cmd $w give_fossil_sync $dir] } {
	    set err [catch { exec fossil pull } ret]
	    if { $err && $ret ne "" } {
		snit_messageBox -message $ret -parent $w
	    }
	}
	set err [catch { exec fossil ls 2>@1 } list_files]
	if { !$err } {
	    set err [catch { exec fossil extras 2>@1 } list_files2]
	    foreach line [split $list_files2 \n] {
		append list_files "\n? $line"
	    }
	}
	if { $what ne "view" } {
	    lassign $what op version
	    switch $op {
		update {
		    set err [catch { exec fossil update 2>@1 } list_files3]
		}
		update_this {
		    set err [catch { exec fossil update $version 2>@1 } list_files3]
		}
		merge_this {
		    set err [catch { exec fossil merge $version 2>@1 } list_files3]
		}
		checkout_this {
		    set err [catch { exec fossil checkout $version 2>@1 } list_files3]
		}
	    }
	    if { !$err } {
		append list_files "\n$list_files3"
	    }
	}
	if { ![$w exists_uservar fossil_timeline_view_more] } {
	    $w set_uservar_value fossil_timeline_view_more 0
	}
	if { [$w give_uservar_value fossil_timeline_view_more] } {
	    set err [catch { exec fossil timeline -n 200 -t ci } timeline]
	} else {
	    set err [catch { exec fossil timeline descendants current } timeline]
	}
	if { !$err } {
	    set itemT [$tree insert end [list [_ Timeline]] $item]
	    lassign "" date line_full
	    foreach line [split $timeline \n] {
		if { [regexp {===\s*(\S+)\s*===} $line {} date] } {
		    # nothing
		} elseif { [regexp {^(\S+)\s+(.*)} $line {} time l] } {
		    if { $line_full ne "" } {
		        $tree insert end [list $line_full] $itemT
		    }
		    set d [clock scan "$date $time" -timezone :UTC]
		    set dateF [clock format $d -format "%Y-%m-%d %H:%M:%S"]
		    set line_full "$dateF $l"
		} else {
		    append line_full " [string trim $line]"
		}
	    }
	    if { $line_full ne "" } {
		$tree insert end [list $line_full] $itemT
	    }
	    $tree item collapse $itemT
	}
	foreach line [split $list_files \n] {
	    # this is to avoid problems with update
	    if { ![winfo exists $tree] } {
		cd $olddir    
		return
	    }
	    if { [regexp {^Total network traffic:} $line] } {  continue }
	    if { $item eq "" } {
		set item [$tree insert end [list $dir] $itemP]
	    }
	    set i [$tree insert end [list "$line"] $item]
	    if { ![regexp {^(\w+)\s+(.*)} $line {} mode file] || $mode eq "UNCHANGED" } {
		$tree item configure $i -visible 0
	    }
	    update
	}
	set has_vcs 1
    }
    cd $olddir

    if { !$has_vcs } {
	if { $item ne "" } { set itemP $item }
	foreach d [glob -nocomplain -dir $dir -type d *] {
	    update_recursive_accept $w $what $d $tree $itemP
	}
    }
    if { $item ne "" } {
	set num 0
	foreach i [$tree item children $item] {
	    if { [$tree item cget $i -visible] } { incr num }
	}
	if { !$num } {
	    $tree item configure $item -visible 0
	}
    }
}

proc RamDebugger::CVS::update_recursive_cmd { w what args } {
    
    switch $what {
	contextual {
	    lassign $args tree menu id sel_ids
	    lassign "0 0 0 0 0 0" has_cvs has_fossil cvs_active fossil_active can_be_added is_timeline
	    foreach item $sel_ids {
		set txt [$tree item text $item 0]
		if { [regexp {^\d+} $txt] && [$tree item text [$tree item parent $item] 0] eq [_ "Timeline"] } {
		    set is_timeline 1
		} elseif { [regexp {^\s*(\w)\s+} $txt] } {
		    set has_cvs 1
		    set cvs_active 1
		} elseif  { [regexp {^\s*\w{2,}\s*} $txt] } {
		    set has_fossil 1
		    set fossil_active 1
		} elseif { [regexp {^\s*\?\s+} $txt] } {
		    set can_be_added 1
		}
	    }
	    set pwd [pwd]
	    cd [$w give_uservar_value dir]
	    if { [auto_execok cvs] ne "" && [file isdirectory CVS] } {
		set cvs_active 1
	    }
	    if { [auto_execok fossil] ne "" && [catch { exec fossil info }] == 0 } {
		set fossil_active 1
	    }
	    cd $pwd
	    if { !$is_timeline && ($has_cvs || $has_fossil) } {
		$menu add command -label [_ "Commit"] -accelerator Ctrl-i -command \
		    [list "update_recursive_cmd" $w commit $tree $sel_ids]
	    }
	    $menu add command -label [_ "Refresh view"] -command \
		[list "update_recursive_cmd" $w update view $tree $sel_ids]
	    $menu add command -label [_ "Update to last"] -command \
		[list "update_recursive_cmd" $w update update $tree $sel_ids]
	    
	    if { $is_timeline } {
		$menu add command -label [_ "Update to this"] -command \
		    [list "update_recursive_cmd" $w apply_version update_this $tree $sel_ids]
		$menu add command -label [_ "Merge to this"] -command \
		    [list "update_recursive_cmd" $w apply_version merge_this $tree $sel_ids]
		$menu add command -label [_ "Checkout to this"] -command \
		    [list "update_recursive_cmd" $w apply_version checkout_this $tree $sel_ids]
		$menu add separator
		$menu add checkbutton -label [_ "View more timeline"] -variable \
		    [$w give_uservar fossil_timeline_view_more] -command \
		    [list "update_recursive_cmd" $w toggle_fossil_timeline_view_more $tree]
		if { ![$w exists_uservar fossil_timeline_view_more] } {
		    $w set_uservar_value fossil_timeline_view_more 0
		}
	    }
	    $menu add separator
	    if { $can_be_added } {
		if { $cvs_active } {
		    $menu add command -label [_ "CVS add"] -command \
		        [list "update_recursive_cmd" $w add $tree $sel_ids]
		    $menu add command -label [_ "CVS add binary"] -command \
		        [list "update_recursive_cmd" $w add_binary $tree $sel_ids]
		}
		if { $fossil_active } {
		    $menu add command -label [_ "Fossil add"] -command \
		        [list "update_recursive_cmd" $w add_fossil $tree $sel_ids]
		}
		if { $cvs_active || $fossil_active } {
		    $menu add separator
		}
	    }
	    if { !$is_timeline && ($has_cvs || $has_fossil) } {
		$menu add command -label [_ "View diff"] -accelerator Ctrl-d -command \
		    [list "update_recursive_cmd" $w open_program tkdiff $tree $sel_ids]
	    }
	    if { $cvs_active } {
		$menu add command -label [_ "Open tkcvs"] -command \
		    [list "update_recursive_cmd" $w open_program tkcvs $tree $sel_ids]
	    }
	    if { $fossil_active } {
		if { !$is_timeline && $has_fossil } {
		    $menu add command -label [_ "View diff window"]... -command \
		        [list "update_recursive_cmd" $w diff_window $tree $sel_ids]
		}
		$menu add command -label [_ "Open fossil browser"] -command \
		    [list "update_recursive_cmd" $w open_program fossil_ui $tree $sel_ids]
		$menu add separator
		$menu add checkbutton -label [_ "Fossil autosync"] -variable \
		    [$w give_uservar fossil_autosync] -command \
		    [list "update_recursive_cmd" $w fossil_toggle_autosync]
		$menu add command -label [_ "Fossil syncronize"] -command \
		    [list "update_recursive_cmd" $w fossil_syncronize sync $tree $sel_ids]
		$menu add command -label [_ "Fossil pull"] -command \
		    [list "update_recursive_cmd" $w fossil_syncronize pull $tree $sel_ids]
		$w set_uservar_value fossil_autosync [update_recursive_cmd $w give_fossil_sync]
	    }
	    $menu add separator
	    foreach i [list all normal] t [list [_ All] [_ Normal]] {
		$menu add command -label [_ "View %s" $t] -command \
		    [list "update_recursive_cmd" $w view $tree 0 $i]
	    }
	}
	give_fossil_sync {
	    lassign $args dir
	    if { $dir eq "" } { set dir [$w give_uservar_value dir] }
	    set autosync 0
	    set pwd [pwd]
	    cd $dir
	    set err [catch { exec fossil settings autosync } ret]
	    if { !$err } {
		regexp {(\d)\s*$} $ret {} autosync
	    }
	    cd $pwd
	    return $autosync
	}
	set_fossil_sync {
	    lassign $args autosync
	    set pwd [pwd]
	    cd [$w give_uservar_value dir]
	    set err [catch { exec fossil settings autosync $autosync } ret]
	    if { !$err } {
		snit_messageBox -message $ret -parent $w
	    }
	    cd $pwd
	}
	fossil_toggle_autosync {
	    switch -- [update_recursive_cmd $w give_fossil_sync] {
		0 { update_recursive_cmd $w set_fossil_sync 1 }
		default { update_recursive_cmd $w set_fossil_sync 0 }
	    }
	    $w set_uservar_value fossil_autosync [update_recursive_cmd $w give_fossil_sync]
	}
	toggle_fossil_timeline_view_more {
	    lassign $args tree
	    # toggle is already made by the menu checkbutton
	    set dir [$w give_uservar_value dir]
	    $tree item delete all
	    update_recursive_accept $w view $dir $tree 0
	}
	fossil_syncronize {
	    lassign $args sync_type
	    set pwd [pwd]
	    cd [$w give_uservar_value dir]
	    set err [catch { exec fossil $sync_type } ret]
	    if { $ret ne "" } {
		snit_messageBox -message $ret -parent $w
	    }
	    cd $pwd
	    
	    set dir [$w give_uservar_value dir]
	    set tree [$w give_uservar_value tree]
	    $tree item delete all
	    update_recursive_accept $w view $dir $tree 0
	}
	commit {
	    lassign $args tree sel_ids
	    set message [$w give_uservar_value message]
	    if { [string trim $message] eq "" } { set message "" }
	    
	    if { $message eq "" } {
		set txt [_ "Commit message is void. Proceed?"]
		set ret [snit_messageBox -icon question -title [_ "commit"] -type okcancel \
		        -default cancel -parent $w -message $txt]
		if { $ret eq "cancel" } { return }
	    }
	    if { [llength $sel_ids] == 0 } {
		set sel_ids [$tree selection get]
		if { [llength $sel_ids] == 0 } { return }
		set files ""
		foreach item $sel_ids {
		    if { [regexp {^[MA]\s(\S+)} [$tree item text $item 0] {} file] } { 
		        lappend files $file
		    } elseif { [regexp {(\w{2,})\s+(.*)} [$tree item text $item 0] {} mode file] && $mode ne "UNCHANGED" } {
		        lappend files $file
		    }
		}
		if { [llength $files] > 8 } {
		    set files_p [join [lrange $files 0 7] ","]...
		} else {
		    set files_p [join $files ","]
		}
		set txt [_ "Are you sure to commit %d files? (%s)" [llength $files] $files_p]
		set ret [snit_messageBox -icon question -title [_ "commit"] -type okcancel \
		        -default ok -parent $w -message $txt]
		if { $ret eq "cancel" } { return }
	    }
	    set pwd [pwd]
	    lassign "" cvs_files_dict fossil_files_dict items
	    foreach item $sel_ids {
		set dir [$tree item text [$tree item parent $item] 0]
		if { [regexp {^[MA]\s(\S+)} [$tree item text $item 0] {} file] } { 
		    dict lappend cvs_files_dict $dir $file
		    dict set items cvs $dir $file $item
		} elseif { [regexp {(\w{2,})\s+(.*)} [$tree item text $item 0] {} mode file] && $mode ne "UNCHANGED" } {
		    dict lappend fossil_files_dict $dir $file
		    dict set items fossil $dir $file $item
		}
	    }
	    dict for "dir fs" $cvs_files_dict {
		cd $dir
		set err [catch { exec cvs commit -m $message {*}$fs 2>@1 } ret]
		if { $err } { set color red } else { set color blue }
		foreach file $fs {
		    set item [dict get $items cvs $dir $file]
		    $tree item element configure $item 0 e_text_sel -fill $color -text $ret
		}
	    }
	    dict for "dir fs" $fossil_files_dict {
		cd $dir
		set info [exec fossil info]
		regexp -line {^local-root:\s*(.*)} [exec fossil info] {} dirF
		cd $dirF
		set err [catch { exec fossil commit --nosign -m $message {*}$fs 2>@1 } ret]
		if { $err } { set color red } else { set color blue }
		foreach file $fs {
		    set item [dict get $items fossil $dir $file]
		    $tree item element configure $item 0 e_text_sel -fill $color -text $ret
		}
	    }
	    cd $pwd
	    if { $err } {
		snit_messageBox -message $ret -parent $w
	    }
	    set dict [cu::get_program_preferences -valueName cvs_update_recursive RamDebugger]
	    $w set_uservar_value messages [linsert0 -max_len 20 [dict_getd $dict messages ""] $message]
	    dict set dict messages [$w give_uservar_value messages]
	    cu::store_program_preferences -valueName cvs_update_recursive RamDebugger $dict
	}
	add - add_binary - add_fossil {
	    lassign $args tree sel_ids
	    set message [$w give_uservar_value message]
	    set files ""
	    foreach item $sel_ids {
		if { ![regexp {^\?\s(\S+)} [$tree item text $item 0] {} file] } { continue }
		lappend files $file
	    }
	    if { [string length $files] < 100 } {
		set filesT $files
	    } else {
		set filesT "[lindex $files 0] ... [lindex $files end]"
	    }
	    switch $what {
		add {
		    set txt [_ "Are you sure to add to cvs as TEXT file %d files? (%s)" [llength $files] $filesT]
		    set kopt ""
		}
		add_binary {
		    set txt [_ "Are you sure to add to cvs as BINARY file %d files? (%s)" [llength $files] $filesT]
		    set kopt [list -kb]
		}
		add_fossil {
		    set txt [_ "Are you sure to add to fossil %d files? (%s)" [llength $files] $filesT]
		}
	    }
	    set ret [snit_messageBox -icon question -title [_ "Add files"] -type okcancel \
		    -default ok -parent $w -message $txt]
	    if { $ret eq "cancel" } { return }

	    set pwd [pwd]
	    foreach item $sel_ids {
		if { ![regexp {^\?\s(\S+)} [$tree item text $item 0] {} file] } { continue }
		set dir [$tree item text [$tree item parent $item] 0]
		cd $dir
		if { $what in "add add_binary" } {
		    set err [catch { exec cvs add -m $message {*}$kopt $file 2>@1 } ret]
		} else {
		    set info [exec fossil info]
		    regexp -line {^local-root:\s*(.*)} [exec fossil info] {} dir
		    cd $dir
		    set err [catch { exec fossil add $file 2>@1 } ret]
		}
		if { $err } { break }
		$tree item element configure $item 0 e_text_sel -fill blue -text $ret
	    }
	    cd $pwd
	    set dict [cu::get_program_preferences -valueName cvs_update_recursive RamDebugger]
	    $w set_uservar_value messages [linsert0 -max_len 20 [dict_getd $dict messages ""] $message]
	    dict set dict messages [$w give_uservar_value messages]
	    cu::store_program_preferences -valueName cvs_update_recursive RamDebugger $dict
	}
	update {
	    lassign $args what_in tree sel_ids
	    set ids ""
	    foreach item $sel_ids {
		if { [$tree item children $item] ne "" } {
		    lappend ids $item
		} else {
		    lappend ids [$tree item parent $item]
		}
	    }
	    foreach item [lsort -unique $ids] {
		set dir [$tree item text $item 0]
		update_recursive_accept $w $what_in $dir $tree [$tree item parent $item] $item
	    }
	}
	apply_version {
	    lassign $args what_in tree sel_ids
	    if  { [llength $sel_ids] > 1 } {
		snit_messageBox -message [_ "Select only one version in the timeline"] -parent $w
		return
	    }
	    set item [lindex $sel_ids 0]
	    if { ![regexp {\[(\w{10})\]} [$tree item text $item 0] {} version] } {
		snit_messageBox -message [_ "Select one version in the timeline"] -parent $w
		return
	    }
	    set dir [$w give_uservar_value dir]
	    $tree item delete all
	    update_recursive_accept $w [list $what_in $version] $dir $tree 0
	}
	view {
	    lassign $args tree item view_style
	    set visible 0
	    foreach i [$tree item children $item] {
		update_recursive_cmd $w view $tree $i $view_style
		if { [$tree item cget $i -visible] } { set visible 1 }
	    }
	    if { $item == 0 } { return }
	    switch $view_style {
		all { set visible 1 }
		normal {
		    if { [regexp {^\s*([A-Z]|cvs|\w+)\s} [$tree item text $item 0] {} mode] && $mode ne "UNCHANGED" } {
		        set visible 1
		    }
		}
	    }
	    $tree item configure $item -visible $visible
	}
	open_program {
	    lassign $args what_in tree sel_ids
	    if { $sel_ids eq "" } {
		set sel_ids [$tree selection get]
	    }
	    switch $what_in {
		tkdiff {
		    set fileF ""
		    set pwd [pwd]
		    set errList ""
		    foreach item $sel_ids {
		        if { ![regexp {^(\w+)\s+(\S+)} [$tree item text $item 0] {} mode file] } { continue }
		        set dir [$tree item text [$tree item parent $item] 0]
		        
		        if { [string length $mode] == 1 } {
		            set fileF [file join $dir $file]
		        } else {
		            cd $dir
		            set err [catch {
		                    set info [exec fossil info]
		                    regexp -line {^local-root:\s*(.*)} [exec fossil info] {} dirF
		                } ret]
		            if { $err } {
		                snit_messageBox -message $ret -parent $w
		                return
		            }
		            set fileF [file join $dirF $file]
		        }
		        if { [file exists $fileF] } {
		            if { [string length $mode] == 1 } {
		                cd [file dirname $fileF]
		                RamDebugger::OpenProgram -new_interp 1 tkdiff -r [file tail $fileF]
		            } else {
		                file rename -force $fileF $fileF.current
		                set err [catch {
		                        cd $dirF
		                        exec fossil revert $file
		                        file rename -force $fileF $fileF.trunk
		                    } ret]
		                file rename -force $fileF.current $fileF
		                if { !$err } {
		                    set err [catch { RamDebugger::OpenProgram -new_interp 1 tkdiff $fileF $fileF.trunk } ret]
		                    file delete -force $fileF.trunk
		                }
		                if { $err } {
		                    lappend errList $ret
		                }
		            }
		        }
		    }
		    if { [llength $errList] } {
		        if { [llength $errList] > 5 } {
		            set errstr "[join [range $errList 0 4] ,]...[_ {More errors}]"
		        } else {
		            set errstr "[join $errList ,]"
		        }
		        snit_messageBox -message $errstr -parent $w
		    }
		    cd $pwd
		}
		tkcvs {
		    RamDebugger::OpenProgram tkcvs -dir [$w give_uservar_value dir]
		}
		fossil_ui {
		    set dirs ""
		    foreach item $sel_ids {
		        if { [$tree item text [$tree item parent $item] 0] eq [_ "Timeline"] } { continue }
		        lappend dirs [$tree item text [$tree item parent $item] 0]
		    }
		    lappend dirs [$w give_uservar_value dir]
		    set pwd [pwd]
		    foreach dir $dirs {
		        cd $dir
		        if { [catch { exec fossil info }] } { continue }
		        exec fossil ui &
		        break
		    }
		    cd $pwd
		}
	    }
	}
	diff_window {
	    lassign $args tree sel_ids
	    if { $sel_ids eq "" } {
		set sel_ids [$tree selection get]
	    }
	    if { [llength $sel_ids] > 1 } {
		snit_messageBox -message [_ "Differences window can only be used with one file"] -parent $w
		return
	    }
	    foreach item $sel_ids {
		if { ![regexp {^(\w+)\s+(\S+)} [$tree item text $item 0] {} mode file] } { return }
		set dir [$tree item text [$tree item parent $item] 0]
		break
	    }
	    set pwd [pwd]
	    cd $dir
	    set err [catch { exec fossil finfo $file } data]
	    cd $pwd
	    if { $err } {
		snit_messageBox -message [_ "Fossil version is too old. It needs subcommand 'finfo'. Please, upgrade"] \
		    -parent $w
		return
	    }
	   lassign "" lines line
	    foreach l [split $data \n] {
		if { [regexp {^\d} $l] } {
		    if { $line ne "" } {
		        lappend lines $line
		    }
		    set line $l
		} elseif { [regexp {^\s} $l] } {
		    append line " [string trim $l]"
		}
	    }
	    if { $line ne "" } {
		lappend lines $line
	    }
	    set wD $w.diffs
	    destroy $wD
	    dialogwin_snit $wD -title [_ "Choose version"] -entrytext \
		[_ "Choose one or two versions for file '%s'" $file] -okname [_ View] -cancelname [_ Close] \
		-grab 1 -transient 1 -callback [list "update_recursive_cmd" $w diff_window_accept $dir $file]
	    set f [$wD giveframe]
	    
	    set columns [list \
		    [list  12 [_ "Date"] left text 0] \
		    [list 12 [_ "Checkin"] left text 0] \
		    [list  45 [_ "Text"] center text 0] \
		    [list  9 [_ "User"] left text 0] \
		    [list  12 [_ "Artifact"] left text 0] \
		    ]
	    fulltktree $f.lf -width 750 \
		-columns $columns -expand 0 \
		-selectmode extended -showheader 1 -showlines 0  \
		-indent 0 -sensitive_cols all \
		-selecthandler2 "[list $wD invokeok];#"
	    $wD set_uservar_value tree $f.lf
	    
	    set num 0
	    foreach i $lines {
		regexp {(\S+)\s+\[(\w+)\]\s+(.*)\(user:\s*(\S+),\s*artifact:\s*\[(\w+)\]\s*\)} $i {} date checkin txt user artifact
		$f.lf insert end [list $date $checkin $txt $user $artifact]
		incr num
	    }
	    if { $num } {
		$f.lf selection add 1
		$f.lf activate 1
	    }
	    focus $f.lf
	    
	    grid $f.lf -stick nsew
	    grid rowconfigure $f 1 -weight 1
	    grid columnconfigure $f 0 -weight 1

	    set action [$wD createwindow]
	}
	diff_window_accept {
	    lassign $args dir file wD
	    set action [$wD giveaction] 
	    set tree [$wD give_uservar_value tree]
	    
	    if { $action <= 0 } {
		destroy $wD
		return
	    }
	    set selecteditems ""
	    foreach i [$tree selection get] {
		lappend selecteditems [$tree item text $i]
	    }
	    if { [llength $selecteditems] == 0 } {
		snit_messageBox -message [_ "Select one version to view the differences with current file"] \
		    -parent $wD
	    } elseif { [llength $selecteditems] > 2  } {
		snit_messageBox -message [_ "Select only one or two versions"] \
		    -parent $wD
	    } else {
		lassign [lindex $selecteditems 0] date1 - - - artifact1
		set pwd [pwd]
		cd $dir
		exec fossil artifact $artifact1 $file.$date1
		if { [llength $selecteditems] == 1 } {
		    set err [catch { RamDebugger::OpenProgram -new_interp 1 tkdiff $file $file.$date1 } ret]
		} else {
		    lassign [lindex $selecteditems 1] date2 - - - artifact2
		    exec fossil artifact $artifact2 $file.$date2
		    set err [catch { RamDebugger::OpenProgram -new_interp 1 tkdiff $file.$date1 $file.$date2 } ret]
		}
		if { $err } {
		    snit_messageBox -message $ret -parent $wD
		}
		file delete -force $file.$date1
		if { [llength $selecteditems] == 2 } {
		    file delete -force $file.$date2
		}
		cd $pwd
	    }
	}
	default {
	    error "error in update_recursive_cmd"
	}
    } 
}











