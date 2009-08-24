
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

    foreach i $list {
	$f.lf insert end $i
    }
    $f.lf selection add 1
    $f.lf activate 1
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

    foreach i $list {
	$f.lf insert end $i
    }
    $f.lf selection add 1
    $f.lf activate 1
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

    if { [auto_execok cvs] eq "" } { return }

    set cvs_indicator_frame $f
    ttk::label $f.l1 -text CVS:
    ttk::label $f.l2 -width 3
    ttk::label $f.l3 -width 3
    
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
    variable cvs_indicator_data
    variable cvs_indicator_frame
    
    set currentfile $RamDebugger::currentfile
    
    if { [auto_execok cvs] eq "" } { return }
    
    set f $cvs_indicator_frame
    if { [regexp {^\*.*\*$} $currentfile] } {
	$f.l2 configure -image ""
	tooltip::tooltip $f.l2 [_ "No CVS information"]
	$f.l3 configure -image ""
	tooltip::tooltip $f.l3 [_ "No CVS information"]
	return
    }
    if { [info exists cvs_indicator_fileid] } {
	catch { close $cvs_indicator_fileid }
	unset cvs_indicator_fileid
    }
    foreach i [list 1 2 3] {
	raise $f.l$i
    }
    set pwd [pwd]
    cd [file dirname $currentfile]
    set cvs_indicator_data ""
    set cvs_indicator_fileid [open "|cvs -n update" r]
    fconfigure $cvs_indicator_fileid -blocking 0
    fileevent $cvs_indicator_fileid readable [list RamDebugger::CVS::indicator_update_do]
    cd $pwd
}

proc RamDebugger::CVS::indicator_update_do {} {
    variable cvs_indicator_fileid
    variable cvs_indicator_data
    variable cvs_indicator_frame
    
    set f $cvs_indicator_frame
    set currentfile $RamDebugger::currentfile
    set cfile [file tail $currentfile]
    set cdir [file tail [file dirname $currentfile]]
    
    if { ![info exists cvs_indicator_fileid] } { return }
    append cvs_indicator_data "[gets $cvs_indicator_fileid]\n"
    
    if { ![eof $cvs_indicator_fileid] } { return }
    catch { close $cvs_indicator_fileid }
    unset cvs_indicator_fileid
    
    set files ""
    set currentfile_mode ""
    foreach line [split $cvs_indicator_data \n] {
	if { ![regexp {^\s*(\w)\s+(\S.*)} $line {} mode file] } { continue }
	lappend files $line
	if { $file eq $cfile } {
	    set currentfile_mode $mode
	}
    }
    switch -- $currentfile_mode {
	"" {
	    $f.l2 configure -image ""
	    tooltip::tooltip $f.l2 [_ "CVS up to date for current file '%s'" $cfile]
	}
	M {
	    $f.l2 configure -image up-16
	    tooltip::tooltip $f.l2 [_ "It is necessary to COMMIT current file '%s'" $cfile]
	}
	default {
	    $f.l2 configure -image down-16
	    tooltip::tooltip $f.l2 [_ "CVS is NOT up to date for current file '%s'" $cfile]
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
	tooltip::tooltip $f.l3 [_ "CVS up to date for current directory '%s'" $cdir]
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
    foreach cmd [list update_recursive_do0 select_directory update_recursive_do1 \
	    update_recursive_accept update_recursive_cmd] {
	set full_cmd RamDebugger::CVS::$cmd
	append script "[list proc $cmd [info_fullargs $full_cmd] [info body $full_cmd]]\n"
    }
    append script "[list namespace eval RamDebugger ""]\n"
    append script "[list set RamDebugger::MainDir $RamDebugger::MainDir]\n"
    append script "[list lappend ::auto_path {*}$::auto_path]\n"
    append script "[list update_recursive_do0 $directory $current_or_last]\n"
    
    if { $::tcl_platform(os) ne "Darwin" && $::tcl_platform(threaded) } {
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
    #package require compass_utils

    wm withdraw .
    
    destroy ._ask
    set w [dialogwin_snit ._ask -title [_ "CVS update recursive"] -entrytext \
	    [_ "Select origin directory for CVS update recursive:"] \
	    -okname [_ View] -morebuttons [list [_ "Update CVS"]] \
	    -cancelname [_ Close] -grab 0 -callback [list update_recursive_do1]]
    set f [$w giveframe]
    
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
    $w set_uservar_value messages [dict_getd $dict messages ""]
    
    ttk::label $f.l1 -text [_ "Directory"]:
    cu::combobox $f.e1 -textvariable [$w give_uservar dir ""] -valuesvariable \
	[$w give_uservar directories] -width 60
    ttk::button $f.b1 -text F -command [namespace code [list select_directory $w]] \
	-style Toolbutton

    package require fulltktree
    set columns [list [list 100 [_ "line"] left item 0]]
    fulltktree $f.toctree -height 350 \
	-columns $columns -expand 0 \
	-selectmode extended -showheader 1 -showlines 0  \
	-indent 0 -sensitive_cols all \
	-contextualhandler_menu [list "update_recursive_cmd" $w contextual]
    $w set_uservar_value tree $f.toctree
    
    ttk::label $f.l2 -text [_ "Commit messages"]:
    cu::combobox $f.e2 -textvariable [$w give_uservar message ""] -valuesvariable \
	[$w give_uservar messages] -width 60

    grid $f.l1 $f.e1 $f.b1 -sticky w -padx 2 -pady 2
    grid $f.toctree - - -sticky nsew
    grid $f.l2 $f.e2 - -sticky w -padx 2 -pady 2
    grid configure $f.e1 $f.e2 -sticky ew
    grid columnconfigure $f 1 -weight 1
    grid rowconfigure $f 2 -weight 1
    
    $w set_uservar_value dir $dir
    $w set_uservar_value message ""
    
    tk::TabToWindow $f.e1
    bind [winfo toplevel $f] <Return> [list $w invokeok]
    $w createwindow
}

proc RamDebugger::CVS::select_directory { w } {
    set dir [tk_chooseDirectory -initialdir [$w give_uservar_value dir] \
	    -mustexist 1 -parent $w -title [_ "Select origin directory"]]
    if { $dir eq "" } { return }
    $w set_uservar_value dir $dir
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
    update_recursive_accept $what $dir $tree 0
}

proc RamDebugger::CVS::update_recursive_accept { what dir tree itemP { item "" } } {
    
    if { $item ne "" } {
	foreach i [$tree item children $item] { $tree item delete $i }
    }
    if { [file exists [file join $dir CVS]] } {
	set olddir [pwd]
	cd $dir
	if { $what eq "view" } {
	    set err [catch { exec cvs -n -q update 2>@1 } ret]
	} else {
	    set err [catch { exec cvs -q update 2>@1 } ret]
	}
	cd $olddir
	foreach line [split $ret \n] {
	    if { ![winfo exists $tree] } { return }
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
    } else {
	if { $item ne "" } { set itemP $item }
	foreach d [glob -nocomplain -dir $dir -type d *] {
	    update_recursive_accept $what $d $tree $itemP
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
	    $menu add command -label [_ "Commit"] -command \
		[list "update_recursive_cmd" $w commit $tree $sel_ids]
	    $menu add command -label [_ "Update view"] -command \
		[list "update_recursive_cmd" $w update view $tree $sel_ids]
	    $menu add command -label [_ "Update CVS"] -command \
		[list "update_recursive_cmd" $w update update $tree $sel_ids]
	    $menu add separator
	    $menu add command -label [_ "CVS add"] -command \
		[list "update_recursive_cmd" $w add $tree $sel_ids]
	    $menu add command -label [_ "CVS add binary"] -command \
		[list "update_recursive_cmd" $w add_binary $tree $sel_ids]
	    $menu add separator
	    $menu add command -label [_ "View diff"] -command \
		[list "update_recursive_cmd" $w open_program tkdiff $tree $sel_ids]
	    $menu add command -label [_ "Open tkcvs"] -command \
		[list "update_recursive_cmd" $w open_program tkcvs $tree $sel_ids]
	    $menu add separator
	    foreach i [list all normal modified] t [list [_ All] [_ Normal] [_ Modified]] {
		$menu add command -label [_ "View %s" $t] -command \
		    [list "update_recursive_cmd" $w view $tree 0 $i]
	    }
	}
	commit {
	    lassign $args tree sel_ids
	    set message [$w give_uservar_value message]
	    foreach item $sel_ids {
		if { ![regexp {^[MA]\s(\S+)} [$tree item text $item 0] {} file] } { continue }
		set dir [$tree item text [$tree item parent $item] 0]
		set pwd [pwd]
		cd $dir
		set err [catch { exec cvs commit -m $message $file 2>@1 } ret]
		$tree item element configure $item 0 e_text_sel -fill blue -text $ret
		cd $pwd
	    }
	    $w set_uservar_value messages [linsert0 [$w give_uservar_value messages] $message]
	    set dict [cu::get_program_preferences -valueName cvs_update_recursive RamDebugger]
	    dict set dict messages [$w give_uservar_value messages]
	    cu::store_program_preferences -valueName cvs_update_recursive RamDebugger $dict
	}
	add - add_binary {
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
		    set txt [_ "Are you user to add as TEXT file %d files? (%s)" [llength $files] $filesT]
		    set kopt ""
		}
		add_binary {
		    set txt [_ "Are you user to add as BINARY file %d files? (%s)" [llength $files] $filesT]
		    set kopt [list -kb]
		}
	    }
	    set ret [snit_messageBox -icon question -title [_ "Add files"] -type okcancel \
		    -default ok -parent $w -message $txt]
	    if { $ret eq "cancel" } { return }

	    foreach item $sel_ids {
		if { ![regexp {^\?\s(\S+)} [$tree item text $item 0] {} file] } { continue }
		set dir [$tree item text [$tree item parent $item] 0]
		set pwd [pwd]
		cd $dir
		set err [catch { exec cvs add -m $message {*}$kopt $file 2>@1 } ret]
		$tree item element configure $item 0 e_text_sel -fill blue -text $ret
		cd $pwd
	    }
	    $w set_uservar_value messages [linsert0 [$w give_uservar_value messages] $message]
	    set dict [cu::get_program_preferences -valueName cvs_update_recursive RamDebugger]
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
		update_recursive_accept $what_in $dir $tree [$tree item parent $item] $item
	    }
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
		    if { [regexp {^[A-Z]\s|^cvs} [$tree item text $item 0]] } {
		        set visible 1
		    }
		}
		modified {
		    if { [regexp {^M\s} [$tree item text $item 0]] } {
		        set visible 1
		    }
		}
	    }
	    $tree item configure $item -visible $visible
	}
	open_program {
	    lassign $args what_in tree sel_ids
	    foreach item $sel_ids {
		if { ![regexp {^[A-Z]\s(\S+)} [$tree item text $item 0] {} file] } { continue }
		set dir [$tree item text [$tree item parent $item] 0]
		set file [file join $dir $file]
		if { [file exists $file] } {
		    switch $what_in {
		        tkdiff {
		            set pwd [pwd]
		            cd [file dirname $file]
		            set file [file tail $file]
		            RamDebugger::OpenProgram tkdiff -r $file
		            cd $pwd
		        }
		        tkcvs {
		            if { [file isdirectory $file] } {
		                RamDebugger::OpenProgram tkcvs -dir $file
		            } else {
		                RamDebugger::OpenProgram tkcvs -dir [file dirname $file]
		            }
		        }
		    }
		    return
		}
	    }
	}
	default {
	    error "error in update_recursive_cmd"
	}
    } 
}











