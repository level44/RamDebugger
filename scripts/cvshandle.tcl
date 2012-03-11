
package require Tk 8.5
#package require textutil

namespace eval RamDebugger::VCS {
    variable vcsrootdir
    variable vcsworkdir
    variable cvs_or_fossil fossil
    variable null
    variable lasttimeautosave ""
    variable autosave_after ""
    variable autosaveidle_after ""
    #variable try_threaded debug
    variable try_threaded 1
}

proc RamDebugger::VCS::Init {} {
    variable vcsrootdir
    variable vcsworkdir
    variable null
    variable cvs_or_fossil
    

    if { $cvs_or_fossil eq "cvs" } {
	if { [auto_execok cvs] eq "" } {
	    error "error: It is necessary to have program 'cvs' in the path"
	}
	
	set vcsrootdir [file join $RamDebugger::AppDataDir cvsroot]
	set vcsworkdir [file join $RamDebugger::AppDataDir cvswork]

	unset -nocomplain ::env(CVSROOT)
	unset -nocomplain ::env(CVS_RSH)
	
	if { $::tcl_platform(platform) eq "windows" } {
	    set null NUL:
	} else {
	    set null /dev/null
	}
	if { ![file exists $vcsworkdir] } {
	    file mkdir $vcsworkdir
	}
	if { ![file exists $vcsrootdir] } {
	    set pwd [pwd]
	    cd $vcsworkdir
	    exec cvs -d :local:$vcsrootdir init
	    exec cvs -d :local:$vcsrootdir import -m "" cvswork RamDebugger start
	    cd ..
	    exec cvs -d :local:$vcsrootdir checkout cvswork 2> $null
	    cd $pwd
	}
    } else {
	set fossil [auto_execok fossil]
	if { $fossil eq "" } {
	    set comment "fossil is a version control management system and can be freely download from http://www.fossil-scm.org"
	    error "error: It is necessary to have program 'fossil' in the path ($comment)"
	}
	
	if { [file exists [file join $RamDebugger::AppDataDir cvsroot]] } {
	    file delete -force [file join $RamDebugger::AppDataDir cvsroot]
	}
	if { [file exists [file join $RamDebugger::AppDataDir cvswork]] } {
	    file delete -force [file join $RamDebugger::AppDataDir cvswork]
	}

	set vcsrootdir [file join $RamDebugger::AppDataDir vcsroot]
	set vcsworkdir [file join $RamDebugger::AppDataDir vcswork]

	if { ![file exists $vcsworkdir] } {
	    file mkdir $vcsworkdir
	}
	if { ![file exists $vcsrootdir] } {
	    file mkdir $vcsrootdir
	}
	if { ![file exists [file join $vcsrootdir rep.fossil]] } {
	    exec $fossil new [file join $vcsrootdir rep.fossil]
	    set pwd [pwd]
	    cd $vcsworkdir
	    exec $fossil open [file join $vcsrootdir rep.fossil]
	    exec $fossil settings repo-cksum 0
	    exec $fossil settings mtime-changes 1
	    exec $fossil settings autosync 0
	    cd $pwd
	}
    }
}

proc RamDebugger::VCS::SetUserActivity {} {
    variable autosaveidle_after

    if { $autosaveidle_after ne "" } {
	after cancel $autosaveidle_after
	set time [expr {int($RamDebugger::options(AutoSaveRevisions_idletime)*1000)}]
	set autosaveidle_after [after $time RamDebugger::VCS::ManageAutoSaveDo]
    }
}

proc RamDebugger::VCS::ManageAutoSave {} {
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
	set $autosave_after [after $time RamDebugger::VCS::ManageAutoSave]
    } else {
	set time [expr {int($RamDebugger::options(AutoSaveRevisions_idletime)*1000)}]
	set autosaveidle_after [after $time RamDebugger::VCS::ManageAutoSaveDo]
    }
}

proc RamDebugger::VCS::ManageAutoSaveDo {} {
    variable lasttimeautosave
    variable autosaveidle_after
    variable autosave_warning

    set autosaveidle_after ""

    if { ![winfo exists $RamDebugger::text] } { return }

    if { ![info exists RamDebugger::options(AutoSaveRevisions)] || \
	     $RamDebugger::options(AutoSaveRevisions) == 0 } {
	return
    }

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
	set err [catch {SaveRevision -raise_error 1 } errstring opts]
	if { $err } {
	    if { ![info exists autosave_warning] } {
		#set errstring [dict get $opts -errorinfo]
		WarnWin [_ "Failed auto saving revisions. Feature disconnected for this session. Reason: %s" $errstring]
		set autosave_warning 1
	    }
	} else {
	    set lasttimeautosave [clock seconds]
	    ManageAutoSave
	}
    }
    indicator_update
}

proc RamDebugger::VCS::SaveRevision { args } {
    variable vcsworkdir
    variable null
    variable cvs_or_fossil
    
    set optional {
	{ -raise_error boolean 0 }
    }
    set compulsory ""
   parse_args $optional $compulsory $args
    
    RamDebugger::WaitState 1
    
    set file $RamDebugger::currentfile
    RamDebugger::SetMessage "Saving revision for file '$file'..."
    set pwd [pwd]

    set err [catch { SaveRevisionDo $file } ret opts]
    
    cd $pwd
    RamDebugger::SetMessage ""
    RamDebugger::WaitState 0
    
    if { $err } {
	if { $raise_error } {
	    error $ret [dict get $opts -errorinfo]
	} else {
	    WarnWin $ret
	}
    } else {
	RamDebugger::SetMessage "Saved revision for file '$file'"
    }
}
    
proc RamDebugger::VCS::SaveRevisionDo { args } {
    variable vcsworkdir
    variable vcsrootdir
    variable null
    variable cvs_or_fossil

    set optional {
	{ -data data "" }
    }
    set compulsory "file"
   parse_args $optional $compulsory $args

    package require sha1

    set map [list > "" < ""]
    set file [string map $map $file]
    
    if { [regexp {^\*.*\*$} $file] } {
	set file [string trim $file *]
	set lfile $file.[sha1::sha1 $file]
    } else {
	set file [file normalize $file]
	set lfile [file tail $file].[sha1::sha1 $file]
    }

    Init

    cd $vcsworkdir

    if { $data eq "" } {
	set map [list "\n[string repeat { } 16]" "\n\t\t" "\n[string repeat { } 8]" "\n\t"]
	set data [string map $map [$RamDebugger::text get 1.0 end-1c]]
    }
    RamDebugger::_savefile_only [file join $vcsworkdir $lfile] $data
    
    if { $cvs_or_fossil eq "cvs" } {
	set err [catch { exec cvs log -R $lfile }]
	if { $err } {
	    exec cvs add -ko -m $file $lfile 2> $null
	}
	exec cvs commit -m "" $lfile
    } else {
	set fossil [auto_execok fossil]
	set err [catch { exec $fossil finfo $lfile } ret]
	if { $err } {
	    exec $fossil add $lfile
	    exec $fossil commit -m "\"file://$file\"" $lfile
	} else {
	    set d [exec $fossil diff -i $lfile]
	    lassign [list 0 0] plus less
	    foreach line [split $d \n] {
		if  { [regexp {^\+(?!\+)} $line] } {
		        incr plus
		} elseif  { [regexp {^\-(?!\-)} $line] } {
		        incr less
		} 
	    }
	    if { $plus != 0 || $less != 0 } {
		exec $fossil commit -m "+$plus -$less" $lfile
	    }
	}
	file delete $lfile
    }
}

proc RamDebugger::VCS::OpenRevisions { args } {
    variable vcsworkdir

    set optional {
	{ -file file "" }
    }
    set compulsory ""
   parse_args $optional $compulsory $args

    RamDebugger::WaitState 1
    
    if { $file eq "" } {
	set file $RamDebugger::currentfile
    }
    set pwd [pwd]

    set err [catch { OpenRevisionsInit $file } ret opts]
    
    cd $pwd
    RamDebugger::WaitState 0
    
    if { $err } {
	WarnWin $ret
	return
    }
    lassign $ret lfile finfo
    OpenRevisionsDo $file $lfile $finfo
}

proc RamDebugger::VCS::OpenRevisionsInit { file } {
    variable vcsworkdir
    variable vcsrootdir
    variable cvs_or_fossil

    package require sha1
    
    set map [list > "" < ""]
    set file [string map $map $file]
    
    if { [regexp {^\*.*\*$} $file] } {
	set file [string trim $file *]
	set lfile $file.[sha1::sha1 $file]
    } else {
	set file [file normalize $file]
	set lfile [file tail $file].[sha1::sha1 $file]
    }

    Init
    
    cd $vcsworkdir

    if { $cvs_or_fossil eq "cvs" } {
	set err [catch { exec cvs log $lfile } retval]
    } else {
	set fossil [auto_execok fossil]
	set err [catch { exec $fossil finfo -l -b $lfile } finfo]
	if { !$err } {
	    set retval ""
	    foreach line [split $finfo \n] {
		regexp {(\S+)\s+(\S+)\s+(\S+)\s+(.*)} $line {} revision date author comment
		if { [regexp {^\"file://} $comment] } { set comment "" }
		set ret [exec $fossil timeline $revision -n 1]
		regexp {\d{2}:\d{2}:\d{2}} $ret time
		append date " $time"
		set date [clock format [clock scan $date -timezone :UTC] -format "%Y-%m-%d %H:%M:%S"]
		lappend retval [list $revision $date $author $comment]            
	    }
	}
    }
    if { $err } {
	error [_ "File '%s' has no revisions" $file]
    } else {
	return [list $lfile $retval]
    }
}

proc RamDebugger::VCS::OpenRevisionsDo { file lfile finfo } {
    variable vcsworkdir
    variable cvs_or_fossil

    set w $RamDebugger::text._openrev
    dialogwin_snit $w -title [_ "Choose revision"] -class RamDebugger -entrytext \
	[_ "Choose a revision for file '%s'" $file] -morebuttons [list [_ "Differences"]]
    set f [$w giveframe]

    set list ""
    if { $cvs_or_fossil eq "cvs" } {
	foreach i [lrange [textutil::splitx $finfo {--------+}] 1 end] {
	    regexp -line {^revision\s+(\S+)} $i {} revision
	    regexp -line {date:\s+([^;]+)} $i {} date
	    regexp -line {author:\s+([^;]+)} $i {} author
	    set lines ""
	    regexp -line {lines:\s+([^;]+)} $i {} lines
	    lappend list [list $revision $date $author $lines]
	}
    } else {
	set list $finfo
    }
    set columns [list \
	    [list 12 [_ "Rev"] left text 0] \
	    [list 20 [_ "Date"] left text 0] \
	    [list  8 [_ "Author"] center text 0] \
	    [list  8 [_ "Lines"] left text 0] \
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
	    
	    set pwd [pwd]
	    cd $vcsworkdir
	    set err [catch { _OpenRevisionsDo_open $file $lfile $revision } ret]
	    cd $pwd
	    
	    if { $err } {
		WarnWin [_ "Error opening revision (%s)" $ret] $w
	    } else {
		destroy $w
		return
	    }
	} elseif { [llength $selecteditems] < 1 || [llength $selecteditems] > 2 } {
	    WarnWin [_ "Select one or two revisions in order to visualize the differences"] $w
	} else {
	    cd $vcsworkdir
	    set deletefiles ""
	    if { [llength $selecteditems] == 1 } {
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
		    lappend deletefiles [file join $vcsworkdir $file1]
		} else {
		    set file1 $file
		}
		set rev2 [lindex $selecteditems 0 0]
		set file2 [file tail $file].$rev2
		set err [catch { _OpenRevisionsDo_extract $lfile $rev2 $file2 } ret]
		if { !$err } { lappend deletefiles [file join $vcsworkdir $file2] }
	    } else {
		set rev1 [lindex $selecteditems 0 0]
		set rev2 [lindex $selecteditems 1 0]
		set file1 [file tail $file].$rev1
		set file2 [file tail $file].$rev2
		set err [catch {
		        _OpenRevisionsDo_extract $lfile $rev1 $file1
		        _OpenRevisionsDo_extract $lfile $rev2 $file2
		    } ret]
		if { !$err } {
		    lappend deletefiles [file join $vcsworkdir $file1] [file join $vcsworkdir $file2]
		}
	    }
	    if { $err } {
		WarnWin [_ "Error opening revisions (%s)" $ret] $w
		break
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
	    $interp eval [list cd $vcsworkdir]
	    $interp eval [list set argc 2]
	    $interp eval [list set argv [list [file join $vcsworkdir $file1] \
		        [file join $vcsworkdir $file2]]]
	    $interp eval [list source [file join $RamDebugger::topdir addons tkcvs bin tkdiff.tcl]]

	}
	set action [$w waitforwindow]
    }
}

proc RamDebugger::VCS::_OpenRevisionsDo_open { file lfile revision } {
    variable vcsworkdir
    variable vcsrootdir
    variable cvs_or_fossil
    
    if { $cvs_or_fossil eq "cvs" } {
	set data [exec cvs -Q update -p -r $revision $lfile]
    } else {
	set fossil [auto_execok fossil]
	set data [exec $fossil finfo -p -r $revision $lfile]
	file delete $lfile
    }
    RamDebugger::OpenFileSaveHandler *[file tail $file].$revision* $data ""
}

proc RamDebugger::VCS::_OpenRevisionsDo_extract { lfile revision file } {
    variable vcsworkdir
    variable vcsrootdir
    variable cvs_or_fossil
    
    if { $cvs_or_fossil eq "cvs" } {
	exec cvs -Q update -p -r $revision $lfile > $file
    } else {
	set fossil [auto_execok fossil]
	exec $fossil finfo -p -r $revision $lfile > $file
	file delete $lfile
    }
}

proc RamDebugger::VCS::_ShowAllFiles_remove { rcsfile } {
    variable vcsrootdir
    variable cvs_or_fossil
    
    if { $cvs_or_fossil eq "cvs" } {                  
	set lfile [string range [file tail $rcsfile] 0 end-2]
	catch { exec cvs remove $lfile 2> $null }
	exec cvs commit -m "" $lfile
	file delete [file join $vcsrootdir cvswork Attic [file tail $rcsfile]]
    } else {
	error "RamDebugger::VCS::_ShowAllFiles_remove"
    }
}

proc RamDebugger::VCS::_ShowAllFiles_remove_all {} {
    variable vcsworkdir
    variable vcsrootdir
    variable cvs_or_fossil
    
    file delete -force $vcsworkdir $vcsrootdir
    Init
}

proc RamDebugger::VCS::_showallfiles_update {} {
    variable cvs_or_fossil
    
    if { $cvs_or_fossil eq "cvs" } {
	return [_showallfiles_update_cvs]
    } else {
	return [_showallfiles_update_fossil]
    }
}

proc RamDebugger::VCS::_showallfiles_update_fossil {} {
    variable vcsrootdir
    variable vcsworkdir

    set list ""
    set totalsize 0
    
    set fossil [auto_execok fossil]
    
    set pwd [pwd]
    cd $vcsworkdir

    foreach lfile [exec $fossil ls] {
	set ret [exec $fossil finfo $lfile]
	set txt [string trim [lindex [split $ret \n] end-1]]
	append txt [string trim [lindex [split $ret \n] end]]
	regexp {\"file://(.*?)\"} $txt {} file
	set dirname [file dirname $file]
	if { $dirname eq "." } { set dirname "" }

	if { [file exists $file] } {
	    set current_size [file size $file]
	    set current_size_show [format "%.3g KB" [expr {$current_size/1024.0}]]
	} else {
	    set current_size_show ""
	}
	lappend list [list [file tail $file] 0 $current_size_show $dirname ""]
    }
    
    cd $pwd
    
    set totalsize [file size [file join $vcsrootdir rep.fossil]]
    set totalsize_show [format "%.3g MB" [expr {$totalsize/1024.0/1024.0}]]
    if { $totalsize_show < 1 } {
	set totalsize_show [format "%.3g KB" [expr {$totalsize/1024.0}]]
    }
    return [list $list $totalsize_show]
}

proc RamDebugger::VCS::_showallfiles_update_cvs {} {
    variable vcsrootdir
    variable vcsworkdir
    variable null

    package require fileutil

    set pwd [pwd]
    cd $vcsworkdir
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
	lappend list [list [file tail $file] $size_show $current_size_show $dirname $rcsfile]
    }
    set totalsize_show [format "%.3g MB" [expr {$totalsize/1024.0/1024.0}]]
    if { $totalsize_show < 1 } {
	set totalsize_show [format "%.3g KB" [expr {$totalsize/1024.0}]]
    }
    return [list $list $totalsize_show]
}

proc RamDebugger::VCS::ShowAllFiles {} {
    variable vcsrootdir
    variable vcsworkdir
    variable null
    variable cvs_or_fossil

    RamDebugger::WaitState 1
    set err [catch { Init } errstring]
    if { $err } {
	RamDebugger::WaitState 0
	WarnWin $errstring
	return
    }

    set w $RamDebugger::text._openrev
    destroy $w
    dialogwin_snit $w -title [_ "Choose revision file"] -class RamDebugger -entrytext \
	[_ "Choose a revision file to check its revisions or to remove revisions history"] \
	-morebuttons [list [_ "Purge..."]] -okname [_ "Revisions"]
    set f [$w giveframe]

    lassign [_showallfiles_update] list totalsize_show

    ttk::label $f.lsize -text [_ "Total size of revision storage: %s" $totalsize_show]
    
    set columns [list \
	    [list 25 [_ "File"] left text 0] \
	    [list 15 [_ "Storage size"] right text 0] \
	    [list 10 [_ "File size"] right text 0] \
	    [list 25 [_ "Path"] left text 0] \
	    [list 25 [_ "VCS file"] left text 0] \
	]
    fulltktree $f.lf -width 400 \
	-columns $columns -expand 0 \
	-selectmode extended -showheader 1 -showlines 0  \
	-indent 0 -sensitive_cols all \
	-have_search_button automatic \
	-selecthandler2 "[list $w invokeok];#"

    $f.lf column configure 4 -visible 0
    
    if { $cvs_or_fossil eq "fossil" } {
	$f.lf column configure 1 -visible 0
    }
    
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
		WarnWin [_ "Select one file in order to visualize its revisions"] $w
	    } else {
		destroy $w
		OpenRevisions -file [file join [lindex $selecteditems 0 3] \
		        [lindex $selecteditems 0 0]]
		return
	    }
	} else {
	    set isgood 1
	    if { $cvs_or_fossil eq "cvs" } {
		if { [llength $selecteditems] == 0  } {
		    WarnWin [_ "Select one or more files in order to purge the revisions"] $w
		    set isgood 0
		} else {
		    set len [llength $selecteditems]
		    set files ""
		    foreach i [lrange $selecteditems 0 4] {
		        lappend files [lindex $i 0]
		    }
		    set txt_files "\"[join $files {","}]\""
		    if { $len > 5 } { append txt_files "..." }
		    
		    set title [_ "Purge revisions"]
		    set txt [_ "Are you user to purge revision history for the %s selected files? %s" $len $txt_files]
		    
		    set ret [snit_messageBox -icon question -title $title -type okcancel \
		            -default ok -parent $w -message $txt]
		    if { $ret ne "ok" } { set isgood 0 }
		}
	    } else {
		set title [_ "Purge revisions"]
		set txt [_ "Are you user to purge revision history for ALL files?"]
		set ret [snit_messageBox -icon question -title $title -type okcancel \
		        -default ok -parent $w -message $txt]
		if { $ret ne "ok" } { set isgood 0 }
	    }
	    if { $isgood } {
		RamDebugger::WaitState 1
		set pwd [pwd]
		cd $vcsworkdir
		
		if { $cvs_or_fossil eq "cvs" } {
		    foreach i [lrange $selecteditems 0 4] {
		        lassign $i file - - - rcsfile
		        set err [catch { _ShowAllFiles_remove $rcsfile } ret]
		        if { $err } {
		            WarnWin [_ "Error purging revision for file '%s' ($ret)" $file $ret] $w
		            break
		        }
		    }
		} else {
		    set err [catch { _ShowAllFiles_remove_all } ret]
		    if { $err } {
		        WarnWin [_ "Error purging revisions ($ret)" $ret] $w
		    }
		}
		cd $pwd
		lassign [_showallfiles_update] list totalsize_show
		$f.lsize configure -text [_ "Total size of revision storage: %s" $totalsize_show]
		$f.lf item delete 0 end
		set num 0
		foreach i $list {
		    $f.lf insert end $i
		    incr num
		}
		if { $num } {
		    $f.lf selection add 1
		    $f.lf activate 1
		}
		RamDebugger::WaitState 0
	    }
	}
	set action [$w waitforwindow]
    }
}

################################################################################
#    CVS indicator
################################################################################

proc RamDebugger::VCS::indicator_init { f } {
    variable cvs_indicator_frame

    if { [auto_execok cvs] eq "" && [auto_execok fossil] eq "" } { return }

    set cvs_indicator_frame $f
    ttk::label $f.l1 -text VCS:
    ttk::label $f.l2 -width 3
    ttk::label $f.l3 -width 3
    
    tooltip::tooltip $f.l1 [_ "Version control system (CVS or fossil)"]
    
    foreach i [list 1 2 3] {
	#bind $f.l$i <1> [list RamDebugger::OpenProgram tkcvs]
	bind $f.l$i <1> [list RamDebugger::VCS::update_recursive $cvs_indicator_frame last]
	bind $f.l$i <<ContextualPress>> [list RamDebugger::VCS::indicator_menu $cvs_indicator_frame %X %Y]
    }
    grid $f.l1 $f.l2 $f.l3 -sticky w
}

proc RamDebugger::VCS::indicator_menu { cvs_indicator_frame x y } {
    
    set currentfileL $RamDebugger::currentfile
    
    destroy $cvs_indicator_frame.menu
    set menu [menu $cvs_indicator_frame.menu -tearoff 0]
    $menu add command -label [_ "Open"] -accelerator "Ctrl+7" -command \
	[list RamDebugger::VCS::update_recursive $cvs_indicator_frame last]
    $menu add command -label [_ "Open - current directory"] -accelerator "Ctrl+Shift-7" -command \
	[list RamDebugger::VCS::update_recursive $cvs_indicator_frame current]
    $menu add separator
    $menu add command -label [_ "Differences"] -command \
	[list RamDebugger::VCS::update_recursive_cmd "" open_program tkdiff "" "" [list $RamDebugger::currentfile]]
    $menu add command -label [_ "Differences (ignore blanks)"] -command \
	[list RamDebugger::VCS::update_recursive_cmd "" open_program tkdiff_ignore_blanks "" "" [list $RamDebugger::currentfile]]

    $menu add command -label [_ "Differences window"] -command \
	[list RamDebugger::VCS::update_recursive_cmd "" diff_window "" "" [list $RamDebugger::currentfile]]

    tk_popup $menu $x $y
}

proc RamDebugger::VCS::indicator_update {} {
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
	fileevent $cvs_indicator_fileid readable [list RamDebugger::VCS::indicator_update_do cvs]
    }
    set  fossil_indicator_data ""
    set fossil [auto_execok fossil]
    if { $fossil ne "" && [catch { exec $fossil info }] == 0 } {
	set fossil_indicator_fileid [open "|$fossil changes" r]
	fconfigure $fossil_indicator_fileid -blocking 0
	fileevent $fossil_indicator_fileid readable \
	    [list RamDebugger::VCS::indicator_update_do fossil]
    }
    cd $pwd
}

proc RamDebugger::VCS::indicator_update_do { cvs_or_fossil } {
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

proc RamDebugger::VCS::update_recursive { wp current_or_last_or_this args } {
    variable try_threaded
    
    if { $current_or_last_or_this eq "this" } {
	set directory [file normalize [lindex $args 0]]
	set current_or_last_or_this current
    } elseif { [file isdirectory [file dirname $RamDebugger::currentfile]] } {
	set directory [file dirname $RamDebugger::currentfile]
	set pwd [pwd]
	cd $directory
	set fossil [auto_execok fossil]
	if { $fossil ne "" && [catch { exec $fossil info } info] == 0 } {
	    regexp -line {^local-root:\s*(.*)} $info {} dirLocal
	    set directory [string trimright $dirLocal /]
	}
	cd $pwd
    } else {
	set directory ""
    }
    set script ""
    append script "[list set ::control $::control]\n"
    append script "[list set ::control_txt $::control_txt]\n"

    foreach cmd [list update_recursive_do0 select_directory messages_menu clear_entry insert_in_entry \
	    insert_ticket update_recursive_do1 update_recursive_accept update_recursive_cmd \
	    waitstate parse_timeline parse_finfo open_program show_help] {
	set full_cmd RamDebugger::VCS::$cmd
	append script "[list proc $cmd [info_fullargs $full_cmd] [info body $full_cmd]]\n"
    }
    append script "[list namespace eval RamDebugger ""]\n"
    append script "[list set RamDebugger::topdir $RamDebugger::topdir]\n"
    append script "[list set RamDebugger::AppDataDir $::RamDebugger::AppDataDir]\n"
    append script "[list lappend ::auto_path {*}$::auto_path]\n"
    append script "[list update_recursive_do0 $directory $current_or_last_or_this]\n"
    

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

proc RamDebugger::VCS::show_help {} {
    package require helpviewer

    set file "01RamDebugger/RamDebugger12.html"
    
    HelpViewer::EnterDirForIndex $::RamDebugger::AppDataDir
    HelpViewer::HelpWindow [file join $::RamDebugger::topdir help $file]
}

proc RamDebugger::VCS::update_recursive_do0 { directory current_or_last } {

    package require dialogwin
    package require tooltip
    #package require compass_utils

    wm withdraw .
    
    destroy ._ask
    set w [dialogwin_snit ._ask -title [_ "VCS management"] -class RamDebugger \
	    -okname [_ View] -morebuttons [list [_ "Update"]] \
	    -cancelname [_ Close] -grab 0 -callback [list update_recursive_do1]]
    
    if { $::tcl_platform(platform) ne "windows" } {
	catch {
	    package require img::png
	    set img [image create photo -file [file join $::RamDebugger::topdir addons ramdebugger.png]]
	    wm iconphoto $w $img
	}
    } else {
	catch { wm iconbitmap $w $::RamDebugger::topdir/addons/ramdebugger.ico }
    }
    set f [$w giveframe]
    
    ttk::label $f.l0 -text [_ "Select origin directory for fossil or CVS update recursive, then use the contextual menu on the files"] \
	-justify left -width 50
    cu::adapt_text_length $f.l0
    
    ttk::button $f.b0 -text [_ "Help"] -command show_help
    
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
    $w set_uservar_value ticket_status [dict_getd $dict ticket_status "Fixed"]
    
    if { [info command RamDebugger::VCS::document-open-16] eq "" } {
	image create photo RamDebugger::VCS::document-open-16 -data {
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
	image create photo RamDebugger::VCS::edit-clear-16 -data {
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
	image create photo RamDebugger::VCS::list-add-16 -data {
	    R0lGODlhEAAQAPQAAAAAADRlpH2m13+o14Oq2Iat2ZCz2pK02pS225W325++4LTM5bXM5rbM5rbN
	    5rfO5rvR57zR58DT6MzMzAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACH5BAEA
	    ABMALAAAAAAQABAAAAU94CSOZGmeqBisQToGz9O6EyzTdTyb7Kr3pIAEEonFHIzGrrZIIA6GAmEg
	    UCx7gUIBi4Jtcd5lVwdm4c7nEAA7
	}
	image create photo RamDebugger::VCS::::semaphore_green -data {
	    R0lGODlhNgAjAOf/AAEEAAUBBwAFCAkCAAMDEAAGEAQHAxEBARkCAAENBwgL
	    ByACASQDAAYSBCsGARUQDDEEAwUbBhEZAxMVExYUFzcJABkXGggjDj8KAjMR
	    AgUqBhscGgwnCysbAB4gHTcYBCUeHk0PBgwxDj4aCSMiKAwyGBsqGyQmI1YT
	    CAo6Ewg+Dl4WB1gfBywvLGwWBRRDDAlKER8+IQlRERhNDjo7ExlNHj06OzBI
	    FYYeDX0jEBJZFWgxDj4/PZ4bCw9iGRhfGxxcIi5XE3E4ChVmFaYjAEtGRWNF
	    ExhpIBNtG0hKSDhVOqApDxByHw90GbQlCwZ8IDZgNMInBCZxJFVSU1NUUht6
	    IBZ+GrEyE5FEC39NFFZXVSZ6KA6IIxyDHwaPIeMpABqLHkB3KNcuFV1fXBKS
	    JTKCKCGKLd8wCtY2BP8gC1FtU1RsWQ+bI1x0JNc4Gh6WH090Uf8rAP4qCfcv
	    BDaJOfIyDqVXG/oyADKRJWhqZ0iASu07APU3ABGlJPE1ICKhISmbMlCCVVl/
	    V29xbkaOSmV6YjyVQmKGLB2qKBWxJoB6OE+OUzmdP3h1dneCNXR4dFOOWkyW
	    URO7KVKbMjykRbxsGzqoQUmeU31+fEqmURXILCTBLVijXoWHhGKiZb6AI4KO
	    gUy0V4qMiR7TNHSmQBfWQJKMiyPXLW2mbbKON2S0QeB9F3ihemOwZ+t/DZOV
	    kmuwcV65aVu9YGK8Y1DHYxznRh7pO5mcmoGxhfmNEw3+P3W/eSLyUaOhpXzH
	    RSf0QH6+fHDKcJ+5UGbRbaWnpPiYFSL7UOuiICv9SZbFUzL8P43WSZ3Ana61
	    qnrZgLK0sYzPktm1Tcm8Y/qsOIXaiIrXkZbQmsLHYrq8ufS8JqrcRabaW7TD
	    tqrbUX7niZHbmbfYR7vUU8XSTK3Mr8LVRm34ecfWPtDQTtXRRtvNT+jKQPLH
	    OfHFSNDSXt7PSdzLZOfLSeLNUefLVr7QvKTiq6LspbDkss7QzarssNXR0tja
	    18vrzd/i3enr6O3r7////yH+FUNyZWF0ZWQgd2l0aCBUaGUgR0lNUAAh+QQB
	    CgD/ACwAAAAANgAjAAAI/gD/CRxIsKDBgwgTKlxocB4ohvkwQczDUCE+VkrU
	    LLxVhMdCYkU8VERoLRAuQXAS5stzq5FHhPoadTK1YeRAfNCgeQKCyhqkQAX5
	    YcMmigQmbC6DDr1FohE2UyJH1ov0w4ePqrC+LdIzcF8hEw0CCJhgCunLf/4a
	    UaAwIAAFTM+g2qSDpG7dI62qRVo0EI6GCA0MAChQNqnAQQYUDEhMAK7citS2
	    MKnSpEqVJ7KoXbok0F6NFBwAA9hQ+KW/EwEApBYQ4GinmgupBQvVxQoYMFy4
	    dKEkyxCdXbt2ptBwQUICBVMaJSHRq5cpBanFqqbSSIuF5vwSZrLC5Q0ZPIC+
	    /pMh0+VJFSQ+dMBQUeLChQYCCCxWQED+gLZiA+g3oH+AgGYJWdIFGYAA8scf
	    ffzBxhu4PcHEED4AAUMKIrwHXWqqBdCWAAYkAABrAGQIgAEAHuSNJXgciAgi
	    m2ySCCIKkgGGFUwcocMML9xwAw0dIBBAAfeJRUAACixQAQYLtNXWh6KYWKCK
	    m5TCCy+11KIJIm+80UUVR/wAxCHCXCNNKllkoN8BACSQAAIrOCGGG0S4AIGP
	    AgjQQiMGhYLHG3+wWIsxgCLDyymjJMKgFVVIQUo45KQTjzzHVDKChnU+kIMY
	    csQRhx9R5FCBWAKMYYM//+BTDTXOWEKgn4Eigwyg/qds8oeWZUyyDTjmoOOO
	    OtnkYscC+iGQgxuZ3rHHHXI4kUOSBViQxDP/ANNKLLJ890cimvBijDLIcNtt
	    LadgaUYXvnQDDjnopAMPO8WswoJ/DkRRxx3G7jFHHGcsgYF+AbTQJDCXZBJK
	    d32OYky3yiTs6qCSsMEFHsuYK0476cgjzzSuYGGAARWgMce99N4RxxxOrLAh
	    BY9EOwQS3B0YpasIu2rML6ck8h3E2oAjzjkVX5yxfwygwYemmWoaRxQoHKCf
	    ACnD4gMST4BhIIuAJpzwwbbU/AYeXER8Ls/ysLvKDm0x4IYfcszBxx5Df3FF
	    CEqnJhE0hBjCiNR9bvIn/syu/mKLJn18V4Uq5ua6azbFVJJBWwj0gLYca/Mh
	    RxQ4ONAWf70UREmWiCQyiravHgxuImx4sWUZyTCaLjzHjK30AAdgcEUdctzB
	    Bx/5ooAAawIMkE9Bw3Bh4LVSGqNtLaNsEjgXViAxRBipt/NONJ8IoTSa/IVw
	    xR51fHEGESgAyyEANhxECyB8rhhlKaW4OKtuNf4AQxBtOKKIER8EMCSa8R3A
	    wAo5wMEKHHCAywkgCb87iCW6A4gErQgRfVgQGZ7wBPTIQAYU4oAEHvAjDQ1A
	    NUNKzQEKKAAALCkAJToII7oAhjewAX1sWBDXzPMgH8gABjFojwQEUIDe6Y8A
	    /nHT0IfqlADWLCaFBslEGczAtds40QxmqAITtiAFIMyAPRcAjAAosIEHPIAE
	    JACBYO7jH7dsgAIBAMEJsLEQZ1DQCtwpgxVk4YxMcOYf0PgMcRqQgLeYRSD5
	    2IBgerfFRsQFNgwpxxaQ0IQm1IUJsaAGJwjRGSi0pwHwIcwfBVKE3qnGAI3B
	    BiaiUpFdHGEIpzwCXrTCF4EwIwZa5GFpBoIN1ICoMXEhZUW+4Ym9eAkW1lgE
	    UAbikDFo4QQWmOVAIjKGMbTgLbm0SUHooQdIlEQQCPHHGPLwDMMYJC1jiKY0
	    C8IKlKQkIb0w5lkO8ozk6HKcApmHEtawkHx0ZCH6FJjCO+H5j31woyJsZAi0
	    +ElQggYEADs=   
	}
	image create photo RamDebugger::VCS::::semaphore_red -data {
	    R0lGODlhNgAjAOf/AAQBBwEEAAgBAAAFCA4BCQwFAwALBAYJBQENABQDBBYF
	    AB8FAxwHAgwOCx4GCwMWAQAZAyUHAREOEiQLACoHCCsJAAEfCjEKAwMiAjgK
	    AgAlDhgXHBgZFwQoAz8MADYSBkwMAAstF0gSBAIzGDYcBjYaEyAjICUhIFUP
	    AD4ZEAk2CgA9BF0QBF8RAFEZAFgVCmYQA0kfE1AeCQY/JVEeEglEDGYYBUEo
	    IU4jHEYqEnQVB24ZA3YXAQVKL30VBX8XAFIqFjI1NF8lFIcWBGkkFIMcA3Ui
	    DDg6NwZaFIsbAJMYBHQlEwBhDnAqEmExGhNUN4khCJYcAJ4aA4EnE4YmDz9B
	    QJIiCJshAqMfAI8nFKodAmI6LwFyG10/N4IxH64hALUeAEdJRnU5K209MXI8
	    K384J5gxEgh6FcEgALokABltS1JRUJc4H8YmAbgvBINHGtAlAB51SaM5IrE5
	    B1haV9snBgCQKoVKPHxSKpBHNBOFQ3hRR688I6dAJt8rAGZaWItTI15gXekq
	    BM85AO0sAJ9KOXNdViSFXHhcVmNlYv0pAPYtAwChQPkxALhLMmtraNRICaRc
	    JbFWJ7JVPaVbSJ1fUI5mXAmxQJhlWMFYQLNeSrtdReZTBrFjU9hcFKltPJ90
	    PXl8evNVCD2gdALFUattXqVwYxu5VZh2bdRmKMJoVaJ1aPZhA75uWpN+ezmw
	    e81sVSO/boaJhvxlDNluVIyOi/xsHNN4XdJ4Y/pzFtt3X6aNh/97C9SJQNCM
	    R5ialzvNh8OTTtSEcsWKfP6AHN2CbPyCKtGKeUHOmPaFNuiJSB/ne6Cin+eI
	    bfuLKv6MIs2ThfSGaqOlov2MNbuclseaj+qWT0Pcpf2UMNyeXNejUPeVRQD/
	    sDXsiv6VOe+TfBf9mBH+qfydPSH7oPafS/2hLjvtq+ubiP6hN+KsT/GmTTX2
	    rSf9rzL6p8GwrvyoOuiikre4tvGuX/uwP/iwS/WwVL/BvsTGw9W/u+a5r9DT
	    0OLSzdnb1+Hk4Onr6P///yH+FUNyZWF0ZWQgd2l0aCBUaGUgR0lNUAAh+QQB
	    CgD/ACwAAAAANgAjAAAI/gD/CRxIsKDBgwgTKlxo0F4thvhQMcxniaFCf7KC
	    0Fm4q0uXhcPuxLCI8N6fX4E2IsSHaJelLQnhVSpVigbJgfzixQu1wVW8RCoH
	    9rPnztUNVNUwjSkI75iwVkQoHStF5Ga/QA0KHJAgQdbPoP0ecUjAYEGKVUnF
	    DPxGyYiRHTZ2aBLWqSrJKhICANDb9R7QgYEKBEgwocJZaJXKDJxkhIdjHj42
	    4VK1hOS9BgEOCBgwgIDXRyr1cShQgEGFCjRMHaOk+N83Kj9iJ/kBZZOuTVMY
	    xmP26ACAAgL2SqATqkoQZtJCDSiQIEKFDyLuUMrjRRWsTEWSJImiPQofR32o
	    /sBqdS7hGsEGAhAAICBAAM57CwA4wNwBhQseRLxoYUOHj+xWRHHFgFdEEYUV
	    SRRRBA+4mAfAXg/uJQBwA0TI3mAR4CcCCiywsAMPP2x3BRZYfFEiiQVq98Mt
	    COlTxYQENGBACDOEgEAAAmxWIQDNXeBCE004BgOIBo6IBRhptNEGGF+YKEUS
	    Q2xykD4m+FaABBaogYw5wOhRgwX0RejABYD4Yo0yqcwx2w8CXuFGGnD4sQgh
	    fviBxhdPJmHGJAYpp9cBFoxiDjvhtNMNI0xg8GcBH3xSzzvieDNNLpAUOOIX
	    adTRiCKbNkJnGiZa4Ygc0fzjjz332BPEbwAgcMig/ty00844y9hRAwIAGKAA
	    HvK8g0452DxDDCuQYHHFF2D4QYginHZKCBxgYKGEFWw0yEwosoTSwG8FYHDN
	    OuO0s8467YBzCRcYDBBACcHQ80453mBjjDGsiOLGsWgQ0mmzjTRiJxZSWGGG
	    lL8kksgfviVQQA/mxNoOOxCDQwoXKlSYQzrziCPONtNMk0wuogxSIhqL7Nsv
	    p4S08cUVVkDB5y8D6ChAAgE8Yc7DD0M8DilnVCzBxe5G2nEytoRsYhsln7wp
	    yipLoUQRLwMQ83vyjXANobKyU+4pXHSwHAnaBO2NN/MW7UaJbejLbL9sp/wF
	    dz/wGU8YVYSxLc0PvELu/sPrhNONHUxAMICuoDxaDjrYYDMsJCZ+gYYfbEdO
	    SB1oSAslLAWtwd4AB/SADDvchBMu1yocMLgCQGTjLuLG5MKJGwIemfYizSoy
	    Jxwrw/1NQdKoO6EBT2zJzjqxcLECAvLhuIATvqhDzjbJeKLmEFGQCCfki2Tv
	    h8pvWzEEGwfJYsKEAsyoxiFxqKBBegIQoBcBFLjwRiSSmLEgD0UUqYXjcfoB
	    Rxpg0MKBkiCHYiBkDROSmnsMYICYkcZCBZjAfUCAgha0wEM+GEISriAFEn3B
	    DU1CEdxYdMC9HEA9BAAOANbzoAoJ4AAJsM8HPOCCF7CAB/6BgoGqRyKASWGA
	    /gsi4UEecQIOnPBCetnABiQwABOc4AT0sc8FMqAfIrxlCllA0HZ2qIQomAEK
	    RaDCFAyokHio6zd88UpKBBKP0SRgARW4QGpW4wWBNIMKCtIOghxBi0xQ4SYn
	    EMx7ANCXv5gqCINRgHNSg5jW/IMNIPqBD/C3iVtoIjckYQYHHvSersRjjWw0
	    AWkYEIGzJMWRwpjCh1Y5l05U5ib6CEUgAqFEV/glKP/Qhyz+8IcblAAplVjK
	    QM7RiUIUwgs26ARd7HKTgehjDY/4SSAQsg9D7IEalhCmQb5RiTxMRQjNNEgt
	    HrEGXBpkF4hABEwSEhJM2CScBYlHFcxpEHx4ZCH5E9gDDuBpkFNZxB0WoQY/
	    B0rQgAAAOw==
	}
    }
    ttk::label $f.l1 -text [_ "Directory"]:
    cu::combobox $f.e1 -textvariable [$w give_uservar dir ""] -valuesvariable \
	[$w give_uservar directories] -width 60
    ttk::button $f.b1 -image RamDebugger::VCS::document-open-16 \
	-command [namespace code [list select_directory $w]] \
	-style Toolbutton
    
    tooltip::tooltip $f.b1 [_ "Select the directory under version control"]
    
    ttk::label $f.l2 -text [_ "Commit messages"]:
    cu::multiline_entry $f.e2 -textvariable [$w give_uservar message ""] -valuesvariable \
	[$w give_uservar messages] -width 60
    bind $f.e2 <Return> "[bind Text <Return>] ; break"
    
    label $f.sem -image RamDebugger::VCS::::semaphore_green -compound left -height 35 \
	-text " "
    $w set_uservar_value semaphore_label $f.sem
    
    ttk::button $f.b2 -image RamDebugger::VCS::edit-clear-16 \
	-command [namespace code [list clear_entry $w $f.e2]] \
	-style Toolbutton
    
    tooltip::tooltip $f.b2 [_ "Clear the messages entry"]
    
    ttk::menubutton $f.b3 -image RamDebugger::VCS::list-add-16 \
	-menu $f.b3.m -style Toolbutton
    menu $f.b3.m -tearoff 0 -postcommand [namespace code [list messages_menu $w $f.b3.m  $f.e2]] 
    
    ttk::label $f.l3 -text [_ "Change ticket status"]:
    ttk::entry $f.e3 -textvariable [$w give_uservar tickets ""]
    
    tooltip::tooltip $f.e3 [_ "Enter a list of 'ticket unique ids' in order to change its status"]
    
    set statusList [list "" Open Verified Review Deferred Fixed Tested Closed]
    ttk::combobox $f.cb1 -textvariable [$w give_uservar ticket_status] -width 8 \
	-values $statusList -state readonly
    
    package require fulltktree
    set columns [list [list 20 [_ "line"] left item 0]]
    fulltktree $f.toctree -height 350 \
	-columns $columns -expand 1 \
	-selectmode extended -showheader 1 -showlines 0  \
	-indent 0 -sensitive_cols all \
	-have_search_button automatic \
	-contextualhandler_menu [list "update_recursive_cmd" $w contextual]
    $w set_uservar_value tree $f.toctree
    
    $f.toctree element configure  e_text_sel -lines 2
    
    grid $f.l0    -      -   $f.b0 -   -sticky w -padx 2 -pady 2
    grid $f.l1 $f.e1 -        -   $f.b1 -sticky w -padx 2 -pady 2
    grid $f.l2 $f.e2 -        -   $f.b2 -sticky w -padx 2 -pady 2
    grid $f.sem         ^  ^ ^ $f.b3 -sticky w -padx 2 -pady 2
    grid $f.l3  $f.e3  $f.cb1 -  - -sticky w -padx 2 -pady 2
    grid $f.toctree - - - - -sticky nsew -padx 2 -pady 2
    grid configure $f.l0 -sticky ew -padx "2 10" -columnspan 3
    grid configure $f.l2 -pady "2 0"
    grid configure $f.sem -pady 0
    grid configure $f.e1 $f.e2 $f.e3 -sticky ew
    grid columnconfigure $f "0" -weight 1
    grid columnconfigure $f "1" -weight 100
    grid rowconfigure $f 5 -weight 1
    
    $w set_uservar_value dir $dir
    $w set_uservar_value message ""
    
    bind $w <$::control-d> [list "update_recursive_cmd" $w open_program tkdiff $f.toctree ""]
    bind $w <$::control-D> [list "update_recursive_cmd" $w open_program tkdiff_ignore_blanks $f.toctree ""]
    bind $w <$::control-i> [list "update_recursive_cmd" $w commit $f.toctree ""]
    bind $w <$::control-f> [list "update_recursive_cmd" $w open_program fossil_ui $f.toctree ""]

    bind $w <$::control-e> [list $w invokeok]
    bind $w <$::control-u> [list $w invokebutton 2]
    bind $f.e2 <$::control-i> "[bind $w <$::control-i>];break"
    bind $f.e2 <$::control-f> "[bind $w <$::control-f];break"

    $w tooltip_button 1 [_ "View modified files and file to be updated from repository %s" $::control_txt-e]
    $w tooltip_button 2 [_ "Update files from repository %s" $::control_txt-u]
    
    tk::TabToWindow $f.e1
    bind $w <Return> [list $w invokeok]
    $w createwindow
    return $w
}

proc RamDebugger::VCS::waitstate { w on_off { txt "" } } {
    variable waitstate_stack

    if { ![info exists waitstate_stack] } {
	set waitstate_stack ""
    }
    switch $on_off {
	on {
	    lappend waitstate_stack $txt
	}
	off {
	    set waitstate_stack [lrange $waitstate_stack 0 end-1]
	}
    }
    if { [llength $waitstate_stack] } {
	set img RamDebugger::VCS::::semaphore_red
    } else {
	set img RamDebugger::VCS::::semaphore_green
    }
    set txt [lindex $waitstate_stack end]
    if { $txt eq "" } { set txt " " }
    
    if { ![winfo exists $w] } { return }
    set l [$w give_uservar_value semaphore_label]
    if { [$l cget -image] ne $img } {
	$l configure -image $img
    }
    if { [$l cget -text] ne $txt } {
       $l configure -text $txt
    }
    update
}

proc RamDebugger::VCS::messages_menu { w menu entry } {
    
    $menu delete 0 end
    $menu add command -label [_ "Clear message"] -image RamDebugger::VCS::edit-clear-16 \
	-compound left -command [namespace code [list clear_entry $w $entry]] 

    set tree [$w give_uservar_value tree]
    set files ""
    set dirList ""
    foreach item [$tree selection get] {
	set itemL $item
	while { $itemL != 0 } {
	    if { [file isdirectory [$tree item text $itemL 0]] } {
		lappend dirList [$tree item text $itemL 0]
		break
	    }
	    set itemL [$tree item parent $itemL]
	}
	set color [$tree item element cget $item 0 e_text_sel -fill]
	if { $color in "red blue" } { continue }
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
	    if { [llength [file split $file]] > 1 } {
		set f [file tail $file]
		if { $f in $insertedList } { continue }
		set txt "$f: "
		$menu add command -label [_ "Insert '%s'" $txt] -command  \
		    [namespace code [list insert_in_entry $w $entry $txt]] 
		lappend insertedList $f
	    }
	}
    }
    
    set dir [$w give_uservar_value dir]
    if { [llength $dirList] } {
	set pwd [pwd]
	cd [lindex $dirList 0]
	set fossil [auto_execok fossil]
	if { $fossil ne "" && [catch { exec $fossil info } info] == 0 } {
	    regexp -line {^local-root:\s*(.*)} $info {} dir
	}
	cd $pwd
    }
    
    set pwd [pwd]
    cd $dir
    set fossil [auto_execok fossil]
    if { $fossil ne "" && [catch { exec $fossil info }] == 0 } {
	set err [catch { exec $fossil ticket show 0 "status=='Open'" } ret]
	if { !$err } {
	    set fieds [split [lindex [split $ret \n] 0] \t]
	    set ipos_tkt_uuid [lsearch -exact $fieds tkt_uuid]
	    set ipos_title [lsearch -exact $fieds title]
	    if { $ipos_tkt_uuid == -1 || $ipos_title == -1 } {
		set err 1
	    }
	}
    } else {
	set err 1
    }
    if { !$err } {
	set ticketList ""
	foreach line [lrange [split $ret \n] 1 end] {
	    set values [split $line \t]
	    set ticket [lindex $values $ipos_tkt_uuid]
	    set message [lindex $values $ipos_title]
	    lappend ticketList [list "\[[string range $ticket 0 9]\]" $message 1]
	}
    } else {
	set err [catch { parse_timeline [exec $fossil timeline -n 2000 -t t] } ret]
	if { $err } { set ret "" }
	
	set ticketList ""
	foreach i $ret {
	    lassign $i date time checkin comment user tags
	    if { [regexp {New ticket\s*(\[\w+\]):?\s+<i>(.*)</i>} $comment {} ticket message] } {
		set ipos [lsearch -exact -index 0 $ticketList $ticket] 
		if { $ipos != -1 } {
		    #nothing
		} else {
		    lappend ticketList [list $ticket $message 1]
		}
	    } elseif { [regexp {(?:Fixed|Closed) ticket\s*(\[\w+\]):?\s+<i>(.*)</i>} $comment {} ticket message] } {
		set ipos [lsearch -exact -index 0 $ticketList $ticket]
		if { $ipos != -1 } {
		    lset ticketList $ipos 2 0
		} else {
		    lappend ticketList [list $ticket $message 0]
		}
	    }
	}
    }
    set has_sep 0
    foreach i $ticketList {
	lassign $i ticket message active
	if { !$active } { continue }
	if { !$has_sep } {
	    $menu add separator
	    set has_sep 1
	}
	set txt1 [string range "$ticket $message" 0 80]...
	set txt2 "ticket $ticket: $message"
	$menu add command -label [_ "Insert ticket '%s'" $txt1] -command  \
	    [namespace code [list insert_ticket $w $entry $ticket $txt2]]
    }

    set num 1
    set fossil [auto_execok fossil]
    set err [catch { exec $fossil ticket list reports } ret]
    if { !$err } {
	foreach line [split $ret \n] {
	    if { [regexp {(\d)\s+(.*)} $line {} num_in txt] && $num_in > 0 && [regexp {(?i)open} $txt] } {
		set num $num_in
		break
	    }
	}
    }
    set url_suffix "/rptview?rn=$num"
    $menu add separator
    $menu add command -label [_ "Open tickets browser"] -command \
	[list "update_recursive_cmd" $w open_program fossil_ui $tree "" $url_suffix]
    
    cd $pwd
}

proc RamDebugger::VCS::select_directory { w } {
    set dir [tk_chooseDirectory -initialdir [$w give_uservar_value dir] \
	    -mustexist 1 -parent $w -title [_ "Select origin directory"]]
    if { $dir eq "" } { return }
    $w set_uservar_value dir $dir
}

proc RamDebugger::VCS::clear_entry { w entry } {
    update
    $entry delete 1.0 end
    tk::TabToWindow $entry
}

proc RamDebugger::VCS::insert_in_entry { w entry txt } {
    update
    tk::TextInsert $entry $txt
    tk::TabToWindow $entry
}

proc RamDebugger::VCS::insert_ticket { w entry ticket txt } {
    
    set tickets [string trim [$w give_uservar_value tickets]]
    if { $tickets ne "" } {
	append tickets ","
    }
    append tickets $ticket
    $w set_uservar_value tickets $tickets
    insert_in_entry $w $entry $txt
}

proc RamDebugger::VCS::update_recursive_do1 { w } {

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
    update_recursive_accept $w $what $dir $tree 0 0
}

proc RamDebugger::VCS::parse_timeline { timeline } {
    
    lassign "" list date time checkin comment
    foreach line [split $timeline \n] {
	if { [regexp {===\s*(\S+)\s*===} $line {} date_curr] } {
	    # nothing
	} elseif { [regexp {^(\S+)\s+\[([^]]*)\]\s*(.*)} $line {} time_curr checkin_curr comment_curr] } {
	    if { $comment ne "" } {
		lassign "" user tags
		regexp {(.*)\(\s*user:\s*(.*)\s*tags:\s*(.*)\s*\)} $comment {} comment user tags
		lappend list [list $date $time $checkin $comment $user $tags]
	    }
	    set d [clock scan "$date_curr $time_curr" -timezone :UTC]
	    set date  [clock format $d -format "%Y-%m-%d"]
	    set time [clock format $d -format "%H:%M:%S"]
	    set checkin $checkin_curr
	    set comment $comment_curr
	} else {
	    append comment " [string trim $line]"
	}
    }
    if { $comment ne "" } {
	lassign "" user tags
	regexp {(.*)\(\s*user:\s*(.*)\s*tags:\s*(.*)\s*\)} $comment {} comment user tags
	lappend list [list $date $time $checkin $comment $user $tags]
    }
    return $list
}

proc RamDebugger::VCS::parse_finfo { finfo } {
    
    set fossil [auto_execok fossil]
    
    lassign "" list line
    foreach l [split $finfo \n] {
	if { [regexp {^\d} $l] } {
	    if { $line ne "" } {
		regexp {(\S+)\s+\[(\w+)\]\s+(.*)\(user:\s*([^,]+),\s*artifact:\s*\[(\w+)\]\s*\)} $line {} date checkin \
		    comment user artifact
		set ret [parse_timeline [exec $fossil timeline $checkin -n 1]]
		lassign [lindex $ret 0] date time - - - tags
		set date [clock format [clock scan "$date $time" -timezone :UTC] -format "%Y-%m-%d %H:%M:%S"]
		lappend list [list $date $checkin $comment $user $artifact $tags]
	    }
	    set line $l
	} elseif { [regexp {^\s} $l] } {
	    append line " [string trim $l]"
	}
    }
    if { $line ne "" } {
	regexp {(\S+)\s+\[(\w+)\]\s+(.*)\(user:\s*([^,]+),\s*artifact:\s*\[(\w+)\]\s*\)} $line {} date checkin comment user artifact
	set ret [parse_timeline [exec $fossil timeline $checkin -n 1]] 
	lassign [lindex $ret 0] date time - - - tags
	set date [clock format [clock scan "$date $time" -timezone :UTC] -format "%Y-%m-%d %H:%M:%S"]
	lappend list [list $date $checkin $comment $user $artifact $tags]
    }
    return $list
}

proc RamDebugger::VCS::update_recursive_accept { args } {
    variable fossil_version
    
    set optional {
	{ -item item "" }
	{ -cvs_and_fossil boolean 0 }
    }
    set compulsory "w what dir tree itemP level"
   parse_args $optional $compulsory $args
    
    if { $cvs_and_fossil } {
	$w set_uservar_value cvs_and_fossil 1
    }
    
    waitstate $w on $what 
    if { $item ne "" } {
	foreach i [$tree item children $item] { $tree item delete $i }
    }
    set curr_item $item
    
    set olddir [pwd]
    set err [catch { cd $dir } ret]
    if { $err } { return }
    
    set has_vcs 0

    set fossil [auto_execok fossil]
    if { $fossil ne "" && [catch { exec $fossil info } info] == 0 } {
	regexp -line {^local-root:\s*(.*)} $info {} dirLocal
	set dirLocal [string trimright $dirLocal /]
	set fossil_version 0
	set err [catch { exec $fossil version } ret]
	if { !$err && [regexp {\d{4}-\d{2}-\d{2}} $ret date] } {
	    if { [clock scan $date] >= [clock scan "2009-12-18"] } {
		set fossil_version 1
	    }
	}
	# this is to avoid problems with update
	if { ![winfo exists $tree] } {
	    cd $olddir    
	    waitstate $w off
	    return
	}
	cd $dirLocal
	if { $curr_item eq "" || [$tree item text $curr_item 0] ne $dirLocal } {
	    set curr_item [$tree insert end [list $dirLocal] $itemP]
	}
	if { $what eq "view" &&  [update_recursive_cmd $w give_fossil_sync $dir] } {
	    set err [catch { exec $fossil remote } ret]
	    if { !$err && $ret ne "off" } {
		set err [catch { exec $fossil pull } ret]
	    }
	    if { $err && $ret ne "" } {
		snit_messageBox -message $ret -parent $w
	    }
	}
	if { $fossil_version == 0 } {
	    set err [catch { exec $fossil ls 2>@1 } list_files]
	} else {
	    set err [catch { exec $fossil ls -l 2>@1 } list_files]
	}
	if { !$err } {
	    set err [catch { exec $fossil extras 2>@1 } list_files2]
	    foreach line [split $list_files2 \n] {
		append list_files "\n? $line"
	    }
	}
	lassign $what op version
	switch $op {
	    update {
		set err [catch { exec $fossil update 2>@1 } list_files3]
	    }
	    update_this {
		set err [catch { exec $fossil update $version 2>@1 } list_files3]
	    }
	    merge_this {
		set err [catch { exec $fossil merge $version 2>@1 } list_files3]
	    }
	    checkout_this {
		set err [catch { exec $fossil checkout $version 2>@1 } list_files3]
	    }
	    view {
		set err [catch { exec $fossil update --nochange 2>@1 } list_files3]
	    }
	}
	if { !$err } {
	    append list_files "\n$list_files3"
	}
	if { ![$w exists_uservar fossil_timeline_view_more] } {
	    $w set_uservar_value fossil_timeline_view_more 0
	}
	if { [$w give_uservar_value fossil_timeline_view_more] } {
	    set err [catch { parse_timeline [exec $fossil timeline -n 200 -t ci] } ret]
	} else {
	    set err [catch { parse_timeline [exec $fossil timeline after current] } ret]
	}
	if { !$err } {
	    set timeline_txt [_ "Timeline"]
	    foreach i $ret {
		lassign $i date time checkin comment user tags
		if { [regexp {\*CURRENT\*} $comment] } {
		    append timeline_txt " (tags: $tags)"
		}
	    }
	    set itemT [$tree insert end [list $timeline_txt] $curr_item]
	    foreach i $ret {
		lassign $i date time checkin comment user tags
		set item_t [$tree insert end [list "$date $time \[$checkin\] $comment (user: $user tags: $tags)"] $itemT]
		if { [regexp {\*CURRENT\*} $comment] } {
		    $tree item element configure $item_t 0 e_text_sel -fill orange
		}
	    }
	    $tree item collapse $itemT
	}
	
	if { ![$w exists_uservar view_unchanged] } {
	    $w set_uservar_value view_unchanged 0
	}
	set view_unchanged [$w give_uservar_value view_unchanged]

	foreach line [split $list_files \n] {
	    # this is to avoid problems with update
	    if { ![winfo exists $tree] } {
		cd $olddir    
		waitstate $w off
		return
	    }
	    if { [regexp {^Total network traffic:} $line] } {  continue }
	    if { [regexp {^waiting for server} $line] } {  continue }
	    if { [regexp {^(Autosync:|Sent:|processed:|Received:|\s*Bytes|\s*$)} $line] } {  continue }
	    if { [regexp {^-{5,}} $line] } { break }
	    if { $curr_item eq "" } {
		set curr_item [$tree insert end [list $dir] $itemP]
	    }
	    set i [$tree insert end [list "$line"] $curr_item]
	    
	    if { ![regexp {^(\w+)\s+(.*)} $line {} mode file] || $mode eq "UNCHANGED" } {
		if { [regexp {^(\w+)\s+(.*)} [$tree item text $i 0] {} mode file] && $mode eq "UNCHANGED" } {
		    $tree item element configure $i 0 e_text_sel -fill grey
		} elseif { [regexp {^\s*\?} [$tree item text $i 0]] } {
		    $tree item element configure $i 0 e_text_sel -fill brown
		}
		if { !$view_unchanged } {
		    $tree item configure $i -visible 0
		}
	    }
	    update
	}
	set has_vcs 1
    }
    if { [file isdirectory [file join $dir CVS]] } {
	if { ![winfo exists $w] } {
	    return
	}
	if { ![$w exists_uservar cvs_and_fossil] } {
	    $w set_uservar_value cvs_and_fossil 0
	}
	if { $has_vcs && ![$w give_uservar_value cvs_and_fossil] } {
	    set found 0
	    foreach i [$tree item children $curr_item] {
		if { [$tree item text $i 0] eq [_ "Update CVS"] } {
		    set found 1
		    break
		}
	    }
	    if { !$found } {
		set itemCVS [$tree insert end [list [_ "Update CVS"]] $curr_item]
		set cmd [list update_recursive_accept -cvs_and_fossil 1 -item $curr_item $w $what $dir $tree $itemP 0]
		$tree item_window_configure_button -text [_ "Update CVS"] -command $cmd $itemCVS
	    }
	} else {
	    set err [catch { cd $dir } ret]
	    if { $err } { return }
	    
	    if { $what eq "view" } {
		set err [catch { exec cvs -n -q update 2>@1 } ret]
	    } else {
		set err [catch { exec cvs -q update 2>@1 } ret]
	    }
	    set itemD ""
	    foreach line [split $ret \n] {
		if { ![winfo exists $tree] } {
		    cd $olddir
		    waitstate $w off
		    return
		}
		if { $line eq "cvs server: WARNING: global `-l' option ignored." } { continue }
		if { $itemD eq "" } {
		    set itemD [$tree insert end [list $dir] $itemP]
		}
		set i [$tree insert end [list "$line"] $itemD]
		if { ![regexp {^[A-Z]\s|^cvs} $line] } {
		    $tree item configure $i -visible 0
		}
		update
	    }
	    set has_vcs 1
	}
	if { $curr_item eq "" } { set curr_item $itemD }
    }
    cd $olddir

    if { !$has_vcs } {
	if { $curr_item ne "" } { set itemP $curr_item }
	foreach d [glob -nocomplain -dir $dir -type d *] {
	    update_recursive_accept $w $what $d $tree $itemP [expr {$level+1}]
	}
    }
    if { $curr_item ne "" } {
	set num 0
	foreach i [$tree item children $curr_item] {
	    if { [$tree item cget $i -visible] } { incr num }
	}
	if { !$num } {
	    $tree item configure $curr_item -visible 0
	}
    }
    if { ![$w exists_uservar view_unchanged] } {
	$w set_uservar_value view_unchanged 0
    }
    set view_unchanged [$w give_uservar_value view_unchanged]
    if { !$view_unchanged && $item eq "" && $level == 0 && [llength [$tree item children $itemP]] } {
	set itemVU [$tree insert end [list [_ "View unchanged"]] $itemP]
	$tree item_window_configure_button -text [_ "View unchanged"] -command \
	    [list "update_recursive_cmd" $w toggle_view_unchanged $tree] $itemVU
    }
    waitstate $w off
}

proc RamDebugger::VCS::update_recursive_cmd { w what args } {
    variable fossil_version
    
    set fossil [auto_execok fossil]
    
    switch $what {
	toggle_view_unchanged {
	    lassign $args tree
	    if { [$w give_uservar_value view_unchanged] } {
		$w set_uservar_value view_unchanged 0
	    } else {
		$w set_uservar_value view_unchanged 1
	    }
	    update_recursive_cmd $w view $tree 0
	}
	contextual {
	    lassign $args tree menu id sel_ids
	    lassign "0 0 0 0 0 0 0" has_cvs has_fossil cvs_active fossil_active can_be_added \
		can_be_removed is_timeline
	    set dirs ""
	    foreach item $sel_ids {
		set txt [$tree item text $item 0]
		if { [regexp {^\d+} $txt] && [string match [_ "Timeline"]* [$tree item text [$tree item parent $item] 0]] } {
		    set is_timeline 1
		} elseif { [string match [_ "Timeline"]* [$tree item text $item 0]] } {
		    set is_timeline timeline
		} elseif { [regexp {^\s*(\w)\s+} $txt] } {
		    set has_cvs 1
		    set cvs_active 1
		} elseif  { [regexp {^\s*(\w{2,})\s*} $txt {} word] } {
		    set has_fossil 1
		    set fossil_active 1
		    if { $word eq "DELETED" } {
		        set can_be_added 1
		    } elseif { $word in "UNCHANGED DELETED MISSING UPDATE" } {
		        set can_be_removed 1
		    }
		} elseif { [regexp {^\s*\?\s+} $txt] } {
		    set can_be_added 1
		}
		set itemL $item
		while { $itemL != 0 } {
		    if { [file isdirectory [$tree item text $itemL 0]] } {
		        set dir [$tree item text $itemL 0]
		        lappend dirs $dir
		        set pwd [pwd]
		        cd $dir
		        if { [auto_execok cvs] ne "" && [file isdirectory CVS] } {
		            set cvs_active 1
		        }
		        set fossil [auto_execok fossil]
		        if { $fossil ne "" && [catch { exec $fossil info } ret] == 0 } {
		            set fossil_active 1
		        }
		        cd $pwd
		    }
		    set itemL [$tree item parent $itemL]
		}
	    }
	    set pwd [pwd]
	    cd [$w give_uservar_value dir]
	    if { [auto_execok cvs] ne "" && [file isdirectory CVS] } {
		set cvs_active 1
	    }
	    set fossil [auto_execok fossil]
	    if { $fossil ne "" && [catch { exec $fossil info } ret] == 0 } {
		set fossil_active 1
	    }
	    cd $pwd
	    if { $is_timeline == 0 && ($has_cvs || $has_fossil) } {
		$menu add command -label [_ "Commit"] -accelerator $::control_txt-i -command \
		    [list "update_recursive_cmd" $w commit $tree $sel_ids]
	    }
	    if { $is_timeline == 0 && ($has_cvs || $has_fossil) } {
		$menu add command -label [_ "Revert"]... -command \
		    [list "update_recursive_cmd" $w revert $tree $sel_ids]
	    }
	    $menu add command -label [_ "Refresh view"] -command \
		[list "update_recursive_cmd" $w update view $tree $sel_ids]
	    $menu add command -label [_ "Update"] -command \
		[list "update_recursive_cmd" $w update update $tree $sel_ids]
	    
	    if { $is_timeline == 1 } {
		$menu add command -label [_ "Update to this"] -command \
		    [list "update_recursive_cmd" $w apply_version update_this $tree $sel_ids]
		$menu add command -label [_ "Merge this"] -command \
		    [list "update_recursive_cmd" $w apply_version merge_this $tree $sel_ids]
		$menu add command -label [_ "Checkout to this"] -command \
		    [list "update_recursive_cmd" $w apply_version checkout_this $tree $sel_ids]
		$menu add separator
	    }
	    if { $is_timeline != 0 } {
		$menu add checkbutton -label [_ "View more timeline"] -variable \
		    [$w give_uservar fossil_timeline_view_more] -command \
		    [list "update_recursive_cmd" $w toggle_fossil_timeline_view_more $tree]
		if { ![$w exists_uservar fossil_timeline_view_more] } {
		    $w set_uservar_value fossil_timeline_view_more 0
		}
	    }
	    $menu add separator

	    if { $cvs_active } {
		$menu add command -label [_ "CVS add"] -command \
		    [list "update_recursive_cmd" $w add $tree $sel_ids]
		if { !$can_be_added } {
		    $menu entryconfigure end -state disabled
		}
		$menu add command -label [_ "CVS add binary"] -command \
		    [list "update_recursive_cmd" $w add_binary $tree $sel_ids]
		if { !$can_be_added } {
		    $menu entryconfigure end -state disabled
		}
	    }
	    if { $fossil_active } {
		$menu add command -label [_ "Add files"] -command \
		    [list "update_recursive_cmd" $w add_fossil $tree $sel_ids]
		if { !$can_be_added } {
		    $menu entryconfigure end -state disabled
		}
	    }
	    if { $cvs_active || $fossil_active } {
		$menu add separator
	    }
	    if { $is_timeline == 0 && $has_fossil } {
		$menu add command -label [_ "Remove files"]... -command \
		    [list "update_recursive_cmd" $w remove $tree $sel_ids]
		if { !$can_be_removed } {
		    $menu entryconfigure end -state disabled
		}
	    }
	    set need_sep 0
	    if { $is_timeline == 0 && ($has_cvs || $has_fossil) } {
		$menu add command -label [_ "View diff"] -accelerator $::control_txt-d -command \
		    [list "update_recursive_cmd" $w open_program tkdiff $tree $sel_ids]
		$menu add command -label [_ "View diff (ignore blanks)"] -accelerator $::control_txt-D -command \
		    [list "update_recursive_cmd" $w open_program tkdiff_ignore_blanks $tree $sel_ids]
		set need_sep 1
	    }
	    if { $cvs_active } {
		$menu add command -label [_ "Open tkcvs"] -command \
		    [list "update_recursive_cmd" $w open_program tkcvs $tree $sel_ids]
		set need_sep 1
	    }
	    if { $fossil_active || $is_timeline != 0 } {
		if { $is_timeline == 0 && $has_fossil } {
		    $menu add command -label [_ "View diff window"]... -command \
		        [list "update_recursive_cmd" $w diff_window $tree $sel_ids]
		}
		$menu add command -label [_ "Open fossil browser"] -accelerator $::control_txt-f -command \
		    [list "update_recursive_cmd" $w open_program fossil_ui $tree $sel_ids]
		$menu add separator
		$menu add checkbutton -label [_ "Fossil autosync"] -variable \
		    [$w give_uservar fossil_autosync] -command \
		    [list "update_recursive_cmd" $w fossil_toggle_autosync]
		$menu add command -label [_ "Fossil syncronize"] -command \
		    [list "update_recursive_cmd" $w fossil_syncronize sync $tree $sel_ids $dirs]
		$menu add command -label [_ "Fossil pull"] -command \
		    [list "update_recursive_cmd" $w fossil_syncronize pull $tree $sel_ids $dirs]
		$w set_uservar_value fossil_autosync [update_recursive_cmd $w give_fossil_sync]
		set need_sep 1
	    }
	    if { $need_sep } {
		$menu add separator
	    }
	    $menu add checkbutton -label [_ "View unchanged"] -variable \
		[$w give_uservar view_unchanged] -command \
		[list "update_recursive_cmd" $w view $tree 0]
	}
	give_fossil_sync {
	    lassign $args dir
	    if { $dir eq "" } { set dir [$w give_uservar_value dir] }
	    set autosync 0
	    set pwd [pwd]
	    cd $dir
	    set fossil [auto_execok fossil]
	    set err [catch { exec $fossil settings autosync } ret]
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
	    set fossil [auto_execok fossil]
	    set err [catch { exec $fossil settings autosync $autosync } ret]
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
	    update_recursive_accept $w view $dir $tree 0 0
	}
	fossil_syncronize {
	    lassign $args sync_type tree sel_ids dirs
	    waitstate $w on sync
	    set pwd [pwd]
	    foreach dir $dirs {
		cd $dir
		set fossil [auto_execok fossil]
		set err [catch { exec $fossil $sync_type } ret]
		if { $ret ne "" } {
		    snit_messageBox -message $ret -parent $w
		}
	    }
	    waitstate $w off
	    cd $pwd
	    
	    set dir [$w give_uservar_value dir]
	    set tree [$w give_uservar_value tree]
	    $tree item delete all
	    update_recursive_accept $w view $dir $tree 0 0
	}
	commit {
	    lassign $args tree sel_ids
	    
	    set needs_update_view 0
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
	    waitstate $w on commit
	    dict for "dir fs" $cvs_files_dict {
		cd $dir
		set err [catch { exec cvs commit -m $message {*}$fs 2>@1 } ret]
		if { $err } {
		    set color red
		} else {
		    set color blue
		}
		foreach file $fs {
		    set item [dict get $items cvs $dir $file]
		    $tree item element configure $item 0 e_text_sel -fill $color -text $ret
		}
	    }
	    dict for "dir fs" $fossil_files_dict {
		cd $dir
		set fossil [auto_execok fossil]
		set info [exec $fossil info]
		regexp -line {^local-root:\s*(.*)} $info {} dirF
		cd $dirF
		if { $message eq "" } { set message " " }
		set err [catch { exec $fossil commit --nosign -m $message {*}$fs 2>@1 } ret]
		if { $err && [regexp {cannot do a partial commit of a merge} $ret] } {
		    set txt [_ "Cannot do a partial commit of a merge. Do you want to make a full commit?"]
		    set ret_in [snit_messageBox -icon question -type okcancel \
		            -default ok -parent $w -message $txt]
		    if { $ret_in eq "ok" } {
		        set err [catch { exec $fossil commit --nosign -m $message 2>@1 } ret]
		        if { !$err } {
		           set needs_update_view 1
		        }
		    }
		}
		regsub -all {processed:\s*\d+%\s*} [string trim $ret] {} ret
		if { $err } { set color red } else { set color blue }
		foreach file $fs {
		    set item [dict get $items fossil $dir $file]
		    $tree item element configure $item 0 e_text_sel -fill $color -text $ret
		}
		if { [regexp {New_Version:\s*(\S+)} $ret {} c] } {
		    set new_commit "\[[string range $c 0 9]\]"
		} else {
		    set new_commit ""
		}
		set tickets [string trim [$w give_uservar_value tickets]]
		set ticket_status [$w give_uservar_value ticket_status]
		if { !$err && $ticket_status ne "" && $tickets ne "" } {
		    set err [catch { exec $fossil ticket show 0 } ret]
		    if { !$err } {
		        set fieds [split [lindex [split $ret \n] 0] \t]
		        set ipos_tkt_uuid [lsearch -exact $fieds tkt_uuid]
		        set ipos_comment [lsearch -exact $fieds comment]
		        set ipos_status [lsearch -exact $fieds status]
		        if { $ipos_tkt_uuid == -1 || $ipos_comment == -1 || $ipos_status == -1 } {
		            set err 1
		        }
		    } else {
		        set ret [_ "Version of fossil executable is too old to manage tickets. Upgrade fossil"]
		    }
		    if { !$err } {
		        set ticketList ""
		        foreach line [lrange [split $ret \n] 1 end] {
		            set values [split $line \t]
		            set ticket [lindex $values $ipos_tkt_uuid]
		            set comment [lindex $values $ipos_comment]
		            lappend ticketList [list "\[[string range $ticket 0 9]\]" $ticket $comment]
		        }
		        foreach ticket [split $tickets ", ;"] {
		            set ipos [lsearch -exact -index 0 $ticketList $ticket]
		            if { $ipos == -1 } {
		                set err 1
		                set ret [_ "Ticket '%s' not found in list" $ticket]
		                break
		            }
		            lassign [lindex $ticketList $ipos] - ticketF comment
		            append comment "\ncommit: $new_commit. Change status to: $ticket_status"
		            exec $fossil ticket change $ticketF status $ticket_status comment $comment
		        }
		    }
		    if { $err } { break }
		}
	    }
	    cd $pwd
	    if { $err } {
		snit_messageBox -message $ret -parent $w
	    } else {
		$w set_uservar_value tickets ""
	    }
	    waitstate $w off
	    if { ![winfo exists $w] } { return }
	    set dict [cu::get_program_preferences -valueName cvs_update_recursive RamDebugger]
	    $w set_uservar_value messages [linsert0 -max_len 20 [dict_getd $dict messages ""] $message]
	    dict set dict messages [$w give_uservar_value messages]
	    dict set dict ticket_status [$w give_uservar_value ticket_status]
	    cu::store_program_preferences -valueName cvs_update_recursive RamDebugger $dict
	    
	    if { $needs_update_view } {
		update_recursive_cmd $w update update $tree $sel_ids
	    }
	}
	add - add_binary - add_fossil {
	    lassign $args tree sel_ids
	    set message [$w give_uservar_value message]
	    set files ""
	    foreach item $sel_ids {
		if { ![regexp {^(?:\?|DELETED)\s+(\S+)} [$tree item text $item 0] {} file] } { continue }
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

	    waitstate $w on Add
	    set pwd [pwd]
	    foreach item $sel_ids {
		if { ![regexp {^(?:\?|DELETED)\s+(\S+)} [$tree item text $item 0] {} file] } { continue }
		set dir [$tree item text [$tree item parent $item] 0]
		cd $dir
		if { $what in "add add_binary" } {
		    set err [catch { exec cvs add -m $message {*}$kopt $file 2>@1 } ret]
		} else {
		    set fossil [auto_execok fossil]
		    set info [exec $fossil info]
		    regexp -line {^local-root:\s*(.*)} $info {} dir
		    cd $dir
		    set err [catch { exec $fossil add $file 2>@1 } ret]
		}
		if { $err } { break }
		$tree item element configure $item 0 e_text_sel -fill blue -text $ret
	    }
	    cd $pwd
	    waitstate $w off
	    set dict [cu::get_program_preferences -valueName cvs_update_recursive RamDebugger]
	    $w set_uservar_value messages [linsert0 -max_len 20 [dict_getd $dict messages ""] $message]
	    dict set dict messages [$w give_uservar_value messages]
	    cu::store_program_preferences -valueName cvs_update_recursive RamDebugger $dict
	}
	remove {
	    lassign $args tree sel_ids
	    set files ""
	    foreach item $sel_ids {
		if { ![regexp {^(?:UNCHANGED|DELETED|MISSING|UPDATE)\s+(\S+)} [$tree item text $item 0] {} file] } { continue }
		lappend files $file
	    }
	    if { [string length $files] < 100 } {
		set filesT $files
	    } else {
		set filesT "[lindex $files 0] ... [lindex $files end]"
	    }
	    set txt [_ "Are you sure to remove from fossil %d files? (%s)" [llength $files] $filesT]
	    set ret [snit_messageBox -icon question -title [_ "Remove files"] -type okcancel \
		    -default ok -parent $w -message $txt]
	    if { $ret eq "cancel" } { return }

	    waitstate $w on Remove
	    set pwd [pwd]
	    foreach item $sel_ids {
		if { ![regexp {^(?:UNCHANGED|DELETED|MISSING|UPDATE)\s+(\S+)} [$tree item text $item 0] {} file] } { continue }
		set dir [$tree item text [$tree item parent $item] 0]
		cd $dir

		set fossil [auto_execok fossil]
		set info [exec $fossil info]
		regexp -line {^local-root:\s*(.*)} $info {} dir
		cd $dir
		set err [catch { exec $fossil rm $file 2>@1 } ret]
		
		if { $err } { break }
		$tree item element configure $item 0 e_text_sel -fill blue -text $ret
	    }
	    cd $pwd
	    waitstate $w off
	}
	revert {
	    lassign $args tree sel_ids
	    set files ""
	    foreach item $sel_ids {
		if { [regexp {^[MA]\s(\S+)} [$tree item text $item 0] {} file] } { 
		    lappend files $file
		} elseif { [regexp {(\w{2,})\s+(.*)} [$tree item text $item 0] {} mode file] && $mode ne "UNCHANGED" } {
		    lappend files $file
		}
	    }
	    if { [string length $files] < 100 } {
		set filesT $files
	    } else {
		set filesT "[lindex $files 0] ... [lindex $files end]"
	    }
	    set txt [_ "Are you sure to revert %d files to repository contents and loose local changes? (%s)" [llength $files] $filesT]
	    set ret [snit_messageBox -icon question -title [_ "Revert files"] -type okcancel \
		    -default ok -parent $w -message $txt]
	    if { $ret eq "cancel" } { return }

	    waitstate $w on Revert
	    set pwd [pwd]
	    foreach item $sel_ids {
		set dir [$tree item text [$tree item parent $item] 0]
		cd $dir
		if { [regexp {(\w{2,})\s+(.*)} [$tree item text $item 0] {} mode file] && $mode ne "UNCHANGED" } {
		    set info [exec $fossil info]
		    regexp -line {^local-root:\s*(.*)} $info {} dir
		    cd $dir
		    if { $fossil_version == 0 } {
		        set err [catch { exec $fossil revert --yes $file 2>@1 } ret]
		    } else {
		        set err [catch { exec $fossil revert $file 2>@1 } ret]   
		    }
		} elseif { [regexp {^[MA]\s(\S+)} [$tree item text $item 0] {} file] } {
		    set err [catch { exec cvs update -C $file } ret] 
		}
		if { $err } { break }
		$tree item element configure $item 0 e_text_sel -fill blue -text $ret
	    }
	    cd $pwd
	    waitstate $w off
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
		set pwd [pwd]
		catch { 
		    cd $dir
		    # if there is a problem with tme modification time, fossil seem to remember this checking later
		    exec $fossil stat --sha1sum
		}
		cd $pwd
		update_recursive_accept -item $item $w $what_in $dir $tree [$tree item parent $item] 0
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
	    update_recursive_accept $w [list $what_in $version] $dir $tree 0 0
	}
	view {
	    lassign $args tree item
	    foreach i [$tree item children $item] {
		update_recursive_cmd $w view $tree $i
	    }
	    if { $item == 0 } { return }
	    
	    if { [regexp {^(\w+)\s+(.*)} [$tree item text $item 0] {} mode file] && $mode eq "UNCHANGED" } {
		set to_hide 1
	    } elseif { [regexp {^\s*\?} [$tree item text $item 0]] } {
		set to_hide 1
	    } else {
		set to_hide 0
	    }
	    if { $to_hide } {
		if { ![$w give_uservar_value view_unchanged] } {
		    $tree item configure $item -visible 0
		} else {
		    $tree item configure $item -visible 1
		    if { [regexp {^(\w+)\s+(.*)} [$tree item text $item 0] {} mode file] && $mode eq "UNCHANGED" } {
		        $tree item element configure $item 0 e_text_sel -fill grey
		    } elseif { [regexp {^\s*\?} [$tree item text $item 0]] } {
		        $tree item element configure $item 0 e_text_sel -fill brown
		    }
		}
	    }
	}
	open_program {
	    lassign $args what_in tree sel_ids files
	    if { $sel_ids eq "" && $tree ne "" } {
		set sel_ids [$tree selection get]
	    }
	    switch $what_in {
		tkdiff - tkdiff_ignore_blanks {
		    set files_list ""
		    set fileF ""
		    set pwd [pwd]
		    set errList ""
		    if { $what_in eq "tkdiff_ignore_blanks" } {
		        set ignore_blanks -b
		    } else {
		        set ignore_blanks ""
		    }
		    foreach file $files {
		        cd [file dirname $file]
		        set fossil [auto_execok fossil]
		        set err [catch { exec $fossil info } info]
		        if { !$err } {
		            regexp -line {^local-root:\s*(.*)} $info {} dirF
		            set len [llength [file split $dirF]]
		            set file [file join {*}[lrange [file split $file] $len end]]
		            lappend files_list [list EDITED $dirF $file]
		        } else {
		            set dir [file dirname $file]
		            lappend files_list [list M [file dirname $file] [file tail $file]]
		        }
		        cd $pwd
		    }
		    foreach item $sel_ids {
		        if { ![regexp {^(\w+)\s+(\S+)} [$tree item text $item 0] {} mode file] } { continue }
		        set dir [$tree item text [$tree item parent $item] 0]
		        lappend files_list [list $mode $dir $file]
		    }
		    set fossil [auto_execok fossil]
		    set num_open 0
		    foreach i $files_list {
		        lassign $i mode dir file
		        if { [string length $mode] == 1 } {
		            set fileF [file join $dir $file]
		        } else {
		            cd $dir
		            set err [catch {
		                    set info [exec $fossil info]
		                    regexp -line {^local-root:\s*(.*)} $info {} dirF
		                } ret]
		            if { $err } {
		                snit_messageBox -message $ret -parent $w
		                return
		            }
		            set fileF [file join $dirF $file]
		        }
		        set rex {gdiff-command\s+\((local|global)\)\s+(.*)}
		        
		        if { [file exists $fileF] } {
		            if { [string length $mode] == 1 } {
		                cd [file dirname $fileF]
		                open_program -new_interp 1 tkdiff {*}$ignore_blanks -r [file tail $fileF]
		            } elseif { ![catch { exec $fossil settings gdiff-command } ret] && [regexp $rex $ret {} {} cmd] } {
		                cd [file dirname $fileF]
		                exec $fossil gdiff $fileF &
		            } else {
		                cd $dirF
		                set err [catch { parse_timeline [exec $fossil descendants] } ret]
		                if { !$err && [llength $ret] > 0 } {
		                    lassign [lindex $ret 0] date time checkin comment user tags
		                    set err [catch { parse_finfo [exec $fossil finfo $file] } ret]
		                    set finfo_list $ret
		                } else {
		                    set err 1
		                }
		                if { $err } {
		                    cd $pwd
		                    snit_messageBox -message [_ "Fossil version is too old. It needs subcommand 'finfo'. Please, upgrade (%s)" $ret] \
		                        -parent $w
		                    return
		                }
		                set found 0
		                foreach i $finfo_list {
		                    lassign $i date_in checkin_in comment_in user_in artifact tags
		                    set len [string length $checkin_in]
		                    if { [string equal -length $len $checkin $checkin_in] } {
		                        set found 1
		                        break
		                    }
		                    if { $date_in < $date } {
		                        break
		                    }
		                }
		                if { !$found } {
		                    set ret [parse_timeline [exec $fossil timeline parents $checkin -n 2000]]
		                    foreach i $ret {
		                        lassign $i date time checkin comment user tags
		                        foreach i $finfo_list {
		                            lassign $i date_in checkin_in comment_in user_in artifact
		                            set len [string length $checkin_in]
		                            if { [string equal -length $len $checkin $checkin_in] } {
		                                set found 1
		                                break
		                            }
		                            if { $date_in < $date } {
		                                break
		                            }
		                        }
		                        if { $found } {
		                            break
		                        }
		                    }
		                }
		                if { !$found } {
		                    cd $pwd
		                    snit_messageBox -message [_ "Could not find version to compare"] -parent $w
		                    return
		                }
		                set c [string range $checkin 0 9]
		                exec $fossil artifact $artifact $file.$c.$date
		                
		                if { [winfo screenheight .] > 500 } {
		                    set y [expr {[winfo screenheight .]-500+$num_open*40}]
		                    if { $y > [winfo screenheight .] - 100 } {
		                        set y [expr {[winfo screenheight .] - 100}]
		                    }
		                    set geom_opt [list -geometry 80x20+0+$y]
		                } else {
		                    set geom_opt ""
		                }
		                set err [catch { open_program -new_interp 1 tkdiff {*}$geom_opt {*}$ignore_blanks \
		                            $file $file.$c.$date } ret]
		                if { $err } {
		                    lappend errList $ret
		                } else {
		                    incr num_open
		                }
		                file delete -force $file.$c.$date
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
		    open_program tkcvs -dir [$w give_uservar_value dir]
		}
		fossil_ui {
		    if { [llength $sel_ids] == 0 } {
		        set sel_ids [$tree selection get]
		    }
		    set dirs ""
		    foreach item $sel_ids {
		        set itemL $item
		        while { $itemL != 0 } {
		            if { [file isdirectory [$tree item text $itemL 0]] } {
		                lappend dirs [$tree item text $itemL 0]
		            }
		            set itemL [$tree item parent $itemL]
		        }
		    }
		    lappend dirs [$w give_uservar_value dir]
		    set url_suffix $files
		    set pwd [pwd]
		    foreach dir $dirs {
		        cd $dir
		        if { [catch { exec $fossil info }] } { continue }
		        
		        if { ![$w exists_uservar local_remote_web_browser] } {
		            set err [catch {
		                    set remote [exec $fossil remote]
		                    set autosync 0
		                    regexp {\d\s*$} [exec $fossil settings autosync] autosync
		                }]
		            if { !$err && $remote ne "off" } {
		                set w_lr [dialogwin_snit $w.lr -title [_ "Open local or remote"] -class RamDebugger -entrytext \
		                        [_ "open local or remote web page?"]]
		                set f [$w_lr giveframe]
		                ttk::radiobutton $f.r1 -text [_ "Open local web page"] -variable \
		                    [$w give_uservar local_remote_web_browser] -value local
		                ttk::radiobutton $f.r2 -text [_ "Open remote web page"] -variable \
		                    [$w give_uservar local_remote_web_browser] -value remote
		                grid $f.r1 -padx 20 -pady 2 -sticky w
		                grid $f.r2 -padx 20 -pady 2 -sticky w

		                $w set_uservar_value remote_web_browser $remote
		                if { $autosync } {
		                    $w set_uservar_value local_remote_web_browser remote
		                } else {
		                    $w set_uservar_value local_remote_web_browser local
		                }
		                tk::TabToWindow $f.r1
		                bind $w_lr <Return> [list $w invokeok]
		                set action [$w_lr createwindow]
		                destroy $w_lr
		                if { $action < 1 } {
		                    $w unset_uservar local_remote_web_browser
		                    return
		                }
		                update
		            } else {
		                $w set_uservar_value local_remote_web_browser local
		            }
		        }
		        if { [$w give_uservar_value local_remote_web_browser] eq "remote" } {
		            set url [$w give_uservar_value remote_web_browser]$url_suffix
		            cu::file::execute url $url
		        } else {
		            set port ""
		            set fin [open "|[list $fossil server]" r]
		            fconfigure $fin -blocking 0
		            while { ![eof $fin] } {
		                set line [read $fin]
		                if { [string length $line] } {
		                    regexp {port\s+(\d+)} $line {} port
		                    break
		                }
		            }
		            if { $port ne "" } {
		                set url http://localhost:$port$url_suffix
		                set line [exec $fossil settings web-browser]
		                set browser ""
		                regexp {\((local|global)\)\s+(.*)$} $line {} {} browser
		                if { $browser ne "" } {
		                    exec $browser $url &
		                } else {
		                    cu::file::execute url $url
		                }
		            } else {
		                snit_messageBox -message [_ "Could not open server"] -parent $w
		                return
		            }
		        }
		        break
		    }
		    cd $pwd
		}
	    }
	}
	diff_window {
	    lassign $args tree sel_ids files
	    set files_list ""
	    set pwd [pwd]
	    foreach file $files {
		cd [file dirname $file]
		set err [catch { exec $fossil info } info]
		if { !$err } {
		    regexp -line {^local-root:\s*(.*)} $info {} dirF
		    set len [llength [file split $dirF]]
		    set file [file join {*}[lrange [file split $file] $len end]]
		    lappend files_list [list EDITED $dirF $file]
		} else {
		    set dir [file dirname $file]
		    lappend files_list [list M [file dirname $file] [file tail $file]]
		}
		cd $pwd
	    }
	    if { $sel_ids eq "" && $tree ne "" } {
		set sel_ids [$tree selection get]
	    }
	    foreach item $sel_ids {
		if { ![regexp {^(\w+)\s+(\S+)} [$tree item text $item 0] {} mode file] } { continue }
		set dir [$tree item text [$tree item parent $item] 0]
		lappend files_list [list $mode $dir $file]
	    }
	    if { [llength $files_list] > 1 } {
		snit_messageBox -message [_ "Differences window can only be used with one file"] -parent $w
		return
	    }
	    lassign [lindex $files_list 0] mode dir file
	    cd $dir
	    set err [catch { parse_finfo [exec $fossil finfo $file] } ret]
	    if { $err } {
		set err_version [catch { exec $fossil version }]
		set err_info [catch { exec $fossil info }]
	    }
	    cd $pwd
	    if { $err } {
		if { $err_version } {
		    set txt [_ "Fossil executable is not installed or not in the PATH"]
		} elseif { $err_info } {
		    set txt [_ "File not located in a fossil checkout"]
		} else {
		    set txt [_ "Fossil version is too old. It needs subcommand 'finfo'. Please, upgrade"]
		}
		snit_messageBox -message $txt -parent $w
		return
	    }
	    set wD $w.diffs
	    destroy $wD
	    dialogwin_snit $wD -title [_ "Choose version"] -class RamDebugger -entrytext \
		[_ "Choose one or two versions for file '%s'" $file] -okname [_ View] -cancelname [_ Close] \
		-grab 1 -transient 1 -callback [namespace code [list "update_recursive_cmd" $w diff_window_accept $dir $file]]
	    set f [$wD giveframe]
	    
	    set columns [list \
		    [list  20 [_ "Date"] left text 0] \
		    [list 12 [_ "Checkin"] left text 0] \
		    [list  45 [_ "Text"] center text 0] \
		    [list  9 [_ "User"] left text 0] \
		    [list  12 [_ "Artifact"] left text 0] \
		    [list  12 [_ "Tags"] left text 0] \
		    ]
	    fulltktree $f.lf -width 750 \
		-columns $columns -expand 0 \
		-selectmode extended -showheader 1 -showlines 0  \
		-indent 0 -sensitive_cols all \
		-selecthandler2 "[list $wD invokeok];#"
	    $wD set_uservar_value tree $f.lf
	    
	    ttk::checkbutton $f.cb1 -text [_ "Ignore white space"] -variable [$wD give_uservar ignore_blanks 0]
	    
	    set num 0
	    foreach i $ret {
		lassign $i date checkin comment user artifact tags
		$f.lf insert end [list $date $checkin $comment $user $artifact $tags]
		incr num
	    }
	    if { $num } {
		$f.lf selection add 1
		$f.lf activate 1
	    }
	    focus $f.lf
	    
	    grid $f.lf -stick nsew -padx 2 -pady 2
	    grid $f.cb1 -sticky w -padx 2 -pady 2
	    grid rowconfigure $f 1 -weight 1
	    grid columnconfigure $f 0 -weight 1

	    set action [$wD createwindow]
	}
	diff_window_accept {
	    lassign $args dir file wD
	    set action [$wD giveaction] 
	    set tree [$wD give_uservar_value tree]
	    if { [$wD give_uservar_value ignore_blanks] } {
		set ignore_blanks -b
	    } else {
		set ignore_blanks ""
	    }
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
		lassign [lindex $selecteditems 0] date1 checkin1 - - artifact1
		set c1 [string range $checkin1 0 9]
		set pwd [pwd]
		cd $dir
		exec $fossil artifact $artifact1 $file.$c1.$date1
		if { [llength $selecteditems] == 1 } {
		    set err [catch { open_program -new_interp 1 tkdiff {*}$ignore_blanks $file $file.$c1.$date1 } ret]
		} else {
		    lassign [lindex $selecteditems 1] date2 checkin2 - - artifact2
		    set c2 [string range $checkin2 0 9]
		    exec $fossil artifact $artifact2 $file.$c2.$date2
		    set err [catch { open_program -new_interp 1 tkdiff {*}$ignore_blanks $file.$c1.$date1 $file.$c2.$date2 } ret]
		}
		if { $err } {
		    snit_messageBox -message $ret -parent $wD
		}
		file delete -force $file.$c1.$date1
		if { [llength $selecteditems] == 2 } {
		    file delete -force $file.$c2.$date2
		}
		cd $pwd
	    }
	}
	default {
	    error "error in update_recursive_cmd"
	}
    }
}

proc RamDebugger::VCS::open_program { args } {
    variable openprogram_uniqueid
    
    upvar #0 ::RamDebugger::topdir topdir
    
    set optional {
	{ -new_interp boolean 0 }
    }
    set compulsory "what"
    set argv [parse_args -raise_compulsory_error 0  $optional $compulsory $args]

    switch $what {
	visualregexp { set file [file join $topdir addons visualregexp visual_regexp.tcl] }
	tkcvs {
	    set file [file join $topdir addons tkcvs bin tkcvs.tcl]
	}
	tkdiff { set file [file join $topdir addons tkcvs bin tkdiff.tcl] }
    }
    if { $new_interp } {
	set what $what[incr openprogram_uniqueid]
    }
    if { [interp exists $what] } {
	interp delete $what
    }
    interp create $what
    interp alias $what exit_interp "" interp delete $what
    $what eval [list proc exit { args } "destroy . ; exit_interp"]
    interp alias $what puts "" RamDebugger::_OpenProgram_puts
    $what eval [list set argc [llength $argv]]
    $what eval [list set argv $argv]
    $what eval [list load {} Tk]
    $what eval [list source $file]
}

if { $argv0 eq [info script] || ( [info exists ::starkit::topdir] && 
    [file root [file tail $::starkit::topdir]] eq "vcs-ramdebugger") } {
    wm withdraw .
    source [file join [file dirname [info script]] mini_compass_utils.tcl]
    
    
    if { $::tcl_platform(platform) eq "windows" } {
	event add <<ContextualPress>> <ButtonPress-3>
	event add <<Contextual>> <ButtonRelease-3>
	event add <<Contextual>> <App>
	set ::control Control
	set ::control_txt Ctrl
    } elseif { [tk windowingsystem] eq "aqua" } {
	event add <<ContextualPress>> <ButtonPress-2>
	event add <<Contextual>> <ButtonRelease-2>
	set ::control Command
	set ::control_txt Command
	
	foreach ev [bind Text] {
	    if { [regsub {Control} $ev {Command} evC] } {
		bind Text $evC [bind Text $ev]
	    }
	}
    } else {
	event add <<ContextualPress>> <ButtonPress-3>
	event add <<Contextual>> <ButtonRelease-3>
	set ::control Control
	set ::control_txt Ctrl
    }
    
    #package require compass_utils
    set RamDebugger::currentfile ""
    set RamDebugger::topdir [file dirname [file dirname [file normalize [info script]]]]

    if { $::tcl_platform(platform) eq "windows" } {
	set usecommR 1
	if { [info exists ::env(APPDATA)] } {
	    set RamDebugger::AppDataDir [file join $::env(APPDATA) RamDebugger]
	} else {
	    package require registry
	    set key {HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion}
	    append key {\Explorer\Shell Folders}
	    set err [catch { registry get $key AppData } AppData]
	    if { !$err } {
		set RamDebugger::AppDataDir [file join [registry get $key AppData] RamDebugger]
	    } else {
		set RamDebugger::AppDataDir [file join $::env(HOME) .RamDebugger]
	    }
	}
    } else {
	set RamDebugger::AppDataDir [file join $::env(HOME) .RamDebugger]
    }
    
    set RamDebugger::VCS::try_threaded debug
    if { [lindex $argv 0] ne "" } {
	set w [RamDebugger::VCS::update_recursive "" this [lindex $argv 0]]
    } else {
	set w [RamDebugger::VCS::update_recursive "" last]
    }

    bind $w <Destroy> {
	if { "%W" eq [winfo toplevel %W] } {
	    exit
	}
    }
    bind $w <Control-q> [list destroy $w]
}






