
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

    if { ![info exists cvsrootdir] } {
	if { $::tcl_platform(platform) eq "windows" } {
	    set null NUL:
	} else {
	    set null /dev/null
	}
	set cvsrootdir [file join $RamDebugger::AppDataDir cvsroot]
	set cvsworkdir  [file join $RamDebugger::AppDataDir cvswork]

	if { ![file exists $cvsrootdir] } {
	    file mkdir $cvsrootdir
	    file mkdir $cvsworkdir
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
}

proc RamDebugger::CVS::SaveRevision { { raiseerror 0 } } {
    variable cvsworkdir
    variable null

    RamDebugger::WaitState 1

    if { [regexp {^\*.*\*$} $RamDebugger::currentfile] } {
	set file [string trim $RamDebugger::currentfile *]
	set lfile $file.[sha1::sha1 $file]
    } else {
	set file [file normalize $RamDebugger::currentfile]
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
    if { $file eq "" } {
	if { [regexp {^\*.*\*$} $RamDebugger::currentfile] } {
	    set file $RamDebugger::currentfile
	} else {
	    set file [file normalize $RamDebugger::currentfile]
	}
    }
    if { [regexp {^\*.*\*$} $file] } {
	set file [string trim $file *]
	set lfile $file.[sha1::sha1 $file]
    } else {
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
	WarnWin "File '$file' has no revisions"
	return
    }
    RamDebugger::WaitState 0

    set w $RamDebugger::text._openrev
    dialogwin_snit $w -title "Choose revision" -entrytext \
	"Choose a revision for file '$file'" -morebuttons [list Diff]
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

    set sw [ScrolledWindow $f.lf -relief sunken -borderwidth 0]
    $w set_uservar_value tablelist [tablelist::tablelist $sw.lb -width 50\
		  -exportselection 0 \
		  -columns [list \
		                4  Rev      left \
		                20  date     left \
		                8 author   center \
		                5  lines    left \
		               ] \
		  -labelcommand tablelist::sortByColumn \
		  -background white \
		  -selectbackground navy -selectforeground white \
		  -stretch all -selectmode extended \
		  -highlightthickness 0]
  
    $sw setwidget $sw.lb
    $sw.lb insertlist end $list
    $sw.lb selection set 0
    $sw.lb activate 0
    focus $sw.lb
    bind [$sw.lb bodypath] <Double-1> [list $w invokeok]
    bind [$sw.lb bodypath] <Return> [list $w invokeok]

    grid $sw -stick nsew
    grid rowconfigure $f 1 -weight 1
    grid columnconfigure $f 0 -weight 1

    set action [$w createwindow]
    while 1 {
	set selecteditems ""
	foreach i [$sw.lb curselection] {
	    lappend selecteditems [$sw.lb get $i]
	}
	if { $action <= 0 } {
	    destroy $w
	    return
	}
	if { $action == 1 } {
	    if { [llength $selecteditems] != 1  } {
		WarnWin "Select one revision in order to visualize it" $w
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
	    WarnWin "Select one or two revisions in order to visualize the differences" $w
	} else {
	    cd $cvsworkdir
	    set deletefiles ""
	    if { [llength $selecteditems] == 1 } {
		set revision [lindex $selecteditems 0 0]
		if { [regexp {^\*.*\*$} $RamDebugger::currentfile] } {
		    set currentfile [string trim $RamDebugger::currentfile *]
		} else {
		    set currentfile [file normalize $RamDebugger::currentfile]
		}
		if { $file eq $currentfile } {
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
    dialogwin_snit $w -title "Choose revision file" -entrytext \
	"Choose a revision file to check its revisions or to remove revisions history" \
	-morebuttons [list "Remove..." "Purge..."] -okname "Revisions"
    set f [$w giveframe]

    foreach "list totalsize_show retcvslog" [_showallfiles_update] break

    label $f.lsize -text "Total size of revision storage: $totalsize_show"
    set sw [ScrolledWindow $f.lf -relief sunken -borderwidth 0]
    $w set_uservar_value tablelist [tablelist::tablelist $sw.lb -width 85\
		  -exportselection 0 \
		  -columns [list \
		                15  file      left \
		                50  path     left \
		                15  "storage size"     left \
		                10  "file size" left \
		               ] \
		  -labelcommand tablelist::sortByColumn \
		  -background white \
		  -selectbackground navy -selectforeground white \
		  -stretch all -selectmode extended \
		  -highlightthickness 0]
  
    foreach i "0 1 2 3" { $sw.lb columnconfigure $i -sortmode dictionary }
    $sw setwidget $sw.lb
    $sw.lb insertlist end $list
    $sw.lb selection set 0
    $sw.lb activate 0
    focus $sw.lb
    bind [$sw.lb bodypath] <Double-1> [list $w invokeok]
    bind [$sw.lb bodypath] <Return> [list $w invokeok]

    grid $f.lsize -sticky w
    grid $sw -stick nsew
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
	foreach i [$sw.lb curselection] {
	    lappend selecteditems [$sw.lb get $i]
	}
	if { $action == 1 } {
	    if { [llength $selecteditems] != 1  } {
		WarnWin "Select one file in order to visualize its revisions"
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
		    WarnWin "Select one or more files in order to remove the revisions"
		} else {
		    WarnWin "Select one or more files in order to purge the revisions"
		}
		set isgood 0
	    } else {
		set len [llength $selecteditems]
		if { $action == 2 } {
		    set title "Remove revisions"
		    set txt "Are you user to remove revision history for the $len selected files?"
		} else {
		    set title "Purge revisions"
		    set txt "Are you user to purge revision history for the $len selected files?"
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
		foreach "list totalsize_show retcvslog" [_showallfiles_update] break
		$f.lsize configure -text "Total size of revision storage: $totalsize_show"
		$sw.lb delete 0 end
		$sw.lb insertlist end $list
		RamDebugger::WaitState 0
	    }
	}
	set action [$w waitforwindow]
    }
}

