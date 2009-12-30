#
# TCL Library for TkCVS
#

#
# $Id: cvs.tcl,v 1.2 2006-06-30 17:40:13 ramsan Exp $
# 
# Contains procedures used in interaction with CVS.
#

proc cvs_notincvs {} {
  cvsfail "This directory is not in CVS."
}

proc cvs_incvs {} {
  cvsfail "You can\'t do that here because this directory is already in CVS."
}

proc cvs_remote_bad {} {
  cvsfail "You can\'t do that with a remote CVS repository."
}

#
#  Create a temporary directory
#  cd to that directory
#  run the CVS command in that directory
#
#  returns: the current wd (ERROR) or the sandbox directory (OK)
#
proc cvs_sandbox_runcmd {cmd output_var} {
  global cvscfg
  global cwd

  upvar $output_var view_this

  # Big note: the temp directory fed to a remote servers's command line
  # needs to be seen by the server.  It can't cd to an absolute path.
  # In addition it's fussy about where you are when you do a checkout -d.
  # Best avoid that altogether.
  gen_log:log T "ENTER ($cmd $output_var)"
  set pid [pid]
  
  cd $cvscfg(tmpdir)
  gen_log:log F "CD [pwd]"
  if {! [file isdirectory cvstmpdir.$pid]} {
    gen_log:log F "MKDIR cvstmpdir.$pid"
    file mkdir cvstmpdir.$pid
  }

  cd cvstmpdir.$pid
  gen_log:log F "CD [pwd]"

  gen_log:log C "$cmd"
  set ret [catch {eval "exec $cmd"} view_this]
  #if {$ret} {
    #cvsfail "$view_this"
    #cd $cwd
    #gen_log:log F "CD [pwd]"
    #gen_log:log T "LEAVE ERROR - cvs command failed"
    #return $cwd
  #}
  return $cvscfg(tmpdir)/cvstmpdir.$pid
}

#
#  cvs_sandbox_filetags
#   assume that the sandbox contains the checked out files
#   return a list of all the tags in the files
#
proc cvs_sandbox_filetags {mcode filenames} {
  global cvscfg
  global cvs

  set pid [pid]
  gen_log:log T "ENTER"
  
  cd [file join $cvscfg(tmpdir) cvstmpdir.$pid $mcode]
  set commandline "$cvs -d $cvscfg(cvsroot) -n log -l $filenames"
  gen_log:log C "$commandline"
  set ret [catch {eval "exec $commandline"} view_this]
  if {$ret} {
    cvsfail $view_this
    gen_log:log T "LEAVE ERROR"
    return $keepers
  }
  set view_lines [split $view_this "\n"]
  foreach line $view_lines {
    if {[string index $line 0] == "\t" } {
      regsub -all {[\t ]*} $line "" tag
      append keepers "$tag "
    }
  }
  gen_log:log T "LEAVE"
  return $keepers
}

proc cvs_remove {args} {
#
# This deletes a file from the directory and the repository,
# asking for confirmation first.
#
  global cvs
  global incvs
  global cvscfg

  gen_log:log T "ENTER ($args)"
  set exec_idx [exec_command_init "CVS Delete"]
  foreach file $args {
    gen_log:log F "DELETE $file"
    file delete -force $file
    if {$incvs} {
      set commandline "$cvs -d $cvscfg(cvsroot) remove $file"
      gen_log:log C "$commandline"
      exec_command_body $exec_idx "$commandline"
    }
  }
  exec_command_end $exec_idx
  if {$cvscfg(auto_status) == "true"} {
    setup_dir
  }
  gen_log:log T "LEAVE"
}

proc cvs_history {allflag mcode} {
  global cvs
  global cvscfg

  set all ""
  gen_log:log T "ENTER ($allflag $mcode)"
  if {$allflag == "all"} {
    set all "-a"
  }
  if {$mcode == ""} {
    set commandline "$cvs -d $cvscfg(cvsroot) history $all"
  } else {
    set commandline "$cvs -d $cvscfg(cvsroot) history $all -n $mcode"
  }
  # Note: If $all, it would be nice to process the output
  gen_log:log C "$commandline"
  exec_command "CVS Checkouts" "$commandline"
  gen_log:log T "LEAVE"
}

proc cvs_add {binflag args} {
#
# This adds a file to the repository.
#
  global cvs
  global cvscfg
  global incvs

  gen_log:log T "ENTER ($binflag $args)"
  if {! $incvs} {
    cvs_notincvs
    return 1
  }
  set filelist [join $args]
  if {$filelist == $cvscfg(thisdir)} {
    set mess "This will add all new files"
  } else {
    set mess "This will add these files:\n\n"
    foreach file $filelist {
      append mess "   $file\n"
    }  
  }

  set exec_idx [exec_command_init "CVS Add"]
  if {$filelist == $cvscfg(thisdir)} {
    foreach file [glob -nocomplain $cvscfg(aster)] {
      set commandline "$cvs -d $cvscfg(cvsroot) add $binflag $file"
      gen_log:log C "$commandline"
      exec_command_body $exec_idx "$commandline"
    }
  } else {
    foreach file $filelist {
      set commandline "$cvs -d $cvscfg(cvsroot) add $binflag $file"
      gen_log:log C "$commandline"
      exec_command_body $exec_idx "$commandline"
    }
  }
  exec_command_end $exec_idx
  if {$cvscfg(auto_status) == "true"} {
    setup_dir
  }

  gen_log:log T "LEAVE"
}

proc cvs_diff {args} {
#
# This diffs a file with the repository.
#
  global cvs
  global cvscfg
  global incvs

  gen_log:log T "ENTER ($args)"
  if {! $incvs} {
    cvs_notincvs
    return 1
  }

  set filelist [join $args]
  if {$filelist == $cvscfg(thisdir)} {
    cvsfail "Please select one or more files to compare!"
  } else {
    foreach file $filelist {
      gen_log:log C "$cvscfg(tkdiff) $file"
      catch {eval "exec $cvscfg(tkdiff) $file &"} view_this
    }
  }
  gen_log:log T "LEAVE"
}

proc cvs_diff_r {rev1 rev2 args} {
#
# This diffs a file with the repository, using two revisions or tags.
#
  global cvs
  global cvscfg
  global incvs
 
  gen_log:log T "ENTER ($rev1 $rev2 $args)"

  if {$rev1 == {} || $rev2 == {}} {
    cvsfail "Must have two revision numbers for this function!"
    return 1
  }
 
  foreach file $args {
    gen_log:log C "$cvscfg(tkdiff) -r$rev1 -r$rev2 $file"
    catch {eval "exec $cvscfg(tkdiff) -r$rev1 -r$rev2 $file &"} view_this
  }
  gen_log:log T "LEAVE"
}

proc cvs_view_r {rev args} {
#
# This views a specific revision of a file in the repository.
#
  global cvs
  global incvs
  global cvscfg
 
  gen_log:log T "ENTER ($rev $args)"
  set filelist [join $args]
  if {$filelist == $cvscfg(thisdir)} {
    foreach file [glob -nocomplain $cvscfg(aster)] {
      set commandline \
	 "$cvs -d $cvscfg(cvsroot) update -p -r $rev $file 2>$cvscfg(null)"
      gen_log:log C "$commandline"
      catch {eval "exec $commandline"} view_this
      ### exec_command: problems to handle stdout/stderr seperately
      view_output "CVS View: $file" $view_this
    }
  } else {
    foreach file $filelist {
      set commandline \
	 "$cvs -d $cvscfg(cvsroot) update -p -r $rev $file 2>$cvscfg(null)"
      gen_log:log C "$commandline"
      catch {eval "exec $commandline"} view_this
      ### exec_command: problems to handle stdout/stderr seperately
      view_output "CVS View: $file" $view_this
    }
  }
  gen_log:log T "LEAVE"
}

proc cvs_logcanvas {args} {
#
# This looks at the revision log of a file.  It's is called from workdir.tcl,
# when we are in a CVS-controlled directory.  Merges are enabled.
#
  global cvs
  global incvs
  global cvscfg

  gen_log:log T "ENTER ($args)"
  if {! $incvs} {
    cvs_notincvs
    return 1
  }
  set filelist [join $args]
  if {$filelist == $cvscfg(thisdir)} {
    cvsfail "Please select one or more files!"
    return
  }

  foreach file $filelist {
    set commandline "$cvs -d $cvscfg(cvsroot) log $file"
    gen_log:log C "$commandline"
    set ret [catch {eval "exec $commandline"} view_this]
    if {$ret} {
      cvsfail $view_this
      return
    }
    # Set up the log diagram
    new_logcanvas $file $view_this
  }
  gen_log:log T "LEAVE"
}

proc cvs_log {args} {
#
# This looks at a log from the repository.
# Called by Workdir menu Reports->"CVS log ..."
#
  global cvs
  global incvs
  global cvscfg

  gen_log:log T "ENTER ($args)"
  if {! $incvs} {
    cvs_notincvs
    return 1
  }

  set filelist [join $args]

  set commandline "$cvs -d $cvscfg(cvsroot) log $filelist"
  gen_log:log C "$commandline"
  catch {eval "exec $commandline"} raw_log

  if {$cvscfg(ldetail) == "verbose"} {
    view_output "CVS Log ($cvscfg(ldetail))" $raw_log
  } else {
    set log_lines [split $raw_log "\n"]
    set cooked_log ""
    set n -9999
    foreach logline $log_lines {
      # Beginning of a file's record
      gen_log:log D "$logline"
      if {[string match "Working file:*" $logline]} {
	append cooked_log "$logline\n"
	set n -9999
      }
      # Beginning of a revision
      if {[string match "----------------------------" $logline]} {
	append cooked_log "$logline\n"
	set n 0
      }
      # In between except line 2
      if {$n == 1} {
	append cooked_log "$logline\n"
      }
      if {$n >= 3} {
	append cooked_log "$logline\n"
      }
      incr n
    }
    view_output "CVS Log ($cvscfg(ldetail))" $cooked_log
  }

  gen_log:log T "LEAVE"
}

proc cvs_commit {revision comment args} {
#
# This commits changes to the repository.
#
# The parameters work differently here -- args is a list.  The first
# element of args is a list of file names.  This is because I can't
# use eval on the parameters, because comment contains spaces.
#
  global cvs
  global cvscfg
  global incvs
  global filelist

  gen_log:log T "ENTER ($revision $comment $args)"
  if {! $incvs} {
    cvs_notincvs
    return 1
  }

  if {$comment == ""} {
    cvsfail "You must enter a comment!"
    return 1
  }
  regsub -all "\"" $comment "\\\"" comment

  set filelist [lindex $args 0]

  #
  #   Check if mode ist "Locking" and no file explicitly selected.
  #   10-Jan-2000  lcs
  #
  if { $filelist == $cvscfg(thisdir) && $cvscfg(workmode) == "locking" } {
    set errstr    "This will commit your changes to ** ALL ** files in"
    append errstr " and under this directory and you are working in"
    append errstr " the traditional locking mode.  This will currently"
    append errstr " set all files in and under this directory to"
    append errstr " read-only.  This maybe not what you intend, so please"
    append errstr " explicitly select the files you want to checkin.\n"
    cvsfail $errstr
    gen_log:log T "LEAVE ERROR"
    return
  }

  # changed the message to be a little more explicit.
  # -sj
  set commit_output ""
  if { $filelist == $cvscfg(thisdir)} {
    set mess "This will commit your changes to ** ALL ** files in"
    append mess " and under this directory."
  } else {
    foreach file $filelist {
      set commit_output "$commit_output\n$file"
    }
    set mess "This will commit your changes to:$commit_output"
  }
  append mess "\n\nAre you sure?"
  set commit_output ""

  if {[cvsconfirm $mess] == 0} {
    set exec_idx [exec_command_init "CVS Commit"]
    if {$revision != ""} {
      foreach file $filelist {
	set commandline "$cvs -d $cvscfg(cvsroot) commit -r $revision -m \"$comment\" $file"
	gen_log:log C "$commandline"
	exec_command_body $exec_idx "$commandline"

	if { $cvscfg(workmode) == "locking" } {
	  # Commit removes the locks. So we have to set the file to r/o. - lcs
	  set commandline "$cvscfg(chmod_ro_cmd) $file"
	  gen_log:log C "$commandline"
	  exec_command_body $exec_idx "$commandline"
	}
      }
    } else {
      foreach file $filelist {
	set commandline "$cvs -d $cvscfg(cvsroot) commit -m \"$comment\" $file"
	gen_log:log C "$commandline"
	exec_command_body $exec_idx "$commandline"
	if { $cvscfg(workmode) == "locking" } {
	  set commandline "$cvscfg(chmod_ro_cmd) $file"
	  gen_log:log C "$commandline"
	  exec_command_body $exec_idx "$commandline"
	}
      }
    }
    exec_command_end $exec_idx
    if {$cvscfg(auto_status) == "true"} {
      setup_dir
    }
  }

  if { $cvscfg(workmode) == "locking" } {
    set    mess "This commit has removed your locks on the commited files"
    append mess " and reset them to read-only access.\n"
    append mess "If you want to continue editing, please lock them again.\n"
    cvsok $mess
  }
  gen_log:log T "LEAVE"
}

proc cvs_tag {tagname force branch args} {
#
# This tags a file in a directory.
#
  global cvs
  global cvscfg
  global incvs
  global filelist

  gen_log:log T "ENTER ($tagname $force $branch $args)"

  if {! $incvs} {
    cvs_notincvs
    return 1
  }

  if {$tagname == ""} {
    cvsfail "You must enter a tag name!"
    return 1
  }

  set filelist [join $args]

  set exec_idx [exec_command_init "CVS Tag"]
  if {$branch == "yes"} {
    # Make the branch
    set commandline "$cvs -d $cvscfg(cvsroot) tag $force -b $tagname $filelist"
    gen_log:log C "$commandline"
    exec_command_body $exec_idx "$commandline"

    # update so we're on the branch
    set commandline "$cvscfg(cvsroot) update -r $tagname $filelist"
    gen_log:log C "$commandline"
    exec_command_body $exec_idx "$commandline"
  } else {
    set commandline "$cvs -d $cvscfg(cvsroot) tag $force $tagname $filelist"
    gen_log:log C "$commandline"
    exec_command_body $exec_idx "$commandline"
  }
  exec_command_end $exec_idx
  if {$cvscfg(auto_status) == "true"} {
    setup_dir
  }
  gen_log:log T "LEAVE"
}


proc cvs_lock {action rev args} {
#
# This locks/unlocks the file(s) given by $args.
# If $rev is empty the head revision of all files given by $args will be locked.
# If $rev is not empty, then $args must contain exactly one name.
# Currently $rev is always empty.
#
# 10-Jan-2000  lcs
#

  global cvs
  global cvscfg
  global incvs
  global filelist

  gen_log:log T "ENTER ($action $rev $args)"

  if {! $incvs} {
    cvs_notincvs
    return 1
  }

  set filelist [join $args]
  if {$filelist == $cvscfg(thisdir)} {
    cvsfail "Lock / Unlock:\nPlease select one or more files !"
    gen_log:log T "LEAVE (Unselected files)"
    return
  }

  if { $action == "lock" } {
    set title "CVS Lock"
    set lockOpt "-l"
    set chmod_cmd $cvscfg(chmod_rw_cmd)
  } elseif { $action == "unlock" } {
    set title "CVS Unlock"
    set lockOpt "-u"
    set chmod_cmd $cvscfg(chmod_ro_cmd)
  } else {
    cvsfail "cvs_lock(): Invalid mode $action.\nPlease inform your local guru about this."
    gen_log:log T "LEAVE (Invalid action $action)"
    return 1
  }
    
  set exec_idx [exec_command_init $title]

  set files [lindex $filelist 0]

  set commandline "$cvs -d $cvscfg(cvsroot) -l admin $lockOpt$rev $files"
  gen_log:log C   "$commandline"
  exec_command_body $exec_idx "$commandline"

  foreach i $files {
    set commandline "$chmod_cmd $i"
    gen_log:log C   "$commandline"
    exec_command_body $exec_idx "$commandline"
  }
  exec_command_end $exec_idx

  if {$cvscfg(auto_status) == "true"} {
    setup_dir
  }
  gen_log:log T "LEAVE"
}

#
# Build report of locked files
# 13-Jan-2000  lcs
#

proc report_locks {} {
  global cvs
  global cvscfg
  global cwd
  upvar  linenum linenum

  gen_log:log T "ENTER"

  if {! [winfo exists .viewer]} {
    viewer_setup
  } else {
    .viewer.text configure -state normal
    .viewer.text delete 1.0 end
  }
  set linenum 1

  set commandline "$cvs -d $cvscfg(cvsroot) log"
  gen_log:log C "$commandline"
  catch {eval "exec $commandline"} view_this

  set filelist ""
  set found "f"
  set view_lines [split $view_this "\n"]

  foreach line $view_lines {
    if {[string match "Working file: *" $line]} {
      regsub "Working file: " $line "" filename
      lappend filelist $filename
      set locklist($filename) ""
    }
    if {[string match "*locked by*" $line]} {
      lappend locklist($filename) $line
      set found "t"
    }
  }

  .viewer.text insert end "\nLocked files:\n"
  .viewer.text insert end   "--------------------\n"
  incr linenum 2

  if { $found == "t" } {
    foreach filename $filelist {
      if { [llength $locklist($filename)] > 0 } {
	.viewer.text insert end [format "\n %s:\n" $filename]
	incr linenum
	foreach rev $locklist($filename) {
	  .viewer.text insert end [format "    %s\n" $rev]
	  incr linenum
	}
      }
    }
  } else {
    .viewer.text insert end "\n $cwd:"
    .viewer.text insert end "\n   No files locked in and under THIS directory."
    incr linenum 2
  }

  .viewer.text configure -state disabled
  wm deiconify .viewer
  raise .viewer

  gen_log:log T "LEAVE"
}


proc cvs_update {tagname normal_binary action_if_no_tag get_all_dirs dir args} {
#
# This updates the files in the current directory.
#
  global cvs
  global cvscfg
  global incvs

  gen_log:log T "ENTER ($tagname $normal_binary $action_if_no_tag $get_all_dirs $dir $args)"
  if {! $incvs} {
    cvs_notincvs
    return 1
  }

  set filelist [join $args]

  if { $normal_binary == "Normal" } {
      set mess "Using normal (text) mode.\n"
  } elseif { $normal_binary == "Binary" } {
      set mess "Using binary mode.\n"
  } else {
      set mess "Unknown mode:  $normal_binary\n"
  }

  if { $tagname != "BASE"  && $tagname != "HEAD" } {
      append mess "\nIf a file does not have tag $tagname"
      if { $action_if_no_tag == "Remove" } {
	  append mess " it will be removed from your local directory.\n"
      } elseif { $action_if_no_tag == "Get_head" } {
	  append mess " the head revision will be retrieved.\n"
      } elseif { $action_if_no_tag == "Skip" } {
	  append mess " it will be skipped.\n"
      }
  }

  if { $tagname == "HEAD" } {
    append mess "\nYour local files will be updated to the"
    append mess " latest main trunk (head) revision."
    append mess " CVS will try to preserve any local, un-committed changes.\n"
  }

  append mess "\nIf there is a directory in the repository"
  append mess " that is not in your local, working directory,"
  if { $get_all_dirs == "Yes" } {
    append mess " it will be checked out at this time.\n"
  } else {
    append mess " it will not be checked out.\n"
  }

  if {$filelist == $cvscfg(thisdir)} {
    append mess "\nYou are about to download from"
    append mess " the repository to your local"
    append mess " filespace ** ALL ** files which"
    append mess " have changed in it."
  } else {
    append mess "\nYou are about to download from"
    append mess " the repository to your local"
    append mess " filespace these files which"
    append mess " have changed:\n"
  
    foreach file $filelist {
      append mess "\n\t$file"
    }
  }
  append mess "\n\nAre you sure?"
  if {[cvsconfirm $mess] == 0} {
    set str [join $filelist]
    # modified by jo to build the commandline incrementally
    set commandline "$cvs $cvscfg(co_file_mode) update -P"
    if { $normal_binary == "Binary" } {
      append commandline " -kb"
    }
    if { $get_all_dirs == "Yes" } {
      append commandline " -d $dir"
    }
    if { $tagname != "BASE" && $tagname != "HEAD" } {
      if { $action_if_no_tag == "Remove" } {
	  append commandline " -r $tagname"
      } elseif { $action_if_no_tag == "Get_head" } {
	  append commandline " -f -r $tagname"
      } elseif { $action_if_no_tag == "Skip" } {
	  append commandline " -s -r $tagname"
      }
    }
    if { $tagname == "HEAD" } {
      append commandline " -A"
    }
    append commandline " $str"

    gen_log:log C $commandline
    exec_command "CVS Update" $commandline
    if {$cvscfg(auto_status) == "true"} {
      setup_dir
    }
  }
  gen_log:log T "LEAVE"
}

proc cvs_join {localfile branchver} {
#
# This does a join (merge) of the branchver revision of localfile to the
# head revision.
#
  global cvs
  global cvscfg
  global incvs

  gen_log:log T "ENTER ($localfile $branchver)"
  if {! $incvs} {
    cvs_notincvs
    return 1
  }

  set mess "This will merge revision $branchver to"
  append mess " the head revision of $localfile"
  append mess "\n\nAre you sure?"
  if {[cvsconfirm $mess] == 0} {
    set commandline "$cvs -d $cvscfg(cvsroot) update -j$branchver $localfile"
    gen_log:log C "$commandline"
    exec_command "CVS Merge" "$commandline"
    if {$cvscfg(auto_status) == "true"} {
      setup_dir
    }
  }
  gen_log:log T "LEAVE"
}

proc cvs_delta {localfile ver1 ver2} {
#
# This merges the changes between ver1 and ver2 into the head revision.
#
  global cvs
  global cvscfg
  global incvs

  gen_log:log T "ENTER ($localfile $ver1 $ver2)"
  if {! $incvs} {
    cvs_notincvs
    return 1
  }

  if {$ver1 == {} || $ver2 == {}} {
    cvsfail "Must have two revision numbers for this function!"
    return 1
  }
  set mess "This will merge the changes between revision $ver1 and $ver2"
  append mess " (if $ver1 > $ver2 the changes are removed)"
  append mess " to the head revision of $localfile"
  append mess "\n\nAre you sure?"
  if {[cvsconfirm $mess] == 0} {
    set commandline "$cvs -d $cvscfg(cvsroot) update -j$ver1 -j$ver2 $localfile"
    gen_log:log C "$commandline"
    exec_command "CVS Merge" "$commandline"
    if {$cvscfg(auto_status) == "true"} {
      setup_dir
    }
  }
}

proc cvs_status {args} {
#
# This does a status report on the files in the current directory.
#
  global cvs
  global incvs
  global cvscfg

  gen_log:log T "ENTER ($args)"
  set cmd_options ""

  # if there are selected files, I want verbose output for those files
  # so I'm going to save the current setting here
  # - added by Jo
  set verbosity_setting ""

  if {! $incvs} {
    cvs_notincvs
    return 1
  }

  set filelist [join $args]
  gen_log:log D "filelist: \"$filelist\""
  gen_log:log D "cvscfg(thisdir) \"$cvscfg(thisdir)\""
  # if recurse option is false or there are selected files, don't recurse
  if {($cvscfg(recurse) == "false") || ($filelist != $cvscfg(thisdir))} { 
    set cmd_options "$cmd_options -l"
  }

  # if there are selected files, use verbose output
  # but save the current setting so it can be reset
  # - added by Jo
  if {$filelist != $cvscfg(thisdir)} {
    set verbosity_setting $cvscfg(rdetail)
    set cvscfg(rdetail) "verbose"
  }

  # support verious levels of verboseness. Ideas derived from GIC
  set commandline "$cvs -d $cvscfg(cvsroot) -Q status $cmd_options $filelist"
  gen_log:log C "$commandline"
  catch {eval "exec $commandline"} raw_status

  if {$cvscfg(rdetail) == "verbose"} {
    view_output "CVS Status ($cvscfg(rdetail))" $raw_status
  } else {
    set cooked_status ""
    set stat_lines [split $raw_status "\n"]
    foreach statline $stat_lines {
      if {[string match "*Status:*" $statline]} {
	# This nastiness is all because versions of tcl < 8.1 dont have
	# advanced regular expressions.
	set line [split $statline "\t "]
	set line [join $line]
	regsub -all {  *} $line " " line
	gen_log:log D "$line"
	if {$cvscfg(rdetail) == "terse" && [lindex $line 3] == "Up-to-date"} {
	  continue
	} else {
	  append cooked_status [lrange $line 3 end]
	  append cooked_status "\t"
	  append cooked_status [lindex $line 1]
	  append cooked_status "\n"
	}
      }
    }
    view_output "CVS Status ($cvscfg(rdetail))" $cooked_status
  }

  # reset the verbosity setting if necessary -jo
  if { $verbosity_setting != "" } {
    set cvscfg(rdetail) $verbosity_setting
  }
  gen_log:log T "LEAVE"
}


proc cvs_tag_status {args} {
#
# This grep through the output of 'cvs status' to provide a simplistic
# report of the current tags on files
#
  global cvs
  global incvs
  global cvscfg

  gen_log:log T "ENTER ($args)"
  if {! $incvs} {
    cvs_notincvs
    return 1
  }

  set filelist [join $args]

  set commandline "$cvs -d $cvscfg(cvsroot) -Q status -l $filelist"
  gen_log:log C "$commandline"
  catch {eval "exec $commandline"} raw_status

  set cooked_status ""
  set stat_lines [split $raw_status "\n"]
  foreach statline $stat_lines {
    if {[string match "*Status:*" $statline]} {
      # This nastiness is all because versions of tcl < 8.1 dont have
      # advanced regular expressions.
      set line [split $statline "\t "]
      set line [join $line]
      regsub -all {  *} $line " " line
      gen_log:log D "$line"
      append cooked_status [lindex $line 1]
      append cooked_status "\t"
      append cooked_status [lrange $line 3 end]
      append cooked_status "\n"
    }
    if {[string match "*Sticky Tag:*" $statline]} {
      regsub -all {  *} $statline { } statline
      set line [split $statline]
      gen_log:log D "$line"
      append cooked_status "   [lrange $line 4 end]"
      append cooked_status "\n"
    }

  }
  view_output "CVS Sticky Status" $cooked_status

  gen_log:log T "LEAVE"
}

proc format_check_msg {file msg} {
  return [format "%-40s: %s" $file $msg]
}

proc cvs_check_filter_proc {line} {
#
# This filter annotates each line of cvs_check output
#
  global cvscfg
  gen_log:log T "ENTER $line"

  regexp {^([UARMC?]) (.*)} $line junk mode file
  if {[info exists mode]} {
    switch -exact -- $mode {
      U {
	set new_line [format_check_msg $file \
		  "file changed in repository, needs updating"]
      }
      A {
	set new_line [format_check_msg $file \
		  "file added, not committed"]
      }
      R {
	set new_line [format_check_msg $file \
		  "file removed, not committed"]
      }
      M {
	set new_line [format_check_msg $file \
		  "file modified, not committed"]
      }
      C {
	set new_line [format_check_msg $file \
		  "file modified and in conflict, not committed"]
      }
      ? {
	# samba changes the case of the cvs file in different ways
	gen_log:log D "file: $file"
	if {! [regexp -nocase [file tail $file] {^CVS$}]} {
	  set new_line [format_check_msg $file "file unknown, not in CVS"]
	}
      }
      default {
	set new_line $line
      }
    }
  } else {
    set new_line $line
  }
  gen_log:log T "LEAVE ($new_line)"
  return $new_line
}

proc cvs_check_eof_proc {} {
#
# This proc is called after cvs check is done, code is from cvscheck.tcl
#
# now find directories not added.  This is accomplished by finding all of
# the directories in the current directory seeing if there is a CVS
# control file in each one.
#
  global cvscfg

  gen_log:log T "ENTER"
  set dir_lines ""
  set files [glob -nocomplain -- .??* *]
  set dirs {}
  gen_log:log D "files: ($files)"
  foreach file $files {
    gen_log:log D "file: \"$file\""
    # samba changes the case of the cvs file in different ways
    if {[file isdirectory $file] && ! [regexp -nocase $file {^CVS$}]} {
      lappend dirs $file
    }
  }
  gen_log:log D "dirs: $dirs"
  # see if there are any directories not added.
  if {[llength $dirs]} {
    foreach dir $dirs {
      if {! [file exists [file join $dir "CVS"]] \
      || ! [file isdirectory [file join $dir "CVS"]]} {
	append dir_lines \
	  [format_check_msg $dir "directory unknown, not in CVS\n"]
      }
    }
  }
  gen_log:log T "LEAVE"
  return $dir_lines
}

proc cvs_check {} {
#
# This does a cvscheck on the files in the current directory.
#
  global cvs
  global incvs
  global cvscfg

  gen_log:log T "ENTER"
  if {! $incvs} {
    cvs_notincvs
    return 1
  }

  set cvscfg(exec_line_filter) "cvs_check_filter_proc"
  set cvscfg(exec_eof) "cvs_check_eof_proc"
  set commandline "$cvs -n -q update $cvscfg(checkrecursive)"
  gen_log:log C "$commandline"
  exec_command "CVS Check" "$commandline" 0 "Nothing to report."
  set cvscfg(exec_line_filter) ""
  set cvscfg(exec_eof) ""
  gen_log:log T "LEAVE"
}

proc cvs_checkout {mcode revision} {
  #
  # This checks out a new module into the current directory.
  #
  global cvs
  global cvscfg
  global incvs
  global feedback

  gen_log:log T "ENTER ($mcode $revision)"
  if {$incvs} {
    set mess "You are already in a CVS controlled directory.  Are you"
    append mess " sure that you want to check out another module in"
    append mess " to this directory?"
    if {[cvsconfirm $mess] == 1} {
      return 1
    }
  }

  set mess "This will check out $mcode from CVS.\nAre you sure?"
  if {[cvsconfirm $mess] == 0} {
    feedback_cvs $feedback(cvs) "Checking out module $mcode, please wait"
    if {$revision == {} || $revision == "HEAD"} {
      set commandline "$cvs -d $cvscfg(cvsroot) $cvscfg(co_file_mode) checkout -P $mcode"
      gen_log:log C "$commandline"
      exec_command "CVS Checkout" "$commandline"
    } else {
      set commandline \
	  "$cvs -d $cvscfg(cvsroot) $cvscfg(co_file_mode) checkout -P -r $revision $mcode"
      gen_log:log C "$commandline"
      exec_command "CVS Checkout" "$commandline"
    }
    feedback_cvs $feedback(cvs) ""
    if {$cvscfg(auto_status) == "true"} {
      setup_dir
    }
  }
  gen_log:log T "LEAVE"
}

proc cvs_filelog {mcode filename} {
#
# This looks at the revision log of a file.  It's called from filebrowse.tcl, 
# so we can't do operations such as merges.
#
  global cvs
  global location
  global cvscfg
  global cwd
  
  gen_log:log T "ENTER ($mcode $filename)"
  set pid [pid]
  set file $location($mcode)/$filename
  set filetail [file tail $filename]
  
  set commandline "$cvs -d $cvscfg(cvsroot) -l checkout $mcode/$filename"
  gen_log:log C "$commandline"
  set ret [cvs_sandbox_runcmd "$commandline" cmd_output]
  if {$ret == $cwd} {
    cvsfail $cmd_output
    cd $cwd
    gen_log:log T "LEAVE -- cvs checkout failed"
    return
  }

  set commandline "$cvs -d $cvscfg(cvsroot) -n log -l $mcode/$filename"
  gen_log:log C "$commandline"
  set ret [catch {eval "exec $commandline"} view_this]
  if {$ret} {
    cvsfail $view_this
    gen_log:log T "LEAVE ERROR ($view_this)"
    cd $cwd
    return
  }
  cd $cwd

  # Log canvas viewer
  new_logcanvas "no file" $view_this
  gen_log:log T "LEAVE"
}

proc cvs_fileview {mcode filename revision} {
#
# This looks at a revision of a file from the repository.
# Called from Module Browser -> File Browse -> View
#
  global cvs
  global cvscfg

  gen_log:log T "ENTER ($mcode $filename $revision)"
  if {$revision == {}} {
    set commandline "$cvs -d $cvscfg(cvsroot) checkout -p $mcode/$filename 2>$cvscfg(null)"
    gen_log:log C "$commandline"
    catch {eval "exec $commandline"} view_this
  } else {
    set commandline "$cvs -d $cvscfg(cvsroot) checkout -p -r $revision $mcode/$filename 2>$cvscfg(null)"
    gen_log:log C "$commandline"
    catch {eval "exec $commandline"} view_this
  }
  view_output "CVS File View: $mcode/$filename" $view_this
}

proc rcs_filediff {filename ver1 ver2} {
#
# This does a diff of an RCS file within the repository.  It can be done
# with a remote repository.
#
  global cvscfg

  if {$ver1 == {} || $ver2 == {}} {
    cvsfail "Must have two revision numbers for this function!"
    return 1
  }
  # view_output "CVS File Diff" $view_this
  gen_log:log C "$cvscfg(tkdiff) -r$ver1 -r$ver2 $filename &"
  catch {eval "exec $cvscfg(tkdiff) -r$ver1 -r$ver2 $filename &"} view_this
}


proc cvs_filediff {mcode filename ver1 ver2} {
#
# This looks at a diff of a file from the repository without
# checking it out.
#
  global cvscfg
  global location

  rcs_filediff $cvscfg(cvsroot)/$location($mcode)/$filename $ver1 $ver2
}

proc cvs_export {mcode revision} {
#
# This exports a new module (see man cvs and read about export) into
# the current directory.
#
  global cvs
  global incvs
  global cvscfg

  gen_log:log T "ENTER ($mcode $revision)"

  if {$incvs} {
    set mess "You are already in a CVS controlled directory.  Are you"
    append mess " sure that you want to export a module in"
    append mess " to this directory?"
    if {[cvsconfirm $mess] == 1} {
      return 1
    }
  }

  if {$revision == {}} {
    cvsfail "You must enter a tag name for this function."
    return
  } elseif {$mcode == {}} {
    cvsfail "You must select a module to export."
    return
  }
  set mess "This will export $mcode from CVS.\nAre you sure?"
  if {[cvsconfirm $mess] == 0} {
    set commandline "$cvs -d $cvscfg(cvsroot) export -r $revision $mcode"
    gen_log:log C "$commandline"
    catch {eval "exec $commandline"} view_this
    view_output "CVS Export" $view_this
    if {$cvscfg(auto_status) == "true"} {
      setup_dir
    }
  }
  gen_log:log T "LEAVE"
}

proc cvs_patch {mcode rev1 rev2} {
#
# This creates a patch file between two revisions of a module.  If the
# second revision is null, it creates a patch to the head revision.
#
  global cvs
  global cvscfg
 
  gen_log:log T "ENTER ($mcode $rev1 $rev2)"
  if {$mcode == ""} {
    cvsfail "Please select a module!"
    return
  }
  if {$rev1 == {}} {
    cvsfail "You must enter a tag name for this function."
    return
  }
 
  set mess "This will make a patch file for $mcode from CVS.\nAre you sure?"
  if {[cvsconfirm $mess] == 0} {
    if {$rev2 == {}} {
      set commandline "$cvs -d $cvscfg(cvsroot) patch -r $rev1 $mcode >$mcode.pat"
    } else {
      set commandline "$cvs -d $cvscfg(cvsroot) patch -r $rev1 -r $rev2 $mcode >$mcode.pat"
    }
    gen_log:log C "$commandline"
    # Can't use exec_command because we redirected stdout
    set ret [catch {eval "exec $commandline"} view_this]
    if {$ret} {
       append view_this "Patch file is $mcode.pat"
    } else {
       cvsfail $view_this
    }
    view_output "CVS Patch" $view_this

    if {$cvscfg(auto_status) == "true"} {
      setup_dir
    }
  }
  gen_log:log T "LEAVE"
}

proc cvs_patch_summary {mcode rev1 rev2} {
#
# This creates a patch summary of a module between 2 revisions.
#
  global cvs
  global cvscfg
 
  gen_log:log T "ENTER ($mcode $rev1 $rev2)"
  if {$mcode == ""} {
    cvsfail "Please select a module!"
    return
  }
  if {$rev1 == {}} {
    cvsfail "You must enter a tag name for this function."
    return
  }
 
  if {$rev2 == {}} {
    set commandline "$cvs -d $cvscfg(cvsroot) patch -s -r $rev1 $mcode"
  } else {
    set commandline "$cvs -d $cvscfg(cvsroot) patch -s -r $rev1 -r $rev2 $mcode"
  }
  gen_log:log C "$commandline"
  exec_command "CVS Patch Summary" "$commandline"

  gen_log:log T "LEAVE"
}

proc cvs_version {} {
#
# This shows the current CVS version number.
#
  global cvs
  global cvscfg

  gen_log:log C "$cvs -d $cvscfg(cvsroot) -v"
  exec_command "CVS version" "$cvs -d $cvscfg(cvsroot) -v"
}

proc cvs_rtag {mcode branch tagnameA tagnameB} {
#
# This tags a module in the repository.
#
  global cvs
  global cvscfg

  set command "rtag"

  set cmd_options ""

  if {$branch == "yes"} {
    set cmd_options "-b"
  }

  set cmd_options "$cmd_options -F"

  if {$tagnameA == ""} {
    cvsfail "You must enter a tag name!"
    return 1
  }

  set mess "This will tag module \"$mcode\" in CVS with tag \"$tagnameA\"."
  if {$tagnameB == ""} {
    append mess "\n\nThe head revision of all files will be tagged."
  } else {
    append mess "\n\nThe revisions tagged with \"$tagnameB\" will be tagged."
    set cmd_options "$cmd_options -r $tagnameB"
  }
  append mess "\n\nAre you sure?"
  if {[cvsconfirm $mess] == 0} {
    exec_command "CVS Rtag" \
	"$cvs -d $cvscfg(cvsroot) $command $cmd_options $tagnameA $mcode"
  }
}

proc cvs_usercmd {args} {
#
# Allows the user to run a user-specified cvs command.
#
  global cvs

  gen_log:log C "$cvs $args"
  exec_command "CVS $args" "$cvs $args"
}

proc cvs_anycmd {args} {
#
# Allows the user to run any user-specified command.
#
  # exec_command wont work because the command may expect to open its own window
  gen_log:log C "$args"
  catch {eval "exec $args"} view_this
  view_output [lindex $args 0] $view_this
}

# RAMSAN
proc cvs_anycmd_tcl {args} {
#
# Allows the user to run any user-specified command.
#
  # exec_command wont work because the command may expect to open its own window
  gen_log:log C "$args"
  catch {eval $args} view_this
  view_output [lindex $args 0] $view_this
}

#
# Merge conflicts with tkdiff
# 
proc cvs_merge_conflict {args} {
  global cvscfg
  global cvs

  gen_log:log T "ENTER ($args)"

  set filelist [join $args]
  if {$filelist == $cvscfg(thisdir)} {
    cvsfail "Please select some files to merge first!"
    return
  }

  foreach file $filelist {
    set commandline "$cvs -d $cvscfg(cvsroot) -n -q update $file"
    gen_log:log C "$commandline"
    catch {eval "exec $commandline"} status
    # Make sure its really a conflict - tkdiff will bomb otherwise
    gen_log:log C "$status"
    if {[string match "C *" $status]} {
      set commandline "$cvs -d $cvscfg(cvsroot) update $file"
      gen_log:log C "$commandline"
      catch {eval "exec $commandline"}
    } else {
      cvsfail "This file does not appear to have a conflict."
      return
    }
    # Invoke tkdiff with the proper option for a conflict file
    # and have it write to the original file
    set commandline "$cvscfg(tkdiff) -conflict -o $file $file"
    gen_log:log C "$commandline"
    catch {eval "exec $commandline"} view_this
    # Update the file status
    set commandline "$cvs -d $cvscfg(cvsroot) update $file"
    gen_log:log C "$commandline"
    catch {eval "exec $commandline"}
  }  
  
  if {$cvscfg(auto_status) == "true"} {
    setup_dir
  }
  gen_log:log T "LEAVE"
}


#
# Set up a dialog containing a text box to view
# the report of the command during execution.
#
proc exec_command_init {title} {
  global exec_win

  static {exec_viewer 0}
  set my_idx $exec_viewer
  incr exec_viewer

  set exec_win($my_idx,lines) 0
  set exec_win($my_idx,win) ".exec$my_idx"
  set exec_win($my_idx,text) ".exec$my_idx.text"
  set exec_win($my_idx,scroll) ".exec$my_idx.scroll"
  set exec_win($my_idx,ok) ".exec$my_idx.ok"
  set exec_win($my_idx,destroyed) 0

  toplevel $exec_win($my_idx,win)
  text $exec_win($my_idx,text) -setgrid yes -relief sunken -border 2 \
	-yscroll "$exec_win($my_idx,scroll) set"
  scrollbar $exec_win($my_idx,scroll) -relief sunken \
	-command "$exec_win($my_idx,text) yview"
  button $exec_win($my_idx,ok) -text "Stop" \
      -command "stop_command 0 $my_idx" -state disabled
  pack $exec_win($my_idx,ok) -side bottom -fill x
  pack $exec_win($my_idx,scroll) -side right -fill y
  pack $exec_win($my_idx,text) -fill both -expand 1
  wm title $exec_win($my_idx,win) "$title"

  # RAMSAN
  bind $exec_win($my_idx,text) <1> "focus $exec_win($my_idx,text)"
  wm geom $exec_win($my_idx,win) +50+50
  focus $exec_win($my_idx,ok)

  return $my_idx
}

proc exec_command_body { my_idx command } {
  global exec_win
  global cvscfg

  if {$command == ""} {
    cvsfail "Nothing to execute."
  } else {
    if {[catch {open "| $command |& cat"} exec_win($my_idx,log)]} {
      $exec_win($my_idx,text) insert end $exec_win($my_idx,log)\n
      exec_command_end $my_idx
    } else {                    
      set exec_win($my_idx,run) 1
      fileevent $exec_win($my_idx,log) readable "ins_exec_log_line $my_idx"
      if { $cvscfg(allow_abort) == "yes" } {
	$exec_win($my_idx,ok) configure -state normal
      } else {
	$exec_win($my_idx,ok) configure -state normal \
	    -text "Please wait..." -command ""
      }
      tkwait variable exec_win($my_idx,run)
    }
  }
  update idletasks
}

proc exec_command_end { my_idx } {
  global exec_win

  if { $exec_win($my_idx,destroyed) == 0 } {
	$exec_win($my_idx,ok) configure -text "Please wait..." -command ""
    update idletasks
    $exec_win($my_idx,ok) configure -text "Ok" -command "stop_command 1 $my_idx"
  }
}

proc exec_command {title command args} {
  global exec_win

  set my_idx [exec_command_init $title]
  set exec_win($my_idx,lines) 0
  exec_command_body $my_idx $command
  if {[llength $args] == 2} {
     set arg1 [lindex $args 0]
     set arg2 [lindex $args 1]
     if {$arg1 == 0} {
       # append if text empty
       if {$exec_win($my_idx,lines) == 0} {
	 $exec_win($my_idx,text) insert end "$arg2\n"
       }
     } else {
       if {$arg1 == 1} {
	# always append
	$exec_win($my_idx,text) insert end "$arg2\n"
      }
    }
  }
  exec_command_end $my_idx
}

proc ins_exec_log_line { my_idx } {
  global exec_win cvscfg
    
  #puts stderr "ins_exec_log_line ($my_idx)"
  if {[eof $exec_win($my_idx,log)]} {
    catch {close $exec_win($my_idx,log)}
    # do some thing at end of program
    if { [info exists cvscfg(exec_eof)] } {
      if { $cvscfg(exec_eof) != "" } {
	set line [$cvscfg(exec_eof)]
	if { $line!="" } {
	  $exec_win($my_idx,text) insert end $line
	  $exec_win($my_idx,text) see end
	  incr exec_win($my_idx,lines)
	}
      }
    }
    set exec_win($my_idx,run) 0
    #puts stderr "end of pipe ($my_idx)!"
  } else {
    gets $exec_win($my_idx,log) line
    $exec_win($my_idx,text) insert end $line\n
    $exec_win($my_idx,text) see end
    if {$line!=""} {
      incr exec_win($my_idx,lines)
    }
  }
  update idletasks
}

proc stop_command {force my_idx } {
  global exec_win

  set mess "Really quit command?"
  if { $force == 1 || [tk_dialog .message {Confirm!} $mess warning 0 NO YES]} {
    catch {close $exec_win($my_idx,log)}
    if { $exec_win($my_idx,destroyed) == 0 } {
      set exec_win($my_idx,destroyed) 1
      destroy $exec_win($my_idx,win)
    }
  }
  update idletasks
}

proc view_output {title output_string} {
#
# Set up a dialog containing a text box that can be used to view
# the report on the screen.
#
  static {viewer 0}

  # If nothing to report, then say so.
  if {$output_string == ""} {
    set mess "Nothing to report."
    cvsok $mess
  } else {
    incr viewer
    set cvsview ".cvsview$viewer"
    toplevel $cvsview
    text $cvsview.text -setgrid yes -relief sunken -border 2 \
      -yscroll "$cvsview.scroll set"
    scrollbar $cvsview.scroll -relief sunken \
      -command "$cvsview.text yview"
    button $cvsview.ok -text "OK" \
      -command "destroy $cvsview"

    pack $cvsview.ok -side bottom -fill x
    pack $cvsview.scroll -side right -fill y
    pack $cvsview.text -fill both -expand 1

    wm title $cvsview "$title Output"
    $cvsview.text insert end $output_string
    $cvsview.text configure -state disabled

    # RAMSAN
    bind $cvsview.text <1> "focus $cvsview.text"
    wm geom $cvsview +50+50
    focus $cvsview.ok
  }
}

proc cvs_gettaglist {mcode filename} {
  global cvs
  global location
  global cvscfg
  global cwd

  set keepers ""
  set pid [pid]
  gen_log:log T "ENTER ($mcode $filename)"
  set file $location($mcode)/$filename
  set filetail [file tail $filename]
  
  set commandline "$cvs -d $cvscfg(cvsroot) -l checkout $mcode/$filename" 
  # run a command, possibly creating the sandbox to play in
  set ret [cvs_sandbox_runcmd $commandline cmd_output]
  if {$cwd == $ret} {
    cvsfail $cmd_output
    gen_log:log T "LEAVE ERROR ($cmd_output)"
    return $keepers
  }

  cd $mcode
  gen_log:log F "[pwd]"
  set commandline "$cvs -d $cvscfg(cvsroot) -l -n log -l $filename"
  gen_log:log C "$commandline"
  set ret [catch {eval "exec $commandline"} view_this]
  if {$ret} {
    cvsfail $view_this
    cd $cwd
    gen_log:log T "LEAVE ERROR"
    return $keepers
  }
  set view_lines [split $view_this "\n"]
  foreach line $view_lines {
    if {[string index $line 0] == "\t" } {
      append keepers "$line\n"
    }
  }
  cd $cwd

  gen_log:log T "LEAVE"
  return "$keepers"
}
