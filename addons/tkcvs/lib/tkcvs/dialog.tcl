#
# Tcl Library for TkCVS
#
 
# 
# $Id: dialog.tcl,v 1.1.1.1 2001-06-07 15:03:42 ramsan Exp $
#
# Smallish dialogs - add, tag
#

proc add_dialog {args} {
  global cvs
  global incvs
  global cvscfg

  gen_log:log T "ENTER ($args)"
  if {! $incvs} {
    cvs_notincvs
    return 1
  }

  set binflag ""
  toplevel .add

  set filelist [join $args]
  if {$filelist == $cvscfg(thisdir)} {
    set mess "This will add all new files"
  } else {
    set mess "This will add these files:\n\n"
    foreach file $filelist {
      append mess "   $file\n"
    }  
  }

  message .add.top -justify left -aspect 300 -relief groove \
    -text "Add a file or files to the module.  The repository\
           will not be changed until you do a commit.
\ "
  pack .add.top -side top -fill x

  message .add.middle -text $mess
  pack .add.middle -side top -fill x

  checkbutton .add.binary -text "-kb (binary)" \
     -variable binflag -onvalue "-kb" -offvalue ""
  pack .add.binary -side top

  frame .add.down
  button .add.down.add -text "Add" \
    -command {
      cvs_add $binflag [workdir_list_files]
      destroy .add
    }
  button .add.down.cancel -text "Cancel" -command { destroy .add }
  pack .add.down -side bottom -fill x -expand 1
  pack .add.down.add .add.down.cancel -side left \
    -ipadx 2 -ipady 2 -padx 4 -pady 4 -fill both -expand 1

  wm title .add "Add Files"
  wm minsize .add 1 1

  gen_log:log T "LEAVE"
}

proc tag_dialog {} {
  global incvs
  global cvscfg

  gen_log:log T "ENTER"

  if {! $incvs} {
    cvs_notincvs
    return 1
  }

  toplevel .tag

  message .tag.msg -justify left -aspect 300 -relief groove \
    -text "Apply a new tag or branch tag\
           to the marked files, recursively.\
           Will apply tag to repository and,\
           if a branch, also update local directory.\
           With the -F option, will move the tag\
           if it already exists.
\ "
  pack .tag.msg -side top -fill x -pady 1

  checkbutton .tag.force -text "-F (force)" \
     -variable forceflag -onvalue "-F" -offvalue ""
  pack .tag.force -side top -fill x -expand 1

  frame .tag.middle
  pack .tag.middle -side top -fill x

  label .tag.middle.lbl -text "Tag Name" -anchor w
  entry .tag.middle.entry -relief sunken -textvariable usertagname
  pack .tag.middle.lbl -side left -fill x -pady 3
  pack .tag.middle.entry -side right -fill x -expand 1 -pady 3 -padx 3

  frame .tag.down
  pack .tag.down -side bottom -fill x -expand 1
  button .tag.tag -text "Tag" \
    -command {
      cvs_tag $usertagname $forceflag no [workdir_list_files]
      destroy .tag
    }
  button .tag.branchtag -text "Branch Tag (-b)" \
    -command {
      cvs_tag $usertagname $forceflag yes [workdir_list_files]
      destroy .tag
    }
  button .tag.cancel -text "Cancel" -command { destroy .tag }
 
  pack .tag.tag .tag.branchtag .tag.cancel -in .tag.down -side left \
    -ipadx 2 -ipady 2 -padx 4 -pady 4 -fill both -expand 1
 
  wm title .tag "Tag a Module"
  wm minsize .tag 1 1
  gen_log:log T "LEAVE"
}

proc subtract_dialog {args} {
  global cvs
  global incvs
  global cvscfg

  gen_log:log T "ENTER ($args)"
  if {! $incvs} {
    cvs_notincvs
    return 1
  }
  toplevel .subtract

  set filelist [join $args]
  if {$filelist == $cvscfg(thisdir)} {
    cvsfail "Please select some files to delete first!"
    return
  }

  set mess "This will remove these files:\n\n"
  foreach file $filelist {
    append mess "   $file\n"
  }  

  message .subtract.top -justify left -aspect 300 -relief groove \
    -text "Remove a file or files from the module.  The repository\
           will not be changed until you do a commit.
\ "
  pack .subtract.top -side top -fill x

  message .subtract.middle -text $mess
  pack .subtract.middle -side top -fill x
  frame .subtract.down
  button .subtract.down.remove -text "Remove" \
    -command {
      cvs_remove [workdir_list_files]
      destroy .subtract
    }
  button .subtract.down.cancel -text "Cancel" -command { destroy .subtract }
  pack .subtract.down -side bottom -fill x -expand 1
  pack .subtract.down.remove .subtract.down.cancel -side left \
    -ipadx 2 -ipady 2 -padx 4 -pady 4 -fill both -expand 1

  wm title .subtract "Remove Files"
  wm minsize .subtract 1 1

  gen_log:log T "LEAVE"
}

#
# Set up a small(?) update dialog.
#
proc update_setup {} {
  global cvscfg

  gen_log:log T "ENTER"
  toplevel .update
  frame .update.explaintop
  frame .update.tagmode -relief groove -border 4
  frame .update.normbin -relief groove -border 2
  frame .update.alldirs -relief groove -border 2
  frame .update.down

  frame .update.tagmode.keep -relief groove -border 4

  frame .update.tagmode.trunk -relief groove -border 4

  frame .update.tagmode.join -relief groove -border 4
  frame .update.joinleft
  frame .update.joinright
  frame .update.joinentry

  frame .update.tagmode.getrev -relief groove -border 4
  frame .update.getrevleft
  frame .update.getrevright
  frame .update.getreventry

  frame .update.getdirsleft
  frame .update.getdirsright
  frame .update.getdirsentry

  pack .update.down -side bottom -fill x
  pack .update.normbin -side bottom -fill x
  pack .update.alldirs -side bottom -fill x
  pack .update.explaintop -side top -fill x -pady 1
  pack .update.tagmode -side top -fill x -pady 1


  # Provide an explanation of this dialog box
  label .update.explain1 -text "Update files in local directory."
  label .update.explain2 -text "(always recursive)"
  label .update.explain3 -text "'Reset defaults' button will show defaults."
  label .update.explain4 -text "Empty directories always pruned (-P)"

  pack .update.explain1 .update.explain2 .update.explain3 .update.explain4 \
    -in .update.explaintop -side top -fill x

  # Where user chooses the type of update being performed
  label .update.explainmode -text "Update mode:" -anchor w

  pack .update.explainmode -in .update.tagmode -side top -fill x

  pack .update.tagmode.keep -in .update.tagmode -side top -fill x
  pack .update.tagmode.trunk -in .update.tagmode -side top -fill x
  pack .update.tagmode.join -in .update.tagmode -side top -fill x
  pack .update.tagmode.getrev -in .update.tagmode -side bottom -fill x

  # If the user wants to simply do a normal update
  radiobutton .update.tagmode.keep.select -text "Keep same branch or trunk." \
    -variable tagmode_selection -value "Keep" -anchor w

  label .update.tagmode.keep.explain1 \
    -text "If local directory is on main trunk, get latest on main trunk."
  label .update.tagmode.keep.explain2 \
    -text "If local directory is on a branch, get latest on that branch."
  label .update.tagmode.keep.explain3 \
    -text "If local directory/file has non-branch tag, no update."

  pack .update.tagmode.keep.select -in .update.tagmode.keep \
    -side top -fill x -pady 3
  pack .update.tagmode.keep.explain1 .update.tagmode.keep.explain2 \
    .update.tagmode.keep.explain3 \
    -in .update.tagmode.keep -side top -fill x -pady 1 -ipady 0

  # If the user wants to update to the head revision
  radiobutton .update.tagmode.trunk.select \
    -text "Update local files to be on main trunk (head) (-A)" \
    -variable tagmode_selection -value "Trunk" -anchor w

  label .update.tagmode.trunk.explain1 \
    -text "Advice:  If your local directories are currently on a branch,"

  label .update.tagmode.trunk.explain2 \
    -text "you may want to commit any local changes to that branch first."

  pack .update.tagmode.trunk.select \
    -in .update.tagmode.trunk -side top -fill x -pady 3
  pack .update.tagmode.trunk.explain1 .update.tagmode.trunk.explain2 \
    -in .update.tagmode.trunk -side top -fill x -pady 1 -ipady 0


  # If the user wants to join a branch to the main trunk (head)

  radiobutton .update.tagmode.join.select  \
    -text "Join (-j) local files to include changes from branch:" \
    -variable tagmode_selection -value "Join" -anchor w

  label .update.tagmode.join.explain \
    -text "(To be written.)"

  # Note:  May be useful also to add the capability for something like:
  # cvs -n update -p -r <tagname> filename > newfilename
  # for cases where diff3 bombs (too many differences)
  # or where the change is too dramatic.
  # If this is added, then perhaps also the ability to retrieve the latest
  # revision from the branch, then merge any changes between the
  # nearest common ancestor and the HEAD revision into _that_.

  label .update.tagmode.join.explain1 \
    -text "Advice:  Update local files to main trunk (head) first."

  pack .update.tagmode.join.select -in .update.tagmode.join \
    -side top -fill x -pady 3
  pack .update.tagmode.join.explain -in .update.tagmode.join \
    -side top -fill x -pady 1
  pack .update.tagmode.join.explain1 -in .update.tagmode.join \
    -side top -fill x -pady 1


  # If the user wants to update local files to a branch/tag

  # Where user enters a tag name (optional)
  radiobutton .update.tagmode.getrev.select \
    -text "Update (-r) local files to be on tag/branch:" \
    -variable tagmode_selection -value "Getrev" -anchor w

  label .update.tagmode.getrev.explain \
    -text "Advice:  Update local files to main trunk (head) first."

  label .update.tagmode.getrev.sticky \
    -text "Note:  This tag will be 'sticky' for each file & the directory."

  label .update.lname -text "Tag Name" -anchor w

  entry .update.tname -relief sunken -textvariable updatename

  # bind_motifentry .update.tname

  pack .update.lname -in .update.getrevleft \
    -side top -fill x -pady 3
 
  pack .update.tname -in .update.getrevright \
    -side top -fill x -pady 3

  # Where user chooses the action to take if tag is not on a file
  label .update.lnotfound -text "If tag not found for file,"

  radiobutton .update.notfoundremove -text "Remove file from local directory" \
    -variable action_notag -value "Remove"

  # radiobutton .update.notfoundhead -text "Get head revision"

  radiobutton .update.notfoundhead -text "'Get head' (-f) option is being debugged" \
    -variable action_notag -value "Get_head"

  radiobutton .update.notfoundskip -text "Skip the file (-s)" \
    -variable action_notag -value "Skip"

  pack .update.tagmode.getrev.select -in .update.tagmode.getrev \
    -side top -fill x -pady 3
  pack .update.tagmode.getrev.explain -in .update.tagmode.getrev \
    -side top -fill x -pady 3
  pack .update.tagmode.getrev.sticky -in .update.tagmode.getrev \
    -side top -fill x -pady 0
  pack .update.getreventry -in .update.tagmode.getrev -side top -fill x -pady 3
  pack .update.lnotfound -in .update.tagmode.getrev -side top -fill x -pady 3
  pack .update.notfoundskip .update.notfoundhead .update.notfoundremove \
    -in .update.tagmode.getrev -side bottom \
    -ipadx 2 -ipady 2 -padx 4 -pady 4 

  pack .update.getrevleft -in .update.getreventry \
     -side left -fill y
  pack .update.getrevright -in .update.getreventry \
     -side left -fill both -expand 1


  # Where user chooses whether to pick up directories not currently in local
  label .update.lalldirs \
    -text "If directory is in repository but not in local:" -anchor w

  radiobutton .update.noalldirs -text "Ignore it" \
    -variable get_all_dirs -value "No" -anchor w
  radiobutton .update.getalldirs -text "Get it (-d)" \
    -variable get_all_dirs -value "Yes" -anchor w

  label .update.lgetdirname -text "Specific directory (optional)" -anchor w
  entry .update.tgetdirname -relief sunken -textvariable getdirname

  pack .update.lgetdirname -in .update.getdirsleft \
    -side top -fill x -pady 3
  pack .update.tgetdirname -in .update.getdirsright \
    -side top -fill x -pady 3

  pack .update.getdirsleft -in .update.getdirsentry \
    -side left -fill y
  pack .update.getdirsright -in .update.getdirsentry \
    -side left -fill both -expand 1

  pack .update.lalldirs -in .update.alldirs \
    -side top -fill x -pady 3
  pack .update.getdirsentry -in .update.alldirs \
    -side bottom -fill x -pady 3
  pack .update.noalldirs .update.getalldirs -in .update.alldirs \
    -side left -fill both -ipadx 2 -ipady 2 -padx 4 -pady 4 -expand 1

  # Where user chooses whether file is normal or binary
  label .update.lnormalbinary -text "Treat each file as:" -anchor w

  radiobutton .update.normalfile -text "Normal File" \
    -variable norm_bin -value "Normal" -anchor w
  radiobutton .update.binaryfile -text "Binary File (-kb)" \
    -variable norm_bin -value "Binary" -anchor w

  pack .update.lnormalbinary -in .update.normbin -side top -fill both -pady 3
  pack .update.normalfile .update.binaryfile -in .update.normbin -side left \
    -fill both -ipadx 2 -ipady 2 -padx 4 -pady 4 -expand 1

  # The OK/Cancel buttons
  button .update.ok -text "OK" \
    -command {
      if { $tagmode_selection == "" } { set tagmode_selection "Keep" }
      if { $updatename == "" } { 
          set tagname "BASE"
      } else {
          set tagname $updatename
      }
      if { $action_notag == "" } { set action_notag "Remove" }
      if { $get_all_dirs == "" } { set get_all_dirs "No" }
      if { $get_all_dirs == "No" } { set getdirname "" }
      if { $getdirname == "" } { 
        set dirname " "
      } else {
        set dirname $getdirname
      }
      if { $norm_bin == "" }     { set norm_bin "Normal" }
      #puts "from update_setup, tagname $tagname.  norm_bin $norm_bin"
      if { $tagmode_selection == "Keep" } {
        eval "cvs_update {BASE} {$norm_bin} {$action_notag} {$get_all_dirs} {$dirname} [workdir_list_files]"
      } elseif { $tagmode_selection == "Trunk" } {
        eval "cvs_update {HEAD} {$norm_bin} {$action_notag} {$get_all_dirs} {$dirname} [workdir_list_files]"
      } elseif { $tagmode_selection == "Join" } {
        notyet
      } elseif { $tagmode_selection == "Getrev" } {
        eval "cvs_update {$tagname} {$norm_bin} {$action_notag} {$get_all_dirs} {$dirname} [workdir_list_files]"
      } else {
        cvsfail "Internal TkCVS error.\ntagmode_selection $tagmode_selection."
      }
      wm withdraw .update
    }

  # Ugly hack.  Just a copy of the above.  Need to work on this.  -JW
  button .update.apply -text "Apply" \
    -command {
      if { $tagmode_selection == "" } { set tagmode_selection "Keep" }
      if { $updatename == "" } { 
          set tagname "BASE"
      } else {
          set tagname $updatename
      }
      if { $action_notag == "" } { set action_notag "Remove" }
      if { $get_all_dirs == "" } { set get_all_dirs "No" }
      if { $get_all_dirs == "No" } { set getdirname "" }
      if { $getdirname == "" } { 
        set dirname " "
      } else {
        set dirname $getdirname
      }
      if { $norm_bin == "" }     { set norm_bin "Normal" }
      #puts "from update_setup, tagname $tagname.  norm_bin $norm_bin"
      if { $tagmode_selection == "Keep" } {
        eval "cvs_update {BASE} {$norm_bin} {$action_notag} {$get_all_dirs} {$dirname} [workdir_list_files]"
      } elseif { $tagmode_selection == "Trunk" } {
        eval "cvs_update {HEAD} {$norm_bin} {$action_notag} {$get_all_dirs} {$dirname} [workdir_list_files]"
      } elseif { $tagmode_selection == "Join" } {
        notyet
      } elseif { $tagmode_selection == "Getrev" } {
        eval "cvs_update {$tagname} {$norm_bin} {$action_notag} {$get_all_dirs} {$dirname} [workdir_list_files]"
      } else {
        cvsfail "Internal TkCVS error.\ntagmode_selection $tagmode_selection."
      }
    }

  button .update.reset -text "Reset defaults" \
    -command {
      set tagmode_selection "Keep"
      set updatename ""
      set action_notag "Remove"
      set get_all_dirs "No"
      set getdirname ""
      set norm_bin "Normal"
    }

  button .update.quit -text "Quit" -command { wm withdraw .update }
 
  pack .update.ok .update.apply .update.reset .update.quit -in .update.down \
    -side left -ipadx 2 -ipady 2 -padx 4 -pady 4 -fill both -expand 1
 
  # Window Manager stuff
  wm withdraw .update
  wm title .update "Update a Module"
  wm minsize .update 1 1
  gen_log:log T "LEAVE"
}

proc update_run {} {
  global incvs

  gen_log:log T "ENTER"

  if {! [winfo exists .update]} {
     update_setup
  }
   
  wm deiconify .update
  raise .update
  gen_log:log T "LEAVE"
}
