#
# Tcl Library for TkCVS
#

#
# $Id: import.tcl,v 1.2 2006-06-30 17:40:13 ramsan Exp $
#
# Adds a new document to the repository.
#

proc import_setup {} {
  global cvsglb
  global cvscfg

  gen_log:log T "ENTER"
  # Give it a default
  set cvsglb(newvers) 1

  toplevel .import
  frame .import.top -relief groove -border 2
  frame .import.center
  frame .import.center.left
  frame .import.center.right
  frame .import.down -relief groove -border 2

  # When packing, I only want the right hand side and the buttons
  # to be interactively strechable (in case the user wants more space
  # to enter long document names).

  pack .import.top -side top -fill x -expand 1
  pack .import.center -side top -fill x -expand 1
  pack .import.down -side bottom -fill x -expand 1

  message .import.top.explain -justify left -aspect 400 -relief groove \
    -text "This will import the current directory and its sub-directories\
          into CVS, creating a new module."
  label .import.lnewcode -text "Module Name" -anchor w
  label .import.lnewdir  -text "Module Path" -anchor w
  label .import.lnewdesc -text "Descriptive Title" -anchor w
  label .import.lnewvers  -text "Version Number" -anchor w

  entry .import.tnewcode -relief sunken -textvariable cvsglb(newcode) -width 40
  entry .import.tnewdir -relief sunken -textvariable cvsglb(newdir) -width 40
  entry .import.tnewdesc -relief sunken -textvariable cvsglb(newdesc) -width 40
  entry .import.tnewvers -relief sunken -textvariable cvsglb(newvers) -width 40

  pack .import.center.left -side left -fill y
  pack .import.center.right -side left -fill both -expand 1

  pack .import.top.explain -side top -padx 2 -pady 2 -fill x -expand 1
  pack .import.lnewcode .import.lnewdir \
       .import.lnewdesc .import.lnewvers \
    -in .import.center.left \
    -side top -fill x -pady 3

  pack .import.tnewcode .import.tnewdir \
       .import.tnewdesc .import.tnewvers \
    -in .import.center.right \
    -side top -fill both -expand 1 -pady 3

  button .import.ok -text "OK" \
    -command do_import
  button .import.newdir -text "New Directory" \
    -command inewdir_run
  button .import.quit -text "Quit" \
    -command { wm withdraw .import }

  pack .import.ok .import.newdir .import.quit -in .import.down -side left \
    -ipadx 2 -ipady 2 -padx 4 -pady 4 -fill both -expand 1

  # Needed for slower framebuffers
  tkwait visibility .import

  wm withdraw .import
  wm title .import "Import a New Module"
  wm minsize .import 1 1

  toplevel .inewdir
  frame .inewdir.top
  frame .inewdir.loc
  frame .inewdir.desc
  frame .inewdir.down -relief groove -border 2

  # When packing, I only want the right hand side and the buttons
  # to be interactively strechable (in case the user wants more space
  # to enter long document names).

  pack .inewdir.down -side bottom -fill x -expand 1
  pack .inewdir.top -side top -fill x -expand 1
  pack .inewdir.loc -side top -fill x -expand 1
  pack .inewdir.desc -side left -fill both -expand 1

  message .inewdir.top.explain -justify left -aspect 400 -relief groove \
    -text "This will create a new directory under \$CVSROOT"

  label .inewdir.loc.lnewdir  -text "Location relative to \$CVSROOT" -anchor w
  entry .inewdir.loc.tnewdir -relief sunken -width 20 \
    -textvariable cvsglb(dnewdir)
  label .inewdir.loc.more  -text "(parent must already exist)" -anchor w

  label .inewdir.desc.lnewdesc -text "Descriptive Title" -anchor w
  entry .inewdir.desc.tnewdesc -relief sunken -width 40 \
    -textvariable cvsglb(dnewdesc)

  pack .inewdir.top.explain -side top -padx 2 -pady 2 -fill x -expand 1
  pack .inewdir.loc.lnewdir .inewdir.loc.tnewdir \
    -side left -fill x -expand 1 -pady 3
  pack .inewdir.desc.lnewdesc .inewdir.desc.tnewdesc \
    -side left -fill both -expand 1 -pady 3

  button .inewdir.ok -text "OK" \
    -command {do_inewdir; wm withdraw .inewdir}
  button .inewdir.quit -text "Quit" \
    -command { wm withdraw .inewdir }

  pack .inewdir.ok .inewdir.quit -in .inewdir.down -side left \
    -ipadx 2 -ipady 2 -padx 4 -pady 4 -fill both -expand 1

  if {$cvscfg(remote)} {
    .import.newdir configure -state disabled
  }

  wm withdraw .inewdir
  wm title .inewdir "Import a New Directory"
  wm minsize .inewdir 1 1
  gen_log:log T "LEAVE"
}

proc import_run {} {
  global cvsglb
  global cwd

  gen_log:log T "ENTER"

  set cvsglb(newcode) [file tail $cwd]
  if {! [winfo exists .import]} {
    import_setup
  }
  wm deiconify .import
  raise .import
  gen_log:log T "LEAVE"
}

proc inewdir_run {} {
  global cvscfg

  gen_log:log T "ENTER"
  if {$cvscfg(remote)} {
    cvs_remote_bad
    return 1
  }
  if {! [winfo exists .inewdir]} {
    import_setup
  }

  wm deiconify .inewdir
  raise .inewdir
  gen_log:log T "LEAVE"
}

proc do_import {} {
  global cvs
  global cvsglb
  global cvscfg
  global cwd
  global incvs
  global dtitle
  global dcontents
  global location
  global feedback

  gen_log:log T "ENTER"
  # Error checks
  if {$incvs} {
    cvsok "This directory is already in CVS.\nCan\'t import here!"
    return 1
  }
  if { $cvsglb(newdir) == "" } {
    cvsok "You must type in a directory."
    return 1
  }
  if { $cvsglb(newdesc) == "" } {
    cvsok "You must type in a module name."
    return 1
  }
  if { $cvsglb(newvers) == "" } {
    cvsok "You must type in a version number."
    return 1
  }
  if { $cvsglb(newcode) == "" } {
    cvsok "You must type in a module code."
    return 1
  }

  # Check that all apropriate Directories in newdirname exist
  set cvsglb(newdir) [string trimleft $cvsglb(newdir) "/"]
  set checkdirname [file dirname $cvsglb(newdir)]
  if {$checkdirname != $cvscfg(thisdir)} {
    set knowndirs [array names dtitle]
    foreach dc [array names location] {
      lappend knowndirs $location($dc)
    }
    gen_log:log D "Known directories ($knowndirs)"
    if {[lsearch -exact $knowndirs $checkdirname] == -1} {
     cvsok "The upper directory \"$checkdirname\" doesn\'t exist."
     return 1
    }
  }
  set dirpath [file join $cvscfg(cvsroot) $cvsglb(newdir)]
  gen_log:log D "New directory $dirpath"
  set create_module 1
  if {[file isdirectory $dirpath]} {
    set mess "NOTE:  You are importing over a directory that already exists!"
    if {[cvsconfirm $mess] == 0} {
      set create_module 0
    } else {
      return 1
    }
  }

  # Make a baseline tag

  set versions [split $cvsglb(newvers) ".,/ -"]
  set baseline "baseline-[join $versions {_}]"

  feedback_cvs $feedback(cvs) "Importing $cvsglb(newcode), please wait"
  set commandline "$cvs -d $cvscfg(cvsroot) import -m \"Imported using TkCVS\" \
	    $cvsglb(newdir) VENDOR $baseline"
  gen_log:log C "$commandline"
  exec_command "CVS Import" "$commandline"
  feedback_cvs $feedback(cvs) ""

  # Update the modules file.
  if {$create_module == 1} {
    set commandline "$cvs -d $cvscfg(cvsroot) checkout CVSROOT/modules"
    exec_command "Checkout New Module" "$commandline"

    cd CVSROOT                            
    gen_log:log F "CD [pwd]"
    set modfile [open modules a]
    # ramsan: commented following line
    #puts $modfile "#D\t$cvsglb(newcode)\t$cvsglb(newdesc)"
    puts $modfile "#M\t$cvsglb(newcode)\t$cvsglb(newdesc)"
    puts $modfile "$cvsglb(newcode)\t$cvsglb(newdir)"
    close $modfile
    set commandline "$cvs -d $cvscfg(cvsroot) ci -m \"added $cvsglb(newcode)\" modules"
    gen_log:log C "$commandline"
    exec_command "CVS Checkin CVSROOT" "$commandline"
    cd ../                              
    gen_log:log F "CD [pwd]"
    set commandline "$cvs -d $cvscfg(cvsroot) -Q release -d CVSROOT"
    gen_log:log C "$commandline"
    eval exec "$commandline"
    cd $cwd
    gen_log:log F "CD [pwd]"
  }

  modbrowse_run $cvscfg(cvsroot)
  wm withdraw .import

  # Now check out the new module
  cd ..
  gen_log:log F "CD [pwd]"
  cvs_checkout $cvsglb(newcode) HEAD
  cd $cwd
  gen_log:log F "CD [pwd]"

  gen_log:log T "LEAVE"
}

proc do_inewdir { } {
  global cvsglb
  global cvs
  global cwd
  global dtitle
  global cvscfg

  gen_log:log T "ENTER"
  # Error checks
  if { $cvsglb(dnewdir) == "" } {
    cvsok "You must type in a directory."
    return 1
  }
  if { $cvsglb(dnewdesc) == "" } {
    cvsok "You must type in a description."
    return 1
  }

  # Check that all apropriate Directories in newdirname exist
  set cvsglb(dnewdir) [string trimleft $cvsglb(dnewdir) "/"]
  set checkdirname [file dirname $cvsglb(dnewdir)]

  if {[lsearch -exact [array names dtitle] $cvsglb(dnewdir)] >= 0} {
     cvsok "This directory already exists!"
     return 1
  }
  set newdir [file join $cvscfg(cvsroot) $cvsglb(dnewdir)]
  gen_log:log F "MKDIR $newdir"
  file mkdir $newdir

  cd
  gen_log:log F "CD [pwd]"
  set commandline "$cvs -d $cvscfg(cvsroot) checkout CVSROOT/modules"
  gen_log:log C "$commandline"
  catch {eval "exec $commandline"}
  cd CVSROOT
  gen_log:log F "CD [pwd]"
  set modfile [open modules a]
  puts $modfile ""
  puts $modfile "#D	$cvsglb(dnewdir)\t$cvsglb(dnewdesc)"
  close $modfile
  set commandline "$cvs -d $cvscfg(cvsroot) ci -m \"added $cvsglb(dnewdir)\" modules"
  exec_command "CVS Checkin CVSROOT" "$commandline"
  cd $cwd
  gen_log:log F "CD [pwd]"

  read_modules_setup $cvscfg(cvsroot)
  modbrowse_run $cvscfg(cvsroot)
  gen_log:log T "LEAVE"
}
