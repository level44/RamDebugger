#
# TCL Library for tkcvs
#

#
# $Id: workdir.tcl,v 1.2 2006-06-30 17:40:19 ramsan Exp $
#
# Current working directory display.  Handles all of the functions
# concerned with navigating about the current directory on the main
# window.
#

set indexPrefix "^"
set filenamePrefix "!"

proc workdir_setup {} {
  global cwd
  global module_dir
  global cvscfg
  global current_tagname
  global feedback
  global logclass

  gen_log:log T "ENTER"
  set cwd [pwd]
  set pid [pid]

  # Window manager stuff
  wm title . "TkCVS"
  wm iconname . "TkCVS"
  wm iconbitmap . @$cvscfg(bitmapdir)/tkcvs48.xbm
  wm minsize . 5 10

  workdir_menus

  #
  # Top section - where we are, where the module is
  #
  frame .top -relief groove -border 2 -width 480
  frame .top.cwd
  frame .top.root
  frame .top.dtag
  pack .top -side top -fill x

  pack .top.cwd -side top -fill x -expand yes
  label .top.cwd.lcwd -text "Current Directory" \
     -anchor w -width 18 -font $cvscfg(guifont)
  entry .top.cwd.tcwd -textvariable cwd
  pack .top.cwd.lcwd -side left -fill x
  pack .top.cwd.tcwd -side left -fill x -expand yes

  pack .top.root -side top -fill x -expand yes
  label .top.root.lmodule -text "Module Location" \
     -anchor w -width 18 -font $cvscfg(guifont)
  label .top.root.tmodule -textvariable module_dir \
     -anchor w -font $cvscfg(guifont)
  pack .top.root.lmodule -side left -fill x
  pack .top.root.tmodule -side left -fill x -expand yes

  pack .top.dtag -side top -fill x -expand yes
  label .top.dtag.ltagname -text "Directory Tag" \
     -anchor w -width 18 -font $cvscfg(guifont)
  label .top.dtag.ttagname -textvariable current_tagname \
     -anchor w -font $cvscfg(guifont)
  pack .top.dtag.ltagname -side left -fill x
  pack .top.dtag.ttagname -side left -fill x -expand yes

  bind .top.cwd.tcwd <Return> {change_dir $cwd} 

  # Pack the bottom before the middle so it doesnt disappear if
  # the window is resized smaller
  frame .bottom -relief groove -border 2 -height 128
  frame .bottom.filters
  pack .bottom -side bottom -fill x
  pack .bottom.filters -side top -fill x

  label .bottom.filters.showlbl -text "Show:" -anchor w -font $cvscfg(guifont)
  entry .bottom.filters.showentry -textvariable cvscfg(file_filter) -width 12
  label .bottom.filters.hidelbl -text "   Hide:" -anchor w \
     -font $cvscfg(guifont)

  entry .bottom.filters.hideentry -width 12 \
     -textvariable cvscfg(default_ignore_filter)
  label .bottom.filters.space -text "    " -anchor w -font $cvscfg(guifont)
  button .bottom.filters.cleanbutton -text "Clean:" -font $cvscfg(guifont) \
     -command workdir_cleanup
  entry .bottom.filters.cleanentry -width 12 \
     -textvariable cvscfg(clean_these)
  bind .bottom.filters.showentry <Return> {setup_dir}
  bind .bottom.filters.hideentry <Return> {
     set cvscfg(default_ignore_filter) [.bottom.filters.hideentry get]
     setup_dir}
  bind .bottom.filters.cleanentry <Return> {workdir_cleanup}
  pack .bottom.filters.showlbl -side left
  pack .bottom.filters.showentry -side left
  pack .bottom.filters.hidelbl -side left
  pack .bottom.filters.hideentry -side left
  pack .bottom.filters.space -side left
  pack .bottom.filters.cleanbutton -side left -ipadx 0 -ipady 0
  pack .bottom.filters.cleanentry -side left


  frame .bottom.buttons
  frame .bottom.buttons.dirfuncs -relief groove -bd 2
  frame .bottom.buttons.cvsfuncs -relief groove -bd 2
  frame .bottom.buttons.modfuncs -relief groove -bd 2
  pack .bottom.buttons -side top -fill x -expand yes
  pack .bottom.buttons.dirfuncs -side left -fill x -expand yes
  pack .bottom.buttons.cvsfuncs -side left -fill x -expand yes
  pack .bottom.buttons.modfuncs -side left -fill x -expand yes

  #
  # Action buttons along the bottom of the screen.
  #
  button .bedit_files -image Fileedit \
     -command { workdir_edit_file [workdir_list_files] }
  button .bdelete_file -image Delete \
     -command { workdir_delete_file [workdir_list_files] }
  button .bclear -image Clear \
     -command { .main.file_list select clear 0 end }
  button .brefresh -image Refresh \
     -command setup_dir
  button .bconflict -image Conflict \
     -command { cvs_merge_conflict [workdir_list_files] }

  button .blogfile -image Branches \
     -command { cvs_logcanvas [workdir_list_files] }
  button .btag -image Tag \
     -command tag_dialog
  button .badd_files -image Add \
     -command { add_dialog [workdir_list_files] }
  button .bremove -image Remove \
     -command { subtract_dialog [workdir_list_files] }
  button .bdiff -image Diff \
     -command { eval cvs_diff [workdir_list_files] } 
  button .bcheckin -image Checkin \
     -command commit_run
  button .bupdate -image Checkout \
     -command { cvs_update "BASE" "Normal" \
        "Remove" "No" " " [workdir_list_files] }
  button .bmodbrowse -image Modules \
     -command { modbrowse_run $cvscfg(cvsroot) }
  button .bquit -text "Quit" -font $cvscfg(guifont) \
     -command exit_cleanup

  pack .bedit_files \
       .bdelete_file \
       .bclear \
       .brefresh \
    -in .bottom.buttons.dirfuncs \
    -side left -ipadx 1 -ipady 1 -fill x -expand 1

  pack .blogfile \
       .btag \
       .badd_files \
       .bremove \
       .bdiff \
     -in .bottom.buttons.cvsfuncs \
     -side left -ipadx 1 -ipady 1 -fill x -expand 1

  pack .bcheckin \
       .bupdate \
       .bconflict \
       .bmodbrowse \
     -in .bottom.buttons.modfuncs \
     -side left -ipadx 1 -ipady 1 -fill x -expand 1
  pack .bquit \
     -in .bottom.buttons.modfuncs \
     -side right -ipadx 0 -ipady 0 -fill x -expand 1

# RAMSAN adding: the Ctrl's
  set_tooltips .bedit_files \
     {"Edit the selected files Ctrl-e"}
  set_tooltips .bdelete_file \
     {"Delete the selected files from the current directory Ctrl-x"}
  set_tooltips .bclear \
     {"Unselect all files"}
  set_tooltips .brefresh \
     {"Re-read the current directory Ctrl-r"}
  set_tooltips .bconflict \
     {"Merge Conflicts using TkDiff"}
  set_tooltips .blogfile \
     {"See the revision log and branches of the selected files Ctrl-l"}
  set_tooltips .badd_files \
     {"Add the selected files to the repository Ctrl-a"}
  set_tooltips .btag \
     {"Tag the selected files Ctrl-t"}
  set_tooltips .bremove \
     {"Remove the selected files from the repository"}
  set_tooltips .bcheckin \
     {"Check in (commit) the selected files to the repository Ctrl-i"}
  set_tooltips .bupdate \
     {"Update (checkout, patch) the selected files from the repository Ctrl-u"}
  set_tooltips .bdiff \
     {"Compare the selected files with the repository version Ctrl-d"}
  set_tooltips .bmodbrowse \
     {"Open the Module Browser"}
  set_tooltips .bquit \
     {"Exit from TkCVS Ctrl-q"}


  #
  # Entry widget to be used for feedback
  #
  set feedback(cvs) [entry .bottom.feedback -width 55 -state disabled]
  pack .bottom.feedback -side bottom -fill x -expand yes
    
  #
  # The listboxes - where the files, their status and tag are listed
  #
  frame .main
  pack .main -side bottom -fill both -expand yes

  # Try to make sure the window isn't taller than the screen
  set screenHeight [winfo vrootheight .]
  set linespace_gui [font metrics $cvscfg(guifont) -linespace]
  set linespace_list [font metrics $cvscfg(listboxfont) -linespace]
  set mainreq [expr {$cvscfg(y_size) * $linespace_list}]
  set bottomreq [winfo reqheight .bottom]
  set widgetreq [expr {$bottomreq * 2}]
  set totalreq [expr {$widgetreq + $mainreq}]
  gen_log:log D "Total $totalreq, $screenHeight available"
  if {$totalreq > $screenHeight} {
    # Leave a few pixels for the window frame
    set avail [expr {$screenHeight - $widgetreq - 20}]
    gen_log:log D "($avail / $linespace_list)"
    set cvscfg(y_size) [expr {$avail / $linespace_list}]
    gen_log:log D "reducing y_size to $cvscfg(y_size) lines"
  }

  listbox .main.file_list -yscroll {workdir_scroll_file_list} \
    -relief sunken -height $cvscfg(y_size) -width 0 -setgrid yes \
    -selectmode extended -font $cvscfg(listboxfont)
  listbox .main.status_list -yscroll {workdir_scroll_status_list} \
    -relief sunken -height $cvscfg(y_size) -width 0 -setgrid yes \
    -font $cvscfg(listboxfont)
  listbox  .main.tag_list -yscroll {workdir_scroll_tag_list} \
    -relief sunken -height $cvscfg(y_size) -width 0 -setgrid yes \
    -font $cvscfg(listboxfont)
  scrollbar .main.scroll -command {workdir_scroll_scrollbar} -relief sunken

  pack .main.scroll -side right -fill both -expand no
  pack .main.file_list -side left -fill both -expand yes
  pack .main.status_list -side left -fill both -expand yes
  pack .main.tag_list -side left -fill both -expand yes

  bind .main.file_list  <Double-Button-1> \
     { workdir_edit_file [workdir_list_files] }
  #RAMSAN
  bind .main.file_list  <Return> \
     { workdir_edit_file [workdir_list_files] }
  bind .main.file_list  <BackSpace> \
     { workdir_edit_file .. }
  focus .main.file_list

  bind .main.file_list  <Button-2>         { nop }
  bind .main.file_list  <ButtonRelease-3>  { nop }
  bind .main.status_list <Double-Button-1> { nop }
  bind .main.status_list <ButtonRelease-1> { nop }
  bind .main.status_list <1>               { nop }
  bind .main.status_list <2>               { nop }
  bind .main.status_list <Any-B1-Motion>   { nop }
  bind .main.status_list <Any-B2-Motion>   { nop }
  bind .main.status_list <Any-B3-Motion>   { nop }


  # Do the directory listing.  We go to change_dir so that the original
  # working directory gets in the Go menu
  change_dir [pwd]
  gen_log:log T "LEAVE"
}

proc workdir_menus {} {
  global cvscfg
  global usermenu
  #RAMSAN
  global usermenuTCL
  global cvsmenu

  gen_log:log T "ENTER"
  set startdir [pwd]

  frame .menubar -relief raised -bd 2
  pack .menubar -side top -fill x

  #
  # Create the Menu bar
  #
  menubutton .menubar.file -text File -underline 0 \
     -menu .menubar.file.m -font $cvscfg(guifont)
  menubutton .menubar.reports -text Reports -underline 0 \
     -menu .menubar.reports.m -font $cvscfg(guifont)
  menubutton .menubar.options -text Options -underline 0 \
     -menu .menubar.options.m -font $cvscfg(guifont)
  menubutton .menubar.help -text Help -underline 0 \
     -menu .menubar.help.m -font $cvscfg(guifont)
  pack .menubar.file \
       .menubar.reports \
       .menubar.options \
     -side left
  if { [info exists cvsmenu] || [info exists usermenu] || [info exists usermenuTCL] } {
    menubutton .menubar.user -text "User Defined" -underline 0 \
       -menu .menubar.user.m -font $cvscfg(guifont)
    pack .menubar.user -side left
    gen_log:log T "Adding user defined menu"
  }
  menubutton .menubar.goto -text "Go" -underline 0 \
     -menu .menubar.goto.m -font $cvscfg(guifont)
  pack .menubar.goto -side left
  pack .menubar.help -side right

  #
  # Create the Menus
  #
  menu .menubar.file.m -font $cvscfg(guifont)
  .menubar.file.m add command -label "Open" -underline 0 \
     -command { workdir_edit_file [workdir_list_files] }
  .menubar.file.m add command -label "Print" -underline 0 \
     -command { workdir_print_file  [workdir_list_files ] }
  .menubar.file.m add separator
  .menubar.file.m add command -label "Browse Modules" -underline 0 \
     -command { modbrowse_run $cvscfg(cvsroot) }
  .menubar.file.m add command -label "Cleanup" -underline 0 \
     -command workdir_cleanup
  .menubar.file.m add separator
  .menubar.file.m add command -label "Check Out (Update)" -underline 11 \
     -command { \
        cvs_update {BASE} {Normal} {Remove} {No} { } [workdir_list_files] }
  .menubar.file.m add command -label "Check In (Commit)" -underline 6 \
     -command commit_run
  .menubar.file.m add command -label "Add Files" -underline 0 \
     -command { add_dialog [workdir_list_files] }
  .menubar.file.m add command -label "Remove Files" -underline 0 \
     -command { subtract_dialog [workdir_list_files] }
  .menubar.file.m add command -label "Tag Files" -underline 0 \
     -command tag_dialog
  .menubar.file.m add command -label "Log Browse" -underline 0 \
     -command { cvs_logcanvas [workdir_list_files] }
  .menubar.file.m add command -label "Merge Conflict" -underline 0 \
     -command { cvs_merge_conflict [workdir_list_files] }
  .menubar.file.m add separator
  .menubar.file.m add command -label "Lock Files" \
     -command {cvs_lock "lock" { } [workdir_list_files] }
  .menubar.file.m add command -label "Unlock Files" \
     -command {cvs_lock "unlock" { } [workdir_list_files] }
  .menubar.file.m add separator
  .menubar.file.m add command -label "Shell window" -underline 0 \
     -command {eval exec $cvscfg(shell) >& $cvscfg(null) &}
  .menubar.file.m add separator
  .menubar.file.m add command -label Exit -underline 1 \
     -command exit
  
  menu .menubar.reports.m -font $cvscfg(guifont)
  .menubar.reports.m add command -label "CVS check" -underline 4 \
     -command cvs_check
  .menubar.reports.m add command -label "CVS status" -underline 4 \
     -command { cvs_status [workdir_list_files] }
  .menubar.reports.m add command -label "Sticky status" -underline 4 \
     -command { cvs_tag_status [workdir_list_files] }
  .menubar.reports.m add command -label "CVS diff" -underline 4 \
     -command { cvs_diff [workdir_list_files] }
  .menubar.reports.m add command -label "CVS log" -underline 4 \
     -command { cvs_log [workdir_list_files] }
  .menubar.reports.m add separator
  .menubar.reports.m add command -label "Locked files" \
     -command {report_locks}



  menu .menubar.options.m -font $cvscfg(guifont)
  .menubar.options.m add command -label "Checkout with Options" -underline 0 \
     -command update_run
  .menubar.options.m add separator
  .menubar.options.m add checkbutton -label "Show hidden files" \
     -variable cvscfg(allfiles) -onvalue 1 -offvalue 0 \
     -command setup_dir
  .menubar.options.m add checkbutton -label "Automatic directory status" \
     -variable cvscfg(auto_status) -onvalue "true" -offvalue "false" \
     -command setup_dir
  .menubar.options.m add separator
  .menubar.options.m add checkbutton -label "Report Check recursively" \
     -variable cvscfg(checkrecursive) -onvalue {} -offvalue -l 
  .menubar.options.m add checkbutton -label "Report Status Recursively" \
     -variable cvscfg(recurse) -onvalue "true" -offvalue "false" 
  .menubar.options.m add cascade -label "CVS Status Detail" \
     -menu .menubar.options.m.report_detail
  .menubar.options.m add cascade -label "CVS Log Detail" \
     -menu .menubar.options.m.logfile_detail
  .menubar.options.m add cascade -label "Work Mode" \
     -menu .menubar.options.m.work_mode
  .menubar.options.m add separator
  .menubar.options.m add checkbutton -label "Tracing On/Off" \
     -variable cvscfg(logging) -onvalue 1 -offvalue 0 \
     -command log_toggle
  .menubar.options.m add cascade -label "Trace Level" \
     -menu .menubar.options.m.trace_level

  menu .menubar.options.m.trace_level -font $cvscfg(guifont)
  .menubar.options.m.trace_level add checkbutton -label "CVS commands (C)" \
     -variable logclass(C) -onvalue "C" -offvalue "" \
     -command gen_log:changeclass
  .menubar.options.m.trace_level add checkbutton -label "File creation/deletion (F)" \
     -variable logclass(F) -onvalue F -offvalue "" \
     -command gen_log:changeclass
  .menubar.options.m.trace_level add checkbutton -label "Function entry/exit (T)" \
     -variable logclass(T) -onvalue T -offvalue "" \
     -command gen_log:changeclass
  .menubar.options.m.trace_level add checkbutton -label "Debugging (D)" \
     -variable logclass(D) -onvalue D -offvalue "" \
     -command gen_log:changeclass

  menu .menubar.options.m.report_detail -font $cvscfg(guifont)
  .menubar.options.m.report_detail add radiobutton -label "Verbose" \
     -variable cvscfg(rdetail) -value "verbose"
  .menubar.options.m.report_detail add radiobutton -label "Summary" \
     -variable cvscfg(rdetail) -value "summary"
  .menubar.options.m.report_detail add radiobutton -label "Terse" \
     -variable cvscfg(rdetail) -value "terse"

  menu .menubar.options.m.logfile_detail -font $cvscfg(guifont)
  .menubar.options.m.logfile_detail add radiobutton -label "Verbose" \
     -variable cvscfg(ldetail -value "verbose"
  .menubar.options.m.logfile_detail add radiobutton -label "Summary" \
     -variable cvscfg(ldetail) -value "summary"

  menu .menubar.options.m.work_mode
  .menubar.options.m.work_mode add radiobutton -label "Locking" \
     -variable cvscfg(workmode)  -value "locking"
  .menubar.options.m.work_mode add radiobutton -label "Concurrent" \
     -variable cvscfg(workmode) -value "concurrent"

  menu .menubar.goto.m -font $cvscfg(guifont)
  .menubar.goto.m add command -label "Home" \
     -command {change_dir ~}
  .menubar.goto.m add command -label $startdir \
     -command [ format {change_dir %s} $startdir ]

  menu .menubar.help.m -font $cvscfg(guifont)
  .menubar.help.m add command -label "About TkCVS" -underline 0 \
     -command aboutbox
  .menubar.help.m add command -label "About CVS" -underline 6 \
     -command cvs_version
  .menubar.help.m add separator
  .menubar.help.m add command -label "Working with TkCVS" \
     -command working_with_tkcvs
  .menubar.help.m add command -label "Checking out Modules" \
     -command checking_out_modules
  .menubar.help.m add command -label "Exporting" \
     -command exporting
  .menubar.help.m add command -label "Tagging and Branching" \
     -command tagging_and_branching
  .menubar.help.m add command -label "Importing New Modules" \
     -command importing_new_modules
  .menubar.help.m add separator
  .menubar.help.m add command -label "Current Directory Display" \
     -command current_directory
  .menubar.help.m add command -label "Buttons" \
     -command buttons_help
  .menubar.help.m add separator
  .menubar.help.m add command -label "Module Browser" \
     -command module_browser
  .menubar.help.m add command -label "File Browser" \
     -command file_browser
  .menubar.help.m add command -label "Log Browser" \
     -command log_browser
  .menubar.help.m add separator
  .menubar.help.m add command -label "Configuration Files" \
     -command configuration_files
  .menubar.help.m add command -label "Environment Variables" \
     -command environment_variables
  .menubar.help.m add command -label "User Defined Menu" \
     -command user_defined_menu
  .menubar.help.m add command -label "CVS modules File" \
     -command cvs_modules_file

  #
  # Add user commands to the menu.
  #
  if { [info exists cvsmenu] || [info exists usermenu] || [info exists usermenuTCL] } {
    menu .menubar.user.m -font $cvscfg(guifont)
  }
  if {[info exists cvsmenu]} {
    foreach item [array names cvsmenu] {
      .menubar.user.m add command -label $item \
         -command "eval cvs_usercmd $cvsmenu($item) \[workdir_list_files\]"
    }
  }
  if {[info exists usermenu]} {
    .menubar.user.m add separator
    foreach item [array names usermenu] {
      .menubar.user.m add command -label $item \
         -command "eval cvs_anycmd $usermenu($item) \[workdir_list_files\]"
    }
  }
  # RAMSAN
  if {[info exists usermenuTCL]} {
    .menubar.user.m add separator
    foreach item [array names usermenuTCL] {
      .menubar.user.m add command -label $item \
         -command "eval cvs_anycmd_tcl $usermenuTCL($item) \[workdir_list_files\]"
    }
  }
  gen_log:log T "LEAVE"
}

proc workdir_list_files {} {
  global cvscfg

  gen_log:log T "ENTER"
  foreach item [.main.file_list curselection] {
    set itemstring [.main.file_list get $item]
    gen_log:log D "selection is $itemstring"
    if {[string match "no file *" $itemstring]} {
      set str [split $itemstring]
      set itemstring [lindex $str 2]
    }
    if {[info exists getlist]} {
      lappend getlist $itemstring
    } else {
      set getlist $itemstring
    }
  }

  if {[info exists getlist]} {
    return $getlist
  } else {
    return $cvscfg(thisdir)
  }
}

proc workdir_edit_command {file} {
  global cvscfg

  gen_log:log T "ENTER ($file)"
  if {[info exists cvscfg(editors)]} {
    foreach {editor pattern} $cvscfg(editors) {
      if {[string match $pattern $file]} {
        return "$editor $file"
      }
    }
  }
  return "$cvscfg(editor) $cvscfg(editorargs) $file"
}

proc workdir_edit_file {args} {
  global cvscfg
  global cwd
  global feedback

  gen_log:log T "ENTER ($args)"

  set filelist [join $args]
  if {$filelist == $cvscfg(thisdir)} {
    cvsfail "Please select some files to edit first!"
    return
  }

  feedback_cvs $feedback(cvs) "Building scroll list, please wait!"
  if {[file isdirectory $filelist]} {
    change_dir $filelist
  } else {
    feedback_cvs $feedback(cvs) "Starting editor, please wait!"
    foreach file $filelist {
      if {[file isfile $file]} {
        eval exec [workdir_edit_command $file] >& $cvscfg(null) &
      }
    }
  }
  feedback_cvs $feedback(cvs) ""
  gen_log:log T "LEAVE"
}

proc workdir_status_list_files {} {
  foreach item [.main.status_list curselection] {
    if {[info exists getlist]} {
      lappend getlist [.main.file_list get $item]
    } else {
      set getlist [.main.file_list get $item]
    }
  }

  if {[info exists getlist]} {
    set cur_select [.main.status_list curselection]
    set start_pos [lindex $cur_select 0]
    set end_pos [expr {[llength $cur_select] + $start_pos - 1}]
    .main.main.file_list select set $start_pos $end_pos
    return $getlist
  } else {
    set cur_select [.main.status_list curselection]
    return {}
  }
}

proc workdir_status_list_file {yposition} {
  set cur_select [.main.status_list nearest $yposition]
  # .main.file_list select from $cur_select
  # .main.file_list select to   $cur_select
  return $cur_select
}

#------------------------------------------------------
# Update the "Go" menu for directories we can go to
# new_dir - the directory we're going to
# doPwd   - tells whether the directory path has
#           been specified  1 means relative to cwd
#                           0 means fully path specified
#-------------------------------------------------------
proc update_go {new_dir} {
  global cvscfg
  global dirlist
  global maxdirs
  global dirlen
  
  gen_log:log T "ENTER ($new_dir)"
  if {$new_dir == $cvscfg(thisdir)} { return }
  if {$new_dir == "~" } { return }
  if {$new_dir == ".."} {
    set new_dir [file dirname [pwd]]
  }
  if {[file pathtype $new_dir] == "relative"} {
    # Get full pathname of directory
    set new_dir [format {%s/%s} [pwd] $new_dir]
  }

  #puts $new_dir
  # Check if already in Go list
  set dirlocation  [lsearch -exact $dirlist $new_dir]

  # Move a directory already in the list to the top of the list
  if {$dirlocation != -1} {
    set dirlist [lreplace $dirlist $dirlocation $dirlocation ]
    set dirlist [linsert $dirlist 0 $new_dir]
  } else {
    set dirlist [linsert $dirlist 0 $new_dir]
  }
  set dirlen  [llength $dirlist]

  # Truncate end of directory list if we have too many directories
  if {$dirlen > $maxdirs} {
    set dirlen [incr dirlen -1]
    set dirlist [lreplace $dirlist $dirlen $dirlen ]
  }
 
  # Destroy old menu selections for "Go"
  destroy .menubar.goto.m
  menu .menubar.goto.m -font $cvscfg(guifont)
  .menubar.goto.m add command -label "Home" \
     -command {change_dir ~}

  # Rebuild menu selections for "Go" with new dirlist
  for {set i 0} {$i < $dirlen} {incr i 1} {
    set tmpdir [lindex $dirlist $i]
    .menubar.goto.m add command -label $tmpdir \
       -command [format {change_dir %s} $tmpdir]
  }
  gen_log:log T "LEAVE"
}

proc change_dir {new_dir} {
  global cwd

  gen_log:log T "ENTER ($new_dir)"
  update_go $new_dir
  set cwd $new_dir
  setup_dir

  gen_log:log T "LEAVE"
}


# I modified this a lot to support the status listbox and marked canvas.
# I cringe at the size of the procedure -- it needs to be broken into smaller 
# ones badly.
# -sj

proc setup_dir { } {
  #
  # Call this when entering a directory.  It puts all of the file names
  # in the listbox, and reads the CVS or directory.
  #
  global cwd
  global module_dir
  global incvs
  global cvscfg
  global current_tagname

  gen_log:log T "ENTER"

  if { ! [winfo exists .] } {
    workdir_setup
  } else {
    .main.file_list delete 0 end
    .main.status_list delete 0 end
    .main.tag_list delete 0 end
  }
  set module_dir "Not in the repository"
  set current_tagname "No directory tag"

  if {[file isdirectory $cwd]} {
    cd $cwd
    gen_log:log F "CD [pwd]"
    set cwd [pwd]

    cvsroot_check
    gen_log:log D " incvs = $incvs"

    set cvscfg(ignore_file_filter) $cvscfg(default_ignore_filter)

    if { [ file exists ".cvsignore" ] } {
      set fileId [ open ".cvsignore" "r" ]
      while { [ eof $fileId ] == 0 } {
        gets $fileId line
        append cvscfg(ignore_file_filter) " $line"
      }
      close $fileId
    }

    set filelist [ getFiles ]

    # Select from those files only the ones we want (e.g., no CVS dirs)
    set j 0
    foreach i $filelist {
      if { [ isCmDirectory $i ] } {
        if {$i == "CVS"} {
          read_cvs_dir $cwd/$i
        } else {
          nop
        }
      } else {
        .main.file_list insert end $i
        # count actual number of visible elements (not showing CM directories)
        set j [expr {$j + 1}]
      }
    }

    if {! $incvs} {
      set module_dir "Not a CVS directory."
      set current_tagname "Not a CVS directory."
      # unpack the status listbox and scrollbar from the screen
      pack forget .main.status_list .main.tag_list
    } else {
      pack .main.status_list -side left -fill both -expand yes
      pack .main.tag_list -side left -fill both -expand yes
      setup_columns
    }
  }
  
  gen_log:log T "LEAVE"
}


# Assumes .file_list has a list, and .status_list and .tag_list are empty
# and it will modify all three listboxes
proc setup_columns { } {
  global incvs
  global cvs
  global cvscfg

  gen_log:log T "ENTER"
  if {! $incvs} {
    cvs_notincvs
    return 1
  }

  # gets cvs status in current directory only, pulling out lines that include
  # Status: or Sticky Tag:, putting each file's info (name, status, and tag)
  # into a list, which is an element in the overall list.
  set commandline "$cvs -n -q -l status -l $cvscfg(thisdir)"
  gen_log:log C "$commandline"
  catch {eval "exec $commandline"} raw_log

  set log_lines [split $raw_log "\n"]
  foreach logline $log_lines {
    if {[string match "File:*" $logline]} {
      # This nastiness is all because versions of tcl < 8.1 dont have
      # advanced regular expressions.
      set line [split $logline "\t "]
      set line [join $line]
      regsub -all {  *} $line " " line
      gen_log:log D "$line"
      set elems [llength $line]
      set statindex [lsearch -exact $line "Status:"]
      set filename [lrange $line 1 [expr {$statindex - 1}]]
      if {[llength $filename] > 1} {
         set filename [lindex $line [expr {$statindex - 1}]]
         set nofile($filename) 1
      }
      set status($filename) [lrange $line [expr {$statindex + 1}] end]

     # RAMSAN
      if { $status($filename) == "Up-to-date" } {
	  set status($filename) ok
      }


    } elseif {[string match "*Working revision:*" $logline]} {
      regsub -all {  *} $logline { } logline
      set line [split $logline]
      #gen_log:log D "$line"
      set elems [llength $line]
      set workingrev($filename) [lrange $line 3 3]
    } elseif {[string match "*Sticky Tag:*" $logline]} {
      regsub -all {  *} $logline { } logline
      set line [split $logline]
      #gen_log:log D "$line"
      set elems [llength $line]
      set stickytag($filename) [lrange $line 4 end]
      if { $stickytag($filename) == "(none)" } {
        set stickytag($filename) "(main)"
      }
    }
  }

  set givenfiles [.main.file_list get 0 end] 

  # There may be files that CVS doesnt know about
  foreach givenfilename $givenfiles {
    # Match log entries to "given" files
    if {! [info exists status($givenfilename)] } {
      if {[file isdirectory $givenfilename]} {
        if {[file isdirectory [file join $givenfilename "CVS"]]} {
          .main.status_list insert end "<directory:CVS>"
        } elseif {[file isdirectory [file join $givenfilename CVS.adm]]} {
          .main.status_list insert end "<directory:oldCVS>"
        } elseif {[file isdirectory [file join $givenfilename RCS]]} {
          .main.status_list insert end "<directory:RCS>"
        } elseif {[file isdirectory [file join $givenfilename SCCS]]} {
          .main.status_list insert end "<directory:SCCS>"
        } else {
          .main.status_list insert end "<directory>"
        }
	.main.tag_list insert end " "
      } else {
        .main.status_list insert end "  ?"
        .main.tag_list insert end " "
      }
    } else {
      .main.status_list insert end "$status($givenfilename)"
      .main.tag_list insert end "$workingrev($givenfilename)   on   $stickytag($givenfilename)" 
    }
  }

  # There may be files that CVS knows about, but they've been removed
  foreach cvsfile [array names nofile] {
    set idx 0
    foreach givenfile [.main.file_list get 0 end] {
      set order [concat $givenfile $cvsfile]
      set sorted [lsort -ascii $order]
      if {[lindex $order 0] != [lindex $sorted 0]} {
        set newfile($idx) $cvsfile
        break
      }
      incr idx
    }
  }
  set boost 0
  foreach i [lsort -integer [array names newfile]] {
    set newpos [expr {$i + $boost}]
    # When you insert one, the indices jump ahead - hence the boost
    .main.file_list insert $newpos "no file $newfile($i)"
    .main.status_list insert $newpos "$status($newfile($i))"
    .main.tag_list insert $newpos "$workingrev($newfile($i))  on  $stickytag($newfile($i))"
    incr boost
  }

  gen_log:log T "LEAVE"
}

proc read_cvs_dir {dirname} {
#
# Reads a CVS directory
#
  global module_dir
  global cvscfg
  global current_tagname

  gen_log:log T "ENTER ($dirname)"
  if {[file isdirectory $dirname]} {
    if {[file isfile [file join $dirname Repository]]} {
      set f [open [file join $dirname Repository] r]
      gets $f module_dir
      close $f
      gen_log:log D "  MODULE $module_dir"
      if {[file isfile [file join $dirname Root]]} {
        set f [open [file join $dirname Root] r]
        gets $f cvscfg(cvsroot)
        close $f
        gen_log:log D "  CVSROOT $cvscfg(cvsroot)"
      }
      if {[file isfile [file join $dirname Tag]]} {
        set f [open [file join $dirname Tag] r]
        gets $f current_tagname
        close $f
        set current_tagname [string trimleft $current_tagname "T"]
        gen_log:log D "  BRANCH TAG $current_tagname"
      }
    } else {
      cvsfail "Repository file not found in $dirname"
    }
  } else {
    cvsfail "$dirname is not a directory"
  }
  gen_log:log T "LEAVE"
}

proc workdir_scroll_file_list {first last} {
# support middle-button drag scrolling

  .main.scroll set $first $last
  .main.status_list yview moveto $first
  .main.tag_list yview moveto $first
}

proc workdir_scroll_status_list {first last} {
# support middle-button drag scrolling

  .main.scroll set $first $last
  .main.file_list yview moveto $first
  .main.tag_list yview moveto $first
}

proc workdir_scroll_tag_list {first last} {
# support middle-button drag scrolling

  .main.scroll set $first $last
  .main.file_list yview moveto $first
  .main.status_list yview moveto $first
}

proc workdir_scroll_scrollbar {args} {
# To support scrolling 3 listboxes simultaneously

  eval ".main.file_list     yview $args"
  eval ".main.status_list   yview $args"
  eval ".main.tag_list      yview $args"
}

proc workdir_cleanup {} {
  global cvscfg

  gen_log:log T "ENTER"
  set list [ split $cvscfg(clean_these) " " ]
  if { [ are_you_sure "You are about to delete" $list] == 1 } {
    foreach item $list {
      gen_log:log D "item $item"
      if { $item != "" } {
        set rmitem [glob -nocomplain $item]
        if {$rmitem != ""} {
          gen_log:log F "DELETE $rmitem"
          eval file delete -force -- $rmitem
        }
      } else {
        nop
      }
    }
    setup_dir
  }
  gen_log:log T "LEAVE"
}

proc workdir_delete_file {args} {
  global cvscfg

  gen_log:log T "ENTER ($args)"

  set filelist [join $args]
  if {$filelist == $cvscfg(thisdir)} {
    cvsfail "Please select some files to delete first!"
    return
  }

  if { [ are_you_sure "This will delete these files from your local, working directory:" $filelist ] == 1 } {
    gen_log:log F "DELETE $filelist"
    eval file delete -force -- $filelist
    setup_dir
  }
  gen_log:log T "LEAVE"
}

proc are_you_sure {mess args} {
#
# General posting message
#
  global cvscfg

  gen_log:log T "ENTER ($mess $args)"

  set filelist [join $args]  
  if { $cvscfg(confirm_prompt) != "false" } {
    append mess "\n"
    set indent "      "

    foreach item $filelist {
      if { $item != {} } {
        append mess " $indent"
        set val [ lindex $item 0 ]
        append mess " $val\n"
      }
    }
    append mess "\nAre you sure?"
    if {[cvsconfirm $mess] == 1} {
      cvsok "Aborted at user request."
      gen_log:log T "LEAVE 0"
      return 0
    }
  }
  gen_log:log T "LEAVE 1"
  return 1
}

# 
# Sets all cursors to busy, executes command, and restores cursors.
# 
# I believe I got this from GIC. Only some of the functions use it;
# was not immediately clear to me how to get all functions to use it, 
# however.
# -sj
#
proc busy {cmds} {
  #global errorInfo

  set busy {.app}
  set list [winfo children .]
  while {$list != ""} {
    set next {}
    foreach w $list {
      set cursor [lindex [$w config -cursor] 4]
      if {[winfo toplevel $w] == $w || $cursor != ""} {
        lappend busy [list $w $cursor]
      } else {
        lappend busy [list $w {}]
      }
      set next [concat $next [winfo children $w]]
    }
    set list $next
  }

  foreach w $busy {
    catch {[lindex $w 0] config -cursor watch}
  }

  update idletasks

  set error [catch {uplevel eval $cmds} result]
  #set ei $errorInfo

  foreach w $busy {
    catch {[lindex $w 0] config -cursor [lindex $w 1]}
  }

  if {$error} {
    #error $result $ei
  } else {
    return $result
  }
}

proc workdir_print_file {args} {
  global cvscfg

  set filelist [join $args]
  if {$filelist == $cvscfg(thisdir)} {
    cvsfail "Please select some files to print first!"
    return
  }

  set mess "This will print these files:\n\n"
  foreach file $filelist {
    append mess "   $file\n"
  }
  append mess "\nUsing $cvscfg(print_cmd)\n"
  append mess "\nAre you sure?"
  if {[cvsconfirm $mess] == 0} {
    set final_result ""
    foreach file $filelist {
      catch { eval exec $cvscfg(print_cmd) $file } file_result
      if { $file_result != "" } {
        set final_result "$final_result\n$file_result"
      }
    }
    if { $final_result != "" } {
      view_output "Print" $final_result
    }
    if { $cvscfg(auto_status) == "true"} {
      setup_dir
    }
  }
}

proc cvsroot_check {} {
  global cvscfg
  global incvs
  global env

  gen_log:log T "ENTER"
  set incvs 0
  if {[file isfile [file join . CVS Root]]} {
    set f [open [file join . CVS Root] r]
    gets $f root
    close $f
    set incvs 1
    set cvscfg(cvsroot) $root
  }
  gen_log:log T " cvsroot: $cvscfg(cvsroot)"
  if {[string match "*:*" $cvscfg(cvsroot)]} {
    set cvscfg(remote) 1
  } else {
    set cvscfg(remote) 0
  }
  gen_log:log T " remote=$cvscfg(remote)"
  gen_log:log T "LEAVE"
}

proc nop {} {}

proc disabled {} {
  cvsok "Command disabled."
}

proc isCmDirectory { file } {
  switch $file  {
    "CVS"  -
    "RCS"  - 
    "SCCS" { set value 1 }
    default { set value 0 } 
  }
  return $value
}

# Get the files in the current working directory.  Use the file_filter
# values Add hidden files if desired by the user.  Sort them to match
# the ordering that will be returned by cvs commands (this matches the
# default ls ordering.).
proc getFiles { } {
  global cvscfg

  set filelist ""
    
  # make sure the file filter is at least set to "*".
  if { $cvscfg(file_filter) == "" } {
    set cvscfg(file_filter) "*"
  }

  # get the initial file list, including hidden if requested
  if {$cvscfg(allfiles)} {
    # get hidden as well
    foreach item $cvscfg(file_filter) {
      catch { set filelist [ concat [ glob .$item $item ] $filelist ] }
    }
  } else {
    foreach item $cvscfg(file_filter) {
      catch { set filelist [ concat [ glob $item ] $filelist ] }
    }
  }

  # ignore files if requested
  if { $cvscfg(ignore_file_filter) != "" } {
    foreach item $cvscfg(ignore_file_filter) {
      # for each pattern
      if { $item != "*" } {
        # if not "*"
        while { [set idx [lsearch $filelist $item]] != -1 } {
          # for each occurence, delete
          catch { set filelist [ lreplace $filelist $idx $idx ] }
        }
      }
    }
  }

  # make sure "." is always in the list for 'cd' purposes
  if { ( [ lsearch -exact $filelist "." ] == -1 ) } {
    set filelist [ concat "." $filelist ]
  }
    
  # make sure ".." is always in the list for 'cd' purposes
  if { ( [ lsearch -exact $filelist ".." ] == -1 ) } {
    set filelist [ concat ".." $filelist ]
  }
    
  # sort it
  set filelist [ lsort $filelist ]
    
  # if this directory is under CVS and CVS is not in the list, add it. Its
  # presence is needed for later processing
  if { ( [ file exists "CVS" ] ) && 
       ( [ lsearch -exact $filelist "CVS" ] == -1 ) } {
    #puts "********* added CVS"
    catch { set filelist [ concat "CVS" $filelist ] }
  }
  #puts stdout "-------------\nfilelist=$filelist\n------------\n"
  set cvscfg(ignore_filter) $cvscfg(default_ignore_filter)
  return $filelist
}

proc feedback_cvs { e message } {
#
# This code is adapted from the text "Practical Programming in
# Tcl and Tk", by Brent B. Welch (see page 209)
# An entry widget is used because it won't change size
# base on the message length, and it can be scrolled by
# dragging with button 2.
# Author: Eugene Lee, Aerospace Corporation, 9/6/95
#
  global feedback
  global cvscfg

  $e config -state normal
  $e delete 0 end
  $e insert 0 $message
  # Leave the entry in a read-only state
  $e config -state disabled

  # Force a disable update
  update idletasks
}

proc log_toggle { } {
  global cvscfg

  if {$cvscfg(logging)} {
    gen_log:init
  } else {
    gen_log:quit
  }
}

proc exit_cleanup { } {
  global cvscfg
  
  set pid [pid]
  gen_log:log F "DELETE $cvscfg(tmpdir)/cvstmpdir.$pid"
  catch [file delete -force [file join $cvscfg(tmpdir) cvstmpdir.$pid]]
  destroy .
  exit
}

