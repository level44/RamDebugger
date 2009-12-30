#
#Tcl library for TkCVS
#

#
# $Id: filebrowse.tcl,v 1.1.1.1 2001-06-07 15:03:42 ramsan Exp $
#
# Sets up a dialog to browse the contents of a module.
#

proc browse_files {mcode} {
  global filenames
  global mtitle
  global checkout_version
  global cvscfg

  gen_log:log T "ENTER ($mcode)"
  static {browser 0}

  if {$mcode == ""} {
    cvsfail "Please select a module!"
    return
  }
  if {! [info exists mtitle($mcode)]} {
    cvsfail "Sorry, this function only works on modules, not directories!"
    return
  }

  # Find the list of file names.
  find_filenames $mcode

  if {! [info exists filenames($mcode)]} {
    cvsfail "There are no files in this module!"
    return
  }

  #
  # Create the browser window.
  #

  incr browser
  set filebrowse ".filebrowse$browser"
  toplevel $filebrowse
  frame $filebrowse.up   -relief groove -border 2
  frame $filebrowse.up.left
  frame $filebrowse.up.right
  frame $filebrowse.down -relief groove -border 2

  pack $filebrowse.up -side top -fill x
  pack $filebrowse.up.left -side left -fill both
  pack $filebrowse.up.right -side left -fill both -expand 1
  pack $filebrowse.down -side bottom -fill x

  label $filebrowse.lver1 -text "Version / Tag " \
     -anchor w -font $cvscfg(guifont)
  # label $filebrowse.lver2 -anchor w -text "Version / Tag 2 (diff)"

  entry $filebrowse.tver1 -relief sunken -textvariable checkout_version
  # bind_motifentry $filebrowse.tver1
  # entry $filebrowse.tver2 -relief sunken
  # bind_motifentry $filebrowse.tver2

  pack $filebrowse.lver1 \
    -in $filebrowse.up.left -side top -fill x
  pack $filebrowse.tver1 \
    -in $filebrowse.up.right -side top -fill x

  #
  # Create buttons
  #
  button $filebrowse.help -text "Help" -font $cvscfg(guifont) \
    -command file_browser
  button $filebrowse.view -image Fileview \
    -command "fileview $filebrowse $mcode"
  button $filebrowse.log -image Branches \
    -command "filelog $filebrowse.list $mcode"
  button $filebrowse.tag -image Tags \
    -command "tagview $filebrowse $mcode"
  # button $filebrowse.diff -text "Diff" \
  #   -command "filediff $filebrowse $mcode"
  button $filebrowse.quit -text "Close" -font $cvscfg(guifont) \
    -command "destroy $filebrowse"

  pack $filebrowse.help \
       $filebrowse.view \
       $filebrowse.log \
       $filebrowse.tag \
    -in $filebrowse.down -side left -ipadx 1 -ipady 1 -fill x -expand 1
  pack $filebrowse.quit \
    -in $filebrowse.down -side left -ipadx 1 -ipady 1 -fill x -expand 1

  set_tooltips $filebrowse.view \
    {"View the selected file"}
  set_tooltips $filebrowse.log \
    {"See the revision log and branches of the selected file"}
  set_tooltips $filebrowse.tag \
    {"List the tags of the selected file"}

  #
  # Create a scrollbar and a list box.
  #
  scrollbar $filebrowse.scroll -relief sunken \
    -command "$filebrowse.list yview"
  listbox $filebrowse.list \
    -yscroll "$filebrowse.scroll set" -relief sunken \
    -font $cvscfg(listboxfont) \
    -width 40 -height 25 -setgrid yes
  pack $filebrowse.scroll -side right -fill y
  pack $filebrowse.list -side left -fill both -expand 1

  #
  # Window manager stuff.
  #
  wm title $filebrowse "Files in $mcode"
  wm minsize $filebrowse 5 5

  #
  # Fill the list.
  #
  foreach file $filenames($mcode) {
    $filebrowse.list insert end $file
  }
  gen_log:log T "LEAVE"
}

proc filelog {listname mcode} {

  gen_log:log T "ENTER ($listname $mcode)"
  foreach item [$listname curselection] {
    cvs_filelog $mcode [$listname get $item]
  }
  gen_log:log T "LEAVE"
}

proc fileview {toplevelname mcode} {

  gen_log:log T "ENTER ($toplevelname $mcode)"
  set listname $toplevelname.list
  foreach item [$listname curselection] {
    cvs_fileview $mcode [$listname get $item] [$toplevelname.tver1 get]
  }
  gen_log:log T "LEAVE"
}

proc tagview {toplevelname mcode} {

  gen_log:log T "ENTER ($toplevelname $mcode)"
  set listname $toplevelname.list
  foreach item [$listname curselection] {
    filebrowse_tagview $mcode [$listname get $item]
  }
  gen_log:log T "LEAVE"
}

proc filebrowse_tagview {mcode filename} {

  gen_log:log T "ENTER ($mcode $filename)"
  view_output "$filename Tags" [cvs_gettaglist $mcode $filename]
  gen_log:log T "LEAVE"
}

