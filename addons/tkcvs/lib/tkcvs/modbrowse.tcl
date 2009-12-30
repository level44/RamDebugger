# Tcl Library for TkCVS
#

#
# $Id: modbrowse.tcl,v 1.1.1.1 2001-06-07 15:03:42 ramsan Exp $
#
# Set up a check out dialog.
#

proc modbrowse_setup {} {
  global modbrowse_mcode
  global modbrowse_version
  global cvscfg
  global feedback

  gen_log:log T "ENTER"
  set modbrowse_version "HEAD"

  # Window manager stuff.
  toplevel .modbrowse
  wm title .modbrowse "Module Browser"
  wm iconname .modbrowse "CVS Modules"
  wm iconbitmap .modbrowse @$cvscfg(bitmapdir)/tkcvs48.xbm

  modbrowse_menus

  #
  # Top section - module, tags, root
  #
  frame .modbrowse.top -relief groove -border 2
  frame .modbrowse.top.mod
  frame .modbrowse.top.taga
  frame .modbrowse.top.tagb
  frame .modbrowse.top.root
  pack .modbrowse.top -side top -fill x

  pack .modbrowse.top.mod -side top -fill x -expand yes
  label .modbrowse.top.mod.lmcode -text "Module" \
     -anchor w -width 10 -font $cvscfg(guifont)
  entry .modbrowse.top.mod.tmcode -textvariable modbrowse_mcode
  pack .modbrowse.top.mod.lmcode -side left -fill x
  pack .modbrowse.top.mod.tmcode -side left -fill x -expand yes

  pack .modbrowse.top.taga -side top -fill x -expand yes
  label .modbrowse.top.taga.ltaga -text "Tag A" \
     -anchor w -width 10 -font $cvscfg(guifont)
  entry .modbrowse.top.taga.ttaga -textvariable modbrowse_version
  pack .modbrowse.top.taga.ltaga -side left -fill x
  pack .modbrowse.top.taga.ttaga -side left -fill x -expand yes

  pack .modbrowse.top.tagb -side top -fill x -expand yes
  label .modbrowse.top.tagb.ltagb -text "Tag B" \
     -anchor w -width 10 -font $cvscfg(guifont)
  entry .modbrowse.top.tagb.ttagb -textvariable modbrowse_version2
  pack .modbrowse.top.tagb.ltagb -side left -fill x
  pack .modbrowse.top.tagb.ttagb -side left -fill x -expand yes

  pack  .modbrowse.top.root -side top -fill x -expand yes
  label .modbrowse.top.root.lroot -text "CVSROOT" \
     -anchor w -width 10 -font $cvscfg(guifont)
  entry .modbrowse.top.root.troot -textvariable cvscfg(cvsroot)
  pack .modbrowse.top.root.lroot -side left -fill x
  pack .modbrowse.top.root.troot -side left -fill x -expand yes

  bind .modbrowse.top.root.troot <KeyPress-Return> { \
     destroy .modbrowse.treeframe.tree; \
     destroy .modbrowse.treeframe.col; \
     destroy .modbrowse.treeframe.yscroll; \
     modbrowse_run $cvscfg(cvsroot) \
  }

  # Make sure we have a color for the highlight rectangle in the tree canvas
  if {![info exists cvscfg(glb_highlight)]} {
    set cvscfg(glb_highlight) [.modbrowse.top.mod.tmcode cget -selectbackground]
  }

  # Pack the bottom before the middle so it doesnt disappear if
  # the window is resized smaller
  frame .modbrowse.bottom -relief groove -border 2 -height 128
  frame .modbrowse.bottom.buttons
  frame .modbrowse.bottom.buttons.cvsfuncs -relief groove -bd 2
  frame .modbrowse.bottom.buttons.modfuncs -relief groove -bd 2
  #frame .modbrowse.bottom.buttons.dirfuncs -relief groove -bd 2

  pack .modbrowse.bottom -side bottom -fill x
  pack .modbrowse.bottom.buttons -side top -fill x -expand yes
  pack .modbrowse.bottom.buttons.cvsfuncs -side left -fill x -expand yes
  pack .modbrowse.bottom.buttons.modfuncs -side left -fill x -expand yes
  #pack .modbrowse.bottom.buttons.dirfuncs -side left -fill x -expand yes

  #
  # The main part - where the modules are listed
  #
  frame .modbrowse.treeframe
  pack .modbrowse.treeframe -side bottom -fill both -expand 1 -fill both

  #
  # Create buttons
  #
  button .modbrowse.filebrowse -image Files \
    -command { browse_files $modbrowse_mcode }
  button .modbrowse.patchsummary -image Patches \
    -command {
       cvs_patch_summary $modbrowse_mcode \
                         $modbrowse_version \
                         $modbrowse_version2
    }
  button .modbrowse.patchfile -image Patchfile \
    -command {
       cvs_patch $modbrowse_mcode $modbrowse_version $modbrowse_version2
    }
  button .modbrowse.clear -image Clear \
    -command {
      set Tree(.modbrowse.treeframe:selection) {}
      ModTree:drawselection .modbrowse.treeframe
      set modbrowse_mcode ""
    }
  button .modbrowse.checkout -image Checkout \
    -command { 
       cvsroot_check
       cvs_checkout $modbrowse_mcode $modbrowse_version 
    }
  button .modbrowse.export -image Export \
    -command {
       cvsroot_check
       cvs_export $modbrowse_mcode $modbrowse_version
    }
  button .modbrowse.tag -image Tag \
    -command {
       cvs_rtag $modbrowse_mcode "no" $modbrowse_version $modbrowse_version2
    }
  button .modbrowse.branchtag -image Branchtag \
    -command {
       cvs_rtag $modbrowse_mcode "yes" $modbrowse_version $modbrowse_version2
    }
  button .modbrowse.import -image Import \
     -command import_run
  button .modbrowse.who -image Who \
     -command {cvs_history all $modbrowse_mcode}

  button .modbrowse.close -text "Close" -font $cvscfg(guifont) \
    -command {
       destroy .modbrowse
       catch {destroy .tooltips_wind}
       module_exit
    }

  pack .modbrowse.who \
       .modbrowse.filebrowse \
       .modbrowse.clear \
       .modbrowse.checkout  \
       .modbrowse.export  \
    -in .modbrowse.bottom.buttons.cvsfuncs \
    -side left -ipadx 1 -ipady 1 -fill x -expand 1
  pack .modbrowse.tag  \
       .modbrowse.branchtag  \
       .modbrowse.patchsummary \
       .modbrowse.patchfile \
       .modbrowse.import \
    -in .modbrowse.bottom.buttons.modfuncs \
    -side left -ipadx 1 -ipady 1 -fill x -expand 1
  pack .modbrowse.close \
    -in .modbrowse.bottom.buttons.modfuncs \
    -side right -ipadx 1 -ipady 1 -fill x -expand 1

  set_tooltips .modbrowse.checkout \
     {"Check out a module from the repository"}
  set_tooltips .modbrowse.export \
     {"Export a module from the repository"}
  set_tooltips .modbrowse.tag \
     {"Tag all files in a module"}
  set_tooltips .modbrowse.branchtag \
     {"Branch all files in a module"}
  set_tooltips .modbrowse.filebrowse \
     {"Browse the files in a module"}
  set_tooltips .modbrowse.patchsummary \
     {"Show a summary of differences between versions"}
  set_tooltips .modbrowse.patchfile \
     {"Create a patch file"}
  set_tooltips .modbrowse.clear \
     {"Unselect all modules"}
  set_tooltips .modbrowse.import \
     {"Import the current directory into the repository"}
  set_tooltips .modbrowse.who \
     {"Show who has modules checked out"}
  set_tooltips .modbrowse.close \
     {"Close the module browser"}

  set feedback(mod) [entry .modbrowse.bottom.feedback -width 55 -state disabled]
  pack .modbrowse.bottom.feedback -side bottom -fill x -expand yes

  if {$cvscfg(remote)} {
    .modbrowse.menubar.file.m  entryconfigure "Create*" -state disabled
  }
 
  set screenWidth [winfo vrootwidth .]
  set screenHeight [winfo vrootheight .]

  wm maxsize .modbrowse $screenWidth $screenHeight
  wm minsize .modbrowse 10 10

  gen_log:log T "LEAVE"
}

proc modbrowse_menus {} {
  global cvscfg
  global cvs

  gen_log:log T "ENTER"

  frame .modbrowse.menubar -relief raised -bd 2
  pack .modbrowse.menubar -side top -fill x

  #
  # Create the Menu bar
  #
  menubutton .modbrowse.menubar.file -text "File" -underline 0 \
    -menu .modbrowse.menubar.file.m -font $cvscfg(guifont)
  menubutton .modbrowse.menubar.reports -text "Reports" -underline 0 \
    -menu .modbrowse.menubar.reports.m -font $cvscfg(guifont)
  menubutton .modbrowse.menubar.checkouts -text "Checkouts" -underline 0 \
    -menu .modbrowse.menubar.checkouts.m -font $cvscfg(guifont)
  menubutton .modbrowse.menubar.patch -text "Patch" -underline 0 \
    -menu .modbrowse.menubar.patch.m -font $cvscfg(guifont)
  menubutton .modbrowse.menubar.tag -text "Tag" -underline 0 \
    -menu .modbrowse.menubar.tag.m -font $cvscfg(guifont)
  menubutton .modbrowse.menubar.help -text "Help" -underline 0 \
    -menu .modbrowse.menubar.help.m -font $cvscfg(guifont)
  pack .modbrowse.menubar.file \
       .modbrowse.menubar.reports \
       .modbrowse.menubar.checkouts \
       .modbrowse.menubar.patch\
       .modbrowse.menubar.tag \
    -side left
  pack .modbrowse.menubar.help -side right

  #
  # Create menus
  #
  menu .modbrowse.menubar.file.m -font $cvscfg(guifont)
  .modbrowse.menubar.file.m add command -label "Import" \
     -underline 0 -command import_run
  .modbrowse.menubar.file.m add command -label "Create Dir" \
     -underline 7 -command inewdir_run
  .modbrowse.menubar.file.m add separator
  .modbrowse.menubar.file.m add command -label "Module Level Merge" -underline 0 \
     -command {merge_run $modbrowse_mcode}

  menu .modbrowse.menubar.reports.m -font $cvscfg(guifont)
  .modbrowse.menubar.reports.m add command -label "Module Tree" \
     -underline 0 \
     -command {modlist_by_code $modbrowse_mcode 0 $modbrowse_version}
  .modbrowse.menubar.reports.m add command -label "Modules Sorted by Name" \
     -command {modlist_by_name $modbrowse_mcode 0 $modbrowse_version}
  .modbrowse.menubar.reports.m add command -label "Version Tree" \
     -underline 0 \
     -command {modlist_by_code $modbrowse_mcode 1 $modbrowse_version}
  .modbrowse.menubar.reports.m add command -label "Version Listing by Name"\
     -command {modlist_by_name $modbrowse_mcode 1 $modbrowse_version}
  .modbrowse.menubar.reports.m add separator
  .modbrowse.menubar.reports.m add command \
     -label "Search Repository by Code" -underline 21 \
     -command code_search
  .modbrowse.menubar.reports.m add command \
     -label "Search Repository by Name" -underline 21 \
     -command name_search
  .modbrowse.menubar.reports.m add command \
     -label "Search Repository by Keyword" -underline 21 \
     -command keyword_search

  menu .modbrowse.menubar.checkouts.m -font $cvscfg(guifont)
  .modbrowse.menubar.checkouts.m add command -label "My Checkouts" \
     -underline 0 -command {cvs_history me ""}
  .modbrowse.menubar.checkouts.m add command -label "Checkouts of Selected Module" \
     -underline 0 -command {cvs_history all $modbrowse_mcode}
  .modbrowse.menubar.checkouts.m add command -label "All Checkouts" \
     -underline 0 -command {cvs_history all ""}

  menu .modbrowse.menubar.patch.m -font $cvscfg(guifont)
  .modbrowse.menubar.patch.m add command -label "Make Patch File" -underline 0 \
     -command {
      cvs_patch $modbrowse_mcode $modbrowse_version $modbrowse_version2
    }
  .modbrowse.menubar.patch.m add command -label "View Patch Summary" -underline 0 \
     -command {
      cvs_patch_summary $modbrowse_mcode $modbrowse_version $modbrowse_version2
    }

  menu .modbrowse.menubar.tag.m -font $cvscfg(guifont)
  .modbrowse.menubar.tag.m add command -label "Tag Module" -underline 0 \
     -command {
      cvs_rtag $modbrowse_mcode "no" $modbrowse_version $modbrowse_version2
    }
  .modbrowse.menubar.tag.m add command -label "Branch Tag Module" -underline 0 \
     -command {
      cvs_rtag $modbrowse_mcode "yes" $modbrowse_version $modbrowse_version2
    }

  menu .modbrowse.menubar.help.m -font $cvscfg(guifont)
  .modbrowse.menubar.help.m add command -label "Module Browser Overview" \
     -command module_browser
  .modbrowse.menubar.help.m add separator
  .modbrowse.menubar.help.m add command -label "Checking out Modules" \
     -command checking_out_modules
  .modbrowse.menubar.help.m add command -label "Exporting" \
     -command exporting
  .modbrowse.menubar.help.m add command -label "Tagging and Branching" \
     -command tagging_and_branching
  .modbrowse.menubar.help.m add separator
  .modbrowse.menubar.help.m add command -label "Menu" \
     -command module_browser_menu
  .modbrowse.menubar.help.m add command -label "Buttons" \
     -command module_browser_buttons
  .modbrowse.menubar.help.m add separator
  .modbrowse.menubar.help.m add command -label "File Browser" \
     -command file_browser
  .modbrowse.menubar.help.m add command -label "Log Browser" \
     -command log_browser
  .modbrowse.menubar.help.m add separator
  .modbrowse.menubar.help.m add command -label "CVS modules File" \
     -command cvs_modules_file

  gen_log:log T "LEAVE"
}

proc modbrowse_run {root} {
  global modbrowse_mcode
  global dtitle
  global Tree

  gen_log:log T "ENTER ($root)"

  set modbrowse_mcode ""
  read_modules_setup $root

  if {! [winfo exists .modbrowse]} {
    modbrowse_setup
  }

  if {[winfo exists .modbrowse.treeframe.tree]} {
    ModTree:delitem .modbrowse.treeframe /
  }
  ModTree:create .modbrowse.treeframe \
      -relief sunken -bd 2 \
      -width 200 -height 300

  .modbrowse.treeframe.tree bind x <1> {
    set lbl [ModTree:labelat %W %x %y]
    if { $lbl == "" } {
      return
    }
    ModTree:setselection %W $lbl
    set modbrowse_mcode $Tree(.modbrowse.treeframe:$lbl:name)
  }
  .modbrowse.treeframe.tree bind x <Double-1> {
    ModTree:open %W [ModTree:labelat %W %x %y]
  }

  # Populate the tree
  modbrowse_tree [array names dtitle] "/" 0

  wm deiconify .modbrowse
  raise .modbrowse
  gen_log:log T "LEAVE"
}

proc modbrowse_tree { dnames node level } {
#
# Do this to update the display of the listbox (body proc).
#
  global mtitle
  global dtitle
  global dcontents
  global dsubmenus
  global Tree

  gen_log:log T "ENTER ($dnames $node $level)"

  # Sort to make parent directories come before children
  set dnames [lsort $dnames]
  foreach dname $dnames {
    gen_log:log T "$dname"
    set has_contents 0
    set dimage dir
    if {[info exists dcontents($dname)]} {
      #gen_log:log D " dcontents($dname): $dcontents($dname)"
      set has_contents 1
      set dimage dir
    }
    set title ""
    if {[info exists dtitle($dname)]} {
      set title $dtitle($dname)
    }
    if {[info exists mtitle($dname)]} {
      #gen_log:log D " mtitle($dname): $mtitle($dname)"
      set title $mtitle($dname)
      set dimage mod
      if {$node == "/aliases"} {
         set dimage amod
      }
      if {[info exists dcontents($dname)]} {
         set has_contents 1
         set dimage mdir
      }
    }
    if {$dname == "aliases"} {
      set dimage adir
    }
    set treepath [file join $node $dname]
    regsub -all {//} $treepath {/} treepath
    gen_log:log D " $treepath:  $dimage (has_contents=$has_contents)"
    set root [file dirname $treepath]
    set tail [file tail $treepath]
    gen_log:log D " ROOT $root node=$node treepath=$treepath"
    if {! [info exists Tree(.modbrowse.treeframe:$root:tags)]} {
      set root [file dirname $root]
      gen_log:log D "   ROOT is now $root"
      set treepath [file join $root $tail]
      gen_log:log D "  ROOT is now $root TREEPATH is now $treepath"
    }
    ModTree:newitem .modbrowse.treeframe \
       $treepath $dname $title -image $dimage

    if {$has_contents} {
      incr level
      modbrowse_tree $dcontents($dname) $treepath $level
    }
  }
  update
  gen_log:log T "LEAVE"
}

proc modbrowse_select_code {yposition} {
#
# Do this when a code is clicked on.
#
  global modbrowse_ypos
  global modbrowse_mcode

  set modbrowse_ypos $yposition

  selection clear
  # This does the actual selection
  .modbrowse.codelist select set \
    [.modbrowse.codelist nearest $yposition]
  set code [selection get]

  # This will update the "Module Name" entry box.
  set modbrowse_mcode [lindex $code 0]

  return $code
}

proc module_exit { } {
  global cvscfg
  global cvs

  gen_log:log T "ENTER"
  set pid [pid]
  set cwd [pwd]
  set sandbox [file join $cvscfg(tmpdir) cvstmpdir.$pid]
  if {[file isdirectory $sandbox]} {
    gen_log:log F "CD $sandbox"
    cd $sandbox
    foreach d [glob -nocomplain *] {
      gen_log:log C "$cvs -Q release $d"
      catch {eval exec $cvs -Q release $d}
    }
  }
  cd $cwd
  gen_log:log F "cd [pwd]"
  gen_log:log T "LEAVE"
}
