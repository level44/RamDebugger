#
# Tcl Library for TkCVS
#

#
# $Id: search.tcl,v 1.1.1.1 2001-06-07 15:03:42 ramsan Exp $
#
# Set up some search dialogs.
#

proc search_setup {} {

  gen_log:log T "ENTER"
  toplevel .search1
  frame .search1.left
  frame .search1.right
  frame .search1.down -relief groove -border 2

  pack .search1.down -side bottom -fill x
  pack .search1.left -side left -fill y
  pack .search1.right -side left -fill both -expand 1

  label .search1.lname -text "Search for Code" -anchor w
  entry .search1.tname -relief sunken -textvariable search1name
  pack .search1.lname -in .search1.left \
    -side top -fill x -pady 3
  pack .search1.tname -in .search1.right \
    -side top -fill x -pady 3

  button .search1.ok -text "OK" \
    -command {
      eval "run_search1 $search1name"
      wm withdraw .search1
    }
  button .search1.quit -text "Quit" -command { wm withdraw .search1 }
  pack .search1.ok .search1.quit -in .search1.down -side left \
    -ipadx 2 -ipady 2 -padx 4 -pady 4 -fill both -expand 1
 
  wm withdraw .search1
  wm title .search1 "Search for Module Code"
  wm minsize .search1 1 1

  toplevel .search2
  frame .search2.left
  frame .search2.right
  frame .search2.down -relief groove -border 2

  pack .search2.down -side bottom -fill x
  pack .search2.left -side left -fill y
  pack .search2.right -side left -fill both -expand 1

  label .search2.lname -text "Search for Name" -anchor w
  entry .search2.tname -relief sunken -textvariable search2name
  pack .search2.lname -in .search2.left \
    -side top -fill x -pady 3
  pack .search2.tname -in .search2.right \
    -side top -fill x -pady 3

  button .search2.ok -text "OK" \
    -command {
      eval "run_search2 $search2name"
      wm withdraw .search2
    }
  button .search2.quit -text "Quit" -command { wm withdraw .search2 }
 
  pack .search2.ok .search2.quit -in .search2.down -side left \
    -ipadx 2 -ipady 2 -padx 4 -pady 4 -fill both -expand 1
 
  wm withdraw .search2
  wm title .search2 "Search for Module Name"
  wm minsize .search2 1 1

  toplevel .search3
  frame .search3.left
  frame .search3.right
  frame .search3.down -relief groove -border 2

  pack .search3.down -side bottom -fill x
  pack .search3.left -side left -fill y
  pack .search3.right -side left -fill both -expand 1

  label .search3.lname -text "Search for Keyword" -anchor w
  entry .search3.tname -relief sunken -textvariable search3name
  pack .search3.lname -in .search3.left \
    -side top -fill x -pady 3
  pack .search3.tname -in .search3.right \
    -side top -fill x -pady 3

  button .search3.ok -text "OK" \
    -command {
      eval "run_search3 $search3name"
      wm withdraw .search3
    }
  button .search3.quit -text "Quit" -command { wm withdraw .search3 }
 
  pack .search3.ok .search3.quit -in .search3.down -side left \
    -ipadx 2 -ipady 2 -padx 4 -pady 4 -fill both -expand 1
 
  wm withdraw .search3
  wm title .search3 "Search for Keyword"
  wm minsize .search3 1 1

  if {! [winfo exists .viewer]} {
     viewer_setup
  }
  gen_log:log T "LEAVE"
}

proc code_search {} {
  global dtitle
  global mtitle

  gen_log:log T "ENTER"
  if {! [winfo exists .search1]} {
    search_setup
  }
  gen_log:log D "[winfo exists .search1]"

  if {! [info exists dtitle]} {
    cvsfail "You do not have any #D lines in your modules file."
    return
  }
  if {! [info exists mtitle]} {
    cvsfail "You do not have any #M lines in your modules file."
    return
  }

  wm deiconify .search1
  raise .search1
  gen_log:log T "LEAVE"
}

proc name_search {} {
  global dtitle
  global mtitle

  gen_log:log T "ENTER"
  if {! [winfo exists .search2]} {
    search_setup
  }

  if {! [info exists dtitle]} {
    cvsfail "You do not have any #D lines in your modules file."
    return
  }
  if {! [info exists mtitle]} {
    cvsfail "You do not have any #M lines in your modules file."
    return
  }

  wm deiconify .search2
  raise .search2
  gen_log:log T "LEAVE"
}

proc keyword_search {} {
  global dtitle
  global mtitle

  gen_log:log T "ENTER"
  if {! [winfo exists .search3]} {
    search_setup
  }

  if {! [info exists dtitle]} {
    cvsfail "You do not have any #D lines in your modules file."
    return
  }
  if {! [info exists mtitle]} {
    cvsfail "You do not have any #M lines in your modules file."
    return
  }

  wm deiconify .search3
  raise .search3
  gen_log:log T "LEAVE"
}

proc run_search1 {code} {

  global mtitle
  global dcontents

  gen_log:log T "ENTER ($code)"
  if {[info exists mtitle($code)]} {
    .viewer.text configure -state normal
    .viewer.text delete 1.0 end
    foreach subdir [array names dcontents] {
      if {[lsearch -exact $dcontents($subdir) $code] != -1} {
        .viewer.text insert end "Parent Directory :  $subdir\n"
      }
    }
    .viewer.text insert end "Module Code      :  $code\n"
    .viewer.text insert end "Module Name      :  $mtitle($code)\n"
    .viewer.text configure -state disabled
    wm deiconify .viewer
    raise .viewer
  } else {
    cvsok "Module $code not found."
  }
}

proc run_search2 {name} {
  global mtitle
  global dcontents

  gen_log:log T "ENTER ($name)"
  .viewer.text configure -state normal
  .viewer.text delete 1.0 end
  set counter 0

  set searchstring [string tolower $name]
  foreach code [array names mtitle] {
    if {[string match "$searchstring" [string tolower $mtitle($code)]]} {
      foreach subdir [array names dcontents] {
        if {[lsearch -exact $dcontents($subdir) $code] != -1} {
          .viewer.text insert end "Parent Directory :  $subdir\n"
        }
      }
      .viewer.text insert end "Module Code      :  $code\n"
      .viewer.text insert end "Module Name      :  $mtitle($code)\n\n"
      set counter 1
    }
  }

  if {$counter} {
    .viewer.text configure -state disabled
    wm deiconify .viewer
    raise .viewer
  } else {
    cvsok "Module $name not found."
  }
}

proc run_search3 {name} {
  global mtitle
  global dcontents

  gen_log:log T "ENTER ($name)"
  .viewer.text configure -state normal
  .viewer.text delete 1.0 end
  set counter 0

  foreach code [array names mtitle] {
    set seq_match_code [string first [string tolower $name] [string tolower $code]]
    set seq_match_title [string first [string tolower $name] [string tolower $mtitle($code)]]
    if {$seq_match_code != -1 || $seq_match_title != -1} {
      foreach subdir [array names dcontents] {
        if {[lsearch -exact $dcontents($subdir) $code] != -1} {
          .viewer.text insert end "Parent Directory :  $subdir\n"
        }
      }
      .viewer.text insert end "Module Code      :  $code\n"
      .viewer.text insert end "Module Name      :  $mtitle($code)\n\n"
      set counter 1
    }
  }

  if {$counter} {
    .viewer.text configure -state disabled
    wm deiconify .viewer
    raise .viewer
  } else {
    cvsok "Module containing $name not found."
  }
}
