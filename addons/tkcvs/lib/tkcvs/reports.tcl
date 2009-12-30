#
# TCL Library for TkCVS
#

#
# $Id: reports.tcl,v 1.2 2006-06-30 17:40:16 ramsan Exp $
#
# Procedures for CVS reports.
#

proc reports_setup {} {
#
# This sets up a dialog to determine whether a report should
# be put on the screen, printed, or saved to a file.
#
  global printer_name
  global file_name
  global sorp
  global sorp_button

  gen_log:log T "ENTER"
  toplevel .sorp
  frame .sorp.left
  frame .sorp.right
  frame .sorp.down -relief groove -border 2

  pack .sorp.down -side bottom -fill x
  pack .sorp.left -side left -fill y
  pack .sorp.right -side left -fill both -expand 1

  set sorp "Screen"
  radiobutton .sorp.rprinter -text "Printer" \
    -variable sorp -value "Printer" -anchor w
  radiobutton .sorp.rfile -text "File" \
    -variable sorp -value "File" -anchor w
  radiobutton .sorp.rscreen -text "Screen" \
    -variable sorp -value "Screen" -anchor w

  entry .sorp.tprinter -relief sunken -textvariable printer_name
  entry .sorp.tfile -relief sunken -textvariable file_name

  pack .sorp.rprinter .sorp.rfile .sorp.rscreen -in .sorp.left \
    -side top -anchor w -fill x -pady 2
  pack .sorp.tprinter .sorp.tfile -in .sorp.right \
    -side top -fill x -pady 3
 
  button .sorp.ok -text "OK" -command { set sorp_button 1 }
  button .sorp.quit -text "Quit" -command { set sorp_button 0 }
 
  pack .sorp.ok .sorp.quit -in .sorp.down -side left \
    -ipadx 2 -ipady 2 -padx 4 -pady 4 -fill both -expand 1
 
  wm withdraw .sorp
  wm title .sorp "Select Report Destination"

  if {! [winfo exists .viewer]} {
     viewer_setup
  }

  gen_log:log T "LEAVE"
}

proc viewer_setup { } {
  #
  # Set up a dialog containing a text box that can be used to view
  # the report on the screen.
  #
  gen_log:log T "ENTER"

  toplevel .viewer
  text .viewer.text -setgrid yes -yscroll {.viewer.scroll set} \
    -relief sunken -border 2
  scrollbar .viewer.scroll -command {.viewer.text yview} -relief sunken
  button .viewer.ok -text "OK" -command { wm withdraw .viewer }

  pack .viewer.ok -side bottom -fill x
  pack .viewer.scroll -side right -fill y
  pack .viewer.text -fill both -expand 1

  # RAMSAN
  bind .viewer.text <1> "focus .viewer.text"

  wm withdraw .viewer
  wm title .viewer "Report"

  gen_log:log T "LEAVE"
}

proc screen_or_printer_run {} {
  global sorp_button

  gen_log:log T "ENTER"
  set sorp_button 2
  if {! [winfo exists .sorp]} {
     reports_setup 
  }
  wm deiconify .sorp
  raise .sorp
  
  set oldFocus [focus]
  grab set .sorp
  focus .sorp

  tkwait variable sorp_button
  wm withdraw .sorp
  focus $oldFocus
  grab release .sorp

  gen_log:log T "LEAVE"
  return $sorp_button
}

proc modlist_by_name {dcode versions tagname} {
#
# This produces a module listing by module name.
#
  global sorp
  global dtitle
  global mtitle

  gen_log:log T "ENTER ($dcode $versions $tagname)"
  if {! [info exists dtitle]} {
    cvsfail "You do not have any #D lines in your modules file."
    return
  }
  if {! [info exists mtitle]} {
    cvsfail "You do not have any #M lines in your modules file."
    return
  }

  if {$dcode == ""} {
    set dcode "."
  }
  #puts "modlist_by_name: $dcode"
  if {[screen_or_printer_run]} {
    if {$tagname == ""} {
      modlist_by_name_run $dcode $versions "HEAD"
    } else {
      modlist_by_name_run $dcode $versions $tagname
    }
  }
  gen_log:log T "LEAVE"
}

proc modlist_by_code {dcode versions tagname} {
#
# This produces a module listing by module code.
# Set versions to 1 to give the file version numbers.
#
  global sorp
  global dtitle
  global mtitle

  gen_log:log T "ENTER ($dcode $versions $tagname)"
  if {! [info exists dtitle]} {
    cvsfail "You do not have any #D lines in your modules file."
    return
  }
  if {! [info exists mtitle]} {
    cvsfail "You do not have any #M lines in your modules file."
    return
  }

  if {$dcode == ""} {
    set dcode "."
  }
  #puts "modlist_by_code: $dcode"
  if {[screen_or_printer_run]} {
    if {$tagname == ""} {
      modlist_by_code_run $dcode $versions "HEAD"
    } else {
      modlist_by_code_run $dcode $versions $tagname
    }
  }
  gen_log:log T "LEAVE"
}

proc modlist_by_name_run {dcode versions tagname} {
#
# Called by  Modules Sorted by Name
#       and  Version Listing by Name
#
  global dtitle
  global mtitle
  global dcontents
  global location
  global sorp
  global file_name
  global printer_name
  global cvscfg
  global feedback
  global modlist_by_title

  gen_log:log T "ENTER ($dcode $versions $tagname)"
  feedback_cvs $feedback(cvs) "Generating report, please wait"

  # Initialize the output
  if {$sorp == "Screen"} {
    set linenum 1
    .viewer.text configure -state normal
    .viewer.text delete 1.0 end
  } elseif {$sorp == "Printer"} {
    gen_log:log F "OPEN tkcvs.tmp"
    set outfile [open "tkcvs.tmp" w]
    set cvscfg(ycurrent) $cvscfg(ystart)
    set cvscfg(pagenum) 1
    postscript_setup $outfile
  } else {
    gen_log:log F "OPEN $file_name"
    set outfile [open $file_name w]
  }

  # If dcode is "." then report the entire tree.  Otherwise restrict
  # the report to a portion of the tree.

  if {$dcode == "."} {
    foreach item $modlist_by_title {
      set items [split $item "\t"]
      if {$sorp == "Screen"} {
        set printme [format "%-55s %s" [lindex $items 0] [lindex $items 1]]
        .viewer.text insert end "$printme\n"
      } elseif {$sorp == "Printer"} {
        postscript_line $outfile [lindex $items 0] [lindex $items 1]
      } else {
        set printme [format "%-55s %s" [lindex $items 0] [lindex $items 1]]
        puts $outfile "$printme"
      }
      if {$versions} {
        report_versions [lindex $items 1] $tagname
      }
    }
  } else {
    foreach item $modlist_by_title {
      set items [split $item "\t"]
      set code [lindex $items 1]

      if {[info exists dcontents($code)]} {
        #puts "dcontents: $dcontents($code)"
      }
      if {[info exists dsubmenus($code)]} {
        #puts "dsubmenus: $dsubmenus($code)"
      }
      if {[info exists location($code)]} {
        set loc $location($code)
        #puts "$code location: $loc"

        #puts " matching $dcode against $loc"
        #puts " matching $dcode/* against $loc"
        if {$code == $dcode || \
              [string match "$dcode" $loc] || \
              [string match "$dcode/*" $loc]} {

          #puts " $code matched $loc"
          if {$sorp == "Screen"} {
            set printme [format "%-55s %s" [lindex $items 0] $code]
            .viewer.text insert end "$printme\n"
          } elseif {$sorp == "Printer"} {
            postscript_line $outfile [lindex $items 0] $code
          } else {
            set printme [format "%-55s %s" [lindex $items 0] $code]
            puts $outfile "$printme"
          }
          if {$versions} {
            report_versions $code $tagname
          }
        }
      }
    }
  }
   
  # Finish the output
  if {$sorp == "Screen"} {
    .viewer.text configure -state disabled
    wm deiconify .viewer
    raise .viewer
  } elseif {$sorp == "Printer"} {
    postscript_end $outfile
    close $outfile
    gen_log:log F "lpr -P$printer_name tkcvs.tmp"
    exec lpr -P$printer_name tkcvs.tmp
    gen_log:log F "DELETE tkcvs.tmp"
    file delete tkcvs.tmp
  } else {
    close $outfile
  }
  feedback_cvs $feedback(cvs) ""
  gen_log:log T "LEAVE"
}

proc modlist_by_code_run {dcode versions tagname} {
#
# This does all the hard work in creating the module listing
# Called by  Module Tree
#       and  Version Tree
#
  global dtitle
  global mtitle
  global sorp
  global file_name
  global printer_name
  global cvscfg
  global feedback
  
  gen_log:log T "ENTER ($dcode $versions $tagname)"
  feedback_cvs $feedback(cvs) "Generating Report Please Wait"

  # Initialize the output
  if {$sorp == "Screen"} {
    set linenum 1
    .viewer.text configure -state normal
    .viewer.text delete 1.0 end
  } elseif {$sorp == "Printer"} {
    set outfile [open "tkcvs.tmp" w]
    set cvscfg(ycurrent) $cvscfg(ystart)
    set cvscfg(pagenum) 1
    postscript_setup $outfile
  } else {
    set outfile [open $file_name w]
  }

  # If dcode is "." then report the entire tree.  Otherwise restrict
  # the report to a portion of the tree.

  # Special case where $dcode is not a top level.
  if {$dcode != "." && [file dirname $dcode] != "."} {
    if {[info exists dtitle($dcode)]} {
      # Must be a subdirectory.
      if {$sorp == "Screen"} {
        .viewer.text insert end "\n$dtitle($dcode) \[$dcode\]\n"
        .viewer.text tag add sublevel $linenum.0 "[expr {$linenum + 1}].0 lineend" 
        incr linenum 2
      } elseif {$sorp == "Printer"} {
        postscript_subheading $outfile "   $dtitle($dcode)" $dcode
      } else {
        puts $outfile "\n$dtitle($dcode) \[$dcode\]"
      }
      report_on_dir $dcode $versions $tagname
    }
  } elseif {$dcode != "." && [info exists mtitle($dcode)]} {
    # Must be a module.
    if {$sorp == "Screen"} {
      set printme [format "%-55s %s" $mtitle($dcode) $dcode]
      .viewer.text insert end "$printme\n"
      incr linenum
    } elseif {$sorp == "Printer"} {
      postscript_line $outfile "          $mtitle($dcode)" $dcode
    } else {
      set printme [format "%-55s %s" $mtitle($dcode) $dcode]
      puts $outfile "$printme"
    }
    if {$versions} {
      report_versions $dcode $tagname
    }
  # Either dcode is a toplevel or is "." which means do all toplevels.
  } else {
    foreach dname [array names dtitle] {
      if {$dcode != "." && $dname != $dcode} {
        continue
      }
      if {[file dirname $dname] == "."} {
        if {$sorp == "Screen"} {
          .viewer.text insert end "\n$dtitle($dname) \[$dname\]\n\n"
          .viewer.text tag add toplevel $linenum.0 "[expr {$linenum + 2}].0 lineend"
          incr linenum 3
        } elseif {$sorp == "Printer"} {
          postscript_heading $outfile $dtitle($dname) $dname
        } else {
          puts $outfile "\n$dtitle($dname) \[$dname\]\n"
        }
        report_on_dir $dname $versions $tagname
      }
    }
  }

  # Finish the output
  if {$sorp == "Screen"} {
    .viewer.text tag configure toplevel \
      -font -Adobe-Helvetica-Bold-R-Normal-*-18-*
    .viewer.text tag configure sublevel \
      -font -Adobe-Helvetica-Bold-R-Normal-*-14-*
    .viewer.text configure -state disabled
    wm deiconify .viewer
    raise .viewer
  } elseif {$sorp == "Printer"} {
    postscript_end $outfile
    close $outfile
    gen_log:log F "lpr -P$printer_name tkcvs.tmp" 
    exec lpr -P$printer_name tkcvs.tmp
    gen_log:log F "DELETE tkcvs.tmp"
    file delete tkcvs.tmp
  } else {
    close $outfile
  }

  feedback_cvs $feedback(cvs) ""
  gen_log:log T "LEAVE"
}
 
proc report_on_dir {dname versions tagname} {
  global mtitle
  global dtitle
  global dcontents
  global dsubmenus
  global cvs
  global sorp
  upvar linenum linenum
  upvar outfile outfile

  gen_log:log T "ENTER ($dname $versions $tagname)"
  if {[info exists dcontents($dname)]} {
    #puts "dcontents($dname) $dcontents($dname)"
    foreach mname $dcontents($dname) {
      if {$sorp == "Screen"} {
        set printme [format "%-55s %s" $mtitle($mname) $mname]
        .viewer.text insert end "$printme\n"
        incr linenum
      } elseif {$sorp == "Printer"} {
        postscript_line $outfile "          $mtitle($mname)" $mname
      } else {
        set printme [format "%-55s %s" $mtitle($mname) $mname]
        puts $outfile "$printme"
      }
      if {$versions} {
        report_versions $mname $tagname
      }
    }
  } else {
     if {$versions} {
        report_versions $dname $tagname
      }
  }

  if {[info exists dsubmenus($dname)]} {
    #puts "dsubmenus($dname) $dsubmenus($dname)"
    foreach subdir $dsubmenus($dname) {
      if {$sorp == "Screen"} {
        .viewer.text insert end "\n$dtitle($subdir) \[$subdir\]\n"
        .viewer.text tag add sublevel $linenum.0 "[expr {$linenum + 1}].0 lineend" 
        incr linenum 2
      } elseif {$sorp == "Printer"} {
        postscript_subheading $outfile "   $dtitle($subdir)" $subdir
      } else {
        puts $outfile "\n$dtitle($subdir) \[$subdir\]" 
      }
      report_on_dir $subdir $versions $tagname
    }
  }
  gen_log:log T "LEAVE"
}

proc report_versions {mcode tagname} {
  global filenames
  global location
  global cwd
  global sorp
  global cvs
  global cvscfg
  upvar linenum linenum
  upvar outfile outfile

  gen_log:log T "ENTER ($mcode $tagname)"
  # Aliases won't have locations so be careful.
  if {! [info exists location($mcode)]} {
    #puts "location($mcode) does not exist - must be an alias"
    return
  }
  #puts "location($mcode) is $location($mcode)"

  # If a list of files does not exist for this module, create it now.
  if {! [info exists filenames($mcode)]} {
    find_filenames $mcode
  }

  # Be careful of empty modules.
  if {! [info exists filenames($mcode)]} {
    #puts "didnt find any files in $mcode"
    return
  }
  #puts "report_versions: filenames($mcode) is $filenames($mcode)"

  if {$tagname == {}} {
    set commandline "$cvs -d $cvscfg(cvsroot) checkout -p $mcode >$cvscfg(null)"
  } else {
    set commandline "$cvs -d $cvscfg(cvsroot) checkout -r $tagname -p $mcode >$cvscfg(null)"
  }
  gen_log:log C "$commandline"
  catch {eval "exec $commandline"} view_this

  set filelist ""
  set view_lines [split $view_this "\n"]
  foreach line $view_lines {
    if {[string match "Checking out *" $line]} {
      set dname [lindex [split $line] 2]
      regsub "$mcode/" $dname "" filename
      lappend filelist $filename
    }
    if {[string match "VERS:*" $line]} {
      set ver [lindex [split $line] 1]
      set version($filename) $ver
    }
  }

  foreach filename $filelist {
    if {$sorp == "Screen"} {
      set printme [format "   %-55s %s" $filename $version($filename)]
      .viewer.text insert end "$printme\n"
      incr linenum
    } elseif {$sorp == "Printer"} {
      postscript_line $outfile "               $filename" $version($filename)
    } else {
      set printme [format "  %-55s %s" $filename $version($filename)]
      puts $outfile "$printme"
    }
  }

  gen_log:log T "LEAVE"
}

proc postscript_setup {outfile} {
  global cvscfg

  gen_log:log T "ENTER $outfile"
  set col1 [expr {$cvscfg(xend) * 0.55}]

  puts $outfile "%!PS-Adobe-2.0"
  puts $outfile "%%Title: module listing"
  puts $outfile "%%Creator: TkCVS"
  puts $outfile "%%DocumentFonts: Times-Roman"
  puts $outfile "%%ProofMode: Substitute"
  puts $outfile "%%Pages: (atend)"
  puts $outfile "%%EndComments"
  puts $outfile "%"
  puts $outfile "% Constants definition"
  puts $outfile "%"
  puts $outfile "/ystart $cvscfg(ystart) def"
  puts $outfile "/yend $cvscfg(yend) def"
  puts $outfile "/xstart $cvscfg(xstart) def"
  puts $outfile "/xend $cvscfg(xend) def"
  puts $outfile "/col1 $col1 def"
  puts $outfile "/div1 col1 xstart add 10 sub def"
  puts $outfile "/pointsize $cvscfg(pointsize) def"
  puts $outfile "/topsize $cvscfg(headingsize) def"
  puts $outfile "/subsize $cvscfg(subheadingsize) def"
  puts $outfile "/lineseparator pointsize 1 add def"
  puts $outfile "/textfont /Times-Roman findfont pointsize scalefont def"
  puts $outfile "/topfont /Helvetica findfont topsize scalefont def"
  puts $outfile "/subfont /Helvetica findfont subsize scalefont def"
  puts $outfile "/pagenum 1 def"
  puts $outfile "%"
  puts $outfile "% procedure definitions"
  puts $outfile "%"
  puts $outfile "/newpage"
  puts $outfile "  {"
  puts $outfile "   textfont setfont"
  puts $outfile "   /ycurrent ystart def"
  puts $outfile "   /xcurrent xstart def"
  puts $outfile "   (Module Name) col1 showtab"
  puts $outfile "   (Module Code) showln"
  puts $outfile "   () showln"
  puts $outfile "   /x1 xstart 5 sub def"
  puts $outfile "   /x2 xend def"
  puts $outfile "   /y1 ystart lineseparator add def"
  puts $outfile "   /y2 yend lineseparator 3 mul sub def"
  puts $outfile "   x1 y1 moveto"
  puts $outfile "   x2 y1 lineto"
  puts $outfile "   x2 y2 lineto"
  puts $outfile "   x1 y2 lineto"
  puts $outfile "   x1 y1 lineto"
  puts $outfile "   x1 ystart 2 sub moveto"
  puts $outfile "   x2 ystart 2 sub lineto"
  puts $outfile "   div1 y1 moveto"
  puts $outfile "   div1 y2 lineto"
  puts $outfile "   stroke"
  puts $outfile "   xstart ystart lineseparator 2 mul add moveto"
  puts $outfile "   (TkCVS     Module Listing             Page ) show"
  puts $outfile "   pagenum 10 string cvs show"
  puts $outfile "   /pagenum pagenum 1 add def"
  puts $outfile "  } def"
  puts $outfile "%"
  puts $outfile "/showtab"
  puts $outfile "  {"
  puts $outfile "   /xdelta exch def"
  puts $outfile "   xcurrent ycurrent moveto show"
  puts $outfile "   /xcurrent xcurrent xdelta add def"
  puts $outfile "  } def"
  puts $outfile "%"
  puts $outfile "/showtop"
  puts $outfile "  {"
  puts $outfile "   /xdelta exch def"
  puts $outfile "   topfont setfont"
  puts $outfile "   /ycurrent ycurrent topsize sub def"
  puts $outfile "   xcurrent ycurrent moveto show"
  puts $outfile "   /xcurrent xcurrent xdelta add def"
  puts $outfile "   xcurrent ycurrent moveto show"
  puts $outfile "   /ycurrent ycurrent lineseparator 2 mul sub def"
  puts $outfile "   /xcurrent xstart def"
  puts $outfile "   textfont setfont"
  puts $outfile "  } def"
  puts $outfile "%"
  puts $outfile "/showsub"
  puts $outfile "  {"
  puts $outfile "   /xdelta exch def"
  puts $outfile "   subfont setfont"
  puts $outfile "   /ycurrent ycurrent subsize sub def"
  puts $outfile "   xcurrent ycurrent moveto show"
  puts $outfile "   /xcurrent xcurrent xdelta add def"
  puts $outfile "   xcurrent ycurrent moveto show"
  puts $outfile "   /ycurrent ycurrent lineseparator 2 mul sub def"
  puts $outfile "   /xcurrent xstart def"
  puts $outfile "   textfont setfont"
  puts $outfile "  } def"
  puts $outfile "%"
  puts $outfile "/showln"
  puts $outfile "  {"
  puts $outfile "   xcurrent ycurrent moveto show"
  puts $outfile "   /ycurrent ycurrent lineseparator sub def"
  puts $outfile "   /xcurrent xstart def"
  puts $outfile "  } def"
  puts $outfile "%%EndProlog"
  puts $outfile "%"
  puts $outfile "% Start main program"
  puts $outfile "%"
  puts $outfile "%%Page: 1 1"
  puts $outfile "newpage"

  gen_log:log T "LEAVE"
}

proc postscript_line {outfile docname doccode} {
  global cvscfg

  gen_log:log T "ENTER ($outfile $docname $doccode)"
  puts $outfile "( $docname ) col1 showtab"
  puts $outfile "( $doccode ) showln"

  set cvscfg(ycurrent) [expr {$cvscfg(ycurrent) - $cvscfg(pointsize) - 1}]
  if {$cvscfg(ycurrent) < $cvscfg(yend)} {
    set cvscfg(ycurrent) $cvscfg(ystart)
    incr cvscfg(pagenum)
    puts $outfile "showpage"
    puts $outfile "%%Page: $cvscfg(pagenum) $cvscfg(pagenum)"
    puts $outfile "newpage"
  }
  gen_log:log T "LEAVE"
}

proc postscript_heading {outfile docname doccode} {
  global cvscfg
 
  gen_log:log T "ENTER ($outfile $docname $doccode)"
  puts $outfile "($doccode) ( $docname ) col1 showtop"
 
  set cvscfg(ycurrent) \
    [expr {$cvscfg(ycurrent) - $cvscfg(headingsize) - (2*$cvscfg(pointsize)) - 2}]
  if {$cvscfg(ycurrent) < $cvscfg(yend)} {
    set cvscfg(ycurrent) $cvscfg(ystart)
    incr cvscfg(pagenum)
    puts $outfile "showpage"
    puts $outfile "%%Page: $cvscfg(pagenum) $cvscfg(pagenum)"
    puts $outfile "newpage"
  }
  gen_log:log T "LEAVE"
}

proc postscript_subheading {outfile docname doccode} {
  global cvscfg
 
  gen_log:log T "ENTER ($outfile $docname $doccode)"
  puts $outfile "($doccode) ( $docname ) col1 showsub"
 
  set cvscfg(ycurrent) \
   [expr {$cvscfg(ycurrent) - $cvscfg(subheadingsize) - (2*$cvscfg(pointsize)) - 2}]
  if {$cvscfg(ycurrent) < $cvscfg(yend)} {
    set cvscfg(ycurrent) $cvscfg(ystart)
    incr cvscfg(pagenum)
    puts $outfile "showpage"
    puts $outfile "%%Page: $cvscfg(pagenum) $cvscfg(pagenum)"
    puts $outfile "newpage"
  }
  gen_log:log T "LEAVE"
}

proc postscript_end {outfile} {
  global cvscfg

  gen_log:log T "ENTER $outfile"
  puts $outfile "showpage"
  puts $outfile "%%Trailer"
  puts $outfile "%%Pages: $cvscfg(pagenum)"
  gen_log:log T "LEAVE"
}
