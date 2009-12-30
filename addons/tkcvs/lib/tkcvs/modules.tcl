#
# TCL Library for TkCVS
#

#
# $Id: modules.tcl,v 1.2 2006-06-30 17:40:15 ramsan Exp $
#
# Procedures to parse the CVS modules file and store whatever is
# read into various associative arrays, sorted, and unsorted lists.
#

#
# Global variables:
#
# mtitle
#   For each module, the name of the module.
# dtitle
#   For each directory, the name of the directory.
# dcontents
#   For each directory, the list of modules within it.
# dsubmenus
#   For each directory, the list of subdirectories within it.
# cvscfg
#   General configuration variables (array)
# filenames
#   For each module, the list of files that it contains.
# location
#   For each module, its location in the repository.

proc read_modules_setup {root} {
#
# Read one pass through the modules file.
#
  global cvs
  global mtitle
  global dtitle
  global dcontents
  global dsubmenus
  global cvscfg
  global filenames
  global location

  gen_log:log T "ENTER ($root)"
  if {[string match "*:*" $root]} {
    set cvscfg(remote) 1
  } else {
    set cvscfg(remote) 0
  }
  gen_log:log T " remote=$cvscfg(remote)"

  # ramsan: changes to support filenames with spaces
  set cvscfg(modfile) [file join $cvscfg(tmpdir) modules-[pid]]
  set commandline [list $cvs -d $root checkout -p CVSROOT/modules > $cvscfg(modfile)]
  gen_log:log C $commandline
  set ret [catch {eval "exec $commandline"} view_this]
  # should only fail if the checkout has been aborted
  if {$ret} {
    gen_log:log D "cvs checkout returned $ret"
    if {[string match "* aborted*" $view_this]} {
      cvsfail $view_this
    }
  }

  catch {unset mtitle}
  catch {unset dtitle}
  catch {unset dcontents}
  catch {unset dsubmenus}

  # Set up a top level directory for "aliases"
  set dtitle(aliases) "Aliases"


  # Include a default name for the "world" alias that everyone tends
  # to ignore.
  set mtitle(world) "The Whole CVS Repository."

  # Read through the entire modules file to get out the module names.
  gen_log:log F "OPEN $cvscfg(modfile)"
  set modules [open $cvscfg(modfile)]
  while {[gets_full_line $modules line] >= 0} {
    # Split and parse the line
    if {$line != {}} {
      set text [split $line "\t"]

      # #D describes a directory title.
      if {[lindex $text 0] == "#D"} {
	set dname [lindex $text 1]
	set dtitle($dname) [lindex $text 2]
	set layers [split $dname "/"]
	gen_log:log D " DIRECTORY $dname \"$dtitle($dname)\""
	if {[llength $layers] > 1} {
	  set pname [file dirname $dname]
	  if {[info exists dsubmenus($pname)]} {
	    lappend dsubmenus($pname) $dname
	  } else {
	    set dsubmenus($pname) $dname
	  }
	  gen_log:log D "$dname added to submenu dsubmenus($pname)"
	}
	continue
      }

      # #M means this is a module title
      if {[lindex $text 0] == "#M"} {
	set mcode [lindex $text 1]
	set mtitle($mcode) [lindex $text 2]
	gen_log:log D " MODULE $mcode \"$mtitle($mcode)\""
	continue
      }

      # Any other non-comment means that this is a module.  These
      # can be separated by whitespace not just tabs.
      set text [clean_list $line]
      set mcode [lindex $text 0]

      # Process aliases as part of the "aliases" directory.
      if {! [regexp {^#} $mcode] && [regexp {^-a} [lindex $text 1]] } {
	if {[info exists mtitle($mcode)]} {
	  gen_log:log D " ALIAS $mcode \"$mtitle($mcode)\""
	  set filenames($mcode) ",,#ALIAS"
	  if {[info exists dcontents(aliases)]} {
	    lappend dcontents(aliases) $mcode
	  } else {
	    set dcontents(aliases) $mcode
	  }
	} else {
	  puts "ALIAS $mcode has no title"
	}
      }

      # Process all other modules as part of their parent directories.
      if {! [regexp {^#} $mcode] && ! [regexp {^-a} [lindex $text 1]] } {
	 set mcode [lindex $text 0]
	 set mname [lindex $text 1]
	 set location($mcode) $mname
	 set layers [split $mname "/"]
	 gen_log:log D "  $mcode path is $location($mcode)"
	 # If the text list has more than two elements, then this
	 # module has files.  In that case it is a child of the current
	 # directory of the module, not the parent directory.
	 if {[llength $text] > 2 && ! [regexp {[\w*&]} $text] } {
	   set pname $mname
	   set filenames($mcode) [lrange $text 2 end]
	 } else {
	   set pname [file dirname $mname]
	   # In this case filenames($mcode) is unset.  Take this to mean
	   # that the module comprises all files (recursively) in the
	   # module directory.  If filenames is needed later it can be
	   # established by reading the directory.
	 }
	 if {[info exists mtitle($mcode)]} {
	   if {[llength $layers] > 1} {
	     if {[info exists dcontents($pname)]} {
	       lappend dcontents($pname) $mcode
	     } else {
	       set dcontents($pname) $mcode
	     }
	     gen_log:log D "  $mcode added to dcontents($pname)"
	   } else {
	     # The module is a submodule of a directory, because the defined
	     # directory is identical with a top level dir but the module
	     # contains a subset of files, thus add the module to the list.
	     # The module appears in the reports and 'check-out' window.
	     if { "$layers" == "$pname" } {
	       if {[info exists dcontents($pname)]} {
		 lappend dcontents($pname) $mcode
	       } else {
		 set dcontents($pname) $mcode
	       }
	     } else {
	       gen_log:log D "  $mcode is a top level directory"
	     }
	   }
	 } else {
	   gen_log:log D "  $mcode has no title"
	 }
      }

    }
    # No more lines in modules
  }
  close $modules
  file delete -force $cvscfg(modfile)
  gen_log:log F "DELETE $cvscfg(modfile)"

  # If there are no aliases, get rid of the folder
  if {! [info exists dcontents(aliases)]} {
    unset dtitle(aliases)
  }

  # report_on_menu
  gather_mod_index
  gen_log:log T "LEAVE"
}

proc gather_mod_index {} {
#
# Creates a new global list called modlist_by_title that
# contains a sorted list of the module titles.  The module
# code is appended to the module title, separated by a tab.
#
  global mtitle
  global modlist_by_title

  gen_log:log T "ENTER ()"

  if {! [info exists mtitle]} {
    set modlist_by_title {}
    gen_log:log T "LEAVE"
    return
  }

  set modlist {}

  foreach mcode [array names mtitle] {
    lappend modlist "  $mtitle($mcode)\t$mcode"
  }

  set modlist_by_title [lsort $modlist]
  foreach idx $modlist_by_title {
    gen_log:log D "$idx"
  }
  gen_log:log T "LEAVE"
}

proc clean_list {line} {
#
# Returns a list clean of null items after splitting line.
# Also removes any -<x> options and their arguments from the list.
#
# If the line is an alias line (like "myfiles -a hisfiles") then just
# return "myfiles -a".  TkCVS handles aliases as a special case.
#
# If the line contains any other options (like "myfiles -i checkinprog dir/files")
# then remove the options and their arguments (so return "myfiles dir/files"
# only).  -i/-o/etc options are supported by CVS but ignored by TkCVS.
#
# Arguments apart from options and their option arguments are preserved.
# (eg: myfiles -i ciprog dir/files my1 my2 returns myfiles dir/files my1 my2).
# These can be used to select the file names for a module.  NOTE:  THIS IS
# NOT RECOMMENDED!  CVS will not stop you attempting "cvs add" on such a
# module, but the "cvs add" will not add the file names to the module in the
# modules database!
#
# skip_args:
#   Set to 1 to skip the next item in the loop.  Do this when
#   the item is -<x> where x is not "a".
#

  set oldlist [split $line]
  set skip_args 0
  #puts stderr $oldlist

  foreach item $oldlist {
    # If the item is "-a" then completely ignore this line (don't do aliases).
    if {$item == "-a"} {
      if {[info exists newlist]} {
	set newlist [lindex $newlist 0]
      } else {
	set newlist "error_in_modules_file"
      }
      lappend newlist $item
      return $newlist
    }
    # If the item is any other option then skip this item and the next one.
    if {[regexp {^-} $item]} {
      set skip_args 1
      continue
    }
    # If the item is non-blank then process it.
    if {$item != {}} {
      # However, if the last item was an option skip this one.
      if {$skip_args} {
	set skip_args 0
	continue
      }
      # Add the item to the list, or create the list if it is empty.
      if {[info exists newlist]} {
	lappend newlist $item
      } else {
	set newlist $item
      }
    }
  }

  if {[info exists newlist]} {
    return $newlist
  } else {
    return {}
  }
}

proc gets_full_line {file varname} {
#
# Gets a full line of text from file, taking into account that
# the line may be split by backslashes.
#
  upvar $varname line
  set numchars 0
  set myline ""
  set line ""

  while 1 {
    set getchars [gets $file myline]
    # If we hit the end of the file then go home.
    if {$getchars == -1} {
      if {$numchars == 0} {
	return -1
      } else {
	return $numchars
      }
    } else {
      incr numchars $getchars
      set line [format "%s%s" $line $myline]
      if {$getchars == 0} {
	return $numchars
      }
      # If there is no trailing backslash, go home.
      if {[string index $myline [expr {[string length $myline] - 1}]] != "\\"} {
	return $numchars
      }
      # If there is one, chop it off and reloop.
      set line [string range $line 0 [expr {[string length $line] - 2}]]
      incr numchars -1
    }
  }
}

proc find_filenames {mcode} {
#
# This does the work of setting up the filenames array for a module,
# containing the list of file names within it.
#
  global filenames
  global location
  global cwd
  global cvs
  global cvscfg
  global checkout_version
  global modbrowse_version
  global feedback

  gen_log:log T "ENTER"
  gen_log:log D "mcode = $mcode"

  if {[info exists filenames($mcode)]} {
    set filenames($mcode) ""
  }

  feedback_cvs $feedback(mod) "Building file list, please wait!"
  gen_log:log D "remote = $cvscfg(remote)"
  if {$cvscfg(remote)} {
    # This was the only way I could think of to use a cvs command to
    # get a list of files.  It sends the stuff I want to stderr, or
    # else we could use exec_command.  If the module is large, this
    # can take an awfully long time - dr
    #
    if {[info exists checkout_version] && $checkout_version != {} } {
      set rev $checkout_version
    } elseif {[info exists modbrowse_version] && $modbrowse_version != {} } {
      set rev $modbrowse_version
    } else {
      set rev HEAD
    }
    set commandline \
       "$cvs -d $cvscfg(cvsroot) checkout -r $rev -p $mcode >$cvscfg(null)"
    gen_log:log C  $commandline
    catch {eval "exec $commandline"} view_this
   
    set view_lines [split $view_this "\n"]
    foreach line $view_lines {
      if {[string match "Checking out *" $line]} {
	set dname [lindex [split $line] 2]
	regsub "$mcode/" $dname "" fname
	if {[info exists filenames($mcode)]} {
	  lappend filenames($mcode) $fname
	} else {
	  set filenames($mcode) $fname
	}
      }
    }
  } else {
    # We are not remote, so we can cd to the repository.
    # cd to the module location and find all of the files in it.
    # The method for remote can be very slow, so I'm keeping
    # the local method if we can use it.
    if {[catch {cd [file join $cvscfg(cvsroot) $location($mcode)]}]} {
      # If the directory doesn't exist, go home.
      feedback_cvs $feedback(mod) ""
      return
    }
    gen_log:log F "CD [pwd]"
    gen_log:log F "|find . -type f -print"
    set fd [open "|find . -type f -print"]
    while {[gets $fd line] != -1} {
      # strip off the leading "./" that find puts in.
      set fname [string range $line 2 end]
      # only bother with this if it is a ,v file.
      if {[regexp {,v$} $fname]} {
	# Strip off the ,v bit.
	set fname [string range $fname 0 [expr {[string length $fname] - 3}]]
	if {[info exists filenames($mcode)]} {
	  lappend filenames($mcode) $fname
	} else {
	  set filenames($mcode) $fname
	}
      }
    }
    if {[info exists filenames($mcode)]} {
      set filenames($mcode) [lsort $filenames($mcode)]
    }
    catch {close $fd}
    cd $cwd
    gen_log:log F "CD [pwd]"
  }
  feedback_cvs $feedback(mod) ""
}
