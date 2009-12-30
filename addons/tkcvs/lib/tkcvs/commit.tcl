#
# Tcl Library for TkCVS
#
 
# 
# $Id: commit.tcl,v 1.1.1.1 2001-06-07 15:03:42 ramsan Exp $
#
# Set up a small commit dialog.
#

# RAMSAN adding
set SaveComments(last) 0
set SaveComments(current) 0

proc EnterNextComment { text } {
    global SaveComments
    set SaveComments($SaveComments(last)) $text
    incr SaveComments(last)
    set SaveComments(current) $SaveComments(last)
}

proc GivePrevNextComment { { what next } { last no } } {
    global SaveComments
    
    if { $last != "no" } {
	set SaveComments(current) $SaveComments(last)
	set what prev
    }

    switch $what {
	prev { incr SaveComments(current) -1 }
	next { incr SaveComments(current) 1 }
    }
    if { $SaveComments(current) < 0 } {
	set SaveComments(current) -1
	return ""
    }
    if { $SaveComments(current) >= $SaveComments(last) } {
	set SaveComments(current) $SaveComments(last)
	return ""
    }
    return $SaveComments($SaveComments(current))
}


proc commit_setup {} {
  global cvsglb
  global cvscfg

  gen_log:log T "ENTER"

  toplevel .commit
  frame .commit.top -border 8
  frame .commit.vers
  frame .commit.comment
  frame .commit.down -relief groove -border 2

  pack .commit.top -side top -fill x
  pack .commit.down -side bottom -fill x
  pack .commit.vers -side top -fill y
  pack .commit.comment -side top -fill both -expand 1

  label .commit.lvers -text "Specify Revision (-r) (usually ignore)" \
     -anchor w -font $cvscfg(guifont)
  entry .commit.tvers -relief sunken -textvariable version

  pack .commit.lvers .commit.tvers -in .commit.vers \
    -side left -fill x -pady 3


  label .commit.lcomment -text "Please enter a log" -anchor w -font $cvscfg(guifont)
  text .commit.tcomment -relief sunken -width 70 -height 10 \
    -wrap word -border 2 -setgrid yes
  #RAMSAN
  focus .commit.tcomment

  pack .commit.lcomment -in .commit.comment \
    -side left -fill x -pady 3
  pack .commit.tcomment -in .commit.comment \
    -side left -fill both -expand 1 -pady 3

  # Explain what it means to "commit" files
  message .commit.message -justify left -aspect 500 -relief groove \
    -font $cvscfg(guifont) \
    -text "This will commit changes from your \
           local, working directory into the repository, recursively.

\
          For any local (sub)directories or files that are on a branch, \
           your changes will be added to the end of that branch.  \
           This includes new or deleted files as well as modifications.

\
          For any local (sub)directories or files that have \
           a non-branch tag, a branch will be created, and \
           your changes will be placed on that branch.  \
           (CVS bug.  Sorry.)

\
          For all other (sub)directories, your changes will be \
           added to the end of the main trunk."

  pack .commit.message -in .commit.top -padx 2 -pady 5


  button .commit.ok -text "OK" -font $cvscfg(guifont)  -und 0 \
    -command {
      #RAMSAN
      EnterNextComment [.commit.tcomment get 1.0 end] 
      cvs_commit $version [.commit.tcomment get 1.0 end] $cvsglb(commit_list)
      if {$cvscfg(auto_status) == "true"} {
        setup_dir
      }
      wm withdraw .commit
    }

    #RAMSAN
    button .commit.prev -text "Prev comment" -underline 0 -command { 
	.commit.tcomment del 1.0 end
	.commit.tcomment ins end [GivePrevNextComment prev]
    }
    button .commit.next -text "Next comment" -underline 0 -command { 
	.commit.tcomment del 1.0 end
	.commit.tcomment ins end [GivePrevNextComment next]
    }


  # Ugly hack.  Just a copy of above.  Need to work on this.  -JW
  button .commit.apply -text "Apply" -font $cvscfg(guifont) \
    -command {
      cvs_commit $version [.commit.tcomment get 1.0 end] $cvsglb(commit_list)
      if {$cvscfg(auto_status) == "true"} {
        setup_dir
      }
    }
  button .commit.clear -text "ClearAll" -font $cvscfg(guifont) -command {
    set version ""
    .commit.tcomment delete 1.0 end
    }
  button .commit.quit -text "Quit" -font $cvscfg(guifont) -und 0 \
    -command { wm withdraw .commit }
 
  pack .commit.ok .commit.prev .commit.next .commit.clear \
	  .commit.quit -in .commit.down \
    -side left -ipadx 2 -ipady 2 -padx 4 -pady 4 -fill both -expand 1
 
  # RAMSAN
  bind .commit <Alt-o> ".commit.ok invoke"
  bind .commit <Alt-q> ".commit.quit invoke"
  bind .commit <Alt-n> ".commit.next invoke"
  bind .commit <Alt-p> ".commit.prev invoke"


  # Needed for slower framebuffers
# RAMSAN
  #tkwait visibility .commit

#  wm withdraw .commit
  wm title .commit "Commit Changes to a Module"
  wm minsize .commit 1 1
wm geom .commit +50+50
  gen_log:log T "LEAVE"
}

proc commit_run {} {
  global incvs
  global cvsglb

  gen_log:log T "ENTER"
  if {! $incvs} {
    cvs_notincvs
    return 1
  }
  if { ! [winfo exists .commit] } {
    commit_setup
  }

  # If marked files, commit these.  If no marked files, then
  # commit any files selected via listbox selection mechanism.
  # The cvsglb(commit_list) list remembers the list of files
  # to be committed.
  set cvsglb(commit_list) [workdir_list_files]
  wm deiconify .commit
  raise .commit

  gen_log:log T "LEAVE"
}
