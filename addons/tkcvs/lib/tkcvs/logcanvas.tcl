#
# TCL Library for TkCVS
#

#
# $Id: logcanvas.tcl,v 1.3 2008-07-30 11:55:35 ramsan Exp $
#
# Contains procedures used for the log canvas for tkCVS.
# this version attempts to implement a bounding-box scenario
# for each revision, and modifies logcanvas_rectangle to look for
# bounding-box collisions.
# Leif E.
#

set const(boxx) 80
set const(xfactor) 14
set const(boxy) 30
set const(spacex) 60
set const(spacey) 16
set const(textheight) 12
set cvscanv ""

proc new_logcanvas {localfile filelog} {
  #
  # Creates a new log canvas.  filelog must be the output of a cvs
  # log or rlog command.  If localfile is not "no file" then it is
  # the file name in the local directory that this applies to.
  #
  global cvscfg
  global cvs
  global revdate
  global revwho
  global revcomment
  global revbranches
  global revlist
  global cvscanv
  global tags
  global bndbox
  global const

  gen_log:log T "ENTER ($localfile <filelog suppressed>)"
  unset cvscanv
  # Height and width to draw boxes
  set cvscanv(boxx) $const(boxx)
  set cvscanv(boxy) $const(boxy)
  # Gaps between boxes
  set cvscanv(gapx) [expr {$cvscanv(boxx) + $const(spacex)}]
  set cvscanv(gapy) [expr {$cvscanv(boxy) + $const(spacey)}]
  # Indent at top left of canvas
  set cvscanv(indx) 5
  set cvscanv(indy) 5
  # Static type variables used while drawing on the canvas.
  set cvscanv(xhigh) 0
  set cvscanv(yhigh) 0
  set cvscanv(xlow)  0
  set cvscanv(ylow)  0

  if {[info exists revlist]} { unset revlist }
  if {[info exists revdate]} { unset revdate }
  if {[info exists revwho]} { unset revwho }
  if {[info exists revbranches]} { unset revbranches }
  if {[info exists revcomment]} { unset revcomment }
  if {[info exists tags]} { unset tags }
  if {[info exists bndbox]} { unset bndbox }

  static {canvasnum 0}

  # Make the canvas

  incr canvasnum
  set logcanvas ".logcanvas$canvasnum"
  toplevel $logcanvas

  frame $logcanvas.up -relief groove -border 2
  label $logcanvas.up.lfname -text "RCS File" \
     -width 12 -anchor w -font $cvscfg(guifont)
  entry $logcanvas.up.rfname
  pack $logcanvas.up -side top -fill x
  pack $logcanvas.up.lfname -side left
  pack $logcanvas.up.rfname -side left -fill x -expand 1

  set textfont [$logcanvas.up.rfname cget -font]

  frame $logcanvas.up1 -relief groove -border 2
  frame $logcanvas.up1.rev
  label $logcanvas.up1.rev.lvers -text "Revision A" \
     -width 12 -anchor w -font $cvscfg(guifont)
  entry $logcanvas.up1.rev.rvers
  pack $logcanvas.up1 -side top -fill x
  pack $logcanvas.up1.rev -side top -fill x -expand 1
  pack $logcanvas.up1.rev.lvers \
    -side left
  pack $logcanvas.up1.rev.rvers \
    -side left -fill x -expand 1

  frame $logcanvas.up1.bydate
  label $logcanvas.up1.bydate.ldate -text "Committed" \
     -width 12 -anchor w -font $cvscfg(guifont)
  label $logcanvas.up1.bydate.rdate -text "--" \
     -anchor w -font $cvscfg(guifont)
  label $logcanvas.up1.bydate.lwho -text " by " \
     -anchor w -font $cvscfg(guifont)
  label $logcanvas.up1.bydate.rwho -text "--" \
     -anchor w -font $textfont
  pack $logcanvas.up1.bydate -side top -fill x -expand 1
  pack $logcanvas.up1.bydate.ldate \
       $logcanvas.up1.bydate.rdate \
       $logcanvas.up1.bydate.lwho \
       $logcanvas.up1.bydate.rwho \
    -side left

  frame $logcanvas.up1.log
  label $logcanvas.up1.log.lcomment -text "Log" \
     -width 12 -anchor w -font $cvscfg(guifont)
  frame $logcanvas.up1.log.rlogfm
  text  $logcanvas.up1.log.rlogfm.rcomment -height 5 -width 75 \
     -yscrollcommand "$logcanvas.up1.log.rlogfm.yscroll set"
  scrollbar $logcanvas.up1.log.rlogfm.yscroll \
     -command "$logcanvas.up1.log.rlogfm.rcomment yview"
  pack $logcanvas.up1.log -side top -fill x -expand 1
  pack $logcanvas.up1.log.lcomment -side left
  pack $logcanvas.up1.log.rlogfm -side left -fill x -expand 1
  pack $logcanvas.up1.log.rlogfm.rcomment \
     -side left -fill x -expand 1
  pack $logcanvas.up1.log.rlogfm.yscroll \
     -side left -fill y

  frame $logcanvas.up2 -relief groove -border 2
  frame $logcanvas.up2.rev
  label $logcanvas.up2.rev.lvers -text "Revision B" \
     -width 12 -anchor w -font $cvscfg(guifont)
  entry $logcanvas.up2.rev.rvers
  pack $logcanvas.up2 -side top -fill x
  pack $logcanvas.up2.rev -side top -fill x -expand 1
  pack $logcanvas.up2.rev.lvers \
    -side left
  pack $logcanvas.up2.rev.rvers \
    -side left -fill x -expand 1

  frame $logcanvas.up2.bydate
  label $logcanvas.up2.bydate.ldate -text "Committed" \
     -width 12 -anchor w -font $cvscfg(guifont)
  label $logcanvas.up2.bydate.rdate -text "--" \
     -anchor w -font $cvscfg(guifont)
  label $logcanvas.up2.bydate.lwho -text " by"  \
     -anchor w -font $cvscfg(guifont)
  label $logcanvas.up2.bydate.rwho -text "--" \
     -anchor w -font $textfont
  pack $logcanvas.up2.bydate -side top -fill x -expand 1
  pack $logcanvas.up2.bydate.ldate \
       $logcanvas.up2.bydate.rdate \
       $logcanvas.up2.bydate.lwho \
       $logcanvas.up2.bydate.rwho \
    -side left

  frame $logcanvas.up2.log
  label $logcanvas.up2.log.lcomment -text "Log" \
     -width 12 -anchor w -font $cvscfg(guifont)
  frame $logcanvas.up2.log.rlogfm
  text  $logcanvas.up2.log.rlogfm.rcomment -height 5 -width 75 \
     -yscrollcommand "$logcanvas.up2.log.rlogfm.yscroll set"
  scrollbar $logcanvas.up2.log.rlogfm.yscroll \
     -command "$logcanvas.up2.log.rlogfm.rcomment yview"
  pack $logcanvas.up2.log -side top -fill x -expand 1
  pack $logcanvas.up2.log.lcomment -side left
  pack $logcanvas.up2.log.rlogfm -side left -fill x -expand 1
  pack $logcanvas.up2.log.rlogfm.rcomment \
     -side left -fill x -expand 1
  pack $logcanvas.up2.log.rlogfm.yscroll \
     -side left -fill y

  # Pack the bottom before the middle so it doesnt disappear if
  # the window is resized smaller
  frame $logcanvas.down -relief groove -border 2
  pack $logcanvas.down -side bottom -fill x

  # This is a hidden entry that stores the local file name.
  entry $logcanvas.tlocalfile
  $logcanvas.tlocalfile delete 0 end
  $logcanvas.tlocalfile insert end $localfile

  # The canvas for the big picture
  canvas $logcanvas.canvas -relief sunken -border 2 \
    -yscrollcommand "$logcanvas.yscroll set" \
    -xscrollcommand "$logcanvas.xscroll set"
  scrollbar $logcanvas.xscroll -relief sunken -orient horizontal \
    -command "$logcanvas.canvas xview"
  scrollbar $logcanvas.yscroll -relief sunken \
    -command "$logcanvas.canvas yview"

  #
  # Create buttons
  #
  button $logcanvas.help -text "Help" -font $cvscfg(guifont) \
    -command log_browser
  button $logcanvas.view -image Fileview \
    -command "logcanvas_view $logcanvas"
  button $logcanvas.diff -image Diff \
    -command "logcanvas_diff $logcanvas"
  button $logcanvas.join -image Mergebranch \
      -command "logcanvas_join $localfile $logcanvas"
  button $logcanvas.delta -image Mergediff \
      -command "logcanvas_delta $localfile $logcanvas"
  # This has command -nop because it's configured later
  button $logcanvas.viewtags -image Tags -command "nop"
  button $logcanvas.quit -text "Close" -font $cvscfg(guifont) \
    -command "destroy $logcanvas"

  pack $logcanvas.help \
       $logcanvas.view \
       $logcanvas.diff \
       $logcanvas.join \
       $logcanvas.delta \
       $logcanvas.viewtags \
    -in $logcanvas.down -side left \
    -ipadx 1 -ipady 1 -fill x -expand 1
  pack $logcanvas.quit \
    -in $logcanvas.down -side right \
    -ipadx 1 -ipady 1 -fill x -expand 1

  if {$localfile == "no file"} {
    $logcanvas.view configure -state disabled
    $logcanvas.join configure -state disabled
    $logcanvas.delta configure -state disabled
  }

  set_tooltips $logcanvas.view \
     {"View a version of the file"}
  set_tooltips $logcanvas.diff \
     {"Compare two versions of the file"}
  set_tooltips $logcanvas.join \
     {"Merge branch to head"}
  set_tooltips $logcanvas.delta \
     {"Merge changes to head"}
  set_tooltips $logcanvas.viewtags \
     {"List the file\'s tags"}

  #
  # Put the canvas on to the display.
  #
  pack $logcanvas.xscroll -side bottom -fill x -padx 1 -pady 1
  pack $logcanvas.yscroll -side right -fill y -padx 1 -pady 1
  pack $logcanvas.canvas -fill both -expand 1

  $logcanvas.canvas delete all

  #
  # Window manager stuff.
  #
  wm minsize $logcanvas 1 1

  # Collect the history from the RCS log
  parse_cvslog $logcanvas $filelog
  # After we've parsed the log, we know the name of the file
  set fname [file tail [$logcanvas.up.rfname get]]
  set fname [string trimright $fname ",v"]
  wm title $logcanvas "CVS Log: $fname"

  # Sort it into order - this makes drawing the tree much easier
  set revlist [lsort -command sortrevs [array names revdate]]
 #puts $revlist

  # Now draw the revision tree
  set n 0
  foreach rev $revlist {
    set revprev [lindex $revlist [expr {$n-1}]]
    logcanvas_draw_box $logcanvas $rev $revprev
    incr n
  }
  # Find stray tags that aren't attached to revisions
  stray_tags $logcanvas

  $logcanvas.canvas yview moveto 0

  gen_log:log T "LEAVE"
  return $logcanvas
}

proc stray_tags { w } {
  global revdate
  global revlist
  global tags
  global cvscanv
  global const

  set sortags [lsort -command sortrevs [array names tags]]
  foreach t $sortags {
    set foundowner 0
    if {! [info exists revdate($t)]} {
      # If its revision doesn't exist, it may be a sticky tag
      #puts "$t    $tags($t) has no revision"
      set pospart [split $t "."]
      set poslen [llength $pospart]
      set nextolast [expr {[llength $pospart] - 2}]
      set num_nextolast [lindex $pospart $nextolast]
      if {$num_nextolast != 0} {
	# Forget it, it's just an orphaned tag
	#puts " not interested in $t because $num_nextolast is not 0"
	continue
      }
      set revb [expr {[lindex $pospart [expr {[llength $pospart] - 1}]]}]
      set pospart [lreplace $pospart $nextolast $nextolast $revb]
      set pospart [lreplace $pospart end end 1]
      set pospart [join $pospart "."]
      #puts " mangled brachrev $pospart"
      if {! [info exists revdate($pospart)]} {
	# A branch for this sticky doesn't seem to exist
	set pospart [split $t "."]
	set trunk [join [lrange $pospart 0 [expr {$nextolast - 1}]] "."]
	set stub "$trunk.$revb"
	#puts " => $tags($t) looks like a branch bud coming off $trunk"
	if { [info exists cvscanv(posx$trunk)] && \
	    [info exists cvscanv(posx$trunk)] } {
	  set xbegin [expr {$cvscanv(posx$trunk) + $cvscanv(${trunk}boxwidth)}]
	  incr xbegin 2
	  set ybegin [expr {$cvscanv(posy$trunk) + $cvscanv(boxy)}]
	  set xend [expr {$xbegin + 45}]
	  set yend [expr {$ybegin + 0}]
	  if {[info exists lasttrunk]} {
	    if {$trunk == $lasttrunk} {
	      if {[info exists score]} {
		incr score
	      } else {
		set score 1
	      }
	      incr yend [expr {$const(textheight) * $score}]
	    } else {
	      set score 0
	    }
	  }
	  $w.canvas create line \
	    $xbegin $ybegin $xend $yend \
	    -fill red
	  $w.canvas create oval \
	    $xend [expr {$yend - 3}] \
	    [expr {$xend + 6}] [expr {$yend + 3}] \
	    -outline red
	  $w.canvas create text \
	    [expr {$xend + 10}] $yend \
	    -text "$tags($t)" \
	    -font -*-Helvetica-Bold-R-Normal-*-11-* \
	    -anchor w -fill red
	  set lasttrunk $trunk
	}
      }
    }
  }
}

proc sortrevs {a b} {
  #
  # Proc for lsort -command, to sort revision numbers
  # Return -1 if a<b, 0 if a=b, and 1 if a>b
  # Leif E. I have modified the sort to operate thus
  # If two revisions are the same length, then their ascii collating sequence
  # is fine. If the length(a) < length(b), return -1, length(a) > length(b)
  # return 1.

  set alist [split $a "."]
  set blist [split $b "."]
  set alength [llength $alist]
  set blength [llength $blist]

  # if two revisions are the same length, return their ascii sort order
  if {$alength == $blength} {
    for {set i 0} {$i <= $alength} {incr i} {
       set A [lindex $alist $i]
       set B [lindex $blist $i]
       if {$A < $B} { return -1}
       if {$A > $B} { return 1}
    }
  }

  # Draw the parents before the children
  if {$alength < $blength} {
    return -1
  } elseif {$alength > $blength} {
    return 1
  } else {
    return 0
  }
}

proc ldelete { list value } {
  set ix [lsearch -exact $list $value]
  while {$ix >= 0} {
    set list [lreplace $list $ix $ix]
    set ix [lsearch -exact $list $value]
  }
  return $list
}

proc logcanvas_draw_box {logcanvas rev revprev} {
  #
  # Draws a box containing a revision of a file.
  #
  global cvscanv
  global const
  global tags
  global bndbox
  global revwho
  global revbranches
  global revlist

  # bndbox.
  # The semantic of max and min are
  # the bottom (relative to the screen) left most point of all text, version boxes,
  # labels, tags etc (in other words the lowest left most point of all graphical
  # symbols associated with a revision) is (xmin, ymin).
  # The upper right is (xmax, ymax). Thus, because in our (tkcvs) universe things drawn
  # higher up in the display are increasingly y-negative, if (ymin < y), then ymin is
  # reset to y, to make ymin reflect the new lower (on the display) element.
  # If (ymax > y), ymax is set to y to reflect the new higher element. xmin and xmax
  # follow the usual semantics ( if (xmin < x) then xmin really is smaller (more left)
  # than x, so no change is required.
  # This single axis logic reversal will eventually freak you out.
  # Leif E.

  set parts [split $rev "."]
  set depth [llength $parts]
  set branchn [expr {$depth - 2}]
  set prevparts [split $revprev "."]
  set prevdepth [llength $prevparts]

  # Default - if theres no previous revision, put it at the beginning index
  set parent ""
  set y [expr {-1 * $cvscanv(indy)}]; # lift it off the bottom
  set x $cvscanv(indx)
  # update bndbox coord's
  update_bndbox $rev $x $y origin
  set leaf [lindex $parts [expr {$depth - 1}]]
  set branch [join [lrange $parts 0 $branchn] "."]
  set prevbranch [join [lrange $prevparts 0 [expr {$prevdepth - 2}]] "."]
  set branchroot [join [lrange $parts 0 [expr {$depth - 3}]] "."]
  set branchnum [lrange $parts $branchn $branchn]
  set prevroot [join [lrange $prevparts 0 [expr {$prevdepth - 3}]] "."]
 #puts "In logcanvas_draw_box $rev\tleaf $leaf\tbranch $branch\tbranchnum $branchnum\troot $branchroot"

  # Else, find the parent and place this one after it
  if {[info exists cvscanv(posy$revprev)]} {
    if {$depth <= $prevdepth} {
      if {[string compare $branch $prevbranch] != 0} {
	# We're on a different branch now, so the parent isn't revprev.
	for {set int [expr {$leaf - 1}]} {$int >= 0} {incr int -1} {
	  foreach r $revlist {
	    #puts "comparing $r with $branch.$int"
	    if {[string compare $r "$branch.$int"] == 0} {
	      #puts "Found parent $r on same branch"
	      set parent $r
	      set x $cvscanv(posx$parent)
	      set y [expr {$cvscanv(posy$parent) - $cvscanv(gapy)}]
		      update_bndbox $rev $x $y origin
	      break
	    }
	  }
	  if {$parent != ""} {
	    break
	  }
	}
	if {$parent == ""} {
	  # Check our list of branches and see if this matches one of them.
	  foreach br [array names revbranches] {
	    foreach b $revbranches($br) {
	      if {[string compare $b $branch] == 0} {
		set parent $br
		#puts "  $b matched $branch, so parent is $br"
		set x [expr {$cvscanv(posx$parent) + $cvscanv(gapx)}]
		set off [make_space_for_tags $parent]
		set y [expr {$y - (12 * $off)}]

  # For visual preference reasons, we increase the slope for branches off second
  # generation branches (eg 1.9.2.2 is a second generation branch, 1.9 is a first
  # generation, 1.9.2.2.2.1 is third generation.
  # Deeper branching scheme need another method.
		set ylist [split $parent .]
		set ytail [lindex $ylist end]
		set ytail [expr {$ytail % 5}]
		set y [expr {$cvscanv(posy$parent) - [expr {$ytail * $cvscanv(gapy)}]}]

		set offset [check_xspace $x $y $rev]
		#puts "check_xspace returned $offset"
		set x [expr {$x + ($offset * $cvscanv(gapx))}]
		update_bndbox $rev $x $y origin
	      }
	    }
	  }
	  # We're on an already-existing branch although it's not the
	  # same as the immediately preceding revision.  Look for
	  # a parent at the same branching level.
	  if {$parent == ""} {
	    set shortlist ""
	    foreach r $revlist {
	      set l [llength [split $r "."]]
	      if {$l == $depth} {
		lappend shortlist $r
	      }
	    }
	   #puts "trying to find a parent for $rev from $shortlist"
	    foreach item $shortlist {
	      if {[sortrevs $item $rev] < 0} {
		set parent $item
		set x $cvscanv(posx$parent)
		set y [expr {$cvscanv(posy$parent) - $cvscanv(gapy)}]
		#puts "  picked parent $parent from list"
		update_bndbox $rev $x $y origin
		continue
	      }
	    }
	  }
	}
	if {$parent == ""} {
	  #puts "Didn't find a parent for $rev"
	}
      } else {
	# branch is the same as previous, so parent is $revprev
	set parent $revprev
	set x $cvscanv(posx$parent)
	set y [expr {$cvscanv(posy$parent) - $cvscanv(gapy)}]
	# If the parent has more than two tags, it needs more vertical space
	# this has been moved from after the check_xspace, as we weren't
	# letting that proc know the real position we wanted to draw in
	# Leif E.
	set off [make_space_for_tags $parent]
	set y [expr {$y - (12 * $off)}]
	update_bndbox $rev $x $y origin
	#puts "  parent $parent is the previous rev"
      }
    } else {
      # $depth > $prevdepth, meaning we have a new branch
      # Find parent by simply truncating rev number
      set parent [join [lrange $parts 0 [expr {$depth - 3}]] "."]
      #puts "  new branch, found parent $parent by truncation"
      # If it's on the main trunk, it doesn't need any special processing
      if {$depth == 2} {
	set x $cvscanv(posx$parent)
	set y [expr {$cvscanv(posy$parent) - $cvscanv(gapy)}]
	update_bndbox $rev $x $y origin
      } else {
	# Else, maybe theres a branch already occupying this position
	# so we have to offset it
	set x [expr {$cvscanv(posx$parent) + $cvscanv(gapx)}]
	set y [expr {$cvscanv(posy$parent) - $cvscanv(gapy)}]
	# If the parent has more than two tags, it needs more vertical space
	# this has been moved from after the check_xspace, as we weren't
	# letting that proc know the real position we wanted to draw in
	# Leif E.
	set off [make_space_for_tags $parent]
	set y [expr {$y - (12 * $off)}]

	set offset [check_xspace $x $y $rev]
	#puts "check_xspace returned $offset"
	set x [expr {$x + ($offset * $cvscanv(gapx))}]
	    update_bndbox $rev $x $y origin
      }
    }
  }
  #if {$parent != ""} {
   #puts " parent $parent"
  #}

  # draw the box and remember its position
  #puts "About to enter logcanvas_rectangle for $rev"
  logcanvas_rectangle $logcanvas $rev $x $y $parent
  set cvscanv(posx$rev) $x
  set cvscanv(posy$rev) $y
}

proc check_xspace {x y rev} {
  #
  # See if we have to offset a branch to the right.
  # Return number of columns to offset by.
  #
  global cvscanv

 #puts "In check_xspace \{$x $y $rev\}"
  set offsetcolumns 0
  set stack ""
  # Collect all the x position data
  foreach xb [array names cvscanv] {
    if {[string match "posx*" $xb]} {
      lappend stack $xb
    }
  }

  # sometimes the offset algorithm is sensitive to the order that
  # the versions are placed in the stack. Remove this sensitivity
  # with a sort - default ascii increasing is fine. Leif E.

  set stack [lsort $stack]

  set ymin 0
  set ymax -1000000
  # Find the things with the same x position and check their bounding box status
  foreach xb $stack {
    set d [string range $xb 4 end]
    if {$x == $cvscanv(posx$d)} {
      # We found something else with the same x position
      #puts "check_xoffset::ymin $ymin\tymax $ymax"
      #puts "check_xoffset::Match on X positions: x $x\ty $y\tposx($d): $cvscanv(posx$d)\tposy($d): $cvscanv(posy$d)"

      if {$cvscanv(posy$d) < $ymin} {
	set oldymin $ymin
	set ymin $cvscanv(posy$d)
      }
      if {$cvscanv(posy$d) > $ymax} {
	set oldymax $ymax
	set ymax $cvscanv(posy$d)
      }
      # Check if the potential y pos will overwrite an existing box
      if {$y >= $ymin && $y <= $ymax} {
	#puts "check_xoffset::Incr xoffset, due to x and y collisions"
	incr offsetcolumns
	set x [expr {$x + $cvscanv(gapx)}]
	if {[check_xspace $x $y $rev] == 0} {
	  continue
	} else {
	  incr offsetcolumns
	  set x [expr {$x + $cvscanv(gapx)}]
	}
      }
    }
  }
  return $offsetcolumns
}

proc make_space_for_tags {parent} {
  # If the parent has more than two tags, it needs more vertical space
  # this has been moved from after the check_xspace, as we weren't
  # letting that proc know the real position we wanted to draw in
  # Leif E.
 #puts "make_space_for_tags $parent"

  global tags
  set extratags 0
  if {[info exists tags($parent)]} {
    set ntags [llength $tags($parent)]
    if {$ntags > 2} {
      set extratags [expr {$ntags - 2}]
    }
  }
  return $extratags
}

proc logcanvas_rectangle {logcanvas rev x y revprev} {
  #
  # Breaks out some of the code from the logcanvas_draw_box procedure.
  # Change the order sightly. Work out the width of the text to go in the box first,
  # then draw a box high and wide enough. Update bndbox.
  #
  global cvscanv
  global cvscfg
  global const
  global tags
  global bndbox
  global revwho
  global revdate
  global revcomment
  global revlist
  upvar x xpos

  #puts "In logcanvas_rectangle \{logcanvas $rev $x $y $revprev \}"

  set parts [split $rev "."]
  set prevparts [split $revprev "."]

  # Put the version number and user in the box. Store the widths, so we can draw the box
  # wide enough.
  $logcanvas.canvas create text \
    [expr {$x + 4}] [expr {$y + 2}] \
    -anchor nw -text $rev  \
    -tags v$rev

  # To make the drawing of the version boxes more regular, we split the version number
  # ,using the period as the tokeniser, and obtain the length of the resulting list.
  # We then multiple by a fixed factor. This way, all x.x revsions get a rectangle 2
  # standard factors wide, x.x.x.x revisions get a box 4 standard factors wide. The factor
  # is stored in const(xfactor). If the whorev_len is longer than the revision number
  # length * it factor, the box is made the who len wide. This may cause some slight
  # visual coarseness. Leif E

  set versnum_len [llength [split $rev "."]]

  $logcanvas.canvas create text \
    [expr {$x + 4}] [expr {$y + 14}] \
    -anchor nw -text $revwho($rev) \
    -tags v$rev

  set revwho_len [font measure {Helvetica 12} \
  -displayof $logcanvas.canvas $revwho($rev)]

  # set width of text box to widest string
  if {$revwho_len < [expr {$versnum_len * $const(xfactor)}]} {
    set box_width [expr {$versnum_len * $const(xfactor)}]
  } else {
    set box_width $revwho_len
  }

  # save width of this revsions box
  set cvscanv(${rev}boxwidth) $box_width

  # draw the box
  set boxid [$logcanvas.canvas create rectangle \
    $x $y \
    [expr {$x + $cvscanv(${rev}boxwidth)}] [expr {$y + $cvscanv(boxy)}] \
    -width 3 \
    -fill gray90 \
    -tags v$rev]
  $logcanvas.canvas lower $boxid

  # draw the tags
  if {[info exists tags($rev)]} {
    set n 0
    foreach tag $tags($rev) {
      if {[info exists cvscfg(tagcolour,$tag)]} {
	set tagcolour $cvscfg(tagcolour,$tag)
      } else {
	set tagcolour black
      }
      $logcanvas.canvas create text \
	[expr {$x + $cvscanv(${rev}boxwidth) + 2}] [expr {$y + $cvscanv(boxy) - $n}] \
	-anchor sw -text $tag \
	-fill $tagcolour \
	-tags v$rev
      set tagwidth [font measure {Helvetica 12} -displayof $logcanvas.canvas $tag]
      incr n $const(textheight)
    }
  }
 #puts "About to add sticky tags for branches, point is $rev"
  # Mangle the rev number to become the branch number
  set pospart [split $rev "."]
  # do this only for the first revision on a branch
  if {[lindex $pospart end] == 1} {
    set poslen [llength $pospart]
    set revb [lindex $pospart [expr {[llength $pospart] - 2}]]
    set pospart [lreplace $pospart end end $revb]
    set pospart [lreplace $pospart [expr {$poslen - 2}] [expr {$poslen - 2}] 0]
    # revision list is now the branch number, restore to a string
    set pospart [join $pospart "."]
    # arbitrary constant chosen to push sticky branch label under the box
    # there is almost certainly a better way (an array var somewhere ??)
    set n 15
    foreach tag [array names tags] {
      if {[string match $pospart $tag]} {
	set parts [split $tag "."]
	# Rip last number off revision string
	set stub [join [lreplace [split $rev "."] end end] "."]
	# For each sticky tag for this branch, write it on the canvas
	foreach t $tags($tag) {
	  $logcanvas.canvas create text \
	      $x \
	      [expr {$y + $n + $cvscanv(boxy)}] \
	      -anchor sw -text "($stub) $t" \
	      -font -*-Helvetica-Bold-R-Normal-*-12-* \
	      -fill blue \
	      -tags v$rev
	  set tagtext "($stub) $t"
	  set tagwidth [font measure {Helvetica 12} -displayof $logcanvas.canvas $tagtext]
	  incr n $const(textheight)
	}
      }
    }
  }

  # now calculate the bndbox using the canvas bbox function
  set bbox [$logcanvas.canvas bbox v$rev]

  update_bndbox $rev [lindex $bbox 0] [lindex $bbox 1] upleft
  update_bndbox $rev [lindex $bbox 2] [lindex $bbox 1] upright
  update_bndbox $rev [lindex $bbox 0] [lindex $bbox 3] loleft
  update_bndbox $rev [lindex $bbox 2] [lindex $bbox 3] loright

  # draw the bounding box, if you want to see the collision regions.
  # set/append Marcel's $cvscfg(log_classes) to contain a [dD] .
  if {$cvscfg(logging) && [regexp -nocase {d} $cvscfg(log_classes)]} {
    $logcanvas.canvas create rectangle \
    $bndbox(${rev}xmin) $bndbox(${rev}ymax) \
    $bndbox(${rev}xmax) $bndbox(${rev}ymin) \
    -width 1 -outline red\
    -tags v$rev
  }


  # check if we overlap or underlie anybody.
  # we will loop through overlaps and overhead collisions until neither moves
  set overmove  "true"
  set undermove "true"
  while {[string compare $overmove  "true"] == 0
      || [string compare $undermove "true"] == 0} {
    if {![regexp -nocase {\.1$} $rev]} {
      # only attempt comparisons for parent, as children follow without question
      break
    }
    set overmove "false"
    set bbox [$logcanvas.canvas bbox v$rev]
    set overlist [eval {$logcanvas.canvas find overlapping } $bbox]
    #if we overlap anyone, analyse the overlaps. If overlist is empty, we skip this
    set we_moved "false"
    foreach overindex $overlist {
      #foreach one we overlie, move if req'd
      #get the tags for the item we overlay
      set taglist [ $logcanvas.canvas gettags $overindex]
      # remove ourselves from the list of tags, so that any remaining tags
      # belong to other revisions.
      set taglist [ldelete $taglist v$rev]
      # if we have tags, they are for a revision other than us, and we must move 5
      # pixels to the right of the conflict.
      foreach tagindex $taglist {
	set tagindex [string trimleft $tagindex v]
	set bbox [$logcanvas.canvas bbox v$tagindex]
	#work out how far to go to the right
	set xoffset [expr {$bndbox(${tagindex}xmax) - $x}]
	incr xoffset 5; # just to avoid problems
	incr x $xoffset
	#reset return value
	set xpos $x
	# record our new position
	update_bndbox $rev $x $y translate $xoffset
	# move there
	$logcanvas.canvas move v$rev $xoffset 0
	# cause we moved, set flag and exit loop
	set overmove "true"
	break
      }
      # if we moved for one of the overlays, break out and look for
      # underlying conflictions
      if {[string compare "true" $overmove] == 0} {
	break
      }
    }
    #we may underlie someone above us.
    # check if anybody above us would conflict with our children, resulting in unevenness
    set undermove "false"
    foreach revitem $revlist {
      if {$rev == $revitem} {
	break;
      }
      set move_req "false"
      if {$bndbox(${rev}ymin) < $bndbox(${revitem}ymin)} {
	continue;
      }
      if {$bndbox(${revitem}xmin) >= $bndbox(${rev}xmin) && $bndbox(${revitem}xmin) < $bndbox(${rev}xmax)} {
	set move_req "true"
      }
      if {$bndbox(${revitem}xmax) >= $bndbox(${rev}xmin) && $bndbox(${revitem}xmax) < $bndbox(${rev}xmax)} {
	set move_req "true"
      }
      if {$bndbox(${revitem}xmin) <= $bndbox(${rev}xmin) && $bndbox(${revitem}xmax) > $bndbox(${rev}xmax)} {
	set move_req "true"
      }
      if {[string compare "true" $move_req] == 0}       {
	set xoffset [expr {$bndbox(${revitem}xmax) - $x}]
	incr xoffset 5
	# perform the move
	$logcanvas.canvas move v$rev $xoffset 0
	incr x $xoffset
	#reset return value
	set xpos $x
	update_bndbox $rev $x $y translate $xoffset
	set undermove "true"
      }
    }
  }

  # draw connecting line
  if {[info exists cvscanv(posx$revprev)]} {
    if {[llength $parts] > [llength $prevparts]} {
      set xbegin [expr {$cvscanv(posx$revprev) + $cvscanv(${revprev}boxwidth)}]
      # end line a standard distance from the bottom left edge of the prev revision box
      set xend $xpos
      set ybegin [expr {$cvscanv(posy$revprev) + ($cvscanv(boxy)/2)}]
    } else {
      # create line a standard distance from the top left edge of the revision box
      set xbegin [expr {$x + $const(xfactor)}]
      set ybegin $cvscanv(posy$revprev)
      # end line a standard distance from the bottom left edge of the prev revision box
      set xend [expr {$x + $const(xfactor)}]
    }
    set yend [expr {$y + $cvscanv(boxy)}]
    # add arrowhead
    $logcanvas.canvas create line $xbegin $ybegin $xend $yend -arrow last
  }

  # stretch canvas if reqd
  if {$cvscanv(xlow) >  $bndbox(${rev}xmin)} {
    set cvscanv(xlow) $bndbox(${rev}xmin)
    incr cvscanv(xlow) -5
  }
  if {$cvscanv(xhigh) <  $bndbox(${rev}xmax)} {
    set cvscanv(xhigh) $bndbox(${rev}xmax)
    incr cvscanv(xhigh) 5
  }

  # ylow is the top edge of the canvas. It is the most negative (least) value.
  if {$cvscanv(ylow) >  $bndbox(${rev}ymax)} {
    set cvscanv(ylow) $bndbox(${rev}ymax)
    incr cvscanv(ylow) -5
  }
  # yhigh is the bottom edge of the canvas. It is the least negative (most) value.
  if {$cvscanv(yhigh) <  $bndbox(${rev}ymin)} {
    set cvscanv(yhigh) $bndbox(${rev}ymin)
    incr cvscanv(yhigh) 5
  }

  $logcanvas.canvas configure \
    -scrollregion "$cvscanv(xlow) $cvscanv(ylow) \
    $cvscanv(xhigh) $cvscanv(yhigh)"

  # Bind to the tag.

  # RAMSAN change

  $logcanvas.canvas bind v$rev <ButtonPress-1> \
    "$logcanvas.up1.rev.rvers delete 0 end
    $logcanvas.up1.rev.rvers insert end $rev
    $logcanvas.up1.bydate.rwho configure -text $revwho($rev)
    $logcanvas.up1.bydate.rdate configure -text $revdate($rev)
    $logcanvas.up1.log.rlogfm.rcomment delete 1.0 end
    $logcanvas.up1.log.rlogfm.rcomment insert end [list $revcomment($rev)]"
  $logcanvas.canvas bind v$rev <ButtonPress-2> \
    "$logcanvas.up2.rev.rvers delete 0 end
    $logcanvas.up2.rev.rvers insert end $rev
    $logcanvas.up2.bydate.rwho configure -text $revwho($rev)
    $logcanvas.up2.bydate.rdate configure -text $revdate($rev)
    $logcanvas.up2.log.rlogfm.rcomment delete 1.0 end
    $logcanvas.up2.log.rlogfm.rcomment insert end [list $revcomment($rev)]"
  $logcanvas.canvas bind v$rev <ButtonPress-3> \
    "$logcanvas.up2.rev.rvers delete 0 end
    $logcanvas.up2.rev.rvers insert end $rev
    $logcanvas.up2.bydate.rwho configure -text $revwho($rev)
    $logcanvas.up2.bydate.rdate configure -text $revdate($rev)
    $logcanvas.up2.log.rlogfm.rcomment delete 1.0 end
    $logcanvas.up2.log.rlogfm.rcomment insert end [list $revcomment($rev)]"
}

proc nocollisions{collist rev} {

}

proc logcanvas_view {logcanvas} {
  #
  # Views the selected version.
  #
  gen_log:log T "ENTER ($logcanvas)"
  set ver1 [$logcanvas.up1.rev.rvers get]
  set localfile [$logcanvas.tlocalfile get]
    cvs_view_r $ver1 $localfile
  gen_log:log T "LEAVE"
  }

proc logcanvas_diff {logcanvas} {
  #
  # Diffs two versions.
  #
  gen_log:log T "ENTER ($logcanvas)"
  set ver1 [$logcanvas.up1.rev.rvers get]
  set ver2 [$logcanvas.up2.rev.rvers get]
  set localfile [$logcanvas.tlocalfile get]
  if {$localfile != "no file"} {
    cvs_diff_r $ver1 $ver2 $localfile
  } else {
    set fname [$logcanvas.up.rfname get]
    rcs_filediff $fname $ver1 $ver2
  }
  gen_log:log T "LEAVE"
}

proc logcanvas_join {localfile logcanvas} {
  #
  # Joins a branch version to the head version.
  #
  gen_log:log T "ENTER ($localfile $logcanvas)"
  set ver1 [$logcanvas.up1.rev.rvers get]
  set versions [split $ver1 "."]
  set depth [llength $versions]
  if {$depth < 4} {
    cvsfail "Please select a branch version for this function!"
    return 1
  }

  cvs_join $localfile $ver1
  gen_log:log T "LEAVE"
}

proc logcanvas_delta {localfile logcanvas} {
  #
  # Merges changes in the delta between two versions to the head
  # version.
  #
  gen_log:log T "ENTER ($localfile $logcanvas)"
  set ver1 [$logcanvas.up1.rev.rvers get]
  set ver2 [$logcanvas.up2.rev.rvers get]

  cvs_delta $localfile $ver1 $ver2
  gen_log:log T "LEAVE"
}

proc parse_cvslog {logcanvas filelog} {
  #
  # Splits the rcs file up and parses it using a simple state machine.
  #
  global revdate
  global revwho
  global revcomment
  global tags
  global revbranches

  gen_log:log T "ENTER ($logcanvas <filelog suppressed>)"
  set loglist [split $filelog "\n"]
  set logstate "rcsfile"
  foreach logline $loglist {
    switch -exact -- $logstate {
      "rcsfile" {
	# Look for the first text line which should give the file name.
	set fileline [split $logline]
	if {[lindex $fileline 0] == "RCS"} {
	  $logcanvas.up.rfname delete 0 end
	  $logcanvas.up.rfname insert end [lindex $fileline 2]
	  set logstate "tags"
	  set taglist ""
	  continue
	}
      }
      "tags" {
	# Any line with a tab leader is a tag
	if { [string index $logline 0] == "\t" } {
	  set taglist "$taglist$logline\n"
	  set tagitems [split $logline ":"]
	  set tagstring [string trim [lindex $tagitems 0]]
	  set tagrevision [string trim [lindex $tagitems 1]]
	  lappend tags($tagrevision) $tagstring
	} else {
	  if {$logline == "description:"} {
	    # No more tags after this point
	    $logcanvas.viewtags configure \
	      -command "view_output Tags \"$taglist\""
	    set logstate "searching"
	    continue
	  }
	  if {$logline == "----------------------------"} {
	    # Oops, missed something.
	    $logcanvas.viewtags configure \
	      -command "view_output Tags \"$taglist\""
	    set logstate "revision"
	    continue
	  }
	}
      }
      "searching" {
	# Look for the line that starts a revision message.
	if {$logline == "----------------------------"} {
	  set logstate "revision"
	  continue
	}
      }
      "revision" {
	# Look for a revision number line
	set revline [split $logline]
	set revnum [lindex $revline 1]
	set logstate "date"
      }
      "date" {
	# Look for a date line.  This also has the name of the author.
	set dateline [split $logline]
	set revdate($revnum) [lindex $dateline 1]

	# ramsan change
	#set who [lindex $dateline 5]
	#set revwho($revnum) [string range $who 0 [expr {[string length $who] - 2}]]
		
	set who ""
	regexp {author:\s*(\w+)} $logline {} who
	set revwho($revnum) $who    
	

	set revcomment($revnum) ""
	set revbranches($revnum) ""
	set logstate "logmessage"
      }
      "logmessage" {
	# See if there are branches off this revision
	if {[string match "branches:*" $logline]} {
	  set br [split $logline]
	  set branches [lrange $logline 1 [llength $logline]]
	  foreach br $branches {
	    lappend revbranches($revnum) [string trimright $br ";"]
	  }
	  continue
	}
	# Read the log message which follows the date line.
	if {$logline == "----------------------------"} {
	  set logstate "revision"
	  continue
	} elseif {[regexp {^==*$} $logline]} {
	  set logstate "terminated"
	  continue
	}
	# Process a revision log line
	regsub -all "\"" $logline "'" newline
	set revcomment($revnum) "$revcomment($revnum)$newline\n"
      }
      "terminated" {
	# ignore any further lines
	continue
      }
    }
  }
}

proc update_bndbox {rev x y vertex {offset 0} } {
  # update_bndbox check's the current limits of rev's bounding-box, and if the values
  # of the corner dictated by vertex indicate they require adjustment, we do so
  # origin sets the origin of our bounding box for this revision
  # Leif E.

  global bndbox

  #puts "In update_bndbox $rev $x $y $vertex"

  switch -exact -- $vertex {
    origin  { ; #reset origin of bounding box
	      set bndbox(${rev}xmin) $x
	      set bndbox(${rev}xmax) $x
	      set bndbox(${rev}ymin) $y
	      set bndbox(${rev}ymax) $y
	    }
    loleft { ; # we adjust xmin if x < xmin, and ymin if y > ymin
	      if { $x < $bndbox(${rev}xmin)} {set bndbox(${rev}xmin) $x}
	      if { $y > $bndbox(${rev}ymin)} {set bndbox(${rev}ymin) $y}
	    }
    upleft  { ; # we adjust xmin if x < xmin, and ymax if y < ymax
	      if { $x < $bndbox(${rev}xmin)} {set bndbox(${rev}xmin) $x}
	      if { $y < $bndbox(${rev}ymax)} {set bndbox(${rev}ymax) $y}
	    }
    upright { ; # we adjust xmax if x > xmax, and ymax if y < ymax
	      if { $x > $bndbox(${rev}xmax)} {set bndbox(${rev}xmax) $x}
	      if { $y < $bndbox(${rev}ymax)} {set bndbox(${rev}ymax) $y}
	    }
    loright { ; # we adjust xmax if x > xmax, and ymin if y > ymin
	      if { $x > $bndbox(${rev}xmax)} {set bndbox(${rev}xmax) $x}
	      if { $y > $bndbox(${rev}ymin)} {set bndbox(${rev}ymin) $y}
	    }
    translate { ; #move the xmin and max settings by offset
		set bndbox(${rev}xmin) [expr {$bndbox(${rev}xmin) + $offset}]
		set bndbox(${rev}xmax) [expr {$bndbox(${rev}xmax) + $offset}]
	    }
    default { ; # egad's!!
	      error "bogus vertex value: $vertex"
	    }
  } ; # switch
}
