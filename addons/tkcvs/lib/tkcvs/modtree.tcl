# Adapted from tree.tcl released under GPL by
#
# Copyright (C) 1997,1998 D. Richard Hipp
#
# $Revision: 1.1.1.1 $
#

#
# Create a new tree widget.  $args become the configuration arguments to
# the canvas widget from which the tree is constructed.
#
proc ModTree:create {w args} {
  global Tree

  gen_log:log T "ENTER ($w $args)"

  eval canvas $w.tree $args
  eval canvas $w.col $args
  #scrollbar $w.tree.xscroll -orient horizontal -command "$w.tree xview"
  #scrollbar $w.col.xscroll -orient horizontal -command "$w.col xview"
  scrollbar $w.yscroll -orient vertical -command "ModTree:scroll_windows $w"

  $w.col configure -width 300

  $w.tree configure -yscrollcommand "$w.yscroll set"
  $w.col configure -yscrollcommand "$w.yscroll set"
  pack $w.tree -side left -fill both -expand yes
  #pack $w.tree.xscroll -side bottom -fill x
  pack $w.yscroll -side right -fill y
  pack $w.col -side right -fill both -expand yes
  #pack $w.col.xscroll -side bottom -fill x

  ModTree:dfltconfig $w /
  set Tree(vsize) 16
  ModTree:buildwhenidle $w 
  set Tree($w:selection) {}
  set Tree($w:selidx) {}
  gen_log:log T "LEAVE"
}

# Initialize a element of the tree.
# Internal use only
#
proc ModTree:dfltconfig {w v} {
  global Tree

  gen_log:log T "ENTER ($w $v)"
  set Tree($w:$v:children) {}
  set Tree($w:$v:open) 0
  set Tree($w:$v:icon) {}
  set Tree($w:$v:tags) {}
  gen_log:log T "LEAVE"
}

#
# Pass configuration options to the tree widget
#
proc ModTree:config {w args} {
  gen_log:log T "ENTER ($w $args)"
  eval $w.tree config $args
  eval $w.col config $args
  gen_log:log T "LEAVE"
}

#
# Insert a new element $v into the tree $w.
#
proc ModTree:newitem {w v name title args} {
  global Tree

  gen_log:log T "ENTER ($w $v $name $title $args)"
  set dir [file dirname $v]
  set n [file tail $v]

  if {![info exists Tree($w:$dir:open)]} {
    error "parent item \"$dir\" is missing"
  }
  set i [lsearch -exact $Tree($w:$dir:children) $n]
  if {$i>=0} {
    error "item \"$v\" already exists"
  }
  lappend Tree($w:$dir:children) $n
  set Tree($w:$dir:children) [lsort $Tree($w:$dir:children)]
  ModTree:dfltconfig $w $v
  set Tree($w:$v:name) $name
  set Tree($w:$v:title) $title
  foreach {op arg} $args {
    switch -exact -- $op {
      -image {set Tree($w:$v:icon) $arg}
      -tags {set Tree($w:$v:tags) $arg}
    }
  }
  ModTree:buildwhenidle $w 
  gen_log:log T "LEAVE"
}

#
# Delete element $v from the tree $w.  If $v is /, then the widget is
# deleted.
#
proc ModTree:delitem {w v} {
  global Tree

  gen_log:log T "ENTER ($w $v)"
  if {![info exists Tree($w:$v:open)]} return
  if {[string compare $v /]==0} {
    # delete the whole widget
    catch {destroy $w.tree}
    catch {destroy $w.col}
    catch {destroy $w.yscroll}
    foreach t [array names Tree $w:*] {
      unset Tree($t)
    }
    return
  }
  if {[info exists Tree($w:$v:children)]} {
    foreach c $Tree($w:$v:children) {
      catch {ModTree:delitem $w $v/$c}
    }
    unset Tree($w:$v:open)
    unset Tree($w:$v:children)
    unset Tree($w:$v:icon)
    set dir [file dirname $v]
    set n [file tail $v]
    set i [lsearch -exact $Tree($w:$dir:children) $n]
    if {$i>=0} {
      set Tree($w:$dir:children) [lreplace $Tree($w:$dir:children) $i $i]
    }
  }
  ModTree:buildwhenidle $w 
  gen_log:log T "LEAVE"
}

#
# Change the selection to the indicated item
#
proc ModTree:setselection {tree v} {
  global Tree

  gen_log:log T "ENTER ($tree $v)"
  set w [winfo parent $tree]
  set Tree($w:selection) $v
  ModTree:drawselection $w
  gen_log:log T "LEAVE"
}

# 
# Retrieve the current selection
#
proc ModTree:getselection w {
  global Tree

  gen_log:log T "ENTER ($w)"
  gen_log:log T "RETURN Tree($w:selection)"
  return $Tree($w:selection)
}

#
# Bitmaps used to show which parts of the tree can be opened.
#
set maskdata "#define solid_width 9\n#define solid_height 9"
append maskdata {
  static unsigned char solid_bits[] = {
   0xff, 0x01, 0xff, 0x01, 0xff, 0x01, 0xff, 0x01, 0xff, 0x01, 0xff, 0x01,
   0xff, 0x01, 0xff, 0x01, 0xff, 0x01
  };
}
set data "#define open_width 9\n#define open_height 9"
append data {
  static unsigned char open_bits[] = {
   0xff, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x7d, 0x01, 0x01, 0x01,
   0x01, 0x01, 0x01, 0x01, 0xff, 0x01
  };
}
image create bitmap ModTree:openbm -data $data -maskdata $maskdata \
  -foreground black -background white
set data "#define closed_width 9\n#define closed_height 9"
append data {
  static unsigned char closed_bits[] = {
   0xff, 0x01, 0x01, 0x01, 0x11, 0x01, 0x11, 0x01, 0x7d, 0x01, 0x11, 0x01,
   0x11, 0x01, 0x01, 0x01, 0xff, 0x01
  };
}
image create bitmap ModTree:closedbm -data $data -maskdata $maskdata \
  -foreground black -background white

# Internal use only.
# Draw the tree on the canvas
proc ModTree:build {w} {
  global Tree

  gen_log:log T "ENTER ($w)"
  $w.tree delete all
  $w.col delete all
  catch {unset Tree($w:buildpending)}
  set Tree($w:y) 30
  ModTree:buildlayer $w / $Tree(vsize)
  $w.tree config -scrollregion [$w.tree bbox all]
  # Use tree's bbox for col, because col's is a little shorter
  # but we need to keep them in sync
  $w.col config -scrollregion [$w.tree bbox all]
  ModTree:drawselection $w
  gen_log:log T "LEAVE"
}

# Internal use only.
# Build a single layer of the tree on the canvas.  Indent by $in pixels
proc ModTree:buildlayer {w v in} {
  global Tree
  global cvscfg

  gen_log:log T "ENTER ($w $v $in)"
  if {$v=="/"} {
    set vx {}
  } else {
    set vx $v
  }
  set start [expr {$Tree($w:y)-10}]
  foreach c $Tree($w:$v:children) {
    set y $Tree($w:y)
    incr Tree($w:y) [expr {$Tree(vsize)+3}]
    $w.tree create line $in $y [expr {$in+$Tree(vsize)}] $y -fill gray50 
    set icon $Tree($w:$vx/$c:icon)
    set taglist x
    foreach tag $Tree($w:$vx/$c:tags) {
      lappend taglist $tag
    }
    set x [expr {$in+12}]
    if {[string length $icon]>0} {
      set k [$w.tree create image $x $y -image $icon -anchor w -tags $taglist]
      incr x 24
      set Tree($w:tag:$k) $vx/$c
    }
    set lbl $Tree($w:$vx/$c:name)
    set j [$w.tree create text $x $y -text $lbl \
       -font $cvscfg(listboxfont) -anchor w -tags $taglist]
    if {[info exists Tree($w:$vx/$c:title)]} {
      set k [$w.col create text [expr {$x - $Tree(vsize) - 22}] $y \
         -text $Tree($w:$vx/$c:title) \
         -font $cvscfg(listboxfont) -anchor w]
    }
    set Tree($w:tag:$j) $vx/$c
    set Tree($w:$vx/$c:tag) $j
    if {[string length $Tree($w:$vx/$c:children)]} {
      if {$Tree($w:$vx/$c:open)} {
         set j [$w.tree create image $in $y -image ModTree:openbm]
         $w.tree bind $j <1> "set Tree($w:$vx/$c:open) 0; ModTree:build $w"
         ModTree:buildlayer $w $vx/$c [expr {$in+$Tree(vsize)+8}]
      } else {
         set j [$w.tree create image $in $y -image ModTree:closedbm]
         $w.tree bind $j <1> "set Tree($w:$vx/$c:open) 1; ModTree:build $w"
      }
    }
  }
  if {![info exists y]} {return}
  set j [$w.tree create line $in $start $in [expr {$y+1}] -fill gray50 ]
  $w.tree lower $j
  gen_log:log T "LEAVE"
}

# Open a branch of a tree
#
proc ModTree:open {w v} {
  global Tree

  if {[info exists Tree($w:$v:open)] && $Tree($w:$v:open)==0
      && [info exists Tree($w:$v:children)] 
      && [string length $Tree($w:$v:children)]>0} {
    set Tree($w:$v:open) 1
    ModTree:build $w 
  }
  gen_log:log T "LEAVE"
}

proc ModTree:close {w v} {
  global Tree
  if {[info exists Tree($w:$v:open)] && $Tree($w:$v:open)==1} {
    set Tree($w:$v:open) 0
    ModTree:build $w 
  }
  gen_log:log T "LEAVE"
}

# Internal use only.
# Draw the selection highlight
proc ModTree:drawselection w {
  global Tree
  global cvscfg

  gen_log:log T "ENTER ($w)"
  if {[info exists Tree($w:selidx)]} {
    $w.tree delete $Tree($w:selidx)
  }
  set v $Tree($w:selection)
  if {[string length $v]==0} return
  if {![info exists Tree($w:$v:tag)]} return
  set bbox [$w.tree bbox $Tree($w:$v:tag)]
  if {[llength $bbox]==4} {
    set i [eval $w.tree create rectangle $bbox -fill $cvscfg(glb_highlight)]
    set Tree($w:selidx) $i
    $w.tree lower $i
  } else {
    set Tree($w:selidx) {}
  }
  gen_log:log T "LEAVE"
}

# Internal use only
# Call ModTree:build then next time we're idle
proc ModTree:buildwhenidle {w} {
  global Tree

  gen_log:log T "ENTER ($w)"
  if {![info exists Tree($w:buildpending)]} {
    set Tree($w:buildpending) 1
    after idle "ModTree:build $w"
  }
  gen_log:log T "LEAVE"
}

#
# Return the full pathname of the label for widget $w that is located
# at real coordinates $x, $y
#
proc ModTree:labelat {tree x y} {
  global Tree

  gen_log:log T "ENTER ($tree $x $y)"
  set x [$tree canvasx $x]
  set y [$tree canvasy $y]
  set w [winfo parent $tree]

  foreach m [$tree find overlapping $x $y $x $y] {
    if {[info exists Tree($w:tag:$m)]} {
      gen_log:log T "RETURN Tree($w:tag:$m)"
      return $Tree($w:tag:$m)
    }
  }
  gen_log:log T "RETURN \"\""
  return ""
}

proc ModTree:scroll_windows {w args} {

  #gen_log:log T "ENTER ($w $args)"
  eval $w.tree yview $args
  eval $w.col yview $args
}
