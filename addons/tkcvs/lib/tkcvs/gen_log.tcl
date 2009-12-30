#
# Debugging trace functions adapted from set by Marcel Koelewijn 
#

proc gen_log:init {} {
  global cvscfg

  toplevel .trace
  wm protocol .trace WM_DELETE_WINDOW gen_log:quit
  text .trace.text -setgrid yes -relief sunken -border 2 \
      -yscroll ".trace.scroll set"
  scrollbar .trace.scroll -relief sunken \
      -command ".trace.text yview"
  button .trace.quit -text "Stop Tracing" -command gen_log:quit

  pack .trace.quit -side bottom -fill x
  pack .trace.scroll -side right -fill y
  pack .trace.text -fill both -expand 1


  .trace.text tag configure tagC -foreground purple
  .trace.text tag configure tagF -foreground darkgreen
  .trace.text tag configure tagT -foreground black
  .trace.text tag configure tagD -foreground red

  wm title .trace "TkCVS Trace"
}

proc gen_log:log { class string } { 
  global cvscfg

  # check class+level first, if no logging required, skip
  if {$cvscfg(logging) && [string match "*\[$class\]*" $cvscfg(log_classes)]} {
    set callerlevel [expr {[info level] - 1}]
    if { $callerlevel == 0 } {
      # called from the toplevel
      set callerid "toplevel"
    } else {
      set callerid [lindex [info level $callerlevel] 0]
    }
    .trace.text insert end [format "\[%s] %s\n" $callerid "$string"] tag$class
    .trace.text yview end
    #update idletasks
  }
}

proc gen_log:quit { } {
  global cvscfg

  set cvscfg(logging) 0
  if {[winfo exists .trace]} {
    destroy .trace
  }
}

proc gen_log:changeclass { } {
  global cvscfg
  global logclass

  set cvscfg(log_classes) ""
  foreach c [array names logclass] {
    append cvscfg(log_classes) $logclass($c)
  }
}
