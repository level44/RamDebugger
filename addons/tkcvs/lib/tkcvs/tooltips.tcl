#
# tooltips version 0.1
# Paul Boyer
# Science Applications International Corp.
#
# THINGS I'D LIKE TO DO:
# 1. make a widget called "tooltip_button" which does it all
# and takes name and helptext as arguments in addition to all 
# button args
# 2. Keep visibility of tooltip always on top
# 3. Must be a better way to maintain button presses than rebinding?
#   Because I don't want to explicitly handle all possible bindings
#   such as <Button-2> etc
# 4. Allow for capability for status window at bottom of a frame
#  that gets the status of the selected icon


##############################
# set_tooltips gets a button's name and the tooltip string as
# arguments and creates the proper bindings for entering
# and leaving the button

proc set_tooltips {widget name} {
  # first, restore it's native Button 1 capability
  # bind $widget <Button-1> {internal_button_press %W}
  # bind $widget <ButtonRelease-1> {internal_button_release %W}
  bind $widget <Enter> "internal_tooltips_PopUp %W $name %X %Y"
  bind $widget <Leave> {internal_tooltips_PopDown %W}

  # RAMSAN
  if { [regexp {Ctrl-([a-zA-Z])} $name {} letter] } {
      bind all <Control-$letter> "$widget invoke"
  }
}

##############################
# internal_tooltips_PopUp is used to activate the tooltip window

proc internal_tooltips_PopUp {wid name X Y} {
  global cvscfg

  # get rid of other existing tooltips
  catch {destroy .tooltips_wind}
  toplevel .tooltips_wind

  # add a slight offset to make tooltips fall below cursor
  set Y [expr {$Y+15}]

  # Now pop up the new widgetLabel
  wm overrideredirect .tooltips_wind 1
  wm geometry .tooltips_wind +${X}+${Y}
  label .tooltips_wind.l -text $name -border 2 -relief raised
  pack .tooltips_wind.l -in .tooltips_wind
  .tooltips_wind.l configure -bg $cvscfg(tool_colour)
  .tooltips_wind.l configure -fg black
}

proc internal_tooltips_PopDown {widget} {
  catch {destroy .tooltips_wind}
  $widget configure -relief raised 
}

proc internal_button_press {widget} {
  $widget configure -relief sunken
  eval [lindex [$widget configure -command ] 4]
}

proc internal_button_release {widget} {
  $widget configure -relief raised 
}

