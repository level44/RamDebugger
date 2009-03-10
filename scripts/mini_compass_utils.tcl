
package require snit

proc info_fullargs { procname } {
    set ret ""
    foreach arg [uplevel 1 [list info args $procname]] {
	if { [uplevel 1 [list info default $procname $arg value]] } {
	    upvar 1 value value
	    lappend ret [list $arg $value]
	} else {
	    lappend ret $arg
	}
    }
    return $ret
}

namespace eval cu {}

# for tclIndex to work 
proc cu::menubutton_button { args } {}


snit::widgetadaptor cu::menubutton_button {
    option -command ""
    option -image ""
    option -text ""

    delegate method * to hull
    delegate option * to hull
    delegate option -_image to hull as -image
    delegate option -_text to hull as -text

    variable is_button_active 1
    
    constructor args {
	installhull using ttk::menubutton -style Toolbutton
	bind $win <ButtonPress-1> [mymethod BP1 %x %y]
	bind $win <ButtonRelease-1> [mymethod BR1 %x %y]

	$self configurelist $args
    }
    onconfigure -image {img} {
	set options(-image) $img

	if { $options(-text) ne "" } {
	    $self configure -_image $img
	    return
	} elseif { $img ne "" } {
	    set width [image width $img]
	    set height [image height $img]
	} else { foreach "width height" [list 0 16] break }

	set new_img [image create photo -width [expr {$width+7}] -height $height]
	if { $img ne "" } { $new_img copy $img -to 0 0 }
	set coords {
	    -3 -1
	    -4 -2 -3 -2 -2 -2
	    -5 -3 -4 -3 -3 -3 -2 -3 -1 -3
	}
	foreach "x y" $coords {
	    $new_img put black -to [expr {$width+7+$x}] [expr {$height+$y}]
	}
	$self configure -_image $new_img
	bind $win <Destroy> +[list image delete $new_img]
    }
    onconfigure -text {value} {
	set options(-text) $value

	if { $options(-text) ne "" } {
	    $self configure -style ""
	    if { $options(-image) ne "" } {
		$self configure -_image $options(-image)
	    }
	}
	$self configure -_text $value
    }
    method give_is_button_active_var {} {
	return [myvar is_button_active]
    }
    method BP1 { x y } {
	if { !$is_button_active } { return }
	if { $x < [winfo width $win]-10 && $options(-command) ne "" } {
	    $win instate !disabled {
		catch { tile::clickToFocus $win }
		catch { ttk::clickToFocus $win }
		$win state pressed
	    }
	    return -code break
	}
    }
    method BR1 { x y } {
	if { !$is_button_active } { return }
	if { $x < [winfo width $win]-10 && $options(-command) ne "" } {
	    $win instate {pressed !disabled} {
		$win state !pressed
		uplevel #0 $options(-command)
	    } 
	    return -code break
	}
    }
}