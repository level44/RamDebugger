
################################################################################
#  HELPER FUNCTIONS
################################################################################

package require fileutil

proc CopyPackages { files todir packages packagesout } {

    set tclfiles ""
    for { set i 0 } { $i < [llength $files] } { incr i } {
	set file [lindex $files $i]
	if { [file extension $file] == ".tcl" } {
	    lappend tclfiles $file
	} elseif { [file isdirectory $file] } {
	    eval lappend files [glob -nocomplain -dir $file *.tcl]
	}
    }
    foreach i [fileutil::grep package $tclfiles] {
	foreach "filename number contents" [split $i :] break
	foreach "- - p" [regexp -inline -all {(^|\n)\s*package\s+require\s+(\w*)} \
			     $contents] {
	    if { $p == "smtp" } { set p mime }
	    puts "package $p from file '$filename'"
	    lappend packages $p
	}
    }
    if { [interp exists temp] } { interp delete temp }
    interp create temp
    temp eval { package require Tk ; wm withdraw . }
    temp eval [list set argv0 [set ::argv0]]
    temp eval [list set auto_path $::auto_path]
    foreach i $packages {
        if { [lsearch "tkdnd BLT RamDebugger tcllib" $i] != -1 } { continue }
        temp eval package require $i
    }
    set packages ""
    foreach i [temp eval package names] {
        if { ![catch {temp eval package present $i} pversion] && \
            [temp eval [list package ifneeded $i $pversion]] != "" } {
            lappend packages $i
        }
    }
    interp delete temp
    set packages [lsort -dictionary -unique [string tolower $packages]]
    foreach i [list tcl tk http registry ramdebugger img::* jpegtcl \
            pngtcl tifftcl zlibtcl] {
        while { [set ipos [lsearch $packages $i]] != -1 } {
            set packages [lreplace $packages $ipos $ipos]
        }
    }
    foreach i [list tkdnd cmdline base64] {
        set ipos [lsearch $packages $i]
        if { $ipos == -1 } {
            lappend packages $i
        }
    }
    set packages [lsort -unique -dictionary $packages]

    foreach i $packages {
	if { $i == "registry" } { set i reg }
	if { [lsearch $packagesout $i] != -1 } { continue }
	set dir ""
	set dirs $::auto_path
	for { set k 0 } { $k < [llength $dirs] } { incr k } {
	    set kdir [lindex $dirs $k]
	    foreach j [glob -nocomplain -types d -dir $kdir *] {
		if { [regexp "(?i)^${i}(\[0-9]+|-|$)" [file tail $j]] } {
		    set dir $j
		    break
		}
	    }
	    if { $dir != "" } { break }
	}
	if { $dir == "" } {
	    set dir ********************
	} else {
	    file copy $dir $todir
	}
	puts "$i    $dir"
    }

}


proc regsubfile { args } {

    set opts ""
    set noopts ""
    set isopt 0
    foreach i $args {
	if { $isopt || ![string match -* $i] } {
	    lappend noopts $i
	    set isopt 1
	} else {
	    lappend opts $i
	    if { $i == "--" } { set isopt 1 }
	}
    }
    if { [llength $noopts] == 4 } {
	foreach "exp file subSpec varName" $noopts break
    } elseif { [llength $noopts] == 3 } {
	foreach "exp file subSpec" $noopts break
    } else { error "error. usage: regsubfile ?switches? exp file subSpec ?varName?" }
    
    set fin [open $file r]
    set data [read $fin]
    close $fin
    set retval [eval regsub $opts [list $exp $data $subSpec data_out]]

    if { [info exists varName] } {
	upvar $varName x
	set x $data_out
    }

    #file copy -force $file $file~
    if { $data ne $data_out } {
	set fout [open $file w]
	puts -nonewline $fout $data_out
	close $fout
    } else { set retval 0 }
    return $retval
}

proc zipfile { zipname directory files } {

    set cwd [pwd]
    file delete $zipname
    cd $directory
    puts -nonewline zipping... ; update
    set comm "exec \"$::ZIP\" \"[file join $cwd $zipname]\" -r $files"
    eval $comm
    puts "done\n" ; update
    cd $cwd
}

proc CleanCVSdirs { dir } {

    set savebackups ask

    foreach i [glob -dir $dir "{.,}*"] {
	if { [file tail $i] == "." || [file tail $i] == ".." } { continue }
	if { [file isdir $i] } {
	    if { [string tolower [file tail $i]] == "cvs" } {
		file delete -force $i
		puts -nonewline "deleting $i\n" ; update
	    } elseif { [string tolower [file tail $i]] == ".xvpics" } {
		file delete -force $i
		puts -nonewline "deleting $i\n" ; update
	    } else {
		CleanCVSdirs $i
	    }
	} elseif { [string match .#* [file tail $i]] } {
	    file delete -force $i
	    puts -nonewline "deleting $i\n" ; update
	} elseif { [string match *~ [file tail $i]] } {
	    file delete -force $i
	    puts -nonewline "deleting $i\n" ; update
	} elseif { [string match *.log [string tolower $i]] } {
	    file delete -force $i
	    puts -nonewline "deleting $i\n" ; update
	} elseif { [string equal [file tail $i] TemporalVariables] || \
		[string equal [file tail $i] password.txt] } {
	    set destname [file join /temp [file tail [file dirname $i]]_[file tail $i]]

	    if { $savebackups == "ask" } {
		set retval [tk_dialog .__temp question "Do you want to save backup file '$destname'?" \
		                question 0 Yes "Yes to all" No "No to all" Cancel]
		switch $retval {
		    -1 - 4 { exit }
		    0 { set retval yes }
		    1 { set savebackups always }
		    2 { set retval no }
		    3 { set savebackups never }
		}
	    }
	    if { $savebackups != "never" } {
		if { $savebackups == "always" || $retval == "yes" } {
		    file copy -force $i $destname
		}
	    }
	    file delete -force $i
	    puts -nonewline "deleting $i\n" ; update
	}
    }
}

proc CheckHiddenFiles { directory files } {

    set cwd [pwd]
    cd $directory

    set fail 0
    for { set i 0 } { $i < [llength $files] } { incr i } {
	set file [lindex $files $i]
	if { [file isdir $file] } {
	    eval lappend files [glob -nocomplain -dir $file *]
	}
	foreach j [glob -nocomplain -dir $file -types hidden *] {
	    puts -nonewline "Hidden file: $j\n" ; .t see end ; update
	    incr fail
	    file attributes $j -hidden 0
	}
	if { [file attributes $file -hidden] || [file attributes $file -system] } {
	    puts -nonewline "Hidden or system file: $file\n" ; .t see end ; update
	    incr fail
	    file attributes $file -hidden 0 -system 0
	}
    }
    cd $cwd
    if { $fail } {
	error "There are hidden or system files"
    }
}

proc MustCompile { file args } {

    if { ![file exists $file] } { return 1 }
    foreach i $args {
	if { [file mtime $i] > [file mtime $file] } {
	    return 1
	}
    }
    return 0
}

proc Execute { args } {
    puts -nonewline $args... ; update
    eval [concat exec $args]
    puts -nonewline "done\n"
}

################################################################################
#  CONFIGURATION SECTION
################################################################################


# set FREEWRAP /tcltk/freewrap44/freewrap.exe
# set FREEWRAPTCLSH /utils/freewrapTCLSH
set TEXI2HTML [list perl [file normalize "~/Gid Project/info/html-version/texi2html"] \
		              -split_node -menu]
set ZIP zip.exe
set Version 5.3
set Date "February 2005"
set Copyright "2002-2005 Ramon Ribó"

set files [list RamDebugger.tcl license.terms Readme addons scripts Examples help \
	       pkgIndex.tcl]

set deletefiles [list help/02TclTk8.5 help/wordindex]

set packages [list Img Tkhtml treectrl]
set packagesout [list Tcl Tk bwidget1.6 reg1.1 RamDebugger tcllib \
		     resizer trf]

set tclkitdir "C:/TclTk/tclkit"


################################################################################
#  CREATING THE ZIP
################################################################################


#pack [text .t -width 80 -height 8] -fill both -expand 1
console show
wm withdraw .

CheckHiddenFiles . $files

foreach i [list RamDebugger.tcl pkgIndex.tcl] {
    puts -nonewline regsubfile($i)=[regsubfile {set Version ([0-9.]+)} $i "set Version $Version"]\n
}

foreach "name value" [list Version $Version Date $Date Copyright $Copyright] {
    puts -nonewline regsubfile(Ramdebugger.texinfo,$name)=[regsubfile -line "@set $name\\s+.*$" \
	  Ramdebugger.texinfo "@set $name $value"]\n
}


if { [MustCompile help/RamDebugger/RamDebugger_toc.html RamDebugger.texinfo] } {
    set oldcwd [pwd]
    cd help/01RamDebugger
    eval [concat Execute $TEXI2HTML ../../RamDebugger.texinfo]
    cd $oldcwd
}
auto_mkindex scripts *.tcl


file delete -force install_temp
file mkdir install_temp/RamDebugger
file delete RamDebugger$Version.zip
foreach i $files {
    file copy $i install_temp/RamDebugger/.
}
foreach i $deletefiles {
    file delete -force [file join install_temp RamDebugger $i]
}
CleanCVSdirs install_temp/RamDebugger
CopyPackages $files install_temp/RamDebugger/addons $packages $packagesout

set tclfiles ""
foreach i $files { lappend tclfiles RamDebugger/$i }
zipfile RamDebugger$Version.zip install_temp $tclfiles

################################################################################
#  CREATING THE STARKIT
################################################################################

file mkdir install_temp/RamDebugger.vfs/lib
file rename install_temp/RamDebugger install_temp/RamDebugger.vfs/lib/app-RamDebugger

set fout [open install_temp/RamDebugger.vfs/main.tcl w]

puts $fout {
    package require starkit
    starkit::startup
    source [file join $::starkit::topdir lib app-RamDebugger RamDebugger.tcl]
}
close $fout

set pwd [pwd]
cd install_temp

puts -nonewline "creating starkit..." ; update
exec [file join $tclkitdir tclkit-win32-sh.upx.exe] [file join $tclkitdir sdx.kit] \
	    wrap RamDebugger
file copy -force RamDebugger ../RamDebugger$Version.kit
puts "done\n" ; update
cd $pwd

################################################################################
#  CLEAN UP
################################################################################

file delete -force install_temp
puts -nonewline DONE\n
#.t see end
update
#after 500
#exit
