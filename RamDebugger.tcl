#!/bin/sh
# the next line restarts using wish \
exec wish "$0" "$@"

# 	$Id: RamDebugger.tcl,v 1.2 2002/07/30 18:09:49 ramsan Exp $	
#RamDebugger  -*- TCL -*- Created: ramsan Jul-2002, Modified: ramsan Jul-2002


namespace eval RamDebugger {

    ################################################################################
    #    Non GUI commands
    ################################################################################

    namespace export rhelp rdebug rlist reval rcont rnext rstep rbreak rcond rinfo rdel \
            rstack routput rtime

    ################################################################################
    # communications issues
    ################################################################################

    variable CheckRemotes 1
    variable remoteserverIsLocal 0
    variable remoteserver ""
    variable remoteserverNum ""
    variable debuggerserver ""
    variable debuggerserverNum ""
    variable services
    variable CheckRemotes
    
    ################################################################################
    # debugger state
    ################################################################################

    variable debuggerstate "" ;# can be: "" or debug or time
    variable currentfile ""
    variable currentline 1
    variable files
    variable instrumentedfiles
    variable instrumentedfilesTime
    variable instrumentedfilesSent
    variable fileslist ""
    variable breakpoints ""
    variable TimeMeasureData ""

    variable MainDir
    variable CacheDir

    ################################################################################
    # GUI state
    ################################################################################

    variable text ""
    variable IsInStop 0
    variable TextMotionAfterId ""
    variable ExpressionResult ""
    variable count
    variable listbox ""
    variable progressvar
    variable status
}

################################################################################
#   Init proc
################################################################################

proc RamDebugger::Init {} {
    variable CheckRemotes
    variable debuggerserver
    variable debuggerserverNum
    variable MainDir
    variable CacheDir

    set dir [info script]
    if { $dir == "" } {
	set dir [info nameofexecutable]
    }
    set dir [file join [pwd] [file dirname $dir]]
    set MainDir $dir

    lappend ::auto_path [file join $MainDir addons]

    if { [file isdir [file join $MainDir cache]] } {
	set CacheDir [file join $MainDir cache]
    }

    if { $debuggerserver != "" } { return }

    set debuggerserver ramdebugger

    if { $::tcl_platform(platform) == "windows" } {
	if { $CheckRemotes } {
	    lappend ::auto_path {C:\TclTk\ActiveTcl\lib\tcllib-1.3\comm}
	    uplevel \#0 package require comm
	    set debuggerserverNum [comm::register RamDebugger 1]
	}
    } else {
	package require Tk
	wm withdraw .
	set debuggerserver [tk appname $debuggerserver]
    }
}

################################################################################
#       Main non GUI functions
################################################################################


proc RamDebugger::rhelp { args } {

    set usagestring {usage: rhelp ?switches? ?command?
	-h:       displays usage
	--:     end of options

        To obtain more information of a command, use 'rhelp command' or 'command -h'.
        To begin debugging, use rdebug.
    }
    ParseArgs $args $usagestring opts

    if { $opts(command) != "" } {
	if { [info command $opts(command)] == "" } {
	    error "command '$opts(command)' does not exists\n$usagestring"
	}
	catch { $opts(command) -h } string
	return $string
    }
    set list [namespace export]
    set retval "Use 'rhelp -h' or 'rhelp command' for more information\n\n"
    foreach i [namespace export] {
	catch { $i -h } string
	regexp {^.*:(.*)} [lindex [split $string \n] 0] {} string
	append retval "$string\n"
    }
    return $retval
}

proc RamDebugger::rdebug { args } {
    variable remoteserver
    variable remoteserverIsLocal
    variable remoteserverNum
    variable debuggerserver
    variable debuggerserverNum
    variable currentfile
    variable services
    variable instrumentedfilesSent
    variable debuggerstate

    set usagestring {usage: rdebug ?switches? ?program?
	-h:            displays usage
	-actives:      return active programs
	-forceupdate:  force update of remote program search
	-disconnect:   disconnect from remoteserver
	-currentfile:  execute and debug currentfile
	--:            end of options

        To begin debugging a TCL file, select the file with 'rlist' and use 'rdebug -currentfile'.
        To begin debugging a remote program, use 'rdebug program', where program is one active
        program, that must belong to the services list.
    }
    ParseArgs $args $usagestring opts

    if { $opts(-forceupdate) } {
	FindActivePrograms 1
    } else { FindActivePrograms 0 }
    
    if { $opts(-actives) } { return [array names services] }

    if { $opts(-disconnect) } {
	if { $remoteserver == "" } {
	    error "error. There is no connected remote server"
	}
	set remoteserver ""
	set debuggerstate ""
	if { $remoteserverIsLocal } { interp delete local }
	return
    }
    if { $opts(-currentfile) } {
	if { [interp exists local] } { interp delete local }
	interp create local
	interp alias local sendmaster "" eval
	interp eval local [list load {} Tk]
	set remoteserverIsLocal 1
	if { $currentfile == "" } {
	    error "Error. there is no current file"
	}
	set remoteserver $currentfile
    } else {
	if { $opts(program) == "" } {
	    if { $remoteserver != "" } {
		return $remoteserver
	    } else { error "error. $usagestring\nActive programs: [array names services]" }
	}
	if { [lsearch [array names services] $opts(program)] == -1 } {
	    FindActivePrograms 1
	}
	if { [lsearch [array names services] $opts(program)] != -1 } {
	    set remoteserver $opts(program)
	    set remoteserverNum $services($remoteserver)
	} else { error "error. $usagestring\nActive programs: [array names services]" }
	if { $remoteserverIsLocal } { interp delete local }
	set remoteserverIsLocal 0
    }
    set remotecomm {
	namespace eval RDC {
	    variable breaks
	    variable evalhandler ""
	    variable code ""
	    variable stopnext 0
	    variable contto ""
	    variable outputline 0
	    variable lastprocname ""
	    variable lastlevel 0
	    variable currentfile ""
	}
	if { [info commands ::RDC::infoproc] == "" } {
	    rename ::info ::RDC::infoproc
	    proc ::info { args } {
		set retval [uplevel 1 ::RDC::infoproc $args]
		if { [lindex $args 0] == "script" && $retval == "" } {
		    return $::RDC::currentfile
		}
		return $retval
	    }
	}

	proc RDC::SendDev { comm } {
	    SENDDEVBODY
	}
	proc RDC::MeasureTime { name timestr } {
	    SendDev [list RamDebugger::RecieveTimeFromProgram $name [lindex $timestr 0]]
	}
	proc RDC::Continue {} {
	    set ::RDC::code ""
	}
	proc RDC::Eval { comm { handler "" } } {
	    variable evalhandler $handler
	    set ::RDC::code $comm
	    update
	}
	proc RDC::F { filenum line } {
	    variable code
	    variable evalhandler
	    # == 1 next ; == 2 step
	    variable stopnext
	    variable contto
	    variable breaks
	    variable outputline
	    variable lastprocname
	    variable lastlevel

	    if { [info level] > 1 } {
		set prognameL [lindex [info level -1] 0]
		set procname [uplevel 1 [list namespace which -command $prognameL]]
	    } else { set procname "GLOBAL" }
	    set stop 0
	    set breaknum 0
	    set condinfo ""
	    if { $stopnext == 2 } { set stop 1 }
	    if { $stopnext == 1 && ($procname == $lastprocname || [info level] < $lastlevel) } {
		set stop 1
	    }
	    if { [lindex $contto 0] == $filenum && [lindex $contto 1] == $line } {
		set stop 1
		set contto ""
	    }
	    if { [info exists breaks($filenum,$line)] } {
		set breaknum [lindex $breaks($filenum,$line) 0]
		set cond [lindex $breaks($filenum,$line) 1]
		if { $cond == "" } {
		    set stop 1
		} else {
		    set err [catch [list uplevel 1 [list expr $cond]] condinfo]
		    if { $err || $condinfo != 0 } { set stop 1 }
		}
	    }
	    if { !$stop } {
		if { $outputline } {
		    set procname [lindex [info level -1] 0]
		    SendDev [list RamDebugger::RecieveFromProgram output \
			    $filenum $line $procname "" ""]
		    
		}
		return
	    }
	    set lastprocname $procname
	    set lastlevel [info level]
	    set textline ""
	    set code ""
	    catch {
		regexp "RDC::F\\s+$filenum+\\s+$line\\s+; (\[^\n]*)" [info body $procname] {} textline
	    }
	    RDC::SendDev [list RamDebugger::RecieveFromProgram $breaknum $filenum \
		    $line $procname $textline $condinfo]
	    while 1 {
		if { $code == "" } { vwait ::RDC::code }
		if { $code == "" } {
		    return
		}
		set err [catch {
		    uplevel 1 $code
		} returnvalue]

		set code ""
		if { $evalhandler != "" } {
		    RDC::SendDev "$evalhandler [list [list $err $returnvalue]]"
		} else {
		    RDC::SendDev [list RamDebugger::RecieveFromProgramExpr $err $returnvalue]
		}
	    }
	}
    }
    if { $remoteserverIsLocal } {
	set remotecomm [string map [list SENDDEVBODY "sendmaster \$comm"] \
			    $remotecomm]
    } elseif { $::tcl_platform(platform) == "windows" } {
	set remotecomm [string map [list SENDDEVBODY "comm::comm send $debuggerserverNum \$comm"] \
			    $remotecomm]
    } else {
	set remotecomm [string map [list SENDDEVBODY "send $debuggerserver \$comm"] \
			    $remotecomm]
    }
    EvalRemote $remotecomm
    catch { unset instrumentedfilesSent }

    if { $debuggerstate == "" || $debuggerstate == "debug" } {
	set debuggerstate debug
	UpdateRemoteBreaks
    }
    if { $opts(-currentfile) } {
	EvalRemote [list set ::RDC::currentfile $currentfile]
    	after idle [list RamDebugger::rlist -quiet $currentfile]
    }
    return "Begin debugging of program '$remoteserver'"
}

proc RamDebugger::reval { args } {
    variable ExpressionResult
    variable debuggerstate

    if { $debuggerstate == "time" } {
	error "Command reval cannot be used in 'time' mode. Check rtime"
    }

    set usagestring {usage: reval ?switches? arg ?arg...?
	-h:           displays usage
	-nonice:      return a list with an integer of the error and the string
	-handler comm: returns inmediately and calls later to 'comm' with the results as argument
	--:           end of options

        This command is typically use when the program has already stopped in one breakpoint.
        Permmits to evaluate one expresion in the context level of the breakpoint. The expression
        can also change the value of one variable.
    }
    ParseArgs $args $usagestring opts
    set ExpressionResult ""
    if { $opts(-handler) != 0 } {
	EvalRemote [list ::RDC::Eval $opts(arg) $opts(-handler)]
	return ""
    }
    EvalRemote [list ::RDC::Eval $opts(arg)]
    if { $ExpressionResult == "" } { vwait RamDebugger::ExpressionResult }

    if { !$opts(-nonice) } {
	if { [lindex $ExpressionResult 0] == 0 } {
	    return [lindex $ExpressionResult 1]
	} else {
	    error [lindex $ExpressionResult 1]
	}
    } else {
	return $ExpressionResult
    }
}

proc RamDebugger::rstack { args } {
    variable debuggerstate

    if { $debuggerstate == "time" } {
	error "Command rstack cannot be used in 'time' mode. Check rtime"
    }

    set usagestring {usage: rstack ?switches?
	-h:       displays usage
	-nonice: return a list with an integer of the error and the string
	-handler comm: returns inmediately and calls later to 'comm' with the results as argument
	--:     end of options
    }
    ParseArgs $args $usagestring opts

    set comm {
	set ::RDC::retval "STACK TRACE\n"
	set ::RDC::current [info level]
	for { set ::RDC::i $::RDC::current } { $::RDC::i > 0 } { incr ::RDC::i -1 } {
	    set ::RDC::level ""
	    foreach ::RDC::j [info level $::RDC::i] {
		regsub -all {\n} $::RDC::j { } ::RDC::j
		if { [string length $::RDC::j] > 50 } {
		    set ::RDC::j [string range $::RDC::j 0 46]...
		}
		lappend ::RDC::level $::RDC::j
	    }
	    append ::RDC::retval "level $::RDC::i $::RDC::level\n"
	}
	set ::RDC::retval
    }
    return [eval reval $args [list $comm]]
}

proc RamDebugger::rcont { args } {
    variable currentfile
    variable instrumentedfiles
    variable currentline
    variable fileslist
    variable debuggerstate

    if { $debuggerstate == "time" } {
	error "Command rcont cannot be used in 'time' mode. Check rtime"
    }

    set usagestring {usage: rcont ?switches? ?line?
	-h:       displays usage
	--:     end of options
    }
    ParseArgs $args $usagestring opts

    if { ![string is integer $opts(line)] } {
	error "line '$opts(line)' must be a number\n$usagestring"
    }

    if { $currentfile == "" } {
	error "There is no file selected\$usagestring"
    }
    if { $opts(line) != "" } {
	set currentline $opts(line)
	set filenum [lsearch $fileslist $currentfile]
	set ipos [string first "RDC::F $filenum $currentline ;" $instrumentedfiles($currentfile)]
	if { $ipos == -1 } {
	    error "error: line $currentline is not instrumented"
	}
    }

    StopAtGUI "" ""
    if { $opts(line) != "" } {
	set filenum [lsearch $fileslist $currentfile]
	EvalRemote [list set ::RDC::contto [list $filenum $currentline]]
    }
    EvalRemote [list set ::RDC::stopnext 0]
    EvalRemote ::RDC::Continue
}

proc RamDebugger::rnext { args } {
    variable debuggerstate

    if { $debuggerstate == "time" } {
	error "Command rnext cannot be used in 'time' mode. Check rtime"
    }
    
    set usagestring {usage: rnext ?switches?
	-h:       displays usage
	--:     end of options
    }
    ParseArgs $args $usagestring opts

    StopAtGUI "" ""


    EvalRemote [list set ::RDC::stopnext 1]
    EvalRemote ::RDC::Continue
}

proc RamDebugger::rstep { args } {
    variable debuggerstate

    if { $debuggerstate == "time" } {
	error "Command rstep cannot be used in 'time' mode. Check rtime"
    }

     set usagestring {usage: rstep ?switches?
	-h:       displays usage
	--:     end of options
    }
    ParseArgs $args $usagestring opts

    StopAtGUI "" ""
    EvalRemote [list set ::RDC::stopnext 2]
    EvalRemote ::RDC::Continue
}

proc RamDebugger::routput { args } {
    variable debuggerstate

    if { $debuggerstate == "time" } {
	error "Command routput cannot be used in 'time' mode. Check rtime"
    }

    set usagestring {usage: routput ?switches? boolean
	-h:       displays usage
	--:     end of options
    }
    ParseArgs $args $usagestring opts

    if { $opts(boolean) } {
	set what 1
    } else { set what 0 }
    EvalRemote [list set ::RDC::outputline $what]
}

proc RamDebugger::rtime { args } {
    variable debuggerstate
    variable remoteserver
    variable remoteserverIsLocal
    variable TimeMeasureData
    variable currentfile

    set usagestring {usage: rtime ?switches? ?name? ?lineini? ?lineend?
	-h:             displays usage
	-start:         start time mode
	-stop:          stop time mode and go to debugging mode
	-add:           Add a time block by giving name lineini and lineend
	-delete:        Delete named time block
	-list:          List previusly defined time blocks
	-display units: Displays table of results. units can be: microsec, milisec, sec, min
	--:             end of options

	This function is used to obtain absolute and relative times of several blocks
	of the code. The process is: define one or several blocks giving the block name,
	the beginning line and the end line with option -add. After, select option -start.
        When finished measuring times, use option -display to see the results. Use -delete
	to finish.
    }
    ParseArgs $args $usagestring opts
    
    if { $opts(-start) } {
	set debuggerstate time

	set TimeMeasureDataNew ""
	set files ""
	foreach i $TimeMeasureData {
	    foreach "name file lineini lineend lasttime" $i {
		lappend TimeMeasureDataNew [list $name $file $lineini $lineend ""]
		if { [lsearch $files $file] == -1 } {
		    rlist -quiet $file
		    lappend files $file
		}
	    }
	}
	set TimeMeasureData $TimeMeasureDataNew
	if { $remoteserverIsLocal && $remoteserver != "" } {
	    set currentfile $remoteserver
	    rdebug -currentfile
	}
	return "Using 'measure times' mode"
    }
    if { $opts(-stop) } {
	if { $remoteserver == "" } {
	    set debuggerstate ""
	    return "Using no mode"
	} else {
	    set debuggerstate debug
	    return "Using 'debug' mode"
	}
    }
    if { $opts(-delete) } {
	set ipos 0
	foreach i $TimeMeasureData {
	    foreach "name file lineini lineend lasttime" $i {
		if { $name == $opts(name) } {
		    set TimeMeasureData [lreplace TimeMeasureData $ipos $ipos]
		    return "deleted time block '$opts(name)'"
		}
	    }
	    incr ipos
	}
	error "error: time block '$opts(name)' not found"
    }
    if { $opts(-list) } {
	set retval ""
	foreach i $TimeMeasureData {
	    append retval $i\n
	}
	return $retval
    }
    if { $opts(-display) != 0 } {
	set datanames ""

	for { set i 0 } { $i < [llength $TimeMeasureData] } { incr i } {
	    foreach "name - lineini lineend time" [lindex $TimeMeasureData $i] break
	    if { ![info exists data($name)] } {
		set level 0
		set data($name) [list $level $time 0]
	    } else {
		set level [lindex $data($name) 0]
		
	    }
	    lappend datanames $name
	    if { $time == "" || $time == 0 } { continue }

	    incr level
	    set sum 0
	    for {set j [expr $i+1] } { $j < [llength $TimeMeasureData] } { incr j } {
		foreach "name_in - lineini_in lineend_in time_in" [lindex $TimeMeasureData $j] break
		if { $lineini_in > $lineend } { break }
		if { $lineini_in == $lineini && $lineend_in == $lineend } { continue }
		if { $time_in == "" || $time_in == 0 } {
		    set percent ""
		} else {
		    set percent [expr $time_in*100/double($time)]
		    incr sum $time_in
		}
		set data($name_in) [list $level $time_in $percent]
	    }
	    if { $sum > 0 && $time != "" && $sum < $time } {
		set remname "Remaining time for '$name'"
		set time_rem [expr $time-$sum]
		set data($remname) [list $level $time_rem [expr $time_rem*100/double($time)]]
		lappend datanames $remname
	    }
	}
	set unitname $opts(-display)
	switch -- $opts(-display) {
	    microsec { set unitfactor 1 }
	    milisec { set unitfactor 1e-3 }
	    sec { set unitfactor 1e-6 }
	    min { set unitfactor 1e-6/60.0 }
	    default {
		error "error in display units.\n$usagestring"
	    }
	}
	set retval ""
	foreach i $datanames {
	    append retval [string repeat "....." [lindex $data($i) 0]]
	    append retval $i
	    set time [lindex $data($i) 1]
	    if { $time != "" } { set time [format %.4g [expr $time*$unitfactor]] }
	    append retval " $time $unitname"
	    if { [lindex $data($i) 2] != "" && [lindex $data($i) 2] != 0 } {
		append retval " ([format %.3g [lindex $data($i) 2]]%)"
	    }
	    append retval \n
	}
	return $retval
    }
    if { !$opts(-add) } {
	error "error: it is necessary to select one switch\n$usagestring"
    }
    if { $opts(name) == "" } {
	error "error: it is necessary to enter a name for the block\n$usagestring"
    }
    if { ![string is integer -strict $opts(lineini)] || $opts(lineini) < 1 } {
	error "error: lineini must be a positive number\n$usagestring"
    }
    if { ![string is integer -strict $opts(lineend)] || $opts(lineend) < 1 } {
	error "error: lineend must be a positive number\n$usagestring"
    }
    if { $opts(lineend) < $opts(lineini) } {
	error "error: lineend cannot be smaller than lineini\n$usagestring"
    }
    if { $currentfile == "" } {
	error "error: there is no current file"
    }
    foreach i $TimeMeasureData {
	foreach "name file lineini lineend lasttime" $i {
	    if { $name == $opts(name) } {
		error "block name '$opts(name)' already exists"
	    }
	    set fail 0
	    if { $opts(lineini) < $lineini && $opts(lineend) >= $lineend } { set fail 1 }
	    if { $opts(lineini) <= $lineend && $opts(lineend) > $lineend } { set fail 1 }
	    if { $fail } {
		error "error: block is crossing with block '$name'"
	    }
	}
    }
    proc SortTimeMeasureData { a1 a2 } {
	set a1_li [lindex $a1 2]
	set a1_le [lindex $a1 3]
	set a2_li [lindex $a2 2]
	set a2_le [lindex $a2 3]
	
	if { $a1_li < $a2_li } { return -1 }
	if { $a1_li > $a2_li } { return 1 }
	if { $a1_le > $a2_le } { return -1 }
	if { $a1_le < $a2_le } { return -1 }
	return 0
    }
    lappend TimeMeasureData [list $opts(name) $currentfile $opts(lineini) $opts(lineend) ""]
    set TimeMeasureData [lsort -command RamDebugger::SortTimeMeasureData $TimeMeasureData]
    return "Added time block '$opts(name)'"
}

proc RamDebugger::rlist { args } {
    variable currentfile
    variable currentline
    variable files
    variable filesmtime
    variable fileslist
    variable instrumentedfiles
    variable instrumentedfilesTime
    variable instrumentedfilesSent
    variable instrumentedfilesInfo
    variable remoteserver
    variable remoteserverIsLocal
    variable debuggerstate
    variable TimeMeasureData

    set usagestring {usage: rlist ?switches? ?file? ?line?
	-h:       displays usage
	-quiet: do not print anything
	-force: force to reload file
	--:     end of options
    }
    ParseArgs $args $usagestring opts
    set force $opts(-force)

    if { ![string is integer $opts(line)] } {
	error "line '$opts(line)' must be a number\n$usagestring"
    }

    set currentfile_save $currentfile
    if { $opts(file) != "" } { set currentfile $opts(file) }

    if { $currentfile == "" } {
	error "it is necessary to enter a file name\n$usagestring"
    }

    if { ![info exists files($currentfile)] || $force} {
	set err [catch [list open $currentfile r] fin]
	if { $err } {
	    set filetry $currentfile
	    set currentfile $currentfile_save
	    error "file '$filetry' does not exist\n$usagestring"
	}
	set files($currentfile) [read $fin]
	close $fin
	if { [lsearch $fileslist $currentfile] == -1 } {
	    lappend fileslist $currentfile
	}
	set filesmtime($currentfile) [file mtime $currentfile]
    }

    if { [info exists instrumentedfiles($currentfile)] && !$force } {
	regexp {InstrumentOnlyProcs=([0-9]+)} $instrumentedfiles($currentfile) {} \
		InstrumentOnlyProcs
	set fail 0
	if { $InstrumentOnlyProcs == -1 } { set fail 1 }
	if { !$remoteserverIsLocal && !$InstrumentOnlyProcs } { set fail 1 }
	if { $remoteserverIsLocal && $InstrumentOnlyProcs } { set fail 1 }
	if { $fail && [info exists instrumentedfiles($currentfile)] } {
	    unset instrumentedfiles($currentfile)
	}
    }

    if { ![info exists instrumentedfilesInfo($currentfile)] || $force } {
	set infofile [GiveInstFile $currentfile 1 -1]
	if { $infofile != "" } {
	    set fin [open $infofile r]
	    gets $fin instrumentedfilesInfo($currentfile)
	    close $fin
	}
    }

    if { ![info exists instrumentedfiles($currentfile)] || $force } {
	set filenum [lsearch $fileslist $currentfile]
	if {!$force } {
	    set instfile [GiveInstFile $currentfile 1 $remoteserverIsLocal]

	    if { $instfile != "" } {
		set fin [open $instfile r]
		gets $fin instrumentedfiles($currentfile)
		set InstrumentOnlyProcs -1
		regexp {InstrumentOnlyProcs=([0-9]+)} $instrumentedfiles($currentfile) {} \
		    InstrumentOnlyProcs
		set fail 0
		if { $InstrumentOnlyProcs == -1 } { set fail 1 }
		if { !$remoteserverIsLocal && !$InstrumentOnlyProcs } { set fail 1 }
		if { $remoteserverIsLocal && $InstrumentOnlyProcs } { set fail 1 }
		if { !$fail } {
		    append instrumentedfiles($currentfile) \n[read $fin]
		    regexp {RDC::F ([0-9]+)} $instrumentedfiles($currentfile) {} oldfilenum
		    if { $oldfilenum != $filenum } {
			set instrumentedfiles($currentfile) \
			    [string map [list "RDC::F $oldfilenum " "RDC::F $filenum "] \
				 $instrumentedfiles($currentfile)]
		    }
		} else {
		    unset instrumentedfiles($currentfile)
		}
		close $fin
	    }
	}
	if { ![info exists instrumentedfiles($currentfile)] || \
	    ![info exists instrumentedfilesInfo($currentfile)] || $force } {
	    SetMessage "Instrumenting file '$currentfile'..."

	    if { [catch {
		if { !$remoteserverIsLocal } {
		    Instrumenter::DoWork $files($currentfile) $filenum \
			    instrumentedfiles($currentfile) instrumentedfilesInfo($currentfile) 1
		} else {
		    Instrumenter::DoWork $files($currentfile) $filenum \
			    instrumentedfiles($currentfile) instrumentedfilesInfo($currentfile) 0
		}
	    } errstring] } {
		RamDebugger::ProgressVar 100
		if { [info exists instrumentedfiles($currentfile)] } {
		    unset instrumentedfiles($currentfile)
		}
		WarnWin $errstring
	    }
		
	    set instfile [GiveInstFile $currentfile 0 $remoteserverIsLocal]
	    if { $instfile != "" && [info exists instrumentedfiles($currentfile)] } {
		set fout [open $instfile w]
		puts -nonewline $fout $instrumentedfiles($currentfile)
		close $fout
	    }
	    set infofile [GiveInstFile $currentfile 0 -1]
	    if { $infofile != "" && [info exists instrumentedfilesInfo($currentfile)] } {
		set fout [open $infofile w]
		puts -nonewline $fout $instrumentedfilesInfo($currentfile)
		close $fout
	    }
	    SetMessage ""
	}
    }
    if { $debuggerstate == "time" && [info exists instrumentedfiles($currentfile)] && \
	    (![info exists instrumentedfilesTime($currentfile)] || $force) } {
	SetMessage "Instrumenting file '$currentfile' for time measure..."
    	Instrumenter::DoWorkForTime $files($currentfile) $currentfile \
	   instrumentedfilesTime($currentfile) $TimeMeasureData
	SetMessage ""
    }

    if { $debuggerstate != "" && $remoteserver != "" && [info exists instrumentedfiles($currentfile)] &&\
	    (![info exists instrumentedfilesSent($currentfile)] || \
	    $instrumentedfilesSent($currentfile) != $debuggerstate || $force) } {
	if { [catch {
	    if { $debuggerstate == "debug" } {
		EvalRemote $instrumentedfiles($currentfile)
	    } else {
		EvalRemote $instrumentedfilesTime($currentfile)
	    }
	    set instrumentedfilesSent($currentfile) $debuggerstate
	    FillListBox
	} errstring] } {
	    set err $::errorInfo
	    if { [catch {
		WarnWin $err
	    }] } {
		puts $err
	    }
	}
    }

    if { $opts(line) != "" } {
	set currentline $opts(line)
    }
    if { $opts(-quiet) } { return "" }

    set lines [split $files($currentfile) \n]
    set retval ""
    for { set i -1 } { $i < 3 } { incr i } {
	append retval "[format %4d [expr $currentline+$i+1]] --> "
	append retval "[lindex $lines [expr $currentline+$i]]\n"
    }
    incr currentline 4
    return $retval
}

proc RamDebugger::rcond { args } {
    variable breakpoints
    variable debuggerstate

    if { $debuggerstate == "time" } {
	error "Command rcond cannot be used in 'time' mode. Check rtime"
    }

    set usagestring {usage: rcond ?switches? breakpointnum cond
	-h:       displays usage
	-quiet: do not print anything
	--:     end of options
    }
    ParseArgs $args $usagestring opts

    set found 0
    set ipos 0
    foreach i $breakpoints {
	if { [lindex $i 0] == $opts(breakpointnum) } {
	    set found 1
	    break
	}
	incr ipos
    }
    if { !$found } {
	error "Breakpoints $opts(breakpointnum) does not exist\n$usagestring"
    }
    set breakpoints [lreplace $breakpoints $ipos $ipos [lreplace $i 3 3 $opts(cond)]]

    UpdateRemoteBreaks

    if { !$opts(-quiet) } {
	return "condition for breakpoint $opts(breakpointnum): $opts(cond)"
    }
}

proc RamDebugger::rbreak { args } {
    variable remoteserver
    variable currentfile
    variable currentline
    variable files
    variable fileslist
    variable instrumentedfiles
    #variable instrumentedfilesSent
    variable breakpoints
    variable debuggerstate

    if { $debuggerstate == "time" } {
	error "Command rbreak cannot be used in 'time' mode. Check rtime"
    }

    set usagestring {usage: rbreak ?switches? ?file? line
	-h:       displays usage
	-quiet: do not print anything
	-force: force to reload file
	--:     end of options
    }
    ParseArgs $args $usagestring opts

    if { $opts(line) == "" } {
	error "It is necessary to enter a line number\n$usagestring"
    }
    if { [catch {
	rlist -quiet $opts(file) $opts(line)
    } errcatch] } {
	error "[lindex [split $errcatch \n] 0]\n$usagestring"
    }

    set filenum [lsearch $fileslist $currentfile]

    set ipos [string first "RDC::F $filenum $currentline ;" $instrumentedfiles($currentfile)]
    if { $ipos == -1 } {
	error "error: line $currentline is not instrumented"
    }

#     if { ![info exists instrumentedfilesSent($currentfile)] && $remoteserver != "" } {
# 	EvalRemote $instrumentedfiles($currentfile)
# 	set instrumentedfilesSent($currentfile) 1
#     }
    
    set NumBreakPoint 1
    foreach i $breakpoints {
	if { [lindex $i 0] >= $NumBreakPoint } {
	    set NumBreakPoint [expr [lindex $i 0]+1]
	}
    }
    lappend breakpoints [list $NumBreakPoint $currentfile $currentline ""]
    UpdateRemoteBreaks

    if { !$opts(-quiet) } {
	return "set breakpoint $NumBreakPoint at $currentfile $currentline"
    }
}

proc RamDebugger::rinfo { args } {
    variable breakpoints
    variable currentfile

    set usagestring {usage: rinfo ?switches? ?line?
	-h:       displays usage
	--:     end of options
    }
    ParseArgs $args $usagestring opts

    if { $opts(line) != "" } {
	if { ![string is integer $opts(line)] } {
	    error "line '$line' must be a number\n$usagestring"
	}
	set retval ""
	foreach i $breakpoints {
	    if { [lindex $i 1] == $currentfile && [lindex $i 2] == $opts(line) } {
		lappend retval [lindex $i 0]
	    }
	}
	return $retval
    }

    set retval ""
    foreach i $breakpoints {
	append retval $i\n
    }
    return $retval
}

proc RamDebugger::rdel { args } {
    variable files
    variable breakpoints

    set usagestring {usage: rdel ?switches? ?breakpointnum?
	-h:     displays usage
	-all:   delete all breakpoints 
	--:     end of options
    }
    ParseArgs $args $usagestring opts

    if { $opts(-all) } {
	if { $opts(breakpointnum) != "" } {
	    error "when using -all, no breakpoint num must be written\n$usagestring"
	}
	set breakpoints ""
	UpdateRemoteBreaks
	return "deleted all breakpoints"
    }
    if { ![string is integer -strict $opts(breakpointnum)] } {
	error "breakpointnum '$opts(breakpointnum)' must be a number\n$usagestring"
    }
    set ipos 0
    foreach i $breakpoints {
	if { [lindex $i 0] == $opts(breakpointnum) } {
	    set breakpoints [lreplace $breakpoints $ipos $ipos]
	    UpdateRemoteBreaks
	    return "deleted breakpoint $opts(breakpointnum)"
	}
    }
    error "breakpoint $opts(breakpointnum) not found"
}

################################################################################
#    Helper basic functions
################################################################################

proc RamDebugger::ParseArgs { args usagestring OptsName } {
    upvar 1 $OptsName opts

    set switches ""
    foreach i [lrange [split $usagestring \n] 1 end] {
	if { [string trim $i] == "" } { break }
	regexp {(.*):} $i {} sw
	if { [regexp {(-\S+)\s+(\S+)} $sw {} sw argsw] } {
	    lappend switches $sw
	    set switchesarg($sw) 1
	} else {
	    lappend switches [string trim $sw]
	}
    }
    set normalargs [lrange [lindex [split $usagestring \n] 0] 3 end]

    foreach i $switches { set opts($i) 0 }
    foreach i $normalargs { set opts([string trim $i ?]) "" }

    set canhaveflags 1
    set iargs 0
    for { set i 0 } { $i < [llength $args] } { incr i } {
	set arg [lindex $args $i]
	if { $canhaveflags && $arg == "--" } {
	    set canhaveflags 0
	} elseif { $canhaveflags && $arg == "-h" } {
	    return -code return [string map [list \t [string repeat " " 8]] $usagestring]
	} elseif { $canhaveflags && [lsearch $switches $arg] != -1 } {
	    if { [info exists switchesarg($arg)] } {
		incr i
		set opts($arg) [lindex $args $i]
	    } else {
		set opts($arg) 1
	    }
	} elseif { [regexp {^[?].*[.]{3}[?]$} [lindex $normalargs $iargs]] } {
	    set argname [string trim [lindex $normalargs [expr $iargs-1]] "?."]
	    set opts($argname) [concat $opts($argname) $arg]
	} else {
	    set canhaveflags 0
	    while { [regexp {^[?].*[?]$} [lindex $normalargs $i]] && \
			[llength $args]-$i < [llength $normalargs]-$iargs && \
		    ![regexp {^[?].*[?]$} [lindex $normalargs end]] } {
		if { $iargs >= [llength $normalargs] } { error "error. $usagestring" }
		set opts([string trim [lindex $normalargs $iargs] ?]) ""
		incr iargs
	    }
	    if { $iargs >= [llength $normalargs] } { error "error. $usagestring" }
	    set opts([string trim [lindex $normalargs $iargs] ?]) $arg
	    incr iargs
	}
    }
    for { set i $iargs } { $i < [llength $normalargs] } { incr i } {
	if { ![regexp {^[?].*[?]$} [lindex $normalargs $i]] } {
	    error "error. $usagestring"
	} else { set opts([string trim [lindex $normalargs $i] ?]) "" }
    }
}

proc RamDebugger::MyNameIs { name id } {
    variable services

    set services($name) $id
}

proc RamDebugger::FindActivePrograms { force } {
    variable services
    variable debuggerserver
    variable debuggerserverNum
    variable CheckRemotes

    if { !$CheckRemotes } {
	# dirty trick to make array exist
	set services(11) ""
	unset services(11)
	return
    }

    if { $::tcl_platform(platform) == "windows" } {
	if { [info exists services] && !$force } { return }
	catch { unset services }

	for { set i 12350 } { $i < 12360 } { incr i } {
	    if { $i == $debuggerserverNum } { continue }
	    set comm [list comm::comm send -async $i \
			  [list catch [list comm::givename $debuggerserverNum]]]
	    if { [catch $comm] } { break }
	}
	# dirty trick to make array exist
	set services(11) ""
	unset services(11)
    } else {
	catch { unset services }
	foreach i [winfo interps] {
	    if { $i == $debuggerserver } { continue }
	    set services($i) $i
	}
    }
}

proc RamDebugger::RecieveTimeFromProgram { name time } {
    variable TimeMeasureData

    set ipos 0
    foreach i $TimeMeasureData {
	foreach "name_in file lineini lineend lasttime" $i {
	    if { $name == $name_in } {
		if { $lasttime == "" } { set lasttime 0 }
		incr lasttime $time
		set i [lreplace $i 4 4 $lasttime]
		set TimeMeasureData [lreplace $TimeMeasureData $ipos $ipos $i]
		return
	    }
	}
	incr ipos
    }
    error "error recieving from program. Time measure block '$name' does not exists"
}

proc RamDebugger::RecieveFromProgramExpr { err val } {
    variable ExpressionResult

    after 0 [list set RamDebugger::ExpressionResult [list $err $val]]
    return ""
}

proc RamDebugger::RecieveFromProgram { breaknum filenum line procname textline condinfo } {
    variable fileslist
    variable text

    set file [lindex $fileslist $filenum]
    
    if { ![winfo exists $text] } {
	# non GUI mode
	if { $breaknum == "output" } {
	    puts "output line $breaknum $file $line"
	} elseif { $breaknum } {
	    puts "break $breaknum at $procname $file $line"	
	    if { $textline != "" } { puts "---> $textline" }
	} elseif { $filenum >= 0 } {
	    puts "break at $procname $file $line"
	    if { $textline != "" } { puts "---> $textline" }
	} else {
	    puts "break at $procname local func line: $line"
	    if { $textline != "" } { puts "---> $textline" }
	}
	if { $condinfo != "" } {
	    puts "Condition results: $condinfo"
	}
    } elseif { $file != "" && $line != ""  } {
	# GUI mode
	after 0 [list RamDebugger::StopAtGUI $file $line $condinfo]
    }
    return ""
}



# proc RamDebugger::Pause {} {
#     #after 100
#     after 500 [list set RamDebugger::PauseVar ""]
#     vwait RamDebugger::PauseVar
# }

proc RamDebugger::EvalRemote { comm } {
    variable remoteserver
    variable remoteserverNum
    variable remoteserverIsLocal

    if { $remoteserver == "" } {
	error "Error: a program to debug must be selected using rdebug"
    }

    if { $remoteserverIsLocal } {
	interp eval local after idle [list $comm]
    } elseif { $::tcl_platform(platform) == "windows" } {
	comm::comm send $remoteserverNum $comm

# 	if { $remoteserver == "Project" } {
# 	    dde poke GIDDDE Project TclCommand $comm
# 	    dde request GIDDDE Project TclCommand
# 	} else {
# 	    set len [string length $comm]
# 	    if { $len < 2100 } {
# 		dde eval $remoteserver $comm
# 		Pause
# 	    } else {
# 		dde eval $remoteserver [list namespace eval RDC [list variable codetoeval ""]]
# 		for { set i 0 } { $i < $len } { incr i 2000 } {
# 		    dde eval $remoteserver [list append RDC::codetoeval [string range $comm \
# 			    $i [expr $i+1999]]]
# 		    Pause
# 		}
# 		dde eval $remoteserver {after idle [set RDC::codetoeval]}
# 		Pause
# 	    }
# 	}
    } else {
	send $remoteserver $comm
    }
}

proc RamDebugger::GiveInstFile { file onlyifnewer IsLocal } {
    variable CacheDir

    if { [info exists CacheDir] } {
	regsub -all {(/|:)} [file join [pwd] [file dirname $file]] \# modpath
	set modpath [string trimright $modpath ".\#"]
	if { $IsLocal == -1 } {
	    set instfile [file join $CacheDir [file tail $file]_$modpath.info]
	} elseif { !$IsLocal } {
	    set instfile [file join $CacheDir [file tail $file]_$modpath.instr]
	} else {
	    set instfile [file join $CacheDir [file tail $file]_$modpath.instrL]
	}
	if { $onlyifnewer } {
	    if { [file exists $instfile] && [file mtime $instfile] > [file mtime $file] } {
		return $instfile
	    } else { return "" }
	} else { return $instfile }
    }
    return ""
}

proc RamDebugger::UpdateRemoteBreaks {} {
    variable breakpoints
    variable fileslist
    variable debuggerstate

    if { $debuggerstate != "debug" } { return }

    EvalRemote { if { [info exists RDC::breaks] } { unset RDC::breaks } }
    foreach i $breakpoints {
	set line [lindex $i 2]
	set filenum [lsearch $fileslist [lindex $i 1]]
	if { $filenum == -1 } { continue }
	EvalRemote [list set RDC::breaks($filenum,$line) [list [lindex $i 0] [lindex $i 3]]]
    }
}


################################################################################
#    Parser and instrument functions
################################################################################

# proc RamDebugger::ParseLine { comm beginline } {

#     set openblock ""
#     set openblockpos ""
#     set before(0) ""
#     set level 0
#     set ielem 0
#     set IsComment 0
#     set length [string length $comm]
#     for { set j 0 } { $j < $length } { incr j } {
# 	set c [string index $comm $j]
# 	switch $openblock {
# 	    "" {
# 		if { [string is space $c] } {
# 		    append before($ielem) $c
# 		    continue 
# 		}
# 		switch -- $c {
# 		    \" {
# 			set openblock \"
# 			lappend openblockpos $j
# 			append before($ielem) $c
# 			set elm($ielem) ""
# 		    }
# 		    \{ {
# 			set openblock \{
# 			lappend openblockpos $j
# 			append before($ielem) $c
# 			set elm($ielem) ""
# 		    }
# 		    \[ {
# 			set openblock \[
# 			lappend openblockpos $j
# 			append before($ielem) ""
# 			set elm($ielem) $c
# 		    }
# 		    ";" {
# 			append before($ielem) ""
# 			set elm($ielem) $c
# 			set after($ielem) ""
# 			incr ielem
# 		    }
# 		    default {
# 			set openblock none
# 			lappend openblockpos $j
# 			append before($ielem) ""

# 			if { $j == "#" && ($ielem == 0 || $elm([expr $ielem-1]) == ";") } {
# 			    set IsComment 1
# 			}
# 			if { $c == "\\" } {
# 			    append elm($ielem) $c
# 			    incr j
# 			    set c [string index $comm $j]
# 			}
# 			append elm($ielem) $c
# 		    }
# 		}
# 	    }
# 	    none {
# 		if { $c == ";" } {
# 		    set after($ielem) ""
# 		    incr ielem
# 		    append before($ielem) ""
# 		    set elm($ielem) $c
# 		    set after($ielem) ""
# 		    incr ielem
# 		    set openblock ""
# 		    set openblockpos [lreplace $openblockpos end end]
# 		}
# 		if { $c == "\[" } {
# 		    set openblock "\["
# 		    lappend openblockpos $j
# 		    append elm($ielem) $c
# 		} elseif { [string is space $c] } {
# 		    set after($ielem) ""
# 		    incr ielem
# 		    append before($ielem) $c
# 		    set openblock ""
# 		    set openblockpos [lreplace $openblockpos end end]
# 		} else {
# 		    if { $c == "\\" } {
# 			incr j
# 			set c [string index $comm $j]
# 			append elm($ielem) "\\"
# 		    }
# 		    append elm($ielem) $c
# 		}
# 	    }
# 	    \" {
# 		if { $c == "\"" } {
# 		    set nextpos [expr $j+1]
# # 		    if { !$IsComment && $nextpos < $length && ![string is space -strict \
# # 						     [string index $comm $nextpos]] } {
# # 			incr beginline [regexp -all {\n} [string range $comm 0 $j]]
# # 			error "error: Not legal char after \" in line $beginline"
# # 		    }
# 		    set after($ielem) \"
# 		    incr ielem
# 		    set openblock ""
# 		    set openblockpos [lreplace $openblockpos end end]
# 		} else {
# 		    if { $c == "\\" } {
# 			incr j
# 			set c [string index $comm $j]
# 			append elm($ielem) "\\"
# 		    }
# 		    append elm($ielem) $c
# 		}
# 	    }
# 	    \{ {
# 		if { $c == "\}" } {
# 		    if { !$level } {
# 			set nextpos [expr $j+1]
# 			if { !$IsComment && $nextpos < $length && ![string is space -strict \
# 							 [string index $comm $nextpos]] } {
# 			    incr beginline [regexp -all {\n} [string range $comm 0 $j]]
# 			    error "error: Not legal char after \} in line $beginline"
# 			}
# 			set after($ielem) \}
# 			incr ielem
# 			set openblock ""
# 			set openblockpos [lreplace $openblockpos end end]
# 		    } else {
# 			incr level -1
# 			append elm($ielem) $c
# 		    }
# 		} else {
# 		    if { $c == "\\" } {
# 			set c2 [string index $comm [expr $j+1]]
# 			if { $c2 == "\{" || $c2 == "\}" } {
# 			    incr j
# 			    set c [string index $comm $j]
# 			    append elm($ielem) "\\"
# 			}
# 		    } elseif { $c == "\{" } { incr level }
# 		    append elm($ielem) $c
# 		}
# 	    }
# 	    \[ {
# 		if { $c == "\]" } {
# 		    if { !$level } {
# 			set openblockpos [lreplace $openblockpos end end]
# 			set openblock none
# 			lappend openblockpos $j
# 		    } else { incr level -1 }
# 		    append elm($ielem) $c
# 		} else {
# 		    if { $c == "\[" } { incr level }
# 		    if { $c == "\\" } {
# 			incr j
# 			set c [string index $comm $j]
# 			append elm($ielem) "\\"
# 		    }
# 		    append elm($ielem) $c
# 		}
# 	    }

# 	}
#     }
#     if { $openblock == "none" } {
# 	set after($ielem) ""
# 	incr ielem
# 	set openblock ""
# 	set openblockpos [lreplace $openblockpos end end]
#     }
# #     if { $openblock != "" } {
# # 	incr beginline [regexp -all {\n} [string range $comm 0 $openblockpos]]

# # 	puts "error: there is an open block in '$comm'. Type '$openblock' in line $beginline"
# # 	puts "----------------------"
# # 	for { set i 0 } { $i < [expr $ielem+1] } { incr i } {
# # 	    catch {
# # 		puts "before=$before($i)"
# # 		puts "elm=$elm($i)"
# # 		puts "after=$after($i)"
# # 	    }
# # 	}
# # 	if { [string length $comm] > 100 } {
# # 	    set comm [string range $comm 0 96]...
# # 	}
# # 	error "error: there is an open block in '$comm'. Type '$openblock' in line $beginline"
# #     }
#     set elms ""
#     for { set i 0 } { $i < $ielem } { incr i } {
# 	if { [catch { 
# 	    lappend elms $before($i) $elm($i) $after($i)
# 	}]} {
# 	    puts $::errorInfo
# 	    error $comm
# 	}
#     }
#     return $elms
# }

# type can be: "", switch or proc
# proc RamDebugger::InstrumentBlock { block filenum line level { type "" } } {

#     set lines [split $block \n]
#     set length [llength $lines]
#     set newlines ""


#     if { $type == "switch" } { set GlobalElem 0 }

#     for { set i 0 } { $i < $length } { incr i } {

# 	if { $level == 0 } { ProgressVar [expr $i*100/$length] }

# 	while { [string trim [lindex $lines $i]] == "" } {
# 	    if { $level > 0 } { lappend newlines [lindex $lines $i] }
# 	    if { $i >= $length } { break }
# 	    incr i
# 	}
# 	if { $i >= $length } { break }
# 	set iini $i
# 	set iline [expr $line+$i]
# 	set comm ""
# 	while 1 {
# 	    if { $i == $length } {
# 		error "error: there is an open block beginning at line $iline"
# 	    }
# 	    append comm [lindex $lines $i]
# 	    if { [info complete $comm] && [string index $comm end] != "\\" } { break }
# 	    append comm \n
# 	    incr i
# 	}
# 	set elms [ParseLine $comm $iline]
# 	set newcomm ""
# 	set iend $iini
# 	set toeval ""
# 	set toevalasswitch ""
# 	set toevalasproc ""
# 	set weareinproc 0
# 	set length2 [llength $elms]

# 	set strpos 0
# 	for { set j 0 } { $j < $length2 } { incr j } {
# 	    set before [lindex $elms $j]
# 	    incr strpos [string length $before]
# 	    incr j
# 	    set elm [lindex $elms $j]
# 	    if { [string index $elm 0] == "#" && [string trim $before] == "" && \
# 		     ($j == 1 || [lindex $elms [expr $j-3]] == ";") } {
# 		# this is a comment
# 		set elm [string range $comm $strpos end]
# 		append newcomm $elm
# 		incr iend [regexp -all {\n} $elm]
# 		break
# 	    }
# 	    incr strpos [string length $elm]							       

# 	    incr j
# 	    set after [lindex $elms $j]
# 	    incr strpos [string length $after]

# 	    if { $type == "switch" } {
# 		if { $GlobalElem%2 } {
# 		    lappend toeval $j
# 		}
# 		incr GlobalElem
# 	    } else {
# 		set blockrprocs3 [list else catch]
# 		if { [lsearch $blockrprocs3 $elm] != -1 } { lappend toeval [expr $j+3] }
# 		set blockrprocs6 [list if while elseif]
# 		if { [lsearch $blockrprocs6 $elm] != -1 } { lappend toeval [expr $j+6] }
# 		set blockrprocs9 [list foreach]
# 		if { [lsearch $blockrprocs9 $elm] != -1 } { lappend toeval [expr $j+9] }
# 		set blockrprocs12 [list for]
# 		if { [lsearch $blockrprocs12 $elm] != -1 } { lappend toeval [expr $j+12] }
		
# 		if { $elm == "proc" } {
# 		    lappend toevalasproc [expr $j+9]
# 		    set weareinproc 1
# 		} elseif { $elm == "switch" } {
# 		    set FirstNoOption -1
# 		    set NumArgs 0
# 		    for { set k [expr $j+2] } { $k < $length2 } { incr k 3 } {
# 			if { [lindex $elms $k] == ";" } {
# 			    break
# 			} elseif { $FirstNoOption > -1 } {
# 			    incr NumArgs
# 			} elseif { [lindex $elms $k] == "--" } {
# 			    set FirstNoOption [expr $k+3]
# 			} elseif { ![string match -* [lindex $elms $k]] } {
# 			    set FirstNoOption $k
# 			    incr NumArgs
# 			}
# 		    }
# 		    if { $NumArgs == 2 } {
# 			lappend toevalasswitch [expr $j+6]
# 		    } else {
# 			for { set k [expr $FirstNoOption+3] } { $k < $length2 } { incr k 6 } {
# 			    if { [lindex $elms $k] == ";" } {
# 				break
# 			    }
# 			    lappend toeval $k
# 			}
# 		    }
# 		}
# 	    }

# 	    if { $level > 0 || $weareinproc } {
# 		append newcomm $before
# 	    }
# 	    incr iend [regexp -all {\n} $before]

# 	    if { $level > 0 && $j == 2 && $type != "switch" } {
# 		# the type=proc will be used in the future to mark end of functions
# 		# and returns in order to know when quitting of a function
# 		append newcomm "RDC::F $filenum [expr $line+$iini] ; "
# 	    }
# 	    if { [lsearch $toevalasproc $j] != -1 && [regexp {\n} $elm] } {
# 		append newcomm [InstrumentBlock $elm $filenum [expr $line+$iend] [expr $level+1] proc]
# 	    } elseif { ($level > 0 || $weareinproc) && \
# 			   [lsearch $toevalasswitch $j] != -1 && [regexp {\n} $elm] } {
# 		append newcomm  [InstrumentBlock $elm $filenum [expr $line+$iend] [expr $level+1] switch]
# 	    } elseif { ($level > 0 || $weareinproc) && [lsearch $toeval $j] != -1 && [regexp {\n} $elm] } {
# 		append newcomm [InstrumentBlock $elm $filenum [expr $line+$iend] [expr $level+1]]
# 	    } elseif { ($level > 0  || $weareinproc) } {
# 		append newcomm $elm
# 	    }
# 	    incr iend [regexp -all {\n} $elm]
# 	    if { $level > 0 || $weareinproc } {
# 		append newcomm $after
# 	    }
# 	    incr iend [regexp -all {\n} $after]
# 	}
# 	eval lappend newlines [split $newcomm \n]
# 	#set lines [eval lreplace [list $lines] $iini $iend [split $newcomm \n]]
#     }
#     if { $level == 0 } { ProgressVar 100 }
#     return [join $newlines \n]
# }

################################################################################
#               RamDebugger::Instrumenter
################################################################################

namespace eval RamDebugger::Instrumenter {
    variable stack
    variable words
    variable currentword
    variable wordtype
    variable wordtypeline
    variable wordtypepos
    variable DoInstrument
    variable DoOutput

    variable level
    variable colors
}

proc RamDebugger::Instrumenter::InitState {} {
    variable stack ""
    variable words ""
    variable currentword ""
    variable wordtype ""
    variable wordtypeline ""
    variable wordtypepos ""
    variable DoInstrument 0
    variable DoOutput 0
    variable level 0
    variable colors

    foreach i [list return break while eval foreach for if else elseif error switch default] {
	set colors($i) magenta
    }
    foreach i [list variable set] {
	set colors($i) green
    }
}

proc RamDebugger::Instrumenter::PushState { type line } {
    variable stack
    variable words
    variable currentword
    variable wordtype
    variable wordtypeline
    variable wordtypepos
    variable DoInstrument
    variable DoOutput
    variable level

    set NewDoInstrument 0
    if { $DoOutput } {
	set NewDoOutput 2
    } else { set NewDoOutput 0 }

    set PushState 0
    if { $type == "\[" } {
	set PushState 1
	if { $DoInstrument == 1 } {
	    set NewDoInstrument 1
	} else {
	    set NewDoInstrument 0
	}
    } else {
	if { [lindex $words 0] == "proc" && [llength $words] == 3 } {
	    set NewDoInstrument 1
	} elseif { $DoInstrument == 0 } {
	    if { [lindex $words 0] == "namespace" && [lindex $words 1] == "eval" && \
		     [llength $words] >= 3 } {
		set PushState 1
	    }
	} elseif { $DoInstrument == 1 } {
	    switch -- [lindex $words 0] {
		"if" {
		    if { [llength $words] == 2 } {
			set NewDoInstrument 1
		    } elseif { [lindex $words end] == "then" || [lindex $words end] == "else" } {
			set NewDoInstrument 1
		    } elseif { [lindex $words end-1] == "elseif" } {
			set NewDoInstrument 1
		    }
		}
		"namespace" {
		    if { [lindex $words 1] == "eval" && [llength $words] >= 3 } {
			set NewDoInstrument 1
		    }
		}
		"catch" {
		    if { [llength $words] == 1 } {
			set NewDoInstrument 1
		    }
		}
		"while" {
		    if { [llength $words] == 2 } {
			set NewDoInstrument 1
		    }
		}
		"foreach" {
		    if { [llength $words] >= 3 } {
			set NewDoInstrument 1
		    }
		}
		"for" {
		    if { [llength $words] == 4 } {
			set NewDoInstrument 1
		    }
		}
		"eval" {
		    set NewDoInstrument 1
		}
		"uplevel" {
		    set len [llength $words]
		    if { [regexp {[\#]?[0-9]+} [lindex $words 1]] } {
		        incr len -1
		    }
		    if { $len > 0 } {
			set NewDoInstrument 1
		    }
		}
		"switch" {
		    for { set i 1 } { $i < [llength $words] } { incr i } {
			if { [lindex $words $i] == "--" } {
			    incr i
			    break
			} elseif { ![string match -* [lindex $words $i]] } { break }
		    }
		    set len [expr [llength $words]-$i]
		    if { $len == 1 } {
			set NewDoInstrument 2
		    }
		}
	    }
	} elseif { $DoInstrument == 2 } {
	    if { [llength $words]%2 } {
		set NewDoInstrument 1
	    }
	}
    }
    if { !$PushState && !$NewDoInstrument } { return 1 }

    incr level
    lappend stack [list $words $currentword $wordtype $wordtypeline \
	$wordtypepos $DoInstrument $DoOutput $line $type]

    set words ""
    set currentword ""
    set wordtype ""
    set wordtypeline ""
    set wordtypepos ""
    set DoInstrument $NewDoInstrument
    set DoOutput $NewDoOutput
    return 0
}

proc RamDebugger::Instrumenter::PopState { type line } {
    variable stack
    variable wordtype
    variable words
    variable currentword
    variable wordtype
    variable wordtypeline
    variable wordtypepos
    variable DoInstrument
    variable DoOutput
    variable level

    set lasttype [lindex [lindex $stack end] end]
    if { $type == "\]" && $lasttype != "\[" } { return 1 }

    if { $type == "\}" } {
	if { $wordtype == "w" } {
	    set numopen 0
	    for { set i 0 } { $i < [string length $currentword] } { incr i } {
		switch -- [string index $currentword $i] {
		    "\\" { incr i }
		    \{ { incr numopen }
		    \} { incr numopen -1 }
		}
	    }
	    if { $numopen } { return 1 }
	}
	if { $lasttype != "\{" } {
	    foreach i $stack {
		if { [lindex $i end] == "\{" } {
		    set text "Using a close brace (\}) in line $line when there is an open brace "
		    append text "in line [lindex $i end-1] and an open bracket (\[) in line "
		    append text "[lindex [lindex $stack end] end-1]"
		    error $text
		}
		return 1
	    }
	} 
    }
    foreach "words currentword wordtype wordtypeline wordtypepos DoInstrument DoOutput" \
        [lindex $stack end] break
    set stack [lreplace $stack end end]
    incr level -1

    return 0
}

proc RamDebugger::Instrumenter::CheckEndOfFileState {} {
    variable stack

    set text ""
    foreach i $stack {
	set line [lindex $i end-1]
	set type [lindex $i end]
	append text "There is a block of type ($type) beginning at line $line "
	append text "that is not closed at the end of the file\n"
    }
    if { $text != "" } {
	error $text
    }
}

proc RamDebugger::Instrumenter::DoWork { block filenum newblockname blockinfoname \
	InstrumentOnlyProcs } {

    variable words
    variable currentword
    variable wordtype
    variable wordtypeline
    variable DoInstrument
    variable DoOutput
    variable level
    variable colors

    RamDebugger::ProgressVar 0
    set length [string length $block]

    upvar $newblockname newblock
    upvar $blockinfoname blockinfo
    set newblock ""
    set blockinfo ""
    set blockinfocurrent [list 0 n]
    InitState

    if { $InstrumentOnlyProcs } {
	set DoInstrument 0
	set DoOutput 0
    } else {
	set DoInstrument 1
	set DoOutput 2
    }
    append newblock "# RamDebugger instrumented file. InstrumentOnlyProcs=$InstrumentOnlyProcs\n"

    set checkExtraCharsAfterCQB ""
    set braceslevel 0
    set lastc ""
    set lastinstrumentedline ""
    set line 1
    set ichar 0
    set icharline 0
    foreach c [split $block ""] {

	if { $ichar%1000 == 0 } {
	    RamDebugger::ProgressVar [expr $ichar*100/$length]
	}
	if { $checkExtraCharsAfterCQB != "" } {
	    if { ![string is space $c] && $c != "\}" && $c != "\]" && $c != "\\" } {
		if { $c == "\"" && $checkExtraCharsAfterCQB == "\}" } {
		    # nothing
		} else {
		    set text "There is a non valid character ($c) in line $line "
		    append text "after a closing block with ($checkExtraCharsAfterCQB)"
		    error $text
		}
	    }
	    set checkExtraCharsAfterCQB ""
	}
	if { $DoInstrument == 1 && $DoOutput && $lastinstrumentedline != $line && \
	    ![string is space $c] && \
	     $c != "\#" && $words == "" } {
	    append newblock "RDC::F $filenum $line ; "
	    set lastinstrumentedline $line
	}
	set consumed 0
	switch -- $c {
	    \" {
		if { $lastc != "\\" && [lindex $words 0] != "\#" } {
		    if { $wordtype == "" } {
			set wordtype \"
			set wordtypeline $line
			set wordtypepos $icharline
			set consumed 1
		    } elseif { $wordtype == "\"" } {
			set wordtype ""
			lappend words $currentword
			set currentword ""
			set consumed 1
			set checkExtraCharsAfterCQB \"

			if { $wordtypeline == $line } {
			    lappend blockinfocurrent grey $wordtypepos [expr $icharline+1]
			} else {
			    lappend blockinfocurrent grey 0 [expr $icharline+1]
			}

			if { $DoOutput == 0 && $words == "proc" } {
			    append newblock $words
			    set DoOutput 1
			}
		    } elseif { $wordtype == "\{" } {
			if { ![info exists quoteintobraceline] } {
			    set quoteintobraceline $line
			    set quoteintobracepos $icharline
			} else {
			    if { $line == $quoteintobraceline } {
				lappend blockinfocurrent grey $quoteintobracepos [expr $icharline+1]
			    }
			    unset quoteintobraceline quoteintobracepos
			}
		    }
		}
	    }
	    \{ {
		if { $lastc != "\\" } {
		    if { $wordtype == "\{" } {
			incr braceslevel
		    } elseif { $wordtype != "\"" && $wordtype != "w" } {
			set consumed 1
			set fail [PushState \{ $line]
			if { $fail } {
			    set wordtype \{
			    set wordtypeline $line
			    set braceslevel 0
			} else {
			    set lastinstrumentedline $line
			}
		    }
		}
	    }
	    \} {
		if { $lastc != "\\" } {
		    if { $wordtype == "\{" } {
			if { $braceslevel } {
			    incr braceslevel -1
			} else {
			    set wordtype ""
			    lappend words $currentword
			    set currentword ""
			    set consumed 1
			    if { [lindex $words 0] != "\#" } {
				set checkExtraCharsAfterCQB \}
			    }
			    if { $DoOutput == 0 && $words == "proc" } {
				append newblock $words
				set DoOutput 1
			    }
			}
		    } elseif { $wordtype != "\"" } {
			set fail [PopState \} $line]
			if { !$fail } {
			    set consumed 1
			    lappend words ""
			    if { [lindex $words 0] != "\#" } {
				set checkExtraCharsAfterCQB \}
			    }
			}
		    }
		}
	    }
	    " " - \t {
		if { $lastc != "\\" } {
		    if { $wordtype == "w" } {
			set consumed 1
			set wordtype ""
			lappend words $currentword
			set currentword ""

			if { $DoOutput == 0 && $words == "proc" } {
			    append newblock $words
			    set DoOutput 1
			}

			if { [lindex $words 0] == "proc" } {
			    if { [llength $words] == 1 } {
				set icharlineold [expr $icharline-4]
				lappend blockinfocurrent magenta $icharlineold $icharline
			    } elseif { [llength $words] == 2 } {
				set icharlineold [expr $icharline-[string length [lindex $words end]]]
				lappend blockinfocurrent blue $icharlineold $icharline
			    }
			} elseif { [info exists colors([lindex $words end])] } {
			    set icharlineold [expr $icharline-[string length [lindex $words end]]]
			    lappend blockinfocurrent $colors([lindex $words end]) $icharlineold \
			       $icharline
			}
		    } elseif { $wordtype == "" } { set consumed 1 }
		}
	    }
	    \[ {
		if { $lastc != "\\" && $wordtype != "\{" } {
		    if { $wordtype == "" } { set wordtype "w" }
		    set consumed 1
		    PushState \[ $line
		    set lastinstrumentedline $line
		}
	    }
	    \] {
		if { $lastc != "\\" && $wordtype != "\"" && $wordtype != "\{" } {
		    set fail [PopState \] $line]
		    if { !$fail } {
			set consumed 1
		    }
		    # note: the word inside words is not correct when using []
		}
	    }
	    \n {
		if { [lindex $words 0] == "\#" } {
		    lappend blockinfocurrent red 0 $icharline
		} elseif { $wordtype == "\"" } {
		    lappend blockinfocurrent grey $wordtypepos $icharline
		}
		lappend blockinfo $blockinfocurrent
		incr line
		set blockinfocurrent [expr $level+$braceslevel]

		if { $wordtype != "\{" } {
		    set consumed 1
		    if { $lastc != "\\" } {
			if { $wordtype == "\"" } {
			    set text "Quotes (\") in line $line "
			    append text "are not closed"
			    error $text
			}
			set words ""
			set currentword ""
			set wordtype ""

			if { $DoOutput == 1 } {
			    append newblock $c
			    set DoOutput 0 
			}
			lappend blockinfocurrent "n"
		    } else {
			if { $wordtype == "w" } {
			    set wordtype ""
			    lappend words $currentword
			    set currentword ""
			}
			lappend blockinfocurrent "c"
		    }
		} else { lappend blockinfocurrent "n" }
	    }
	    \# {
		if { [llength $words] == 0 && $currentword == "" && $wordtype == "" } {
		    set consumed 1
		    lappend words \#
		}
	    }
	    ; {
		if { $lastc != "\\" && $wordtype != "\"" && $wordtype != "\{" } {
		    set consumed 1
		    set words ""
		    set currentword ""
		    set wordtype ""

		    if { $DoOutput == 1 } {
			append newblock $c
			set DoOutput 0
		    }
		}
	    }
	}
	if { $DoOutput } { append newblock $c }
	if { !$consumed } {
	    if { $wordtype == "" } {
		set wordtype w
		set wordtypeline $line
	    }
	    append currentword $c
	}
	if { $lastc == "\\" && $c == "\\" } {
	    set lastc "\\\\"
	} else { set lastc $c }
	incr ichar

	if { $c == "\t" } {
	    incr icharline 8
	} elseif { $c != "\n" } {
	    incr icharline
	} else { set icharline 0 }
    }
    if { $wordtype != "" && $wordtype != "w" } {
	set text "There is a block of type ($wordtype) beginning at line $wordtypeline "
	append text "that is not closed at the end of the file"
	error $text
    }
    CheckEndOfFileState

    lappend blockinfo $blockinfocurrent

    RamDebugger::ProgressVar 100
}

proc RamDebugger::Instrumenter::DoWorkForTime { block filename newblockname timedata } {

    upvar $newblockname newblock
    set newblock "# RamDebugger time instrumented file\n"

    set lines [split $block \n]

    set lastline ""
    set lastpos ""
    foreach i $timedata {
	foreach "name file lineini lineend lasttime" $i {
	    if { $file != $filename } { continue }
	    if { $lineini != $lastline } { set lastpos 0 }
	    set text "RDC::MeasureTime [list $name] \[time { "
	    set linepos [expr $lineini-1]
	    set line [lindex $lines $linepos]
	    set newline [string range $line 0 [expr $lastpos-1]]
	    append newline $text [string range $line $lastpos end]
	    set lines [lreplace $lines $linepos $linepos $newline]
	    set lastline $lineini
	    set lastpos [expr $lastpos+[string length $text]]

	    set linepos [expr $lineend-1]
	    set newline "[lindex $lines $linepos] }]"
	    set lines [lreplace $lines $linepos $linepos $newline]
	}
    }

    regsub -all {RDC::F [0-9]+ [0-9+] ; } [join $lines \n] {} newblock2
    append newblock $newblock2
}

################################################################################
#                   RamDebugger GUI
################################################################################

proc WarnWin { text { par .}} {

    if { $par == "."} {
	set w .__WarnWin
    } else {
	set w $par.__WarnWin
    }
    #tk_dialogRAMFull $par "" $text "" "" $::Images::questionmark 0 OK
    #tk_messageBox -icon warning -message $text -parent $par -type ok

    tk_dialog  $w Warning $text warning 0 OK
}


# tkTabToWindow --
# This procedure moves the focus to the given widget.  If the widget
# is an entry, it selects the entire contents of the widget.
#
# Arguments:
# w - Window to which focus should be set.

proc tkTabToWindow {w} {
    focus $w
    
    after 100 {
	set w [focus]
	if {[string equal [winfo class $w] Entry]} {
	    $w selection range 0 end
	    $w icursor end
	}
    }
}


# orient must be: h or v
proc RamDebugger::ManagePanes { panedw orient default } {
    variable opts

    if { [info exists opts(paneweights,$orient,$panedw)] } {
	return $opts(paneweights,$orient,$panedw)
    } else {
	return [set opts(paneweights,$orient,$panedw) $default]
    }
}

proc RamDebugger::ExitGUI {} {
    variable opts
    variable text
    variable remoteserver
    variable remoteserverIsLocal
    variable EvalEntries
    variable currentfile
    variable currentline
    variable breakpoints

    set opts(watchedvars) ""
    set i 0
    while 1 {
	if { ![info exists EvalEntries($i,left)] } { break }
	lappend opts(watchedvars) $EvalEntries($i,left)
	incr i
    }
    foreach i [array names opts paneweights,*] {
	regexp {paneweights,(.*),(.*)} $i {} orient panedw
	set opts($i) ""
	if { [winfo exists $panedw] } {
	    set idx 0
	    while { [set pane [$panedw getframe $idx]] != "" } {
		switch $orient {
		    h { lappend opts($i) [winfo width $pane] }
		    v { lappend opts($i) [winfo height $pane] }
		}
		incr idx
	    }
	} else { unset opts($i) }
    }
    set opts(currentfile) $currentfile
    set opts(currentidx) [$text index insert]
    set opts(breakpoints) $breakpoints

    set opts(remoteserverIsLocal) $remoteserverIsLocal
    set opts(remoteserver) $remoteserver
    set opts(maingeometry) [wm geometry [winfo toplevel $text]]
    if { $::tcl_platform(platform) == "windows" } {
	registry set {HKEY_CURRENT_USER\Software\RamDebugger} IniData [array get opts]
    } else {
	set fout [open ~/.ramdebugger w]
	puts -nonewline $fout [array get opts]
	close $fout
    }
    if { [info command exit_final] != "" } {
	exit_final
    } else { exit }
}

proc RamDebugger::Colorize {} {
    variable text
    variable instrumentedfilesInfo
    variable currentfile

    $text conf -editable 1
    $text tag conf magenta -foreground magenta
    $text tag conf blue -foreground blue
    $text tag conf grey -foreground grey
    $text tag conf green -foreground green
    $text tag conf red -foreground red

    set textO [$text original]
    set iline 1
    foreach i $instrumentedfilesInfo($currentfile) {
	foreach "tag li le" [lrange $i 2 end] {
	    $textO tag add $tag $iline.$li $iline.$le
	}
	incr iline
    }
    $text tag raise sel
    $text conf -editable 0
}

proc RamDebugger::ColorizeSlow {} {
    variable text

    $text conf -editable 1
    $text tag conf magenta -foreground magenta
    $text tag conf blue -foreground blue
    $text tag conf grey -foreground grey
    $text tag conf green -foreground green
    $text tag conf red -foreground red

    set idx 1.0
    while 1 {
	set idx2 [$text search -count RamDebugger::count -regexp {proc\s+\S+} $idx end]
	if { $idx2 == "" } { break }
	$text tag add magenta $idx2 $idx2+4c
	$text tag add blue $idx2+5c $idx2+${RamDebugger::count}c
	set idx [$text index $idx2+${RamDebugger::count}c]
    }

    set string {[^\\]\"([^\"]+[^\\\"])?\"}
    set magentas {\m(return|break|while|eval|foreach|for|if|else|elseif|error|switch|default)\M}
    set greens {\mvariable\M}
    set comments {#.*$}

    set idx 1.0
    while 1 {
	set idx2 [$text search -count RamDebugger::count -regexp \
		      $string|$magentas|$greens|$comments $idx end]
	if { $idx2 == "" } { break }
	foreach "rex tag icr" [list $string grey 1 $magentas magenta 0 $greens green 0 \
				   $comments red 0] {
	    set idx3 [$text search -regexp $rex $idx2 $idx2+${RamDebugger::count}c]
	    if { $idx3 == $idx2 } {
		$text tag add $tag $idx2+${icr}c $idx2+${RamDebugger::count}c
		break
	    }
	}
	set idx [$text index $idx2+${RamDebugger::count}c]
    }
    $text tag raise sel
    $text conf -editable 0
}

proc RamDebugger::OpenFile { w } {
    variable opts

    set types {
	{{TCL Scripts}      {.tcl}        }
	{{All Files}        *             }
    }
    if { ![info exists opts(defaultdir)] } { set opts(defaultdir) [pwd] }
    set file [tk_getOpenFile -filetypes $types -initialdir $opts(defaultdir) -parent $w \
		  -title "Open source file"]
    if { $file == "" } { return }
    set opts(defaultdir) [file dirname $file]
    OpenFileF $file
}

proc RamDebugger::OpenFileF { file { force 0 } } {
    variable marker
    variable text
    variable files
    variable breakpoints
    variable currentfile

    WaitState 1

    if { !$force } {
	set comm [list rlist -quiet $file {}]
    } else { set comm [list rlist -quiet -force $file {}] }
    if { [catch $comm errstring] } {
	WaitState 0
	WarnWin $errstring
	return 1
    }
    if { $file == $currentfile } {
	set idx [$text index insert]
    } else { set idx 1.0 }

    $marker delete arrow
    $marker delete break
    $marker delete arrowbreak
    $text conf -editable 1
    set textO [$text original]
    $textO del 1.0 end
    $textO ins end [string map [list "\t" "        "] $files($file)]
    $textO tag add normal 1.0 end
    $text conf -editable 0

    Colorize
    FillListBox

    foreach i $breakpoints {
	if { [lindex $i 1] != $file } { continue }
	set line [lindex $i 2]
	UpdateArrowAndBreak $line 1 ""
    }
    UpdateRemoteBreaks

    set Numlines [scan [$text index end-1c] %d]
    set font [$text cget -font]
    $marker configure -scrollregion [list 0 0 [winfo reqwidth $marker] \
					 [expr $Numlines*[font metrics $font -linespace]]]
    $text mark set insert $idx
    $text see $idx

    wm title [winfo toplevel $text] "RamDebugger     [file tail $currentfile]"

    WaitState 0
    return 0
}

proc RamDebugger::ViewInstrumentedFile { what } {
    variable marker
    variable text
    variable currentfile
    variable instrumentedfiles
    variable instrumentedfilesInfo

    if { $currentfile == "" } {
	WarnWin "There is no file to see its instrumented file"
	return
    }
    if { $what == "instrumented" } {
	if { ![info exists instrumentedfiles($currentfile)] } {
	    WarnWin "There is no instrumented file for file '$currentfile'"
	    return
	}
    } else {
	if { ![info exists instrumentedfilesInfo($currentfile)] } {
	    WarnWin "There is no instrumented info file for file '$currentfile'"
	    return
	}
    }
    WaitState 1

    $text conf -editable 1
    $text del 1.0 end
    if { $what == "instrumented" } {
	$text ins end [string map [list "\t" "        "] $instrumentedfiles($currentfile)]
    } else {
	foreach i $instrumentedfilesInfo($currentfile) {
	    $text ins end [string map [list "\t" "        "] $i\n]
	}
    }
    $text tag add normal 1.0 end
    $text conf -editable 0
    ColorizeSlow
    WaitState 0

    $marker delete arrow
    $marker delete break
    $marker delete arrowbreak

    wm title [winfo toplevel $text] "RamDebugger      [file tail $currentfile] instrumented"

    set currentfile ""
}

proc RamDebugger::ViewHelpFile {} {
    variable MainDir

    HelpViewer::HelpWindow [file join $MainDir help]
}

proc RamDebugger::DebugProgram { name } {
    variable currentfile

    rdebug $name
    if { $currentfile != "" } {
	rlist -quiet $currentfile
    }
}
proc RamDebugger::ActualizeActivePrograms { menu { force 0 } } {
    variable mainframe
    variable remoteserver

    WaitState 1
    SetMessage "Searching for active programs..."

    if { $force } {
	set services [rdebug -forceupdate -actives]
    } else {
	set services [rdebug -actives]
    }

    $menu del 0 end
    if { [llength $services] == 0 } {
	$menu add command -label "There are no active programs" -state disabled
    } else {
	foreach i $services {
	    $menu add command -label $i -command [list RamDebugger::DebugProgram $i]
	}
    }
    $menu add separator
    $menu add command -label "Current file" -command {
	RamDebugger::rdebug -currentfile
    }
    if { $::tcl_platform(platform) == "windows" } {
	$menu add command -label Update -command "RamDebugger::ActualizeActivePrograms $menu 1"
    }
    $menu add command -label Disconnect -command {
	if { [catch [list RamDebugger::rdebug -disconnect] errstring] } {
	    WarnWin $errstring
	}
    }
    if { $remoteserver == "" } {
	$menu entryconfigure end -state disabled
    }
    SetMessage ""
    WaitState 0
}

proc RamDebugger::SetGUIBreakpoint {} {
    variable images
    variable marker
    variable text
    variable remoteserver
    variable currentfile

    if { $currentfile == "" } {
	WarnWin "This file does not admit breakpoints"
	return
    }
    set idx [$text index insert]

    if { $idx == "" } {
	WarnWin "Before setting breakpoint, select something or pick in the text"
	return
    }
    WaitState 1
    $text see $idx
    set line [scan $idx "%d"]

    set hasbreak 0
    foreach i [$marker gettags l$line] {
	switch $i arrowbreak - break { set hasbreak 1 }
    }
    if { $hasbreak } {
	set hasbreak 0
	foreach num [rinfo $line] {
	    rdel $num
	}
    } else {
	set hasbreak 1
	if { [catch [list rbreak $line] errorstring] } {
	    WarnWin $errorstring
	    set hasbreak 0
	}
    }
    UpdateArrowAndBreak $line $hasbreak ""
    WaitState 0
}

proc RamDebugger::UpdateArrowAndBreak { line hasbreak hasarrow } {
    variable marker
    variable text
    variable textST
    variable images
    variable IsInStop


    set hadarrow 0
    foreach i [$marker gettags l$line] {
	switch $i {
	    arrowbreak {
		if { $hasbreak == "" } { set hasbreak 1 }
		if { $hasarrow == "" } { set hasarrow 1 }
		set hadarrow 1
	    }
	    arrow {
		if { $hasarrow == "" } { set hasarrow 1 }
		set hadarrow 1
	    }
	    break {
		if { $hasbreak == "" } { set hasbreak 1 }
	    }
	}
    }
    if { $hasbreak == "" } { set hasbreak 0 }
    if { $hasarrow == "" } { set hasarrow 0 }

    $marker delete l$line

    if { !$hasarrow } { set IsInStop 0 } else { set IsInStop 1 }

    if { !$hasbreak && !$hasarrow } {
	$textST conf -state normal
	$textST del 1.0 end
	$textST conf -state disabled
	return
    }

    set font [$text cget -font]
    set ypos [expr ($line-1)*[font metrics $font -linespace]+[font metrics $font -ascent]+\
		  [$text cget -pady]+2]
    if { $hasarrow && $hasbreak } {
	$marker create image 0 $ypos -anchor sw -image $images(arrowbreak) -tags "arrowbreak l$line"
    } elseif { $hasarrow } {
	$marker create image 0 $ypos -anchor sw -image $images(arrow) -tags "arrow l$line"
    } elseif { $hasbreak } {
	$marker create image 0 $ypos -anchor sw -image $images(break) -tags "break l$line"
    }

    if { $hasarrow } {
	if { !$hadarrow } {
	    after 100 "raise [winfo toplevel $text] ; focus -force $text"
	}
	$text see $line.0
	$text mark set insert $line.0
    }

    if { $IsInStop } {
	after 100 RamDebugger::CheckEvalEntries do
	after 200 RamDebugger::CheckEvalEntriesL do
	rstack -handler RamDebugger::UpdateGUIStack
    } else {
	$textST conf -state normal
	$textST del 1.0 end
	$textST conf -state disabled
    }

}

proc RamDebugger::UpdateGUIStack { res } {
    variable textST

    $textST conf -state normal
    $textST mark set insert 1.0
    if { [lindex $res 0] == 0 } {
	foreach line [lrange [split [lindex $res 1] \n] 1 end] {
	    $textST ins insert $line\n
	} 
    }
    $textST del insert end
    $textST conf -state disabled
}

proc RamDebugger::StopAtGUI { file line { condinfo "" } } {
    variable marker
    variable currentfile
    variable text

    if { ![info exists text] || ![winfo exists $text] } { return }

    foreach j [concat [$marker gettags arrow] [$marker gettags arrowbreak]] {
	if { [string match l* $j] } {
	    regexp {l([0-9]+)} $j {} arrowline
	    UpdateArrowAndBreak $arrowline "" 0
	}
    }
    if { $file == "" } {
	$text conf -editable 1
	return
    }

    if { $file != $currentfile } {
	OpenFileF $file 
    }
    UpdateArrowAndBreak $line "" 1

    if { $condinfo != "" } {
	WarnWin "Conditional breakpoint result: $condinfo" [winfo toplevel $text]
    }
}

proc RamDebugger::ContNextGUI { what } {
    variable text
    variable remoteserver
    variable remoteserverIsLocal
    variable IsInStop
    variable currentfile

    if { $remoteserver == "" || ($remoteserverIsLocal && !$IsInStop) } {
	if { $currentfile == "" } {
	    WarnWin "Cannot start debugging. There is no currentfile" $text
	    return
	}
	set ret [tk_messageBox -default ok -icon question -message \
	    "Do you want to start to debug locally '$currentfile?'" -parent $text \
	    -title "start debugging" -type okcancel]
	if { $ret == "cancel" } { return }
	rdebug -currentfile
    }

    switch $what {
	rcont { rcont }
	rnext { rnext }
	rstep { rstep }
	rcontto {
	    set idx [$text index insert] 
	    
	    if { $idx == "" } {
		WarnWin "Before using 'Continue to', pick in the text"
		return
	    }
	    $text see $idx
	    set line [scan $idx "%d"]
	    if { [catch [list rcont $line] errorstring] } {
		WarnWin $errorstring
	    }
	}
    }
}

proc RamDebugger::TextMotion { X Y x y } {
    variable text
    variable currentfile
    variable IsInStop
    variable TextMotionAfterId

    after cancel $TextMotionAfterId
    if { [winfo exists $text.help] } { destroy $text.help }
    if { $currentfile == "" || !$IsInStop } { return }

    set TextMotionAfterId [after 500 RamDebugger::DisplayVar $X $Y $x $y]
}

proc RamDebugger::DisplayVar { X Y x y } {
    variable text

    if { $X != [winfo pointerx $text] || $Y != [winfo pointery $text] } {
	return
    }
    set idx [$text index @$x,$y]
    set range [$text tag ranges sel]
    if { $range != "" && [$text compare [lindex $range 0] <= $idx] && \
	     [$text compare [lindex $range 1] >= $idx] } {
	set var [eval $text get $range]
    } else {
	set var ""
	set idx0 $idx
	while { [string is wordchar [$text get $idx0]] } {
	    set var [$text get $idx0]$var
	    set idx0 [$text index $idx0-1c]
	}
	set idx1 [$text index $idx+1c]
	while { [string is wordchar [$text get $idx1]] } {
	    append var [$text get $idx1]
	    set idx1 [$text index $idx1+1c]
	}
	if { $var == "" } { return }
    }
    set comm "if { \[array exists $var] } { array get $var } else { set $var }"
    set res [reval -handler [list RamDebugger::DisplayVar2 $var $X $Y $x $y] $comm]
}

proc RamDebugger::DisplayVar2 { var X Y x y res } {
    variable text

    if { [lindex $res 0] == 0 } {
	set w $text.help
	if { [winfo exists $w] } { destroy $w }
	toplevel $w
	wm overrideredirect $w 1
	wm transient $w $text
	wm geom $w +$X+$Y
	pack [label $w.l -bg white]
	$w.l conf -bd 1 -relief solid
	set val [lindex $res 1]
	if { [string length $val] > 50 } {
	    set val [string range $val 0 46]...
	}
	$w.l conf -text "$var=$val"
    }
}


proc RamDebugger::DisplayVarWindowEval { what f { res "" } } {

    set w [winfo toplevel $f]

    if { $what == "do" } {
	if { [string trim $DialogWinTop::user($w,expression)] == "" } {
	    set DialogWinTop::user($w,type) ""
	    return
	}
	set var $DialogWinTop::user($w,expression)
	set comm {
	    if { [array exists {VAR}] } {
		list array [array get {VAR}]
	    } elseif { [info exists {VAR}] } {
		list variable [set {VAR}]
	    } else {
		list expression [expr {VAR}]
	    }
	}
	set comm [string map [list VAR [string trim $var]] $comm]
	reval -handler [list RamDebugger::DisplayVarWindowEval res $f] $comm
    } else {
	set var $DialogWinTop::user($w,expression)
	$DialogWinTop::user($w,textv) conf -state normal
	$DialogWinTop::user($w,textv) del 1.0 end

	switch [lindex $res 0] {
	    0 {
		$DialogWinTop::user($w,textv) conf -fg black
		set DialogWinTop::user($w,type) [lindex [lindex $res 1] 0]
		if { $DialogWinTop::user($w,type) == "array" } {
		    foreach "name val" [lindex [lindex $res 1] 1] {
			$DialogWinTop::user($w,textv) insert end "${var}($name) = $val\n"
		    }
		} elseif { $DialogWinTop::user($w,aslist) } {
		    set DialogWinTop::user($w,type) list
		    if { [catch { set list [lrange [lindex [lindex $res 1] 1] 0 end] }] } {
			$DialogWinTop::user($w,textv) insert end "Error: variable is not a list"
		    } else {
			set ipos 0
			foreach i $list {
			    $DialogWinTop::user($w,textv) insert end "$ipos = $i\n"
			    incr ipos
			}
		    }
		} else {
		    $DialogWinTop::user($w,textv) insert end [lindex [lindex $res 1] 1]
		}
	    }
	    1 {
		set DialogWinTop::user($w,type) error
		$DialogWinTop::user($w,textv) conf -fg red
		$DialogWinTop::user($w,textv) insert end [lindex $res 1]
	    }
	}
	$DialogWinTop::user($w,textv) conf -state disabled
    }
}

proc RamDebugger::DisplayVarWindowCancel { f } {
    destroy [winfo toplevel $f]
}

proc RamDebugger::DisplayVarWindow {} {
    variable text
    
    set range [$text tag ranges sel]
    if { $range != "" } {
	set var [eval $text get $range]
    } else {
	set idx [$text index insert]
	if { $idx != "" } {
	    set var ""
	    set idx0 $idx
	    while { [string is wordchar [$text get $idx0]] } {
		set var [$text get $idx0]$var
		set idx0 [$text index $idx0-1c]
	    }
	    set idx1 [$text index $idx+1c]
	    while { [string is wordchar [$text get $idx1]] } {
		append var [$text get $idx1]
		set idx1 [$text index $idx1+1c]
	    }
	} else { set var "" }
    }

    set commands [list "RamDebugger::DisplayVarWindowEval do" RamDebugger::DisplayVarWindowCancel]
    set f [DialogWinTop::Init $text "View expression or variable" separator $commands \
	       "" Eval]
    set w [winfo toplevel $f]

    label $f.l1 -text "Expression:" -grid "0 px3"
    entry $f.e1 -textvariable DialogWinTop::user($w,expression) -grid "1 px3"

    set DialogWinTop::user($w,expression) $var
    
    set sw [ScrolledWindow $f.lf -relief sunken -borderwidth 0 -grid "0 2"]
    set DialogWinTop::user($w,textv) [text $sw.text -background white -wrap word -width 40 -height 10 \
				       -exportselection 0 -font FixedFont -highlightthickness 0]
    $sw setwidget $DialogWinTop::user($w,textv)

    label $f.l2 -textvar DialogWinTop::user($w,type) -grid "0 w" -bg [CCColorActivo [$w  cget -bg]]
    checkbutton $f.cb1 -text "As list" -variable DialogWinTop::user($w,aslist) -grid "1 w" \
       -command "DialogWinTop::InvokeOK $f"
    set DialogWinTop::user($w,aslist) 0
    supergrid::go $f

    tkTabToWindow $f.e1
    bind [winfo toplevel $f] <Return> "DialogWinTop::InvokeOK $f"
    $DialogWinTop::user($w,textv) conf -state disabled
    bind $DialogWinTop::user($w,textv) <1> "focus $DialogWinTop::user($w,textv)"

    DialogWinTop::CreateWindow $f
    DialogWinTop::InvokeOK $f
}

proc RamDebugger::DisplayBreakpointsWindowSetCond {} {

    set curr [$DialogWin::user(list) curselection]
    if { [llength $curr] != 1 } { return }

    set DialogWin::user(cond) [lindex [$DialogWin::user(list) get $curr] 3]
}

proc RamDebugger::DisplayBreakpointsWindow {} {
    variable text
    variable breakpoints
    variable currentfile
    
    set f [DialogWin::Init $text "Breakpoints window" separator [list Delete "Delete all"] \
	Apply Close]
    set w [winfo toplevel $f]

    label $f.l1 -text "Condition:" -grid 0
    entry $f.e1 -textvariable DialogWin::user(cond) -width 80 -grid "1 px3 py3"

    set sw [ScrolledWindow $f.lf -relief sunken -borderwidth 0 -grid "0 2"]
    
    set DialogWin::user(list) [tablelist::tablelist $sw.lb -width 55\
		  -exportselection 0 \
		  -columns [list \
				4 Num	left \
				40 File	right \
				5 Line left \
				40 Condition left \
			       ] \
		  -labelcommand tablelist::sortByColumn \
		  -background white \
		  -selectbackground navy -selectforeground white \
		  -stretch 1 -selectmode extended \
		  -highlightthickness 0]

    $sw setwidget $DialogWin::user(list)

    supergrid::go $f

    focus $DialogWin::user(list)
    bind [$DialogWin::user(list) bodypath] <ButtonRelease-1> {
	focus $DialogWin::user(list)
	RamDebugger::DisplayBreakpointsWindowSetCond
    }
    bind [$DialogWin::user(list) bodypath] <Double-1> "DialogWin::InvokeOK"

    foreach i $breakpoints {
	$DialogWin::user(list) insert end $i
    }
    set action [DialogWin::CreateWindow]
    while 1 {
	switch $action {
	    0 {
		DialogWin::DestroyWindow
		return
	    }
	    1 {
		set curr [$DialogWin::user(list) curselection]
		if { [llength $curr] != 1 } {
		    WarnWin "Select just one breakpoint before applying condition" $w
		    return
		}
		set val [$DialogWin::user(list) get $curr]
		rcond [lindex $val 0] $DialogWin::user(cond)
		$DialogWin::user(list) delete $curr
		$DialogWin::user(list) insert $curr [lreplace $val 3 3 $DialogWin::user(cond)]
		set file [lindex $val 1]
		set line [lindex $val 2]
		if { $file == $currentfile } {
		    $text mark set insert $line.0
		    $text see $line.0
		}
	    }
	    2 {
		foreach i [$DialogWin::user(list) curselection] {
		    set ent [$DialogWin::user(list) get $i]
		    set num [lindex $ent 0]
		    set file [lindex $ent 1]
		    set line [lindex $ent 2]
		    if { $file == $currentfile } {
			UpdateArrowAndBreak $line 0 ""
		    }
		    rdel $num
		}
		$DialogWin::user(list) del 0 end
		foreach i $breakpoints {
		    $DialogWin::user(list) insert end $i
		}
	    }
	    3 {
		set ret [tk_messageBox -default ok -icon warning -message \
			     "Are you sure to delete all breakpoints?" -parent $f \
			     -title "delete all breakpoints" -type okcancel]
		if { $ret == "ok" } {
		    $DialogWin::user(list) del 0 end
		    foreach i $breakpoints {
			set num [lindex $i 0]
			set file [lindex $i 1]
			set line [lindex $i 2]
			if { $file == $currentfile } {
			    UpdateArrowAndBreak $line 0 ""
			}
			rdel $num
		    }
		}
	    }
	}
	set action [DialogWin::WaitForWindow]
    }
}

proc RamDebugger::DisplayTimesWindowStart { f } {
    rtime -start
}

proc RamDebugger::DisplayTimesWindowReport { f } {

    set f [DialogWin::Init $text "Timing report" separator "" -]
    set w [winfo toplevel $f]

    set sw [ScrolledWindow $f.lf -relief sunken -borderwidth 0 -grid "0 2"]
    text $sw.t
    $sw setwidget $sw.t

    bind $sw.t <1> "focus $sw.t"
    
    supergrid::go $f

    focus $sw.t

    $sw.t ins end [rtime -display]

    set action [DialogWin::CreateWindow]
    DialogWin::DestroyWindow
}

proc RamDebugger::DisplayTimesWindowCancel { f } {

    rtime -stop
    destroy [winfo toplevel $f]
}

proc RamDebugger::PickSelection { w } {
    variable text

    if { [catch {
	set DialogWinTop::user($w,lineini) [scan %d [$text index sel.first]]
	set DialogWinTop::user($w,lineend) [scan %d [$text index sel.last]]
    }]} {
	WarnWin "It is necessary to select some text in main window"
    }
}

proc RamDebugger::DrawSelection { w } {
    variable text

    if { [catch {
	$text tag add sel "$DialogWinTop::user($w,lineini) linestart" \
	   "$ DialogWinTop::user($w,lineend) lineend"
	$text see $DialogWinTop::user($w,lineini)
    }] } {
	WarnWin "Lines are not correct"
    }
}

proc RamDebugger::ModifyTimingBlock { w what } {
    variable TimeMeasureData

    set idx [$DialogWinTop::user($w,list) curselection]

    switch $what {
	create {
	    set err [catch [list rtime -add $DialogWinTop::user($w,name) \
		$DialogWinTop::user($w,lineini) $DialogWinTop::user($w,lineend)] errstring]
	    if { $err } {
		WarnWin [lindex [split $errstring \n] 0]
		return $err
	    } else {
		ModifyTimingBlock $w update
		return 0
	    }
	}
	edit {
	    set err [ModifyTimingBlock $w delete]
	    if { !$err } { ModifyTimingBlock $w create }
	}
	delete {
	    if { [llength $idx] != 1 } {
		WarnWin "Error: it is necessary to select one block in list"
		return 1
	    }
	    set selname [lindex [$DialogWinTop::user($w,list) get $idx] 0]
	    set err [catch [list rtime -delete $selname] errstring]
	    if { $err } {
		WarnWin [lindex [split $errstring \n] 0]
		return $err
	    } else {
		ModifyTimingBlock $w update
		return 0
	    }
	}
	updatecurrent {
	    if { [llength $idx] != 1 } {
		WarnWin "Error: it is necessary to select one block in list"
		return 1
	    }
	    set DialogWinTop::user($w,name) [lindex [$DialogWinTop::user($w,list) get $idx] 0]
	    set DialogWinTop::user($w,lineini) [lindex [$DialogWinTop::user($w,list) get $idx] 1]
	    set DialogWinTop::user($w,lineend) [lindex [$DialogWinTop::user($w,list) get $idx] 2]
	}
	update {
	    $DialogWinTop::user($w,list) del 0 end
	    foreach i $TimeMeasureData {
		$DialogWinTop::user($w,list) ins end [lrange $i 0 2]
	    }
	}
    }
}

proc RamDebugger::DisplayTimesWindow {} {
    variable text

    set commands [list RamDebugger::DisplayTimesWindowStart RamDebugger::DisplayTimesWindowReport \
	RamDebugger::DisplayTimesWindowCancel]
    set f [DialogWinTop::Init $text "Timing control" separator $commands \
	       [list Report] Start]
    set w [winfo toplevel $f]

    TitleFrame $f.f1 -text [_ "current block"] -grid "0 ew"
    set f1 [$f.f1 getframe]

    label $f1.l1 -text "Name" -grid 0
    entry $f1.e1 -textvariable DialogWinTop::user($w,name) -grid "1 ew"
    label $f1.l2 -text "Initial line:" -grid "0 e"
    entry $f1.e2 -textvariable DialogWinTop::user($w,lineini) -grid "1 w py2" -width 8
    label $f1.l3 -text "End line:" -grid "0 e"
    entry $f1.e3 -textvariable DialogWinTop::user($w,lineend) -grid "1 w py2" -width 8
    
    set bbox [ButtonBox $f1.bbox1 -spacing 0 -padx 1 -pady 1 -homogeneous 1 -grid "0 2 w"]
    $bbox add -text "Pick selection" \
         -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1 -padx 1 -pady 1 \
         -helptext [_ "Gets the selection from the text window"] \
         -command "RamDebugger::PickSelection $w"
    $bbox add -text "Draw selection" \
         -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1 -padx 1 -pady 1 \
         -helptext [_ "Selects current block in text"] \
         -command "RamDebugger::DrawSelection $w"

    TitleFrame $f.f2 -text [_ "blocks"] -grid 0
    set f2 [$f.f2 getframe]

    set sw [ScrolledWindow $f2.lf -relief sunken -borderwidth 0 -grid "0 ewns"]
    
    set DialogWinTop::user($w,list) [tablelist::tablelist $sw.lb -width 55 \
	-exportselection 0 \
	-columns [list \
	    10 Name	left \
	    10 "Initial line"	right \
	    10 "End line"  right \
	   ] \
	-labelcommand tablelist::sortByColumn \
	-background white \
	-selectbackground navy -selectforeground white \
	-stretch 0 -selectmode browse \
	-highlightthickness 0]

    $sw setwidget $DialogWinTop::user($w,list)

    bind $DialogWinTop::user($w,list) <1> "RamDebugger::ModifyTimingBlock $w updatecurrent"

    set bbox [ButtonBox $f2.bbox1 -spacing 0 -padx 1 -pady 1 -homogeneous 1 -grid "0 w"]
    $bbox add -image acttick16 \
         -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1 -padx 1 -pady 1 \
         -helptext [_ "Create new block"] \
         -command "proc RamDebugger::ModifyTimingBlock $w create"
    $bbox add -image edit16 \
         -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1 -padx 1 -pady 1 \
         -helptext [_ "Update selected block"] \
         -command "proc RamDebugger::ModifyTimingBlock $w edit"
    $bbox add -image actcross16 \
         -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1 -padx 1 -pady 1 \
         -helptext [_ "Delete selected block"] \
         -command "proc RamDebugger::ModifyTimingBlock $w delete"

    supergrid::go $f1
    supergrid::go $f2
    supergrid::go $f

    tkTabToWindow $f1.e1
    bind [winfo toplevel $f] <Return> "DialogWinTop::InvokeOK $f"
    ModifyTimingBlock $w update

    DialogWinTop::CreateWindow $f
    DialogWinTop::InvokeOK $f
}

proc RamDebugger::DisplayWindowsHierarchyInfo { canvas w x y } {
    variable TextMotionAfterId
    
    after cancel $TextMotionAfterId
    set TextMotionAfterId ""
    if { [winfo exists $canvas.help] } { destroy $canvas.help }

    if { $w != "" } {
	set TextMotionAfterId [after 100 RamDebugger::DisplayWindowsHierarchyInfoDo $canvas $w $x $y]
    }
}

proc RamDebugger::DisplayWindowsHierarchyInfoDo { canvas w x y } {
    variable TextMotionAfterId

    set TextMotionAfterId ""
    set comm {
	set retval "WIDGET\n\n"
	if { [winfo class WIDGET] == "Toplevel" } {
	    append retval "TITLE: [wm title WIDGET]\n"
	}
	if { [winfo class WIDGET] == "Label" || [winfo class WIDGET] == "Button" || \
	   [winfo class WIDGET] == "Radiobutton" || [winfo class WIDGET] == "Checkbutton" } {
	    if { [WIDGET cget -text] != "" } {
		append retval "LABEL: [WIDGET cget -text]\n"
	    }
	}
	foreach "cols rows" [grid size WIDGET] break
	if { $cols > 0 } {
	    append retval "GRID MASTER\n"
	    for { set i 0 } { $i < $rows } { incr i } {
		for { set j 0 } { $j < $cols } { incr j } {
		    set slave [grid slaves WIDGET -row $i -col $j]
		    if { $slave != "" } {
			append retval "    $i,$j $slave [grid info $slave]\n"
		    }
		}
	    }
	    append retval "  ROWS\n"
	    for { set i 0 } { $i < $rows } { incr i } {
		append retval "    $i [grid rowconfigure WIDGET $i]\n" 
	    }
	    append retval "  COLUMNS\n"
	    for { set j 0 } { $j < $cols } { incr j } {
		append retval "    $i [grid columnconfigure WIDGET $j]\n" 
	    }
	}
	set info [grid info WIDGET]
	if { $info != "" } {
	    append retval "GRID SLAVE\n$info\n"
	}
	if { [pack slaves WIDGET] != "" } {
	    append retval "PACK MASTER\n"
	    foreach i [pack slaves WIDGET] {
		append retval "    $i [pack info $i]\n"
	    }
	}
	if { ![catch [list pack info WIDGET] info] } {
	    append retval "PACK SLAVE\n$info\n"
	}
	append retval "OPTIONS\n"
	set retval_in ""
	foreach i [WIDGET configure] {
	    append retval_in "[lindex $i 0] [WIDGET cget [lindex $i 0]] "
	    while { [string length $retval_in] > 80 } {
		append retval [string range $retval_in 0 79]\n
		set retval_in [string range $retval_in 80 end]
	    }
	    if { [string length $retval_in] > 40 } {
		append retval $retval_in\n
		set retval_in ""
	    }
	}
	append retval $retval_in
	EVAL
    }
    if { $DialogWin::user(type) == "ramdebugger" } {
	set retcomm "RamDebugger::DisplayWindowsHierarchyInfoDo2 $canvas $w $x $y \[set retval]"
    } else {
	set retcomm "RDC::SendDev \[list RamDebugger::DisplayWindowsHierarchyInfoDo2 $canvas \
		$w $x $y \[set retval]]"
    }
    set comm [string map [list EVAL $retcomm] $comm]
    set comm [string map [list WIDGET $w] $comm]
    if { $DialogWin::user(type) == "ramdebugger" } {
	eval $comm
    } else {
	EvalRemote $comm
    }
}

proc RamDebugger::DisplayWindowsHierarchyInfoDo2 { canvas w x y res } {

    set w $canvas.help
    if { [winfo exists $w] } { destroy $w }
    toplevel $w
    wm overrideredirect $w 1
    wm transient $w $canvas

    pack [label $w.l -bg white -justify left -anchor w]
    $w.l conf -bd 1 -relief solid
    $w.l conf -text $res
    bind $w <Motion> "destroy $w"

    wm withdraw $w
    update idletasks

    incr x 5
    incr y 5

    if { $x+[winfo reqwidth $w]+5 > [winfo screenwidth $w] } {
	set x [expr [winfo screenwidth $w]-[winfo reqwidth $w]-5]
    }
    if { $x < 5 } { set x 5 }

    if { $y+[winfo reqheight $w]+5 > [winfo screenheight $w] } {
	set y [expr [winfo screenheight $w]-[winfo reqheight $w]-5]
    }
    if { $y < 0 } { set y 5 }

    wm geom $w +$x+$y
    wm deiconify $w

    focus $w.l
    bind $w.l <Control-c> "clipboard clear; clipboard append [list $res]"

}

proc RamDebugger::DisplayWindowsHierarchyDoDraw { canvas list x y linespace } {

    set maxwidth 0
    foreach "w info rest" $list {
	foreach i $info {
	    set width [font measure NormalFont $i]
	    if { $width > $maxwidth } { set maxwidth $width }
	}
    }
    set maxwidth [expr $maxwidth+10]

    set maxx [expr $x+$maxwidth]
    set maxy $y
    set ty $y
    set num 0
    foreach "w info rest" $list {
	if { $rest != "-" } { 
	    foreach "newx newy" [DisplayWindowsHierarchyDoDraw $canvas $rest [expr $x+$maxwidth] \
		$maxy $linespace] break
	    if { $newx > $maxx } { set maxx $newx}
	    set ty [expr ($maxy+$newy)/2-$linespace/2]
	    set maxy $newy

	    set width 0
	    foreach i $info {
		set widthi [font measure NormalFont $i]
		if { $widthi > $width } { set width $widthi }
	    }

	    $canvas create line [expr $x+$width+2] [expr $ty+$linespace/2] \
	        [expr $x+$maxwidth-5] [expr $ty+$linespace/2] -width 2 -fill red -tags "items $w"
	    incr maxy 12
	}
	set inum 0
	foreach i $info {
	    if { $i != "" } {
		switch $inum {
		    0 { set color black }
		    1 { set color blue }
		    2 { set color green }
		}
		$canvas create text $x $ty -text $i -tags "items $w" -font NormalFont -anchor nw \
		    -fill $color
		set ty [expr $ty+$linespace]
	    }
	    incr inum
	}
	$canvas bind $w <Enter> "RamDebugger::DisplayWindowsHierarchyInfo $canvas $w %X %Y"
	$canvas bind $w <Leave> "RamDebugger::DisplayWindowsHierarchyInfo $canvas {} %X %Y"

	if { $ty > $maxy } {
	    set maxy $ty
	} else { set ty $maxy }

	incr num
    }
    if { $num > 1 } {
	$canvas create line [expr $x-5] $y [expr $x-1] [expr $y-4] -width 2 -fill red -tags items
	$canvas create line [expr $x-5] $y [expr $x-5] $maxy -width 2 -fill red -tags items
	$canvas create line [expr $x-5] $maxy [expr $x-1] [expr $maxy+4] -width 2 -fill red -tags items
    }
    return [list $maxx $maxy]
}

proc RamDebugger::DisplayWindowsHierarchyDo { what { res "" } } {
    variable remoteserver

    if { $what == "do" } {
	set comm {
	    namespace eval ::RDC {}
	    proc ::RDC::WindowsHierarchy { w } {
		set info [list "[winfo name $w] [winfo class $w]"]
		if { [winfo class $w] == "Label" || [winfo class $w] == "Button" || \
		    [winfo class $w] == "Radiobutton" || [winfo class $w] == "Checkbutton" } {
		    if { [$w cget -text] != "" } {
			lappend info [$w cget -text]
		    }
		} elseif { [winfo class $w] == "Toplevel" && [wm title $w] != "" } {
		    lappend info "" [wm title $w]
		}
		set retval [list $w $info]
		set retval_in ""
		foreach i [winfo children $w] {
		    eval lappend retval_in [RDC::WindowsHierarchy $i]
		}
		if { $retval_in == "" } {
		    lappend retval -
		} else { lappend retval $retval_in }
		return $retval
	    }
	    EVAL
	}
	if { $DialogWin::user(type) == "ramdebugger" } {
	    set retcomm "RamDebugger::DisplayWindowsHierarchyDo res \[::RDC::WindowsHierarchy .]"
	} else {
	    set retcomm "RDC::SendDev \[list RamDebugger::DisplayWindowsHierarchyDo res \
	    	    \[::RDC::WindowsHierarchy .]]"
	}
	set comm [string map [list EVAL $retcomm] $comm]

	if { [info exists RamDebugger::DisplayWindowsHierarchyFindLastId] } {
	    unset RamDebugger::DisplayWindowsHierarchyFindLastId
	}

	if { $DialogWin::user(type) == "ramdebugger" } {
	    eval $comm
	} else {
	    if { $remoteserver == "" } {
		after idle [list WarnWin "Error: there is no debugged application" \
		    $DialogWin::user(canvas)]
		$DialogWin::user(canvas) delete items
		return
	    }
	    EvalRemote $comm
	}
    } else {
	$DialogWin::user(canvas) delete items
	set linespace [expr [font metrics NormalFont -linespace]+1]

	foreach "newx newy" [DisplayWindowsHierarchyDoDraw $DialogWin::user(canvas) $res \
	    5 5 $linespace] break

	$DialogWin::user(canvas) conf -scrollregion [list 0 0 $newx $newy]
    }
}

proc RamDebugger::DisplayWindowsHierarchyFind {} {
    variable DisplayWindowsHierarchyFindLastId

    set canvas $DialogWin::user(canvas)

    if { ![info exists DisplayWindowsHierarchyFindLastId] } {
	set DisplayWindowsHierarchyFindLastId -1
    }
    set found 0
    foreach i [$canvas find all] {
	if { $DisplayWindowsHierarchyFindLastId > -1 && $i <= $DisplayWindowsHierarchyFindLastId } {
	    continue
	}
	if { [$canvas type $i] == "text" } {
	    set text [$canvas itemcget $i -text]
	    if { [string match -nocase *$DialogWin::user(find)* $text] } {
		set found 1
		set DisplayWindowsHierarchyFindLastId $i
		break
	    }
	}
    }
    if { $found } {
	$canvas select from $i 0
	$canvas select to $i end
	foreach "x y" [$canvas coords $i] break
	foreach "- - width height" [$canvas cget -scrollregion] break
	
	foreach "f1 f2" [$canvas xview] break
	set xs [expr $x/double($width)-($f2-$f1)/2.0]
	if { $xs < 0 } { set xs 0 }
	$canvas xview moveto $xs

	foreach "f1 f2" [$canvas yview] break
	set ys [expr $y/double($height)-($f2-$f1)/2.0]
	if { $ys < 0 } { set ys 0 }
	$canvas yview moveto $ys
    } else {
	unset RamDebugger::DisplayWindowsHierarchyFindLastId
	bell
    }
}

proc RamDebugger::DisplayWindowsHierarchy {} {
    variable text

    set f [DialogWin::Init $text "Windows hierarchy" separator "" -]
    set w [winfo toplevel $f]

    radiobutton $f.r1 -text "In debugged app" -variable DialogWin::user(type) -value debug \
       -command "RamDebugger::DisplayWindowsHierarchyDo do" -grid "0 w"
    radiobutton $f.r2 -text "In RamDebugger" -variable DialogWin::user(type) -value ramdebugger \
       -command "RamDebugger::DisplayWindowsHierarchyDo do" -grid "1 w"

    frame $f.f -grid "0 2"
    label $f.f.l -text "Find:" -grid 0
    entry $f.f.e -textvar DialogWin::user(find) -grid 1
    button $f.f.b1 -text Go -width 5 -grid "2 px3 py3" -command \
       "RamDebugger::DisplayWindowsHierarchyFind"
    bind $f.f.e <Return> "$f.f.b1 invoke"

    set DialogWin::user(type) debug

    set sw [ScrolledWindow $f.lf -relief sunken -borderwidth 0 -grid "0 2"]
    set DialogWin::user(canvas) [canvas $sw.t -width 600 -height 400 -bg white -bd 0 ]
    $sw setwidget $DialogWin::user(canvas)

    supergrid::go $f

    DisplayWindowsHierarchyDo do
    set action [DialogWin::CreateWindow]
    DialogWin::DestroyWindow
}

proc RamDebugger::GotoLine {} {
    variable text

    set f [DialogWin::Init $text "Goto line" separator ""]
    set w [winfo toplevel $f]

    label $f.l -text "Go to line:" -grid "0 px3 py5"
    SpinBox $f.sb -range "1 1000 1" -textvariable DialogWin::user(line) \
	    -width 8 -grid "1 px3"

    tkTabToWindow $f.sb

    bind $w <Return> "DialogWin::InvokeOK"

    supergrid::go $f

    set action [DialogWin::CreateWindow]
    while 1 {
	switch $action {
	    0 {
		DialogWin::DestroyWindow
		return
	    }
	    1 {
		if { ![string is integer -strict $DialogWin::user(line)] || \
			$DialogWin::user(line) < 1 } {
		    WarnWin "Line number must be a positive number" $w
		} else {
		    $text mark set insert $DialogWin::user(line).0
		    $text see $DialogWin::user(line).0
		    DialogWin::DestroyWindow
		    return
		}
	    }
	}
	set action [DialogWin::WaitForWindow]
    }
}

proc RamDebugger::CheckEvalEntries { what { name "" } { res "" } } {
    variable EvalEntries
    variable IsInStop

    if { !$IsInStop } { return }
    if { $name == "" } {
	if { $what == "do" } {
	    set vars ""
	    set i 0
	    while 1 {
		if { ![info exists EvalEntries($i,left)] } { break }
		if { [string trim $EvalEntries($i,left)] != "" } {
		    lappend vars $EvalEntries($i,left)
		}
		incr i
	    }
	    set comm {
		set ::RDC::retval ""
		foreach ::RDC::i [list VARS] {
		    if { [array exists $::RDC::i] } {
			lappend ::RDC::retval array [array get $::RDC::i]
		    } elseif { [info exists $::RDC::i] } {
			lappend ::RDC::retval variable [set $::RDC::i]
		    } else {
			set ::RDC::err [catch {expr [set ::RDC::i]} ::RDC::val]
			if { !$::RDC::err } {
			    lappend ::RDC::retval expr $::RDC::val
			} else {
			    lappend ::RDC::retval error "variable or expr $::RDC::i does not exist"
			}
		    }
		}
		set ::RDC::retval
	    }
	    set comm [string map [list VARS $vars] $comm]
	    reval -handler [list RamDebugger::CheckEvalEntries res $name] $comm
	} else {
	    set i 0
	    foreach "type val" [lindex $res 1] {
		if { [string length $val] > 50 } {
		    set val [string range $val 0 46]...
		}
		while { [info exists EvalEntries($i,left)] && \
			    [string trim $EvalEntries($i,left)] == "" } {
		    incr i
		}
		set RamDebugger::EvalEntries($i,right) $val
		if { $type == "error" } {
		    $RamDebugger::EvalEntries($i,rightentry) conf -fg red
		} else {
		    $RamDebugger::EvalEntries($i,rightentry) conf -fg black
		}
		incr i
	    }
	}
    } elseif { [string match *left $name] } {
	if { $what == "do" } {
	    regexp {[0-9]+} $name i
	    set var $EvalEntries($name)
	    if { [string trim $var] == "" } {
		$RamDebugger::EvalEntries($i,rightentry) conf -fg black
		set RamDebugger::EvalEntries($i,right) ""
		return
	    }
	    set comm {
		if { [array exists {VAR}] } {
		    set ::RDC::retval [list array [array get {VAR}]]
		} elseif { [info exists {VAR}] } {
		    set ::RDC::retval [list variable [set {VAR}]]
		} else {
		    set ::RDC::err [catch {expr {VAR}} ::RDC::val]
		    if { !$::RDC::err } {
			set ::RDC::retval [list expr $::RDC::val]
		    } else {
			set ::RDC::retval [list error "variable or expr 'VAR' does not exist"]
		    }
		}
		set ::RDC::retval
	    }
	    set comm [string map [list VAR [string trim $var]] $comm]
	    reval -handler [list RamDebugger::CheckEvalEntries res $name] $comm
	} else {
	    regexp {[0-9]+} $name i
	    foreach "type val" [lindex $res 1] break
	    if { [string length $val] > 50 } {
		set val [string range $val 0 46]...
	    }
	    set RamDebugger::EvalEntries($i,right) $val
	    if { $type == "error" } {
		$RamDebugger::EvalEntries($i,rightentry) conf -fg red
	    } else {
		$RamDebugger::EvalEntries($i,rightentry) conf -fg black
	    }
	}
    } else {
	if { $what == "do" } {
	    regexp {[0-9]+} $name i

	    set var [string trim $EvalEntries($i,left)]
	    if { $var == "" } { return }
	    set value [string trim $EvalEntries($name)]
	    set comm [list set $var $value]
	    reval -handler [list RamDebugger::CheckEvalEntries res $name] $comm
	} else {
	    regexp {[0-9]+} $name i
	    set RamDebugger::EvalEntries($i,right) [lindex $res 1]
	    switch [lindex $res 0] {
		0 { $RamDebugger::EvalEntries($i,rightentry) conf -fg black }
		1 { $RamDebugger::EvalEntries($i,rightentry) conf -fg red }
	    }
	}
    }
}

proc RamDebugger::CheckEvalEntriesL { what { name "" } { res "" } } {
    variable EvalEntries
    variable IsInStop

    if { !$IsInStop } { return }
    if { $name == "" } {
	if { $what == "do" } {
	    set comm {
		set ::RDC::retval ""
		foreach ::RDC::i [info locals] {
		    if { [array exists $::RDC::i] } {
			set ::RDC::val [array get $::RDC::i]
			if { [string length $::RDC::val] > 50 } {
			    set ::RDC::val [string range $::RDC::val 0 46]...
			}
			lappend ::RDC::retval $::RDC::i array $::RDC::val
		    } elseif { [info exists $::RDC::i] } {
			set ::RDC::val [set $::RDC::i]
			if { [string length $::RDC::val] > 50 } {
			    set ::RDC::val [string range $::RDC::val 0 46]...
			}
			lappend ::RDC::retval $::RDC::i variable $::RDC::val
		    } else {
			lappend ::RDC::retval $::RDC::i error "error"
		    }
		}
		set ::RDC::retval
	    }
	    reval -handler [list RamDebugger::CheckEvalEntriesL res $name] $comm
	} else {
	    set i 0
	    foreach "name type val" [lindex $res 1] {
		if { $type == "error" } { continue }
		if { ![info exists EvalEntries($i,leftL)] } { break }
		set EvalEntries($i,leftL) $name
		set EvalEntries($i,rightL) $val
		incr i
	    }
	    while 1 {
		if { ![info exists EvalEntries($i,leftL)] } { break }
		set EvalEntries($i,leftL) ""
		set EvalEntries($i,rightL) ""
		incr i
	    }
	}
    } else {
	if { $what == "do" } {
	    regexp {[0-9]+} $name i
	    set var [string trim $EvalEntries($i,leftL)]
	    if { $var == "" } { return }
	    set value [string trim $EvalEntries($name)]
	    set comm [list set $var $value]
	    reval -handler [list RamDebugger::CheckEvalEntriesL res $name] $comm
	} else {
	    regexp {[0-9]+} $name i
	    set RamDebugger::EvalEntries($i,rightR) [lindex $res 1]
	    switch [lindex $res 0] {
		0 { $RamDebugger::EvalEntries($i,rightentryL) conf -fg black }
		1 { $RamDebugger::EvalEntries($i,rightentryL) conf -fg red }
	    }
	}
    }
}

proc RamDebugger::WaitState { what { w . } } {
    variable text
    variable listbox

    if { $what == 1 } {
	$text configure -cursor watch
	$listbox configure -cursor watch
	$w configure -cursor watch
	if { [winfo toplevel $w] != $w } {
	    [winfo toplevel $w] configure -cursor watch
	}
    } else {
	$text configure -cursor xterm
	$listbox configure -cursor ""
	$w configure -cursor ""
	if { [winfo toplevel $w] != $w } {
	    [winfo toplevel $w] configure -cursor ""
	}
    }
    update
}

proc RamDebugger::ProgressVar { value } {
    variable progressvar
    set progressvar $value
    if { $value == 100 } {
	after 1000 set RamDebugger::progressvar -1
    }
    update
}

proc RamDebugger::SetMessage { mess } {
    variable status
    after cancel set RamDebugger::status ""
    set status $mess
    update
    after 5000 { set RamDebugger::status "" }
}


proc RamDebugger::FillListBox {} {
    variable listbox
    variable opts
    variable instrumentedfiles
    variable instrumentedfilesSent
    variable remoteserverIsLocal
    variable images

    if { $listbox == "" || ![winfo exists $listbox] } { return }

    $listbox delete [$listbox items]
    
    set parent [file dirname $opts(defaultdir)]
    $listbox insert end .. -image [Bitmap::get folder] -text ".." -data [list folder $parent]
    set idxfolder 1
    set files ""
    foreach i [glob -nocomplain -dir $opts(defaultdir) *] {
	lappend files [file tail $i]
    }

    foreach i [lsort -dictionary $files] {
	regsub -all {\s} $i _ item
	set fullpath [file join $opts(defaultdir) $i]
	if { [file isdir $fullpath] } {
	    if { [string tolower $i] == "cvs" } { continue }
	    $listbox insert $idxfolder $item -image [Bitmap::get folder] -text $i \
		-data [list folder $fullpath]
	    incr idxfolder
	} elseif { [file ext $i] == ".tcl" } {
	    if { [info exists instrumentedfilesSent($fullpath)] } {
		switch $instrumentedfilesSent($fullpath) {
		    debug { set img $images(file_blue) }
		    time { set img $images(file_magenta) }
		}
	    } elseif { [GiveInstFile $fullpath 1 $remoteserverIsLocal] != "" } {
		set img $images(file_yellow)
	    } else { set img [Bitmap::get file] }

	    $listbox insert end $item -image $img -text $i -data \
		[list file [file join $opts(defaultdir) $i]]
	}
    }
}

proc RamDebugger::ListBoxDouble1 { item } {
    variable listbox
    variable opts

    set data [$listbox itemcget $item -data]
    if { [lindex $data 0] == "folder" } {
	set opts(defaultdir) [lindex $data 1]
	FillListBox
    } else {
	OpenFileF [lindex $data 1]
    }
}

proc RamDebugger::SearchWindow {} {
    variable text
    
    set f [DialogWin::Init $text "Search" separator]
    set w [winfo toplevel $f]

    label $f.l1 -text "Text:" -grid 0
    entry $f.e1 -textvariable ::RamDebugger::searchstring -grid "1 2 px3 py3"

    set f2 [frame $f.f2 -bd 1 -relief ridge -grid "0 2 w px3"]
    radiobutton $f2.r1 -text Exact -variable ::RamDebugger::searchmode \
	-value -exact -grid "0 w"
    radiobutton $f2.r2 -text Regexp -variable ::RamDebugger::searchmode \
	-value -regexp -grid "0 w"

    set f25 [frame $f.f25 -bd 1 -relief ridge -grid "2 w px3"]
    radiobutton $f25.r1 -text Forward -variable ::RamDebugger::SearchType \
	-value -forwards -grid "0 w"
    radiobutton $f25.r2 -text Backward -variable ::RamDebugger::SearchType \
	-value -backwards -grid "0 w"

    set f3 [frame $f.f3 -grid "0 3 w"]
    checkbutton $f3.cb1 -text "Consider case" -variable ::RamDebugger::searchcase \
	-grid 0
    checkbutton $f3.cb2 -text "From beginning" -variable ::RamDebugger::searchFromBegin \
	-grid 1
   
    supergrid::go $f

    set ::RamDebugger::searchstring ""
    catch { set ::RamDebugger::searchstring [$text get sel.first sel.last] }

    set ::RamDebugger::searchmode -exact
    set ::RamDebugger::searchcase 0
    set ::RamDebugger::searchFromBegin 0
    set ::RamDebugger::SearchType -forwards

    tkTabToWindow $f.e1
    bind $w <Return> "DialogWin::InvokeOK"
    
    set action [DialogWin::CreateWindow]
    switch $action {
	0 {
	    DialogWin::DestroyWindow
	    return
	}
	1 {
	    if { $::RamDebugger::searchstring == "" } {
		DialogWin::DestroyWindow
		return
	    }
	    if { $::RamDebugger::searchFromBegin } {
		if { $::RamDebugger::SearchType == "-forwards" } {
		    set idx 1.0
		} else { set idx [$text index end] }
	    } else {
		set idx [$text index insert]
	    }
	    set ::RamDebugger::SearchIni $idx
	    set ::RamDebugger::SearchPos $idx
	    set ::RamDebugger::Lastsearchstring ""
	    Search [winfo toplevel $text] any
	    DialogWin::DestroyWindow
	}
    }
}

proc RamDebugger::Search { w { what {} } } {
    variable text

    if { ![winfo exists $w.search] && $what != "any" } {
	entry $w.search -width 25 -textvariable RamDebugger::searchstring
	place $w.search -in $w -x 0 -rely 1 -y -1 -anchor sw

	focus $text
	bindtags $text [linsert [bindtags $text] 0 $w.search]
	bind $w.search <FocusOut> "destroy $w.search"
	bind $w.search <Escape> "destroy $w.search"
	bind $w.search <KeyPress> [list if { [string is wordchar %A] || [string is punct %A] \
					     || [string is space %A] } \
	    "tkEntryInsert $w.search %A" else "destroy $w.search"]
	bind $w.search <Delete> "$w.search delete insert"
	bind $w.search <BackSpace> "tkEntryBackspace $w.search"
	bind $w.search <1> "destroy $w.search"
	bind $w.search <3> "destroy $w.search"
	bind $w.search <Return> "destroy $w.search"
	bind $w.search <Control-s> "RamDebugger::Search $w iforward ; break"
	bind $w.search <Control-r> "RamDebugger::Search $w ibackward ; break"
	bind $w.search <Control-g> "RamDebugger::Search $w stop ; break"

	trace var RamDebugger::searchstring w "RamDebugger::Search $w ;#"
	bind $w.search <Destroy> [list trace vdelete RamDebugger::searchstring w \
				      "RamDebugger::Search $w ;#"]
	bind $w.search <Destroy> "+ [list bindtags $text [lreplace [bindtags $text] 0 0]]"

	foreach i [bind Text] {
	    if { [bind $w.search $i] == "" } {
		if { [string match *nothing* [bind Text $i]] } {
		    bind $w.search $i [bind Text $i]
		} else {
		    bind $w.search $i "destroy $w.search" }
	    }
	}
	set idx [$text index insert]
	if { $idx == "" } { set idx 1.0 }
	set ::RamDebugger::SearchIni $idx
	set ::RamDebugger::SearchPos $idx
	set ::RamDebugger::searchcase -1
	set ::RamDebugger::searchmode -exact
	set ::RamDebugger::Lastsearchstring ""
	set ::RamDebugger::searchstring ""
    }

    switch $what {
	iforward {
	    set ::RamDebugger::SearchType -forwards
	}
	ibackward {
	    set ::RamDebugger::SearchType -backwards
	}
	any {
	    if { ![info exists ::RamDebugger::Lastsearchstring] } {
		WarnWin "Before using 'Continue search', use 'Search'" $w
		return
	    }
	}
	stop {
	    destroy $w.search
	    return
	}
    }
    if { $RamDebugger::searchstring != "" || $RamDebugger::Lastsearchstring != "" } {
	if { $RamDebugger::SearchType == "-forwards" } {
	    set stopindex end
	} else {
	    set stopindex 1.0
	}
	if { $RamDebugger::searchstring == $RamDebugger::Lastsearchstring } {
	    set len [string length $RamDebugger::searchstring]
	    if { $RamDebugger::SearchType == "-forwards" } {
		set idx [$text index $RamDebugger::SearchPos+${len}c]
	    } else {
		set idx [$text index $RamDebugger::SearchPos-${len}c]
	    }
	} elseif { [string length $RamDebugger::searchstring] < \
		       [string length $RamDebugger::Lastsearchstring] } {
	    set idx $RamDebugger::SearchIni
	} else { set idx $RamDebugger::SearchPos }

	set options $RamDebugger::SearchType
	lappend options $::RamDebugger::searchmode

	if { $::RamDebugger::searchcase == 1 } {
	    # nothing
	} elseif { $::RamDebugger::searchcase == 0 } {
	    lappend options -nocase
	} elseif { $RamDebugger::searchstring == [string tolower $RamDebugger::searchstring] } {
	    lappend options -nocase
	}
	lappend options --

	set idx [eval $text search $options [list $RamDebugger::searchstring] \
		     $idx $stopindex]
	if { $idx == "" } {
	    bell
	} else {
	    set RamDebugger::SearchPos $idx
	    set len [string length $RamDebugger::searchstring]
	    if { $RamDebugger::SearchType == "-forwards" } {
		set idx2 [$text index $RamDebugger::SearchPos+${len}c]
	    } else {
		set idx2 $RamDebugger::SearchPos
		set RamDebugger::SearchPos [$text index $RamDebugger::SearchPos+${len}c]
	    }
 	    $text conf -editable 1
	    $text tag remove sel 1.0 end
	    if { $RamDebugger::SearchType == "-forwards" } {
		$text tag add sel $RamDebugger::SearchPos $idx2
	    } else { $text tag add sel $idx2 $RamDebugger::SearchPos }
 	    $text conf -editable 0
	    $text mark set insert $idx2
	    $text see $RamDebugger::SearchPos
	}
	set RamDebugger::Lastsearchstring $RamDebugger::searchstring
    }
}

proc RamDebugger::OpenVisualRegexp {} {

    exec [info nameofexecutable] [file join [info script] addons visual_regexp-2.2 visual_regexp.tcl] &
}

proc RamDebugger::OpenConsole {} {

    set tkcon [lindex [glob -nocomplain -dir [file dirname [info nameofexecutable]] tkcon*] 0]

    if { $tkcon == "" } {
	WarnWin "Could not find tkcon"
	return
    }

    set tkconprefs {
	if { $::tcl_platform(platform) == "windows" } {
	    tkcon font "MS Sans Serif" 8
	} else {
	    tkcon font "new century schoolbook" 12
	}
	# Keep 50 commands in history
	set ::tkcon::OPT(history)  50
	set ::tkcon::OPT(buffer) 5000
	
	# Use a pink prompt
	set ::tkcon::COLOR(prompt) red
	# set tkcon(autoload) "Tk"
	set ::tkcon::OPT(cols)  80
	set ::tkcon::OPT(rows) 24
	set ::tkcon::OPT(gc-delay) 0
	#set tkcon(cols) 60
	#set tkcon(rows) 18
	catch [list namespace import RamDebugger::*]
	puts "Welcome to Ramdebugger inside tkcon. Use 'rhelp' for help"
    }
    set argv [list -rcfile "" -exec "" -root .tkcon -eval $tkconprefs]
    uplevel \#0 source $tkcon
    uplevel \#0 ::tkcon::Init $argv
    proc ::tkcon::FinalExit { args } { destroy .tkcon }
}

proc RamDebugger::CutCopyPasteText { what } {
    variable text

    switch $what {
	copy {
	    tk_textCopy $text
	}
    }
}

proc RamDebugger::SearchInListbox { char } {
    variable listbox
    variable SearchListboxString

    if { [string is wordchar $char] || [string is punct $char] \
	     || [string is space $char] } {
	append SearchListboxString $char

	set idx [$listbox selection get]
	if { [llength $idx] != 1 } {
	    set idx 0
	} else {
	    set idx [$listbox index $idx]
	    incr idx
	}
	set found 0
	foreach i [$listbox items $idx end] {
	    if { [string match -nocase $SearchListboxString* [$listbox itemcget $i -text]] } {
		$listbox selection set $i
		$listbox see $i
		set found 1
		break
	    }
	}
	if { !$found } {
	    foreach i [$listbox items 0 [expr $idx-1]] {
		if { [string match -nocase $SearchListboxString* [$listbox itemcget $i -text]] } {
		    $listbox selection set $i
		    $listbox see $i
		    set found 1
		    break
		}
	    }
	}
	if { !$found } {
	    bell
	    catch [list unset RamDebugger::SearchListboxString]
	} else {
	    after 100 [list catch [list unset RamDebugger::SearchListboxString]]
	}
    }
}

proc RamDebugger::UpdateLineNum { command args } {
    variable LineNum
    variable text
    variable filesmtime
    variable currentfile

    if { $command == "index" } { return }

    set idx [$text index insert]
    foreach "line col" [scan $idx "%d,%d"] break
    set LineNum "L: $line"

    if { $currentfile != "" && [file mtime $currentfile] > $filesmtime($currentfile) } {
	set filesmtime($currentfile) [file mtime $currentfile]
	set ret [tk_messageBox -default ok -icon warning -message \
		     "File '$currentfile' has been modified. Reload it?" -parent $text \
		     -title "reload file" -type okcancel]
	if { $ret == "ok" } {
	    OpenFileF $currentfile 1
	}
    }
}

proc RamDebugger::CreateImages {} {
    variable images
   
    set images(break) [image create photo -data {
	R0lGODlhCwALAIQAAMQ4OMQ6OsZBQchGRsZCQslLS9NsbNd7e9RubspNTcU6
	OuKfn+q5ueOiotRvb8dDQ/Xd3eu8vNl/f8lJSeSlpdVxccpPT///////////
	/////////////////////////yH5BAEKAB8ALAAAAAALAAsAAAU94CcCJCme
	ZYoCgTAQgUkGhXEgiZIKxsI0jkdqcGBAIpJJioBoRCgVIeDDSjgkFYtuSlI8
	Jo/tKjU9jUqnEAA7
    }]
    set images(arrow) [image create photo -data {
	R0lGODlhCwALAIQAAM/Ptbq6ltjYw8zMsb6+nby8ma2thLa2kd7ezb6+nKys
	ga2tgrOzjM3Ns8DAn729m66uhLe3kt/fzrq6l9nZxdDQt///////////////
	/////////////////////////yH5BAEKAB8ALAAAAAALAAsAAAUn4CeOZGme
	H4CKgTAOREwUxoGIiaLvC6N8DYfQ8YBEJKgJZVVZOU0hADs=
    }]
    set images(arrowbreak) [image create photo -data {
	R0lGODlhCwALAKUAAMQ4OMQ6OsZBQchGRsZCQs/PtclLS9NsbNd7e9Rubrq6
	ltjYw8zMsb6+nby8ma2thLa2kd7ezb6+nKysga2tgrOzjM3Ns8DAn729m66u
	hLe3kt/fzspNTdRvb9l/f9Vxcbq6l9nZxcU6OsdDQ8lJSdDQt///////////
	////////////////////////////////////////////////////////////
	/////////////////////////////////yH5BAEKAD8ALAAAAAALAAsAAAZF
	wJ8QQCQKj8UkEhAQDAgFIzFgOCASikWR0eg2HA9IhCiZmM+UygRgubgvmIxm
	A/gxOR3PBxSSAkQjJCMlRkNJhUd2RUdBADs=
    }]

    set images(blank) [image create photo -data {
	R0lGODlhCwALAIAAAP///////yH5BAEKAAEALAAAAAALAAsAAAIKjI+py+0P
	o5yhAAA7
    }]
    set images(file_blue) [image create photo -data {
	R0lGODlhCwANAKEAAAAAAFl48v///////yH5BAEAAAMALAAAAAALAA0AAAIh
	nI8Gy3sQogRCyelgmwNeri0X0H1g0HikZ67ohpZmnKYFADs=
    }]
    set images(file_magenta) [image create photo -data {
	R0lGODlhCwANAKEAAAAAANw35f///////yH5BAEAAAMALAAAAAALAA0AAAIh
	nI8Gy3sQogRCyelgmwNeri0X0H1g0HikZ67ohpZmnKYFADs=
    }]
    set images(file_yellow) [image create photo -data {
	R0lGODlhCwANAKEAAAAAAOi/PP///////yH5BAEAAAMALAAAAAALAA0AAAIh
	nI8Gy3sQogRCyelgmwNeri0X0H1g0HikZ67ohpZmnKYFADs=
    }]

    image create photo acttick16 -data {
	R0lGODlhEAAQAIIAAPwCBMT+xATCBASCBARCBAQCBEQCBAAAACH5BAEAAAAA
	LAAAAAAQABAAAAM2CLrc/itAF8RkdVyVye4FpzUgJwijORCGUhDDOZbLG6Nd
	2xjwibIQ2y80sRGIl4IBuWk6Af4EACH+aENyZWF0ZWQgYnkgQk1QVG9HSUYg
	UHJvIHZlcnNpb24gMi41DQqpIERldmVsQ29yIDE5OTcsMTk5OC4gQWxsIHJp
	Z2h0cyByZXNlcnZlZC4NCmh0dHA6Ly93d3cuZGV2ZWxjb3IuY29tADs=
    }
    image create photo edit16 -data {
	R0lGODlhEAAQAIYAAPwCBFxaVMR+RPzKjNze3AQCBMR6RPzGjPyODPz+/MzO
	zPyKDPyKBPz29OTWzPyGDPyGBOx6BOza1OR2BKROBNSOXKRKBBwOBOzu7PTW
	xPzizOySZPyCDFxaXOy2lNRyRMxmJCQOBPTm1OzStPTKrMR+XIRWLFxGNCQS
	BDQyNIRSNDQuJERGRLyqlNzSvIx6ZKRuVEw6LLSyrLymhKSShBwaFFROTJyW
	jMS+vNzW1OTazNzKrHRqXOzezOTOpPTq3OzWvOTStLyedMS+rLy2pMSynMSu
	lAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
	AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
	AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
	AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACH5BAEAAAAA
	LAAAAAAQABAAAAewgAAAAYSFhoQCA4IBBI2OjgUGBwiLBAmXlpcKkgsMlZcJ
	BA0JDpIPEBGVjwkSBgOnExSfmBIVBxAMExYXswkYGRobHLq8gh2PHhoeHyAW
	IYKzIiMkJSYnKCnQg5YNHtQqKywtK9qMBC4vMDEBMjIz2dCMDTQ1Njc4OToz
	5PEEOzw3ZPToMcLHO23HfogQ0QMIkCA+hPBbhAPHECJFjMyYIUQIvEUpUqwQ
	OXKkSEF+AgEAIf5oQ3JlYXRlZCBieSBCTVBUb0dJRiBQcm8gdmVyc2lvbiAy
	LjUNCqkgRGV2ZWxDb3IgMTk5NywxOTk4LiBBbGwgcmlnaHRzIHJlc2VydmVk
	Lg0KaHR0cDovL3d3dy5kZXZlbGNvci5jb20AOw==
    }
    image create photo actcross16 -data {
	R0lGODlhEAAQAIIAAASC/PwCBMQCBEQCBIQCBAAAAAAAAAAAACH5BAEAAAAA
	LAAAAAAQABAAAAMuCLrc/hCGFyYLQjQsquLDQ2ScEEJjZkYfyQKlJa2j7AQn
	MM7NfucLze1FLD78CQAh/mhDcmVhdGVkIGJ5IEJNUFRvR0lGIFBybyB2ZXJz
	aW9uIDIuNQ0KqSBEZXZlbENvciAxOTk3LDE5OTguIEFsbCByaWdodHMgcmVz
	ZXJ2ZWQuDQpodHRwOi8vd3d3LmRldmVsY29yLmNvbQA7
    }

    image create photo fileopen-22 -data {
	R0lGODlhFgAWAIYAAPwCBAQCBCQWDCwaDDwmFPSubPzGhPzCfPy2dOSmZPzK
	lPzSnPzOlPzKjBQODPzChPzWnPy2bPSmXPyuZOyeXIRSLEQuFEwyHEQqFDQi
	FCweDKRuPFRSTPT29PTy9Ozq7OTi3Nze3NTW1MzOzMTGxMTCxLy6tLSytKyu
	rDQyNMzKxOTm5OTi5Nza1NTS1MTCvLS2tLSyrKSmpJyenJSWlIyKjHx+fFxe
	XBwaHKxuPMzKzLy6vIyOjHx6fDw6NPy6dGxubLy+vISChCQmJNza3KyqrBQS
	FLR2RKSinJyanGxqZAwGBJSSlCwqLAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
	AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
	AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
	AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACH5BAEAAAAA
	LAAAAAAWABYAAAf/gACCg4SFhoeDAYqKiIeLj4wBjQCMhY+NkoiLk5qbhQID
	oJyGBAUGBwgEo4MECQoLDA2pDrS1tKQPEAwHERITE77AvxKqhAQNDA8UFRYX
	Fs8YBAQZGqGPxw0RGxwdHR4eHyAhIiMkJSYnKCgpBAYPEhcqHyssLS4kLzAx
	MjM0NTY3cBA4UCAHBw8gVnhgEcKFjhc7UPDj0cMHAAI/KFgY4YLFio/jRpTY
	sW8GDyCSCEQw2DChOHIqgsCQSEPIEEEEJFhAoUNECCJEyOk4d6KIyRtGcB7h
	IJKjixEjHu4oimSGEIs4d8IIUoKECnNB0ElMwkNJJgBLlJBAcQKGiR07KGAU
	RVGViY0mhIwwSTKjr99+THjUoIg0r48hTRIrRtxkiOMhDgrZCQQAIf5oQ3Jl
	YXRlZCBieSBCTVBUb0dJRiBQcm8gdmVyc2lvbiAyLjUNCqkgRGV2ZWxDb3Ig
	MTk5NywxOTk4LiBBbGwgcmlnaHRzIHJlc2VydmVkLg0KaHR0cDovL3d3dy5k
	ZXZlbGNvci5jb20AOw==
    }
    image create photo wizard-22 -data {
	R0lGODlhFgAWAIQAAPwCBNzaTPz6BAQCBPz+BExKBMzOTPz+rPz+3ISCBPTy
	hISCLISChPz+xOTiVPz+/MTCxKSipKyqrExOTDw+PDQyNAAAAAAAAAAAAAAA
	AAAAAAAAAAAAAAAAAAAAAAAAACH5BAEAAAAALAAAAAAWABYAAAV+ICCOZAkE
	ZqoKqoqKAzHAb1sIxhAQwWAQhRaJd0AcdgkhqaBAOBUL5WjAaD6TUqog0DAi
	ldqcg+cDtgaPACTCiM0AOhV6sG4DWOAHnf2uyfV1b1lsgVIwEgwTFHaGA2yK
	FYJgiJCSQo6JFJGGcJSalkKPn5wimZukAJWoIgYhACH+aENyZWF0ZWQgYnkg
	Qk1QVG9HSUYgUHJvIHZlcnNpb24gMi41DQqpIERldmVsQ29yIDE5OTcsMTk5
	OC4gQWxsIHJpZ2h0cyByZXNlcnZlZC4NCmh0dHA6Ly93d3cuZGV2ZWxjb3Iu
	Y29tADs=
    }
    image create photo editcopy-22 -data {
	R0lGODlhFgAWAIUAAPwCBBQSFPz+/DQyNISChDw6PMzKzMTGxERGRIyKjFxa
	XMTCvKSmpHR2dPz6/Pz29PTq3MS2rPz69MTCxFxWVHx6dJyWjNzSzPz27Pzy
	7Pzu5PTm3NTKvIR+fJyGfHxuZHxqXNTCtPTq5PTi1PTezNS+rExOTFRORMyy
	lPTaxOzWxOzSvNze3NTOxMy2nMyulMyqjAQCBAAAAAAAAAAAAAAAAAAAAAAA
	AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACH5BAEAAAAALAAAAAAWABYAAAbY
	QIBwSCwahYGkUnk0BgTQ6IAQaBKfUWhBYKhaAU+CgXAQIAyChLeJzSIQhcH6
	GFaM0QtGY5kstqEODw8QEQELAhJTc08KBBMEFBUWDRcBE1pca20SGBkaEBsc
	AY5maFRIAgoLHRQRHh8gIQFlZnByqA8ZGSIQIyQjJQEmYgJ5p2ACrK4gJx4g
	KIZZAgdeAQ4ZI9kjKSor0AwEjeAs1S0cHAslLi4vMDDRWeRIfEsxMeET4ATy
	VoYLC5fizXEiAR84BeMG+pEm8EsAFhAjSlR4hR6fLxiF0AkCACH+aENyZWF0
	ZWQgYnkgQk1QVG9HSUYgUHJvIHZlcnNpb24gMi41DQqpIERldmVsQ29yIDE5
	OTcsMTk5OC4gQWxsIHJpZ2h0cyByZXNlcnZlZC4NCmh0dHA6Ly93d3cuZGV2
	ZWxjb3IuY29tADs=
    }
    image create photo find-22 -data {
	R0lGODlhFgAWAIYAAPwCBBQSFJSWlFxaXJyanKy2tLzCxDw+PHyChNTa3Nzq
	7Nz29NTy9Mzu9OT29Nzi5Oz6/Mzy9Kzi7Jze5Lze5Nzy9JyenIyKjHR2dMTu
	9Kzm7JzW3ITW3ISChGxubERGROz+/Lzq9IzW5HzK1LTm7FRSVHTS3GS+zLTS
	1DQ2NMzOzMTq7Lzq7ITW5FS2vMTi5ExOTKTm7EzCzEy2vEyutJTa5FTK1ESi
	rGy+zMzi5ESerESmtITa5OTy9KTe5KyqrAz+/Azi3ASutERCRExKTFRWVAAA
	AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
	AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
	AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
	AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACH5BAEAAAAA
	LAAAAAAWABYAAAf/gACCggGFhYOIAAIDiYUEBQYFAoeJAweKhAgJCgsLDA0O
	CAGJg4wBCA8QDhENEhMUFaKDFhcYgwEJCxAQDBkaGxwSFaMAFh0eAx8AAQIK
	ECAODCEaIiMjJJMAtQMlygEGEBkVva4mJycTKKMYJd0pyyoOKyssGhMtIycu
	Gi/EMB/vlhmoQEFDjHsmZMygUUMdqWUCGgCbYMKEjRk3cEjI9jBADg3Wzs3Q
	sYOHA2IdO1SQkI/GjRMtFhBA2RFBDwk+OASrMPMHkIe3AhBA8QLFzAAHgvyg
	2dEQMSEHPCwltQgooSFSmVpSxKjjh6wPtwINgHVqsVpWESEFeywZ17RkED2s
	40YEgFirlIq4SwvUQCAAIf5oQ3JlYXRlZCBieSBCTVBUb0dJRiBQcm8gdmVy
	c2lvbiAyLjUNCqkgRGV2ZWxDb3IgMTk5NywxOTk4LiBBbGwgcmlnaHRzIHJl
	c2VydmVkLg0KaHR0cDovL3d3dy5kZXZlbGNvci5jb20AOw==
    }

    image create photo player_end-22 -data {
	R0lGODlhFgAWAIUAAPwCBDQyNFxeXAQCBMTCxGReZBQSFOzm7AwKDKymrJSS
	lFRSVCwqLLy6vPTy9OTe5Ozq7CQiJLSytOTi5BwaHPz+/HRydMzKzKSepJSO
	lKSipJyanIyGjIyKjKyurISGhMzGzJyWnHR2dISChIyOjLSutDw+PERCRHx6
	fJSWlIR+hJyenGRmZHx2fAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
	AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACH5BAEAAAAALAAAAAAWABYAAAa6
	QIBwSCwaj8ikMBBQKgOCgRMZIBSkxYHWoDVWD9EigpBQLLBERsPxCA8NDUhj
	gTBGJJNGG02RVBQWdUV3FxgZe0IGfoAGdhoXGxwdiAYef4FGFBoeHB8dGSBR
	ihUhIo1FBhkbIyMkJRYmAwYal4JEBh2RChIWJ1IIGxUZFqdECCgkHR6wWAYp
	FR2YWSobvL5vFgfDaEMDIivMRBEsD9HcQgMWvecDLB0tZ0btsfJa9vLXU/X6
	/P3+b0EAACH+aENyZWF0ZWQgYnkgQk1QVG9HSUYgUHJvIHZlcnNpb24gMi41
	DQqpIERldmVsQ29yIDE5OTcsMTk5OC4gQWxsIHJpZ2h0cyByZXNlcnZlZC4N
	Cmh0dHA6Ly93d3cuZGV2ZWxjb3IuY29tADs=
    }
    image create photo player_stop-22 -data {
	R0lGODlhFgAWAIUAAPwCBAQCBAwKDBQSFBwaHCQmJJSSlISChJSOlJSWlGxq
	bGRiZNTS1PTy9Pz+/Ozm7OTi5FRSVIyKjOTe5MTCxIR+hExOTHR2dLy6vLSy
	tLy2vHRydFxWXIyGjIyOjPz2/FRWVHx6fExKTMzOzJyanKSmpKyqrKSipAQG
	BLSutHx2fDw6PAwODAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
	AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACH5BAEAAAAALAAAAAAWABYAAAa1
	QIBwSCwaj8ikcslsAgKCAWEQjQ4KgSwyYDAcugZEQqFYYJECA6PhaLcfEEUk
	gJZAGJB8fkKpWOhHAxcOGBQZGBoaGQgbHIBGAhUOGR0SBxISBh4Xf0iCHxQS
	lRIIXhsgj0UCIaCXmJgHGyKpRJ+hmB5dHQqOaCENIx0epBIkBhdzngoPGCQl
	JifQJBvJRygRKRcKGxcXGypys1srEREc5SLnICLiR1koLFVUWfRO9vf4+Uwy
	QQAh/mhDcmVhdGVkIGJ5IEJNUFRvR0lGIFBybyB2ZXJzaW9uIDIuNQ0KqSBE
	ZXZlbENvciAxOTk3LDE5OTguIEFsbCByaWdodHMgcmVzZXJ2ZWQuDQpodHRw
	Oi8vd3d3LmRldmVsY29yLmNvbQA7
    }
    image create photo finish-22 -data {
	R0lGODlhFgAWAIUAAPwCBAw2VAQCBBxCXDR+nIS21Aw+XJTC1Nzu/KzO3Pz+
	/Nzq9Pz6/MTe7KTW5FzC1Nzu9CRKZMzi7IzK3Lzi7LTe7HzG3Gy+3AyuzAye
	xFzC3DRSbHy+1Dy61CSqzAySvAyStLze7IzO5AyGrEze7BRmjCTC1ETS3ETa
	5BTC3Bx2nAyWvAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
	AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACH5BAEAAAAALAAAAAAWABYAAAak
	QIBwSCwaj8hkMqBsBgTN5IAAJQqqykCBasUmDQcEV3gtBs7oATihGJeJgcOC
	QWc0HA8Ig/seRiQTFAsVFhcYGRp6VH1CGwscHQ8dGB4fIBkPIWKMAAMLIiAj
	IJcgH5gkGSWcARIiJicoJikpHikoHqqrKiW8JSogKymoqgCrV8cCARgkuFWc
	RwYeqVjPRgEExEPVRQbZ2l5IBuBRQ0zk5+hRBkEAIf5oQ3JlYXRlZCBieSBC
	TVBUb0dJRiBQcm8gdmVyc2lvbiAyLjUNCqkgRGV2ZWxDb3IgMTk5NywxOTk4
	LiBBbGwgcmlnaHRzIHJlc2VydmVkLg0KaHR0cDovL3d3dy5kZXZlbGNvci5j
	b20AOw==
    }
    image create photo down-22 -data {
	R0lGODlhFgAWAIUAAPwCBAw2VCRKZDRSbBxCXJTC1Mzi7Nzq9NTm9Bx2nAQC
	BNzu9JzG3Hy+1HzG3IzO5BRmjPz6/LTe7Dy61AyStCTC1FzC1AyGrETS3ETC
	1ETa5BRulAyuzBRylAw+XMTe7Gy+3CSqzAyexBTC3DR+nIS21KTW5Nzu/KzO
	3FzC3Pz+/ByixEze7AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
	AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACH5BAEAAAAALAAAAAAWABYAAAaR
	QIBwSCwaj8ikcnkMBAQDgjPAFAYKhsMBkVBUAYEFo+F4QLzVQEQyoVTOX/XB
	csHA0+vMRbNBMwkRDhxuHX5GTlIeHh8gISIjFAEeiVRECiQlDAUmgxQjIhwi
	JHdFlycoKSIUFCEjGiGkRpcqCxYijxorsUezcxYsuoZJsxLAu0qXB7DCTJfH
	VQrMX9PU1Uh0QQAh/mhDcmVhdGVkIGJ5IEJNUFRvR0lGIFBybyB2ZXJzaW9u
	IDIuNQ0KqSBEZXZlbENvciAxOTk3LDE5OTguIEFsbCByaWdodHMgcmVzZXJ2
	ZWQuDQpodHRwOi8vd3d3LmRldmVsY29yLmNvbQA7
    }
}


proc RamDebugger::InitGUI { { w .gui } } {
    variable opts
    variable marker
    variable text
    variable mainframe
    variable listbox
    variable images
    variable textST
    variable CacheDir
    variable breakpoints
    variable MainDir

    # WARNING
    if { $::tcl_platform(platform) == "windows" } {
	lappend ::auto_path /compasser/addons
    } else {
	lappend ::auto_path /c/compasser/addons
    }

    package require Tablelist
    package require BWidgetR
    package require supertext
    supertext::overrideTextCommand
    package require supergrid
    package require dialogwin
    package require helpviewer

    if { $::tcl_platform(platform) == "windows" } {
	package require registry
    }

    CreateImages

    catch {
	if { $::tcl_platform(platform) == "windows" } {
	    set data [registry get {HKEY_CURRENT_USER\Software\RamDebugger} IniData]
	} else {
	    set fin [open ~/.ramdebugger r]
	    set data [read $fin]
	    close $fin
	}
	    array set opts $data 
    }
 

    if { $::tcl_platform(platform) == "windows"} {
	font create NormalFont -family "MS Sans Serif" -size 8
	option add *font NormalFont
	font create FixedFont -family Courier -size 8
    } else {
	font create NormalFont -family {new century schoolbook} -size 12
	option add *font NormalFont
	font create FixedFont -family Courier -size 12
    }
    option add *Menu*TearOff 0

    toplevel $w
    wm title $w RamDebugger
    wm protocol $w WM_DELETE_WINDOW "RamDebugger::ExitGUI"

    set descmenu [list \
		"&File" all file 0 [list \
		[list command "&Open file" {} "Select source file" "Ctrl o" \
		-command "RamDebugger::OpenFile $w"] \
		[list cascad "&Debug on" {} activeprograms 0 {}] \
		separator \
                [list command "&Quit" {} "Exit program" "Ctrl q" \
		-command RamDebugger::ExitGUI] \
		] \
                "&Edit" all edit 0 [list \
		[list command "&Copy" {} "Copy selected text to clipboard" "Ctrl c" \
		-command "RamDebugger::CutCopyPasteText copy"] \
                separator \
		[list command "Search..." {} "Search text in source file" "Ctrl f" \
		-command "RamDebugger::SearchWindow"] \
		[list command "Continue search" {} "Continue searching text" "F3" \
		-command "RamDebugger::Search $w any"] \
		[list command "Isearch forward" {} "Incrementally search forward" "Ctrl s" \
		-command "RamDebugger::Search $w iforward"] \
		[list command "Isearch backward" {} "Incrementally search backward" "Ctrl r" \
		-command "RamDebugger::Search $w ibackward"] \
		[list command "&Goto line" {} "Go to the given line" "Ctrl g" \
		-command "RamDebugger::GotoLine"] \
                ] \
		"&Debug" all debug 0 [list \
		[list command "&Continue/Go" {} "begin/continue execution" "F5" \
		-command "RamDebugger::ContNextGUI rcont"] \
		[list command "Set/unset &breakpoint" {} "Set/unset breakpoint" "F9" \
		-command "RamDebugger::SetGUIBreakpoint"] \
		[list command "&Next" {} "continue one command" "F10" \
		-command "RamDebugger::ContNextGUI rnext"] \
		[list command "&Step" {} "continue one command, entering in subcommands" "F11" \
		-command "RamDebugger::ContNextGUI rstep"] \
		[list command "Continue &to" {} "continue to selected line" "Ctrl F5" \
		-command "RamDebugger::ContNextGUI rcontto"] \
		separator \
		[list command "Expressions..." {} \
                    "Open a window to visualize expresions or variables" "" \
		-command "RamDebugger::DisplayVarWindow"] \
		[list command "Breakpoints..." {} \
                    "Open a window to visualize the breakpoints list" "" \
		-command "RamDebugger::DisplayBreakpointsWindow"] \
		[list command "&Timing control..." {} \
                    "Open a window to control execution times" "" \
		-command "RamDebugger::DisplayTimesWindow"] \
		] \
		"&Utilities" all utilites 0 [list \
		[list command "&Open console" {} "Open console" "" \
		-command "RamDebugger::OpenConsole"] \
		[list command "O&pen VisualRegexp" {} "Open VisualRegexp" "" \
		-command "RamDebugger::OpenVisualRegexp"] \
		[list command "&View instrumented file" {} "View instrumented file" "" \
		-command "RamDebugger::ViewInstrumentedFile instrumented"] \
		[list command "&View instrumented info file" {} "View instrumented info file" "" \
		-command "RamDebugger::ViewInstrumentedFile info"] \
		separator \
		[list command "&Windows hierarchy" {} "View windows hierarchy" "" \
		-command "RamDebugger::DisplayWindowsHierarchy"] \
		] \
		"&Help" all help 0 [list \
		[list command "&Help" {} "Gives help" "Ctrl h" \
		-command "RamDebugger::ViewHelpFile"] \
		] \
		]

    set mainframe [MainFrame $w.mainframe \
		       -textvariable RamDebugger::status \
		       -progressvar RamDebugger::progressvar -progressmax 100 \
		       -progresstype normal -menu $descmenu -grid 0]
    $mainframe showstatusbar progression 
    $mainframe addindicator -textvariable RamDebugger::LineNum -width 6 \
	-anchor e -padx 3
    set label [$mainframe addindicator -textvariable RamDebugger::remoteserver -width 15 \
		   -anchor e -padx 3]
    set menu [$mainframe getmenu activeprograms]
    $menu configure -postcommand [list RamDebugger::ActualizeActivePrograms $menu]

    bind $label <1> "tk_popup $menu %X %Y"


    ################################################################################
    #     The toolbar
    ################################################################################

    set toolbar [$mainframe addtoolbar]

    set bbox [ButtonBox $toolbar.bbox1 -spacing 0 -padx 1 -pady 1 -homogeneous 1 -grid "0 w"]
    $bbox add -image fileopen-22 \
         -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1 -padx 1 -pady 1 \
         -helptext "Select source file" \
         -command "RamDebugger::OpenFile $w"

    set menudev [$mainframe getmenu debug]

    $bbox add -image editcopy-22 \
         -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1 -padx 1 -pady 1 \
         -helptext "Copy selected text to clipboard" \
         -command "RamDebugger::CutCopyPasteText copy"
    $bbox add -image find-22 \
         -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1 -padx 1 -pady 1 \
         -helptext "Search text in source file" \
         -command "RamDebugger::SearchWindow"

    Separator $toolbar.sep -orient vertical -grid "1 ns px3"

    set bbox [ButtonBox $toolbar.bbox2 -spacing 0 -padx 1 -pady 1 -homogeneous 1 -grid "2 w"]

    $bbox add -image player_end-22 \
         -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1 -padx 1 -pady 1 \
         -helptext "begin/continue execution" \
         -command "RamDebugger::ContNextGUI rcont"
    $bbox add -image player_stop-22 \
         -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1 -padx 1 -pady 1 \
         -helptext [_ "Set/unset &breakpoint"] \
         -command "RamDebugger::SetGUIBreakpoint"
    $bbox add -image finish-22 \
         -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1 -padx 1 -pady 1 \
         -helptext "continue one command" \
         -command "RamDebugger::ContNextGUI rnext"
    $bbox add -image down-22 \
         -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1 -padx 1 -pady 1 \
         -helptext "continue one command, entering in subcommands" \
         -command "RamDebugger::ContNextGUI rstep"

    supergrid::go $toolbar


    set f [$mainframe getframe]

    ################################################################################
    # the horizontal 3 levels pane
    ################################################################################

    set pw [PanedWindow $f.pw -side top -pad 0 -weights available -grid 0 -activator line]

    foreach "weight1 weight2 weight3" [ManagePanes $pw h "1 6 3"] break

    set pane1 [$pw add -weight $weight1]

    if { ![info exists opts(defaultdir)] } {
	set opts(defaultdir) [pwd]
    }
    label $pane1.l -textvar RamDebugger::opts(defaultdir) -anchor e -relief raised -bd 1 \
	-padx 5 -grid "0 ew"

    set sw [ScrolledWindow $pane1.lf -relief sunken -borderwidth 0 -grid 0]
    set listbox [ListBox $sw.lb -background white -multicolumn 0 -selectmode single]
    $sw setwidget $listbox

    set pane2 [$pw add -weight $weight2]

    ################################################################################
    # the vertical edit window and stack trace
    ################################################################################

    set pwin [PanedWindow $pane2.pw -side left -pad 0 -weights available -grid 0 -activator line]

    foreach "weight1in weight2in" [ManagePanes $pwin v "6 1"] break

    set pane2in1 [$pwin add -weight $weight1in]


    set marker [canvas $pane2in1.can -bg grey90 -grid "0 wns" -width 14 -bd 0 -highlightthickness 0]
    set text [text $pane2in1.text -background white -wrap none -width 80 -height 40 \
		  -exportselection 0 -font FixedFont -highlightthickness 0 -editable 0 \
		  -postproc RamDebugger::UpdateLineNum -bd 0 -grid 1 \
		  -xscrollcommand [list $pane2in1.xscroll set] \
		  -yscrollcommand [list RamDebugger::ScrollScrollAndCanvas $pane2in1.text \
				       $pane2in1.yscroll $pane2in1.can]]
    scrollbar $pane2in1.yscroll -orient vertical -grid 2 -command \
	[list RamDebugger::ScrollTextAndCanvas $pane2in1.text $pane2in1.can]
    scrollbar $pane2in1.xscroll -orient horizontal -grid "0 2" -command "$pane2in1.text xview"

    if { $::tcl_platform(platform) != "windows" } {
	$text conf -exportselection 1 -selectbackground \#48c96f -selectforeground white
    }

    proc ScrollTextAndCanvas { text canvas args } {
 	eval $text yview $args
	$canvas yview moveto [lindex [$text yview] 0]
    }
    proc ScrollScrollAndCanvas { text yscroll canvas args } {
	eval $yscroll set $args
	$canvas yview moveto [lindex [$text yview] 0]
    }

#    set sw [ScrolledWindow $pane2in1.lf -relief sunken -borderwidth 0 -grid 0]
#     set text [text $sw.text -background white -wrap none -width 80 -height 40 \
# 		  -exportselection 0 -font FixedFont -highlightthickness 0 -editable 0 \
# 		  -postproc RamDebugger::UpdateLineNum]
#     $sw setwidget $text

    set pane2in2 [$pwin add -weight $weight2in]

    set sw2 [ScrolledWindow $pane2in2.lf2 -relief sunken -borderwidth 0 -grid "0"]
    set textST [text $sw2.text2 -background white -wrap none -width 80 -height 4 \
	    -highlightthickness 0]
    $sw2 setwidget $textST


    set pane3 [$pw add -weight $weight3]

    ################################################################################
    # the vertical user defined - local
    ################################################################################

    set pw1 [PanedWindow $pane3.pw -side left -pad 0 -weights available -grid "0" -activator line]

    foreach "weight3in1 weight3in2" [ManagePanes $pw1 h "1 1"] break

    set pane3in1 [$pw1 add -weight $weight3in1]

    label $pane3in1.l1 -text "User defined variables" -relief raised -bd 1 -grid "0 ew"

    set sw [ScrolledWindow $pane3in1.sw -borderwidth 0 -bd 1 -relief raised -grid "0 nswe"]
    set sf [ScrollableFrame $sw.f -constrainedwidth 1]
    $sw setwidget $sf
    set f1 [$sf getframe]


    ################################################################################
    # the horizontal user defined vars
    ################################################################################

    set pw [PanedWindow $f1.pw -side top -pad 0 -weights available -grid "0 ns" -activator line]

    foreach "weight1 weight2" [ManagePanes $pw h "1 1"] break

    set pane1_vars [$pw add -weight $weight1]
    set pane2_vars [$pw add -weight $weight2]

    label $pane1_vars.l -text Variables -relief raised -bd 1 -grid "0 ew"
    label $pane2_vars.l -text Values -relief raised -bd 1 -grid "0 ew"
    for { set i 0 } { $i < 20 } { incr i } {
	set RamDebugger::EvalEntries($i,leftentry) [entry $pane1_vars.e$i -textvariable \
            RamDebugger::EvalEntries($i,left) -bd 0 \
	    -highlightthickness 1 -highlightbackground grey90 -grid 0]
	set RamDebugger::EvalEntries($i,rightentry) [entry $pane2_vars.e$i -textvariable \
            RamDebugger::EvalEntries($i,right) -bd 0 \
	    -highlightthickness 1 -highlightbackground grey90 -grid 0]

	bind $pane1_vars.e$i <Return> {tkTabToWindow [tk_focusNext %W]}
	bind $pane2_vars.e$i <Return> {tkTabToWindow [tk_focusNext %W]}
	bind $pane1_vars.e$i <FocusOut> "RamDebugger::CheckEvalEntries do $i,left"
	bind $pane2_vars.e$i <FocusOut> "RamDebugger::CheckEvalEntries do $i,right"
	bind $pane1_vars.e$i <ButtonRelease-1> {
	    %W selection range 0 end
	    %W icursor end
	}
	bind $pane2_vars.e$i <ButtonRelease-1> {
	    %W selection range 0 end
	    %W icursor end
	}
    }
    if { [info exists opts(watchedvars)] } {
	set i 0
	foreach j $opts(watchedvars) {
	    set RamDebugger::EvalEntries($i,left) $j
	    incr i
	}
    }

    set pane3in2 [$pw1 add -weight $weight3in2]

    label $pane3in2.l1 -text "Local variables" -relief raised -bd 1 -grid "0 ew"

    set sw [ScrolledWindow $pane3in2.sw -borderwidth 0 -bd 1 -relief raised -grid "0 nswe"]
    set sf [ScrollableFrame $sw.f -constrainedwidth 1]
    $sw setwidget $sf
    set f1L [$sf getframe]


    ################################################################################
    # the horizontal local vars
    ################################################################################

    set pwL [PanedWindow $f1L.pw -side top -pad 0 -weights available -grid "0 ns" -activator line]

    foreach "weight1 weight2" [ManagePanes $pwL h "1 1"] break

    set pane1_varsL [$pwL add -weight $weight1]
    set pane2_varsL [$pwL add -weight $weight2]

    label $pane1_varsL.l -text Variables -relief raised -bd 1 -grid "0 ew"
    label $pane2_varsL.l -text Values -relief raised -bd 1 -grid "0 ew"
    for { set i 0 } { $i < 20 } { incr i } {
	set RamDebugger::EvalEntries($i,leftentryL) [entry $pane1_varsL.e$i -textvariable \
            RamDebugger::EvalEntries($i,leftL) -bd 0 \
	    -highlightthickness 1 -highlightbackground grey90 -state disabled -grid 0]
	set RamDebugger::EvalEntries($i,rightentryL) [entry $pane2_varsL.e$i -textvariable \
            RamDebugger::EvalEntries($i,rightL) -bd 0 \
	    -highlightthickness 1 -highlightbackground grey90 -grid 0]

	bind $pane2_varsL.e$i <Return> {tkTabToWindow [tk_focusNext %W]}
	bind $pane2_varsL.e$i <FocusOut> "RamDebugger::CheckEvalEntriesL do $i,rightL"
	bind $pane2_varsL.e$i <ButtonRelease-1> {
	    %W selection range 0 end
	    %W icursor end
	}
    }


    ################################################################################
    # the nice supergrid
    ################################################################################

    supergrid::go $pane3in1
    supergrid::go $pane3in2
    supergrid::go $pane2_vars
    supergrid::go $pane1_vars
    supergrid::go $pane2_varsL
    supergrid::go $pane1_varsL
    supergrid::go $f1
    supergrid::go $f1L
    supergrid::go $pane2in1
    supergrid::go $pane2in2
    supergrid::go $pane1
    supergrid::go $pane2
    supergrid::go $pane3
    supergrid::go $f
    supergrid::go $w

    $listbox bindImage <1> "+focus $listbox ;#"
    $listbox bindText <1> "+focus $listbox ;#"
    $listbox bindImage <Double-1> "RamDebugger::ListBoxDouble1"
    $listbox bindText <Double-1> "RamDebugger::ListBoxDouble1"
    bind $listbox <KeyPress> [list RamDebugger::SearchInListbox %A]
    bind $listbox <Return> "RamDebugger::ListBoxDouble1 \[$listbox selection get]"
    bind $listbox <Home> "$listbox see 0"
    bind $listbox <End> "$listbox see end"
    set menudev [$mainframe getmenu debug]
    bind $text <3> "%W mark set insert @%x,%y ; tk_popup $menudev %X %Y"


    $textST conf -state disabled
    bind $textST <1> { focus %W }
    bind $text <Motion> "RamDebugger::TextMotion %X %Y %x %y"
    # in linux, F10 makes some stupid thing
    bind all <F10> ""

    bind $marker <1> {
	set tkPriv(x) 0
	set tkPriv(y) %y
	set tkPriv(mouseMoved) 0
	set tkPriv(pressX) 0
	$RamDebugger::text mark set insert [tkTextClosestGap $RamDebugger::text 0 %y]
	$RamDebugger::text mark set anchor insert

	set ini [$RamDebugger::text index "@0,%y linestart"]
	set end [$RamDebugger::text index "@0,%y lineend"]
	$RamDebugger::text tag remove sel 1.0 end
	$RamDebugger::text tag add sel $ini $end
	set tkPriv(selectMode) line
    }
    bind $marker <B1-Motion> {
	set tkPriv(x) 0
	set tkPriv(y) %y
	tkTextSelectTo $RamDebugger::text 0 %y
    }
    bind $marker <B1-Leave> {
	set tkPriv(x) 0
	set tkPriv(y) %y
	tkTextAutoScan $RamDebugger::text
    }
    bind $marker <B1-Enter> {
	tkCancelRepeat
    }
    bind $marker <ButtonRelease-1> {
	tkCancelRepeat
    }
    if { ![info exists opts(maingeometry)] } {
	set opts(maingeometry) 300x300+300+300
    }
    wm geom $w $opts(maingeometry)

    ActualizeActivePrograms $menu 1

    if { [info exists opts(breakpoints)] } {
	set breakpoints $opts(breakpoints)
    }

    if { [info exists opts(remoteserverIsLocal)] && $opts(remoteserverIsLocal) } {
	# nothing 
    } elseif { [info exists opts(remoteserver)] } {
	SetMessage "Connecting remoteserver $opts(remoteserver)..."
	catch { rdebug $opts(remoteserver) }
	SetMessage ""
    }

    if { [info exists opts(currentfile)] && $opts(currentfile) != ""  } {
	SetMessage "Opening file '$opts(currentfile)'..."
	OpenFileF $opts(currentfile)
	if { [winfo exists opts(currentidx)] } {
	    $text see $opts(currentidx)
	    $text mark set insert $opts(currentidx)
	}
	SetMessage ""
    } else { FillListBox }

    focus $text

    # for tkcon
    rename ::exit ::exit_final
    proc ::exit { args } {}

}

RamDebugger::Init

################################################################################
#     Init the GUI part
################################################################################

if { [info command wm] != "" && [info commands tkcon_puts] == "" } {
    wm withdraw .
    RamDebugger::InitGUI
    bind all <Control-x><Control-l> "source [info script] ; WarnWin reload"
}

