#!/bin/sh
# the next line restarts using wish \
exec wish "$0" "$@"

#         $Id: RamDebugger.tcl,v 1.10 2002/09/02 08:35:46 ramsan Exp $        
# RamDebugger  -*- TCL -*- Created: ramsan Jul-2002, Modified: ramsan Aug-2002


################################################################################
#  This software is copyrighted by Ramon Ribó (RAMSAN) ramsan@cimne.upc.es.
#  (http://gid.cimne.upc.es/ramsan) The following terms apply to all files 
#  associated with the software unless explicitly disclaimed in individual files.

#  The authors hereby grant permission to use, copy, modify, distribute,
#  and license this software and its documentation for any purpose, provided
#  that existing copyright notices are retained in all copies and that this
#  notice is included verbatim in any distributions. No written agreement,
#  license, or royalty fee is required for any of the authorized uses.
#  Modifications to this software may be copyrighted by their authors
#  and need not follow the licensing terms described here, provided that
#  the new terms are clearly indicated on the first page of each file where
#  they apply.

#  IN NO EVENT SHALL THE AUTHORS OR DISTRIBUTORS BE LIABLE TO ANY PARTY
#  FOR DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES
#  ARISING OUT OF THE USE OF THIS SOFTWARE, ITS DOCUMENTATION, OR ANY
#  DERIVATIVES THEREOF, EVEN IF THE AUTHORS HAVE BEEN ADVISED OF THE
#  POSSIBILITY OF SUCH DAMAGE.

#  THE AUTHORS AND DISTRIBUTORS SPECIFICALLY DISCLAIM ANY WARRANTIES,
#  INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY,
#  FITNESS FOR A PARTICULAR PURPOSE, AND NON-INFRINGEMENT.  THIS SOFTWARE
#  IS PROVIDED ON AN "AS IS" BASIS, AND THE AUTHORS AND DISTRIBUTORS HAVE
#  NO OBLIGATION TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR
#  MODIFICATIONS.
################################################################################

namespace eval RamDebugger {

    ################################################################################
    #    Non GUI commands
    ################################################################################

    namespace export rhelp rdebug rlist reval rcont rnext rstep rbreak rcond rinfo rdel \
	    rstack routput rtime

    ################################################################################
    # communications issues
    ################################################################################

    # can be: remote; local or gdb
    variable remoteserverType ""
    variable remoteserver ""
    variable remoteserverNum ""
    variable debuggerserver ""
    variable debuggerserverNum ""
    variable services
    
    ################################################################################
    # debugger state
    ################################################################################

    variable debuggerstate "" ;# can be: "" or debug or time
    variable currentfile ""
    variable currentline 1
    variable currentfileIsModified 0
    variable files
    variable instrumentedfilesP
    variable instrumentedfilesR
    variable instrumentedfilesTime
    variable instrumentedfilesSent
    variable fileslist ""
    variable breakpoints ""
    variable TimeMeasureData ""
    variable gdblog ""

    variable MainDir
    variable CacheDir

    ################################################################################
    # GUI state
    ################################################################################

    variable text ""
    variable textST
    variable textOUT
    variable textCOMP
    variable IsInStop 0
    variable TextMotionAfterId ""
    variable ExpressionResult ""
    variable count
    variable listbox ""
    variable progressvar
    variable status
    variable WindowFilesList ""
    variable WindowFilesListCurr -1

    ################################################################################
    # Preferences
    ################################################################################

    variable options
    variable options_def

}

################################################################################
#   Init proc
################################################################################

proc RamDebugger::Init { readprefs } {
    variable debuggerserver
    variable debuggerserverNum
    variable MainDir
    variable CacheDir
    variable options_def
    variable options

    if { [info exists ::freewrap::scriptFile] } {
	set dir $::argv0
    } else {
	set dir [info script]
    }
    set dir [file join [pwd] [file dirname $dir]]
    set MainDir $dir

    if { ![file isdir [file join $MainDir addons]] } {
	set text "error: bad installation. Directory 'addons' could not be found in '$MainDir'"
	puts $text
	catch { tk_messageBox -message $text }
    }

    lappend ::auto_path [file join $MainDir addons]
    lappend ::auto_path [file join $MainDir scripts]

    if { ![file isdir [file join $MainDir cache]] } {
	catch { file mkdir [file join $MainDir cache] }
    }
    if { [file isdir [file join $MainDir cache]] } {
	set CacheDir [file join $MainDir cache]
    }

    ################################################################################
    # Setting preferences
    ################################################################################

    set options_def(indentsizeTCL) 4
    set options_def(indentsizeC++) 2
    set options_def(ConfirmStartDebugging) 1
    set options_def(LocalDebuggingType) tk

    switch $::tcl_platform(platform) {
	windows {
	    set options_def(NormalFont) { -family "MS Sans Serif" -size 8 -weight normal \
		-slant roman -underline 0 -overstrike 0 }
	    set options_def(FixedFont)  { -family "Courier" -size 8 -weight normal \
		-slant roman -underline 0 -overstrike 0 }
	    set options_def(HelpFont)  { -family "Helvetica" -size 11 -weight normal \
		-slant roman -underline 0 -overstrike 0 }
	}
	default {
	    set options_def(NormalFont) { -family "new century schoolbook" -size 12 \
		    -weight normal -slant roman -underline 0 -overstrike 0 }
	    set options_def(FixedFont)  { -family "Courier" -size 12 -weight normal \
		-slant roman -underline 0 -overstrike 0 }
	    set options_def(HelpFont)  { -family "Helvetica" -size 15 -weight normal \
		-slant roman -underline 0 -overstrike 0 }
	}
    }

    # this variable is only used on windows. It can be:
    # 0: Only check remote programs on demand (useful if not making remote debugging, the
    #    start up is faster)
    # 1: Register as remote and check remote programs on start up. It can be slower the
    #    start up but is better when making remote debugging

    if { $::tcl_platform(platform) == "windows" } {
	set options_def(CheckRemotes) 0
    }


    ################################################################################
    # Reading preferences (they are only saved in GUI mode)
    ################################################################################

    if { $::tcl_platform(platform) == "windows" } {
	package require registry
    }

    array set options [array get options_def]

    if { $readprefs } {
	catch {
	    if { $::tcl_platform(platform) == "windows" } {
		set data [registry get {HKEY_CURRENT_USER\Software\RamDebugger} IniData]
	    } else {
		set fin [open ~/.ramdebugger r]
		set data [read $fin]
		close $fin
	    }
	    array set options $data 
	}
    }

    ################################################################################
    # Increasing the path variable (just to ckeck typical locations)
    ################################################################################

    if { $::tcl_platform(platform) == "windows" } {
	set err [catch { set shortname [file native [file attributes [file join $MainDir addons] \
		-shortname]] }]
	if { !$err } {
	    if { [info exists ::env(PATH)] && $::env(PATH) != "" } {
		append ::env(PATH) ";$shortname"
	    } else {
		set ::env(PATH) "$shortname"
	    }
	}
	set dirs [list "c:/tcltk/mingw1.1" "c:/mingw1.1" "c:/mingw" "c:/mingw32"]
	foreach i $dirs {
	    foreach j [list $i [file join $i bin] [file join $i mingw bin] \
		[file join $i mingw32 bin]] {
		set err [catch { set shortname [file native [file attributes $j -shortname]] }]
		if { !$err } {
		    if { [info exists ::env(PATH)] && $::env(PATH) != "" } {
		        append ::env(PATH) ";$shortname"
		    } else {
		        set ::env(PATH) "$shortname"
		    }
		}
	    }
	}
    }

    ################################################################################
    # Registering as remote server
    ################################################################################

    if { $debuggerserver != "" } { return }

    set debuggerserver ramdebugger

    if { $::tcl_platform(platform) == "windows" } {
	if { $options(CheckRemotes) == 1 } {
	    uplevel \#0 package require commR
	    set debuggerserverNum [comm::register RamDebugger 1]
	}
    } else {
	if { [info command wm] != "" } {
	    package require Tk
	    wm withdraw .
	}
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
    variable remoteserverType
    variable remoteserverNum
    variable debuggerserver
    variable debuggerserverNum
    variable currentfile
    variable services
    variable instrumentedfilesSent
    variable debuggerstate
    variable gdblog
    variable MainDir
    variable options

    set usagestring {usage: rdebug ?switches? ?program?
	-h:             displays usage
	-actives:       return active programs
	-forceupdate:   force update of remote program search
	-forceupdate2:  force update of remote program search, try harder
	-disconnect:    disconnect from remoteserver
	-currentfile:   execute and debug currentfile
	-debugcplusplus: execute and debug a c++ file. program is a list with prg. name and args
	--:             end of options

	To begin debugging a TCL file, select the file with 'rlist' and use 'rdebug -currentfile'.
	To begin debugging a remote program, use 'rdebug program', where program is one active
	program, that must belong to the services list.
    }
    ParseArgs $args $usagestring opts

    if { $opts(-forceupdate2) } {
	FindActivePrograms 2
    } elseif { $opts(-forceupdate) } {
	FindActivePrograms 1
    } else { FindActivePrograms 0 }
    
    if { $opts(-actives) } { return [array names services] }

    if { $opts(-disconnect) } {
	if { $remoteserver == "" } {
	    error "error. There is no connected remote server"
	}
	if { $remoteserverType == "local" } {
	    interp delete local
	} elseif { $remoteserverType == "gdb" } {
	    catch { close [lindex $remoteserver 0] }
	}
	set remoteserver ""
	set remoteserverType ""
	set debuggerstate ""
	TakeArrowOutFromText
	return
    }
    if { $opts(-currentfile) } {
	if { [interp exists local] } { interp delete local }
	interp create local
	interp alias local sendmaster "" eval
	# dirty trick to avoid the slave interp block
	interp eval local {
	    proc updatemaster {} {
		sendmaster update
		after 3000 updatemaster
	    }
	}
	interp alias local exit "" interp delete local
	local eval { set argc 0 ; set argv "" }
	set err [catch {package present registry} ver]
	if { !$err } {
	    interp alias local registry "" registry
	    interp eval local package provide registry $ver
	}
	if { ![info exists options(LocalDebuggingType)] || $options(LocalDebuggingType) == "tk" } {
	    interp eval local [list load {} Tk]
	}
	set remoteserverType local
	if { $currentfile == "" } {
	    error "Error. there is no current file"
	}
	set remoteserver $currentfile
	TakeArrowOutFromText
    } elseif { $opts(-debugcplusplus) } {
	if { $opts(program) == "" } {
	    if { $remoteserver != "" } {
		return [lindex $remoteserver 1]
	    } else { error "error. $usagestring\nActive programs: [array names services]" }
	}

	if { [auto_execok gdb] == "" } {
	    variable text
	    if { [info exists text] && [winfo exists $text] } {
		set ret [DialogWin::messageBox -default yes -icon question -message \
		    "Could not find command 'gdb'. Do you want to see the help?" -parent $textST \
		    -title "Command not found" -type yesno]
		if { $ret == "yes" } {
		    RamDebugger::ViewHelpFile "01RamDebugger/RamDebugger_12.html"
		}
		return
	    } else {
		error "Could not find command 'gdb'"
	    }
	}
	
	if { $remoteserverType == "local" } {
	    interp delete local
	} elseif { $remoteserverType == "gdb" } {
	    catch { close [lindex $remoteserver 0] }
	}
	set gdblog ""
	set remoteserverType gdb
#          if { $::tcl_platform(platform) == "windows" } {
#              set cat [file join $MainDir addons cat.exe]
#          } else { set cat cat }
	set dir [lindex $opts(program) 1]
	set pwd [pwd]
	cd $dir        
	set fid [open "|gdb -q |& cat" r+]
	cd $pwd
	set remoteserver [list $fid $opts(program) start]
	fconfigure $fid -blocking 0 -buffering line
	fileevent $fid readable RamDebugger::RecieveFromGdb
        TakeArrowOutFromText
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
	if { $remoteserverType == "local" } {
	    interp delete local
	} elseif { $remoteserverType == "gdb" } {
	    catch { close [lindex $remoteserver 0] }
	}
	set remoteserverType remote
	TakeArrowOutFromText
    }
    TextOutClear

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
	if { [info commands ::RDC::bgerror_base] == "" } {
	    auto_import ::bgerror
	    if { [info commands ::bgerror] != "" } {
		rename ::bgerror ::RDC::bgerror_base
	    }
	    proc ::bgerror err {
		RDC::SendDev [list RamDebugger::RecieveErrorFromProgram $err $::errorInfo]
	    }
	}
	if { [info commands ::RDC::puts_base] == "" } {
	    rename ::puts ::RDC::puts_base
	    proc ::puts args {
		set argsN $args
		set hasnewline 1
		if { [lindex $argsN 0] == "-nonewline" } {
		    set hasnewline 0
		    set argsN [lrange $argsN 1 end]
		}
		set channelId stdout
		if { [llength $argsN] == 2 } {
		    set channelId [lindex $argsN 0]
		    set argsN [lrange $argsN 1 end]
		}
		if { [llength $argsN] == 1 && [regexp {stdout|stderr} $channelId] } {
		    RDC::SendDev [list RamDebugger::RecieveOutputFromProgram $channelId \
		        [lindex $argsN 0] $hasnewline]
		} else {
		    eval ::RDC::puts_base $args
		}
	    }
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
		regexp "RDC::F\\s+$filenum+\\s+$line\\s+; (\[^\n]*)" [info body $procname] {} \
		        textline
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
    if { $remoteserverType == "local" } {
	set remotecomm [string map [list SENDDEVBODY "sendmaster \$comm"] \
		            $remotecomm]
    } elseif {  $remoteserverType == "gdb" } {
	set remotecomm "set confirm off\n"
	append remotecomm "file \"[lindex $opts(program) 0]\"\n"
	if { [lindex $opts(program) 1] != "" } {
	    append remotecomm "set args [lindex $opts(program) 2]"
	}

    } elseif { $::tcl_platform(platform) == "windows" } {
	set remotecomm [string map [list SENDDEVBODY "comm::comm send $debuggerserverNum \$comm"] \
		            $remotecomm]
    } else {
	set remotecomm [string map [list SENDDEVBODY "send \"$debuggerserver\" \$comm"] \
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
	after idle [list RamDebugger::rlist -quiet -asmainfile $currentfile]
    }
    if { $opts(-debugcplusplus) } {
	EvalRemote "run"
    }
    return "Begin debugging of program '$remoteserver'"
}

proc RamDebugger::reval { args } {
    variable ExpressionResult
    variable debuggerstate
    variable remoteserverType
    variable remoteserver

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
	if { $remoteserverType != "gdb" } {
	    EvalRemote [list ::RDC::Eval $opts(arg) $opts(-handler)]
	} else {
	    set remoteserver [lreplace $remoteserver 2 2 [list print $opts(-handler)]]
	    EvalRemote "print $opts(arg)"
	}
	return ""
    }
    if { $remoteserverType != "gdb" } {
	EvalRemote [list ::RDC::Eval $opts(arg)]
    } else {
	set remoteserver [lreplace $remoteserver 2 2 print]
	EvalRemote "print $opts(arg)"
    }
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
    variable remoteserver
    variable remoteserverType
    variable ExpressionResult

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

    if { $remoteserverType == "gdb" } {
	set remoteserver [lreplace $remoteserver 2 2 backtrace]
	set ExpressionResult ""
	EvalRemote "backtrace\nprintf \"FINISHED BACKTRACE\\n\""
	if { $ExpressionResult == "" } { vwait RamDebugger::ExpressionResult }
	if { $opts(-handler) != "" } {
	    uplevel \#0 $opts(-handler) [list $ExpressionResult]
	    return
	} else {
	    return $ExpressionResult
	}
    }
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
    variable instrumentedfilesP
    variable instrumentedfilesR
    variable currentline
    variable fileslist
    variable debuggerstate
    variable remoteserverType

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
	set filenum [lsearchfile $fileslist $currentfile]
	if { $remoteserverType != "gdb" } {
	    set ipos [string first "RDC::F $filenum $currentline ;" $instrumentedfilesP($currentfile)]
	    if { $ipos == -1 } {
		set ipos [string first "RDC::F $filenum $currentline ;" \
		    $instrumentedfilesR($currentfile)]
		if { $ipos == -1 } {
		    error "error: line $currentline is not instrumented"
		}
	    }
	}
    }

    rlist -quiet
    StopAtGUI "" ""
    if { $remoteserverType != "gdb" } {
	if { $opts(line) != "" } {
	    set filenum [lsearchfile $fileslist $currentfile]
	    EvalRemote [list set ::RDC::contto [list $filenum $currentline]]
	}
	EvalRemote [list set ::RDC::stopnext 0]
	EvalRemote ::RDC::Continue
    } else {
	if { $opts(line) != "" } {
	    EvalRemote "tbreak [file tail $currentfile]:$currentline"
	}
	EvalRemote "cont"
    }
}

proc RamDebugger::rnext { args } {
    variable debuggerstate
    variable remoteserverType
    variable remoteserver

    if { $debuggerstate == "time" } {
	error "Command rnext cannot be used in 'time' mode. Check rtime"
    }
    
    set usagestring {usage: rnext ?switches?
	-h:       displays usage
	--:     end of options
    }
    ParseArgs $args $usagestring opts

    rlist -quiet
    StopAtGUI "" ""

    if { $remoteserverType != "gdb" } {
	EvalRemote [list set ::RDC::stopnext 1]
	EvalRemote ::RDC::Continue
    } else {
	set remoteserver [lreplace $remoteserver 2 2 next]
	EvalRemote next
    }
}

proc RamDebugger::rstep { args } {
    variable debuggerstate
    variable remoteserverType
    variable remoteserver

    if { $debuggerstate == "time" } {
	error "Command rstep cannot be used in 'time' mode. Check rtime"
    }

     set usagestring {usage: rstep ?switches?
	-h:       displays usage
	--:     end of options
    }
    ParseArgs $args $usagestring opts

    rlist -quiet
    StopAtGUI "" ""

    if { $remoteserverType != "gdb" } {
	EvalRemote [list set ::RDC::stopnext 2]
	EvalRemote ::RDC::Continue
    } else {
	set remoteserver [lreplace $remoteserver 2 2 step]
	EvalRemote step
    }
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
    variable remoteserverType
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
		if { [lsearchfile $files $file] == -1 } {
		    rlist -quiet $file
		    lappend files $file
		}
	    }
	}
	set TimeMeasureData $TimeMeasureDataNew
	if { $remoteserverType == "local" && $remoteserver != "" } {
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
		    set TimeMeasureData [lreplace $TimeMeasureData $ipos $ipos]
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
	    if { $opts(lineini) > $lineini && $opts(lineini) <= $lineend && 
		$opts(lineend) > $lineend } { set fail 1 }
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
	if { $a1_le < $a2_le } { return 1 }
	return 0
    }
    lappend TimeMeasureData [list $opts(name) $currentfile $opts(lineini) $opts(lineend) ""]
    set TimeMeasureData [lsort -command RamDebugger::SortTimeMeasureData $TimeMeasureData]
    return "Added time block '$opts(name)'"
}

proc RamDebugger::rlist { args } {
    variable currentfile
    variable currentline
    variable currentfileIsModified
    variable files
    variable filesmtime
    variable fileslist
    variable instrumentedfilesP
    variable instrumentedfilesR
    variable instrumentedfilesTime
    variable instrumentedfilesSent
    variable instrumentedfilesInfo
    variable remoteserver
    variable remoteserverType
    variable debuggerstate
    variable TimeMeasureData
    variable options

    set usagestring {usage: rlist ?switches? ?file? ?line?
	-h:       displays usage
	-quiet: do not print anything
	-force: force to reload file
	-reinstrument: force to reinstrument
	-asmainfile:   When debugging locally, the first file, first time  must be list like this
	--:     end of options
    }
    ParseArgs $args $usagestring opts
    set force $opts(-force)
    set reinstrument $opts(-reinstrument)

    if { ![string is integer $opts(line)] } {
	error "line '$opts(line)' must be a number\n$usagestring"
    }

    set currentfile_save $currentfile
    if { $opts(file) != "" } { set currentfile [filenormalize $opts(file)] }

    if { $currentfile == "" } {
	error "it is necessary to enter a file name\n$usagestring"
    }

    if { [regexp {\.(h|c|cc)$} $currentfile] } {
	set filetype c++
    } else {
	set filetype tcl
    }

    if { $currentfileIsModified && ![info exists instrumentedfilesP($currentfile)] } {
	variable text
	set map [list "\n[string repeat { } 16]" "\n\t\t" "\n[string repeat { } 8]" "\n\t"]
	set files($currentfile) [string map $map [$text get 1.0 end-1c]]

	if { [lsearchfile $fileslist $currentfile] == -1 } {
	    lappend fileslist $currentfile
	}
	if { [info exists instrumentedfilesTime($currentfile)] } {
	    unset instrumentedfilesTime($currentfile)
	}
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
	if { [lsearchfile $fileslist $currentfile] == -1 } {
	    lappend fileslist $currentfile
	}
	set filesmtime($currentfile) [file mtime $currentfile]
    }

    if { ![info exists instrumentedfilesInfo($currentfile)] && !$force && !$reinstrument } {
	set infofile [GiveInstFile $currentfile 1 I]
	if { $infofile != "" } {
	    set fin [open $infofile r]
	    set instrumentedfilesInfo($currentfile) [read $fin]
	    close $fin
	}
    }

    if { $filetype == "tcl" && ![info exists instrumentedfilesP($currentfile)] && !$force && \
	!$reinstrument && !$currentfileIsModified } {
	set filenum [lsearchfile $fileslist $currentfile]

	foreach i [list P R] {
	    set instfile [GiveInstFile $currentfile 1 $i]

	    if { $instfile != "" } {
		set fin [open $instfile r]
		set instrumentedfiles${i}($currentfile) [read $fin]
		
		set oldfilenum 0 ;# for files that do not have any instrum. line
		regexp {RDC::F ([0-9]+)} [set instrumentedfiles${i}($currentfile)] {} oldfilenum
		if { $oldfilenum != $filenum } {
		    set instrumentedfiles${i}($currentfile) \
		    [string map [list "RDC::F $oldfilenum " "RDC::F $filenum "] \
		        [set instrumentedfiles${i}($currentfile)]]
		}
		close $fin
	    }
	}
    }
    if { ($filetype == "tcl" && ![info exists instrumentedfilesP($currentfile)]) || \
	![info exists instrumentedfilesInfo($currentfile)] || $force || $reinstrument } {
	SetMessage "Instrumenting file '$currentfile'..."

	set filenum [lsearchfile $fileslist $currentfile]

	if { $filetype == "c++" } {
	    if { [catch {
		Instrumenter::DoWorkForC++ $files($currentfile) instrumentedfilesInfo($currentfile)
	    } errstring] } {
		RamDebugger::ProgressVar 100
		if { ![string match  "*user demand*" $errstring] } {
		    RamDebugger::TextOutRaise
		    RamDebugger::TextOutInsertRed $::errorInfo
		}
		WarnWin $errstring
	    }
	}

	if { $filetype == "tcl" } {
	    if { [catch {
		Instrumenter::DoWork $files($currentfile) $filenum instrumentedfilesP($currentfile) \
		         instrumentedfilesR($currentfile) instrumentedfilesInfo($currentfile)
	    } errstring] } {
		RamDebugger::ProgressVar 100
		if { [info exists instrumentedfilesP($currentfile)] } {
		    unset instrumentedfilesP($currentfile)
		}
		if { [info exists instrumentedfilesR($currentfile)] } {
		    unset instrumentedfilesR($currentfile)
		}
		if { ![string match  "*user demand*" $errstring] } {
		    RamDebugger::TextOutRaise
		    RamDebugger::TextOutInsertRed $::errorInfo
		}
		WarnWin $errstring
	    }
	}
	
	foreach i [list P R] {
	    set instfile [GiveInstFile $currentfile 0 $i]
	    if { $instfile != "" && [info exists instrumentedfiles${i}($currentfile)] && \
		!$currentfileIsModified } {
		set fout [open $instfile w]
		puts -nonewline $fout [set instrumentedfiles${i}($currentfile)]
		close $fout
	    }
	}
	set infofile [GiveInstFile $currentfile 0 I]
	if { $infofile != "" && [info exists instrumentedfilesInfo($currentfile)] && \
	    !$currentfileIsModified } {
	    set fout [open $infofile w]
	    puts -nonewline $fout $instrumentedfilesInfo($currentfile)
	    close $fout
	}
	if { [info exists instrumentedfilesSent($currentfile)] } {
	    unset instrumentedfilesSent($currentfile)
	}
	SetMessage ""
    }
    if { $debuggerstate == "time" && [info exists files($currentfile)] && \
	    (![info exists instrumentedfilesTime($currentfile)] || $force || $reinstrument) } {
	SetMessage "Instrumenting file '$currentfile' for time measure..."
	    Instrumenter::DoWorkForTime $files($currentfile) $currentfile \
		instrumentedfilesTime($currentfile) $TimeMeasureData
	SetMessage ""
    }

    if { $debuggerstate != "" && $remoteserver != "" && \
	[info exists instrumentedfilesP($currentfile)] &&\
	(![info exists instrumentedfilesSent($currentfile)] || \
	    $instrumentedfilesSent($currentfile) != $debuggerstate || $force) } {
	if { [catch {
	    if { $debuggerstate == "debug" } {
		EvalRemote $instrumentedfilesP($currentfile)
		if { $opts(-asmainfile) } {
		    EvalRemote $instrumentedfilesR($currentfile)
		}
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
    variable instrumentedfilesP
    variable instrumentedfilesR
    variable remoteserverType
    variable breakpoints
    variable debuggerstate

    if { $debuggerstate == "time" } {
	error "Command rbreak cannot be used in 'time' mode. Check rtime"
    }

    set usagestring {usage: rbreak ?switches? ?file? line
	-h:     displays usage
	-quiet: do not print anything
	-force: force to reload file
	--:     end of options

	This command sets a breakpoint in the given line.
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
    
    if { ![regexp {\.(h|c|cc)$} $currentfile] } {
	set ipos [string first "RDC::F $filenum $currentline ;" $instrumentedfilesP($currentfile)]
	if { $ipos == -1 } {
	    set ipos [string first "RDC::F $filenum $currentline ;" $instrumentedfilesR($currentfile)]
	    if { $ipos == -1 } {
		error "error: line $currentline is not instrumented"
	    }
	}
    }
   
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
	--:       end of options

	Display active breakpoints. If line is entered, it refers to the
	currentfile.
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

	Delete one previusly defined breakpoints
    }
    ParseArgs $args $usagestring opts

    if { $opts(-all) } {
	if { $opts(breakpointnum) != "" } {
	    error "when using -all, no breakpointnum must be written\n$usagestring"
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
	incr ipos
    }
    error "breakpoint $opts(breakpointnum) not found"
}

################################################################################
#    Helper basic functions
################################################################################

proc RamDebugger::filenormalize { file } {

    if { $file == "New file" } { return $file }
    
    if { $::tcl_platform(platform) == "windows" } {
	catch { set file [file attributes $file -longname] }
    } else {
	set pwd [pwd]
	catch {
	    cd [file dirname $file]
	    set file [file join [pwd] [file tail $file]]
	}
	cd $pwd
    }
    return $file
}
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
    variable options

    if { $::tcl_platform(platform) == "windows" } {
	if { !$options(CheckRemotes) && !$force } {
	    # dirty trick to make array exist
	    set services(11) ""
	    unset services(11)
	    return
	}
	if { [info exists services] && !$force } { return }
	catch { unset services }

	SetMessage "Searching external programs..."
	ProgressVar 0
	WaitState 1

	if { $debuggerserverNum == "" } {
	    uplevel \#0 package require commR
	    set debuggerserverNum [comm::register RamDebugger 1]
	}
	RamDebugger::ProgressVar 20
	set iprogress 20

	for { set i 12350 } { $i < 12360 } { incr i } {
	    if { $i == $debuggerserverNum } { continue }
	    incr iprogress 40
	    if { $iprogress > 90 } { set iprogress 90 }
	    RamDebugger::ProgressVar $iprogress
	    set comm [list comm::comm send -async $i \
		          [list catch [list comm::givename $debuggerserverNum]]]
	    if { [catch $comm] && $force != 2 } { break }
	}
	# dirty trick to make array exist
	set services(11) ""
	unset services(11)
	WaitState 0
	RamDebugger::ProgressVar 100
	RamDebugger::SetMessage "Searching external programs...done"
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

proc RamDebugger::AreFilesEqual { file1 file2 } {

    if { $::tcl_platform(platform) == "windows" } {
	return [string equal -nocase $file1 $file2]
    } else {
	return [string equal $file1 $file2]
    }
}

proc RamDebugger::lsearchfile { list file } {
    if { $::tcl_platform(platform) == "windows" } {
	return [lsearch -regexp $list (?iq)$file]
    } else {
	return [lsearch $list $file]
    }
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

proc RamDebugger::RecieveErrorFromProgram { err errInfo } {

    TextOutInsertRed "------RECIEVED ERROR FROM DEBUGGED PROGRAM-------------\n"
    TextOutInsertRed $errInfo\n
    TextOutInsertRed "-------------------------------------------------------\n"
    TextOutRaise
    after idle [list WarnWin "Recieved Error from Debugged program:\n$err\nCheck Output for details"]
}

proc RamDebugger::RecieveOutputFromProgram { channelId string hasnewline } {

    if { $hasnewline } { append string \n }
    switch $channelId {
	stdout {
	    TextOutInsert $string
	}
	stderr {
	    TextOutInsertRed $string
	}
    }
    TextOutRaise
    update
}

proc RamDebugger::EvalRemote { comm } {
    variable remoteserver
    variable remoteserverNum
    variable remoteserverType
    variable gdblog

    if { $remoteserver == "" } {
	error "Error: a program to debug must be selected using rdebug"
    }

    if { $remoteserverType == "local" } {
	interp eval local after idle [list $comm]
    } elseif { $remoteserverType == "gdb" } {
	foreach "fid program state" $remoteserver break
	append gdblog $comm\n
	puts $fid $comm
	flush $fid
    } elseif { $::tcl_platform(platform) == "windows" } {
	comm::comm send $remoteserverNum $comm
    } else {
	send $remoteserver $comm
    }
}

proc RamDebugger::GiveInstFile { file onlyifnewer filetype } {
    variable CacheDir

    if { [info exists CacheDir] } {
	regsub -all {(/|:)} [file join [pwd] [file dirname $file]] \# modpath
	set modpath [string trimright $modpath ".\#"]
	if { $filetype == "I" } {
	    set instfile [file join $CacheDir [file tail $file]_$modpath.info]
	} elseif { $filetype == "P" } {
	    set instfile [file join $CacheDir [file tail $file]_$modpath.instrP]
	} else {
	    set instfile [file join $CacheDir [file tail $file]_$modpath.instrR]
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
    variable remoteserver
    variable remoteserverType

    if { $debuggerstate != "debug" } { return }


    if { $remoteserverType == "gdb" } {
	set remoteserver [lreplace $remoteserver 2 2 setbreakpoints]
	EvalRemote "delete"
	foreach i $breakpoints {
	    set line [lindex $i 2]
	    set filenum [lsearchfile $fileslist [lindex $i 1]]
	    if { $filenum == -1 } { continue }
	    set file [file tail [lindex $fileslist $filenum]]
	    if { [regexp {\.(h|c|cc)$} $file] } {
		EvalRemote "break $file:$line"
	    }
	    # CONDITION is forgotten by now
	}
	EvalRemote "printf \"FINISHED SET BREAKPOINTS\\n\""
    } else {
	EvalRemote { if { [info exists RDC::breaks] } { unset RDC::breaks } }
	foreach i $breakpoints {
	    set line [lindex $i 2]
	    set filenum [lsearch $fileslist [lindex $i 1]]
	    if { $filenum == -1 } { continue }
	    EvalRemote [list set RDC::breaks($filenum,$line) [list [lindex $i 0] [lindex $i 3]]]
	}
    }
}

proc RamDebugger::RecieveFromGdb {} {
    variable debuggerstate
    variable remoteserverType
    variable remoteserver
    variable currentfile
    variable gdblog
    variable breakpoints
    variable fileslist
    variable ExpressionResult
    variable options

    foreach "fid program state" $remoteserver break

    if { [eof $fid] } {
	set err [catch { close $fid } errstring]
	set remoteserverType ""
	set remoteserver ""
	set debuggerstate ""
	WarnWin "Program exited ($errstring)"
	return
    }
    set aa [read $fid]
    regsub -all {[ \t]*\(gdb\)[ \t]*} $aa {} aa
    append gdblog $aa

    #if { [string trim $aa] == "" } { return }

    switch -glob -- $state {
	start {
	    if { [string match "*No symbol table is loaded*" $aa] || \
		    [string match "*No such file or directory*" $aa] || \
		    [string match "*No executable file specified*" $aa] } {
		set err [catch { close $fid } errstring]
		set remoteserverType ""
		set remoteserver ""
		set debuggerstate ""
		WarnWin "Program exited ($aa)"
		return
	    } 
	    set remoteserver [lreplace $remoteserver 2 2 ""]
	}
	getdata* {
	    set handler [lindex $state 1]
	    set aa [lindex $state 2]\n$aa
	    if { ![regexp {FINISHED GETDATA\s*$} $aa] } {
		set remoteserver [lreplace $remoteserver 2 2 [list getdata $handler $aa]]
	    } else {
		set remoteserver [lreplace $remoteserver 2 2 ""]
		regexp {(.*)FINISHED GETDATA\s*$} $aa {} block
		uplevel \#0 $handler [list [string trimleft $block \n]]
	    }
	    return
	}
	print* {
	    set handler [lindex $state 1]
	    set remoteserver [lreplace $remoteserver 2 2 ""]
	    
	    if { [regexp {^\$[0-9]+\s+=\s+(.*)} $aa {} res] } {
		set ExpressionResult [list 0 $res]
	    } else {
		set ExpressionResult [list 1 $aa]
	    }
	    if { $handler != "" } {
		uplevel \#0 $handler [list $ExpressionResult]
	    }
	    return
	}
	backtrace* {
	    set aa [lindex $state 1]\n$aa
	    if { ![regexp {FINISHED BACKTRACE\s*$} $aa] } {
		set remoteserver [lreplace $remoteserver 2 2 [list backtrace $aa]]
	    } else {
		set remoteserver [lreplace $remoteserver 2 2 ""]
		regexp {(.*)FINISHED BACKTRACE\s*$} $aa {} block
		set ExpressionResult [list 0 "STACK TRACE\n$block"]
	    }
	    return
	}
	next - step {
	    if { ![regexp {at\s+([^:]+):([0-9]+)} $aa {} file line] } {
		if { [regexp {^\s*([0-9]+)} $aa {} line] } {
		    set file $currentfile
		} else {
		    set line ""
		    #puts ---$aa---
		    #WarnWin "Could not do a '$state'"
		}
	    }
	    if { $line != "" } {
		set remoteserver [lreplace $remoteserver 2 2 ""]
		set filenum [lsearch $fileslist $file]
		if { $filenum == -1 } {
		    set err [catch {OpenFileF $file} errstring]
		    if { $err } {
			WarnWin "Could not open file '$file' for stopping program"
			return
		    }
		    set filenum [lsearch $fileslist $file]
		}
		RecieveFromProgram "" $filenum $line "" "" ""
		return
	    }
	}
	infolocals* {
	    set aa [lindex $state 1]$aa
	    if { ![regexp {FINISHED INFO LOCALS\s*$} $aa] } {
		set remoteserver [lreplace $remoteserver 2 2 [list infolocals $aa]]
	    } else {
		set remoteserver [lreplace $remoteserver 2 2 ""]
		regexp {(.*)FINISHED INFO LOCALS\s*$} $aa {} block
		set list ""
		set line ""
		foreach i [split $block \n] {
		    append line $i
		    if { [info complete $line] } {
		        if { [regexp {^([^=]+)=(.*)} $line {} name value] } {
		            lappend list $name "" $value
		        }
		        set line ""
		    }
		}
		CheckEvalEntriesL res "" [list 0 $list]
	    }
	    return
	}
	multipleprint* {
	    set aa [lindex $state 1]$aa
	    if { ![regexp {FINISHED MULTIPLEPRINT\s*$} $aa] } {
		set remoteserver [lreplace $remoteserver 2 2 [list multipleprint $aa]]
	    } else {
		set remoteserver [lreplace $remoteserver 2 2 ""]
		regexp {(.*)FINISHED MULTIPLEPRINT\s*$} $aa {} block
		set list ""
		set line ""
		foreach i [split $block \n] {
		    if { [string trim $i] == "" } { continue }
		    append line $i
		    if { [info complete $line] } {
		        if { [regexp {^\s*\$[0-9]+\s*=\s+(.*)} $line {} rest] } {
		            lappend list "" $rest
		        } else {
		            lappend list error $line
		        }
		        set line ""
		    }
		}
		CheckEvalEntries res "" [list 0 $list]
	    }
	    return
	}
	setbreakpoints {
	    if { [regexp {FINISHED SET BREAKPOINTS\s*$} $aa] } {
		set remoteserver [lreplace $remoteserver 2 2 ""]
	    }
	    return
	}
    }

    if { [regexp {Breakpoint\s[0-9]+,\s+(\S+\s+\([^\)]*\))\s+at\s+([^:]+):([0-9]+)} \
	    $aa {} procname file line] } {

	if { [file pathtype $file] == "relative" } {
	    set executable [lindex $options(debugcplusplus) 0]
	    set dir [file dirname $executable]
	    if { [file exists [file join $dir $file]] } {
		set file [file join $dir $file]
	    }
	    if { [info exists cproject::project] && \
		    [file exists [file join [file dirname $cproject::project] $file]] } {
		set file [file join [file dirname $cproject::project] $file]
	    }
	}
	if { [file pathtype $file] == "volumerelative" } {
	    set drive [string trim [lindex [file split [pwd]] 0] /]
	    set file $drive$file
	}
	set file [filenormalize $file]
	set found 0
	foreach i $breakpoints {
	    set breaknum [lindex $i 0]
	    set line_in [lindex $i 2]
	    set file_in [lindex $i 1]
	    if { $line == $line_in && [AreFilesEqual $file $file_in] } {
		set found 1
		break
	    }
	    # CONDITION is forgotten by now
	}
	if { $found } {
	    set filenum [lsearch $fileslist $file]
	    if { $filenum == -1 } {
		set err [catch {OpenFileF $file} errstring]
		if { $err } {
		    WarnWin "Could not open file '$file' for stopping program"
		    return
		}
		set filenum [lsearch $fileslist $file]
	    }
	    RecieveFromProgram $breaknum $filenum $line $procname "" ""
	    return
	} else {
	    WarnWin "Problems finding breakpoint in file $file ($aa)"
	}
	return
    }

    if { [regexp {Program exited[^\n]*} $aa mess] } {
	set err [catch { close $fid } errstring]
	set debuggerstate ""
	set remoteserverType ""
	set remoteserver ""
	if { $err } {
	    append mess " ($errstring)"
	}
	WarnWin $mess
    }
    TextOutInsert $aa
    TextOutRaise
}

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
    variable OutputType
    variable NeedsNamespaceClose
    variable braceslevel

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
    variable OutputType
    variable NeedsNamespaceClose 0
    variable braceslevel 0
    variable level 0
    variable colors

    foreach i [list return break while eval foreach for if else elseif error switch default \
	    continue] {
	set colors($i) magenta
    }
    foreach i [list variable set global] {
	set colors($i) green
    }
}

proc RamDebugger::Instrumenter::PushState { type line newblocknameP newblocknameR } {
    variable stack
    variable words
    variable currentword
    variable wordtype
    variable wordtypeline
    variable wordtypepos
    variable DoInstrument
    variable OutputType
    variable NeedsNamespaceClose
    variable braceslevel
    variable level

    set NewDoInstrument 0
    if { $OutputType == "P" } {
	set NewOutputType PP
    } else { set NewOutputType $OutputType }

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
#                 if { $OutputType == "R" } {
#                     upvar 2 $newblocknameP newblock
#                 } else { upvar 2 $newblocknameR newblock }
#                 append newblock "namespace eval [lindex $words 2] \{\n"
#                 set NeedsNamespaceClose 1
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
		        if { $OutputType == "R" } {
		            upvar 2 $newblocknameP newblockP
		            append newblockP "namespace eval [lindex $words 2] \{\n"
		            set NeedsNamespaceClose 1
		        }
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
	$wordtypepos $DoInstrument $OutputType $NeedsNamespaceClose $braceslevel $line $type]

    set words ""
    set currentword ""
    set wordtype ""
    set wordtypeline ""
    set wordtypepos ""
    set DoInstrument $NewDoInstrument
    set OutputType $NewOutputType
    set NeedsNamespaceClose 0
    set braceslevel 0
    return 0
}

proc RamDebugger::Instrumenter::PopState { type line newblocknameP newblocknameR } {
    variable stack
    variable wordtype
    variable words
    variable currentword
    variable wordtype
    variable wordtypeline
    variable wordtypepos
    variable DoInstrument
    variable OutputType
    variable NeedsNamespaceClose
    variable braceslevel
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
    foreach [list words currentword wordtype wordtypeline wordtypepos DoInstrument OutputType \
	NeedsNamespaceClose braceslevel] [lindex $stack end] break
    set stack [lreplace $stack end end]
    incr level -1

    if { $NeedsNamespaceClose } {
	upvar 2 $newblocknameP newblockP
	append newblockP "\}\n"
	set NeedsNamespaceClose 0
    }

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

# newblocknameP is for procs
# newblocknameR is for the rest
proc RamDebugger::Instrumenter::DoWork { block filenum newblocknameP newblocknameR blockinfoname } {

    variable words
    variable currentword
    variable wordtype
    variable wordtypeline
    variable DoInstrument
    variable OutputType
    variable braceslevel
    variable level
    variable colors

    set length [string length $block]
    if { $length >= 1000 } {
	RamDebugger::ProgressVar 0 1
    }

    # a trick: it is the same to use newblockP or newblockPP. Only convenience
    upvar $newblocknameP newblockP
    upvar $newblocknameP newblockPP
    upvar $newblocknameR newblockR
    upvar $blockinfoname blockinfo
    set newblockP ""
    set newblockR ""
    set blockinfo ""
    set blockinfocurrent [list 0 n]
    InitState

    set DoInstrument 1
    set OutputType R

    append newblockP "# RamDebugger instrumented file. InstrumentProcs=1\n"
    append newblockR "# RamDebugger instrumented file. InstrumentProcs=0\n"

    set braceslevelNoEval 0
    set checkExtraCharsAfterCQB ""
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
	    if { ![string is space $c] && $c != "\}" && $c != "\]" && $c != "\\" && $c != ";" } {
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
	if { $DoInstrument == 1 && $lastinstrumentedline != $line && \
	    ![string is space $c] && \
	     $c != "\#" && $words == "" } {
	    append newblock$OutputType "RDC::F $filenum $line ; "
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
		        set wordtypeline 0

		        if { $OutputType == "R" && $words == "proc" } {
		            if { $lastinstrumentedline == $line } {
		                set numdel [expr 4+[string length "RDC::F $filenum $line ; "]]
		            } else { set numdel 4 }
		            set newblockR [string range $newblockR 0 end-$numdel]
		            append newblockP $words
		            set OutputType P
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
		        incr braceslevelNoEval
		    } elseif { $wordtype == "\"" || $wordtype == "w" } {
		        incr braceslevel
		    } else {
		        set consumed 1
		        set fail [PushState \{ $line $newblocknameP $newblocknameR]
		        if { $fail } {
		            set wordtype \{
		            set wordtypeline $line
		            set braceslevelNoEval 1
		        } else {
		            set lastinstrumentedline $line
		        }
		    }
		}
	    }
	    \} {
		if { $lastc != "\\" } {
		    if { $wordtype == "\{" } {
		        incr braceslevelNoEval -1
		        if { $braceslevelNoEval == 0 } {
		            set wordtype ""
		            lappend words $currentword
		            set currentword ""
		            set consumed 1
		            if { [lindex $words 0] != "\#" } {
		                set checkExtraCharsAfterCQB \}
		            }
		            if { $OutputType == "R" && $words == "proc" } {
		                if { $lastinstrumentedline == $line } {
		                    set numdel [expr 4+[string length "RDC::F $filenum $line ; "]]
		                } else { set numdel 4 }
		                set newblockR [string range $newblockR 0 end-$numdel]
		                append newblockP $words
		                set OutputType P
		            }
		        }
		    } elseif { $braceslevel > 0 } {
		        incr braceslevel -1
		    } else {
		        set wordtype_before $wordtype
		        set fail [PopState \} $line $newblocknameP $newblocknameR]
		        if { !$fail } {
		            if { $wordtype_before == "\"" } {
		                set text "Quoted text (\") in line $line "
		                append text "contains and invalid brace (\})"
		                error $text
		            }
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

		        if { $OutputType == "R" && $words == "proc" } {
		            if { $lastinstrumentedline == $line } {
		                set numdel [expr 4+[string length "RDC::F $filenum $line ; "]]
		            } else { set numdel 4 }
		            set newblockR [string range $newblockR 0 end-$numdel]
		            append newblockP $words
		            set OutputType P
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
		    PushState \[ $line $newblocknameP $newblocknameR
		    set lastinstrumentedline $line
		}
	    }
	    \] {
		if { $lastc != "\\" && $wordtype != "\"" && $wordtype != "\{" } {
		    set fail [PopState \] $line $newblocknameP $newblocknameR]
		    if { !$fail } {
		        set consumed 1
		    }
		    # note: the word inside words is not correct when using []
		}
	    }
	    \n {
		if { [lindex $words 0] == "\#" } {
		    lappend blockinfocurrent red $commentpos $icharline
		} elseif { $wordtype == "\"" } {
		    if { $wordtypeline == $line } {
		        lappend blockinfocurrent grey $wordtypepos $icharline
		    } else {
		        lappend blockinfocurrent grey 0 $icharline
		    }
		} elseif { $wordtype == "w" && [info exists colors($currentword)] } {
		    set icharlineold [expr $icharline-[string length $currentword]]
		    lappend blockinfocurrent $colors($currentword) $icharlineold \
		       $icharline
		}
		lappend blockinfo $blockinfocurrent
		incr line
		set blockinfocurrent [expr $level+$braceslevelNoEval]

		if { ($wordtype == "w" || $wordtype == "\"") && $braceslevel > 0 } {
		    lappend blockinfocurrent "n"
		} elseif { $wordtype != "\{" } {
		    set consumed 1
		    if { $lastc != "\\" && $wordtype != "\"" } {
#                         if { $wordtype == "\"" } {
#                             set text "Quotes (\") in line [expr $line-1] "
#                             append text "are not closed"
#                             error $text
#                         }
		        set words ""
		        set currentword ""
		        set wordtype ""

		        if { $OutputType == "P" } {
		            append newblockP $c
		            set OutputType R
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
		    set commentpos $icharline
		}
	    }
	    ; {
		if { $lastc != "\\" && $wordtype != "\"" && $wordtype != "\{"  && \
		        [lindex $words 0] != "\#"} {
		    set consumed 1
		    set words ""
		    set currentword ""
		    set wordtype ""

		    if { $OutputType == "P" } {
		        append newblockP $c
		        set OutputType R
		    }
		}
	    }
	}
	append newblock$OutputType $c
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
    lappend blockinfo $blockinfocurrent

    if { $wordtype != "" && $wordtype != "w" } {
	set text "There is a block of type ($wordtype) beginning at line $wordtypeline "
	append text "that is not closed at the end of the file"
	error $text
    }
    CheckEndOfFileState

    if { $length >= 1000 } {
	RamDebugger::ProgressVar 100
    }
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


proc RamDebugger::Instrumenter::DoWorkForC++ { block blockinfoname { braceslevelIni 0 } } {

    set length [string length $block]
    if { $length >= 5000 } {
	RamDebugger::ProgressVar 0 1
    }

    upvar $blockinfoname blockinfo
    set blockinfo ""
    set blockinfocurrent [list $braceslevelIni n]


    foreach i [list \#include static const if else new delete for return sizeof while continue \
	    break class typedef struct \#else \#endif \#if] {
	set colors($i) magenta
    }
    foreach i [list \#ifdef \#ifndef \#define \#undef] {
	set colors($i) magenta2
    }
    foreach i [list char int double void ] {
	set colors($i) green
    }

    set wordtype ""
    set wordtypeline ""
    # = -1 -> // ; > 0 -> comment type /*
    set commentlevel 0
    set braceslevel $braceslevelIni
    set braceshistory ""
    set lastc ""
    set line 1
    set ichar 0
    set icharline 0
    set finishedline 0
    set nextiscyan 0
    set simplechar ""
    foreach c [split $block ""] {

	if { $ichar%5000 == 0 } {
	    RamDebugger::ProgressVar [expr $ichar*100/$length]
	}

	if { $simplechar != "" } {
	    foreach "iline icharinto" $simplechar break
	    if { $line > $iline } {
		error "error in line $iline, position $icharinto. There is no closing (')"
	    }
	    if { $c == "'" } {
		set simplechar ""
	    }
	    set lastc $c 
	    incr ichar
	
	    if { $c == "\t" } {
		incr icharline 8
	    } elseif { $c != "\n" } {
		incr icharline
	    } else { set icharline 0 }
	    continue
	}

	switch -- $c {
	    \" {
		if { $commentlevel } {
		    # nothing
		} elseif { $wordtype != "\"" } {
		    set wordtype \"
		    set wordtypeline $line
		    set wordtypepos $icharline
		    set finishedline 0
		} elseif { $lastc != "\\" } {
		    set wordtype ""
		    lappend blockinfocurrent grey $wordtypepos [expr $icharline+1]
		}
	    }
	    ' {
		if { !$commentlevel && $wordtype != "\"" } {
		    set simplechar [list $line $icharline]
		}
	    }
	    \{ {
		if { $commentlevel || $wordtype == "\"" } {
		    #nothing
		} else {
		    if { $wordtype == "w" } {
		        if { [info exists colors($currentword)] } {
		            lappend blockinfocurrent $colors($currentword) $wordtypepos \
		               $icharline
		            if { $colors($currentword) == "green" } {
		                set nextiscyan 1
		            }
		        } elseif { $nextiscyan } {
		            lappend blockinfocurrent cyan $wordtypepos \
		            $icharline
		            set nextiscyan 0
		        }
		        set wordtype ""
		    }
		    incr braceslevel
		    lappend braceshistory [list o $braceslevel $line $icharline]
		    set finishedline 1
		}
	    }
	    \} {
		if { $commentlevel || $wordtype == "\"" } {
		    #nothing
		} else {
		    if { $wordtype == "w" } {
		        if { [info exists colors($currentword)] } {
		            lappend blockinfocurrent $colors($currentword) $wordtypepos \
		            $icharline
		            if { $colors($currentword) == "green" || \
		                    $colors($currentword) == "magenta2" } {
		                set nextiscyan 1
		            }
		        } elseif { $nextiscyan } {
		            lappend blockinfocurrent cyan $wordtypepos \
		            $icharline
		            set nextiscyan 0
		        }
		        set wordtype ""
		    }
		    incr braceslevel -1
		    lappend braceshistory [list c $braceslevel $line $icharline]
		    if { $braceslevel < 0 } {
		        if { $braceslevelIni == 0 } {
		            RamDebugger::TextOutClear
		            RamDebugger::TextOutRaise
		            RamDebugger::TextOutInsert "BRACES POSITIONS\n"
		            set file $RamDebugger::currentfile
		            foreach i $braceshistory {
		                switch [lindex $i 0] {
		                    o { set tt "open brace" }
		                    c { set tt "close brace" }
		                }
		                set data "$file:[lindex $i 2] $tt pos=[lindex $i 3] "
		                append data "Level after=[lindex $i 1]\n"
		                RamDebugger::TextOutInsert $data
		            }
		        }
		        error "error in line $line. There is one unmatched closing brace (\})"
		    }
		    set finishedline 1
		}
	    }
	    * {
		if { $commentlevel == -1  || $wordtype == "\"" } {
		    #nothing
		} elseif { $lastc == "/" } {
		    if { $commentlevel == 0 } {
		        set wordtype ""
		        set wordtypepos [expr $icharline-1]
		    }
		    incr commentlevel
		} elseif { !$commentlevel && $wordtype == "w" } {
		    if { [info exists colors($currentword)] } {
		        lappend blockinfocurrent $colors($currentword) $wordtypepos \
		        $icharline
		        if { $colors($currentword) == "green" || \
		                    $colors($currentword) == "magenta2" } {
		            set nextiscyan 1
		        }
		    } elseif { $nextiscyan } {
		        lappend blockinfocurrent cyan $wordtypepos \
		        $icharline
		        set nextiscyan 0
		    }
		    set wordtype ""
		}
	    }
	    / {
		if { $commentlevel == -1  || $wordtype == "\"" } {
		    #nothing
		} elseif { !$commentlevel && $lastc == "/" } {
		    set wordtype ""
		    set wordtypepos [expr $icharline-1]
		    set commentlevel -1
		} elseif { $lastc == "*" } {
		    set wordtype ""
		    if { $commentlevel >= 1 } {
		        incr commentlevel -1
		        if { $commentlevel == 0 } {
		            lappend blockinfocurrent red $wordtypepos [expr $icharline+1]
		        }
		    } 
		} elseif { !$commentlevel && $wordtype == "w" } {
		    if { [info exists colors($currentword)] } {
		            lappend blockinfocurrent $colors($currentword) $wordtypepos \
		        $icharline
		        if { $colors($currentword) == "green" || \
		                $colors($currentword) == "magenta2" } {
		            set nextiscyan 1
		        }
		    } elseif { $nextiscyan } {
		        lappend blockinfocurrent cyan $wordtypepos \
		        $icharline
		        set nextiscyan 0
		    }
		    set wordtype ""
		}
	    }
	    \( {
		if { !$commentlevel && $braceslevel == 0 && $wordtype != "\"" } {
		    set ipos [string first :: $currentword]
		    if { $ipos == -1 } {
		        lappend blockinfocurrent blue $wordtypepos $icharline
		    } else {
		        lappend blockinfocurrent green $wordtypepos [expr $wordtypepos+$ipos+2]
		        lappend blockinfocurrent blue [expr $wordtypepos+$ipos+2] $icharline
		    }
		    set nextiscyan 0
		} elseif { $wordtype == "w" } {
		    if { [info exists colors($currentword)] } {
		        lappend blockinfocurrent $colors($currentword) $wordtypepos \
		        $icharline
		        if { $colors($currentword) == "green"  || \
		                $colors($currentword) == "magenta2" } {
		            set nextiscyan 1
		        }
		    } elseif { $nextiscyan } {
		        lappend blockinfocurrent cyan $wordtypepos \
		        $icharline
		        set nextiscyan 0
		    }
		    set wordtype ""
		}
	    }
	    ";" {
		if { !$commentlevel && $wordtype == "w" } {
		    if { [info exists colors($currentword)] } {
		        lappend blockinfocurrent $colors($currentword) $wordtypepos \
		           $icharline
		        if { $colors($currentword) == "green" || \
		                $colors($currentword) == "magenta2" } {
		            set nextiscyan 1
		        }
		    } elseif { $nextiscyan } {
		        lappend blockinfocurrent cyan $wordtypepos \
		        $icharline
		        set nextiscyan 0
		    }
		    set wordtype ""
		}
		if { !$commentlevel && $wordtype != "\"" } {
		    set finishedline 1
		}
	    }
	    \n {
		if { $wordtype == "\"" } {
		    lappend blockinfocurrent grey $wordtypepos $icharline
		    set wordtypepos 0
		} elseif { $wordtype == "w" } {
		    if { [info exists colors($currentword)] } {
		        set icharlineold [expr $icharline-[string length $currentword]]
		        lappend blockinfocurrent $colors($currentword) $icharlineold \
		           $icharline
		        if { $colors($currentword) == "green" || \
		                    $colors($currentword) == "magenta2" } {
		            set nextiscyan 1
		        }
		    } elseif { $nextiscyan } {
		        lappend blockinfocurrent cyan $wordtypepos \
		           $icharline
		        set nextiscyan 0
		    }
		    set wordtype ""
		} elseif { $commentlevel } {
		    lappend blockinfocurrent red $wordtypepos $icharline
		    set wordtypepos 0
		    if { $commentlevel == -1 } { set commentlevel 0 }
		    set finishedline 1
		}
		lappend blockinfo $blockinfocurrent
		incr line
		set blockinfocurrent [expr $braceslevel]

		if { $finishedline } {
		    lappend blockinfocurrent "n"
		} else { lappend blockinfocurrent "c" }
	    }
	    default {
		if { $commentlevel || $wordtype == "\"" } {
		    # nothing
		} elseif { $wordtype == "" } {
		    if { [string is wordchar $c] || $c == "\#" || $c == ":" || $c == "," } {
		        set wordtype w
		        set wordtypepos $icharline
		        set currentword $c
		        set finishedline 0
		    }
		} elseif { $wordtype == "w" } {
		    if { [string is wordchar $c] || $c == "\#" || $c == ":" || $c == "," } {
		        append currentword $c
		    } else {
		        if { [info exists colors($currentword)] } {
		            lappend blockinfocurrent $colors($currentword) $wordtypepos \
		               $icharline
		            if { $colors($currentword) == "green" || \
		                    $colors($currentword) == "magenta2" } {
		                set nextiscyan 1
		            }
		        } elseif { $nextiscyan } {
		            lappend blockinfocurrent cyan $wordtypepos \
		               $icharline
		            set nextiscyan 0
		        }
		        set wordtype ""
		    }
		}
	    }
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
    lappend blockinfo $blockinfocurrent

    if { $wordtype != "" && $wordtype != "w" } {
	set text "There is a block of type ($wordtype) beginning at line $wordtypeline "
	append text "that is not closed at the end of the file"
	error $text
    }
    if { $commentlevel > 0 } {
	error "error: There is a non-closed comment beginning at line $wordtypeline"
    }
    if { $braceslevel } {
	if { $braceslevelIni == 0 } {
	    RamDebugger::TextOutClear
	    RamDebugger::TextOutRaise
	    RamDebugger::TextOutInsert "BRACES POSITIONS\n"
	    set file $RamDebugger::currentfile
	    foreach i $braceshistory {
		switch [lindex $i 0] {
		    o { set tt "open brace" }
		    c { set tt "close brace" }
		}
		set data "$file:[lindex $i 2] $tt pos=[lindex $i 3] Level after=[lindex $i 1]\n"
		RamDebugger::TextOutInsert $data
	    }
	}
	error "error: There is a non-closed brace at the end of the file (see Output for details)"
    }
    if { $length >= 1000 } {
	RamDebugger::ProgressVar 100
    }
}

################################################################################
#                   RamDebugger GUI
################################################################################


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

proc RamDebugger::ViewOnlyTextOrAll {} {
    variable mainframe
    variable text
    variable pane2in1
    variable options

    set f [$mainframe getframe]

    if { [lindex [grid info $f.fulltext] 1] != $f } {
	foreach i [winfo children $f] {
	    grid remove $i
	}
	grid $f.fulltext -in $f -sticky nsew
	grid rowconf $f 0 -weight 1
	grid columnconf $f 0 -weight 1

	set options(ViewOnlyTextOrAll) OnlyText
    } else {
	foreach i [winfo children $f] {
	    grid $i
	}
	grid $f.fulltext -in $pane2in1
	set options(ViewOnlyTextOrAll) All
    }


}

# orient must be: h or v
proc RamDebugger::ManagePanes { panedw orient default } {
    variable options
    variable optionsinitial

    set optionsinitial($orient,$panedw) $default

    if { [info exists options(paneweights,$orient,$panedw)] } {
	return $options(paneweights,$orient,$panedw)
    } else {
	return [set options(paneweights,$orient,$panedw) $default]
    }
}

proc RamDebugger::ExitGUI {} {
    variable options
    variable text
    variable remoteserver
    variable remoteserverType
    variable EvalEntries
    variable currentfile
    variable currentline
    variable breakpoints

    if { [SaveFile ask] == -1 } { return }

    set options(NormalFont) [font configure NormalFont]
    set options(FixedFont) [font configure FixedFont]
    set options(HelpFont) [font configure HelpFont]

    set options(watchedvars) ""
    set i 0
    while 1 {
	if { ![info exists EvalEntries($i,left)] } { break }
	lappend options(watchedvars) $EvalEntries($i,left)
	incr i
    }
    foreach i [array names options paneweights,*] {
	regexp {paneweights,(.*),(.*)} $i {} orient panedw
	set options($i) ""
	if { [winfo exists $panedw] } {
	    set idx 0
	    while { [set pane [$panedw getframe $idx]] != "" } {
		switch $orient {
		    h { lappend options($i) [winfo width $pane] }
		    v { lappend options($i) [winfo height $pane] }
		}
		incr idx
	    }
	} else { unset options($i) }
    }
    set options(currentfile) $currentfile
    set options(currentidx) [$text index insert]

    set options(breakpoints) ""
    foreach i $breakpoints {
	if { [lindex $i 1] != "New file" } {
	    lappend options(breakpoints) $i
	}
    }

    set options(remoteserverType) $remoteserverType
    set options(remoteserver) $remoteserver
    if { [wm state [winfo toplevel $text]] == "zoomed" } {
	set options(maingeometry) zoomed
    } else {
	set options(maingeometry) [wm geometry [winfo toplevel $text]]
    }
    if { $::tcl_platform(platform) == "windows" } {
	registry set {HKEY_CURRENT_USER\Software\RamDebugger} IniData [array get options]
    } else {
	set fout [open ~/.ramdebugger w]
	puts -nonewline $fout [array get options]
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

    set ed [$text cget -editable]
    $text conf -editable 1
    $text tag conf magenta -foreground magenta
    $text tag conf magenta2 -foreground magenta2
    $text tag conf blue -foreground blue
    $text tag conf grey -foreground grey40
    $text tag conf green -foreground green
    $text tag conf red -foreground red
    $text tag conf cyan -foreground \#b8860b

    set textO [$text original]
    set iline 1
    foreach i $instrumentedfilesInfo($currentfile) {
	foreach "tag li le" [lrange $i 2 end] {
	    $textO tag add $tag $iline.$li $iline.$le
	}
	incr iline
    }
    $text tag raise sel
    $text conf -editable $ed
}

proc RamDebugger::ColorizeLines { l1 l2 } {
    variable text
    variable instrumentedfilesInfo
    variable currentfile

    set ed [$text cget -editable]
    $text conf -editable 1
    $text tag conf magenta -foreground magenta
    $text tag conf blue -foreground blue
    $text tag conf grey -foreground grey
    $text tag conf green -foreground green
    $text tag conf red -foreground red

    set textO [$text original]
    foreach i [list magenta blue grey green red] {
	$textO tag remove $i $l1.0 "$l2.0 lineend"
    }

    for { set i $l1 } { $i <= $l2 } { incr i } {
	foreach "tag li le" [lrange [lindex $instrumentedfilesInfo($currentfile) [expr $i-1]] 2 end] {
	    $textO tag add $tag $i.$li $i.$le
	}
    }
    $text tag raise sel
    $text conf -editable $ed
}

proc RamDebugger::ColorizeSlow {} {
    variable text

    set ed [$text cget -editable]
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
    set comments {\#.*$}

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
    $text conf -editable $ed
}

proc RamDebugger::SaveFile { what } {
    variable text
    variable options
    variable currentfile
    variable currentfileIsModified
    variable filesmtime

    if { $what == "ask" } {
	if { !$currentfileIsModified } { return 0 }
	set ret [DialogWin::messageBox -default yes -icon question -message \
	    "Do you want to save file '$currentfile'?" -parent $text \
	    -title "save file" -type yesnocancel]
	if { $ret == "cancel" } { return -1 }
	if { $ret == "no" } {
	    set currentfileIsModified 0
	    return 0
	}
    }

    if { $what == "saveas" || $currentfile == "New file" || $currentfile == "" } {
	set w [winfo toplevel $text]
	set types {
	    {{TCL Scripts}      {.tcl}        }
	    {{All Files}        *             }
	}
	if { ![info exists options(defaultdir)] } { set options(defaultdir) [pwd] }
	set file [tk_getSaveFile -filetypes $types -initialdir $options(defaultdir) -parent $w \
	    -title "Savefile"]
	if { $file == "" } { return }
	set options(defaultdir) [file dirname $file]
    } else {
	set file $currentfile

	if { [file mtime $file] > $filesmtime($file) } {
	    set ret [DialogWin::messageBox -default yes -icon question -message \
		"File '$currentfile' has been modified outside RamDebugger. Loose external changes?" \
		-parent $text -title "Warning" -type yescancel]
	    if { $ret == "cancel" } { return -1 }
	}
    }
    SaveFileF $file
    return 0
}

proc RamDebugger::OpenFile {} {
    variable options
    variable text

    set w [winfo toplevel $text]

    if { [SaveFile ask] == -1 } { return }

    set types {
	{{TCL Scripts}      {.tcl}        }
	{{C,C++ files}      {.cc .c .h}   }
	{{All Files}        *             }
    }
    if { ![info exists options(defaultdir)] } { set options(defaultdir) [pwd] }
    set file [tk_getOpenFile -filetypes $types -initialdir $options(defaultdir) -parent $w \
		  -title "Open source file"]
    if { $file == "" } { return }
    OpenFileF $file -1
}

proc RamDebugger::OpenFileF { file { force 0 } } {
    variable marker
    variable text
    variable files
    variable breakpoints
    variable currentfile
    variable currentfileIsModified
    variable WindowFilesList
    variable WindowFilesListCurr
    variable options

    if { $force == -1 } {
	set force 0
    } else {
	if { [SaveFile ask] == -1 } { return }
    }

    WaitState 1

    if { !$force } {
	set comm [list rlist -quiet $file {}]
    } elseif { $force == 2 } { 
	set comm [list rlist -quiet -reinstrument $file {}]
    } else {
	set comm [list rlist -quiet -force $file {}]
    }
    if { [catch $comm errstring] } {
	WaitState 0
	WarnWin [lindex [split $errstring \n] 0]
	return 1
    }
    if { $file == $currentfile } {
	set idx [$text index insert]
    } else { set idx 1.0 }

    $marker delete arrow
    $marker delete break
    $marker delete arrowbreak
    set ed [$text cget -editable]
    $text conf -editable 1
    $text clearundo
    set textO [$text original]
    $textO del 1.0 end
    $textO ins end [string map [list "\t" "        "] $files($file)]
    $textO tag add normal 1.0 end
    $text conf -editable $ed

    Colorize

    foreach i $breakpoints {
	if { ![AreFilesEqual [lindex $i 1]  $file] } { continue }
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
    set currentfileIsModified 0

    if { [lsearch $WindowFilesList $file] != -1 } {
	set WindowFilesListCurr [lsearch $WindowFilesList $file]
    } else {
	incr WindowFilesListCurr
	if { $WindowFilesListCurr == [llength $WindowFilesList] } {
	    lappend WindowFilesList $file
	} else {
	    set WindowFilesList [lreplace $WindowFilesList $WindowFilesListCurr end $file]
	}
    }
    $text conf -editable 1

    if { ![info exists options(RecentFiles)] } {
	set options(RecentFiles) ""
    }
    set ipos [lsearchfile $options(RecentFiles) $file]
    if { $ipos != -1 } {
	set options(RecentFiles) [lreplace $options(RecentFiles) $ipos $ipos]
    }
    set options(RecentFiles) [linsert $options(RecentFiles) 0 $file]
    if { [llength $options(RecentFiles)] > 6 } {
	set options(RecentFiles) [lreplace $options(RecentFiles) 5 end]
    }
    set options(defaultdir) [file dirname $file]
    FillListBox

    WaitState 0
    return 0
}

proc RamDebugger::NewFile {} {
    variable marker
    variable text
    variable files
    variable instrumentedfilesInfo
    variable breakpoints
    variable currentfile
    variable currentfileIsModified
    variable WindowFilesList
    variable WindowFilesListCurr

    if { [SaveFile ask] == -1 } { return }

    WaitState 1

    set currentfile "New file"
    set currentfileIsModified 0

    $marker delete arrow
    $marker delete break
    $marker delete arrowbreak
    set ed [$text cget -editable]
    $text conf -editable 1
    $text clearundo
    set textO [$text original]
    $textO del 1.0 end
    $textO tag add normal 1.0 end
    $text conf -editable $ed

    FillListBox

    set Numlines [scan [$text index end-1c] %d]
    set font [$text cget -font]
    $marker configure -scrollregion [list 0 0 [winfo reqwidth $marker] \
		                         [expr $Numlines*[font metrics $font -linespace]]]
    $text mark set insert 1.0
    $text see 1.0
    set instrumentedfilesInfo($currentfile) ""

    wm title [winfo toplevel $text] "RamDebugger     [file tail $currentfile]"

    $text conf -editable 1
    WaitState 0
    return 0
}

proc RamDebugger::SaveFileF { file } {
    variable text
    variable currentfile
    variable currentfileIsModified
    variable files
    variable instrumentedfilesP
    variable instrumentedfilesR
    variable instrumentedfilesTime
    variable instrumentedfilesSent
    variable filesmtime

    WaitState 1
    SetMessage "Saving file '$file'..."

    set map [list "\n[string repeat { } 16]" "\n\t\t" "\n[string repeat { } 8]" "\n\t"]
    set files($file) [string map $map [$text get 1.0 end-1c]]
    set err [catch { open $file w } fout]
    if { $err } {
	WaitState 0
	SetMessage ""
	WarnWin "Error saving file '$file'" $text
	return
    }
    puts -nonewline $fout $files($file)
    close $fout

    set currentfile $file

    catch { unset instrumentedfilesP($currentfile) instrumentedfilesR($currentfile) }
    catch { unset instrumentedfilesTime($currentfile) }
    catch { unset instrumentedfilesSent($currentfile) }
    
    wm title [winfo toplevel $text] [string trimright [wm title [winfo toplevel $text]] *]
    set currentfileIsModified 0
    set filesmtime($currentfile) [file mtime $file]

    WaitState 0
    SetMessage "Saved file '$file'"
}

proc RamDebugger::ViewInstrumentedFile { what } {
    variable marker
    variable text
    variable currentfile
    variable instrumentedfilesP
    variable instrumentedfilesR
    variable instrumentedfilesInfo
    variable gdblog

    if { [SaveFile ask] == -1 } { return }

    if { $currentfile == "" && $what != "gdb" } {
	WarnWin "There is no file to see its instrumented file"
	return
    }
    if { $what == "instrumentedP" } {
	if { ![info exists instrumentedfilesP($currentfile)] } {
	    WarnWin "There is no instrumented file P for file '$currentfile'"
	    return
	}
    } elseif { $what == "instrumentedR" } {
	if { ![info exists instrumentedfilesR($currentfile)] } {
	    WarnWin "There is no instrumented file R for file '$currentfile'"
	    return
	}
    } elseif { $what == "gdb" } {
	if { $gdblog == "" } {
	    WarnWin "There is no GDB log file. Use Files->Debug on->Debug c++ to obtain it"
	    return
	}
    } else {
	if { ![info exists instrumentedfilesInfo($currentfile)] } {
	    WarnWin "There is no instrumented info file for file '$currentfile'"
	    return
	}
    }
    WaitState 1

    set ed [$text cget -editable]
    $text conf -editable 1
    $text clearundo
    set textO [$text original]
    $textO del 1.0 end
    if { $what == "instrumentedP" } {
	wm title [winfo toplevel $text] "RamDebugger      [file tail $currentfile] instrumented P"
	$textO ins end [string map [list "\t" "        "] $instrumentedfilesP($currentfile)]
    } elseif { $what == "instrumentedR" } {
	wm title [winfo toplevel $text] "RamDebugger      [file tail $currentfile] instrumented R"
	$textO ins end [string map [list "\t" "        "] $instrumentedfilesR($currentfile)]
    } elseif { $what == "gdb" } {
	wm title [winfo toplevel $text] "RamDebugger      GDB log info"
	$textO ins end $gdblog
    } else {
	wm title [winfo toplevel $text] "RamDebugger      [file tail $currentfile] instrumented info"
	foreach i $instrumentedfilesInfo($currentfile) {
	    $textO ins end [string map [list "\t" "        "] $i\n]
	}
    }
    $textO tag add normal 1.0 end
    $text conf -editable $ed
    ColorizeSlow
    WaitState 0

    $marker delete arrow
    $marker delete break
    $marker delete arrowbreak

    $text conf -editable 0
    set currentfile ""
}

proc RamDebugger::ViewHelpFile { { file "" } } {
    variable MainDir

    if { $file == "" } {
	set w [HelpViewer::HelpWindow [file join $MainDir help]]
    } else {
	set w [HelpViewer::HelpWindow [file join $MainDir help $file]]
    }
    return $w
}


proc RamDebugger::ViewHelpForWord {} {
    variable text

    set w [ViewHelpFile]

    set range [$text tag ranges sel]
    if { $range != "" } {
	set word [eval $text get $range]
    } else {
	set word ""
	set idx [$text index insert]
	set idx0 $idx
	while { [string is wordchar [$text get $idx0]] } {
	    set word [$text get $idx0]$word
	    set idx0 [$text index $idx0-1c]
	}
	set idx1 [$text index $idx+1c]
	while { [string is wordchar [$text get $idx1]] } {
	    append word [$text get $idx1]
	    set idx1 [$text index $idx1+1c]
	}
	if { $word == "" } { return }
    }
    HelpViewer::HelpSearchWord $word
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

    if { $force == 2 } {
	set services [rdebug -forceupdate2 -actives]
    } elseif { $force } {
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
    $menu add command -label "Debug c++" -command "RamDebugger::DebugCplusPlusWindow"
    $menu add command -label "Debug c++ (no ask)" -command "RamDebugger::DebugCplusPlusWindow 1"

    if { $::tcl_platform(platform) == "windows" } {
	$menu add command -label "Update remotes" -command \
	     "RamDebugger::ActualizeActivePrograms $menu 1"
	 $menu add command -label "Update remotes slow" -command \
	     "RamDebugger::ActualizeActivePrograms $menu 2"
    }
    $menu add command -label Disconnect/Stop -command {
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

proc RamDebugger::GotoPreviusNextInWinList { what } {
    variable WindowFilesList
    variable WindowFilesListCurr

    switch $what {
	prev {
	    incr WindowFilesListCurr -1
	    if { $WindowFilesListCurr < 0 } {
		set WindowFilesListCurr [expr [llength $WindowFilesList]-1]
	    }
	    OpenFileF [lindex $WindowFilesList $WindowFilesListCurr]
	}
	next {
	    incr WindowFilesListCurr 1
	    if { $WindowFilesListCurr >= [llength $WindowFilesList] } {
		set WindowFilesListCurr 0
	    }
	    OpenFileF [lindex $WindowFilesList $WindowFilesListCurr]
	}
    }
}

proc RamDebugger::ActualizeWindowsList { menu } {
    variable WindowFilesList
    variable WindowFilesListCurr
    variable text

    $menu del 0 end
    $menu add command -label "Previus" -acc "Alt-Left" -command \
       "RamDebugger::GotoPreviusNextInWinList prev"
    $menu add command -label "Next" -acc "Alt-Right" -command \
       "RamDebugger::GotoPreviusNextInWinList next"

    $menu add separator
    set ipos 0
    foreach i $WindowFilesList {
	if { $ipos == $WindowFilesListCurr } {
	    $menu add checkbutton -label $i -variable ::pp -command [list RamDebugger::OpenFileF $i]
	    set ::pp 1
	} else {
	    $menu add command -label $i -command [list RamDebugger::OpenFileF $i]
	}
	incr ipos
    }
}

proc RamDebugger::AddRecentfilesToMenu { menu } {
    variable options

    $menu del 0 end

    if { ![info exists options(RecentFiles)] } { return }

    foreach i $options(RecentFiles) {
	$menu add command -label $i -command [list RamDebugger::OpenFileF $i]
    }
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

    if { [rinfo $line] != "" } {
	set hasbreak 1
    } else { set hasbreak 0 }

    if { $hasbreak } {
	set hasbreak 0
	foreach num [rinfo $line] {
	    rdel $num
	}
    } else {
	set hasbreak 1
	if { [catch [list rbreak $line] errorstring] } {
	    WaitState 0
	    WarnWin $errorstring
	    return
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
	    }
	    arrow {
		if { $hasarrow == "" } { set hasarrow 1 }
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
	if { [focus] != $text } {
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
    TextStackTraceRaise
}

proc RamDebugger::TakeArrowOutFromText {} {
    variable text
    variable marker

    if { ![info exists text] || ![winfo exists $text] } { return }

    foreach j [concat [$marker gettags arrow] [$marker gettags arrowbreak]] {
	if { [string match l* $j] } {
	    regexp {l([0-9]+)} $j {} arrowline
	    UpdateArrowAndBreak $arrowline "" 0
	}
    }
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

    if { ![AreFilesEqual $file $currentfile] } {
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
    variable remoteserverType
    variable IsInStop
    variable currentfile
    variable options

    if { $remoteserver == "" || ($remoteserverType == "local" && !$IsInStop) } {
	if { $currentfile == "" } {
	    WarnWin "Cannot start debugging. There is no currentfile" $text
	    return
	}
	if { [regexp {\.(h|c|cc)$} $currentfile] } {
	    if { $options(ConfirmStartDebugging) } {
		set ret [DialogWin::messageBox -default yes -icon question -message \
		    "Do you want to start to debug c++ program?" -parent $text \
		    -title "start debugging" -type yesnocancel]
	    } else { set ret yes }
	    if { $ret == "cancel" } { return }
	    if { $ret == "yes" } {
		DebugCplusPlusWindow 1
		return
	    }
	} else {
	    if { $options(ConfirmStartDebugging) } {
		set ret [DialogWin::messageBox -default yes -icon question -message \
		        "Do you want to start to debug locally '$currentfile'?" -parent $text \
		        -title "start debugging" -type yesnocancel]
	    } else { set ret yes }
	    if { $ret == "cancel" } { return }
	    if { $ret == "yes" } {
		rdebug -currentfile
		return
	    }
	}
	if { $remoteserver == "" } { return }
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
    if { $X == -1 || $currentfile == "" || !$IsInStop } { return }

    set TextMotionAfterId [after 500 RamDebugger::DisplayVar $X $Y $x $y]
}

proc RamDebugger::DisplayVar { X Y x y } {
    variable text
    variable remoteserverType
    variable debuggerstate

    if { $debuggerstate != "debug" } { return }

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
    if { $remoteserverType == "gdb" } {
	set comm "$var"
    } else {
	set comm "if { \[array exists $var] } { array get $var } else { set $var }"
    }
    # catch is here for strange situations, like when changing source file
    catch {
	set res [reval -handler [list RamDebugger::DisplayVar2 $var $X $Y $x $y] $comm]
    }
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
    variable remoteserver
    variable remoteserverType

    set w [winfo toplevel $f]

    if { $what == "do" } {
	if { [string trim $DialogWinTop::user($w,expression)] == "" } {
	    set DialogWinTop::user($w,type) ""
	    return
	}
	set var $DialogWinTop::user($w,expression)
	if { $remoteserverType == "gdb" } {
	    if { ![regexp {\[([0-9]+):([0-9]+)\]} $var {} ini1 end1] } {
		set ini1 1
		set end1 1
	    }
	    if { ![regexp {\[([0-9]+)::([0-9]+)\]} $var {} ini2 end2] } {
		set ini2 1
		set end2 1
	    }
	    set remoteserver [lreplace $remoteserver 2 2 [list getdata \
		"RamDebugger::DisplayVarWindowEval res $f"]]

	    set comm ""
	    set isinit 0
	    for { set i1 $ini1 } { $i1 <= $end1 } { incr i1 } {
		regsub {\[([0-9]+):([0-9]+)\]} $var \[$i1\] varn
		for { set i2 $ini2 } { $i2 <= $end2 } { incr i2 } {
		    regsub {\[([0-9]+)::([0-9]+)\]} $varn \[$i2\] varn
		    if { !$isinit } {
		        if { $ini1 != $end1 || $ini2 != $end2 } {
		            append comm "printf \"MULTIPLE RESULT\\n\"\n"
		        }
		        append comm "whatis $varn\n"
		        set isinit 1
		    }
		    append comm "printf \"\\n$varn=\"\noutput $varn\nprintf \"\\n\"\n"
		}
	    }
	    append comm "printf \"FINISHED GETDATA\\n\""
	    EvalRemote $comm
	    return
	} else {
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
	}
	reval -handler [list RamDebugger::DisplayVarWindowEval res $f] $comm
    } else {
	set var $DialogWinTop::user($w,expression)
	$DialogWinTop::user($w,textv) conf -state normal
	$DialogWinTop::user($w,textv) del 1.0 end

	if { $remoteserverType == "gdb" } {
	    if { [regexp {^(\s*MULTIPLE RESULT\s*type\s+=\s+char\s)(.*)} $res {} ini rest] } {
		set res $ini
		append res "   \""
		foreach "i c" [regexp -all -inline {'(.)'\n} $rest] {
		    append res "$c"
		}
		append res "\"\n$rest"
	    }
	    set res [list 0 [list variable $res]]
	}

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

    if { $::tcl_platform(platform) != "windows" } {
	$sw.text conf -exportselection 1
    }

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

    set curr [$DialogWinBreak::user(list) curselection]
    if { [llength $curr] != 1 } { return }

    set DialogWinBreak::user(cond) [lindex [$DialogWinBreak::user(list) get $curr] 3]
}

proc RamDebugger::DisplayBreakpointsWindow {} {
    variable text
    variable breakpoints
    variable currentfile
    
    CopyNamespace ::DialogWin ::DialogWinBreak

    set f [DialogWinBreak::Init $text "Breakpoints window" separator [list Delete \
	    "Delete all" View] "Apply Cond" Close]
    set w [winfo toplevel $f]

    label $f.l1 -text "Condition:" -grid 0
    entry $f.e1 -textvariable DialogWinBreak::user(cond) -width 80 -grid "1 px3 py3"

    set sw [ScrolledWindow $f.lf -relief sunken -borderwidth 0 -grid "0 2"]
    
    set DialogWinBreak::user(list) [tablelist::tablelist $sw.lb -width 55\
		  -exportselection 0 \
		  -columns [list \
		                4  Num        left \
		                15 File        right \
		                5  Line left \
		                40 Condition left \
		                40 Path left
		               ] \
		  -labelcommand tablelist::sortByColumn \
		  -background white \
		  -selectbackground navy -selectforeground white \
		  -stretch 1 -selectmode extended \
		  -highlightthickness 0]

    $sw setwidget $DialogWinBreak::user(list)

    supergrid::go $f

    focus $DialogWinBreak::user(list)
    bind [$DialogWinBreak::user(list) bodypath] <Double-1> {
	focus $DialogWinBreak::user(list)
	RamDebugger::DisplayBreakpointsWindowSetCond
    }
    bind [$DialogWinBreak::user(list) bodypath] <ButtonPress-3> \
	    [bind TablelistBody <ButtonPress-1>]

    bind [$DialogWinBreak::user(list) bodypath] <ButtonRelease-3> {
	catch { destroy %W.menu }
	set menu [menu %W.menu]
	$menu add command -label "Apply condition" -command "DialogWinBreak::InvokeOK"
	$menu add command -label "View" -command "DialogWinBreak::InvokeButton 4"
	$menu add separator
	$menu add command -label "Delete" -command "DialogWinBreak::InvokeButton 2"
	tk_popup $menu %X %Y
    }

    foreach i $breakpoints {
	foreach "num file line cond" $i break
	$DialogWinBreak::user(list) insert end [list $num [file tail $file] $line $cond \
		[file dirname $file]]
    }
    set action [DialogWinBreak::CreateWindow]
    while 1 {
	switch $action {
	    0 {
		DialogWinBreak::DestroyWindow
		namespace delete ::DialogWinBreak
		return
	    }
	    1 {
		set curr [$DialogWinBreak::user(list) curselection]
		if { [llength $curr] != 1 } {
		    WarnWin "Select just one breakpoint before applying condition" $w
		} else {
		    set val [$DialogWinBreak::user(list) get $curr]
		    rcond [lindex $val 0] $DialogWinBreak::user(cond)
		    $DialogWinBreak::user(list) delete $curr
		    $DialogWinBreak::user(list) insert $curr [lreplace $val 3 3 \
		            $DialogWinBreak::user(cond)]
		    set file [file join [lindex $val 4 ] [lindex $val 1]]
		    set line [lindex $val 2]
		    if { $file == $currentfile } {
		        $text mark set insert $line.0
		        $text see $line.0
		    }
		}
	    }
	    2 {
		foreach i [$DialogWinBreak::user(list) curselection] {
		    set ent [$DialogWinBreak::user(list) get $i]
		    set num [lindex $ent 0]
		    set file [file join [lindex $ent 4 ] [lindex $ent 1]]
		    set line [lindex $ent 2]
		    if { $file == $currentfile } {
		        UpdateArrowAndBreak $line 0 ""
		    }
		    rdel $num
		}
		$DialogWinBreak::user(list) del 0 end
		foreach i $breakpoints {
		    foreach "num file line cond" $i break
		    $DialogWinBreak::user(list) insert end [list $num [file tail $file] $line $cond \
		            [file dirname $file]]
		}
	    }
	    3 {
		set ret [DialogWinBreak::messageBox -default ok -icon warning -message \
		             "Are you sure to delete all breakpoints?" -parent $f \
		             -title "delete all breakpoints" -type okcancel]
		if { $ret == "ok" } {
		    $DialogWinBreak::user(list) del 0 end
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
	    4 {
		set curr [$DialogWinBreak::user(list) curselection]
		if { [llength $curr] != 1 } {
		    WarnWin "Select just one breakpoint in order to see the file" $w
		    return
		}
		set val [$DialogWinBreak::user(list) get $curr]
		set file [file join [lindex $val 4 ] [lindex $val 1]]
		set line [lindex $val 2]
		if { $file != $currentfile } {
		    OpenFileF $file
		}
		$text mark set insert $line.0
		$text see $line.0
	    }
	}
	set action [DialogWinBreak::WaitForWindow]
    }
}

proc RamDebugger::CreateModifyFonts {} {
    variable options

    if { [lsearch [font names] NormalFont] == -1 } { font create NormalFont }
    if { [lsearch [font names] FixedFont] == -1 } { font create FixedFont }
    if { [lsearch [font names] HelpFont] == -1 } { font create HelpFont }

    eval font configure NormalFont $options(NormalFont)
    eval font configure FixedFont $options(FixedFont)
    eval font configure HelpFont $options(HelpFont)

    option add *font NormalFont

}

proc RamDebugger::UpdateFont { w but fontname } {

    set newfont [SelectFont $w.fonts -type dialog -parent $w -sampletext \
	"RamDebugger is nice" -title "Select font"]
    if { $newfont == "" } { return }
    set list [list family size weight slant underline overstrike]
    foreach $list $newfont break

    set comm "font configure"
    lappend comm $fontname
    foreach i [lrange $list 0 3] {
	lappend comm -$i [set $i]
    }
    if { [info exists underline] && $underline != "" } {
	lappend comm -underline 1
    } else { lappend comm -underline 0 }

    if { [info exists overstrike] && $overstrike != "" } {
	lappend comm -overstrike 1
    } else { lappend comm -overstrike 0 }

    eval $comm

    regexp {^[^:]+} [$but cget -text] type
    set btext "$type: [font conf $fontname -family] [font conf $fontname -size] "
    append btext "[font conf $fontname -weight] [font conf $fontname -slant]"
    $but configure -text $btext
}

proc RamDebugger::PreferencesWindow {} {
    variable text
    variable options
    variable options_def
    
    set f [DialogWin::Init $text "Preferences window" separator [list Apply Defaults]]
    set w [winfo toplevel $f]

    TitleFrame $f.f1 -text [_ debugging] -grid 0
    set f1 [$f.f1 getframe]

    if { $::tcl_platform(platform) == "windows" } {
	checkbutton $f1.cb0 -text "Automatically check remote files" -variable \
	DialogWin::user(CheckRemotes) -grid "0 3 w"
	DynamicHelp::register $f1.cb0 balloon [string trim {
	    this variable is only used on windows. It can be:
	    0: Only check remote programs on demand (useful if not making remote debugging, the
	       start up is faster)
	    1: Register as remote and check remote programs on start up. The start up can be slower
		but is better when making remote debugging
	}]
	set DialogWin::user(CheckRemotes) $options(CheckRemotes)
    }

    checkbutton $f1.cb1 -text "Confirm start debugging" -variable \
       DialogWin::user(ConfirmStartDebugging) -grid "0 3 w"
    DynamicHelp::register $f1.cb1 balloon [string trim {
	If this option is set, a confirmation window will be displayed
	when starting the execution of the debugger
    }]

    set DialogWin::user(ConfirmStartDebugging) $options(ConfirmStartDebugging)

    set helptext "When debugging locally, choose here if the debugged script is only TCL "
    append helptext "or also TK"
    Label $f1.l15 -text "Local debugging type:" -helptext $helptext -grid "0 e"
    radiobutton $f1.r1 -text "TCL" -variable DialogWin::user(LocalDebuggingType) -value tcl \
	    -grid "1 w"
    radiobutton $f1.r2 -text "TK" -variable DialogWin::user(LocalDebuggingType) -value tk \
	    -grid "2 w"

    set DialogWin::user(LocalDebuggingType) $options(LocalDebuggingType)

    Label $f1.l2 -text "Indent size TCL:" -helptext "Size used when indenting TCL with key: <Tab>" \
       -grid "0 e"
    SpinBox $f1.sb -range "0 10 1" -textvariable DialogWin::user(indentsizeTCL) \
       -width 4 -grid "1 2 px3"
    set DialogWin::user(indentsizeTCL) $options(indentsizeTCL)

    Label $f1.l3 -text "Indent size c++:" -helptext "Size used when indenting c++ with key: <Tab>" \
       -grid "0 e"
    SpinBox $f1.sb2 -range "0 10 1" -textvariable DialogWin::user(indentsizeC++) \
       -width 4 -grid "1 2 px3"
    set DialogWin::user(indentsizeC++) $options(indentsizeC++)

    TitleFrame $f.f2 -text [_ fonts] -grid 0
    set f2 [$f.f2 getframe]
    
    foreach "but type fontname" [list $f2.b1 {GUI font} NormalFont $f2.b2 {Text font} FixedFont \
	$f2.b3 {Help font} HelpFont] {
	set btext "$type: [font conf $fontname -family] [font conf $fontname -size] "
	append btext "[font conf $fontname -weight] [font conf $fontname -slant]"
	Button $but -text $btext -font $fontname -relief link -command \
	    "RamDebugger::UpdateFont $w $but $fontname" -grid "0 w"
    }

    supergrid::go $f1
    supergrid::go $f2
    supergrid::go $f

    focus $f1.cb1

    set action [DialogWin::CreateWindow "" "" 200]
    while 1 {
	switch $action {
	    0 {
		DialogWin::DestroyWindow
		return
	    }
	    1 - 2 {
		if { ![string is integer -strict $DialogWin::user(indentsizeTCL)] || \
		    $DialogWin::user(indentsizeTCL) < 0 || $DialogWin::user(indentsizeTCL) > 10 } {
		    WarnWin "Error: indent size must be between 0 and 10" $w
		} elseif { ![string is integer -strict $DialogWin::user(indentsizeC++)] || \
		    $DialogWin::user(indentsizeC++) < 0 || $DialogWin::user(indentsizeC++) > 10 } {
		    WarnWin "Error: indent size must be between 0 and 10" $w
		} else {
		    set options(indentsizeTCL) $DialogWin::user(indentsizeTCL)
		    set options(indentsizeC++) $DialogWin::user(indentsizeC++)
		    set options(ConfirmStartDebugging) $DialogWin::user(ConfirmStartDebugging)
		    set options(LocalDebuggingType) $DialogWin::user(LocalDebuggingType)
		    if { [info exists options(CheckRemotes)] } {
		        set options(CheckRemotes) $DialogWin::user(CheckRemotes)
		    }
		    if { $action == 1 } {
		        DialogWin::DestroyWindow
		        return
		    }
		}
	    }
	    3 {
		foreach i [list indentsizeTCL indentsizeC++ ConfirmStartDebugging LocalDebuggingType \
		        CheckRemotes NormalFont FixedFont HelpFont] {
		    if { [info exists options_def($i)] } {
		        set options($i) $options_def($i)
		        set DialogWin::user($i) $options_def($i)
		    }
		}
		CreateModifyFonts

		foreach "but fontname" [list $f2.b1 NormalFont $f2.b2 FixedFont $f2.b3 HelpFont] {
		    regexp {^[^:]+} [$but cget -text] type
		    set btext "$type: [font conf $fontname -family] [font conf $fontname -size] "
		    append btext "[font conf $fontname -weight] [font conf $fontname -slant]"
		    $but configure -text $btext
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
    variable text

    set f [DialogWin::Init $text "Timing report" separator "" -]
    set w [winfo toplevel $f]

    ComboBox $f.cb1 -textvariable DialogWin::user(units) -editable 0 \
       -modifycmd {
	   $DialogWin::user(text) del 1.0 end
	   $DialogWin::user(text) ins end [RamDebugger::rtime -display $DialogWin::user(units)]
       } -width 10 -values [list microsec  milisec  sec  min] -grid "0 w"
    set DialogWin::user(units) sec

    set sw [ScrolledWindow $f.lf -relief sunken -borderwidth 0 -grid "0 2"]
    text $sw.t
    $sw setwidget $sw.t

    set DialogWin::user(text) $sw.t

    bind $sw.t <1> "focus $sw.t"
    
    supergrid::go $f

    focus $sw.t

    $sw.t ins end [rtime -display sec]

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
	set DialogWinTop::user($w,lineini) [scan [$text index sel.first] %d]
	set DialogWinTop::user($w,lineend) [scan [$text index sel.last] %d]
    }]} {
	WarnWin "It is necessary to select some text in main window" $w
    }
}

proc RamDebugger::DrawSelection { w } {
    variable text

    if { [catch {
	$text tag add sel "$DialogWinTop::user($w,lineini).0 linestart" \
	   "$DialogWinTop::user($w,lineend).0 lineend"
	$text see $DialogWinTop::user($w,lineini).0
    }] } {
	WarnWin "Lines are not correct" $w
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
	    set DialogWinTop::user($w,lineini) [lindex [$DialogWinTop::user($w,list) get $idx] 2]
	    set DialogWinTop::user($w,lineend) [lindex [$DialogWinTop::user($w,list) get $idx] 3]
	}
	update {
	    $DialogWinTop::user($w,list) del 0 end
	    foreach i $TimeMeasureData {
		$DialogWinTop::user($w,list) insert end [lrange $i 0 3]
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
	    10 "Name"        left \
	    10 "File"        right \
	     7 "Initial line"        right \
	     7 "End line"  right \
	   ] \
	-labelcommand tablelist::sortByColumn \
	-background white \
	-selectbackground navy -selectforeground white \
	-stretch "0 1" -selectmode browse \
	-highlightthickness 0]

    $sw setwidget $DialogWinTop::user($w,list)

    bind [$DialogWinTop::user($w,list) bodypath] <Double-1> \
       "RamDebugger::ModifyTimingBlock $w updatecurrent"

    set bbox [ButtonBox $f2.bbox1 -spacing 0 -padx 1 -pady 1 -homogeneous 1 -grid "0 w"]
    $bbox add -image acttick16 \
	 -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1 -padx 1 -pady 1 \
	 -helptext [_ "Create new block"] \
	 -command "RamDebugger::ModifyTimingBlock $w create"
    $bbox add -image edit16 \
	 -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1 -padx 1 -pady 1 \
	 -helptext [_ "Update selected block"] \
	 -command "RamDebugger::ModifyTimingBlock $w edit"
    $bbox add -image actcross16 \
	 -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1 -padx 1 -pady 1 \
	 -helptext [_ "Delete selected block"] \
	 -command "RamDebugger::ModifyTimingBlock $w delete"

    supergrid::go $f1
    supergrid::go $f2
    supergrid::go $f

    tkTabToWindow $f1.e1
    bind [winfo toplevel $f] <Return> "DialogWinTop::InvokeOK $f"
    ModifyTimingBlock $w update

    DialogWinTop::CreateWindow $f
    DialogWinTop::InvokeOK $f
}

proc RamDebugger::AboutWindow {} {
    variable text
    
    set par [winfo toplevel $text]
    set w $par.about
    toplevel $w
    wm protocol $w WM_DELETE_WINDOW {
	  # nothing
    }
    
    label $w.l -text RamDebugger -font "-family {new century schoolbook} -size 24 -weight bold" \
	    -fg \#d3513d -grid 0

    set tt "Author: Ramon Ribó (RAMSAN)\n"
    append tt "ramsan@cimne.upc.es\nhttp://gid.cimne.upc.es/ramsan\n"
    append tt "http://gid.cimne.upc.es/RamDebugger"

    label $w.l2 -grid 0 -text $tt
    canvas $w.c -height 50 -grid 0 -bg white -highlightthickness 0
    ScrolledWindow $w.lf -relief sunken -borderwidth 0 -grid 0

    tablelist::tablelist $w.lf.lb -width 55\
		  -exportselection 0 \
		  -columns [list \
		                10 Package        left \
		                10 Author        left \
		                10 License right\
		                4  Mod left \
		               ] \
		  -labelcommand tablelist::sortByColumn \
		  -background white \
		  -selectbackground navy -selectforeground white \
		  -stretch 1 -selectmode extended \
		  -highlightthickness 0


    $w.lf setwidget $w.lf.lb

    frame $w.buts -height 35 -bg [CCColorActivo [$w  cget -bg]] -grid 0

    supergrid::go $w

    button $w.close -text Close -width 10
    place $w.close -in $w.buts -anchor n -y 3
    
    wm withdraw $w
    update idletasks
    set x [expr [winfo x $par]+[winfo width $par]/2-[winfo reqwidth $w]/2]
    set y [expr [winfo y $par]+[winfo height $par]/2-[winfo reqheight $w]/2]
    wm geom $w +$x+$y
    wm deiconify $w
    wm transient $w $par

    bind $w.close <Enter> "RamDebugger::MotionInAbout $w.close ; break"
    bind $w.close <ButtonPress-1> "WarnWin [list {Congratulations, you got it!!!}] ; destroy $w; break"


    $w.c create text 0 0 -anchor n -font "-family {new century schoolbook} -size 16 -weight bold"\
	    -fill \#d3513d -text "Version 1.3" -tags text
    RamDebugger::AboutMoveCanvas $w.c 0


    set data {
	tkcon "Jeffrey Hobbs" BSD SLIGHTLY
	comm "Open Group Res. Ins." BSD YES
	BWidgets "Unifix" BSD YES
	dialogwin  RAMSAN BSD NO
	helpviewer RAMSAN BSD NO
	supergrid  RAMSAN BSD NO
	supertext  "Bryan Oakley" "FREE SOFT" YES
	tablelist  "Csaba Nemethi" "FREE SOFT" NO
	"visual regexp" "Laurent Riesterer" GPL NO
	Tkhtml  "D. Richard Hipp" LGPL NO
	tcllib Many BSD NO
	icons "Adrian Davis" BSD NO
    }
    foreach "pack author lic mod" $data {
	$w.lf.lb insert end [list $pack $author $lic $mod]
    }


}

proc RamDebugger::AboutMoveCanvas { c t } {

    if { ![winfo exists $c] } { return }
    set w [winfo width $c]
    set h [winfo height $c]

    set x [expr $w/2*(1.0-sin($t))]
    set y [expr $h/2*(cos($t)+1)]

    $c coords text $x $y

    set t [expr $t+.2]
    after 100 [list RamDebugger::AboutMoveCanvas $c $t]
}

proc RamDebugger::MotionInAbout { but } {

    if { ![winfo exists $but] } { return }

    set w [winfo toplevel $but]

    while 1 {
	if { ![winfo exists $but] } { return }
	set x [expr [winfo pointerx $w]-[winfo rootx $w]]
	set y [expr [winfo pointery $w]-[winfo rooty $w]]
	
	set x1 [expr [winfo rootx $but]-[winfo rootx $w]]
	set y1 [expr [winfo rooty $but]-[winfo rooty $w]]
	set x2 [expr $x1+[winfo width $but]]
	set y2 [expr $y1+[winfo height $but]]

	if { $x < $x1-2 || $x > $x2+2 || $y < $y1-2 || $y > $y2+2 } { return }
	
	set xmax [winfo width $w]
	set ymax [winfo height $w]
	
	if { $x < ($x1+$x2)/2 } {
	    set newx [expr $x1+int((rand()-.2)*5.1)]
	} else { set newx [expr $x1+int((rand()-.8)*5.1)] }
	
	if { $newx < 0 } { set newx 0 }
	if { $newx > $xmax-[winfo width $but] } { set newx [expr $xmax-[winfo width $but]] }
	
	if { $y < ($y1+$y2)/2 } {
	    set newy [expr $y1+int((rand()-.2)*5.1)]
	} else { set newy [expr $y1+int((rand()-.8)*5.1)] }
	
	
	if { $newy < 0 } { set newy 0 }
	if { $newy > $ymax-[winfo height $but] } { set newy [expr $ymax-[winfo height $but]] }
	place $but -in $w -x $newx -y $newy -anchor nw
	update
    }
}

proc RamDebugger::DisplayWindowsHierarchyInfo { canvas w x y } {
    variable TextMotionAfterId
    
    after cancel $TextMotionAfterId
    set TextMotionAfterId ""
    if { [winfo exists $canvas.help] } { destroy $canvas.help }

    if { $w != "" } {
	set TextMotionAfterId [after 100 RamDebugger::DisplayWindowsHierarchyInfoDo \
		$canvas $w $x $y]
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
		    set slave [grid slaves WIDGET -row $i -column $j]
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
	if { [string index $retval_in end] != "\n" } {
	    append retval_in \n
	}
	append retval $retval_in
	append retval "SIZES\n"
	append retval "    width=[winfo width WIDGET] reqwidth=[winfo reqwidth WIDGET]\n"
	append retval "    height=[winfo height WIDGET] reqheight=[winfo reqheight WIDGET]\n"
	EVAL
    }
    if { $DialogWinHier::user(type) == "ramdebugger" } {
	set retcomm "RamDebugger::DisplayWindowsHierarchyInfoDo2 $canvas $w $x $y \[set retval]"
    } else {
	set retcomm "RDC::SendDev \[list RamDebugger::DisplayWindowsHierarchyInfoDo2 $canvas \
		$w $x $y \[set retval]]"
    }
    set comm [string map [list EVAL $retcomm] $comm]
    set comm [string map [list WIDGET $w] $comm]
    if { $DialogWinHier::user(type) == "ramdebugger" } {
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
	if { $DialogWinHier::user(type) == "ramdebugger" } {
	    set retcomm "RamDebugger::DisplayWindowsHierarchyDo res \[::RDC::WindowsHierarchy .]"
	} else {
	    set retcomm "RDC::SendDev \[list RamDebugger::DisplayWindowsHierarchyDo res \
		        \[::RDC::WindowsHierarchy .]]"
	}
	set comm [string map [list EVAL $retcomm] $comm]

	if { [info exists RamDebugger::DisplayWindowsHierarchyFindLastId] } {
	    unset RamDebugger::DisplayWindowsHierarchyFindLastId
	}

	if { $DialogWinHier::user(type) == "ramdebugger" } {
	    eval $comm
	} else {
	    if { $remoteserver == "" } {
		after 100 [list WarnWin "Error: there is no debugged application" \
		    $DialogWinHier::user(canvas)]
		$DialogWinHier::user(canvas) delete items
		return
	    }
	    EvalRemote $comm
	}
    } else {
	$DialogWinHier::user(canvas) delete items
	set linespace [expr [font metrics NormalFont -linespace]+1]

	foreach "newx newy" [DisplayWindowsHierarchyDoDraw $DialogWinHier::user(canvas) $res \
	    5 5 $linespace] break

	$DialogWinHier::user(canvas) conf -scrollregion [list 0 0 $newx $newy]
    }
}

proc RamDebugger::DisplayWindowsHierarchyFind {} {
    variable DisplayWindowsHierarchyFindLastId

    set canvas $DialogWinHier::user(canvas)

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
	    if { [string match -nocase *$DialogWinHier::user(find)* $text] } {
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

    CopyNamespace ::DialogWin ::DialogWinHier

    set f [DialogWinHier::Init $text "Windows hierarchy" separator "" -]
    set w [winfo toplevel $f]

    radiobutton $f.r1 -text "In debugged app" -variable DialogWinHier::user(type) -value debug \
       -command "RamDebugger::DisplayWindowsHierarchyDo do" -grid "0 w"
    radiobutton $f.r2 -text "In RamDebugger" -variable DialogWinHier::user(type) -value ramdebugger \
       -command "RamDebugger::DisplayWindowsHierarchyDo do" -grid "1 w"

    frame $f.f -grid "0 2"
    label $f.f.l -text "Find:" -grid 0
    entry $f.f.e -textvar DialogWinHier::user(find) -grid 1

    tkTabToWindow $f.f.e

    button $f.f.b1 -text Go -width 5 -grid "2 px3 py3" -command \
       "RamDebugger::DisplayWindowsHierarchyFind"
    bind $f.f.e <Return> "$f.f.b1 invoke"

    set DialogWinHier::user(type) debug

    set sw [ScrolledWindow $f.lf -relief sunken -borderwidth 0 -grid "0 2"]
    set DialogWinHier::user(canvas) [canvas $sw.t -width 600 -height 400 -bg white -bd 0 ]
    $sw setwidget $DialogWinHier::user(canvas)

    bind $DialogWinHier::user(canvas) <2> {
	%W scan mark %x %y
	set tkPriv(x) %x
	set tkPriv(y) %y
	set tkPriv(mouseMoved) 0
    }
    bind $DialogWinHier::user(canvas) <B2-Motion> {
	if {(%x != $tkPriv(x)) || (%y != $tkPriv(y))} {
	    set tkPriv(mouseMoved) 1
	}
	if {$tkPriv(mouseMoved)} {
	    %W scan dragto %x %y
	}
    }

    supergrid::go $f

    DisplayWindowsHierarchyDo do

    set action [DialogWinHier::CreateWindow]
    DialogWinHier::DestroyWindow
    namespace delete ::DialogWinHier
}

proc RamDebugger::GotoLine {} {
    variable text

    set f [DialogWin::Init $text "Goto line" separator ""]
    set w [winfo toplevel $f]

    label $f.l -text "Go to line:" -grid "0 px3 py5"
    SpinBox $f.sb -range "1 1000 1" -textvariable DialogWin::user(line) \
	    -width 8 -grid "1 px3"

    checkbutton $f.cb1 -text "Relative to current line" -variable DialogWin::user(relative) \
	    -grid "0 2 w"

    set DialogWin::user(relative) 0

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
		set line $DialogWin::user(line)
		set good 1
		if { ![string is integer -strict $line] } {
		    WarnWin "Line number must be a positive number" $w
		    set good 0
		}
		if { $good && $DialogWin::user(relative) } {
		    set insline [scan [$text index insert] %d]
		    set lastline [scan [$text index end-1c] %d]
		    set line [expr $insline+$line]
		    if { $line < 1 || $line > $lastline } {
		        WarnWin "Trying to go to line $line. Out of limits" $w
		        set good 0
		    }
		}
		if { $good } {
		    set lastline [scan [$text index end-1c] %d]
		    if { $line < 1 } { set line 1 }
		    if { $line > $lastline } { set line $lastline }
		    $text mark set insert $line.0
		    $text see $line.0
		    focus $text
		    DialogWin::DestroyWindow
		    return
		}
	    }
	}
	set action [DialogWin::WaitForWindow]
    }
}

proc RamDebugger::DebugCplusPlusWindow { { tryautomatic 0 } } {
    variable text
    variable options

    if { ![info exists options(debugcplusplus)] } {
	set options(debugcplusplus) ""
    }

    set exes ""
    set dirs ""
    set args ""
    foreach "exe dir arg" $options(debugcplusplus) {
	lappend exes $exe
	lappend dirs $dir
	lappend args $arg
    }

    set exe ""
    foreach "exe dir args" [cproject::GiveDebugData] break

    if { $tryautomatic && $exe != "" } {
	set found 0
	set ipos 0
	foreach "exe_in dir_in args_in" $options(debugcplusplus) {
	    if { $exe == $exe_in && $dir == $dir_in && $args == $args_in } {
		set found 1
		break
	    }
	    incr ipos 3
	}
	if { $found && $ipos != 0 } {
	    set options(debugcplusplus) [lreplace $options(debugcplusplus) $ipos \
		    [expr $ipos+2]]
	}
	if { !$found || $ipos != 0 } {
	    set options(debugcplusplus) [linsert $options(debugcplusplus) 0 \
		    $exe $dir $args]
	}

	rdebug -debugcplusplus [list $exe $dir $args]
	return
    }
    if { $tryautomatic && $options(debugcplusplus) != "" } {
	rdebug -debugcplusplus [lrange $options(debugcplusplus) 0 2]
	return
    }

    set f [DialogWin::Init $text "Debug c++" separator ""]
    set w [winfo toplevel $f]
    
    label $f.l -text "Program to debug:" -grid "0 e px3 py5"
    ComboBox $f.cb1 -textvariable DialogWin::user(executable) -width 40 -grid 1 -values \
	    $exes
    Button $f.b1 -image [Bitmap::get file] -width 16 -grid 2 -relief link


    label $f.l2 -text "Directory:" -grid "0 e px3 py5"
    ComboBox $f.cb2 -textvariable DialogWin::user(directory) -width 40 -grid "1" -values \
	    $dirs
    Button $f.b2 -image [Bitmap::get folder] -grid 2 -relief link

    label $f.l3 -text "Arguments:" -grid "0 e"
    ComboBox $f.cb3 -textvariable DialogWin::user(arguments) -width 40 -grid "1 2" -values \
	    $args


    set comm {
	set DialogWin::user(executable) [tk_getOpenFile -filetypes {{{All Files} *}} \
		-initialdir $RamDebugger::options(defaultdir) -initialfile \
		$DialogWin::user(executable) -parent PARENT -title "Debug executable"]
    }
    set comm [string map [list PARENT $w] $comm]
    $f.b1 configure -command $comm

    set comm {
	set initial $RamDebugger::options(defaultdir)
	catch { set initial [file dirname $DialogWin::user(executable)] }
	set DialogWin::user(directory) [tk_chooseDirectory   \
	    -initialdir $initial -parent PARENT \
	    -title "Debug directory" -mustexist 1]
    }
    set comm [string map [list PARENT $w] $comm]
    $f.b2 configure -command $comm

    if { [info exists options(debugcplusplus)] } {
	set DialogWin::user(executable) [lindex $options(debugcplusplus) 0]
	set DialogWin::user(directory) [lindex $options(debugcplusplus) 1]
	set DialogWin::user(arguments) [lindex $options(debugcplusplus) 2]
    }
    tkTabToWindow $f.cb1

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
		set found 0
		set ipos 0
		foreach "exe dir args" $options(debugcplusplus) {
		    if { $exe == $DialogWin::user(executable) && \
		        $dir == $DialogWin::user(directory) && \
		        $args == $DialogWin::user(arguments) } {
		        set found 1
		        break
		    }
		    incr ipos 3
		}
		if { $found && $ipos != 0 } {
		    set options(debugcplusplus) [lreplace $options(debugcplusplus) $ipos \
		            [expr $ipos+2]]
		}
		if { !$found || $ipos != 0 } {
		    set options(debugcplusplus) [linsert $options(debugcplusplus) 0 \
		        $DialogWin::user(executable) $DialogWin::user(directory) \
		        $DialogWin::user(arguments)]
		}

		rdebug -debugcplusplus [list $DialogWin::user(executable) \
		        $DialogWin::user(directory) $DialogWin::user(arguments)]
		DialogWin::DestroyWindow
		return
	    }
	}
	set action [DialogWin::WaitForWindow]
    }
}

proc RamDebugger::CheckEvalEntries { what { name "" } { res "" } } {
    variable EvalEntries
    variable IsInStop
    variable remoteserver
    variable remoteserverType
    variable gdblog

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
	    if { $remoteserverType == "gdb" } {
		while { [lindex $remoteserver 2] != "" } {
		    vwait RamDebugger::remoteserver
		}
		set remoteserver [lreplace $remoteserver 2 2 multipleprint]
		set command ""
		foreach i $vars {
		    append command "print $i\n"
		}
		append command "printf \"FINISHED MULTIPLEPRINT\\n\""
		EvalRemote $command
		return
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
	    if { $remoteserverType == "gdb" } {
		set remoteserver [lreplace $remoteserver 2 2 [list getdata \
		        "RamDebugger::CheckEvalEntries res $name"]]
		set comm "output $var\n"
		append comm "printf \"FINISHED GETDATA\\n\""
		EvalRemote $comm
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
	    if { $remoteserverType == "gdb" } {
		set res [list 0 [list variable $res]]
	    }
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
	    if { $remoteserverType == "gdb" } {
		set remoteserver [lreplace $remoteserver 2 2 setvariable]
		EvalRemote "set variable $var=$value"
		return
	    }
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
    variable remoteserver
    variable remoteserverType

    if { !$IsInStop } { return }
    if { $name == "" } {
	if { $what == "do" } {
	    if { $remoteserverType == "gdb" } {
		while { [lindex $remoteserver 2] != "" } {
		    vwait RamDebugger::remoteserver
		}
		set remoteserver [lreplace $remoteserver 2 2 infolocals]
		EvalRemote "info locals\nprintf \"\\nFINISHED INFO LOCALS\\n\""
		return
	    }
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
		if { ![info exists EvalEntries($i,leftL)] } {
		    set pane1 [winfo parent $EvalEntries(0,leftentryL)]
		    set pane2 [winfo parent $EvalEntries(0,rightentryL)]
		    CreatePanedEntries [expr $i+1] $pane1 $pane2 L
		}
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
	    if { $remoteserverType == "gdb" } {
		set remoteserver [lreplace $remoteserver 2 2 setvariable]
		EvalRemote "set variable $var=$value"
		return
	    }
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

proc RamDebugger::TextOutClear {} {
    variable textOUT

    if { ![info exists textOUT] || ![winfo exists $textOUT] } { return }

    $textOUT conf -state normal
    $textOUT del 1.0 end
    $textOUT conf -state disabled
}

proc RamDebugger::TextOutInsert { data } {
    variable textOUT

    if { ![info exists textOUT] || ![winfo exists $textOUT] } { return }

    $textOUT conf -state normal
    $textOUT ins end $data
    $textOUT conf -state disabled
    $textOUT yview moveto 1
}

proc RamDebugger::TextOutInsertRed { data } {
    variable textOUT

    if { ![info exists textOUT] || ![winfo exists $textOUT] } { return }

    $textOUT conf -state normal
    $textOUT ins end $data red
    $textOUT tag configure red -foreground red
    $textOUT conf -state disabled
    $textOUT yview moveto 1
}

proc RamDebugger::TextCompClear {} {
    variable textCOMP

    if { ![info exists textCOMP] || ![winfo exists $textCOMP] } { return }

    $textCOMP conf -state normal
    $textCOMP del 1.0 end
    $textCOMP conf -state disabled
}

proc RamDebugger::TextCompInsert { data } {
    variable textCOMP

    if { ![info exists textCOMP] || ![winfo exists $textCOMP] } { return }

    $textCOMP conf -state normal
    $textCOMP ins end $data
    $textCOMP conf -state disabled
    $textCOMP yview moveto 1
}


proc RamDebugger::ProgressVar { value { canstop 0 } } {
    variable progressvar
	variable text

    if { [info exists progressvar] && $progressvar == -2 } {
	set RamDebugger::progressvar -1
	if { [winfo exists $text.__frame] } { destroy $text.__frame }
	bind all <Escape> ""
	error "Stop at user demand"
    }

    set progressvar $value

    if { $canstop == 1 && $value == 0 } {
	if { [winfo exists $text.__frame] } { destroy $text.__frame }
	frame $text.__frame
	focus $text.__frame
	catch { grab $text.__frame }
	bind all <Escape> "set RamDebugger::progressvar -2"
    }

    if { $value == 100 } {
	after 1000 set RamDebugger::progressvar -1
	if { [winfo exists $text.__frame] } { destroy $text.__frame }
	bind all <Escape> ""
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

proc RamDebugger::GiveListBoxItemName { listbox string } {

    regsub -all {\W} $string _ item

    while { [$listbox exists $item] } {
	append item _
    }
    return $item
}

proc RamDebugger::FillListBox {} {
    variable listbox
    variable options
    variable instrumentedfilesP
    variable instrumentedfilesR
    variable instrumentedfilesSent
    variable remoteserverType
    variable images

    if { $listbox == "" || ![winfo exists $listbox] } { return }

    $listbox delete [$listbox items]
    
    set parent [file dirname $options(defaultdir)]
    $listbox insert end .. -image [Bitmap::get folder] -text ".." -data [list folder $parent]
    set idxfolder 1
    set files ""
    foreach i [glob -nocomplain -dir $options(defaultdir) *] {
	lappend files [file tail $i]
    }

    if { $remoteserverType == "local" } {
	set IsLocal 1
    } else { set IsLocal 0 }

    foreach i [lsort -dictionary $files] {
	set item [GiveListBoxItemName $listbox $i]
	set fullpath [file join $options(defaultdir) $i]
	if { [file isdir $fullpath] } {
	    if { [string tolower $i] == "cvs" } { continue }
	    $listbox insert $idxfolder $item -image [Bitmap::get folder] -text $i \
		-data [list folder $fullpath]
	    incr idxfolder
	} elseif { [regexp {(\.tcl|\.h|\.c|\.cc|Makefil.*[^~])$} $i] } {
	    if { [info exists instrumentedfilesSent($fullpath)] } {
		switch $instrumentedfilesSent($fullpath) {
		    debug { set img $images(file_blue) }
		    time { set img $images(file_magenta) }
		}
	    } elseif { [GiveInstFile $fullpath 1 I] != "" } {
		set img $images(file_yellow)
	    } else { set img [Bitmap::get file] }

	    $listbox insert end $item -image $img -text $i -data \
		[list file [file join $options(defaultdir) $i]]
	}
    }
}

proc RamDebugger::PrevNextCompileError { what } {
    variable textCOMP

    set err [catch { $textCOMP index sel.first} idx]
    if { $err } {
	switch $what {
	    next { set idx 1.0 }
	    prev { set idx end-1c }
	}
    } else {
	switch $what {
	    next {
		set idx [$textCOMP index $idx+1l]
		if { [$textCOMP compare $idx > end-1c] } {
		    set idx 1.0
		}
	    }
	    prev {
		set idx [$textCOMP index $idx-1l]
		if { [$textCOMP compare $idx < 1.0] } {
		    set idx end-1c
		}
	    }
	}
    }
    $textCOMP tag remove sel 1.0 end
    $textCOMP tag add sel "$idx linestart" "$idx lineend"
    $textCOMP see $idx
    StackDouble1 $textCOMP $idx
}


proc RamDebugger::StackDouble1 { textstack idx } {
    variable text
    variable currentfile
    variable options

    set data [$textstack get "$idx linestart" "$idx lineend"]

    foreach pattern [list {((?:[a-zA-Z]:/)?[-/\w.]+):([0-9]+)} \
	    {((?:[a-zA-Z]:/)?[-/\w. ]+):([0-9]+)}] {

	if { [regexp $pattern $data {} file line] } {
	    
	    if { ![file exists $file] && [file exists [file join $options(defaultdir) \
		    $file]] } {
		set file [file join $options(defaultdir) $file]
	    }
	    if { ![file exists $file] } {
		set fullfile [cproject::TryToFindPath $file]
		if { $fullfile != "" } {
		    set file $fullfile
		}
	    }
	    if { [file exists $file] } {
		set file [filenormalize $file]
		if { $file != $currentfile } {
		    OpenFileF $file
		}
		$text see $line.0
		$text mark set insert $line.0
		focus $text
		return
	    }
	}
    }
}

proc RamDebugger::Compile { name } {
    variable text
    variable mainframe
    variable MainDir

    $mainframe setmenustate debugentry disabled

    set dir [file dirname $name]

    set pwd [pwd]
    cd $dir

    if { [regexp {\.(c|cc)$} $name] } {
	if { [auto_execok gcc] == "" } {
	    $mainframe setmenustate debug normal
	    cd $pwd

	    set ret [DialogWin::messageBox -default yes -icon question -message \
		"Could not find command 'gcc'. Do you want to see the help?" -parent $text \
		-title "Command not found" -type yesno]
	    if { $ret == "yes" } {
		RamDebugger::ViewHelpFile "01RamDebugger/RamDebugger_12.html"
	    }
	    return
	}
	set comm "gcc -c -I$dir $name"
	set commS "gcc -c -I$dir $name"
    } elseif { [regexp {Makefil.*[^~]$} $name] } {
	$mainframe setmenustate debug normal
	if { [auto_execok make] == "" } {
	    cd $pwd
	    set ret [DialogWin::messageBox -default yes -icon question -message \
		"Could not find command 'make'. Do you want to see the help?" -parent $text \
		-title "Command not found" -type yesno]
	    if { $ret == "yes" } {
		RamDebugger::ViewHelpFile "01RamDebugger/RamDebugger_12.html"
	    }
	    return
	}
	set comm "make -f $name"
	set commS "make -f $name"
    }

#     if {$::tcl_platform(platform) == "windows" && \
#         [info exists ::env(COMSPEC)]} {
#         set comm  "[file join $::env(COMSPEC)] /c $comm"
#     }

    if { $::tcl_platform(platform) == "windows" } {
	    set cat [file join $MainDir addons cat.exe]
    } else { set cat cat }

    set fin [open "|$comm |& $cat" r]
    fconfigure $fin -blocking 0
    fileevent $fin readable [list RamDebugger::CompileFeedback $fin $name]
    cd $pwd
    
    TextCompClear
    TextCompRaise
    TextCompInsert "[string repeat - 20]Compiling [file tail $name][string repeat - 20]\n"
    TextCompInsert "--> $commS\n"
    update
}

proc RamDebugger::CompileFeedback { fin name } {
    variable text
    variable mainframe

    if { [eof $fin] } {
	if { $name != "" } {
	    TextCompInsert "End compilation of [file tail $name]\n"
	    TextCompRaise
	}

	set err [catch { close $fin } errstring]
	$mainframe setmenustate debugentry normal
	if { $err } {
	    TextCompInsert $errstring\n
	    TextCompRaise
	    set cproject::compilationstatus 1
	} else {
	    set cproject::compilationstatus 0
	}
	return
    }
    gets $fin aa

    if { $aa != "" } {
	TextCompInsert $aa\n
	update
    }
}

proc RamDebugger::FindFilesWithPattern { dir patternlist recurse } {

    set retval [eval glob -nocomplain -dir [list $dir] -types f -- $patternlist]
    if { $recurse } {
	foreach i [glob -nocomplain -dir $dir -type d *] {
	    set retval [concat $retval [FindFilesWithPattern $i $patternlist $recurse]]
	}
    }
    return $retval
}

proc RamDebugger::SearchInFilesDo {} {
    variable options
    variable MainDir
    
#      if { $::tcl_platform(platform) == "windows" } {
#          set grep [file join $MainDir addons grep.exe]
#      } else { set grep grep }

    set comm "\"grep\" -ns "
    switch -- $::RamDebugger::searchmode {
	-exact { append comm "-F " }
	-regexp { append comm "-E " }
    }
    if { !$::RamDebugger::searchcase } {
	append comm "-i "
    }
    append comm "\"$RamDebugger::searchstring\" "

    set patternlist [regexp -inline -all {[^\s,;]+} $RamDebugger::searchextensions]

    set files [FindFilesWithPattern $RamDebugger::searchdir $patternlist \
	    $RamDebugger::searchrecursedirs]

    if { [llength $files] == 0 } {
	TextOutClear
	TextOutInsert "No files found"
	TextOutRaise
	return
    }

    append comm "\"[join $files {" "}]\""

    set fin [open "|$comm" r]

    set result ""
    while { ![eof $fin] } {
	append result [gets $fin]\n
    }
    set err [catch { close $fin } errstring]
    if { [string trim $result] == "" } { set result "Nothing found" }

    TextOutClear
    TextOutInsert $result
    TextOutRaise
    
    foreach "i j search" [list texts RamDebugger::searchstring lsearch \
	    exts RamDebugger::searchextensions lsearchfile \
	    dirs RamDebugger::searchdir lsearchfile] {
	set ipos [$search $options(SearchInFiles,$i) [set $j]]
	if { $ipos != -1 } {
	    set options(SearchInFiles,$i) [lreplace $options(SearchInFiles,$i) $ipos $ipos]
	}
	set options(SearchInFiles,$i) [linsert $options(SearchInFiles,$i) 0 [set $j]]
	if { [llength $options(SearchInFiles,$i)] > 6 } {
	    set options(SearchInFiles,$i) [lreplace $options(SearchInFiles,$i) 6 end]
	}
    }
}

proc RamDebugger::SearchInFiles {} {
    variable options
    variable text

    set range [$text tag ranges sel]
    if { $range != "" } {
	set txt [eval $text get $range]
    } else {
	set idx [$text index insert]
	if { $idx != "" } {
	    set txt ""
	    set idx0 $idx
	    while { [string is wordchar [$text get $idx0]] } {
		set txt [$text get $idx0]$txt
		set idx0 [$text index $idx0-1c]
		if { [$text compare $idx0 <= 1.0] } { break }
	    }
	    set idx1 [$text index $idx+1c]
	    while { [string is wordchar [$text get $idx1]] } {
		append txt [$text get $idx1]
		set idx1 [$text index $idx1+1c]
		if { [$text compare $idx1 >= end] } { break }
	    }
	} else { set txt "" }
    }
    
    set f [DialogWin::Init $text "Search in files" separator]
    set w [winfo toplevel $f]

    if { ![info exists options(SearchInFiles,texts)] } {
	set options(SearchInFiles,texts) ""
	set options(SearchInFiles,exts) ""
	set options(SearchInFiles,dirs) ""
    }

    label $f.l1 -text "Text:" -grid 0
    ComboBox $f.e1 -textvariable ::RamDebugger::searchstring -grid "1 2 px3 py3" \
	    -values $options(SearchInFiles,texts) -width 30

    if { [string trim $txt] != "" } {
	set ::RamDebugger::searchstring $txt
    } else { set ::RamDebugger::searchstring [lindex $options(SearchInFiles,texts) 0] }

    label $f.l2 -text "File ext:" -grid 0
    ComboBox $f.e2 -textvariable ::RamDebugger::searchextensions -grid "1 2 px3 py3" \
	    -values [concat $options(SearchInFiles,exts) [list "*.tcl" "*.h,*.cc,*.c"]]

    set ::RamDebugger::searchextensions [lindex [$f.e2 cget -values] 0]

    label $f.l3 -text "Directory:" -grid 0
    ComboBox $f.e3 -textvariable ::RamDebugger::searchdir -grid "1 1 px3 py3" \
	    -values $options(SearchInFiles,dirs)
    Button $f.b3 -image [Bitmap::get folder] -relief link -grid "2" 


    set comm {
	set initial $::RamDebugger::searchdir
	set ::RamDebugger::searchdir [tk_chooseDirectory   \
	    -initialdir $initial -parent PARENT \
	    -title "Search directory" -mustexist 1]
    }
    set comm [string map [list PARENT $w] $comm]
    $f.b3 configure -command $comm

    set ::RamDebugger::searchdir $options(defaultdir)

    set f1 [frame $f.f1 -bd 0 -grid "0 3 w"]
    set f2 [frame $f1.f2 -bd 1 -relief ridge -grid "0 w px3"]
    radiobutton $f2.r1 -text Exact -variable ::RamDebugger::searchmode \
	-value -exact -grid "0 w"
    radiobutton $f2.r2 -text Regexp -variable ::RamDebugger::searchmode \
	-value -regexp -grid "0 w"

    set f3 [frame $f1.f3 -grid "1 w"]
    checkbutton $f3.cb1 -text "Consider case" -variable ::RamDebugger::searchcase \
	-grid 0
    checkbutton $f3.cb2 -text "Recurse dirs" -variable ::RamDebugger::searchrecursedirs \
	-grid 0
   
    supergrid::go $f


    set ::RamDebugger::searchmode -exact
    set ::RamDebugger::searchcase 0
    set ::RamDebugger::searchrecursedirs 1

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
	    DialogWin::DestroyWindow
	    update
	    SearchInFilesDo
	    return
	}
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
	bind $w.search <FocusOut> "destroy $w.search ; break"
	bind $w.search <Escape> "destroy $w.search ; break"
	bind $w.search <KeyPress> [list if { [string is wordchar %A] || [string is punct %A] \
		                             || [string is space %A] } \
	    "tkEntryInsert $w.search %A ; break" else "destroy $w.search ; break"]
	bind $w.search <Delete> "$w.search delete insert ; break"
	bind $w.search <BackSpace> "tkEntryBackspace $w.search ; break"
	bind $w.search <1> "destroy $w.search ; break"
	bind $w.search <3> "destroy $w.search ; break"
	bind $w.search <Return> "destroy $w.search ; break"
	bind $w.search <Control-i> "RamDebugger::Search $w iforward ; break"
	bind $w.search <Control-r> "RamDebugger::Search $w ibackward ; break"
	bind $w.search <Control-g> "RamDebugger::Search $w stop ; break"

	trace var RamDebugger::searchstring w "RamDebugger::Search $w ;#"
	bind $w.search <Destroy> [list trace vdelete RamDebugger::searchstring w \
		                      "RamDebugger::Search $w ;#"]
	bind $w.search <Destroy> "+ [list bindtags $text [lreplace [bindtags $text] 0 0]] ; break"

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
	    set ed [$text cget -editable]
	     $text conf -editable 1
	    $text tag remove sel 1.0 end
	    if { $RamDebugger::SearchType == "-forwards" } {
		$text tag add sel $RamDebugger::SearchPos $idx2
	    } else { $text tag add sel $idx2 $RamDebugger::SearchPos }
	     $text conf -editable $ed
	    $text mark set insert $idx2
	    $text see $RamDebugger::SearchPos
	}
	set RamDebugger::Lastsearchstring $RamDebugger::searchstring
    }
}

# proc RamDebugger::OpenVisualRegexp {} {
#     variable MainDir

#     exec [info nameofexecutable] [file join $MainDir addons visual_regexp-2.2 visual_regexp.tcl] &
# }


proc RamDebugger::OpenProgram { what } {
    variable MainDir

    switch $what {
	visualregexp { set file [file join $MainDir addons visual_regexp-2.2 visual_regexp.tcl] }
	tkcvs { set file [file join $MainDir addons tkcvs bin tkcvs.tcl] }
	tkdiff { set file [file join $MainDir addons tkcvs bin tkdiff.tcl] }
    }
    if { [interp exists $what] } { interp delete $what }
    interp create $what
    interp alias $what exit "" interp delete $what
    $what eval [list load {} Tk]
    $what eval { set argc 0 ; set argv "" }
    $what eval [list source $file]
}

proc RamDebugger::OpenConsole {} {
    variable MainDir

    set tkcon [file join $MainDir addons tkcon tkcon.tcl]

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
    if { [winfo exists .tkcon] } { destroy .tkcon }
    set argv [list -rcfile "" -exec "" -root .tkcon -eval $tkconprefs]
    uplevel \#0 [list source $tkcon]
    uplevel \#0 ::tkcon::Init $argv
    proc ::tkcon::FinalExit { args } { destroy .tkcon }
}

proc RamDebugger::CutCopyPasteText { what } {
    variable text

    switch $what {
	undo {
	    $text undo
	}
	cut {
	    tk_textCut $text
	}
	copy {
	    tk_textCopy $text
	}
	paste {
	    tk_textPaste $text
	}
    }
}


proc RamDebugger::ListBoxDouble1 { listb item } {
    variable options

    set data [$listb itemcget $item -data]
    if { [lindex $data 0] == "folder" } {
	set options(defaultdir) [lindex $data 1]
	FillListBox
    } else {
	OpenFileF [lindex $data 1]
    }
}

proc RamDebugger::ListboxMenu { listb x y item } {

    set data [$listb itemcget $item -data]

    catch { destroy $listb.menu }
    set menu [menu $listb.menu]

    if { [lindex $data 0] == "folder" } {
	$menu add command -label Explore -command [concat [list set \
		RamDebugger::options(defaultdir) \
	    [lindex $data 1]] \; RamDebugger::FillListBox]
    } else {
	set name [lindex $data 1]
	$menu add command -label Open -command [list RamDebugger::OpenFileF $name]
	$menu add command -label Reinstrument -command [list RamDebugger::OpenFileF \
		$name 2]
	# by now, disconnected on windows
	#if { $::tcl_platform(platform) != "windows" } {
	    if { [regexp {\.(c|cc)$} $name] } {
		$menu add separator
		$menu add command -label Compile -command [list RamDebugger::Compile $name]
	    }
	    if { [regexp {Makefil.*[^~]$} $name] } {
		$menu add separator
		$menu add command -label Compile -command [list RamDebugger::Compile $name]
	    }
	#}
    }
    tk_popup $menu $x $y
}


proc RamDebugger::SearchInListbox { listb ev char exec_callback } {
    variable SearchListboxString

    if { $ev == "Up" || $ev == "Down" } {
	set sel [$listb selection get]
	if { $sel != "" } {
	    set idx [$listb index $sel]
	} else { set idx 0 }
	$listb selection clear
	set idxend [$listb index [$listb items end]]
	if { $ev == "Up" } {
	    incr idx -1
	    if { $idx < 0 } { set idx $idxend }
	} else {
	    incr idx 
	    if { $idx > $idxend } { set idx 0 }
	}
	$listb selection set [$listb items $idx]
	$listb see [$listb items $idx]
	return
    } elseif { $ev == "Home" } {
	$listb selection clear
	$listb selection set [$listb items 0]
	$listb see [$listb items 0]
    } elseif { $ev == "End" } {
	$listb selection clear
	$listb selection set [$listb items end]
	$listb see [$listb items end]
	return
    } elseif { $ev == "Return" } {
	uplevel \#0 [list $exec_callback $listb [$listb selection get]]
	return
    } elseif { $ev == "BackSpace" } {
	uplevel \#0 [list $exec_callback $listb [$listb items 0]]
    }
    if { [string is wordchar -strict $char] || [string is punct -strict $char] \
	     || [string is space -strict $char] } {
	if { ![info exists SearchListboxString] || [string index $SearchListboxString end] != $char } {
	    append SearchListboxString $char
	}
	set idx [$listb selection get]
	if { [llength $idx] != 1 } {
	    set idx 0
	} else {
	    set idx [$listb index $idx]
	    incr idx
	}
	set found 0
	foreach i [$listb items $idx end] {
	    if { [string match -nocase $SearchListboxString* [$listb itemcget $i -text]] } {
		$listb selection set $i
		$listb see $i
		set found 1
		break
	    }
	}
	if { !$found } {
	    foreach i [$listb items 0 [expr $idx-1]] {
		if { [string match -nocase $SearchListboxString* [$listb itemcget $i -text]] } {
		    $listb selection set $i
		    $listb see $i
		    set found 1
		    break
		}
	    }
	}
	if { !$found } {
	    bell
	    set RamDebugger::SearchListboxString ""
	} else {
	    after 300 [list set RamDebugger::SearchListboxString ""]
	}
    }
    $listb xview moveto 0
}

proc RamDebugger::ListBoxEvents { listb exec_callback menu_callback } {

    $listb bindImage <1> "focus $listb ; $listb selection set"
    $listb bindText <1> "focus $listb ; $listb selection set"
    $listb bindImage <3> "focus $listb ; $listb selection set"
    $listb bindText <3> "focus $listb ; $listb selection set"
    $listb bindImage <ButtonRelease-3> "$menu_callback $listb %X %Y"
    $listb bindText <ButtonRelease-3> "$menu_callback $listb %X %Y"
    $listb bindImage <Double-1> "$exec_callback $listb"
    $listb bindText <Double-1> "$exec_callback $listb"
    bind $listb <KeyPress> [list RamDebugger::SearchInListbox $listb %K %A \
	    $exec_callback]


}

proc RamDebugger::UndoCallback {} {
    variable text
    variable currentfileIsModified

    wm title [winfo toplevel $text] [string trimright [wm title [winfo toplevel $text]] *]
    set currentfileIsModified 0
    bell
}

proc RamDebugger::CheckTextBefore { command args } {
    variable text
    variable CheckTextSave

    if { ![regexp {^(ins|del)} $command] } { return }

    switch -glob -- $command {
	ins* {
	    set idx1 [$text index [lindex $args 0]]
	    set txt [lindex $args 1]
	    set l1 [scan $idx1 %d]
	    set l2 [expr $l1+[regexp -all {\n} $txt]]
	}
	del* {
	    set idx1 [$text index [lindex $args 0]]
	    set idx2 [lindex $args 1]
	    if { $idx2 == "" } {
		set idx2 [$text index "$idx1+1c"]
	    } else {
		set idx2 [$text index $idx2]
	    }
	    set txt [$text get $idx1 $idx2]
	    set l1 [scan $idx1 %d]
	    set l2 [scan $idx2 %d]
	}
    }
    set Numlines [scan [$text index end-1c] %d]
    set CheckTextSave [list $l1 $l2 $txt $Numlines]
}

proc RamDebugger::CheckText { command args } {
    variable marker
    variable text
    variable instrumentedfilesP
    variable instrumentedfilesR
    variable instrumentedfilesInfo
    variable currentfile
    variable CheckTextSave

    foreach "l1 l2 txt NumlinesOld" $CheckTextSave break
    if { $txt == "" } { return }

    if { [regexp {\.(h|c|cc)$} $currentfile] } {
	set filetype c++
    } else {
	set filetype tcl
    }

    switch -glob -- $command {
	ins* {
	    set l1_old $l1
	    set l2_old $l1
	    set l1_new $l1
	    set l2_new $l2
	}
	del* {
	    set l1_old $l1
	    set l2_old $l2
	    set l1_new $l1
	    set l2_new $l1
	}
    }
    set Numlines [scan [$text index end-1c] %d]
    set font [$text cget -font]
    $marker configure -scrollregion [list 0 0 [winfo reqwidth $marker] \
	[expr $Numlines*[font metrics $font -linespace]]]

    set diff [expr $l2-$l1]

    while { $l1_old > 1 && [lindex [lindex $instrumentedfilesInfo($currentfile) \
	[expr $l1_old-1]] 1] != "n" } {
	incr l1_new -1
	incr l1_old -1
    }
    while { $l2_old < $NumlinesOld } {
	incr l2_new
	incr l2_old
	set state_curr [lindex [lindex $instrumentedfilesInfo($currentfile) [expr $l2_old-1]] 1]
	if { $l2_old <= $NumlinesOld } {
	    set state_next [lindex [lindex $instrumentedfilesInfo($currentfile) $l2_old] 1]
	} else { set state_next "" }
	if { $state_curr == "n" && $state_next != "c" } { break }
    }

    set oldlevel [lindex [lindex $instrumentedfilesInfo($currentfile) [expr $l1_old-1]] 0]
  
    set block [$text get $l1_new.0 "$l2_new.0 lineend"]
    set blockinfo ""
    switch $filetype {
	tcl {
	    set err [catch { Instrumenter::DoWork $block 0 newblockP newblockR blockinfo } errstring]
	}
	c++ {
	    set err [catch { Instrumenter::DoWorkForC++ $block blockinfo $oldlevel } errstring]
	    set oldlevel 0
	}
    }


    set blockinfo2 ""
    foreach i $blockinfo {
	lappend blockinfo2 [concat [expr $oldlevel+[lindex $i 0]] [lrange $i 1 end]]
    }

    set instrumentedfilesInfo($currentfile) [eval lreplace [list $instrumentedfilesInfo($currentfile)] \
	[expr $l1_old-1] [expr $l2_old-1] $blockinfo2]

    ColorizeLines $l1_new $l2_new

    if { [info exists instrumentedfilesP($currentfile)] } {
	unset instrumentedfilesP($currentfile) instrumentedfilesR($currentfile)
    }
    if { [info exists instrumentedfilesTime($currentfile)] } {
	unset instrumentedfilesTime($currentfile)
    }
}

proc RamDebugger::SearchBraces { x y } {
    variable text

    set sel [$text get insert]
    $text tag remove sel 0.0 end
    $text tag add sel insert insert+1c
    if { [lsearch -exact [list \[ \] \{ \}] $sel] == -1 } {
	set sel [$text get insert-1c]
	$text tag remove sel 0.0 end
	$text mark set insert insert-1c
	$text tag add sel insert insert+1c
    }
    if {[lsearch -exact [list \[ \] \{ \}] $sel] == -1 } {
	set ::tkPriv(selectMode) word
	tkTextSelectTo $text $x $y
	catch { $text mark set insert sel.last}
	catch { $text mark set anchor sel.first}
    } else {
	if { $sel == "\[" || $sel == "\{" } {
	    set dir -forwards
	    set stopindex [$text index end]
	    set idx [$text index sel.last]
	    set incr +1
	} else {
	    set dir -backwards
	    set stopindex 1.0
	    set idx [$text index sel.first]
	    set incr -1
	    $text mark set insert insert+1c
	}
	switch $sel {
	    "\{" { set open "\{" ; set close "\}" ; set openalt "\[" ; set closealt "\]" }
	    "\[" { set open "\[" ; set close "\]" ; set openalt "\{" ; set closealt "\}" }
	    "\}" { set open "\}" ; set close "\{" ; set openalt "\]" ; set closealt "\[" }
	    "\]" { set open "\]" ; set close "\[" ; set openalt "\}" ; set closealt "\{" }
	}
	set error 0
	set found 0
	set level 0
	set level_alt 0
	set idx_alt ""
	while { [set idx2 [$text search $dir -regexp -- {\{|\}|\[|\]} $idx $stopindex]] != "" } {
	    if { [$text get "$idx2-1c"] == "\\" && [$text get "$idx2-2c"] != "\\" } {
		if { $dir == "-forwards" } {
		    set idx [$text index $idx2+1c]
		} else { set idx $idx2 }
		continue
	    }
	    set newsel [$text get $idx2]
	    switch $newsel \
		$open { incr level } \
		$openalt {
		    incr level_alt
		    set idx_alt $idx2
		} \
		$close {
		    incr level -1
		    if { $level < 0 } {
#                         if { $level_alt > 0 } {
#                             set error 1
#                             set idx2 $idx_alt
#                             break
#                         }
		        set found 1
		        break
		    }
		} \
		$closealt {
		    incr level_alt -1
		    if { $level_alt < 0 } {
		        set level_alt 0
		        #set error 1
		        #break
		    }
		}
	    if { $dir == "-forwards" } {
		set idx [$text index $idx2+1c]
	    } else { set idx $idx2 }
	}
	if { $idx2 == "" } {
	    set error 1
	    set idx2 $stopindex
	}
	if { $error } { bell }
	$text tag remove sel 0.0 end
	
	if { $dir == "-forwards" } {
	    $text tag add sel insert $idx2+1c
	} else {
	    $text tag add sel $idx2 insert
	} 
	$text see $idx2
    }
}

proc RamDebugger::CenterDisplay {} {
    variable text

    scan [$text index insert] "%d" line
    set NumLines [scan [$text index end-1c] %d]

    foreach "f1 f2" [$text yview] break
    set ys [expr $line/double($NumLines)-($f2-$f1)/2.0]
    if { $ys < 0 } { set ys 0 }
    $text yview moveto $ys
}

proc RamDebugger::CommentSelection { what } {
    variable text
    variable currentfile

    if { [regexp {\.(h|c|cc)$} $currentfile] } {
	set commentchar "//"
    } else {
	set commentchar "#"
    }
    if { [catch {
	scan [$text index sel.first] "%d" line1
	scan [$text index sel.last] "%d" line2
    }] } {
	scan [$text index insert] "%d" line1
	set line2 $line1
	#WarnWin "Select something first" $text
	#return
    }
    switch $what {
	on {
	    for { set i $line1 } { $i <= $line2 } { incr i } {
		$text insert $i.0 "$commentchar "
	    }
	}
	off {
	    for { set i $line1 } { $i <= $line2 } { incr i } {
		set line [$text get $i.0 "$i.0 lineend"]
		regsub "^\\s*$commentchar\\s?" $line {} line
		$text delete $i.0 "$i.0 lineend"
		$text insert $i.0 $line
	    }
	}
    }
}

proc RamDebugger::IndentCurrent {} {
    variable text

    scan [$text index insert] "%d.%d" line pos
    IndentLine $line $pos
}

proc RamDebugger::IndentSelection {} {
    variable text

    if { [catch {
	scan [$text index sel.first] "%d" line1
	scan [$text index sel.last] "%d" line2
    }] } {
	WarnWin "Select something first" $text
	return
    }
    for { set i $line1 } { $i <= $line2 } { incr i } {
	IndentLine $i
    }
}

proc RamDebugger::IndentLine { line { pos -1 } } {
    variable text
    variable instrumentedfilesInfo
    variable currentfile
    variable options

    if { [regexp {\.(h|c|cc)$} $currentfile] } {
	set filetype c++
	set indent_val $options(indentsizeC++)
    } else {
	set filetype tcl
	set indent_val $options(indentsizeTCL)
    }

    set level 0
    set type ""
    foreach "level type" [lindex $instrumentedfilesInfo($currentfile) [expr $line-1]] break

    switch $type {
	n { set indent [expr $level*$indent_val] }
	c { set indent [expr $level*$indent_val+$indent_val] }
	"" { set indent 0 }
    }
    foreach "- col" [scan [$text index "$line.0 lineend"] %d.%d] break
    set FirstPos 0
    set FirstChar ""
    for { set i 0 } { $i < $col } { incr i } {
	if { [$text get $line.$i] != " " } {
	    set FirstPos $i
	    set FirstChar [$text get $line.$i]
	    break
	}
    }
    if { $filetype == "tcl" && $FirstChar == "\#" } {
	set indent 0
    } elseif { $filetype == "c++" && $FirstChar == "\{" && $type == "c" } {
	set indent [expr $indent-$indent_val]
    } elseif { $FirstChar == "\}" && $indent >= $indent_val } {
	set indent [expr $indent-$indent_val]
    }
    if { $FirstPos < $indent } {
	$text insert $line.0 [string repeat " " [expr $indent-$FirstPos]]
    } elseif { $FirstPos > $indent } {
	$text delete $line.0 $line.[expr $FirstPos-$indent]        
    }
    if { $pos >= 0 && $pos < $indent } { $text mark set insert $line.$indent }
}

proc RamDebugger::UpdateLineNum { command args } {
    variable LineNum
    variable text
    variable filesmtime
    variable currentfile
    variable currentfileIsModified

    if { $command == "index" } { return }

    if { [regexp {^(ins|del)} $command] } { CheckText $command $args }

    if { [regexp {^(ins|del)} $command] && !$currentfileIsModified } {
	wm title [winfo toplevel $text] [wm title [winfo toplevel $text]]*
	set currentfileIsModified 1
    }

    set idx [$text index insert]
    set line ""
    foreach "line col" [scan $idx "%d,%d"] break
    if { $line == "" } { return }
    set LineNum "L: $line"

    if { $currentfile != "" && $currentfile != "New file" &&  [file exists $currentfile] && \
	[file mtime $currentfile] > $filesmtime($currentfile) } {
	set filesmtime($currentfile) [file mtime $currentfile]

	if { $currentfileIsModified } {
	    set quest "File '$currentfile' has been modified outside RamDebugger. Reload it "
	    append quest "and loose the changes made inside RamDebugger?"
	} else {
	    set quest "File '$currentfile' has been modified outside RamDebugger. Reload it?"
	}
	set ret [DialogWin::messageBox -default ok -icon warning -message $quest \
	    -parent $text -title "reload file" -type okcancel]
	if { $ret == "ok" } {
	    OpenFileF $currentfile 1
	}
    }
}

proc RamDebugger::CreatePanedEntries { num pane1 pane2 suffix } {
    variable EvalEntries

    set panew $pane1
    while 1 {
	set panew [winfo parent $panew]
	if { [winfo class $panew] == "PanedWindow" } { break }
    }

    for { set i 0 } { $i < $num } { incr i } {
	if { [winfo exists $pane1.e$i] } { continue }

	set EvalEntries($i,leftentry$suffix) [entry $pane1.e$i -textvariable \
	    RamDebugger::EvalEntries($i,left$suffix) -bd 0 \
	    -highlightthickness 1 -highlightbackground grey90 -bg white]
	set EvalEntries($i,rightentry$suffix) [entry $pane2.e$i -textvariable \
	    RamDebugger::EvalEntries($i,right$suffix) -bd 0 \
	    -highlightthickness 1 -highlightbackground grey90 -bg white]

	catch {
	    $pane1.e$i conf -disabledbackground white
	    $pane2.e$i conf -disabledbackground white
	}
	grid $EvalEntries($i,leftentry$suffix) -sticky ew
	grid $EvalEntries($i,rightentry$suffix) -sticky ew

	if { $suffix == "" } {
	    set comm {
		set w $pane1.e[expr $i+1]
		if { ![winfo exists $w] } {
		    RamDebugger::CreatePanedEntries [expr $i+2] $pane1 $pane2 {$suffix}
		}
		tkTabToWindow $w
	    }
	    set comm [string map [list \$i $i \$pane1 $pane1 \$pane2 $pane2 \
		\$suffix $suffix] $comm]

	    bind $pane1.e$i <Return> $comm
	    bind $pane1.e$i <FocusOut> "RamDebugger::CheckEvalEntries$suffix do $i,left$suffix"
	    bind $pane1.e$i <ButtonRelease-1> {
		%W selection range 0 end
		%W icursor end
	    }
	    bind $pane1.e$i <Down> {tkTabToWindow [tk_focusNext %W]}
	    bind $pane1.e$i <Up> {tkTabToWindow [tk_focusPrev %W]}

	} else { $EvalEntries($i,leftentry$suffix) configure -state disabled }

	set comm {
	    set w $pane2.e[expr $i+1]
	    if { ![winfo exists $w] } {
		RamDebugger::CreatePanedEntries [expr $i+2] $pane1 $pane2 {$suffix}
	    }
	    tkTabToWindow $w
	}
	set comm [string map [list \$i $i \$pane1 $pane1 \$pane2 $pane2 \
	    \$suffix $suffix] $comm]

	bind $pane2.e$i <Return> $comm

	bind $pane2.e$i <FocusOut> "RamDebugger::CheckEvalEntries$suffix do $i,right$suffix"
	bind $pane2.e$i <ButtonRelease-1> {
	    %W selection range 0 end
	    %W icursor end
	}
	bind $pane2.e$i <Down> {tkTabToWindow [tk_focusNext %W]}
	bind $pane2.e$i <Up> {tkTabToWindow [tk_focusPrev %W]}
    }
    while { [winfo exists $pane1.e$i] } {
	destroy $pane1.e$i $pane2.e$i
	unset EvalEntries($i,leftentry$suffix) EvalEntries($i,rightentry$suffix)
	unset EvalEntries($i,left$suffix) EvalEntries($i,right$suffix)
	incr i
    }

    grid columnconf $pane1 0 -weight 1
    grid columnconf $pane2 0 -weight 1

    # dirty trick to make the paned window actualize sizes
    after idle event generate $panew <Configure>
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
    image create photo filenew16 -data {
       R0lGODlhEAAQAIUAAPwCBFxaXNze3Ly2rJyanPz+/Ozq7GxqbPz6/GxubNTK
       xDQyNIyKhHRydERCROTi3PT29Pz29Pzy7PTq3My2pPzu5PTi1NS+rPTq5PTe
       zMyynPTm1Pz69OzWvMyqjPTu5PTm3OzOtOzGrMSehNTCtNS+tAAAAAAAAAAA
       AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
       AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACH5BAEAAAAALAAAAAAQABAAAAZ/
       QAAgQCwWhUhhQMBkDgKEQFIpKFgLhgMiOl1eC4iEYrtIer+MxsFRRgYe3wLk
       MWC0qXE5/T6sfiMSExR8Z1YRFRMWF4RwYIcYFhkahH6AGBuRk2YCCBwSFZgd
       HR6UgB8gkR0hpJsSGCAZoiEiI4QKtyQlFBQeHrVmC8HCw21+QQAh/mhDcmVh
       dGVkIGJ5IEJNUFRvR0lGIFBybyB2ZXJzaW9uIDIuNQ0KqSBEZXZlbENvciAx
       OTk3LDE5OTguIEFsbCByaWdodHMgcmVzZXJ2ZWQuDQpodHRwOi8vd3d3LmRl
       dmVsY29yLmNvbQA7
    }
    
    image create photo fileopen16 -data {
       R0lGODlhEAAQAIUAAPwCBAQCBOSmZPzSnPzChPzGhPyuZEwyHExOTFROTFxa
       VFRSTMSGTPT29Ozu7Nze3NTS1MzKzMTGxLy6vLS2tLSytDQyNOTm5OTi5Ly+
       vKyqrKSmpIyOjLR+RNTW1MzOzJyenGxqZBweHKSinJSWlExKTMTCxKyurGxu
       bBQSFAwKDJyanERCRERGRAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
       AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACH5BAEAAAAALAAAAAAQABAAAAaR
       QIBwGCgGhkhkEWA8HpNPojFJFU6ryitTiw0IBgRBkxsYFAiGtDodDZwPCERC
       EV8sEk0CI9FoOB4BEBESExQVFgEEBw8PFxcYEBIZGhscCEwdCxAPGA8eHxkU
       GyAhIkwHEREQqxEZExUjJCVWCBAZJhEmGRUnoygpQioZGxsnxsQrHByzQiJx
       z3EsLSwWpkJ+QQAh/mhDcmVhdGVkIGJ5IEJNUFRvR0lGIFBybyB2ZXJzaW9u
       IDIuNQ0KqSBEZXZlbENvciAxOTk3LDE5OTguIEFsbCByaWdodHMgcmVzZXJ2
       ZWQuDQpodHRwOi8vd3d3LmRldmVsY29yLmNvbQA7
    }
    image create photo folderopen16 -data {
       R0lGODlhEAAQAIYAAPwCBAQCBExKTBQWFOzi1Ozq7ERCRCwqLPz+/PT29Ozu
       7OTm5FRSVHRydIR+fISCfMTCvAQ6XARqnJSKfIx6XPz6/MzKxJTa9Mzq9JzO
       5PTy7OzizJSOhIyCdOTi5Dy65FTC7HS2zMzm7OTSvNTCnIRyVNza3Dw+PASq
       5BSGrFyqzMyyjMzOzAR+zBRejBxqnBx+rHRmTPTy9IyqvDRylFxaXNze3DRu
       jAQ2VLSyrDQ2NNTW1NTS1AQ6VJyenGxqbMTGxLy6vGRiZKyurKyqrKSmpDw6
       PDw6NAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
       AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
       AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
       AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACH5BAEAAAAA
       LAAAAAAQABAAAAfCgACCAAECg4eIAAMEBQYCB4mHAQgJCgsLDAEGDQGIkw4P
       BQkJBYwQnRESEREIoRMUE6IVChYGERcYGaoRGhsbHBQdHgu2HyAhGSK6qxsj
       JCUmJwARKCkpKsjKqislLNIRLS4vLykw2MkRMRAGhDIJMzTiLzDXETUQ0gAG
       CgU2HjM35N3AkYMdAB0EbCjcwcPCDBguevjIR0jHDwgWLACBECRIBB8GJekQ
       MiRIjhxEIlBMFOBADR9FIhiJ5OnAEQB+AgEAIf5oQ3JlYXRlZCBieSBCTVBU
       b0dJRiBQcm8gdmVyc2lvbiAyLjUNCqkgRGV2ZWxDb3IgMTk5NywxOTk4LiBB
       bGwgcmlnaHRzIHJlc2VydmVkLg0KaHR0cDovL3d3dy5kZXZlbGNvci5jb20A
       Ow==
    }

    image create photo filesave16 -data {
       R0lGODlhEAAQAIUAAPwCBAQCBFRSVMTCxKyurPz+/JSWlFRWVJyenKSipJSS
       lOzu7ISChISGhIyOjHR2dJyanIyKjHx6fMzOzGRiZAQGBFxeXGRmZHRydGxq
       bAwODOTm5ExOTERGRExKTHx+fGxubNza3Dw+PDQ2NAAAAAAAAAAAAAAAAAAA
       AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
       AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACH5BAEAAAAALAAAAAAQABAAAAaA
       QIAQECgOj0jBgFAoBpBHpaFAbRqRh0F1a30ClAhuNZHwZhViqgFhJizSjIZX
       QCAoHOKHYw5xRBiAElQTFAoVQgINFBYXGBkZFxYHGRqIDBQbmRwdHgKeH2Yg
       HpmkIR0HAhFeTqSZIhwCFIdIrBsjAgcPXlBERZ4Gu7xCRZVDfkEAIf5oQ3Jl
       YXRlZCBieSBCTVBUb0dJRiBQcm8gdmVyc2lvbiAyLjUNCqkgRGV2ZWxDb3Ig
       MTk5NywxOTk4LiBBbGwgcmlnaHRzIHJlc2VydmVkLg0KaHR0cDovL3d3dy5k
       ZXZlbGNvci5jb20AOw==
    }

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
    image create photo filenew22 -data {
	R0lGODlhFgAWAIUAAPwCBExOTERCRDw6PCwuLBwaHAwODAQCBOze1NTW1OTi
	5Nze3MTGxLS2tJyanPz+/Ozu7OTi3BQSFCwqLDw+PDQyNFRSVPTu7MzKxLyy
	rIR+fCQmJPz6/NTOxPz69Pzy7PTu5Pz29Pzu5PTq5PTm1My6pBQWFPTq3PTm
	3NS+rAwKDPTi1PTezOzWxMy2pPz27PTazOzSvMyynOzaxOzOtPTaxOzKrMyq
	jOzGpMymhPTizOTCpNzSzNTGvMymjMSihCH5BAEAAAAALAAAAAAWABYAAAbo
	QIBwSCwaiYGAYEAgFAqGg/Q4DCASCsTiymgcHAcqQLB4mM+QiIQBppLPcMjk
	wQ4bB2X4maKgt4sVCHpnFhQTElNFE3mDDxcYGRp2RBuMgxwIHX9EBZZwHh8g
	CBmTQ52NISEiIyQlpUImng8hHyInKAgprwAqgnC0IKwrLLpGB4wctLYkwy0u
	uwd9Z8AnJywsLcVFx2YcL7UnJCwwLTEy0GXJoSgrCCwzNDTnxgjeH9UrKzXw
	NDY36LRGhEOwLx4NHDmgJbh3QoeOgv127EhojEeHDj16pEhRQoZHHzl+QJNC
	sqTJSXaCAAAh/mhDcmVhdGVkIGJ5IEJNUFRvR0lGIFBybyB2ZXJzaW9uIDIu
	NQ0KqSBEZXZlbENvciAxOTk3LDE5OTguIEFsbCByaWdodHMgcmVzZXJ2ZWQu
	DQpodHRwOi8vd3d3LmRldmVsY29yLmNvbQA7
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
    image create photo filesave22 -data {
	R0lGODlhFgAWAIUAAPwCBGxqbAQCBLy+vERCRExKTHRydIyKjMTCxFxaXGRi
	ZFRSVFRWVPz6/Nze3Nzm5Pz+/JyanDw+PExOTHR2dMTGxBQWFLSytHx+fISC
	hOzy9Ly6vAQGBJSWlMzKzAwODJSSlHx6fIyOjOTi5DQ2NISGhGxubCwuLOzq
	7ERGRFxeXNTW1CwqLPT29Dw6PGRmZKSmpAAAAAAAAAAAAAAAAAAAAAAAAAAA
	AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACH5BAEAAAAALAAAAAAWABYAAAb/
	QIBQGBAMj8ikUDAgFAzKKCBwQCQUCcICKh0SEAhGw5EIZAmBrgCxeDQgcDJW
	yz0GIggJfL+XGwQJRxNgC3yGDwwUFUZDFhdthnwMGAZNQwEZFwQakXANBBQb
	HIIdERIBnRAOiR4ERx8gsSEMBBmGCyEGG3YGBwcgIr8UCwQHECOgG4xCtRkE
	JAvBJRklJgkSFBQeJ68hJiEoESkFKiEZIbkGARsLlwEGExENGhorGSkpFAYm
	66NDLAECpGiBYsUIFA8wLHBBQMWLVkdUCFCwaYVFBOymkVCgYEMgOykEpICB
	ccMBAhhELFigTEqAAgIIwCiQ4eRKDyS6EAlJIAI0EpaudF4iIKDAAn9CkRT5
	eMROEAAh/mhDcmVhdGVkIGJ5IEJNUFRvR0lGIFBybyB2ZXJzaW9uIDIuNQ0K
	qSBEZXZlbENvciAxOTk3LDE5OTguIEFsbCByaWdodHMgcmVzZXJ2ZWQuDQpo
	dHRwOi8vd3d3LmRldmVsY29yLmNvbQA7
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
    image create photo actundo22 -data {
	R0lGODlhFgAWAIUAAPwCBCReDJzGjMzivOTu3PT69MTivHy+VJTWbIzKZEym
	JESmFESiHDyiHESqLAQCBFzKNGzWPFS2LNTmzCxqDDRqHPz+/KTGnBQqBAQO
	BAwaBESCHHy2XBxGDOzy7HTCTEyyJDSqFHzWTAwSBBQ6BIy+dESKJFySPFSS
	NAwiBCRGFBQmDAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
	AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACH5BAEAAAAALAAAAAAWABYAAAal
	QIBwSCwaj8ikMsBkKotMwYAwEDiXgYLhwD0gCFZiQKxNKBYMRqPh+D6G16y5
	AYnYIxBJAyF4AwITTAUJdBESD4gPFBV6Fn6ABBcJDIYPGEQZGhQbHAIdfx4J
	Hw2VSBodGwWfAR4LDSALfkgYAQurBiAhICKfSSMkvQElGyYnGyi9Rxkdj4nO
	skUYyU9FpxnURikdGtjRKivdRKfQ2Inh5+jpRwZBACH+aENyZWF0ZWQgYnkg
	Qk1QVG9HSUYgUHJvIHZlcnNpb24gMi41DQqpIERldmVsQ29yIDE5OTcsMTk5
	OC4gQWxsIHJpZ2h0cyByZXNlcnZlZC4NCmh0dHA6Ly93d3cuZGV2ZWxjb3Iu
	Y29tADs=
    }
    image create photo editcut22 -data {
	R0lGODlhFgAWAIMAAPwCBAQCBAwCBPz+/OTi5JyanOzq7DQyNGxqbAAAAAAA
	AAAAAAAAAAAAAAAAAAAAACH5BAEAAAAALAAAAAAWABYAAARbEMhJq704gxBE
	0Bf3cZo4kRJqBQNRfBucyudgvJS6VaxLzyMa6/bLiWA9HOg4VIIkL5vzuRkc
	pkvRIIAorphJLzBW84WEuRZWp6uaT7J2Sh1Hit3OY/ZO7WvsEQAh/mhDcmVh
	dGVkIGJ5IEJNUFRvR0lGIFBybyB2ZXJzaW9uIDIuNQ0KqSBEZXZlbENvciAx
	OTk3LDE5OTguIEFsbCByaWdodHMgcmVzZXJ2ZWQuDQpodHRwOi8vd3d3LmRl
	dmVsY29yLmNvbQA7
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
    image create photo editpaste22 -data {
	R0lGODlhFgAWAIYAAPwCBBQWFDw6FHRuFGRaBFxSBAQCBAQKBCQiBIx6HPz6
	/NTOfKyiXDQuFOTm5Pz+/Ozu7PTq5Pz63PTyxNTOjKSeRExGLMTGxMzKzNTS
	1NTW1Dw2NKSmpKyqrKSipJyanNzWlLy6ZLSuVIx6FISChIyKhJSSlCQiJLS2
	tDw6NDQyNCQiFCQmHBQSDGRiZHRydGxubHx6dGxqbFxeXGRmZFxaXCwuLOzq
	7KyurHx+fDwmFEQuFCweFCQWDBQODBwaHBweHKSinJSWlOTi5JyepHR2dDw6
	PBQSFNze3ERGRIyKjIyOjISGhPz29Pzy7MS2rMzOzFRWVHx2dHxybDQiFPz2
	7Pzu5PTq3PTm1NTCtJyGdHxuZHxqXPzq3PTaxNS6pFxWVFRKRNS2nPTi1PTS
	tNSulNzOxNSynMymhAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
	AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACH5BAEAAAAA
	LAAAAAAWABYAAAf/gACCgwABAgMEBYSLggaOjgcICQoLDA2Pj4MGDg8QEZ4F
	DxITFBUWFxcYGRobjQ8cHR4fCQ8gCyEiFSMWJCUkJieNEB4dKB4pKissK8wr
	LS4vMDHBAAYQHx8dFx0fJDIzNDU0M+IyHzaNNyg43Ng5Ojs7Ojw9Pj9AMkCN
	DiZB/h9CSOx4QLCgihItqBkYgqIDESElitAYWJCgkQcXjjRCgi1Ihw4BB5LA
	QOLCgyQYHihpUU3DBw5ElpAgAYNixSRJjKjQaECDCRPZPDB5IbIGSQwKLnh4
	wbInLA4kmJB4oaPiAwVNnER40hRK1BIAaVatUZJEFCkmpmjgCeWDCalFe4q4
	oFKwSRUrEa5gycLzwq8lUnPQ4PEgSpYcUZ5o2cIlS1O/JHLEDdfjQZMIVrpg
	weLFy5e+M6WSmBGlxYMYYBRzCaOFi5imHWBIfOEiShLTVjaP6eyFTBmN1TA5
	OvLDjJksWb58OVMGDRqWjAYdmU79SIvpjqJr104nEAAh/mhDcmVhdGVkIGJ5
	IEJNUFRvR0lGIFBybyB2ZXJzaW9uIDIuNQ0KqSBEZXZlbENvciAxOTk3LDE5
	OTguIEFsbCByaWdodHMgcmVzZXJ2ZWQuDQpodHRwOi8vd3d3LmRldmVsY29y
	LmNvbQA7
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

proc RamDebugger::TkBackCompatibility {} {

    set comms [list tkButtonInvoke tkTextSelectTo tkEntryInsert tkEntryBackspace \
	    tk_textCut tk_textCopy tk_textPaste tk_focusNext tk_focusPrev tkTextClosestGap \
	    tkTextAutoScan tkCancelRepeat]

    foreach i $comms {
	auto_load $i
	if {![llength [info commands $i]]} {
	    tk::unsupported::ExposePrivateCommand $i
	}
    }
}

proc RamDebugger::InitGUI { { w .gui } } {
    variable options
    variable options_def
    variable marker
    variable text
    variable mainframe
    variable listbox
    variable pane2in1
    variable images
    variable textST
    variable textOUT
    variable textCOMP
    variable breakpoints
    variable MainDir

    proc ::bgerror { errstring } {
	RamDebugger::TextOutRaise
	RamDebugger::TextOutInsertRed "-------------ERROR FROM RAMDEBUGGER-----------\n"
	RamDebugger::TextOutInsertRed $::errorInfo
	RamDebugger::TextOutInsertRed "----------------------------------------------\n"
	WarnWin $errstring
    }

    package require Tablelist
    package require BWidgetR
    package require supertext
    package require supergrid
    package require dialogwin
    package require helpviewer

    CreateImages
    TkBackCompatibility
    CreateModifyFonts

    option add *Menu*TearOff 0

    toplevel $w
    wm title $w RamDebugger
    wm protocol $w WM_DELETE_WINDOW "RamDebugger::ExitGUI"

    set descmenu [list \
		"&File" all file 0 [list \
		[list command "&New file" {} "Begin new file" "" \
		-command "RamDebugger::NewFile"] \
		[list command "&Open file" {} "Select source file" "Ctrl o" \
		-command "RamDebugger::OpenFile"] \
		[list command "&Save" {} "Save file" "Ctrl s" \
		-command "RamDebugger::SaveFile save"] \
		[list command "Save &as" {} "Save file as" "" \
		-command "RamDebugger::SaveFile saveas"] \
		separator \
		[list cascad "&Debug on" {} activeprograms 0 {}] \
		separator \
		[list cascad "&Recent files" {} recentfiles 0 {}] \
		separator \
		[list command "&Quit" {} "Exit program" "Ctrl q" \
		-command RamDebugger::ExitGUI] \
		] \
		"&Edit" all edit 0 [list \
		[list command "&Undo" {} "Undo previus insert/delete operation" "Ctrl z" \
		-command "RamDebugger::CutCopyPasteText undo"] \
		separator \
		[list command "&Cut" {} "Cut selected text to clipboard" "Ctrl x" \
		-command "RamDebugger::CutCopyPasteText cut"] \
		[list command "C&opy" {} "Copy selected text to clipboard" "Ctrl c" \
		-command "RamDebugger::CutCopyPasteText copy"] \
		[list command "&Paste" {} "Past text from clipboard" "Ctrl v" \
		-command "RamDebugger::CutCopyPasteText paste"] \
		separator \
		[list cascad "&Advanced" {} editadvanced 0 [list \
		    [list command "&Indent region" {} "Indent selected region" "" \
		        -command "RamDebugger::IndentSelection"] \
		    [list command "&Comment region" {} "Comment selected region" "" \
		        -command "RamDebugger::CommentSelection on"] \
		    [list command "&Uncomment region" {} "Un-comment selected region" "" \
		        -command "RamDebugger::CommentSelection off"] \
		    separator \
		    [list command "Center display" {} "Center text display" "Ctrl l" \
		        -command "RamDebugger::CenterDisplay"] \
		    [list command "Search in files" {} "Search for pattern in given files" \
		    "ShiftCtrl f" \
		    -command "RamDebugger::SearchInFiles"] \
		    ] \
		] \
		separator \
		[list command "Search..." {} "Search text in source file" "Ctrl f" \
		-command "RamDebugger::SearchWindow"] \
		[list command "Continue search" {} "Continue searching text" "F3" \
		-command "RamDebugger::Search $w any"] \
		[list command "Isearch forward" {} "Incrementally search forward" "Ctrl i" \
		-command "RamDebugger::Search $w iforward"] \
		[list command "Isearch backward" {} "Incrementally search backward" "Ctrl r" \
		-command "RamDebugger::Search $w ibackward"] \
		[list command "&Goto line" {} "Go to the given line" "Ctrl g" \
		-command "RamDebugger::GotoLine"] \
		separator \
		[list command "&Preferences" {} "Choose preferences for RamDebugger" "Ctrl p" \
		-command "RamDebugger::PreferencesWindow"] \
		] \
		"&View" all view 0 [list \
		[list command "&View text/all" {} \
		"Toggle between viewing all windows or only text window" "Ctrl t" \
		-command "RamDebugger::ViewOnlyTextOrAll"] \
		separator \
		[list cascad "&Windows" {} windowslist 0 {}] \
		] \
		"&Debug" all debug 0 [list \
		[list command "&Continue/Go" debugentry "begin/continue execution" "F5" \
		-command "RamDebugger::ContNextGUI rcont"] \
		[list command "Set/unset &breakpoint" debugentry "Set/unset breakpoint" "F9" \
		-command "RamDebugger::SetGUIBreakpoint"] \
		[list command "&Next" debugentry "continue one command" "F10" \
		-command "RamDebugger::ContNextGUI rnext"] \
		[list command "&Step" debugentry "continue one command, entering in subcommands" "F11" \
		-command "RamDebugger::ContNextGUI rstep"] \
		[list command "Continue &to" debugentry "continue to selected line" "Ctrl F5" \
		-command "RamDebugger::ContNextGUI rcontto"] \
		separator \
		[list command "&Expressions..." debugentry \
		    "Open a window to visualize expresions or variables" "" \
		-command "RamDebugger::DisplayVarWindow"] \
		[list command "Breakpoints..." debugentry \
		    "Open a window to visualize the breakpoints list" "Alt F9" \
		-command "RamDebugger::DisplayBreakpointsWindow"] \
		[list command "&Timing control..." debugentry \
		    "Open a window to control execution times" "" \
		-command "RamDebugger::DisplayTimesWindow"] \
		] \
		"&C++ project" all c++ 0 [list \
		[list command "&Create/Edit" c++entry "Create or edit a c++ compile project" "Alt F7" \
		-command "cproject::Create $w"] \
		[list cascad "&Active configuration" {} activeconfiguration 0 [list \
			[list radiobutton "Debug" {} "Compile debug version" "" \
			-variable ::cproject::debugrelease -value debug] \
			[list radiobutton "Release" {} "Compile release version" "" \
			-variable ::cproject::debugrelease -value release] \
			] \
		] \
		[list command "&Compile" c++entry "Compile project" "F7" \
		-command "cproject::Compile $w"] \
		separator \
		[list command "&Clean" c++entry "Clean compiled project files" "" \
		-command "cproject::CompileClean $w"] \
		] \
		"&Utilities" all utilities 0 [list \
		[list command "&Open console" {} "Open console" "" \
		-command "RamDebugger::OpenConsole"] \
		[list command "O&pen VisualRegexp" {} "Open VisualRegexp" "" \
		-command "RamDebugger::OpenProgram visualregexp"] \
		separator \
		[list command "&View instrumented file P" {} "View instrumented file P" "" \
		-command "RamDebugger::ViewInstrumentedFile instrumentedP"] \
		[list command "&View instrumented file R" {} "View instrumented file R" "" \
		-command "RamDebugger::ViewInstrumentedFile instrumentedR"] \
		[list command "&View instrumented info file" {} "View instrumented info file" "" \
		-command "RamDebugger::ViewInstrumentedFile info"] \
		[list command "&View gdb log" {} \
		  "View all commands transferred from/to gdb, if debugging c++" "" \
		-command "RamDebugger::ViewInstrumentedFile gdb"] \
		separator \
		[list command "&Windows hierarchy" {} "View windows hierarchy" "Ctrl 1" \
		-command "RamDebugger::DisplayWindowsHierarchy"] \
		] \
		"&Help" all help 0 [list \
		[list command "&Help" {} "Gives help" "Ctrl h" \
		-command "RamDebugger::ViewHelpFile"] \
		[list command "&Contextual help" {} "Gives help for commands in editor" "F1" \
		-command "RamDebugger::ViewHelpForWord"] \
		separator \
		[list command "&About" {} "Information about the program" "" \
		-command "RamDebugger::AboutWindow"] \
		] \
		]

    set mainframe [MainFrame $w.mainframe \
		       -textvariable RamDebugger::status \
		       -progressvar RamDebugger::progressvar -progressmax 100 \
		       -progresstype normal -menu $descmenu -grid 0]
    $mainframe showstatusbar progression 
    set label [$mainframe addindicator -textvariable RamDebugger::LineNum -width 6 \
	    -anchor e -padx 3]

    bind $label <1> RamDebugger::GotoLine
    set label [$mainframe addindicator -textvariable RamDebugger::remoteserver -width 15 \
		   -anchor e -padx 3]
    set menu [$mainframe getmenu activeprograms]
    $menu configure -postcommand [list RamDebugger::ActualizeActivePrograms $menu]

    bind $label <1> "tk_popup $menu %X %Y"

    set menu [$mainframe getmenu windowslist]
    $menu configure -postcommand [list RamDebugger::ActualizeWindowsList $menu]

    set menu [$mainframe getmenu recentfiles]
    $menu configure -postcommand [list RamDebugger::AddRecentfilesToMenu $menu]


    ################################################################################
    #     The toolbar
    ################################################################################

    set toolbar [$mainframe addtoolbar]

    set bbox [ButtonBox $toolbar.bbox1 -spacing 0 -padx 1 -pady 1 -homogeneous 1 -grid "0 w" \
	    -spacing 2]
    $bbox add -image filenew22 \
	 -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1 -padx 1 -pady 1 \
	 -helptext "Begin new file" \
	 -command "RamDebugger::NewFile"
    $bbox add -image fileopen-22 \
	 -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1 -padx 1 -pady 1 \
	 -helptext "Open source file" \
	 -command "RamDebugger::OpenFile"
    $bbox add -image filesave22 \
	 -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1 -padx 1 -pady 1 \
	 -helptext "Save file" \
	 -command "RamDebugger::SaveFile save"

    Separator $toolbar.sep -orient vertical -grid "1 ns px3"
    set bbox [ButtonBox $toolbar.bbox2 -spacing 0 -padx 1 -pady 1 -homogeneous 1 -grid "2 w"]

    $bbox add -image actundo22 \
	 -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1 -padx 1 -pady 1 \
	 -helptext "Undo previus insert/delete operation" \
	 -command "RamDebugger::CutCopyPasteText undo"
    $bbox add -image editcut22 \
	 -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1 -padx 1 -pady 1 \
	 -helptext "Cut selected text to clipboard" \
	 -command "RamDebugger::CutCopyPasteText cut"
    $bbox add -image editcopy-22 \
	 -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1 -padx 1 -pady 1 \
	 -helptext "Copy selected text to clipboard" \
	 -command "RamDebugger::CutCopyPasteText copy"
    $bbox add -image editpaste22 \
	 -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1 -padx 1 -pady 1 \
	 -helptext "Past text from clipboard" \
	 -command "RamDebugger::CutCopyPasteText paste"
    $bbox add -image find-22 \
	 -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1 -padx 1 -pady 1 \
	 -helptext "Search text in source file" \
	 -command "RamDebugger::SearchWindow"

    Separator $toolbar.sep2 -orient vertical -grid "3 ns px3"
    set bbox [ButtonBox $toolbar.bbox3 -spacing 0 -padx 1 -pady 1 -homogeneous 1 -grid "4 w"]

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

    foreach "weight1 weight2 weight3" [ManagePanes $pw h "1 6 2"] break

    set pane1 [$pw add -weight $weight1]

    if { ![info exists options(defaultdir)] } {
	set options(defaultdir) [pwd]
    }
    label $pane1.l -textvar RamDebugger::options(defaultdir) -anchor e -relief raised -bd 1 \
	-padx 5 -grid "0 ew"

    set sw [ScrolledWindow $pane1.lf -relief sunken -borderwidth 0 -grid 0]
    set listbox [ListBox $sw.lb -background white -multicolumn 0 -selectmode single]
    $sw setwidget $listbox

    if { $::tcl_platform(platform) != "windows" } {
	$listbox configure -selectbackground \#48c96f -selectforeground white
    }

    $sw.lb configure -deltay [expr [font metrics [$sw.lb cget -font] -linespace]]

    set pane2 [$pw add -weight $weight2]

    ################################################################################
    # the vertical edit window and stack trace
    ################################################################################

    set pwin [PanedWindow $pane2.pw -side left -pad 0 -weights available -grid 0 -activator line]

    foreach "weight1in weight2in" [ManagePanes $pwin v "6 1"] break

    set pane2in1 [$pwin add -weight $weight1in]

    set fulltext [frame $f.fulltext -grid no -bd 1 -relief sunken]
    grid $fulltext -in $pane2in1 -sticky nsew
    grid rowconf $pane2in1 0 -weight 1
    grid columnconf $pane2in1 0 -weight 1

    set marker [canvas $fulltext.can -bg grey90 -grid "0 wns" -width 14 -bd 0 \
	    -highlightthickness 0]
    set text [supertext::text $fulltext.text -background white -wrap none -width 80 -height 40 \
		  -exportselection 0 -font FixedFont -highlightthickness 0 -editable 0 \
		  -preproc RamDebugger::CheckTextBefore \
		  -postproc RamDebugger::UpdateLineNum -bd 0 -grid 1 \
		  -undocallback "RamDebugger::UndoCallback" \
		  -xscrollcommand [list $fulltext.xscroll set] \
		  -yscrollcommand [list RamDebugger::ScrollScrollAndCanvas $fulltext.text \
		                       $fulltext.yscroll $fulltext.can]]
    scrollbar $fulltext.yscroll -orient vertical -grid 2 -command \
	[list RamDebugger::ScrollTextAndCanvas $fulltext.text $fulltext.can]
    scrollbar $fulltext.xscroll -orient horizontal -grid "0 2" -command "$fulltext.text xview"

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

    set pane2in2 [$pwin add -weight $weight2in]

    NoteBook $pane2in2.nb -homogeneous 1 -bd 1 -internalborderwidth 0 \
	    -grid "0 py2" -side bottom

    set f1 [$pane2in2.nb insert end stacktrace -text "Stack trace"]
 
    set sw2 [ScrolledWindow $f1.lf2 -relief sunken -borderwidth 0 -grid "0" \
	    -scrollbar both -auto both]
    set textST [text $sw2.text2 -background white -wrap none -width 80 -height 4 \
	    -highlightthickness 0]
    $sw2 setwidget $textST

    supergrid::go $f1

    set f2 [$pane2in2.nb insert end output -text "Output"]

    set sw2 [ScrolledWindow $f2.lf2 -relief sunken -borderwidth 0 -grid "0" \
	    -scrollbar both -auto both]
    set textOUT [text $sw2.text2 -background white -wrap none -width 80 -height 4 \
	    -highlightthickness 0]
    $sw2 setwidget $textOUT

    supergrid::go $f2

    set f3 [$pane2in2.nb insert end compile -text "Compile"]

    set sw2 [ScrolledWindow $f3.lf3 -relief sunken -borderwidth 0 -grid "0" \
	    -scrollbar both -auto both]
    set textCOMP [text $sw2.text2 -background white -wrap none -width 80 -height 4 \
	    -highlightthickness 0]
    $sw2 setwidget $textCOMP

    supergrid::go $f3

    #$pane2in2.nb compute_size
    $pane2in2.nb raise stacktrace

    proc TextStackTraceRaise {} "$pane2in2.nb raise stacktrace"
    proc TextOutRaise {} "$pane2in2.nb raise output"
    proc TextCompRaise {} "$pane2in2.nb raise compile"


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

    label $pane1_vars.l -text Variables -relief raised -bd 1
    label $pane2_vars.l -text Values -relief raised -bd 1

    grid $pane1_vars.l -sticky ew
    grid $pane2_vars.l -sticky ew

    CreatePanedEntries 12 $pane1_vars $pane2_vars ""

    if { [info exists options(watchedvars)] } {
	set i 0
	foreach j $options(watchedvars) {
	    if { [string trim $j] == "" } { continue }
	    set RamDebugger::EvalEntries($i,left) $j
	    incr i
	    if { $i > 12 } {
		CreatePanedEntries $i $pane1_vars $pane2_vars ""
	    }
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

    grid $pane1_varsL.l -sticky ew
    grid $pane2_varsL.l -sticky ew

    CreatePanedEntries 12 $pane1_varsL $pane2_varsL L

    ################################################################################
    # the nice supergrid
    ################################################################################

    supergrid::go $pane3in1
    supergrid::go $pane3in2
    supergrid::go $f1
    supergrid::go $f1L
    supergrid::go $fulltext
    supergrid::go $pane2in2
    supergrid::go $pane1
    supergrid::go $pane2
    supergrid::go $pane3
    supergrid::go $f
    supergrid::go $w

    if {[string equal "unix" $::tcl_platform(platform)]} {
	foreach "but units" [list 4 -5 5 5] {
	    set comm {
		set w %W
		while { $w != [winfo toplevel $w] } {
		    catch {
		        set ycomm [$w cget -yscrollcommand]
		        if { $ycomm != "" } {
		            $w yview scroll $units units
		            break
		        }
		    }
		    set w [winfo parent $w]
		}
	    }
	    regsub -all {(?q)$units} $comm $units comm
	    bind all <$but> $comm
	}
    } else {
	bind all <MouseWheel> {
	    set w %W
	    while { $w != [winfo toplevel $w] } {
		catch {
		    set ycomm [$w cget -yscrollcommand]
		    if { $ycomm != "" } {
		        $w yview scroll [expr int(-1*%D/36)] units
		        break
		    }
		}
		set w [winfo parent $w]
	    }
	}
    }

    ListBoxEvents $listbox RamDebugger::ListBoxDouble1 RamDebugger::ListboxMenu

    set menudev [$mainframe getmenu debug]

    bind $text <3> "%W mark set insert @%x,%y ; RamDebugger::TextMotion -1 -1 -1 -1;\
	    tk_popup $menudev %X %Y"
    bind $text <Double-1> "RamDebugger::SearchBraces %x %y ;break" 

    $textST conf -state disabled
    bind $textST <1> { focus %W }
    bind $textST <Double-1> { RamDebugger::StackDouble1 %W @%x,%y }

    $textOUT conf -state disabled
    bind $textOUT <1> { focus %W }
    bind $textOUT <Double-1> { RamDebugger::StackDouble1 %W @%x,%y }

    $textCOMP conf -state disabled
    bind $textCOMP <1> { focus %W }
    bind $textCOMP <Double-1> { RamDebugger::StackDouble1 %W @%x,%y }

    bind all <F4> "RamDebugger::PrevNextCompileError next"
    bind all <Shift-F4> "RamDebugger::PrevNextCompileError prev"
#      bind all <F1> "RamDebugger::ViewHelpForWord"
    bind $text <Motion> "RamDebugger::TextMotion %X %Y %x %y"
    # in linux, F10 makes some stupid thing
    bind all <F10> ""

    bind $text <Alt-Left> "RamDebugger::GotoPreviusNextInWinList prev ; break"
    bind $text <Control-Tab> "RamDebugger::GotoPreviusNextInWinList prev ; break"
    bind $text <Alt-Right> "RamDebugger::GotoPreviusNextInWinList next ; break"
    bind $text <Control-Shift-Tab> "RamDebugger::GotoPreviusNextInWinList next ; break"
    bind $text <Tab> "RamDebugger::IndentCurrent ; break"

    foreach i [bind $w] {
	bind $text $i "[bind $w $i] ;break"
    }
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

    bind all <Control-Key-1> "RamDebugger::DisplayWindowsHierarchy ;break"

    if { ![info exists options(maingeometry)] } {
	set options(maingeometry) 800x600
    }

    if { [info exists options(ViewOnlyTextOrAll)] && $options(ViewOnlyTextOrAll) == "OnlyText" } {
	RamDebugger::ViewOnlyTextOrAll
    }


    # trick to know if we are debugging RamDebugger
    if { [info command sendmaster] != "" } {
	if { [regexp {(\d+)x(\d+)[+](\d+)[+](\d+)} $options(maingeometry) {} wi he xpos ypos] } {
	    incr xpos 20
	    incr ypos 20
	    wm geom $w ${wi}x$he+$xpos+$ypos
	    if { [info exists options(currentfile)] } {
		set options(currentfile) ""
	    }
	}
    } else {
	if { $options(maingeometry) == "zoomed" } {
	    wm geom $w 800x600
	    wm state $w zoomed
	} else {
	    wm geom $w $options(maingeometry)
	}
    }

    ActualizeActivePrograms $menu

    if { [info exists options(breakpoints)] } {
	set breakpoints $options(breakpoints)
    }

#     if { [info exists options(remoteserverType)] && $options(remoteserverType) == "remote" && \
#          [info exists options(remoteserver)] } {
#         SetMessage "Connecting remoteserver $options(remoteserver)..."
#         catch { rdebug $options(remoteserver) }
#         SetMessage ""
#     }

    if { [info exists options(currentfile)] && $options(currentfile) != ""  && \
	    [file exists $options(currentfile)] } {
	SetMessage "Opening file '$options(currentfile)'..."
	OpenFileF $options(currentfile)

	if { [winfo exists options(currentidx)] } {
	    $text see $options(currentidx)
	    $text mark set insert $options(currentidx)
	}
	SetMessage ""
    } else { NewFile }
    
    focus $text

    cproject::Init $w

    # for tkcon
    rename ::exit ::exit_final
    proc ::exit { args } {}

    # for defining what is a word for text widgets
    auto_load tcl_wordBreakAfter
    set ::tcl_wordchars "\\w"
    set ::tcl_nonwordchars "\\W"
}

if { [lsearch $argv "-noprefs"] != -1 } {
    RamDebugger::Init 0
} else { RamDebugger::Init 1 }

################################################################################
#     Init the GUI part
################################################################################

if { [info command wm] != "" && [info commands tkcon_puts] == "" } {
    wm withdraw .
    RamDebugger::InitGUI
    bind all <Control-x><Control-l> "source [info script] ; WarnWin reload"
}
