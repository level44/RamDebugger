#!/bin/sh
# the next line restarts using wish \
exec wish "$0" "$@"

#         $Id: RamDebugger.tcl,v 1.18 2003/02/24 14:27:03 ramsan Exp $        
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

    # can be: remote; local; master; or gdb
    variable remoteserverType ""
    # when remoteserverType is master, it can be master, "master proc", or "master all"
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
    variable WindowFilesListLineNums ""
    variable WindowFilesListCurr -1
    variable SavedPositionsStack ""

    ################################################################################
    # Preferences
    ################################################################################

    variable options
    variable options_def

}

################################################################################
#   Init proc
################################################################################

proc RamDebugger::Init { readprefs { registerasremote 1 } } {
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
    set options_def(instrument_source) auto
    set options_def(ConfirmModifyVariable) 1
    set options_def(LocalDebuggingType) tk
    set options_def(executable_dirs) ""
    set options_def(debugrelease) debug
    set options_def(ViewLocalVariables) 1

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
	set options_def(CheckRemotes) 1
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

    UpdateExecDirs

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

proc RamDebugger::UpdateExecDirs {} {
    variable options
    variable MainDir

    if { $::tcl_platform(platform) == "windows" } {
	set file [filenormalize [file join $MainDir addons]]
	if { [lsearch $options(executable_dirs) $file] == -1 } {
	    lappend options(executable_dirs) $file
	}
	foreach i [glob -nocomplain -dir c: mingw*] {
	    set dirs_in [glob -nocomplain -dir $i mingw*]
	    foreach j [concat [list $i] $dirs_in] {
		if { [file exists [file join $j bin]] } {
		    set file [filenormalize [file join $j bin]]
		    if { [lsearch $options(executable_dirs) $file] == -1 } {
		        lappend options(executable_dirs) $file
		    }
		}
	    }
	}
    }

    if { ![info exists ::env(PATH)] } {
	set list ""
    } else {
	set list [split $::env(PATH) \;]
    }
    set haschanged 0
    foreach i $options(executable_dirs) {
	if { $::tcl_platform(platform) == "windows" } {
	    set err [catch { set shortname [file native [file attributes $i -shortname]] }]
	    if { !$err } { set i $shortname }
	}
	if { [lsearch $list $i] == -1 } {
	    lappend list $i
	    set haschanged 1
	}
    }
    if { $haschanged } {
	set ::env(PATH) [join $list \;]
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
	-master:        only valid when RamDebugger is included as package. Debugs master program
	-debugcplusplus: execute and debug a c++ file. program is a list with prg. name and args
	--:             end of options

	To begin debugging a TCL file, select the file with 'rlist' and use 'rdebug -currentfile'.
	To begin debugging a remote program, use 'rdebug program', where program is one active
	program, that must belong to the services list.
    }
    ParseArgs $args $usagestring opts

    if { $opts(-master) } {
	# nothing
    } elseif { $opts(-forceupdate2) } {
	FindActivePrograms 2
    } elseif { $opts(-forceupdate) } {
	FindActivePrograms 1
    } else { FindActivePrograms 0 }
    
    if { $opts(-actives) } { return [array names services] }

    if { $opts(-disconnect) } {
	if { $remoteserver == "" } {
	    error "error. There is no debugging session active to stop"
	}
	if { $remoteserverType == "master" } {
	    error "It is not possible to stop this type of debugging. Use 'Quit' instead"
	}
	if { $remoteserverType == "local" } {
	    interp delete local
	} elseif { $remoteserverType == "gdb" } {
	    catch {
		puts [lindex $remoteserver 0] quit
		close [lindex $remoteserver 0]
	    }
	}
	set remoteserver ""
	set remoteserverType ""
	set debuggerstate ""
	TakeArrowOutFromText
	return
    }
    if { $opts(-master) } {
	if { $remoteserverType == "local" } {
	    interp delete local
	} elseif { $remoteserverType == "gdb" } {
	    catch {
		puts [lindex $remoteserver 0] quit
		close [lindex $remoteserver 0]
	    }
	}
	set gdblog ""

	if { [info exists options(master_type)] } {
	    set remoteserver $options(master_type)
	} else { set remoteserver master }

	set remoteserverType master
	TakeArrowOutFromText
    } elseif { $opts(-currentfile) } {
	if { [interp exists local] } { interp delete local }
	interp create local
	interp alias local sendmaster "" eval
	if { $::tcl_platform(platform) == "windows" } {
	    interp alias local console "" console
	}
	# dirty trick to avoid the slave interp block
	interp eval local {
	    proc updatemaster {} {
		sendmaster update
		after 3000 updatemaster
	    }
	}
	proc exit_slave { args } {
	    if { [catch [list RamDebugger::rdebug -disconnect] errstring] } {
		WarnWin $errstring
	    } else {
		set code 0
		if { [llength $args] == 1 } { set code [lindex $args 0] }
		TextOutRaise
		TextOutInsertRed "Program exited with code $code"
	    }
	}
	interp alias local exit "" RamDebugger::exit_slave
	local eval { set argc 0 ; set argv "" }
	local eval [list set argv0 $currentfile]

	if { [info exists options(currentfileargs)] } {
	    foreach "curr dir_in arg_in" $options(currentfileargs) {
		if { $curr == $currentfile } {
		    if { [file isdirectory $dir_in] } { cd $dir_in }
		    local eval [list set argc [llength $arg_in]]
		    local eval [list set argv $arg_in]
		    break
		}
	    }
	}

	set err [catch {package present registry} ver]
	if { !$err } {
	    interp alias local registry "" registry
	    interp eval local package provide registry $ver
	}
	if { ![info exists options(LocalDebuggingType)] || $options(LocalDebuggingType) == "tk" } {
	    interp eval local [list load {} Tk]
	}
	#local eval { catch { bind . <Destroy> exit } }
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
		    ViewHelpForWord "Debugging c++"
		    #RamDebugger::ViewHelpFile "01RamDebugger/RamDebugger_12.html"
		}
		return
	    } else {
		error "Could not find command 'gdb'"
	    }
	}
	
	if { $remoteserverType == "local" } {
	    interp delete local
	} elseif { $remoteserverType == "gdb" } {
	    catch {
		puts [lindex $remoteserver 0] quit
		close [lindex $remoteserver 0]
	    }
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
	    catch {
		puts [lindex $remoteserver 0] quit
		close [lindex $remoteserver 0]
	    }
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
	if { [info commands ::RDC::sourceproc] == "" } {
	    rename ::source ::RDC::sourceproc
	    proc ::source { file } {
		set file [file join [pwd] $file]
		set retval [RDC::SendDev [list RamDebugger::RecieveFromProgramSource $file]]
		if { $retval != "" } { uplevel 1 $retval }
	    }
	}
    }
    if { $remoteserverType == "local" } {
	set remotecomm [string map [list SENDDEVBODY "sendmaster \$comm"] \
	    $remotecomm]
    } elseif { $remoteserverType == "master" } {
	set remotecomm [string map [list SENDDEVBODY "ramdebugger eval \$comm"] \
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

    switch $remoteserverType {
	remote - master {
	    if { $currentfile != "" } {
		rlist -quiet $currentfile
	    }
	}
	local {
	    EvalRemote [list set ::RDC::currentfile $currentfile]
	    after idle [list RamDebugger::rlist -quiet -asmainfile $currentfile]
	}
	gdb {
	    EvalRemote "run"
	}
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
		if { [string length $::RDC::j] > 100 } {
		    set ::RDC::j [string range $::RDC::j 0 96]...
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
    variable remoteserver
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
	error "There is no file selected\n$usagestring"
    }
    if { $opts(line) != "" } {
	set currentline $opts(line)
	set filenum [lsearchfile $fileslist $currentfile]
	if { $remoteserverType != "gdb" } {
	    set ipos [string first "RDC::F $filenum $currentline ;" \
		    $instrumentedfilesP($currentfile)]
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
	    set remoteserver [lreplace $remoteserver 2 2 setbreakpoints]
	    EvalRemote "tbreak [file tail $currentfile]:$currentline"
	    EvalRemote "printf \"FINISHED SET BREAKPOINTS\\n\""
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
    variable instrumentedfilesTime

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
	set currentfile_save $currentfile
	foreach i $TimeMeasureData {
	    foreach "name file lineini lineend lasttime" $i {
		lappend TimeMeasureDataNew [list $name $file $lineini $lineend ""]
		if { [lsearchfile $files $file] == -1 } {
		    rlist -quiet $file
		    lappend files $file
		}
	    }
	}
	set currentfile $currentfile_save
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
		    if { [info exists instrumentedfilesTime($file)] } {
		        unset instrumentedfilesTime($file)
		    }
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
	    microsec { set unitfactor 1 ; set format %i }
	    milisec { set unitfactor 1e-3  ; set format %i }
	    sec { set unitfactor 1e-6  ; set format %.4g }
	    min { set unitfactor 1e-6/60.0  ; set format %.4g }
	    default {
		error "error in display units.\n$usagestring"
	    }
	}
	set retval ""
	foreach i $datanames {
	    append retval [string repeat "....." [lindex $data($i) 0]]
	    append retval $i
	    set time [lindex $data($i) 1]
	    if { $time != "" } { set time [format $format [expr $time*$unitfactor]] }
	    append retval " $time $unitname"
	    if { [lindex $data($i) 2] != "" && [lindex $data($i) 2] != 0 } {
		append retval " ([format %.3g [lindex $data($i) 2]]%)"
	    }
	    if {$time == "" } { append retval " (this block not executed since start)" }
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
	    if { $opts(lineini) < $lineini && $opts(lineend) >= $lineini && \
		$opts(lineend) < $lineend } { set fail 1 }
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

    if { [info exists instrumentedfilesTime($currentfile)] } {
	unset instrumentedfilesTime($currentfile)
    }
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
	-h:            displays usage
	-quiet:        do not print anything
	-force:        force to reload file
	-reinstrument: force to reinstrument
	-resend:       force to send again
	-asmainfile:   When debugging locally, the first file, first time  must be list like this
	-returndata:   Instead of sending instr file, return it
	--:            end of options
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

    if { ($currentfile == "New file" || $currentfileIsModified ) && \
	    ![info exists instrumentedfilesP($currentfile)] } {
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
		regexp {RDC::F ([-0-9]+)} [set instrumentedfiles${i}($currentfile)] {} oldfilenum
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
	if { [info exists instrumentedfilesSent($currentfile)] } {
	    unset instrumentedfilesSent($currentfile)
	}
	SetMessage ""
    }

    set retval ""

    set dosend -1

    if { $debuggerstate == "" || $remoteserver == "" || \
	![info exists instrumentedfilesP($currentfile)] } { set dosend 0 }
    if { $remoteserver == "master" && !$opts(-returndata) } { set dosend 0 }
    if { $dosend != 0 && ![info exists instrumentedfilesSent($currentfile)] } { set dosend 1 }
    if { $dosend != 0 && [info exists instrumentedfilesSent($currentfile)] && \
	$instrumentedfilesSent($currentfile) != $debuggerstate  } { set dosend 1 }
    if { $dosend != 0 && ($opts(-resend) || $force || $opts(-returndata)) } { set dosend 1 }

    if { $dosend == 1 } {
	set err [catch {
	    if { $debuggerstate == "debug" } {
		if { $opts(-returndata) } {
		    append retval $instrumentedfilesP($currentfile)\n
		} else {
		    EvalRemote $instrumentedfilesP($currentfile)
		}
		if { $opts(-asmainfile) || $remoteserver == "master all" } {
		    if { $opts(-returndata) } {
		        append retval $instrumentedfilesR($currentfile)
		    } else {
		        EvalRemote $instrumentedfilesR($currentfile)
		    }
		}
	    } else {
		EvalRemote $instrumentedfilesTime($currentfile)
	    }
	    set instrumentedfilesSent($currentfile) $debuggerstate

	    FillListBox
	} errstring]
		                
	if { $err } {
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
    if { $opts(-returndata) || $opts(-quiet) } {
	return $retval
    }

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
    if { $file == "" } { return "" }
    
    set pwd [pwd]
    catch {
	cd [file dirname $file]
	set file [file join [pwd] [file tail $file]]
    }
    cd $pwd

    if { $::tcl_platform(platform) == "windows" } {
	catch { set file [file attributes $file -longname] }
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
    SetMessage "Remote program: $name"
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
	    set err [catch $comm errstring]
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
	return [lsearch [string tolower $list] [string tolower $file]]
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

proc RamDebugger::RecieveFromProgramSource { file } {
    variable currentfile

    set retval [RamDebugger::DoinstrumentThisfile $file]
    if { $retval } {
	TextOutRaise
	TextOutInsertBlue "Sending Instrumented file '$file'\n"

	set currentfile_save $currentfile
	set retval [rlist -returndata -asmainfile $file]
	set currentfile $currentfile_save
	return $retval
    }
    return [list ::RDC::sourceproc $file]
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
    } elseif { $remoteserverType == "master" } {
	master after idle [list $comm]
    } elseif { $remoteserverType == "gdb" } {
	foreach "fid program state" $remoteserver break
	regsub -all {(^|\n)(.)} $comm\n {\1-->\2} commlog
	append gdblog $commlog
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

    if { $file == "New file" } { return }

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

    regsub -all {(^|\n)(.)} $aa {\1<--\2} aalog
    append gdblog $aalog

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
	    if { [regexp {^\$[0-9]+\s+=\s+(.*)} $aa {} res] } {
		set ExpressionResult [list 0 $res]
	    } elseif { [regexp {No symbol .* in current context} $aa] } {
		set ExpressionResult [list 1 $aa]
	    } else { return }

	    set handler [lindex $state 1]
	    set remoteserver [lreplace $remoteserver 2 2 ""]

	    if { $handler != "" } {
		uplevel \#0 $handler [list $ExpressionResult]
	    }
	    return
	}
	backtrace* {
	    set aa [lindex $state 1]$aa
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
	    } else {
		if { ![file exists $file] } {
		    set fullfile [cproject::TryToFindPath $file]
		    if { $fullfile != "" } {
		        set file $fullfile
		      
		    }
		}
		if { [file exists $file] } {
		    set file [filenormalize $file]
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
	    if { [regexp {FINISHED SET BREAKPOINTS\s*(.*)$} $aa {} rest] } {
		set remoteserver [lreplace $remoteserver 2 2 ""]
		if { [string trim $rest] == "" } { return }
	    } else { return }
	}
    }

    if { [regexp {(Breakpoint\s[0-9]+,\s+)?(\S+\s+\([^\)]*\))\s+at\s+([^:]+):([0-9]+)} \
	$aa {} {} procname file line] } {

	if { ![file exists $file] } {
	    set fullfile [cproject::TryToFindPath $file]
	    if { $fullfile != "" } {
		set file $fullfile
	    }
	}
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
	    set filenum [lsearch $fileslist $file]
	    if { $filenum == -1 } {
		set err [catch {OpenFileF $file} errstring]
		if { $err } {
		    WarnWin "Could not open file '$file' for stopping program"
		    return
		}
		set filenum [lsearch $fileslist $file]
	    }
	    RecieveFromProgram "" $filenum $line $procname "" ""
	    return

#         set found 0
#         foreach i $breakpoints {
#             set breaknum [lindex $i 0]
#             set line_in [lindex $i 2]
#             set file_in [lindex $i 1]
#             if { $line == $line_in && [AreFilesEqual $file $file_in] } {
#                 set found 1
#                 break
#             }
#             # CONDITION is forgotten by now
#         }
#         if { $found } {
#             set filenum [lsearch $fileslist $file]
#             if { $filenum == -1 } {
#                 set err [catch {OpenFileF $file} errstring]
#                 if { $err } {
#                     WarnWin "Could not open file '$file' for stopping program"
#                     return
#                 }
#                 set filenum [lsearch $fileslist $file]
#             }
#             RecieveFromProgram $breaknum $filenum $line $procname "" ""
#             return
#         } else {
#             WarnWin "Problems finding breakpoint in file $file ($aa)"
#         }
#         return
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
    if { [regexp {Program received signal} $aa] } {
	WarnWin $aa
	TextOutInsert $aa
	rstack -handler RamDebugger::UpdateGUIStack
	return
    }
    if { [string match "*No executable specified, use `target exec'.*" $aa] } {
	WarnWin "Error defining the debugged executable. Use 'Utilities->gdb log' for details"
    }
    TextOutInsert $aa
    TextOutRaise
}

################################################################################
#                   RamDebugger GUI
################################################################################

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
    variable TimeMeasureData
    variable debuggerstate

    if { [SaveFile ask] == -1 } { return }

    if { $remoteserver != "" && $remoteserverType != "master" } {
	rdebug -disconnect
    }

    set options(NormalFont) [font configure NormalFont]
    set options(FixedFont) [font configure FixedFont]
    set options(HelpFont) [font configure HelpFont]

    if { [info exists remoteserver] && [string match master* $remoteserver] } {
	set options(master_type) $remoteserver
    }

    set options(debuggerstate) $debuggerstate

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
    set options(TimeMeasureData) $TimeMeasureData
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
    if { $remoteserverType == "master" } {
	EvalRemote [list package forget RamDebugger]
	variable breakpoints ""
	UpdateRemoteBreaks
	destroy [winfo toplevel $text]
    } elseif { [info command exit_final] != "" } {
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

proc RamDebugger::ColorizeSlow { txt } {

    set ed [$txt cget -editable]
    $txt conf -editable 1
    $txt tag conf magenta -foreground magenta
    $txt tag conf blue -foreground blue
    $txt tag conf grey -foreground grey
    $txt tag conf green -foreground green
    $txt tag conf red -foreground red

    set idx 1.0
    while 1 {
	set idx2 [$txt search -count RamDebugger::count -regexp {proc\s+\S+} $idx end]
	if { $idx2 == "" } { break }
	$txt tag add magenta $idx2 $idx2+4c
	$txt tag add blue $idx2+5c $idx2+${RamDebugger::count}c
	set idx [$txt index $idx2+${RamDebugger::count}c]
    }

    set string {[^\\]\"([^\"]+[^\\\"])?\"}
    set magentas {\m(return|break|while|eval|foreach|for|if|else|elseif|error|switch|default)\M}
    set greens {\m(variable|set)\M}
    set comments {\#.*$}

    set idx 1.0
    while 1 {
	set idx2 [$txt search -count RamDebugger::count -regexp \
		      $string|$magentas|$greens|$comments $idx end]
	if { $idx2 == "" } { break }
	foreach "rex tag icr" [list $string grey 1 $magentas magenta 0 $greens green 0 \
		                   $comments red 0] {
	    set idx3 [$txt search -regexp $rex $idx2 $idx2+${RamDebugger::count}c]
	    if { $idx3 == $idx2 } {
		$txt tag add $tag $idx2+${icr}c $idx2+${RamDebugger::count}c
		break
	    }
	}
	set idx [$txt index $idx2+${RamDebugger::count}c]
    }
    $txt tag raise sel
    $txt conf -editable $ed
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

    set NeedsReinstrument 0
    if { $what == "saveas" || $currentfile == "New file" || $currentfile == "" } {
	set NeedsReinstrument 1
	set w [winfo toplevel $text]
	set types {
	    {{TCL Scripts}      {.tcl}        }
	    {{All Files}        *             }
	}
	if { ![info exists options(defaultdir)] } { set options(defaultdir) [pwd] }
	set file [tk_getSaveFile -filetypes $types -initialdir $options(defaultdir) -parent $w \
	    -title "Save file"]
	if { $file == "" } { return }
	set options(defaultdir) [file dirname $file]
    } else {
	set file $currentfile

	if { [file mtime $file] > $filesmtime($file) } {
	    set ret [DialogWin::messageBox -default ok -icon question -message \
		"File '$currentfile' has been modified outside RamDebugger. Loose external changes?" \
		-parent $text -title "Warning" -type okcancel]
	    if { $ret == "cancel" } { return -1 }
	}
    }
    SaveFileF $file

    if { $NeedsReinstrument } { rlist -quiet }
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
    FillListBox
}

proc RamDebugger::OpenFileF { file { force 0 } } {
    variable marker
    variable text
    variable files
    variable breakpoints
    variable currentfile
    variable currentfileIsModified
    variable WindowFilesList
    variable WindowFilesListLineNums
    variable WindowFilesListCurr
    variable options

    if { $force == -1 } {
	set force 0
    } else {
	if { [SaveFile ask] == -1 } { return }
    }

    WaitState 1

    if { [lsearch $WindowFilesList $currentfile] != -1 } {
	set pos [lsearch $WindowFilesList $currentfile]
	set line [scan [$text index insert] %d]
	set WindowFilesListLineNums [lreplace $WindowFilesListLineNums $pos $pos $line]
    }
    set linenum 1
    if { [lsearch $WindowFilesList $file] != -1 } {
	set pos [lsearch $WindowFilesList $file]
	set linenum [lindex $WindowFilesListLineNums $pos]
    }
    if { $file == $currentfile } {
	set idx [$text index insert]
    } else { set idx $linenum.0 }

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

    set Numlines [scan [$text index end] %d]

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
	    lappend WindowFilesListLineNums 1
	} else {
	    set WindowFilesList [lreplace $WindowFilesList $WindowFilesListCurr end $file]
	    set WindowFilesListLineNums [lreplace $WindowFilesListLineNums $WindowFilesListCurr \
		end 1]
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
    #FillListBox

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
    variable WindowFilesListLineNums

    if { [SaveFile ask] == -1 } { return }

    WaitState 1

    if { [lsearch $WindowFilesList $currentfile] != -1 } {
	set pos [lsearch $WindowFilesList $currentfile]
	set line [scan [$text index insert] %d]
	set WindowFilesListLineNums [lreplace $WindowFilesListLineNums $pos $pos $line]
    }

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

    set Numlines [scan [$text index end] %d]
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
    
    wm title [winfo toplevel $text] "RamDebugger     [file tail $currentfile]"
    set currentfileIsModified 0
    set filesmtime($currentfile) [file mtime $file]

    WaitState 0
    SetMessage "Saved file '$file'"
}

proc RamDebugger::ViewInstrumentedFile { what } {
    variable marker
    variable text
    variable currentfile
    variable WindowFilesList
    variable WindowFilesListLineNums
    variable instrumentedfilesP
    variable instrumentedfilesR
    variable instrumentedfilesInfo
    variable instrumentedfilesTime
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
    } elseif { $what == "time" } {
	if { ![info exists instrumentedfilesTime($currentfile)] } {
	    WarnWin "There is no instrumented file time for file '$currentfile'"
	    return
	}
    } else {
	if { ![info exists instrumentedfilesInfo($currentfile)] } {
	    WarnWin "There is no instrumented info file for file '$currentfile'"
	    return
	}
    }
    WaitState 1

    if { [lsearch $WindowFilesList $currentfile] != -1 } {
	set pos [lsearch $WindowFilesList $currentfile]
	set line [scan [$text index insert] %d]
	set WindowFilesListLineNums [lreplace $WindowFilesListLineNums $pos $pos $line]
    }

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
    } elseif { $what == "time" } {
	wm title [winfo toplevel $text] "RamDebugger      [file tail $currentfile] instrumented time"
	$textO ins end [string map [list "\t" "        "] $instrumentedfilesTime($currentfile)]
    } else {
	wm title [winfo toplevel $text] "RamDebugger      [file tail $currentfile] instrumented info"
	foreach i $instrumentedfilesInfo($currentfile) {
	    $textO ins end [string map [list "\t" "        "] $i\n]
	}
    }
    $textO tag add normal 1.0 end
    $text conf -editable $ed
    ColorizeSlow $text
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


proc RamDebugger::ViewHelpForWord { { word "" } } {
    variable text

    set w [ViewHelpFile]

    set range [$text tag ranges sel]
    if { $word != "" } {
	# nothing
    } elseif { $range != "" } {
	set word [eval $text get $range]
    } else {
	set word ""
	set idx [$text index insert]
	if { [$text compare $idx > 1.0] } {
	    set idx0 [$text index $idx-1c]
	    while { [string is wordchar [$text get $idx0]] } {
		set word [$text get $idx0]$word
		if { [$text compare $idx0 <= 1.0] } { break }
		set idx0 [$text index $idx0-1c]
	    }
	}
	set idx1 $idx
	while { [string is wordchar [$text get $idx1]] } {
	    append word [$text get $idx1]
	    if { [$text compare $idx1 >= end-1c] } { break }
	    set idx1 [$text index $idx1+1c]
	}
	if { $word == "" } { return }
    }
    HelpViewer::HelpSearchWord $word
}

proc RamDebugger::ActualizeActivePrograms { menu { force 0 } } {
    variable text
    variable mainframe
    variable remoteserver
    variable remoteserverType

    # the correct thing would be to check remoteserverType but it is not
    # set at program start up
    if { [info command master] != "" } {
	$menu del 0 end
	$menu add radio -label "No autosend" -variable RamDebugger::remoteserver -value master
	$menu add radio -label "Send procs" -variable RamDebugger::remoteserver -value "master proc"
	$menu add radio -label "Send all" -variable RamDebugger::remoteserver -value "master all"

	DynamicHelp::register $menu menu RamDebugger::status
	DynamicHelp::register $menu menuentry 0 "Only instrument sourced files"
	DynamicHelp::register $menu menuentry 1 "Instrument all procs in active files"
	DynamicHelp::register $menu menuentry 2 "Instrument/execute all active files"
	return
    }

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

    set fontbold [font actual [$menu cget -font]]
    set ipos [lsearch $fontbold -weight]
    incr ipos
    set fontbold [lreplace $fontbold $ipos $ipos bold]

    $menu add command -label "Remote TCL debugging" -font $fontbold -state disabled \
       -background grey85

    if { [llength $services] == 0 } {
	$menu add command -label "There are no active programs" -state disabled
	if { $::tcl_platform(platform) == "windows" } {
	    $menu add command -label "Use 'Remote TCL update' to actualize" -state disabled
	}
    } else {
	foreach i $services {
	    if { $remoteserverType == "remote" && $i == $remoteserver } {
		$menu add check -label $i -command [list RamDebugger::rdebug $i] \
		   -variable ::checked 
		set ::checked 1
	    } else {
		$menu add command -label $i -command [list RamDebugger::rdebug $i]
	    }
	}
    }
    $menu add separator

    if { $::tcl_platform(platform) == "windows" } {
	$menu add command -label "Remote TCL update" -font $fontbold -state disabled \
	    -background grey85
	$menu add command -label "Update remotes" -command \
	"RamDebugger::ActualizeActivePrograms $menu 1"
	$menu add separator
    }

    $menu add command -label "Local TCL debugging" -font $fontbold -state disabled \
       -background grey85
    if { $remoteserverType == "local" } {
	$menu add check -label "Current file" -command {
	    RamDebugger::rdebug -currentfile
	} -variable ::checked 
	set ::checked 1
    } else {
	$menu add command -label "Current file" -command {
	    RamDebugger::rdebug -currentfile
	}
    }
    $menu add command -label "Current file args" -command {
	RamDebugger::DebugCurrentFileArgsWindow
    }
    $menu add separator
    $menu add command -label "Local c/c++ debugging" -font $fontbold -state disabled \
       -background grey85
    if { $remoteserverType == "gdb" } {
	$menu add check -label "Debug c++" -command "RamDebugger::DebugCplusPlusWindow" \
	   -variable ::checked 
	set ::checked 1
    } else {
	$menu add command -label "Debug c++" -command "RamDebugger::DebugCplusPlusWindow"
    }
    $menu add command -label "Debug c++ (no ask)" -command "RamDebugger::DebugCplusPlusWindow 1"
    $menu add separator
    $menu add command -label Disconnect/Stop -command RamDebugger::DisconnectStop -acc "Shift+F5"

    if { $remoteserver == "" } {
	$menu entryconfigure end -state disabled
    }
    SetMessage ""
    WaitState 0
}

proc RamDebugger::DisconnectStop {} {
    if { [catch [list RamDebugger::rdebug -disconnect] errstring] } {
	WarnWin $errstring
    }
}

proc RamDebugger::GotoPreviusNextInWinList { what } {
    variable WindowFilesList
    variable WindowFilesListLineNums
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

proc RamDebugger::ActualizeViewMenu { menu } {
    variable WindowFilesList
    variable WindowFilesListLineNums
    variable WindowFilesListCurr
    variable text

    if { [$menu index end] > 1 } {
	$menu del 2 end
    }

    $menu add command -label "Previus" -acc "Alt-Left" -command \
       "RamDebugger::GotoPreviusNextInWinList prev"
    $menu add command -label "Next" -acc "Alt-Right" -command \
       "RamDebugger::GotoPreviusNextInWinList next"

    set needssep 1
    set ipos 0
    foreach i $WindowFilesList {
	if { $needssep } {
	    $menu add separator
	    set needssep 0
	}
	if { $ipos == $WindowFilesListCurr } {
	    set label $i
	    if { [string length $label] > 25 } { set label ...[string range $label end-22 end] }
	    $menu add checkbutton -label $label -variable ::pp -command \
	       [list RamDebugger::OpenFileF $i]
	    set ::pp 1
	} else {
	    set label $i
	    if { [string length $label] > 25 } { set label ...[string range $label end-22 end] }
	    $menu add command -label $label -command [list RamDebugger::OpenFileF $i]
	}
	incr ipos
    }
}

proc RamDebugger::AddRecentfilesToMenu { menu } {
    variable options

    $menu del 0 end

    if { ![info exists options(RecentFiles)] } { return }

    foreach i $options(RecentFiles) {
	$menu add command -label $i -command \
	   "[list RamDebugger::OpenFileF $i] ; RamDebugger::FillListBox"
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
	if { $::tcl_platform(platform) == "windows" } {
	    set doit 1
	} elseif { [focus] != $text } {
	    # the horrible bug in Linux with the raise command
	    set doit 1
	} else { set doit 0 }
	if { $doit } {
	    after 100 "raise [winfo toplevel $text] ; focus -force $text"
	}
	$text see $line.0
	$text mark set insert $line.0
    }
    MoveCanvas $text $marker

    if { $IsInStop } {
	after 100 RamDebugger::CheckEvalEntries do
	after 200 RamDebugger::CheckEvalEntriesL do
	after 300 RamDebugger::InvokeAllDisplayVarWindows
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

    if { $remoteserver == "" || ($remoteserverType == "local" && !$IsInStop) || \
	($remoteserver == "master all" && !$IsInStop) } {
	if { $currentfile == "" } {
	    WarnWin "Cannot start debugging. There is no currentfile" $text
	    return
	}
	#if { $currentfile == "New file"  } {

	#}
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
		if { $remoteserverType == "local" || $remoteserverType == "" } {
		    set tt "Do you want to start to debug locally '$currentfile'?"
		} else {
		    set tt "Do you want to execute file '$currentfile'?"
		}
		set ret [DialogWin::messageBox -default yes -icon question -message \
		        $tt -parent $text \
		        -title "start debugging" -type yesnocancel]
	    } else { set ret yes }
	    if { $ret == "cancel" } { return }
	    if { $ret == "yes" } {
		if { $remoteserverType == "local" || $remoteserverType == "" } {
		    rdebug -currentfile
		} else {
		    rlist -resend -quiet
		}
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

proc RamDebugger::CheckEvalEntries { what { name "" } { res "" } } {
    variable EvalEntries
    variable IsInStop
    variable remoteserver
    variable remoteserverType
    variable gdblog
    variable options

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
		if { [string length $val] > 100 } {
		    set val [string range $val 0 96]...
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
	    if { [string length $val] > 100 } {
		set val [string range $val 0 96]...
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

	    if { $options(ConfirmModifyVariable) } {
		set ret [DialogWin::messageBox -default ok -icon question -message \
		        "Are you sure to change variable '$var' to value '$value'?" \
		        -title "Warning" -type okcancel]
		if { $ret == "cancel" } { return }
	    }

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
    variable options

    if { !$options(ViewLocalVariables) } {
	set i 0
	while 1 {
	    if { ![info exists EvalEntries($i,leftL)] } { break }
	    set EvalEntries($i,leftL) ""
	    set EvalEntries($i,rightL) ""
	    incr i
	}
	return
    }

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
		        if { [string length $::RDC::val] > 100 } {
		            set ::RDC::val [string range $::RDC::val 0 96]...
		        }
		        lappend ::RDC::retval $::RDC::i array $::RDC::val
		    } elseif { [info exists $::RDC::i] } {
		        set ::RDC::val [set $::RDC::i]
		        if { [string length $::RDC::val] > 100 } {
		            set ::RDC::val [string range $::RDC::val 0 96]...
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
    } elseif { [string match *leftL $name] } {

	if { $what == "do" } {
	    regexp {[0-9]+} $name i
	    set var $EvalEntries($name)
	    if { [string trim $var] == "" } {
		$RamDebugger::EvalEntries($i,rightentryL) conf -fg black
		set RamDebugger::EvalEntries($i,rightL) ""
		return
	    }
	    if { $remoteserverType == "gdb" } {
		set remoteserver [lreplace $remoteserver 2 2 [list getdata \
		        "RamDebugger::CheckEvalEntriesL res $name"]]
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
	    reval -handler [list RamDebugger::CheckEvalEntriesL res $name] $comm
	} else {
	    regexp {[0-9]+} $name i
	    if { $remoteserverType == "gdb" } {
		set res [list 0 [list variable $res]]
	    }
	    foreach "type val" [lindex $res 1] break
	    if { [string length $val] > 100 } {
		set val [string range $val 0 96]...
	    }
	    set RamDebugger::EvalEntries($i,rightL) $val
	    if { $type == "error" } {
		$RamDebugger::EvalEntries($i,rightentryL) conf -fg red
	    } else {
		$RamDebugger::EvalEntries($i,rightentryL) conf -fg black
	    }
	}
    } else {
	if { $what == "do" } {
	    regexp {[0-9]+} $name i
	    set var [string trim $EvalEntries($i,leftL)]
	    if { $var == "" } { return }
	    set value [string trim $EvalEntries($name)]

	    if { $options(ConfirmModifyVariable) } {
		# its necessary to take out the focus out binding to avoid interaction
		# with this func
		set comm [bind $RamDebugger::EvalEntries($i,rightentryL) <FocusOut>]
		bind $RamDebugger::EvalEntries($i,rightentryL) <FocusOut> ""

		set ret [DialogWin::messageBox -default ok -icon question -message \
		        "Are you sure to change variable '$var' to value '$value'?" \
		        -title "Warning" -type okcancel]

		bind $RamDebugger::EvalEntries($i,rightentryL) <FocusOut> $comm

		if { $ret == "cancel" } { return }
	    }
	    
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

    foreach "- yend" [$textOUT yview] break
    $textOUT conf -state normal
    TextInsertAndWrap $textOUT $data 200
    $textOUT conf -state disabled
    if { $yend == 1 } { $textOUT yview moveto 1 }
}

proc RamDebugger::TextOutInsertRed { data } {
    variable textOUT

    if { ![info exists textOUT] || ![winfo exists $textOUT] } { return }

    foreach "- yend" [$textOUT yview] break
    $textOUT conf -state normal
    TextInsertAndWrap $textOUT $data 200 red
    $textOUT tag configure red -foreground red
    $textOUT conf -state disabled
    if { $yend == 1 } { $textOUT yview moveto 1 }
}

proc RamDebugger::TextOutInsertBlue { data } {
    variable textOUT

    if { ![info exists textOUT] || ![winfo exists $textOUT] } { return }

    foreach "- yend" [$textOUT yview] break
    $textOUT conf -state normal
    TextInsertAndWrap $textOUT $data 200 blue
    $textOUT tag configure blue -foreground blue
    $textOUT conf -state disabled
    if { $yend == 1 } { $textOUT yview moveto 1 }
}

proc RamDebugger::TextCompClear {} {
    variable textCOMP

    if { ![info exists textCOMP] || ![winfo exists $textCOMP] } { return }

    $textCOMP conf -state normal
    $textCOMP del 1.0 end
    $textCOMP conf -state disabled
}

proc RamDebugger::TextInsertAndWrap { text data { maxlen 80 } { tag "" } } {

    set tolerance 20
    if { $maxlen < 30 } { set tolerance 5 }
    while 1 {
	foreach "line pos" [scan [$text index end] "%d.%d"] break
	if { $pos+[string length $data] <= $maxlen } {
	    if { $tag == "" } {
		$text insert end $data
	    } else {
		$text insert end $data $tag
	    }
	    break
	}
	set found 0
	for { set i [expr $maxlen-$pos] } { $i >= $maxlen-$pos-$tolerance } { incr i -1 } {
	    if { [string index $data $i] == " " } {
		set found 1
		break
	    }
	}
	if { $found } {
	     if { $tag == "" } {
		 $text insert end "[string range $data 0 $i]\n  "
	     } else {
		 $text insert end "[string range $data 0 $i]\n  " $tag         
	     }
	    set data [string range $data [expr $i+1] end]
	} else {
	    if { $tag == "" } {
		$text insert end "[string range $data 0 [expr $maxlen-$pos]]\n  "
	    } else {
		$text insert end "[string range $data 0 [expr $maxlen-$pos]]\n  " $tag
	    }
	    set data [string range $data [expr $maxlen-$pos+1] end]
	}
    }
}

proc RamDebugger::TextCompInsert { data } {
    variable textCOMP

    if { ![info exists textCOMP] || ![winfo exists $textCOMP] } { return }

    foreach "- yend" [$textCOMP yview] break
    $textCOMP conf -state normal
    TextInsertAndWrap $textCOMP $data
    $textCOMP conf -state disabled
    if { $yend == 1 } { $textCOMP yview moveto 1 }
}

proc RamDebugger::TextCompInsertRed { data } {
    variable textCOMP

    if { ![info exists textCOMP] || ![winfo exists $textCOMP] } { return }

    foreach "- yend" [$textCOMP yview] break
    $textCOMP conf -state normal
    $textCOMP ins end $data red
    $textCOMP tag configure red -foreground red
    $textCOMP conf -state disabled
    if { $yend == 1 } { $textCOMP yview moveto 1 }
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

proc RamDebugger::ConfigureLabel { label } {

    set tt [$label cget -text]
    if { [string match "...*" $tt] } {
	set tt [$label cget -helptext]
    }

    set lwidth [expr [winfo width $label]-5]
    if { [font measure [$label cget -font] $tt] <= $lwidth } {
	$label configure -text $tt -helptext ""
    } else {
	$label configure -helptext $tt
	while { [string length $tt] > 3 } {
	    set tt [string range $tt 1 end]
	    if { [font measure [$label cget -font] ...$tt] <= $lwidth } {
		break
	    }
	}
	$label configure -text ...$tt
    }
}

proc RamDebugger::ListBoxLabelMenu { w x y } {
    variable currentfile
    variable options
    variable WindowFilesList
    variable MainDir

    set dirs [list $options(defaultdir)]
    set sep 0
    if { $currentfile != "" && $currentfile != "New file" && \
	    [file dirname $currentfile] != $options(defaultdir) } {
	if { !$sep } {
	    lappend dirs ---
	    set sep 1
	}
	lappend dirs [file dirname $currentfile]
    }
    set sep 0
    foreach i $WindowFilesList {
	set dir [file dirname $i]
	if { $dir != $options(defaultdir) && [lsearchfile $dirs $dir] == -1 } {
	    if { !$sep } {
		lappend dirs ---
		set sep 1
	    }
	    lappend dirs $dir
	}
    }

    if { ![info exists options(RecentFiles)] } {
	set options(RecentFiles) ""
    }
    set sep 0
    foreach i $options(RecentFiles) {
	set dir [file dirname $i]
	if { $dir != $options(defaultdir) && [lsearchfile $dirs $dir] == -1 } {
	    if { !$sep } {
		lappend dirs ---
		set sep 1
	    }
	    lappend dirs $dir
	}
    }
    set sep 0
    if { $MainDir != $options(defaultdir) && [lsearchfile $dirs $MainDir] == -1 } {
	if { !$sep } {
	    lappend dirs ---
	    set sep 1
	}
	lappend dirs $MainDir
    }

    set menu $w.menu
    catch { destroy $menu }
    
    menu $menu
    foreach dir $dirs {
	if { $dir == "---" } {
	    $menu add separator
	} else {
	    set label $dir
	    if { $label > 50 } { set label ...[string range $label end-47 end] }
	    $menu add command -label $label -command \
		    "[list set RamDebugger::options(defaultdir) $dir] ;\
		    RamDebugger::FillListBox"
	}
    }
    tk_popup $menu $x $y
}

proc RamDebugger::FillListBox {} {
    variable listbox
    variable listboxlabel
    variable options
    variable instrumentedfilesP
    variable instrumentedfilesR
    variable instrumentedfilesSent
    variable remoteserverType
    variable images

    if { $listbox == "" || ![winfo exists $listbox] } { return }

    $listboxlabel configure -text $options(defaultdir)
    ConfigureLabel $listboxlabel

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
	    } elseif { [GiveInstFile $fullpath 1 P] != "" } {
		set img $images(file_yellow)
	    } else { set img [Bitmap::get file] }

	    $listbox insert end $item -image $img -text $i -data \
		[list file [file join $options(defaultdir) $i]]
	}
    }
}

proc RamDebugger::PrevNextCompileError { what } {
    variable textCOMP

    $textCOMP tag conf sel2 -background [$textCOMP tag cget sel -background]
    $textCOMP tag conf sel2 -foreground [$textCOMP tag cget sel -foreground]

    set err [catch { $textCOMP index sel2.first} idx]
    if { $err } {
	switch $what {
	    next { set idx 1.0 }
	    prev { set idx end-1c }
	}
	$textCOMP tag add sel2 "$idx linestart" "$idx lineend"
    } else {
	$textCOMP tag remove sel2 1.0 end
	set idxini $idx
	while { [$textCOMP tag range sel2] == "" } {
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
	    $textCOMP tag add sel2 "$idx linestart" "$idx lineend"
	    if { $idx == $idxini } { break }
	}
    }
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

	    # strange case in Windows mingw
	    if { [regexp {^//([a-zA-Z])(/.*)} $file {} letter sfile] && \
		[file exists $letter:$sfile] } {
		set file $letter:$sfile
	    }
	    
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

		if { [regexp {^\#([0-9]+)} $data {} stacktrace] } {
		    EvalRemote "frame $stacktrace"
		}
		return
	    }
	}
    }
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

	if { [regexp {\.(c|cc)$} $name] } {
	    $menu add separator
	    set w [winfo toplevel $listb]
	    $menu add command -label Compile -command [list cproject::Compile $w $name]
	}
	if { [regexp {Makefil.*[^~]$} $name] } {
	    $menu add separator
	    $menu add command -label Compile -command [list RamDebugger::Compile $name]
	}
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
    variable breakpoints

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

    set delta [expr {$l2_new-$l2_old}]
    if { $delta != 0 } {
	for { set i 0 } { $i < [llength $breakpoints] } { incr i } {
	    set br [lindex $breakpoints $i]
	    if { [AreFilesEqual [lindex $br 1] $currentfile] } {
		set line [lindex $br 2]
		if { $delta < 0 && $line >= $l2_new && $line < $l2_old } {
		    UpdateArrowAndBreak $line 0 ""
		    set breakpoints [lreplace $breakpoints $i $i]
		    incr i -1 ;# breakpoints has now one element less
		}
		if { $line >= $l2_old } {
		    UpdateArrowAndBreak $line 0 ""
		    set line [expr {$line+$delta}]
		    UpdateArrowAndBreak $line 1 ""
		    set br [lreplace $br 2 2 $line]
		    set breakpoints [lreplace $breakpoints $i $i $br]
		}
	    }
	}
	UpdateRemoteBreaks
    }

    set Numlines [scan [$text index end] %d]
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
	set ::tkPriv(selectMode) word ;# tcl8.3
	catch { set ::tk::Priv(selectMode) word } ;# tcl8.4
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
	scan [$text index sel.last] "%d.%d" line2 pos2
	if { $pos2 == 0 } { incr line2 -1 }
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

# proc RamDebugger::IndentCurrent {} {
#     variable text

#     scan [$text index insert] "%d.%d" line pos
#     IndentLine $line $pos
# }

proc RamDebugger::Indent {} {
    variable text

    set pos -1
    if { [catch {
	scan [$text index sel.first] "%d" line1
	scan [$text index sel.last] "%d" line2
    }] } {
	scan [$text index insert] "%d.%d" line1 pos
	set line2 $line1
    }
    for { set i $line1 } { $i <= $line2 } { incr i } {
	IndentLine $i $pos
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
    set FirstPos -1
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
    if { $FirstPos == -1 } { set FirstPos $col }
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

    if { [regexp {index|bbox|get} $command] } { return }
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

	bind $pane2.e$i <Return> "RamDebugger::CheckEvalEntries$suffix do $i,right$suffix"

	bind $pane2.e$i <FocusOut> "RamDebugger::CheckEvalEntries$suffix do $i,left$suffix"
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

proc RamDebugger::MoveCanvas { text canvas } {

    $canvas yview moveto [lindex [$text yview] 0]

    # fine adjustment
    # as the text widget adds arbitrarily more space at the end of the text,
    # it is necessary to fine tune by hand

    set yb1 [$canvas canvasy 0]
    set yb2 [$canvas canvasy [winfo height $text]]
    foreach i [$canvas find overlapping 0 $yb1 20 $yb2] {
	regexp {l([0-9]+)} [$canvas gettags $i] {} line
	set yline ""
	foreach "- yline - -" [$text bbox $line.0] break
	if { $yline == "" } { continue }
	set yline [$canvas canvasy $yline]
	foreach "- ycanvas - -" [$canvas bbox [$canvas find withtag l$line]] break
	incr ycanvas -3
	foreach "cy1 cy2" [$canvas yview] break
	set 1p [expr ($cy2-$cy1)/double([winfo height $text])]
	$canvas yview moveto [expr $cy1+($ycanvas-$yline)*$1p]
	return
    }
}

proc RamDebugger::ScrollTextAndCanvas { text canvas args } {
    eval $text yview $args
    MoveCanvas $text $canvas
}

proc RamDebugger::ScrollScrollAndCanvas { text yscroll canvas args } {
    eval $yscroll set $args
    $canvas yview moveto [lindex [$text yview] 0]
    MoveCanvas $text $canvas
}

proc RamDebugger::InitOptions {} {

#      option add *background AntiqueWhite3
#      option add *Button*background bisque3
#      option add *Menu*background bisque3
#      option add *Button*foreground black
#      option add *Entry*background thistle
#      option add *DisabledForeground grey60
#      option add *HighlightBackground AntiqueWhite3
    

    if { $::tcl_platform(platform) != "windows" } {
	option add *selectBackground \#48c96f
	option add *selectForeground white
    } else {
	option add *selectBackground \#48c96f
	option add *selectForeground white
    }
    option add *Menu*TearOff 0

}

# what can be save or go or clean
proc RamDebugger::PositionsStack { what } {
    variable SavedPositionsStack
    variable text
    variable currentfile

    set line [scan [$text index insert] %d]
    set tag $currentfile:$line
    
    switch $what {
	save {
	    while { [set pos [lsearch $SavedPositionsStack $tag]] != -1 } {
		set SavedPositionsStack [lreplace $SavedPositionsStack $pos $pos]
	    }
	    lappend SavedPositionsStack $tag
	    SetMessage "Saved position in line $line"
	}
	go {
	    if { [set pos [lsearch $SavedPositionsStack $tag]] != -1 } {
		incr pos -1
		if { $pos < 0 } { set pos end }
	    } else { set pos end }
	    set tag [lindex $SavedPositionsStack $pos]
	    if { $tag == "" } {
		SetMessage "Stack is void"
		return
	    }
	    regexp {^(.+):([0-9]+)$} $tag {} file line
	    if { ![AreFilesEqual $file $currentfile] } {
		RamDebugger::OpenFileF $file
	    }
	    $text mark set insert $line.0
	    $text see $line.0
	    SetMessage "Gone to position in line $line"
	}
	clean {
	    set SavedPositionsStack ""
	    SetMessage "Clean positions stack"
	}
    }
}

proc RamDebugger::ApplyDropBinding { w command } {

    if { [info command dnd] == "" } { return }

    if { $::tcl_platform(platform) == "windows"} {
	dnd bindtarget $w Files <Drop> $command
    } else {
	dnd bindtarget $w text/uri-list <Drop> $command
	foreach i [winfo children $w] {
	    ApplyDropBinding $i $command
	}
    }
}

proc RamDebugger::DropBindingDone { files } {

    foreach i $files {
	OpenFileF $i
    }
}

proc RamDebugger::InitGUI { { w .gui } } {
    variable options
    variable options_def
    variable marker
    variable text
    variable mainframe
    variable listbox
    variable listboxlabel
    variable pane2in1
    variable images
    variable textST
    variable textOUT
    variable textCOMP
    variable breakpoints
    variable MainDir
    variable TimeMeasureData


    proc ::bgerror { errstring } {
	if { [info command RamDebugger::TextOutRaise] != "" } {
	    RamDebugger::TextOutRaise
	    RamDebugger::TextOutInsertRed "-------------ERROR FROM RAMDEBUGGER-----------\n"
	    RamDebugger::TextOutInsertRed $::errorInfo
	    RamDebugger::TextOutInsertRed "----------------------------------------------\n"
	    WarnWin $errstring
	} else { puts "$errstring ($::errorInfo)" }
    }

    # dirty trick to avoid conflicts with other bwidget packages
    # only necessary when working inside a master
    auto_load ComboBox

    package require Tablelist
    package require BWidgetR
    package require supergrid
    package require supertext
    package require dialogwin
    package require helpviewer
    catch { package require tkdnd } ;# only if it is compiled

    CreateImages
    TkBackCompatibility
    CreateModifyFonts
    InitOptions

    toplevel $w
    wm title $w RamDebugger
    wm protocol $w WM_DELETE_WINDOW "RamDebugger::ExitGUI"
    ApplyDropBinding $w [list RamDebugger::DropBindingDone %D]

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
		    [list command "&Indent region" {} "Indent selected region or line" "Print Tab" \
		        -command "RamDebugger::Indent"] \
		    [list command "&Comment region" {} "Comment selected region" "F6" \
		        -command "RamDebugger::CommentSelection on"] \
		    [list command "&Uncomment region" {} "Un-comment selected region" "Shift F6" \
		        -command "RamDebugger::CommentSelection off"] \
		    separator \
		    [list command "Center display" {} "Center text display" "Ctrl l" \
		        -command "RamDebugger::CenterDisplay"] \
		    [list command "Search in files" {} "Search for pattern in given files" \
		    "ShiftCtrl f" \
		        -command "RamDebugger::SearchInFiles"] \
		    separator \
		    [list command "&Save position" {} "Save position to stack" "Shift F2" \
		        -command "RamDebugger::PositionsStack save"] \
		    [list command "&Go to position" {} "Recover position from stack" "F2" \
		        -command "RamDebugger::PositionsStack go"] \
		    [list command "&Clean positions stack" {} "Clean positions stack" "Ctrl F2" \
		        -command "RamDebugger::PositionsStack clean"] \
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
		-command "RamDebugger::DisplayVarWindow $w"] \
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
		[list cascad "C&onfiguration" {} activeconfiguration 0 [list \
		        [list radiobutton "Debug" activeconfiguration "Compile debug version" "" \
		        -variable RamDebugger::options(debugrelease) -value debug -selectcolor black] \
		        [list radiobutton "Release" activeconfiguration "Compile release version" "" \
		        -variable RamDebugger::options(debugrelease) -value release \
		            -selectcolor black] \
		        ] \
		] \
		[list command "Co&mpile" c++entry "Compile project" "F7" \
		-command "cproject::Compile $w"] \
		[list command "Com&pile non stop" c++entry "Compile project, do not stop on errors" \
		"Ctrl F7" -command "cproject::CompileNoStop $w"] \
		[list command "Compile all" c++entry "Compile project, all targets" "" \
		-command "cproject::CompileAll $w"] \
		separator \
		[list command "&Touch files" c++entry "Actualize date for all compilation files" "" \
		-command "cproject::TouchFiles $w"] \
		[list command "Cl&ean" c++entry "Clean compiled project files" "" \
		-command "cproject::CleanCompiledFiles $w"] \
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
		[list command "&View instrumented time file" {} "View instrumented time file" "" \
		-command "RamDebugger::ViewInstrumentedFile time"] \
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

    set menu [$mainframe getmenu view]
    $menu configure -postcommand [list RamDebugger::ActualizeViewMenu $menu]

    set menu [$mainframe getmenu recentfiles]
    $menu configure -postcommand [list RamDebugger::AddRecentfilesToMenu $menu]

    # very dirty. Without it, the radiobutton indicator is not drawn. Why???
    set menu [$mainframe getmenu activeconfiguration]
    $menu conf -postcommand "$menu conf -selectcolor black"

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
	 -helptext "Set/unset &breakpoint" \
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
    set listboxlabel [Label $pane1.l -anchor e -relief raised -bd 1 \
	-padx 5 -grid "0 ew"]
    bind $listboxlabel <Configure> "RamDebugger::ConfigureLabel $listboxlabel"

    bind $listboxlabel <ButtonPress-1> "RamDebugger::ListBoxLabelMenu $listboxlabel %X %Y"
    bind $listboxlabel <ButtonPress-3> "RamDebugger::ListBoxLabelMenu $listboxlabel %X %Y"

    set sw [ScrolledWindow $pane1.lf -relief sunken -borderwidth 0 -grid 0]
    set listbox [ListBox $sw.lb -background white -multicolumn 0 -selectmode single]
    $sw setwidget $listbox

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

    set pane2in2 [$pwin add -weight $weight2in]

    NoteBook $pane2in2.nb -homogeneous 1 -bd 1 -internalborderwidth 0 \
	    -grid "0 py2" -side bottom

    set f1 [$pane2in2.nb insert end stacktrace -text "Stack trace"]
 
    set sw2 [ScrolledWindow $f1.lf2 -relief sunken -borderwidth 0 -grid "0" \
	    -scrollbar both -auto both]
    set textST [text $sw2.text2 -background white -wrap none -width 80 -height 4 \
	    -highlightthickness 0 -exportselection 0]
    $sw2 setwidget $textST

    supergrid::go $f1

    set f2 [$pane2in2.nb insert end output -text "Output"]

    set sw2 [ScrolledWindow $f2.lf2 -relief sunken -borderwidth 0 -grid "0" \
	    -scrollbar both -auto both]
    set textOUT [text $sw2.text2 -background white -wrap none -width 80 -height 4 \
	    -highlightthickness 0 -exportselection 0]
    $sw2 setwidget $textOUT

    supergrid::go $f2

    set f3 [$pane2in2.nb insert end compile -text "Compile"]

    set sw2 [ScrolledWindow $f3.lf3 -relief sunken -borderwidth 0 -grid "0" \
	    -scrollbar both -auto both]
    set textCOMP [text $sw2.text2 -background white -wrap none -width 80 -height 4 \
	    -highlightthickness 0 -exportselection 0]
    $sw2 setwidget $textCOMP

    supergrid::go $f3

    #$pane2in2.nb compute_size
    $pane2in2.nb raise stacktrace

    proc NoteBookPopupMenu { f x y page } {

	if { $page == "stacktrace" } { return }
	catch { destroy $f.m }
	menu $f.m
	switch $page {
	    output { $f.m add command -label Clear -command RamDebugger::TextOutClear }
	    compile { $f.m add command -label Clear -command RamDebugger::TextCompClear }
	}
	tk_popup $f.m $x $y
    }
    $pane2in2.nb bindtabs <ButtonPress-3> [list RamDebugger::NoteBookPopupMenu %W %X %Y]
    bind $textOUT <ButtonPress-3> [list RamDebugger::NoteBookPopupMenu %W %X %Y output]
    bind $textCOMP <ButtonPress-3> [list RamDebugger::NoteBookPopupMenu %W %X %Y compile]


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

    checkbutton $pane3in2.l1 -text "Local variables" -relief raised -bd 1 -grid "0 ew" \
	-variable RamDebugger::options(ViewLocalVariables) \
	-command "RamDebugger::CheckEvalEntriesL do"

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

    ################################################################################
    # the bindings
    ################################################################################

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
    bind $text <Motion> "RamDebugger::TextMotion %X %Y %x %y"
    # in linux, F10 makes some stupid thing
    bind all <F10> ""

    bind $text <Alt-Left> "RamDebugger::GotoPreviusNextInWinList prev ; break"
    bind $text <Control-Tab> "RamDebugger::GotoPreviusNextInWinList prev ; break"
    bind $text <Alt-Right> "RamDebugger::GotoPreviusNextInWinList next ; break"
    bind $text <Control-Shift-Tab> "RamDebugger::GotoPreviusNextInWinList next ; break"
    bind $text <Tab> "RamDebugger::Indent ; break"
    bind [winfo toplevel $text] <Tab> ""

    bind $w <Shift-Key-F5> "RamDebugger::DisconnectStop ;break"

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

    # BWidgets automatically sets these because they are in the main main
    # we only want them individually in every widget
    bind $w <Control-c> ""
    bind $w <Control-v> ""

    ################################################################################
    # start up options
    ################################################################################

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

    set menu [$mainframe getmenu recentfiles]
    ActualizeActivePrograms $menu

    if { [info exists options(breakpoints)] } {
	set breakpoints $options(breakpoints)
    }
    if { [info exists options(TimeMeasureData)] } {
	set TimeMeasureData $options(TimeMeasureData)
    }

    if { [info exists options(debuggerstate)] && $options(debuggerstate) == "time" } {
	RamDebugger::DisplayTimesWindow
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
	FillListBox

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

    # it is done in this way because if not, the definition gets reload
    uplevel \#0 {
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
    }

    # if we do it at the beginning, an ugly update is made
    if { $::tcl_platform(platform) != "windows" } {
	wm iconbitmap $w @$MainDir/addons/ramdebugger.xbm
    } else {
	wm iconbitmap $w -default $MainDir/addons/ramdebugger.ico
    }

}

if { [info command master] != "" } {
    set registerasremote 0
} else { set registerasremote 1 }

if { [lsearch $argv "-noprefs"] != -1 } {
    set readprefs 0
} else { set readprefs 1 }

RamDebugger::Init $readprefs $registerasremote

################################################################################
#     Init the GUI part
################################################################################

if { [info command wm] != "" && [info commands tkcon_puts] == "" } {
    wm withdraw .
    RamDebugger::InitGUI
    bind all <Control-x><Control-l> "source [info script] ; WarnWin reload"
}
if { [info command master] != "" } {
    RamDebugger::rdebug -master
}
