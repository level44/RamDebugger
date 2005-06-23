#!/bin/sh
# the next line restarts using wish \
exec wish "$0" "$@"
#         $Id: RamDebugger.tcl,v 1.56 2005/06/23 11:36:57 ramsan Exp $        
# RamDebugger  -*- TCL -*- Created: ramsan Jul-2002, Modified: ramsan Jan-2005

package require Tcl 8.4
#package require Tk 8.4


if { [info exists ::starkit::topdir] } {
    # This is for the starkit in UNIX to start graphically
    # that the following line out if you want to run without GUI
    package require Tk 8.4
}

################################################################################
#  This software is copyrighted by Ramon Ribó (RAMSAN) ramsan@compassis.com
#  (http://gid.cimne.com/ramsan) The following terms apply to all files 
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
    #    RamDebugger version
    ################################################################################

    set Version 5.4

    ################################################################################
    #    Non GUI commands
    ################################################################################

    namespace export rhelp rdebug rlist reval rcont rnext rstep rbreak rcond rinfo rdel \
	rstack routput rtime renabledisable

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
    variable AppDataDir

    ################################################################################
    # GUI state
    ################################################################################

    variable text ""
    variable textST
    variable textOUT
    variable textCOMP
    variable IsInStop 0
    variable TextMotionAfterId ""
    variable afterid_formessage ""
    variable ExpressionResult ""
    variable count
    variable listbox ""
    variable progressvar
    variable status
    variable WindowFilesList ""
    variable WindowFilesListLineNums ""

    ################################################################################
    # Handlers to save files. Array with names: filename
    ################################################################################

    variable FileSaveHandlers

    ################################################################################
    # Preferences
    ################################################################################

    variable readwriteprefs
    variable options
    variable options_def

}

################################################################################
#   Init proc
################################################################################

proc RamDebugger::Init { _readwriteprefs { registerasremote 1 } } {
    variable debuggerserver
    variable debuggerserverNum
    variable MainDir
    variable CacheDir
    variable AppDataDir
    variable options_def
    variable options
    variable readwriteprefs $_readwriteprefs
    variable iswince

    if { [info exists ::freewrap::scriptFile] } {
	set dir $::argv0
    } else {
	set dir [info script]
    }
    if {[file type $dir] eq "link"} {
	set dir [file readlink $dir]
    }
    set dir [file join [pwd] [file dirname $dir]]
    set MainDir $dir

    if { ![file isdir [file join $MainDir addons]] } {
	set text "error: bad installation. Directory 'addons' could not be found in '$MainDir'"
	puts $text
	catch { tk_messageBox -message $text }
    }

    if { [info command winfo] ne "" && [winfo screenwidth .] < 300 } {
	set iswince 1
    } else { set iswince 0 }

    lappend ::auto_path [file join $MainDir addons]
    lappend ::auto_path [file join $MainDir scripts]

    if { $::tcl_platform(platform) eq "windows" } {
	if { [info exists ::env(APPDATA)] } {
	    set AppDataDir [file join $::env(APPDATA) RamDebugger]
	} else {
	    package require registry
	    set key {HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion}
	    append key {\Explorer\Shell Folders}
	    set err [catch { registry get $key AppData } AppData]
	    if { !$err } {
		set AppDataDir [file join [registry get $key AppData] RamDebugger]
	    } else {
		set AppDataDir [file join $::env(HOME) .RamDebugger]
	    }
	}
    } else {
	set AppDataDir [file join $::env(HOME) .RamDebugger]
    }

    if { [auto_execok cvs] eq "" && $::tcl_platform(platform) eq "windows" && \
	     !$iswince} {
	set exe [file join $AppDataDir exe]
	if { ![file exists $exe] } {
	    file mkdir $exe
	    foreach i [list cat.exe cvs.exe diff.exe grep.exe kill.exe tlist.exe] {
		file copy [file join $MainDir addons $i] $exe
	    }
	}
	if { ![info exists ::env(PATH)] } {
	    set list ""
	} else {
	    set list [split $::env(PATH) \;]
	}
	set shortname [file native [file attributes $exe -shortname]]
	if { [lsearch -exact $list $shortname] == -1 } {
	    lappend list $shortname
	    set ::env(PATH) [join $list \;]
	    # this is a variable from the TCL library
	    array unset ::auto_execs
	}
    }

    set dirs ""
    lappend dirs [file join $AppDataDir cache]
    lappend dirs [file join $MainDir cache]

    foreach i $dirs {
	catch { file mkdir $i }
	if { [file isdirectory $i] } {
	    set CacheDir $i
	    break
	}
    }

    ################################################################################
    # Setting preferences
    ################################################################################

    set options_def(indentsizeTCL) 4
    set options_def(indentsizeC++) 2
    set options_def(ConfirmStartDebugging) 1
    set options_def(instrument_source) auto
    set options_def(instrument_proc_last_line) 0
    set options_def(ConfirmModifyVariable) 1
    set options_def(openfile_browser) 1
    set options_def(LocalDebuggingType) tk
    set options_def(executable_dirs) ""
    set options_def(debugrelease) debug
    set options_def(ViewLocalVariables) 1
    set options_def(saved_positions_stack) ""
    set options_def(showstatusbar) 1
    set options_def(showbuttonstoolbar) 1
    set options_def(CompileFastInstrumenter) -1

    set options_def(colors,foreground) black
    set options_def(colors,background) white
    set options_def(colors,commands) magenta
    set options_def(colors,defines) magenta2
    set options_def(colors,procnames) blue
    set options_def(colors,quotestrings) grey40
    set options_def(colors,set) green
    set options_def(colors,comments) red
    set options_def(colors,varnames) \#b8860b

    set optiods_def(listfilespane) 0
    
    switch $::tcl_platform(platform) {
	windows {
	    if { $iswince } {
		# Wince
		set options_def(NormalFont) { -family "Tahoma" -size 7 -weight normal \
		                                  -slant roman -underline 0 -overstrike 0 }
		set options_def(FixedFont)  { -family "Courier" -size 7 -weight normal \
		                                  -slant roman -underline 0 -overstrike 0 }
		set options_def(HelpFont)  { -family "Helvetica" -size 7 -weight normal \
		                                 -slant roman -underline 0 -overstrike 0 }
		set options_def(showstatusbar) 0
		set options_def(ViewOnlyTextOrAll) OnlyText
	    } else {
		set options_def(NormalFont) { -family "MS Sans Serif" -size 8 -weight normal \
		                                  -slant roman -underline 0 -overstrike 0 }
		set options_def(FixedFont)  { -family "Courier" -size 8 -weight normal \
		                                  -slant roman -underline 0 -overstrike 0 }
		set options_def(HelpFont)  { -family "Helvetica" -size 11 -weight normal \
		                                 -slant roman -underline 0 -overstrike 0 }
	    }
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

    set options_def(extensions,TCL) ".tcl *"
    set options_def(extensions,C/C++) ".c .cpp .cc .h"
    set options_def(extensions,XML) ".xml .html .htm"
    set "options_def(extensions,GiD BAS file)" .bas
    set "options_def(extensions,GiD data files)" ".prb .mat .cnd"


    # this variable is only used on windows. It can be:
    # 0: Only check remote programs on demand (useful if not making remote debugging, the
    #    start up is faster)
    # 1: Register as remote and check remote programs on start up. It can be slower the
    #    start up but is better when making remote debugging

    if { $::tcl_platform(platform) == "windows" } {
	set options_def(CheckRemotes) 1
    }

    set options_def(AutoSaveRevisions) 1
    set options_def(AutoSaveRevisions_time) 5
    set options_def(AutoSaveRevisions_idletime) 5

    ################################################################################
    # Reading preferences (they are only saved in GUI mode)
    ################################################################################

    if { $::tcl_platform(platform) == "windows" } {
	if { [catch { package require registry }] } {
	    foreach i [info loaded] {
		if { [string equal -nocase [lindex $i 1] "registry"] } {
		    load [lindex $i 0]
		    break
		}
	    }
	}
	package require registry
    }

    array set options [array get options_def]

    if { $readwriteprefs } {
	catch {
	    if { $::tcl_platform(platform) == "windows" } {
		set data [registry get {HKEY_CURRENT_USER\Software\RamDebugger} IniData]
	    } else {
		set fin [open ~/.ramdebugger_prefs r]
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
	if { [file isdirectory $file] && [lsearch -exact $options(executable_dirs) $file] == -1 } {
	    lappend options(executable_dirs) $file
	}
	foreach i [glob -nocomplain -dir c: mingw*] {
	    set dirs_in [glob -nocomplain -dir $i mingw*]
	    foreach j [concat [list $i] $dirs_in] {
		if { [file isdirectory [file join $j bin]] } {
		    set file [filenormalize [file join $j bin]]
		    if { [lsearch -exact $options(executable_dirs) $file] == -1 } {
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
	if { [lsearch -exact $list $i] == -1 } {
	    lappend list $i
	    set haschanged 1
	}
    }
    if { $haschanged } {
	set ::env(PATH) [join $list \;]
	# this is a variable from the TCL library
	array unset ::auto_execs
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
    variable initialcommands

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
	    catch { local eval destroy . }
	    catch { interp delete local }
	} elseif { $remoteserverType == "gdb" } {
	    catch {
		#puts -nonewline [lindex $remoteserver 0] {\x03}
		puts [lindex $remoteserver 0] quit
		close [lindex $remoteserver 0]
	    }
	}
	set remoteserver ""
	set remoteserverType ""
	if { $debuggerstate == "debug" } { set debuggerstate "" }
	TakeArrowOutFromText
	return
    }
    TextOutClear

    if { $opts(-master) } {
	if { $remoteserverType == "local" } {
	    catch { local eval destroy . }
	    catch { interp delete local }
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
	if { [interp exists local] } {
	    catch { local eval destroy . }
	    catch { interp delete local }
	}
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
	local eval [list set ::auto_path $::auto_path]
	local eval {
	    if { [info exists env(TCLLIBPATH)] } {
		foreach i $env(TCLLIBPATH) {
		    lappend ::auto_path $i
		}
	    }
	}

	set filetodebug $currentfile
	set LocalDebuggingType tk
	if { [info exists options(LocalDebuggingType)] } {
	    set LocalDebuggingType $options(LocalDebuggingType)
	}
	if { [info exists options(currentfileargs5)] } {
	    foreach "curr curr_as dir_in arg_in tcl_or_tk" $options(currentfileargs5) {
		if { $curr == $currentfile } {
		    if { $curr_as != "" } {
		        set filetodebug $curr_as
		    }
		    if { [file isdirectory $dir_in] } {
		        local eval [list cd $dir_in]
		        set txt "Executing '$filetodebug'\nin directory: $dir_in"
		    } else { set txt "Executing '$filetodebug'" }
		    TextOutInsertBlue $txt
		    SetMessage "Executing '$filetodebug'"
		    local eval [list set argc [llength $arg_in]]
		    local eval [list set argv $arg_in]
		    TextOutInsertBlue "Using arguments: '$arg_in'"
		    if { $tcl_or_tk != "auto" } {
		        set LocalDebuggingType $tcl_or_tk
		        TextOutInsertBlue "Considering file as type: $tcl_or_tk"
		    }
		    TextOutInsertBlue "Defined in: File->Debug on->Current file arguments"
		    break
		}
	    }
	}
	local eval [list set argv0 $filetodebug]

	set err [catch {package present registry} ver]
	if { !$err } {
	    interp alias local registry "" registry
	    interp eval local package provide registry $ver
	}
	if { $LocalDebuggingType == "tk" } {
	    interp eval local [list load {} Tk]
	    #interp eval local package require Tk
	    local eval [list bind . <Destroy> { if { "%W" == "." } { exit } }]
	}
	set remoteserverType local
	if { $filetodebug == "" } {
	    error "Error. there is no current file"
	}
	local eval [cd [file dirname $filetodebug]]

	set remoteserver $filetodebug
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
	    catch { local eval destroy . }
	    catch { interp delete local }
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
	if { [lsearch -exact [array names services] $opts(program)] == -1 } {
	    FindActivePrograms 1
	}
	if { [lsearch -exact [array names services] $opts(program)] != -1 } {
	    set remoteserver $opts(program)
	    set remoteserverNum $services($remoteserver)
	} else { error "error. $usagestring\nActive programs: [array names services]" }
	if { $remoteserverType == "local" } {
	    catch { local eval destroy . }
	    catch { interp delete local }
	} elseif { $remoteserverType == "gdb" } {
	    catch {
		puts [lindex $remoteserver 0] quit
		close [lindex $remoteserver 0]
	    }
	}
	set remoteserverType remote
	TakeArrowOutFromText
    }

    set remotecomm {
	namespace eval RDC {
	    variable breaks
	    variable evalhandler ""
	    variable code ""
	    variable stopnext 0
	    variable contto ""
	    variable outputline 0
	    variable lastprocstack ""
	    variable currentfile ""
	    variable linecounter 0
	}
	proc RDC::SendDev { comm } {
	    SENDDEVBODY
	}
	proc RDC::MeasureTime { name level timestr } {
	    SendDev [list RamDebugger::RecieveTimeFromProgram $name $level [lindex $timestr 0]]
	}
	proc RDC::Continue {} {
	    set ::RDC::code ""
	}
	proc RDC::Eval { comm { handler "" } } {
	    variable evalhandler $handler
	    set ::RDC::code $comm
	    update
	}
	proc RDC::GetLastVisited { } {
	    variable data
	    if { [info exists data(visited,filenum)] } {
		return [list $data(visited,filenum) $data(visited,line)]
	    } else { return "" }
	}
	proc RDC::F { filenum line } {
	    variable code
	    variable evalhandler
	    # == 1 next ; == 2 step ; == 3 nextfull
	    variable stopnext
	    variable contto
	    variable breaks
	    variable outputline
	    variable lastprocstack
	    variable linecounter

	    variable data
	    set data(visited,filenum) $filenum
	    set data(visited,line) $line

	    set procstack ""
	    set procname ""
	    for { set i [expr {[info level]-1}] } { $i >= 1 } { incr i -1 } {
		set prognameL [lindex [info level -$i] 0]
		set procnameL [uplevel $i [list namespace which -command $prognameL]]
		lappend procstack $procnameL
		if { $i == 1 } { set procname $procnameL }
	    }
	    set stop 0
	    set breaknum 0
	    set condinfo ""
	    set len [llength $procstack]
	    set lastlen [llength $lastprocstack]
	    switch $stopnext {
		1 {
		    set lm1 [expr {$len-1}]
		    if { $len <= $lastlen && $procstack == [lrange $lastprocstack 0 $lm1] } {
		        set stop 1
		    }
		}
		2 {
		    if { $len < $lastlen } { set lm1 [expr {$len-1}]
		    } else { set lm1 [expr {$lastlen-1}] }
		    if { [lrange $procstack 0 $lm1] == [lrange $lastprocstack 0 $lm1] } {
		        set stop 1
		    }
		}
		3 { set stop 1 }
	    }
	    if { [lindex $contto 0] == $filenum && [lindex $contto 1] == $line } {
		if { $len > $lastlen && [lindex $procstack end] eq [lindex $lastprocstack end] } {
		    # nothing
		} else {
		    set stop 1
		    set contto ""
		}
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
	    incr linecounter
	    if { $linecounter >= 500 } {
		RDC::SendDev update
		set linecounter 0
	    }
	    if { !$stop } {
		if { $outputline } {
		    set procname [lindex [info level -1] 0]
		    SendDev [list RamDebugger::RecieveFromProgram output \
		                 $filenum $line $procname "" ""]
		    
		}
		return
	    }
	    set lastprocstack $procstack
	    set textline ""
	    set code ""
	    set ::RDC::errorInfo $::errorInfo
	    set ::RDC::err [catch {
		regexp "RDC::F\\s+$filenum+\\s+$line\\s+; (\[^\n]*)" [info body $procname] {} \
		    qtextline
	    }]
	    if { $::RDC::err } { set ::errorInfo $::RDC::errorInfo }

	    RDC::SendDev [list RamDebugger::RecieveFromProgram $breaknum $filenum \
		              $line $procname $textline $condinfo]
	    while 1 {
		if { $code == "" } { vwait ::RDC::code }
		if { $code == "" } {
		    return
		}
		set err [catch { uplevel 1 $code } returnvalue]
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
	    proc ::source { args } {
		set file [file join [pwd] [lindex $args end]]
		set args [lreplace $args end end $file]

		set retval [RDC::SendDev "RamDebugger::RecieveFromProgramSource $args"]
		if { $retval != "" } {
		    if { ![string match "::RDC::sourceproc*" $retval] } {
		        set oldfile [info script]
		        # catch is here for version 8.3
		        catch { info script $file }
		    }
		    uplevel 1 $retval
		    if { ![string match "::RDC::sourceproc*" $retval] } {
		        # catch is here for version 8.3
		        catch { info script $oldfile }
		    }
		}
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
	    EvalRemote [list set ::RDC::currentfile $filetodebug]
	    set todo "[list RamDebugger::rlist -quiet -asmainfile $filetodebug];"
	    append todo "[list set RamDebugger::currentfile $currentfile]"
	    after idle $todo
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
    variable files

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
		    set errormessage "error: line $currentline is not instrumented"
		    set linetxt [lindex [split $files($currentfile) \n] [expr {$currentline-1}]]
		    if { [string trim $linetxt] == "\}" } {
		        append errormessage ". Consider option 'Instrument proc last line' "
		        append errormessage "in Preferences"
		    }
		    error $errormessage
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
	-full:    Tries to stop program execution in any line
	--:     end of options
    }
    ParseArgs $args $usagestring opts

    rlist -quiet
    StopAtGUI "" ""

    if { $remoteserverType != "gdb" } {
	if { $opts(-full) } {
	    EvalRemote [list set ::RDC::stopnext 3]
	} else {
	    EvalRemote [list set ::RDC::stopnext 1]
	}
	EvalRemote ::RDC::Continue
    } else {
	set remoteserver [lreplace $remoteserver 2 2 next]
	#EvalRemote \003
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
	-cleartimes:    Clear times table
	--:             end of options

	This function is used to obtain absolute and relative times of several blocks
	of the code. The process is: define one or several blocks giving the block name,
	the beginning line and the end line with option -add. After, select option -start.
	When finished measuring times, use option -display to see the results. Use -delete
	to finish.
    }
    ParseArgs $args $usagestring opts
    
    if { $opts(-cleartimes) } {
	set TimeMeasureDataNew ""
	foreach i $TimeMeasureData {
	    foreach "name file lineini lineend lasttime" $i {
		lappend TimeMeasureDataNew [list $name $file $lineini $lineend ""]
	    }
	}
	set TimeMeasureData $TimeMeasureDataNew
	return "cleared times table"
    }
    if { $opts(-start) } {
	set debuggerstate time

	set TimeMeasureDataNew ""
	set files ""
	set currentfile_save $currentfile
	set err [catch {
	    foreach i $TimeMeasureData {
		foreach "name file lineini lineend lasttime" $i {
		    lappend TimeMeasureDataNew [list $name $file $lineini $lineend ""]
		    if { [lsearchfile $files $file] == -1 } {
		        if { [file exists $file] } {
		            rlist -quiet $file
		            lappend files $file
		        }
		    }
		}
	    }
	} errorstring]
	set currentfile $currentfile_save
	if { $err } {
	    error $errorstring
	}
	set TimeMeasureData $TimeMeasureDataNew
	if { $remoteserverType == "local" && $remoteserver != "" } {
	    # why currentfile should be changed here?
	    #             set currentfile $remoteserver
	    rdebug -currentfile
	}
	if { $remoteserverType == "" && [info command master] != "" } {
	    rdebug -master
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
	set tdata ""
	for { set i 0 } { $i < [llength $TimeMeasureData] } { incr i } {
	    foreach "name file lineini lineend time" [lindex $TimeMeasureData $i] break
	    if { [llength $time] == 0 } {
		set tdate [lindex $TimeMeasureData $i]
	    } elseif { [llength $time] ==1 } {
		lappend tdata [list $name $file $lineini $lineend \
		                   [lindex [lindex $time 0] 1]]
	    } else {
		foreach j [lsort -integer -index 0 $time] {
		    lappend tdata [list "$name (level=[lindex $j 0])" $file $lineini $lineend \
		                       [lindex $j 1]]
		}
	    }
	}
	for { set i 0 } { $i < [llength $tdata] } { incr i } {
	    foreach "name - lineini lineend time" [lindex $tdata $i] break
	    if { ![info exists data($name)] } {
		set hilevel 0
		set data($name) [list $hilevel $time 0]
	    } else {
		set hilevel [lindex $data($name) 0]
		
	    }
	    lappend datanames $name
	    if { $time == "" } { continue }

	    incr hilevel
	    set sum 0
	    for {set j [expr $i+1] } { $j < [llength $tdata] } { incr j } {
		foreach "name_in - lineini_in lineend_in time_in" [lindex $tdata $j] break
		if { $lineini_in > $lineend } { break }
		if { $lineini_in == $lineini && $lineend_in == $lineend } { continue }
		if { $time_in == "" } {
		    set percent ""
		} else {
		    set percent [expr $time_in*100/double($time)]
		    incr sum $time_in
		}
		set data($name_in) [list $hilevel $time_in $percent]
	    }
	    if { $sum > 0 && $time != "" && $sum < $time } {
		set remname "Remaining time for '$name'"
		set time_rem [expr $time-$sum]
		set data($remname) [list $hilevel $time_rem [expr $time_rem*100/double($time)]]
		lappend datanames $remname
	    }
	}
	set unitname $opts(-display)
	switch -- $opts(-display) {
	    microsec { set unitfactor 1 ; set format %i }
	    milisec { set unitfactor 1e-3  ; set format %.4g }
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
	-encoding enc: open file with the given encoding
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
	set currentfile $currentfile_save
	error "it is necessary to enter a file name\n$usagestring"
    }

    set filetype [GiveFileType $currentfile]
    #     if { [regexp {\.(h|c|cc)$} $currentfile] } {
    #         set filetype c++
    #     } else {
    #         set filetype tcl
    #     }
    if { $currentfile == "*Macros*" && !$currentfileIsModified && \
	     ![info exists instrumentedfilesP($currentfile)] } {
	set files($currentfile) [GiveMacrosDocument]

	if { [lsearchfile $fileslist $currentfile] == -1 } {
	    lappend fileslist $currentfile
	}
	if { [info exists instrumentedfilesTime($currentfile)] } {
	    unset instrumentedfilesTime($currentfile)
	}
    }

    if { ($currentfile == "*New file*" || $currentfileIsModified ) && \
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
    if { [lsearchfile $fileslist $currentfile] == -1 } {
	lappend fileslist $currentfile
    }

    if { ![info exists files($currentfile)] || $force} {
	
	set err [catch [list open $currentfile r] fin]
	if { $err } {
	    set filetry $currentfile
	    set currentfile $currentfile_save
	    error "file '$filetry' does not exist\n$usagestring"
	}
	if { $opts(-encoding) != 0 && $opts(-encoding) != "" } {
	    fconfigure $fin -encoding $opts(-encoding)
	} else {
	    set header [read $fin 256]
	    if { [regexp -line -- {-\*-.*coding:\s*utf-8\s*;.*-\*-} $header] } {
		fconfigure $fin -encoding utf-8
	    }
	    seek $fin 0
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

    if { $filetype == "TCL" && ![info exists instrumentedfilesP($currentfile)] \
	     && !$force && !$reinstrument && !$currentfileIsModified } {
	set filenum [lsearchfile $fileslist $currentfile]

	foreach i [list P R] {
	    set instfile [GiveInstFile $currentfile 1 $i]

	    if { $instfile != "" } {
		set fin [open $instfile r]
		if { $opts(-encoding) != 0 && $opts(-encoding) != "" } {
		    fconfigure $fin -encoding $opts(-encoding)
		} else {
		    set header [read $fin 256]
		    if { [regexp -- {-\*-\s*coding:\s*utf-8\s*;\s*-\*-} $header] } {
		        fconfigure $fin -encoding utf-8
		    }
		    seek $fin 0
		}
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
    if { ($filetype == "TCL"  && ![info exists instrumentedfilesP($currentfile)]) || \
	     ![info exists instrumentedfilesInfo($currentfile)] || $force || $reinstrument } {
	SetMessage "Instrumenting file '$currentfile'..."

	set filenum [lsearchfile $fileslist $currentfile]

	if { $filetype == "C/C++" } {
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
	if { $filetype == "XML" } {
	    set err [catch { Instrumenter::DoWorkForXML $files($currentfile) instrumentedfilesInfo($currentfile) } errstring]
	    if { $err } {
		set einfo $::errorInfo
		RamDebugger::ProgressVar 100
		if { ![string match  "*user demand*" $errstring] } {
		    RamDebugger::TextOutRaise
		    RamDebugger::TextOutInsertRed $einfo
		}
		#WarnWin $errstring--$einfo
		WarnWin $errstring
	    }
	}
	if { $filetype == "GiD BAS file" } {
	    if { [catch {
		Instrumenter::DoWorkForBas $files($currentfile) instrumentedfilesInfo($currentfile)
	    } errstring] } {
		RamDebugger::ProgressVar 100
		if { ![string match  "*user demand*" $errstring] } {
		    RamDebugger::TextOutRaise
		    RamDebugger::TextOutInsertRed $::errorInfo
		}
		WarnWin $errstring
	    }
	}
	if { $filetype == "GiD data files" } {
	    if { [catch {
		Instrumenter::DoWorkForGiDData $files($currentfile) instrumentedfilesInfo($currentfile)
	    } errstring] } {
		RamDebugger::ProgressVar 100
		if { ![string match  "*user demand*" $errstring] } {
		    RamDebugger::TextOutRaise
		    RamDebugger::TextOutInsertRed $::errorInfo
		}
		WarnWin $errstring
	    }
	}
	if { $filetype == "TCL" } {
	    if { [catch {
		Instrumenter::DoWorkForTcl $files($currentfile) $filenum instrumentedfilesP($currentfile) \
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
		if { $opts(-encoding) != 0 && $opts(-encoding) != "" } {
		    fconfigure $fout -encoding $opts(-encoding)
		} else {
		    set header [string range [set instrumentedfiles${i}($currentfile)] 0 255]
		    if { [regexp -- {-\*-\s*coding:\s*utf-8\s*;\s*-\*-} $header] } {
		        fconfigure $fout -encoding utf-8
		    }
		}
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
    set breakpoints [lreplace $breakpoints $ipos $ipos [lreplace $i 4 4 $opts(cond)]]

    UpdateRemoteBreaks

    if { !$opts(-quiet) } {
	return "condition for breakpoint $opts(breakpointnum): $opts(cond)"
    }
}

proc RamDebugger::renabledisable { args } {
    variable breakpoints
    variable debuggerstate

    if { $debuggerstate == "time" } {
	error "Command renabledisable cannot be used in 'time' mode. Check rtime"
    }

    set usagestring {usage: renabledisable ?switches? breakpointnum
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
    set br [lindex $breakpoints $ipos]
    if { [lindex $br 1] } {
	lset br 1 0
	set enabledisable 0
    } else {
	lset br 1 1
	set enabledisable 1
    }
    set breakpoints [lreplace $breakpoints $ipos $ipos $br]

    UpdateRemoteBreaks

    if { !$opts(-quiet) } {
	return "breakpoint $opts(breakpointnum) enable/disable: $enabledisable"
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
    set currentfile_save $currentfile
    if { [catch {
	rlist -quiet $opts(file) $opts(line)
    } errcatch] } {
	set currentfile $currentfile_save
	error "[lindex [split $errcatch \n] 0]\n$usagestring"
    }

    set filenum [lsearch -exact $fileslist $currentfile]
    
    set filetype [GiveFileType $currentfile]
    if { $filetype == "TCL" } {
	set ipos [string first "RDC::F $filenum $currentline ;" $instrumentedfilesP($currentfile)]
	if { $ipos == -1 } {
	    set ipos [string first "RDC::F $filenum $currentline ;" $instrumentedfilesR($currentfile)]
	    if { $ipos == -1 } {
		set errormessage "error: line $currentline is not instrumented"
		set linetxt [lindex [split $files($currentfile) \n] [expr {$currentline-1}]]
		if { [string trim $linetxt] == "\}" } {
		    append errormessage ". Consider option 'Instrument proc last line' "
		    append errormessage "in Preferences"
		}
		set currentfile $currentfile_save
		error $errormessage
	    }
	}
    } elseif { $filetype == "C/C++" } {
	# nothing
    } else {
	set errormessage "error: this type of file does not permmit debugging"
	set currentfile $currentfile_save
	error $errormessage
    }
    
    set NumBreakPoint 1
    foreach i $breakpoints {
	if { [lindex $i 0] >= $NumBreakPoint } {
	    set NumBreakPoint [expr [lindex $i 0]+1]
	}
    }
    lappend breakpoints [list $NumBreakPoint 1 $currentfile $currentline ""]
    UpdateRemoteBreaks

    set currentfile $currentfile_save
    if { !$opts(-quiet) } {
	return "set breakpoint $NumBreakPoint at $opts(file) $currentline"
    }
}

proc RamDebugger::rinfo { args } {
    variable breakpoints
    variable currentfile

    set usagestring {usage: rinfo ?switches? ?line?
	-full:    returns all breakpoint information for line
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
	    if { [lindex $i 2] == $currentfile && [lindex $i 3] == $opts(line) } {
		if { $opts(-full) } {
		    lappend retval $i
		} else {
		    lappend retval [lindex $i 0]
		}
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

    if { [string index $file 0] == "*" } { return $file }
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

proc RamDebugger::GiveFileTypeForFileBrowser {} {
    variable options

    set types ""
    set exts [array names options extensions,*]
    set ipos [lsearch $exts extensions,TCL]
    set elm [lindex $exts $ipos]
    set exts [lreplace $exts $ipos $ipos]
    set exts [linsert $exts 0 $elm]
    foreach i $exts {
	regexp {,(.*)} $i {} type
	lappend types [list $type $options($i)]
    }
    lappend types [list "All Files" "*"]
    return $types
}

proc RamDebugger::GiveFileType { filename } {
    variable options
    variable options_def

    if { [array names options extensions,*] == "" } {
	foreach i [array names options_def extensions,*] {
	    set options($i) $options_def($i)
	}
    }
    foreach i [array names options extensions,*] {
	foreach ext $options($i) {
	    if { $ext == "*" && ![regexp {^\*.*\*$} $filename] && [file extension $filename] != "" } {
		continue
	    }
	    if { [string match $ext [file extension $filename]] } {
		regexp {,(.*)} $i {} type
		return $type
	    }
	}
    }
    return ""
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

proc RamDebugger::RecieveTimeFromProgram { name level time } {
    variable TimeMeasureData

    set ipos 0
    foreach i $TimeMeasureData {
	foreach "name_in file lineini lineend lasttime" $i {
	    if { $name == $name_in } {
		set found 0
		set ic 0
		foreach j $lasttime {
		    if { [lindex $j 0] == $level } {
		        lset lasttime $ic [list $level [expr {[lindex $j 1]+$time}]]
		        set found 1
		        break
		    }
		    incr ic
		}
		if { !$found } { lappend lasttime [list $level $time] }
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
	return [lsearch -exact [string tolower $list] [string tolower $file]]
    } else {
	return [lsearch -exact $list $file]
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

proc RamDebugger::RecieveErrorFromProgram { err errInfo args } {
    TextOutInsertRed "------RECIEVED ERROR FROM DEBUGGED PROGRAM-------------\n"
    TextOutInsertRed $errInfo\n
    TextOutInsertRed "-------------------------------------------------------\n"
    TextOutRaise
    after idle [string map [list %e [list $err] %n \n] {
	WarnWin {Recieved Error from Debugged program:%n%e%nCheck Output for details}
	RamDebugger::StopAtGUI "" -1
    }]
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

proc RamDebugger::RecieveFromProgramSource { args } {
    variable currentfile

    if { [lindex $args 0] eq "-encoding" } {
	set encoding [lindex $args 1]
    } else { set encoding "" }
    set file [lindex $args end]
    set retval [RamDebugger::DoinstrumentThisfile $file]
    if { $retval == 1 } {
	TextOutRaise
	TextOutInsertBlue "Sending Instrumented file '$file'\n"

	set currentfile_save $currentfile
	set err [catch {
	    set retval [rlist -returndata -encoding $encoding -asmainfile $file]
	} errstring]
	set currentfile $currentfile_save
	if { $err } {
	    error $errstring
	}
	return $retval
    } elseif { $retval == 2 } {
	TextOutRaise
	TextOutInsertBlue "Sourcing file '$file'\n"
    }
    return "::RDC::sourceproc $args"
}

proc RamDebugger::EvalRemote { comm } {
    variable remoteserver
    variable remoteserverNum
    variable remoteserverType
    variable gdblog

    if { $remoteserver == "" } {
	error "Error: a program to debug must be selected using rdebug"
    }

    set err 0
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
    } elseif { $::tcl_platform(platform) eq "windows" } {
	set err [catch { comm::comm send $remoteserverNum $comm }]
    } else {
	set err [catch { send $remoteserver $comm }]
    }
    if { $err } {
	WarnWin "Debugged program is not available anymore. Disconnecting"
	DisconnectStop
    }
}

proc RamDebugger::EvalRemoteAndReturn { comm } {
    variable remoteserver
    variable remoteserverNum
    variable remoteserverType
    variable gdblog

    if { $remoteserver == "" } {
	error "Error: a program to debug must be selected using rdebug"
    }
    if { $remoteserverType == "local" } {
	set ret [local eval $comm]
    } elseif { $remoteserverType == "master" } {
	set ret [master $comm]
    } elseif { $remoteserverType == "gdb" } {
	foreach "fid program state" $remoteserver break
	regsub -all {(^|\n)(.)} $comm\n {\1-->\2} commlog
	append gdblog $commlog
	puts $fid $comm
	flush $fid
	set ret ""
    } elseif { $::tcl_platform(platform) == "windows" } {
	set ret [comm::comm send $remoteserverNum $comm]
    } else {
	set ret [send $remoteserver $comm]
    }
    return $ret
}

proc RamDebugger::GiveInstFile { file onlyifnewer filetype } {
    variable CacheDir

    if { [string index $file 0] == "*" } { return }

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
    variable currentfile

    if { $debuggerstate != "debug" } { return }


    if { $remoteserverType == "gdb" } {
	set remoteserver [lreplace $remoteserver 2 2 setbreakpoints]
	EvalRemote "delete"
	foreach i $breakpoints {
	    if { ![lindex $i 1] } { continue }
	    set line [lindex $i 3]
	    set filenum [lsearchfile $fileslist [lindex $i 2]]
	    if { $filenum == -1 } { continue }
	    set file [file tail [lindex $fileslist $filenum]]
	    set filetype [GiveFileType $currentfile]
	    if { $filetype == "C/C++" } {
		EvalRemote "break $file:$line"
	    }
	    # CONDITION is forgotten by now
	}
	EvalRemote "printf \"FINISHED SET BREAKPOINTS\\n\""
    } else {
	EvalRemote { if { [info exists RDC::breaks] } { unset RDC::breaks } }
	foreach i $breakpoints {
	    if { ![lindex $i 1] } { continue }
	    set line [lindex $i 3]
	    set filenum [lsearch $fileslist [lindex $i 2]]
	    if { $filenum == -1 } { continue }
	    EvalRemote [list set RDC::breaks($filenum,$line) [list [lindex $i 0] [lindex $i 4]]]
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
		set filenum [lsearch -exact $fileslist $file]
		if { $filenum == -1 } {
		    set err [catch {OpenFileF $file} errstring]
		    if { $err } {
		        WarnWin "Could not open file '$file' for stopping program"
		        return
		    }
		    set filenum [lsearch -exact $fileslist $file]
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
	set filenum [lsearch -exact $fileslist $file]
	if { $filenum == -1 } {
	    set err [catch {OpenFileF $file} errstring]
	    if { $err } {
		WarnWin "Could not open file '$file' for stopping program"
		return
	    }
	    set filenum [lsearch -exact $fileslist $file]
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
	#             set filenum [lsearch -exact $fileslist $file]
	#             if { $filenum == -1 } {
	#                 set err [catch {OpenFileF $file} errstring]
	#                 if { $err } {
	#                     WarnWin "Could not open file '$file' for stopping program"
	#                     return
	#                 }
	#                 set filenum [lsearch -exact $fileslist $file]
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

proc RamDebugger::SecondaryTextHelp { what } {
    variable text_secondary
    variable currentfile_secondary
    variable _secondtexthelp_after

    switch $what {
	begin {
	    set _secondtexthelp_after [after 500 RamDebugger::SecondaryTextHelp show]
	    bind $text_secondary <Leave> [list RamDebugger::SecondaryTextHelp hide]
	}
	show {
	    if { ![info exists text_secondary] } { return }
	    set txt "$currentfile_secondary\nPress central mouse button to drag"
	    if { [winfo exists $text_secondary.l] } {
		$text_secondary.l configure -text $txt
		return
	    }
	    label $text_secondary.l -text $txt -justify left -bd 1 -relief ridge \
		-bg [$text_secondary cget -bg]
	    place $text_secondary.l -anchor ne -relx 1 -rely 0
	    after 1000 [list RamDebugger::SecondaryTextHelp hide]
	    bind $text_secondary <FocusOut> [list RamDebugger::SecondaryTextHelp hide]
	}
	hide {
	    if { [info exists _secondtexthelp_after] } {
		after cancel $_secondtexthelp_after
		unset _secondtexthelp_after
	    }
	    if { [info exists text_secondary] } {
		destroy $text_secondary.l
	    }
	}

    }
}

proc RamDebugger::_secondtextsavepos {} {
    variable mainframe
    variable options

    set options(secondarypanes) ""
    set f [$mainframe getframe]
    if { [winfo exists $f.textpane] } {
	foreach i [$f.textpane panes] {
	    lappend options(secondarypanes) [winfo height $i]
	}
    }
}

proc RamDebugger::ToggleViews {} {
    variable text
    variable text_secondary
    variable currentfile
    variable currentfile_secondary

    if { ![info exists text_secondary] } { return }

    focus $text
    
    set new_currentfile_secondary $currentfile
    OpenFileF $currentfile_secondary
    OpenFileSecondary $new_currentfile_secondary
}

proc RamDebugger::ViewSecondText {} {
    variable mainframe
    variable text
    variable text_secondary
    variable currentfile
    variable currentfile_secondary
    variable options

    set f [$mainframe getframe]
    if { ![winfo exists $f.textpane] } {

	$f.fulltext configure -height 5
	grid propagate $f.fulltext 0

	panedwindow $f.textpane -orient vertical -bd 0
	frame $f.textpane.f
	set text_secondary [text $f.textpane.f.fulltext_secondary -bg grey90 -state disabled \
		                -font FixedFont -highlightthickness 1 -bd 0 -wrap none\
		                -yscrollcommand [list $f.textpane.f.yscroll set]]
	scrollbar $f.textpane.f.yscroll -orient vertical -command [list $text_secondary yview]
	bind $text_secondary <1> [list focus %W]
	bind $text_secondary <FocusIn> [list RamDebugger::SecondaryTextHelp show]
	bind $text_secondary <Enter> [list RamDebugger::SecondaryTextHelp begin]
	grid $text_secondary $f.textpane.f.yscroll -sticky nsew

	bind $text_secondary <Alt-Left> "RamDebugger::GotoPreviusNextInWinList prev ; break"
	bind $text_secondary <Alt-Right> "RamDebugger::GotoPreviusNextInWinList next ; break"
	bind $text_secondary <Control-Tab> [bind $text <Control-KeyPress-Tab>]
	bind $text_secondary <Tab> "RamDebugger::Indent ; break"

	ApplyColorPrefs $text_secondary
	
	grid columnconfigure $f.textpane.f 0 -weight 1
	grid rowconfigure $f.textpane.f 0 -weight 1

	$f.textpane add $f.textpane.f
	set parent [lindex [grid info $f.fulltext] 1]
	grid remove $f.fulltext
	$f.textpane add $f.fulltext

	grid $f.textpane -in $parent -sticky nsew

	foreach "weight1 weight2" [list 1 1] break
	if { [info exists options(secondarypanes)] } {
	    foreach "weight1 weight2" $options(secondarypanes) break
	}
	if { ![info exists currentfile_secondary] } {
	    set currentfile_secondary $currentfile
	}
	update idletasks
	if { [winfo exists $f.textpane] } {
	    set wsum [expr {double($weight1+$weight2)}]
	    set h1 [expr {int($weight1*[winfo height $parent]/$wsum)}]
	    $f.textpane sash place 0 0 $h1
	}
	OpenFileSecondary $currentfile_secondary
	raise $f.fulltext
    } else {
	_secondtextsavepos
	if { [$text cget -synctextwidget] ne "" } {
	    $text configure -synctextwidget ""
	}
	set parent [lindex [grid info $f.textpane] 1]
	destroy $f.textpane
	grid $f.fulltext -in $parent -sticky nsew
	grid propagate $f.fulltext 1
	unset text_secondary
	unset currentfile_secondary
	focus $text
    }
}

proc RamDebugger::FocusSecondTextToggle {} {
    variable text
    variable text_secondary

    if { [info exists text_secondary] } {
	if { [focus] eq $text } {
	    focus $text_secondary
	} else { focus $text }
    } else { focus $text }

}

proc RamDebugger::CheckListFilesPane {} {
    variable options
    variable pane1
    variable pane2
    
    set pw [FindPanedWindowFromPane $pane2]

    if { $options(listfilespane) } {
	if { [lsearch [$pw panes] $pane1] == -1 } {
	    $pw add $pane1 -sticky nsew -before $pane2 -width 100
	}
    } elseif { [lsearch [$pw panes] $pane1] != -1 } {
	$pw forget $pane1
    }
}

proc RamDebugger::ViewOnlyTextOrAll {} {
    variable mainframe
    variable text
    variable pane2in1
    variable options
    variable pane1
    variable pane2
    variable pane3
    variable listboxlabelframe

    set f [$mainframe getframe]
    set t [winfo toplevel $mainframe]

    if { [winfo exists $f.textpane] } {
	set fulltext $f.textpane
    } else {
	set fulltext $f.fulltext
    }

    set delta [expr {[$f.pw cget -sashwidth]+2*[$f.pw cget -sashpad]}]
    set delta_ext [expr {2*[$f.pw cget -borderwidth]+4}]

    if { [lindex [grid info $fulltext] 1] != $f } {
	foreach i [array names options paneweights,*] {
	    regexp {paneweights,(.*),(.*)} $i {} orient panedw
	    set options($i) ""
	    if { [winfo exists $panedw] } {
		set idx 0
		set sum 0
		set res ""
		foreach pane [$panedw panes] {
		    switch $orient {
		        h { lappend res [winfo width $pane] }
		        v { lappend res [winfo height $pane] }
		    }
		    incr idx
		    set sum [expr {$sum+[lindex $res end]}]
		}
		if { $sum > $idx } { set options($i) $res }
	    }
	}
	grid remove $f.pw
	grid $fulltext -in $f -sticky nsew

	grid rowconf $f 0 -weight 1
	grid columnconf $f 0 -weight 1

	if { [lsearch [$f.pw panes] $pane1] != -1 } {
	    set wpane1 [winfo width $pane1]
	    set x [expr {[winfo x $t]+$wpane1+$delta}]
	} else {
	    set wpane1 0
	    set x [winfo x $t]
	}
	wm geometry $t [winfo width $fulltext]x[winfo height $t]+$x+[winfo y $t]
	
	set options(ViewOnlyTextOrAll) OnlyText
    } else {
	set width [winfo width $fulltext]
	
	grid $f.pw
	grid $fulltext -in $pane2in1 -sticky nsew
	
	set wpane3 [winfo width $pane3]
	if { $wpane3 <= 1 } {
	    set wpane3 [winfo reqwidth $pane3]
	}
	incr width $wpane3
	if { [lsearch [$f.pw panes] $pane1] != -1 } {
	    set wpane1 [winfo width $pane1]
	    if { $wpane1 <= 1 } {
		set wpane1 [winfo reqwidth $pane1]
	    }
	    incr width $wpane1
	    incr width $delta
	    set x [expr {[winfo x $t]-$delta-$wpane1}]
	} else {
	    set wpane1 0
	    set x [winfo x $t]
	}
	
	incr width [expr {$delta+$delta_ext}]
	wm geometry $t ${width}x[winfo height $t]+$x+[winfo y $t]
	set options(ViewOnlyTextOrAll) All
    }
    if { [[winfo toplevel $t] cget -use] == "" } {
	wm withdraw $t
	update
	after 0 wm deiconify $t
    }
}

#option add *Panedwindow.Stretch always

if { [package vcompare [package present Tcl] 8.5] >= 0 } {
     option add *Panedwindow.Stretch always
} else {
    bind Panedwindow <Configure> [list RamDebugger::ResizePanedWindow %W]
}

proc RamDebugger::ResizePanedWindow { w } {
    variable resize_panedwindow_id
    if { [info exists resize_panedwindow_id] } {
	after cancel $resize_panedwindow_id
    }
    set resize_panedwindow_id [after idle \
	    [list RamDebugger::ResizePanedWindowDo $w]]
}
proc RamDebugger::ResizePanedWindowDo { w } {
    variable resize_panedwindow_id

    unset resize_panedwindow_id

    switch [$w cget -orient] {
	horizontal { set LEN width }
	vertical { set LEN height }
    }
    set len 0
    foreach i [$w panes] {
	if { [$w panecget $i -$LEN] ne "" } {
	    incr len [$w panecget $i -$LEN]
	} else { incr len [winfo req$LEN $i] }
    }
    set delta [expr {[winfo $LEN $w]-$len}]

    set spad [$w cget -sashpad]
    set swidth [$w cget -sashwidth]
    set tlen 0
    for { set i 0 } { $i < [llength [$w panes]] } { incr i } {
	set pane [lindex [$w panes] $i]
	if { [$w panecget $pane -$LEN] ne "" } {
	    set l [$w panecget $pane -$LEN]
	} else { set l [winfo req$LEN $pane] }
	incr tlen [expr {$l+$spad+int($delta*$l/$len+.5)}]
	if { $i < [llength [$w panes]]-1 } {
	    $w sash place $i $tlen $tlen
	}
	incr tlen [expr {$swidth+$spad}]
    }
}

# orient must be: h or v
proc RamDebugger::ManagePanes { panedw orient default } {
    variable options
    variable optionsinitial

    set optionsinitial($orient,$panedw) $default

    if { [info exists options(paneweights,$orient,$panedw)] && \
	     $options(paneweights,$orient,$panedw) != "" } {
	set ret $options(paneweights,$orient,$panedw)
    } else {
	set ret [set options(paneweights,$orient,$panedw) $default]
    }
#     set newret ""
#     foreach i $ret {
#         if { $i == 1 } {
#             set i 20
#         }
#         lappend newret $i
#     }
    return $ret
    
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
    variable readwriteprefs
    variable SearchToolbar

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

    if { [info exists SearchToolbar] } {
	set options(SearchToolbar) $SearchToolbar
    }

    set options(debuggerstate) $debuggerstate

    set options(watchedvars) ""
    set i 0
    while 1 {
	if { ![info exists EvalEntries($i,left)] } { break }
	lappend options(watchedvars) $EvalEntries($i,left)
	incr i
    }
    if { ![info exists options(ViewOnlyTextOrAll)] || $options(ViewOnlyTextOrAll) != "OnlyText" } {
	foreach i [array names options paneweights,*] {
	    regexp {paneweights,(.*),(.*)} $i {} orient panedw
	    if { [winfo exists $panedw] } {
		set idx 0
		set sum 0
		set res ""
		foreach pane [$panedw panes] {
		    switch $orient {
		        h { lappend res [winfo width $pane] }
		        v { lappend res [winfo height $pane] }
		    }
		    incr idx
		    set sum [expr {$sum+[lindex $res end]}]
		}
		if { $sum > $idx } { set options($i) $res }
	    }
	}
    }
    set options(currentfile) $currentfile
    set options(currentidx) [$text index insert]

    set options(breakpoints) ""
    foreach i $breakpoints {
	set file [lindex $i 2]
	if { [string index $file 0] != "*" } {
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

    _secondtextsavepos

    if { $readwriteprefs } {
	set err [catch {
	    if { $::tcl_platform(platform) == "windows" } {
		registry set {HKEY_CURRENT_USER\Software\RamDebugger} IniData [array get options]
	    } else {
		set fout [open ~/.ramdebugger_prefs w]
		puts -nonewline $fout [array get options]
		close $fout
	    }
	} errstring]
	if { $err } {
	    WarnWin "Could not save preferences: $errstring"
	}
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

proc RamDebugger::ApplyColorPrefs { t } {
    variable options
    
    $t conf -foreground $options(colors,foreground) \
	-background $options(colors,background)
    $t tag conf magenta -foreground $options(colors,commands)
    $t tag conf magenta2 -foreground $options(colors,defines)
    $t tag conf blue -foreground $options(colors,procnames)
    $t tag conf grey -foreground $options(colors,quotestrings)
    $t tag conf green -foreground $options(colors,set)
    $t tag conf red -foreground $options(colors,comments)
    $t tag conf cyan -foreground $options(colors,varnames)
}

# what can be text or text_secondary
proc RamDebugger::Colorize { { what text } } {
    variable text
    variable text_secondary
    variable instrumentedfilesInfo
    variable currentfile
    variable currentfile_secondary
    variable options

    if { $what eq "text_secondary" } {
	set file $currentfile_secondary
    } else {  set file $currentfile }

    if { ![info exists instrumentedfilesInfo($file)] } { return }

    if { $what eq "text" } {
	set ed [$text cget -editable]
	$text conf -editable 1
	set t [$text original]
    } else {
	$text_secondary configure -state normal
	set t $text_secondary
    }

#     $t conf -foreground $options(colors,foreground) \
#         -background $options(colors,background)
#     $t tag conf magenta -foreground $options(colors,commands)
#     $t tag conf magenta2 -foreground $options(colors,defines)
#     $t tag conf blue -foreground $options(colors,procnames)
#     $t tag conf grey -foreground $options(colors,quotestrings)
#     $t tag conf green -foreground $options(colors,set)
#     $t tag conf red -foreground $options(colors,comments)
#     $t tag conf cyan -foreground $options(colors,varnames)

    set iline 1
    foreach i $instrumentedfilesInfo($file) {
	foreach "tag li le" [lrange $i 2 end] {
	    $t tag add $tag $iline.$li $iline.$le
	}
	incr iline
    }
    $t tag raise sel
    if { $what eq "text" } {
	$text conf -editable $ed
    } else {
	$text_secondary configure -state disabled
    }
}

# what can be text or text_secondary
proc RamDebugger::ColorizeLines { l1 l2 { what text } } {
    variable text
    variable text_secondary
    variable instrumentedfilesInfo
    variable currentfile
    variable currentfile_secondary
    variable options

    if { $what eq "text_secondary" } {
	set file $currentfile_secondary
    } else {  set file $currentfile }


    if { $what eq "text" } {
	set ed [$text cget -editable]
	$text conf -editable 1
	set t [$text original]
    } else {
	$text_secondary configure -state normal
	set t $text_secondary
    }
#     $t conf -foreground $options(colors,foreground) \
#         -background $options(colors,background)
#     $t tag conf magenta -foreground $options(colors,commands)
#     $t tag conf magenta2 -foreground $options(colors,defines)
#     $t tag conf blue -foreground $options(colors,procnames)
#     $t tag conf grey -foreground $options(colors,quotestrings)
#     $t tag conf green -foreground $options(colors,set)
#     $t tag conf red -foreground $options(colors,comments)
#     $t tag conf cyan -foreground $options(colors,varnames)

    foreach i [list magenta blue grey green red] {
	$t tag remove $i $l1.0 "$l2.0 lineend"
    }

    for { set i $l1 } { $i <= $l2 } { incr i } {
	foreach "tag li le" [lrange [lindex $instrumentedfilesInfo($file) \
		                         [expr $i-1]] 2 end] {
	    $t tag add $tag $i.$li $i.$le
	}
    }
    $t tag raise sel
    if { $what eq "text" } {
	$text conf -editable $ed
    } else {
	$text_secondary configure -state disabled
    }
}

proc RamDebugger::ColorizeSlow { txt } {
    variable options

    set ed [$txt cget -editable]
    $txt conf -editable 1

#     $txt conf -foreground $options(colors,foreground) \
#         -background $options(colors,background)
#     $txt tag conf magenta -foreground $options(colors,commands)
#     $txt tag conf magenta2 -foreground $options(colors,defines)
#     $txt tag conf blue -foreground $options(colors,procnames)
#     $txt tag conf grey -foreground $options(colors,quotestrings)
#     $txt tag conf green -foreground $options(colors,set)
#     $txt tag conf red -foreground $options(colors,comments)
#     $txt tag conf cyan -foreground $options(colors,varnames)

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
    variable FileSaveHandlers

    if { $what == "ask" } {
	if { !$currentfileIsModified } { return 0 }

	if { [string index $currentfile 0] != "*" } {
	    set message "Do you want to save file '$currentfile'?"
	} else {
	    set message "Do you want to save '$currentfile'?"
	}

	set ret [DialogWin::messageBox -default yes -icon question -message \
		     $message -parent $text \
		     -title "Save" -type yesnocancel]
	if { $ret == "cancel" } { return -1 }
	if { $ret == "no" } {
	    set currentfileIsModified 0
	    return 0
	}
    }

    set NeedsReinstrument 0
    if { $what ne "saveas" && ($currentfile == "*Macros*" || \
		                   [info exists FileSaveHandlers($currentfile)]) } {
	set file $currentfile
    } elseif { $what == "saveas" || $currentfile == "*New file*" || $currentfile == "" || \
		   [regexp {^\*.*\*$} $currentfile] } {
	set NeedsReinstrument 1
	set w [winfo toplevel $text]
	set types [GiveFileTypeForFileBrowser]
	if { ![info exists options(defaultdir)] } { set options(defaultdir) [pwd] }
	set title "Save file"
	if { $options(openfile_browser) } {
	    set file [tk_getSaveFile -filetypes $types -initialdir $options(defaultdir) -parent $w \
		    -title $title]
	} else {
	    set file [GetFile save $types $title]
	}
	if { $file == "" } { return }
	set options(defaultdir) [file dirname $file]
    } elseif { $currentfile == "*Macros*" || [info exists FileSaveHandlers($currentfile)] } {
	set file $currentfile
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
    variable text_secondary

    set w [winfo toplevel $text]

    if { ![info exists text_secondary] || [focus -lastfor $text] ne $text_secondary } {
	if { [SaveFile ask] == -1 } { return }
    }

    set types [GiveFileTypeForFileBrowser]

    #     set types {
    #         {{TCL Scripts}      {.tcl}        }
    #         {{C,C++ files}      {.cc .c .h}   }
    #         {{GiD files}      {.bas .prb .mat .cnd}   }
    #         {{All Files}        *             }
    #     }
    set title "Open source file"
    if { ![info exists options(defaultdir)] } { set options(defaultdir) [pwd] }
    if { $options(openfile_browser) } {
	set file [tk_getOpenFile -filetypes $types -initialdir $options(defaultdir) -parent $w \
		-title $title]
    } else {
	set file [GetFile open $types $title]
    }

    if { $file == "" } { return }
    OpenFileF $file -1
    FillListBox
}

proc RamDebugger::OpenFileF { file { force 0 } { UserNumLine -1 } } {
    variable marker
    variable text
    variable text_secondary
    variable files
    variable breakpoints
    variable currentfile
    variable currentfileIsModified
    variable WindowFilesList
    variable WindowFilesListLineNums
    variable options
    variable currentfile_secondary

    if { $file == "" } { return }
    set file [filenormalize $file]

    if { [info exists text_secondary] && [focus -lastfor $text] eq $text_secondary } {
	return [OpenFileSecondary $file]
    }

    if { $force == -1 } {
	set force 0
    } else {
	if { [SaveFile ask] == -1 } { return }
    }

    WaitState 1

    if { [set pos [lsearch -exact $WindowFilesList $currentfile]] != -1 } {
	set line [scan [$text index insert] %d]
	set WindowFilesListLineNums [lreplace $WindowFilesListLineNums $pos $pos $line]
    }
    set linenum 1
    if { [set pos [lsearch -exact $WindowFilesList $file]] != -1 } {
	set linenum [lindex $WindowFilesListLineNums $pos]
    }
    if { $file == $currentfile } {
	set idx [$text index insert]
    } else { set idx $linenum.0 }

    if { $UserNumLine != -1 } { set idx $UserNumLine.0 }

    set currentfile_save $currentfile
    if { !$force } {
	set comm [list rlist -quiet $file {}]
    } elseif { $force == 2 } { 
	set comm [list rlist -quiet -reinstrument $file {}]
    } else {
	set comm [list rlist -quiet -force $file {}]
    }

    if { [catch $comm errstring] } {
	set currentfile $currentfile_save
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
	if { ![AreFilesEqual [lindex $i 2]  $file] } { continue }
	set line [lindex $i 3]
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

    if { [set pos [lsearch -exact $WindowFilesList $file]] != -1 } {
	set WindowFilesList [lreplace $WindowFilesList $pos $pos]
	set WindowFilesListLineNums [lreplace $WindowFilesListLineNums $pos $pos]
    }
    if { [string index $file 0] != "*" } {
	set WindowFilesList [linsert $WindowFilesList 0 $file]
	set WindowFilesListLineNums [linsert $WindowFilesListLineNums 0 $linenum]
    }
    $text conf -editable 1

    if { [string index $file 0] != "*" } {
	if { ![info exists options(RecentFiles)] } {
	    set options(RecentFiles) ""
	}
	set ipos [lsearchfile $options(RecentFiles) $file]
	if { $ipos != -1 } {
	    set options(RecentFiles) [lreplace $options(RecentFiles) $ipos $ipos]
	}
	set options(RecentFiles) [linsert $options(RecentFiles) 0 $file]
	if { [llength $options(RecentFiles)] > 10 } {
	    set options(RecentFiles) [lreplace $options(RecentFiles) 10 end]
	}
	set options(defaultdir) [file dirname $file]
	#FillListBox
    }
    set filetype [GiveFileType $file]
    RamDebugger::AddFileTypeMenu $filetype

    if { [info exists currentfile_secondary] } {
	if { $currentfile eq $currentfile_secondary } {
	    $text configure -synctextwidget $text_secondary
	} else {
	    $text configure -synctextwidget ""
	}
    }
    ManagePositionsImages
    WaitState 0
    if { [focus -lastfor $text] eq $text || \
	     [focus -lastfor $text] eq [winfo toplevel $text] } {
	focus -force $text
    }
    return 0
}

proc RamDebugger::OpenFileSecondary { file } {
    variable text
    variable text_secondary
    variable files
    variable currentfile
    variable currentfile_secondary
    variable currentfileIsModified
    variable WindowFilesList
    variable WindowFilesListLineNums
    variable options

    WaitState 1

    set linenum 1
    if { $file eq $currentfile } {
	set linenum [scan [$text index insert] %d]
    } elseif { [set pos [lsearch -exact $WindowFilesList $file]] != -1 } {
	set linenum [lindex $WindowFilesListLineNums $pos]
    }
    set idx $linenum.0

    set currentfile_save $currentfile
    set comm [list rlist -quiet $file {}]

    if { [catch $comm errstring] } {
	set currentfile $currentfile_save
	WaitState 0
	WarnWin [lindex [split $errstring \n] 0]
	return 1
    }
    set currentfile $currentfile_save

    $text_secondary configure -state normal
    $text_secondary delete 1.0 end
    $text_secondary ins end [string map [list "\t" "        "] $files($file)]
    $text_secondary tag add normal 1.0 end
    $text_secondary configure -state disabled

    set options(defaultdir) [file dirname $file]
    set currentfile_secondary $file

    Colorize text_secondary

    $text_secondary mark set insert $idx
    $text_secondary see $idx
    
    if { [lsearch -exact $WindowFilesList $file] == -1 } {
	lappend WindowFilesList $file
	lappend WindowFilesListLineNums $linenum
    }
    SecondaryTextHelp show

    if { $currentfile eq $currentfile_secondary } {
	$text configure -synctextwidget $text_secondary
    } else {
	$text configure -synctextwidget ""
    }
    WaitState 0
    return 0
}

proc RamDebugger::OpenFileSaveHandler { file data handler } {
    variable marker
    variable text
    variable files
    variable breakpoints
    variable currentfile
    variable currentfileIsModified
    variable WindowFilesList
    variable WindowFilesListLineNums
    variable options
    variable FileSaveHandlers
    variable currentfile_secondary
    variable text_secondary

    if { [string index $file 0] != "*" } {
	WarnWin "File name must begin with: '*'"
	return 1
    }
    
    if { [SaveFile ask] == -1 } { return }

    WaitState 1

    set linenum 1
    if { $file == $currentfile } {
	set idx [$text index insert]
    } else { set idx $linenum.0 }

    set currentfile_save $currentfile
    set files($file) $data
    set comm [list rlist -quiet $file {}]

    if { [catch $comm errstring] } {
	set currentfile $currentfile_save
	unset files($file)
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
	if { ![AreFilesEqual [lindex $i 2]  $file] } { continue }
	set line [lindex $i 3]
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

    $text conf -editable 1

    if { $handler ne "" } {
	set FileSaveHandlers($file) $handler
    } else { unset -nocomplain FileSaveHandlers($file) }

    if { [info exists currentfile_secondary] } {
	if { $currentfile eq $currentfile_secondary } {
	    $text configure -synctextwidget $text_secondary
	} else {
	    $text configure -synctextwidget ""
	}
    }
    ManagePositionsImages
    WaitState 0
    return 0
}

proc RamDebugger::ReinstrumentCurrentFile {} {
    variable currentfile
    variable currentfile_secondary

    rlist -quiet -reinstrument $currentfile
    Colorize
    if { [info exists currentfile_secondary] && $currentfile eq $currentfile_secondary } {
	Colorize text_secondary
    }
}


proc RamDebugger::CloseFile {} {
    variable currentfile
    variable WindowFilesList
    variable WindowFilesListLineNums

    if { [set pos [lsearch -exact $WindowFilesList $currentfile]] != -1 } {
	set WindowFilesList [lreplace $WindowFilesList $pos $pos]
	set WindowFilesListLineNums [lreplace $WindowFilesListLineNums $pos $pos]
    }
    if { [llength $WindowFilesList] > 0 } {
	OpenFileF [lindex $WindowFilesList 0]
    } else { NewFile }
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
    variable WindowFilesListLineNums
    variable currentfile_secondary
    variable text_secondary

    if { [SaveFile ask] == -1 } { return }

    WaitState 1

    if { [set pos [lsearch -exact $WindowFilesList $currentfile]] != -1 } {
	set line [scan [$text index insert] %d]
	set WindowFilesListLineNums [lreplace $WindowFilesListLineNums $pos $pos $line]
    }

    set currentfile "*New file*"
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

    if { [info exists currentfile_secondary] } {
	if { $currentfile eq $currentfile_secondary } {
	    $text configure -synctextwidget $text_secondary
	} else {
	    $text configure -synctextwidget ""
	}
    }
    ManagePositionsImages
    WaitState 0
    return 0
}

proc RamDebugger::_savefile_only { file data } {
    variable FileSaveHandlers

    if { $file eq "*Macros*" } {
	SaveMacrosDocument $data
    } elseif { [info exists FileSaveHandlers($file)] } {
	set err [catch {eval $FileSaveHandlers($file) [list $file $data]} errstring]
	if { $err } { error "Error saving file '$file' ($errstring)" }
    } else {
	if { [file exists $file] } {
	    set ic 0
	    while { [file exists $file.~$ic~] } { incr ic }     
	    set renfile $file.~$ic~
	    set err [catch { file rename -force $file $renfile } errstring]
	    if { $err } { error "Error saving file '$file' ($errstring)" }
	}
	set err [catch { open $file w } fout]
	if { $err } { error "Error saving file '$file'" }

	set header [string range $data 0 255]
	if { [regexp -- {-\*-\s*coding:\s*utf-8\s*;\s*-\*-} $header] } {
	    fconfigure $fout -encoding utf-8
	}
	puts -nonewline $fout $data
	close $fout
	if { [info exists renfile] } {
	    file delete -force $renfile
	}
    }
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
    variable FileSaveHandlers
    variable WindowFilesList
    variable WindowFilesListLineNums
    variable options
    variable currentfile_secondary
    variable text_secondary

    WaitState 1
    SetMessage "Saving file '$file'..."

    set map [list "\n[string repeat { } 16]" "\n\t\t" "\n[string repeat { } 8]" "\n\t"]
    set files($file) [string map $map [$text get 1.0 end-1c]]

    set err [catch {_savefile_only $file $files($file)} errstring]

    if { $err } {
	WaitState 0
	SetMessage ""
	WarnWin $errstring $text
	return
    }

    #     if { $file == "*Macros*" } {
    #         SaveMacrosDocument $files($file)
    #     } elseif { [info exists FileSaveHandlers($file)] } {
    #         set err [catch {eval $FileSaveHandlers($file) [list $file $files($file)]} errstring]
    #         if { $err } {
    #             WaitState 0
    #             SetMessage ""
    #             WarnWin "Error saving file '$file' ($errstring)" $text
    #             return
    #         }
    #     } else {
    #         set err [catch { open $file w } fout]
    #         if { $err } {
    #             WaitState 0
    #             SetMessage ""
    #             WarnWin "Error saving file '$file'" $text
    #             return
    #         }
    #         set header [string range $files($file) 0 255]
    #         if { [regexp -- {-\*-\s*coding:\s*utf-8\s*;\s*-\*-} $header] } {
    #             fconfigure $fout -encoding utf-8
    #         }
    #         puts -nonewline $fout $files($file)
    #         close $fout
    #     }
    set currentfile $file

    catch { unset instrumentedfilesP($currentfile) instrumentedfilesR($currentfile) }
    catch { unset instrumentedfilesTime($currentfile) }
    catch { unset instrumentedfilesSent($currentfile) }
    
    wm title [winfo toplevel $text] "RamDebugger     [file tail $currentfile]"
    set currentfileIsModified 0
    if { [string index $file 0] != "*" } {
	set filesmtime($currentfile) [file mtime $file]
    }
    set linenum [scan [$text index insert] %d]
    if { [set pos [lsearch -exact $WindowFilesList $file]] != -1 } {
	set WindowFilesList [lreplace $WindowFilesList $pos $pos]
	set WindowFilesListLineNums [lreplace $WindowFilesListLineNums $pos $pos]
    }
    if { [string index $file 0] != "*" } {
	set WindowFilesList [linsert $WindowFilesList 0 $file]
	set WindowFilesListLineNums [linsert $WindowFilesListLineNums 0 $linenum]
    }
    if { [string index $file 0] != "*" } {
	if { ![info exists options(RecentFiles)] } {
	    set options(RecentFiles) ""
	}
	set ipos [lsearchfile $options(RecentFiles) $file]
	if { $ipos != -1 } {
	    set options(RecentFiles) [lreplace $options(RecentFiles) $ipos $ipos]
	}
	set options(RecentFiles) [linsert $options(RecentFiles) 0 $file]
	if { [llength $options(RecentFiles)] > 10 } {
	    set options(RecentFiles) [lreplace $options(RecentFiles) 10 end]
	}
	set options(defaultdir) [file dirname $file]
	#FillListBox
    }
    set filetype [GiveFileType $file]
    RamDebugger::AddFileTypeMenu $filetype

    if { [info exists currentfile_secondary] } {
	if { $currentfile eq $currentfile_secondary } {
	    $text configure -synctextwidget $text_secondary
	} else {
	    $text configure -synctextwidget ""
	}
    }
    ManagePositionsImages
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

    if { [set pos [lsearch -exact $WindowFilesList $currentfile]] != -1 } {
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
    variable AppDataDir

    HelpViewer::EnterDirForIndex $AppDataDir

    if { $file == "" } {
	set w [HelpViewer::HelpWindow [file join $MainDir help]]
    } else {
	set w [HelpViewer::HelpWindow [file join $MainDir help $file]]
    }
    return $w
}


proc RamDebugger::ViewHelpForWord { { word "" } } {
    variable text
    variable AppDataDir

    HelpViewer::EnterDirForIndex $AppDataDir

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

proc RamDebugger::ActualizeActiveProgramsIfVoid { menu } {

    if { [$menu index end] eq "none" } {
	ActualizeActivePrograms $menu
    }
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
		$menu add check -label $i -command \
		    "[list RamDebugger::rdebug $i]
		    [namespace code [list ActualizeActivePrograms $menu]]" \
		    -variable ::checked 
		set ::checked 1
	    } else {
		$menu add command -label $i -command \
		    "[list RamDebugger::rdebug $i]
		    [namespace code [list ActualizeActivePrograms $menu]]"
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
    $menu add command -label "Current file arguments" -command {
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
    variable text
    variable text_secondary
    variable currentfile
    variable currentfile_secondary

    if { [llength $WindowFilesList] < 1 } { return }

    if { [info exists text_secondary] && [focus -lastfor $text] eq $text_secondary } {
	set file $currentfile_secondary
    } else {
	set file $currentfile
    }
    set pos [lsearch -exact $WindowFilesList $file]
    if { [llength $WindowFilesList] == 1 && $pos == 0 } { return }

    switch $what prev { incr pos } next { incr pos -1 }

    if { $pos < 0 } { set pos [expr {[llength $WindowFilesList]-1}] }
    if { $pos >= [llength $WindowFilesList] } { set pos 0 }
    OpenFileF [lindex $WindowFilesList $pos]
}

proc RamDebugger::ChooseViewFile { what args } {
    variable WindowFilesList
    variable text
    variable text_secondary
    variable currentfile
    variable currentfile_secondary
    variable options

    if { [info exists text_secondary] && [focus -lastfor $text] eq \
	     $text_secondary } {
	set file $currentfile_secondary
    } else {
	set file $currentfile
    }
    set w $text

    if { [winfo exists $w._choosevf] } {
	wm geometry $w._choosevf [wm geometry $w._choosevf]
    }

    set entrylen 16
    set numcols 6

    switch $what {
	start - startrecent - startcurrdir {
	    if { ![info exists options(RecentFiles)] } { set options(RecentFiles) "" }
	    if { $what eq "start" && [llength $WindowFilesList] < 2 } {
		set what startrecent
	    }
	    if { $what eq "startrecent" && ![llength options(RecentFiles)] } {
		set what startcurrdir
	    }
	    if { $what eq "startcurrdir" } {
		set patterns ""
		foreach ext $options(extensions,TCL) {
		    if { $ext ne "*" } { lappend patterns *[string trim $ext *] }
		}
		set dir [file dirname $file]
		if { $dir ne "." } {
		    set list [eval [list glob -nocomplain -dir $dir] $patterns]
		} else { set list "" }
		if { $list eq "" } {
		    if { [llength $WindowFilesList] >= 2 || \
		             ![llength options(RecentFiles)] } {
		        set what start
		    } else { set what startrecent }
		}
	    }
	    switch $what {
		start {
		    set list $WindowFilesList
		}
		startrecent {
		    set list $options(RecentFiles)
		}
		startcurrdir {
		    set list [lsort -dictionary $list]
		    # already assigned
		}
	    }
	    set list [lrange $list 0 39]
	    set ipos [lsearch -exact $list $file]
	    if { $ipos == -1 } {
		set list [linsert $list 0 $file]
	    } elseif { $ipos != 0 } {
		if { $what eq "startcurrdir" } {
		    set list [lreplace $list $ipos $ipos]
		    set list [linsert $list 0 $file]
		} else {
		    set tmplist $list
		    set list [lrange $tmplist $ipos end]
		    eval lappend list [lrange $tmplist 0 [expr {$ipos-1}]]
		}
	    }
	    destroy $w._choosevf
	    toplevel $w._choosevf -relief raised -bd 2
	    wm withdraw $w._choosevf
	    wm overrideredirect $w._choosevf 1

	    label $w._choosevf.ld -bd 2 -relief sunken -anchor ne \
		-justify right -width 20
	    foreach "row col" [list 0 0] break
	    for { set i 0 } { $i < [llength $list] } { incr i } {
		if { $i > 0 && $i%$numcols == 0 } {
		    incr row
		    set col 0
		}
		entry $w._choosevf.l$i -width $entrylen -bd 0 -highlightthickness 2 \
		    -highlightcolor #b5b6bd -highlightbackground [$w._choosevf cget -bg] \
		    -justify center -cursor "" \
		    -disabledbackground [$w._choosevf.ld cget -background] \
		    -disabledforeground [$w._choosevf.ld cget -foreground]

		set path [lindex $list $i]
		set txt [file tail $path]
		if { [string length $txt] > $entrylen } {
		    set txt "[string range $txt 0 [expr {$entrylen-4}]]..."
		}
		$w._choosevf.l$i insert end $txt
		$w._choosevf.l$i xview end
		$w._choosevf.l$i configure -state disabled
		grid $w._choosevf.l$i -row $row -column $col -sticky nw
		bind $w._choosevf.l$i <Tab> "[list RamDebugger::ChooseViewFile next $i] ; break"
		bind $w._choosevf.l$i <Shift-Tab> "[list RamDebugger::ChooseViewFile prev $i] ; break"
		bind $w._choosevf.l$i <Right> "[list RamDebugger::ChooseViewFile next $i] ; break"
		bind $w._choosevf.l$i <Left> "[list RamDebugger::ChooseViewFile prev $i] ; break"
		bind $w._choosevf.l$i <Up> "[list RamDebugger::ChooseViewFile up $row $col] ; break"
		bind $w._choosevf.l$i <Down> "[list RamDebugger::ChooseViewFile down $row $col] ; break"
		bind $w._choosevf.l$i <FocusIn> [list $w._choosevf.ld configure -text $path]
		bind $w._choosevf.l$i <1> "[list focus $w._choosevf.l$i] ;
		    [list RamDebugger::ChooseViewFile keyrelease button1 $list] ; break"
		incr col
	    }
	    grid $w._choosevf.ld -row [incr row] -column 0 -columnspan $numcols -sticky ew \
		-padx 5 -pady 5
	    set fontsize [expr {[font actual [$w._choosevf.ld cget -font] -size] \
		                    -2}]
	    label $w._choosevf.note -text "Press <Space> to change file list" \
		-font "-size $fontsize"
	    grid $w._choosevf.note -row [incr row] -column 0 -columnspan $numcols -sticky ew

	    if { [llength $list] < $numcols } { set numcols [llength $list] }

	    grid columnconfigure $w._choosevf [expr {$numcols-1}] -weight 1

	    update
	    set t [winfo toplevel [winfo parent $w._choosevf]]
	    set x [expr {int([winfo x $t]+.5*[winfo width $t]-.5*[winfo reqwidth $w._choosevf])}]
	    set y [expr {int([winfo y $t]+.5*[winfo height $t]-.5*[winfo reqheight $w._choosevf])}]

	    if { $x+[winfo reqwidth $w._choosevf] > [winfo screenwidth $w] } {
		set x [expr {[winfo screenwidth $w]-[winfo reqwidth $w._choosevf]}]
	    }
	    if { $y+[winfo reqheight $w._choosevf] > [winfo screenheight $w] } {
		set y [expr {[winfo screenheight $w]-[winfo reqheight $w._choosevf]}]
	    }
	    wm geometry $w._choosevf +$x+$y
	    wm deiconify $w._choosevf
	    
	    bind $w._choosevf <KeyRelease> [list RamDebugger::ChooseViewFile \
		                                keyrelease %K $list]
	    bind $w._choosevf <KeyPress> [list RamDebugger::ChooseViewFile \
		                              keypress %K $what]
	    raise $w._choosevf
	    if { [llength $list] > 1 } {
		after idle focus -force $w._choosevf.l1
	    } else { after idle focus -force $w._choosevf.l0 }
	}
	keyrelease {
	    foreach "K list" $args break
	    if { [regexp {(?i)^(control|return|button1)} $K] } {
		regexp {[0-9]+$} [focus] pos
		destroy $w._choosevf
		if { [lindex $list $pos] ne $file } {
		    update ;# to let focus change
		    OpenFileF [lindex $list $pos]
		}
	    } elseif { [regexp {(?i)^escape} $K] } {
		destroy $w._choosevf
	    }
	}
	keypress {
	    foreach "K what_in" $args break
	    if { [regexp {(?i)^space} $K] } {
		switch $what_in {
		    start {
		        set whatnext startrecent
		    }
		    startrecent {
		        set whatnext startcurrdir
		    }
		    startcurrdir {
		        set whatnext start
		    }
		}
		ChooseViewFile $whatnext
	    }
	}
	next {
	    set i [lindex $args 0]
	    incr i
	    if { [winfo exists $w._choosevf.l$i] } {
		focus $w._choosevf.l$i
	    } else {
		focus $w._choosevf.l0
	    }
	}
	prev {
	    set i [lindex $args 0]
	    incr i -1
	    if { $i >= 0 } {
		focus $w._choosevf.l$i
	    } else {
		while 1 {
		    incr i
		    if { ![winfo exists $w._choosevf.l$i] } { break }
		}
		incr i -1
		focus $w._choosevf.l$i
	    }
	}
	up {
	    foreach "row col" $args break
	    incr row -1
	    foreach "maxcol maxrow" [grid size $w._choosevf] break
	    if { $row < 0 } { set row [expr {$maxrow-3}] }
	    focus [grid slaves $w._choosevf -row $row -col $col]
	}
	down {
	    foreach "row col" $args break
	    incr row 1
	    foreach "maxcol maxrow" [grid size $w._choosevf] break
	    if { [grid slaves $w._choosevf -row $row -col $col] eq "" || \
		     [grid slaves $w._choosevf -row $row -col $col] eq "$w._choosevf.ld" } {
		set row 0
	    }
	    focus [grid slaves $w._choosevf -row $row -col $col]
	}
    }
}


proc RamDebugger::ActualizeViewMenu { menu } {
    variable WindowFilesList
    variable WindowFilesListLineNums
    variable text
    variable currentfile

    if { [$menu index end] > 7 } {
	$menu del 8 end
    }

    $menu add command -label "Previus" -acc "Alt-Left" -command \
	"RamDebugger::GotoPreviusNextInWinList prev"
    $menu add command -label "Next" -acc "Alt-Right" -command \
	"RamDebugger::GotoPreviusNextInWinList next"
    $menu add command -label "Select..." -acc "Ctrl Tab" -command \
	[list RamDebugger::ChooseViewFile start]

    set needssep 1
    foreach i $WindowFilesList {
	if { $needssep } {
	    $menu add separator
	    set needssep 0
	}
	set label $i
	if { [string length $label] > 45 } { set label ...[string range $label end-42 end] }
	
	if { $i eq $currentfile } {
	    $menu add checkbutton -label $label -variable ::pp -command \
		[list RamDebugger::OpenFileF $i]
	    set ::pp 1
	} else {
	    $menu add command -label $label -command [list RamDebugger::OpenFileF $i]
	}
    }
}

proc RamDebugger::AddRecentfilesToMenu { menu } {
    variable options

    $menu del 0 end

    if { ![info exists options(RecentFiles)] } { return }

    foreach i $options(RecentFiles) {
	set label $i
	if { [string length $label] > 45 } {
	    set label ...[string range $label end-42 end]
	}
	$menu add command -label $label -command \
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

proc RamDebugger::UpdateArrowAndBreak { line hasbreak hasarrow { forceraise 1 } } {
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

    $marker delete "l$line&&(arrowbreak||arrow||break)"

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
	set endis 1
	foreach "- endis - - -" [lindex [rinfo -full $line] 0] break
	if { $endis } {
	    $marker create image 0 $ypos -anchor sw -image $images(arrowbreak) -tags "arrowbreak l$line"
	} else {
	    $marker create image 0 $ypos -anchor sw -image $images(arrowdbreak) -tags "arrowbreak l$line"
	}
    } elseif { $hasarrow } {
	$marker create image 0 $ypos -anchor sw -image $images(arrow) -tags "arrow l$line"
    } elseif { $hasbreak } {
	set endis 1
	foreach "- endis - - -" [lindex [rinfo -full $line] 0] break
	if { $endis } {
	    $marker create image 0 $ypos -anchor sw -image $images(break) -tags "break l$line"
	} else {
	    $marker create image 0 $ypos -anchor sw -image $images(dbreak) -tags "break l$line"
	}
    }

    if { $forceraise && $hasarrow } {
	if { $::tcl_platform(platform) == "windows" } {
	    set doit 1
	} elseif { [focus] != $text } {
	    # the horrible bug in Linux with the raise command
	    set doit 1
	} else { set doit 0 }
	if { $doit } {
	    after 100 "raise [winfo toplevel $text] ; focus -force $text"
	}
    }
    if { !$hadarrow && $hasarrow } {
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

proc RamDebugger::ManagePositionsImages {} {
    variable text
    variable marker
    variable options
    variable currentfile
    variable images

    $marker delete bookmark
    set font [$text cget -font]
    foreach i $options(saved_positions_stack) {
	foreach "file line -" $i break
	if { $file eq $currentfile } {
	    set ypos [expr ($line-1)*[font metrics $font -linespace]+\
		          [font metrics $font -ascent]+[$text cget -pady]+2]
	    set id [$marker create image 0 $ypos -anchor sw -image $images(bookmark) \
		        -tags "bookmark l$line"]
	    $marker lower $id
	}
    }
    MoveCanvas $text $marker
}

proc RamDebugger::StopAtGUI { file line { condinfo "" } } {
    variable marker
    variable currentfile
    variable text

    set forceraise 1
    if { $line == -1 } {
	# called from bgerror
	variable fileslist
	
	set filenum ""
	foreach {filenum line} [EvalRemoteAndReturn ::RDC::GetLastVisited] break;
	if { $filenum eq "" } { return }
	set file [lindex $fileslist $filenum]
	set forceraise 0
    }
    if { ![info exists text] || ![winfo exists $text] } { return }

    foreach j [concat [$marker gettags arrow] [$marker gettags arrowbreak]] {
	if { [string match l* $j] } {
	    regexp {l([0-9]+)} $j {} arrowline
	    UpdateArrowAndBreak $arrowline "" 0 $forceraise
	}
    }
    if { $file == "" } {
	$text conf -editable 1
	return
    }

    if { ![AreFilesEqual $file $currentfile] } {
	OpenFileF $file 
    }
    UpdateArrowAndBreak $line "" 1 $forceraise

    RamDebugger::SetMessage "" ;# to take out old SetMessageFlash
    if { $condinfo != "" } {
	RamDebugger::SetMessageFlash "Conditional breakpoint result: $condinfo"
	#WarnWin "Conditional breakpoint result: $condinfo" [winfo toplevel $text]
    }
}

proc RamDebugger::ContNextGUI { what } {
    variable text
    variable remoteserver
    variable remoteserverType
    variable IsInStop
    variable currentfile
    variable options

    # before, there was the additional cond: || ($remoteserverType == "local" && !$IsInStop) || \
	# ($remoteserver == "master all" && !$IsInStop) 


    if { $remoteserverType == "" && [info command master] != "" } {
	RamDebugger::rdebug -master
    }

    if { $remoteserver == "" } {
	if { $currentfile == "" } {
	    WarnWin "Cannot start debugging. There is no currentfile" $text
	    return
	}
	set filetype [GiveFileType $currentfile]
	if { $filetype == "C/C++" } {
	    if { $options(ConfirmStartDebugging) && $remoteserver != "" } {
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
	    if { $options(ConfirmStartDebugging) && $remoteserver != "" } {
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
    } elseif { $remoteserver == "master all" && !$IsInStop } {
	rlist -resend -quiet
    }

    switch $what {
	rcont { rcont }
	rnext { rnext }
	rnextfull { rnext -full }
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
	rbreak { rbreak }
    }
}

proc RamDebugger::TextMotion { X Y x y } {
    variable text
    variable currentfile
    variable IsInStop
    variable TextMotionAfterId

    RamDebugger::CVS::SetUserActivity

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
		        set ::RDC::errorInfo $::errorInfo
		        set ::RDC::err [catch {expr [set ::RDC::i]} ::RDC::val]
		        if { !$::RDC::err } {
		            lappend ::RDC::retval expr $::RDC::val
		        } else {
		            lappend ::RDC::retval error "variable or expr $::RDC::i does not exist"
		            set ::errorInfo $::RDC::errorInfo
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
		    set ::RDC::errorInfo $::errorInfo
		    set ::RDC::err [catch {expr {VAR}} ::RDC::val]
		    if { !$::RDC::err } {
		        set ::RDC::retval [list expr $::RDC::val]
		    } else {
		        set ::RDC::retval [list error {variable or expr 'VAR' does not exist}]
		        set ::errorInfo $::RDC::errorInfo
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
		    set ::RDC::errorInfo $::errorInfo
		    set ::RDC::err [catch {expr {VAR}} ::RDC::val]
		    if { !$::RDC::err } {
		        set ::RDC::retval [list expr $::RDC::val]
		    } else {
		        set ::RDC::retval [list error "variable or expr 'VAR' does not exist"]
		        set ::errorInfo $::RDC::errorInfo
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
	if { [winfo exists $listbox] } { $listbox configure -cursor watch }
	$w configure -cursor watch
	if { [winfo toplevel $w] != $w } {
	    [winfo toplevel $w] configure -cursor watch
	}
    } else {
	$text configure -cursor xterm
	if { [winfo exists $listbox] } { $listbox configure -cursor "" }
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
    foreach i [::textutil::splitx  $data {(\n)(?!$)}] {
	TextInsertAndWrap $textOUT "$i" 200
	if { [info command tkcon_puts] != "" } { catch { tkcon_puts "$i" } }
    }

    $textOUT conf -state disabled
    if { $yend == 1 } { $textOUT yview moveto 1 }
}

proc RamDebugger::TextOutInsertRed { data } {
    variable textOUT

    if { ![info exists textOUT] || ![winfo exists $textOUT] } { return }

    foreach "- yend" [$textOUT yview] break
    $textOUT conf -state normal
    foreach i [::textutil::splitx  $data {(\n)(?!$)}] {
	TextInsertAndWrap $textOUT "$i" 200 red
	if { [info command tkcon_puts] != "" } { catch { tkcon_puts stderr "$i" } }
    }
    $textOUT tag configure red -foreground red
    $textOUT conf -state disabled
    if { $yend == 1 } { $textOUT yview moveto 1 }
}

proc RamDebugger::TextOutInsertBlue { data } {
    variable textOUT

    if { ![info exists textOUT] || ![winfo exists $textOUT] } { return }

    foreach "- yend" [$textOUT yview] break
    $textOUT conf -state normal
    foreach i [::textutil::splitx  $data {(\n)(?!$)}] {
	TextInsertAndWrap $textOUT "$i" 200 blue
    }
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
    variable mainframe
    variable label_for_ProgressVar

    if { ![info exists label_for_ProgressVar] } { set label_for_ProgressVar "" }

    if { [info exists progressvar] && $progressvar == -2 } {
	set RamDebugger::progressvar -1
	if { [winfo exists $label_for_ProgressVar] } { destroy $label_for_ProgressVar }
	error "Stop at user demand"
    }

    set progressvar $value

    if { $canstop == 1 && $value == 0 } {
	if { [winfo exists $label_for_ProgressVar] } { destroy $label_for_ProgressVar }

	set label_for_ProgressVar [$mainframe addindicator -text "Wait please..."]
	bindtags $label_for_ProgressVar [list $label_for_ProgressVar]
	focus $label_for_ProgressVar
	# catch is necessary because it fails in Linux (it says window needs to be viewable)
	catch {grab -global $label_for_ProgressVar}

	bind $label_for_ProgressVar <Escape> "set RamDebugger::progressvar -2"
    }

    if { $value == 100 } {
	after 1000 set RamDebugger::progressvar -1
	if { [winfo exists $label_for_ProgressVar] } { destroy $label_for_ProgressVar }
    }
    update
}

proc RamDebugger::SetMessage { mess } {
    variable status
    variable afterid_formessage

    after cancel $afterid_formessage
    set status $mess
    update
    set afterid_formessage [after 5000 { set RamDebugger::status "" }]
}

proc RamDebugger::SetMessageFlash { mess { time 7000 } } {
    variable status
    variable afterid_formessage

    after cancel $afterid_formessage

    if { $status == "" } {
	set status $mess
    } else { set status "" }

    incr time -300
    if { $time <= 0 } {
	set status ""
    } else {
	set afterid_formessage [after 300 [list RamDebugger::SetMessageFlash $mess $time]]
    }
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
    if { $currentfile != "" && [string index $currentfile 0] != "*" && \
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
	set filetype [GiveFileType $i]
	if { [file isdir $fullpath] } {
	    if { [string tolower $i] == "cvs" } { continue }
	    $listbox insert $idxfolder $item -image [Bitmap::get folder] -text $i \
		-data [list folder $fullpath]
	    incr idxfolder
	} elseif { $filetype != "" && [file extension $i] != "" } {
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
	    set rex {(((?:[a-zA-Z]:/)?[-/\w.]+):([0-9]+))|(((?:[a-zA-Z]:/)?[-/\w. ]+):([0-9]+))}
	    if { [regexp $rex [$textCOMP get "$idx linestart" "$idx lineend"]] } {
		$textCOMP tag add sel2 "$idx linestart" "$idx lineend"
	    }
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
	    if { ![file exists $file] } {
		set fullfile [cproject::TryToFindPath $file]
		if { $fullfile != "" } {
		    set file $fullfile
		}
	    }
	    if { ![file exists $file] && [file exists [file join $options(defaultdir) \
		                                           $file]] } {
		set file [file join $options(defaultdir) $file]
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
	    if { [package vcompare $::tcl_version 8.4] >= 0 } {
		# paste is made here in order to substitute tabs by spaces
		global tcl_platform
		if {![catch {::tk::GetSelection $text CLIPBOARD} sel]} {
		    set sel [string map [list "\t" "        "] $sel]
		    set oldSeparator [$text cget -autoseparators]
		    if { $oldSeparator } {
		        $text configure -autoseparators 0
		        $text edit separator
		    }
		    if {[string compare [tk windowingsystem] "x11"]} {
		        catch { $text delete sel.first sel.last }
		    }
		    $text insert insert $sel
		    if { $oldSeparator } {
		        $text edit separator
		        $text configure -autoseparators 1
		    }
		}
	    } else {
		tk_textPaste $text
	    }
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
    variable currentfile

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
	$menu add command -label "Open & Debug" -command \
	    "[list RamDebugger::OpenFileF $name] ; RamDebugger::ContNextGUI rcont"
	$menu add separator
	$menu add command -label Reinstrument -command [list RamDebugger::OpenFileF \
		                                            $name 2]

	set filetype [GiveFileType $currentfile]
	if { $filetype == "C/C++" } {
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

    if { $currentfileIsModified } {
	# take out the modified *
	set title [string range [wm title [winfo toplevel $text]] 0 end-1]
	wm title [winfo toplevel $text] $title
	set currentfileIsModified 0
    }
    bell
}

proc RamDebugger::CheckTextBefore { command args } {
    variable text
    variable CheckTextSave

    if { $command eq "tag" && [regexp {^(add|delete|remove)$} [lindex $args 0]] && \
	     [lindex $args 1] eq "sel" } {
	$text tag remove search 1.0 end
    }

    if { ![regexp {^(ins|del)} $command] } { return }

    # for the search braces stuff
    $text tag remove tempmarker 1.0 end

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
    variable currentfile_secondary
    variable CheckTextSave
    variable breakpoints

    foreach "l1 l2 txt NumlinesOld" $CheckTextSave break
    if { $txt == "" } { return }

    set filetype [GiveFileType $currentfile]

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
	    if { [AreFilesEqual [lindex $br 2] $currentfile] } {
		set line [lindex $br 3]
		if { $delta < 0 && $line >= $l2_new && $line < $l2_old } {
		    set breakpoints [lreplace $breakpoints $i $i]
		    UpdateArrowAndBreak $line 0 ""
		    incr i -1 ;# breakpoints has now one element less
		}
		if { $line >= $l2_old } {
		    UpdateArrowAndBreak $line 0 ""
		    set line [expr {$line+$delta}]
		    set br [lreplace $br 3 3 $line]
		    set breakpoints [lreplace $breakpoints $i $i $br]
		    UpdateArrowAndBreak $line 1 ""
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

    if { ![info exists instrumentedfilesInfo($currentfile)] } { return }

    while { $l1_old > 1 && [lindex [lindex $instrumentedfilesInfo($currentfile) \
		                        [expr $l1_old-1]] 1] != "n" } {
	incr l1_new -1
	incr l1_old -1
    }

#     set level0 [lindex [lindex $instrumentedfilesInfo($currentfile) \
#                 [expr $l1_old-1]] 0]
#     set level $level0
#     while { $level0 > 0 && $level == $level0 } {
#         incr l1_new -1
#         incr l1_old -1
#         set level [lindex [lindex $instrumentedfilesInfo($currentfile) \
#                     [expr $l1_old-1]] 0]
#     }
    if { [regexp {(?n)^[^\[]*\]\s*$} [$text get $l1_new.0 "$l2_new.0 lineend"]] && $l1_old > 1 &&
	 [lindex [lindex $instrumentedfilesInfo($currentfile) [expr $l1_old-1]] 0] > 0 } {
	set newlevel [expr {[lindex [lindex $instrumentedfilesInfo($currentfile) [expr $l1_old-1]] 0]-1}]
	while { $l1_old > 1 && [lindex [lindex $instrumentedfilesInfo($currentfile) [expr $l1_old-1]] 0] > $newlevel } {
	    incr l1_new -1
	    incr l1_old -1
	}
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
    
    set block [$text get $l1_new.0 "$l2_new.0 lineend"]\n
    set blockinfo ""
    switch $filetype {
	TCL {
	    set err [catch { Instrumenter::DoWorkForTcl $block 0 newblockP newblockR blockinfo 0 } errstring]
	}
	C/C++ {
	    set err [catch { Instrumenter::DoWorkForC++ $block blockinfo 0 $oldlevel } errstring]
	    set oldlevel 0
	}
	XML {
	    set err [catch { Instrumenter::DoWorkForXML $block blockinfo 0 $oldlevel 0 } errstring]
	    set oldlevel 0
	}
	"GiD BAS file" {
	    set err [catch { Instrumenter::DoWorkForBas $block blockinfo 0 $oldlevel } errstring]
	    set oldlevel 0
	}
	"GiD data files" {
	    set err [catch { Instrumenter::DoWorkForGiDData $block blockinfo 0 $oldlevel } errstring]
	    set oldlevel 0
	}
    }

    set blockinfo2 ""
    for { set i 0 } { $i < [expr {$l2_new-$l1_new+1}] } { incr i } {
	set bi [lindex $blockinfo $i]
	if { $bi == "" } { set bi [list 0 n] }
	lappend blockinfo2 [concat [expr $oldlevel+[lindex $bi 0]] [lrange $bi 1 end]]
    }

    set instrumentedfilesInfo($currentfile) [eval lreplace [list $instrumentedfilesInfo($currentfile)] \
		                                 [expr $l1_old-1] [expr $l2_old-1] $blockinfo2]

    ColorizeLines $l1_new $l2_new
    if { [info exists currentfile_secondary] && $currentfile eq $currentfile_secondary } {
	ColorizeLines $l1_new $l2_new text_secondary
    }

    if { [info exists instrumentedfilesP($currentfile)] } {
	unset instrumentedfilesP($currentfile) instrumentedfilesR($currentfile)
    }
    if { [info exists instrumentedfilesTime($currentfile)] } {
	unset instrumentedfilesTime($currentfile)
    }
    if { [string match ins* $command] && [$text get "insert-1c"] == "\}" || \
	     [$text get "insert-1c"] == "\]" } {
	SearchBraces -1 -1
    }
}

proc RamDebugger::SearchBraces { x y } {
    variable text
    variable currentfile

    if { [regexp {^\*(loop|if|for|end|endloop|endif|endfor)\M} \
	      [$text get "insert linestart" "insert lineend"]] && \
	     [$text compare [$text index insert] <= [$text index "insert linestart +1c"]] && \
	     [GiveFileType $currentfile] == "GiD BAS file" } {
	SelectBasLoop
	return
    }

    set sel [$text get insert-1c]
    set selm1 [$text get insert-2c]
    $text tag remove sel 0.0 end
    $text tag add sel insert-1c insert
    if { [lsearch -exact [list \[ \] \{ \}] $sel] == -1 || $selm1 == "\\" } {
	set sel [$text get insert]
	set selm1 [$text get insert-1c]
	$text tag remove sel 0.0 end
	$text tag add sel insert insert+1c
	$text mark set insert insert+1c
    }
    if {[lsearch -exact [list \[ \] \{ \}] $sel] == -1  || $selm1 == "\\" } {
	# when not doing it by mouse, use x=-1
	if { $x >= 0 } {
	    set ::tkPriv(selectMode) word ;# tcl8.3
	    catch { set ::tk::Priv(selectMode) word } ;# tcl8.4
	    tkTextSelectTo $text $x $y
	    catch { $text mark set insert sel.last}
	    catch { $text mark set anchor sel.first}
	}
    } else {
	if { $sel == "\[" || $sel == "\{" } {
	    set dir -forwards
	    set stopindex [$text index end]
	    set idx [$text index sel.last]
	    set incr +1
	    $text mark set insert insert-1c
	} else {
	    set dir -backwards
	    set stopindex 1.0
	    set idx [$text index sel.first]
	    set incr -1
	    #$text mark set insert insert+1c
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
		        set error 1
		        break
		    }
		}
	    if { $dir == "-forwards" } {
		set idx [$text index $idx2+1c]
	    } else { set idx $idx2 }
	}
	if { $level_alt != 0 } {
	    set error 1
	}
	if { $idx2 == "" } {
	    set error 1
	    set idx2 $stopindex
	}
	if { $error } { bell }
	$text tag remove sel 1.0 end
	
	if { $dir == "-forwards" } {
	    set idxA insert
	    set idxB $idx2+1c
	} else {
	    set idxA $idx2
	    set idxB insert
	}
	
	if { $error } { SetMessage "error: braces not OK" }
	
	# when not doing it by mouse, use x=-1
	if { $x >= 0 } {
	    $text tag add sel $idxA $idxB
	    $text see $idx2
	} else {
	    $text tag add tempmarker $idxA $idxB
	    $text tag conf tempmarker -background [$text tag cget sel -background] \
		-foreground [$text tag cget sel -foreground]
	    if { $error } { $text tag conf tempmarker -background red }
	    after 1000 $text tag remove tempmarker 1.0 end
	}
    }
}

proc RamDebugger::CenterDisplay {} {
    variable text
    variable text_secondary

    if { [info exists text_secondary] && [focus -lastfor $text] eq $text_secondary } {
	set mytext $text_secondary
    } else { set mytext $text }

    scan [$mytext index insert] "%d" line
    set NumLines [scan [$mytext index end-1c] %d]

    foreach "f1 f2" [$mytext yview] break
    set ys [expr $line/double($NumLines)-($f2-$f1)/2.0]
    if { $ys < 0 } { set ys 0 }
    $mytext yview moveto $ys
}

proc RamDebugger::CommentSelection { what } {
    variable text
    variable currentfile

    set filetype [GiveFileType $currentfile]
    if { $filetype == "C/C++" } {
	set commentchar "//"
    } elseif { $filetype == "GiD BAS file" } {
	set commentchar "*#"
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

    set filetype [GiveFileType $currentfile]
    if { $filetype == "C/C++" } {
	set indent_val $options(indentsizeC++)
    } elseif { $filetype == "XML" } {
	set indent_val $options(indentsizeC++)
    } elseif { $filetype == "TCL" } {
	set indent_val $options(indentsizeTCL)
    } elseif { $filetype == "GiD BAS file" } {
	set indent_val 0
    } else { return }

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
    if { $filetype == "C/C++" && $FirstChar == "\{" && $type == "c" } {
	set indent [expr $indent-$indent_val]
    } elseif { [regexp {TCL|C/C\+\+} $filetype] && $FirstChar == "\}" && $indent >= $indent_val } {
	set indent [expr $indent-$indent_val]
    }
    if { $FirstPos == -1 } { set FirstPos $col }
    if { $FirstPos < $indent } {
	$text insert $line.0 [string repeat " " [expr $indent-$FirstPos]]
    } elseif { $FirstPos > $indent } {
	$text delete $line.0 $line.[expr $FirstPos-$indent]        
    }
    if { $pos >= 0 && $pos < $indent && [string trim [$text get $line.0 insert]] == "" } {
	$text mark set insert $line.$indent
    }
}

proc RamDebugger::UpdateLineNum { command args } {
    variable LineNum
    variable text
    variable filesmtime
    variable currentfile
    variable currentfileIsModified

    RamDebugger::CVS::SetUserActivity

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

    if { $currentfile ne "" && [string index $currentfile 0] != "*" } {
	if { [lindex [file system $currentfile] 0] eq "native" } {
	    set exists [file exists $currentfile]
	    set mtime [file mtime $currentfile]
	} else {
	    set exists 1
	    set mtime $filesmtime($currentfile)
	}
	if { $exists && $mtime > $filesmtime($currentfile) } {
	    set filesmtime($currentfile) $mtime

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
}

proc RamDebugger::_DynamicHelpInEntryWithVar { entry } {
    set var [$entry cget -textvariable]
    DynamicHelp::register $entry balloon [set $var]
}

proc RamDebugger::DynamicHelpInEntryWithVar { entry } {
    set var [$entry cget -textvariable]
    trace var $var w "_DynamicHelpInEntryWithVar $entry ;#"
}

proc RamDebugger::FindPanedWindowFromPane { pane } {

    while 1 {
	set pane [winfo parent $pane]
	if { [string tolower [winfo class $pane]] eq "panedwindow" } { break }
    }
    return $pane
}

proc RamDebugger::CreatePanedEntries { num pane1 pane2 suffix } {
    variable EvalEntries

    set panew [FindPanedWindowFromPane $pane1]

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

	bind $pane1.e$i <3> {
	    set menu %W.menu
	    destroy $menu
	    menu $menu -tearoff 0
	    $menu add command -label "Expressions..." -command \
		[list RamDebugger::DisplayVarWindow [winfo toplevel %W] \
		     [%W get]]
	    focus %W
	    %W selection range 0 end
	    tk_popup $menu %X %Y
	}

	bind $pane2.e$i <Return> "RamDebugger::CheckEvalEntries$suffix do $i,right$suffix"

	bind $pane2.e$i <FocusOut> "RamDebugger::CheckEvalEntries$suffix do $i,left$suffix"
	bind $pane2.e$i <ButtonRelease-1> {
	    %W selection range 0 end
	    %W icursor end
	}
	bind $pane2.e$i <Down> {tkTabToWindow [tk_focusNext %W]}
	bind $pane2.e$i <Up> {tkTabToWindow [tk_focusPrev %W]}
	
	DynamicHelpInEntryWithVar $EvalEntries($i,rightentry$suffix)
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
	    #regsub {^tk} $i {::tk::} new
	    #interp alias "" $i "" $new
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
	set id [lindex [$canvas find withtag l$line] 0]
	foreach "- ycanvas - -" [$canvas bbox $id] break
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

proc RamDebugger::ApplyDropBinding { w command } {

    if { [info command dnd] == "" } { return }

    dnd bindtarget $w text/uri-list <Drop> $command
    foreach i [winfo children $w] {
	ApplyDropBinding $i $command
    }


    #     if { $::tcl_platform(platform) == "windows"} {
    #         dnd bindtarget $w Files <Drop> $command
    #     } else {
    #         dnd bindtarget $w text/uri-list <Drop> $command
    #         foreach i [winfo children $w] {
    #             ApplyDropBinding $i $command
    #         }
    #     }
}

proc RamDebugger::DropBindingDone { files } {

    foreach i $files {
	OpenFileF $i
    }
}

proc RamDebugger::AddFileTypeMenu { filetype } {
    variable mainframe
    variable descmenu
    variable text
    variable currentfile

    set menu [$mainframe getmenu filetypemenu]
    
    set changes 0
    switch $filetype {
	"GiD BAS file" - "GiD data files" {
	    set menu [list \
		          [list command "&Select loop" {} "Search and select the loop that begins or ends here" \
		               "Ctrl Space" -command "RamDebugger::SelectBasLoop"] \
		          [list separator] \
		          [list command "&Update numbers" {} \
		               "Change Conditions or Materials numbers and orders them" \
		               "" -command "RamDebugger::UpdateNumbersInGiDFiles"] \
		          [list separator] \
		          [list command "&Conditions/Materials wizard" {} \
		               "Makes it easy to create one new condition" \
		               "" -command [list RamDebugger::Wizard::CondMatWizard $text $currentfile]]]
	    set descmenu_new [linsert $descmenu 30 "&GiD" all filetypemenu 0 $menu]
	    set changes 1
	}
	default { set descmenu_new $descmenu }
    }
    if { [$mainframe getmenu filetypemenu] != "" } { set changes 1 }

    if { $changes } {
	MainFrame::_create_menubar $mainframe $descmenu_new
	
	set menu [$mainframe getmenu activeprograms]
	$menu configure -postcommand [list RamDebugger::ActualizeActiveProgramsIfVoid \
		$menu]

	set menu [$mainframe getmenu view]
	$menu configure -postcommand [list RamDebugger::ActualizeViewMenu $menu]
	
	set menu [$mainframe getmenu recentfiles]
	$menu configure -postcommand [list RamDebugger::AddRecentfilesToMenu $menu]
	
	# very dirty. Without it, the radiobutton indicator is not drawn. Why???
	set menu [$mainframe getmenu activeconfiguration]
	$menu conf -postcommand "$menu conf -selectcolor black"
	
    }
}

if { [llength [info command lrepeat]] == 0 } {
    proc lrepeat { count element } {
	set retval ""
	for { set i 0 } { $i < $count } { incr i } {
	    lappend retval $element
	}
	return $retval
    }
}

proc RamDebugger::MarkerContextualSubmenuDo { line what } {

    switch $what {
	set {
	    if { [catch [list rbreak $line] errorstring] } {
		WaitState 0
		WarnWin $errorstring
		return
	    }
	    UpdateArrowAndBreak $line 1 ""
	}
	clear {
	    foreach num [rinfo $line] {
		rdel $num
	    }
	    UpdateArrowAndBreak $line 0 ""
	}
	enabledisable {
	    foreach num [rinfo $line] {
		renabledisable $num
	    }
	    UpdateArrowAndBreak $line "" ""
	}
	clearcond {
	    foreach num [rinfo $line] {
		rcond $num ""
	    }
	}
	window {
	    DisplayBreakpointsWindow
	}
    }
}

proc RamDebugger::MarkerContextualSubmenu { w x y X Y } {
    variable text
    variable marker
    variable options

    set line [scan [$text index @0,$y] %d]
    set num -1
    foreach "num endis - - cond" [lindex [rinfo -full $line] 0] break

    set menu $w.menu
    catch { destroy $menu }
    menu $menu -tearoff 0

    if { $num == -1 } {
	$menu add command -label "Set breakpoint" -command \
	    [list RamDebugger::MarkerContextualSubmenuDo $line set]
    } else {
	$menu add command -label "Clear breakpoint" -command \
	    [list RamDebugger::MarkerContextualSubmenuDo $line clear]
	if { $endis } {
	    $menu add command -label "Disable breakpoint" -command \
		[list RamDebugger::MarkerContextualSubmenuDo $line enabledisable]
	} else {
	    $menu add command -label "Enable breakpoint" -command \
		[list RamDebugger::MarkerContextualSubmenuDo $line enabledisable]
	}
	if { $cond != "" } {
	    $menu add command -label "Clear condition: $cond" -command \
		[list RamDebugger::MarkerContextualSubmenuDo $line clearcond]
	}
    }
    $menu add separator

    set item [$marker find withtag "bookmark&&l$line"]

    if { $item eq "" } {
	$menu add command -label "Save position" -command \
	    [list RamDebugger::PositionsStack save $text $line]
    } else {
	$menu add command -label "Clear position" -command \
	    [list RamDebugger::PositionsStack clean $text $line]
    }
    $menu add separator
    $menu add cascade -menu $menu.m -label "Go to position"
    menu $menu.m -tearoff 0

    foreach i $options(saved_positions_stack) {
	foreach "file line context" $i break
	if { $context ne "" } { set context "-- $context" }
	set txt "[file tail $file]:$line $context"
	if { [string length $txt] > 60 } {
	    set txt [string range $txt 0 56]...
	}
	$menu.m add command -label $txt -command \
	    [list RamDebugger::PositionsStack goto $text $line $file]
    }
    $menu add command -label "Positions window" -command \
	[list RamDebugger::DisplayPositionsStack $text $line]
    $menu add command -label "Breakpoints window" -command \
	[list RamDebugger::MarkerContextualSubmenuDo $line window]

    tk_popup $menu $X $Y
}

# only for windows
proc RamDebugger::RegisterExtension {} {
    variable text

    package require registry

    set key(1) {HKEY_CLASSES_ROOT\.tcl}

    if { [catch {registry get $key(1) ""} val(1)] } {
	set val(1) TclFile
    }

    set key(2) "HKEY_CLASSES_ROOT\\$val(1)"

    if { [catch {registry get $key(2) ""} val(2)] } {
	set val(2) TCL-TK
    }

    set key(3) "$key(2)\\shell\\RamDebugger\\command"
    set val(3) "\"[file nativename [info nameofexecutable]]\" "
    if { ![string equal [file tail $::argv0] main.tcl] } {
	append val(3) "\"[file nativename $::argv0]\" "
    }
    append val(3) "\"%1\" %*"

    for { set i 1 } { $i <= 3 } { incr i } {
	if { [catch { registry get $key($i) "" } rval($i)] } {
	    set rval($i) ""
	}
    }

    if { $val(1) eq $rval(1) && $val(2) eq $rval(2) && $val(3) eq $rval(3) } {
	dialogwin_snit $text._ask -title "Unassociate extension"
	set f [$text._ask giveframe]
	label $f.l1 -text "Do you want to unassociate command 'RamDebugger from extension .tcl?"
	set smallfontsize [expr {[font actual [$f.l1 cget -font] -size]-1}]
	label $f.l2 -font "-size $smallfontsize" -text "Note: this command can be used in the Windows\
	      explorer by using the contextual menu over one .tcl file" -wraplength 170 -justify left

	grid $f.l1 -sticky nw -padx 5 -pady 5
	grid $f.l2 -sticky nw -padx 5 -pady 5

	set action [$text._ask createwindow]
	destroy $text._ask
	if { $action <= 0 } {  return }

	if { [catch {
	    registry delete "$key(2)\\shell\\RamDebugger"
	}] } {
	    tk_messageBox -message \
		"Error in the operation. Check your permissions and/or enter as administrator"
	}
	return
    }
    dialogwin_snit $text._ask -title "Associate extension"
    set f [$text._ask giveframe]
    label $f.l1 -text "Do you want to associate command 'RamDebugger to extension .tcl?"
    set smallfontsize [expr {[font actual [$f.l1 cget -font] -size]-1}]
    label $f.l2 -font "-size $smallfontsize" -text "Note: this command can be used in the Windows\
	   explorer by using the contextual menu over one .tcl file" -wraplength 170 -justify left

    grid $f.l1 -sticky nw -padx 5 -pady 5
    grid $f.l2 -sticky nw -padx 5 -pady 5

    set action [$text._ask createwindow]
    destroy $text._ask
    if { $action <= 0 } {  return }

    if { [catch {
	for { set i 1 } { $i <= 3 } { incr i } {
	    registry set $key($i) "" $val($i)
	}
    }] } {
	tk_messageBox -message "Error in the operation. Check your permissions and/or enter as administrator"
    }
}

proc RamDebugger::ExtractExamplesDir {} {
    variable MainDir
    variable text 

    set dir [tk_chooseDirectory -initialdir $MainDir -parent $text \
		 -title "Select directory where to extract the Examples directory"]
    if { $dir eq "" } { return }
    file copy -force [file join $MainDir Examples] $dir
    SetMessage "Copied examples directory into directory '$dir'"
}

proc RamDebugger::ShowStatusBar {} {
    variable mainframe
    variable options

    switch $options(showstatusbar) {
	1 {
	    $mainframe showstatusbar progression
	}
	0 {
	    $mainframe showstatusbar none
	}
    }
}

proc RamDebugger::ShowButtonsToolBar {} {
    variable mainframe
    variable options

    switch $options(showbuttonstoolbar) {
	1 {
	    $mainframe showtoolbar 0 1
	}
	0 {
	    $mainframe showtoolbar 0 0
	}
    }
}


proc RamDebugger::InitGUI { { w .gui } { geometry "" } { ViewOnlyTextOrAll "" } { topleveluse "" } } {
    variable options
    variable options_def
    variable marker
    variable text
    variable mainframe
    variable listbox
    variable listboxlabel
    variable listboxlabelframe
    variable pane2in1
    variable images
    variable textST
    variable textOUT
    variable textCOMP
    variable breakpoints
    variable MainDir
    variable TimeMeasureData
    variable debuggerstate
    variable descmenu
    variable pane1
    variable pane2
    variable pane3
    variable iswince

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
    #auto_load ComboBox
    package require Tablelist
    package require BWidgetR
    # dirty trick to avoid conflicts with other bwidget packages
    auto_load ComboBox
    package require supergrid
    package require supertext
    package require dialogwin
    package require textutil
    #needed a catch for wince
    package require helpviewer 1.1
    catch { package require tkdnd } ;# only if it is compiled
    #catch { package require mytile }

    CreateImages
    TkBackCompatibility
    CreateModifyFonts
    InitOptions

    if { $topleveluse == "" } {
	toplevel $w
    } else {
	toplevel $w -use $topleveluse
	update idletasks ;# doesn't work if this is removed; does not work with it either
    }
    if { !$iswince && $topleveluse == "" } {
	wm withdraw $w
	wm geom $w 800x600
    } else { update }

    wm title $w RamDebugger
    wm protocol $w WM_DELETE_WINDOW "RamDebugger::ExitGUI"

    ApplyDropBinding $w [list RamDebugger::DropBindingDone %D]
    set descmenu [list \
		"&File" all file 0 [list \
		[list command "&New" {} "Begin new file" "" \
		-command "RamDebugger::NewFile"] \
		[list command "&Open" {} "Select source file" "Ctrl o" \
		-command "RamDebugger::OpenFile"] \
		[list command "&Save" {} "Save file" "Ctrl s" \
		-command "RamDebugger::SaveFile save"] \
		[list command "Save &as" {} "Save file as" "" \
		-command "RamDebugger::SaveFile saveas"] \
		[list command "&Close" {} "Close current file" "Ctrl F4" \
		-command "RamDebugger::CloseFile"] \
		separator \
		[list cascad "&Debug on" {} activeprograms 0 {}] \
		separator \
		[list cascad "&Revisions" {} revisions 0 [list \
		[list command "&Save revision" {} "Saves a revision of the file" "ShiftCtrl s" \
		        -command "RamDebugger::CVS::SaveRevision"] \
		[list command "&Open revisions list" {} "Open revisions list for current file" "" \
		        -command "RamDebugger::CVS::OpenRevisions"] \
		separator \
		[list command "&View revised files" {} \
		     "View all files under revision control" "" \
		     -command "RamDebugger::CVS::ShowAllFiles"] \
		                                             ]] \
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
		    [list command "&Save/clear position" {} \
		        "Save position to stack or clear position" "Shift F2" \
		        -command "RamDebugger::PositionsStack save"] \
		    [list command "&Go to position" {} "Recover position from stack" "F2" \
		        -command "RamDebugger::PositionsStack go"] \
		    [list command "&Display positions stack" {} "Display positions stack" "Ctrl F2" \
		        -command "RamDebugger::DisplayPositionsStack"] \
		   ] \
		] \
		[list cascad "&Macros" {} macros 0 [list \
		    [list command "Macros..." {} "Modify or execute macros" "" \
		        -command "RamDebugger::Macros $w"] \
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
		[list command "&Replace..." {} "Replace text in source file" "" \
		-command "RamDebugger::SearchWindow 1"] \
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
		[list checkbutton "&View files pane" {} \
		"Toggle between viewing the file list pane" "" \
		-command "RamDebugger::CheckListFilesPane" \
		-variable RamDebugger::options(listfilespane)] \
		[list command "&Secondary view" {} \
		"Toggle between activating a secondary view for files" "Ctrl 2" \
		-command "RamDebugger::ViewSecondText"] \
		[list command "&Toggle focus" {} \
		"Toggle between activating the main or the secondary view" "Ctrl 3" \
		-command "RamDebugger::FocusSecondTextToggle"] \
		[list command "&Toggle views" {} \
		"Toggle files between the main and the secondary view" "Ctrl 4" \
		-command "RamDebugger::ToggleViews"] \
		separator \
		[list checkbutton "Status bar" {} \
		 "View/hide status bar" "" \
		-variable RamDebugger::options(showstatusbar) -command RamDebugger::ShowStatusBar] \
		[list checkbutton "Buttons toolbar" {} \
		 "View/hide buttons toolbar" "" \
		-variable RamDebugger::options(showbuttonstoolbar) -command RamDebugger::ShowButtonsToolBar] \
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
		[list command "Break" debugentry "Break execution as fast as possible" "" \
		-command "RamDebugger::ContNextGUI rnextfull"] \
		[list command "Stop debugging" debugentry "Stop current debugging" "Shift F5" \
		     -command RamDebugger::DisconnectStop] \
		separator \
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
		[list command "&Profile procedures..." debugentry \
		    "Open a window to profile execution time of procs" "" \
		-command "profileprocs::OpenGUI"] \
		separator \
		[list command "&Reinstrument" debugentry \
		    "Reinstrument and recolorize a file" "" \
		-command "RamDebugger::ReinstrumentCurrentFile"] \
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
		[list command "Open Tkdiff" {} "Open Tkdiff" "" \
		-command "RamDebugger::OpenProgram tkdiff"] \
		separator \
		[list command "&View instrumented file P" {} "View instrumented file P" "" \
		-command "RamDebugger::ViewInstrumentedFile instrumentedP"] \
		[list command "View instrumented file R" {} "View instrumented file R" "" \
		-command "RamDebugger::ViewInstrumentedFile instrumentedR"] \
		[list command "View instrumented info file" {} "View instrumented info file" "" \
		-command "RamDebugger::ViewInstrumentedFile info"] \
		[list command "View instrumented time file" {} "View instrumented time file" "" \
		-command "RamDebugger::ViewInstrumentedFile time"] \
		[list command "View gdb log" {} \
		  "View all commands transferred from/to gdb, if debugging c++" "" \
		-command "RamDebugger::ViewInstrumentedFile gdb"] \
		[list command "&Count LOC" {} "Count number of lines of code" "" \
		-command "RamDebugger::CountLOCInFiles $w"] \
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
		[list command "&Extract examples" "extractexamples" \
		     "Extracts examples directory to a user-selectable directory" "" \
		-command "RamDebugger::ExtractExamplesDir"] \
		[list command "&Register cmd extension..." registerextension \
		     "Register RamDebugger as command in the .tcl extension" "" \
		-command "RamDebugger::RegisterExtension"] \
		separator \
		[list command "&About" {} "Information about the program" "" \
		-command "RamDebugger::AboutWindow"] \
		] \
		]


    set mainframe [MainFrame $w.mainframe \
		       -textvariable RamDebugger::status \
		       -progressvar RamDebugger::progressvar -progressmax 100 \
		       -progresstype normal -menu $descmenu -grid 0]
    #$mainframe showstatusbar progression 

    if { $::tcl_platform(platform) ne "windows" } {
	$mainframe setmenustate registerextension disabled
    }

    if { ![info exists ::starkit::topdir] } {
	$mainframe setmenustate extractexamples disabled
    }

    set label [$mainframe addindicator -textvariable RamDebugger::debuggerstate -width 6 \
	    -anchor e -padx 3]

    bind $label <1> [list RamDebugger::DisplayTimesWindowMenu $label %X %Y]

    set label [$mainframe addindicator -textvariable RamDebugger::LineNum -width 6 \
	    -anchor e -padx 3]

    bind $label <1> RamDebugger::GotoLine
    set label [$mainframe addindicator -textvariable RamDebugger::remoteserver -width 15 \
		   -anchor e -padx 3]
    set menu [$mainframe getmenu activeprograms]
    #$menu configure -postcommand [list RamDebugger::ActualizeActivePrograms $menu]

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
    $mainframe addtoolbar ;# search
    $mainframe showtoolbar 1 0
    $mainframe addtoolbar ;# getfile
    $mainframe showtoolbar 2 0
    if { $iswince } {
	#wince
	set bbox [ButtonBox $toolbar.bbox1 -spacing 0 -padx 0 -pady 0 -homogeneous 1 -grid "0 w" \
		-spacing 0]
	$bbox add -text . \
	    -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1 -padx 0 -pady 0 \
	    -helptext "Menu" \
	    -command "tk_popup [$w cget -menu] \[winfo pointerx .] \[winfo pointery .]"
	$bbox add -image filenew16 \
	    -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1 -padx 0 -pady 0 \
	    -helptext "Begin new file" \
	    -command "RamDebugger::NewFile"
	$bbox add -image fileopen16 \
	    -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1 -padx 0 -pady 0 \
	    -helptext "Open source file" \
	    -command "RamDebugger::OpenFile"
	$bbox add -image filesave16 \
	    -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1 -padx 0 -pady 0 \
	    -helptext "Save file" \
	    -command "RamDebugger::SaveFile save"
    } else {
	set bbox [ButtonBox $toolbar.bbox1 -spacing 0 -padx 1 -pady 1 -homogeneous 1 -grid "0 w" \
		-spacing 2]
	$bbox add -image filenew22 \
	    -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1 -padx 1 -pady 1 \
	    -helptext "Begin new file" \
	    -command "RamDebugger::NewFile"
	$bbox add -image fileopen22 \
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
	$bbox add -image stop-22 \
	    -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1 -padx 1 -pady 1 \
	    -helptext "stop debugging" \
	    -command "RamDebugger::DisconnectStop"
    }
    supergrid::go $toolbar


    set f [$mainframe getframe]

    ################################################################################
    # the horizontal 3 levels pane
    ################################################################################

    #set pw [PanedWindow $f.pw -side top -pad 0 -weights available -activator line]
    set pw [panedwindow $f.pw -orient horizontal]

    if { [llength [ManagePanes $pw h "300 100"]] == 3 } {
	foreach "weight1 weight2 weight3" [ManagePanes $pw h "100 300 100"] break
    } else {
	set weight1 2
	foreach "weight2 weight3" [ManagePanes $pw h "300 100"] break
    }

    if { ![info exists options(defaultdir)] } {
	set options(defaultdir) [pwd]
    }

    #set pane1 [$pw add -weight $weight1]

    set listboxlabelframe [frame $f.lflf]
    set pane1 $listboxlabelframe

    if { $options(listfilespane) } {
	set pane1 $listboxlabelframe
	$pw add $f.lflf -sticky nsew -width $weight1
    }

#     grid $f.lflf -in $pane1 -row 0 -column 0 -sticky nsew
#     grid columnconfigure $pane1 0 -weight 1
#     grid rowconfigure $pane1 0 -weight 1

    set listboxlabel [Label $listboxlabelframe.l -anchor e -relief raised -bd 1 \
	-padx 5 -grid "0 ew"]
    bind $listboxlabel <Configure> "RamDebugger::ConfigureLabel $listboxlabel"

    bind $listboxlabel <ButtonPress-1> "RamDebugger::ListBoxLabelMenu $listboxlabel %X %Y"
    bind $listboxlabel <ButtonPress-3> "RamDebugger::ListBoxLabelMenu $listboxlabel %X %Y"

    set sw [ScrolledWindow $listboxlabelframe.lf -relief sunken -borderwidth 0 -grid 0]
    set listbox [ListBox $sw.lb -background white -multicolumn 0 -selectmode single]
    $sw setwidget $listbox

    $sw.lb configure -deltay [expr [font metrics [$sw.lb cget -font] -linespace]]
    ListBoxEvents $listbox RamDebugger::ListBoxDouble1 RamDebugger::ListboxMenu
    supergrid::go $f.lflf

    #set pane2 [$pw add -weight $weight2]
    set pane2 [frame $pw.pane2]
    $pw add $pane2 -sticky nsew -width $weight2

    ################################################################################
    # the vertical edit window and stack trace
    ################################################################################

    set pwin [panedwindow $pane2.pw -orient vertical -grid 0]

    foreach "weight1in weight2in" [ManagePanes $pwin v "300 50"] break

    #set pane2in1 [$pwin add -weight $weight1in]
    set pane2in1 [frame $pwin.pane2in1]
    $pwin add $pane2in1 -height $weight1in

    set fulltext [frame $f.fulltext -grid no -bd 1 -relief sunken]
    grid $fulltext -in $pane2in1 -sticky nsew
    grid rowconf $pane2in1 0 -weight 1
    grid columnconf $pane2in1 0 -weight 1

    set marker [canvas $fulltext.can -bg grey90 -grid "0 wns" -width 14 -bd 0 \
	    -highlightthickness 0]

    bind $marker <3> [list RamDebugger::MarkerContextualSubmenu %W %x %y %X %Y]

    set text [supertext::text $fulltext.text -background white -foreground black \
		  -wrap none -width 80 -height 40 \
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
    
    ApplyColorPrefs $text

    #set pane2in2 [$pwin add -weight $weight2in]
    set pane2in2 [frame $pwin.pane2in2]
    $pwin add $pane2in2 -height $weight2in

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


    proc TextStackTraceRaise {} "catch { $pane2in2.nb raise stacktrace }"
    proc TextOutRaise {} "catch { $pane2in2.nb raise output }"
    proc TextCompRaise {} "catch { $pane2in2.nb raise compile }"


    #set pane3 [$pw add -weight $weight3]
    set pane3 [frame $pw.pane3]
    $pw add $pane3 -sticky nsew -width $weight3


    ################################################################################
    # the vertical user defined - local
    ################################################################################

    set pw1 [panedwindow $pane3.pw -orient vertical -grid "0"]

    foreach "weight3in1 weight3in2" [ManagePanes $pw1 h "100 100"] break

    #set pane3in1 [$pw1 add -weight $weight3in1]
    set pane3in1 [frame $pw1.pane3in1]
    $pw1 add $pane3in1 -sticky nsew -height $weight3in1

    label $pane3in1.l1 -text "User defined variables" -relief raised -bd 1 -grid "0 ew"

    set sw [ScrolledWindow $pane3in1.sw -borderwidth 0 -bd 1 -relief raised -grid "0 nswe"]
    set sf [ScrollableFrame $sw.f -constrainedwidth 1]
    $sw setwidget $sf
    set f1 [$sf getframe]


    ################################################################################
    # the horizontal user defined vars
    ################################################################################

    set pw [panedwindow $f1.pw -orient horizontal -grid "0 ns"]

    foreach "weight1 weight2" [ManagePanes $pw h "100 100"] break

#     set pane1_vars [$pw add -weight $weight1]
#     set pane2_vars [$pw add -weight $weight2]
    set pane1_vars [frame $pw.pane1_vars]
    $pw add $pane1_vars -sticky nsew -width $weight1
    set pane2_vars [frame $pw.pane2_vars]
    $pw add $pane2_vars -sticky nsew -width $weight2

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

#     set pane3in2 [$pw1 add -weight $weight3in2]
    set pane3in2 [frame $pw1.pane3in2]
    $pw1 add $pane3in2 -sticky nsew -height $weight3in2

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

    set pwL [panedwindow $f1L.pw -orient horizontal -grid "0 ns"]

    foreach "weight1 weight2" [ManagePanes $pwL h "100 100"] break

#     set pane1_varsL [$pwL add -weight $weight1]
#     set pane2_varsL [$pwL add -weight $weight2]
    set pane1_varsL [frame $pwL.pane1_varsL]
    $pwL add $pane1_varsL -sticky nsew -width $weight1
    set pane2_varsL [frame $pwL.pane2_varsL]
    $pwL add $pane2_varsL -sticky nsew -width $weight2

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
    #if { [info exists pane1] } { supergrid::go $pane1 }
    supergrid::go $pane2
    supergrid::go $pane3

    grid $f.pw -sticky nsew
    grid columnconfigure $f 0 -weight 1
    grid rowconfigure $f 0 -weight 1
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
	bind Text <MouseWheel> ""
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


    set menudev [$mainframe getmenu debug]

    bind $text <1> [list focus $text]
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
    bind $text <Control-Tab> "[list RamDebugger::ChooseViewFile start] ; break"
#     bind $text <Control-Tab> "RamDebugger::GotoPreviusNextInWinList prev ; break"
#     bind $text <Control-Shift-Tab> "RamDebugger::GotoPreviusNextInWinList next ; break"
    bind $text <Alt-Right> "RamDebugger::GotoPreviusNextInWinList next ; break"
    bind $text <Tab> "RamDebugger::Indent ; break"
    bind $text <Control-x> "RamDebugger::CutCopyPasteText cut   ; break"
    bind $text <Control-c> "RamDebugger::CutCopyPasteText copy  ; break"
    bind $text <Control-v> "RamDebugger::CutCopyPasteText paste ; break"
    bind [winfo toplevel $text] <Tab> ""
    bind $text <FocusIn> [list RamDebugger::SearchWindow_autoclose]

    bind $w <Shift-Key-F5> "RamDebugger::DisconnectStop ;break"

    foreach i [bind $w] {
	bind $text $i "[bind $w $i] ;break"
    }
    bind $marker <1> {
	tk::TextButton1 $RamDebugger::text 0 %y
	 set tk::Priv(selectMode) line
	 tk::TextSelectTo $RamDebugger::text 0 %y
# 
#         set tkPriv(x) 0
#         set tkPriv(y) %y
#         set tkPriv(mouseMoved) 0
#         set tkPriv(pressX) 0
#         set tk::Priv(mouseMoved) 0
#         set tk::Priv(pressX) 0
#         set tk::Priv(x) 0
#         set tk::Priv(y) %y
#         $RamDebugger::text mark set insert [tkTextClosestGap $RamDebugger::text 0 %y]
#         $RamDebugger::text mark set anchor insert
# 
#         set ini [$RamDebugger::text index "@0,%y linestart"]
#         set end [$RamDebugger::text index "@0,%y lineend"]
#         $RamDebugger::text tag remove sel 1.0 end
#         $RamDebugger::text tag add sel $ini $end
#         set tkPriv(selectMode) line
    }
    bind $marker <B1-Motion> {
	set tkPriv(x) 0
	set tkPriv(y) %y
	set tk::Priv(x) 0
	set tk::Priv(y) %y
	tk::TextSelectTo $RamDebugger::text 0 %y
    }
    bind $marker <B1-Leave> {
	set tkPriv(x) 0
	set tkPriv(y) %y
	set tk::Priv(x) %x
	set tk::Priv(y) %y
	tk::TextAutoScan $RamDebugger::text
    }
    bind $marker <B1-Enter> {
	tk::CancelRepeat
    }
    bind $marker <ButtonRelease-1> {
	tk::CancelRepeat
    }

    bind all <Control-Key-1> "RamDebugger::DisplayWindowsHierarchy ;break"

    # BWidgets automatically sets these because they are in the main main
    # we only want them individually in every widget
    bind $w <Control-c> ""
    bind $w <Control-v> ""

    if { $::tcl_platform(platform) eq "windows" } {
	event delete <<PasteSelection>>
    }

    ################################################################################
    # start up options
    ################################################################################

    if { $geometry != "" } {
	set options(maingeometry) $geometry
    } elseif { ![info exists options(maingeometry)] } {
	set options(maingeometry) 800x600
    }

    if { $ViewOnlyTextOrAll == "OnlyText" } {
	set options(ViewOnlyTextOrAll) "OnlyText"
    }
    if { [info exists options(ViewOnlyTextOrAll)] && $options(ViewOnlyTextOrAll) == "OnlyText" } {
	RamDebugger::ViewOnlyTextOrAll
    }

    # trick to know if we are debugging RamDebugger
    if { [info command sendmaster] != "" } {
	if { [regexp {(\d+)x(\d+)[+]([-\d]+)[+]([-\d]+)} $options(maingeometry) {} wi he xpos ypos] } {
	    incr xpos 20
	    incr ypos 20
	    wm geometry $w ${wi}x$he+$xpos+$ypos
	    if { [info exists options(currentfile)] && \
		     [AreFilesEqual $options(currentfile) \
		          [sendmaster set ::RamDebugger::currentfile]] } {
		
		set options(currentfile) ""
		for { set i 0 } { $i < [llength $options(RecentFiles)] } { incr i } {
		    set options(currentfile) [lindex $options(RecentFiles) $i]
		    if { ![AreFilesEqual $options(currentfile) \
		               [sendmaster set ::RamDebugger::currentfile]] } {
		        break
		    } else { set options(currentfile) "" }
		}
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

    if { $iswince } {
	# wince
	wm geometry $w 244x268+-6+0
    }

    set menu [$mainframe getmenu activeprograms]
    ActualizeActivePrograms $menu

    set menu [$mainframe getmenu macros]
    AddActiveMacrosToMenu $mainframe $menu


    if { [info exists options(breakpoints)] } {
	if { [llength [lindex $options(breakpoints) 0]] == 4 } {
	    set breakpoints ""
	    foreach i $options(breakpoints) {
		lappend breakpoints [concat [list [lindex $i 0]] 1 [lrange $i 1 3]]
	    }
	} else {
	    set breakpoints $options(breakpoints)
	}
    }
    if { [info exists options(TimeMeasureData)] } {
	set TimeMeasureData $options(TimeMeasureData)
    }

    if { [info exists options(debuggerstate)] && $options(debuggerstate) == "time" } {
	set debuggerstate $options(debuggerstate)
	#RamDebugger::DisplayTimesWindow
    }
    if { [info exists options(SearchToolbar)] && [lindex $options(SearchToolbar) 0] && \
	(![info exists options(SearchToolbar_autoclose)] || !$options(SearchToolbar_autoclose)) } {
	SearchWindow [lindex $options(SearchToolbar) 1]
    }
    ShowStatusBar
    
#     if { [info exists options(remoteserverType)] && $options(remoteserverType) == "remote" && \
#          [info exists options(remoteserver)] } {
#         SetMessage "Connecting remoteserver $options(remoteserver)..."
#         catch { rdebug $options(remoteserver) }
#         SetMessage ""
#     }

    NewFile
    
    focus -force $text
    cproject::Init $w

    # for tkcon
    rename ::exit ::exit_final
    proc ::exit { args } {}

    # for defining what is a word for text widgets
    auto_load tcl_wordBreakAfter
    set ::tcl_wordchars "\\w"
    set ::tcl_nonwordchars "\\W"

    # it is done in this way because if not, the definition gets reload
    proc ::tkTabToWindow { w } { after 100 Widget::traverseTo $w }
#     uplevel \#0 {
#         proc tkTabToWindow {w} {
#             focus $w
#             after 100 {
#                 set w [focus]
#                 if {[string equal [winfo class $w] Entry]} {
#                     $w selection range 0 end
#                     $w icursor end
#                 }
#             }
#         }
#     }

    # if we do it at the beginning, an ugly update is made
    if { $::tcl_platform(platform) != "windows" } {
	wm iconbitmap $w @$MainDir/addons/ramdebugger.xbm
    } elseif { !$iswince } {
	wm iconbitmap $w  $MainDir/addons/ramdebugger.ico
	catch { wm iconbitmap $w -default $MainDir/addons/ramdebugger.ico }
    }
    if { !$iswince } {
	RamDebugger::CVS::ManageAutoSave
    }
    update idletasks
    if { [[winfo toplevel $w] cget -use] == "" } {
	wm deiconify $w
	focus -force $text
    }
}

proc RamDebugger::OpenDefaultFile {} {
    variable options
    variable text

    if { [info exists options(currentfile)] && $options(currentfile) != ""  && \
	     [file exists $options(currentfile)] } {
	SetMessage "Opening file '$options(currentfile)'..."
	OpenFileF $options(currentfile)
	FillListBox
	
	if { [info exists options(currentidx)] } {
	    $text see $options(currentidx)
	    $text mark set insert $options(currentidx)
	}
	SetMessage ""
    }
}

if { ![info exists SkipRamDebuggerInit] } {
    if { [info command master] != "" } {
	set registerasremote 0
    } else { set registerasremote 1 }
    
    if { [set ipos [lsearch $argv "-noprefs"]] != -1 } {
	set readwriteprefs 0
	set argv [lreplace $argv $ipos $ipos]
    } else { set readwriteprefs 1 }
    
    if { [set ipos [lsearch $argv "-onlytext"]] != -1 } {
	set ViewOnlyTextOrAll OnlyText
	set argv [lreplace $argv $ipos $ipos]
    } else { set ViewOnlyTextOrAll "" }
    
    if { [set ipos [lsearch $argv "-rgeom*"]] != -1 } {
	set iposm1 [expr {$ipos+1}]
	set geometry [lindex $argv $iposm1]
	set argv [lreplace $argv $ipos $iposm1]
    } else { set geometry "" }
    
    if { [set ipos [lsearch $argv "-ruse"]] != -1 } {
	set iposm1 [expr {$ipos+1}]
	set topleveluse [lindex $argv $iposm1]
	set argv [lreplace $argv $ipos $iposm1]
    } else { set topleveluse "" }
    
    RamDebugger::Init $readwriteprefs $registerasremote
    
    ################################################################################
    #     Init the GUI part
    ################################################################################
    
    if { [info command wm] != "" && [info commands tkcon_puts] == "" } {
	wm withdraw .
	RamDebugger::InitGUI .gui $geometry $ViewOnlyTextOrAll $topleveluse
	if { [llength $argv] } {
	    RamDebugger::OpenFileF [file normalize [lindex $argv 0]]
	} else {
	    RamDebugger::OpenDefaultFile
	}
	bind all <Control-x><Control-l> "[list source [info script]] ; WarnWin reload"
	
	if { [info command master] != "" } {
	    #RamDebugger::rdebug -master
	}
    }
} 
    
