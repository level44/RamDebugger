
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
		"eval" - "html::eval" {
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
		"bind" {
		    if { [llength $words] == 3 } {
		        set NewDoInstrument 1
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

proc RamDebugger::Instrumenter::GiveCommandUplevel {} {
    variable stack

    return [lindex [lindex [lindex $stack end] 0] 0]
}

# newblocknameP is for procs
# newblocknameR is for the rest
proc RamDebugger::Instrumenter::DoWork { block filenum newblocknameP newblocknameR blockinfoname "progress 1" } {

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
    if { $length >= 1000 && $progress } {
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

	if { $ichar%1000 == 0 && $progress } {
	    RamDebugger::ProgressVar [expr {$ichar*100/$length}]
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
	    $c != "\#" &&  $words == "" } {
	    if { $c != "\}" || [GiveCommandUplevel] != "proc" || \
		$RamDebugger::options(instrument_proc_last_line) } {
		append newblock$OutputType "RDC::F $filenum $line ; "
		set lastinstrumentedline $line
	    }
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
		if { $lastc != "\\" && $wordtype != "\{" && \
		    [lindex $words 0] != "\#" } {
		    if { $wordtype == "" } { set wordtype "w" }
		    set consumed 1
		    PushState \[ $line $newblocknameP $newblocknameR
		    set lastinstrumentedline $line
		}
	    }
	    \] {
		if { $lastc != "\\" && $wordtype != "\"" && $wordtype != "\{" && \
		    [lindex $words 0] != "\#"} {
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
		            if { $currentword != "\\" } {
		                lappend words $currentword
		            }
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

    if { $length >= 1000 && $progress } {
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


proc RamDebugger::Instrumenter::DoWorkForC++ { block blockinfoname "progress 1" { braceslevelIni 0 } } {

    set length [string length $block]
    if { $length >= 5000 && $progress } {
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

	if { $ichar%5000 == 0  && $progress } {
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
    if { $length >= 1000  && $progress } {
	RamDebugger::ProgressVar 100
    }
}
