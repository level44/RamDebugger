#
# $Id: tkcvs_def.tcl,v 1.6 2009-03-25 23:47:15 ramsan Exp $
#
# TkCVS defaults file.
#
# This file is read by TkCVS on startup.  It will be installed
# automatically by the "configure" script.
#
# Defaults in the .tkcvs file in the user's home directory will
# over-ride this file.
#

# Print setup.
set cvscfg(papersize) "A4"
set cvscfg(pointsize) 9
set cvscfg(headingsize) 14
set cvscfg(subheadingsize) 12


#RAMSAN
source [file join $TCDIR listbox.tcl]

#RAMSAN
option add *TearOff 0


# If you want the module browser to come up on startup, uncomment this.
#set cvscfg(startwindow) "module"

# Colours.  "Colors" that is if you are a yanqui who can't spell.
# Added support for monochrome machines. -sj
if { [winfo depth .] == 1 } {

    set cvscfg(glb_background)      "white"
    set cvscfg(glb_highlight)       "black"
    set cvscfg(bt_lookhere)         "white"
    set cvscfg(bt_pressme)          "black"
    set cvscfg(tool_colour)          "white"

} else {

    # These colours are entered into the options database, see below.
    # configure -background
    # set cvscfg(glb_background)      "SkyBlue2"
    # configure -highlightcolor
    # The selected item in the module tree gets this one
    # set cvscfg(glb_highlight)       "green"
    # set cvscfg(bt_lookhere)         "LightSteelBlue"
    # set cvscfg(bt_pressme)          "green"


#      set cvscfg(glb_background)      "AntiqueWhite3"
#      set cvscfg(glb_highlight)       "green"
#      set cvscfg(bt_lookhere)         "LightSteelBlue"
#      set cvscfg(bt_pressme)          "green"

    # This is the colour of the ToolTips popup windows.
    set cvscfg(tool_colour)          "LightGoldenrod1"

    # These colours are used to support tag marking.
#       set cvscfg(tagcolour,tkcvs_r6)  "Purple"
#       set cvscfg(tagcolour,tkcvs_r5)  "Blue"
#       set cvscfg(tagcolour,tkcvs_r4)  "Green"
#       set cvscfg(tagcolour,tkcvs_r3)  "Yellow"
#       set cvscfg(tagcolour,tkcvs_r2)  "Orange"
#       set cvscfg(tagcolour,tkcvs_r1)  "Red"
}

#
# If you want to configure the colours via the above options,
# then you will also need to un-comment these lines.  Otherwise
# you can use the Xdefaults method of colouring the windows.
#

#  option add *background                 $cvscfg(glb_background)
#  option add *activeBackground           $cvscfg(glb_highlight)
#  option add *Button.background          $cvscfg(bt_lookhere)
#  option add *Button.activeBackground    $cvscfg(bt_pressme)
#  option add *Scrollbar.foreground       $cvscfg(bt_lookhere)
#  option add *Scrollbar.activeForeground $cvscfg(bt_pressme)

 
#RAMSAN
set tk_strictMotif 1
option add *Menu*TearOff 0
option add *background AntiqueWhite3
option add *Button*background bisque3
option add *Menu*background bisque3
option add *Button*foreground black
option add *Entry*background thistle
option add *DisabledForeground grey60
option add *HighlightBackground AntiqueWhite3
font create NormalFont -family {new century schoolbook} -size 10 -slant italic
option add *font NormalFont





#
# To use the Xdefaults method, put lines like the following into
# your .Xdefaults or .Xresources file:
#
# tkcvs*background:                        SkyBlue2
# tkcvs*activeBackground:                green
# tkcvs*Button.background:                LightSteelBlue
# tkcvs*Button.activeBackground:        green
# tkcvs*Scrollbar.background:                LightSteelBlue
# tkcvs*Scrollbar.activeBackground:        green

# Leave this option here so that TkCVS knows what its own background
# colour is.  This is so that it can erase things like file mark dots.
set cvscfg(glb_background) [option get . background Background]

#
# Other defaults
#

#
# Set this to 1 to see all files displayed in the directory
# browser (including hidden files) by default.
#
set cvscfg(allfiles)           0

#
# set the default log file detail to be reported; one of 
#   "summary"    version number and comment string for all check-ins
#   "verbose"    all logfile detail possible, including symbolic tags
# -sj
#set cvscfg(ldetail)            "last"
#RAMSAN
set cvscfg(ldetail)            "verbose"

# --------------------
# set the default detail for repository and workdir reports; one of
#   "terse"      report "status" only and only on those files which 
#                are not "up-to-date"
#   "summary"    report the "status" and include "up-to-date"
#   "verbose"    provide the report as it would appear unmodified
# -sj
set cvscfg(rdetail)            "terse"

# --------------------
# set the number of file names the listbox should accomodate. 
# -sj
#RAMSAN
#set cvscfg(y_size)             24
if {$tcl_platform(platform) == "windows"} {
    set cvscfg(y_size)             25
} else {
    set cvscfg(y_size)             25
}

# --------------------
# set the default pattern to be used by the filter.  Use any valid
# pattern that can be used for a pattern for 'ls'. An empty string
# is equilivant to the entire directory (minus hidden files); 
# i.e., ls *
# -sj
set cvscfg(file_filter)        ""

#RAMSAN
set cvscfg(file_filter_list)  {* {*.{h,c,cc} Makefile* Post } \
	{*.tcl tclfile-opengl  GrInterpNamesPost gidDefaults* tclIndex} \
	{*.{texinfo,info,tex,ps} html-version} }

set cvscfg(ignore_file_filter) "*.o *~ .#*"

# --------------------
# set the default for automatic statusing of a CVS controlled 
# directory.  Automatic updates are done when a directory is 
# entered and after some operations.  More operations should cause
# an automatic update ... TBD.
# -sj
set cvscfg(auto_status)        "true"

# --------------------
# set the command to be used to format selected files.
# e.g., "aimap" is a formatter for Ada code.
# -sj
set cvscfg(format_cmd)         "aimap"

# --------------------
# set the default value for confirmation prompting before performing an
# operation over selected files.
# -sj
set cvscfg(confirm_prompt)     "true"

# I moved setting cvscfg(cvsroot) to env(CVSROOT) to the main program,
# because we can run without it and I don't want an if [info exists] here.
# -dr

# --------------------
# some of the reporting operations could usefully be recursive.  Set
# the default value here.
# -sj
set cvscfg(recurse)            "false"

# Dos specific.
#
# Decide wether you are unlucky and have to run tkcvs on DOS/WIN
# some things will be setup in the following
#
# Please note that you may have to setup a bit more.
#
# Select, if you are using the 'plain' windows or the 'cygnus' stuff.

if {$tcl_platform(platform) == "windows"} {
    # cvs-dos doesn't like the dot "." in some sub-commands
    set cvscfg(thisdir) ""
    # file mask for all files
    set cvscfg(aster) "*.*"
    # null-device
    set cvscfg(null) "nul"
    # Font preferences for the GUI
    #RAMSAN
#      set cvscfg(guifont) {Helvetica -14 bold}
#      set cvscfg(listboxfont) {Helvetica -14 normal}
    set cvscfg(guifont) {"MS Sans Serif" 8 normal}
    set cvscfg(listboxfont) {"MS Sans Serif" 8 normal}
    #
    # Please don't ask me why you have to set -T on DOS,
    # experiments say you have! - CJ
    #
    #set cvs "cvs -T $cvscfg(tmpdir)"
    set cvs "cvs"
    set cvscfg(editor) "notepad"

    # set temp directory
    set cvscfg(tmpdir) $env(TEMP)
    set cvscfg(tkdiff) [list $TclExe [file join $ScriptBin tkdiff.tcl]]    
    set cvscfg(shell)  ""
    #
    # Commands to change file attributes. For lock / unlock.
    # 12-Jan-2000 lcs
    #
    set cvscfg(chmod_ro_cmd) "attrib +R"
    set cvscfg(chmod_rw_cmd) "attrib -R"
    set cvscfg(allow_abort)  "no"
} else {
    set cvscfg(tmpdir) "/tmp"
    set cvscfg(thisdir) .
    set cvscfg(aster) "*"
    set cvscfg(null) "/dev/null"
    # Font preferences for the GUI
    set cvscfg(guifont) {Helvetica -12 bold}
    set cvscfg(listboxfont) {Helvetica -12 normal}
    #
    # Other defaults
    #
    # Full path to the CVS program if you want to give it,
    # otherwise the PATH environment variable will be searched.
    set cvs "cvs"
    # To override the default editor (setup when tkcvs is configured and 
    # installed) a user can set the cvscfg(editor) variable to the editor
    # of choice in their .tkcvs file (if they have one).
    #set cvscfg(editor) "dtpad"


    #RAMSAN
    #set cvscfg(editor) "xterm -e vi"

    #set cvscfg(editor) "emacsclient"

    # The file editor to be used may also be identified by pattern-matching the 
    # filename by setting the cvscfg(editors) variable.  This contains a series
    # of string pairs giving the editor-command and string-match-pattern.  The 
    # first pattern (see rules for [string match]) which matches the filename 
    # going down the list determines which editor is run.  If no patterns match 
    # or the option is not set, the cvscfg(editor) value will be used instead.
    # - anj@aps.anl.gov

    #RAMSAN
    set cvscfg(editors) {
	bitmap *.xbm
	gimp *.xpm
	gimp *.gif
	gnuclient *
    }
    set cvscfg(tkdiff) [list $TclExe [file join $ScriptBin tkdiff.tcl]]
    #set cvscfg(tkdiff) "tkdiff"
    # Commands to change file attributes.
    # 12-Jan-2000 lcs
    set cvscfg(chmod_ro_cmd)   "chmod a-w"
    set cvscfg(chmod_rw_cmd)   "chmod u+w"
    set cvscfg(allow_abort)    "yes"
    # What do you want to happen when you ask for a shell?
    set cvscfg(shell) "xterm -name tkcvsxterm -n {TkCVS xterm}"
}

# set the default command for printing output.
#set cvscfg(print_cmd)          "enscript -Ghr -fCourier8"
set cvscfg(print_cmd)          "lpr"
#
# --------------------
# User Menus
#
# Set any of these strings to a cvs command to add to the User Menu
set cvsmenu(Show_My_Checkouts) "history"
set cvsmenu(Show_All_Checkouts) "history -a"

proc RamCheckUpdateInDir { args } {
    set retval ""
    foreach i $args {
	if { [file isdir $i] } {
	    set olddir [pwd]
	    cd $i
	    catch { exec cvs -n -q update } ret
	    append retval $ret\n
	    cd $olddir
	}
    }
    return $retval
}

proc RamUpdateInDir { args } {
    set retval ""
    foreach i $args {
	if { [file isdir $i] } {
	    set olddir [pwd]
	    cd $i
	    catch { exec cvs -q update } ret
	    append retval $ret\n
	    cd $olddir
	}
    }
    return $retval
}

proc RamHistoryDiff { files } {
    set file [lindex $files 0]
    
    set retval ""
    regexp {Repository revision:\s*([\d]+)\.([\d]+)} [exec cvs status $file] {} {} v
    for { set i 2 } { $i <= $v } { incr i } {    
	catch { exec cvs diff --ignore-all-space -r 1.[expr {$i-1}] -r 1.$i $file 2>@1 } ret
	append retval $ret
    }
    return $retval
}

set "usermenuTCL(RAM check update)" RamCheckUpdateInDir
set "usermenuTCL(RAM update)" RamUpdateInDir
set "usermenuTCL(RAM full history)" RamHistoryDiff

# Set any of these to a shell command to add to the User Menu
# set usermenu(Merge_Conflicts) "$cvscfg(tkdiff) -conflict "

# Kinds of messages for debugging:
#         C       CVS commands
#         F       File creation/deletion
#         T       Function entry/exit tracing
#         D       Debugging"
set cvscfg(log_classes) "CF"
# On (1) or off (0)
set cvscfg(logging)    0

# At the very end, look for a file called "site_def" in the installation
# directory.  That's a good place to define your tagcolours and other
# site-specific things.  It won't be overwritten by installs like this file is.
set cvscfg(tkcvs_path) [lrange $auto_path 0 0]
if {[file exists [file join $cvscfg(tkcvs_path) site_def]]} {
  source [file join $cvscfg(tkcvs_path) site_def]
}



bind Listbox <4> {
    %W yview scroll -3 units
}

bind Listbox <5> {
    %W yview scroll 3 units
}

bind Text <4> {
    %W yview scroll -3 units
}

bind Text <5> {
    %W yview scroll 3 units
}
