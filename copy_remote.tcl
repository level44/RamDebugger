#!/bin/sh
# use -*-Tcl-*- \
    exec tclsh "$0" "$@"

package require createdistribution
namespace import createdistribution::*

set data [tDOM::xmlReadFile [file dirname [info script]]/makefile.xml]
set doc [dom parse $data]

set pNodes [$doc selectNodes {/*/Program}]
set pNode [lindex $pNodes 0]

set program_name [$pNode selectNodes string(Name)]
set version [$pNode selectNodes string(Version)]

if { $tcl_platform(platform) eq "windows" } {
    set dist windows
} elseif { $tcl_platform(os) eq "Darwin" } {
    set dist macosx
} else {
    set dist linux
}

IconifiedConsole
if { $tcl_platform(platform) eq "windows" && [info command console] ne "" } {
    DeiconifyConsole
}

set filesList ""

if { $tcl_platform(platform) eq "windows" } {
    lappend filesList [list $program_name$version-$dist.zip $program_name-$dist.zip]
    #lappend filesList [list setup-$program_name$version-$dist.exe setup-$program_name-$dist.exe]
} elseif { $tcl_platform(os) ne "Darwin" } {
    set exe [string tolower $program_name]
    lappend filesList [list  ${exe}_$version-1_i386.deb ${exe}_i386.deb]
    lappend filesList [list  ${exe}$version-linux_i386.tar.gz ${exe}-linux_i386.tar.gz]
    lappend filesList [list  ${program_name}-source$version.zip ${program_name}-source.zip]
    ## RPM ???
} else {
    lappend filesList [list  $program_name$version-macosx.dmg $program_name-macosx.dmg]
}

set remote_dir "/home/ftp/pub/ramdebugger"
set host ramsan@ftp.compassis.com

puts "starting copy remote..."
copy_remote $filesList $remote_dir $host

puts "done"

if { $tcl_platform(platform) eq "windows" } {
    DeiconifyConsole
} else {
    exit
}
