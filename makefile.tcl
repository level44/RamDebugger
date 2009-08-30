#!/bin/sh
# use -*-Tcl-*- \
    exec tclsh "$0" "$@"

if { [lindex $argv 0] eq "clean" } {
    puts "deleting generated files"
    file delete -force {*}[glob -dir [file dirname [info script]] *.deb *.tar.gz *.zip *.app *.dmg modulesinfo-*]
    exit
}

if { $tcl_platform(platform) eq "unix" } {
    # necessary when using directly "tclkit"
    lappend auto_path {*}$env(TCLLIBPATH)
}

package require createdistribution
namespace import createdistribution::*

if { $tcl_platform(platform) eq "windows" } {
    foreach dir [list "C:/TclTk/tclkit" "e:/TclTk/tclkit"] {
	if { [file isdirectory $dir] } {
	    set createdistribution::tclkitdir $dir
	    break
	}
    }
    set createdistribution::tclkitsh_b tclkitsh85.exe
    set createdistribution::tclkit_b tclkit85.exe
} elseif { $tcl_platform(os) eq "Darwin" } {
    set createdistribution::tclkitdir /Users/ramsan/bin
    set createdistribution::tclkitsh_b tclkit85
    set createdistribution::tclkit_b tclkit85-2
} else {
    set createdistribution::tclkitdir /home/ramsan/mytcltk/tclkit
    set createdistribution::tclkitsh_b tclkit85-linux2
    set createdistribution::tclkit_b tclkit85-linux
}

set createdistribution::do_ask_packages 0
set createdistribution::do_create_dmg 1

set data [tDOM::xmlReadFile [file root [info script]].xml]
set doc [dom parse $data]

set pNodes [$doc selectNodes {/*/Program}]
set pNode [lindex $pNodes 0]

set program_name [$pNode selectNodes string(Name)]
set version [$pNode selectNodes string(Version)]
 
if { $tcl_platform(os) eq "Darwin" } {
    set dist_type starpack
    set dist macosx
} elseif { $tcl_platform(platform) ne "windows" } {
    set dist_type starpack
    set dist linux
} else {
    set dist_type starpack
    set dist windows
}

IconifiedConsole

set createdistribution::doencrypt 0
#set createdistribution::encrypt_packages_list [list compass_utils]

set createdistribution::add_packages [list treectrl BWidgetR tkhtml tdom]
lappend createdistribution::remove_packages trf bwidget \
    vfs::ftp he_dialog wce compass_utils

regsubfiles [list \
	{set Version ([0-9.]+)} RamDebugger.tcl "set Version $version" \
	{set Version ([0-9.]+)} pkgIndex.tcl "set Version $version" \
	]

auto_mkindex scripts *.tcl

set createdistribution::libdir ""

create_README $pNode Readme
set files [list license.terms Readme addons scripts Examples help]

# cannot contain file pkgIndex.tcl
CreateDistribution $dist_type $program_name . RamDebugger.tcl \
    $files addons/ramdebugger.ico $version

if { $tcl_platform(platform) eq "windows" } {
    set exe RamDebugger.exe
} else {
    file rename -force RamDebugger.exe ramdebugger
    set exe ramdebugger
}
lappend files $exe

if { $tcl_platform(platform) eq "windows" } {
    
    CopyAndCreateZipDist $program_name$version-$dist.zip ramdebugger $files
    
} elseif { $tcl_platform(os) ne "Darwin" } {
    set exe [string tolower $program_name]
    set tarfile ${exe}_$version-linux_i386.tar.gz

    set dir $program_name$version
    file mkdir $dir
    file copy {*}$files $dir
    create_README $pNode $dir/README
    cp license.terms $dir/License.txt
    CreateTarGzDist $tarfile $dir
    set deb [create_gid_debian_package $pNode $dir ".RamDebugger .ramdebugger_prefs" \
	    addons/ramdebugger.png]
    #exec alien --to-rpm --scripts $deb
    
    file delete -force $dir
} else {
    create_README $pNode Readme.txt
    file copy -force license.terms License.txt
    set filesM $files
    lappend filesM Readme.txt License.txt
    create_macosx_app $pNode $filesM ramdebugger.icns
    file delete Readme.txt License.txt
}

set files [lrange $files 0 end-1]
lappend files pkgIndex.tcl
set createdistribution::libdir addons

lappend createdistribution::remove_packages autoscroll base64 cmdline fileutil \
    htmlparse img ncgi sha1 snit struct textutil tile vfs treectrl

CreateDistribution zip $program_name-source  . RamDebugger.tcl \
    $files addons/ramdebugger.ico $version


DeiconifyConsole
