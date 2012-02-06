#!/bin/sh
# use -*-Tcl-*- \
    exec tclsh "$0" "$@"

package require createdistribution
namespace import createdistribution::*

set optional {
    { -clean "" 0 }
    { -install "" 0 }
    { -install_type local_tgz|system_tgz|deb local_tgz }
    { -fossil "" 0 }
    { -copy_remote "" 0 }
}
parse_args $optional "" $argv

if { $tcl_platform(platform) eq "unix" } {
    # necessary when using directly "tclkit"
    lappend auto_path {*}$env(TCLLIBPATH)
}
set topdir [file normalize [file dirname [info script]]/..]

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
set pNode0 [lindex $pNodes 0]
set version [$pNode0 selectNodes string(Version)]

set architecture ""
if { $tcl_platform(os) eq "Darwin" } {
    set dist_type starpack
    set dist macosx
} elseif { $tcl_platform(platform) ne "windows" } {
    set dist_type starpack
    set dist linux
    switch $tcl_platform(machine) {
	"x86_64" { set architecture amd64 }
	default { set architecture i386 }
    }
} else {
    set dist_type starpack
    set dist windows
}

IconifiedConsole

if { $clean } {
    foreach pNode $pNodes {
	set program_name [$pNode selectNodes string(Name)]
	set files [glob -nocomplain -dir [file dirname [info script]] *.deb *.rpm *.tar.gz *.zip \
		*.app *.dmg *.exe modulesinfo-* [string tolower $program_name] \
		[string tolower $program_name]\[0-9\]* $program_name-source\[0-9\]* *~]
	puts "deleting generated files '$files'"
	if { [llength $files] } {
	    file delete -force {*}$files
	}
    }
    exit
}

if { $copy_remote } {
    set filesList ""
    foreach pNode $pNodes {
	set pname [$pNode @n]
	set program_name [$pNode selectNodes string(Name)]
	set version [$pNode selectNodes string(Version)]
	
	if { $tcl_platform(platform) eq "windows" } {
	    lappend filesList [list $program_name$version-$dist.zip $program_name-current-$dist.zip]
	    #lappend filesList [list setup-$program_name$version-$dist.exe setup-$program_name-current-$dist.exe]
	} elseif { $tcl_platform(os) ne "Darwin" } {
	    set exe [string tolower $program_name]
	    lappend filesList [list ${exe}$version-linux_$architecture.tar.gz ${exe}-current-linux_$architecture.tar.gz]
	    lappend filesList [list ${exe}_$version-1_$architecture.deb ${exe}_current_$architecture.deb]
	    lappend filesList [list ${program_name}-source$version.zip ${program_name}-source-current.zip]
	    ## RPM ???
	} else {
	    lappend filesList [list $program_name$version-macosx.dmg $program_name-current-macosx.dmg]
	}
    }
    switch $pname {
	"ramdebugger" { set remote_dir "/home/ftp/pub/ramdebugger" }
	"vcs-ramdebugger" { set remote_dir "/home/ftp/pub/ramdebugger/vcs-ramdebugger" }
    }
    set remote_dir "/home/ftp/pub/ramdebugger"
    set host gidweb@ftp.compassis.com

    puts "starting copy remote..."
    copy_remote $filesList $remote_dir $host
    puts "done"
    exit
}

regsubfiles [list \
	{set Version ([0-9.]+)} ../RamDebugger.tcl "set Version $version" \
	{set Version ([0-9.]+)} ../pkgIndex.tcl "set Version $version" \
	]

set createdistribution::doencrypt 0
#set createdistribution::encrypt_packages_list [list compass_utils]

#auto_mkindex scripts *.tcl

set createdistribution::libdir ""

if { $::tcl_platform(platform) eq "windows" } {
    set exeList [list cat.exe cvs.exe diff.exe grep.exe kill.exe tlist.exe]
} elseif { $::tcl_platform(os) eq "Darwin" } {
    set exeList [list cvs]
} else {
    set exeList ""
}
foreach i $exeList {
    if { ![file exists [file join .. addons exe $i]] } {
	error "file '[file normalize [file join .. addons exe]]' does not exist"
    }
}

if { $fossil } {
    fossil_tag_add . release_$version
}

set remove_packages0 [list {*}$createdistribution::remove_packages trf bwidget \
	vfs vfs::ftp he_dialog wce compass_utils compass_utils::c \
	textutil::adjust textutil::repeat \
	textutil::split textutil::tabify  textutil::trim  textutil::string tile \
	tooltip htmlparse math autoscroll base64 cmdline md5 struct textutil uri thread \
	ncgi sha1 \
	]

foreach pNode $pNodes {
    set pname [$pNode @n]
    set program_name [$pNode selectNodes string(Name)]
    set version [$pNode selectNodes string(Version)]
    
    puts "\n[string repeat # 80]\n   building $program_name\n[string repeat # 80]\n"

    set createdistribution::add_packages [list treectrl BWidgetR tkhtml tdom tcltklib tktablet snit]
    if { $::tcl_platform(os) ne "Darwin" } {
	lappend createdistribution::add_packages tkdnd
    }
    set createdistribution::remove_packages $remove_packages0

    switch $pname {
	"ramdebugger" {
	    set file0 RamDebugger.tcl
	    set files [list addons scripts help]
	    set dir $topdir
	    set ico addons/ramdebugger.ico
	    set filesFT [list [list [give_package_dir commR] addons/commR .tcl] \
		    [list $topdir/Examples Examples .tcl]]
	}
	"vcs-ramdebugger" {
	    set file0 cvshandle.tcl
	    set files [list HelperWindows.tcl Images.tcl mini_compass_utils.tcl tclIndex ../help]
	    set dir $topdir/scripts
	    set ico ../addons/ramdebugger.ico
	    set filesFT ""
	}
	default {
	    error "error. program_name=$program_name"
	}
    }
    
    if { $pname eq "ramdebugger" } {
	update_lognoter_changes_page $pNode ../docs/RamDebugger.wnl Changes
    }
    
    # cannot contain file pkgIndex.tcl
    CreateDistribution $dist_type $program_name $dir $file0 $files $ico $version

    if { $tcl_platform(platform) eq "windows" } {
	set exe $program_name.exe
    } else {
	file rename -force $program_name.exe [string tolower $program_name]
	set exe [string tolower $program_name]
    }
    set files [list $exe]

    if { $tcl_platform(platform) eq "windows" } {
	create_README $pNode README.txt
	lappend filesFT [list README.txt . .txt] [list license.terms ./License.txt .txt]
	CopyAndCreateZipDist $program_name$version-$dist.zip $exe $files $filesFT
	file delete README.txt
    } elseif { $tcl_platform(os) ne "Darwin" } {
	set tarfile ${exe}$version-linux_$architecture.tar.gz
	
	set dir $exe$version
	file delete -force $dir
	file mkdir $dir
	file copy {*}$files $dir
	copy_filesFT $dir $filesFT
	
	create_README -architecture $architecture $pNode $dir/README
	file copy license.terms $dir/License.txt
	clean_vcs_dirs $dir
	CreateTarGzDist $tarfile $dir
	set deb [create_debian_package -architecture $architecture $pNode $dir ".RamDebugger .ramdebugger_prefs" \
		$topdir/addons/ramdebugger.png]
	#exec sudo alien --to-rpm --scripts $deb
	
	file delete -force $dir
    } else {
	create_README $pNode Readme.txt
	file copy -force license.terms License.txt
	set filesM $files
	lappend filesM Readme.txt License.txt
	create_macosx_app $pNode $filesM $filesFT ramdebugger.icns
	file delete Readme.txt License.txt
    }
    file delete $exe

    if { $tcl_platform(platform) eq "unix" && $pname eq "ramdebugger" } {
	create_README -architecture $architecture  -tcl_source_dist RamDebugger.tcl $pNode README
	set files [list addons scripts Examples help pkgIndex.tcl]
	#lappend files [file normalize README]
	#lappend files [file normalize license.terms]
	set createdistribution::libdir addons
	
	lappend createdistribution::remove_packages autoscroll base64 cmdline fileutil \
	    htmlparse img img::gid img::png img::jpeg img::gif img::base jpegtcl pngtcl \
	    ncgi sha1 snit struct textutil tile vfs treectrl tcltklib tdom zlibtcl Tkhtml tcl8 starkit
	
	set exts [list .dll .so .dylib]
	set ipos [lsearch $exts [info sharedlibextension]]
	set exts [lreplace $exts $ipos $ipos]
	foreach ext $exts {
	    if { ![file exists $topdir/libs/RamDebuggerInstrumenter6_x32$ext] } { continue }
	    file copy -force $topdir/libs/RamDebuggerInstrumenter6_x32$ext $topdir/scripts
	}
	CreateDistribution zip $program_name-source  $topdir RamDebugger.tcl \
	    $files $topdir/addons/ramdebugger.ico $version
	
	foreach ext $exts {
	    file delete $topdir/scripts/RamDebuggerInstrumenter6_x32$ext
	}
	file delete README
	set createdistribution::libdir ""
    }

    if { $install } {
	if { $tcl_platform(platform) ne "windows" } {
	    switch $install_type {
		"local_tgz" {
		    puts "installing in '[pwd]' ..."
		    set ret [exec sudo tar xvfz $tarfile]
		}
		"system_tgz" {
		    puts "installing in /usr/local ..."
		    set ret [exec sudo tar xvfz $tarfile --directory /usr/local]
		}
		"deb" {
		    puts "installing package $deb ..."
		    puts [exec sudo dpkg -i $deb]
		}
	    }
	} else {
	    puts "uncompressing zip file '$program_name$version-$dist.zip' ..."
	    set ret [exec unzip $program_name$version-$dist.zip]
	    puts $ret
	}
    }
}

createdistribution::end

if { $tcl_platform(platform) eq "windows" } {
    DeiconifyConsole
} else {
    exit
}









