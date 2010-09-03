#!/bin/sh
# use -*-Tcl-*- \
    exec tclsh "$0" "$@"

package require createdistribution
namespace import createdistribution::*

set optional {
    { -clean "" 0 }
    { -install "" 0 }
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

if { $clean } {
    set files [glob -nocomplain -dir [file dirname [info script]] *.deb *.rpm *.tar.gz *.zip \
	    *.app *.dmg *.exe modulesinfo-* [string tolower $program_name] \
	    [string tolower $program_name]\[0-9\]* $program_name-source\[0-9\]* *~]
    puts "deleting generated files '$files'"
    if { [llength $files] } {
	file delete -force {*}$files
    }
    exit
}

if { $copy_remote } {
    set filesList ""
    if { $tcl_platform(platform) eq "windows" } {
	lappend filesList [list $program_name$version-$dist.zip $program_name-$dist.zip]
	#lappend filesList [list setup-$program_name$version-$dist.exe setup-$program_name-$dist.exe]
    } elseif { $tcl_platform(os) ne "Darwin" } {
	set exe [string tolower $program_name]
	lappend filesList [list ${exe}$version-linux_i386.tar.gz ${exe}-linux_i386.tar.gz]
	lappend filesList [list ${exe}_$version-1_i386.deb ${exe}_i386.deb]
	lappend filesList [list ${program_name}-source$version.zip ${program_name}-source.zip]
	## RPM ???
    } else {
	lappend filesList [list  $program_name$version-macosx.dmg $program_name-macosx.dmg]
    }
    set remote_dir "/home/ftp/pub/ramdebugger"
    set host ramsan@ftp.compassis.com

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

set createdistribution::add_packages [list treectrl BWidgetR tkhtml tdom tcltklib]

if { $::tcl_platform(os) ne "Darwin" } {
    lappend createdistribution::add_packages tkdnd
}

lappend createdistribution::remove_packages trf bwidget \
    vfs vfs::ftp he_dialog wce compass_utils compass_utils::c \
    textutil::adjust textutil::repeat \
    textutil::split textutil::tabify  textutil::trim  textutil::string tile \
    tooltip htmlparse math autoscroll base64 cmdline md5 struct textutil uri thread \
    ncgi sha1

#auto_mkindex scripts *.tcl

set createdistribution::libdir ""

set files [list addons scripts help]

if { $fossil } {
    fossil_tag_add . release_$version
}

update_lognoter_changes_page $pNode ../docs/RamDebugger.wnl Changes 

# cannot contain file pkgIndex.tcl
CreateDistribution $dist_type $program_name $topdir RamDebugger.tcl \
    $files addons/ramdebugger.ico $version

if { $tcl_platform(platform) eq "windows" } {
    set exe RamDebugger.exe
} else {
    file rename -force RamDebugger.exe ramdebugger
    set exe ramdebugger
}
set files [list $exe]
set filesFT [list [list [give_package_dir commR] addons/commR .tcl] \
	[list $topdir/Examples Examples .tcl]]

if { $tcl_platform(platform) eq "windows" } {
    create_README $pNode README.txt
    lappend filesFT [list README.txt . .txt] [list license.terms ./License.txt .txt]
    CopyAndCreateZipDist $program_name$version-$dist.zip ramdebugger $files $filesFT
    file delete README.txt
} elseif { $tcl_platform(os) ne "Darwin" } {
    set exe [string tolower $program_name]
    set tarfile ${exe}$version-linux_i386.tar.gz

    set dir $exe$version
    file delete -force $dir
    file mkdir $dir
    file copy {*}$files $dir
    copy_filesFT $dir $filesFT

    create_README $pNode $dir/README
    file copy license.terms $dir/License.txt
    clean_vcs_dirs $dir
    CreateTarGzDist $tarfile $dir
    set deb [create_debian_package $pNode $dir ".RamDebugger .ramdebugger_prefs" \
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

if { $tcl_platform(platform) eq "unix" } {
    create_README -tcl_source_dist RamDebugger.tcl $pNode README
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
}

if { $install } {
    if { $tcl_platform(platform) ne "windows" } {
	if 0 {
	    puts "installing in /usr/local ..."
	    set ret [exec sudo tar xvfz $tarfile --directory /usr/local]
	}
	puts "installing package $deb ..."
	set ret [exec sudo dpkg -i $deb]
	puts $ret
    } else {
	puts "uncompressing zip file '$program_name$version-$dist.zip' ..."
	set ret [exec unzip $program_name$version-$dist.zip]
	puts $ret
    }
}

if { $tcl_platform(platform) eq "windows" } {
    DeiconifyConsole
} else {
    exit
}









