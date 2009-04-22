
package require createdistribution
namespace import createdistribution::*

set name RamDebugger
set version 6.3

IconifiedConsole

set createdistribution::doencrypt 1
set createdistribution::encrypt_packages_list [list compass_utils]

set createdistribution::add_packages [list treectrl BWidgetR tkhtml compass_utils]
lappend createdistribution::remove_packages trf bwidget \
    vfs::ftp he_dialog wce

createdistribution::regsubfiles [list \
      {set Version ([0-9.]+)} RamDebugger.tcl "set Version $version" \
      {set Version ([0-9.]+)} pkgIndex.tcl "set Version $version" \
      ]

# foreach "name value" [list Version $version Date $Date Copyright $Copyright] {
#     puts -nonewline regsubfile(Ramdebugger.texinfo,$name)=[regsubfile -line "@set $name\\s+.*$" \
#           Ramdebugger.texinfo "@set $name $value"]\n
# }

auto_mkindex scripts *.tcl

set createdistribution::libdir ""

# cannot contain file pkgIndex.tcl
CreateDistribution starkit $name . RamDebugger.tcl \
    [list license.terms Readme addons scripts Examples help] \
   addons/ramdebugger.ico $version

set createdistribution::libdir addons

lappend createdistribution::remove_packages autoscroll base64 cmdline fileutil \
    htmlparse img ncgi sha1 snit struct textutil tile vfs treectrl

CreateDistribution zip $name . RamDebugger.tcl \
    [list license.terms Readme addons scripts Examples help \
	       pkgIndex.tcl] \
    addons/ramdebugger.ico $version

DeiconifyConsole
