
package require createdistribution
namespace import createdistribution::*

set name RamDebugger
set version 5.6

IconifiedConsole

set createdistribution::doencrypt 0
set createdistribution::add_packages [list treectrl BWidgetR tkhtml]
set createdistribution::remove_packages [list resizer trf bwidget tcllibc \
	vfs::ftp]

createdistribution::regsubfiles [list \
      {set Version ([0-9.]+)} RamDebugger.tcl "set Version $version" \
      {set Version ([0-9.]+)} pkgIndex.tcl "set Version $version" \
      ]

# foreach "name value" [list Version $version Date $Date Copyright $Copyright] {
#     puts -nonewline regsubfile(Ramdebugger.texinfo,$name)=[regsubfile -line "@set $name\\s+.*$" \
#           Ramdebugger.texinfo "@set $name $value"]\n
# }

auto_mkindex scripts *.tcl

CreateDistribution zip $name . RamDebugger.tcl \
    [list license.terms Readme addons scripts Examples help \
	       pkgIndex.tcl] \
    addons/ramdebugger.ico $version

set createdistribution::libdir ""

# cannot contain file pkgIndex.tcl
CreateDistribution starkit $name . RamDebugger.tcl \
    [list license.terms Readme addons scripts Examples help] \
    addons/ramdebugger.ico $version


DeiconifyConsole
