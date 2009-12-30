#
# Tcl Library for TkCVS
#

#
# $Id: help.tcl,v 1.1.1.1 2001-06-07 15:03:42 ramsan Exp $
#
# Help procedures and help data.
#

proc aboutbox {} {
 
  set about_string "     TkCVS Revision 6.3\n\nA"
  append about_string " Tk interface to the CVS version"
  append about_string " control system.\n\nSend"
  append about_string " bug reports, comments,"
  append about_string " and suggestions to the maintainer:"
  append about_string "\nmokuren@aracnet.com"
  append about_string "\n\nSend chocolate to the author:"
  append about_string "\ndel@babel.com.au"
 
  tk_dialog .aboutbox "About TkCVS!" \
    $about_string \
    "" \
    0 "OK"
}

######################################################################
#
# text formatting routines derived from Klondike
# Reproduced here with permission from their author.
#
# Copyright (C) 1993,1994 by John Heidemann <johnh@ficus.cs.ucla.edu>
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions
# are met:
# 1. Redistributions of source code must retain the above copyright
#    notice, this list of conditions and the following disclaimer.
# 2. Redistributions in binary form must reproduce the above copyright
#    notice, this list of conditions and the following disclaimer in the
#    documentation and/or other materials provided with the distribution.
# 3. The name of John Heidemann may not be used to endorse or promote products
#    derived from this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY JOHN HEIDEMANN ``AS IS'' AND
# ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED.  IN NO EVENT SHALL JOHN HEIDEMANN BE LIABLE
# FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
# DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
# OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
# HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
# OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
# SUCH DAMAGE.
#
######################################################################

proc put-text {tw txt} {

    $tw configure -font -*-Times-Medium-R-Normal-*-14-*

    $tw tag configure bld -font -*-Times-Bold-R-Normal-*-14-*
    $tw tag configure cmp -font -*-Courier-Bold-R-Normal-*-12-*
    $tw tag configure h1 -font -*-Helvetica-Bold-R-Normal-*-18-* -underline 1
    $tw tag configure h2 -font -*-Helvetica-Bold-R-Normal-*-18-*
    $tw tag configure h3 -font -*-Helvetica-Bold-R-Normal-*-14-*
    $tw tag configure itl -font -*-Times-Medium-I-Normal-*-14-*
    $tw tag configure rev -foreground white -background black

    $tw tag configure btn \
            -font -*-Courier-Medium-R-Normal-*-12-* \
            -foreground black -background white \
            -relief groove -borderwidth 2

    $tw mark set insert 0.0

    set t $txt

    while {[regexp -indices {<([^@>]*)>} $t match inds] == 1} {

        set start [lindex $inds 0]
        set end [lindex $inds 1]
        set keyword [string range $t $start $end]

        set oldend [$tw index end]

        $tw insert end [string range $t 0 [expr {$start - 2}]]

        purge-all-tags $tw $oldend insert

        if {[string range $keyword 0 0] == "/"} {
            set keyword [string trimleft $keyword "/"]
            if {[info exists tags($keyword)] == 0} {
                error "end tag $keyword without beginning"
            }
            $tw tag add $keyword $tags($keyword) insert
            unset tags($keyword)
        } else {
            if {[info exists tags($keyword)] == 1} {
                error "nesting of begin tag $keyword"
            }
            set tags($keyword) [$tw index insert]
        }

        set t [string range $t [expr {$end + 2}] end]
    }

    set oldend [$tw index end]
    $tw insert end $t
    purge-all-tags $tw $oldend insert
}

proc purge-all-tags {w start end} {
    foreach tag [$w tag names $start] {
        $w tag remove $tag $start $end
    }
}

######################################################################
#
# End of text formatting routines.
#
######################################################################

proc do_help {title helptext} {

  static {helpviewer 0}

  incr helpviewer
  set cvshelpview ".cvshelpview$helpviewer"
  toplevel $cvshelpview
  text $cvshelpview.text -setgrid yes -wrap word \
    -width 55 -relief sunken -border 2 \
    -yscroll "$cvshelpview.scroll set"
  scrollbar $cvshelpview.scroll -relief sunken \
    -command "$cvshelpview.text yview"
  button $cvshelpview.ok -text "OK" \
    -command "destroy $cvshelpview"

  pack $cvshelpview.ok -side bottom -fill x
  pack $cvshelpview.scroll -side right -fill y
  pack $cvshelpview.text -fill both -expand 1

  wm title $cvshelpview "$title Help"
  wm minsize $cvshelpview 1 1

  put-text $cvshelpview.text $helptext
}

proc working_with_tkcvs {} {

  do_help "Working With TkCVS" {

<h1>WORKING WITH TkCVS</h1>

To work with files from the repository using TkCVS, first you need to select a directory (sometimes called a sandbox) to work in. You can create a subdirectory from your home directory (I use develop for all of my development software, you may like to use src or progs), or you may work from your home directory.
 
After selecting this directory and navigating into it using the directory display, you can select a module to work with.  In CVS, groups of files are arranged into modules for easy access.
 
TkCVS arranges modules into directories and subdirectories in a tree structure much in the same way that you would arrange them in your home directory.  You can navigate through the TkCVS module tree using the Module Browser window.

<h2>Working with files from the repository</h2>

Using the Module Browser window, you can select a module to check out.  Each module has a module code which is displayed as you browse through the module tree.  When you check out a module, a new directory is created in your working directory with the same name as the module code.
 
You can then change into the new directory that TkCVS has created, and all of the files in that module will be in the directory.  You can use the simple file management and editing features of TkCVS to work with files in this directory, or you may use your own text editor, word processor, or whatever to work with the files.
 
Once you have finished your edits (or whenever you have reached a stable stage in your development, or possibly even on a daily basis), you can check files back in to TkCVS using the Check In button.

<h2>Tagging and Branching</h2>

You can tag particular versions of a module or file in the repository.  Normally you will do this when your programs are ready to be released into a testing or production environment.  Tagging is useful for taking snapshots of what the repository contained at a particular date (for example you can tag all of the files associated with a particular software release).

You can also create branches based on tags or on particular versions of the files in the repository. This allows you to go back and fix particular bugs or make important patches to an earlier release of software while still being able to work on the latest version.
 
You can use symbolic tag names to refer to versions of files in the repository, instead of just numeric version numbers. For example you can use the tag name software_release_1_5 to indicate release 1.5 of a group of files destined for a customer site.

<h2>Exporting</h2>

Once a software release has been tagged, you can use a special type of check out called an export. This allows you to more cleanly check out files from the repository,  without all of the administrivia that CVS requires you to have while working on the files.  This type of facility is useful for delivery of a software release to a customer.

<h2>Importing</h2>

TkCVS contains a special dialog to allow users to import new files into the repository. New modules can be assigned places within the repository, as well as descriptive names (so that other people know what they are for).

<h2>Working inside the repository</h2>

TkCVS contains some features that enable it to work inside the repository.  The module browser window displays the directories and modules within the repository in a tree structured manner.
 
The module browser can invoke a file browser window that enables you to list the files within the module, and view a specific version of a file and see its branches and tags.
 
The log browser window contains a graphical display of a revision log for a file, and enables you to see the differences between two versions of a file by clicking on the versions in the display.

<h2>Other Features</h2>

Other features of TkCVS allow you to tag files, add new files or delete files from the repository, or view and print reports about the repository contents.  You can also search the module database for module codes, module names, and keywords within a module name.

  }
}

#
# Help procedures from the TkCVS users guide.
#

proc checking_out_modules {} {

  do_help "Checking Out Modules" {

<h1>CHECKING OUT MODULES USING TkCVS</h1>

To check out a module, first display the Module Browser using the Module Browse button. Note that you do not have to check out modules using this dialog -- you can use it for just browsing through the modules in the repository.

Once you have found the module you are looking for in the module tree, you can check it out by pressing the Check Out button, or export it using the Export button (see the section on Exporting later in this manual).

  }
}

proc tagging_and_branching {} {

  do_help "Tagging and Branching" {

<h1>TAGGING AND BRANCHING</h1>

CVS uses a system of tags to indicate places in the development cycle when the repository (or parts of it) are in a stable state. These tags can be applied to modules in the repository or to files within a module.

<h2>Tagging Modules</h2>

To tag a module, first select the module using the module browser (note that you may tag a module or a directory tree -- to tag an entire directory tree including all of its sub-directories and modules, select the top level directory of the tree).

Enter the tag name into the Tag A entry box.  A tag name may be any alphanumeric string, but may not contain any spaces or dots.  It must begin with a letter instead of a number. Sample tag names include things like software-release-1-0 or integration-test-release.

If you also enter a tag name into the Tag B entry box, then all files in that module that are currently tagged with Tag B will be also tagged with Tag A.

Select the Tag->Tag Module menu item from the menu on the Module Browser dialog, and the module or directory will be tagged with the tag name that you have entered.

<h2>Tagging Files</h2>

You may tag individual files from within the directory browser.  This is mostly useful when you only want to tag part of the files in a module, or when you are re-tagging certain files within an already tagged module.

Select the files that are to be tagged, and select the Tag button or the File->Tag Files item from the menu.  This will request that you enter a tag name. The selected files will then be tagged with this tag name.

<h2>Updating to a Tag</h2>

Once a file or module has been tagged within the repository, you may use the tag name for checking out files, or updating files to a tag name.
 
If a tag name is entered in the Tag A field of the browser window when you select a module for check out, then the tagged version of the module (rather than the latest version) will be checked out of the repository.
 
<h2>Branching</h2>

CVS also supports the concept of branching files. When you are applying a tag, the tag may be applied as a branch tag, which means that all versions based on this tag will be placed on a branch of the main version tree instead of on the trunk.
 
You should update files to a branch tag before you change them if you want your changes to not affect the head version.
 
To read more about branching and branch tags, read the cvs manual page.

  }
}

proc exporting {} {

  do_help "Exporting Modules" {

<h1>EXPORTING</h1>

An important use of tagging modules is to indicate when a new release of software is ready to be delivered, and to mark the versions of all of the files in that release within the repository.
 
When a release is about to be made, you may export it from the repository.  This is like a check-out, except for the following:

*  Exported directories do not contain the CVS or administrative directories, and are therefore cleaner (but cannot be used for checking files back in to the repository).
 
*  Exported files do keyword expansion differently from non-exported  files  (to  ensure that the keyword information is not lost should the file be imported into CVS at a different site).

To export a module or directory, press the Export button at the bottom of the module browser. You must supply a tag name when you are exporting a module -- you cannot just export the default (head) version. This is to make sure that you have the tag in place so that you can reproduce the exported files at a later date. This is called good configuration management.

  }
}

proc importing_new_modules {} {

  do_help "Importing" {

<h1>IMPORTING NEW MODULES USING TkCVS</h1>

Before importing a new module, first check to make sure that you have write permission to the repository. Ask the configuration management staff at your organisation to determine whether you can write to the repository.

You should also take some time and view how the repository is structured using the Module Browser (see above). You will need to provide a directory name for your module (which should not be already in use) and so you will need to know that the parent directory for your module exists in the repository first. Although a module can be imported without the parent directory existing, this means that the CVS modules file must be patched manually after the operation is finised to insert the directory name in the correct place in the file.

To import a module, first enter the directory where the module is located. Make sure that there is nothing in this directory except the files that you want to import (create a new empty directory first and copy the files from the module into it if necessary).

Once this is done, press the Import button. You will need to enter the following details:

<h2>Module Directory</h2>

The location in the module tree where your new module will be put.  This directory must not already exist, although its parent directory must exist.

<h2>Module Description</h2>

A one-line descriptive title for your module.  This will be displayed in the right-hand column of the browser.

<h2>Version Number</h2>

The current version number of the module. This should be a number of the form X.Y.Z where .Y and .Z are optional. You can leave this blank, in which case 1 will be used as the first version number.

<h2>Module Code</h2>

A module code for the module.  This code must not already exist in the repository. I strongly suggest that your organisation settles on a single unambiguous code for modules. One possibility is something like:

<cmp>     [project code]-[subsystem code]-[module code]</cmp>

Using this scheme, you could have a 3 digit project code, a 3 letter subsystem code, and a 4 letter module code for each module, and everything would be easy to locate.

Once you have entered these details, press the OK button to import the module.

<h1>CREATING NEW DIRECTORIES USING TkCVS</h1>

TkCVS is not a file manager! You can't use it to create directories within your home directory.

You can, however, use it to create new directories within the repository if the repository is not remote.  This is best done while importing new modules. If you are about to import a new module, and the parent directory does not exist in the repository, you can create it using the New Directory button.

Press this button and a dialog will appear allowing you to create the new directory.  You must enter the following information:

<h2>New Directory Location</h2>

Where (within the module tree) the new directory will be created.  This directory should not already exist, but the parent directory must exist.

<h2>New Directory Name</h2>

This is a one-line descriptive name for the new directory.  Something like Accounting software or Test data for my files should be sufficient.

Once you have entered this information, press the OK button to create the directory.

You may use this dialog multiple times in one session to create multiple directories, or an entire tree of directories in your repository.

  }
}

proc current_directory {} {

  do_help "Current Directory" {

<h1>CURRENT DIRECTORY DISPLAY</h1>

The current directory display shows:
 
*  The name of the current directory,
 
*  The location of the current repository, and the location of the current directory in the CVS repository.  This will only be shown if the current directory is contained in the CVS repository. If it is not contained in the repository you may import it using the Import button.

*  A Directory Tag name, if the directory is contained in the CVS repository and it has been checked out against a particular CVS version tag.

*  A list of the files in the current directory.  You may select a file by clicking on it once with the left mouse button.  You may select a group of files by dragging over them with the mouse or holding the shift key while clicking. You may select a group of non-contiguous files by holding the control key while clicking.

You may move into a directory by double-clicking on it.
 
Double clicking on a file will load the file into a suitable editor so you can change it.  A different editor can be used for different file types.
 
<h2>File Status</h2>

When you are in a directory that is contained in the CVS repository, a file status will be shown next to each file.  The various statuses that can be shown are:

<h3>Up-to-date</h3>

The file is up to date with respect to the repository.

<h3>?</h3>

The file is not contained in the repository. You may need to add the file to the repository by pressing the "Add" button.

<h3>Locally Modified</h3>

The file has been modified in the current directory since being checked out of the repository.

<h3>Locally Added</h3>

The file has been added to the repository. This file will become permanent in the repository once a commit is made.

<h3>[directory:CVS]</h3>

The file is a directory which has been checked out from the CVS repository.

<h3>[directory:RCS]</h3>

The file is a directory which is maintained using RCS.

<h3>[directory:SCCS]</h3>

The file is a directory which is maintained using SCCS.

<h3>[directory]</h3>

The file is a directory.

<h2>File Filters</h2>

You can specify file matching patterns to instruct TkCVS which files you wish to see.  You can also specify patterns telling it which files to remove when you press the "Clean" button or select the File->Cleanup menu item.
  }
}

proc buttons_help {} {

  do_help "Main Window Buttons" {

<h1>BUTTONS</h1>

There are a number of buttons at the bottom of the window. Pressing on one of these causes the following actions:

<h2>Edit</h2>

Press this button to load the selected files in to an appropriate editor.

<h2>Delete</h2>

Press this button to delete the selected files. The files will not be removed from the repository. To remove the files from the repository as well as delete them, press the "Remove" button instead.

<h2>Unselect</h2>

Press this button to un-select all of the selected files.

<h2>Refresh</h2>

Press this button to re-read the current directory.  You might want to do this immediately after a check-in or update, and also if you add new files to the directory.

<h2>Log (Branch) Browse</h2>

This button will bring up the log browser window for each of the selected files in the window. This window is described later.

<h2>Tag</h2>

This button will tag the selected files.  The -F (force) option will move the tag if it already exists on the file.  The -b (branch) option will create a new branch.

<h2>Add Files</h2>

Press this button when you want to add new files to the repository.

You can do this when you have created a new file in the current directory. You must create the file before adding it to the repository.

To add some files, first select them in the list above the buttons. Then press the Add Files button.

The files that you have added to the repository will be committed next time you press the Check In button.

<h2>Remove</h2>

This button will remove files.

To remove a file, first select the file in the list above the buttons. Then press the Remove button.

The file will disappear from the directory and from the file list, and will be removed from the repository next time you press the Check In button.

<h2>Diff</h2>

This compares the selected files with the equivalent files in the repository. A separate program called "TkDiff" (also supplied with TkCVS) is used to do this.

For more information on TkDiff, see TkDiff's help menu.

<h2>Check In</h2>

This button commits your changes to the repository.  This includes adding new files and removing deleted files.

When you press this button, a dialog will appear asking you for the version number of the files you want to commit, and a comment.

You need only enter a version number if you want to bring the files in the repository up to the next major version number. For example, if a file is version 1.10, and you do not enter a version number, it will be checked in as version 1.11. If you enter the version number 3, then it will be checked in as version 3.0 instead.

<h2>Update</h2>

This updates your sandbox directory with any changes committed to the repository by other developers.

<h2>Merge Conflict</h2>

If a file's status says "Needs Merge", "Conflict", or is marked with a "C" in CVS Check, there was a difference which CVS needs help to reconcile.  This button invokes TkDiff with the -conflict option, opening a merge window to help you merge the differences.

<h2>Module Browse</h2>

Press this button to see the Module browser window.  This is described later.

<h2>Quit</h2>

Press this button to leave TkCVS.

  }
}

proc module_browser {} {

  do_help "Module Browser" {

<h1>THE MODULE BROWSER</h1>

Most of the file related actions of TkCVS are performed within the main window. The module related actions of TkCVS are performed within the module browser. The module browser is displayed using the Module Browse button from the main window.

When the dialog is displayed, it shows 2 columns. The first column is a tree showing the module codes and directory names of all of the items in the repository.  The icon shows whether the item is a directory (which may contain other directories or modules), or whether it is a module (which may be checked out from TkCVS).  It is possible for an item to be both a module and a directory.  If it has a red ball on it, you can check it out.  If it shows a plain folder icon, you have to open the folder to get to the items that you can check out.

The second column shows descriptive titles of the items in the repository.

You can exit the Module Browser by pressing the Close button.

  }
}

proc module_browser_menu {} {

  do_help "Module Browser Menu" {

<h2>Module Browser Menu</h2>

The module browser also contains a menu, with the following options:
 
<h3>Reports</h3>

This menu contains 4 options. These are Module Tree, Modules Sorted by Name, Version Tree, and Version Listing by Name

These perform the same functions as the equivalent items on the TkCVS main menu, except for the following note:

Each of these options restricts itself to the currently selected module or directory in the module browser.  The version reports do not report the head version, instead they report the version number attached to the tag name given in the Tag A field of the module browser.

The next 3 items are Search Repository by Code, Search Repository by Name, and Search Repository by Keyword.
These can help you find a module's location within the repository.

<h3>Checkouts</h3>

The items on this menu run cvs history commands.  They show which modules are checked out by whom.

<h3>Patch->Make Patch File</h3>

This item creates a Larry Wall format patch(1) file in the current directory of the module selected. A tag name must be entered into the Tag A field of the module browser. If a second tag name is entered into the Tag B field, then the patch file will contain differences between the two tagged versions of the module. Otherwise the patch file will contain differences between the head version and the tagged version of the module.

<h3>Patch->View Patch Summary</h3>

This item displays a short summary of the differences between two versions of a module. A tag name must be entered into the Tag A field of the module browser. If a second tag name is entered into the field, then the summary will list the changes between the two tagged versions of the module. Otherwise the summary will list differences between the head version and the tagged version of the module.

<h3>Tag</h3>

See the section titled Tagging and Branching for more information on the options on this menu.

  }
}

proc module_browser_buttons {} {

  do_help "Module Browser Buttons" {

<h2>Module Browser Buttons</h2>

The module browser contains the following buttons:
 
<h3>Who</h3>

Shows which modules are checked out by whom.

<h3>File Browse</h3>

Displays a list of the selected module's files.

<h3>Unselect</h3>

Press this button to un-select the selected module.

<h3>Check Out</h3>

Checks out the current version of a module. If a tag name is entered into the Tag A field, then the tagged version is checked out rather than the head version.

<h3>Export</h3>

Exports the current version of a module. If a tag name is entered into the Tag A field, then the tagged version is exported rather than the head version.

See the section titled Exporting for more information on exporting modules from the repository.

<h3>Tag</h3>

This button tags an entire module.

<h3>Branch Tag</h3>

This creates a branch of a module by giving it a branch tag.

<h3>Patch Summary</h3>

This item displays a short summary of the differences between two versions of a module. A tag name must be entered into the Tag A field of the module browser. If a second tag name is entered into the field, then the summary will list the changes between the two tagged versions of the module. Otherwise the summary will list differences between the head version and the tagged version of the module.

<h3>>Create Patch File</h3>

This item creates a Larry Wall format patch(1) file in the current directory of the module selected. A tag name must be entered into the Tag A field of the module browser. If a second tag name is entered into the Tag B field, then the patch file will contain differences between the two tagged versions of the module. Otherwise the patch file will contain differences between the head version and the tagged version of the module.

<h3>Import</h3>

This item will import the contents of the current directory (the one shown in the Current Directory Display) into the repository as a module.  See the section titled Importing for more information.

<h3>Close</h3>

This button closes the module browser.

  }
}

proc file_browser {} {

  do_help "File Browser" {

<h1>THE FILE BROWSER</h1>

Pressing the File Browse button in the module browser window displays the TkCVS file browser. This window shows a list of each file contained in the selected module.  The buttons are:

<h2>View</h2>

This displays a window showing the contents of the file. The file may be viewed but not edited using this window. (To edit a file in the repository, you must check the file out of the repository).

If a tag name or version number is entered into the Version / Tag field at the top of the window, the specified version of the file is displayed, and not the head version.

<h2>Log (Branch)</h2>

This displays the log browser window for the file.

<h2>Tags</h2>

Lists the file's tags.

<h2>Close</h2>

This button closes the file browser.

  }
}

proc log_browser {} {

  do_help "Log (Branch) Browser" {

<h1>THE LOG BROWSER</h1>

The TkCVS Log Browser window enables you to view a graphical display of the revision log of a file, including all previous versions, and any branched versions.
 
You can get to the log browser window in two ways, either by selecting a file within the main window of TkCVS and pressing the Log Browse button, or by selecting a file in the file browser window (available from within the module browser) and pressing the Log Browse button.

The two forms of the log browser are slightly different.  From within the main window it is possible to perform merge operations on a file, and the results of the merge are stored in the current directory. From within the module browser (working directly within the repository) merge operations are not possible.

<h2>Log Browser Window</h2>

The log browser window has three components. These are the file name and version information section at the top, the log display in the middle, and a row of buttons along the bottom.

<h2>Log Display</h2>

The main log display is fairly self explanatory. It shows a group of boxes connected by small lines indicating the main trunk of the file development (on the left hand side) and any branches that the file has (which spread out to the right of the main trunk).

Each box contains the version number and author of the version.

<h2>Version Numbers</h2>

Once a file is loaded into the log browser, up to two version numbers may be selected. The primary version is selected by clicking the left mouse button on a version box in the main log display.

The secondary version (Version B) is selected by clicking the right mouse button on a version box in the main log display.

Operations such as "View" and "Merge Branch to Head" always operate only on the primary version selected.

Operations such as "Diff" and "Merge Changes to Head" require two versions to be selected.

<h2>Log Browser Buttons</h2>

The log browser contains the following buttons:

<h3>View</h3>

Pressing this button displays a window containing the primary version of the file selected.

<h3>Diff</h3>

Pressing this button runs the "tkdiff" program (by John Klassa) to display the differences between version A and version B.

<h3>Merge Branch to Head</h3>

To use this button, first select a branch version of the file as the primary version.

The changes made along the branch up to that version will then be merged into the head version, and stored in the current directory. The version of the file in the current directory will be over-written.

<h3>Merge Changes to Head</h3>

To use this button, first select two versions anywhere in the file log (although two adjacent branch versions are more commonly selected). It is expected that in most cases version B will be later than version A.

CVS will then calculate the changes made between the two versions selected and merge the differences to the head version.

Note that if version A is later (higher on the trunk or further along the branch) than version B, a "reverse diff" will be created. This will have the effect of removing the changes from the head version.

For example, if the head version is 1.6, version A is 1.5, and version B is 1.3, this button will remove all of the changes made between versions 1.3 and 1.5 from the head version.

It may not make sense to select two branch versions on different branches for this function.

<h3>View Tags</h3>

This button displays a window showing the tags contained in the file.

<h3>Close</h3>

This button closes the Log Browser.

  }
}

proc configuration_files {} {

  do_help "Configuration Files" {

<h1>CONFIGURATION FILES</h1>

There are two configuration files for TkCVS. The first is stored in the directory in which the *.tcl files for TkCVS are installed. This is called tkcvs_def.tcl.  You can put a file called site_def in that directory, too.  That's a good place for site-specific things like tagcolours.  Unlike tkcvs_def.tcl, it will not be overwritten when you install a newer version of TkCVS.

Values in the site configuration files can be over-ridden at the user level by placing a .tkcvs file in your home directory. Commands in either of these files should be Tcl scripts. In other words, to set a variable name, you should have the following command in your .tkcvs file:

    set variable-name "value"

for example:
    set cvscfg(papersize) "A4"

The following variables are supported by TkCVS:

<h2>Startup</h2>
<h3>cvscfg(y_size)</h3> Number of files that the main working listbox should accomodate
<h3>cvscfg(startwindow)</h3> Which window you want to see on startup. (workdir or module)

<h2>GUI</h2>
<h3>cvscfg(guifont)</h3> Font for menus, buttons, and labels
<h3>cvscfg(listboxfont)</h3> Font for text in list boxes
<h3>cvscfg(glb_background)</h3> Background colour to use for all items in TkCVS
<h3>cvscfg(glb_highlight)</h3> Background colour to use for highlighted items
<h3>cvscfg(bt_lookhere)</h3> Background colour for buttons and scrollbars
<h3>cvscfg(bt_pressme)</h3> Background colour for active buttons and scrollbars
<h3>cvscfg(tool_colour)</h3> Colour of the ToolTips popup labels
<h3>cvscfg(bitmapdir)</h3> Where to look for the icon bitmaps

<h2>Log browser</h2>
<h3>cvscfg(tagcolour,tagstring)</h3> Colors for marking tags. For example "set cvscfg(tagcolour,tkcvs_r6) Purple"

<h2>User preferences</h2>
<h3>cvscfg(allfiles)</h3> Set this to 0 to see normal files only in the directory browser, set it to 1 to see all files including hidden files.
<h3>cvscfg(auto_status)</h3> Set the default for automatic statusing of a CVS controlled directory. Automatic updates are done when a directory is entered and after some operations.
<h3>cvscfg(checkrecursive)</h3> Report Check recursively (onvalue {}, offvalue -l)
<h3>cvscfg(confirm_prompt)</h3> Ask for confirmation before performing an operation(true or false)
<h3>cvscfg(editor)</h3> Preferred default editor 
<h3>cvscfg(editors)</h3> String pairs giving the editor-command and string-match-pattern, for deciding which editor to use
<h3>cvscfg(format_cmd)</h3> Command to be used to format selected files, ie. indent or aimap
<h3>cvscfg(ldetail)</h3> Detail level for status reports (summary, verbose)
<h3>cvscfg(rdetail)</h3> Detail for repostitory and workdir reports (terse, summary, verbose)
<h3>cvscfg(recurse)</h3> Whether reports are recursive (true or false)

<h2>File filters</h2>
<h3>cvscfg(file_filter)</h3> Pattern for which files to list. Empty string is equilivant to the entire directory (minus hidden files)
<h3>cvscfg(ignore_file_filter)</h3> Pattern used in the workdir listbox filter for files to be ignored
<h3>cvscfg(clean_these)</h3> Pattern to be used for cleaning a directory (removing unwanted files)

<h2>System</h2>
<h3>cvscfg(print_cmd)</h3> System command used for printing. lpr, enscript -Ghr, etc)
<h3>cvscfg(shell)</h3> What you want to happen when you ask for a shell

<h2>Portability</h2>
<h3>cvscfg(aster)</h3> File mask for all files (* for unix, *.* for windows)
<h3>cvscfg(null)</h3> The null device. /dev/null for unix, nul for windows
<h3>cvscfg(tkdiff)</h3> How to start tkdiff. Example sh /usr/local/bin/tkdiff
<h3>cvscfg(tmpdir)</h3> Directory in which to do behind-the-scenes checkouts. Usually /tmp or /var/tmp)
<h3>cvscfg(thisdir)</h3> CVS-DOS doesn't like a dot in some execs. ("." for unix, "" for windows)

<h2>Printed reports</h2>
<h3>cvscfg(papersize)</h3> Paper size for PostScript reports. A4 for international, 8x11 for US)
<h3>cvscfg(xstart)</h3> Printed area top for PostScript reports
<h3>cvscfg(xend)</h3> Printed area bottom for PostScript reports
<h3>cvscfg(ystart)</h3> Printed area left for PostScript reports
<h3>cvscfg(yend)</h3> Printed area right for PostScript reports
<h3>cvscfg(pointsize)</h3> Point size for body of PostScript reports
<h3>cvscfg(headingsize)</h3> Point size to use for heading text in PostScript reports
<h3>cvscfg(subheadingsize)</h3> Point size for subheadings in PostScript reports

<h2>Debugging</h2>
<h3>cvscfg(log_classes)</h3> For debugging: C=CVS commands, F=File creation/deletion, T=Function entry/exit tracing, D=Debugging
<h3>cvscfg(logging)</h3> Logging (debugging) on or off

  }
}

proc environment_variables {} {

  do_help "Environment Variables" {

<h1>ENVIRONMENT VARIABLES</h1>

You should have the CVSROOT environment variable pointing to the location of your CVS repository before you run TkCVS.  It will still allow you to work with different repositories within the same session.

  }
}

proc user_defined_menu {} {

  do_help "User Defined Menu" {

<h1>USER CONFIGURABLE MENU EXTENSIONS</h1>

It is possible to extend the TkCVS menu by inserting additional commands into the .tkcvs or tkcvs_def.tcl files.  These extensions appear on an extra menu to the right of the TkCVS Options menu.

To create new menu entries on the user-defined menu, set the following variables:

<h2>cvsmenu(command)</h2>

Setting a variable with this name to a value like "commandname" causes the CVS command "cvs commandname" to be run when this menu option is selected. For example, the following line:

set cvsmenu(update_A) "update -A"

Causes a new menu option titled "update_A" to be added to the user defined menu that will run the command "cvs update -A" on the selected files when it is activated.

(This example command, for versions of CVS later than 1.3, will force an update to the head version of a file, ignoring any sticky tags or versions attached to the file).

<h2>usermenu(command)</h2>

Setting a variable with this name to a value like "commandname" causes the command "commandname" to be run when this menu option is selected. For example, the following line:

set cvsmenu(view) "cat"

Causes a new menu option titled "update_A" to be added to the User defined menu that will run the command "cat" on the selected files when it is activated.

Any user-defined commands will be passed a list of file names corresponding to the files selected on the directory listing on the main menu as arguments.

The output of the user defined commands will be displayed in a window when the command is finished.

  }
}

proc cvs_modules_file {} {

  do_help "CVS modules File" {

<h1>CVS modules FILE</h1>

CVS maintains a file called modules in the $CVSROOT/CVSROOT (or $CVSROOT/CVSROOT.adm for versions prior to 1.3) directory in which it stores the location of all of the modules in the CVS repository. TkCVS maintains a set of backwards - compatible extensions to this file to store the names of directories and modules.

The extensions are:

<h2>Directory Name</h2>

#D [TAB] directory [TAB] directory name.

<h2>Module Name</h2>

#M [TAB] module code [TAB] module name

Where module code is the same as the module code that CVS uses to refer to the module in the modules file.

The #M line must be after the #D line for the directory in which the module is contained, but must be before the line containing the module code line used by CVS.

eg:
#D      softproj        Software Development Projects
#D      softproj/choc   Chocolate Detection Project
#M      sniffer Chocolate Sniffer
sniffer softproj/choc/sniffer
#M      snuff   Chocolate Snuffler
snuff   softproj/choc/snuffler
#M      biter   Chocolate Biter
biter   softproj/choc/biter


When you are installing TkCVS, you may like to add these additional lines to the modules file (remember to check out the modules module from the repository, and then commit it again when you have finished the edits).

These extension lines commence with a "#" character, so CVS interprets them as comments. Only TkCVS uses them for naming modules. They can be safely left in the file whether you are using TkCVS or not (in fact they are a useful reference when browsing your modules file).

  }
}

proc null_help_procedure {} {

  do_help "" {
  }
}
