#-----------------------------------------------------------------
#
# Makefile para C++
#
# All Right Reserved, Copyright RAMSAN
#  from  RAMSAN&RAMSAN
#                                                 Aug 2002
#-----------------------------------------------------------------


TEXI2HTML = /home/ramsan/bin/info/html-version/texi2html -split_node -menu

help/RamDebugger_toc.html : RamDebugger.texinfo
	(cd help/01RamDebugger ; $(TEXI2HTML) ../../RamDebugger.texinfo)
