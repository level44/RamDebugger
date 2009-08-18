
# -*- mode: Makefile;-*-

# to compile debug version, do: make DEBUG=yes

HOST = $(shell hostname)
LINUX = $(shell if [ -e /usr/include/linux ]; then echo yes; else echo no; fi)

ifeq ($(HOST),rrg7.local)
  LIBSDIR=/Users/ramsan/gidproject/libs
else ifeq ($(USER),hoschi)
  LIBSDIR=/Users/miguel
else
  LIBSDIR=
endif

OBJDIR = $(if $(filter yes,$(DEBUG)),debug,release)

CC = gcc
CPPFLAGS=-DUSE_TCL_STUBS -DUSE_TK_STUBS -DTCLVERSION -Wunused-variable

INCLUDE_DIRECTORIES = /usr/local/ActiveTcl-8.5/include $(LIBSDIR)/include
CPPFLAGS += $(addprefix -I ,$(INCLUDE_DIRECTORIES))

ifeq ($(LINUX),no)
  CPPFLAGS += -fast
  LDFLAGS += -dynamiclib
  LIB_DIRECTORIES = $(LIBSDIR)/lib
  LIBS =
  LIBEXT = dylib
  LD = g++
else
  CPPFLAGS += -fast
  LDFLAGS += -shared -static-libgcc
  LIB_DIRECTORIES = /usr/local/ActiveTcl-8.5/lib
  LIBS =`g++ -print-file-name=libstdc++.a`
  LIBEXT = so
  LD = gcc
endif

LDFLAGS += $(addprefix -L,$(LIB_DIRECTORIES))
LIBS += -ltclstub8.5

EXE = $(OBJDIR)/RamDebuggerInstrumenter6_x32.$(LIBEXT)

EXE_INSTALL = scripts/RamDebuggerInstrumenter6_x32.$(LIBEXT)

VPATH = scripts

SRCS = RamDebuggerInstrumenter.cc

OBJS = $(addprefix $(OBJDIR)/,$(SRCS:.cc=.o))

all: compile $(EXE_INSTALL)
compile: $(OBJDIR) $(EXE)


ifeq ($(OBJDIR),debug)
CPPFLAGS += -g -D_DEBUG -DDEBUG
LDFLAGS  += -g
endif
ifeq ($(OBJDIR),release)
CPPFLAGS += -O3
LDFLAGS  += -O3
endif

$(EXE): $(OBJS)
	$(LD) $(LDFLAGS) -o $(EXE) $(OBJS) $(LIBS)

$(OBJDIR):
	mkdir $(OBJDIR)

$(OBJDIR)/%.o: %.cc
	$(COMPILE.cc) $(OUTPUT_OPTION) $<

$(EXE_INSTALL): $(EXE)
ifneq ($(OBJDIR),debug)
	cp $(EXE) $(EXE_INSTALL)
endif

clean:
	rm -f $(OBJDIR)/*.o $(EXE)

copy:
	cp $(EXE) $(EXE_INSTALL)
