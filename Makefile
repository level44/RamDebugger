
# -*- mode: Makefile;-*-

# to compile debug version, do: make DEBUG=yes
# to compile 64 bits version, do: make M64=yes

HOST = $(shell hostname)
LINUX = $(shell if [ -e /usr/include/linux ]; then echo yes; else echo no; fi)

ifeq ($(HOST),rrg7.local)
  LIBSDIR=/Users/ramsan/gidproject/libs
else ifeq ($(HOST),hoschi)
  LIBSDIR=/Users/miguel
else ifeq ($(HOST),akenatonix)
  LIBSDIR=/opt/ActiveTcl-8.5
  M64 = yes
else ifeq ($(HOST),akenatonviii)
  LIBSDIR=/opt/ActiveTcl-8.5
else
  LIBSDIR=
endif

OBJDIR = $(if $(filter yes,$(DEBUG)),debug,release)

CC = gcc
CPPFLAGS=-DUSE_TCL_STUBS -DUSE_TK_STUBS -DTCLVERSION -Wunused-variable

INCLUDE_DIRECTORIES = /usr/local/ActiveTcl-8.5/include /opt/ActiveTcl-8.5/include \
 /usr/include/tcl8.5 $(LIBSDIR)/include
CPPFLAGS += $(addprefix -I ,$(INCLUDE_DIRECTORIES))

ifeq ($(M64),yes)
  CFLAGS += -m64 -march=nocona -fPIC
  CPPFLAGS += -m64 -march=nocona -fPIC
  LDFLAGS  += -m64 -march=nocona -shared -fPIC -static-libgcc
  LIBS += 
  LIBEXT = _x64.so
  LD = g++
else
  ifeq ($(LINUX),no)
    CPPFLAGS += -fast -arch i386
    LDFLAGS += -dynamiclib -arch i386 -shared -fPIC
    LIBS =
    LIBEXT = _x32.dylib
    LD = g++
  else
    CFLAGS += -m32 -mtune=prescott -march=pentium4
    CPPFLAGS += -m32 -mtune=prescott -march=pentium4
    LDFLAGS  += -m32 -mtune=prescott -march=pentium4 -shared -static-libgcc
    LIBS += `g++ -m32 -print-file-name=libstdc++.a`
    LIBEXT = _x32.so
    LD = gcc
  endif
endif

LIB_DIRECTORIES = $(LIBSDIR)/lib
LDFLAGS += $(addprefix -L,$(LIB_DIRECTORIES))
LIBS += -ltclstub8.5

EXE = $(OBJDIR)/RamDebuggerInstrumenter6$(LIBEXT)

EXE_INSTALL = scripts/RamDebuggerInstrumenter6$(LIBEXT)

VPATH = scripts

SRCS = RamDebuggerInstrumenter.cc

OBJS = $(addprefix $(OBJDIR)/,$(SRCS:.cc=.o))

all: compile $(EXE_INSTALL)
compile: $(OBJDIR) $(EXE)

-include $(addprefix $(OBJDIR)/,$(SRCS:.cc=.o.depend))

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
	$(CC) -MM $(CPPFLAGS) -MQ $@ $< > $@.depend
	$(CC) -c $(CPPFLAGS) $< -o $@

$(EXE_INSTALL): $(EXE)
ifneq ($(OBJDIR),debug)
	cp $(EXE) $(EXE_INSTALL)
endif

clean:
	rm -f $(OBJDIR)/*.o $(OBJDIR)/*.o.depend $(EXE)

copy:
	cp $(EXE) $(EXE_INSTALL)
