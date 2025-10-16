#-------------------------------------------------------------------------------
# SVN version info:
# $Id: Makefile 19458 2025-10-16 20:03:43Z sbu062 $
#-------------------------------------------------------------------------------
# Build Modelling tools as a static and shared libraries, produce doc file (pdf)
# Note that linking the model code with shared libraries does not look like a
#      good idea as they will have some runtime loading-unloading overhead.
#      Also, building shared dll's might be problematic with Intel Fortran on
#      Windows.
# Requires the following command line utilities
#      (may NOT be available on Windows): echo, make, uname, zip, svn
#      tools use also expr, grep
#
# Notes on Oracle/Sun Fortran f95
#     If Oracle Developer Studio is installed (on modern Linux) along with
#     gfortran, f95 may conflict between the two compilers. Then, set specific
#     PATH to basic bin's and the Oracle Developer Studio bin, e.g.
#       export PATH=$HOME/bin/developerstudio12.6/bin:/usr/bin:/bin
#       make FC=f95
#-------------------------------------------------------------------------------

# Supported Fortran compiler types
GF_FC = gfortran
IF_FC = ifort
XF_FC = ifx
SF_FC = f95

# Choose the compiler type
FC = $(GF_FC)

#-------------------------------------------------------------------------------
# Binary tools, basenames without .f90, their executables will have .exe suffix.
TOOLS_LIST := htintrpl htscatter hthist

# Path to binary tools, normally subdirectory
# Graphic tools go in two versions:
#  - based on PGPLOT (legacy) in `gtools.pgplot` (supports Unix/Linux
#    and Windows via GrWin library);
#  - based on DISLIN (newer) in `gtools.dislin` (supports Unix/Linux
#    and native Windows).
# Refer to the specific Makefile in each of these sub-directories.
TOOLS_PATH = gtools.dislin/

# Path to tests
TESTS_PATH = tests/

#===============================================================================
# Accessory settings for the build environment

# The native file delete command on the Windows platform is 'del' or 'erase'.
# However, if Cygwin is used, this native file removal command may not call
# with a "command not found" error. In such a case, use the Unix 'rm' tool
# provided by Cygwin.
#WINRM = del /Q /F
WINRM := rm -fr

#===============================================================================
# Determine what is the build platform, Windows / non-Windows
# Use uname -- but it may not be installed on Windows. Probably the method
# based on ComSpec is safer. Note that the -c switch on grep suppresses normal
# output and just prints count.
# We use PLATFORM only for outputting the OS in zipfile and other things that
# are unimportant if uname is absent on the system (Windows), for real
# OS/platform check better use a safer mechanism based on ComSpec (below).
# PLATFORM is still used for zip file name generation on Unix.
#   PLATFORM = $(shell uname)
#   IS_WINDOWS = $(shell uname | grep -ci windows)
# The hardware type is determined by uname on Unix and from standard
# environment variable %PROCESSOR_ARCHITECTURE% on Windows.
# Ideally should check WOW64 compatibility layer too, but on amd64 we won't
# like to build 32 bit executable.
#     IF PROCESSOR_ARCHITECTURE == amd64 OR
#        PROCESSOR_ARCHITEW6432 == amd64 THEN
#        // Windows is 64bit
#     ELSE
#        // Windows is 32bit
#     END IF
# See https://blogs.msdn.microsoft.com/david.wang/2006/03/27/howto-detect-process-bitness/
#
# A safer way to check platform not dependent on 'uname' is ComSpec on Windows.
# Note that ComSpec is (may be?) case-sensitive, check with env.exe;
#      also we set the platform-specific null device NULLDEV for redirecting
#      unneeded outputs (e.g. REQUIRED_EXECS below generates huge full PATH
#      on Windows).
# Quirk: - Microsoft Windows echo does not require escaping quotes parentheses
#          and other symbols and outputrs them as is into redirected shell
#          output, e.g. in  $(shell echo "bla bla bla" >> file.txt), i.e
#          file.txt gets "bla bla bla". Linux echo, in contrast deletes the
#          quites, which can create portability problems if a piece of code
#          (e.g. include file) is auto-generated in such a way.
#          One way to ensure identical behaviour on all platforms is to use
#          a third-party echo.exe program from Cygwin, GnuWin32 of Linux
#          Subsystem on Windows 10 (WSL).
#        - Windows 10 does not seem to use third-party `echo.exe` (e.g. from
#          GNUwin32 or Linux subsystem if simple echo is referred under shell.
#          This requires redefining ECHO to refer to a specific `exe`
#          explicitly.
ifdef ComSpec
	PLATFORM_TYPE=Windows
	IS_WINDOWS=1
	WHICH_CMD=where
	NULLDEV=":NULL"
	RM := $(WINRM)
	MV := move
	#CP := copy
	CP := cp
	MKDIR := mkdir
	RMDIR := rmdir /s /q
	#ECHO := "$(firstword $(shell $(WHICH_CMD) echo))"
	ECHO := echo
	UNAME := ver
	ASCDOC := a2x
	PLATFORM = windows_$(PROCESSOR_ARCHITECTURE)
else
	PLATFORM_TYPE=Unix
	IS_WINDOWS=0
	WHICH_CMD=which
	NULLDEV="/dev/null"
	RM := rm -f
	MV := mv -f
	CP := cp
	MKDIR := mkdir
	RMDIR := rmdir
	ECHO := echo
	UNAME := uname
	ASCDOC := a2x
	PLATFORM = $(shell $(UNAME) -s)_$(shell $(UNAME) -p)
endif

# Check if we build on Windows platform with Intel Compiler. It is specific in
# two ways: (1) build options are different, start with slash and often have Q
# and (2) .obj is used as the extension for object files rather than .o
# here we check if with AND condition. Do this by simply concatenating two
# things Windowsifort.
ifeq ($(PLATFORM_TYPE)$(FC),Windowsifort)
	OBJEXT=obj
	LIBEXT=lib
	DIBEXT=dll
	CCMD=/c
else
	OBJEXT=o
	LIBEXT=a
	DIBEXT=so
	CCMD=-c
endif

ifeq ($(PLATFORM_TYPE)$(FC),Windowsifx)
	OBJEXT=obj
	LIBEXT=lib
	DIBEXT=dll
	CCMD=/c
endif

# Check if certain required executables exist and are callable in path. This is
# important on the Windows platform because such GNU command line utilities as
# uname and zip are not installed by default.
REQUIRED_EXECS = svn $(UNAME) zip $(ASCDOC) pdftk ifx ifort f95 gfortran
K := $(foreach exec,$(REQUIRED_EXECS),\
	$(if $(shell $(WHICH_CMD) $(exec) ),check executables,\
	$(warning ************ $(exec) unavailable in PATH ************)))

#===============================================================================
# Main building blocks, define source and object files to include.
# Warning: BASE_STRINGS must go before BASE_CSV_IO because BASE_CSV_IO now
#          depends on procedures from BASE_STRINGS.

SRC = BASE_UTILS.f90 BASE_STRINGS.f90 BASE_CSV_IO.f90 BASE_LOGGER.f90  \
      BASE_RANDOM.f90

OBJ = $(addsuffix .$(OBJEXT), $(basename $(SRC)))

MOD = base_utils.mod  base_strings.mod csv_io.mod  logger.mod base_random.mod

DOC = HEDTOOLS.adoc

LIBNAME = hedtools
LIB = $(LIBNAME).$(LIBEXT)
DIB = $(LIBNAME).$(DIBEXT)

#  Header file setting compiler/platform specific code for PRNG module
AUTOGEN_HEADER_RAND = BASE_RANDOM.inc

#-------------------------------------------------------------------------------
# We normally do not include _TRAPS and _CHECKS into the build command (_FFLAGS)
# because they may have overhead, and the library tool routines are supposed to
# be well tested.

# Options for GNU Fortran
GF_STATIC = -static-libgfortran -static -static-libgcc
GF_TRAPS = -ffpe-trap=
GF_RCHECKS = -Wall -fbounds-check
GF_FFLAGS = $(GF_STATIC) -O3 -funroll-loops -fPIC
GF_STLIBBLD = ar rc $(LIB) $(OBJ)
GF_DYLIBBLD = $(FC) $(GF_TRAPS) -O3 -fPIC -shared -o $(DIB)
#-ffpe-trap=zero,invalid,overflow,underflow

# Options for Intel Fortran
IF_STATIC = -static
IF_TRAPS =-fpe3
IF_RCHECKS = -warn -check bounds,pointers,format,uninit
IF_FFLAGS = -sox -O3 -parallel -fpic $(IF_STATIC)
IF_STLIBBLD = ar cr $(LIB) $(OBJ)
IF_DYLIBBLD = $(FC) -sox -O3 -parallel -fpic $(IF_TRAPS) -shared -o $(DIB)
# -fpe3 no traps; -fpe0 all traps

# Options for Intel Fortran on Windows, they are different from Unix
# /Qsox produces extra warnings  warning LNK4229: invalid directive '/comment
# /fast breaks linking under Windows
IF_STATIC_WINDOWS = /static
IF_TRAPS_WINDOWS =/fpe:3
IF_RCHECKS_WINDOWS = /warn /check bounds,pointers,format,uninit
IF_FFLAGS_WINDOWS = /c /O3 /Qparallel $(IF_STATIC_WINDOWS)
IF_STLIBBLD_WINDOWS = lib /out:$(LIB) $(OBJ)
IF_DYLIBBLD_WINDOWS = $(FC) /dll $(IF_FFLAGS_WINDOWS)

# Options for new Intel Fortran ifx
XF_STATIC = -static
XF_TRAPS =-fpe3
XF_RCHECKS = -warn -check bounds,pointers,format,uninit
XF_FFLAGS = -O3 -fp-model fast -xHost
XF_STLIBBLD = ar cr $(LIB) $(OBJ)
XF_DYLIBBLD = $(FC) -O3 -parallel -fpic $(XF_TRAPS) -shared -o $(DIB)

# Options for new Intel Fortran on Wondows ifx
XF_STATIC_WINDOWS = /static
XF_TRAPS_WINDOWS =/fpe:3
XF_RCHECKS_WINDOWS = /warn /check bounds,pointers,format,uninit
XF_FFLAGS_WINDOWS = /Ot /fp:fast /QxHost
XF_STLIBBLD_WINDOWS = lib /out:$(LIB) $(OBJ)
XF_DYLIBBLD_WINDOWS = $(FC) /dll $(XF_FFLAGS_WINDOWS)

# Options for Sun/Oracle Solaris Studio
SF_STATIC = –Bstatic –dn
SF_TRAPS = -ftrap=%none
SF_RCHECKS = –C
SF_FFLAGS = -fast -autopar -xcache=generic -depend=yes -pic $(SF_STATIC) $(SF_TRAPS) $(SF_RCHECKS)
SF_STLIBBLD = ar cr $(LIB) $(OBJ)
SF_DYLIBBLD = $(FC) -fast -autopar -depend=yes -pic $(SF_TRAPS) -G -o $(DIB)
# -fast = O5
# -ftrap=common is a macro for -ftrap=invalid,overflow,division.
# -ftrap=%all, %none, common

#-------------------------------------------------------------------------------
# Set other build options depending on the specific compiler

ifeq ($(FC),$(GF_FC))
	FFLAGS = $(GF_FFLAGS)
	STLIBBLD =  $(GF_STLIBBLD)
	DYLIBBLD =  $(GF_DYLIBBLD)
	FCVER = $(shell $(FC) --version)
endif

ifeq ($(FC),$(IF_FC))
	FFLAGS = $(IF_FFLAGS)
	STLIBBLD = $(IF_STLIBBLD)
	DYLIBBLD = $(IF_DYLIBBLD)
	FCVER = $(shell $(FC) --version)
endif

ifeq ($(PLATFORM_TYPE)$(FC),Windowsifort)
	FFLAGS = $(IF_FFLAGS_WINDOWS)
	STLIBBLD = $(IF_STLIBBLD_WINDOWS)
	DYLIBBLD = $(IF_DYLIBBLD_WINDOWS)
	FCVER = $(shell $(FC) 2>&1)
endif

ifeq ($(FC),$(XF_FC))
	FFLAGS = $(XF_FFLAGS)
	STLIBBLD = $(XF_STLIBBLD)
	DYLIBBLD = $(XF_DYLIBBLD)
	FCVER = $(shell $(FC) --version)
endif

ifeq ($(PLATFORM_TYPE)$(FC),Windowsifx)
	FFLAGS = $(XF_FFLAGS_WINDOWS)
	STLIBBLD = $(XF_STLIBBLD_WINDOWS)
	DYLIBBLD = $(XF_DYLIBBLD_WINDOWS)
	FCVER = $(shell $(FC) --version)
endif

ifeq ($(FC),$(SF_FC))
	FFLAGS = $(SF_FFLAGS)
	STLIBBLD = $(SF_STLIBBLD)
	DYLIBBLD = $(SF_DYLIBBLD)
	FCVER = $(shell $(FC) -V 2>&1)
endif

# DEBUG turns off all optimisations and keeps debug symbols.
ifdef DEBUG
	GF_FFLAGS = -O0 -g -ffpe-trap=zero,invalid,overflow,underflow $(GF_RCHECKS)
	IF_FFLAGS = -O0 -g -debug all -fpe0 -traceback $(IF_RCHECKS)
	IF_FFLAGS_WINDOWS = /c /Zi /Od /debug:full /fpe:0 /traceback
	#$(IF_RCHECKS_WINDOWS)
  XF_FFLAGS = -debug all -O0 -g -fstack-protector -traceback ${XF_RCHECKS}
  XF_FFLAGS_WINDOWS = /Od /debug:full /traceback ${XF_RCHECKS_WINDOWS}
	SF_FFLAGS = -O0 -g -ftrap=%all $(SF_RCHECKS)
endif

#-------------------------------------------------------------------------------
# Documentation builder parameters (asciidoc)

DOCFIL = HEDTOOLS
DOCFMT = pdf
DOCDIR = doc/

# Determine current SVN version of the code
SVN_VER = $(shell svn info --show-item last-changed-revision)
#-------------------------------------------------------------------------------

# Determine this makefile's path. Be sure to place this BEFORE `include`s
THIS_FILE := $(lastword $(MAKEFILE_LIST))

# This is the search paths for looking for components, separated by blanks
VPATH = $(DOCDIR)

#-------------------------------------------------------------------------------
# Zipfile distro to be generated
ZIPFILE = LIBHEDTOOLS_$(PLATFORM)_$(FC)_rev$(SVN_VER).zip
DDISTRO = libhedtools_$(PLATFORM)_$(FC)_rev$(SVN_VER)

# Autogenerated README.txt for static library ZIP AUTOGEN_README
AUTOGEN_README_FILE = Readme.txt
define AUTOGEN_README_LIB
	$(shell $(ECHO) "HEDTOOLS: SVN revision: $(SVN_VER)" > $(AUTOGEN_README_FILE))
	$(shell $(ECHO) "" >> $(AUTOGEN_README_FILE))
	$(shell $(ECHO) "This is a static library binary build of the modelling tools." >> $(AUTOGEN_README_FILE))
	$(shell $(ECHO) "Note that .mod files are needed for compiling." >> $(AUTOGEN_README_FILE))
	$(shell $(ECHO) "Compile command using the library:" >> $(AUTOGEN_README_FILE))
	$(shell $(ECHO) "" >> $(AUTOGEN_README_FILE))
	$(shell $(ECHO) "    $(FC) file.f90 $(LIB)" >> $(AUTOGEN_README_FILE))
	$(shell $(ECHO) "" >> $(AUTOGEN_README_FILE))
	$(shell $(ECHO) "Fortran compiler information" >> $(AUTOGEN_README_FILE))
	$(shell $(ECHO) "" >> $(AUTOGEN_README_FILE))
	$(shell $(ECHO) "$(FCVER)" >> $(AUTOGEN_README_FILE))
	$(shell $(ECHO) "" >> $(AUTOGEN_README_FILE))
	$(shell $(ECHO) "IMPORTANT: Use exactly the same platform and compiler that" >> $(AUTOGEN_README_FILE))
	$(shell $(ECHO) "           was used to generate the static library!" >> $(AUTOGEN_README_FILE))
endef

# Autogenerated README.txt for dynamic library. Note that I had difficulties
#  building and using shared libraries on cerrain platforms, e.g building on
#  ifort for Windows is broken
AUTOGEN_README_FILE = Readme.txt
define AUTOGEN_README_DIB
	$(shell $(ECHO) "HEDTOOLS: SVN revision: $(SVN_VER)" > $(AUTOGEN_README_FILE))
	$(shell $(ECHO) "" >> $(AUTOGEN_README_FILE))
	$(shell $(ECHO) "This is the shared library binary build of the modelling tools" >> $(AUTOGEN_README_FILE))
	$(shell $(ECHO) "Note that .mod files are needed for compiling." >> $(AUTOGEN_README_FILE))
	$(shell $(ECHO) "Compile command:" >> $(AUTOGEN_README_FILE))
	$(shell $(ECHO) "    $(FC) $(DIB) file.f90" >> $(AUTOGEN_README_FILE))
	$(shell $(ECHO) "   ifort Windows produces both .dll and .lib files" >> $(AUTOGEN_README_FILE))
	$(shell $(ECHO) "   .lib file should be used for building: " >> $(AUTOGEN_README_FILE))
	$(shell $(ECHO) "    $(FC) file.f90 $(LIB)" >> $(AUTOGEN_README_FILE))
	$(shell $(ECHO) "" >> $(AUTOGEN_README_FILE))
	$(shell $(ECHO) "Fortran compiler information" >> $(AUTOGEN_README_FILE))
	$(shell $(ECHO) "" >> $(AUTOGEN_README_FILE))
	$(shell $(ECHO) "$(FCVER)" >> $(AUTOGEN_README_FILE))
endef


#-------------------------------------------------------------------------------
# Autogenerated include file setting compiler/platform specific code for PRNG
# The module BASE_RANDOM includes some non-portable header code that
# depends on the conmpiler type. It is autogenerated here. The Fortran Module
# code contains INCLUDE statement. The appropriate include file is then
# autogenerated during the build process.
# The strategy of the build is two-step:
#    (1) Provide comment macro for the include file as well as the code adapted
#        for the specific compiler type and platform
#    (2) Select macro from the above list specific for the
#        compiler type
#    (3) build the BASE_RANDOM.o, the first statement is the include
#        autogeneration macro, then follow the compile instructions.

# Autogenerated include files
define AUTOGEN_COMMENT_RANDOM
	$(shell $(ECHO) "! ---------------------------------------------------------" > $(AUTOGEN_HEADER_RAND))
	$(shell $(ECHO) "! WARNING: auto-generated file, do NOT edit" >> $(AUTOGEN_HEADER_RAND))
	$(shell $(ECHO) "! Sets compiler-specific code for random number generator" >> $(AUTOGEN_HEADER_RAND))
	$(shell $(ECHO) "! ---------------------------------------------------------" >> $(AUTOGEN_HEADER_RAND))
endef

# Include file code for GNU
define AUTOGEN_CODE_GF
	$(shell $(ECHO) "! GNU Fortran compiler:" >> $(AUTOGEN_HEADER_RAND))
	$(shell $(ECHO) "use ISO_FORTRAN_ENV, only: int64">> $(AUTOGEN_HEADER_RAND))
	$(shell $(ECHO) "implicit none">> $(AUTOGEN_HEADER_RAND))
	$(shell $(ECHO) "integer, allocatable :: seed(:)">> $(AUTOGEN_HEADER_RAND))
	$(shell $(ECHO) "integer :: i, n, un, istat, dt(8), pid">> $(AUTOGEN_HEADER_RAND))
	$(shell $(ECHO) "integer(int64) :: t">> $(AUTOGEN_HEADER_RAND))
endef

# Include file code for Intel
define AUTOGEN_CODE_IF
	$(shell $(ECHO) "! Intel Fortran compiler:" >> $(AUTOGEN_HEADER_RAND))
	$(shell $(ECHO) "use ISO_FORTRAN_ENV, only: int64 " >> $(AUTOGEN_HEADER_RAND))
	$(shell $(ECHO) "use IFPORT, only : getpid" >> $(AUTOGEN_HEADER_RAND))
	$(shell $(ECHO) "implicit none" >> $(AUTOGEN_HEADER_RAND))
	$(shell $(ECHO) "integer, allocatable :: seed(:)" >> $(AUTOGEN_HEADER_RAND))
	$(shell $(ECHO) "integer :: i, n, un, istat, dt(8), pid" >> $(AUTOGEN_HEADER_RAND))
	$(shell $(ECHO) "integer(int64) :: t" >> $(AUTOGEN_HEADER_RAND))
endef

# Include file code for Oracle
define AUTOGEN_CODE_SF
	$(shell $(ECHO) "! Intel Oracle compiler:" >> $(AUTOGEN_HEADER_RAND))
	$(shell $(ECHO) "implicit none" >> $(AUTOGEN_HEADER_RAND))
	$(shell $(ECHO) "integer, allocatable :: seed(:)" >> $(AUTOGEN_HEADER_RAND))
	$(shell $(ECHO) "integer :: i, n, un, istat, dt(8), pid" >> $(AUTOGEN_HEADER_RAND))
	$(shell $(ECHO) "integer, parameter :: int64 = selected_int_kind(18)" >> $(AUTOGEN_HEADER_RAND))
	$(shell $(ECHO) "integer(int64) :: t" >> $(AUTOGEN_HEADER_RAND))
	$(shell $(ECHO) "include \"system.inc\"" >> $(AUTOGEN_HEADER_RAND))
endef

# Select autogeneration of include file for the specific compiler type
ifeq ($(FC),$(GF_FC))
	AUTOGEN_CODE_RANDOM=$(AUTOGEN_CODE_GF)
endif

ifeq ($(FC),$(IF_FC))
	AUTOGEN_CODE_RANDOM=$(AUTOGEN_CODE_IF)
endif

ifeq ($(FC),$(XF_FC))
	AUTOGEN_CODE_RANDOM=$(AUTOGEN_CODE_IF)
endif

ifeq ($(FC),$(SF_FC))
	AUTOGEN_CODE_RANDOM=$(AUTOGEN_CODE_SF)
endif

#===============================================================================

all: static

lib: $(LIB)

# Make the binary and plotting tools in the tools directory.
.PHONY: tools

tools:
ifeq ($(PLATFORM_TYPE),Windows)
	for %%t in ($(basename $(TOOLS_LIST))) do $(MAKE) -C $(TOOLS_PATH) SRC=%%t.f90 OUT=%%t.exe
else
	for tool in $(basename $(TOOLS_LIST)); do \
		$(MAKE) -C $(TOOLS_PATH) SRC=$$tool.f90 OUT=$$tool.exe; \
	done
endif

.PHONY: tests
tests:
	$(MAKE) DEBUG=1 -C $(TESTS_PATH) tests

# Specific compiler targets: intel, GNU, Oracle/Sun

.PHONY: intel
intel:
	@$(MAKE) -f $(THIS_FILE) FC=ifort

.PHONY: gnu
gnu:
	@$(MAKE) -f $(THIS_FILE) FC=gfortran

.PHONY: sun
sun:
	@$(MAKE) -f $(THIS_FILE) FC=f95

# Make static library
static: $(LIB)

# Dynamic linking, this might not work well on all platforms / compilers
shared: $(DIB)

# Build shared library for calling from R dyn.load(), doesn't work on Windows
#  R CMD SHLIB -o Rlib_hedutils.so $(OBJ)
libr:  $(OBJ)
	gfortran -shared -o Rlib_hedutils.so $(OBJ) -L/usr/lib/R/lib -lR

# Prepare include files
inc: $(AUTOGEN_HEADER_RAND)

# Make PDF (or other::  DOCFMT=xxx) using asciidoc
doc: $(DOCFIL).$(DOCFMT)

# Book is the PDF manual with title page
book:
	$(ASCDOC) -fpdf $(DOC)
	pdftk $(DOCDIR)pg_cover/cover_page.pdf $(DOCFIL).pdf cat output $(DOCDIR)$(DOCFIL).pdf
	-$(RM) $(DOCFIL).pdf

# Make fancy HTML documentation
webdoc: $(DOCFIL).adoc
	asciidoc -b html5 -a icons -a toc2 -a theme=flask $(DOCFIL).adoc

# Clean workspace completely - distribution state
distclean: neat cleandistro
	-$(RM) *.o *.obj $(MOD) *.lib *.a *.dll *.so $(DOCDIR)/$(DOCFIL).$(DOCFMT)
	-$(RM) $(ZIPFILE) $(AUTOGEN_README_FILE) $(AUTOGEN_HEADER_RAND) *.pdb 
	-$(RM) *.zip $(DOCFIL).html *.css *.xml ?NULL
	-$(RM) $(DOCFIL).pdf
	-$(RM) *NULL
	$(MAKE) -C $(TOOLS_PATH) distclean
	$(MAKE) -C $(TESTS_PATH) distclean

# We don't clean .mod files as they are necessary for building with .so
clean: neat
	-$(RM) *.o *.obj

cleandistro: neat
	-$(RM) $(DDISTRO)/*.*
	-$(RMDIR) $(DDISTRO)
	-$(RM) $(ZIPFILE)

neat:
	-$(RM) $(TMPFILES) *conflict*  .syncthing* *.orig

#-------------------------------------------------------------------------------
.PHONY: help
help:
	@$(ECHO) ""
	@$(ECHO) ------------------------------------------------------------------------
	@$(ECHO) "Building Model Tools as libratry ------ via $(THIS_FILE)"
	@$(ECHO) ------------------------------------------------------------------------
	@$(ECHO) "Normal build: make (uses $(FC) by default)"
	@$(ECHO) ""
	@$(ECHO) "Build the library with Intel Fortran Compiler:"
	@$(ECHO) "    make FC=ifort"
	@$(ECHO) ""
	@$(ECHO) "Build binary (and) plotting tools:"
	@$(ECHO) "    make tools"
	@$(ECHO) ""
	@$(ECHO) "Autogenerate documentation -- requires the asciidoc tool, output"
	@$(ECHO) "format is set by DOCFMT variable, default format is pdf"
	@$(ECHO) "    make doc"
	@$(ECHO) "    make webdoc"
	@$(ECHO) "    make book"
	@$(ECHO) ""
	@$(ECHO) "Cleaning:"
	@$(ECHO) "    make clean, make cleandata (removes object files but not library),"
	@$(ECHO) "    make distclean (everything, leaving the distro state!)"
	@$(ECHO) ------------------------------------------------------------------------
	@$(ECHO) "NOTES:"
	@$(ECHO) " 1. Required command line utilities: echo, make, uname, zip, svn"
	@$(ECHO) "    For Windows, see http://gnuwin32.sourceforge.net/packages.html"
	@$(ECHO) " 2. Intel Fortran compiler under Windows: set up environment for "
	@$(ECHO) "    Microsoft Visual Studio 2010 x64 tools before calling make."
	@$(ECHO) "    Check the Command Prompt menu under Intel Parallel Studio"
	@$(ECHO) ------------------------------------------------------------------------
	@$(ECHO) Revision: $(SVN_VER), Platform: $(PLATFORM_TYPE)
	@$(ECHO) ------------------------------------------------------------------------

#-------------------------------------------------------------------------------

$(LIB): $(OBJ)
	@$(MAKE) -f $(THIS_FILE) inc
	$(STLIBBLD)
	$(AUTOGEN_README_LIB)
	-$(MKDIR) $(DDISTRO) 
	-$(CP)  *.mod  $(DDISTRO)
	-$(CP)  $(LIB) $(DDISTRO)
	-$(CP)  $(AUTOGEN_README_FILE) $(DDISTRO)
ifdef ZIPDISTRO
	zip $(ZIPFILE) $(MOD) $(LIB) $(AUTOGEN_README_FILE)
endif


$(DIB): $(SRC)
	@$(MAKE) -f $(THIS_FILE) inc
ifeq ($(PLATFORM_TYPE)$(FC),Windowsifx)
	@$(MAKE) -f $(THIS_FILE) objects
	$(DYLIBBLD) $(OBJ) -o $@
else
	$(DYLIBBLD) $(SRC)
endif
	$(AUTOGEN_README_DIB)
	-$(MKDIR) $(DDISTRO) 
	-$(CP)  *.mod  $(DDISTRO)
	-$(CP)  $(DIB) $(DDISTRO)
	-$(CP)  $(AUTOGEN_README_FILE) $(DDISTRO)
ifdef ZIPDISTRO
	zip $(ZIPFILE) $(MOD) $(DIB) $(AUTOGEN_README_FILE)
endif

zip: $(LIB)
	zip $(ZIPFILE) $(MOD) $(LIB) $(AUTOGEN_README_FILE)

# make object files
objects: ${OBJ}

%.${OBJEXT}: %.f90
	$(FC) $(FFLAGS) -c -o $@ $^

$(DOCFIL).$(DOCFMT): $(DOCFIL).adoc
	$(ASCDOC) -f$(DOCFMT) $(DOC)
	-$(MV) $(DOCFIL).$(DOCFMT) $(DOCDIR)

#-------------------------------------------------------------------------------

# make include
$(AUTOGEN_HEADER_RAND): $(BASE_RANDOM.f90) $(THIS_FILE)
	$(AUTOGEN_COMMENT_RANDOM)
	$(AUTOGEN_CODE_RANDOM)
	@$(ECHO) Generated include: $(AUTOGEN_HEADER_RAND) for $(FC) using $(ECHO)

# compile modules
BASE_UTILS.$(OBJEXT): BASE_UTILS.f90
	$(FC) $(FFLAGS) -c $<
BASE_CSV_IO.$(OBJEXT): BASE_CSV_IO.f90
	$(FC) $(FFLAGS) -c $<
BASE_LOGGER.$(OBJEXT): BASE_LOGGER.f90
	$(FC) $(FFLAGS) -c $<
BASE_RANDOM.$(OBJEXT): BASE_RANDOM.f90 $(THIS_FILE)
	@$(MAKE) -f $(THIS_FILE) inc
	$(FC) $(FFLAGS) -c $<
BASE_STRINGS.$(OBJEXT): BASE_STRINGS.f90
	$(FC) $(FFLAGS) -c $<
