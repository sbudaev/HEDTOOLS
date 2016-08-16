#-------------------------------------------------------------------------------
# SVN version info:
# $Id$
#-------------------------------------------------------------------------------
# Build Modelling tools as a static and shared libraries, produce doc file (pdf)
# Note that linking the model code with shared libraries does not look like a
#      good idea as they will have some runtime loading-unloading overhead.
#      Also, building shared dll's might be problematic with Intel Fortran on
#      Windows.
# Requires the following command line utilities
#      (may NOT be available on Windows): make, uname, zip, svn
#-------------------------------------------------------------------------------

# Supported Fortran compiler types
GF_FC = gfortran
IF_FC = ifort
SF_FC = f95

# Choose the compiler type
FC = $(GF_FC)

#*******************************************************************************
# Determine what is the build platform, Windows / non-Windows
# Use uname -- but it may not be installed on Windows. Probably the method
# based on ComSpec is safer. Note that the -c switch on grep suppresses normal
# output and just prints count.
# We use PLATFORM only for outputting the OS in zipfile and other things that
# are unimportant if uname is absent on the system (Windows), for real
# OS/platform check better use a safer mechanism based on ComSpec (below).
PLATFORM = $(shell uname)
#IS_WINDOWS = $(shell uname | grep -ci windows)

# A safer way to check platform if uname is not available, ComSpec on Windows
# Note that ComSpec is (may be?) case-sensitive, check with env.exe
ifdef ComSpec
	PLATFORM_TYPE=Windows
	IS_WINDOWS=1
else
	PLATFORM_TYPE=Unix
	IS_WINDOWS=0
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

# Check if certain required executables exist and are callable in path. This is
# important on the Windows platform because such GNU command line utilities as
# uname and zip are not installed by default.
REQUIRED_EXECS = uname zip a2x
K := $(foreach exec,$(REQUIRED_EXECS),\
	$(if $(shell which $(exec)),check executables,\
	$(warning ************ No $(exec) available in PATH ************)))

#*******************************************************************************
# Main building blocks, define
SRC = BASE_UTILS.f90 BASE_CSV_IO.f90 BASE_LOGGER.f90 BASE_RANDOM.f90 \
      BASE_ERRORS.f90 BASE_STRINGS.f90

OBJ = BASE_UTILS.$(OBJEXT) BASE_CSV_IO.$(OBJEXT) BASE_LOGGER.$(OBJEXT) \
      BASE_RANDOM.$(OBJEXT) BASE_ERRORS.$(OBJEXT) BASE_STRINGS.$(OBJEXT)

MOD = base_utils.mod  csv_io.mod  logger.mod base_random.mod \
      assert.mod errors.mod exception.mod throwable.mod precision_str.mod \
      strings.mod

DOC = BASE_UTILS.adoc

LIBNAME = lib_hedutils
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

# Options for Sun/Oracle Solaris Studio
SF_STATIC = –Bstatic –dn
SF_TRAPS = -ftrap=%none
SF_RCHECKS = –C
SF_FFLAGS = -fast -autopar -depend=yes -pic $(SF_STATIC) $(SF_TRAPS) $(SF_RCHECKS)
SF_STLIBBLD = ar cr $(LIB) $(OBJ)
SF_DYLIBBLD = $(FC) -fast -autopar -depend=yes -pic $(SF_TRAPS) -G -o $(DIB)
# -fast = O5
# -ftrap=common is a macro for -ftrap=invalid,overflow,division.
# -ftrap=%all, %none, common

#-------------------------------------------------------------------------------
# Set other build options depending on the specific compiler

ifeq ($(FC),gfortran)
	FFLAGS = $(GF_FFLAGS)
	STLIBBLD =  $(GF_STLIBBLD)
	DYLIBBLD =  $(GF_DYLIBBLD)
endif

ifeq ($(FC),ifort)
	FFLAGS = $(IF_FFLAGS)
	STLIBBLD = $(IF_STLIBBLD)
	DYLIBBLD = $(IF_DYLIBBLD)
endif

ifeq ($(PLATFORM_TYPE)$(FC),Windowsifort)
	FFLAGS = $(IF_FFLAGS_WINDOWS)
	STLIBBLD = $(IF_STLIBBLD_WINDOWS)
	DYLIBBLD = $(IF_DYLIBBLD_WINDOWS)
endif

ifeq ($(FC),f95)
	FFLAGS = $(SF_FFLAGS)
	STLIBBLD = $(SF_STLIBBLD)
	DYLIBBLD = $(SF_DYLIBBLD)
endif

# DEBUG turns off all optimisations and keeps debug symbols.
ifdef DEBUG
	GF_FFLAGS = -O0 -g -ffpe-trap=zero,invalid,overflow,underflow $(GF_RCHECKS)
	IF_FFLAGS = -O0 -g -fpe0 $(IF_RCHECKS)
	IF_FFLAGS_WINDOWS = /c /Zi /Od /debug:full /fpe:0 /nopdbfile
	#$(IF_RCHECKS_WINDOWS)
	SF_FFLAGS = -O0 -g -ftrap=%all $(SF_RCHECKS)
endif

#-------------------------------------------------------------------------------
# Documentation builder parameters (asciidoc)

DOCFIL = BASE_UTILS
DOCFMT = pdf
DOCDIR = doc/

# Determine current SVN version of the code
SVN_VER = $(shell svn info | grep Revision: | cut -d " " -f 2)
#-------------------------------------------------------------------------------

# Determine this makefile's path. Be sure to place this BEFORE `include`s
THIS_FILE := $(lastword $(MAKEFILE_LIST))

# This is the search paths for looking for components, separated by blanks
VPATH = $(DOCDIR)

#-------------------------------------------------------------------------------
# Zipfile distro to be generated
ZIPFILE = LIBHEDUTILS_$(PLATFORM)_$(FC)_rev$(SVN_VER).zip

# Autogenerated README.txt for static library ZIP AUTOGEN_README
AUTOGEN_README_FILE = Readme.txt
define AUTOGEN_README_LIB
	$(shell echo "HEDTOOLS: SVN revision: $(SVN_VER)" > $(AUTOGEN_README_FILE))
	$(shell echo "" >> $(AUTOGEN_README_FILE))
	$(shell echo "This is a static library binary build of the modelling tools." >> $(AUTOGEN_README_FILE))
	$(shell echo "Note that .mod files are needed for compiling." >> $(AUTOGEN_README_FILE))
	$(shell echo "Compile command using the library:" >> $(AUTOGEN_README_FILE))
	$(shell echo "" >> $(AUTOGEN_README_FILE))
	$(shell echo "    $(FC) file.f90 $(LIB)" >> $(AUTOGEN_README_FILE))
	$(shell echo "" >> $(AUTOGEN_README_FILE))
	$(shell echo "IMPORTANT: Use exactly the same platform and compiler that" >> $(AUTOGEN_README_FILE))
	$(shell echo "           was used to generate the static library!" >> $(AUTOGEN_README_FILE))
endef

# Autogenerated README.txt for dynamic library. Note that I had difficulties
#  building and using shared libraries on cerrain platforms, e.g building on
#  ifort for Windows is broken
AUTOGEN_README_FILE = Readme.txt
define AUTOGEN_README_DIB
	$(shell echo "HEDTOOLS: SVN revision: $(SVN_VER)" > $(AUTOGEN_README_FILE))
	$(shell echo "" >> $(AUTOGEN_README_FILE))
	$(shell echo "This is the shared library binary build of the modelling tools" >> $(AUTOGEN_README_FILE))
	$(shell echo "Note that .mod files are needed for compiling." >> $(AUTOGEN_README_FILE))
	$(shell echo "Compile command:" >> $(AUTOGEN_README_FILE))
	$(shell echo "    $(FC) $(DIB) file.f90" >> $(AUTOGEN_README_FILE))
	$(shell echo "   ifort Windows produces both .dll and .lib files" >> $(AUTOGEN_README_FILE))
	$(shell echo "   .lib file should be used for building: " >> $(AUTOGEN_README_FILE))
	$(shell echo "    $(FC) file.f90 $(LIB)" >> $(AUTOGEN_README_FILE))
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
	$(shell echo "! ---------------------------------------------------------" > $(AUTOGEN_HEADER_RAND))
	$(shell echo "! WARNING: auto-generated file, do NOT edit" >> $(AUTOGEN_HEADER_RAND))
	$(shell echo "! Sets compiler-specific code for random number generator" >> $(AUTOGEN_HEADER_RAND))
	$(shell echo "! ---------------------------------------------------------" >> $(AUTOGEN_HEADER_RAND))
endef

# Include file code for GNU
define AUTOGEN_CODE_GF
	$(shell echo "! GNU Fortran compiler:" >> $(AUTOGEN_HEADER_RAND))
	$(shell echo "use ISO_FORTRAN_ENV, only: int64">> $(AUTOGEN_HEADER_RAND))
	$(shell echo "implicit none">> $(AUTOGEN_HEADER_RAND))
	$(shell echo "integer, allocatable :: seed(:)">> $(AUTOGEN_HEADER_RAND))
	$(shell echo "integer :: i, n, un, istat, dt(8), pid">> $(AUTOGEN_HEADER_RAND))
	$(shell echo "integer(int64) :: t">> $(AUTOGEN_HEADER_RAND))
endef

# Include file code for Intel
define AUTOGEN_CODE_IF
	$(shell echo "! Intel Fortran compiler:" >> $(AUTOGEN_HEADER_RAND))
	$(shell echo "use ISO_FORTRAN_ENV, only: int64 " >> $(AUTOGEN_HEADER_RAND))
	$(shell echo "use IFPORT, only : getpid" >> $(AUTOGEN_HEADER_RAND))
	$(shell echo "implicit none" >> $(AUTOGEN_HEADER_RAND))
	$(shell echo "integer, allocatable :: seed(:)" >> $(AUTOGEN_HEADER_RAND))
	$(shell echo "integer :: i, n, un, istat, dt(8), pid" >> $(AUTOGEN_HEADER_RAND))
	$(shell echo "integer(int64) :: t" >> $(AUTOGEN_HEADER_RAND))
endef

# Include file code for Oracle
define AUTOGEN_CODE_SF
	$(shell echo "! Intel Oracle compiler:" >> $(AUTOGEN_HEADER_RAND))
	$(shell echo "implicit none" >> $(AUTOGEN_HEADER_RAND))
	$(shell echo "integer, allocatable :: seed(:)" >> $(AUTOGEN_HEADER_RAND))
	$(shell echo "integer :: i, n, un, istat, dt(8), pid" >> $(AUTOGEN_HEADER_RAND))
	$(shell echo "integer, parameter :: int64 = selected_int_kind(18)" >> $(AUTOGEN_HEADER_RAND))
	$(shell echo "integer(int64) :: t" >> $(AUTOGEN_HEADER_RAND))
	$(shell echo "include \"system.inc\"" >> $(AUTOGEN_HEADER_RAND))
endef

# Select autogeneration of include file for the specific compiler type
ifeq ($(FC),$(GF_FC))
	AUTOGEN_CODE_RANDOM=$(AUTOGEN_CODE_GF)
endif

ifeq ($(FC),$(IF_FC))
	AUTOGEN_CODE_RANDOM=$(AUTOGEN_CODE_IF)
endif

ifeq ($(FC),$(SF_FC))
	AUTOGEN_CODE_RANDOM=$(AUTOGEN_CODE_SF)
endif

#*******************************************************************************

all: static

lib: $(LIB)

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

# Clean workspace completely - distribution state
distclean: neat
	-rm -f *.o *.obj $(MOD) *.lib *.a *.dll *.so $(DOCDIR)/BASE_UTILS.$(DOCFMT) \
	       $(ZIPFILE) $(AUTOGEN_README_FILE) $(AUTOGEN_HEADER_RAND)

# We don't clean .mod files as they are necessary for building with .so
clean: neat
	-rm -f *.o *.obj

neat:
	-rm -f $(TMPFILES) *conflict*  .syncthing*

#-------------------------------------------------------------------------------

help:
	@echo ""
	@echo ------------------------------------------------------------------------
	@echo "Building Model Tools as libratry ------ via $(THIS_FILE)"
	@echo ------------------------------------------------------------------------
	@echo "Normal build: make (uses $(FC) by default)"
	@echo ""
	@echo "Build the library with Intel Fortran Compiler:"
	@echo "    make FC=ifort"
	@echo ""
	@echo "Autogenerate documentation -- requires the asciidoc tool!"
	@echo "    make doc"
	@echo ""
	@echo "Cleaning:"
	@echo "    make clean, make cleandata (removes object files but not library),"
	@echo "    make distclean (everything, leaving the distro state!)"
	@echo ------------------------------------------------------------------------
	@echo "NOTES:"
	@echo " 1. Required command line utilities: uname, zip. For Windows available"
	@echo "     from: http://gnuwin32.sourceforge.net/packages.html"
	@echo " 2. Intel Fortran compiler under Windows: set up environment for "
	@echo "    Microsoft Visual Studio 2010 x64 tools before calling make."
	@echo "    Check the Command Prompt menu under Intel Parallel Studio XE"
	@echo ------------------------------------------------------------------------
	@echo Revision: $(SVN_VER), Platform: $(PLATFORM_TYPE)
	@echo ------------------------------------------------------------------------

#-------------------------------------------------------------------------------

$(LIB): $(OBJ)
	@$(MAKE) -f $(THIS_FILE) inc
	$(STLIBBLD)
	$(AUTOGEN_README_LIB)
	zip $(ZIPFILE) $(MOD) $(LIB) $(AUTOGEN_README_FILE)

$(DIB): $(SRC)
	@$(MAKE) -f $(THIS_FILE) inc
	$(DYLIBBLD) $(SRC)
	$(AUTOGEN_README_DIB)
	zip $(ZIPFILE) $(MOD) $(DIB) $(AUTOGEN_README_FILE)

$(DOCFIL).$(DOCFMT): $(DOCFIL).adoc
	a2x -f$(DOCFMT) BASE_UTILS.adoc
	mv $(DOCFIL).$(DOCFMT) $(DOCDIR)

#-------------------------------------------------------------------------------

# make include
$(AUTOGEN_HEADER_RAND): $(BASE_RANDOM.f90) $(THIS_FILE)
	$(AUTOGEN_COMMENT_RANDOM)
	$(AUTOGEN_CODE_RANDOM)
	@echo Generated include: $(AUTOGEN_HEADER_RAND) for $(FC)

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
BASE_ERRORS.$(OBJEXT): BASE_ERRORS.f90
	$(FC) $(FFLAGS) -c $<
BASE_STRINGS.$(OBJEXT): BASE_STRINGS.f90
	$(FC) $(FFLAGS) -c $<
