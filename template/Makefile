#-------------------------------------------------------------------------------
# SVN version info:
# $Id: Makefile 4793 2017-08-05 04:57:01Z sbu062 $
# Build model automatically generating include files for PRNG and IEEE
# author Sergey Budaev <sbudaev@gmail.com>
#-------------------------------------------------------------------------------
##//! @file Makefile GNU Make build configuration.
##//! @brief Makefile to build the model executable from the source code.
##//! @details This is a standard GNU Make "Makefile" to build the model from
##//!          the sources. It also automatically generates some platform and
##//!          compiler specific include files for the `BASE_RANDOM` and the
##//!          IEEE math modules. See @ref intro_main "Working with the model"
##//!          section and the @ref README.md for details.
##//!
##//! ### Dependencies ###
##//! In addition to the GNU Make, this Makefile also depends on the
##//! `grep`, `cut`, `sed` and `awk` utilities that are installed on any
##//! Unix/Linux system, but usually absent on Windows. For Windows
##//! they can be obtained from several locations on the web, e.g.
##//! Cygwin or GnuWin32. See http://158.37.63.57/doc/ar01s01.html for
##//! more information.
##//!
##//! ### Main adjustable parameters ###
##//! @param FC sets the default compiler type from the above. Normally GNU
##//!        gfortran.
##//! @param HOST_HPC_ROOT is the hostname to run the model executable in the
##//!        HPC batch mode. If the hostname the Makefile is called in is this
##//!        system, `make run` starts a new batch task. Otherwise, the model
##//!        executable is just started normally.
##//! @param SRC is the name of the main source code (can be several files).
##//! @param DRV is the source code of the model "driver", that is the main
##//!        Fortran program that produces the executable. It should be very
##//!        very small. The "driver" is a separate file from the AHA Model
##//!        modules.
##//! @param OUT is the executable file name that is actually run, on the
##//!        Windows platform must have the `.exe` suffix. By default, the
##//!        executable name is set to `MODEL.exe` for maximum portability.
##//! @param DOXYCFG the Doxygen documentation system configuration file name.
##//! @param DOXYPATH defines the Doxygen output directory where the
##//!        documentation is generated. This could be set manually or parsed
##//!        automatically using the `grep` command. Note that the output
##//!        directory is set in the `Doxyfile` as:
##//!        `OUTPUT_DIRECTORY = ./model_docs`.
##//! @param HEDTOOLS is the list of HEDTOOLS Fortran source files that are
##//!        used in the AHA Model. Note that `BASE_STRINGS` in the list must
##//!        go before `BASE_CSV_IO` because `BASE_CSV_IO` depends on
##//!        procedures from `BASE_STRINGS`.
##//! @param HEDTOOLSDIR defines the path to the HEDTOOLS source code.
##//! @param IEEEPATH defines the path to the IEEE non-intrinsic math modules
##//!        (part of HEDTOOLS).
##//! @param WINRM defines the command that is used to delete files and
##//!        directories on the Windows platform; can be set either `del` or
##//!        `rm`.
##//! @note  The native file delete command on the Windows platform is 'del' or
##//!        'erase'. However, if Cygwin is used, this native file removal
##//!        command may not call with a "command not found" error. In such a
##//!        case, use the Unix 'rm' tool provided by Cygwin.
##//!
##//! The source code of the @ref Makefile contains many other parameters and
##//! is well documented throughout.
##//! @cond
#  Doxygen note: block marked with cond .. endcond is excluded from Doxygen
#                processing. Therefore, everything below to the end of the
#                Makefile is not parsed. Makefile is not normally supported
#                by Doxygen.

#===============================================================================

# Supported Fortran compiler types
GF_FC = gfortran
IF_FC = ifort
SF_FC = f95

# Choose the default compiler type
FC = $(GF_FC)

# Graphics library to use, for example:
# PGPLOT, if installed by the system:
#   -lpgplot
# If PGPLOT is manually installed by the user into /usr/local/pgplot with
#   X11 development libraries in /usr/lib/x86_64-linux-gnu (as in a
#   Debian system), use this value for GRAPHLIB:
# -L/usr/local/pgplot -L/usr/lib/x86_64-linux-gnu -lpgplot -lX11
# --------------------------------------
# PLPLOT:
#   -lplplotf95d -lplplotf95cd -lplf95demolibd -I/usr/lib/fortran/modules/plplot
GRAPHLIB = -lpgplot

# Root directory of the GrWin installation on Windows.
# NOTE: can differ in 32 and 64 bit distributions!
GRWIN_ROOT = C:/GrWin/MinGW_gfortran_x6

# Root of the host name to run in HPC cluster / batch mode, not directly
HOST_HPC_ROOT=fimm

#===============================================================================
# Main building blocks, define the main program source, executable name (-o)
# and possible module (.mod) names.

# These names should be set for particular project,
# Most probably they should be edited manually for each specific project
# SRC is the name of the main source code (can be several files). Note that
#     Intel fortran doesn't like f95 file extension, use f90
# DRV is the source code of the model "driver", that is the main Fortran
#     program that produces the executable. It should be very very small.
#     The "driver" is a separate file from the modules.
# OUT is the executable file, on the Windows platform has the .exe suffix.

SRC = HEDG2_04.f90

DRV = HEDG2_DRV.f90

OUT = MODEL.exe

# Doxygen configuration file within the same directory.
DOXYCFG = Doxyfile

# Doxygen output directory, can be grepped from Configuration file.
# OUTPUT_DIRECTORY       = ./model_docs
# DOXYPATH = model_docs
DOXYPATH = $(shell grep ^OUTPUT_DIRECTORY Doxyfile | cut -d '=' -f 2)

#===============================================================================
# HEDTOOLS sources and module files:
# Warning: BASE_STRINGS must go before BASE_CSV_IO because BASE_CSV_IO now
#          depends on procedures from BASE_STRINGS.

HEDTOOLS = BASE_UTILS.f90 BASE_STRINGS.f90 BASE_CSV_IO.f90 BASE_LOGGER.f90 \
           BASE_RANDOM.f90

# Path to HEDTOOLS and IEEE non-intrinsic libs (part of HEDTOOLS).
HEDTOOLSDIR = ../HEDTOOLS
IEEEPATH = $(HEDTOOLSDIR)/IEEE

#===============================================================================
# Accessory settings for the build environment

# The native file delete command on the Windows platform is 'del' or 'erase'.
# However, if Cygwin is used, this native file removal command may not call
# with a "command not found" error. In such a case, use the Unix 'rm' tool
# provided by Cygwin.
WINRM := rm -fr

#===============================================================================
# Define compiler options

# **** Options for GNU Fortran
GF_STATIC = -static-libgfortran -static -static-libgcc
GF_TRAPS = -ffpe-trap=
GF_RCHECKS = -Wall -fbounds-check
#GF_FFLAGS = $(GF_STATIC) $(GF_TRAPS) $(GF_RCHECKS) -O3
GF_FFLAGS = -O3 -funroll-loops -fforce-addr
#-ffpe-trap=zero,invalid,overflow,underflow
# GC_FFLAGS are for gcc C compiler (C may be used for IEEEs)
GC_FFLAGS = -O3
# On Windows might need  -mwindows

# **** Options for Intel Fortran
IF_STATIC = -static
IF_TRAPS = -fpe3
IF_RCHECKS = -gen-interfaces -warn -check bounds,pointers,format,uninit
IF_FFLAGS = -sox -parallel -O3 -ipo
# For Intel Fortran, options are somewhat different on Windows platform.
# Note: Windows stack size is insufficient for the large number of concurrent
#       threads and huge arrays used in the model. Therefore, stack overflow
#       error is issued (ifort 17) with the default build options. Stack size
#       available for the program is controlled by the /Fn option (n is in
#       bytes). Setting a huge value resolves the stack overflow issue on the
#       Windows platform. This can sometimes occur on the Linux and Mac OS X
#       platforms too. To fix it on Linux, issue this command:
#           ulimit -s unlimited
#       Increasing stack size on Mac OS X is described here:
#       https://developer.apple.com/library/content/qa/qa1419/_index.html.
IF_FFLAGS_WINDOWS = /Qsox /fast /fpe:0 /Qparallel /gen-interfaces /warn \
                    /F100000000

#IF_FFLAGS = -sox -fast -parallel $(IF_STATIC) $(IF_TRAPS)
# -fpe3 no traps; -fpe0 all traps
# -O3
# Aggressive optimizations: -fast = -O3 –ipo –static

# **** Options for Sun/Oracle Solaris Studio
SF_STATIC = -Bstatic -dn
SF_TRAPS = -ftrap=%none
SF_RCHECKS = -C
SF_FFLAGS = -O1 -depend=yes
# NOTE: optimisations exceeding -O1 might not work with all CSV_IO routines
# SF_FFLAGS = -fast -autopar -depend=yes
# -fast = O5
# -ftrap=common is a macro for -ftrap=invalid,overflow,division.
# -ftrap=%all, %none, common

#-------------------------------------------------------------------------------

# DEBUG turns off all optimisations and keeps debug symbols.
ifdef DEBUG
	GF_FFLAGS = -O0 -g -ffpe-trap=zero,invalid,overflow,underflow \
							-Warray-temporaries $(GF_RCHECKS)
	IF_FFLAGS = -O0 -g -debug all -fpe0 -traceback $(IF_RCHECKS)
	SF_FFLAGS = -O0 -g -ftrap=common
	IF_FFLAGS_WINDOWS = /Qsox /fpe:0 /debug:all /traceback /gen-interfaces \
											/check /warn /F100000000
	RUNFLAG = DEBUG
endif

# PROFILE adds profiling options (-pg on GNU). Then running the program provides
#    a profiling report: gmon.out. It can be read with: gprof MODEL.exe
ifdef PROFILE
	GF_FFLAGS += -pg
endif

#-------------------------------------------------------------------------------
# Set other build options depending on the specific compiler

ifeq ($(FC),$(GF_FC))
	FFLAGS = $(GF_FFLAGS)
	STATIC = $(GF_STATIC)
	TRAPS = $(GF_TRAPS)
	RCHECKS = $(GF_RCHECKS)
	CC=gcc
endif

ifeq ($(FC),$(IF_FC))
	FFLAGS = $(IF_FFLAGS)
	STATIC = $(IF_STATIC)
	TRAPS = $(IF_TRAPS)
	RCHECKS = $(IF_RCHECKS)
endif

ifeq ($(FC),$(SF_FC))
	FFLAGS = $(SF_FFLAGS)
	STATIC = $(SF_STATIC)
	TRAPS = $(SF_TRAPS)
	RCHECKS = $(SF_RCHECKS)
endif

#===============================================================================
# Determine what is the build platform, Windows / non-Windows

# Use uname -- but it may not be installed on Windows. Probably the method
# based on ComSpec is safer. Note that the -c switch on grep suppresses normal
# output and just prints count.
##PLATFORM = $(shell uname)
##IS_WINDOWS = $(shell uname | grep -ci windows)

# A safer way to check platform if uname is not available, ComSpec on Windows
# Note that ComSpec is (may be?) case-sensitive, check with env.exe
ifdef ComSpec
	PLATFORM_TYPE=Windows
	IS_WINDOWS=1
	WHICH_CMD=where
	NULLDEV=":NULL"
	GF_FFLAGS += -L$(GRWIN_ROOT)/lib -Wl,--subsystem,console -mwindows
	GRAPHLIB += -lGrWin
	RM := $(WINRM)
	MV := move
else
	PLATFORM_TYPE=Unix
	IS_WINDOWS=0
	WHICH_CMD=which
	NULLDEV="/dev/null"
	RM := rm -fr
	MV := mv -f
endif

# Check if certain required executables exist and are callable in path. This is
# important on the Windows platform because such GNU command line utilities as
# uname and zip are not installed by default.
REQUIRED_EXECS = uname zip a2x ifort f95 gfortran
K := $(foreach exec,$(REQUIRED_EXECS),\
	$(if $(shell $(WHICH_CMD) $(exec) 2>$(NULLDEV)),check executables,\
	$(warning ************ No $(exec) available in PATH ************)))

# Check if we build on Windows platform with Intel Compiler. It is specific in
# two ways: (1) build options are different, start with slash and often have Q
# and (2) .obj is used as the extension for object files rather than .o
# here we check if with an emulated AND condition. Do this by simply
# concatenating two things into Windowsifort.
ifeq ($(PLATFORM_TYPE)$(FC),Windowsifort)
	OBJEXT=obj
	OBJ = $(addsuffix .$(OBJEXT), $(basename $(SRC)))
	IF_FFLAGS:=$(IF_FFLAGS_WINDOWS)
else
	OBJEXT=o
	OBJ = $(addsuffix .$(OBJEXT), $(basename $(SRC)))
endif

#===============================================================================
# HEDTOOLS object files:
# Warning: BASE_STRINGS must go before BASE_CSV_IO because BASE_CSV_IO now
#          depends on procedures from BASE_STRINGS. This should be set by
#          correct ordering of components in the HEDTOOLS variable.

HTOOLOBJ = $(addsuffix .$(OBJEXT), $(basename $(HEDTOOLS)))

#===============================================================================
# Generate the list of modules used in the source.
# First, get the list of module calls from the 'use' statements within the
# source code
# NOTE: Make turns $$ into $ in awk call, otherwise it goes empty.
#       The sort GNU make function produces a sorted list deleting duplicates.
MOD_LIST = $(sort $(shell grep -i "^ *use " $(SRC) | sed -e 's/,//g' | awk '{printf(" %s ",$$2) }'))

# Second, produce the final list of all .mod files that are to be generated.
# MOD_ALL is a list of Fortran modules generated by the main program as well
# as HEDTOOLS. So far MOD_ALL is not used for anything very important, only in
# cleaning temporary files, so can be set to .mod files remaining after make
# clean/distclean. Modules can be included into prerequisites?
# NOTE: The code automatically generating the list of modules uses the MOD_LIST
#       variable that gets the list of modules by applying Unix 'grep' to the
#       source code files; extracts basename (just in case) and adds the
#       '.mod' suffix to each file.
MOD_ALL = $(addsuffix .mod, $(basename $(call lowercase, $(MOD_LIST))))

#===============================================================================
# Auto-generated include file setting compiler/platform specific code for PRNG
# The module BASE_RANDOM includes some non-portable header code that
# depends on the compiler type. It is auto-generated here. The Fortran Module
# code contains INCLUDE statement. The appropriate include file is then
# auto-generated during the build process.
# The strategy of the build is two-step:
#    (1) Provide comment macro for the include file as well as the code adapted
#        for the specific compiler type and platform
#    (2) Select macro from the above list specific for the
#        compiler type
#    (3) build the BASE_RANDOM.o, the first statement is the include
#        auto-generation macro, then follow the compile instructions.

#  Header file setting compiler/platform specific code for PRNG module
AUTOGEN_HEADER_RAND = $(HEDTOOLSDIR)/BASE_RANDOM.inc

# Auto-generated include files, note that the first two lines do doxygen
# comments.
define AUTOGEN_COMMENT_RANDOM
	$(shell echo "!> @file $(AUTOGEN_HEADER_RAND)"                               > $(AUTOGEN_HEADER_RAND))
	$(shell echo "!! Autogenerated header for module RANDOM built with $(FC)"   >> $(AUTOGEN_HEADER_RAND))
	$(shell echo "!! Sets compiler-specific code for random number generator"   >> $(AUTOGEN_HEADER_RAND))
	$(shell echo "!+---------------------------------------------------------+" >> $(AUTOGEN_HEADER_RAND))
	$(shell echo "!| WARNING: auto-generated file, do NOT edit               |" >> $(AUTOGEN_HEADER_RAND))
	$(shell echo "!| Sets compiler-specific code for random number generator |" >> $(AUTOGEN_HEADER_RAND))
	$(shell echo "!+---------------------------------------------------------+" >> $(AUTOGEN_HEADER_RAND))
endef

# Include file code for GNU
define AUTOGEN_CODE_GF
	$(shell echo "! GNU Fortran compiler:" >> $(AUTOGEN_HEADER_RAND))
	$(shell echo "use ISO_FORTRAN_ENV, only: int64" >> $(AUTOGEN_HEADER_RAND))
	$(shell echo "implicit none" >> $(AUTOGEN_HEADER_RAND))
	$(shell echo "integer, allocatable :: seed(:)" >> $(AUTOGEN_HEADER_RAND))
	$(shell echo "integer :: i, n, un, istat, dt(8), pid" >> $(AUTOGEN_HEADER_RAND))
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

#===============================================================================
# Determine compiler versions, we may need GCC version > 5 to use
# non-intrinsic IEEE math modules for gfortran < 5, GCC > 5 fully
# supports intrinsic IEEE math.
# IEEE should work with recent versions

# Check GCC version and need for non-intrinsic modules
GFORTVERSION = $(shell $(GF_FC) -dumpversion)
GFORT_LESS_5 = $(shell expr `echo $(GFORTVERSION) | cut -d. -f1` \< 5)
ifeq ($(GFORT_LESS_5),1)
	COMPILE_NONINTRINSIC_IEEE=YES
else
	COMPILE_NONINTRINSIC_IEEE=NO
endif

# IEEE non-intrinsic modules, list of sources
IEEE_NON_INTRINSIC_SRC = precision_m.f90 IEEE_FEATURES.f90 IEEE_EXCEPTIONS.f90 \
                        IEEE_ARITHMETIC.f90 c_control.c

IEEEMOD = ieee_arithmetic.mod  ieee_exceptions.mod  ieee_features.mod  \
          precision.mod

# IEEE non-intrinsic objects
IEEEOBJ = precision_m.$(OBJEXT) c_control.$(OBJEXT) IEEE_FEATURES.$(OBJEXT) \
					IEEE_EXCEPTIONS.$(OBJEXT) IEEE_ARITHMETIC.$(OBJEXT)

# Macros to auto-generate F90 headers for IEEE modules -- AUTOGEN_HEADER_FILE
AUTOGEN_HEADER_IEEE = IEEE_wrap.inc

# Check if the source does include IEEE calls at all, disable non-intrinsic IEEE
# if the Fortran source does never refer to them
IEEE_GREPPED_SRC = $(shell grep -ci $(AUTOGEN_HEADER_IEEE) $(SRC))
ifeq ($(IEEE_GREPPED_SRC),0)
	COMPILE_NONINTRINSIC_IEEE=NO
endif

ifneq ($(FC),gfortran)
	COMPILE_NONINTRINSIC_IEEE=NO
endif

# Autogenerated include files
define AUTOGEN_COMMENT_HEADER_F90
	$(shell echo "!> @file $(AUTOGEN_HEADER_IEEE)"                                >  $(AUTOGEN_HEADER_IEEE))
	$(shell echo "!! Autogenerated header for IEEE math modules built with $(FC)" >> $(AUTOGEN_HEADER_IEEE))
	$(shell echo "!! Calls intrinsic or non-intrinsic IEEE math modules"          >> $(AUTOGEN_HEADER_IEEE))
	$(shell echo "!+-----------------------------------------------------+" >> $(AUTOGEN_HEADER_IEEE))
	$(shell echo "!| WARNING: auto-generated file, do NOT edit           |" >> $(AUTOGEN_HEADER_IEEE))
	$(shell echo "!| Calls intrinsic or non-intrinsic IEEE math modules  |" >> $(AUTOGEN_HEADER_IEEE))
	$(shell echo "!+-----------------------------------------------------+" >> $(AUTOGEN_HEADER_IEEE))
endef

# Now only use IEEE_EXCEPTIONS are managed. Other modules may be added:
#  IEEE_FEATURES IEEE_ARITHMETIC IEEE_EXCEPTIONS
define AUTOGEN_INTRINSIC
	$(AUTOGEN_COMMENT_HEADER_F90)
	$(shell echo "use, intrinsic :: IEEE_EXCEPTIONS" >> $(AUTOGEN_HEADER_IEEE))
endef

# Now only use IEEE_EXCEPTIONS are managed. Other modules may be added:
#  IEEE_FEATURES IEEE_ARITHMETIC IEEE_EXCEPTIONS
define AUTOGEN_NON_INTRINSIC
	$(AUTOGEN_COMMENT_HEADER_F90)
	$(shell echo "use, non_intrinsic :: IEEE_EXCEPTIONS" >> $(AUTOGEN_HEADER_IEEE))
endef

#-------------------------------------------------------------------------------

# Function to convert UPPERCASE to lowercase by Eric Melski, taken from
# https://stackoverflow.com/questions/664601/in-gnu-make-how-do-i-convert-a-variable-to-lower-case
# It looks a little clunky, but it gets the job done...
lowercase = $(subst A,a,$(subst B,b,$(subst C,c,$(subst D,d,$(subst E,e,$(subst F,f,$(subst G,g,$(subst H,h,$(subst I,i,$(subst J,j,$(subst K,k,$(subst L,l,$(subst M,m,$(subst N,n,$(subst O,o,$(subst P,p,$(subst Q,q,$(subst R,r,$(subst S,s,$(subst T,t,$(subst U,u,$(subst V,v,$(subst W,w,$(subst X,x,$(subst Y,y,$(subst Z,z,$1))))))))))))))))))))))))))

#-------------------------------------------------------------------------------

# Determine this makefile's path. Be sure to place this BEFORE `include`s
THIS_FILE := $(lastword $(MAKEFILE_LIST))

# This is the search paths for looking for components, separated by blanks
VPATH = $(HEDTOOLSDIR) $(IEEEPATH)

# Determine if we are running on HPC cluster
RUNHOST_HPC = $(shell hostname | grep -c $(HOST_HPC_ROOT))

# Determine current SVN version of the code
SVN_VER = $(shell svn info | grep Revision: | cut -d " " -f 2)

#*******************************************************************************
# targets follow
#*******************************************************************************

# Default build target = executable
all: exec

docs: $(DOXYCFG)
	doxygen $(DOXYCFG)

# Short help on the options
# For Intel Fortran compiler on Windows, we have to call the batch file to setup
#   the build environment. This command is taken from the Command Prompt
#   shortcut in the Intel Parallel Studio menu:
#C:\Windows\SysWOW64\cmd.exe /E:ON /V:ON /K ""C:\Program Files (x86)\Intel\Composer XE 2013\bin\ipsxe-comp-vars.bat" intel64 vs2010"
#   This mitght be different on different systems, depending on the
#   installation options for Intel Parallel Studio and the Windows platform
#   type (e.g. 32 vs. 64 bit)
help:
	@echo ""
	@echo ------------------------------------------------------------------------
	@echo "Building model $(SRC) ------ via $(THIS_FILE)"
	@echo ------------------------------------------------------------------------
	@echo "Normal build: make (uses $(FC) by default)"
	@echo ""
	@echo "Main (re)build (for different compiler type targets):"
	@echo "    make gnu, make intel, make sun"
	@echo ""
	@echo "Compile HEDTOOLS modelling tools: make tools"
	@echo ""
	@echo "Autogenerate documentation with Doxygen (config: $(DOXYCFG))"
	@echo "    make docs"
	@echo ""
	@echo "Produce system-specific include headers (PRNG and IEEE): make inc"
	@echo ""
	@echo "Produce debug symbols: define DEBUG, e.g.: make DEBUG=1"
	@echo "   profiling output (GNU): define PROFILE, e.g.: make PROFILE=1"
	@echo ""
	@echo "Run model: make run (make run_local for local run)"
	@echo ""
	@echo "Cleaning:"
	@echo "    make clean, make cleandata (model data only),"
	@echo "    make distclean (everything, leaving the distro state!)"
	@echo ------------------------------------------------------------------------
	@echo "NOTES:"
	@echo " 1. IEEE non-intrinsic modules: make ieee, needed only for GCC<5"
	@echo " 2. Intel Fortran compiler under Windows: set up environment for "
	@echo "    Microsoft Visual Studio 2010 x64 tools before calling make."
	@echo "    Call to the shortcuts from the Command Prompt menu under Intel "
	@echo "    Parallel Studio XE"
	@echo ------------------------------------------------------------------------
	@echo PROJECT: Rev: $(SVN_VER), $(PLATFORM_TYPE), HPC \($(HOST_HPC_ROOT)\) $(RUNHOST_HPC)
	@echo Source: $(SRC) -- $(OUT) -- $(OBJ)
	@echo ------------------------------------------------------------------------

# shortcut to build for GNU Fortran
.PHONY: gnu
gnu:
	@$(MAKE) -f $(THIS_FILE) FC=gfortran

# shortcut to build for Intel Fortran
.PHONY: intel
intel:
	@$(MAKE) -f $(THIS_FILE) FC=ifort

# shortcut to build for Solaris Studio Fortran
.PHONY: sun
sun:
	@$(MAKE) -f $(THIS_FILE) FC=f95

# Run through PBS job script on HPC, check if we are on cluster RUNHOST_HPC
#run: $(OUT) $(JOB) $(SRC)
run: $(OUT) $(SRC)
ifeq ($(RUNHOST_HPC),1)
	qsub $(JOB)
	@echo "NOTE: fimm job created"
	@echo "Batch commands:"
	@echo "  showq -u USER = show user's jobs'"
	@echo "  qstat = display all jobs"
	@echo "  qdel jobid = delete job"
	@echo "  qhold = pause job"
	@echo "  qrls = resume job"
	@echo "https://docs.hpc.uib.no/wiki/Job_execution_(Fimm)"
else
	@$(MAKE) -f $(THIS_FILE) run_local
endif

# Note -- this is local start/run on all supported systems.
# If the DEBUG flag was set when starting make, the executable also
# starts in the DEBUG mode (command line DEBUG flag).
run_local: $(OUT) $(SRC)
	$(info ********************************************************************)
	$(info *** Executing model $(OUT) now)
	$(info ********************************************************************)
ifeq ($(IS_WINDOWS),1)
	$(OUT) $(RUNFLAG)
else
	./$(OUT) $(RUNFLAG)
endif

# Main build the executable
exec: $(OUT)

# Build object files only without the final executable
objects: $(OBJ) $(HTOOLOBJ)

# Produce include file for PRNG module
inc: $(AUTOGEN_HEADER_RAND) $(AUTOGEN_HEADER_IEEE)

# Compile the HEDTOOLS modelling tools
tools: $(HTOOLOBJ)

# Compile the HEDTOOLS modelling tools
headtools: $(HTOOLOBJ)

# Cleanup data files, quotes needed on Windows if file starts from blank
cleandata:
	-$(RM) HED*.txt ?HED*.txt HED18*.log behaviour gene*
	-$(RM) debug*  *.csv *.ps *.png *.svg
ifeq ($(IS_WINDOWS),1)
	-$(RM) "?HED*.txt" "HED*.txt"
endif

# Return to default state
distclean: clean cleandata
	-$(RM) $(AUTOGEN_HEADER_RAND) $(AUTOGEN_HEADER_IEEE) $(HTOOLOBJ) $(MOD_ALL) \
					$(IEEEOBJ) $(IEEEMOD) gmon.out *.mod *.log *.o *.obj *.pdb
	-$(RM) $(DOXYPATH)

# Clean obj etc for new build of the model, but we don't clean HEDTOOLS and
# any .mod files.
clean: neat
	-$(RM) $(OBJ) $(OUT) *.csv *.ps *.png

# Cean temporary files and conflict that remain from syncthing synchronisation.
neat:
	-$(RM) $(TMPFILES) *conflict* .syncthing* ~syncthing* fort.* *.tmp *.orig

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Automatic build follows, usually there is no need to edit the code below...

$(OUT): $(OBJ) $(HTOOLOBJ)
	@$(MAKE) -f $(THIS_FILE) inc
ifeq ($(COMPILE_NONINTRINSIC_IEEE),YES)
	$(FC) $(FFLAGS) -o $(OUT) $(DRV) $^ $(IEEEOBJ) $(GRAPHLIB)
else
	$(FC) $(FFLAGS) -o $(OUT) $(DRV) $^ $(GRAPHLIB)
endif

$(OBJ): $(SRC) $(HTOOLOBJ)
ifeq ($(COMPILE_NONINTRINSIC_IEEE),YES)
	@$(MAKE) -f $(THIS_FILE) ieee_need
else
	@$(MAKE) -f $(THIS_FILE) ieee_not_need
endif
	$(FC) $(FFLAGS) -c $^ $(GRAPHLIB)

#-------------------------------------------------------------------------------
# Build HEDTOOLS

# Produce tweaked include file for PRNG
$(AUTOGEN_HEADER_RAND): $(BASE_RANDOM.f90) $(THIS_FILE)
	$(AUTOGEN_COMMENT_RANDOM)
	$(AUTOGEN_CODE_RANDOM)
	@echo Generated include: $(AUTOGEN_HEADER_RAND) for $(FC)

# compile HEDTOOLS modules. Note that the extension (.o or .obj)
# is set via $(OBJEXT). .obj is used on Intel Fortran on Windows.
# Also note that generic pattern rules are used here, with the
# include files as additional prerequisites.
%.$(OBJEXT) : %.f90 $(AUTOGEN_HEADER_RAND) $(AUTOGEN_HEADER_IEEE)
	$(FC) $(FFLAGS) -c $<

#-------------------------------------------------------------------------------
# Build IEEE non-intrinsic modules for GNU Fortran if needed

# Generate IEEE include file
$(AUTOGEN_HEADER_IEEE): $(THIS_FILE)
ifeq ($(COMPILE_NONINTRINSIC_IEEE),YES)
	$(AUTOGEN_NON_INTRINSIC)
	@echo Generated include: $(AUTOGEN_HEADER_IEEE) for $(FC) v. $(GFORTVERSION)
else
	$(AUTOGEN_INTRINSIC)
	@echo Generated include: $(AUTOGEN_HEADER_IEEE) for $(FC)
endif

#-------------------------------------------------------------------------------
# Compile all IEEE non-intrinsic math modules if needed
ieee:
ifeq ($(COMPILE_NONINTRINSIC_IEEE),YES)
	@$(MAKE) -f $(THIS_FILE) ieee_need
else
	@$(MAKE) -f $(THIS_FILE) ieee_not_need
endif

# just issue note that we do not need IEEEs
ieee_not_need:
	@echo ------------------------------------------------------------------------
	@echo We do not need non-intrinsic IEEE modules: $(FC), grep test $(IEEE_GREPPED_SRC)
	@echo ------------------------------------------------------------------------

# build IEEEs assuming we do need them (GNU)
ieee_need: $(IEEEOBJ)
	@echo ------------------------------------------------------------------------
	@echo GNU Fortran version: $(GFORTVERSION). We NEED NON-INTRINSIC IEEE MODULES.
	@echo ------------------------------------------------------------------------

# specific components of IEEE modules are below
precision_m.$(OBJEXT): precision_m.f90
	$(FC) -c $(FFLAGS) $<
c_control.$(OBJEXT): c_control.c
	$(CC) -c $(GC_FFLAGS) $<
IEEE_FEATURES.$(OBJEXT): IEEE_FEATURES.f90
	$(FC) -c -fno-range-check $(FFLAGS) $<
IEEE_EXCEPTIONS.$(OBJEXT): IEEE_EXCEPTIONS.f90
	$(FC) -c -fno-range-check $(FFLAGS) $<
IEEE_ARITHMETIC.$(OBJEXT): IEEE_ARITHMETIC.f90
	$(FC) -c -fno-range-check $(FFLAGS) $<

##//! @endcond
