#-------------------------------------------------------------------------------
# SVN version info:
# $Id$
#-------------------------------------------------------------------------------
# Build Modelling tools as a static and shared libraries, produce doc file (pdf)
#-------------------------------------------------------------------------------

# Supported Fortran compiler types
GF_FC = gfortran
IF_FC = ifort
SF_FC = f95

# Choose the compiler type
FC = $(GF_FC)

#*******************************************************************************

# Main building blocks, define
SRC = BASE_UTILS.f90 BASE_CSV_IO.f90 BASE_LOGGER.f90
OBJ = BASE_UTILS.o BASE_CSV_IO.o BASE_LOGGER.o
MOD = base_utils.mod  csv_io.mod  logger.mod
DOC = BASE_UTILS.adoc
LIB = lib_hedutils.a
DIB = lib_hedutils.so

#-------------------------------------------------------------------------------

# Options for GNU Fortran
GF_STATIC = -static-libgfortran -static -static-libgcc
GF_TRAPS = -ffpe-trap=
GF_RCHECKS = -Wall -fbounds-check
GF_FFLAGS = $(GF_STATIC) $(GF_TRAPS) $(GF_RCHECKS) -O3 -fPIC
GF_STLIBBLD = ar rc $(LIB) $(OBJ)
GF_DYLIBBLD = $(FC) $(GF_TRAPS) -O3 -fPIC -shared -o $(DIB)
#-ffpe-trap=zero,invalid,overflow,underflow
GF_INFO = "Include the library file or -Ldir"

# Options for Intel Fortran
IF_STATIC = -static
IF_TRAPS =-fpe3
IF_RCHECKS = -warn -check bounds,pointers,format,uninit
IF_FFLAGS = -sox -O3 -parallel -fpic $(IF_STATIC) $(IF_TRAPS) $(IF_RCHECKS)
IF_STLIBBLD = ar cr $(LIB) $(OBJ)
IF_DYLIBBLD = $(FC) -sox -O3 -parallel -fpic $(IF_TRAPS) -shared -o $(DIB)
# -fpe3 no traps; -fpe0 all traps
IF_INFO = "Include the library file or -Ldir"

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
SF_INFO = "Include the library file or use -l and -L compile options"

#-------------------------------------------------------------------------------
# Set other build options depending on the specific compiler

ifeq ($(FC),gfortran)
	FFLAGS = $(GF_FFLAGS)
	STLIBBLD =  $(GF_STLIBBLD)
	DYLIBBLD =  $(GF_DYLIBBLD)
	INFO = $(GF_INFO)
endif

ifeq ($(FC),ifort)
	FFLAGS = $(IF_FFLAGS)
	STLIBBLD = $(IF_STLIBBLD)
	DYLIBBLD = $(IF_DYLIBBLD)
	INFO = $(IF_INFO)
endif

ifeq ($(FC),f95)
	FFLAGS = $(SF_FFLAGS)
	STLIBBLD = $(SF_STLIBBLD)
	DYLIBBLD = $(SF_DYLIBBLD)
	INFO = $(SF_INFO)
endif

# DEBUG turns off all optimisations and keeps debug symbols.
ifdef DEBUG
	GF_FFLAGS = -O0 -g -ffpe-trap=zero,invalid,overflow,underflow $(GF_RCHECKS)
	IF_FFLAGS = -O0 -g -fpe0 $(IF_RCHECKS)
	SF_FFLAGS = -O0 -g -ftrap=%all $(SF_RCHECKS)
endif

#-------------------------------------------------------------------------------
# Documentation builder parameters (asciidoc)

DOCFIL = BASE_UTILS
DOCFMT = pdf
DOCDIR = doc/

#-------------------------------------------------------------------------------


# Determine this makefile's path. Be sure to place this BEFORE `include`s
THIS_FILE := $(lastword $(MAKEFILE_LIST))

# This is the search paths for looking for components, separated by blanks
VPATH = $(DOCDIR)

#*******************************************************************************

all: shared

lib: $(LIB)

# Static linking doesn't work well with gnu fortran
static: $(LIB)

shared: $(DIB)

doc: $(DOCFIL).$(DOCFMT)

distclean: neat
	-rm -f $(OBJ) $(MOD) $(LIB) $(DIB) $(DOCDIR)/BASE_UTILS.$(DOCFMT)

# We don't clean .mod files as they are necessary for building with .so
clean: neat
	-rm -f $(OBJ)

neat:
	-rm -f $(TMPFILES) *conflict*

$(LIB): $(OBJ)
	$(STLIBBLD)

$(DIB): $(SRC)
	$(DYLIBBLD) $(SRC)

$(DOCFIL).$(DOCFMT): $(DOCFIL).adoc
	a2x -f$(DOCFMT) BASE_UTILS.adoc
	mv $(DOCFIL).$(DOCFMT) $(DOCDIR)

BASE_UTILS.o: BASE_UTILS.f90
	$(FC) $(FFLAGS) -c BASE_UTILS.f90
BASE_CSV_IO.o: BASE_CSV_IO.f90
	$(FC) $(FFLAGS) -c BASE_CSV_IO.f90
BASE_LOGGER.o: BASE_LOGGER.f90
	$(FC) $(FFLAGS) -c BASE_LOGGER.f90
