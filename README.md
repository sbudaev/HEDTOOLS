# HEDTOOLS #

HEDTOOLS is a set of modelling utilities and tools (implemented as portable
Fortran modules, not object-oriented) that have general applicability and
are used for data conversion, input/output, random number generation and
execution logging. There are also a few computational tools. Even though
HEDTOOLS modules are primarily designed to be used in the AHA Model, they
are designed such that they can be used in many different simulation projects.

 - [The AHA! Project Development Pages](http://ahamodel.uib.no)

# Credits #

HEDTOOLS use modules and pieces from these sources:

- [FLIBS - A collection of Fortran modules, by by Arjen Markus and Michael Baudin](http://flibs.sourceforge.net/);
- [Source Codes in Fortran90 by John Burkardt](https://people.sc.fsu.edu/~jburkardt/f_src/f_src.html);
- [Fortran Strings Module by George Benthien](http://gbenthien.net/strings/str-index.html);
- [ORDERPACK 2.0 by Michel Olagnon](http://www.fortran-2000.com/rank/);
- [Linear interpolation by David G. Simpson](http://www.davidgsimpson.com/software/linterpol_f90.txt);
- [Non-intrinsic IEEE modules for gfortran by R. J. Hanson](http://mathalacarte.com/hpcconsult).

# License #

The code is covered by the [GNU GPL 3.0](https://www.gnu.org/licenses/gpl-3.0.en.html)
or [GNU LGPL 3.0](https://www.gnu.org/licenses/lgpl-3.0.en.html) license.

# Building HEDTOOLS as a static library #

Normally HEDTOOLS are intended for use in a particular modelling project as
source code. Consult the [Makefile](http://ahamodel.uib.no/doxydoc/Makefile.html)
that is provided in the AHA Model and the AHA Model
[README](http://ahamodel.uib.no/doxydoc/md_README.html) for details. A template
Makefile is included in the `template` subdirectory.

The `Makefile` that is found in the HEDTOOLS directory is used to build it as
a static library. Normally, the build system produces a zip archive with a
"distribution" of the static library and headers ready to use in other
projects. Here are the main build commands:

- Build the static library with the default compiler and options: `make`;
- Build the static library using specific compiler, e.g. Intel
  Fortran (ifort): `make FC=ifort`;
- Build plotting tools: `make tools`;
- Clean all temporary files and the library that is built: `make distclean`;
- Short help on the build commands: `make help`.

For more details about using HEDTOOLS as a static library see
[AHA Modelling Tools Manual](http://ahamodel.uib.no/doc/#_building_the_modelling_tools_as_a_static_library).

Recent pre-built versions of the static library for Intel Fortran on Windows 64
are available [here](http://ahamodel.uib.no/#HEDTOOLS_BIN_GET).

# Wiki #

- HEDTOOLS Wiki is [here](https://bitbucket.org/teg_uib/hedtools/wiki/Home).
