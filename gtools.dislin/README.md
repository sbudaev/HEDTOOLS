# Plotting tools #

The directory `tools` contains plotting tools. They are used to produce
various output data-based plots in the *DEBUG* mode of the model. Such
plotting tools are implemented as wholly separate binary executable programs.

- There is **no graphics output** in the model code. This makes the model
  code itself independent on any non-essential third-party plotting or
  graphical tools.

Thus, the model code has maximum portability. This is a crucial requirement
since the model code should build without issues on various platforms and
operating systems.

- The working **model code** is updated and changed frequently, so there
  would be a significant hassle if it required any special tweaks for the
  plotting libraries.

- Plotting **tools**, on the other hand, are fixed and updated very
  rarely. They can be built in the binary form independently on the model
  using a different compiler or not built at all, if debug plots are not
  required. There is, for example, little sense building and using the tools
  on the main HPC model running environment, such as high performance Linux
  cluster, provided no debugging is done here.

Building the plotting tools is based on the **PGPLOT** library
(see [http://www.astro.caltech.edu/~tjp/pgplot/](http://www.astro.caltech.edu/~tjp/pgplot/)).
But it is easy to reimplement them for different toolbox (e.g. PlPlot or
distlin). PGPLOT can often be found in the standard Linux repositories
(e.g. Ubuntu). For Windows, PGPLOT is a part of the GrWin library
(see [http://spdg1.sci.shizuoka.ac.jp/grwinlib/english/](http://spdg1.sci.shizuoka.ac.jp/grwinlib/english/)).
