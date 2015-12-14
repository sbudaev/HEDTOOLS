This subdirectory will contain documentation for the modules. 

Almost Nothing so far...

1. programming tools and tips.

Most of the time use GNU Fortran (gfortran), test with Intel ifort and Oracle 
  f95. GNU Fortran is trivial to install on most Linux distros. But may be 
  quite tricky on Windows as there are numerous components in the GCC, plus 
  automake, GDB debugger and various additional tools that need to be 
  downloaded and installed separately (in some cases built from sources). 
  
  An easy way to get gfortran together with all other GCC tools, gdb and make 
  utilities is from this web site:
  
  http://www.equation.com/servlet/equation.cmd?fa=fortran
  
  It is packaged into a single bundle easy to install. 
  NOTE: Use v.5  and higher as it supports intrinsic IEEE math modules

...
  
Reading logs. There is a very useful utility called follow that reads text logs 
  that the model will provide... Downloadable from: 
  
  http://sourceforge.net/projects/follow/
  
  It is written in Java and runs everywhere (e.g. Linux and Windows). The 
  simplest way to get log on the terminal in Linus is: tail -f some_log_file.txt
  
  
  
  

