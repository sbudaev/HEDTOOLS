!-------------------------------------------------------------------------------
! SVN version info:
! $Id$
!-------------------------------------------------------------------------------
! Produce scatter plot of data that are provided at the command line.
! If the output plot file name is not provided, the plot goes to the
! screen (X11 on Unix, GrWin on Windows).
!
! Input: (1) input CSV filename, (2) output plot file name.
!
! Example:
!  htscatterplot data_file_csv plot_file.png
!
! BUILD: The program can be built using the standard Makefile from the
!        HEDTOOLS subproject, but needs tweaking certain build options to plug
!        the PGLIB.
!
! Build command on *Linux* with *gfortran* and PGPLOT (other compilers like
! Oracle f95 also do work as usual with FC=f95):
!   make GRAPHLIB=-lpgplot SRC=htscatter.f90 OUT=htscatter.exe
!
! Build on *Windows* using *gfortran* and GrWin:
!   make GF_FFLAGS="-LC:/GrWin/MinGW_gfortran_x6/lib -Wl,--subsystem,console
!         -O3 -funroll-loops -fforce-addr -mwindows" GRAPHLIB="-lpgplot -lGrWin"
!         SRC=htscatter.f90 OUT=htscatter.exe
!
! Build on *Windows* using *ifort* and GrWin (builds but failed to run):
!   set LIB=%LIB%;C:/GrWin/Intel6/lib
!   make FC=ifort GRAPHLIB="pgplot.lib GrWin.lib user32.lib gdi32.lib
!         Advapi32.lib" SRC=htscatter.f90 OUT=htscatter.exe
!
! NOTE:    Depends on the HEDTOOLS and PGPLOT Fortran library for plotting,
!          on Windows may also require GrWin.
!
! WARNING: Building PGPLOTS on Windows platform might be non-trivial, also
!          GUI window output interface (/XWINDOW) is not easily available,
!          although PostScript (/PS) file output should work.
!          The easiest way to get PGPLOT on Microsoft Windows is the
!          GrWin/GrWinC library by Tsuguhiro Tamaribuchi, freely available at
!          http://spdg1.sci.shizuoka.ac.jp/grwinlib/english/ . The program
!          uses the /GR device that is available on Microsoft Windows from the
!          GrWin library.
!          Caveat: GrWin does not support any non-interactive devices other
!                  than PostScript (.ps output) on Windows.
!
! Author: Sergey Budaev <sergey.budaev@uib.no>
!-------------------------------------------------------------------------------
program scatter

use, intrinsic :: ISO_FORTRAN_ENV, only : ERROR_UNIT
use CSV_IO
use BASE_UTILS
use BASE_STRINGS
implicit none

! Set DEBUG mode. Debug mode prints more diagnostics.
logical, parameter :: IS_DEBUG = .FALSE.

! Command line argument(s)
character(len=255) :: command_line_str !> Note: allocatable doesn't work

integer :: pgopen ! pgplot function is integer type.

! Output devices. See PGPLOT docs or set ? for runtime choice.
! NOTE: /XWINDOW is the standard native X11 graphics device for Unix systems.
!       /GW is the native graphics device for Microsoft Windows, requires
!       separate GrWin/GrWinC library.
character(len=*), parameter :: DEV_UNDEF = '?',        EXT_UNDEF = "" ! not file
character(len=*), parameter :: DEV_XWIN  = '/XWINDOW', EXT_XWIN  = "" ! not file
character(len=*), parameter :: DEV_GW    = '/GW',      EXT_GW    = "" ! not file
character(len=*), parameter :: DEV_PS    = '/PS',      EXT_PS    = ".ps"
character(len=*), parameter :: DEV_PSV   = '/VPS',     EXT_VPS   = ".ps"
character(len=*), parameter :: DEV_PNG   = '/PNG',     EXT_PNG   = ".png"
character(len=*), parameter :: DEV_TPNG  = '/TPNG',    EXT_TPNG  = ".png"
character(len=*), parameter :: DEV_GIF   = '/GIF',     EXT_GIF   = ".gif"
character(len=*), parameter :: DEV_VGIF  = '/VGIF',    EXT_VGIF  = ".gif"
character(len=*), parameter :: DEV_HPGL  = '/HPGL',    EXT_HPGL  = ".hpgl"
character(len=*), parameter :: DEV_HPGL2 = '/HPGL2',   EXT_HPGL2 = ".hpplot"

! Output device.
character(len=:), allocatable :: pg_default_name, output_dev, output_save

! Exit codes.
integer, parameter :: EXIT_CODE_CLEAN = 0
integer, parameter :: EXIT_CODE_ERROR = 1

! Input CSV file name with the data, output plot file
character(len=:), allocatable :: csv_file_name, output_file

! Minimum length of the output file, if too short, ignore output
! file name. 6 chars: xx.png
integer, parameter :: MIN_FILE_LENGTH = 6

! File read error flag
logical :: errorflag

! Input data matrix from CSV file.
real, dimension(:,:), allocatable :: data_matrix

! The maximum number of points when large points are used for plotting,
!  if the n of rows in the data exceed this value, small dots are used.
integer, parameter :: SMALL_DOTS_MIN = 500
integer, parameter :: DOTS_SMALL=1, DOTS_BIG=8
integer :: plot_symbol

!-------------------------------------------------------------------------------

! The default graphic output device is different for different platforms.
if (PLATFORM_IS_WINDOWS()) then
  output_dev  = DEV_GW   ! Default output device is GrWin on Microsoft Windows.
else
  output_dev  = DEV_XWIN ! Default output device is X11.
end if

! Default output device for file save. Default PostScript, but can be PNG
! (= DEV_PNG then).
output_save = DEV_PS

! Output file name in PGPLOT library always has the same name. EXT_xx should
! agree with the default file output device that is set above by output_save.
! For example, output_save = DEV_PS leads to ... EXT_PS
pg_default_name = "pgplot" // EXT_PS

if (IS_DEBUG) print *, "DEBUG: ", pg_default_name

if (command_argument_count()==0) then
  write(ERROR_UNIT,*) "ERROR: Required command line arguments not found."
  print *, "-------------------------------------------------"
  print *, ""
  print *, "Produce scatter plot of data that are provided at the command line."
  print *, "If the output plot file name is not provided, the plot goes to the "
  print *, "screen (X11 on Unix, GrWin on Windows)."
  if (PLATFORM_IS_WINDOWS()) then                       ! GrWin supports only
    print *, "File formats supported: .PS."             ! PostScript format.
  else
    print *, "File formats supported: .PS, .HPGL, .HPPLOT, .PNG, .GIF ."
  end if
  print *, ""
  print *, "Input: (1) input CSV filename, (2) output plot file name."
  print *, ""
  print *, "Example:"
  print *, " htscatterplot data_file_csv plot_file.png"
  print *, ""
  stop
end if

! Process command line arguments...
call get_command_argument(number=1, value=command_line_str)
if (IS_DEBUG) print *, "DEBUG: >",trim(command_line_str), "<"
csv_file_name = trim(command_line_str)


call get_command_argument(number=2, value=command_line_str)
if (IS_DEBUG) print *, "DEBUG: >",trim(command_line_str), "<"
output_file = trim(command_line_str)

if (len(output_file)>MIN_FILE_LENGTH) then
  output_dev = output_save

  OUT_FORMAT: if                                                              &
    (LOWERCASE(output_file(len(output_file)-2:len(output_file)))==EXT_PS)     &
    then
      output_save = DEV_PS
      output_dev=output_save
      pg_default_name = "pgplot" // EXT_PS
    else if                                                                   &
    (LOWERCASE(output_file(len(output_file)-3:len(output_file)))==EXT_PNG)    &
    then OUT_FORMAT
      output_save = DEV_PNG
      output_dev=output_save
      pg_default_name = "pgplot" // EXT_PNG
    else if                                                                   &
    (LOWERCASE(output_file(len(output_file)-3:len(output_file)))==EXT_GIF)    &
    then OUT_FORMAT
      output_save = DEV_GIF
      output_dev=output_save
      pg_default_name = "pgplot" // EXT_GIF
    else if                                                                   &
    (LOWERCASE(output_file(len(output_file)-4:len(output_file)))==EXT_HPGL)   &
    then OUT_FORMAT
      output_save = DEV_HPGL
      output_dev=output_save
      pg_default_name = "pgplot" // EXT_HPGL
    else if                                                                   &
    (LOWERCASE(output_file(len(output_file)-6:len(output_file)))==EXT_HPGL2)  &
    then OUT_FORMAT
      output_save = DEV_HPGL2
      output_dev=output_save
      pg_default_name = "pgplot" // EXT_HPGL2
    else
      output_save = DEV_PS
      output_dev=output_save
      pg_default_name = "pgplot" // EXT_PS
  end if OUT_FORMAT

  if (IS_DEBUG) print *, "DEBUG: >", output_dev, ", ", output_file

end if

! Read CSV data from the file.

data_matrix = CSV_MATRIX_READ(csv_file_name, errorflag)
if (.not. errorflag) call error_csv(csv_file_name)
if (IS_DEBUG) print *, "DEBUG: >", size(data_matrix,1), "/", size(data_matrix,2)

! Dot size depends on the sdample size.
if (size(data_matrix, 1) < SMALL_DOTS_MIN) then
  plot_symbol = DOTS_BIG
else
  plot_symbol = DOTS_SMALL
end if
if (IS_DEBUG) print *, "DEBUG: >", plot_symbol

! Produce the plot itself -- using PGPLOT library.
if (pgopen(output_dev) .lt. 1) then
  write(ERROR_UNIT,*) "ERROR: Cannot open output device ", output_dev
  stop EXIT_CODE_ERROR
end if

call pgenv( minval(data_matrix(:,1)), maxval(data_matrix(:,1)),               &
            minval(data_matrix(:,2)), maxval(data_matrix(:,2)), 0, 0 )

if (IS_DEBUG) print *, "DEBUG: >",    minval(data_matrix(:,1)),               &
                                      maxval(data_matrix(:,1)),               &
                                      minval(data_matrix(:,2)),               &
                                      maxval(data_matrix(:,2))

call pglab('X', 'Y', 'Scatterplot ' // output_file)

! Plot the scatterplot based on the first two columns of the data.
call pgpt(size(data_matrix,1), data_matrix(:,1), data_matrix(:,2), plot_symbol)

! Close the plot
call pgclos

! Rename the output file.
! WARNING:: rename subroutine is GNU extension and may not be available
!           on all compiler systems. Does work with gfortran, Oracle f95
if (output_dev /= DEV_XWIN .and. output_dev /= DEV_GW ) then
  call rename (pg_default_name, output_file)
  print *, "Wrote plot to output file (", output_dev, "): " , output_file
end if

contains !----------------------------------------------------------------------

subroutine error_csv(filename)
  character(len=*), intent(in) ::filename

  write(ERROR_UNIT,*) "ERROR: Error reading file ", filename
  stop EXIT_CODE_ERROR

end subroutine error_csv


end program scatter
