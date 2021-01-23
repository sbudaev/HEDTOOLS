!-------------------------------------------------------------------------------
! SVN version info:
! $Id: htscatter.f90 9343 2020-01-28 18:58:40Z sbu062 $
!-------------------------------------------------------------------------------
! Produce scatter plot of data that are provided at the command line.
! If the output plot file name is not provided, the plot goes to the
! screen (X11 on Unix).
!
! Input: (1) input CSV filename, (2) output plot file name.
!
! Example:
!  htscatterplot data_file_csv plot_file.png
!
! BUILD: The program can be built using the standard Makefile from the
!        HEDTOOLS subproject, but needs tweaking certain build options to plug
!        the DISLIN
!
! NOTE:    Depends on the HEDTOOLS and DISLIN Fortran library for plotting,
!
! Author: Sergey Budaev <sergey.budaev@uib.no>
!-------------------------------------------------------------------------------
program scatter

use, intrinsic :: ISO_FORTRAN_ENV, only : ERROR_UNIT
use CSV_IO
use BASE_UTILS
use BASE_STRINGS
implicit none

! Warning: SVN_REVISION is updated automatically by Subversion at commit.
!          do not edit manually!
character(len=*), parameter :: SVN_REVISION = "$Revision: 9343 $"

! The name of the graphics library used for plotting.
character(len=*), parameter :: GRAPHICS_LIB = "DISLIN"

! Set DEBUG mode. Debug mode prints more diagnostics.
logical, parameter :: IS_DEBUG = .FALSE.

! Command line argument(s)
character(len=255) :: command_line_str !> Note: allocatable doesn't work

! Output devices (Metafile). See DISLIN docs.
! NOTE: XWIN is the standard native X11 graphics device for Unix systems.
!       CON is the full screen graphics device.
character(len=*), parameter :: DEV_UNDEF = '?',        EXT_UNDEF = "" ! not file
character(len=*), parameter :: DEV_XWIN  = 'XWIN',     EXT_XWIN  = "" ! not file
character(len=*), parameter :: DEV_CON   = 'CONS',     EXT_CON   = "" ! not file
character(len=*), parameter :: DEV_PS    = 'PS',       EXT_PS    = ".ps"
character(len=*), parameter :: DEV_PDF   = 'PDF',      EXT_PDF   = ".pdf"
character(len=*), parameter :: DEV_PNG   = 'PNG',      EXT_PNG   = ".png"
character(len=*), parameter :: DEV_EPS   = 'EPS',      EXT_EPS   = ".eps"
character(len=*), parameter :: DEV_GIF   = 'GIF',      EXT_GIF   = ".gif"
character(len=*), parameter :: DEV_HPGL  = 'HPGL',     EXT_HPGL  = ".hpgl"
character(len=*), parameter :: DEV_SVG   = 'SVG',      EXT_SVG   = ".svg"
character(len=*), parameter :: DEV_WMF   = 'WMF',      EXT_WMF   = ".wmf"

! Output device.
character(len=:), allocatable :: output_dev, output_save

! Exit codes.
integer, parameter :: EXIT_CODE_CLEAN = 0
integer, parameter :: EXIT_CODE_ERROR = 1

! Input CSV file name with the data, output plot file
character(len=:), allocatable :: csv_file_name, output_file

! Minimum length of the output file, if too short, ignore output
! file name. 6 chars: xx.png
integer, parameter :: MIN_FILE_LENGTH = 5

! Default output file name if no graphics is available
character(len=*), parameter :: OUT_FILE_NO_GRAPHICS   = 'plot'

! File read error flag
logical :: errorflag

! Input data matrix from CSV file.
real, dimension(:,:), allocatable :: data_matrix

!-------------------------------------------------------------------------------

! The default graphic output device is different for different platforms.
if ( graphics_is_available() ) then
  output_dev  = DEV_XWIN   ! Default output device on Microsoft Windows.
else
  output_dev  = DEV_PDF    ! Default output device is PDF.
end if

! Default output device for file save.
! (= DEV_PNG then).
output_save = DEV_PDF

if (command_argument_count()==0) then
  write(ERROR_UNIT,*) "ERROR: Required command line arguments not found."
  print *, "-------------------------------------------------"
  print *, "*** HTSCATTER --- ", SVN_REVISION, " (", GRAPHICS_LIB, ")"
  print *, ""
  print *, "Produce scatter plot of data that are provided at the command line."
  print *, "If the output plot file name is not provided, the plot goes to the "
  print *, "screen (X11 on Unix)."
  print *, "File formats supported: .PDF .PS .PNG .EPS .GIF .HPGL, .SVG, .WMF."
  print *, ""
  print *, "Input: (1) input CSV filename, (2) output plot file name."
  print *, ""
  print *, "Example:"
  print *, " htscatter data_file_csv plot_file.png"
  print *, ""
  if ( .not. graphics_is_available() )                                        &
                                 print *, "NOTE: graphics is NOT available."
  print *, ""
  stop
end if

! Process command line arguments...
call get_command_argument(number=1, value=command_line_str)
if (IS_DEBUG) print *, "DEBUG 1: >",trim(command_line_str), "<"
csv_file_name = trim(command_line_str)

call get_command_argument(number=2, value=command_line_str)
if (IS_DEBUG) print *, "DEBUG 2: >",trim(command_line_str), "<"
output_file = trim(command_line_str)

if (len(output_file)>MIN_FILE_LENGTH) then
  output_dev = output_save

  ! Determine the output format depending on the last symbols of
  ! the file name (extension).
  OUT_FORMAT: if                                                              &
    (LOWERCASE(output_file(len(output_file)-2:len(output_file)))==EXT_PS)     &
    then
      output_save = DEV_PS
      output_dev=output_save
    else if                                                                   &
    (LOWERCASE(output_file(len(output_file)-3:len(output_file)))==EXT_PDF)    &
    then OUT_FORMAT
      output_save = DEV_PDF
      output_dev=output_save
    else if                                                                   &
    (LOWERCASE(output_file(len(output_file)-3:len(output_file)))==EXT_PNG)    &
    then OUT_FORMAT
      output_save = DEV_PNG
      output_dev=output_save
    else if                                                                   &
    (LOWERCASE(output_file(len(output_file)-3:len(output_file)))==EXT_EPS)    &
    then OUT_FORMAT
      output_save = DEV_EPS
      output_dev=output_save
    else if                                                                   &
    (LOWERCASE(output_file(len(output_file)-3:len(output_file)))==EXT_GIF)    &
    then OUT_FORMAT
      output_save = DEV_GIF
      output_dev=output_save
    else if                                                                   &
    (LOWERCASE(output_file(len(output_file)-4:len(output_file)))==EXT_HPGL)   &
    then OUT_FORMAT
      output_save = DEV_HPGL
      output_dev=output_save
    else if                                                                   &
    (LOWERCASE(output_file(len(output_file)-3:len(output_file)))==EXT_SVG)    &
    then OUT_FORMAT
      output_save = DEV_SVG
      output_dev=output_save
    else if                                                                   &
    (LOWERCASE(output_file(len(output_file)-3:len(output_file)))==EXT_WMF)    &
    then OUT_FORMAT
      output_save = DEV_WMF
      output_dev=output_save
    else
      output_save = DEV_PDF
      output_dev=output_save
  end if OUT_FORMAT

else if ( .not. graphics_is_available() ) then

  output_file = OUT_FILE_NO_GRAPHICS // EXT_PDF

end if

if (IS_DEBUG) print *, "DEBUG 3: >", output_dev, ", ", output_file

! Read CSV data from the file.

data_matrix = CSV_MATRIX_READ(csv_file_name, errorflag)
if (.not. errorflag) call error_csv(csv_file_name)
if (IS_DEBUG) print *, "DEBUG 4: >", size(data_matrix,1),"/",size(data_matrix,2)

if (IS_DEBUG) print *, "DEBUG 5: >",  minval(data_matrix(:,1)),               &
                                      maxval(data_matrix(:,1)),               &
                                      minval(data_matrix(:,2)),               &
                                      maxval(data_matrix(:,2))

!-------------------------------------------------------------------------------
! *** Use DISLIN library functions to do the plot.
! Note: block construct is F2008 and might not be supported by all compilers
!       and systems. In such a case comment it out (along with end block) as
!       it is not essential.
!-------------------------------------------------------------------------------
!block PLOT_DISLIN

  call setfil( output_file ) ! set file name
  call metafl( output_dev )
  call disini()

  call errmod('ALL', 'OFF') ! Disable all messages (goes after disini)

  call titlin( "Scatterplot: " // csv_file_name //                            &
               " (" // TOSTR(size(data_matrix,1)) // "," //                   &
               TOSTR(size(data_matrix,2)) // ")" , 1 )

  call qplsca( data_matrix(:,1), data_matrix(:,2), size(data_matrix,1) )

!end block PLOT_DISLIN
!-------------------------------------------------------------------------------
! *** End of the DISLIN block
!-------------------------------------------------------------------------------

contains !----------------------------------------------------------------------

subroutine error_csv(filename)
  character(len=*), intent(in) ::filename

  write(ERROR_UNIT,*) "ERROR: Error reading file ", filename
  stop EXIT_CODE_ERROR

end subroutine error_csv


!> Determine if graphics is available. If graphics is available, then GUI
!! mode is possible and plots can be produced even in the CMD mode.
function graphics_is_available() result (is_available)
  logical :: is_available
  character(len=255) :: t_string
  integer, parameter :: DISP_MIN_LIMIT = 4 ! for :0.0 is 4

  !> - If the program is running under Windows, it should be TRUE.
  if ( PLATFORM_IS_WINDOWS() ) then
    is_available = .TRUE.
  !> - Under Unix systems, it depends on the X availability and
  !! configuration, as set by the `DISPLAY` environment variable.
  !!  TODO: What is under OSX??
  else
    call get_environment_variable(name="DISPLAY", value=t_string)
    t_string = LOWERCASE(t_string)
    if ( len_trim(t_string) < DISP_MIN_LIMIT ) then
      is_available = .FALSE.
    else
      is_available = .TRUE.
    end if
  end if

end function graphics_is_available

end program scatter
