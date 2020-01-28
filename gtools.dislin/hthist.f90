!-------------------------------------------------------------------------------
! SVN version info:
! $Id$
!-------------------------------------------------------------------------------
! Produce a histogram of data that are provided at the command line.
! If the output plot file name is not provided, the plot goes to the
! screen (X11 on Unix, GrWin on Windows).
!
! Input: (1) column, (2) input CSV filename, (3) output plot file name.
!
! Example:
!  hthist 1 data_file_csv plot_file.png
!
! BUILD: The program can be built using the standard Makefile from the
!        HEDTOOLS subproject, but needs tweaking certain build options to plug
!        the DISLIN.
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
character(len=*), parameter :: SVN_REVISION = "$Revision$"

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

! Column number to show
integer :: hcolumn, error_int

! Minimum length of the output file, if too short, ignore output
! file name. 6 chars: xx.png
integer, parameter :: MIN_FILE_LENGTH = 5

! Default output file name if no graphics is available
character(len=*), parameter :: OUT_FILE_NO_GRAPHICS   = 'plot'

! File read error flag
logical :: errorflag

! Input data matrix from CSV file.
real, dimension(:,:), allocatable :: data_matrix

! The maximum number of points when large points are used for plotting,
!  if the n of rows in the data exceed this value, small dots are used.
integer, parameter :: SMALL_SAMPLE = 50
integer :: bins_n

! Arrays to keep histogram group data
real, dimension(:), allocatable :: hist_ranges
integer, dimension(:), allocatable :: hist_buckets
real :: hist_delta

! Temporary
integer :: i

!-------------------------------------------------------------------------------

! The default graphic output device is different for different platforms.
if ( graphics_is_available() ) then
  output_dev  = DEV_XWIN   ! Default output device on Microsoft Windows.
else
  output_dev  = DEV_PDF    ! Default output device is PDF.
end if

! Default output device for file save. Default PostScript, but can be PNG
! (= DEV_PNG then).
output_save = DEV_PDF

if (command_argument_count()==0) then
  write(ERROR_UNIT,*) "ERROR: Required command line arguments not found."
  print *, "-------------------------------------------------"
  print *, "*** HTHIST --- ", SVN_REVISION, " (", GRAPHICS_LIB, ")"
  print *, ""
  print *, "Produce a histogram  of data that are provided at the command line."
  print *, "If the output plot file name is not provided, the plot goes to the "
  print *, "screen (X11 on Unix)."
  print *, "File formats supported: .PDF .PS .PNG .EPS .GIF .HPGL, .SVG, .WMF."
  print *, ""
  print *, "Input: (1) Column number in the input file, (2) input CSV filename,"
  print *, "       (3) output plot file name."
  print *, ""
  print *, "Example:"
  print *, " hthist 2 data_file_csv plot_file.png"
  print *, ""
  if ( .not. graphics_is_available() )                                        &
                                 print *, "NOTE: graphics is NOT available."
  print *, ""
  stop
end if

! Process command line arguments...
call get_command_argument(number=1, value=command_line_str)
if (IS_DEBUG) print *, "DEBUG: >",trim(command_line_str), "<"
if (.not. IS_NUMERIC(trim(command_line_str))) then
  write(ERROR_UNIT,*) "ERROR: first argument is not a number: ",              &
                      trim(command_line_str)
  stop EXIT_CODE_ERROR
end if
if (len_trim(command_line_str)>0) then
  call VALUE(command_line_str, hcolumn, error_int)
  if (error_int /=0 ) then
    write(ERROR_UNIT,*) "ERROR: cannot convert first parameter to number ",   &
                        error_int
    stop EXIT_CODE_ERROR
  end if
else
  write(ERROR_UNIT,*) "ERROR: cannot convert first parameter to number."
  stop EXIT_CODE_ERROR
end if
if (IS_DEBUG) print *, "DEBUG: > column:", hcolumn, error_int

call get_command_argument(number=2, value=command_line_str)
if (IS_DEBUG) print *, "DEBUG: >",trim(command_line_str), "<"
csv_file_name = trim(command_line_str)


call get_command_argument(number=3, value=command_line_str)
if (IS_DEBUG) print *, "DEBUG: >",trim(command_line_str), "<"
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

if (IS_DEBUG) print *, "DEBUG: >", output_dev, ", ", output_file

! Read CSV data from the file.

data_matrix = CSV_MATRIX_READ(csv_file_name, errorflag)
if (.not. errorflag) call error_csv(csv_file_name)
if (IS_DEBUG) print *, "DEBUG: >", size(data_matrix,1),"/",size(data_matrix,2)

! hcolumn cannot exceed that in size of the data
if (hcolumn>size(data_matrix,2)) hcolumn=size(data_matrix,2)
if (IS_DEBUG) print *, "DEBUG: > column:", hcolumn

! N of bins depends on the sample size.
if (size(data_matrix, 1) < SMALL_SAMPLE) then
  bins_n = 5 - 1
else
  bins_n = 20 - 1
end if
if (IS_DEBUG) print *, "DEBUG: bins>", bins_n

if (IS_DEBUG) print *, "DEBUG: min/max>", minval(data_matrix(:,hcolumn)),     &
                                          maxval(data_matrix(:,hcolumn))

! Calculate the histogram groups
allocate( hist_ranges(bins_n) )
allocate( hist_buckets(bins_n+1) )

hist_delta = (maxval(data_matrix(:,hcolumn))-minval(data_matrix(:,hcolumn)))  &
              / bins_n

hist_ranges(1) = minval(data_matrix(:,hcolumn)) + hist_delta

do i = 2, bins_n
  hist_ranges(i) = hist_ranges(i-1) + hist_delta
end do

if (IS_DEBUG) print *, "DEBUG: groups: ", hist_ranges

call distribute( data_matrix(:,hcolumn), size(data_matrix ,1),                &
                 hist_ranges, bins_n, hist_buckets )

if (IS_DEBUG) print *, "DEBUG: count data: ",  hist_buckets

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

  call name('Bins: ' // ' min=' // TOSTR(minval(data_matrix(:,hcolumn))) //   &
             ', max=' // TOSTR(maxval(data_matrix(:,hcolumn))) , 'X')
  call name('Count', 'Y')
  call titlin( 'Histogram of ' //   &
                csv_file_name // ', col: ' // TOSTR(hcolumn) //               &
                '. Mean=' // TOSTR(mean_val(data_matrix(:,hcolumn))) //       &
                ', N=' // TOSTR(size(data_matrix,1))  , 1 )

  call qplbar( real( hist_buckets ), bins_n+1 )

!end block PLOT_DISLIN
!-------------------------------------------------------------------------------
! *** End of the DISLIN block
!-------------------------------------------------------------------------------


contains !----------------------------------------------------------------------

function mean_val(xarray) result (mean_out)
  real, dimension(:), intent(in) :: xarray
  real :: mean_out
  real, parameter :: MISSING=-9999.0

  mean_out = sum(xarray, xarray/=MISSING) / count(xarray/=MISSING)

end function mean_val

!- - - - - - - - - - - - - - - - - - - - - - - - -  - - - - - - - - - - - - -

subroutine error_csv(filename)
  character(len=*), intent(in) ::filename

  write(ERROR_UNIT,*) "ERROR: Error reading file ", filename
  stop EXIT_CODE_ERROR

end subroutine error_csv

!- - - - - - - - - - - - - - - - - - - - - - - - -  - - - - - - - - - - - - -

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


! --------------------------------------------------------------------
! distribute() :
! This subroutine receives a score array and a range array, counts
! the number of each scores in each range.
! https://pages.mtu.edu/~shene/COURSES/cs201/NOTES/chap08/hist-2.html
! --------------------------------------------------------------------
subroutine  distribute(x, n, range, m, bucket)
  implicit none

  real,    dimension(1:), intent(in) :: x     ! input score
  integer, intent(in)                :: n     ! # of scores
  real,    dimension(1:), intent(in) :: range ! range array
  integer, intent(in)                :: m     ! # of ranges
  integer                            :: i, j
  integer, intent(out), dimension(1:m+1)  :: bucket! counting bucket

  do i = 1, m+1                         ! clear buckets
   bucket(i) = 0
  end do

  do i = 1, n                           ! for each input score
   do j = 1, m                          ! determine the bucket
    if (x(i) < range(j)) then
      bucket(j) = bucket(j) + 1
      exit
    end if
   end do                               ! don't forget the last bucket
   if (x(i) >= range(m))  bucket(m+1) = bucket(m+1)+1
  end do

end subroutine  distribute

end program scatter
