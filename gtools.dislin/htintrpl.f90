!-------------------------------------------------------------------------------
! SVN version info:
! $Id: htintrpl.f90 9350 2020-01-29 11:22:46Z sbu062 $
!-------------------------------------------------------------------------------
! Produce interpolation plot of data that are ptovided at the command line
! Input: Two arrays for the interpolation grid X, Y, they must be of the same
!        size,the third array containing the interpolation data.
!        There may be also two other parameters: algorithm (linear or
!        nonlinear) and output file name. Defaults are nonlinear DDPINTERPOL
!        algorithm. Each of the parameters must be enclosed in square brackets.
!
! Example run:
!    Produce interpolation plot with the default non-linear algorithm, output
!    to screen (X11):
! ./htintrpl.exe [1 2 3 4 ] [10., 45., 14., 10.] [2.5 1.9]
!
!    Produce interpolation plot with linear algorithm, output to the file
! ./htintrpl.exe [1 2 3 4 ] [10., 45., 14., 10.] [2.5 1.9] [linear] [file.ps]
!
! Note: Quotes can also be used instead of square brackets, but the shell
!       will normally process and remove them, so that backslash \ would be
!       necessary for correct processing, e.g.:
!       ./htintrpl.exe [1 2 3 4 ] [10., 45., 14., 10.] [2.5 1.9] \"linear\"
!
!
! Author: Sergey Budaev <sergey.budaev@uib.no>
!-------------------------------------------------------------------------------
program INTERPOLATE

use, intrinsic :: ISO_FORTRAN_ENV, only : ERROR_UNIT
use BASE_UTILS
use BASE_STRINGS
implicit none

! Warning: SVN_REVISION is updated automatically by Subversion at commit.
!          do not edit manually!
character(len=*), parameter :: SVN_REVISION = "$Revision: 9350 $"

! The name of the graphics library used for plotting.
character(len=*), parameter :: GRAPHICS_LIB = "DISLIN"

! Set DEBUG mode. Debug mode prints more diagnostics.
logical, parameter :: IS_DEBUG = .FALSE.

! Command line arguments, whole line.
character(len=255*3) :: command_line_str !> Note: allocatable doesn't work
character(len=255), dimension(10) :: command_str
integer :: n_cmds, n_sub, err_flag, n_xx, n_yy

! String array for xx grid array
character(len=255), dimension(100) :: tmp_array_str, xx_array_str, yy_array_str

! Grid values and target interpolation data values
real, dimension(:), allocatable :: xx, yy, xx_interpolate, yy_interpolate

! Logical flag indicating that only the grid curve should be produced.
logical :: is_grid_only

!> Plot scale range coincide with grid values.
real :: min_xx, max_xx

! Number of steps for drawing interpolation line.
integer, parameter :: n_steps = 100

! Plot drawing data
real, dimension(n_steps+1) :: plotx, ploty

real :: step, plotstep

integer :: i, j, k

integer int_alg   ! algorithm number

! Optional output file name.
character(len=:), allocatable :: output_file

! Delimiter characters for command line arguments:
character(len=*), parameter :: STRDEL="[]" // '"' // "'"

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

! Default output file name if no graphics is available
character(len=*), parameter :: OUT_FILE_NO_GRAPHICS   = 'plot' // EXT_PDF

! Interpolation algorithms for selection. Implemented in HEDTOOLS.
integer, parameter :: ALG_DDPI = 1  ! Nonlinear, divided difference.
integer, parameter :: ALG_LIN  = 2  ! Linear.
integer, parameter :: ALG_LAG  = 3  ! Lagrange, array-based.
integer, parameter :: ALG_CSPL = 4  ! Cubic splines, array-based.

! The name of the algorithm:s
character(len=14), dimension(4), parameter :: ALG_NAME = [ "DDPI    ",        &
                                                           "LINEAR  ",        &
                                                           "LAGRANGE",        &
                                                           "SPLINES "  ]

! Exit codes.
integer, parameter :: EXIT_CODE_CLEAN = 0
integer, parameter :: EXIT_CODE_ERROR = 1

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

! Initialise output file.
output_file = OUT_FILE_NO_GRAPHICS

int_alg = ALG_DDPI       ! default algorithm is DDPINTERPOL

k=1

if (IS_DEBUG) print *, "DEBUG: Delimiters: >",STRDEL, "<"

! Process command line arguments...
call get_command(command_line_str)

!> Parse grid arrays enclosed in square brackets.
call PARSE(command_line_str, STRDEL ,command_str, n_cmds)

if (IS_DEBUG) print *, "DEBUG: >",command_line_str, "<"

if (n_cmds==1) then
  write(ERROR_UNIT,*) "ERROR: Required command line arguments not found."
  print *, "-------------------------------------------------"
  print *, "*** HTINTRPL --- ", SVN_REVISION, " (", GRAPHICS_LIB, ")"
  print *, ""
  print *, "Produce interpolation plot of data that are provided at the command line."
  print *, "Interpolation is basedon subroutines from in HEDTOOLS, so debug plots can"
  print *, "easily be generated in the model independently from the code (i.e."
  print *, "no calls to non-portable and potentially platform-specific graphics "
  print *, ""
  print *, "File formats supported: .PDF .PS .PNG .EPS .GIF .HPGL, .SVG, .WMF."
  print *, ""
  print *, "Input: Two arrays for the interpolation grid X, Y, they must be of the same"
  print *, "     size, the third array containing the interpolation data."
  print *, "     There may be also two other parameters: algorithm (linear, nonlinear,"
  print *, "     lagrange,ddp,splines) and output file name. Defaults are nonlinear "
  print *, "     DDPINTERPOL algorithm. Each of the parameters must be enclosed in "
  print *, "     square brackets."
  print *, ""
  print *, "Examples:"
  print *, "* Produce interpolation screen plot with the default non-linear algorithm:"
  print *, "  htintrpl [1 2 3 4 ] [10., 45., 14., 10.] [2.5 1.9]"
  print *, "* Produce interpolation plot with linear algorithm, output to PS vector file:"
  print *, "  htintrpl [1 2 3 4 ] [10., 45., 14., 10.] [2.5 1.9] [linear] [file.ps]"
  print *, "* Produce interpolation plot with linear algorithm, output to PNG image file:"
  print *, "  htintrpl [1 2 3 4 ] [10, 45, 14, 10] [2.5 1.9] [linear] [file.png]"
  print *, "* Produce interpolation screen plot with cubic splines algorithm:"
  print *, "  htintrpl [1 2 3 4 4.5 5] [10, 45, 14, 2.5, 1 0.5] [2.5 1.9] [splines]"
  print *, ""
  if ( .not. graphics_is_available() )                                        &
                                 print *, "NOTE: graphics is NOT available."
  print *, ""
  stop
end if

! Parse substring arrays elclosed into the square brackets.
! NOTE: Star loopfrom 2 as the name of the executable file is also
!       in the command line string parsed as the first substring.
do i=2, n_cmds

  NONZERO: if (len(trim(command_str(i)))>0) then

    ! Print out the string containing the array.
    if (IS_DEBUG) print *, "DEBUG:", i, k,  ">",trim(command_str(i)), "<"
    ! Parse xx grid values, it is the second substring (first is command itself)
    call PARSE( trim(command_str(i)), " ," // STRDEL, tmp_array_str, n_sub )

    ! Parse xx numeric values, it is the 1st non-empty parameter string.
    PARSE_SEL: if (k==1) then
      n_xx = n_sub
      allocate(xx(n_xx))
      do j=1, n_xx
        call VALUE(trim(tmp_array_str(j)), xx(j), err_flag)
      end do

    ! Parse yy numeric values, it is the 2nd non-empty parameter string.
    else if (k==2) then PARSE_SEL
      if (n_sub == n_xx) then
        n_yy = n_sub
        allocate(yy(n_yy))
      else
       write(ERROR_UNIT,*) "ERROR: unequal size grid arrays."
       stop EXIT_CODE_ERROR
      end if
      do j=1, n_yy
        call VALUE(trim(tmp_array_str(j)), yy(j), err_flag)
      end do

    ! Parse the interpolation array x, it is the 3rd non-empty parameter string.
    else if (k==3) then PARSE_SEL
      allocate(xx_interpolate(n_sub))
      allocate(yy_interpolate(n_sub))
      do j=1, n_sub
        call VALUE(trim(tmp_array_str(j)), xx_interpolate(j), err_flag)
      end do

    !> Parse other parameters, they can be either the algorithm or file name
    else PARSE_SEL

      select case (trim(command_str(i)))
        case ("DDPINTERPOL", "nonlinear", "ddp")
          int_alg = ALG_DDPI
        case ("LINTERPOL", "linear")
          int_alg = ALG_LIN
        case ("LAGRANGE", "lagrange")
          int_alg = ALG_LAG
        case ("SPLINES", "splines","CUBIC_SPLINES", "cubic_splines")
          int_alg = ALG_CSPL
        case default
          output_dev=output_save
          output_file=trim(command_str(i))
          ! Check output file extension and select format (vector-based PS or
          ! raster PNG):
          OUT_FORMAT: if                                                      &
            (LOWERCASE(output_file(len(output_file)-2:len(output_file)))==    &
                                                                    EXT_PS)   &
            then
              output_save = DEV_PS
              output_dev=output_save
            else if                                                           &
            (LOWERCASE(output_file(len(output_file)-3:len(output_file)))==    &
                                                                    EXT_PDF)  &
            then OUT_FORMAT
              output_save = DEV_PDF
              output_dev=output_save
            else if                                                           &
            (LOWERCASE(output_file(len(output_file)-3:len(output_file)))==    &
                                                                    EXT_PNG)  &
            then OUT_FORMAT
              output_save = DEV_PNG
              output_dev=output_save
            else if                                                           &
            (LOWERCASE(output_file(len(output_file)-3:len(output_file)))==    &
                                                                    EXT_EPS)  &
            then OUT_FORMAT
              output_save = DEV_EPS
              output_dev=output_save
            else if                                                           &
            (LOWERCASE(output_file(len(output_file)-3:len(output_file)))==    &
                                                                    EXT_GIF)  &
            then OUT_FORMAT
              output_save = DEV_GIF
              output_dev=output_save
            else if                                                           &
            (LOWERCASE(output_file(len(output_file)-4:len(output_file)))==    &
                                                                    EXT_HPGL) &
            then OUT_FORMAT
              output_save = DEV_HPGL
              output_dev=output_save
            else if                                                           &
            (LOWERCASE(output_file(len(output_file)-3:len(output_file)))==    &
                                                                    EXT_SVG)  &
            then OUT_FORMAT
              output_save = DEV_SVG
              output_dev=output_save
            else if                                                           &
            (LOWERCASE(output_file(len(output_file)-3:len(output_file)))==    &
                                                                    EXT_WMF)  &
            then OUT_FORMAT
              output_save = DEV_WMF
              output_dev=output_save
            else
              output_save = DEV_PDF
              output_dev=output_save
          end if OUT_FORMAT
          if (IS_DEBUG) print *, ">", output_save, "<"
        end select

    end if PARSE_SEL

    k=k+1

  end if NONZERO

end do

min_xx = minval(xx)
max_xx = maxval(xx)

is_grid_only = .FALSE.

! If there is no interpolation array provided on the command line, allocate
! a single-value array slightly outside of the grid xx array range, so it
! won't show on the plot.
if (.not. allocated(xx_interpolate)) then
  allocate(xx_interpolate(1)); allocate(yy_interpolate(1))
  xx_interpolate = max_xx + 0.1 * max_xx
  print *, "WARNING: no interpolation array provided. Grid plot only."
  is_grid_only = .TRUE.
end if

plotstep = (max_xx - min_xx) / real(n_steps)

! Produce the grid plot data.
step = min_xx; i = 1
do while (step <= max_xx)
  plotx(i) = step
  if (int_alg==ALG_DDPI) ploty(i) = DDPINTERPOL( xx, yy, plotx(i) )
  if (int_alg==ALG_LIN) ploty(i) = LINTERPOL( xx, yy, plotx(i) )
  step = step + plotstep
  i = i + 1
end do

! Lagrange is array based
if (int_alg==ALG_LAG) ploty = LAGR_INTERPOL_VECTOR( xx, yy, plotx )

! Cubic spline is array based
if (int_alg==ALG_CSPL) ploty = CSPLINE_VECTOR( xx, yy, plotx )

if (IS_DEBUG) print *, "Plot data X:", plotx
if (IS_DEBUG) print *, "Plot data Y:", ploty

!> Produce data for the interpolation target (non-grid) array.
if (int_alg==ALG_LAG) then
  ! Lagrange is array-based.
  yy_interpolate = LAGR_INTERPOL_VECTOR( xx, yy, xx_interpolate )
else if (int_alg==ALG_CSPL) then
  ! Cubic spline is array-based.
  yy_interpolate = CSPLINE_VECTOR( xx, yy, xx_interpolate )
else
  ! Those below are scalar-based.
  do i=1, size(xx_interpolate)
    if (int_alg==ALG_DDPI)                                                    &
                    yy_interpolate(i) = DDPINTERPOL( xx, yy, xx_interpolate(i) )
    if (int_alg==ALG_LIN)                                                     &
                    yy_interpolate(i) = LINTERPOL( xx, yy, xx_interpolate(i) )
  end do
end if

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

  if (is_grid_only) then
    call titlin( 'Interpolation grid (' // trim(ALG_NAME(int_alg)) // ')' , 1 )
  else
    call titlin( 'Interpolation data (' // trim(ALG_NAME(int_alg)) // ')' , 1 )
  end if

  call qplcrv(plotx, ploty, n_steps, 'FIRST')   ! line of interpolation grid

  if (is_grid_only) then
    ! Show dots of interpolation grid (normally DISABLED)
    call qplsca(xx, yy, size(xx))
  else
    ! Plot dots for the target interpolation data.
    call qplsca(xx_interpolate, yy_interpolate, size(xx_interpolate))
  end if

!end block PLOT_DISLIN
!-------------------------------------------------------------------------------
! *** End of the DISLIN block
!-------------------------------------------------------------------------------


contains !----------------------------------------------------------------------

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


end program INTERPOLATE
