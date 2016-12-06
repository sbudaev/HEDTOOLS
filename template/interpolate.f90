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
! ./interpolate.exe [1 2 3 4 ] [10., 45., 14., 10.] [2.5 1.9]
!
!    Produce interpolation plot with linear algorithm, output to the file
! ./interpolate.exe [1 2 3 4 ] [10., 45., 14., 10.] [2.5 1.9] [linear] [file.ps]
!
! Note: Quotes can also be used instead of square brackets, but the shell
!       will normally process and remove them, so that backslash \ would be
!       necessary for correct processing, e.g.:
!       ./interpolate.exe [1 2 3 4 ] [10., 45., 14., 10.] [2.5 1.9] \"linear\"
!
! Build command on Linux:
!   make GRAPHLIB=-lpgplot SRC=interpolate.f90 OUT=interpolate.exe
!   make GRAPHLIB=-lpgplot SRC=interpolate.f90 OUT=interpolate.exe DEBUG=1
!
! NOTE:    Depends on the HEDTOOLS and PGPLOT Fortran library for plotting.
!
! WARNING: Building PGPLOTS on Windows platform might be non-trivial, also
!          GUI window output interface (/XWINDOW) is not easily available,
!          although PostScript (/PS) file output should work.
!
! Author: Sergey Budaev <sergey.budaev@uib.no>
!-------------------------------------------------------------------------------
program INTERPOLATE

use BASE_UTILS
use BASE_STRINGS
implicit none

! Set DEBUG mode.
logical, parameter :: IS_DEBUG=.FALSE.

! Command line arguments, whole line.
character(len=255*3) :: command_line_str !> Note: allocatable doesn't work
character(len=255), dimension(10) :: command_str
integer :: n_cmds, n_sub, err_flag, n_xx, n_yy

! String array for xx grid array
character(len=255), dimension(100) :: tmp_array_str, xx_array_str, yy_array_str

! Grid values and target interpolation data values
real, dimension(:), allocatable :: xx, yy, xx_interpolate, yy_interpolate

!> Plot scale range coincide with grid values.
real :: min_xx, max_xx

! Number of steps for drawing interpolation line.
integer, parameter :: n_steps = 100

! Plot drawing data
real, dimension(n_steps+1) :: plotx, ploty

real :: step, plotstep

integer :: i, j, k
integer :: pgopen ! pgplot function is integer type.

integer int_alg   ! algorithm number

! Optional output file name.
character(len=:), allocatable :: output_file

! Delimiter characters for command line arguments:
character(len=*), parameter :: STRDEL="[]" // '"' // "'"

! Output devices. See PGPLOT docs or set ? for runtime choice.
character(len=*), parameter :: DEV_UNDEF = '?',        EXT_UNDEF = ""
character(len=*), parameter :: DEV_XWIN  = '/XWINDOW', EXT_XWIN  = ""
character(len=*), parameter :: DEV_PS    = '/PS',      EXT_PS    = ".ps"
character(len=*), parameter :: DEV_PSV   = '/VPS',     EXT_VPS   = ".ps"
character(len=*), parameter :: DEV_PNG   = '/PNG',     EXT_PNG   = ".png"
character(len=*), parameter :: DEV_TPNG  = '/TPNG',    EXT_TPNG  = ".png"

! Output device.
character(len=:), allocatable :: pg_default_name, output_dev, output_save

! Interpolation algorithms for selection. Implemented in HEDTOOLS.
integer, parameter :: ALG_DDPI = 1  ! Nonlinear, divided difference.
integer, parameter :: ALG_LIN  = 2  ! Linear.
integer, parameter :: ALG_LAG  = 3  ! Lagrange, array-based.

!-------------------------------------------------------------------------------

output_dev  = DEV_XWIN  ! default output device is X11.
output_save = DEV_PS    ! default output for save file.

! Output file name in PGPLOT library always has the same name.
pg_default_name = "pgplot" // EXT_PS

if (IS_DEBUG) print *, "DEBUG: ", pg_default_name

int_alg = ALG_DDPI       ! default algorithm is DDPINTERPOL

k=1

if (IS_DEBUG) print *, "DEBUG: Delimiters: >",STRDEL, "<"

! Process command line arguments...
call get_command(command_line_str)

!> Parse grid arrays enclosed in square brackets.
call PARSE(command_line_str, STRDEL ,command_str, n_cmds)

if (IS_DEBUG) print *, "DEBUG: >",command_line_str, "<"

if (n_cmds==1) then
  print *, "ERROR: Requires command line arguments."
  print *, "---------------------------------------"
  print *, ""
  print *, "Produce interpolation plot of data that are provided at the command line."
  print *, "Interpolation is basedon subroutines from in HEDTOOLS, so debug plots can"
  print *, "easily be generated in the model independently from the code (i.e."
  print *, "no calls to non-portable and potentially platform-specific graphics "
  print *, "procedures)."
  print *, ""
  print *, "Input: Two arrays for the interpolation grid X, Y, they must be of the same"
  print *, "     size,the third array containing the interpolation data."
  print *, "     There may be also two other parameters: algorithm (linear or"
  print *, "     nonlinear) and output file name. Defaults are nonlinear DDPINTERPOL"
  print *, "     algorithm. Each of the parameters must be enclosed in square brackets."
  print *, ""
  print *, "Examples:"
  print *, ""
  print *, "* Produce interpolation plot with the default non-linear algorithm, output"
  print *, " to screen (X11):"
  print *, trim(command_str(1)), " [1 2 3 4 ] [10., 45., 14., 10.] [2.5 1.9]"
  print *, "* Produce interpolation plot with linear algorithm, output to the file"
  print *, trim(command_str(1)), " [1 2 3 4 ] [10., 45., 14., 10.] [2.5 1.9] [linear] [file.ps]"
  print *, ""
  stop
end if

! Parse substring arrays elclosed into the square brackets.
! NOTE: Star loopfrom 2 as the name of the executable file is also
!       in the command line string parsed as the first substring.
do i=2, n_cmds

  NONZERO: if (len(trim(command_str(i)))>0) then

    ! Print out the string containing the array.
    if (IS_DEBUG) print *, "DEBUG:", i, ">",trim(command_str(i)), "<"
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
       print *, "ERROR: unequal size grid arrays."
       stop
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
        case default
          output_dev=output_save
          output_file=trim(command_str(i))
        end select

    end if PARSE_SEL

    k=k+1

  end if NONZERO

end do

min_xx = minval(xx)
max_xx = maxval(xx)

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

!> Produce data for the interpolation target (non-grid) array
do i=1, size(xx_interpolate)
  if (int_alg==ALG_DDPI)                                                      &
                  yy_interpolate(i) = DDPINTERPOL( xx, yy, xx_interpolate(i) )
  if (int_alg==ALG_LIN)                                                       &
                  yy_interpolate(i) = LINTERPOL( xx, yy, xx_interpolate(i) )
end do

! Produce the plot itself -- using PGPLOT library.
if (pgopen(output_dev) .lt. 1) then
  print *, "ERROR: Cannot open output device ", output_dev
  stop
end if
call pgenv( minval(plotx), maxval(plotx), minval(ploty), maxval(ploty), 0, 0 )
call pglab('X', 'Y', 'Interpolation value ' // output_file)
call pgline( n_steps, plotx, ploty )  ! plot line of interpolation grid
call pgpt( size(xx), xx, yy, 3 )      ! plot dots of interpolation grid
! Plot dots for the target interpolation data.
call pgpt (size(xx_interpolate), xx_interpolate, yy_interpolate, 8)
call pgclos

! Rename the output file.
! WARNING:: rename subroutine is GNU extension and may not be available
!           on all compiler systems. Does work with gfortran, Oracle f95
if (output_dev /= DEV_XWIN) then
  call rename (pg_default_name, output_file)
  print *, "Wrote plot to output file (", output_dev, "): " , output_file
end if

end program INTERPOLATE
