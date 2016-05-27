module BASE_RANDOM
!*******************************************************************************
! SVN $Id$
!*******************************************************************************
! PURPOSE:
!   Working with random numbers. Init random seed, generate random numbers
!
! NOTES:
!  IMPORTANT: Subroutines here are non-portable and depend on the compiler
!  and possibly the platform.
!
! SUPPORTED COMPILERS:
!  GNU Fortran, Intel Fortran, Oracle Solaris Studio Fortran
!*******************************************************************************

implicit none

! Module name for the DEBUG LOGGER: every function/sub must also have
! the PROCNAME parameter referring to its name. This is done for the Debug
! Logger module. Each module must also have a DEBUG Logger subroutine, that
! is a wrapper to module LOGGER (or perhaps any other that is being used)
!   procedure name PROCNAME
character (len=*), private, parameter :: MODNAME = "BASE_RANDOM"

! Set the debug mode to ON or OFF, in the debug mode, events are written to
! the log, determined by the LOG_DBG subroutine, normally, a wrapper to the
! module LOGGER. May also define integer DEBUG_LEVEL parameter...
logical, private, parameter :: IS_DEBUG = .FALSE.

! Sufficiently great maximum unit, in CSV_IO provided globally
integer, private, parameter :: MAX_UNIT=500

!-------------------------------------------------------------------------------
contains  !-----[ SUBROUTINES AND FUNCTIONS FOLLOW ]----------------------------


subroutine RANDOM_SEED_INIT(n_here, seed_here)
!*******************************************************************************
! RANDOM_SEED_INIT
! PURPOSE: initialises the random seed
! CALL PARAMETERS: none
! NOTES:
!   This is a better and more advanced version of the RANDOM_SEED_INIT, adapted
!   for parallel processing (used PID of the current process).
! PORTABILITY NOTES:
!   BUT -- may NOT be PORTABLE, int64 may not be defined in ISO_FORTRAN_ENV
!   getpid() integer function is GNU extension. On Intel Fortran it is
!   defined by:
!     use IFPORT, only : getpid
!   On Solaris Fortran getpid is defined in system.inc (interfaces for
!   most non-intrinsic library routines). Need this:
!      include "system.inc"
!
!   Modified from http://gcc.gnu.org/onlinedocs/gfortran/RANDOM_005fSEED.html
!   ----------------------------------------------------------------------------
!   How to initialize the random seed with a varying seed in order to ensure
!   a different random number sequence for each invocation of the program.
!   Note that setting any of the seed values to zero should be avoided as it
!   can result in poor quality random numbers being generated.
!*******************************************************************************

  !*****************************************************************************
  ! *** NON-PORTABLE CODE BEGIN ***
  ! int64 is defined in ISO_FORTRAN_ENV. But not in all compilers.
  ! Notably, Oracle Fortran doesn't have it and needs explicit typing:
  ! integer, parameter :: int64 = selected_int_kind(18)
  ! use ISO_FORTRAN_ENV, only: int64 ! This works with GNU and Intel

  !use IFPORT, only : getpid         ! use IFPORT needed for the Intel Fortran

  !implicit none

  !integer, allocatable :: seed(:)
  !integer :: i, n, un, istat, dt(8), pid
  !integer, parameter :: int64 = selected_int_kind(18) ! needed for Oracle
  !integer(int64) :: t

  !include "system.inc"   ! Include non-intrinsic lib headers, Oracle Fortran

  !*****************************************************************************
  ! *** NON-PORTABLE CODE CONTINUE ***
  !-----------------------------------------------------------------------------
  ! *** NOTE: ***
  ! The above code is auto-generated in the include file by the build system
  ! Here this include file is referred only:
  include "BASE_RANDOM.inc"
  ! *** NON-PORTABLE CODE END ***
  !*****************************************************************************

  integer, optional, intent(out) :: n_here                   ! output seed
  integer, optional, dimension(:), intent(out) :: seed_here  ! ... and N

  ! Subroutine name for DEBUG LOGGER
  character (len=*), parameter :: PROCNAME = "RANDOM_SEED_INIT_FULL"

  !-----------------------------------------------------------------------------

  call random_seed(size=n)
  allocate(seed(n))

  ! First try if the OS provides a standard random number generator (Linux/Unix)
  ! open(NEWUNIT= returns a unique file unit - may not be supported everywhere,
  ! so we use GET_FREE_FUNIT from CSV_IO
  un=get_free_funit()
  open(unit=un, file="/dev/urandom", access="stream", &
    form="unformatted", action="read", status="old", iostat=istat)

  if (istat == 0) then
    read(un) seed
    close(un)
  else
    ! Fallback to XOR:ing the current time and pid. The PID is
    ! useful in case one launches multiple instances of the same
    ! program in parallel.
    call system_clock(t)
    if (t == 0) then
      call date_and_time(values=dt)
      t = (dt(1) - 1970) * 365_int64 * 24 * 60 * 60 * 1000 &
          + dt(2) * 31_int64 * 24 * 60 * 60 * 1000 &
          + dt(3) * 24_int64 * 60 * 60 * 1000 &
          + dt(5) * 60 * 60 * 1000 &
          + dt(6) * 60 * 1000 + dt(7) * 1000 &
          + dt(8)
    end if
    pid = getpid()
    t = ieor(t, int(pid, kind(t)))
    do i = 1, n
      seed(i) = lcg(t)
    end do
  end if

  if (present(seed_here) .and. present(n_here)) then    ! get output seed & N
    n_here = n
    seed_here = seed
  end if

  call random_seed(put=seed)

!-------------------------------------------------------------------------------

contains

  ! This simple PRNG might not be good enough for real work, but is
  ! sufficient for seeding a better PRNG.
  function lcg(s)
    integer :: lcg
    integer(int64) :: s
    if (s == 0) then
      s = 104729
    else
      s = mod(s, 4294967296_int64)
    end if
    s = mod(s * 279470273_int64, 4294967291_int64)
    lcg = int(mod(s, int(huge(0), int64)), kind(0))
  end function

  function get_free_funit (file_status, max_funit) result (file_unit)
  !*****************************************************************************
  ! GET_FREE_FUNIT
  ! PURPOSE: returns the first free Fortran unit number (search in 1 to
  !     MAX_UNIT).
  ! RETURNS:
  !    Integer unit number
  ! CALL PARAMETERS:
  !    optional logical execution error status (.TRUE.)
  !    optional integer max_funit to search (default MAX_UNIT defined in mudule)
  !
  ! Author: John Burkardt : This code is distributed under the GNU LGPL license.
  ! Modified by Sergey Budaev
  !*****************************************************************************

    use, intrinsic :: ISO_FORTRAN_ENV     ! Provides system-wide scalar constants
                                          ! INPUT_UNIT OUTPUT_UNIT ERROR_UNIT
    implicit none

    ! Function value
    integer :: file_unit

    ! Calling parameters
    logical, optional, intent(out) :: file_status
    integer, optional, intent(in) :: max_funit

    ! Local variables: copies of optional parameters we need co copy optional
    logical  :: file_status_here    ! variables in case they are absent, so always
    integer  :: max_funit_here      ! work with copies of optionals inside
    ! Other local variables
    integer :: i
    integer :: ios
    logical :: lopen

    ! Subroutine name for DEBUG LOGGER
    character (len=*), parameter :: PROCNAME = "get_free_funit_RANDOM_SEED_INIT"

    !-----------------------------------------------------------------------------

    file_unit = 0
    file_status_here = .FALSE.

    if (present(max_funit)) then
        max_funit_here=max_funit
      else
        max_funit_here=MAX_UNIT ! max from globals
    end if

    do i=1, max_funit_here
      if (i /= INPUT_UNIT .and. i /= OUTPUT_UNIT .and. &
            i /= ERROR_UNIT) then             ! exclude standard console units
        inquire (unit=i, opened=lopen, iostat=ios)
        if (ios == 0) then
          if (.not. lopen) then
            file_unit = i                     ! First free unit found
            file_status_here=.TRUE.
            if (present(file_status)) file_status=file_status_here
            return
          end if
        end if
      end if
    end do

    if (.not. file_status_here) file_unit=-1    ! if no free unit found return -1
    if (present(file_status)) file_status=file_status_here ! and error flag

  end function get_free_funit

end subroutine RANDOM_SEED_INIT

!-------------------------------------------------------------------------------

function RAND_R4() result (randreal)
! Standard (trivial) wrapper for random real (0.0 <= r < 1.0)

  implicit none
  real :: randreal

  ! Subroutine name for DEBUG LOGGER
  character (len=*), parameter :: PROCNAME = "RAND_R4"

  call random_number(randreal)

end function RAND_R4

function RAND_R8() result (randreal)
! Standard (trivial) wrapper for random real (0.0 <= r < 1.0)

  implicit none
  real (kind=8) :: randreal

  ! Subroutine name for DEBUG LOGGER
  character (len=*), parameter :: PROCNAME = "RAND_R8"

  call random_number(randreal)

end function RAND_R8

!-------------------------------------------------------------------------------

function RAND_I(a, b) result (randint)
! trivial random integer (a <= r <= b)

  implicit none
  integer :: randint
  integer, intent(in) :: a, b
  integer :: a_here, b_here, tmp

  ! Subroutine name for DEBUG LOGGER
  character (len=*), parameter :: PROCNAME = "RAND_I"

  a_here = a
  b_here = b

  if (a_here > b_here) then ! just swap values
    tmp=a_here
    a_here = b_here
    b_here = tmp
  end if

  randint = a + int(RAND_R4() * (b - a + 1))

end function RAND_I

!-------------------------------------------------------------------------------

function RNORM() RESULT(fn_val)
!*******************************************************************************
! RNORM
! PURPOSE: Returns a normally distributed pseudo-random number with zero mean
!          and unit variance.
! NOTES:   Adapted from the following Fortran 77 code
!          ALGORITHM 712, COLLECTED ALGORITHMS FROM ACM.
!          THIS WORK PUBLISHED IN TRANSACTIONS ON MATHEMATICAL SOFTWARE,
!          VOL. 18, NO. 4, DECEMBER, 1992, PP. 434-435.
!  The function RNORM() returns a normally distributed pseudo-random
!  number with zero mean and unit variance.
!  The algorithm uses the ratio of uniforms method of A.J. Kinderman
!  and J.F. Monahan augmented with quadratic bounding curves.
!  CODE SOURCE: http://www.netlib.org/ (random.f90)
!*******************************************************************************

REAL :: fn_val

!     Local variables
REAL     :: s = 0.449871, t = -0.386595, a = 0.19600, b = 0.25472,           &
            r1 = 0.27597, r2 = 0.27846, u, v, x, y, q

REAL     :: half = 0.5 ! moved here from header of the original module

!     Generate P = (u,v) uniform in rectangle enclosing acceptance region

DO
  CALL random_number(u)
  CALL random_number(v)
  v = 1.7156 * (v - half)

!     Evaluate the quadratic form
  x = u - s
  y = ABS(v) - t
  q = x**2 + y*(a*y - b*x)

!     Accept P if inside inner ellipse
  IF (q < r1) EXIT
!     Reject P if outside outer ellipse
  IF (q > r2) CYCLE
!     Reject P if outside acceptance region
  IF (v**2 < -4.0*LOG(u)*u**2) EXIT
END DO

!     Return ratio of P's coordinates as the normal deviate
fn_val = v/u
RETURN

end function RNORM

!-------------------------------------------------------------------------------


end module BASE_RANDOM ! <EOF>
