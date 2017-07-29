!*******************************************************************************
! SVN $Id$
!*******************************************************************************
! PURPOSE:
!   Working with random numbers. Init random seed, generate random numbers
!
! NOTES:
!  IMPORTANT: Subroutines here are non-portable and depend on the compiler
!  and possibly the platform. They use the standard-conforming
!  intrinsic subroutines RANDOM_SEED to initialize the pseudo-random number
!  generator and RANDOM_NUMBER to generate pseudo-random numbers. It is
!  recommended that these subroutines should be used in all new codes.
!
! NOTE ON PRECISION:
!  Single precision is the default in all generic interfaces (i.e. when there
!  is any interface ambiguity, single precision random procss are always
!  invoked).
!
! SUPPORTED COMPILERS:
!  GNU Fortran, Intel Fortran, Oracle Solaris Studio Fortran
!*******************************************************************************
module BASE_RANDOM

implicit none

! Double precision kind def.
integer, parameter :: dp=8

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

interface RNORM_R4      ! Gaussian normals
  module procedure RNORM_VAL_R4
  module procedure RNORM_RENORMALISED_R4
end interface RNORM_R4

interface RNORM_R8      ! Gaussian normals, dbl prec.
  module procedure RNORM_VAL_R8
  module procedure RNORM_RENORMALISED_R8
end interface RNORM_R8

interface RNORM         ! We can use RNORM as an alias to RNORM_R4 (R8 not
  module procedure RNORM_VAL_R4           ! thoroughly tested)
  module procedure RNORM_RENORMALISED_R4  ! but renormalised versions can be
  module procedure RNORM_RENORMALISED_R8  ! any supported precision.
end interface RNORM

interface RAND_ARRAY                  ! Arrays of uniform random numbers,
  module procedure RAND_ARRAY_1_I     ! wrapper to RAND_R4 and RAND_I
  module procedure RAND_ARRAY_2_I
  module procedure RAND_ARRAY_3_I
  module procedure RAND_ARRAY_4_I
  module procedure RAND_ARRAY_5_I
  module procedure RAND_ARRAY_6_I
  module procedure RAND_ARRAY_1_R4    ! 32 bit
  module procedure RAND_ARRAY_2_R4
  module procedure RAND_ARRAY_3_R4
  module procedure RAND_ARRAY_4_R4
  module procedure RAND_ARRAY_5_R4
  module procedure RAND_ARRAY_6_R4
  module procedure RAND_ARRAY_1_R8    ! 64 bit
  module procedure RAND_ARRAY_2_R8
  module procedure RAND_ARRAY_3_R8
  module procedure RAND_ARRAY_4_R8
  module procedure RAND_ARRAY_5_R8
  module procedure RAND_ARRAY_6_R8
end interface RAND_ARRAY

interface RAND_MATRIX                 ! Matrices of uniform random numbers,
  module procedure RAND_ARRAY_2_I     ! wrapper to RAND_R4 and RAND_I
  module procedure RAND_ARRAY_3_I
  module procedure RAND_ARRAY_4_I
  module procedure RAND_ARRAY_5_I
  module procedure RAND_ARRAY_6_I
  module procedure RAND_ARRAY_2_R4    ! 32 bit
  module procedure RAND_ARRAY_3_R4
  module procedure RAND_ARRAY_4_R4
  module procedure RAND_ARRAY_5_R4
  module procedure RAND_ARRAY_6_R4
  module procedure RAND_ARRAY_2_R8    ! 64 bit
  module procedure RAND_ARRAY_3_R8
  module procedure RAND_ARRAY_4_R8
  module procedure RAND_ARRAY_5_R8
  module procedure RAND_ARRAY_6_R8
end interface RAND_MATRIX

interface RNORM_ARRAY   ! Arrays of normally distributed random numbers
  module procedure RNORM_ARRAY_1_R4         ! 32 bit
  module procedure RNORM_ARRAY_2_R4
  module procedure RNORM_ARRAY_3_R4
  module procedure RNORM_ARRAY_4_R4
  module procedure RNORM_ARRAY_5_R4
  module procedure RNORM_ARRAY_6_R4
  module procedure RNORM_ARRAY_1_R8         ! 64 bit
  module procedure RNORM_ARRAY_2_R8
  module procedure RNORM_ARRAY_3_R8
  module procedure RNORM_ARRAY_4_R8
  module procedure RNORM_ARRAY_5_R8
  module procedure RNORM_ARRAY_6_R8
  module procedure RNORM_RENORM_ARRAY_1_R4  ! 32 bit
  module procedure RNORM_RENORM_ARRAY_2_R4
  module procedure RNORM_RENORM_ARRAY_3_R4
  module procedure RNORM_RENORM_ARRAY_4_R4
  module procedure RNORM_RENORM_ARRAY_5_R4
  module procedure RNORM_RENORM_ARRAY_6_R4
  module procedure RNORM_RENORM_ARRAY_1_R8  ! 64 bit
  module procedure RNORM_RENORM_ARRAY_2_R8
  module procedure RNORM_RENORM_ARRAY_3_R8
  module procedure RNORM_RENORM_ARRAY_4_R8
  module procedure RNORM_RENORM_ARRAY_5_R8
  module procedure RNORM_RENORM_ARRAY_6_R8
end interface RNORM_ARRAY

interface RNORM_MATRIX   ! Matrices of normally distributed random numbers
  module procedure RNORM_ARRAY_2_R4         ! 32 bit
  module procedure RNORM_ARRAY_3_R4
  module procedure RNORM_ARRAY_4_R4
  module procedure RNORM_ARRAY_5_R4
  module procedure RNORM_ARRAY_6_R4
  module procedure RNORM_ARRAY_2_R8         ! 64 bit
  module procedure RNORM_ARRAY_3_R8
  module procedure RNORM_ARRAY_4_R8
  module procedure RNORM_ARRAY_5_R8
  module procedure RNORM_ARRAY_6_R8
  module procedure RNORM_RENORM_ARRAY_2_R4  ! 32 bit
  module procedure RNORM_RENORM_ARRAY_3_R4
  module procedure RNORM_RENORM_ARRAY_4_R4
  module procedure RNORM_RENORM_ARRAY_5_R4
  module procedure RNORM_RENORM_ARRAY_6_R4
  module procedure RNORM_RENORM_ARRAY_2_R8  ! 64 bit
  module procedure RNORM_RENORM_ARRAY_3_R8
  module procedure RNORM_RENORM_ARRAY_4_R8
  module procedure RNORM_RENORM_ARRAY_5_R8
  module procedure RNORM_RENORM_ARRAY_6_R8
end interface RNORM_MATRIX

interface RAND_R4                 ! Uniform random numbers, standard and
  module procedure RAND_VAL_R4    ! renormalised to arbitrary range
  module procedure RAND_RENORMALISED_R4
  module procedure RAND_RENORMALISED_R4I
end interface RAND_R4

interface RAND_R8                 ! Uniform random numbers, standard and
  module procedure RAND_VAL_R8    ! renormalised to arbitrary range
  module procedure RAND_RENORMALISED_R8
  module procedure RAND_RENORMALISED_R8I
end interface RAND_R8

interface RAND                    ! Alias for RAND_R4
  module procedure RAND_VAL_R4    ! uniform distribution
  module procedure RAND_RENORMALISED_R4
  module procedure RAND_RENORMALISED_R4I
  module procedure RAND_RENORMALISED_R8 ! RAND_RENORMALISED_R8I is ambiguous
end interface RAND

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
  ! Here this include file is referred:
  include "BASE_RANDOM.inc"
  ! *** NON-PORTABLE CODE END ***
  !*****************************************************************************

  integer, optional, intent(out) :: n_here                   ! output seed
  integer, optional, dimension(:), intent(out) :: seed_here  ! ... and N

  ! Subroutine name for DEBUG LOGGER
  character (len=*), parameter :: PROCNAME = "RANDOM_SEED_INIT"

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

function RAND_VAL_R4() result (randreal)
! Standard (trivial) wrapper for random real (0.0 <= r < 1.0)
! Notes: On some systems the several PRNGs functions may use different
!        algorithms e.g see ran (standard, from DEC) as well as lcrans and
!        addrans (better but slower). This uses the standard implementation
!        random_number. In particular, for GNU gfortran ran is provided for
!        backwards compatibility with GNU Fortran 77. It implements a simple
!        modulo generator as provided by g77. For new code, one should
!        consider the use of random_number as it implements a superior
!        algorithm. This random_number is just used in this function.

  implicit none
  real :: randreal

  ! Subroutine name for DEBUG LOGGER
  character (len=*), parameter :: PROCNAME = "RAND_R4"

  call random_number(randreal)

end function RAND_VAL_R4

!-------------------------------------------------------------------------------

function RAND_VAL_R8() result (randreal)
! Standard (trivial) wrapper for random real (0.0 <= r < 1.0)

  implicit none
  real (kind=dp) :: randreal

  ! Subroutine name for DEBUG LOGGER
  character (len=*), parameter :: PROCNAME = "RAND_R8"

  call random_number(randreal)

end function RAND_VAL_R8

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

  ! Use int or floor? floor returns more homogeneous numbers if negative
  ! integers are in the range. With positive arguments int and floor eqivalent.
  ! Note: int(-0.2)= 0, floor(-0.2)=-1
  !       int(+0.2)= 0, floor(+0.2)= 0 , so with int there is a small local
  !       raise in the density function, not good.
  randint = a + floor(RAND_R4() * (b - a + 1))

end function RAND_I

!-------------------------------------------------------------------------------

function RAND_STRING(length, char_start, char_end) result (out_string)
!*******************************************************************************
! RAND_STRING
! PURPOSE: returns a string composed of random characters.
! PARAMETERS: integer string length, optional integers defining the range
!             of the ASCII character codes for the string, if they are not set,
!             the character range is defined in the ASCII codes 48:122 (i.e.
!             numbers, 48:57, sime special symbols and Latin alphanumeric
!             characters 65:122)
!*******************************************************************************

  ! Parameters
  integer, intent(in) :: length
  integer, optional, intent(in) :: char_start, char_end

  ! Function value
  character(len=length) :: out_string

  ! local variables
  integer :: i
  integer :: char_start_here, char_end_here

  if (present(char_start)) then
    char_start_here = char_start
  else
    char_start_here = 32
  end if

  if (present(char_end)) then
    char_end_here = char_end
  else
    char_end_here = 122
  end if

  ! produce the random string
  do i = 1, length
    out_string(i:i) = achar(RAND_I(char_start_here, char_end_here))
  end do

end function RAND_STRING

!-------------------------------------------------------------------------------

function RNORM_VAL_R4() result(fn_val)
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

end function RNORM_VAL_R4

!-------------------------------------------------------------------------------

function RNORM_VAL_R8() result(fn_val)
!*******************************************************************************
! RNORM
! PURPOSE: Returns a normally distributed pseudo-random number with zero mean
!          and unit variance. This is the double precision (kind=dp) version.
! NOTES:   Adapted from the following Fortran 77 code
!          ALGORITHM 712, COLLECTED ALGORITHMS FROM ACM.
!          THIS WORK PUBLISHED IN TRANSACTIONS ON MATHEMATICAL SOFTWARE,
!          VOL. 18, NO. 4, DECEMBER, 1992, PP. 434-435.
! WARNING: This is a simple conversion of single precision 32 bit function
!          statistical properties of the generated distribution are to be
!          checked yet.
! The function RNORM() returns a normally distributed pseudo-random
! number with zero mean and unit variance.
! The algorithm uses the ratio of uniforms method of A.J. Kinderman
! and J.F. Monahan augmented with quadratic bounding curves.
! CODE SOURCE: http://www.netlib.org/ (random.f90)
!*******************************************************************************

REAL(kind=dp) :: fn_val

!     Local variables
REAL(kind=dp) :: s=0.449871_dp, t=-0.386595_dp, a=0.19600_dp, b=0.25472_dp,    &
                 r1=0.27597_dp, r2=0.27846_dp, u, v, x, y, q

REAL(kind=dp) :: half = 0.5 ! moved here from header of the original module

!     Generate P = (u,v) uniform in rectangle enclosing acceptance region

DO
  CALL random_number(u)
  CALL random_number(v)
  v = 1.7156_dp * (v - half)

!     Evaluate the quadratic form
  x = u - s
  y = ABS(v) - t
  q = x**2 + y*(a*y - b*x)

!     Accept P if inside inner ellipse
  IF (q < r1) EXIT
!     Reject P if outside outer ellipse
  IF (q > r2) CYCLE
!     Reject P if outside acceptance region
  IF (v**2 < -4.0_dp*LOG(u)*u**2) EXIT
END DO

!     Return ratio of P's coordinates as the normal deviate
fn_val = v/u
RETURN

end function RNORM_VAL_R8

!-------------------------------------------------------------------------------

subroutine RNORM_ARRAY_1_R4(random_array)
!*******************************************************************************
! RNORM_ARRAY, 1-dimensional
! PURPOSE: Wrapper to RNORM_VAL_R4 to produce array of normally distributed values
!*******************************************************************************

! parameters
real, dimension(:) :: random_array

! local variables
integer :: i

do i=1, ubound(random_array,1)
  random_array(i) = RNORM_VAL_R4()
end do

end subroutine RNORM_ARRAY_1_R4

!-------------------------------------------------------------------------------

subroutine RNORM_ARRAY_2_R4(random_array)
!*******************************************************************************
! RNORM_ARRAY, 2-dimensional
! PURPOSE: Wrapper to RNORM_VAL_R4 to produce array of normally distributed values
!*******************************************************************************

! parameters
real, dimension(:,:) :: random_array

! local variables
integer :: i, j

do i=1, ubound(random_array,2)
  do j=1, ubound(random_array,1)
    random_array(j,i) = RNORM_VAL_R4()
  end do
end do

end subroutine RNORM_ARRAY_2_R4

!-------------------------------------------------------------------------------

subroutine RNORM_ARRAY_3_R4(random_array)
!*******************************************************************************
! RNORM_ARRAY, 3-dimensional
! PURPOSE: Wrapper to RNORM_VAL_R4 to produce array of normally distributed values
!*******************************************************************************

! parameters
real, dimension(:,:,:) :: random_array

! local variables
integer :: i, j, k

do i=1, ubound(random_array,3)
  do j=1, ubound(random_array,2)
    do k=1, ubound(random_array,1)
      random_array(k,j,i) = RNORM_VAL_R4()
    end do
  end do
end do

end subroutine RNORM_ARRAY_3_R4

!-------------------------------------------------------------------------------

subroutine RNORM_ARRAY_4_R4(random_array)
!*******************************************************************************
! RNORM_ARRAY, 4-dimensional
! PURPOSE: Wrapper to RNORM_VAL_R4 to produce array of normally distributed values
!*******************************************************************************

! parameters
real, dimension(:,:,:,:) :: random_array

! local variables
integer :: i, j, k, l

do i=1, ubound(random_array,4)
  do j=1, ubound(random_array,3)
    do k=1, ubound(random_array,2)
      do l=1, ubound(random_array,1)
        random_array(l,k,j,i) = RNORM_VAL_R4()
      end do
    end do
  end do
end do

end subroutine RNORM_ARRAY_4_R4

!-------------------------------------------------------------------------------

subroutine RNORM_ARRAY_5_R4(random_array)
!*******************************************************************************
! RNORM_ARRAY, 5-dimensional
! PURPOSE: Wrapper to RNORM_VAL_R4 to produce array of normally distributed values
!*******************************************************************************

! parameters
real, dimension(:,:,:,:,:) :: random_array

! local variables
integer :: i, j, k, l, m

do i=1, ubound(random_array,5)
  do j=1, ubound(random_array,4)
    do k=1, ubound(random_array,3)
      do l=1, ubound(random_array,2)
        do m=1, ubound(random_array,1)
          random_array(m,l,k,j,i) = RNORM_VAL_R4()
        end do
      end do
    end do
  end do
end do

end subroutine RNORM_ARRAY_5_R4

!-------------------------------------------------------------------------------

subroutine RNORM_ARRAY_6_R4(random_array)
!*******************************************************************************
! RNORM_ARRAY, 6-dimensional
! PURPOSE: Wrapper to RNORM_VAL_R4 to produce array of normally distributed values
!*******************************************************************************

! parameters
real, dimension(:,:,:,:,:,:) :: random_array

! local variables
integer :: i, j, k, l, m, n

do i=1, ubound(random_array,6)
  do j=1, ubound(random_array,5)
    do k=1, ubound(random_array,4)
      do l=1, ubound(random_array,3)
        do m=1, ubound(random_array,2)
          do n=1, ubound(random_array,1)
            random_array(n,m,l,k,j,i) = RNORM_VAL_R4()
          end do
        end do
      end do
    end do
  end do
end do

end subroutine RNORM_ARRAY_6_R4

!-------------------------------------------------------------------------------

subroutine RNORM_ARRAY_1_R8(random_array)
!*******************************************************************************
! RNORM_ARRAY, 1-dimensional
! PURPOSE: Wrapper to RNORM_VAL_R4 to produce array of normally distributed values
!*******************************************************************************

! parameters
real(kind=dp), dimension(:) :: random_array

! local variables
integer :: i

do i=1, ubound(random_array,1)
  random_array(i) = RNORM_VAL_R8()
end do

end subroutine RNORM_ARRAY_1_R8

!-------------------------------------------------------------------------------

subroutine RNORM_ARRAY_2_R8(random_array)
!*******************************************************************************
! RNORM_ARRAY, 2-dimensional
! PURPOSE: Wrapper to RNORM_VAL_R4 to produce array of normally distributed values
!*******************************************************************************

! parameters
real(kind=dp), dimension(:,:) :: random_array

! local variables
integer :: i, j

do i=1, ubound(random_array,2)
  do j=1, ubound(random_array,1)
    random_array(j,i) = RNORM_VAL_R8()
  end do
end do

end subroutine RNORM_ARRAY_2_R8

!-------------------------------------------------------------------------------

subroutine RNORM_ARRAY_3_R8(random_array)
!*******************************************************************************
! RNORM_ARRAY, 3-dimensional
! PURPOSE: Wrapper to RNORM_VAL_R4 to produce array of normally distributed values
!*******************************************************************************

! parameters
real(kind=dp), dimension(:,:,:) :: random_array

! local variables
integer :: i, j, k

do i=1, ubound(random_array,3)
  do j=1, ubound(random_array,2)
    do k=1, ubound(random_array,1)
      random_array(k,j,i) = RNORM_VAL_R8()
    end do
  end do
end do

end subroutine RNORM_ARRAY_3_R8

!-------------------------------------------------------------------------------

subroutine RNORM_ARRAY_4_R8(random_array)
!*******************************************************************************
! RNORM_ARRAY, 4-dimensional
! PURPOSE: Wrapper to RNORM_VAL_R4 to produce array of normally distributed values
!*******************************************************************************

! parameters
real(kind=dp), dimension(:,:,:,:) :: random_array

! local variables
integer :: i, j, k, l

do i=1, ubound(random_array,4)
  do j=1, ubound(random_array,3)
    do k=1, ubound(random_array,2)
      do l=1, ubound(random_array,1)
        random_array(l,k,j,i) = RNORM_VAL_R8()
      end do
    end do
  end do
end do

end subroutine RNORM_ARRAY_4_R8

!-------------------------------------------------------------------------------

subroutine RNORM_ARRAY_5_R8(random_array)
!*******************************************************************************
! RNORM_ARRAY, 5-dimensional
! PURPOSE: Wrapper to RNORM_VAL_R4 to produce array of normally distributed values
!*******************************************************************************

! parameters
real(kind=dp), dimension(:,:,:,:,:) :: random_array

! local variables
integer :: i, j, k, l, m

do i=1, ubound(random_array,5)
  do j=1, ubound(random_array,4)
    do k=1, ubound(random_array,3)
      do l=1, ubound(random_array,2)
        do m=1, ubound(random_array,1)
          random_array(m,l,k,j,i) = RNORM_VAL_R8()
        end do
      end do
    end do
  end do
end do

end subroutine RNORM_ARRAY_5_R8

!-------------------------------------------------------------------------------

subroutine RNORM_ARRAY_6_R8(random_array)
!*******************************************************************************
! RNORM_ARRAY, 6-dimensional
! PURPOSE: Wrapper to RNORM_VAL_R4 to produce array of normally distributed values
!*******************************************************************************

! parameters
real(kind=dp), dimension(:,:,:,:,:,:) :: random_array

! local variables
integer :: i, j, k, l, m, n

do i=1, ubound(random_array,6)
  do j=1, ubound(random_array,5)
    do k=1, ubound(random_array,4)
      do l=1, ubound(random_array,3)
        do m=1, ubound(random_array,2)
          do n=1, ubound(random_array,1)
            random_array(n,m,l,k,j,i) = RNORM_VAL_R8()
          end do
        end do
      end do
    end do
  end do
end do

end subroutine RNORM_ARRAY_6_R8

!-------------------------------------------------------------------------------

subroutine RNORM_RENORM_ARRAY_1_R4(random_array, mean, variance)
!*******************************************************************************
! RNORM_ARRAY, 1-dimensional
! PURPOSE: Wrapper to RNORM_VAL_R4 to produce array of normally distributed values
!*******************************************************************************

! parameters
real, dimension(:) :: random_array
real, intent(in) :: mean, variance

! local variables
integer :: i


  do i=1, ubound(random_array,1)
    random_array(i) = RNORM_RENORMALISED_R4(mean, variance)
  end do

end subroutine RNORM_RENORM_ARRAY_1_R4

!-------------------------------------------------------------------------------

subroutine RNORM_RENORM_ARRAY_2_R4(random_array, mean, variance)
!*******************************************************************************
! RNORM_ARRAY, 2-dimensional
! PURPOSE: Wrapper to RNORM_VAL_R4 to produce array of normally distributed values
!*******************************************************************************

! parameters
real, dimension(:,:) :: random_array
real, intent(in) :: mean, variance

! local variables
integer :: i, j

  do i=1, ubound(random_array,2)
    do j=1, ubound(random_array,1)
      random_array(j,i) = RNORM_RENORMALISED_R4(mean, variance)
    end do
  end do

end subroutine RNORM_RENORM_ARRAY_2_R4

!-------------------------------------------------------------------------------

subroutine RNORM_RENORM_ARRAY_3_R4(random_array, mean, variance)
!*******************************************************************************
! RNORM_ARRAY, 3-dimensional
! PURPOSE: Wrapper to RNORM_VAL_R4 to produce array of normally distributed values
!*******************************************************************************

! parameters
real, dimension(:,:,:) :: random_array
real, intent(in) :: mean, variance

! local variables
integer :: i, j, k

  do i=1, ubound(random_array,3)
    do j=1, ubound(random_array,2)
      do k=1, ubound(random_array,1)
        random_array(k,j,i) = RNORM_RENORMALISED_R4(mean, variance)
      end do
    end do
  end do

end subroutine RNORM_RENORM_ARRAY_3_R4

!-------------------------------------------------------------------------------

subroutine RNORM_RENORM_ARRAY_4_R4(random_array, mean, variance)
!*******************************************************************************
! RNORM_ARRAY, 4-dimensional
! PURPOSE: Wrapper to RNORM_VAL_R4 to produce array of normally distributed values
!*******************************************************************************

! parameters
real, dimension(:,:,:,:) :: random_array
real, intent(in) :: mean, variance

! local variables
integer :: i, j, k, l

  do i=1, ubound(random_array,4)
    do j=1, ubound(random_array,3)
      do k=1, ubound(random_array,2)
        do l=1, ubound(random_array,1)
          random_array(l,k,j,i) = RNORM_RENORMALISED_R4(mean, variance)
        end do
      end do
    end do
  end do

end subroutine RNORM_RENORM_ARRAY_4_R4

!-------------------------------------------------------------------------------

subroutine RNORM_RENORM_ARRAY_5_R4(random_array, mean, variance)
!*******************************************************************************
! RNORM_ARRAY, 5-dimensional
! PURPOSE: Wrapper to RNORM_VAL_R4 to produce array of normally distributed values
!*******************************************************************************

! parameters
real, dimension(:,:,:,:,:) :: random_array
real, intent(in) :: mean, variance

! local variables
integer :: i, j, k, l, m

  do i=1, ubound(random_array,5)
    do j=1, ubound(random_array,4)
      do k=1, ubound(random_array,3)
        do l=1, ubound(random_array,2)
          do m=1, ubound(random_array,1)
            random_array(m,l,k,j,i) = RNORM_RENORMALISED_R4(mean, variance)
          end do
        end do
      end do
    end do
  end do

end subroutine RNORM_RENORM_ARRAY_5_R4

!-------------------------------------------------------------------------------

subroutine RNORM_RENORM_ARRAY_6_R4(random_array, mean, variance)
!*******************************************************************************
! RNORM_ARRAY, 6-dimensional
! PURPOSE: Wrapper to RNORM_VAL_R4 to produce array of normally distributed values
!*******************************************************************************

! parameters
real, dimension(:,:,:,:,:,:) :: random_array
real, intent(in) :: mean, variance

! local variables
integer :: i, j, k, l, m, n

  do i=1, ubound(random_array,6)
    do j=1, ubound(random_array,5)
      do k=1, ubound(random_array,4)
        do l=1, ubound(random_array,3)
          do m=1, ubound(random_array,2)
            do n=1, ubound(random_array,1)
              random_array(n,m,l,k,j,i) = RNORM_RENORMALISED_R4(mean, variance)
            end do
          end do
        end do
      end do
    end do
  end do

end subroutine RNORM_RENORM_ARRAY_6_R4

!-------------------------------------------------------------------------------

subroutine RNORM_RENORM_ARRAY_1_R8(random_array, mean, variance)
!*******************************************************************************
! RNORM_ARRAY, 1-dimensional
! PURPOSE: Wrapper to RNORM_VAL_R4 to produce array of normally distributed values
!*******************************************************************************

! parameters
real(kind=dp), dimension(:) :: random_array
real(kind=dp), intent(in) :: mean, variance

! local variables
integer :: i

  do i=1, ubound(random_array,1)
    random_array(i) = RNORM_RENORMALISED_R8(mean, variance)
  end do

end subroutine RNORM_RENORM_ARRAY_1_R8

!-------------------------------------------------------------------------------

subroutine RNORM_RENORM_ARRAY_2_R8(random_array, mean, variance)
!*******************************************************************************
! RNORM_ARRAY, 2-dimensional
! PURPOSE: Wrapper to RNORM_VAL_R4 to produce array of normally distributed values
!*******************************************************************************

! parameters
real(kind=dp), dimension(:,:) :: random_array
real(kind=dp), intent(in) :: mean, variance

! local variables
integer :: i, j

  do i=1, ubound(random_array,2)
    do j=1, ubound(random_array,1)
      random_array(j,i) = RNORM_RENORMALISED_R8(mean, variance)
    end do
  end do

end subroutine RNORM_RENORM_ARRAY_2_R8

!-------------------------------------------------------------------------------

subroutine RNORM_RENORM_ARRAY_3_R8(random_array, mean, variance)
!*******************************************************************************
! RNORM_ARRAY, 3-dimensional
! PURPOSE: Wrapper to RNORM_VAL_R4 to produce array of normally distributed values
!*******************************************************************************

! parameters
real(kind=dp), dimension(:,:,:) :: random_array
real(kind=dp), intent(in) :: mean, variance

! local variables
integer :: i, j, k

  do i=1, ubound(random_array,3)
    do j=1, ubound(random_array,2)
      do k=1, ubound(random_array,1)
        random_array(k,j,i) = RNORM_RENORMALISED_R8(mean, variance)
      end do
    end do
  end do

end subroutine RNORM_RENORM_ARRAY_3_R8

!-------------------------------------------------------------------------------

subroutine RNORM_RENORM_ARRAY_4_R8(random_array, mean, variance)
!*******************************************************************************
! RNORM_ARRAY, 4-dimensional
! PURPOSE: Wrapper to RNORM_VAL_R4 to produce array of normally distributed values
!*******************************************************************************

! parameters
real(kind=dp), dimension(:,:,:,:) :: random_array
real(kind=dp), intent(in) :: mean, variance

! local variables
integer :: i, j, k, l

  do i=1, ubound(random_array,4)
    do j=1, ubound(random_array,3)
      do k=1, ubound(random_array,2)
        do l=1, ubound(random_array,1)
          random_array(l,k,j,i) = RNORM_RENORMALISED_R8(mean, variance)
        end do
      end do
    end do
  end do

end subroutine RNORM_RENORM_ARRAY_4_R8

!-------------------------------------------------------------------------------

subroutine RNORM_RENORM_ARRAY_5_R8(random_array, mean, variance)
!*******************************************************************************
! RNORM_ARRAY, 5-dimensional
! PURPOSE: Wrapper to RNORM_VAL_R4 to produce array of normally distributed values
!*******************************************************************************

! parameters
real(kind=dp), dimension(:,:,:,:,:) :: random_array
real(kind=dp), intent(in) :: mean, variance

! local variables
integer :: i, j, k, l, m

  do i=1, ubound(random_array,5)
    do j=1, ubound(random_array,4)
      do k=1, ubound(random_array,3)
        do l=1, ubound(random_array,2)
          do m=1, ubound(random_array,1)
            random_array(m,l,k,j,i) = RNORM_RENORMALISED_R8(mean, variance)
          end do
        end do
      end do
    end do
  end do

end subroutine RNORM_RENORM_ARRAY_5_R8

!-------------------------------------------------------------------------------

subroutine RNORM_RENORM_ARRAY_6_R8(random_array, mean, variance)
!*******************************************************************************
! RNORM_ARRAY, 6-dimensional
! PURPOSE: Wrapper to RNORM_VAL_R4 to produce array of normally distributed values
!*******************************************************************************

! parameters
real(kind=dp), dimension(:,:,:,:,:,:) :: random_array
real(kind=dp), intent(in) :: mean, variance

! local variables
integer :: i, j, k, l, m, n

  do i=1, ubound(random_array,6)
    do j=1, ubound(random_array,5)
      do k=1, ubound(random_array,4)
        do l=1, ubound(random_array,3)
          do m=1, ubound(random_array,2)
            do n=1, ubound(random_array,1)
              random_array(n,m,l,k,j,i) = RNORM_RENORMALISED_R8(mean, variance)
            end do
          end do
        end do
      end do
    end do
  end do

end subroutine RNORM_RENORM_ARRAY_6_R8

!-------------------------------------------------------------------------------

subroutine RAND_ARRAY_1_R4(random_array, A, B)
!*******************************************************************************
! RANDOM_ARRAY_1, 1-dimensional
! PURPOSE: Wrapper to RAND_R4 to produce array of uniformly distributed values
!*******************************************************************************

! parameters
real, dimension(:) :: random_array
real, optional, intent(in) :: A, B

if (present(A) .and. present(B)) then
  call random_number(random_array)
  random_array = A + random_array * (B - A)
else
  call random_number(random_array)
end if

end subroutine RAND_ARRAY_1_R4

!-------------------------------------------------------------------------------

subroutine RAND_ARRAY_2_R4(random_array, A, B)
!*******************************************************************************
! RAND_ARRAY, 2-dimensional
! PURPOSE: Wrapper to RAND_R4 to produce array of uniformly distributed values
!*******************************************************************************

! parameters
real, dimension(:,:) :: random_array
real, optional, intent(in) :: A, B

if (present(A) .and. present(B)) then
  call random_number(random_array)
  random_array = A + random_array * (B - A)
else
  call random_number(random_array)
end if

end subroutine RAND_ARRAY_2_R4

!-------------------------------------------------------------------------------

subroutine RAND_ARRAY_3_R4(random_array, A, B)
!*******************************************************************************
! RNORM_ARRAY, 3-dimensional
! PURPOSE: Wrapper to RAND_R4 to produce array of uniformly distributed values
!*******************************************************************************

! parameters
real, dimension(:,:,:) :: random_array
real, optional, intent(in) :: A, B

if (present(A) .and. present(B)) then
  call random_number(random_array)
  random_array = A + random_array * (B - A)
else
  call random_number(random_array)
end if

end subroutine RAND_ARRAY_3_R4

!-------------------------------------------------------------------------------

subroutine RAND_ARRAY_4_R4(random_array, A, B)
!*******************************************************************************
! RNORM_ARRAY, 4-dimensional
! PURPOSE: Wrapper to RAND_R4 to produce array of uniformly distributed values
!*******************************************************************************

! parameters
real, dimension(:,:,:,:) :: random_array
real, optional, intent(in) :: A, B

if (present(A) .and. present(B)) then
  call random_number(random_array)
  random_array = A + random_array * (B - A)
else
  call random_number(random_array)
end if

end subroutine RAND_ARRAY_4_R4

!-------------------------------------------------------------------------------

subroutine RAND_ARRAY_5_R4(random_array, A, B)
!*******************************************************************************
! RNORM_ARRAY, 5-dimensional
! PURPOSE: Wrapper to RAND_R4 to produce array of uniformly distributed values
!*******************************************************************************

! parameters
real, dimension(:,:,:,:,:) :: random_array
real, optional, intent(in) :: A, B

if (present(A) .and. present(B)) then
  call random_number(random_array)
  random_array = A + random_array * (B - A)
else
  call random_number(random_array)
end if

end subroutine RAND_ARRAY_5_R4

!-------------------------------------------------------------------------------

subroutine RAND_ARRAY_6_R4(random_array, A, B)
!*******************************************************************************
! RNORM_ARRAY, 6-dimensional
! PURPOSE: Wrapper to RAND_R4 to produce array of uniformly distributed values
!*******************************************************************************

! parameters
real, dimension(:,:,:,:,:,:) :: random_array
real, optional, intent(in) :: A, B

if (present(A) .and. present(B)) then
  call random_number(random_array)
  random_array = A + random_array * (B - A)
else
  call random_number(random_array)
end if

end subroutine RAND_ARRAY_6_R4

!-------------------------------------------------------------------------------

subroutine RAND_ARRAY_1_R8(random_array, A, B)
!*******************************************************************************
! RANDOM_ARRAY_1, 1-dimensional
! PURPOSE: Wrapper to RAND_R4 to produce array of uniformly distributed values
!*******************************************************************************

! parameters
real(kind=dp), dimension(:) :: random_array
real(kind=dp), optional, intent(in) :: A, B

if (present(A) .and. present(B)) then
  call random_number(random_array)
  random_array = A + random_array * (B - A)
else
  call random_number(random_array)
end if

end subroutine RAND_ARRAY_1_R8

!-------------------------------------------------------------------------------

subroutine RAND_ARRAY_2_R8(random_array, A, B)
!*******************************************************************************
! RAND_ARRAY, 2-dimensional
! PURPOSE: Wrapper to RAND_R4 to produce array of uniformly distributed values
!*******************************************************************************

! parameters
real(kind=dp), dimension(:,:) :: random_array
real(kind=dp), optional, intent(in) :: A, B

if (present(A) .and. present(B)) then
  call random_number(random_array)
  random_array = A + random_array * (B - A)
else
  call random_number(random_array)
end if

end subroutine RAND_ARRAY_2_R8

!-------------------------------------------------------------------------------

subroutine RAND_ARRAY_3_R8(random_array, A, B)
!*******************************************************************************
! RNORM_ARRAY, 3-dimensional
! PURPOSE: Wrapper to RAND_R4 to produce array of uniformly distributed values
!*******************************************************************************

! parameters
real(kind=dp), dimension(:,:,:) :: random_array
real(kind=dp), optional, intent(in) :: A, B

if (present(A) .and. present(B)) then
  call random_number(random_array)
  random_array = A + random_array * (B - A)
else
  call random_number(random_array)
end if

end subroutine RAND_ARRAY_3_R8

!-------------------------------------------------------------------------------

subroutine RAND_ARRAY_4_R8(random_array, A, B)
!*******************************************************************************
! RNORM_ARRAY, 4-dimensional
! PURPOSE: Wrapper to RAND_R4 to produce array of uniformly distributed values
!*******************************************************************************

! parameters
real(kind=dp), dimension(:,:,:,:) :: random_array
real(kind=dp), optional, intent(in) :: A, B

if (present(A) .and. present(B)) then
  call random_number(random_array)
  random_array = A + random_array * (B - A)
else
  call random_number(random_array)
end if

end subroutine RAND_ARRAY_4_R8

!-------------------------------------------------------------------------------

subroutine RAND_ARRAY_5_R8(random_array, A, B)
!*******************************************************************************
! RNORM_ARRAY, 5-dimensional
! PURPOSE: Wrapper to RAND_R4 to produce array of uniformly distributed values
!*******************************************************************************

! parameters
real(kind=dp), dimension(:,:,:,:,:) :: random_array
real(kind=dp), optional, intent(in) :: A, B

if (present(A) .and. present(B)) then
  call random_number(random_array)
  random_array = A + random_array * (B - A)
else
  call random_number(random_array)
end if

end subroutine RAND_ARRAY_5_R8

!-------------------------------------------------------------------------------

subroutine RAND_ARRAY_6_R8(random_array, A, B)
!*******************************************************************************
! RNORM_ARRAY, 6-dimensional
! PURPOSE: Wrapper to RAND_R4 to produce array of uniformly distributed values
!*******************************************************************************

! parameters
real(kind=dp), dimension(:,:,:,:,:,:) :: random_array
real(kind=dp), optional, intent(in) :: A, B

if (present(A) .and. present(B)) then
  call random_number(random_array)
  random_array = A + random_array * (B - A)
else
  call random_number(random_array)
end if

end subroutine RAND_ARRAY_6_R8

!-------------------------------------------------------------------------------

subroutine RAND_ARRAY_1_I(random_array, a, b)
!*******************************************************************************
! RANDOM_ARRAY_1, 1-dimensional
! PURPOSE: Wrapper to RAND_I to produce array of uniformly distributed integers
! PARAMETERS: Integer array, minimum cvalue, maximum value
!*******************************************************************************

! parameters
integer, dimension(:) :: random_array
integer :: a, b

! local variables
integer :: i

do i=1, ubound(random_array,1)
  random_array(i) = RAND_I(a, b)
end do

end subroutine RAND_ARRAY_1_I

!-------------------------------------------------------------------------------

subroutine RAND_ARRAY_2_I(random_array, a, b)
!*******************************************************************************
! RAND_ARRAY, 2-dimensional
! PURPOSE: Wrapper to RAND_I to produce array of uniformly distributed integers
! PARAMETERS: Integer array, minimum cvalue, maximum value
!*******************************************************************************

! parameters
integer, dimension(:,:) :: random_array
integer :: a, b

! local variables
integer :: i, j

do i=1, ubound(random_array,2)
  do j=1, ubound(random_array,1)
    random_array(j,i) = RAND_I(a, b)
  end do
end do

end subroutine RAND_ARRAY_2_I

!-------------------------------------------------------------------------------

subroutine RAND_ARRAY_3_I(random_array, a, b)
!*******************************************************************************
! RNORM_ARRAY, 3-dimensional
! PURPOSE: Wrapper to RAND_I to produce array of uniformly distributed integers
! PARAMETERS: Integer array, minimum cvalue, maximum value
!*******************************************************************************

! parameters
integer, dimension(:,:,:) :: random_array
integer :: a, b

! local variables
integer :: i, j, k

do i=1, ubound(random_array,3)
  do j=1, ubound(random_array,2)
    do k=1, ubound(random_array,1)
      random_array(k,j,i) = RAND_I(a, b)
    end do
  end do
end do

end subroutine RAND_ARRAY_3_I

!-------------------------------------------------------------------------------

subroutine RAND_ARRAY_4_I(random_array, a, b)
!*******************************************************************************
! RNORM_ARRAY, 4-dimensional
! PURPOSE: Wrapper to RAND_I to produce array of uniformly distributed integers
! PARAMETERS: Integer array, minimum cvalue, maximum value
!*******************************************************************************

! parameters
integer, dimension(:,:,:,:) :: random_array
integer :: a, b

! local variables
integer :: i, j, k, l

do i=1, ubound(random_array,4)
  do j=1, ubound(random_array,3)
    do k=1, ubound(random_array,2)
      do l=1, ubound(random_array,1)
        random_array(l,k,j,i) = RAND_I(a, b)
      end do
    end do
  end do
end do

end subroutine RAND_ARRAY_4_I

!-------------------------------------------------------------------------------

subroutine RAND_ARRAY_5_I(random_array, a, b)
!*******************************************************************************
! RNORM_ARRAY, 5-dimensional
! PURPOSE: Wrapper to RAND_I to produce array of uniformly distributed integers
! PARAMETERS: Integer array, minimum cvalue, maximum value
!*******************************************************************************

! parameters
integer, dimension(:,:,:,:,:) :: random_array
integer :: a, b

! local variables
integer :: i, j, k, l, m

do i=1, ubound(random_array,5)
  do j=1, ubound(random_array,4)
    do k=1, ubound(random_array,3)
      do l=1, ubound(random_array,2)
        do m=1, ubound(random_array,1)
          random_array(m,l,k,j,i) = RAND_I(a, b)
        end do
      end do
    end do
  end do
end do

end subroutine RAND_ARRAY_5_I

!-------------------------------------------------------------------------------

subroutine RAND_ARRAY_6_I(random_array, a, b)
!*******************************************************************************
! RNORM_ARRAY, 6-dimensional
! PURPOSE: Wrapper to RAND_I to produce array of uniformly distributed integers
! PARAMETERS: Integer array, minimum cvalue, maximum value
!*******************************************************************************

! parameters
integer, dimension(:,:,:,:,:,:) :: random_array
integer :: a, b

! local variables
integer :: i, j, k, l, m, n

do i=1, ubound(random_array,6)
  do j=1, ubound(random_array,5)
    do k=1, ubound(random_array,4)
      do l=1, ubound(random_array,3)
        do m=1, ubound(random_array,2)
          do n=1, ubound(random_array,1)
            random_array(n,m,l,k,j,i) = RAND_I(a, b)
          end do
        end do
      end do
    end do
  end do
end do

end subroutine RAND_ARRAY_6_I

!-------------------------------------------------------------------------------

function RAND_RENORMALISED_R4(A, B) result(s_val)
!*******************************************************************************
! RAND_RENORMALISE_R4
! PURPOSE: Produce uniform random number renormalised from 0..1 to any arbitrary
!          range A..B.
!*******************************************************************************

  implicit none
  real:: s_val
  real:: A, B

  s_val = A+RAND_VAL_R4()*(B-A)

end function RAND_RENORMALISED_R4

function RAND_RENORMALISED_R4I(A, B) result(s_val)
!*******************************************************************************
! RAND_RENORMALISE_R4 for integer range
! PURPOSE: Produce uniform random number renormalised from 0..1 to any arbitrary
!          range A..B.
!*******************************************************************************

  implicit none
  real:: s_val
  integer:: A, B

  s_val = real(A)+RAND_VAL_R4()*(real(B)-real(A))

end function RAND_RENORMALISED_R4I

!-------------------------------------------------------------------------------

function RAND_RENORMALISED_R8(A, B) result(s_val)
!*******************************************************************************
! RAND_RENORMALISE_R8
! PURPOSE: Produce uniform random number renormalised from 0..1 to any arbitrary
!          range A..B.
!*******************************************************************************

  implicit none
  real(kind=dp):: s_val
  real(kind=dp):: A, B

  s_val = A+RAND_VAL_R8()*(B-A)

end function RAND_RENORMALISED_R8

function RAND_RENORMALISED_R8I(A, B) result(s_val)
!*******************************************************************************
! RAND_RENORMALISE_R8
! PURPOSE: Produce uniform random number renormalised from 0..1 to any arbitrary
!          range A..B.
!*******************************************************************************

  implicit none
  real(kind=dp):: s_val
  integer :: A, B

  s_val = real(A,8)+RAND_VAL_R8()*(real(B,8)-real(A,8))

end function RAND_RENORMALISED_R8I

!-------------------------------------------------------------------------------

function RNORM_RENORMALISED_R4(mean, variance) result (fn_val)
!*******************************************************************************
! RNORM_RENORMALISED_R4
! PURPOSE: Produce Gaussian random number renormalised from X=0 s=1 to any
!          arbitrary mean and variance
! THEORY and DETAILS: if x is a random variable whose mean is μx and variance
!          is σ^2x, then the random variable, y, defined by y = ax + b,
!          where a and b are constants, has mean μy = a μx + b
!          and variance σ^2y = a^2 σ^2x.
!          Solving the system for specific case when μx=0 and σ^2x=1 (standard
!          Gaussian generator), we get (solved using wxMaxima)
!             solve ([a*0+b=my, a^2*1^2=sy] , [a,b]);
!             [[a=−sqrt(sy),b=my],[a=sqrt(sy),b=my]]
!*******************************************************************************

  implicit none
  real :: fn_val
  real, intent(in) :: mean, variance

  ! local variables
  real :: A, B

  A = sqrt(variance)
  B = mean

  fn_val = A * RNORM_VAL_R4() + B

end function RNORM_RENORMALISED_R4

!-------------------------------------------------------------------------------

function RNORM_RENORMALISED_R8(mean, variance) result (fn_val)
!*******************************************************************************
! RNORM_RENORMALISED_R4
! PURPOSE: Produce Gaussian random number renormalised from X=0 s=1 to any
!          arbitrary mean and variance
! THEORY and DETAILS: if x is a random variable whose mean is μx and variance
!          is σ^2x, then the random variable, y, defined by y = ax + b,
!          where a and b are constants, has mean μy = a μx + b
!          and variance σ^2y = a^2 σ^2x.
!          Solving the system for specific case when μx=0 and σ^2x=1 (standard
!          Gaussian generator), we get (solved using wxMaxima)
!             solve ([a*0+b=my, a^2*1^2=sy] , [a,b]);
!             [[a=−sqrt(sy),b=my],[a=sqrt(sy),b=my]]
!*******************************************************************************

  implicit none
  real(kind=dp) :: fn_val
  real(kind=dp), intent(in) :: mean, variance

  ! local variables
  real :: A, B

  A = sqrt(variance)
  B = mean

  fn_val = A * RNORM_VAL_R8() + B

end function RNORM_RENORMALISED_R8

!-------------------------------------------------------------------------------

function PERMUTE_RANDOM(N) result (p)
!*******************************************************************************
! RPERMUTE
! PURPOSE: Produce a random integer permutation array, i.e. an array from 1 to N
!          in a random order.
! Author: Bart Vandewoestyne; Based on http://coding.derkeiler.com/Archive/
!          Fortran/comp.lang.fortran/2006-03/msg00743.html
!*******************************************************************************

  integer, intent(in) :: N
  integer, dimension(N) :: p

  integer :: j, k
  real :: u

  p = 0

  do j=1,N

    call random_number(u)
    k = floor(j*u) + 1

    p(j) = p(k)
    p(k) = j

  end do

end function PERMUTE_RANDOM

end module BASE_RANDOM  ! <EOF>
