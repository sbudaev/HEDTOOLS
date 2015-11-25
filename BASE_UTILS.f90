module BASE_UTILS
!*******************************************************************************
! PURPOSE: This module provides basic high level utilities that are used in
!   Different models (and should be of general applicability).
!
! CONTENTS:
!
! EXAMPLE:
!
! NOTES:
!
!*******************************************************************************

implicit none

interface NUMTOSTR            ! Generic interface to number-to-string
                              ! conversion functions. We have two forms:
  module procedure STR_ITOA   ! NUMTOSTR ( number) and TOSTR ( number)
  module procedure STR_RTOA   ! for convenience, they 're identical
  module procedure STR_R8TOA

end interface NUMTOSTR

interface TOSTR               ! Generic interface to number-to-string
                              ! conversion functions. We have two forms:
  module procedure STR_ITOA   ! NUMTOSTR ( number) and TOSTR ( number)
  module procedure STR_RTOA   ! for convenience, they're identical
  module procedure STR_R8TOA

end interface TOSTR

interface STDOUT              ! Short name for stdout-output routine

  module procedure OUT_FREE_STDOUT

end interface STDOUT

interface STDERR              ! Short name for stderr-output routine

  module procedure OUT_FREE_STDERR

end interface STDERR

!-------------------------------------------------------------------------------
contains  !-----[ SUBROUTINES AND FUNCTIONS FOLLOW ]----------------------------

function STR_ITOA(i) result (ToStrA)
!*******************************************************************************
! PURPOSE: Convert INTEGER to a string type.
! CALL PARAMETERS: single integer value
! EXAMPLE:
!          Str_NAME = STR_ITOA(inumber)
!          Str_HEADER = "MODEL_" // STR_ITOA(4)
!*******************************************************************************

! Convert INTEGER to a string type. Trivial:)
! *** This function requires using mandatory
! interface. In such a case STR_ITOA should not
! be declared separately  (e.g. with variables)

  implicit none

  ! Function value
  character(len=:), allocatable  :: ToStrA

  ! Calling parameters
  integer, intent(in) :: i

  ! Local variables
  character(range(i)+2) :: tmpStr

  !--------------------------------------------------

  write(tmpStr,'(i0)') i
  ToStrA = CLEANUP(tmpStr)          ! see notes on _R and _R8 versions below

end function STR_ITOA

!-------------------------------------------------------------------------------

function STR_RTOA(r,formatstr) result (ToStrA)
!*******************************************************************************
! PURPOSE: Convert REAL to a string type.
! CALL PARAMETERS: single real value
!                  optional format string
! EXAMPLE:
!          Str_NAME = "Pi=" // STR_RTOA(rNumberPi)
!          Str_NAME = "Pi=" // STR_RTOA(3.1415)
!          Str_Header = STR_RTOA(rNumber, "(f4.2)")
!*******************************************************************************

! Convert REAL to a string type. Trivial:)
! *** This function requires using mandatory
! interface. In such a case STR_ITOA should not
! be declared separately  (e.g. with variables)
! as used to be in old fortran.

  implicit none

  ! Function value
  character(len=:), allocatable :: ToStrA

  ! Calling parameters
  real, intent(in) :: r
  character(len=*), optional, intent(in) :: formatstr

  ! Local variables
  character(len=24) :: tmpStr ! quick and dirty, with allowance for a big float
  character(len=:), allocatable :: tmpFormat

  !-------------------------------------------------------

  ! we use the present() function to check for optional arguments
  if (present(formatstr))  then
    tmpFormat=formatstr
    write(tmpStr,tmpFormat) r
  else
    write(tmpStr,*) r               ! if format isn't provided on call do *
  endif

  ToStrA = CLEANUP(tmpStr)          ! we have to remove leading/trailing blanks
  ! Portability note: direct assignment
  !   ToStrA = trim(adjustl(tmpStr))
  ! resulted in a magical compiler error on Oracle Solaris Studio (both Linux
  ! and Solaris OS) ::
  ! <---cut--->
  ! ../BASE_UTILS.f90:
  ! f90comp: /scratch/bldmstr/hudson_prod/workspace/z-trunk-lang/label/ &
  ! intel-Linux-5/f90/fe/srcme/compiler/phases/concretize/concretize_&
  ! intrinsic.cpp:231: Assertion `assign' failed.
  ! f90: Fatal error in /home/budaev/bin/solarisstudio12.4/lib/compilers/ &
  ! f90comp : Signal number = 6
  ! <---end cut--->
  ! Therefore, we now use a portable function CLEANUP

end function STR_RTOA

!-------------------------------------------------------------------------------

function STR_R8TOA(r,formatstr) result (ToStrA)
!*******************************************************************************
! PURPOSE: Convert REAL to a string type.
! CALL PARAMETERS: single double precision (kind 8) value
!                  optional format string
! EXAMPLE:
!          Str_NAME = "Pi=" // STR_R8TOA(rNumberPi)
!          Str_NAME = "Pi=" // STR_R8TOA(3.14159265359_8)
!          Str_Header = STR_R8TOA(rNumber, "(f4.2)")
!*******************************************************************************

! Convert REAL to a string type. Trivial:)
! *** This function requires using mandatory
! interface. In such a case STR_ITOA should not
! be declared separately  (e.g. with variables)
! as used to be in old fortran.

  implicit none

  ! Function value
  character(len=:), allocatable :: ToStrA

  ! Calling parameters
  real (kind=8), intent(in) :: r
  character(len=*), optional, intent(in) :: formatstr

  ! Local variables
  character(len=68) :: tmpStr ! quick and dirty, with allowance for a big float
  character(len=:), allocatable :: tmpFormat

  !-------------------------------------------------------

  ! we use the present() function to check for optional arguments
  if (present(formatstr))  then
    tmpFormat=formatstr
    write(tmpStr,tmpFormat) r
  else
    write(tmpStr,*) r               ! if format isn't provided on call do *
  endif

  ToStrA = CLEANUP(tmpStr)          ! we have to remove leading/trailing blanks
  ! Portability note: direct assignment
  !   ToStrA = trim(adjustl(tmpStr))
  ! resulted in a magical compiler error on Oracle Solaris Studio (both Linux
  ! and Solaris OS) ::
  ! <---cut--->
  ! ../BASE_UTILS.f90:
  ! f90comp: /scratch/bldmstr/hudson_prod/workspace/z-trunk-lang/label/ &
  ! intel-Linux-5/f90/fe/srcme/compiler/phases/concretize/concretize_&
  ! intrinsic.cpp:231: Assertion `assign' failed.
  ! f90: Fatal error in /home/budaev/bin/solarisstudio12.4/lib/compilers/ &
  ! f90comp : Signal number = 6
  ! <---end cut--->
  ! Therefore, we now use a portable function CLEANUP

end function STR_R8TOA

!-------------------------------------------------------------------------------

function CLEANUP(instring) result (cleaned)
!*******************************************************************************
! PURPOSE: Removes spaces, tabs, and control characters in string
! CALL PARAMETERS: Character string
! NOTE: This is a modified version from the STRINGS module 
! (http://www.gbenthien.net/strings/index.html)
!*******************************************************************************

! Function value
character (len=:), allocatable :: cleaned

! Calling parameters
character(len=*), intent(in) :: instring

! Copies of calling parameters
character(len=:), allocatable :: str

! Local variables
character(len=1):: ch
character(len=len_trim(instring))::outstr
integer :: i, k, ich, lenstr

str=instring

str=adjustl(str)
lenstr=len_trim(str)
outstr=' '
k=0

do i=1,lenstr
  ch=str(i:i)
  ich=iachar(ch)
  select case(ich)
    case(0:32)  ! space, tab, or control character
         cycle
    case(33:)
      k=k+1
      outstr(k:k)=ch
  end select
end do

cleaned=trim(outstr)

end function CLEANUP

!-------------------------------------------------------------------------------

subroutine OUT_FREE_STDOUT(s01,s02,s03,s04,s05,s06,s07,s08,s09,s10, &
                           s11,s12,s13,s14,s15,s16,s17,s18,s19,s20, &
                           s21,s22,s23,s24,s25)
!*******************************************************************************
! PURPOSE: This subroutine produces free unformatted output messages to stdout
!
! CALL PARAMETERS: up to 25 strings of any length. These strings are then
!                  printed on separate lines. Numbers can be converted
!                  to strings with STR_ITOA (integer) or STR_RTOA (real).
!
! EXAMPLE:
!  call OUT_FREE_STDOUT("---------------------------------------------------",&
!                     ch01 // " = " // ch02 // STR_ITOA(inumber) // " ***", &
!                     ch10 // "; TEST NR= " // STR_RTOA(120.345), &
!                     "Pi equals to = " //  STR_RTOA(realPi, "(f4.2)"), &
!                     "---------------------------------------------------")
! NOTES: Because Fortran does not support variable number of parameters
! we can use optional parameters, more than actually needed (here max=25,
! cf. default terminal size 80x25), every parameters printed on separate line.
! We use optional parameters as a pseudo-variable number of arguments
! this version allows therefore more or less long printout in a single
! call of the sub, but of course we can call each line separately, although
! single run is more convenient if we instead of stdout use a a gui window
! or MATLAB message, if the model is called from within MATLAB.
! Anyway, it's better to modularise everything as much as possible
! *** TODO ultimately change to recursive function, to get rid of the
!          25 lines limit. But this fixed version may still be useful as a
!          prototype to external APIs whenever they don't support recursion.
!*******************************************************************************

  use, intrinsic :: ISO_FORTRAN_ENV   ! Portability, default unit for stdout
  implicit none

  ! Calling parameters
  character(len=*),optional,intent(in)              &
        :: s01,s02,s03,s04,s05,s06,s07,s08,s09,s10, &
           s11,s12,s13,s14,s15,s16,s17,s18,s19,s20, &
           s21,s22,s23,s24,s25

  !-----------------------------------------------------------------------------

  ! YES, it does trivial printing... But this way we can
  ! modularise everything, and change parts very quickly
  ! when needed, and avoid clutter of the main high level code

  if (present(s01)) write(OUTPUT_UNIT,*) s01
  if (present(s02)) write(OUTPUT_UNIT,*) s02
  if (present(s03)) write(OUTPUT_UNIT,*) s03
  if (present(s04)) write(OUTPUT_UNIT,*) s04
  if (present(s05)) write(OUTPUT_UNIT,*) s05
  if (present(s06)) write(OUTPUT_UNIT,*) s06
  if (present(s07)) write(OUTPUT_UNIT,*) s07
  if (present(s08)) write(OUTPUT_UNIT,*) s08
  if (present(s09)) write(OUTPUT_UNIT,*) s09
  if (present(s10)) write(OUTPUT_UNIT,*) s10
  if (present(s11)) write(OUTPUT_UNIT,*) s11
  if (present(s12)) write(OUTPUT_UNIT,*) s12
  if (present(s13)) write(OUTPUT_UNIT,*) s13
  if (present(s14)) write(OUTPUT_UNIT,*) s14
  if (present(s15)) write(OUTPUT_UNIT,*) s15
  if (present(s16)) write(OUTPUT_UNIT,*) s16
  if (present(s17)) write(OUTPUT_UNIT,*) s17
  if (present(s18)) write(OUTPUT_UNIT,*) s18
  if (present(s19)) write(OUTPUT_UNIT,*) s19
  if (present(s20)) write(OUTPUT_UNIT,*) s20
  if (present(s21)) write(OUTPUT_UNIT,*) s21
  if (present(s22)) write(OUTPUT_UNIT,*) s22
  if (present(s23)) write(OUTPUT_UNIT,*) s23
  if (present(s24)) write(OUTPUT_UNIT,*) s24
  if (present(s25)) write(OUTPUT_UNIT,*) s25

end subroutine OUT_FREE_STDOUT

!-------------------------------------------------------------------------------

subroutine OUT_FREE_STDERR(s01,s02,s03,s04,s05,s06,s07,s08,s09,s10, &
                           s11,s12,s13,s14,s15,s16,s17,s18,s19,s20, &
                           s21,s22,s23,s24,s25)
!*******************************************************************************
! PURPOSE: This subroutine produces free unformatted output messages to stderr
!
! CALL PARAMETERS: up to 25 strings of any length. These strings are then
!                  printed on separate lines. Numbers can be converted
!                  to strings with STR_ITOA (integer) or STR_RTOA (real).
!
! EXAMPLE:
!  call OUT_FREE_STDOUT("---------------------------------------------------",&
!                     ch01 // " = " // ch02 // STR_ITOA(inumber) // " ***", &
!                     ch10 // "; TEST NR= " // STR_RTOA(120.345), &
!                     "Pi equals to = " //  STR_RTOA(realPi, "(f4.2)"), &
!                     "---------------------------------------------------")
! NOTES: Because Fortran does not support variable number of parameters
! we can use optional parameters, more than actually needed (here max=25,
! cf. default terminal size 80x25), every parameters printed on separate line.
! We use optional parameters as a pseudo-variable number of arguments
! this version allows therefore more or less long printout in a single
! call of the sub, but of course we can call each line separately, although
! single run is more convenient if we instead of stderr use a a gui window
! or MATLAB message, if the model is called from within MATLAB.
! Anyway, it's better to modularise everything as much as possible
! *** TODO ultimately change to recursive function, to get rid of the
!          25 lines limit. But this fixed version may still be useful as a
!          prototype to external APIs whenever they don't support recursion.
!*******************************************************************************

  use, intrinsic :: ISO_FORTRAN_ENV   ! Portability, default unit for stderr
  implicit none

  ! Calling parameters
  character(len=*),optional,intent(in)              &
        :: s01,s02,s03,s04,s05,s06,s07,s08,s09,s10, &
           s11,s12,s13,s14,s15,s16,s17,s18,s19,s20, &
           s21,s22,s23,s24,s25

  !-----------------------------------------------------------------------------

  ! YES, it does trivial printing... But this way we can
  ! modularise everything, and change parts very quickly
  ! when needed, and avoid clutter of the main high level code

  if (present(s01)) write(ERROR_UNIT,*) s01
  if (present(s02)) write(ERROR_UNIT,*) s02
  if (present(s03)) write(ERROR_UNIT,*) s03
  if (present(s04)) write(ERROR_UNIT,*) s04
  if (present(s05)) write(ERROR_UNIT,*) s05
  if (present(s06)) write(ERROR_UNIT,*) s06
  if (present(s07)) write(ERROR_UNIT,*) s07
  if (present(s08)) write(ERROR_UNIT,*) s08
  if (present(s09)) write(ERROR_UNIT,*) s09
  if (present(s10)) write(ERROR_UNIT,*) s10
  if (present(s11)) write(ERROR_UNIT,*) s11
  if (present(s12)) write(ERROR_UNIT,*) s12
  if (present(s13)) write(ERROR_UNIT,*) s13
  if (present(s14)) write(ERROR_UNIT,*) s14
  if (present(s15)) write(ERROR_UNIT,*) s15
  if (present(s16)) write(ERROR_UNIT,*) s16
  if (present(s17)) write(ERROR_UNIT,*) s17
  if (present(s18)) write(ERROR_UNIT,*) s18
  if (present(s19)) write(ERROR_UNIT,*) s19
  if (present(s20)) write(ERROR_UNIT,*) s20
  if (present(s21)) write(ERROR_UNIT,*) s21
  if (present(s22)) write(ERROR_UNIT,*) s22
  if (present(s23)) write(ERROR_UNIT,*) s23
  if (present(s24)) write(ERROR_UNIT,*) s24
  if (present(s25)) write(ERROR_UNIT,*) s25

end subroutine OUT_FREE_STDERR

!-------------------------------------------------------------------------------

subroutine RANDOM_SEED_INIT()
!*******************************************************************************
! RANDOM_SEED_INIT
! PURPOSE: initialises the random seed
! CALL PARAMETERS: none
!*******************************************************************************

  implicit none
  integer, parameter :: STD=4   ! Here we have to use standard precision kind=4
                                ! DOUBLE does not seem to be supported
  integer(STD) :: i, n, clock
  integer(STD), dimension(:), allocatable :: seed

  call random_seed(size = n)
  allocate(seed(n))

  call system_clock(count=clock)

  seed = clock + 37 * (/ (i - 1, i = 1, n) /)
  call random_seed(PUT = seed)

  deallocate(seed)

end subroutine RANDOM_SEED_INIT

!-------------------------------------------------------------------------------












end module BASE_UTILS
