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
  ToStrA = trim(tmpStr) ! We don't need to adjustl here, with INT, unlike REAL

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

  tmpStr = trim(adjustl(tmpStr))    ! we have to remove leading/trailing blanks
  ToStrA = tmpStr
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
  ! There seems to be a compiler bug for trim(adjustl(XXX)). Therefore, now
  ! used a two stages, with self-change first tmpstr=trim(adjustl(tmpstr))

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

  tmpStr = trim(adjustl(tmpStr))    ! we have to remove leading/trailing blanks
  ToStrA = tmpStr
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
  ! There seems to be a compiler bug for trim(adjustl(XXX)). Therefore, now
  ! used a two stages, with self-change first tmpstr=trim(adjustl(tmpstr))

end function STR_R8TOA

!-------------------------------------------------------------------------------











end module BASE_UTILS
