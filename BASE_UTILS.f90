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

interface NUMTOSTR
  ! Generic interface to number-to-string conversion functions.

  module procedure STR_ITOA
  module procedure STR_RTOA

end interface NUMTOSTR

!-------------------------------------------------------------------------------
contains  !-----[ SUBROUTINES AND FUNCTIONS FOLLOW ]----------------------------

function STR_ITOA(i) result (ToStrA)
!***************************************************
! PURPOSE: Convert INTEGER to a string type.
! CALL PARAMETERS: single integer value
! EXAMPLE:
!          Str_NAME = STR_ITOA(inumber)
!          Str_HEADER = "MODEL_" // STR_ITOA(4)
!***************************************************

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
  ToStrA = trim(tmpStr)

end function STR_ITOA

!-------------------------------------------------------------------------------

function STR_RTOA(r,formatstr) result (ToStrA)
!*******************************************************
! PURPOSE: Convert REAL to a string type.
! CALL PARAMETERS: single integer value
!                  optional format string
! EXAMPLE:
!          Str_NAME = "Pi=" // STR_RTOA(rNumberPi)
!          Str_NAME = "Pi=" // STR_RTOA(3.1415)
!          Str_Header = STR_RTOA(rNumber, "(f4.2)")
!*******************************************************

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
    write(tmpStr,*) r ! ipresentf format isn't provided on call do *
  endif
  ToStrA = trim(tmpStr)

end function STR_RTOA

!-------------------------------------------------------------------------------




end module BASE_UTILS
