module BASE_UTILS
!*******************************************************************************
! BASE_UTILS:
! PURPOSE: This module provides basic high level utilities that are used in
!   Different models (and should be of general applicability).
! VERSION AND DATE: 0.3, 2015/11/30
! CONTENTS:
!
! EXAMPLE:
!
! NOTES:
!
!*******************************************************************************

implicit none

! Module name for the DEBUG LOGGER: every function/sub must also have
! the PROCNAME parameter referring to its name. This is done for the Debug
! Logger module. Each module must also have a DEBUG Logger subroutine, that
! is a wrapper to module LOGGER (or perhaps any other that is being used)
!   procedure name PROCNAME
character (len=*), private, parameter :: MODNAME = "BASE_UTILS"

! Set the debug mode to ON or OFF, in the debug mode, events are written to
! the log, determined by the LOG_DBG subroutine, normally, a wrapper to the
! module LOGGER. May also define integer DEBUG_LEVEL parameter...
logical, private, parameter :: IS_DEBUG = .FALSE.

!*******************************************************************************
! GENERIC INTERFACES
! Generic interfaces to the modules....
!*******************************************************************************

interface NUMTOSTR            ! Generic interface to number-to-string
                              ! conversion functions. We have two forms:
  module procedure STR_ITOA   ! NUMTOSTR ( number) and TOSTR ( number)
  module procedure STR_RTOA   ! for convenience, they 're identical
  module procedure STR_R8TOA

  module procedure STR_ARRAY_ITOA
  module procedure STR_ARRAY_RTOA
  module procedure STR_ARRAY_R8TOA
  module procedure STR_ITOA_LZ

end interface NUMTOSTR

interface TOSTR               ! Generic interface to number-to-string
                              ! conversion functions. We have two forms:
  module procedure STR_ITOA   ! NUMTOSTR ( number) and TOSTR ( number)
  module procedure STR_RTOA   ! for convenience, they're identical
  module procedure STR_R8TOA
  module procedure STR_LTOA
  module procedure STR_ATOA

  module procedure STR_ARRAY_ITOA
  module procedure STR_ARRAY_RTOA
  module procedure STR_ARRAY_R8TOA
  module procedure STR_ARRAY_LTOA
  module procedure STR_ARRAY_ATOA
  module procedure STR_ITOA_LZ

end interface TOSTR

interface STR                 ! An "alias" to TOSTR

  module procedure STR_ITOA
  module procedure STR_RTOA 
  module procedure STR_R8TOA
  module procedure STR_LTOA
  module procedure STR_ATOA

  module procedure STR_ARRAY_ITOA
  module procedure STR_ARRAY_RTOA
  module procedure STR_ARRAY_R8TOA
  module procedure STR_ARRAY_LTOA
  module procedure STR_ARRAY_ATOA
  module procedure STR_ITOA_LZ

end interface STR

interface STDOUT              ! Short name for stdout-output routine

  module procedure OUT_FREE_STDOUT

end interface STDOUT

interface STDERR              ! Short name for stderr-output routine

  module procedure OUT_FREE_STDERR

end interface STDERR

private :: I4_WIDTH, I4_LOG_10  ! They are identical in CSV_IO and BASE_UTILS.
                                ! Private here to avoid possible name conflicts, 
                                ! do we need them outside?

private :: LOG_DBG  ! This wrapper DEBUG LOG is used only for this module, it
                    ! may or may not use the module LOGGER, if not, it can be
                    ! used as a stand-alone module in other projects... But it
                    ! has the same name as in the model proto

!-------------------------------------------------------------------------------
contains  !-----[ SUBROUTINES AND FUNCTIONS FOLLOW ]----------------------------

subroutine LOG_DBG(message_string, procname, modname)
!*******************************************************************************
! LOG_DBG
! PURPOSE: This subroutine is a wrapper for writing debug messages. It can
! either just print to STDERR or use the module LOGGER. Here the selection
! of the module behaviour is made by commenting out unused code, possibly
! change to conditional compilation with preprocessor... but portability will
! be problem in such a case...
!*******************************************************************************

  !#ifdef USE_LOGGER_MODULE
  !use LOGGER      ! we might need logger later
  !#endif
  use, intrinsic :: ISO_FORTRAN_ENV ! need it for write(ERROR_UNIT, *)

  implicit none

  ! Calling parameters
  character(len=*), intent(in) :: message_string
  character (len=*), optional, intent(in) :: procname
  character (len=*), optional, intent(in) :: modname

  ! Local variables
  character (len=:), allocatable :: prefix_msg

  !-----------------------------------------------------------------------------

  if (IS_DEBUG) then        ! Only if IS_DEBUG is set to TRUE, this sub is not
                            ! used in normal operation when not debugging
    ! We first generate the message prefix containing module and procedure name
    if (present(procname)) then
      if (present(modname)) then
        prefix_msg="MODULE:" // modname // "PROCEDURE: " // procname // ":: "
      else
        prefix_msg="PROCEDURE: " // procname // ":: "
      end if
    else
      if (present(modname)) then
        prefix_msg="MODULE:" // modname // ":: "
      else
        prefix_msg=""
      end if
    end if

    ! Second, we print  the message prefix + message
    !#ifdef USE_LOGGER_MODULE
    !call LOG_MSG( prefix_msg // message_string )  ! use module LOGGER
    !# else
    write(ERROR_UNIT, *) prefix_msg, message_string
    !#endif

  end if

end subroutine LOG_DBG

!-------------------------------------------------------------------------------

function STR_ITOA(i, formatstr) result (ToStrA)
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
  character(len=*), optional, intent(in) :: formatstr

  ! Local variables
  character(range(i)+2) :: tmpStr
  character(len=:), allocatable :: tmpFormat


  ! Subroutine name for DEBUG LOGGER
  character (len=*), parameter :: PROCNAME = "STR_ITOA"

  !--------------------------------------------------

  if (present(formatstr))  then
    tmpFormat=formatstr
    write(tmpStr, tmpFormat) i
    ToStrA = trim(tmpStr)
  else
    write(tmpStr,'(i0)') i
    ToStrA = CLEANUP(tmpStr)        ! see notes on _R and _R8 versions below
  end if

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

  ! Subroutine name for DEBUG LOGGER
  character (len=*), parameter :: PROCNAME = "STR_RTOA"

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

  ! Subroutine name for DEBUG LOGGER
  character (len=*), parameter :: PROCNAME = "STR_R8TOA"

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

function STR_LTOA (L) result (ToStrA)

!*******************************************************************************
! PURPOSE: Convert LOGICAL to a string type.
! CALL PARAMETERS: single LOGICAL value
!
! EXAMPLE:
!          Str_NAME = "Status Flag = " // STR_LTOA(file_write_status)
!*******************************************************************************

  implicit none

  ! Function value
  character(len=:), allocatable :: ToStrA

  ! Calling parameters
  logical :: L

  ! Subroutine name for DEBUG LOGGER
  character (len=*), parameter :: PROCNAME = "STR_LTOA"

  !-----------------------------------------------------------------------------

  if (L) then
    ToStrA = "TRUE"
  else
    ToStrA = "FALSE"
  end if

end function STR_LTOA

!-------------------------------------------------------------------------------

function STR_ATOA (A) result (ToStrA)

!*******************************************************************************
! PURPOSE: Convert STRING to a string type.
! CALL PARAMETERS: string
! NOTE:
!  Safety function, to allow wrong inclusion of TOSTR with string variables,
!  TOSTR should normally be applied to convert number to string.
!*******************************************************************************

  implicit none

  ! Function value
  character(len=:), allocatable :: ToStrA

  ! Calling parameters
  character(len=*) :: A

  ! Subroutine name for DEBUG LOGGER
  character (len=*), parameter :: PROCNAME = "STR_ATOA"

  !-----------------------------------------------------------------------------

  ToStrA = A

end function STR_ATOA

!-------------------------------------------------------------------------------

function STR_ARRAY_ITOA (r,formatstr) result(ToStrA)
!*******************************************************************************
! PURPOSE: Convert integer array to a string type.
! CALL PARAMETERS: integer array
!                  optional format string
! EXAMPLE:
!          Str_NAME = "N=" // STR_ARRAY_ITOA(iNum)
!*******************************************************************************

  implicit none

  ! Function value
  character(len=:), allocatable :: ToStrA

  ! Calling parameters
  integer, dimension(:), intent(in) :: r
  character(len=*), optional, intent(in) :: formatstr

  ! Local variables
  character (len=:), allocatable :: tmpStr
  integer :: i

  ! Subroutine name for DEBUG LOGGER
  character (len=*), parameter :: PROCNAME = "STR_ARRAY_ITOA"

  !-----------------------------------------------------------------------------

  tmpStr=""

  do i=lbound(r,1), ubound(r,1)
    if (present(formatstr)) then
      tmpStr=tmpStr // " " // STR_ITOA(r(i), formatstr)
    else
      tmpStr = tmpStr // " " // STR_ITOA(r(i))
    end if
  end do


  ToStrA = tmpStr

end function STR_ARRAY_ITOA

!-------------------------------------------------------------------------------

function STR_ARRAY_RTOA (r,formatstr) result(ToStrA)
!*******************************************************************************
! PURPOSE: Convert REAL array to a string type.
! CALL PARAMETERS: real array
!                  optional format string
! EXAMPLE:
!          Str_NAME = "Pi=" // STR_ARRAY_RTOA(rNumbers)
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
  real, dimension(:), intent(in) :: r
  character(len=*), optional, intent(in) :: formatstr

  ! Local variables
  character (len=:), allocatable :: tmpStr
  integer :: i

  ! Subroutine name for DEBUG LOGGER
  character (len=*), parameter :: PROCNAME = "STR_ARRAY_RTOA"

  !-------------------------------------------------------
  tmpStr=""

  do i=lbound(r,1), ubound(r,1)
    if (present(formatstr)) then
      tmpStr=tmpStr // " " // STR_RTOA(r(i), formatstr)
    else
      tmpStr = tmpStr // " " // STR_RTOA(r(i))
    end if
  end do

  ToStrA = tmpStr

end function STR_ARRAY_RTOA

!-------------------------------------------------------------------------------

function STR_ARRAY_R8TOA (r,formatstr) result(ToStrA)
!*******************************************************************************
! PURPOSE: Convert REAL kind 8 array to a string type.
! CALL PARAMETERS: real kind 8 array
!                  optional format string
! EXAMPLE:
!          Str_NAME = "Pi=" // STR_ARRAY_RTOA(rNumbers)
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
  real (kind=8), dimension(:), intent(in) :: r
  character(len=*), optional, intent(in) :: formatstr

  ! Local variables
  character (len=:), allocatable :: tmpStr
  integer :: i

  ! Subroutine name for DEBUG LOGGER
  character (len=*), parameter :: PROCNAME = "STR_ARRAY_R8TOA"

  !-------------------------------------------------------
  tmpStr=""

  do i=lbound(r,1), ubound(r,1)
    if (present(formatstr)) then
      tmpStr=tmpStr // " " // STR_R8TOA(r(i), formatstr)
    else
      tmpStr = tmpStr // " " // STR_R8TOA(r(i))
    end if
  end do

  ToStrA = tmpStr

end function STR_ARRAY_R8TOA

!-------------------------------------------------------------------------------

function STR_ARRAY_LTOA (r) result(ToStrA)
!*******************************************************************************
! PURPOSE: Convert LOGICAL array to a string type.
! CALL PARAMETERS: Logical array
!                  optional format string
! EXAMPLE:
!          Str_NAME = "Pi=" // STR_ARRAY_RTOA(rNumbers)
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
  logical, dimension(:), intent(in) :: r

  ! Local variables
  character (len=:), allocatable :: tmpStr
  integer :: i

  ! Subroutine name for DEBUG LOGGER
  character (len=*), parameter :: PROCNAME = "STR_ARRAY_LTOA"

  !-------------------------------------------------------

  tmpStr=""
  do i=lbound(r,1), ubound(r,1)
    tmpStr = tmpStr // " " // STR_LTOA(r(i))
  end do

  ToStrA = tmpStr

end function STR_ARRAY_LTOA

!-------------------------------------------------------------------------------

function STR_ARRAY_ATOA (r, delimiter) result(ToStrA)
!*******************************************************************************
! PURPOSE: Convert STRING array to string type.
! CALL PARAMETERS: STRING array
! NOTE:
!  Safety function, to allow wrong inclusion of TOSTR with string variables,
!  TOSTR should normally be applied to convert number to string.
!*******************************************************************************

  implicit none

  ! Function value
  character(len=:), allocatable :: ToStrA

  ! Calling parameters
  character(len=*), dimension(:), intent(in) :: r
  character(len=*), optional, intent(in) :: delimiter

  ! Local variables
  character (len=:), allocatable :: tmpStr
  character (len=:), allocatable :: delimiter_here
  integer :: i

  ! Subroutine name for DEBUG LOGGER
  character (len=*), parameter :: PROCNAME = "STR_ARRAY_ATOA"

  !-------------------------------------------------------

  if (present(delimiter)) then
    delimiter_here=delimiter
  else
    delimiter_here=" "
  end if

  tmpStr=r(lbound(r,1))
  do i=lbound(r,1)+1, ubound(r,1)
    tmpStr = tmpStr // delimiter_here // r(i)
  end do

  ToStrA = tmpStr

end function STR_ARRAY_ATOA

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

  ! Subroutine name for DEBUG LOGGER
  character (len=*), parameter :: PROCNAME = "CLEANUP"

  !-----------------------------------------------------------------------------

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

  ! Subroutine name for DEBUG LOGGER
  character (len=*), parameter :: PROCNAME = "OUT_FREE_STDOUT"

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

  ! Subroutine name for DEBUG LOGGER
  character (len=*), parameter :: PROCNAME = "OUT_FREE_STDERR"

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

  integer :: i, n, clock                ! DEFAULT INTEGER type, for portability
  integer, dimension(:), allocatable :: seed

  ! Subroutine name for DEBUG LOGGER
  character (len=*), parameter :: PROCNAME = "RANDOM_SEED_INIT"

  !-----------------------------------------------------------------------------

  call random_seed(size = n)
  allocate(seed(n))

  call system_clock(count=clock)

  seed = clock + 37 * (/ (i - 1, i = 1, n) /)
  call random_seed(PUT = seed)

  deallocate(seed)

end subroutine RANDOM_SEED_INIT

!-------------------------------------------------------------------------------


function TIMESTAMP_FULL() result (tstamp_string)
!*******************************************************************************
! TIMESTAMP
! PURPOSE: returns the current date as a human readable time stamp, e.g.
!          19 November 2015  11:13:43.662 PM
! CALL PARAMETERS:
!   None
! RETURNS:
!   String timestamp
! NOTES: Useful for CSV file header
! Author: John Burkardt : This code is distributed under the GNU LGPL license.
! Modified by Sergey Budaev
!*******************************************************************************

  implicit none

  ! Function value
  character (len=35) :: tstamp_string

  ! Local variables
  character (len=8) :: ampm
  integer :: d
  integer :: h
  integer :: m
  integer :: mm
  character (len=9), parameter, dimension(12) :: month = (/ &
    'January  ', 'February ', 'March    ', 'April    ', &
    'May      ', 'June     ', 'July     ', 'August   ', &
    'September', 'October  ', 'November ', 'December ' /)
  integer :: n
  integer :: s
  integer :: values(8)
  integer :: y

  ! Subroutine name for DEBUG LOGGER
  character (len=*), parameter :: PROCNAME = "TIMESTAMP_FULL"

  !-----------------------------------------------------------------------------

  call date_and_time (values=values)

  y = values(1)
  m = values(2)
  d = values(3)
  h = values(5)
  n = values(6)
  s = values(7)
  mm = values(8)

  if ( h < 12 ) then
    ampm = 'AM'
  else if ( h == 12 ) then
    if ( n == 0 .and. s == 0 ) then
      ampm = 'Noon'
    else
      ampm = 'PM'
    end if
  else
    h = h - 12
    if ( h < 12 ) then
      ampm = 'PM'
    else if ( h == 12 ) then
      if ( n == 0 .and. s == 0 ) then
        ampm = 'Midnight'
      else
        ampm = 'AM'
      end if
    end if
  end if

  write (tstamp_string, '(i2,1x,a,1x,i4,2x,i2,a1,i2.2,a1,i2.2,a1,i3.3,1x,a)' ) &
    d, trim(month(m)), y, h, ':', n, ':', s, '.', mm, trim(ampm)

end function TIMESTAMP_FULL

!-------------------------------------------------------------------------------

function STR_ITOA_LZ(i, maxi) result (ToStrA)
!*******************************************************************************
! STR_ITOA_LZ
! PURPOSE: Convert integer to a string type including leading zeros. 
!          Useful for generating file and variable names and other strings that
!          that contain a numerical part with fixed width.
! CALL PARAMETERS: integer
!                  integer setting the maximum length of the digit string
! EXAMPLE:
!          FileName = "File" // STR_ITOA_LZ(10, 100) // ".txt"
!          results in: File_0010.txt
!*******************************************************************************
  
  implicit none

  ! Function value
  character(len=:), allocatable  :: ToStrA

  ! Calling parameters
  integer, intent(in) :: i
  integer, intent(in) :: maxi
  
  ! Local variables
  integer :: iwidth, fwidth
  
  ! Subroutine name for DEBUG LOGGER
  character (len=*), parameter :: PROCNAME = "FMLEADZEROS"
  
  iwidth = I4_WIDTH(i)
  fwidth = I4_WIDTH(maxi)
  
  if ( i < 0 ) then
    ToStrA = "-" // repeat("0", fwidth-iwidth+1) // TOSTR(abs(i))
  else
    ToStrA = repeat("0", fwidth-iwidth) // TOSTR(i)
  end if

end function STR_ITOA_LZ

!-------------------------------------------------------------------------------

function I4_WIDTH (i) result (i4width)
!*******************************************************************************
! I4_WIDTH
! PURPOSE: returns the "width" of an I4, the number of characters necessary
!   to represent the integer in base 10, including a negative sign if necessary.
! CALL PARAMETERS:
!    Integer value
! USES: I4_LOG_10 from the same module
! NOTE:
!    The width of an integer is the number of characters necessary to print it.
!    The width of an integer can be useful when setting the appropriate output
!    format for a vector or array of values.
!    An I4 is an integer value.
! EXAMPLE:
!        I  I4_WIDTH
!    -----  -------
!    -1234    5
!     -123    4
!      -12    3
!       -1    2
!        0    1
!        1    1
!       12    2
!      123    3
!     1234    4
!    12345    5
! Author: John Burkardt : This code is distributed under the GNU LGPL license.
! Modified by Sergey Budaev
!*******************************************************************************

  implicit none

  ! Function value
  integer :: i4width

  ! Calling parameters
  integer :: i

  ! Subroutine name for DEBUG LOGGER
  character (len=*), parameter :: PROCNAME = "I4_WIDTH"

  !-----------------------------------------------------------------------------

  if ( 0 < i ) then
    i4width = I4_LOG_10 ( i ) + 1
  else if ( i == 0 ) then
    i4width = 1
  else if ( i < 0 ) then
    i4width = I4_LOG_10 ( i ) + 2
  end if

end function I4_WIDTH

!-------------------------------------------------------------------------------

function I4_LOG_10 (i) result(i4log10)
!*******************************************************************************
! I4_LOG_10
! PURPOSE: returns the integer part of the logarithm base 10 of the absolute
!   value of an integer X.
! CALL PARAMETERS:
!   the number whose logarithm base 10
! EXAMPLE:
!   I4_LOG_10 (I) + 1 is the number of decimal digits in I. I4 is an integer.
!        I  I4_LOG_10
!    -----  --------
!        0    0
!        1    0
!        2    0
!        9    0
!       10    1
!       11    1
!       99    1
!      100    2
!      101    2
!      999    2
!     1000    3
!     1001    3
!     9999    3
!    10000    4
! Author: John Burkardt : This code is distributed under the GNU LGPL license.
! Modified by Sergey Budaev
!*******************************************************************************

  implicit none

  ! Function value
  integer :: i4log10

  ! Calling parameters
  integer :: i

  ! Local variables
  integer :: i_abs
  integer :: ten_pow

  ! Subroutine name for DEBUG LOGGER
  character (len=*), parameter :: PROCNAME = "I4_LOG_10"

  !-----------------------------------------------------------------------------

  if ( i == 0 ) then
    i4log10 = 0
  else
    i4log10 = 0
    ten_pow = 10
    i_abs = abs ( i )
    do while ( ten_pow <= i_abs )
      i4log10 = i4log10 + 1
      ten_pow = ten_pow * 10
    end do
  end if

end function I4_LOG_10

!-------------------------------------------------------------------------------

end module BASE_UTILS
