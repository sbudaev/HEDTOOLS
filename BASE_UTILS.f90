module BASE_UTILS
!*******************************************************************************
! SVN $Id$
!*******************************************************************************
! BASE_UTILS:
! PURPOSE: This module provides basic high level utilities that are used in
!   Different models (and should be of general applicability).
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

interface NUMTOSTR               ! Generic interface to number-to-string
                                 ! conversion functions. We have two forms:
  module procedure STR_ITOA      ! NUMTOSTR ( number) and TOSTR ( number)
  module procedure STR_RTOA      ! for convenience, they 're identical
  module procedure STR_R8TOA

  module procedure STR_ARRAY_ITOA
  module procedure STR_ARRAY_RTOA
  module procedure STR_ARRAY_R8TOA
  module procedure STR_ITOA_LZ

end interface NUMTOSTR

interface TOSTR                  ! Generic interface to number-to-string
                                 ! conversion functions. We have two forms:
  module procedure STR_ITOA      ! NUMTOSTR ( number) and TOSTR ( number)
  module procedure STR_RTOA      ! for convenience, they're identical
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

interface STR                    ! An "alias" to TOSTR

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

interface STDOUT                 ! Short name for stdout-output routine

  module procedure OUT_FREE_STDOUT

end interface STDOUT

interface STDERR                 ! Short name for stderr-output routine

  module procedure OUT_FREE_STDERR

end interface STDERR

interface ARRAY_INDEX            ! Generic interface for calculating an
                                 ! unconstrained  integer vector of ranks
  module procedure MRGRNK_R4     ! for an input vector.
  module procedure MRGRNK_R8
  module procedure MRGRNK_I

  module procedure RNKPAR_R4     ! Generic interfaces for partial ranking
  module procedure RNKPAR_R8     ! obtained by adding an additional integer
  module procedure RNKPAR_I

end interface ARRAY_INDEX

interface LINTERPOL              ! Generic interface for simple linear
                                 ! interpolation functions.
   module procedure LINTERPOL_R4
   module procedure LINTERPOL_R8

end interface LINTERPOL

interface INTERP_LINEAR          ! Generic interface to linear interpolation
                                 ! and extrapolation procedure.
   module procedure INTERP_LINEAR_R4
   module procedure INTERP_LINEAR_R8

end interface INTERP_LINEAR

interface INTERP_LAGRANGE        ! Generic interface for polynominal
                                 ! interpolation and extrapolation routines.
   module procedure INTERP_LAGRANGE_R4
   module procedure INTERP_LAGRANGE_R8

end interface INTERP_LAGRANGE

interface VEC_ASCENDS_STRICTLY   ! Generic interface to ascending vector util.

   module procedure R8VEC_ASCENDS_STRICTLY
   module procedure R4VEC_ASCENDS_STRICTLY

end interface VEC_ASCENDS_STRICTLY

interface LIN_INTERPOL_VECTOR    ! Generic interface to linear interpolation
                                 ! vector-based wrapper.
   module procedure LIN_INTERPOL_VECTOR_R4
   module procedure LIN_INTERPOL_VECTOR_R8

end interface LIN_INTERPOL_VECTOR

interface LAGR_INTERPOL_VECTOR   ! Generic interface to Lagrange polynominal
                                 ! vector-based wrapper.
   module procedure LAGR_INTERPOL_VECTOR_R4
   module procedure LAGR_INTERPOL_VECTOR_R8

end interface LAGR_INTERPOL_VECTOR

!-------------------------------------------------------------------------------

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

subroutine RANDOM_SEED_INIT_SIMPLE()
!*******************************************************************************
! RANDOM_SEED_INIT_SIMPLE
! PURPOSE: initialises the random seed
! CALL PARAMETERS: none
!*******************************************************************************

  implicit none

  integer :: i, n, clock                ! DEFAULT INTEGER type, for portability
  integer, dimension(:), allocatable :: seed

  ! Subroutine name for DEBUG LOGGER
  character (len=*), parameter :: PROCNAME = "RANDOM_SEED_INIT_SIMPLE"

  !-----------------------------------------------------------------------------

  call random_seed(size = n)
  allocate(seed(n))

  call system_clock(count=clock)

  seed = clock + 37 * (/ (i - 1, i = 1, n) /)
  call random_seed(PUT = seed)

  deallocate(seed)

end subroutine RANDOM_SEED_INIT_SIMPLE

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
!          Useful for generating file and variable names and other strings
!          that contain a numerical part with fixed width.
! CALL PARAMETERS: integer
!                  integer setting the maximum length of the digit string
! EXAMPLE:
!          FileName = "File_" // STR_ITOA_LZ(10, 1000) // ".txt"
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
  character (len=*), parameter :: PROCNAME = "STR_ITOA_LZ"

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

subroutine MRGRNK_R4 (xdont, irngt)
!*******************************************************************************
! MRGRNK_R
! PURPOSE: Unconditionally rank real vector placing ranks into an integer
!          vector. This is a real kind 4 version.
! CALL PARAMETERS: Real vector for ranking and integer vector containing
!          the ranks obtained.
! NOTES: From public domain code http://www.fortran-2000.com/rank/
!        Author: Michel Olagnon <Michel.Olagnon@ifremer.fr>.
!        IMPORTANT: This is a performance-optimised code. For performance
!        reasons, the first 2 passes are taken out of the standard loop, and
!        use dedicated coding.
!*******************************************************************************
! __________________________________________________________
!   mrgrnk = merge-sort ranking of an array
!   for performance reasons, the first 2 passes are taken
!   out of the standard loop, and use dedicated coding.
! __________________________________________________________
! _________________________________________________________
      real, dimension (:), intent (in) :: xdont
      integer, dimension (:), intent (out) :: irngt
! __________________________________________________________
      real :: xvala, xvalb
!
      integer, dimension (size(irngt)) :: jwrkt
      integer :: lmtna, lmtnc, irng1, irng2
      integer :: nval, iind, iwrkd, iwrk, iwrkf, jinda, iinda, iindb
!
      nval = min (size(xdont), size(irngt))
      select case (nval)
      case (:0)
        return
      case (1)
        irngt (1) = 1
        return
      case default
        continue
      end select
!
!  fill-in the index array, creating ordered couples
!
      do iind = 2, nval, 2
        if (xdont(iind-1) <= xdont(iind)) then
            irngt (iind-1) = iind - 1
            irngt (iind) = iind
        else
            irngt (iind-1) = iind
            irngt (iind) = iind - 1
        end if
      end do
      if (modulo(nval, 2) /= 0) then
        irngt (nval) = nval
      end if
!
!  we will now have ordered subsets a - b - a - b - ...
!  and merge a and b couples into     c   -   c   - ...
!
      lmtna = 2
      lmtnc = 4
!
!  first iteration. the length of the ordered subsets goes from 2 to 4
!
      do
        if (nval <= 2) exit
!
!   loop on merges of a and b into c
!
        do iwrkd = 0, nval - 1, 4
            if ((iwrkd+4) > nval) then
              if ((iwrkd+2) >= nval) exit
!
!   1 2 3
!
              if (xdont(irngt(iwrkd+2)) <= xdont(irngt(iwrkd+3))) exit
!
!   1 3 2
!
              if (xdont(irngt(iwrkd+1)) <= xdont(irngt(iwrkd+3))) then
                  irng2 = irngt (iwrkd+2)
                  irngt (iwrkd+2) = irngt (iwrkd+3)
                  irngt (iwrkd+3) = irng2
!
!   3 1 2
!
              else
                  irng1 = irngt (iwrkd+1)
                  irngt (iwrkd+1) = irngt (iwrkd+3)
                  irngt (iwrkd+3) = irngt (iwrkd+2)
                  irngt (iwrkd+2) = irng1
              end if
              exit
            end if
!
!   1 2 3 4
!
            if (xdont(irngt(iwrkd+2)) <= xdont(irngt(iwrkd+3))) cycle
!
!   1 3 x x
!
            if (xdont(irngt(iwrkd+1)) <= xdont(irngt(iwrkd+3))) then
              irng2 = irngt (iwrkd+2)
              irngt (iwrkd+2) = irngt (iwrkd+3)
              if (xdont(irng2) <= xdont(irngt(iwrkd+4))) then
!   1 3 2 4
                  irngt (iwrkd+3) = irng2
              else
!   1 3 4 2
                  irngt (iwrkd+3) = irngt (iwrkd+4)
                  irngt (iwrkd+4) = irng2
              end if
!
!   3 x x x
!
            else
              irng1 = irngt (iwrkd+1)
              irng2 = irngt (iwrkd+2)
              irngt (iwrkd+1) = irngt (iwrkd+3)
              if (xdont(irng1) <= xdont(irngt(iwrkd+4))) then
                  irngt (iwrkd+2) = irng1
                  if (xdont(irng2) <= xdont(irngt(iwrkd+4))) then
!   3 1 2 4
                    irngt (iwrkd+3) = irng2
                  else
!   3 1 4 2
                    irngt (iwrkd+3) = irngt (iwrkd+4)
                    irngt (iwrkd+4) = irng2
                  end if
              else
!   3 4 1 2
                  irngt (iwrkd+2) = irngt (iwrkd+4)
                  irngt (iwrkd+3) = irng1
                  irngt (iwrkd+4) = irng2
              end if
            end if
        end do
!
!  the cs become as and bs
!
        lmtna = 4
        exit
      end do
!
!  iteration loop. each time, the length of the ordered subsets
!  is doubled.
!
      do
        if (lmtna >= nval) exit
        iwrkf = 0
        lmtnc = 2 * lmtnc
!
!   loop on merges of a and b into c
!
        do
            iwrk = iwrkf
            iwrkd = iwrkf + 1
            jinda = iwrkf + lmtna
            iwrkf = iwrkf + lmtnc
            if (iwrkf >= nval) then
              if (jinda >= nval) exit
              iwrkf = nval
            end if
            iinda = 1
            iindb = jinda + 1
!
!   shortcut for the case when the max of a is smaller
!   than the min of b. this line may be activated when the
!   initial set is already close to sorted.
!
!          if (xdont(irngt(jinda)) <= xdont(irngt(iindb))) cycle
!
!  one steps in the c subset, that we build in the final rank array
!
!  make a copy of the rank array for the merge iteration
!
            jwrkt (1:lmtna) = irngt (iwrkd:jinda)
!
            xvala = xdont (jwrkt(iinda))
            xvalb = xdont (irngt(iindb))
!
            do
              iwrk = iwrk + 1
!
!  we still have unprocessed values in both a and b
!
              if (xvala > xvalb) then
                  irngt (iwrk) = irngt (iindb)
                  iindb = iindb + 1
                  if (iindb > iwrkf) then
!  only a still with unprocessed values
                    irngt (iwrk+1:iwrkf) = jwrkt (iinda:lmtna)
                    exit
                  end if
                  xvalb = xdont (irngt(iindb))
              else
                  irngt (iwrk) = jwrkt (iinda)
                  iinda = iinda + 1
                  if (iinda > lmtna) exit! only b still with unprocessed values
                  xvala = xdont (jwrkt(iinda))
              end if
!
            end do
        end do
!
!  the cs become as and bs
!
        lmtna = 2 * lmtna
      end do
!
      return
!
end subroutine MRGRNK_R4

!-------------------------------------------------------------------------------

subroutine MRGRNK_R8 (xdont, irngt)
!*******************************************************************************
! MRGRNK_R8
! PURPOSE: Unconditionally rank real vector placing ranks into an integer
!          vector. This is a real kind 8 (double precision) version.
! CALL PARAMETERS: Real vector for ranking and integer vector containing
!          the ranks obtained.
! NOTES: From public domain code http://www.fortran-2000.com/rank/
!        Author: Michel Olagnon <Michel.Olagnon@ifremer.fr>.
!        IMPORTANT: This is a performance-optimised code. For performance
!        reasons, the first 2 passes are taken out of the standard loop, and
!        use dedicated coding.
!*******************************************************************************
! __________________________________________________________
!   mrgrnk = merge-sort ranking of an array
!   for performance reasons, the first 2 passes are taken
!   out of the standard loop, and use dedicated coding.
! __________________________________________________________
! __________________________________________________________
      real (kind=8), dimension (:), intent (in) :: xdont
      integer, dimension (:), intent (out) :: irngt
! __________________________________________________________
      real (kind=8) :: xvala, xvalb
!
      integer, dimension (size(irngt)) :: jwrkt
      integer :: lmtna, lmtnc, irng1, irng2
      integer :: nval, iind, iwrkd, iwrk, iwrkf, jinda, iinda, iindb
!
      nval = min (size(xdont), size(irngt))
      select case (nval)
      case (:0)
         return
      case (1)
         irngt (1) = 1
         return
      case default
         continue
      end select
!
!  fill-in the index array, creating ordered couples
!
      do iind = 2, nval, 2
         if (xdont(iind-1) <= xdont(iind)) then
            irngt (iind-1) = iind - 1
            irngt (iind) = iind
         else
            irngt (iind-1) = iind
            irngt (iind) = iind - 1
         end if
      end do
      if (modulo(nval, 2) /= 0) then
         irngt (nval) = nval
      end if
!
!  we will now have ordered subsets a - b - a - b - ...
!  and merge a and b couples into     c   -   c   - ...
!
      lmtna = 2
      lmtnc = 4
!
!  first iteration. the length of the ordered subsets goes from 2 to 4
!
      do
         if (nval <= 2) exit
!
!   loop on merges of a and b into c
!
         do iwrkd = 0, nval - 1, 4
            if ((iwrkd+4) > nval) then
               if ((iwrkd+2) >= nval) exit
!
!   1 2 3
!
               if (xdont(irngt(iwrkd+2)) <= xdont(irngt(iwrkd+3))) exit
!
!   1 3 2
!
               if (xdont(irngt(iwrkd+1)) <= xdont(irngt(iwrkd+3))) then
                  irng2 = irngt (iwrkd+2)
                  irngt (iwrkd+2) = irngt (iwrkd+3)
                  irngt (iwrkd+3) = irng2
!
!   3 1 2
!
               else
                  irng1 = irngt (iwrkd+1)
                  irngt (iwrkd+1) = irngt (iwrkd+3)
                  irngt (iwrkd+3) = irngt (iwrkd+2)
                  irngt (iwrkd+2) = irng1
               end if
               exit
            end if
!
!   1 2 3 4
!
            if (xdont(irngt(iwrkd+2)) <= xdont(irngt(iwrkd+3))) cycle
!
!   1 3 x x
!
            if (xdont(irngt(iwrkd+1)) <= xdont(irngt(iwrkd+3))) then
               irng2 = irngt (iwrkd+2)
               irngt (iwrkd+2) = irngt (iwrkd+3)
               if (xdont(irng2) <= xdont(irngt(iwrkd+4))) then
!   1 3 2 4
                  irngt (iwrkd+3) = irng2
               else
!   1 3 4 2
                  irngt (iwrkd+3) = irngt (iwrkd+4)
                  irngt (iwrkd+4) = irng2
               end if
!
!   3 x x x
!
            else
               irng1 = irngt (iwrkd+1)
               irng2 = irngt (iwrkd+2)
               irngt (iwrkd+1) = irngt (iwrkd+3)
               if (xdont(irng1) <= xdont(irngt(iwrkd+4))) then
                  irngt (iwrkd+2) = irng1
                  if (xdont(irng2) <= xdont(irngt(iwrkd+4))) then
!   3 1 2 4
                     irngt (iwrkd+3) = irng2
                  else
!   3 1 4 2
                     irngt (iwrkd+3) = irngt (iwrkd+4)
                     irngt (iwrkd+4) = irng2
                  end if
               else
!   3 4 1 2
                  irngt (iwrkd+2) = irngt (iwrkd+4)
                  irngt (iwrkd+3) = irng1
                  irngt (iwrkd+4) = irng2
               end if
            end if
         end do
!
!  the cs become as and bs
!
         lmtna = 4
         exit
      end do
!
!  iteration loop. each time, the length of the ordered subsets
!  is doubled.
!
      do
         if (lmtna >= nval) exit
         iwrkf = 0
         lmtnc = 2 * lmtnc
!
!   loop on merges of a and b into c
!
         do
            iwrk = iwrkf
            iwrkd = iwrkf + 1
            jinda = iwrkf + lmtna
            iwrkf = iwrkf + lmtnc
            if (iwrkf >= nval) then
               if (jinda >= nval) exit
               iwrkf = nval
            end if
            iinda = 1
            iindb = jinda + 1
!
!   shortcut for the case when the max of a is smaller
!   than the min of b. this line may be activated when the
!   initial set is already close to sorted.
!
!          if (xdont(irngt(jinda)) <= xdont(irngt(iindb))) cycle
!
!  one steps in the c subset, that we build in the final rank array
!
!  make a copy of the rank array for the merge iteration
!
            jwrkt (1:lmtna) = irngt (iwrkd:jinda)
!
            xvala = xdont (jwrkt(iinda))
            xvalb = xdont (irngt(iindb))
!
            do
               iwrk = iwrk + 1
!
!  we still have unprocessed values in both a and b
!
               if (xvala > xvalb) then
                  irngt (iwrk) = irngt (iindb)
                  iindb = iindb + 1
                  if (iindb > iwrkf) then
!  only a still with unprocessed values
                     irngt (iwrk+1:iwrkf) = jwrkt (iinda:lmtna)
                     exit
                  end if
                  xvalb = xdont (irngt(iindb))
               else
                  irngt (iwrk) = jwrkt (iinda)
                  iinda = iinda + 1
                  if (iinda > lmtna) exit! only b still with unprocessed values
                  xvala = xdont (jwrkt(iinda))
               end if
!
            end do
         end do
!
!  the cs become as and bs
!
         lmtna = 2 * lmtna
      end do
!
      return
!
end subroutine MRGRNK_R8

!-------------------------------------------------------------------------------

subroutine MRGRNK_I (xdont, irngt)
!*******************************************************************************
! MRGRNK_I
! PURPOSE: Unconditionally rank real vector placing ranks into an integer
!          vector. This is an integer version.
! CALL PARAMETERS: Real vector for ranking and integer vector containing
!          the ranks obtained.
! NOTES: From public domain code http://www.fortran-2000.com/rank/
!        Author: Michel Olagnon <Michel.Olagnon@ifremer.fr>.
!        IMPORTANT: This is a performance-optimised code. For performance
!        reasons, the first 2 passes are taken out of the standard loop, and
!        use dedicated coding.
!*******************************************************************************
! __________________________________________________________
!   mrgrnk = merge-sort ranking of an array
!   for performance reasons, the first 2 passes are taken
!   out of the standard loop, and use dedicated coding.
! __________________________________________________________
! __________________________________________________________
      integer, dimension (:), intent (in)  :: xdont
      integer, dimension (:), intent (out) :: irngt
! __________________________________________________________
      integer :: xvala, xvalb
!
      integer, dimension (size(irngt)) :: jwrkt
      integer :: lmtna, lmtnc, irng1, irng2
      integer :: nval, iind, iwrkd, iwrk, iwrkf, jinda, iinda, iindb
!
      nval = min (size(xdont), size(irngt))
      select case (nval)
      case (:0)
         return
      case (1)
         irngt (1) = 1
         return
      case default
         continue
      end select
!
!  fill-in the index array, creating ordered couples
!
      do iind = 2, nval, 2
         if (xdont(iind-1) <= xdont(iind)) then
            irngt (iind-1) = iind - 1
            irngt (iind) = iind
         else
            irngt (iind-1) = iind
            irngt (iind) = iind - 1
         end if
      end do
      if (modulo(nval, 2) /= 0) then
         irngt (nval) = nval
      end if
!
!  we will now have ordered subsets a - b - a - b - ...
!  and merge a and b couples into     c   -   c   - ...
!
      lmtna = 2
      lmtnc = 4
!
!  first iteration. the length of the ordered subsets goes from 2 to 4
!
      do
         if (nval <= 2) exit
!
!   loop on merges of a and b into c
!
         do iwrkd = 0, nval - 1, 4
            if ((iwrkd+4) > nval) then
               if ((iwrkd+2) >= nval) exit
!
!   1 2 3
!
               if (xdont(irngt(iwrkd+2)) <= xdont(irngt(iwrkd+3))) exit
!
!   1 3 2
!
               if (xdont(irngt(iwrkd+1)) <= xdont(irngt(iwrkd+3))) then
                  irng2 = irngt (iwrkd+2)
                  irngt (iwrkd+2) = irngt (iwrkd+3)
                  irngt (iwrkd+3) = irng2
!
!   3 1 2
!
               else
                  irng1 = irngt (iwrkd+1)
                  irngt (iwrkd+1) = irngt (iwrkd+3)
                  irngt (iwrkd+3) = irngt (iwrkd+2)
                  irngt (iwrkd+2) = irng1
               end if
               exit
            end if
!
!   1 2 3 4
!
            if (xdont(irngt(iwrkd+2)) <= xdont(irngt(iwrkd+3))) cycle
!
!   1 3 x x
!
            if (xdont(irngt(iwrkd+1)) <= xdont(irngt(iwrkd+3))) then
               irng2 = irngt (iwrkd+2)
               irngt (iwrkd+2) = irngt (iwrkd+3)
               if (xdont(irng2) <= xdont(irngt(iwrkd+4))) then
!   1 3 2 4
                  irngt (iwrkd+3) = irng2
               else
!   1 3 4 2
                  irngt (iwrkd+3) = irngt (iwrkd+4)
                  irngt (iwrkd+4) = irng2
               end if
!
!   3 x x x
!
            else
               irng1 = irngt (iwrkd+1)
               irng2 = irngt (iwrkd+2)
               irngt (iwrkd+1) = irngt (iwrkd+3)
               if (xdont(irng1) <= xdont(irngt(iwrkd+4))) then
                  irngt (iwrkd+2) = irng1
                  if (xdont(irng2) <= xdont(irngt(iwrkd+4))) then
!   3 1 2 4
                     irngt (iwrkd+3) = irng2
                  else
!   3 1 4 2
                     irngt (iwrkd+3) = irngt (iwrkd+4)
                     irngt (iwrkd+4) = irng2
                  end if
               else
!   3 4 1 2
                  irngt (iwrkd+2) = irngt (iwrkd+4)
                  irngt (iwrkd+3) = irng1
                  irngt (iwrkd+4) = irng2
               end if
            end if
         end do
!
!  the cs become as and bs
!
         lmtna = 4
         exit
      end do
!
!  iteration loop. each time, the length of the ordered subsets
!  is doubled.
!
      do
         if (lmtna >= nval) exit
         iwrkf = 0
         lmtnc = 2 * lmtnc
!
!   loop on merges of a and b into c
!
         do
            iwrk = iwrkf
            iwrkd = iwrkf + 1
            jinda = iwrkf + lmtna
            iwrkf = iwrkf + lmtnc
            if (iwrkf >= nval) then
               if (jinda >= nval) exit
               iwrkf = nval
            end if
            iinda = 1
            iindb = jinda + 1
!
!   shortcut for the case when the max of a is smaller
!   than the min of b. this line may be activated when the
!   initial set is already close to sorted.
!
!          if (xdont(irngt(jinda)) <= xdont(irngt(iindb))) cycle
!
!  one steps in the c subset, that we build in the final rank array
!
!  make a copy of the rank array for the merge iteration
!
            jwrkt (1:lmtna) = irngt (iwrkd:jinda)
!
            xvala = xdont (jwrkt(iinda))
            xvalb = xdont (irngt(iindb))
!
            do
               iwrk = iwrk + 1
!
!  we still have unprocessed values in both a and b
!
               if (xvala > xvalb) then
                  irngt (iwrk) = irngt (iindb)
                  iindb = iindb + 1
                  if (iindb > iwrkf) then
!  only a still with unprocessed values
                     irngt (iwrk+1:iwrkf) = jwrkt (iinda:lmtna)
                     exit
                  end if
                  xvalb = xdont (irngt(iindb))
               else
                  irngt (iwrk) = jwrkt (iinda)
                  iinda = iinda + 1
                  if (iinda > lmtna) exit! only b still with unprocessed values
                  xvala = xdont (jwrkt(iinda))
               end if
!
            end do
         end do
!
!  the cs become as and bs
!
         lmtna = 2 * lmtna
      end do
!
      return
!
end subroutine MRGRNK_I

!-------------------------------------------------------------------------------

subroutine ARRAY_RANK(Indx, Ranks)
!*******************************************************************************
! ARRAY_RANK
! PURPOSE: Calculate the rank order of an array elements from the rank indices
!          computed by ARRAY_INDEX.
! CALL PARAMETERS: integer input index vector and an output vector of ranks.
!*******************************************************************************
integer, dimension(:), intent(in)  :: Indx
integer, dimension(:), intent(out) :: Ranks
integer :: i

  do i=1, min(size(Indx), size(Ranks))
      Ranks(indx(i)) = i
  end do

end subroutine ARRAY_RANK

!-------------------------------------------------------------------------------

subroutine RNKPAR_R4 (xdont, irngt, nord)
!*******************************************************************************
! RNKPAR_R4
! PURPOSE: Partial ranking of a vector up to a specific order.
!
! CALL PARAMETERS: Real vector for ranking, integer vector containing
!          the ranks obtained, and an integer scalar value setting the
!          ranking limit.
! NOTES: From public domain code http://www.fortran-2000.com/rank/
!        Author: Michel Olagnon <Michel.Olagnon@ifremer.fr>.
!        IMPORTANT: This is a performance-optimised code.
!*******************************************************************************
!
!  Ranks partially XDONT by IRNGT, up to order NORD
! __________________________________________________________
!  This routine uses a pivoting strategy such as the one of
!  finding the median based on the quicksort algorithm, but
!  we skew the pivot choice to try to bring it to NORD as
!  fast as possible. It uses 2 temporary arrays, where it
!  stores the indices of the values smaller than the pivot
!  (ILOWT), and the indices of values larger than the pivot
!  that we might still need later on (IHIGT). It iterates
!  until it can bring the number of values in ILOWT to
!  exactly NORD, and then uses an insertion sort to rank
!  this set, since it is supposedly small.
!  Michel Olagnon - Feb. 2000
! __________________________________________________________
! _________________________________________________________
      real, dimension (:), intent (in) :: xdont
      integer, dimension (:), intent (out) :: irngt
      integer, intent (in) :: nord
! __________________________________________________________
      real    :: xpiv, xpiv0, xwrk, xwrk1, xmin, xmax
!
      integer, dimension (size(xdont)) :: ilowt, ihigt
      integer :: ndon, jhig, jlow, ihig, iwrk, iwrk1, iwrk2, iwrk3
      integer :: ideb, jdeb, imil, ifin, nwrk, icrs, idcr, ilow
      integer :: jlm2, jlm1, jhm2, jhm1
!
      ndon = size (xdont)
!
!    first loop is used to fill-in ilowt, ihigt at the same time
!
      if (ndon < 2) then
         if (nord >= 1) irngt (1) = 1
         return
      end if
!
!  one chooses a pivot, best estimate possible to put fractile near
!  mid-point of the set of low values.
!
      if (xdont(2) < xdont(1)) then
         ilowt (1) = 2
         ihigt (1) = 1
      else
         ilowt (1) = 1
         ihigt (1) = 2
      end if
!
      if (ndon < 3) then
         if (nord >= 1) irngt (1) = ilowt (1)
         if (nord >= 2) irngt (2) = ihigt (1)
         return
      end if
!
      if (xdont(3) <= xdont(ihigt(1))) then
         ihigt (2) = ihigt (1)
         if (xdont(3) < xdont(ilowt(1))) then
            ihigt (1) = ilowt (1)
            ilowt (1) = 3
         else
            ihigt (1) = 3
         end if
      else
         ihigt (2) = 3
      end if
!
      if (ndon < 4) then
         if (nord >= 1) irngt (1) = ilowt (1)
         if (nord >= 2) irngt (2) = ihigt (1)
         if (nord >= 3) irngt (3) = ihigt (2)
         return
      end if
!
      if (xdont(ndon) <= xdont(ihigt(1))) then
         ihigt (3) = ihigt (2)
         ihigt (2) = ihigt (1)
         if (xdont(ndon) < xdont(ilowt(1))) then
            ihigt (1) = ilowt (1)
            ilowt (1) = ndon
         else
            ihigt (1) = ndon
         end if
      else
         if (xdont (ndon) < xdont (ihigt(2))) then
            ihigt (3) = ihigt (2)
            ihigt (2) = ndon
         else
            ihigt (3) = ndon
         endif
      end if
!
      if (ndon < 5) then
         if (nord >= 1) irngt (1) = ilowt (1)
         if (nord >= 2) irngt (2) = ihigt (1)
         if (nord >= 3) irngt (3) = ihigt (2)
         if (nord >= 4) irngt (4) = ihigt (3)
         return
      end if
!
      jdeb = 0
      ideb = jdeb + 1
      jlow = ideb
      jhig = 3
      xpiv = xdont (ilowt(ideb)) + real(2*nord)/real(ndon+nord) * &
                                   (xdont(ihigt(3))-xdont(ilowt(ideb)))
      if (xpiv >= xdont(ihigt(1))) then
         xpiv = xdont (ilowt(ideb)) + real(2*nord)/real(ndon+nord) * &
                                      (xdont(ihigt(2))-xdont(ilowt(ideb)))
         if (xpiv >= xdont(ihigt(1))) &
             xpiv = xdont (ilowt(ideb)) + real (2*nord) / real (ndon+nord) * &
                                          (xdont(ihigt(1))-xdont(ilowt(ideb)))
      end if
      xpiv0 = xpiv
!
!  one puts values > pivot in the end and those <= pivot
!  at the beginning. this is split in 2 cases, so that
!  we can skip the loop test a number of times.
!  as we are also filling in the work arrays at the same time
!  we stop filling in the ihigt array as soon as we have more
!  than enough values in ilowt.
!
!
      if (xdont(ndon) > xpiv) then
         icrs = 3
         do
            icrs = icrs + 1
            if (xdont(icrs) > xpiv) then
               if (icrs >= ndon) exit
               jhig = jhig + 1
               ihigt (jhig) = icrs
            else
               jlow = jlow + 1
               ilowt (jlow) = icrs
               if (jlow >= nord) exit
            end if
         end do
!
!  one restricts further processing because it is no use
!  to store more high values
!
         if (icrs < ndon-1) then
            do
               icrs = icrs + 1
               if (xdont(icrs) <= xpiv) then
                  jlow = jlow + 1
                  ilowt (jlow) = icrs
               else if (icrs >= ndon) then
                  exit
               end if
            end do
         end if
!
!
      else
!
!  same as above, but this is not as easy to optimize, so the
!  do-loop is kept
!
         do icrs = 4, ndon - 1
            if (xdont(icrs) > xpiv) then
               jhig = jhig + 1
               ihigt (jhig) = icrs
            else
               jlow = jlow + 1
               ilowt (jlow) = icrs
               if (jlow >= nord) exit
            end if
         end do
!
         if (icrs < ndon-1) then
            do
               icrs = icrs + 1
               if (xdont(icrs) <= xpiv) then
                  if (icrs >= ndon) exit
                  jlow = jlow + 1
                  ilowt (jlow) = icrs
               end if
            end do
         end if
      end if
!
      jlm2 = 0
      jlm1 = 0
      jhm2 = 0
      jhm1 = 0
      do
         if (jlow == nord) exit
         if (jlm2 == jlow .and. jhm2 == jhig) then
!
!   we are oscillating. perturbate by bringing jlow closer by one
!   to nord
!
           if (nord > jlow) then
                xmin = xdont (ihigt(1))
                ihig = 1
                do icrs = 2, jhig
                   if (xdont(ihigt(icrs)) < xmin) then
                      xmin = xdont (ihigt(icrs))
                      ihig = icrs
                   end if
                end do
!
                jlow = jlow + 1
                ilowt (jlow) = ihigt (ihig)
                ihigt (ihig) = ihigt (jhig)
                jhig = jhig - 1
             else
                ilow = ilowt (jlow)
                xmax = xdont (ilow)
                do icrs = 1, jlow
                   if (xdont(ilowt(icrs)) > xmax) then
                      iwrk = ilowt (icrs)
                      xmax = xdont (iwrk)
                      ilowt (icrs) = ilow
                      ilow = iwrk
                   end if
                end do
                jlow = jlow - 1
             end if
         end if
         jlm2 = jlm1
         jlm1 = jlow
         jhm2 = jhm1
         jhm1 = jhig
!
!   we try to bring the number of values in the low values set
!   closer to nord.
!
        select case (nord-jlow)
         case (2:)
!
!   not enough values in low part, at least 2 are missing
!
            select case (jhig)
!!!!!           case default
!!!!!              write (*,*) "assertion failed"
!!!!!              stop
!
!   we make a special case when we have so few values in
!   the high values set that it is bad performance to choose a pivot
!   and apply the general algorithm.
!
            case (2)
               if (xdont(ihigt(1)) <= xdont(ihigt(2))) then
                  jlow = jlow + 1
                  ilowt (jlow) = ihigt (1)
                  jlow = jlow + 1
                  ilowt (jlow) = ihigt (2)
               else
                  jlow = jlow + 1
                  ilowt (jlow) = ihigt (2)
                  jlow = jlow + 1
                  ilowt (jlow) = ihigt (1)
               end if
               exit
!
            case (3)
!
!
               iwrk1 = ihigt (1)
               iwrk2 = ihigt (2)
               iwrk3 = ihigt (3)
               if (xdont(iwrk2) < xdont(iwrk1)) then
                  ihigt (1) = iwrk2
                  ihigt (2) = iwrk1
                  iwrk2 = iwrk1
               end if
               if (xdont(iwrk2) > xdont(iwrk3)) then
                  ihigt (3) = iwrk2
                  ihigt (2) = iwrk3
                  iwrk2 = iwrk3
                  if (xdont(iwrk2) < xdont(ihigt(1))) then
                     ihigt (2) = ihigt (1)
                     ihigt (1) = iwrk2
                  end if
               end if
               jhig = 0
               do icrs = jlow + 1, nord
                  jhig = jhig + 1
                  ilowt (icrs) = ihigt (jhig)
               end do
               jlow = nord
               exit
!
            case (4:)
!
!
               xpiv0 = xpiv
               ifin = jhig
!
!  one chooses a pivot from the 2 first values and the last one.
!  this should ensure sufficient renewal between iterations to
!  avoid worst case behavior effects.
!
               iwrk1 = ihigt (1)
               iwrk2 = ihigt (2)
               iwrk3 = ihigt (ifin)
               if (xdont(iwrk2) < xdont(iwrk1)) then
                  ihigt (1) = iwrk2
                  ihigt (2) = iwrk1
                  iwrk2 = iwrk1
               end if
               if (xdont(iwrk2) > xdont(iwrk3)) then
                  ihigt (ifin) = iwrk2
                  ihigt (2) = iwrk3
                  iwrk2 = iwrk3
                  if (xdont(iwrk2) < xdont(ihigt(1))) then
                     ihigt (2) = ihigt (1)
                     ihigt (1) = iwrk2
                  end if
               end if
!
               jdeb = jlow
               nwrk = nord - jlow
               iwrk1 = ihigt (1)
               jlow = jlow + 1
               ilowt (jlow) = iwrk1
               xpiv = xdont (iwrk1) + real (nwrk) / real (nord+nwrk) * &
                                      (xdont(ihigt(ifin))-xdont(iwrk1))
!
!  one takes values <= pivot to ilowt
!  again, 2 parts, one where we take care of the remaining
!  high values because we might still need them, and the
!  other when we know that we will have more than enough
!  low values in the end.
!
               jhig = 0
               do icrs = 2, ifin
                  if (xdont(ihigt(icrs)) <= xpiv) then
                     jlow = jlow + 1
                     ilowt (jlow) = ihigt (icrs)
                     if (jlow >= nord) exit
                  else
                     jhig = jhig + 1
                     ihigt (jhig) = ihigt (icrs)
                  end if
               end do
!
               do icrs = icrs + 1, ifin
                  if (xdont(ihigt(icrs)) <= xpiv) then
                     jlow = jlow + 1
                     ilowt (jlow) = ihigt (icrs)
                  end if
               end do
           end select
!
!
         case (1)
!
!  only 1 value is missing in low part
!
            xmin = xdont (ihigt(1))
            ihig = 1
            do icrs = 2, jhig
               if (xdont(ihigt(icrs)) < xmin) then
                  xmin = xdont (ihigt(icrs))
                  ihig = icrs
               end if
            end do
!
            jlow = jlow + 1
            ilowt (jlow) = ihigt (ihig)
            exit
!
!
         case (0)
!
!  low part is exactly what we want
!
            exit
!
!
         case (-5:-1)
!
!  only few values too many in low part
!
            irngt (1) = ilowt (1)
            do icrs = 2, nord
               iwrk = ilowt (icrs)
               xwrk = xdont (iwrk)
               do idcr = icrs - 1, 1, - 1
                  if (xwrk < xdont(irngt(idcr))) then
                     irngt (idcr+1) = irngt (idcr)
                  else
                     exit
                  end if
               end do
               irngt (idcr+1) = iwrk
            end do
!
            xwrk1 = xdont (irngt(nord))
            do icrs = nord + 1, jlow
               if (xdont(ilowt (icrs)) < xwrk1) then
                  xwrk = xdont (ilowt (icrs))
                  do idcr = nord - 1, 1, - 1
                     if (xwrk >= xdont(irngt(idcr))) exit
                     irngt (idcr+1) = irngt (idcr)
                  end do
                  irngt (idcr+1) = ilowt (icrs)
                  xwrk1 = xdont (irngt(nord))
               end if
            end do
!
            return
!
!
         case (:-6)
!
! last case: too many values in low part
!
            ideb = jdeb + 1
            imil = (jlow+ideb) / 2
            ifin = jlow
!
!  one chooses a pivot from 1st, last, and middle values
!
            if (xdont(ilowt(imil)) < xdont(ilowt(ideb))) then
               iwrk = ilowt (ideb)
               ilowt (ideb) = ilowt (imil)
               ilowt (imil) = iwrk
            end if
            if (xdont(ilowt(imil)) > xdont(ilowt(ifin))) then
               iwrk = ilowt (ifin)
               ilowt (ifin) = ilowt (imil)
               ilowt (imil) = iwrk
               if (xdont(ilowt(imil)) < xdont(ilowt(ideb))) then
                  iwrk = ilowt (ideb)
                  ilowt (ideb) = ilowt (imil)
                  ilowt (imil) = iwrk
               end if
            end if
            if (ifin <= 3) exit
!
            xpiv = xdont (ilowt(1)) + real(nord)/real(jlow+nord) * &
                                      (xdont(ilowt(ifin))-xdont(ilowt(1)))
            if (jdeb > 0) then
               if (xpiv <= xpiv0) &
                   xpiv = xpiv0 + real(2*nord-jdeb)/real (jlow+nord) * &
                                  (xdont(ilowt(ifin))-xpiv0)
            else
               ideb = 1
            end if
!
!  one takes values > xpiv to ihigt
!  however, we do not process the first values if we have been
!  through the case when we did not have enough low values
!
            jhig = 0
            jlow = jdeb
!
            if (xdont(ilowt(ifin)) > xpiv) then
               icrs = jdeb
               do
                 icrs = icrs + 1
                  if (xdont(ilowt(icrs)) > xpiv) then
                     jhig = jhig + 1
                     ihigt (jhig) = ilowt (icrs)
                     if (icrs >= ifin) exit
                  else
                     jlow = jlow + 1
                     ilowt (jlow) = ilowt (icrs)
                     if (jlow >= nord) exit
                  end if
               end do
!
               if (icrs < ifin) then
                  do
                     icrs = icrs + 1
                     if (xdont(ilowt(icrs)) <= xpiv) then
                        jlow = jlow + 1
                        ilowt (jlow) = ilowt (icrs)
                     else
                        if (icrs >= ifin) exit
                     end if
                  end do
               end if
           else
               do icrs = ideb, ifin
                  if (xdont(ilowt(icrs)) > xpiv) then
                     jhig = jhig + 1
                     ihigt (jhig) = ilowt (icrs)
                  else
                     jlow = jlow + 1
                     ilowt (jlow) = ilowt (icrs)
                     if (jlow >= nord) exit
                  end if
               end do
!
               do icrs = icrs + 1, ifin
                  if (xdont(ilowt(icrs)) <= xpiv) then
                     jlow = jlow + 1
                     ilowt (jlow) = ilowt (icrs)
                  end if
               end do
            end if
!
         end select
!
      end do
!
!  now, we only need to complete ranking of the 1:nord set
!  assuming nord is small, we use a simple insertion sort
!
      irngt (1) = ilowt (1)
      do icrs = 2, nord
         iwrk = ilowt (icrs)
         xwrk = xdont (iwrk)
         do idcr = icrs - 1, 1, - 1
            if (xwrk < xdont(irngt(idcr))) then
               irngt (idcr+1) = irngt (idcr)
            else
               exit
            end if
         end do
         irngt (idcr+1) = iwrk
      end do
     return
!
!
end subroutine RNKPAR_R4

!-------------------------------------------------------------------------------

subroutine RNKPAR_R8 (xdont, irngt, nord)
!*******************************************************************************
! RNKPAR_R8
! PURPOSE: Partial ranking of a vector up to a specific order.
!
! CALL PARAMETERS: Real (kind 8) vector for ranking, integer vector containing
!          the ranks obtained, and an integer scalar value setting the
!          ranking limit.
! NOTES: From public domain code http://www.fortran-2000.com/rank/
!        Author: Michel Olagnon <Michel.Olagnon@ifremer.fr>.
!        IMPORTANT: This is a performance-optimised code.
!*******************************************************************************
!  ranks partially xdont by irngt, up to order nord
! __________________________________________________________
!  this routine uses a pivoting strategy such as the one of
!  finding the median based on the quicksort algorithm, but
!  we skew the pivot choice to try to bring it to nord as
!  fast as possible. it uses 2 temporary arrays, where it
!  stores the indices of the values smaller than the pivot
!  (ilowt), and the indices of values larger than the pivot
!  that we might still need later on (ihigt). it iterates
!  until it can bring the number of values in ilowt to
!  exactly nord, and then uses an insertion sort to rank
!  this set, since it is supposedly small.
!  michel olagnon - feb. 2000
! __________________________________________________________
! __________________________________________________________
      real (kind=8), dimension (:), intent (in) :: xdont
      integer, dimension (:), intent (out) :: irngt
      integer, intent (in) :: nord
! __________________________________________________________
      real (kind=8) :: xpiv, xpiv0, xwrk, xwrk1, xmin, xmax
!
      integer, dimension (size(xdont)) :: ilowt, ihigt
      integer :: ndon, jhig, jlow, ihig, iwrk, iwrk1, iwrk2, iwrk3
      integer :: ideb, jdeb, imil, ifin, nwrk, icrs, idcr, ilow
      integer :: jlm2, jlm1, jhm2, jhm1
!
      ndon = size (xdont)
!
!    first loop is used to fill-in ilowt, ihigt at the same time
!
      if (ndon < 2) then
         if (nord >= 1) irngt (1) = 1
         return
      end if
!
!  one chooses a pivot, best estimate possible to put fractile near
!  mid-point of the set of low values.
!
      if (xdont(2) < xdont(1)) then
         ilowt (1) = 2
         ihigt (1) = 1
      else
         ilowt (1) = 1
         ihigt (1) = 2
      end if
!
      if (ndon < 3) then
         if (nord >= 1) irngt (1) = ilowt (1)
         if (nord >= 2) irngt (2) = ihigt (1)
         return
      end if
!
      if (xdont(3) <= xdont(ihigt(1))) then
         ihigt (2) = ihigt (1)
         if (xdont(3) < xdont(ilowt(1))) then
            ihigt (1) = ilowt (1)
            ilowt (1) = 3
         else
            ihigt (1) = 3
         end if
      else
         ihigt (2) = 3
      end if
!
      if (ndon < 4) then
         if (nord >= 1) irngt (1) = ilowt (1)
         if (nord >= 2) irngt (2) = ihigt (1)
         if (nord >= 3) irngt (3) = ihigt (2)
         return
      end if
!
      if (xdont(ndon) <= xdont(ihigt(1))) then
         ihigt (3) = ihigt (2)
         ihigt (2) = ihigt (1)
         if (xdont(ndon) < xdont(ilowt(1))) then
            ihigt (1) = ilowt (1)
            ilowt (1) = ndon
         else
            ihigt (1) = ndon
         end if
      else
         if (xdont (ndon) < xdont (ihigt(2))) then
            ihigt (3) = ihigt (2)
            ihigt (2) = ndon
         else
            ihigt (3) = ndon
         endif
      end if
!
      if (ndon < 5) then
         if (nord >= 1) irngt (1) = ilowt (1)
         if (nord >= 2) irngt (2) = ihigt (1)
         if (nord >= 3) irngt (3) = ihigt (2)
         if (nord >= 4) irngt (4) = ihigt (3)
         return
      end if
!
      jdeb = 0
      ideb = jdeb + 1
      jlow = ideb
      jhig = 3
      xpiv = xdont (ilowt(ideb)) + real(2*nord)/real(ndon+nord) * &
                                   (xdont(ihigt(3))-xdont(ilowt(ideb)))
      if (xpiv >= xdont(ihigt(1))) then
         xpiv = xdont (ilowt(ideb)) + real(2*nord)/real(ndon+nord) * &
                                      (xdont(ihigt(2))-xdont(ilowt(ideb)))
         if (xpiv >= xdont(ihigt(1))) &
             xpiv = xdont (ilowt(ideb)) + real (2*nord) / real (ndon+nord) * &
                                          (xdont(ihigt(1))-xdont(ilowt(ideb)))
      end if
      xpiv0 = xpiv
!
!  one puts values > pivot in the end and those <= pivot
!  at the beginning. this is split in 2 cases, so that
!  we can skip the loop test a number of times.
!  as we are also filling in the work arrays at the same time
!  we stop filling in the ihigt array as soon as we have more
!  than enough values in ilowt.
!
!
      if (xdont(ndon) > xpiv) then
         icrs = 3
         do
            icrs = icrs + 1
            if (xdont(icrs) > xpiv) then
               if (icrs >= ndon) exit
               jhig = jhig + 1
               ihigt (jhig) = icrs
            else
               jlow = jlow + 1
               ilowt (jlow) = icrs
               if (jlow >= nord) exit
            end if
         end do
!
!  one restricts further processing because it is no use
!  to store more high values
!
         if (icrs < ndon-1) then
            do
               icrs = icrs + 1
               if (xdont(icrs) <= xpiv) then
                  jlow = jlow + 1
                  ilowt (jlow) = icrs
               else if (icrs >= ndon) then
                  exit
               end if
            end do
         end if
!
!
      else
!
!  same as above, but this is not as easy to optimize, so the
!  do-loop is kept
!
         do icrs = 4, ndon - 1
            if (xdont(icrs) > xpiv) then
               jhig = jhig + 1
               ihigt (jhig) = icrs
            else
               jlow = jlow + 1
               ilowt (jlow) = icrs
               if (jlow >= nord) exit
            end if
         end do
!
         if (icrs < ndon-1) then
            do
               icrs = icrs + 1
               if (xdont(icrs) <= xpiv) then
                  if (icrs >= ndon) exit
                  jlow = jlow + 1
                  ilowt (jlow) = icrs
               end if
            end do
         end if
      end if
!
      jlm2 = 0
      jlm1 = 0
      jhm2 = 0
      jhm1 = 0
      do
         if (jlow == nord) exit
         if (jlm2 == jlow .and. jhm2 == jhig) then
!
!   we are oscillating. perturbate by bringing jlow closer by one
!   to nord
!
           if (nord > jlow) then
                xmin = xdont (ihigt(1))
                ihig = 1
                do icrs = 2, jhig
                   if (xdont(ihigt(icrs)) < xmin) then
                      xmin = xdont (ihigt(icrs))
                      ihig = icrs
                   end if
                end do
!
                jlow = jlow + 1
                ilowt (jlow) = ihigt (ihig)
                ihigt (ihig) = ihigt (jhig)
                jhig = jhig - 1
             else
                ilow = ilowt (jlow)
                xmax = xdont (ilow)
                do icrs = 1, jlow
                   if (xdont(ilowt(icrs)) > xmax) then
                      iwrk = ilowt (icrs)
                      xmax = xdont (iwrk)
                      ilowt (icrs) = ilow
                      ilow = iwrk
                   end if
                end do
                jlow = jlow - 1
             end if
         end if
         jlm2 = jlm1
         jlm1 = jlow
         jhm2 = jhm1
         jhm1 = jhig
!
!   we try to bring the number of values in the low values set
!   closer to nord.
!
        select case (nord-jlow)
         case (2:)
!
!   not enough values in low part, at least 2 are missing
!
            select case (jhig)
!!!!!           case default
!!!!!              write (*,*) "assertion failed"
!!!!!              stop
!
!   we make a special case when we have so few values in
!   the high values set that it is bad performance to choose a pivot
!   and apply the general algorithm.
!
            case (2)
               if (xdont(ihigt(1)) <= xdont(ihigt(2))) then
                  jlow = jlow + 1
                  ilowt (jlow) = ihigt (1)
                  jlow = jlow + 1
                  ilowt (jlow) = ihigt (2)
               else
                  jlow = jlow + 1
                  ilowt (jlow) = ihigt (2)
                  jlow = jlow + 1
                  ilowt (jlow) = ihigt (1)
               end if
               exit
!
            case (3)
!
!
               iwrk1 = ihigt (1)
               iwrk2 = ihigt (2)
               iwrk3 = ihigt (3)
               if (xdont(iwrk2) < xdont(iwrk1)) then
                  ihigt (1) = iwrk2
                  ihigt (2) = iwrk1
                  iwrk2 = iwrk1
               end if
               if (xdont(iwrk2) > xdont(iwrk3)) then
                  ihigt (3) = iwrk2
                  ihigt (2) = iwrk3
                  iwrk2 = iwrk3
                  if (xdont(iwrk2) < xdont(ihigt(1))) then
                     ihigt (2) = ihigt (1)
                     ihigt (1) = iwrk2
                  end if
               end if
               jhig = 0
               do icrs = jlow + 1, nord
                  jhig = jhig + 1
                  ilowt (icrs) = ihigt (jhig)
               end do
               jlow = nord
               exit
!
            case (4:)
!
!
               xpiv0 = xpiv
               ifin = jhig
!
!  one chooses a pivot from the 2 first values and the last one.
!  this should ensure sufficient renewal between iterations to
!  avoid worst case behavior effects.
!
               iwrk1 = ihigt (1)
               iwrk2 = ihigt (2)
               iwrk3 = ihigt (ifin)
               if (xdont(iwrk2) < xdont(iwrk1)) then
                  ihigt (1) = iwrk2
                  ihigt (2) = iwrk1
                  iwrk2 = iwrk1
               end if
               if (xdont(iwrk2) > xdont(iwrk3)) then
                  ihigt (ifin) = iwrk2
                  ihigt (2) = iwrk3
                  iwrk2 = iwrk3
                  if (xdont(iwrk2) < xdont(ihigt(1))) then
                     ihigt (2) = ihigt (1)
                     ihigt (1) = iwrk2
                  end if
               end if
!
               jdeb = jlow
               nwrk = nord - jlow
               iwrk1 = ihigt (1)
               jlow = jlow + 1
               ilowt (jlow) = iwrk1
               xpiv = xdont (iwrk1) + real (nwrk) / real (nord+nwrk) * &
                                      (xdont(ihigt(ifin))-xdont(iwrk1))
!
!  one takes values <= pivot to ilowt
!  again, 2 parts, one where we take care of the remaining
!  high values because we might still need them, and the
!  other when we know that we will have more than enough
!  low values in the end.
!
               jhig = 0
               do icrs = 2, ifin
                  if (xdont(ihigt(icrs)) <= xpiv) then
                     jlow = jlow + 1
                     ilowt (jlow) = ihigt (icrs)
                     if (jlow >= nord) exit
                  else
                     jhig = jhig + 1
                     ihigt (jhig) = ihigt (icrs)
                  end if
               end do
!
               do icrs = icrs + 1, ifin
                  if (xdont(ihigt(icrs)) <= xpiv) then
                     jlow = jlow + 1
                     ilowt (jlow) = ihigt (icrs)
                  end if
               end do
           end select
!
!
         case (1)
!
!  only 1 value is missing in low part
!
            xmin = xdont (ihigt(1))
            ihig = 1
            do icrs = 2, jhig
               if (xdont(ihigt(icrs)) < xmin) then
                  xmin = xdont (ihigt(icrs))
                  ihig = icrs
               end if
            end do
!
            jlow = jlow + 1
            ilowt (jlow) = ihigt (ihig)
            exit
!
!
         case (0)
!
!  low part is exactly what we want
!
            exit
!
!
         case (-5:-1)
!
!  only few values too many in low part
!
            irngt (1) = ilowt (1)
            do icrs = 2, nord
               iwrk = ilowt (icrs)
               xwrk = xdont (iwrk)
               do idcr = icrs - 1, 1, - 1
                  if (xwrk < xdont(irngt(idcr))) then
                     irngt (idcr+1) = irngt (idcr)
                  else
                     exit
                  end if
               end do
               irngt (idcr+1) = iwrk
            end do
!
            xwrk1 = xdont (irngt(nord))
            do icrs = nord + 1, jlow
               if (xdont(ilowt (icrs)) < xwrk1) then
                  xwrk = xdont (ilowt (icrs))
                  do idcr = nord - 1, 1, - 1
                     if (xwrk >= xdont(irngt(idcr))) exit
                     irngt (idcr+1) = irngt (idcr)
                  end do
                  irngt (idcr+1) = ilowt (icrs)
                  xwrk1 = xdont (irngt(nord))
               end if
            end do
!
            return
!
!
         case (:-6)
!
! last case: too many values in low part
!
            ideb = jdeb + 1
            imil = (jlow+ideb) / 2
            ifin = jlow
!
!  one chooses a pivot from 1st, last, and middle values
!
            if (xdont(ilowt(imil)) < xdont(ilowt(ideb))) then
               iwrk = ilowt (ideb)
               ilowt (ideb) = ilowt (imil)
               ilowt (imil) = iwrk
            end if
            if (xdont(ilowt(imil)) > xdont(ilowt(ifin))) then
               iwrk = ilowt (ifin)
               ilowt (ifin) = ilowt (imil)
               ilowt (imil) = iwrk
               if (xdont(ilowt(imil)) < xdont(ilowt(ideb))) then
                  iwrk = ilowt (ideb)
                  ilowt (ideb) = ilowt (imil)
                  ilowt (imil) = iwrk
               end if
            end if
            if (ifin <= 3) exit
!
            xpiv = xdont (ilowt(1)) + real(nord)/real(jlow+nord) * &
                                      (xdont(ilowt(ifin))-xdont(ilowt(1)))
            if (jdeb > 0) then
               if (xpiv <= xpiv0) &
                   xpiv = xpiv0 + real(2*nord-jdeb)/real (jlow+nord) * &
                                  (xdont(ilowt(ifin))-xpiv0)
            else
               ideb = 1
            end if
!
!  one takes values > xpiv to ihigt
!  however, we do not process the first values if we have been
!  through the case when we did not have enough low values
!
            jhig = 0
            jlow = jdeb
!
            if (xdont(ilowt(ifin)) > xpiv) then
               icrs = jdeb
               do
                 icrs = icrs + 1
                  if (xdont(ilowt(icrs)) > xpiv) then
                     jhig = jhig + 1
                     ihigt (jhig) = ilowt (icrs)
                     if (icrs >= ifin) exit
                  else
                     jlow = jlow + 1
                     ilowt (jlow) = ilowt (icrs)
                     if (jlow >= nord) exit
                  end if
               end do
!
               if (icrs < ifin) then
                  do
                     icrs = icrs + 1
                     if (xdont(ilowt(icrs)) <= xpiv) then
                        jlow = jlow + 1
                        ilowt (jlow) = ilowt (icrs)
                     else
                        if (icrs >= ifin) exit
                     end if
                  end do
               end if
           else
               do icrs = ideb, ifin
                  if (xdont(ilowt(icrs)) > xpiv) then
                     jhig = jhig + 1
                     ihigt (jhig) = ilowt (icrs)
                  else
                     jlow = jlow + 1
                     ilowt (jlow) = ilowt (icrs)
                     if (jlow >= nord) exit
                  end if
               end do
!
               do icrs = icrs + 1, ifin
                  if (xdont(ilowt(icrs)) <= xpiv) then
                     jlow = jlow + 1
                     ilowt (jlow) = ilowt (icrs)
                  end if
               end do
            end if
!
         end select
!
      end do
!
!  now, we only need to complete ranking of the 1:nord set
!  assuming nord is small, we use a simple insertion sort
!
      irngt (1) = ilowt (1)
      do icrs = 2, nord
         iwrk = ilowt (icrs)
         xwrk = xdont (iwrk)
         do idcr = icrs - 1, 1, - 1
            if (xwrk < xdont(irngt(idcr))) then
               irngt (idcr+1) = irngt (idcr)
            else
               exit
            end if
         end do
         irngt (idcr+1) = iwrk
      end do
     return
!
!
end subroutine RNKPAR_R8

!-------------------------------------------------------------------------------

subroutine RNKPAR_I (xdont, irngt, nord)
!*******************************************************************************
! RNKPAR_I
! PURPOSE: Partial ranking of a vector up to a specific order.
!
! CALL PARAMETERS: Integer vector for ranking, integer vector containing
!          the ranks obtained, and an integer scalar value setting the
!          ranking limit.
! NOTES: From public domain code http://www.fortran-2000.com/rank/
!        Author: Michel Olagnon <Michel.Olagnon@ifremer.fr>.
!        IMPORTANT: This is a performance-optimised code.
!*******************************************************************************
!  ranks partially xdont by irngt, up to order nord
! __________________________________________________________
!  this routine uses a pivoting strategy such as the one of
!  finding the median based on the quicksort algorithm, but
!  we skew the pivot choice to try to bring it to nord as
!  fast as possible. it uses 2 temporary arrays, where it
!  stores the indices of the values smaller than the pivot
!  (ilowt), and the indices of values larger than the pivot
!  that we might still need later on (ihigt). it iterates
!  until it can bring the number of values in ilowt to
!  exactly nord, and then uses an insertion sort to rank
!  this set, since it is supposedly small.
!  michel olagnon - feb. 2000
! __________________________________________________________
! __________________________________________________________
      integer, dimension (:), intent (in)  :: xdont
      integer, dimension (:), intent (out) :: irngt
      integer, intent (in) :: nord
! __________________________________________________________
      integer :: xpiv, xpiv0, xwrk, xwrk1, xmin, xmax
!
      integer, dimension (size(xdont)) :: ilowt, ihigt
      integer :: ndon, jhig, jlow, ihig, iwrk, iwrk1, iwrk2, iwrk3
      integer :: ideb, jdeb, imil, ifin, nwrk, icrs, idcr, ilow
      integer :: jlm2, jlm1, jhm2, jhm1
!
      ndon = size (xdont)
!
!    first loop is used to fill-in ilowt, ihigt at the same time
!
      if (ndon < 2) then
         if (nord >= 1) irngt (1) = 1
         return
      end if
!
!  one chooses a pivot, best estimate possible to put fractile near
!  mid-point of the set of low values.
!
      if (xdont(2) < xdont(1)) then
         ilowt (1) = 2
         ihigt (1) = 1
      else
         ilowt (1) = 1
         ihigt (1) = 2
      end if
!
      if (ndon < 3) then
         if (nord >= 1) irngt (1) = ilowt (1)
         if (nord >= 2) irngt (2) = ihigt (1)
         return
      end if
!
      if (xdont(3) <= xdont(ihigt(1))) then
         ihigt (2) = ihigt (1)
         if (xdont(3) < xdont(ilowt(1))) then
            ihigt (1) = ilowt (1)
            ilowt (1) = 3
         else
            ihigt (1) = 3
         end if
      else
         ihigt (2) = 3
      end if
!
      if (ndon < 4) then
         if (nord >= 1) irngt (1) = ilowt (1)
         if (nord >= 2) irngt (2) = ihigt (1)
         if (nord >= 3) irngt (3) = ihigt (2)
         return
      end if
!
      if (xdont(ndon) <= xdont(ihigt(1))) then
         ihigt (3) = ihigt (2)
         ihigt (2) = ihigt (1)
         if (xdont(ndon) < xdont(ilowt(1))) then
            ihigt (1) = ilowt (1)
            ilowt (1) = ndon
         else
            ihigt (1) = ndon
         end if
      else
         if (xdont (ndon) < xdont (ihigt(2))) then
            ihigt (3) = ihigt (2)
            ihigt (2) = ndon
         else
            ihigt (3) = ndon
         endif
      end if
!
      if (ndon < 5) then
         if (nord >= 1) irngt (1) = ilowt (1)
         if (nord >= 2) irngt (2) = ihigt (1)
         if (nord >= 3) irngt (3) = ihigt (2)
         if (nord >= 4) irngt (4) = ihigt (3)
         return
      end if
!
      jdeb = 0
      ideb = jdeb + 1
      jlow = ideb
      jhig = 3
      xpiv = xdont (ilowt(ideb)) + real(2*nord)/real(ndon+nord) * &
                                   (xdont(ihigt(3))-xdont(ilowt(ideb)))
      if (xpiv >= xdont(ihigt(1))) then
         xpiv = xdont (ilowt(ideb)) + real(2*nord)/real(ndon+nord) * &
                                      (xdont(ihigt(2))-xdont(ilowt(ideb)))
         if (xpiv >= xdont(ihigt(1))) &
             xpiv = xdont (ilowt(ideb)) + real (2*nord) / real (ndon+nord) * &
                                          (xdont(ihigt(1))-xdont(ilowt(ideb)))
      end if
      xpiv0 = xpiv
!
!  one puts values > pivot in the end and those <= pivot
!  at the beginning. this is split in 2 cases, so that
!  we can skip the loop test a number of times.
!  as we are also filling in the work arrays at the same time
!  we stop filling in the ihigt array as soon as we have more
!  than enough values in ilowt.
!
!
      if (xdont(ndon) > xpiv) then
         icrs = 3
         do
            icrs = icrs + 1
            if (xdont(icrs) > xpiv) then
               if (icrs >= ndon) exit
               jhig = jhig + 1
               ihigt (jhig) = icrs
            else
               jlow = jlow + 1
               ilowt (jlow) = icrs
               if (jlow >= nord) exit
            end if
         end do
!
!  one restricts further processing because it is no use
!  to store more high values
!
         if (icrs < ndon-1) then
            do
               icrs = icrs + 1
               if (xdont(icrs) <= xpiv) then
                  jlow = jlow + 1
                  ilowt (jlow) = icrs
               else if (icrs >= ndon) then
                  exit
               end if
            end do
         end if
!
!
      else
!
!  same as above, but this is not as easy to optimize, so the
!  do-loop is kept
!
         do icrs = 4, ndon - 1
            if (xdont(icrs) > xpiv) then
               jhig = jhig + 1
               ihigt (jhig) = icrs
            else
               jlow = jlow + 1
               ilowt (jlow) = icrs
               if (jlow >= nord) exit
            end if
         end do
!
         if (icrs < ndon-1) then
            do
               icrs = icrs + 1
               if (xdont(icrs) <= xpiv) then
                  if (icrs >= ndon) exit
                  jlow = jlow + 1
                  ilowt (jlow) = icrs
               end if
            end do
         end if
      end if
!
      jlm2 = 0
      jlm1 = 0
      jhm2 = 0
      jhm1 = 0
      do
         if (jlow == nord) exit
         if (jlm2 == jlow .and. jhm2 == jhig) then
!
!   we are oscillating. perturbate by bringing jlow closer by one
!   to nord
!
           if (nord > jlow) then
                xmin = xdont (ihigt(1))
                ihig = 1
                do icrs = 2, jhig
                   if (xdont(ihigt(icrs)) < xmin) then
                      xmin = xdont (ihigt(icrs))
                      ihig = icrs
                   end if
                end do
!
                jlow = jlow + 1
                ilowt (jlow) = ihigt (ihig)
                ihigt (ihig) = ihigt (jhig)
                jhig = jhig - 1
             else
                ilow = ilowt (jlow)
                xmax = xdont (ilow)
                do icrs = 1, jlow
                   if (xdont(ilowt(icrs)) > xmax) then
                      iwrk = ilowt (icrs)
                      xmax = xdont (iwrk)
                      ilowt (icrs) = ilow
                      ilow = iwrk
                   end if
                end do
                jlow = jlow - 1
             end if
         end if
         jlm2 = jlm1
         jlm1 = jlow
         jhm2 = jhm1
         jhm1 = jhig
!
!   we try to bring the number of values in the low values set
!   closer to nord.
!
        select case (nord-jlow)
         case (2:)
!
!   not enough values in low part, at least 2 are missing
!
            select case (jhig)
!!!!!           case default
!!!!!              write (*,*) "assertion failed"
!!!!!              stop
!
!   we make a special case when we have so few values in
!   the high values set that it is bad performance to choose a pivot
!   and apply the general algorithm.
!
            case (2)
               if (xdont(ihigt(1)) <= xdont(ihigt(2))) then
                  jlow = jlow + 1
                  ilowt (jlow) = ihigt (1)
                  jlow = jlow + 1
                  ilowt (jlow) = ihigt (2)
               else
                  jlow = jlow + 1
                  ilowt (jlow) = ihigt (2)
                  jlow = jlow + 1
                  ilowt (jlow) = ihigt (1)
               end if
               exit
!
            case (3)
!
!
               iwrk1 = ihigt (1)
               iwrk2 = ihigt (2)
               iwrk3 = ihigt (3)
               if (xdont(iwrk2) < xdont(iwrk1)) then
                  ihigt (1) = iwrk2
                  ihigt (2) = iwrk1
                  iwrk2 = iwrk1
               end if
               if (xdont(iwrk2) > xdont(iwrk3)) then
                  ihigt (3) = iwrk2
                  ihigt (2) = iwrk3
                  iwrk2 = iwrk3
                  if (xdont(iwrk2) < xdont(ihigt(1))) then
                     ihigt (2) = ihigt (1)
                     ihigt (1) = iwrk2
                  end if
               end if
               jhig = 0
               do icrs = jlow + 1, nord
                  jhig = jhig + 1
                  ilowt (icrs) = ihigt (jhig)
               end do
               jlow = nord
               exit
!
            case (4:)
!
!
               xpiv0 = xpiv
               ifin = jhig
!
!  one chooses a pivot from the 2 first values and the last one.
!  this should ensure sufficient renewal between iterations to
!  avoid worst case behavior effects.
!
               iwrk1 = ihigt (1)
               iwrk2 = ihigt (2)
               iwrk3 = ihigt (ifin)
               if (xdont(iwrk2) < xdont(iwrk1)) then
                  ihigt (1) = iwrk2
                  ihigt (2) = iwrk1
                  iwrk2 = iwrk1
               end if
               if (xdont(iwrk2) > xdont(iwrk3)) then
                  ihigt (ifin) = iwrk2
                  ihigt (2) = iwrk3
                  iwrk2 = iwrk3
                  if (xdont(iwrk2) < xdont(ihigt(1))) then
                     ihigt (2) = ihigt (1)
                     ihigt (1) = iwrk2
                  end if
               end if
!
               jdeb = jlow
               nwrk = nord - jlow
               iwrk1 = ihigt (1)
               jlow = jlow + 1
               ilowt (jlow) = iwrk1
               xpiv = xdont (iwrk1) + real (nwrk) / real (nord+nwrk) * &
                                      (xdont(ihigt(ifin))-xdont(iwrk1))
!
!  one takes values <= pivot to ilowt
!  again, 2 parts, one where we take care of the remaining
!  high values because we might still need them, and the
!  other when we know that we will have more than enough
!  low values in the end.
!
               jhig = 0
               do icrs = 2, ifin
                  if (xdont(ihigt(icrs)) <= xpiv) then
                     jlow = jlow + 1
                     ilowt (jlow) = ihigt (icrs)
                     if (jlow >= nord) exit
                  else
                     jhig = jhig + 1
                     ihigt (jhig) = ihigt (icrs)
                  end if
               end do
!
               do icrs = icrs + 1, ifin
                  if (xdont(ihigt(icrs)) <= xpiv) then
                     jlow = jlow + 1
                     ilowt (jlow) = ihigt (icrs)
                  end if
               end do
           end select
!
!
         case (1)
!
!  only 1 value is missing in low part
!
            xmin = xdont (ihigt(1))
            ihig = 1
            do icrs = 2, jhig
               if (xdont(ihigt(icrs)) < xmin) then
                  xmin = xdont (ihigt(icrs))
                  ihig = icrs
               end if
            end do
!
            jlow = jlow + 1
            ilowt (jlow) = ihigt (ihig)
            exit
!
!
         case (0)
!
!  low part is exactly what we want
!
            exit
!
!
         case (-5:-1)
!
!  only few values too many in low part
!
            irngt (1) = ilowt (1)
            do icrs = 2, nord
               iwrk = ilowt (icrs)
               xwrk = xdont (iwrk)
               do idcr = icrs - 1, 1, - 1
                  if (xwrk < xdont(irngt(idcr))) then
                     irngt (idcr+1) = irngt (idcr)
                  else
                     exit
                  end if
               end do
               irngt (idcr+1) = iwrk
            end do
!
            xwrk1 = xdont (irngt(nord))
            do icrs = nord + 1, jlow
               if (xdont(ilowt (icrs)) < xwrk1) then
                  xwrk = xdont (ilowt (icrs))
                  do idcr = nord - 1, 1, - 1
                     if (xwrk >= xdont(irngt(idcr))) exit
                     irngt (idcr+1) = irngt (idcr)
                  end do
                  irngt (idcr+1) = ilowt (icrs)
                  xwrk1 = xdont (irngt(nord))
               end if
            end do
!
            return
!
!
         case (:-6)
!
! last case: too many values in low part
!
            ideb = jdeb + 1
            imil = (jlow+ideb) / 2
            ifin = jlow
!
!  one chooses a pivot from 1st, last, and middle values
!
            if (xdont(ilowt(imil)) < xdont(ilowt(ideb))) then
               iwrk = ilowt (ideb)
               ilowt (ideb) = ilowt (imil)
               ilowt (imil) = iwrk
            end if
            if (xdont(ilowt(imil)) > xdont(ilowt(ifin))) then
               iwrk = ilowt (ifin)
               ilowt (ifin) = ilowt (imil)
               ilowt (imil) = iwrk
               if (xdont(ilowt(imil)) < xdont(ilowt(ideb))) then
                  iwrk = ilowt (ideb)
                  ilowt (ideb) = ilowt (imil)
                  ilowt (imil) = iwrk
               end if
            end if
            if (ifin <= 3) exit
!
            xpiv = xdont (ilowt(1)) + real(nord)/real(jlow+nord) * &
                                      (xdont(ilowt(ifin))-xdont(ilowt(1)))
            if (jdeb > 0) then
               if (xpiv <= xpiv0) &
                   xpiv = xpiv0 + real(2*nord-jdeb)/real (jlow+nord) * &
                                  (xdont(ilowt(ifin))-xpiv0)
            else
               ideb = 1
            end if
!
!  one takes values > xpiv to ihigt
!  however, we do not process the first values if we have been
!  through the case when we did not have enough low values
!
            jhig = 0
            jlow = jdeb
!
            if (xdont(ilowt(ifin)) > xpiv) then
               icrs = jdeb
               do
                 icrs = icrs + 1
                  if (xdont(ilowt(icrs)) > xpiv) then
                     jhig = jhig + 1
                     ihigt (jhig) = ilowt (icrs)
                     if (icrs >= ifin) exit
                  else
                     jlow = jlow + 1
                     ilowt (jlow) = ilowt (icrs)
                     if (jlow >= nord) exit
                  end if
               end do
!
               if (icrs < ifin) then
                  do
                     icrs = icrs + 1
                     if (xdont(ilowt(icrs)) <= xpiv) then
                        jlow = jlow + 1
                        ilowt (jlow) = ilowt (icrs)
                     else
                        if (icrs >= ifin) exit
                     end if
                  end do
               end if
           else
               do icrs = ideb, ifin
                  if (xdont(ilowt(icrs)) > xpiv) then
                     jhig = jhig + 1
                     ihigt (jhig) = ilowt (icrs)
                  else
                     jlow = jlow + 1
                     ilowt (jlow) = ilowt (icrs)
                     if (jlow >= nord) exit
                  end if
               end do
!
               do icrs = icrs + 1, ifin
                  if (xdont(ilowt(icrs)) <= xpiv) then
                     jlow = jlow + 1
                     ilowt (jlow) = ilowt (icrs)
                  end if
               end do
            end if
!
         end select
!
      end do
!
!  now, we only need to complete ranking of the 1:nord set
!  assuming nord is small, we use a simple insertion sort
!
      irngt (1) = ilowt (1)
      do icrs = 2, nord
         iwrk = ilowt (icrs)
         xwrk = xdont (iwrk)
         do idcr = icrs - 1, 1, - 1
            if (xwrk < xdont(irngt(idcr))) then
               irngt (idcr+1) = irngt (idcr)
            else
               exit
            end if
         end do
         irngt (idcr+1) = iwrk
      end do
     return
!
!
end subroutine RNKPAR_I

!-------------------------------------------------------------------------------

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! The procedures below is LINTERPOL subroutine by David G. Simpson
! http://www.davidgsimpson.com/software/linterpol_f90.txt
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

function LINTERPOL_R4 (xx, yy, x, ierr) result (y)
!*******************************************************************************
! LINTERPOL
!
! PURPOSE: Simple piecewise liner interpolation.  This is a kind 4 real version
!
! CALL PARAMETERS: Ordered array for independent variable, array for the
!          dependent variable, independent variable X value  to interpolate,
!          optional integer error code.
!
! RETURNS: Interpolated value Y(X)
!
! NOTES:   Piecewise linear interpolation.  Given input arrays XX (independent
!          variable) and YY (dependent variable), both of dimension NN,
!          this routine finds, by linear interpolation, the value of Y(X).
!          Array XX must be in ascending order.
!          The flag IERR is returned as -1 if X is below the low end of XX
!          (an error), +1 if X is above the high end of XX (also an error),
!          or 0 if there was no error.
!          If the xx and yy input arrays are not conforming, the ierr error is
!          multiplied by 100.
!
!          Error codes ierr:
!                         0 = no error;
!                       100 = input arrays not conforming;
!                        -1 = X below the low limit;
!                         1 = X above the upper limit;
!                      -101 = input arrays not conforming, X below low limit;
!                       101 = input arrays not conforming, X above upper limit,
!                     -9999 = X array is not strictly increasing ordered.
!
! Author: David G. Simpson, NASA Goddard Space Flight Center, Greenbelt,
!         Maryland  20771; Version 1.00a, October 29, 2013
! From:   http://www.davidgsimpson.com/software/linterpol_f90.txt
!
! Modified by Sergey Budaev
!*******************************************************************************
   implicit none

   real, dimension(:), intent(in) :: xx, yy  ! Indep. and dep. variable arrays.
   real, intent(in) :: x                     ! Interpolate at x
   integer, optional, intent(out) :: ierr    ! Returned error code.

   real :: y                                 ! Interpolated value y(x).

   integer :: nn                             ! dimension of xx and yy arrays

   integer :: i                              ! Local counter.
   integer :: ierr_here                      ! Local copy of ierr.

   real ( kind = 4 ), parameter ::  INVALID = -9999.0_4

   ! Check if the input vector is strictly increasing.
   if ( .not. R4VEC_ASCENDS_STRICTLY ( size(xx), xx ) ) then
      if (present(ierr)) ierr=-9999
      y = INVALID
      return
   end if

   if (size(xx) == size(yy)) then            ! Check the sizes of the arrays
      nn = size(xx)                          ! set as normal if equal...
      ierr_here = 0
   else
      nn = min(size(xx),size(yy))            ! .. or to minimum size otherwise
      ierr_here = 100                        ! (error factor updated by 100),
   end if                                    ! extra values are ignored.

   if (x .lt. xx(1)) then                    ! If below low end of xx (error)..
      y = yy(1)                              !  set y = first yy value,
      ierr_here = (ierr_here + 1) * (-1)     !  return error code -1 (or -101);
   else if (x .gt. xx(nn)) then              ! if above high end of xx (error)..
      y = yy(nn)                             !  set y = last yy value,
      ierr_here = ierr_here + 1              !  return error code +1 (or 101),
   else                                      ! if ok
      do i = 2, nn                           ! loop to find first xx > x .
         if (xx(i) .gt. x) exit
      end do
      y = (yy(i)-yy(i-1))/(xx(i)-xx(i-1))*(x-xx(i-1))+yy(i-1) ! Do interpolate.
      ierr_here = ierr_here                  ! set no error code to 0 (or 100).
   end if

   if (present(ierr)) ierr = ierr_here

end function LINTERPOL_R4

!-------------------------------------------------------------------------------

function LINTERPOL_R8 (xx, yy, x, ierr) result (y)
!*******************************************************************************
! LINTERPOL
!
! PURPOSE: Simple piecewise liner interpolation. This is a kind 8 real version
!
! CALL PARAMETERS: Ordered array for independent variable, array for the
!          dependent variable, independent variable X value  to interpolate,
!          optional integer error code.
!
! RETURNS: Interpolated value Y(X)
!
! NOTES:   Piecewise linear interpolation.  Given input arrays XX (independent
!          variable) and YY (dependent variable), both of dimension NN,
!          this routine finds, by linear interpolation, the value of Y(X).
!          Array XX must be in ascending order.
!          The flag IERR is returned as -1 if X is below the low end of XX
!          (an error), +1 if X is above the high end of XX (also an error),
!          or 0 if there was no error.
!          If the xx and yy input arrays are not conforming, the ier error is
!          multiplied by 100.
!
!          Error codes ierr:
!                         0 = no error;
!                       100 = input arrays not conforming;
!                        -1 = X below the low limit;
!                         1 = X above the upper limit;
!                      -101 = input arrays not conforming, X below low limit;
!                       101 = input arrays not conforming, X above upper limit,
!                     -9999 = X array is not strictly increasing ordered.
!
! Author: David G. Simpson, NASA Goddard Space Flight Center, Greenbelt,
!         Maryland  20771; Version 1.00a, October 29, 2013
! From:   http://www.davidgsimpson.com/software/linterpol_f90.txt
! Modified by Sergey Budaev
!*******************************************************************************
   implicit none

   real(kind=8), dimension(:), intent(in) :: xx ! Indep. variable array.
   real(kind=8), dimension(:), intent(in) :: yy ! Dep. variable array.
   real(kind=8), intent(in) :: x             ! Interpolate at x
   integer, optional, intent(out) :: ierr    ! Returned error code.

   real(kind=8) :: y                         ! Interpolated value y(x).

   integer :: nn                             ! dimension of xx and yy arrays

   integer :: i                              ! Local counter.
   integer :: ierr_here                      ! Local copy of ierr.

   real ( kind = 8 ), parameter ::  INVALID = -9999.0_8

   ! Check if the input vector is strictly increasing.
   if ( .not. R8VEC_ASCENDS_STRICTLY ( size(xx), xx ) ) then
      if (present(ierr)) ierr=-9999
      y = INVALID
      return
   end if

   if (size(xx) == size(yy)) then            ! Check the sizes of the arrays
      nn = size(xx)                          ! set as normal if equal...
      ierr_here = 0
   else
      nn = min(size(xx),size(yy))            ! .. or to minimum size otherwise
      ierr_here = 100                        ! (error factor updated by 100),
   end if                                    ! extra values are ignored.

   if (x .lt. xx(1)) then                    ! If below low end of xx (error)..
      y = yy(1)                              !  set y = first yy value,
      ierr_here = (ierr_here + 1) * (-1)     !  return error code -1 (or -101);
   else if (x .gt. xx(nn)) then              ! if above high end of xx (error)..
      y = yy(nn)                             !  set y = last yy value,
      ierr_here = ierr_here + 1              !  return error code +1 (or 101),
   else                                      ! if ok
      do i = 2, nn                           ! loop to find first xx > x .
         if (xx(i) .gt. x) exit
      end do
      y = (yy(i)-yy(i-1))/(xx(i)-xx(i-1))*(x-xx(i-1))+yy(i-1) ! Do interpolate.
      ierr_here = ierr_here                  ! set no error code to 0 (or 100).
   end if

   if (present(ierr)) ierr = ierr_here

end function LINTERPOL_R8

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! The procedures below are taken from the INTERP library by John Burkardt
! http://people.sc.fsu.edu/~jburkardt%20/f_src/interp/interp.html
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

subroutine INTERP_LAGRANGE_R8 ( t_data, p_data, t_interp, p_interp )
!*******************************************************************************
!
! INTERP_LAGRANGE: Lagrange polynomial interpolant to a curve in M dimensions.
!
! NOTE: This is the real kind 8 procedure. See R4.. for default kind 4 procs.
!
!  Discussion:
!
!    From a space of M dimensions, we are given a sequence of
!    DATA_NUM points, which are presumed to be successive samples
!    from a curve of points P.
!
!    We are also given a parameterization of this data, that is,
!    an associated sequence of DATA_NUM values of a variable T.
!
!    Thus, we have a sequence of values P(T), where T is a scalar,
!    and each value of P is of dimension M.
!
!    We are then given INTERP_NUM values of T, for which values P
!    are to be produced, by linear interpolation of the data we are given.
!
!    The user may request extrapolation.  This occurs whenever
!    a T_INTERP value is less than the minimum T_DATA or greater than the
!    maximum T_DATA.  In that case, extrapolation is used.
!
!    For each spatial component, a polynomial of degree
!    ( DATA_NUM - 1 ) is generated for the interpolation.  In most cases,
!    such a polynomial interpolant begins to oscillate as DATA_NUM
!    increases, even if the original data seems well behaved.  Typically,
!    values of DATA_NUM should be no greater than 10!
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 December 2007
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the spatial dimension.
!
!    Input, integer ( kind = 4 ) DATA_NUM, the number of data points.
!
!    Input, real ( kind = 8 ) T_DATA(DATA_NUM), the value of the
!    independent variable at the sample points.
!
!    Input, real ( kind = 8 ) P_DATA(M,DATA_NUM), the value of the
!    dependent variables at the sample points.
!
!    Input, integer ( kind = 4 ) INTERP_NUM, the number of points
!    at which interpolation is to be done.
!
!    Input, real ( kind = 8 ) T_INTERP(INTERP_NUM), the value of the
!    independent variable at the interpolation points.
!
!    Output, real ( kind = 8 ) P_INTERP(M,DATA_NUM), the interpolated
!    values of the dependent variables at the interpolation points.
!
! Modified by Sergey Budaev
! Source : http://people.sc.fsu.edu/~jburkardt%20/f_src/interp/interp.html
!*******************************************************************************

  implicit none

  real ( kind = 8 ), intent(in) :: p_data(:,:)
  real ( kind = 8 ), intent(out) :: p_interp(:,:)
  real ( kind = 8 ), intent(in) :: t_data(:)
  real ( kind = 8 ), intent(in) :: t_interp(:)

  integer :: interp_num
  integer :: data_num
  integer :: m

  real ( kind = 8 ) l_interp( size(p_data, 2),size(p_interp, 2) )

  interp_num = size(p_interp, 2)
  data_num = size(p_data, 2)
  m = size(p_data, 1)

!  Evaluate the DATA_NUM Lagrange polynomials associated with T_DATA(1:DATA_NUM)
!  for the interpolation points T_INTERP(1:INTERP_NUM).

  call LAGRANGE_VALUE_R8 ( data_num, t_data, interp_num, t_interp, l_interp )

!  Multiply P_DATA(1:M,1:DATA_NUM) * L_INTERP(1:DATA_NUM,1:INTERP_NUM)
!  to get P_INTERP(1:M,1:INTERP_NUM).

  p_interp(1:m,1:interp_num) = &
    matmul ( p_data(1:m,1:data_num), l_interp(1:data_num,1:interp_num) )

  return

end subroutine INTERP_LAGRANGE_R8

!-------------------------------------------------------------------------------

subroutine INTERP_LINEAR_R8 ( t_data, p_data, t_interp, p_interp, error_code )
!*******************************************************************************
!
! INTERP_LINEAR: piecewise linear interpolation to a curve in M dimensions.
!
! NOTE: This is the real kind 8 procedure. See R4.. for default kind 4 procs.
!
!  Discussion:
!
!    From a space of M dimensions, we are given a sequence of
!    data_num points, which are presumed to be successive samples
!    from a curve of points P.
!
!    We are also given a parameterization of this data, that is,
!    an associated sequence of data_num values of a variable T.
!    The values of T are assumed to be strictly increasing.
!
!    Thus, we have a sequence of values P(T), where T is a scalar,
!    and each value of P is of dimension M.
!
!    We are then given interp_num values of T, for which values P
!    are to be produced, by linear interpolation of the data we are given.
!
!    Note that the user may request extrapolation.  This occurs whenever
!    a t_interp value is less than the minimum t_data or greater than the
!    maximum t_data.  In that case, linear extrapolation is used.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 December 2007
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    -- Input, integer ( kind = 4 ) M, the spatial dimension.
!
!    Input, integer ( kind = 4 ) data_num, the number of data points.
!
!    -- Input, real ( kind = 8 ) t_data(data_num), the value of the
!    independent variable at the sample points.  The values of t_data
!    must be strictly increasing.
!
!    Input, real ( kind = 8 ) P_DATA(M,data_num), the value of the
!    dependent variables at the sample points.
!
!    Input, integer ( kind = 4 ) interp_num, the number of points
!    at which interpolation is to be done.
!
!    -- Input, real ( kind = 8 ) t_interp(interp_num), the value of the
!    independent variable at the interpolation points.
!
!    Output, real ( kind = 8 ) p_interp(M,data_num), the interpolated
!    values of the dependent variables at the interpolation points.
!
! Modified by Sergey Budaev
! Source : http://people.sc.fsu.edu/~jburkardt%20/f_src/interp/interp.html
!*******************************************************************************

  implicit none

  integer interp
  integer left
  real ( kind = 8 ), intent(in) :: p_data(:,:)
  real ( kind = 8 ), intent(out) ::  p_interp(:,:)
  logical, optional :: error_code      !> Error code if not strictly increasing.

  integer :: interp_num
  integer :: data_num
  integer :: m

  integer right
  real ( kind = 8 ) t
  real ( kind = 8 ), intent(in) :: t_data(:)
  real ( kind = 8 ), intent(in) :: t_interp(:)

  real ( kind = 8 ), parameter ::  INVALID = -9999.0_8

  m = size(p_data, 1)
  interp_num = size(p_interp, 2)
  data_num = size(p_data, 2)

  if ( .not. R8VEC_ASCENDS_STRICTLY ( data_num, t_data ) ) then
   if (present(error_code)) error_code = .TRUE.
   p_interp = INVALID
   return
  end if

  do interp = 1, interp_num

    t = t_interp(interp)
!
!  Find the interval [ TDATA(LEFT), TDATA(RIGHT) ] that contains, or is
!  nearest to, TVAL.
!
    call R8VEC_BRACKET ( data_num, t_data, t, left, right )

    p_interp(1:m,interp) = &
      ( ( t_data(right) - t                ) * p_data(1:m,left)   &
      + (                 t - t_data(left) ) * p_data(1:m,right) ) &
      / ( t_data(right)     - t_data(left) )

  end do

  if (present(error_code)) error_code = .FALSE. ! No error, flag FALSE.

  return

end subroutine INTERP_LINEAR_R8

!-------------------------------------------------------------------------------

subroutine LAGRANGE_VALUE_R8 ( data_num, t_data, interp_num, t_interp, l_interp )
!*******************************************************************************
!
! LAGRANGE_VALUE_R8 evaluates the Lagrange polynomials.
!
! NOTE: This is the real kind 8 procedure. See R4.. for default kind 4 procs.
!
!  Discussion:
!
!    Given DATA_NUM distinct abscissas, T_DATA(1:DATA_NUM),
!    the I-th Lagrange polynomial L(I)(T) is defined as the polynomial of
!    degree DATA_NUM - 1 which is 1 at T_DATA(I) and 0 at the DATA_NUM - 1
!    other abscissas.
!
!    A formal representation is:
!
!      L(I)(T) = Product ( 1 <= J <= DATA_NUM, I /= J )
!       ( T - T(J) ) / ( T(I) - T(J) )
!
!    This routine accepts a set of INTERP_NUM values at which all the Lagrange
!    polynomials should be evaluated.
!
!    Given data values P_DATA at each of the abscissas, the value of the
!    Lagrange interpolating polynomial at each of the interpolation points
!    is then simple to compute by matrix multiplication:
!
!      P_INTERP(1:INTERP_NUM) =
!        P_DATA(1:DATA_NUM) * L_INTERP(1:DATA_NUM,1:INTERP_NUM)
!
!    or, in the case where P is multidimensional:
!
!      P_INTERP(1:M,1:INTERP_NUM) =
!        P_DATA(1:M,1:DATA_NUM) * L_INTERP(1:DATA_NUM,1:INTERP_NUM)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 December 2007
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) DATA_NUM, the number of data points.
!    DATA_NUM must be at least 1.
!
!    Input, real ( kind = 8 ) T_DATA(DATA_NUM), the data points.
!
!    Input, integer ( kind = 4 ) INTERP_NUM, the number of
!    interpolation points.
!
!    Input, real ( kind = 8 ) T_INTERP(INTERP_NUM), the
!    interpolation points.
!
!    Output, real ( kind = 8 ) L_INTERP(DATA_NUM,INTERP_NUM), the values
!    of the Lagrange polynomials at the interpolation points.
!
! Source : http://people.sc.fsu.edu/~jburkardt%20/f_src/interp/interp.html
!*******************************************************************************

  implicit none

  integer data_num
  integer interp_num

  integer i
  integer j
  real ( kind = 8 ) l_interp(data_num,interp_num)
  real ( kind = 8 ) t_data(data_num)
  real ( kind = 8 ) t_interp(interp_num)
!
!  Evaluate the polynomial.
!
  l_interp(1:data_num,1:interp_num) = 1.0D+00

  do i = 1, data_num

    do j = 1, data_num

      if ( j /= i ) then

        l_interp(i,1:interp_num) = l_interp(i,1:interp_num) &
          * ( t_interp(1:interp_num) - t_data(j) ) / ( t_data(i) - t_data(j) )

      end if

    end do

  end do

  return

end subroutine LAGRANGE_VALUE_R8

!-------------------------------------------------------------------------------

function R8VEC_ASCENDS_STRICTLY ( n, x ) result (is_increasing)
!*******************************************************************************
!
! R8VEC_ASCENDS_STRICTLY determines if an R8VEC is strictly ascending.
!
! NOTE: This is the real kind 8 procedure. See R4.. for default kind 4 procs.
!
!  Discussion:
!
!    An R8VEC is a vector of R8 values.
!
!    Notice the effect of entry number 6 in the following results:
!
!      X = ( -8.1, 1.3, 2.2, 3.4, 7.5, 7.4, 9.8 )
!      Y = ( -8.1, 1.3, 2.2, 3.4, 7.5, 7.5, 9.8 )
!      Z = ( -8.1, 1.3, 2.2, 3.4, 7.5, 7.6, 9.8 )
!
!      R8VEC_ASCENDS_STRICTLY ( X ) = FALSE
!      R8VEC_ASCENDS_STRICTLY ( Y ) = FALSE
!      R8VEC_ASCENDS_STRICTLY ( Z ) = TRUE
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 December 2007
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the size of the array.
!
!    Input, real ( kind = 8 ) X(N), the array to be examined.
!
!    Output, logical R8VEC_ASCENDS_STRICTLY, is TRUE if the
!    entries of X strictly ascend.
!
! Modified by Sergey Budaev
! Source : http://people.sc.fsu.edu/~jburkardt%20/f_src/interp/interp.html
!*******************************************************************************

  implicit none

  integer n

  integer i
  logical is_increasing
  real ( kind = 8 ) x(n)

  do i = 1, n - 1
    if ( x(i+1) <= x(i) ) then
      is_increasing = .false.
      return
    end if
  end do

  is_increasing = .true.

  return

end function R8VEC_ASCENDS_STRICTLY

!-------------------------------------------------------------------------------

subroutine R8VEC_BRACKET ( n, x, xval, left, right )
!*******************************************************************************
!
! R8VEC_BRACKET searches a sorted R8VEC for successive brackets of a value.
!
! NOTE: This is the real kind 8 procedure. See R4.. for default kind 4 procs.
!
!  Discussion:
!
!    An R8VEC is an array of double precision real values.
!
!    If the values in the vector are thought of as defining intervals
!    on the real line, then this routine searches for the interval
!    nearest to or containing the given value.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 April 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, length of input array.
!
!    Input, real ( kind = 8 ) X(N), an array sorted into ascending order.
!
!    Input, real ( kind = 8 ) XVAL, a value to be bracketed.
!
!    Output, integer ( kind = 4 ) LEFT, RIGHT, the results of the search.
!    Either:
!      XVAL < X(1), when LEFT = 1, RIGHT = 2;
!      X(N) < XVAL, when LEFT = N-1, RIGHT = N;
!    or
!      X(LEFT) <= XVAL <= X(RIGHT).
!
! Source : http://people.sc.fsu.edu/~jburkardt%20/f_src/interp/interp.html
!*******************************************************************************

  implicit none

  integer n

  integer i
  integer left
  integer right
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) xval

  do i = 2, n - 1

    if ( xval < x(i) ) then
      left = i - 1
      right = i
      return
    end if

   end do

  left = n - 1
  right = n

  return

end subroutine R8VEC_BRACKET

!-------------------------------------------------------------------------------

subroutine INTERP_LAGRANGE_R4 ( t_data, p_data, t_interp, p_interp )
!*******************************************************************************
!
! INTERP_LAGRANGE: Lagrange polynomial interpolant to a curve in M dimensions.
!
! NOTE: This is the real kind 4 procedure. See R8.. for kind 8 procs.
!
!  Discussion:
!
!    From a space of M dimensions, we are given a sequence of
!    DATA_NUM points, which are presumed to be successive samples
!    from a curve of points P.
!
!    We are also given a parameterization of this data, that is,
!    an associated sequence of DATA_NUM values of a variable T.
!
!    Thus, we have a sequence of values P(T), where T is a scalar,
!    and each value of P is of dimension M.
!
!    We are then given INTERP_NUM values of T, for which values P
!    are to be produced, by linear interpolation of the data we are given.
!
!    The user may request extrapolation.  This occurs whenever
!    a T_INTERP value is less than the minimum T_DATA or greater than the
!    maximum T_DATA.  In that case, extrapolation is used.
!
!    For each spatial component, a polynomial of degree
!    ( DATA_NUM - 1 ) is generated for the interpolation.  In most cases,
!    such a polynomial interpolant begins to oscillate as DATA_NUM
!    increases, even if the original data seems well behaved.  Typically,
!    values of DATA_NUM should be no greater than 10!
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 December 2007
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the spatial dimension.
!
!    Input, integer ( kind = 4 ) data_num, the number of data points.
!
!    Input, real ( kind = 4 ) t_data(data_num), the value of the
!    independent variable at the sample points.
!
!    Input, real ( kind = 4 ) p_data(m,data_num), the value of the
!    dependent variables at the sample points.
!
!    Input, integer ( kind = 4 ) interp_num, the number of points
!    at which interpolation is to be done.
!
!    Input, real ( kind = 4 ) t_interp(interp_num), the value of the
!    independent variable at the interpolation points.
!
!    Output, real ( kind = 4 ) p_interp(m,data_num), the interpolated
!    values of the dependent variables at the interpolation points.
!
! Modified by Sergey Budaev
! Source : http://people.sc.fsu.edu/~jburkardt%20/f_src/interp/interp.html
!*******************************************************************************

  implicit none

  real, intent(in) :: p_data(:,:)
  real, intent(out) :: p_interp(:,:)
  real, intent(in) :: t_data(:)
  real, intent(in) :: t_interp(:)

  integer :: interp_num
  integer :: data_num
  integer :: m

  real :: l_interp( size(p_data, 2),size(p_interp, 2) )

  interp_num = size(p_interp, 2)
  data_num = size(p_data, 2)
  m = size(p_data, 1)

!  Evaluate the DATA_NUM Lagrange polynomials associated with T_DATA(1:DATA_NUM)
!  for the interpolation points T_INTERP(1:INTERP_NUM).

  call LAGRANGE_VALUE_R4 ( data_num, t_data, interp_num, t_interp, l_interp )

!  Multiply P_DATA(1:M,1:DATA_NUM) * L_INTERP(1:DATA_NUM,1:INTERP_NUM)
!  to get P_INTERP(1:M,1:INTERP_NUM).

  p_interp(1:m,1:interp_num) = &
    matmul ( p_data(1:m,1:data_num), l_interp(1:data_num,1:interp_num) )

  return

end subroutine INTERP_LAGRANGE_R4

!-------------------------------------------------------------------------------

subroutine INTERP_LINEAR_R4 ( t_data, p_data, t_interp, p_interp, error_code )
!*******************************************************************************
!
! INTERP_LINEAR: piecewise linear interpolation to a curve in M dimensions.
!
! NOTE: This is the real kind 4 procedure. See R8.. for kind 8 procs.
!
!  Discussion:
!
!    From a space of M dimensions, we are given a sequence of
!    data_num points, which are presumed to be successive samples
!    from a curve of points P.
!
!    We are also given a parameterization of this data, that is,
!    an associated sequence of data_num values of a variable T.
!    The values of T are assumed to be strictly increasing.
!
!    Thus, we have a sequence of values P(T), where T is a scalar,
!    and each value of P is of dimension M.
!
!    We are then given interp_num values of T, for which values P
!    are to be produced, by linear interpolation of the data we are given.
!
!    Note that the user may request extrapolation.  This occurs whenever
!    a t_interp value is less than the minimum t_data or greater than the
!    maximum t_data.  In that case, linear extrapolation is used.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 December 2007
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    -- Input, integer ( kind = 4 ) M, the spatial dimension.
!
!    -- Input, integer ( kind = 4 ) data_num, the number of data points.
!
!    Input, real ( kind = 4 ) t_data(data_num), the value of the
!    independent variable at the sample points.  The values of t_data
!    must be strictly increasing.
!
!    Input, real ( kind = 4 ) p_data(M,data_num), the value of the
!    dependent variables at the sample points.
!
!    -- Input, integer ( kind = 4 ) interp_num, the number of points
!    at which interpolation is to be done.
!
!    Input, real ( kind = 4 ) t_interp(interp_num), the value of the
!    independent variable at the interpolation points.
!
!    Output, real ( kind = 4 ) p_interp(M,data_num), the interpolated
!    values of the dependent variables at the interpolation points.
!
! Modified by Sergey Budaev
! Source : http://people.sc.fsu.edu/~jburkardt%20/f_src/interp/interp.html
!*******************************************************************************

  implicit none

  integer :: interp
  integer :: left
  real, intent(in) :: p_data(:,:)
  real, intent(out) ::  p_interp(:,:)
  logical, optional :: error_code      !> Error code if not strictly increasing.

  integer :: interp_num
  integer :: data_num
  integer :: m

  integer right
  real :: t
  real, intent(in) :: t_data(:)
  real, intent(in) :: t_interp(:)

  real, parameter ::  INVALID = -9999.0_4

  m = size(p_data, 1)
  interp_num = size(p_interp, 2)
  data_num = size(p_data, 2)

  if ( .not. R4VEC_ASCENDS_STRICTLY ( data_num, t_data ) ) then
   if (present(error_code)) error_code = .TRUE.
   p_interp = INVALID
   return
  end if

  do interp = 1, interp_num

    t = t_interp(interp)
!
!  Find the interval [ TDATA(LEFT), TDATA(RIGHT) ] that contains, or is
!  nearest to, TVAL.
!
    call R4VEC_BRACKET ( data_num, t_data, t, left, right )

    p_interp(1:m,interp) = &
      ( ( t_data(right) - t                ) * p_data(1:m,left)   &
      + (                 t - t_data(left) ) * p_data(1:m,right) ) &
      / ( t_data(right)     - t_data(left) )

  end do

  if (present(error_code)) error_code = .FALSE. ! No error, flag FALSE.

  return

end subroutine INTERP_LINEAR_R4

!-------------------------------------------------------------------------------

subroutine LAGRANGE_VALUE_R4 ( data_num, t_data, interp_num, t_interp, l_interp )
!*******************************************************************************
!
! LAGRANGE_VALUE_R8 evaluates the Lagrange polynomials.
!
! NOTE: This is the real kind 4 procedure. See R8.. for kind 8 procs.
!
!  Discussion:
!
!    Given DATA_NUM distinct abscissas, T_DATA(1:DATA_NUM),
!    the I-th Lagrange polynomial L(I)(T) is defined as the polynomial of
!    degree DATA_NUM - 1 which is 1 at T_DATA(I) and 0 at the DATA_NUM - 1
!    other abscissas.
!
!    A formal representation is:
!
!      L(I)(T) = Product ( 1 <= J <= DATA_NUM, I /= J )
!       ( T - T(J) ) / ( T(I) - T(J) )
!
!    This routine accepts a set of INTERP_NUM values at which all the Lagrange
!    polynomials should be evaluated.
!
!    Given data values P_DATA at each of the abscissas, the value of the
!    Lagrange interpolating polynomial at each of the interpolation points
!    is then simple to compute by matrix multiplication:
!
!      P_INTERP(1:INTERP_NUM) =
!        P_DATA(1:DATA_NUM) * L_INTERP(1:DATA_NUM,1:INTERP_NUM)
!
!    or, in the case where P is multidimensional:
!
!      P_INTERP(1:M,1:INTERP_NUM) =
!        P_DATA(1:M,1:DATA_NUM) * L_INTERP(1:DATA_NUM,1:INTERP_NUM)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 December 2007
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) DATA_NUM, the number of data points.
!    DATA_NUM must be at least 1.
!
!    Input, real ( kind = 4 ) T_DATA(DATA_NUM), the data points.
!
!    Input, integer ( kind = 4 ) INTERP_NUM, the number of
!    interpolation points.
!
!    Input, real ( kind = 4 ) T_INTERP(INTERP_NUM), the
!    interpolation points.
!
!    Output, real ( kind = 4 ) L_INTERP(DATA_NUM,INTERP_NUM), the values
!    of the Lagrange polynomials at the interpolation points.
!
! Source : http://people.sc.fsu.edu/~jburkardt%20/f_src/interp/interp.html
!*******************************************************************************

  implicit none

  integer :: data_num
  integer :: interp_num

  integer :: i
  integer :: j
  real :: l_interp(data_num,interp_num)
  real :: t_data(data_num)
  real :: t_interp(interp_num)
!
!  Evaluate the polynomial.
!
  l_interp(1:data_num,1:interp_num) = 1.0D+00

  do i = 1, data_num

    do j = 1, data_num

      if ( j /= i ) then

        l_interp(i,1:interp_num) = l_interp(i,1:interp_num) &
          * ( t_interp(1:interp_num) - t_data(j) ) / ( t_data(i) - t_data(j) )

      end if

    end do

  end do

  return

end subroutine LAGRANGE_VALUE_R4

!-------------------------------------------------------------------------------

function R4VEC_ASCENDS_STRICTLY ( n, x ) result (is_increasing)
!*******************************************************************************
!
! R4VEC_ASCENDS_STRICTLY determines if an R8VEC is strictly ascending.
!
!
! NOTE: This is the real kind 4 procedure. See R8.. for kind 8 procs.

!  Discussion:
!
!    An R8VEC is a vector of R4 values.
!
!    Notice the effect of entry number 6 in the following results:
!
!      X = ( -8.1, 1.3, 2.2, 3.4, 7.5, 7.4, 9.8 )
!      Y = ( -8.1, 1.3, 2.2, 3.4, 7.5, 7.5, 9.8 )
!      Z = ( -8.1, 1.3, 2.2, 3.4, 7.5, 7.6, 9.8 )
!
!      R8VEC_ASCENDS_STRICTLY ( X ) = FALSE
!      R8VEC_ASCENDS_STRICTLY ( Y ) = FALSE
!      R8VEC_ASCENDS_STRICTLY ( Z ) = TRUE
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 December 2007
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the size of the array.
!
!    Input, real ( kind = 4 ) X(N), the array to be examined.
!
!    Output, logical R8VEC_ASCENDS_STRICTLY, is TRUE if the
!    entries of X strictly ascend.
!
! Modified by Sergey Budaev
! Source : http://people.sc.fsu.edu/~jburkardt%20/f_src/interp/interp.html
!*******************************************************************************

  implicit none

  integer :: n

  integer :: i
  logical :: is_increasing
  real :: x(n)

  do i = 1, n - 1
    if ( x(i+1) <= x(i) ) then
      is_increasing = .false.
      return
    end if
  end do

  is_increasing = .true.

  return

end function R4VEC_ASCENDS_STRICTLY

!-------------------------------------------------------------------------------

subroutine R4VEC_BRACKET ( n, x, xval, left, right )
!*******************************************************************************
!
! R4VEC_BRACKET searches a sorted R8VEC for successive brackets of a value.
!
! NOTE: This is the real kind 4 procedure. See R8.. for kind 8 procs.
!
!  Discussion:
!
!    An R4VEC is an array of double precision real values.
!
!    If the values in the vector are thought of as defining intervals
!    on the real line, then this routine searches for the interval
!    nearest to or containing the given value.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 April 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, length of input array.
!
!    Input, real ( kind = 4 ) X(N), an array sorted into ascending order.
!
!    Input, real ( kind = 4 ) XVAL, a value to be bracketed.
!
!    Output, integer ( kind = 4 ) LEFT, RIGHT, the results of the search.
!    Either:
!      XVAL < X(1), when LEFT = 1, RIGHT = 2;
!      X(N) < XVAL, when LEFT = N-1, RIGHT = N;
!    or
!      X(LEFT) <= XVAL <= X(RIGHT).
!
! Source : http://people.sc.fsu.edu/~jburkardt%20/f_src/interp/interp.html
!*******************************************************************************

  implicit none

  integer n

  integer i
  integer left
  integer right
  real :: x(n)
  real :: xval

  do i = 2, n - 1

    if ( xval < x(i) ) then
      left = i - 1
      right = i
      return
    end if

   end do

  left = n - 1
  right = n

  return

end subroutine R4VEC_BRACKET

!-------------------------------------------------------------------------------

function LAGR_INTERPOL_VECTOR_R4 (xx, yy, xi) result (vector_output)
!*******************************************************************************
! LAGR_INTERPOL_VECTOR_R4: A vector (one-dimensional) wrapper to the
!      INTERP_LAGRANGE, Lagrange polynominal interpolation subroutine (default
!      real type version).
!
! CALL PARAMETERS: a vector for independent variable, vector for the
!         dependent variable, vector of the independent values X to
!         interpolate  to interpolate.
!
! NOTE: Unlike LINTERPOL this function results in extrapolation if the
!         independent variable X values are beyond the XX range.
!
!*******************************************************************************

real, dimension(:), intent(in) :: xx
real, dimension(:), intent(in) :: yy            ! (1,:)
real, dimension(:), intent(in) :: xi

real, dimension(size(xi)) :: vector_output      ! (1,:)

real, dimension(1,size(yy)) :: yy_here
real, dimension(1,size(xi)) :: vector_output_here

yy_here(1,:) = yy(:)

call INTERP_LAGRANGE ( xx, yy_here, xi, vector_output_here )

vector_output(:) = vector_output_here(1,:)

end function LAGR_INTERPOL_VECTOR_R4

!-------------------------------------------------------------------------------

function LIN_INTERPOL_VECTOR_R4 (xx, yy, xi) result (vector_output)
!*******************************************************************************
! LIN_INTERPOL_VECTOR_R4: A vector (one-dimensional) wrapper to the
!      INTERP_LINEAR, linear interpolation subroutine (default
!      real type version).
!
! CALL PARAMETERS: a vector for independent variable, vector for the
!         dependent variable, vector of the independent values X to
!         interpolate  to interpolate.
!
! NOTE: Unlike LINTERPOL this function results in extrapolation if the
!         independent variable X values are beyond the XX range.
!
!*******************************************************************************

real, dimension(:), intent(in) :: xx
real, dimension(:), intent(in) :: yy            ! (1,:)
real, dimension(:), intent(in) :: xi

real, dimension(size(xi)) :: vector_output      ! (1,:)

real, dimension(1,size(yy)) :: yy_here
real, dimension(1,size(xi)) :: vector_output_here

yy_here(1,:) = yy(:)

call INTERP_LINEAR ( xx, yy_here, xi, vector_output_here )

vector_output(:) = vector_output_here(1,:)

end function LIN_INTERPOL_VECTOR_R4

!-------------------------------------------------------------------------------

function LAGR_INTERPOL_VECTOR_R8 (xx, yy, xi) result (vector_output)
!*******************************************************************************
! LAGR_INTERPOL_VECTOR_R8: A vector (one-dimensional) wrapper to the
!      INTERP_LAGRANGE, Lagrange polynominal interpolation subroutine (kind 8
!      real type version).
!
! CALL PARAMETERS: a vector for independent variable, vector for the
!         dependent variable, vector of the independent values X to
!         interpolate  to interpolate.
!
! NOTE: Unlike LINTERPOL this function results in extrapolation if the
!         independent variable X values are beyond the XX range.
!
!*******************************************************************************

real(kind=8), dimension(:), intent(in) :: xx
real(kind=8), dimension(:), intent(in) :: yy            ! (1,:)
real(kind=8), dimension(:), intent(in) :: xi

real(kind=8), dimension(size(xi)) :: vector_output      ! (1,:)

real(kind=8), dimension(1,size(yy)) :: yy_here
real(kind=8), dimension(1,size(xi)) :: vector_output_here

yy_here(1,:) = yy(:)

call INTERP_LAGRANGE ( xx, yy_here, xi, vector_output_here )

vector_output(:) = vector_output_here(1,:)

end function LAGR_INTERPOL_VECTOR_R8

!-------------------------------------------------------------------------------

function LIN_INTERPOL_VECTOR_R8 (xx, yy, xi) result (vector_output)
!*******************************************************************************
! LIN_INTERPOL_VECTOR_R8: A vector (one-dimensional) wrapper to the
!      INTERP_LINEAR, linear interpolation subroutine (kind 8
!      real type version).
!
! CALL PARAMETERS: a vector for independent variable, vector for the
!         dependent variable, vector of the independent values X to
!         interpolate  to interpolate.
!
! NOTE: Unlike LINTERPOL this function results in extrapolation if the
!         independent variable X values are beyond the XX range.
!
!*******************************************************************************

real(kind=8), dimension(:), intent(in) :: xx
real(kind=8), dimension(:), intent(in) :: yy            ! (1,:)
real(kind=8), dimension(:), intent(in) :: xi

real(kind=8), dimension(size(xi)) :: vector_output      ! (1,:)

real(kind=8), dimension(1,size(yy)) :: yy_here
real(kind=8), dimension(1,size(xi)) :: vector_output_here

yy_here(1,:) = yy(:)

call INTERP_LINEAR ( xx, yy_here, xi, vector_output_here )

vector_output(:) = vector_output_here(1,:)

end function LIN_INTERPOL_VECTOR_R8


end module BASE_UTILS
