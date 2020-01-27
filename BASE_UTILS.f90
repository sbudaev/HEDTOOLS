!*******************************************************************************
! SVN $Id$
!*******************************************************************************
! BASE_UTILS:
! PURPOSE: This module provides basic high level utilities that are used in
!   Different models (and should be of general applicability).
!*******************************************************************************
module BASE_UTILS

implicit none

! Definition of the double (kind=8) and quadruple (kind=16) precision
integer, parameter, private :: SP = selected_real_kind(6,   37)
integer, parameter, private :: DP = selected_real_kind(15,  307)
integer, parameter, private :: QP = selected_real_kind(33, 4931)

! Qsort was inserted separately, so here are its own constants.
integer, parameter, private :: R4 = SP
integer, parameter, private :: R8 = DP

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

! Set here a flag and output parameter code for invalid calculations.
! real(kind=8), private, parameter ::  INVALID = -9999.0_8
integer, private, parameter :: INVALID = -9999

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
  module procedure STR_R16TOA
  module procedure STR_LTOA
  module procedure STR_ATOA

  module procedure STR_ARRAY_ITOA
  module procedure STR_ARRAY_RTOA
  module procedure STR_ARRAY_R8TOA
  module procedure STR_ARRAY_R16TOA
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
   module procedure LINTERPOL_R4_PURE  ! Note: These pure functions do not have
   module procedure LINTERPOL_R8_PURE  !       the output error parameter ierr.

end interface LINTERPOL

interface DDPINTERPOL            ! Generic interface to the Divided Difference
                                 ! Polynomials (DDP) interpolation functions.
   module procedure DDINT_R4
   module procedure DDINT_R8

end interface DDPINTERPOL

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

interface LINSPACE               ! Equally spaced linear array procedures.

   module procedure LINSPACE_R4
   module procedure LINSPACE_R8

end interface LINSPACE


interface ZEROFUN                ! Find zero of an arbitrary function

  module procedure zeroin_r4
  module procedure zeroin_r8

end interface ZEROFUN


interface ARRAY_QSORT            ! Quick sort module

  module procedure qsort_r4
  module procedure qsort_r8
  module procedure qsort_i

end interface ARRAY_QSORT


interface CSPLINE                ! Cubic spline interpolation
  module procedure spline_r4
end interface CSPLINE


!-------------------------------------------------------------------------------

private :: I4_WIDTH, I4_LOG_10  ! They are identical in CSV_IO and BASE_UTILS.
                                ! Private here to avoid possible name conflicts,
                                ! do we need them outside?

private :: LOG_DBG  ! This wrapper DEBUG LOG is used only for this module, it
                    ! may or may not use the module LOGGER, if not, it can be
                    ! used as a stand-alone module in other projects... But it
                    ! has the same name as in the model proto

private :: R8VEC_ASCENDS_STRICTLY, LIN_INTERPOL_VECTOR_R8

private :: qsort_r4, partition_r4, qsort_r8, partition_r8, qsort_i, partition_i

public  :: CSPLINE
private :: spline_r4, pchsp_r4, pchdf_r4, pchfe_r4, chfev_r4

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

!*******************************************************************************
! PURPOSE: Determines the platform type the program is running on.
! CALL PARAMETERS: none
! NOTES:   This function implements a rudimentary detection of the runtime
!          platform. It detects if the ComSpec environment variable is set,
!          which means that the platform is Windows.
! WARNING: The name of the environment variable appearss case-sensitive.
!          Microsoft might change the the behaviour of the ComSpec environment
!          variable, e.g. make it COMSPEC on different versions of Windows,
!          so correctness of this function is not guaranteed.
!*******************************************************************************
function PLATFORM_IS_WINDOWS() result (is_windows_flag)

  logical :: is_windows_flag

  character(len=*), parameter :: COMSPEC = "ComSpec"
  character(len=255) :: comspec_value_get

  call get_environment_variable(name=COMSPEC, value=comspec_value_get)

  if (len_trim(comspec_value_get)==0) then
    is_windows_flag = .FALSE.
  else
    is_windows_flag = .TRUE.
  end if

end function PLATFORM_IS_WINDOWS

!-------------------------------------------------------------------------------

pure function STR_ITOA(i, formatstr) result (ToStrA)
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

pure function STR_RTOA(r,formatstr) result (ToStrA)
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
  real(kind=SP), intent(in) :: r
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

pure function STR_R8TOA(r,formatstr) result (ToStrA)
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
  real (kind=DP), intent(in) :: r
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

pure function STR_R16TOA(r,formatstr) result (ToStrA)
!*******************************************************************************
! PURPOSE: Convert REAL to a string type.
! CALL PARAMETERS: single double precision (kind 16) value
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
  real (kind=QP), intent(in) :: r
  character(len=*), optional, intent(in) :: formatstr

  ! Local variables
  character(len=68) :: tmpStr ! quick and dirty, with allowance for a big float
  character(len=:), allocatable :: tmpFormat

  ! Subroutine name for DEBUG LOGGER
  character (len=*), parameter :: PROCNAME = "STR_R16TOA"

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

end function STR_R16TOA

!-------------------------------------------------------------------------------

pure function STR_LTOA (L) result (ToStrA)

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
  logical, intent(in) :: L

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

pure function STR_ATOA (A) result (ToStrA)

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
  character(len=*), intent(in) :: A

  ! Subroutine name for DEBUG LOGGER
  character (len=*), parameter :: PROCNAME = "STR_ATOA"

  !-----------------------------------------------------------------------------

  ToStrA = A

end function STR_ATOA

!-------------------------------------------------------------------------------

pure function STR_ARRAY_ITOA (r,formatstr) result(ToStrA)
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

pure function STR_ARRAY_RTOA (r,formatstr) result(ToStrA)
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
  real(kind=SP), dimension(:), intent(in) :: r
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

pure function STR_ARRAY_R8TOA (r,formatstr) result(ToStrA)
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
  real (kind=DP), dimension(:), intent(in) :: r
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

pure function STR_ARRAY_R16TOA (r,formatstr) result(ToStrA)
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
  real (kind=QP), dimension(:), intent(in) :: r
  character(len=*), optional, intent(in) :: formatstr

  ! Local variables
  character (len=:), allocatable :: tmpStr
  integer :: i

  ! Subroutine name for DEBUG LOGGER
  character (len=*), parameter :: PROCNAME = "STR_ARRAY_R16TOA"

  !-------------------------------------------------------
  tmpStr=""

  do i=lbound(r,1), ubound(r,1)
    if (present(formatstr)) then
      tmpStr=tmpStr // " " // STR_R16TOA(r(i), formatstr)
    else
      tmpStr = tmpStr // " " // STR_R16TOA(r(i))
    end if
  end do

  ToStrA = tmpStr

end function STR_ARRAY_R16TOA

!-------------------------------------------------------------------------------

pure function STR_ARRAY_LTOA (r) result(ToStrA)
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

pure function STR_ARRAY_ATOA (r, delimiter) result(ToStrA)
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

pure function CLEANUP(instring) result (cleaned)
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

pure function STR_ITOA_LZ(i, maxi) result (ToStrA)
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

pure function I4_WIDTH (i) result (i4width)
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
  integer, intent(in) :: i

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

pure function I4_LOG_10 (i) result(i4log10)
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
  integer, intent(in) :: i

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

pure subroutine MRGRNK_R4 (xdont, irngt)
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
      real(kind=SP), dimension (:), intent (in) :: xdont
      integer, dimension (:), intent (out) :: irngt
! __________________________________________________________
      real(kind=SP) :: xvala, xvalb
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

pure subroutine MRGRNK_R8 (xdont, irngt)
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
      real (kind=DP), dimension (:), intent (in) :: xdont
      integer, dimension (:), intent (out) :: irngt
! __________________________________________________________
      real (kind=DP) :: xvala, xvalb
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

pure subroutine MRGRNK_I (xdont, irngt)
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

pure subroutine ARRAY_RANK(Indx, Ranks)
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

pure subroutine RNKPAR_R4 (xdont, irngt, nord)
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
      real(kind=SP), dimension (:), intent (in) :: xdont
      integer, dimension (:), intent (out) :: irngt
      integer, intent (in) :: nord
! __________________________________________________________
      real(kind=SP)    :: xpiv, xpiv0, xwrk, xwrk1, xmin, xmax
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
      xpiv = xdont (ilowt(ideb)) + real(2*nord,SP)/real(ndon+nord,SP) * &
                                   (xdont(ihigt(3))-xdont(ilowt(ideb)))
      if (xpiv >= xdont(ihigt(1))) then
         xpiv = xdont (ilowt(ideb)) + real(2*nord,SP)/real(ndon+nord,SP) * &
                                      (xdont(ihigt(2))-xdont(ilowt(ideb)))
         if (xpiv >= xdont(ihigt(1))) &
             xpiv = xdont (ilowt(ideb))+real(2*nord,SP)/real (ndon+nord,SP) * &
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
               xpiv = xdont (iwrk1) + real (nwrk,SP) / real (nord+nwrk,SP) * &
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
            xpiv = xdont (ilowt(1)) + real(nord,SP)/real(jlow+nord,SP) * &
                                      (xdont(ilowt(ifin))-xdont(ilowt(1)))
            if (jdeb > 0) then
               if (xpiv <= xpiv0) &
                   xpiv = xpiv0 + real(2*nord-jdeb,SP)/real (jlow+nord,SP) * &
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

pure subroutine RNKPAR_R8 (xdont, irngt, nord)
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
      real (kind=DP), dimension (:), intent (in) :: xdont
      integer, dimension (:), intent (out) :: irngt
      integer, intent (in) :: nord
! __________________________________________________________
      real (kind=DP) :: xpiv, xpiv0, xwrk, xwrk1, xmin, xmax
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
      xpiv = xdont (ilowt(ideb)) + real(2*nord,DP)/real(ndon+nord,DP) * &
                                   (xdont(ihigt(3))-xdont(ilowt(ideb)))
      if (xpiv >= xdont(ihigt(1))) then
         xpiv = xdont (ilowt(ideb)) + real(2*nord,DP)/real(ndon+nord,DP) * &
                                      (xdont(ihigt(2))-xdont(ilowt(ideb)))
         if (xpiv >= xdont(ihigt(1))) &
             xpiv = xdont (ilowt(ideb))+real(2*nord,DP)/real(ndon+nord,DP) * &
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
               xpiv = xdont (iwrk1) + real (nwrk,DP) / real (nord+nwrk,DP) * &
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
            xpiv = xdont (ilowt(1)) + real(nord,DP)/real(jlow+nord,DP) * &
                                      (xdont(ilowt(ifin))-xdont(ilowt(1)))
            if (jdeb > 0) then
               if (xpiv <= xpiv0) &
                   xpiv = xpiv0 + real(2*nord-jdeb,DP)/real (jlow+nord,DP) * &
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

pure subroutine RNKPAR_I (xdont, irngt, nord)
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
      xpiv = xdont (ilowt(ideb)) + real(2*nord,SP)/real(ndon+nord,SP) * &
                                   (xdont(ihigt(3))-xdont(ilowt(ideb)))
      if (xpiv >= xdont(ihigt(1))) then
         xpiv = xdont (ilowt(ideb)) + real(2*nord,SP)/real(ndon+nord,SP) * &
                                      (xdont(ihigt(2))-xdont(ilowt(ideb)))
         if (xpiv >= xdont(ihigt(1))) &
             xpiv = xdont (ilowt(ideb))+real(2*nord,SP)/real(ndon+nord,SP) * &
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
               xpiv = xdont (iwrk1) + real (nwrk,SP) / real (nord+nwrk,SP) * &
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
            xpiv = xdont (ilowt(1)) + real(nord,SP)/real(jlow+nord,SP) * &
                                      (xdont(ilowt(ifin))-xdont(ilowt(1)))
            if (jdeb > 0) then
               if (xpiv <= xpiv0) &
                   xpiv = xpiv0 + real(2*nord-jdeb,SP)/real (jlow+nord,SP) * &
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

   real(kind=SP), dimension(:), intent(in) :: xx, yy  ! Indep. and dep. arrays.
   real(kind=SP), intent(in) :: x                     ! Interpolate at x
   integer, intent(out) :: ierr              ! Returned error code.

   real(kind=SP) :: y                        ! Interpolated value y(x).

   integer :: nn                             ! dimension of xx and yy arrays

   integer :: i                              ! Local counter.
   integer :: ierr_here                      ! Local copy of ierr.

   ! Check if the input vector is strictly increasing.
   if ( .not. R4VEC_ASCENDS_STRICTLY ( size(xx), xx ) ) then
      ierr=-9999
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

   ierr = ierr_here

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

   real(kind=DP), dimension(:), intent(in) :: xx ! Indep. variable array.
   real(kind=DP), dimension(:), intent(in) :: yy ! Dep. variable array.
   real(kind=DP), intent(in) :: x             ! Interpolate at x
   integer, intent(out) :: ierr              ! Returned error code.

   real(kind=DP) :: y                         ! Interpolated value y(x).

   integer :: nn                             ! dimension of xx and yy arrays

   integer :: i                              ! Local counter.
   integer :: ierr_here                      ! Local copy of ierr.

   ! Check if the input vector is strictly increasing.
   if ( .not. R8VEC_ASCENDS_STRICTLY ( size(xx), xx ) ) then
      ierr=-9999
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

   ierr = ierr_here

end function LINTERPOL_R8

!-------------------------------------------------------------------------------

pure function LINTERPOL_R4_PURE (xx, yy, x) result (y)
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

   real(kind=SP), dimension(:), intent(in) :: xx, yy  ! Indep. and dep. arrays.
   real(kind=SP), intent(in) :: x                     ! Interpolate at x
   !integer, optional, intent(out) :: ierr    ! Returned error code.

   real(kind=SP) :: y                        ! Interpolated value y(x).

   integer :: nn                             ! dimension of xx and yy arrays

   integer :: i                              ! Local counter.
   integer :: ierr_here                      ! Local copy of ierr.

   ! Check if the input vector is strictly increasing.
   if ( .not. R4VEC_ASCENDS_STRICTLY ( size(xx), xx ) ) then
      !if (present(ierr)) ierr=-9999
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

   !if (present(ierr)) ierr = ierr_here

end function LINTERPOL_R4_PURE

!-------------------------------------------------------------------------------

pure function LINTERPOL_R8_PURE (xx, yy, x) result (y)
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

   real(kind=DP), dimension(:), intent(in) :: xx ! Indep. variable array.
   real(kind=DP), dimension(:), intent(in) :: yy ! Dep. variable array.
   real(kind=DP), intent(in) :: x             ! Interpolate at x
   !integer, optional, intent(out) :: ierr    ! Returned error code.

   real(kind=DP) :: y                         ! Interpolated value y(x).

   integer :: nn                             ! dimension of xx and yy arrays

   integer :: i                              ! Local counter.
   integer :: ierr_here                      ! Local copy of ierr.

   ! Check if the input vector is strictly increasing.
   if ( .not. R8VEC_ASCENDS_STRICTLY ( size(xx), xx ) ) then
      !if (present(ierr)) ierr=-9999
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

   !if (present(ierr)) ierr = ierr_here

end function LINTERPOL_R8_PURE

!-------------------------------------------------------------------------------

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! The procedures below are taken from the INTERP library by John Burkardt
! http://people.sc.fsu.edu/~jburkardt%20/f_src/interp/interp.html
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

pure subroutine INTERP_LAGRANGE_R8 ( t_data, p_data, t_interp, p_interp )
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
!
! W A R N I N G:
! Lagrange interpolation is susceptible to Runge's phenomenon, and changing
! the interpolation points requires recalculating the entire interpolant.
!
!*******************************************************************************

  implicit none

  real ( kind = DP ), intent(in) :: p_data(:,:)
  real ( kind = DP ), intent(out) :: p_interp(:,:)
  real ( kind = DP ), intent(in) :: t_data(:)
  real ( kind = DP ), intent(in) :: t_interp(:)

  integer :: interp_num
  integer :: data_num
  integer :: m

  real ( kind = DP ) l_interp( size(p_data, 2),size(p_interp, 2) )

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

pure subroutine INTERP_LINEAR_R8 ( t_data, p_data, t_interp, p_interp, error_code )
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
  real ( kind = DP ), intent(in) :: p_data(:,:)
  real ( kind = DP ), intent(out) ::  p_interp(:,:)
  logical, optional, intent(out) :: error_code  !> Error code if not increasing.

  integer :: interp_num
  integer :: data_num
  integer :: m

  integer right
  real ( kind = DP ) t
  real ( kind = DP ), intent(in) :: t_data(:)
  real ( kind = DP ), intent(in) :: t_interp(:)

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

pure subroutine LAGRANGE_VALUE_R8 ( data_num, t_data, interp_num, t_interp, l_interp )
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

  integer, intent(in) :: data_num
  integer, intent(in) :: interp_num

  integer i
  integer j
  real ( kind = DP ), intent(out) :: l_interp(data_num,interp_num)
  real ( kind = DP ), intent(in) :: t_data(data_num)
  real ( kind = DP ), intent(in) :: t_interp(interp_num)
!
!  Evaluate the polynomial.
!
  l_interp(1:data_num,1:interp_num) = 1.0E+00_DP

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

pure function R8VEC_ASCENDS_STRICTLY ( n, x ) result (is_increasing)
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

  integer, intent(in) :: n

  integer i
  logical is_increasing
  real ( kind = DP ), intent(in) :: x(n)

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

pure subroutine R8VEC_BRACKET ( n, x, xval, left, right )
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

  integer, intent(in) :: n

  integer i
  integer, intent(out) :: left
  integer, intent(out) :: right
  real ( kind = DP ), intent(in) :: x(n)
  real ( kind = DP ), intent(in) :: xval

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

pure subroutine INTERP_LAGRANGE_R4 ( t_data, p_data, t_interp, p_interp )
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

  real(kind=SP), intent(in) :: p_data(:,:)
  real(kind=SP), intent(out) :: p_interp(:,:)
  real(kind=SP), intent(in) :: t_data(:)
  real(kind=SP), intent(in) :: t_interp(:)

  integer :: interp_num
  integer :: data_num
  integer :: m

  real(kind=SP) :: l_interp( size(p_data, 2),size(p_interp, 2) )

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

pure subroutine INTERP_LINEAR_R4 ( t_data, p_data, t_interp, p_interp, error_code )
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
  real(kind=SP), intent(in) :: p_data(:,:)
  real(kind=SP), intent(out) ::  p_interp(:,:)
  logical, optional, intent(out) :: error_code !> Error code if not increasing.

  integer :: interp_num
  integer :: data_num
  integer :: m

  integer right
  real(kind=SP) :: t
  real(kind=SP), intent(in) :: t_data(:)
  real(kind=SP), intent(in) :: t_interp(:)

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

pure subroutine LAGRANGE_VALUE_R4 ( data_num, t_data, interp_num, t_interp, l_interp )
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

  integer, intent(in) :: data_num
  integer, intent(in) :: interp_num

  integer :: i
  integer :: j
  real(kind=SP), intent(out) :: l_interp(data_num,interp_num)
  real(kind=SP), intent(in) :: t_data(data_num)
  real(kind=SP), intent(in) :: t_interp(interp_num)
!
!  Evaluate the polynomial.
!
  l_interp(1:data_num,1:interp_num) = 1.0E+00_SP

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

pure function R4VEC_ASCENDS_STRICTLY ( n, x ) result (is_increasing)
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

  integer, intent(in) :: n

  integer :: i
  logical :: is_increasing
  real(kind=SP), intent(in) :: x(n)

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

pure subroutine R4VEC_BRACKET ( n, x, xval, left, right )
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

  integer, intent(in) :: n

  integer i
  integer, intent(out) :: left
  integer, intent(out) :: right
  real(kind=SP), intent(in) :: x(n)
  real(kind=SP), intent(in) :: xval

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

pure function LAGR_INTERPOL_VECTOR_R4 (xx, yy, xi) result (vector_output)
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
! W A R N I N G:
! Lagrange interpolation is susceptible to Runge's phenomenon, and changing
! the interpolation points requires recalculating the entire interpolant.
!
!*******************************************************************************

  real(kind=SP), dimension(:), intent(in) :: xx
  real(kind=SP), dimension(:), intent(in) :: yy            ! (1,:)
  real(kind=SP), dimension(:), intent(in) :: xi

  real(kind=SP), dimension(size(xi)) :: vector_output      ! (1,:)

  real(kind=SP), dimension(1,size(yy)) :: yy_here
  real(kind=SP), dimension(1,size(xi)) :: vector_output_here

  yy_here(1,:) = yy(:)

  call INTERP_LAGRANGE ( xx, yy_here, xi, vector_output_here )

  vector_output(:) = vector_output_here(1,:)

end function LAGR_INTERPOL_VECTOR_R4

!-------------------------------------------------------------------------------

pure function LIN_INTERPOL_VECTOR_R4 (xx, yy, xi) result (vector_output)
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

  real(kind=SP), dimension(:), intent(in) :: xx
  real(kind=SP), dimension(:), intent(in) :: yy            ! (1,:)
  real(kind=SP), dimension(:), intent(in) :: xi

  real(kind=SP), dimension(size(xi)) :: vector_output      ! (1,:)

  real(kind=SP), dimension(1,size(yy)) :: yy_here
  real(kind=SP), dimension(1,size(xi)) :: vector_output_here

  yy_here(1,:) = yy(:)

  call INTERP_LINEAR ( xx, yy_here, xi, vector_output_here )

  vector_output(:) = vector_output_here(1,:)

end function LIN_INTERPOL_VECTOR_R4

!-------------------------------------------------------------------------------

pure function LAGR_INTERPOL_VECTOR_R8 (xx, yy, xi) result (vector_output)
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
! W A R N I N G:
! Lagrange interpolation is susceptible to Runge's phenomenon, and changing
! the interpolation points requires recalculating the entire interpolant.
!
!*******************************************************************************

  real(kind=DP), dimension(:), intent(in) :: xx
  real(kind=DP), dimension(:), intent(in) :: yy            ! (1,:)
  real(kind=DP), dimension(:), intent(in) :: xi

  real(kind=DP), dimension(size(xi)) :: vector_output      ! (1,:)

  real(kind=DP), dimension(1,size(yy)) :: yy_here
  real(kind=DP), dimension(1,size(xi)) :: vector_output_here

  yy_here(1,:) = yy(:)

  call INTERP_LAGRANGE ( xx, yy_here, xi, vector_output_here )

  vector_output(:) = vector_output_here(1,:)

end function LAGR_INTERPOL_VECTOR_R8

!-------------------------------------------------------------------------------

pure function LIN_INTERPOL_VECTOR_R8 (xx, yy, xi) result (vector_output)
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

  real(kind=DP), dimension(:), intent(in) :: xx
  real(kind=DP), dimension(:), intent(in) :: yy            ! (1,:)
  real(kind=DP), dimension(:), intent(in) :: xi

  real(kind=DP), dimension(size(xi)) :: vector_output      ! (1,:)

  real(kind=DP), dimension(1,size(yy)) :: yy_here
  real(kind=DP), dimension(1,size(xi)) :: vector_output_here

  yy_here(1,:) = yy(:)

  call INTERP_LINEAR ( xx, yy_here, xi, vector_output_here )

  vector_output(:) = vector_output_here(1,:)

end function LIN_INTERPOL_VECTOR_R8

!-------------------------------------------------------------------------------

pure function DDINT_R4 (xi, yi, xx, n_points) result (ddint_out)
!*******************************************************************************
! Interpolation based on Divided Difference Polynomials:
! Alex G: January 2010
!-------------------------------------------------------------------------------
! input ...
! xx    - the abscissa at which the interpolation is to be evaluated
! xi()  - the arrays of data abscissas
! yi()  - the arrays of data ordinates
! ni    - size of the arrays xi() and yi()
! n     - number of points for interpolation (order of interp. = n-1)
! output ...
! ddint   - interpolated value
! comments ...
! if (n > ni) n = ni
! program works for both equally and unequally spaced xi()
!
! Modified by Sergey Budaev
! Source : http://ww2.odu.edu/~agodunov/computing/programs/book2/Ch01/ddint.f90
!
!*******************************************************************************

  implicit none

  real(kind=SP) :: ddint_out
  real(kind=SP), intent(in) :: xx

  integer, intent(in), optional :: n_points
  real(kind=SP), intent(in) :: xi(:), yi(:)

  integer :: ni, n
  real(kind=SP), allocatable, dimension(:,:) :: d
  real(kind=SP), allocatable, dimension(:)   :: x
  integer :: i, j, k, ix
  real(kind=SP) :: c, pn

  ni = min(size(xi),size(yi)) ! Check conformant xi and yi.

  ! default interpolation order is the input array size.
  if (present(n_points)) then
     ! check order of interpolation
     if (n_points > ni) then
        n = ni
     else
        n = n_points
     end if
  else
     n = ni
  end if

  allocate (d(n,n))
  allocate (x(n))

  ! if x is ouside the xi(1)-xi(ni) interval take a boundary value
  if (xx <= xi(1)) then
    ddint_out = yi(1)
    return
  end if
  if (xx >= xi(ni)) then
    ddint_out = yi(ni)
    return
  end if

  ! a binary (bisectional) search to find i so that xi(i) < x < xi(i+1)
  i = 1
  j = ni
  do while (j > i+1)
    k = (i+j)/2
    if (xx < xi(k)) then
       j = k
    else
       i = k
    end if
  end do

  ! shift i that will correspond to n-th order of interpolation
  ! the search point will be in the middle in x_i, x_i+1, x_i+2 ...
  i = i + 1 - n/2

  ! check boundaries: if i is ouside of the range [1, ... n] -> shift i
  if (i < 1) i=1
  if (i + n > ni) i=ni-n+1

  !  old output to test i
  !  write(*,100) xx, i
  !  100 format (f10.5, I5)

  ! just wanted to use index i later for d coefficients
  ix = i

  ! initialization of d(n,1) and x(n)
  do i=1,n
    d(i,1) = yi(ix+i-1)
    x(i)   = xi(ix+i-1)
  end do

  ! calculations for the divided difference coefficients
  do j=2,n
    do i=1,n-j+1
      d(i,j)=(d(i+1,j-1)-d(i,j-1))/(x(i+1+j-2)-x(i))
    end do
  end do

  ! print results for the d coeff.
  !  do i=1,n
  !    write(*,200) (d(i,j),j=1,n-i+1)
  !  end do
  !200 format (4f10.6)

  ! divided difference interpolation
  Pn = d(1,1)
  do i=1,n-1
    c = 1.0_SP
    do j=1,i
      c = c*(xx - x(j))
    end do
    Pn = Pn + c*d(1,i+1);
  end do

  deallocate (d)
  deallocate (x)

  ddint_out = Pn

end function DDINT_R4

!-------------------------------------------------------------------------------

pure function DDINT_R8 (xi, yi, xx, n_points) result (ddint_out)
!*******************************************************************************
! Interpolation based on Divided Difference Polynomials:
! Alex G: January 2010
!-------------------------------------------------------------------------------
! input ...
! xx    - the abscissa at which the interpolation is to be evaluated
! xi()  - the arrays of data abscissas
! yi()  - the arrays of data ordinates
! ni    - size of the arrays xi() and yi()
! n     - number of points for interpolation (order of interp. = n-1)
! output ...
! ddint   - interpolated value
! comments ...
! if (n > ni) n = ni
! program works for both equally and unequally spaced xi()
!
! Modified by Sergey Budaev
! Source : http://ww2.odu.edu/~agodunov/computing/programs/book2/Ch01/ddint.f90
!
!*******************************************************************************

  implicit none

  real (kind=DP) :: ddint_out
  real (kind=DP), intent(in) :: xx

  integer, intent(in), optional :: n_points
  real (kind=DP), intent(in) :: xi(:), yi(:)

  integer :: ni, n
  real (kind=DP), allocatable, dimension(:,:) :: d
  real (kind=DP), allocatable, dimension(:)   :: x
  integer :: i, j, k, ix
  real (kind=DP) :: c, pn

  ni = min(size(xi),size(yi)) ! Check conformant xi and yi.

  ! default interpolation order is the input array size.
  if (present(n_points)) then
     ! check order of interpolation
     if (n_points > ni) then
        n = ni
     else
        n = n_points
     end if
  else
     n = ni
  end if

  allocate (d(n,n))
  allocate (x(n))

  ! if x is ouside the xi(1)-xi(ni) interval take a boundary value
  if (xx <= xi(1)) then
    ddint_out = yi(1)
    return
  end if
  if (xx >= xi(ni)) then
    ddint_out = yi(ni)
    return
  end if

  ! a binary (bisectional) search to find i so that xi(i) < x < xi(i+1)
  i = 1
  j = ni
  do while (j > i+1)
    k = (i+j)/2
    if (xx < xi(k)) then
       j = k
    else
       i = k
    end if
  end do

  ! shift i that will correspond to n-th order of interpolation
  ! the search point will be in the middle in x_i, x_i+1, x_i+2 ...
  i = i + 1 - n/2

  ! check boundaries: if i is ouside of the range [1, ... n] -> shift i
  if (i < 1) i=1
  if (i + n > ni) i=ni-n+1

  !  old output to test i
  !  write(*,100) xx, i
  !  100 format (f10.5, I5)

  ! just wanted to use index i later for d coefficients
  ix = i

  ! initialization of d(n,1) and x(n)
  do i=1,n
    d(i,1) = yi(ix+i-1)
    x(i)   = xi(ix+i-1)
  end do

  ! calculations for the divided difference coefficients
  do j=2,n
    do i=1,n-j+1
      d(i,j)=(d(i+1,j-1)-d(i,j-1))/(x(i+1+j-2)-x(i))
    end do
  end do

  ! print results for the d coeff.
  !  do i=1,n
  !    write(*,200) (d(i,j),j=1,n-i+1)
  !  end do
  !200 format (4f10.6)

  ! divided difference interpolation
  Pn = d(1,1)
  do i=1,n-1
    c = 1.0_DP
    do j=1,i
      c = c*(xx - x(j))
    end do
    Pn = Pn + c*d(1,i+1);
  end do

  deallocate (d)
  deallocate (x)

  ddint_out = Pn

end function DDINT_R8

!-------------------------------------------------------------------------------

function LINSPACE_R4 (x_min, x_max_n, n) result (value_out)
!*******************************************************************************
! PURPOSE: Calculate an array of values equally spaced in the linear space.
! CALL PARAMETERS: minimum value, maximum value, number
! EXAMPLE:
!          linspace(1., 10., 10)
!*******************************************************************************

  real(kind=SP) :: x_min, x_max_n
  integer :: n
  integer:: i

  real(kind=SP), allocatable, dimension(:) :: value_out

  allocate ( value_out(n) )

  do i = 1, n
    value_out(i) = x_min+(((x_max_n-x_min)*(real(i-1,SP)))/(real(n-1,SP)))
  end do

end function LINSPACE_R4

!-------------------------------------------------------------------------------

function LINSPACE_R8 (x_min, x_max_n, n) result (value_out)
!*******************************************************************************
! PURPOSE: Calculate an array of values equally spaced in the linear space.
! CALL PARAMETERS: minimum value, maximum value, number
! EXAMPLE:
!          linspace(1., 10., 10)
!*******************************************************************************

  real(kind=DP) :: x_min, x_max_n
  integer :: n
  integer:: i

  real(kind=DP), allocatable, dimension(:) :: value_out

  allocate ( value_out(n) )

  do i = 1, n
    value_out(i) = x_min + (((x_max_n-x_min)*(real(i-1,DP)))/(real(n-1, DP)))
  end do

end function LINSPACE_R8

!-------------------------------------------------------------------------------

!-----------------------------------------------------------------------------
!> This function calculates a zero of a function f(x) in the interval
!! (ax,bx).
!! - Brent, R.P., (1973). Algorithms for Minimization Without
!!   Derivatives, Prentice-Hall, Inc.
!! - Brent, R.P. (1971). An algorithm with guaranteed convergence for
!!   finding a zero of a function, Computer J.  14, 422–425.
!! .
!! Author: Richard Brent, https://maths-people.anu.edu.au/~brent/
!! Source: http://www.netlib.org/go/
!! With some minor changes by Sergey Budaev.
!> @param[in] ax left endpoint of initial interval
!> @param[in] bx right endpoint of initial interval
!> @param[in] f  function subprogram which evaluates f(x) for any x in
!!            the interval (ax,bx).
!> @param[in] tol desired length of the interval of uncertainty of the
!!            final result (.ge.0.)
!! @returns   Abscissa approximating a zero of f(x) in the
!!            interval (ax,bx). Note that this function returns
!!            commondata::missing if f(ax) and f(bx) do not have
!!            different signs (so there is no function zero within the
!!            range).
real(SP) function zeroin_r4(ax,bx,f,tol)

    real(SP), intent(in) :: ax,bx,tol
    real(SP) :: f
  !
  !      a zero of the function  f(x)  is computed in the interval ax,bx .
  !
  !  input..
  !
  !  ax     left endpoint of initial interval
  !  bx     right endpoint of initial interval
  !  f      function subprogram which evaluates f(x) for any x in
  !         the interval  ax,bx
  !  tol    desired length of the interval of uncertainty of the
  !         final result (.ge.0.)
  !
  !  output..
  !
  !  zeroin abscissa approximating a zero of  f  in the interval ax,bx
  !
  !      it is assumed  that   f(ax)   and   f(bx)   have  opposite  signs
  !  this is checked, and an error message is printed if this is not
  !  satisfied.   zeroin  returns a zero  x  in the given interval
  !  ax,bx  to within a tolerance  4*macheps*abs(x)+tol, where macheps  is
  !  the  relative machine precision defined as the smallest representable
  !  number such that  1.+macheps .gt. 1.
  !      this function subprogram is a slightly  modified  translation  of
  !  the algol 60 procedure  zero  given in  Richard Brent, Algorithms for
  !  Minimization Without Derivatives, Prentice-Hall, Inc. (1973).
  !
    real(SP) ::  a,b,c,d,e,eps,fa,fb,fc,tol1,xm,p,q,r,s
    real(SP), parameter :: INVALID=-9999.0_SP
    eps = epsilon(1.0_SP)
    tol1 = eps+1.0_SP

    a=ax
    b=bx
    fa=f(a)
    fb=f(b)
  !     check that f(ax) and f(bx) have different signs
    if (fa .eq.0.0_SP .or. fb .eq. 0.0_SP) go to 20
    if (fa * (fb/abs(fb)) .le. 0.0_SP) go to 20
  !        write(6,2500)
  !2500    format(1x,'f(ax) and f(bx) do not have different signs, aborting')
       zeroin_r4 = INVALID
       return
 20 c=a
    fc=fa
    d=b-a
    e=d
 30 if (abs(fc).ge.abs(fb)) go to 40
    a=b
    b=c
    c=a
    fa=fb
    fb=fc
    fc=fa
 40 tol1=2.0_SP*eps*abs(b)+0.5_SP*tol
    xm = 0.5_SP*(c-b)
    if ((abs(xm).le.tol1).or.(fb.eq.0.0_SP)) go to 150
  !
  ! see if a bisection is forced
  !
    if ((abs(e).ge.tol1).and.(abs(fa).gt.abs(fb))) go to 50
    d=xm
    e=d
    go to 110
 50 s=fb/fa
    if (a.ne.c) go to 60
  !
  ! linear interpolation
  !
    p=2.0_SP*xm*s
    q=1.0_SP-s
    go to 70
  !
  ! inverse quadratic interpolation
  !
 60 q=fa/fc
    r=fb/fc
    p=s*(2.0_SP*xm*q*(q-r)-(b-a)*(r-1.0_SP))
    q=(q-1.0_SP)*(r-1.0_SP)*(s-1.0_SP)
 70 if (p.le.0.0_SP) go to 80
    q=-q
    go to 90
 80 p=-p
 90 s=e
    e=d
    if (((2.0_SP*p).ge.(3.0_SP*xm*q-abs(tol1*q))) .or.                &
        (p.ge. abs(0.5d0*s*q))) go to 100
    d=p/q
    go to 110
100 d=xm
    e=d
110 a=b
    fa=fb
    if (abs(d).le.tol1) go to 120
    b=b+d
    go to 140
120 if (xm.le.0.0_SP) go to 130
    b=b+tol1
    go to 140
130 b=b-tol1
140 fb=f(b)
    if ((fb*(fc/abs(fc))).gt.0.0_SP) go to 20
    go to 30
150 zeroin_r4=b
    return
end function zeroin_r4

!-----------------------------------------------------------------------------
!> This function calculates a zero of a function f(x) in the interval
!! (ax,bx).
!! - Brent, R.P., (1973). Algorithms for Minimization Without
!!   Derivatives, Prentice-Hall, Inc.
!! - Brent, R.P. (1971). An algorithm with guaranteed convergence for
!!   finding a zero of a function, Computer J.  14, 422–425.
!! .
!! Author: Richard Brent, https://maths-people.anu.edu.au/~brent/
!! Source: http://www.netlib.org/go/
!! With some minor changes by Sergey Budaev.
!> @param[in] ax left endpoint of initial interval
!> @param[in] bx right endpoint of initial interval
!> @param[in] f  function subprogram which evaluates f(x) for any x in
!!            the interval (ax,bx).
!> @param[in] tol desired length of the interval of uncertainty of the
!!            final result (.ge.0.)
!! @returns   Abscissa approximating a zero of f(x) in the
!!            interval (ax,bx). Note that this function returns
!!            commondata::missing if f(ax) and f(bx) do not have
!!            different signs (so there is no function zero within the
!!            range).
real(DP) function zeroin_r8(ax,bx,f,tol)

    real(DP), intent(in) :: ax,bx,tol
    real(DP) :: f
  !
  !      a zero of the function  f(x)  is computed in the interval ax,bx .
  !
  !  input..
  !
  !  ax     left endpoint of initial interval
  !  bx     right endpoint of initial interval
  !  f      function subprogram which evaluates f(x) for any x in
  !         the interval  ax,bx
  !  tol    desired length of the interval of uncertainty of the
  !         final result (.ge.0.)
  !
  !  output..
  !
  !  zeroin abscissa approximating a zero of  f  in the interval ax,bx
  !
  !      it is assumed  that   f(ax)   and   f(bx)   have  opposite  signs
  !  this is checked, and an error message is printed if this is not
  !  satisfied.   zeroin  returns a zero  x  in the given interval
  !  ax,bx  to within a tolerance  4*macheps*abs(x)+tol, where macheps  is
  !  the  relative machine precision defined as the smallest representable
  !  number such that  1.+macheps .gt. 1.
  !      this function subprogram is a slightly  modified  translation  of
  !  the algol 60 procedure  zero  given in  Richard Brent, Algorithms for
  !  Minimization Without Derivatives, Prentice-Hall, Inc. (1973).
  !
    real(DP) ::  a,b,c,d,e,eps,fa,fb,fc,tol1,xm,p,q,r,s
    real(DP), parameter :: INVALID=-9999.0_DP
    eps = epsilon(1.0_DP)
    tol1 = eps+1.0_DP

    a=ax
    b=bx
    fa=f(a)
    fb=f(b)
  !     check that f(ax) and f(bx) have different signs
    if (fa .eq.0.0_DP .or. fb .eq. 0.0_DP) go to 20
    if (fa * (fb/abs(fb)) .le. 0.0_DP) go to 20
  !        write(6,2500)
  !2500    format(1x,'f(ax) and f(bx) do not have different signs, aborting')
       zeroin_r8 = INVALID
       return
 20 c=a
    fc=fa
    d=b-a
    e=d
 30 if (abs(fc).ge.abs(fb)) go to 40
    a=b
    b=c
    c=a
    fa=fb
    fb=fc
    fc=fa
 40 tol1=2.0_DP*eps*abs(b)+0.5_DP*tol
    xm = 0.5_DP*(c-b)
    if ((abs(xm).le.tol1).or.(fb.eq.0.0_DP)) go to 150
  !
  ! see if a bisection is forced
  !
    if ((abs(e).ge.tol1).and.(abs(fa).gt.abs(fb))) go to 50
    d=xm
    e=d
    go to 110
 50 s=fb/fa
    if (a.ne.c) go to 60
  !
  ! linear interpolation
  !
    p=2.0_DP*xm*s
    q=1.0_DP-s
    go to 70
  !
  ! inverse quadratic interpolation
  !
 60 q=fa/fc
    r=fb/fc
    p=s*(2.0_DP*xm*q*(q-r)-(b-a)*(r-1.0_DP))
    q=(q-1.0_DP)*(r-1.0_DP)*(s-1.0_DP)
 70 if (p.le.0.0_DP) go to 80
    q=-q
    go to 90
 80 p=-p
 90 s=e
    e=d
    if (((2.0_DP*p).ge.(3.0_DP*xm*q-abs(tol1*q))) .or.                &
        (p.ge. abs(0.5d0*s*q))) go to 100
    d=p/q
    go to 110
100 d=xm
    e=d
110 a=b
    fa=fb
    if (abs(d).le.tol1) go to 120
    b=b+d
    go to 140
120 if (xm.le.0.0_DP) go to 130
    b=b+tol1
    go to 140
130 b=b-tol1
140 fb=f(b)
    if ((fb*(fc/abs(fc))).gt.0.0_DP) go to 20
    go to 30
150 zeroin_r8=b
    return
end function zeroin_r8

!-----------------------------------------------------------------------------
!> Sorts a real (kind 4) array in ascending order (`is_reverse` is absent or
!! `.FALSE.` ) or descending order (`is_reverse` is .TRUE. ).
recursive subroutine qsort_r4(A, is_reverse)
  !> Input array
  real(R4), intent(inout), dimension(:) :: A
  !> Logical flag for reverse (descending) sorting.
  logical, optional, intent(in) :: is_reverse
  integer :: iq

  if(size(A) > 1) then
     call partition_r4(A, iq)
     call qsort_r4(A(:iq-1))
     call qsort_r4(A(iq:))
  endif

  ! Check if reverse sorted array is requested
  if (present(is_reverse)) then
    if (is_reverse) A = A( size(A):1:-1 )
  end if

end subroutine qsort_r4

subroutine partition_r4(A, marker)
  real(R4), intent(in out), dimension(:) :: A
  integer, intent(out) :: marker
  integer :: i, j
  real(R4) :: temp
  real(R4) :: x      ! pivot point
  x = A(1)
  i= 0
  j= size(A) + 1

  do
     j = j-1
     do
        if (A(j) <= x) exit
        j = j-1
     end do
     i = i+1
     do
        if (A(i) >= x) exit
        i = i+1
     end do
     if (i < j) then
        ! exchange A(i) and A(j)
        temp = A(i)
        A(i) = A(j)
        A(j) = temp
     elseif (i == j) then
        marker = i+1
        return
     else
        marker = i
        return
     endif
  end do

end subroutine partition_r4

!-------------------------------------------------------------------------------
!> Sorts a real (kind 8) array in ascending order (`is_reverse` is absent or
!! `.FALSE.` ) or descending order (`is_reverse` is .TRUE. ).
recursive subroutine qsort_r8(A, is_reverse)
  !> Input array
  real(R8), intent(in out), dimension(:) :: A
  !> Logical flag for reverse (descending) sorting.
  logical, optional, intent(in) :: is_reverse
  integer :: iq

  if(size(A) > 1) then
     call partition_r8(A, iq)
     call qsort_r8(A(:iq-1))
     call qsort_r8(A(iq:))
  endif

  ! Check if reverse sorted array is requested
  if (present(is_reverse)) then
    if (is_reverse) A = A( size(A):1:-1 )
  end if

end subroutine qsort_r8

subroutine partition_r8(A, marker)
  real(R8), intent(in out), dimension(:) :: A
  integer, intent(out) :: marker
  integer :: i, j
  real(R8) :: temp
  real(R8) :: x      ! pivot point
  x = A(1)
  i= 0
  j= size(A) + 1

  do
     j = j-1
     do
        if (A(j) <= x) exit
        j = j-1
     end do
     i = i+1
     do
        if (A(i) >= x) exit
        i = i+1
     end do
     if (i < j) then
        ! exchange A(i) and A(j)
        temp = A(i)
        A(i) = A(j)
        A(j) = temp
     elseif (i == j) then
        marker = i+1
        return
     else
        marker = i
        return
     endif
  end do

end subroutine partition_r8

!-------------------------------------------------------------------------------
!> Sorts an integer array in ascending order (`is_reverse` is absent or
!! `.FALSE.` ) or descending order (`is_reverse` is .TRUE. ).
recursive subroutine qsort_i(A, is_reverse)
  !> Input array
  integer, intent(in out), dimension(:) :: A
  !> Logical flag for reverse (descending) sorting.
  logical, optional, intent(in) :: is_reverse
  integer :: iq

  if(size(A) > 1) then
     call partition_i(A, iq)
     call qsort_i(A(:iq-1))
     call qsort_i(A(iq:))
  endif

  ! Check if reverse sorted array is requested
  if (present(is_reverse)) then
    if (is_reverse) A = A( size(A):1:-1 )
  end if

end subroutine qsort_i

subroutine partition_i(A, marker)
  integer, intent(in out), dimension(:) :: A
  integer, intent(out) :: marker
  integer :: i, j
  integer :: temp
  integer :: x      ! pivot point
  x = A(1)
  i= 0
  j= size(A) + 1

  do
     j = j-1
     do
        if (A(j) <= x) exit
        j = j-1
     end do
     i = i+1
     do
        if (A(i) >= x) exit
        i = i+1
     end do
     if (i < j) then
        ! exchange A(i) and A(j)
        temp = A(i)
        A(i) = A(j)
        A(j) = temp
     elseif (i == j) then
        marker = i+1
        return
     else
        marker = i
        return
     endif
  end do

end subroutine partition_i










!SPLINE
!
!     subroutine spline to
!
!     carry out cubic spline interpolation
!
  subroutine spline_r4 (x, y, xx, yy, n, nn)
!
      real(kind=SP), intent(in)  :: x(:), y(:), xx(:)
      real(kind=SP), intent(out) :: yy(:)
      integer, optional, intent(in) :: n, nn

      integer ic(2), nwk, ierr
      real(kind=SP) :: d(size(x))
      real(kind=SP) :: vc(2), wk(2,size(x))
      logical skip
      integer, parameter :: incfd = 1

      integer :: nloc, nnloc ! Local copies for optionals

      if (present(n)) then
        nloc = n
      else
        nloc =  size(x)
      end if

      if (present(nn)) then
        nnloc = nn
      else
        nnloc = size(xx)
      end if

!     compute splines and interpolate

      nwk = 2*nloc
      ic(1) = 0
      ic(2) = 0
      vc(1) = 0.0
      vc(2) = 0.0
      skip = .false.
      call pchsp_r4(ic, vc, nloc, x, y, d, incfd, wk, nwk, ierr)
      call pchfe_r4(nloc, x, y, d, incfd, skip, nnloc, xx, yy, ierr)

  end subroutine spline_r4


  subroutine pchsp_r4(IC,VC,N,X,F,D,INCFD,WK,NWK,IERR)
!***BEGIN PROLOGUE  PCHSP
!***DATE WRITTEN   820503   (YYMMDD)
!***REVISION DATE  870707   (YYMMDD)
!***CATEGORY NO.  E1B
!***KEYWORDS  LIBRARY=SLATEC(PCHIP),
!             TYPE=SINGLE PRECISION(PCHSP-S DPCHSP-D),
!             CUBIC HERMITE INTERPOLATION,PIECEWISE CUBIC INTERPOLATION,
!             SPLINE INTERPOLATION
!***AUTHOR  FRITSCH, F. N., (LLNL)
!             MATHEMATICS AND STATISTICS DIVISION
!             LAWRENCE LIVERMORE NATIONAL LABORATORY
!             P.O. BOX 808  (L-316)
!             LIVERMORE, CA  94550
!             FTS 532-4275, (415) 422-4275
!***PURPOSE  Set derivatives needed to determine the Hermite represen-
!            tation of the cubic spline interpolant to given data, with
!            specified boundary conditions.
!***DESCRIPTION
!
!          PCHSP:   Piecewise Cubic Hermite Spline
!
!     Computes the Hermite representation of the cubic spline inter-
!     polant to the data given in X and F satisfying the boundary
!     conditions specified by IC and VC.
!
!     To facilitate two-dimensional applications, includes an increment
!     between successive values of the F- and D-arrays.
!
!     The resulting piecewise cubic Hermite function may be evaluated
!     by PCHFE or PCHFD.
!
!     NOTE:  This is a modified version of C. de Boor'S cubic spline
!            routine CUBSPL.
!
! ----------------------------------------------------------------------
!
!  Calling sequence:
!
!        PARAMETER  (INCFD = ...)
!        INTEGER  IC(2), N, NWK, IERR
!        REAL  VC(2), X(N), F(INCFD,N), D(INCFD,N), WK(NWK)
!
!        CALL  PCHSP (IC, VC, N, X, F, D, INCFD, WK, NWK, IERR)
!
!   Parameters:
!
!     IC -- (input) integer array of length 2 specifying desired
!           boundary conditions:
!           IC(1) = IBEG, desired condition at beginning of data.
!           IC(2) = IEND, desired condition at end of data.
!
!           IBEG = 0  to set D(1) so that the third derivative is con-
!              tinuous at X(2).  This is the "not a knot" condition
!              provided by de Boor'S cubic spline routine CUBSPL.
!              < This is the default boundary condition. >
!           IBEG = 1  if first derivative at X(1) is given in VC(1).
!           IBEG = 2  if second derivative at X(1) is given in VC(1).
!           IBEG = 3  to use the 3-point difference formula for D(1).
!                     (Reverts to the default b.c. if N.LT.3 .)
!           IBEG = 4  to use the 4-point difference formula for D(1).
!                     (Reverts to the default b.c. if N.LT.4 .)
!          NOTES:
!           1. An error return is taken if IBEG is out of range.
!           2. For the "natural" boundary condition, use IBEG=2 and
!              VC(1)=0.
!
!           IEND may take on the same values as IBEG, but applied to
!           derivative at X(N).  In case IEND = 1 or 2, the value is
!           given in VC(2).
!
!          NOTES:
!           1. An error return is taken if IEND is out of range.
!           2. For the "natural" boundary condition, use IEND=2 and
!              VC(2)=0.
!
!     VC -- (input) real array of length 2 specifying desired boundary
!           values, as indicated above.
!           VC(1) need be set only if IC(1) = 1 or 2 .
!           VC(2) need be set only if IC(2) = 1 or 2 .
!
!     N -- (input) number of data points.  (Error return if N.LT.2 .)
!
!     X -- (input) real array of independent variable values.  The
!           elements of X must be strictly increasing:
!                X(I-1) .LT. X(I),  I = 2(1)N.
!           (Error return if not.)
!
!     F -- (input) real array of dependent variable values to be inter-
!           polated.  F(1+(I-1)*INCFD) is value corresponding to X(I).
!
!     D -- (output) real array of derivative values at the data points.
!           These values will determine the cubic spline interpolant
!           with the requested boundary conditions.
!           The value corresponding to X(I) is stored in
!                D(1+(I-1)*INCFD),  I=1(1)N.
!           No other entries in D are changed.
!
!     INCFD -- (input) increment between successive values in F and D.
!           This argument is provided primarily for 2-D applications.
!           (Error return if  INCFD.LT.1 .)
!
!     WK -- (scratch) real array of working storage.
!
!     NWK -- (input) length of work array.
!           (Error return if NWK.LT.2*N .)
!
!     IERR -- (output) error flag.
!           Normal return:
!              IERR = 0  (no errors).
!           "Recoverable" errors:
!              IERR = -1  if N.LT.2 .
!              IERR = -2  if INCFD.LT.1 .
!              IERR = -3  if the X-array is not strictly increasing.
!              IERR = -4  if IBEG.LT.0 or IBEG.GT.4 .
!              IERR = -5  if IEND.LT.0 of IEND.GT.4 .
!              IERR = -6  if both of the above are true.
!              IERR = -7  if NWK is too small.
!               NOTE:  The above errors are checked in the order listed,
!                   and following arguments have **NOT** been validated.
!             (The D-array has not been changed in any of these cases.)
!              IERR = -8  in case of trouble solving the linear system
!                         for the interior derivative values.
!             (The D-array may have been changed in this case.)
!             (             Do **NOT** use it!                )
!
!***REFERENCES  CARL DE BOOR, A PRACTICAL GUIDE TO SPLINES, SPRINGER-
!                 VERLAG (NEW YORK, 1978), PP. 53-59.
!***ROUTINES CALLED  PCHDF,XERROR
!***END PROLOGUE  PCHSP
!
! ----------------------------------------------------------------------
!
!  Change record:
!     82-08-04   Converted to SLATEC library version.
!     87-07-07   Minor cosmetic changes to prologue.
!
! ----------------------------------------------------------------------
!
!  Programming notes:
!
!     To produce a double precision version, simply:
!        a. Change PCHSP to DPCHSP wherever it occurs,
!        b. Change the real declarations to double precision, and
!        c. Change the constants ZERO, HALF, ... to double precision.
!
!  DECLARE ARGUMENTS.
!
      INTEGER, INTENT(IN)  ::  IC(2), N, INCFD, NWK
      INTEGER, INTENT(OUT) :: IERR

      REAL(kind=SP), INTENT(IN)    :: VC(2), X(N), F(INCFD,N)
      REAL(kind=SP), INTENT(INOUT) :: WK(2,N)
      REAL(kind=SP), INTENT(OUT)   :: D(INCFD,N)
!
!  DECLARE LOCAL VARIABLES.
!
      INTEGER :: IBEG, IEND, INDEX, J, NM1
      REAL(kind=SP) :: G, HALF, ONE, STEMP(3), THREE, TWO, XTEMP(4), ZERO
!
      DATA  ZERO /0./,  HALF /0.5/,  ONE /1./,  TWO /2./,  THREE /3./
!
!  VALIDITY-CHECK ARGUMENTS.
!
!***FIRST EXECUTABLE STATEMENT  PCHSP
      IF ( N.LT.2 )  GO TO 5001
      IF ( INCFD.LT.1 )  GO TO 5002
      DO 1  J = 2, N
         IF ( X(J).LE.X(J-1) )  GO TO 5003
    1 END DO
!
      IBEG = IC(1)
      IEND = IC(2)
      IERR = 0
      IF ( (IBEG.LT.0).OR.(IBEG.GT.4) )  IERR = IERR - 1
      IF ( (IEND.LT.0).OR.(IEND.GT.4) )  IERR = IERR - 2
      IF ( IERR.LT.0 )  GO TO 5004
!
!  FUNCTION DEFINITION IS OK -- GO ON.
!
      IF ( NWK .LT. 2*N )  GO TO 5007
!
!  COMPUTE FIRST DIFFERENCES OF X SEQUENCE AND STORE IN WK(1,.). ALSO,
!  COMPUTE FIRST DIVIDED DIFFERENCE OF DATA AND STORE IN WK(2,.).
      DO 5  J=2,N
         WK(1,J) = X(J) - X(J-1)
         WK(2,J) = (F(1,J) - F(1,J-1))/WK(1,J)
    5 END DO
!
!  SET TO DEFAULT BOUNDARY CONDITIONS IF N IS TOO SMALL.
!
      IF ( IBEG.GT.N )  IBEG = 0
      IF ( IEND.GT.N )  IEND = 0
!
!  SET UP FOR BOUNDARY CONDITIONS.
!
      IF ( (IBEG.EQ.1).OR.(IBEG.EQ.2) )  THEN
         D(1,1) = VC(1)
      ELSE IF (IBEG .GT. 2)  THEN
!        PICK UP FIRST IBEG POINTS, IN REVERSE ORDER.
         DO 10  J = 1, IBEG
            INDEX = IBEG-J+1
!           INDEX RUNS FROM IBEG DOWN TO 1.
            XTEMP(J) = X(INDEX)
            IF (J .LT. IBEG)  STEMP(J) = WK(2,INDEX)
   10    CONTINUE
!                 --------------------------------
         D(1,1) = PCHDF_R4 (IBEG, XTEMP, STEMP, IERR)
!                 --------------------------------
         IF (IERR .NE. 0)  GO TO 5009
         IBEG = 1
      ENDIF
!
      IF ( (IEND.EQ.1).OR.(IEND.EQ.2) )  THEN
         D(1,N) = VC(2)
      ELSE IF (IEND .GT. 2)  THEN
!        PICK UP LAST IEND POINTS.
         DO 15  J = 1, IEND
            INDEX = N-IEND+J
!           INDEX RUNS FROM N+1-IEND UP TO N.
            XTEMP(J) = X(INDEX)
            IF (J .LT. IEND)  STEMP(J) = WK(2,INDEX+1)
   15    CONTINUE
!                 --------------------------------
         D(1,N) = PCHDF_R4 (IEND, XTEMP, STEMP, IERR)
!                 --------------------------------
         IF (IERR .NE. 0)  GO TO 5009
         IEND = 1
      ENDIF
!
! --------------------( BEGIN CODING FROM CUBSPL )--------------------
!
!  **** A TRIDIAGONAL LINEAR SYSTEM FOR THE UNKNOWN SLOPES S(J) OF
!  F  AT X(J), J=1,...,N, IS GENERATED AND THEN SOLVED BY GAUSS ELIM-
!  INATION, WITH S(J) ENDING UP IN D(1,J), ALL J.
!     WK(1,.) AND WK(2,.) ARE USED FOR TEMPORARY STORAGE.
!
!  CONSTRUCT FIRST EQUATION FROM FIRST BOUNDARY CONDITION, OF THE FORM
!             WK(2,1)*S(1) + WK(1,1)*S(2) = D(1,1)
!
      IF (IBEG .EQ. 0)  THEN
         IF (N .EQ. 2)  THEN
!           NO CONDITION AT LEFT END AND N = 2.
            WK(2,1) = ONE
            WK(1,1) = ONE
            D(1,1) = TWO*WK(2,2)
         ELSE
!           NOT-A-KNOT CONDITION AT LEFT END AND N .GT. 2.
            WK(2,1) = WK(1,3)
            WK(1,1) = WK(1,2) + WK(1,3)
            D(1,1) =((WK(1,2) + TWO*WK(1,1))*WK(2,2)*WK(1,3)            &
     &                        + WK(1,2)**2*WK(2,3)) / WK(1,1)
         ENDIF
      ELSE IF (IBEG .EQ. 1)  THEN
!        SLOPE PRESCRIBED AT LEFT END.
         WK(2,1) = ONE
         WK(1,1) = ZERO
      ELSE
!        SECOND DERIVATIVE PRESCRIBED AT LEFT END.
         WK(2,1) = TWO
         WK(1,1) = ONE
         D(1,1) = THREE*WK(2,2) - HALF*WK(1,2)*D(1,1)
      ENDIF
!
!  IF THERE ARE INTERIOR KNOTS, GENERATE THE CORRESPONDING EQUATIONS AND
!  CARRY OUT THE FORWARD PASS OF GAUSS ELIMINATION, AFTER WHICH THE J-TH
!  EQUATION READS    WK(2,J)*S(J) + WK(1,J)*S(J+1) = D(1,J).
!
      NM1 = N-1
      IF (NM1 .GT. 1)  THEN
         DO 20 J=2,NM1
            IF (WK(2,J-1) .EQ. ZERO)  GO TO 5008
            G = -WK(1,J+1)/WK(2,J-1)
            D(1,J) = G*D(1,J-1)                                         &
     &                  + THREE*(WK(1,J)*WK(2,J+1) + WK(1,J+1)*WK(2,J))
            WK(2,J) = G*WK(1,J-1) + TWO*(WK(1,J) + WK(1,J+1))
   20    CONTINUE
      ENDIF
!
!  CONSTRUCT LAST EQUATION FROM SECOND BOUNDARY CONDITION, OF THE FORM
!           (-G*WK(2,N-1))*S(N-1) + WK(2,N)*S(N) = D(1,N)
!
!     IF SLOPE IS PRESCRIBED AT RIGHT END, ONE CAN GO DIRECTLY TO BACK-
!     SUBSTITUTION, SINCE ARRAYS HAPPEN TO BE SET UP JUST RIGHT FOR IT
!     AT THIS POINT.
      IF (IEND .EQ. 1)  GO TO 30
!
      IF (IEND .EQ. 0)  THEN
         IF (N.EQ.2 .AND. IBEG.EQ.0)  THEN
!           NOT-A-KNOT AT RIGHT ENDPOINT AND AT LEFT ENDPOINT AND N = 2.
            D(1,2) = WK(2,2)
            GO TO 30
         ELSE IF ((N.EQ.2) .OR. (N.EQ.3 .AND. IBEG.EQ.0))  THEN
!           EITHER (N=3 AND NOT-A-KNOT ALSO AT LEFT) OR (N=2 AND *NOT*
!           NOT-A-KNOT AT LEFT END POINT).
            D(1,N) = TWO*WK(2,N)
            WK(2,N) = ONE
            IF (WK(2,N-1) .EQ. ZERO)  GO TO 5008
            G = -ONE/WK(2,N-1)
         ELSE
!           NOT-A-KNOT AND N .GE. 3, AND EITHER N.GT.3 OR  ALSO NOT-A-
!           KNOT AT LEFT END POINT.
            G = WK(1,N-1) + WK(1,N)
!           DO NOT NEED TO CHECK FOLLOWING DENOMINATORS (X-DIFFERENCES).
            D(1,N) = ((WK(1,N)+TWO*G)*WK(2,N)*WK(1,N-1)                 &
     &                  + WK(1,N)**2*(F(1,N-1)-F(1,N-2))/WK(1,N-1))/G
            IF (WK(2,N-1) .EQ. ZERO)  GO TO 5008
            G = -G/WK(2,N-1)
            WK(2,N) = WK(1,N-1)
         ENDIF
      ELSE
!        SECOND DERIVATIVE PRESCRIBED AT RIGHT ENDPOINT.
         D(1,N) = THREE*WK(2,N) + HALF*WK(1,N)*D(1,N)
         WK(2,N) = TWO
         IF (WK(2,N-1) .EQ. ZERO)  GO TO 5008
         G = -ONE/WK(2,N-1)
      ENDIF
!
!  COMPLETE FORWARD PASS OF GAUSS ELIMINATION.
!
      WK(2,N) = G*WK(1,N-1) + WK(2,N)
      IF (WK(2,N) .EQ. ZERO)   GO TO 5008
      D(1,N) = (G*D(1,N-1) + D(1,N))/WK(2,N)
!
!  CARRY OUT BACK SUBSTITUTION
!
   30 CONTINUE
      DO 40 J=NM1,1,-1
         IF (WK(2,J) .EQ. ZERO)  GO TO 5008
         D(1,J) = (D(1,J) - WK(1,J)*D(1,J+1))/WK(2,J)
   40 END DO
! --------------------(  END  CODING FROM CUBSPL )--------------------
!
!  NORMAL RETURN.
!
      RETURN
!
!  ERROR RETURNS.
!
 5001 CONTINUE
!     N.LT.2 RETURN.
      IERR = -1
!     CALL XERROR ('PCHSP -- NUMBER OF DATA POINTS LESS THAN TWO'
!    *           , 44, IERR, 1)
      write(*,*)'pchsp error: number of data points less than two'
      RETURN
!
 5002 CONTINUE
!     INCFD.LT.1 RETURN.
      IERR = -2
!     CALL XERROR ('PCHSP -- INCREMENT LESS THAN ONE'
!    *           , 32, IERR, 1)
      write(*,*)'pchsp error: increment less than one'
      RETURN
!
 5003 CONTINUE
!     X-ARRAY NOT STRICTLY INCREASING.
      IERR = -3
!     CALL XERROR ('PCHSP -- X-ARRAY NOT STRICTLY INCREASING'
!    *           , 40, IERR, 1)
      write(*,*)'pchsp error: x-array not strictly increasing'
      RETURN
!
 5004 CONTINUE
!     IC OUT OF RANGE RETURN.
      IERR = IERR - 3
!     CALL XERROR ('PCHSP -- IC OUT OF RANGE'
!    *           , 24, IERR, 1)
      write(*,*)'pchsp error: ic out of range'
      RETURN
!
 5007 CONTINUE
!     NWK TOO SMALL RETURN.
      IERR = -7
!     CALL XERROR ('PCHSP -- WORK ARRAY TOO SMALL'
!    *           , 29, IERR, 1)
      write(*,*)'pchsp error: work array too small'
      RETURN
!
 5008 CONTINUE
!     SINGULAR SYSTEM.
!   *** THEORETICALLY, THIS CAN ONLY OCCUR IF SUCCESSIVE X-VALUES   ***
!   *** ARE EQUAL, WHICH SHOULD ALREADY HAVE BEEN CAUGHT (IERR=-3). ***
      IERR = -8
!     CALL XERROR ('PCHSP -- SINGULAR LINEAR SYSTEM'
!    *           , 31, IERR, 1)
      write(*,*)'pchsp error: singular linear system'
      RETURN
!
 5009 CONTINUE
!     ERROR RETURN FROM PCHDF.
!   *** THIS CASE SHOULD NEVER OCCUR ***
      IERR = -9
!     CALL XERROR ('PCHSP -- ERROR RETURN FROM PCHDF'
!    *           , 32, IERR, 1)
      write(*,*)'pchsp error: error return from pchdf'
      RETURN
!------------- LAST LINE OF PCHSP FOLLOWS ------------------------------
  end subroutine pchsp_r4


  real(kind=SP) function pchdf_r4 (K, X, S, IERR)
!***BEGIN PROLOGUE  PCHDF
!***SUBSIDIARY
!***PURPOSE  Computes divided differences for PCHCE and PCHSP
!***LIBRARY   SLATEC (PCHIP)
!***TYPE      SINGLE PRECISION (PCHDF-S, DPCHDF-D)
!***AUTHOR  Fritsch, F. N., (LLNL)
!***DESCRIPTION
!
!          PCHDF:   PCHIP Finite Difference Formula
!
!     Uses a divided difference formulation to compute a K-point approx-
!     imation to the derivative at X(K) based on the data in X and S.
!
!     Called by  PCHCE  and  PCHSP  to compute 3- and 4-point boundary
!     derivative approximations.
!
! ----------------------------------------------------------------------
!
!     On input:
!        K      is the order of the desired derivative approximation.
!               K must be at least 3 (error return if not).
!        X      contains the K values of the independent variable.
!               X need not be ordered, but the values **MUST** be
!               distinct.  (Not checked here.)
!        S      contains the associated slope values:
!                  S(I) = (F(I+1)-F(I))/(X(I+1)-X(I)), I=1(1)K-1.
!               (Note that S need only be of length K-1.)
!
!     On return:
!        S      will be destroyed.
!        IERR   will be set to -1 if K.LT.2 .
!        PCHDF  will be set to the desired derivative approximation if
!               IERR=0 or to zero if IERR=-1.
!
! ----------------------------------------------------------------------
!
!***SEE ALSO  PCHCE, PCHSP
!***REFERENCES  Carl de Boor, A Practical Guide to Splines, Springer-
!                 Verlag, New York, 1978, pp. 10-16.
!***ROUTINES CALLED  XERMSG
!***REVISION HISTORY  (YYMMDD)
!   820503  DATE WRITTEN
!   820805  Converted to SLATEC library version.
!   870813  Minor cosmetic changes.
!   890411  Added SAVE statements (Vers. 3.2).
!   890411  REVISION DATE from Version 3.2
!   891214  Prologue converted to Version 4.0 format.  (BAB)
!   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
!   900328  Added TYPE section.  (WRB)
!   910408  Updated AUTHOR and DATE WRITTEN sections in prologue.  (WRB)
!   920429  Revised format and order of references.  (WRB,FNF)
!   930503  Improved purpose.  (FNF)
!***END PROLOGUE  PCHDF
!
!**End
!
!  DECLARE ARGUMENTS.
!
      INTEGER, INTENT(IN)  :: K
      INTEGER, INTENT(OUT) :: IERR

      REAL(kind=SP), INTENT(IN)    :: X(K)
      REAL(kind=SP), INTENT(INOUT) :: S(K)
!
!  DECLARE LOCAL VARIABLES.
!
      INTEGER  I, J
      REAL(kind=SP) :: VALUE, ZERO
      SAVE ZERO
      DATA  ZERO /0./
!
!  CHECK FOR LEGAL VALUE OF K.
!
!***FIRST EXECUTABLE STATEMENT  PCHDF
      IF (K .LT. 3)  GO TO 5001
!
!  COMPUTE COEFFICIENTS OF INTERPOLATING POLYNOMIAL.
!
      DO 10  J = 2, K-1
         DO 9  I = 1, K-J
            S(I) = (S(I+1)-S(I))/(X(I+J)-X(I))
    9    CONTINUE
   10 END DO
!
!  EVALUATE DERIVATIVE AT X(K).
!
      VALUE = S(1)
      DO 20  I = 2, K-1
         VALUE = S(I) + VALUE*(X(K)-X(I))
   20 END DO
!
!  NORMAL RETURN.
!
      IERR = 0
      PCHDF_R4 = VALUE
      RETURN
!
!  ERROR RETURN.
!
 5001 CONTINUE
!     K.LT.3 RETURN.
      IERR = -1
!     CALL XERMSG ('SLATEC', 'PCHDF', 'K LESS THAN THREE', IERR, 1)
      write(*,*)'slatec pchdf error: k less than three'
      PCHDF_R4 = ZERO
      RETURN
!------------- LAST LINE OF PCHDF FOLLOWS ------------------------------
  end function pchdf_r4


  subroutine pchfe_r4(N,X,F,D,INCFD,SKIP,NE,XE,FE,IERR)
!***BEGIN PROLOGUE  PCHFE
!***DATE WRITTEN   811020   (YYMMDD)
!***REVISION DATE  870707   (YYMMDD)
!***CATEGORY NO.  E3
!***KEYWORDS  LIBRARY=SLATEC(PCHIP),
!             TYPE=SINGLE PRECISION(PCHFE-S DPCHFE-D),
!             CUBIC HERMITE EVALUATION,HERMITE INTERPOLATION,
!             PIECEWISE CUBIC EVALUATION
!***AUTHOR  FRITSCH, F. N., (LLNL)
!             MATHEMATICS AND STATISTICS DIVISION
!             LAWRENCE LIVERMORE NATIONAL LABORATORY
!             P.O. BOX 808  (L-316)
!             LIVERMORE, CA  94550
!             FTS 532-4275, (415) 422-4275
!***PURPOSE  Evaluate a piecewise cubic Hermite function at an array of
!            points.  May be used by itself for Hermite interpolation,
!            or as an evaluator for PCHIM or PCHIC.
!***DESCRIPTION
!
!          PCHFE:  Piecewise Cubic Hermite Function Evaluator
!
!     Evaluates the cubic Hermite function defined by  N, X, F, D  at
!     the points  XE(J), J=1(1)NE.
!
!     To provide compatibility with PCHIM and PCHIC, includes an
!     increment between successive values of the F- and D-arrays.
!
! ----------------------------------------------------------------------
!
!  Calling sequence:
!
!        PARAMETER  (INCFD = ...)
!        INTEGER  N, NE, IERR
!        REAL  X(N), F(INCFD,N), D(INCFD,N), XE(NE), FE(NE)
!        LOGICAL  SKIP
!
!        CALL  PCHFE (N, X, F, D, INCFD, SKIP, NE, XE, FE, IERR)
!
!   Parameters:
!
!     N -- (input) number of data points.  (Error return if N.LT.2 .)
!
!     X -- (input) real array of independent variable values.  The
!           elements of X must be strictly increasing:
!                X(I-1) .LT. X(I),  I = 2(1)N.
!           (Error return if not.)
!
!     F -- (input) real array of function values.  F(1+(I-1)*INCFD) is
!           the value corresponding to X(I).
!
!     D -- (input) real array of derivative values.  D(1+(I-1)*INCFD) is
!           the value corresponding to X(I).
!
!     INCFD -- (input) increment between successive values in F and D.
!           (Error return if  INCFD.LT.1 .)
!
!     SKIP -- (input/output) logical variable which should be set to
!           .TRUE. if the user wishes to skip checks for validity of
!           preceding parameters, or to .FALSE. otherwise.
!           This will save time in case these checks have already
!           been performed (say, in PCHIM or PCHIC).
!           SKIP will be set to .TRUE. on normal return.
!
!     NE -- (input) number of evaluation points.  (Error return if
!           NE.LT.1 .)
!
!     XE -- (input) real array of points at which the function is to be
!           evaluated.
!
!          NOTES:
!           1. The evaluation will be most efficient if the elements
!              of XE are increasing relative to X;
!              that is,   XE(J) .GE. X(I)
!              implies    XE(K) .GE. X(I),  all K.GE.J .
!           2. If any of the XE are outside the interval [X(1),X(N)],
!              values are extrapolated from the nearest extreme cubic,
!              and a warning error is returned.
!
!     FE -- (output) real array of values of the cubic Hermite function
!           defined by  N, X, F, D  at the points  XE.
!
!     IERR -- (output) error flag.
!           Normal return:
!              IERR = 0  (no errors).
!           Warning error:
!              IERR.GT.0  means that extrapolation was performed at
!                 IERR points.
!           "Recoverable" errors:
!              IERR = -1  if N.LT.2 .
!              IERR = -2  if INCFD.LT.1 .
!              IERR = -3  if the X-array is not strictly increasing.
!              IERR = -4  if NE.LT.1 .
!             (The FE-array has not been changed in any of these cases.)
!               NOTE:  The above errors are checked in the order listed,
!                   and following arguments have **NOT** been validated.
!
!***REFERENCES  (NONE)
!***ROUTINES CALLED  CHFEV,XERROR
!***END PROLOGUE  PCHFE
!
! ----------------------------------------------------------------------
!
!  Change record:
!     82-08-03   Minor cosmetic changes for release 1.
!     87-07-07   Minor cosmetic changes to prologue.
!
! ----------------------------------------------------------------------
!
!  Programming notes:
!
!     1. To produce a double precision version, simply:
!        a. Change PCHFE to DPCHFE, and CHFEV to DCHFEV, wherever they
!           occur,
!        b. Change the real declaration to double precision,
!
!     2. Most of the coding between the call to CHFEV and the end of
!        the IR-loop could be eliminated if it were permissible to
!        assume that XE is ordered relative to X.
!
!     3. CHFEV does not assume that X1 is less than X2.  thus, it would
!        be possible to write a version of PCHFE that assumes a strict-
!        ly decreasing X-array by simply running the IR-loop backwards
!        (and reversing the order of appropriate tests).
!
!     4. The present code has a minor bug, which I have decided is not
!        worth the effort that would be required to fix it.
!        If XE contains points in [X(N-1),X(N)], followed by points .LT.
!        X(N-1), followed by points .GT.X(N), the extrapolation points
!        will be counted (at least) twice in the total returned in IERR.
!
!  DECLARE ARGUMENTS.
!
      INTEGER, INTENT(IN)  :: N, INCFD, NE
      INTEGER, INTENT(OUT) :: IERR

      REAL(kind=SP), INTENT(IN)  :: X(N), F(INCFD,N), D(INCFD,N), XE(NE)
      REAL(kind=SP), INTENT(OUT) :: FE(NE)

      LOGICAL, INTENT(INOUT) :: SKIP
!
!  DECLARE LOCAL VARIABLES.
!
      INTEGER  I, IERC, IR, J, JFIRST, NEXT(2), NJ
!
!  VALIDITY-CHECK ARGUMENTS.
!
!***FIRST EXECUTABLE STATEMENT  PCHFE
      IF (SKIP)  GO TO 5
!
      IF ( N.LT.2 )  GO TO 5001
      IF ( INCFD.LT.1 )  GO TO 5002
      DO 1  I = 2, N
         IF ( X(I).LE.X(I-1) )  GO TO 5003
    1 END DO
!
!  FUNCTION DEFINITION IS OK, GO ON.
!
    5 CONTINUE
      IF ( NE.LT.1 )  GO TO 5004
      IERR = 0
      SKIP = .TRUE.
!
!  LOOP OVER INTERVALS.        (   INTERVAL INDEX IS  IL = IR-1  . )
!                              ( INTERVAL IS X(IL).LE.X.LT.X(IR) . )
      JFIRST = 1
      IR = 2
   10 CONTINUE
!
!     SKIP OUT OF LOOP IF HAVE PROCESSED ALL EVALUATION POINTS.
!
         IF (JFIRST .GT. NE)  GO TO 5000
!
!     LOCATE ALL POINTS IN INTERVAL.
!
         DO 20  J = JFIRST, NE
            IF (XE(J) .GE. X(IR))  GO TO 30
   20    CONTINUE
         J = NE + 1
         GO TO 40
!
!     HAVE LOCATED FIRST POINT BEYOND INTERVAL.
!
   30    CONTINUE
         IF (IR .EQ. N)  J = NE + 1
!
   40    CONTINUE
         NJ = J - JFIRST
!
!     SKIP EVALUATION IF NO POINTS IN INTERVAL.
!
         IF (NJ .EQ. 0)  GO TO 50
!
!     EVALUATE CUBIC AT XE(I),  I = JFIRST (1) J-1 .
!
!       ----------------------------------------------------------------
        CALL CHFEV_R4 (X(IR-1),X(IR), F(1,IR-1),F(1,IR), D(1,IR-1),D(1,IR),&
     &                 NJ, XE(JFIRST), FE(JFIRST), NEXT, IERC)
!       ----------------------------------------------------------------
         IF (IERC .LT. 0)  GO TO 5005
!
         IF (NEXT(2) .EQ. 0)  GO TO 42
!        IF (NEXT(2) .GT. 0)  THEN
!           IN THE CURRENT SET OF XE-POINTS, THERE ARE NEXT(2) TO THE
!           RIGHT OF X(IR).
!
            IF (IR .LT. N)  GO TO 41
!           IF (IR .EQ. N)  THEN
!              THESE ARE ACTUALLY EXTRAPOLATION POINTS.
               IERR = IERR + NEXT(2)
               GO TO 42
   41       CONTINUE
!           ELSE
!              WE SHOULD NEVER HAVE GOTTEN HERE.
               GO TO 5005
!           ENDIF
!        ENDIF
   42    CONTINUE
!
         IF (NEXT(1) .EQ. 0)  GO TO 49
!        IF (NEXT(1) .GT. 0)  THEN
!           IN THE CURRENT SET OF XE-POINTS, THERE ARE NEXT(1) TO THE
!           LEFT OF X(IR-1).
!
            IF (IR .GT. 2)  GO TO 43
!           IF (IR .EQ. 2)  THEN
!              THESE ARE ACTUALLY EXTRAPOLATION POINTS.
               IERR = IERR + NEXT(1)
               GO TO 49
   43       CONTINUE
!           ELSE
!              XE IS NOT ORDERED RELATIVE TO X, SO MUST ADJUST
!              EVALUATION INTERVAL.
!
!              FIRST, LOCATE FIRST POINT TO LEFT OF X(IR-1).
               DO 44  I = JFIRST, J-1
                  IF (XE(I) .LT. X(IR-1))  GO TO 45
   44          CONTINUE
!              NOTE-- CANNOT DROP THROUGH HERE UNLESS THERE IS AN ERROR
!                     IN CHFEV.
               GO TO 5005
!
   45          CONTINUE
!              RESET J.  (THIS WILL BE THE NEW JFIRST.)
               J = I
!
!              NOW FIND OUT HOW FAR TO BACK UP IN THE X-ARRAY.
               DO 46  I = 1, IR-1
                  IF (XE(J) .LT. X(I)) GO TO 47
   46          CONTINUE
!              NB-- CAN NEVER DROP THROUGH HERE, SINCE XE(J).LT.X(IR-1).
!
   47          CONTINUE
!              AT THIS POINT, EITHER  XE(J) .LT. X(1)
!                 OR      X(I-1) .LE. XE(J) .LT. X(I) .
!              RESET IR, RECOGNIZING THAT IT WILL BE INCREMENTED BEFORE
!              CYCLING.
               IR = MAX0(1, I-1)
!           ENDIF
!        ENDIF
   49    CONTINUE
!
         JFIRST = J
!
!     END OF IR-LOOP.
!
   50 CONTINUE
      IR = IR + 1
      IF (IR .LE. N)  GO TO 10
!
!  NORMAL RETURN.
!
 5000 CONTINUE
      RETURN
!
!  ERROR RETURNS.
!
 5001 CONTINUE
!     N.LT.2 RETURN.
      IERR = -1
!     CALL XERROR ('PCHFE -- NUMBER OF DATA POINTS LESS THAN TWO'
!    *           , 44, IERR, 1)
      write(*,*)'pchfe error: number of data points less than two'
      RETURN
!
 5002 CONTINUE
!     INCFD.LT.1 RETURN.
      IERR = -2
!     CALL XERROR ('PCHFE -- INCREMENT LESS THAN ONE'
!    *           , 32, IERR, 1)
      write(*,*)'pchfe error: increment less than one'
      RETURN
!
 5003 CONTINUE
!     X-ARRAY NOT STRICTLY INCREASING.
      IERR = -3
!     CALL XERROR ('PCHFE -- X-ARRAY NOT STRICTLY INCREASING'
!    *           , 40, IERR, 1)
      write(*,*)'pchfe error: x-array not strictly increasing'
      RETURN
!
 5004 CONTINUE
!     NE.LT.1 RETURN.
      IERR = -4
!     CALL XERROR ('PCHFE -- NUMBER OF EVALUATION POINTS LESS THAN ONE'
!    *           , 50, IERR, 1)
      write(*,*)'pchfe error: number of evaluation points less than one'
      RETURN
!
 5005 CONTINUE
!     ERROR RETURN FROM CHFEV.
!   *** THIS CASE SHOULD NEVER OCCUR ***
      IERR = -5
!     CALL XERROR ('PCHFE -- ERROR RETURN FROM CHFEV -- FATAL'
!    *           , 41, IERR, 2)
      write(*,*)'pchfe error: error return form chfev -- fatal'
      RETURN
!------------- LAST LINE OF PCHFE FOLLOWS ------------------------------
  end subroutine pchfe_r4


  subroutine chfev_r4(X1,X2,F1,F2,D1,D2,NE,XE,FE,NEXT,IERR)
!***BEGIN PROLOGUE  CHFEV
!***DATE WRITTEN   811019   (YYMMDD)
!***REVISION DATE  870707   (YYMMDD)
!***CATEGORY NO.  E3,H1
!***KEYWORDS  LIBRARY=SLATEC(PCHIP),
!             TYPE=SINGLE PRECISION(CHFEV-S DCHFEV-D),
!             CUBIC HERMITE EVALUATION,CUBIC POLYNOMIAL EVALUATION
!***AUTHOR  FRITSCH, F. N., (LLNL)
!             MATHEMATICS AND STATISTICS DIVISION
!             LAWRENCE LIVERMORE NATIONAL LABORATORY
!             P.O. BOX 808  (L-316)
!             LIVERMORE, CA  94550
!             FTS 532-4275, (415) 422-4275
!***PURPOSE  Evaluate a cubic polynomial given in Hermite form at an
!            array of points.  While designed for use by PCHFE, it may
!            be useful directly as an evaluator for a piecewise cubic
!            Hermite function in applications, such as graphing, where
!            the interval is known in advance.
!***DESCRIPTION
!
!          CHFEV:  Cubic Hermite Function EValuator
!
!     Evaluates the cubic polynomial determined by function values
!     F1,F2 and derivatives D1,D2 on interval (X1,X2) at the points
!     XE(J), J=1(1)NE.
!
! ----------------------------------------------------------------------
!
!  Calling sequence:
!
!        INTEGER  NE, NEXT(2), IERR
!        REAL  X1, X2, F1, F2, D1, D2, XE(NE), FE(NE)
!
!        CALL  CHFEV (X1,X2, F1,F2, D1,D2, NE, XE, FE, NEXT, IERR)
!
!   Parameters:
!
!     X1,X2 -- (input) endpoints of interval of definition of cubic.
!           (Error return if  X1.EQ.X2 .)
!
!     F1,F2 -- (input) values of function at X1 and X2, respectively.
!
!     D1,D2 -- (input) values of derivative at X1 and X2, respectively.
!
!     NE -- (input) number of evaluation points.  (Error return if
!           NE.LT.1 .)
!
!     XE -- (input) real array of points at which the function is to be
!           evaluated.  If any of the XE are outside the interval
!           [X1,X2], a warning error is returned in NEXT.
!
!     FE -- (output) real array of values of the cubic function defined
!           by  X1,X2, F1,F2, D1,D2  at the points  XE.
!
!     NEXT -- (output) integer array indicating number of extrapolation
!           points:
!            NEXT(1) = number of evaluation points to left of interval.
!            NEXT(2) = number of evaluation points to right of interval.
!
!     IERR -- (output) error flag.
!           Normal return:
!              IERR = 0  (no errors).
!           "Recoverable" errors:
!              IERR = -1  if NE.LT.1 .
!              IERR = -2  if X1.EQ.X2 .
!                (The FE-array has not been changed in either case.)
!
!***REFERENCES  (NONE)
!***ROUTINES CALLED  XERROR
!***END PROLOGUE  CHFEV
!
! ----------------------------------------------------------------------
!
!  Change record:
!     82-08-03   Minor cosmetic changes for release 1.
!
! ----------------------------------------------------------------------
!
!  Programming notes:
!
!     To produce a double precision version, simply:
!        a. Change CHFEV to DCHFEV wherever it occurs,
!        b. Change the real declaration to double precision,
!        c. Change the constant ZERO to double precision, and
!        d. Change the names of the Fortran functions:  AMAX1, AMIN1.
!
!  DECLARE ARGUMENTS.
!
      INTEGER, INTENT(IN)  :: NE
      INTEGER, INTENT(OUT) :: NEXT(2), IERR

      REAL(kind=SP), INTENT(IN)  :: X1, X2, F1, F2, D1, D2, XE(NE)
      REAL(kind=SP), INTENT(OUT) :: FE(NE)
!
!  DECLARE LOCAL VARIABLES.
!
      INTEGER  I
      REAL(kind=SP) :: C2, C3, DEL1, DEL2, DELTA, H, X, XMI, XMA, ZERO
      DATA  ZERO /0./
!
!  VALIDITY-CHECK ARGUMENTS.
!
!***FIRST EXECUTABLE STATEMENT  CHFEV
      IF (NE .LT. 1)  GO TO 5001
      H = X2 - X1
      IF (H .EQ. ZERO)  GO TO 5002
!
!  INITIALIZE.
!
      IERR = 0
      NEXT(1) = 0
      NEXT(2) = 0
      XMI = AMIN1(ZERO, H)
      XMA = AMAX1(ZERO, H)
!
!  COMPUTE CUBIC COEFFICIENTS (EXPANDED ABOUT X1).
!
      DELTA = (F2 - F1)/H
      DEL1 = (D1 - DELTA)/H
      DEL2 = (D2 - DELTA)/H
!                                           (DELTA IS NO LONGER NEEDED.)
      C2 = -(DEL1+DEL1 + DEL2)
      C3 = (DEL1 + DEL2)/H
!                               (H, DEL1 AND DEL2 ARE NO LONGER NEEDED.)
!
!  EVALUATION LOOP.
!
      DO 500  I = 1, NE
         X = XE(I) - X1
         FE(I) = F1 + X*(D1 + X*(C2 + X*C3))
!          COUNT EXTRAPOLATION POINTS.
         IF ( X.LT.XMI )  NEXT(1) = NEXT(1) + 1
         IF ( X.GT.XMA )  NEXT(2) = NEXT(2) + 1
!        (NOTE REDUNDANCY--IF EITHER CONDITION IS TRUE, OTHER IS FALSE.)
  500 END DO
!
!  NORMAL RETURN.
!
      RETURN
!
!  ERROR RETURNS.
!
 5001 CONTINUE
!     NE.LT.1 RETURN.
      IERR = -1
!     CALL XERROR ('CHFEV -- NUMBER OF EVALUATION POINTS LESS THAN ONE'
!    *           , 50, IERR, 1)
      write(*,*)'chfev error: number of evaluation points less than one'
      RETURN
!
 5002 CONTINUE
!     X1.EQ.X2 RETURN.
      IERR = -2
!     CALL XERROR ('CHFEV -- INTERVAL ENDPOINTS EQUAL'
!    *           , 33, IERR, 1)
      write(*,*)'chfev error: interval endpoints equal'
      RETURN
!------------- LAST LINE OF CHFEV FOLLOWS ------------------------------
  end subroutine chfev_r4



















end module BASE_UTILS
