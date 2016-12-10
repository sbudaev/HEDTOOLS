module CSV_IO
!*******************************************************************************
! SVN $Id$
!*******************************************************************************
! CSV_IO
! PURPOSE: This module provides high level functions for input and output in
!          CSV (Comma Seperated Values) file format along with a few other
!          basic utilities. CSV is accessible by LibreOffice and MS Excel.
! NOTES:
!    (1) The typical workflow for output in CSV file format is like this:
!        * CSV_FILE_OPEN_WRITE - physically open CSV file for writing;
!        * CSV_FILE_HEADER_WRITE - physically write optional descriptive header
!           (header is just the first line of the CSV file);
!        * do -- loop (1) over records (rows of data file)
!            do -- loop (2) over values within the same record
!              CSV_RECORD_APPEND - produce record of data values
!                 of different types, append single values, usually in a loop
!            end do -- end loop (2)
!            CSV_FILE_RECORD_WRITE - physically write record of data
!          end do -- end loop(1) -- go to producing next record;
!        * CSV_FILE_CLOSE - physically closes the output CSV file.
!        Thus, subs ending with _WRITE do physical write, as well as _CLOSE.
!    (2) This module is most suited at this moment (version Nov-2015) for CSV
!        file output rather than input. Input CSV is to be done.
!    (3) This module widely uses optional arguments. They could be called
!      using named parameters, e.g.
!      this way (the first optional parameter absent):
!        intNextunit = GET_FREE_FUNIT(file_status=logicalFlag)
!      or (both parameters present but swapped):
!        intNextunit = GET_FREE_FUNIT(file_status=logicalFlag, max_funit=200)
!      or (optional parameters absent altogether):
!        intNextunit = GET_FREE_FUNIT()
!      or (standard way)
!        intNextunit = GET_FREE_FUNIT(200, logicalFlag)
!    (4) Files can be referred either by unit or by name, but unit has
!      precedence (if both a provided, unit is used).
!
! Author: Sergey Budaev, based on csv_io functions by John Burkardt
!*******************************************************************************

use BASE_STRINGS, only : VALUE, PARSE, SPLIT, COMPACT, IS_NUMERIC

implicit none

! Public constants
integer, public, parameter :: MAX_UNIT=255      ! Maximum unit number (in old
                                                ! Fortran units range 1 to 99)

! These constants are defined in the intrinsic module ISO_FORTRAN_ENV,
! but may be redefined here if compiler doesn't use this. It is not really
! necessary in most cases and might be dangerous if this particular
! platform/compiler actually implements a different assignment.
! Use this intrinsic module as follows:
!   use, intrinsic :: ISO_FORTRAN_ENV
! So, here it is commented it out for portability and standard compliance:
!integer, public, parameter :: INPUT_UNIT=5      ! standard ISO values for input,
!integer, public, parameter :: OUTPUT_UNIT=6     ! output, and standard
!integer, public, parameter :: ERROR_UNIT=9      ! error units

! Module name for the DEBUG LOGGER: every function/sub must also have
! the PROCNAME parameter referring to its name. This is done for the Debug
! Logger module. Each module must also have a DEBUG Logger subroutine, that
! is a wrapper to module LOGGER (or perhaps any other that is being used)
!   procedure name PROCNAME
character (len=*), private, parameter :: MODNAME = "CSV_IO"

! Set the debug mode to ON or OFF, in the debug mode, events are written to
! the log, determined by the module local LOG_DBG subroutine, normally, a
! wrapper to the module LOGGER. May also define integer DEBUG_LEVEL parameter...
logical, private, parameter :: IS_DEBUG = .FALSE.

!*******************************************************************************
! DERIVED TYPE for CSV FILE HANDLE
! Define derived type csv_file structure for keeping csv file handle:
!*******************************************************************************
!
! PORTABILITY NOTE for derived type file handle:
! character (len=:), allocatable :: name -- works on Oracle F95 and probably
! some other, but gfortran prior to v. 5 issues this error:
!
! Deferred-length character component 'name' at (1) is not yet supported
!
! Therefore, use fixed length string for portability here; don't forget that
!  concatenation of fixed strings requires trim() to avoid empty holes in
!  the result or no result, e.g.:
! type (csv_file) :: zoutput
! zoutput%name= trim(directory) // "file_" // TOSTR(number) // ".csv"
!  We also define the maximum length of file name string as a parameter,
!  is it (255) enough length for full file path?
integer, public, parameter :: MAX_FILENAME=255
type, public :: csv_file
 !character (len=:), allocatable :: name  ! name of file :: disabled, unportable
  character (len=MAX_FILENAME) :: name    ! the name of the file
  integer :: unit = -1                    ! file-handle unit, default auto
  logical :: status = .TRUE.              ! flag for success of latest operation
end type csv_file

!*******************************************************************************
! GENERIC INTERFACES
! Generic interfaces to the modules. These allow calling CSV_RECORD_APPEND
! generically, for different data types, which are selected by the module
! automatically. e.g. just call CSV_RECORD_APPEND irrespective of the data type
!*******************************************************************************

! Generic interfaces for record building, one value or array or list

interface CSV_RECORD_APPEND

  module procedure CSV_RECORD_APPEND_I4
  module procedure CSV_RECORD_APPEND_R4
  module procedure CSV_RECORD_APPEND_R8
  module procedure CSV_RECORD_APPEND_S

  module procedure CSV_RECORD_APPEND_ARRAY_I4
  module procedure CSV_RECORD_APPEND_ARRAY_R4
  module procedure CSV_RECORD_APPEND_ARRAY_R8
  module procedure CSV_RECORD_APPEND_ARRAY_S

  module procedure CSV_RECORD_APPEND_LST_I4
  module procedure CSV_RECORD_APPEND_LST_R4
  module procedure CSV_RECORD_APPEND_LST_R8
  module procedure CSV_RECORD_APPEND_LST_S

end interface CSV_RECORD_APPEND

! Generic interfaces for whole array/matrix operations with arbitrary data types

interface CSV_MATRIX_WRITE

  module procedure CSV_MATRIX_WRITE_I4
  module procedure CSV_MATRIX_WRITE_R4
  module procedure CSV_MATRIX_WRITE_R8
  module procedure CSV_MATRIX_WRITE_S

  module procedure CSV_ARRAY_WRITE_I4
  module procedure CSV_ARRAY_WRITE_R4
  module procedure CSV_ARRAY_WRITE_R8
  module procedure CSV_ARRAY_WRITE_S

end interface CSV_MATRIX_WRITE

interface CSV_ARRAY_WRITE

  module procedure CSV_ARRAY_WRITE_I4
  module procedure CSV_ARRAY_WRITE_R4
  module procedure CSV_ARRAY_WRITE_R8
  module procedure CSV_ARRAY_WRITE_S

end interface CSV_ARRAY_WRITE

! Generic interfaces for physical read/write using the derived type file handle

interface CSV_OPEN_READ

  module procedure CSV_FILE_OPEN_READ
  module procedure CSV_FILE_OPEN_READ_T

end interface CSV_OPEN_READ

interface CSV_OPEN_WRITE

  module procedure CSV_FILE_OPEN_WRITE
  module procedure CSV_FILE_OPEN_WRITE_T

end interface CSV_OPEN_WRITE

interface CSV_CLOSE

  module procedure CSV_FILE_CLOSE
  module procedure CSV_FILE_CLOSE_T

end interface CSV_CLOSE

interface CSV_HEADER_WRITE

  module procedure CSV_FILE_HEADER_WRITE
  module procedure CSV_FILE_HEADER_WRITE_T

end interface CSV_HEADER_WRITE

interface CSV_RECORD_WRITE

  module procedure CSV_FILE_RECORD_WRITE
  module procedure CSV_FILE_RECORD_WRITE_T

end interface CSV_RECORD_WRITE

interface CHECK_FILE_OPEN

  module procedure CHECK_FILE_OPEN_S
  module procedure CHECK_FILE_OPEN_T

end interface CHECK_FILE_OPEN

interface CSV_GUESS_RECORD_LENGTH

  module procedure CSV_GUESS_RECORD_LEN_I4
  module procedure CSV_GUESS_RECORD_LEN_R4
  module procedure CSV_GUESS_RECORD_LEN_R8

end interface CSV_GUESS_RECORD_LENGTH

! They are identical in CSV_IO and BASE_UTILS. Private here to avoid possible
! name conflicts, do we need them outside?
private ::  I4_WIDTH, I4_LOG_10, STR_ITOA_LZ, STR_ITOA, CLEANUP

! This wrapper DEBUG LOG is used only for debugging this module. It may
! not or may use the module LOGGER, if not, (normally)  it can be used as a
! stand-alone module in other projects...
private ::  LOG_DBG

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
! NOTE: To use the LOGGER module, it must be compiled before CSV_IO, so
!       (re)place LOGGER first in the Makefile.
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

function CHECK_UNIT_VALID (file_unit) result (file_status)
!*******************************************************************************
! CHECK_UNIT_VALID
! PURPOSE: Checks if file unit is valid, that is within the allowed range and
!     doesn't include standard input/output/stderr units. The unit should
!     not necessarily be linked to any file or be an open file unit.
! CALL PARAMETERS:
!     integer file unit
! RETURNS:
!     logical status (true if valid)
!*******************************************************************************

  use, intrinsic :: ISO_FORTRAN_ENV     ! Provides system-wide scalar constants
                                        ! INPUT_UNIT OUTPUT_UNIT ERROR_UNIT
  implicit none

  ! Calling parameters
  integer, intent(in) :: file_unit
  logical :: file_status

  ! Subroutine name for DEBUG LOGGER
  character (len=*), parameter :: PROCNAME = "CHECK_UNIT_VALID"

  !-----------------------------------------------------------------------------

  if (file_unit > 0 .and. file_unit < MAX_UNIT .and.  &
      file_unit /= INPUT_UNIT .and.                   &
      file_unit /= OUTPUT_UNIT .and.                  &
      file_unit /= ERROR_UNIT ) then
    file_status = .TRUE.
  else
    file_status = .FALSE.
  end if

end function CHECK_UNIT_VALID

!-------------------------------------------------------------------------------

function GET_FREE_FUNIT (file_status, max_funit) result (file_unit)
!*******************************************************************************
! GET_FREE_FUNIT
! PURPOSE: returns the first free Fortran unit number (search in 1 to MAX_UNIT).
! RETURNS:
!    Integer unit number
! CALL PARAMETERS:
!    optional logical execution error status (.TRUE.)
!    optional integer max_funit to search (default MAX_UNIT defined in mudule)
!
! Author: John Burkardt : This code is distributed under the GNU LGPL license.
! Modified by Sergey Budaev
!*******************************************************************************

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
  character (len=*), parameter :: PROCNAME = "GET_FREE_FUNIT"

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

end function GET_FREE_FUNIT

!-------------------------------------------------------------------------------

function GET_FILE_UNIT (csv_file_name, csv_file_status) &
    result (csv_file_unit)
!*******************************************************************************
! GET_FILE_UNIT
! PURPOSE: returns file unit associated with an existing open file name,
!          if no file unit is associated with this name (file is not opened),
!          return unit=-1 and error status
! CALL PARAMETERS:
!    Character file name
!    Optional logical execution error status (.TRUE.)
! RETURNS: Integer file unit
!
! Author: Sergey Budaev
!*******************************************************************************

  implicit none

  ! Function value
  integer :: csv_file_unit

  ! Calling parameters
  character (len=*), intent(in) :: csv_file_name
  logical, optional, intent(out) :: csv_file_status

  ! Local variables, copies of optionals and intent-in
  character (len=:), allocatable :: csv_file_name_here
  logical :: csv_file_status_here

  ! Local variables
  integer :: file_error_status
  logical :: openedq

  ! Subroutine name for DEBUG LOGGER
  character (len=*), parameter :: PROCNAME = "GET_FILE_UNIT"

  !-----------------------------------------------------------------------------

  csv_file_name_here=csv_file_name   ! copy name as it is intent-in
  !csv_file_unit=-1                  ! this is default if no unit linked

  inquire(file=csv_file_name_here, number=csv_file_unit, opened=openedq, &
    iostat=file_error_status)

  ! Check if there were errors inquiring
  if (file_error_status==0) then
    csv_file_status_here=.TRUE.
  else
    csv_file_status_here=.FALSE.      ! there was an error inquiring, go back
    if (present(csv_file_status)) &   ! with error flag
      csv_file_status=csv_file_status_here
    csv_file_unit=-1
    return
  end if

  if (openedq) then                   ! Check if this file is opened
    csv_file_status_here=.TRUE.       ! and set error flag
  else
    csv_file_status_here=.FALSE.
  end if

  if (present(csv_file_status)) csv_file_status=csv_file_status_here

end function GET_FILE_UNIT

!-------------------------------------------------------------------------------

function CHECK_FILE_OPEN_S (csv_file_name, csv_file_unit, csv_file_status, &
    get_csv_file_unit) result (file_opened)
!*******************************************************************************
! CHECK_FILE_OPEN
! PURPOSE: Check if a file identified by name or unit is opened for
!          reading/writing, returns TRUE if opened, FALSE otherwise
!          If there was an error reading the file, returns
!          csv_file_status FALSE. Optionally also return the file unit
!          associated with the file name if it is open (-1 otherwise).
! CALL PARAMETERS:
!    Character file name or integer file unit,
!    Optional logical execution error status (.TRUE.) and
!    optional output file unit
! RETURNS: Logical flag
!
! Author: Sergey Budaev
!*******************************************************************************

  implicit none

  ! Function value
  logical :: file_opened ! file_exist

  ! Calling parameters
  character (len=*), optional, intent(in) :: csv_file_name
  integer, optional, intent(in) :: csv_file_unit
  logical, optional, intent(out) :: csv_file_status
  integer, optional, intent(out) :: get_csv_file_unit

  ! Local variables, copies of optionals and intent-in
  character (len=:), allocatable :: csv_file_name_here
  integer :: csv_file_unit_here
  logical :: csv_file_status_here
  integer :: get_csv_file_unit_here

  ! Local variables
  integer :: file_error_status
  logical :: openedq

  ! Subroutine name for DEBUG LOGGER
  character (len=*), parameter :: PROCNAME = "CHECK_FILE_OPEN"

  !-----------------------------------------------------------------------------

  ! exit if neither name nor unit given
  if(.not. present(csv_file_name) .and. .not. present(csv_file_unit)) then
    if (present(csv_file_status)) csv_file_status=.FALSE.
    if (present(get_csv_file_unit)) get_csv_file_unit=-1
    file_opened = .FALSE.
    return
  end if

  if (present(csv_file_unit)) then  ! if we got unit, just ignore the name
    csv_file_unit_here=csv_file_unit
    get_csv_file_unit_here=csv_file_unit_here
    inquire(unit=csv_file_unit_here, opened=openedq, iostat=file_error_status)
  else
    csv_file_name_here=csv_file_name
    inquire(file=csv_file_name_here, number=get_csv_file_unit_here, &
      opened=openedq, iostat=file_error_status)
  end if

  ! Check if there were errors inquiring
  if (file_error_status==0) then
    csv_file_status_here=.TRUE.
  else                               ! if there was an error inquiring, go back
    csv_file_status_here=.FALSE.     !   with error flag
    if (present(csv_file_status)) csv_file_status=csv_file_status_here
    if (present(get_csv_file_unit)) get_csv_file_unit=-1
    file_opened = .FALSE.
    return
  end if

  file_opened = openedq   ! get the logical flag

  if (.not. file_opened) get_csv_file_unit_here=-1

  if (present(get_csv_file_unit)) get_csv_file_unit=get_csv_file_unit_here
  if (present(csv_file_status)) csv_file_status=csv_file_status_here

end function CHECK_FILE_OPEN_S

!-------------------------------------------------------------------------------

function CHECK_FILE_OPEN_T (csv_file_handle) result (file_opened)
!*******************************************************************************
! CHECK_FILE_OPEN_T
! PURPOSE: Check if a file identified by name or unit is opened for
!          reading/writing, returns TRUE if opened, FALSE otherwise
! CALL PARAMETERS:
!    File handle object of the type csv_file
! RETURNS: Logical flag
!
! Author: Sergey Budaev
!*******************************************************************************

  implicit none

  ! Function value
  logical :: file_opened

  ! Calling parameters
  type(csv_file) :: csv_file_handle

  file_opened = CHECK_FILE_OPEN_S(csv_file_name=csv_file_handle%name, &
                                  csv_file_unit=csv_file_handle%unit, &
                                  csv_file_status=csv_file_handle%status)

end function CHECK_FILE_OPEN_T

!-------------------------------------------------------------------------------

subroutine CSV_FILE_OPEN_READ (csv_file_name, csv_file_unit, csv_file_status)
!*******************************************************************************
! CSV_FILE_OPEN_READ
! PURPOSE: opens a CSV file for reading. Optionally also sets a free file unit
!          for all subsequent input to this file.
! CALL PARAMETERS:
!    Character CSV_FILE_NAME, the name of the file.
!    Integer CSV_FILE_UNIT, file unit number (<1 then returns first free unit)
!    Logical CSV_FILE_STATUS, .TRUE. if successfull, no errors
! Author: John Burkardt : This code is distributed under the GNU LGPL license.
! Modified by Sergey Budaev
! USES: GET_FREE_FUNIT from the same module
! NOTES: Units 5 and 6 are usually for standard input and output (terminal)
!        F2003 uses INPUT_UNIT and OUTPUT_UNIT for that (portability).
!        The subroutine has no error handling itself, use CSV_FILE_STATUS for
!        checking the file write status.
!*******************************************************************************

  implicit none

  ! Calling parameters
  character (len=*), intent(in) :: csv_file_name
  integer :: csv_file_unit        ! intent(inout), doesn't work with literals
  logical, optional, intent(out) :: csv_file_status

  ! Local variables, copies of optionals
  logical :: csv_file_status_here

  ! Local variables
  integer :: file_error_status

  ! Subroutine name for DEBUG LOGGER
  character (len=*), parameter :: PROCNAME = "CSV_FILE_OPEN_READ"

  !-----------------------------------------------------------------------------

  ! First we get a free input unit if one is not provided (invalid, e.g. -1)
  if (.not. CHECK_UNIT_VALID(csv_file_unit)) then
      csv_file_unit=GET_FREE_FUNIT(csv_file_status_here, MAX_UNIT)
      if (.not. csv_file_status_here) then
        if(present(csv_file_status)) csv_file_status=csv_file_status_here
        return ! Return back (with error flag) if no units available
      end if
  end if

  open (unit=csv_file_unit, file=csv_file_name, status='old', &
    iostat=file_error_status)

  if (file_error_status==0) then
      csv_file_status_here=.TRUE.    ! No error
    else
      csv_file_status_here=.FALSE.   ! File error occurred
  end if

  if(present(csv_file_status)) csv_file_status=csv_file_status_here

end subroutine CSV_FILE_OPEN_READ

!-------------------------------------------------------------------------------

subroutine CSV_FILE_OPEN_READ_T (csv_file_handle)
!*******************************************************************************
! CSV_FILE_OPEN_READ_T
! PURPOSE: opens a CSV file for reading. Wrapper using file handle derived type
! CALL PARAMETERS:
!    csv file handle of the type csv_file defined in this module
! Author: Sergey Budaev
!*******************************************************************************

  implicit none

  ! Calling parameters
  type(csv_file), intent(inout) :: csv_file_handle

  call CSV_FILE_OPEN_READ( csv_file_handle%name, &
                           csv_file_handle%unit, &
                           csv_file_handle%status )

end subroutine CSV_FILE_OPEN_READ_T

!-------------------------------------------------------------------------------

subroutine CSV_FILE_OPEN_WRITE (csv_file_name, csv_file_unit, csv_file_status)
!*******************************************************************************
! CSV_FILE_OPEN_WRITE
! PURPOSE: opens a CSV file for writing. Optionally also sets a free file unit
!          for all subsequent output to this file.
! CALL PARAMETERS:
!    Character CSV_FILE_NAME, the name of the file.
!    Integer CSV_FILE_UNIT, file unit number (<1 then returns first free unit)
!    Logical CSV_FILE_STATUS, .TRUE. if successfull, no errors
! Author: John Burkardt : This code is distributed under the GNU LGPL license.
! Modified by Sergey Budaev
! USES: GET_FREE_FUNIT from the same module
! NOTES: Units 5 and 6 are usually for standard input and output (terminal)
!        F2003 uses INPUT_UNIT and OUTPUT_UNIT for that (portability).
!        The subroutine has no error handling itself, use CSV_FILE_STATUS for
!        checking the file write status.
!*******************************************************************************

  implicit none

  ! Calling parameters
  character (len=*), intent(in) :: csv_file_name
  integer :: csv_file_unit        ! intent(inout), doesn't work with literals
  logical, optional, intent(out) :: csv_file_status

  ! Local variables, copies of optionals
  logical :: csv_file_status_here

  ! Local variables
  integer :: file_error_status

  ! Subroutine name for DEBUG LOGGER
  character (len=*), parameter :: PROCNAME = "CSV_FILE_OPEN_WRITE"

  !-----------------------------------------------------------------------------

  ! First we get a free input unit if one is not provided (invalid, e.g. -1)
  if (.not. CHECK_UNIT_VALID(csv_file_unit)) then
      csv_file_unit=GET_FREE_FUNIT(csv_file_status_here, MAX_UNIT)
      if (.not. csv_file_status_here) then
        if(present(csv_file_status)) csv_file_status=csv_file_status_here
        return ! Return back (with error flag) if no units available
      end if
  end if

  open (unit=csv_file_unit, file=csv_file_name, status='replace', &
    iostat=file_error_status)

  if (file_error_status==0) then
      csv_file_status_here=.TRUE.    ! No error
    else
      csv_file_status_here=.FALSE.   ! File error occurred
  end if

  if(present(csv_file_status)) csv_file_status=csv_file_status_here

end subroutine CSV_FILE_OPEN_WRITE

!-------------------------------------------------------------------------------

subroutine CSV_FILE_OPEN_WRITE_T (csv_file_handle)
!*******************************************************************************
! CSV_FILE_OPEN_WRITE_T
! PURPOSE: opens a CSV file for writing. Wrapper using file handle derived type
! CALL PARAMETERS:
!    csv file handle of the type csv_file defined in this module
! Author: Sergey Budaev
!*******************************************************************************

  implicit none

  ! Calling parameters
  type(csv_file), intent(inout) :: csv_file_handle

  call CSV_FILE_OPEN_WRITE( csv_file_handle%name, &
                            csv_file_handle%unit, &
                            csv_file_handle%status )

end subroutine CSV_FILE_OPEN_WRITE_T

!-------------------------------------------------------------------------------

subroutine CSV_FILE_CLOSE (csv_file_name, csv_file_unit, csv_file_status)
!*******************************************************************************
! CSV_FILE_CLOSE
! PURPOSE: closes a CSV file for reading or writing.
! CALL PARAMETERS:
!    Character CSV_FILE_NAME, the name of the file.
!    Integer CSV_FILE_UNIT, file unit number (<1 then returns first free unit)
!    Logical CSV_FILE_STATUS, .TRUE. if successfull, no errors
! USES: GET_FILE_UNIT from the same module
! Author: John Burkardt : This code is distributed under the GNU LGPL license.
! Modified by Sergey Budaev
!*******************************************************************************

  implicit none

  ! Calling parameters
  character (len=*), optional, intent(in) :: csv_file_name
  integer, optional :: csv_file_unit ! intent(inout), doesn't work with literals
  logical, optional, intent(out) :: csv_file_status

  ! Local variables, copies of optionals
  integer :: csv_file_unit_here
  logical:: csv_file_status_here

  ! Local variables
  integer :: file_error_status

  ! Subroutine name for DEBUG LOGGER
  character (len=*), parameter :: PROCNAME = "CSV_FILE_CLOSE"

  !-----------------------------------------------------------------------------

  csv_file_status_here=.FALSE.

  if (.not. present(csv_file_unit)) then
    if (present(csv_file_name)) then
      ! determine file unit from existing file name
      csv_file_unit_here=GET_FILE_UNIT(csv_file_name, &
          csv_file_status_here)
      if (.not. csv_file_status_here) then      ! if there was error
        if (present(csv_file_status)) &         ! inquiring file status,
          csv_file_status=.FALSE.               ! return with error flag
        return
      end if
    else !if (.not. present(csv_file_name))
      ! neither file name nor unit specified, set error status
      if (present(csv_file_status)) csv_file_status=.FALSE.
      return
    end if
  else
    ! file unit present in the list of arguments
    csv_file_unit_here=csv_file_unit
  end if

  close (unit = csv_file_unit_here, iostat=file_error_status)

  ! Check IO errors and report back if optional args are present
  if (file_error_status==0) then
    if (present(csv_file_status)) csv_file_status=.TRUE.  ! No error
  else
    if (present(csv_file_status)) csv_file_status=.FALSE. ! File error
  end if

end subroutine CSV_FILE_CLOSE

!-------------------------------------------------------------------------------

subroutine CSV_FILE_CLOSE_T (csv_file_handle)
!*******************************************************************************
! CSV_FILE_CLOSE_T
! PURPOSE: closes a CSV file for reading or writing. Wrapper using file handle
!    derived type
! CALL PARAMETERS:
!    csv file handle of the type csv_file defined in this module
! Author: Sergey Budaev
!*******************************************************************************

  implicit none

  ! Calling parameters
  type(csv_file), intent(inout) :: csv_file_handle

  call CSV_FILE_CLOSE ( csv_file_handle%name, &
                        csv_file_handle%unit, &
                        csv_file_handle%status )

end subroutine CSV_FILE_CLOSE_T

!-------------------------------------------------------------------------------

subroutine CSV_FILE_HEADER_WRITE (csv_file_name, csv_file_unit, header, &
      csv_file_status)
!*******************************************************************************
! CSV_FILE_HEADER_WRITE
! PURPOSE: writes a header to a CSV file. Should normally be the first line.
! CALL PARAMETERS:
!    Optional character CSV_FILE_NAME, the name of the file.
!    Optional integer CSV_FILE_UNIT, file unit number
!    Cherecter HEADER
!    Optional logical CSV_FILE_STATUS, .TRUE. if successfull, no errors
! USES: GET_FILE_UNIT from the same module
! Author: John Burkardt : This code is distributed under the GNU LGPL license.
! Modified by Sergey Budaev
!*******************************************************************************

  implicit none

  ! Calling parameters
  character (len=*), optional, intent(in) :: csv_file_name
  integer, optional :: csv_file_unit ! intent(inout), doesn't work with literals
  character (len=*), intent(in) :: header
  logical, optional, intent(out) :: csv_file_status

  ! Local variables, copies of optionals
  integer :: csv_file_unit_here
  logical :: csv_file_status_here

  ! Local variables
  integer :: file_error_status

  ! Subroutine name for DEBUG LOGGER
  character (len=*), parameter :: PROCNAME = "CSV_FILE_HEADER_WRITE"

  !-----------------------------------------------------------------------------

  if (.not. present(csv_file_unit)) then
    if (present(csv_file_name)) then
      ! determine file unit from existing file name
      csv_file_unit_here=GET_FILE_UNIT(csv_file_name, &
          csv_file_status_here)
      if (.not. csv_file_status_here) then      ! if there was error
        if (present(csv_file_status)) &         ! inquiring file status,
          csv_file_status=.FALSE.               ! return with error flag
        return
      end if
    else !if (.not. present(csv_file_name))
      ! neither file name nor unit specified, set error status
      if (present(csv_file_status)) csv_file_status=.FALSE.
      return
    end if
  else
    ! file unit present in the list of arguments
    csv_file_unit_here=csv_file_unit
  end if

  write (unit=csv_file_unit_here, fmt='(a)', &
      iostat=file_error_status) trim(header)

  ! Check IO errors
  if (file_error_status==0) then
      if (present(csv_file_status)) csv_file_status=.TRUE.  ! No error
    else
      if (present(csv_file_status)) csv_file_status=.FALSE. ! File error
  end if

end subroutine CSV_FILE_HEADER_WRITE

!-------------------------------------------------------------------------------

subroutine CSV_FILE_HEADER_WRITE_T(header, csv_file_handle)
!*******************************************************************************
! CSV_FILE_HEADER_WRITE_T
! PURPOSE: writes a header to a CSV file. Should normally be the first line.
!    Wrapper using file handle derived type
! CALL PARAMETERS:
!    arbitrary string header
!    csv file handle of the type csv_file defined in this module
! Author: Sergey Budaev
!*******************************************************************************

  implicit none

  ! Calling parameters
  character (len=*), intent(in) :: header
  type(csv_file), intent(inout) :: csv_file_handle


  call CSV_FILE_HEADER_WRITE ( csv_file_handle%name, &
                               csv_file_handle%unit, &
                               header, &
                               csv_file_handle%status )

end subroutine CSV_FILE_HEADER_WRITE_T

!-------------------------------------------------------------------------------

function CSV_FILE_LINES_COUNT (csv_file_name, numeric_only, csv_file_status) &
        result (line_num)
!*******************************************************************************
! CSV_FILE_LINE_COUNT
! PURPOSE: counts the number of lines in a CSV file.
! CALL PARAMETERS:
!    Optional character CSV_FILE_NAME, the name of the file.
!    Optional flag indicating whether to count only numeric lines (if .TRUE.)
!        (numeric are only lines including digits, spaces, comma and colon);
!    Optional Optional logical CSV_FILE_STATUS, .TRUE. if no errors
! USES: GET_FREE_FUNIT from the same module
! NOTE: This routine does not try to distinguish the possible header line,
!    blank lines, or cases where a single CSV record extends over multiple
!    lines.  It simply counts the number of lines.
! Author: John Burkardt : This code is distributed under the GNU LGPL license.
! Modified by Sergey Budaev
!*******************************************************************************

  implicit none

  ! Function value
  integer :: line_num

  ! Calling parameters
  character (len=*), intent(in)  :: csv_file_name
  logical, optional, intent(in)  :: numeric_only
  logical, optional, intent(out) :: csv_file_status

  ! Local variables, copies of optionals
  logical :: numeric_only_here
  logical :: csv_file_status_here

  ! Local variables
  integer :: ierror
  integer :: input_status
  integer :: input_unit
  !character (len=2) :: line   ! We only need to count rows here
  character (len=:), allocatable :: line

  ! Subroutine name for DEBUG LOGGER
  character (len=*), parameter :: PROCNAME = "CSV_FILE_LINES_COUNT"

  !-----------------------------------------------------------------------------

  if(present(numeric_only)) then
    numeric_only_here = numeric_only
  else
    numeric_only_here = .FALSE.
  end if

  ! May need initialise logical on some platforms/compilers
  if (present(csv_file_status)) csv_file_status = .TRUE.

  line_num = -1

  input_unit=GET_FREE_FUNIT(csv_file_status_here, MAX_UNIT)
  if (.not. csv_file_status_here) then
    if (present(csv_file_status)) csv_file_status=.FALSE.
    return ! Return back (with error flag) if no units available
  end if

  open (unit=input_unit, file=csv_file_name, status='old', &
    iostat=input_status )

  ! check if we can open the file, if not, issue error flag and go back
  if ( input_status /= 0 ) then
    if (present(csv_file_status)) csv_file_status=.FALSE.
    return
  end if

  line_num = 0

  do while ( READLINE(input_unit, line, .TRUE.) )
    if (numeric_only_here) then
      if (IS_NUMERIC(line, .TRUE.)) line_num=line_num+1
    else
      line_num = line_num + 1
    end if
  end do

  close (unit=input_unit)

  if (present(csv_file_status)) csv_file_status=.TRUE.

end function CSV_FILE_LINES_COUNT

!-------------------------------------------------------------------------------

subroutine CSV_FILE_RECORD_WRITE (csv_file_name, csv_file_unit, record, &
      csv_file_status)
!*******************************************************************************
! CSV_FILE_RECORD_WRITE
! PURPOSE:  writes a complete record to a CSV file. A record is a single row.
! CALL PARAMETERS:
!    Character CSV_FILE_NAME, the name of the file.
!    Integer CSV_FILE_UNIT, the unit number
!    Character RECORD, the complete record.
! NOTES:
! Author: John Burkardt : This code is distributed under the GNU LGPL license.
! Modified by Sergey Budaev
!*******************************************************************************

  implicit none

  ! Calling parameters
  character (len=*), optional, intent(in) :: csv_file_name
  integer, optional :: csv_file_unit ! intent(inout), doesn't work with literals
  character (len=*), intent(in)  :: record
  logical, optional, intent(out) :: csv_file_status

  ! Local variables, copies of optionals
  integer :: csv_file_unit_here
  logical  :: csv_file_status_here

  ! Local variables
  integer :: file_error_status

  ! Subroutine name for DEBUG LOGGER
  character (len=*), parameter :: PROCNAME = "CSV_FILE_RECORD_WRITE"

  !-----------------------------------------------------------------------------

  if (.not. present(csv_file_unit)) then
    if (present(csv_file_name)) then
      ! determine file unit from existing file name
      csv_file_unit_here=GET_FILE_UNIT(csv_file_name, &
          csv_file_status_here)
      if (.not. csv_file_status_here) then      ! if there was error
        if (present(csv_file_status)) &         ! inquiring file status,
          csv_file_status=.FALSE.               ! return with error flag
        return
      end if
    else !if (.not. present(csv_file_name))
      ! neither file name nor unit specified, set error status
      if (present(csv_file_status)) csv_file_status=.FALSE.
      return
    end if
  else
    ! file unit present in the list of arguments
    csv_file_unit_here=csv_file_unit
  end if

  write (unit=csv_file_unit_here, fmt='(a)', iostat=file_error_status) trim(record)

  ! Check IO errors and report back if optional args are present
  if (file_error_status==0) then
    if (present(csv_file_status)) csv_file_status=.TRUE.  ! No error
  else
    if (present(csv_file_status)) csv_file_status=.FALSE. ! File error
  end if

end subroutine CSV_FILE_RECORD_WRITE

!-------------------------------------------------------------------------------

subroutine CSV_FILE_RECORD_WRITE_T (record, csv_file_handle)
!*******************************************************************************
! CSV_FILE_RECORD_WRITE_T
! PURPOSE: writes a complete record to a CSV file. Wrapper using file handle
!    derived type
! CALL PARAMETERS:
!    Character RECORD, the complete record.
!    csv file handle of the type csv_file defined in this module
! Author: Sergey Budaev
!*******************************************************************************

  implicit none

  ! Calling parameters
  character (len=*), intent(in)  :: record
  type(csv_file), intent(inout) :: csv_file_handle

  call CSV_FILE_RECORD_WRITE ( csv_file_handle%name, &
                               csv_file_handle%unit, &
                               record, &
                               csv_file_handle%status )

end subroutine CSV_FILE_RECORD_WRITE_T

!-------------------------------------------------------------------------------

function CSV_RECORD_SIZE (record, csv_record_status) result (value_count)
!*******************************************************************************
! CSV_RECORD_SIZE
! PURPOSE: counts the number of values in a CSV record.
!    A record is a single row.
! CALL PARAMETERS:
!   Character CSV record
!   Optional Integer number of quoted characters
! RETURNS:
!   Integer count of values in the CSV record
! NOTES:
! Author: John Burkardt : This code is distributed under the GNU LGPL license.
! Modified by Sergey Budaev
!*******************************************************************************

  implicit none

  ! Function value
  integer :: value_count

  ! Calling parameters
  character (len=*), intent(in)  :: record
  integer, optional, intent(out) :: csv_record_status

  ! Local variables, copies of optionals
  integer :: csv_record_status_here

  ! Local variables
  character :: csv_char
  character :: csv_char_old
  integer :: csv_len
  integer :: csv_loc
  character :: TAB=achar(9)
  integer :: word_length

  ! Subroutine name for DEBUG LOGGER
  character (len=*), parameter :: PROCNAME = "CSV_RECORD_SIZE"

  !-----------------------------------------------------------------------------

  value_count = 0             ! zero counter
  csv_record_status_here = 0  ! begin in "unquoted" status.

  ! How many characters in the record?
  csv_len = len_trim ( record )

  ! Count number of characters in each word.
  word_length = 0

  ! Consider each character.
  csv_char_old = ','

  do csv_loc = 1, csv_len
    csv_char = record(csv_loc:csv_loc)
    ! Each comma divides one value from another.
    if ( csv_char_old == ',' ) then
      value_count = value_count + 1
      word_length = 0!
    ! For quotes, try using CSV_RECORD_STATUS to count the number of
    ! quoted characters.
    else if ( csv_char == '"' ) then
      if ( 0 < csv_record_status_here ) then
        csv_record_status_here = 0
      else
        csv_record_status_here = csv_record_status_here + 1
      end if!
    ! Ignore blanks
    else if ( csv_char == ' ' .or. csv_char == TAB ) then
    ! Add character to length of word.
    else
      word_length = word_length + 1
      if ( value_count == 0 ) then
        value_count = 1
      end if
    end if

    csv_char_old = csv_char

  end do

  if (present(csv_record_status)) &             ! return number of strings
        csv_record_status=csv_record_status_here

end function CSV_RECORD_SIZE

!-------------------------------------------------------------------------------

subroutine CSV_RECORD_APPEND_I4 (record, avalue)
!*******************************************************************************
! CSV_RECORD_APPEND_I4
! PURPOSE: appends an Integer kind 4 (I4) to a CSV record.
! CALL PARAMETERS:
!    Character RECORD, the CSV record.
!    Integer to be appended
! USES: I4_WIDTH from the same module
!
! NOTES:
!   The record parameter does not accept allocatable strings, only standard
!   fixed length strings (e.g. character (len=255) :: RECORD_CSV). But there is
!   a little trick to use allocatable strings:
!
!   (1) declare allocatable strings
!         character (len=:), allocatable :: RECORD_CSV
!   (2) allocate/ initialise an empty string of the necessary size for the
!       first record using intrinsic function repeat:
!         RECORD_CSV=repeat(" ", 255) or assess and set the maximum size
!       of array record in advance using
!         size_of_array * (n_characters_per_number + 1), may use len(TOSTR())
!         from BASE_UTILS module to get the number of characters per field.
!         RECORD_CSV=repeat( " ", max_size_record )
!       see CSV_MATRIX_WRITE_ as a model for calculating max_size_record;
!   (3) append numbers to this initially long empty string (NOT zero-length
!       allocatable string "":
!         call CSV_RECORD_APPEND( RECORD_CSV, numbers(1) )
!         call CSV_RECORD_APPEND( RECORD_CSV, numbers(2) )
!         call CSV_RECORD_APPEND( RECORD_CSV, numbers(3) )
!
! Author: John Burkardt : This code is distributed under the GNU LGPL license.
! Modified by Sergey Budaev
!*******************************************************************************

  implicit none

  ! Calling parameters
  character (len=*), intent(inout) :: record
  integer, intent(in) :: avalue       ! was i4

  ! Local variables
  character (len=5) :: fmat
  integer :: i
  integer :: i4_len

  ! Subroutine name for DEBUG LOGGER
  character (len=*), parameter :: PROCNAME = "CSV_RECORD_APPEND_I4"

  !-----------------------------------------------------------------------------

  ! TODO: Check unallocated/uninitialised records
  !if (len_trim(record)==0) record=""   ! uninitialised/unalloc record to blanks

  i = len_trim (record)                 ! Locate last used location in RECORD

  if ( 0 < i ) then                           ! Append comma
    i = i + 1
    record(i:i) = ','
  end if

  i4_len = I4_WIDTH (avalue)                      ! Determine "width" of I4
  write (fmat, '(a,i2,a)') '(i', i4_len, ')'      ! Create format for I4
  write (record(i+1:i+i4_len), fmat) avalue       ! Write I4 to RECORD

end subroutine CSV_RECORD_APPEND_I4

!-------------------------------------------------------------------------------

subroutine CSV_RECORD_APPEND_R4 (record, avalue)
!*******************************************************************************
! CSV_RECORD_APPEND_R4
! PURPOSE: appends a Real kind 4 (R4) to a CSV record.
! CALL PARAMETERS:
!    Character RECORD, the CSV record.
!    real to be appended
! USES: I4_WIDTH from the same module
!
! NOTES:
!   The record parameter does not accept allocatable strings, only standard
!   fixed length strings (e.g. character (len=255) :: RECORD_CSV). But there is
!   a little trick to use allocatable strings:
!
!   (1) declare allocatable strings
!         character (len=:), allocatable :: RECORD_CSV
!   (2) allocate/ initialise an empty string of the necessary size for the
!       first record using intrinsic function repeat:
!         RECORD_CSV=repeat(" ", 255) or assess and set the maximum size
!       of array record in advance using
!         size_of_array * (n_characters_per_number + 1), may use len(TOSTR())
!         from BASE_UTILS module to get the number of characters per field.
!         RECORD_CSV=repeat( " ", max_size_record )
!       see CSV_MATRIX_WRITE_ as a model for calculating max_size_record;
!   (3) append numbers to this initially long empty string (NOT zero-length
!       allocatable string "":
!         call CSV_RECORD_APPEND( RECORD_CSV, numbers(1) )
!         call CSV_RECORD_APPEND( RECORD_CSV, numbers(2) )
!         call CSV_RECORD_APPEND( RECORD_CSV, numbers(3) )
!
! Author: John Burkardt : This code is distributed under the GNU LGPL license.
! Modified by Sergey Budaev
!*******************************************************************************

  implicit none

  ! Calling parameters
  character (len=*), intent(inout) :: record
  real, intent(in) :: avalue          ! was r4

  ! Local variables
  character (len=5) :: fmat
  integer :: i
  integer :: i4
  integer :: i4_len

  ! Subroutine name for DEBUG LOGGER
  character (len=*), parameter :: PROCNAME = "CSV_RECORD_APPEND_R4"

  !-----------------------------------------------------------------------------

  ! TODO: Check unallocated/uninitialised records

  i = len_trim (record)               ! Locate last used location in RECORD

  if ( 0 < i ) then                   ! Append comma
    i = i + 1
    record(i:i) = ','
  end if

  if ( avalue == 0.0E+00 ) then
    i = i + 1
    record(i:i) = '0'
  else if ( avalue == real(int(avalue)) ) then
    i4 = int(avalue)
    i4_len = I4_WIDTH(i4)
    write(fmat, '(a,i2,a)') '(i', i4_len, ')'
    write(record(i+1:i+i4_len), fmat) i4
  else
    write (record(i+1:i+14), '(g14.6)') avalue
  end if

end subroutine CSV_RECORD_APPEND_R4

!-------------------------------------------------------------------------------

subroutine CSV_RECORD_APPEND_R8 (record, avalue)
!*******************************************************************************
! CSV_RECORD_APPEND_R8
! PURPOSE: appends a Double precision Real kind 8 (R8) to a CSV record.
! CALL PARAMETERS:
!    Character RECORD, the CSV record.
!    real, kind 8, to be appended
! USES: I4_WIDTH from the same module
!
! NOTES:
!   The record parameter does not accept allocatable strings, only standard
!   fixed length strings (e.g. character (len=255) :: RECORD_CSV). But there is
!   a little trick to use allocatable strings:
!
!   (1) declare allocatable strings
!         character (len=:), allocatable :: RECORD_CSV
!   (2) allocate/ initialise an empty string of the necessary size for the
!       first record using intrinsic function repeat:
!         RECORD_CSV=repeat(" ", 255) or assess and set the maximum size
!       of array record in advance using
!         size_of_array * (n_characters_per_number + 1), may use len(TOSTR())
!         from BASE_UTILS module to get the number of characters per field.
!         RECORD_CSV=repeat( " ", max_size_record )
!       see CSV_MATRIX_WRITE_ as a model for calculating max_size_record;
!   (3) append numbers to this initially long empty string (NOT zero-length
!       allocatable string "":
!         call CSV_RECORD_APPEND( RECORD_CSV, numbers(1) )
!         call CSV_RECORD_APPEND( RECORD_CSV, numbers(2) )
!         call CSV_RECORD_APPEND( RECORD_CSV, numbers(3) )
!
! Author: John Burkardt : This code is distributed under the GNU LGPL license.
! Modified by Sergey Budaev
!*******************************************************************************

  implicit none

  ! Calling parameters
  character (len=*), intent(inout) :: record
  real (kind=8), intent(in) :: avalue ! was r8

  ! Local variables
  character (len=5) :: fmat
  integer :: i
  integer :: i4
  integer :: i4_len

  ! Subroutine name for DEBUG LOGGER
  character (len=*), parameter :: PROCNAME = "CSV_RECORD_APPEND_R8"

  !-----------------------------------------------------------------------------

  ! TODO: check for unallocated/uninitialised records

  i = len_trim (record)               ! Locate last used location in RECORD.

  if ( 0 < i ) then                   ! Append comma
    i = i + 1
    record(i:i) = ','
  end if

  if ( avalue == 0.0D+00 ) then
    i = i + 1
    record(i:i) = '0'
  else if ( avalue == real(int(avalue), kind=8) ) then
    i4 = int(avalue)
    i4_len = i4_width ( i4 )
    write (fmat, '(a,i2,a)') '(i', i4_len, ')'
    write (record(i+1:i+i4_len), fmat) i4
  else
    write (record(i+1:i+14), '(g14.6)') avalue
  end if

end subroutine CSV_RECORD_APPEND_R8

!-------------------------------------------------------------------------------

subroutine CSV_RECORD_APPEND_S (record, avalue)
!*******************************************************************************
! CSV_RECORD_APPEND_R8
! PURPOSE: appends a string to a CSV record.
! CALL PARAMETERS:
!    Character RECORD, the CSV record.
!    Character string to be appended
!
! NOTES:
!   The record parameter does not accept allocatable strings, only standard
!   fixed length strings (e.g. character (len=255) :: RECORD_CSV). But there is
!   a little trick to use allocatable strings:
!
!   (1) declare allocatable strings
!         character (len=:), allocatable :: RECORD_CSV
!   (2) allocate/ initialise an empty string of the necessary size for the
!       first record using intrinsic function repeat:
!         RECORD_CSV=repeat(" ", 255) or assess and set the maximum size
!       of array record in advance using
!         size_of_array * (n_characters_per_number + 1), may use len(TOSTR())
!         from BASE_UTILS module to get the number of characters per field.
!         RECORD_CSV=repeat( " ", max_size_record )
!       see CSV_MATRIX_WRITE_ as a model for calculating max_size_record;
!   (3) append numbers to this initially long empty string (NOT zero-length
!       allocatable string "":
!         call CSV_RECORD_APPEND( RECORD_CSV, numbers(1) )
!         call CSV_RECORD_APPEND( RECORD_CSV, numbers(2) )
!         call CSV_RECORD_APPEND( RECORD_CSV, numbers(3) )
!
! Author: John Burkardt : This code is distributed under the GNU LGPL license.
! Modified by Sergey Budaev
!*******************************************************************************

  implicit none

  ! Calling parameters
  character (len=*), intent(inout) :: record
  character (len=*), intent(in) :: avalue  ! was s

  ! Local variables
  integer :: i
  integer :: s_len

  ! Subroutine name for DEBUG LOGGER
  character (len=*), parameter :: PROCNAME = "CSV_RECORD_APPEND_S"

  !-----------------------------------------------------------------------------

  ! TODO: Check unallocated/uninitialised records

  i = len_trim (record)               ! Locate last used location in RECORD.

  if ( 0 < i ) then                   ! Append a comma
    i = i + 1
    record(i:i) = ','
  end if

  i = i + 1                           ! Prepend a quote
  record(I:i) = '"'

  s_len = len_trim(avalue)            ! Write S to RECORD
  record(i+1:i+s_len) = avalue(1:s_len)
  i = i + s_len

  i = i + 1                           ! Postpend a quote
  record(i:i) = '"'

end subroutine CSV_RECORD_APPEND_S

!-------------------------------------------------------------------------------

subroutine CSV_RECORD_APPEND_ARRAY_I4 (record, array)
!*******************************************************************************
! CSV_RECORD_APPEND_ARRAY_I4
! PURPOSE: appends an array of integers to a CSV record.
! CALL PARAMETERS:
!    Character RECORD, the CSV record.
!    Integer array to be appended
! NOTE: Whole array is appended, without specifying upper and lower
!    boundaries, use slices if necessary
! Author: Sergey Budaev
!*******************************************************************************

  implicit none

  ! Calling parameters
  character (len=*), intent(inout) :: record
  integer, dimension(:), intent(in) :: array

  ! Local variables
  integer :: i, LBndi, Ubndi

  ! Local variables, copies of optionals
  character (len=len(record)) :: record_here

  !-----------------------------------------------------------------------------

  LBndi=lbound(array, 1)   ! Determining bounds for out array
  UBndi=ubound(array, 1)

  record_here = record

  do i=LBndi, UBndi
    call CSV_RECORD_APPEND_I4(record_here,array(i))
  end do

  record = record_here

end subroutine CSV_RECORD_APPEND_ARRAY_I4

!-------------------------------------------------------------------------------

subroutine CSV_RECORD_APPEND_ARRAY_R4 (record, array)
!*******************************************************************************
! CSV_RECORD_APPEND_ARRAY_R4
! PURPOSE: appends an array of integers to a CSV record.
! CALL PARAMETERS:
!    Character RECORD, the CSV record.
!    Real array to be appended
! NOTE: Whole array is appended, without specifying upper and lower
!    boundaries, use slices if necessary
! Author: Sergey Budaev
!*******************************************************************************

  implicit none

  ! Calling parameters
  character (len=*), intent(inout) :: record
  real, dimension(:), intent(in) :: array

  ! Local variables
  integer :: i, LBndi, Ubndi

  ! Local variables, copies of optionals
  character (len=len(record)) :: record_here

  !-----------------------------------------------------------------------------

  LBndi=lbound(array, 1)   ! Determining bounds for out array
  UBndi=ubound(array, 1)

  record_here = record

  do i=LBndi, UBndi
    call CSV_RECORD_APPEND_R4(record_here,array(i))
  end do

  record = record_here

end subroutine CSV_RECORD_APPEND_ARRAY_R4

!-------------------------------------------------------------------------------

subroutine CSV_RECORD_APPEND_ARRAY_R8 (record, array)
!*******************************************************************************
! CSV_RECORD_APPEND_ARRAY_R8
! PURPOSE: appends an array of integers to a CSV record.
! CALL PARAMETERS:
!    Character RECORD, the CSV record.
!    Real (kind 8, double) array to be appended
! NOTE: Whole array is appended, without specifying upper and lower
!    boundaries, use slices if necessary
! Author: Sergey Budaev
!*******************************************************************************

  implicit none

  ! Calling parameters
  character (len=*), intent(inout) :: record
  real (kind=8), dimension(:), intent(in) :: array

  ! Local variables
  integer :: i, LBndi, Ubndi

  ! Local variables, copies of optionals
  character (len=len(record)) :: record_here

  !-----------------------------------------------------------------------------

  LBndi=lbound(array, 1)   ! Determining bounds for out array
  UBndi=ubound(array, 1)

  record_here = record

  do i=LBndi, UBndi
    call CSV_RECORD_APPEND_R8(record_here,array(i))
  end do

  record = record_here

end subroutine CSV_RECORD_APPEND_ARRAY_R8

!-------------------------------------------------------------------------------

subroutine CSV_RECORD_APPEND_ARRAY_S (record, array)
!*******************************************************************************
! CSV_RECORD_APPEND_ARRAY_S
! PURPOSE: appends an array of integers to a CSV record.
! CALL PARAMETERS:
!    Character RECORD, the CSV record.
!    Character array to be appended
! NOTE: Whole array is appended, without specifying upper and lower
!    boundaries, use slices if necessary
! Author: Sergey Budaev
!*******************************************************************************

  implicit none

  ! Calling parameters
  character (len=*), intent(inout) :: record
  character (len=*), dimension(:), intent(in) :: array

  ! Local variables
  integer :: i, LBndi, Ubndi

  ! Local variables, copies of optionals
  character (len=len(record)) :: record_here

  !-----------------------------------------------------------------------------

  LBndi=lbound(array, 1)   ! Determining bounds for out array
  UBndi=ubound(array, 1)

  record_here = record

  do i=LBndi, UBndi
    call CSV_RECORD_APPEND_S(record_here,array(i))
  end do

  record = record_here

end subroutine CSV_RECORD_APPEND_ARRAY_S

!-------------------------------------------------------------------------------

subroutine CSV_RECORD_APPEND_LST_I4 (record, &
                                      s1, s2, s3, s4, s5, s6, s7, s8, s9,s10,&
                                     s11,s12,s13,s14,s15,s16,s17,s18,s19,s20,&
                                     s21,s22,s23,s24,s25,s26,s27,s28,s29,s30,&
                                     s31,s32,s33,s34,s35,s36,s37,s38,s39,s40,&
                                     s41,s42,s43,s44,s45,s46,s47,s48,s49,s50,&
                                     s51,s52,s53,s54,s55,s56,s57,s58,s59,s60,&
                                     s61,s62,s63,s64,s65,s66,s67,s68,s69,s70,&
                                     s71,s72,s73,s74,s75,s76,s77,s78,s79,s80,&
                                     s81,s82,s83,s84,s85,s86,s87,s88,s89,s90,&
                                     s91,s92,s93,s94,s95,s96,s97,s98,s99,s100 )
!*******************************************************************************
! CSV_RECORD_APPEND_LST_I4
! PURPOSE: appends a list of strings to a CSV record.
! CALL PARAMETERS:
!    Character RECORD, the CSV record.
!    Optional list of integers to be appended, at least two values
!
! NOTES:
!   This is a rudimentary implementation making use of optional parameters,
!   with maximum 100. It should be enough for most cases where the purpose
!   of this subroutine is to construct header line with variable names
!   in a single call instead of multiple CSV_RECORD_APPEND calls. Should be
!   rewritten to use detived types and pointers for arbitrary list size.
!   But for now (in this version) the main aim is implementation and
!   debugging simplicity. May be worth appending more strings... Is 200 enough?
!
! TODO: Make also the same function, in addition to sub with generic interface
! Author: Sergey Budaev
!*******************************************************************************

  implicit none

  ! Calling parameters, first record itself, then 100 list items
  character (len=*), intent(inout) :: record

  integer, intent(in) :: s1  ! The first two strings in the list are
  integer, intent(in) :: s2  ! mandatory to make it unique in the
                                       ! generic interface block

  integer, optional, intent(in) :: s3
  integer, optional, intent(in) :: s4
  integer, optional, intent(in) :: s5
  integer, optional, intent(in) :: s6
  integer, optional, intent(in) :: s7
  integer, optional, intent(in) :: s8
  integer, optional, intent(in) :: s9
  integer, optional, intent(in) :: s10
  integer, optional, intent(in) :: s11
  integer, optional, intent(in) :: s12
  integer, optional, intent(in) :: s13
  integer, optional, intent(in) :: s14
  integer, optional, intent(in) :: s15
  integer, optional, intent(in) :: s16
  integer, optional, intent(in) :: s17
  integer, optional, intent(in) :: s18
  integer, optional, intent(in) :: s19
  integer, optional, intent(in) :: s20
  integer, optional, intent(in) :: s21
  integer, optional, intent(in) :: s22
  integer, optional, intent(in) :: s23
  integer, optional, intent(in) :: s24
  integer, optional, intent(in) :: s25
  integer, optional, intent(in) :: s26
  integer, optional, intent(in) :: s27
  integer, optional, intent(in) :: s28
  integer, optional, intent(in) :: s29
  integer, optional, intent(in) :: s30
  integer, optional, intent(in) :: s31
  integer, optional, intent(in) :: s32
  integer, optional, intent(in) :: s33
  integer, optional, intent(in) :: s34
  integer, optional, intent(in) :: s35
  integer, optional, intent(in) :: s36
  integer, optional, intent(in) :: s37
  integer, optional, intent(in) :: s38
  integer, optional, intent(in) :: s39
  integer, optional, intent(in) :: s40
  integer, optional, intent(in) :: s41
  integer, optional, intent(in) :: s42
  integer, optional, intent(in) :: s43
  integer, optional, intent(in) :: s44
  integer, optional, intent(in) :: s45
  integer, optional, intent(in) :: s46
  integer, optional, intent(in) :: s47
  integer, optional, intent(in) :: s48
  integer, optional, intent(in) :: s49
  integer, optional, intent(in) :: s50
  integer, optional, intent(in) :: s51
  integer, optional, intent(in) :: s52
  integer, optional, intent(in) :: s53
  integer, optional, intent(in) :: s54
  integer, optional, intent(in) :: s55
  integer, optional, intent(in) :: s56
  integer, optional, intent(in) :: s57
  integer, optional, intent(in) :: s58
  integer, optional, intent(in) :: s59
  integer, optional, intent(in) :: s60
  integer, optional, intent(in) :: s61
  integer, optional, intent(in) :: s62
  integer, optional, intent(in) :: s63
  integer, optional, intent(in) :: s64
  integer, optional, intent(in) :: s65
  integer, optional, intent(in) :: s66
  integer, optional, intent(in) :: s67
  integer, optional, intent(in) :: s68
  integer, optional, intent(in) :: s69
  integer, optional, intent(in) :: s70
  integer, optional, intent(in) :: s71
  integer, optional, intent(in) :: s72
  integer, optional, intent(in) :: s73
  integer, optional, intent(in) :: s74
  integer, optional, intent(in) :: s75
  integer, optional, intent(in) :: s76
  integer, optional, intent(in) :: s77
  integer, optional, intent(in) :: s78
  integer, optional, intent(in) :: s79
  integer, optional, intent(in) :: s80
  integer, optional, intent(in) :: s81
  integer, optional, intent(in) :: s82
  integer, optional, intent(in) :: s83
  integer, optional, intent(in) :: s84
  integer, optional, intent(in) :: s85
  integer, optional, intent(in) :: s86
  integer, optional, intent(in) :: s87
  integer, optional, intent(in) :: s88
  integer, optional, intent(in) :: s89
  integer, optional, intent(in) :: s90
  integer, optional, intent(in) :: s91
  integer, optional, intent(in) :: s92
  integer, optional, intent(in) :: s93
  integer, optional, intent(in) :: s94
  integer, optional, intent(in) :: s95
  integer, optional, intent(in) :: s96
  integer, optional, intent(in) :: s97
  integer, optional, intent(in) :: s98
  integer, optional, intent(in) :: s99
  integer, optional, intent(in) :: s100

  ! Local variables, copies of optionals
  character (len=len(record)) :: record_here

  !-----------------------------------------------------------------------------

  record_here = record

  call CSV_RECORD_APPEND_I4(record_here,   s1)
  call CSV_RECORD_APPEND_I4(record_here,   s2)

  if (present(s3))  call CSV_RECORD_APPEND_I4(record_here,  s3)
  if (present(s4))  call CSV_RECORD_APPEND_I4(record_here,  s4)
  if (present(s5))  call CSV_RECORD_APPEND_I4(record_here,  s5)
  if (present(s6))  call CSV_RECORD_APPEND_I4(record_here,  s6)
  if (present(s7))  call CSV_RECORD_APPEND_I4(record_here,  s7)
  if (present(s8))  call CSV_RECORD_APPEND_I4(record_here,  s8)
  if (present(s9))  call CSV_RECORD_APPEND_I4(record_here,  s9)
  if (present(s10)) call CSV_RECORD_APPEND_I4(record_here, s10)
  if (present(s11)) call CSV_RECORD_APPEND_I4(record_here, s11)
  if (present(s12)) call CSV_RECORD_APPEND_I4(record_here, s12)
  if (present(s13)) call CSV_RECORD_APPEND_I4(record_here, s13)
  if (present(s14)) call CSV_RECORD_APPEND_I4(record_here, s14)
  if (present(s15)) call CSV_RECORD_APPEND_I4(record_here, s15)
  if (present(s16)) call CSV_RECORD_APPEND_I4(record_here, s16)
  if (present(s17)) call CSV_RECORD_APPEND_I4(record_here, s17)
  if (present(s18)) call CSV_RECORD_APPEND_I4(record_here, s18)
  if (present(s19)) call CSV_RECORD_APPEND_I4(record_here, s19)
  if (present(s20)) call CSV_RECORD_APPEND_I4(record_here, s20)
  if (present(s21)) call CSV_RECORD_APPEND_I4(record_here, s21)
  if (present(s22)) call CSV_RECORD_APPEND_I4(record_here, s22)
  if (present(s23)) call CSV_RECORD_APPEND_I4(record_here, s23)
  if (present(s24)) call CSV_RECORD_APPEND_I4(record_here, s24)
  if (present(s25)) call CSV_RECORD_APPEND_I4(record_here, s25)
  if (present(s26)) call CSV_RECORD_APPEND_I4(record_here, s26)
  if (present(s27)) call CSV_RECORD_APPEND_I4(record_here, s27)
  if (present(s28)) call CSV_RECORD_APPEND_I4(record_here, s28)
  if (present(s29)) call CSV_RECORD_APPEND_I4(record_here, s29)
  if (present(s30)) call CSV_RECORD_APPEND_I4(record_here, s30)
  if (present(s31)) call CSV_RECORD_APPEND_I4(record_here, s31)
  if (present(s32)) call CSV_RECORD_APPEND_I4(record_here, s32)
  if (present(s33)) call CSV_RECORD_APPEND_I4(record_here, s33)
  if (present(s34)) call CSV_RECORD_APPEND_I4(record_here, s34)
  if (present(s35)) call CSV_RECORD_APPEND_I4(record_here, s35)
  if (present(s36)) call CSV_RECORD_APPEND_I4(record_here, s36)
  if (present(s37)) call CSV_RECORD_APPEND_I4(record_here, s37)
  if (present(s38)) call CSV_RECORD_APPEND_I4(record_here, s38)
  if (present(s39)) call CSV_RECORD_APPEND_I4(record_here, s39)
  if (present(s40)) call CSV_RECORD_APPEND_I4(record_here, s40)
  if (present(s41)) call CSV_RECORD_APPEND_I4(record_here, s41)
  if (present(s42)) call CSV_RECORD_APPEND_I4(record_here, s42)
  if (present(s43)) call CSV_RECORD_APPEND_I4(record_here, s43)
  if (present(s44)) call CSV_RECORD_APPEND_I4(record_here, s44)
  if (present(s45)) call CSV_RECORD_APPEND_I4(record_here, s45)
  if (present(s46)) call CSV_RECORD_APPEND_I4(record_here, s46)
  if (present(s47)) call CSV_RECORD_APPEND_I4(record_here, s47)
  if (present(s48)) call CSV_RECORD_APPEND_I4(record_here, s48)
  if (present(s49)) call CSV_RECORD_APPEND_I4(record_here, s49)
  if (present(s50)) call CSV_RECORD_APPEND_I4(record_here, s50)
  if (present(s51)) call CSV_RECORD_APPEND_I4(record_here, s51)
  if (present(s52)) call CSV_RECORD_APPEND_I4(record_here, s52)
  if (present(s53)) call CSV_RECORD_APPEND_I4(record_here, s53)
  if (present(s54)) call CSV_RECORD_APPEND_I4(record_here, s54)
  if (present(s55)) call CSV_RECORD_APPEND_I4(record_here, s55)
  if (present(s56)) call CSV_RECORD_APPEND_I4(record_here, s56)
  if (present(s57)) call CSV_RECORD_APPEND_I4(record_here, s57)
  if (present(s58)) call CSV_RECORD_APPEND_I4(record_here, s58)
  if (present(s59)) call CSV_RECORD_APPEND_I4(record_here, s59)
  if (present(s60)) call CSV_RECORD_APPEND_I4(record_here, s60)
  if (present(s61)) call CSV_RECORD_APPEND_I4(record_here, s61)
  if (present(s62)) call CSV_RECORD_APPEND_I4(record_here, s62)
  if (present(s63)) call CSV_RECORD_APPEND_I4(record_here, s63)
  if (present(s64)) call CSV_RECORD_APPEND_I4(record_here, s64)
  if (present(s65)) call CSV_RECORD_APPEND_I4(record_here, s65)
  if (present(s66)) call CSV_RECORD_APPEND_I4(record_here, s66)
  if (present(s67)) call CSV_RECORD_APPEND_I4(record_here, s67)
  if (present(s68)) call CSV_RECORD_APPEND_I4(record_here, s68)
  if (present(s69)) call CSV_RECORD_APPEND_I4(record_here, s69)
  if (present(s70)) call CSV_RECORD_APPEND_I4(record_here, s70)
  if (present(s71)) call CSV_RECORD_APPEND_I4(record_here, s71)
  if (present(s72)) call CSV_RECORD_APPEND_I4(record_here, s72)
  if (present(s73)) call CSV_RECORD_APPEND_I4(record_here, s73)
  if (present(s74)) call CSV_RECORD_APPEND_I4(record_here, s74)
  if (present(s75)) call CSV_RECORD_APPEND_I4(record_here, s75)
  if (present(s76)) call CSV_RECORD_APPEND_I4(record_here, s76)
  if (present(s77)) call CSV_RECORD_APPEND_I4(record_here, s77)
  if (present(s78)) call CSV_RECORD_APPEND_I4(record_here, s78)
  if (present(s79)) call CSV_RECORD_APPEND_I4(record_here, s79)
  if (present(s80)) call CSV_RECORD_APPEND_I4(record_here, s80)
  if (present(s81)) call CSV_RECORD_APPEND_I4(record_here, s81)
  if (present(s82)) call CSV_RECORD_APPEND_I4(record_here, s82)
  if (present(s83)) call CSV_RECORD_APPEND_I4(record_here, s83)
  if (present(s84)) call CSV_RECORD_APPEND_I4(record_here, s84)
  if (present(s85)) call CSV_RECORD_APPEND_I4(record_here, s85)
  if (present(s86)) call CSV_RECORD_APPEND_I4(record_here, s86)
  if (present(s87)) call CSV_RECORD_APPEND_I4(record_here, s87)
  if (present(s88)) call CSV_RECORD_APPEND_I4(record_here, s88)
  if (present(s89)) call CSV_RECORD_APPEND_I4(record_here, s89)
  if (present(s90)) call CSV_RECORD_APPEND_I4(record_here, s90)
  if (present(s91)) call CSV_RECORD_APPEND_I4(record_here, s91)
  if (present(s92)) call CSV_RECORD_APPEND_I4(record_here, s92)
  if (present(s93)) call CSV_RECORD_APPEND_I4(record_here, s93)
  if (present(s94)) call CSV_RECORD_APPEND_I4(record_here, s94)
  if (present(s95)) call CSV_RECORD_APPEND_I4(record_here, s95)
  if (present(s96)) call CSV_RECORD_APPEND_I4(record_here, s96)
  if (present(s97)) call CSV_RECORD_APPEND_I4(record_here, s97)
  if (present(s98)) call CSV_RECORD_APPEND_I4(record_here, s98)
  if (present(s99)) call CSV_RECORD_APPEND_I4(record_here, s99)
  if (present(s100)) call CSV_RECORD_APPEND_I4(record_here,s100)

  record = record_here

end subroutine CSV_RECORD_APPEND_LST_I4

!-------------------------------------------------------------------------------

subroutine CSV_RECORD_APPEND_LST_R4 (record, &
                                      s1, s2, s3, s4, s5, s6, s7, s8, s9,s10,&
                                     s11,s12,s13,s14,s15,s16,s17,s18,s19,s20,&
                                     s21,s22,s23,s24,s25,s26,s27,s28,s29,s30,&
                                     s31,s32,s33,s34,s35,s36,s37,s38,s39,s40,&
                                     s41,s42,s43,s44,s45,s46,s47,s48,s49,s50,&
                                     s51,s52,s53,s54,s55,s56,s57,s58,s59,s60,&
                                     s61,s62,s63,s64,s65,s66,s67,s68,s69,s70,&
                                     s71,s72,s73,s74,s75,s76,s77,s78,s79,s80,&
                                     s81,s82,s83,s84,s85,s86,s87,s88,s89,s90,&
                                     s91,s92,s93,s94,s95,s96,s97,s98,s99,s100 )
!*******************************************************************************
! CSV_RECORD_APPEND_LST_R4
! PURPOSE: appends a list of strings to a CSV record.
! CALL PARAMETERS:
!    Character RECORD, the CSV record.
!    Optional list of reals (kind 4) to be appended, at least two values
!
! NOTES:
!   This is a rudimentary implementation making use of optional parameters,
!   with maximum 100. It should be enough for most cases where the purpose
!   of this subroutine is to construct header line with variable names
!   in a single call instead of multiple CSV_RECORD_APPEND calls. Should be
!   rewritten to use detived types and pointers for arbitrary list size.
!   But for now (in this version) the main aim is implementation and
!   debugging simplicity. May be worth appending more strings... Is 200 enough?
!
! TODO: Make also the same function, in addition to sub with generic interface
! Author: Sergey Budaev
!*******************************************************************************

  implicit none

  ! Calling parameters, first record itself, then 100 list items
  character (len=*), intent(inout) :: record

  real, intent(in) :: s1  ! The first two strings in the list are
  real, intent(in) :: s2  ! mandatory to make it unique in the
                                       ! generic interface block

  real, optional, intent(in) :: s3
  real, optional, intent(in) :: s4
  real, optional, intent(in) :: s5
  real, optional, intent(in) :: s6
  real, optional, intent(in) :: s7
  real, optional, intent(in) :: s8
  real, optional, intent(in) :: s9
  real, optional, intent(in) :: s10
  real, optional, intent(in) :: s11
  real, optional, intent(in) :: s12
  real, optional, intent(in) :: s13
  real, optional, intent(in) :: s14
  real, optional, intent(in) :: s15
  real, optional, intent(in) :: s16
  real, optional, intent(in) :: s17
  real, optional, intent(in) :: s18
  real, optional, intent(in) :: s19
  real, optional, intent(in) :: s20
  real, optional, intent(in) :: s21
  real, optional, intent(in) :: s22
  real, optional, intent(in) :: s23
  real, optional, intent(in) :: s24
  real, optional, intent(in) :: s25
  real, optional, intent(in) :: s26
  real, optional, intent(in) :: s27
  real, optional, intent(in) :: s28
  real, optional, intent(in) :: s29
  real, optional, intent(in) :: s30
  real, optional, intent(in) :: s31
  real, optional, intent(in) :: s32
  real, optional, intent(in) :: s33
  real, optional, intent(in) :: s34
  real, optional, intent(in) :: s35
  real, optional, intent(in) :: s36
  real, optional, intent(in) :: s37
  real, optional, intent(in) :: s38
  real, optional, intent(in) :: s39
  real, optional, intent(in) :: s40
  real, optional, intent(in) :: s41
  real, optional, intent(in) :: s42
  real, optional, intent(in) :: s43
  real, optional, intent(in) :: s44
  real, optional, intent(in) :: s45
  real, optional, intent(in) :: s46
  real, optional, intent(in) :: s47
  real, optional, intent(in) :: s48
  real, optional, intent(in) :: s49
  real, optional, intent(in) :: s50
  real, optional, intent(in) :: s51
  real, optional, intent(in) :: s52
  real, optional, intent(in) :: s53
  real, optional, intent(in) :: s54
  real, optional, intent(in) :: s55
  real, optional, intent(in) :: s56
  real, optional, intent(in) :: s57
  real, optional, intent(in) :: s58
  real, optional, intent(in) :: s59
  real, optional, intent(in) :: s60
  real, optional, intent(in) :: s61
  real, optional, intent(in) :: s62
  real, optional, intent(in) :: s63
  real, optional, intent(in) :: s64
  real, optional, intent(in) :: s65
  real, optional, intent(in) :: s66
  real, optional, intent(in) :: s67
  real, optional, intent(in) :: s68
  real, optional, intent(in) :: s69
  real, optional, intent(in) :: s70
  real, optional, intent(in) :: s71
  real, optional, intent(in) :: s72
  real, optional, intent(in) :: s73
  real, optional, intent(in) :: s74
  real, optional, intent(in) :: s75
  real, optional, intent(in) :: s76
  real, optional, intent(in) :: s77
  real, optional, intent(in) :: s78
  real, optional, intent(in) :: s79
  real, optional, intent(in) :: s80
  real, optional, intent(in) :: s81
  real, optional, intent(in) :: s82
  real, optional, intent(in) :: s83
  real, optional, intent(in) :: s84
  real, optional, intent(in) :: s85
  real, optional, intent(in) :: s86
  real, optional, intent(in) :: s87
  real, optional, intent(in) :: s88
  real, optional, intent(in) :: s89
  real, optional, intent(in) :: s90
  real, optional, intent(in) :: s91
  real, optional, intent(in) :: s92
  real, optional, intent(in) :: s93
  real, optional, intent(in) :: s94
  real, optional, intent(in) :: s95
  real, optional, intent(in) :: s96
  real, optional, intent(in) :: s97
  real, optional, intent(in) :: s98
  real, optional, intent(in) :: s99
  real, optional, intent(in) :: s100

  ! Local variables, copies of optionals
  character (len=len(record)) :: record_here

  !-----------------------------------------------------------------------------

  record_here = record

  call CSV_RECORD_APPEND_R4(record_here,   s1)
  call CSV_RECORD_APPEND_R4(record_here,   s2)

  if (present(s3))  call CSV_RECORD_APPEND_R4(record_here,  s3)
  if (present(s4))  call CSV_RECORD_APPEND_R4(record_here,  s4)
  if (present(s5))  call CSV_RECORD_APPEND_R4(record_here,  s5)
  if (present(s6))  call CSV_RECORD_APPEND_R4(record_here,  s6)
  if (present(s7))  call CSV_RECORD_APPEND_R4(record_here,  s7)
  if (present(s8))  call CSV_RECORD_APPEND_R4(record_here,  s8)
  if (present(s9))  call CSV_RECORD_APPEND_R4(record_here,  s9)
  if (present(s10)) call CSV_RECORD_APPEND_R4(record_here, s10)
  if (present(s11)) call CSV_RECORD_APPEND_R4(record_here, s11)
  if (present(s12)) call CSV_RECORD_APPEND_R4(record_here, s12)
  if (present(s13)) call CSV_RECORD_APPEND_R4(record_here, s13)
  if (present(s14)) call CSV_RECORD_APPEND_R4(record_here, s14)
  if (present(s15)) call CSV_RECORD_APPEND_R4(record_here, s15)
  if (present(s16)) call CSV_RECORD_APPEND_R4(record_here, s16)
  if (present(s17)) call CSV_RECORD_APPEND_R4(record_here, s17)
  if (present(s18)) call CSV_RECORD_APPEND_R4(record_here, s18)
  if (present(s19)) call CSV_RECORD_APPEND_R4(record_here, s19)
  if (present(s20)) call CSV_RECORD_APPEND_R4(record_here, s20)
  if (present(s21)) call CSV_RECORD_APPEND_R4(record_here, s21)
  if (present(s22)) call CSV_RECORD_APPEND_R4(record_here, s22)
  if (present(s23)) call CSV_RECORD_APPEND_R4(record_here, s23)
  if (present(s24)) call CSV_RECORD_APPEND_R4(record_here, s24)
  if (present(s25)) call CSV_RECORD_APPEND_R4(record_here, s25)
  if (present(s26)) call CSV_RECORD_APPEND_R4(record_here, s26)
  if (present(s27)) call CSV_RECORD_APPEND_R4(record_here, s27)
  if (present(s28)) call CSV_RECORD_APPEND_R4(record_here, s28)
  if (present(s29)) call CSV_RECORD_APPEND_R4(record_here, s29)
  if (present(s30)) call CSV_RECORD_APPEND_R4(record_here, s30)
  if (present(s31)) call CSV_RECORD_APPEND_R4(record_here, s31)
  if (present(s32)) call CSV_RECORD_APPEND_R4(record_here, s32)
  if (present(s33)) call CSV_RECORD_APPEND_R4(record_here, s33)
  if (present(s34)) call CSV_RECORD_APPEND_R4(record_here, s34)
  if (present(s35)) call CSV_RECORD_APPEND_R4(record_here, s35)
  if (present(s36)) call CSV_RECORD_APPEND_R4(record_here, s36)
  if (present(s37)) call CSV_RECORD_APPEND_R4(record_here, s37)
  if (present(s38)) call CSV_RECORD_APPEND_R4(record_here, s38)
  if (present(s39)) call CSV_RECORD_APPEND_R4(record_here, s39)
  if (present(s40)) call CSV_RECORD_APPEND_R4(record_here, s40)
  if (present(s41)) call CSV_RECORD_APPEND_R4(record_here, s41)
  if (present(s42)) call CSV_RECORD_APPEND_R4(record_here, s42)
  if (present(s43)) call CSV_RECORD_APPEND_R4(record_here, s43)
  if (present(s44)) call CSV_RECORD_APPEND_R4(record_here, s44)
  if (present(s45)) call CSV_RECORD_APPEND_R4(record_here, s45)
  if (present(s46)) call CSV_RECORD_APPEND_R4(record_here, s46)
  if (present(s47)) call CSV_RECORD_APPEND_R4(record_here, s47)
  if (present(s48)) call CSV_RECORD_APPEND_R4(record_here, s48)
  if (present(s49)) call CSV_RECORD_APPEND_R4(record_here, s49)
  if (present(s50)) call CSV_RECORD_APPEND_R4(record_here, s50)
  if (present(s51)) call CSV_RECORD_APPEND_R4(record_here, s51)
  if (present(s52)) call CSV_RECORD_APPEND_R4(record_here, s52)
  if (present(s53)) call CSV_RECORD_APPEND_R4(record_here, s53)
  if (present(s54)) call CSV_RECORD_APPEND_R4(record_here, s54)
  if (present(s55)) call CSV_RECORD_APPEND_R4(record_here, s55)
  if (present(s56)) call CSV_RECORD_APPEND_R4(record_here, s56)
  if (present(s57)) call CSV_RECORD_APPEND_R4(record_here, s57)
  if (present(s58)) call CSV_RECORD_APPEND_R4(record_here, s58)
  if (present(s59)) call CSV_RECORD_APPEND_R4(record_here, s59)
  if (present(s60)) call CSV_RECORD_APPEND_R4(record_here, s60)
  if (present(s61)) call CSV_RECORD_APPEND_R4(record_here, s61)
  if (present(s62)) call CSV_RECORD_APPEND_R4(record_here, s62)
  if (present(s63)) call CSV_RECORD_APPEND_R4(record_here, s63)
  if (present(s64)) call CSV_RECORD_APPEND_R4(record_here, s64)
  if (present(s65)) call CSV_RECORD_APPEND_R4(record_here, s65)
  if (present(s66)) call CSV_RECORD_APPEND_R4(record_here, s66)
  if (present(s67)) call CSV_RECORD_APPEND_R4(record_here, s67)
  if (present(s68)) call CSV_RECORD_APPEND_R4(record_here, s68)
  if (present(s69)) call CSV_RECORD_APPEND_R4(record_here, s69)
  if (present(s70)) call CSV_RECORD_APPEND_R4(record_here, s70)
  if (present(s71)) call CSV_RECORD_APPEND_R4(record_here, s71)
  if (present(s72)) call CSV_RECORD_APPEND_R4(record_here, s72)
  if (present(s73)) call CSV_RECORD_APPEND_R4(record_here, s73)
  if (present(s74)) call CSV_RECORD_APPEND_R4(record_here, s74)
  if (present(s75)) call CSV_RECORD_APPEND_R4(record_here, s75)
  if (present(s76)) call CSV_RECORD_APPEND_R4(record_here, s76)
  if (present(s77)) call CSV_RECORD_APPEND_R4(record_here, s77)
  if (present(s78)) call CSV_RECORD_APPEND_R4(record_here, s78)
  if (present(s79)) call CSV_RECORD_APPEND_R4(record_here, s79)
  if (present(s80)) call CSV_RECORD_APPEND_R4(record_here, s80)
  if (present(s81)) call CSV_RECORD_APPEND_R4(record_here, s81)
  if (present(s82)) call CSV_RECORD_APPEND_R4(record_here, s82)
  if (present(s83)) call CSV_RECORD_APPEND_R4(record_here, s83)
  if (present(s84)) call CSV_RECORD_APPEND_R4(record_here, s84)
  if (present(s85)) call CSV_RECORD_APPEND_R4(record_here, s85)
  if (present(s86)) call CSV_RECORD_APPEND_R4(record_here, s86)
  if (present(s87)) call CSV_RECORD_APPEND_R4(record_here, s87)
  if (present(s88)) call CSV_RECORD_APPEND_R4(record_here, s88)
  if (present(s89)) call CSV_RECORD_APPEND_R4(record_here, s89)
  if (present(s90)) call CSV_RECORD_APPEND_R4(record_here, s90)
  if (present(s91)) call CSV_RECORD_APPEND_R4(record_here, s91)
  if (present(s92)) call CSV_RECORD_APPEND_R4(record_here, s92)
  if (present(s93)) call CSV_RECORD_APPEND_R4(record_here, s93)
  if (present(s94)) call CSV_RECORD_APPEND_R4(record_here, s94)
  if (present(s95)) call CSV_RECORD_APPEND_R4(record_here, s95)
  if (present(s96)) call CSV_RECORD_APPEND_R4(record_here, s96)
  if (present(s97)) call CSV_RECORD_APPEND_R4(record_here, s97)
  if (present(s98)) call CSV_RECORD_APPEND_R4(record_here, s98)
  if (present(s99)) call CSV_RECORD_APPEND_R4(record_here, s99)
  if (present(s100)) call CSV_RECORD_APPEND_R4(record_here,s100)

  record = record_here

end subroutine CSV_RECORD_APPEND_LST_R4

!-------------------------------------------------------------------------------

subroutine CSV_RECORD_APPEND_LST_R8 (record, &
                                      s1, s2, s3, s4, s5, s6, s7, s8, s9,s10,&
                                     s11,s12,s13,s14,s15,s16,s17,s18,s19,s20,&
                                     s21,s22,s23,s24,s25,s26,s27,s28,s29,s30,&
                                     s31,s32,s33,s34,s35,s36,s37,s38,s39,s40,&
                                     s41,s42,s43,s44,s45,s46,s47,s48,s49,s50,&
                                     s51,s52,s53,s54,s55,s56,s57,s58,s59,s60,&
                                     s61,s62,s63,s64,s65,s66,s67,s68,s69,s70,&
                                     s71,s72,s73,s74,s75,s76,s77,s78,s79,s80,&
                                     s81,s82,s83,s84,s85,s86,s87,s88,s89,s90,&
                                     s91,s92,s93,s94,s95,s96,s97,s98,s99,s100 )
!*******************************************************************************
! CSV_RECORD_APPEND_LST_R8
! PURPOSE: appends a list of strings to a CSV record.
! CALL PARAMETERS:
!    Character RECORD, the CSV record.
!    Optional list of reals (kind 8) to be appended, at least two values
!
! NOTES:
!   This is a rudimentary implementation making use of optional parameters,
!   with maximum 100. It should be enough for most cases where the purpose
!   of this subroutine is to construct header line with variable names
!   in a single call instead of multiple CSV_RECORD_APPEND calls. Should be
!   rewritten to use detived types and pointers for arbitrary list size.
!   But for now (in this version) the main aim is implementation and
!   debugging simplicity. May be worth appending more strings... Is 200 enough?
!
! TODO: Make also the same function, in addition to sub with generic interface
! Author: Sergey Budaev
!*******************************************************************************

  implicit none

  ! Calling parameters, first record itself, then 100 list items
  character (len=*), intent(inout) :: record

  real (kind=8), intent(in) :: s1  ! The first two strings in the list are
  real (kind=8), intent(in) :: s2  ! mandatory to make it unique in the
                                       ! generic interface block

  real (kind=8), optional, intent(in) :: s3
  real (kind=8), optional, intent(in) :: s4
  real (kind=8), optional, intent(in) :: s5
  real (kind=8), optional, intent(in) :: s6
  real (kind=8), optional, intent(in) :: s7
  real (kind=8), optional, intent(in) :: s8
  real (kind=8), optional, intent(in) :: s9
  real (kind=8), optional, intent(in) :: s10
  real (kind=8), optional, intent(in) :: s11
  real (kind=8), optional, intent(in) :: s12
  real (kind=8), optional, intent(in) :: s13
  real (kind=8), optional, intent(in) :: s14
  real (kind=8), optional, intent(in) :: s15
  real (kind=8), optional, intent(in) :: s16
  real (kind=8), optional, intent(in) :: s17
  real (kind=8), optional, intent(in) :: s18
  real (kind=8), optional, intent(in) :: s19
  real (kind=8), optional, intent(in) :: s20
  real (kind=8), optional, intent(in) :: s21
  real (kind=8), optional, intent(in) :: s22
  real (kind=8), optional, intent(in) :: s23
  real (kind=8), optional, intent(in) :: s24
  real (kind=8), optional, intent(in) :: s25
  real (kind=8), optional, intent(in) :: s26
  real (kind=8), optional, intent(in) :: s27
  real (kind=8), optional, intent(in) :: s28
  real (kind=8), optional, intent(in) :: s29
  real (kind=8), optional, intent(in) :: s30
  real (kind=8), optional, intent(in) :: s31
  real (kind=8), optional, intent(in) :: s32
  real (kind=8), optional, intent(in) :: s33
  real (kind=8), optional, intent(in) :: s34
  real (kind=8), optional, intent(in) :: s35
  real (kind=8), optional, intent(in) :: s36
  real (kind=8), optional, intent(in) :: s37
  real (kind=8), optional, intent(in) :: s38
  real (kind=8), optional, intent(in) :: s39
  real (kind=8), optional, intent(in) :: s40
  real (kind=8), optional, intent(in) :: s41
  real (kind=8), optional, intent(in) :: s42
  real (kind=8), optional, intent(in) :: s43
  real (kind=8), optional, intent(in) :: s44
  real (kind=8), optional, intent(in) :: s45
  real (kind=8), optional, intent(in) :: s46
  real (kind=8), optional, intent(in) :: s47
  real (kind=8), optional, intent(in) :: s48
  real (kind=8), optional, intent(in) :: s49
  real (kind=8), optional, intent(in) :: s50
  real (kind=8), optional, intent(in) :: s51
  real (kind=8), optional, intent(in) :: s52
  real (kind=8), optional, intent(in) :: s53
  real (kind=8), optional, intent(in) :: s54
  real (kind=8), optional, intent(in) :: s55
  real (kind=8), optional, intent(in) :: s56
  real (kind=8), optional, intent(in) :: s57
  real (kind=8), optional, intent(in) :: s58
  real (kind=8), optional, intent(in) :: s59
  real (kind=8), optional, intent(in) :: s60
  real (kind=8), optional, intent(in) :: s61
  real (kind=8), optional, intent(in) :: s62
  real (kind=8), optional, intent(in) :: s63
  real (kind=8), optional, intent(in) :: s64
  real (kind=8), optional, intent(in) :: s65
  real (kind=8), optional, intent(in) :: s66
  real (kind=8), optional, intent(in) :: s67
  real (kind=8), optional, intent(in) :: s68
  real (kind=8), optional, intent(in) :: s69
  real (kind=8), optional, intent(in) :: s70
  real (kind=8), optional, intent(in) :: s71
  real (kind=8), optional, intent(in) :: s72
  real (kind=8), optional, intent(in) :: s73
  real (kind=8), optional, intent(in) :: s74
  real (kind=8), optional, intent(in) :: s75
  real (kind=8), optional, intent(in) :: s76
  real (kind=8), optional, intent(in) :: s77
  real (kind=8), optional, intent(in) :: s78
  real (kind=8), optional, intent(in) :: s79
  real (kind=8), optional, intent(in) :: s80
  real (kind=8), optional, intent(in) :: s81
  real (kind=8), optional, intent(in) :: s82
  real (kind=8), optional, intent(in) :: s83
  real (kind=8), optional, intent(in) :: s84
  real (kind=8), optional, intent(in) :: s85
  real (kind=8), optional, intent(in) :: s86
  real (kind=8), optional, intent(in) :: s87
  real (kind=8), optional, intent(in) :: s88
  real (kind=8), optional, intent(in) :: s89
  real (kind=8), optional, intent(in) :: s90
  real (kind=8), optional, intent(in) :: s91
  real (kind=8), optional, intent(in) :: s92
  real (kind=8), optional, intent(in) :: s93
  real (kind=8), optional, intent(in) :: s94
  real (kind=8), optional, intent(in) :: s95
  real (kind=8), optional, intent(in) :: s96
  real (kind=8), optional, intent(in) :: s97
  real (kind=8), optional, intent(in) :: s98
  real (kind=8), optional, intent(in) :: s99
  real (kind=8), optional, intent(in) :: s100

  ! Local variables, copies of optionals
  character (len=len(record)) :: record_here

  !-----------------------------------------------------------------------------

  record_here = record

  call CSV_RECORD_APPEND_R8(record_here,   s1)
  call CSV_RECORD_APPEND_R8(record_here,   s2)

  if (present(s3))  call CSV_RECORD_APPEND_R8(record_here,  s3)
  if (present(s4))  call CSV_RECORD_APPEND_R8(record_here,  s4)
  if (present(s5))  call CSV_RECORD_APPEND_R8(record_here,  s5)
  if (present(s6))  call CSV_RECORD_APPEND_R8(record_here,  s6)
  if (present(s7))  call CSV_RECORD_APPEND_R8(record_here,  s7)
  if (present(s8))  call CSV_RECORD_APPEND_R8(record_here,  s8)
  if (present(s9))  call CSV_RECORD_APPEND_R8(record_here,  s9)
  if (present(s10)) call CSV_RECORD_APPEND_R8(record_here, s10)
  if (present(s11)) call CSV_RECORD_APPEND_R8(record_here, s11)
  if (present(s12)) call CSV_RECORD_APPEND_R8(record_here, s12)
  if (present(s13)) call CSV_RECORD_APPEND_R8(record_here, s13)
  if (present(s14)) call CSV_RECORD_APPEND_R8(record_here, s14)
  if (present(s15)) call CSV_RECORD_APPEND_R8(record_here, s15)
  if (present(s16)) call CSV_RECORD_APPEND_R8(record_here, s16)
  if (present(s17)) call CSV_RECORD_APPEND_R8(record_here, s17)
  if (present(s18)) call CSV_RECORD_APPEND_R8(record_here, s18)
  if (present(s19)) call CSV_RECORD_APPEND_R8(record_here, s19)
  if (present(s20)) call CSV_RECORD_APPEND_R8(record_here, s20)
  if (present(s21)) call CSV_RECORD_APPEND_R8(record_here, s21)
  if (present(s22)) call CSV_RECORD_APPEND_R8(record_here, s22)
  if (present(s23)) call CSV_RECORD_APPEND_R8(record_here, s23)
  if (present(s24)) call CSV_RECORD_APPEND_R8(record_here, s24)
  if (present(s25)) call CSV_RECORD_APPEND_R8(record_here, s25)
  if (present(s26)) call CSV_RECORD_APPEND_R8(record_here, s26)
  if (present(s27)) call CSV_RECORD_APPEND_R8(record_here, s27)
  if (present(s28)) call CSV_RECORD_APPEND_R8(record_here, s28)
  if (present(s29)) call CSV_RECORD_APPEND_R8(record_here, s29)
  if (present(s30)) call CSV_RECORD_APPEND_R8(record_here, s30)
  if (present(s31)) call CSV_RECORD_APPEND_R8(record_here, s31)
  if (present(s32)) call CSV_RECORD_APPEND_R8(record_here, s32)
  if (present(s33)) call CSV_RECORD_APPEND_R8(record_here, s33)
  if (present(s34)) call CSV_RECORD_APPEND_R8(record_here, s34)
  if (present(s35)) call CSV_RECORD_APPEND_R8(record_here, s35)
  if (present(s36)) call CSV_RECORD_APPEND_R8(record_here, s36)
  if (present(s37)) call CSV_RECORD_APPEND_R8(record_here, s37)
  if (present(s38)) call CSV_RECORD_APPEND_R8(record_here, s38)
  if (present(s39)) call CSV_RECORD_APPEND_R8(record_here, s39)
  if (present(s40)) call CSV_RECORD_APPEND_R8(record_here, s40)
  if (present(s41)) call CSV_RECORD_APPEND_R8(record_here, s41)
  if (present(s42)) call CSV_RECORD_APPEND_R8(record_here, s42)
  if (present(s43)) call CSV_RECORD_APPEND_R8(record_here, s43)
  if (present(s44)) call CSV_RECORD_APPEND_R8(record_here, s44)
  if (present(s45)) call CSV_RECORD_APPEND_R8(record_here, s45)
  if (present(s46)) call CSV_RECORD_APPEND_R8(record_here, s46)
  if (present(s47)) call CSV_RECORD_APPEND_R8(record_here, s47)
  if (present(s48)) call CSV_RECORD_APPEND_R8(record_here, s48)
  if (present(s49)) call CSV_RECORD_APPEND_R8(record_here, s49)
  if (present(s50)) call CSV_RECORD_APPEND_R8(record_here, s50)
  if (present(s51)) call CSV_RECORD_APPEND_R8(record_here, s51)
  if (present(s52)) call CSV_RECORD_APPEND_R8(record_here, s52)
  if (present(s53)) call CSV_RECORD_APPEND_R8(record_here, s53)
  if (present(s54)) call CSV_RECORD_APPEND_R8(record_here, s54)
  if (present(s55)) call CSV_RECORD_APPEND_R8(record_here, s55)
  if (present(s56)) call CSV_RECORD_APPEND_R8(record_here, s56)
  if (present(s57)) call CSV_RECORD_APPEND_R8(record_here, s57)
  if (present(s58)) call CSV_RECORD_APPEND_R8(record_here, s58)
  if (present(s59)) call CSV_RECORD_APPEND_R8(record_here, s59)
  if (present(s60)) call CSV_RECORD_APPEND_R8(record_here, s60)
  if (present(s61)) call CSV_RECORD_APPEND_R8(record_here, s61)
  if (present(s62)) call CSV_RECORD_APPEND_R8(record_here, s62)
  if (present(s63)) call CSV_RECORD_APPEND_R8(record_here, s63)
  if (present(s64)) call CSV_RECORD_APPEND_R8(record_here, s64)
  if (present(s65)) call CSV_RECORD_APPEND_R8(record_here, s65)
  if (present(s66)) call CSV_RECORD_APPEND_R8(record_here, s66)
  if (present(s67)) call CSV_RECORD_APPEND_R8(record_here, s67)
  if (present(s68)) call CSV_RECORD_APPEND_R8(record_here, s68)
  if (present(s69)) call CSV_RECORD_APPEND_R8(record_here, s69)
  if (present(s70)) call CSV_RECORD_APPEND_R8(record_here, s70)
  if (present(s71)) call CSV_RECORD_APPEND_R8(record_here, s71)
  if (present(s72)) call CSV_RECORD_APPEND_R8(record_here, s72)
  if (present(s73)) call CSV_RECORD_APPEND_R8(record_here, s73)
  if (present(s74)) call CSV_RECORD_APPEND_R8(record_here, s74)
  if (present(s75)) call CSV_RECORD_APPEND_R8(record_here, s75)
  if (present(s76)) call CSV_RECORD_APPEND_R8(record_here, s76)
  if (present(s77)) call CSV_RECORD_APPEND_R8(record_here, s77)
  if (present(s78)) call CSV_RECORD_APPEND_R8(record_here, s78)
  if (present(s79)) call CSV_RECORD_APPEND_R8(record_here, s79)
  if (present(s80)) call CSV_RECORD_APPEND_R8(record_here, s80)
  if (present(s81)) call CSV_RECORD_APPEND_R8(record_here, s81)
  if (present(s82)) call CSV_RECORD_APPEND_R8(record_here, s82)
  if (present(s83)) call CSV_RECORD_APPEND_R8(record_here, s83)
  if (present(s84)) call CSV_RECORD_APPEND_R8(record_here, s84)
  if (present(s85)) call CSV_RECORD_APPEND_R8(record_here, s85)
  if (present(s86)) call CSV_RECORD_APPEND_R8(record_here, s86)
  if (present(s87)) call CSV_RECORD_APPEND_R8(record_here, s87)
  if (present(s88)) call CSV_RECORD_APPEND_R8(record_here, s88)
  if (present(s89)) call CSV_RECORD_APPEND_R8(record_here, s89)
  if (present(s90)) call CSV_RECORD_APPEND_R8(record_here, s90)
  if (present(s91)) call CSV_RECORD_APPEND_R8(record_here, s91)
  if (present(s92)) call CSV_RECORD_APPEND_R8(record_here, s92)
  if (present(s93)) call CSV_RECORD_APPEND_R8(record_here, s93)
  if (present(s94)) call CSV_RECORD_APPEND_R8(record_here, s94)
  if (present(s95)) call CSV_RECORD_APPEND_R8(record_here, s95)
  if (present(s96)) call CSV_RECORD_APPEND_R8(record_here, s96)
  if (present(s97)) call CSV_RECORD_APPEND_R8(record_here, s97)
  if (present(s98)) call CSV_RECORD_APPEND_R8(record_here, s98)
  if (present(s99)) call CSV_RECORD_APPEND_R8(record_here, s99)
  if (present(s100)) call CSV_RECORD_APPEND_R8(record_here,s100)

  record = record_here

end subroutine CSV_RECORD_APPEND_LST_R8

!-------------------------------------------------------------------------------

subroutine CSV_RECORD_APPEND_LST_S (record, &
                                      s1, s2, s3, s4, s5, s6, s7, s8, s9,s10,&
                                     s11,s12,s13,s14,s15,s16,s17,s18,s19,s20,&
                                     s21,s22,s23,s24,s25,s26,s27,s28,s29,s30,&
                                     s31,s32,s33,s34,s35,s36,s37,s38,s39,s40,&
                                     s41,s42,s43,s44,s45,s46,s47,s48,s49,s50,&
                                     s51,s52,s53,s54,s55,s56,s57,s58,s59,s60,&
                                     s61,s62,s63,s64,s65,s66,s67,s68,s69,s70,&
                                     s71,s72,s73,s74,s75,s76,s77,s78,s79,s80,&
                                     s81,s82,s83,s84,s85,s86,s87,s88,s89,s90,&
                                     s91,s92,s93,s94,s95,s96,s97,s98,s99,s100 )
!*******************************************************************************
! CSV_RECORD_APPEND_LST_S
! PURPOSE: appends a list of strings to a CSV record.
! CALL PARAMETERS:
!    Character RECORD, the CSV record.
!    Optional list of character string to be appended, at least two strings
!
! NOTES:
!   This is a rudimentary implementation making use of optional parameters,
!   with maximum 100. It should be enough for most cases where the purpose
!   of this subroutine is to construct header line with variable names
!   in a single call instead of multiple CSV_RECORD_APPEND calls. Should be
!   rewritten to use detived types and pointers for arbitrary list size.
!   But for now (in this version) the main aim is implementation and
!   debugging simplicity. May be worth appending more strings... Is 200 enough?
!
! TODO: Make also the same function, in addition to sub with generic interface
! Author: Sergey Budaev
!*******************************************************************************

  implicit none

  ! Calling parameters, first record itself, then 100 list items
  character (len=*), intent(inout) :: record

  character (len=*), intent(in) :: s1  ! The first two strings in the list are
  character (len=*), intent(in) :: s2  ! mandatory to make it unique in the
                                       ! generic interface block

  character (len=*), optional, intent(in) :: s3
  character (len=*), optional, intent(in) :: s4
  character (len=*), optional, intent(in) :: s5
  character (len=*), optional, intent(in) :: s6
  character (len=*), optional, intent(in) :: s7
  character (len=*), optional, intent(in) :: s8
  character (len=*), optional, intent(in) :: s9
  character (len=*), optional, intent(in) :: s10

  character (len=*), optional, intent(in) :: s11
  character (len=*), optional, intent(in) :: s12
  character (len=*), optional, intent(in) :: s13
  character (len=*), optional, intent(in) :: s14
  character (len=*), optional, intent(in) :: s15
  character (len=*), optional, intent(in) :: s16
  character (len=*), optional, intent(in) :: s17
  character (len=*), optional, intent(in) :: s18
  character (len=*), optional, intent(in) :: s19
  character (len=*), optional, intent(in) :: s20

  character (len=*), optional, intent(in) :: s21
  character (len=*), optional, intent(in) :: s22
  character (len=*), optional, intent(in) :: s23
  character (len=*), optional, intent(in) :: s24
  character (len=*), optional, intent(in) :: s25
  character (len=*), optional, intent(in) :: s26
  character (len=*), optional, intent(in) :: s27
  character (len=*), optional, intent(in) :: s28
  character (len=*), optional, intent(in) :: s29
  character (len=*), optional, intent(in) :: s30

  character (len=*), optional, intent(in) :: s31
  character (len=*), optional, intent(in) :: s32
  character (len=*), optional, intent(in) :: s33
  character (len=*), optional, intent(in) :: s34
  character (len=*), optional, intent(in) :: s35
  character (len=*), optional, intent(in) :: s36
  character (len=*), optional, intent(in) :: s37
  character (len=*), optional, intent(in) :: s38
  character (len=*), optional, intent(in) :: s39
  character (len=*), optional, intent(in) :: s40

  character (len=*), optional, intent(in) :: s41
  character (len=*), optional, intent(in) :: s42
  character (len=*), optional, intent(in) :: s43
  character (len=*), optional, intent(in) :: s44
  character (len=*), optional, intent(in) :: s45
  character (len=*), optional, intent(in) :: s46
  character (len=*), optional, intent(in) :: s47
  character (len=*), optional, intent(in) :: s48
  character (len=*), optional, intent(in) :: s49
  character (len=*), optional, intent(in) :: s50

  character (len=*), optional, intent(in) :: s51
  character (len=*), optional, intent(in) :: s52
  character (len=*), optional, intent(in) :: s53
  character (len=*), optional, intent(in) :: s54
  character (len=*), optional, intent(in) :: s55
  character (len=*), optional, intent(in) :: s56
  character (len=*), optional, intent(in) :: s57
  character (len=*), optional, intent(in) :: s58
  character (len=*), optional, intent(in) :: s59
  character (len=*), optional, intent(in) :: s60

  character (len=*), optional, intent(in) :: s61
  character (len=*), optional, intent(in) :: s62
  character (len=*), optional, intent(in) :: s63
  character (len=*), optional, intent(in) :: s64
  character (len=*), optional, intent(in) :: s65
  character (len=*), optional, intent(in) :: s66
  character (len=*), optional, intent(in) :: s67
  character (len=*), optional, intent(in) :: s68
  character (len=*), optional, intent(in) :: s69
  character (len=*), optional, intent(in) :: s70

  character (len=*), optional, intent(in) :: s71
  character (len=*), optional, intent(in) :: s72
  character (len=*), optional, intent(in) :: s73
  character (len=*), optional, intent(in) :: s74
  character (len=*), optional, intent(in) :: s75
  character (len=*), optional, intent(in) :: s76
  character (len=*), optional, intent(in) :: s77
  character (len=*), optional, intent(in) :: s78
  character (len=*), optional, intent(in) :: s79
  character (len=*), optional, intent(in) :: s80

  character (len=*), optional, intent(in) :: s81
  character (len=*), optional, intent(in) :: s82
  character (len=*), optional, intent(in) :: s83
  character (len=*), optional, intent(in) :: s84
  character (len=*), optional, intent(in) :: s85
  character (len=*), optional, intent(in) :: s86
  character (len=*), optional, intent(in) :: s87
  character (len=*), optional, intent(in) :: s88
  character (len=*), optional, intent(in) :: s89
  character (len=*), optional, intent(in) :: s90

  character (len=*), optional, intent(in) :: s91
  character (len=*), optional, intent(in) :: s92
  character (len=*), optional, intent(in) :: s93
  character (len=*), optional, intent(in) :: s94
  character (len=*), optional, intent(in) :: s95
  character (len=*), optional, intent(in) :: s96
  character (len=*), optional, intent(in) :: s97
  character (len=*), optional, intent(in) :: s98
  character (len=*), optional, intent(in) :: s99
  character (len=*), optional, intent(in) :: s100

  ! Local variables, copies of optionals
  character (len=len(record)) :: record_here

  !-----------------------------------------------------------------------------

  record_here = record

  call CSV_RECORD_APPEND_S(record_here,   s1)
  call CSV_RECORD_APPEND_S(record_here,   s2)

  if (present(s3))  call CSV_RECORD_APPEND_S(record_here,  s3)
  if (present(s4))  call CSV_RECORD_APPEND_S(record_here,  s4)
  if (present(s5))  call CSV_RECORD_APPEND_S(record_here,  s5)
  if (present(s6))  call CSV_RECORD_APPEND_S(record_here,  s6)
  if (present(s7))  call CSV_RECORD_APPEND_S(record_here,  s7)
  if (present(s8))  call CSV_RECORD_APPEND_S(record_here,  s8)
  if (present(s9))  call CSV_RECORD_APPEND_S(record_here,  s9)
  if (present(s10)) call CSV_RECORD_APPEND_S(record_here, s10)

  if (present(s11)) call CSV_RECORD_APPEND_S(record_here, s11)
  if (present(s12)) call CSV_RECORD_APPEND_S(record_here, s12)
  if (present(s13)) call CSV_RECORD_APPEND_S(record_here, s13)
  if (present(s14)) call CSV_RECORD_APPEND_S(record_here, s14)
  if (present(s15)) call CSV_RECORD_APPEND_S(record_here, s15)
  if (present(s16)) call CSV_RECORD_APPEND_S(record_here, s16)
  if (present(s17)) call CSV_RECORD_APPEND_S(record_here, s17)
  if (present(s18)) call CSV_RECORD_APPEND_S(record_here, s18)
  if (present(s19)) call CSV_RECORD_APPEND_S(record_here, s19)
  if (present(s20)) call CSV_RECORD_APPEND_S(record_here, s20)

  if (present(s21)) call CSV_RECORD_APPEND_S(record_here, s21)
  if (present(s22)) call CSV_RECORD_APPEND_S(record_here, s22)
  if (present(s23)) call CSV_RECORD_APPEND_S(record_here, s23)
  if (present(s24)) call CSV_RECORD_APPEND_S(record_here, s24)
  if (present(s25)) call CSV_RECORD_APPEND_S(record_here, s25)
  if (present(s26)) call CSV_RECORD_APPEND_S(record_here, s26)
  if (present(s27)) call CSV_RECORD_APPEND_S(record_here, s27)
  if (present(s28)) call CSV_RECORD_APPEND_S(record_here, s28)
  if (present(s29)) call CSV_RECORD_APPEND_S(record_here, s29)
  if (present(s30)) call CSV_RECORD_APPEND_S(record_here, s30)

  if (present(s31)) call CSV_RECORD_APPEND_S(record_here, s31)
  if (present(s32)) call CSV_RECORD_APPEND_S(record_here, s32)
  if (present(s33)) call CSV_RECORD_APPEND_S(record_here, s33)
  if (present(s34)) call CSV_RECORD_APPEND_S(record_here, s34)
  if (present(s35)) call CSV_RECORD_APPEND_S(record_here, s35)
  if (present(s36)) call CSV_RECORD_APPEND_S(record_here, s36)
  if (present(s37)) call CSV_RECORD_APPEND_S(record_here, s37)
  if (present(s38)) call CSV_RECORD_APPEND_S(record_here, s38)
  if (present(s39)) call CSV_RECORD_APPEND_S(record_here, s39)
  if (present(s40)) call CSV_RECORD_APPEND_S(record_here, s40)

  if (present(s41)) call CSV_RECORD_APPEND_S(record_here, s41)
  if (present(s42)) call CSV_RECORD_APPEND_S(record_here, s42)
  if (present(s43)) call CSV_RECORD_APPEND_S(record_here, s43)
  if (present(s44)) call CSV_RECORD_APPEND_S(record_here, s44)
  if (present(s45)) call CSV_RECORD_APPEND_S(record_here, s45)
  if (present(s46)) call CSV_RECORD_APPEND_S(record_here, s46)
  if (present(s47)) call CSV_RECORD_APPEND_S(record_here, s47)
  if (present(s48)) call CSV_RECORD_APPEND_S(record_here, s48)
  if (present(s49)) call CSV_RECORD_APPEND_S(record_here, s49)
  if (present(s50)) call CSV_RECORD_APPEND_S(record_here, s50)

  if (present(s51)) call CSV_RECORD_APPEND_S(record_here, s51)
  if (present(s52)) call CSV_RECORD_APPEND_S(record_here, s52)
  if (present(s53)) call CSV_RECORD_APPEND_S(record_here, s53)
  if (present(s54)) call CSV_RECORD_APPEND_S(record_here, s54)
  if (present(s55)) call CSV_RECORD_APPEND_S(record_here, s55)
  if (present(s56)) call CSV_RECORD_APPEND_S(record_here, s56)
  if (present(s57)) call CSV_RECORD_APPEND_S(record_here, s57)
  if (present(s58)) call CSV_RECORD_APPEND_S(record_here, s58)
  if (present(s59)) call CSV_RECORD_APPEND_S(record_here, s59)
  if (present(s60)) call CSV_RECORD_APPEND_S(record_here, s60)

  if (present(s61)) call CSV_RECORD_APPEND_S(record_here, s61)
  if (present(s62)) call CSV_RECORD_APPEND_S(record_here, s62)
  if (present(s63)) call CSV_RECORD_APPEND_S(record_here, s63)
  if (present(s64)) call CSV_RECORD_APPEND_S(record_here, s64)
  if (present(s65)) call CSV_RECORD_APPEND_S(record_here, s65)
  if (present(s66)) call CSV_RECORD_APPEND_S(record_here, s66)
  if (present(s67)) call CSV_RECORD_APPEND_S(record_here, s67)
  if (present(s68)) call CSV_RECORD_APPEND_S(record_here, s68)
  if (present(s69)) call CSV_RECORD_APPEND_S(record_here, s69)
  if (present(s70)) call CSV_RECORD_APPEND_S(record_here, s70)

  if (present(s71)) call CSV_RECORD_APPEND_S(record_here, s71)
  if (present(s72)) call CSV_RECORD_APPEND_S(record_here, s72)
  if (present(s73)) call CSV_RECORD_APPEND_S(record_here, s73)
  if (present(s74)) call CSV_RECORD_APPEND_S(record_here, s74)
  if (present(s75)) call CSV_RECORD_APPEND_S(record_here, s75)
  if (present(s76)) call CSV_RECORD_APPEND_S(record_here, s76)
  if (present(s77)) call CSV_RECORD_APPEND_S(record_here, s77)
  if (present(s78)) call CSV_RECORD_APPEND_S(record_here, s78)
  if (present(s79)) call CSV_RECORD_APPEND_S(record_here, s79)
  if (present(s80)) call CSV_RECORD_APPEND_S(record_here, s80)

  if (present(s81)) call CSV_RECORD_APPEND_S(record_here, s81)
  if (present(s82)) call CSV_RECORD_APPEND_S(record_here, s82)
  if (present(s83)) call CSV_RECORD_APPEND_S(record_here, s83)
  if (present(s84)) call CSV_RECORD_APPEND_S(record_here, s84)
  if (present(s85)) call CSV_RECORD_APPEND_S(record_here, s85)
  if (present(s86)) call CSV_RECORD_APPEND_S(record_here, s86)
  if (present(s87)) call CSV_RECORD_APPEND_S(record_here, s87)
  if (present(s88)) call CSV_RECORD_APPEND_S(record_here, s88)
  if (present(s89)) call CSV_RECORD_APPEND_S(record_here, s89)
  if (present(s90)) call CSV_RECORD_APPEND_S(record_here, s90)

  if (present(s91)) call CSV_RECORD_APPEND_S(record_here, s91)
  if (present(s92)) call CSV_RECORD_APPEND_S(record_here, s92)
  if (present(s93)) call CSV_RECORD_APPEND_S(record_here, s93)
  if (present(s94)) call CSV_RECORD_APPEND_S(record_here, s94)
  if (present(s95)) call CSV_RECORD_APPEND_S(record_here, s95)
  if (present(s96)) call CSV_RECORD_APPEND_S(record_here, s96)
  if (present(s97)) call CSV_RECORD_APPEND_S(record_here, s97)
  if (present(s98)) call CSV_RECORD_APPEND_S(record_here, s98)
  if (present(s99)) call CSV_RECORD_APPEND_S(record_here, s99)
  if (present(s100)) call CSV_RECORD_APPEND_S(record_here,s100)

  record = record_here

end subroutine CSV_RECORD_APPEND_LST_S

!-------------------------------------------------------------------------------

subroutine CSV_MATRIX_WRITE_I4 (matrix, csv_file_name, colnames, csv_file_status)
!*******************************************************************************
! CSV_MATRIX_WRITE_I4
! PURPOSE: Writes a matrix of integers to a CSV data file
! CALL PARAMETERS:
!    Matrix, 2-dimensional, of integers
!    Character CSV_FILE_NAME, the name of the file.
!    Logical CSV_FILE_STATUS, .TRUE. if successfull, no errors
! Modified by Sergey Budaev
!*******************************************************************************

  implicit none

  ! Calling parameters
  integer, dimension(:,:), intent(in) :: matrix
  character (len=*), intent(in) :: csv_file_name
  character (len=*), dimension(:), optional, intent(in) :: colnames
  logical, optional, intent(out) :: csv_file_status

  ! Local variables, copies of optionals
  character (len=255), dimension(:), allocatable :: colnames_here
  logical :: csv_file_status_here

  ! Local variables
  character (len=255), dimension(:), allocatable :: colnames_output
  integer, dimension(:), allocatable             :: len_col
  logical :: write_colnames
  integer :: funit
  character (len=:), allocatable :: csv_record
  integer :: i, j, LBndi, Ubndi, Lbndj, Ubndj
  integer :: max_size_record

  ! Subroutine name for DEBUG LOGGER
  character (len=*), parameter :: PROCNAME = "CSV_MATRIX_WRITE_I4"

  !-----------------------------------------------------------------------------

  ! May need initialise logical on some platforms/compilers
  if (present(csv_file_status)) csv_file_status = .TRUE.

  LBndi=lbound(matrix, 1)   ! Determining bounds for out matrix
  UBndi=ubound(matrix, 1)
  LBndj=lbound(matrix, 2)
  UBndj=ubound(matrix, 2)

  COLNAMES_CHK: if (present(colnames)) then
    write_colnames=.TRUE.
    allocate(colnames_here(lbound(colnames,1):ubound(colnames,1)))
    allocate(colnames_output(LBndj:UBndj))
    allocate(len_col(LBndj:UBndj))
    colnames_here=colnames
    call CSV_COLNAMES(colnames_here, colnames_output)
  else COLNAMES_CHK
    write_colnames=.FALSE.
  end if COLNAMES_CHK

  ! Assess the maximum size of the whole record in advance, we
  ! cannot make record allocatable, now implemented CVS_GUESS_RECORD_LENGTH
  max_size_record = size(matrix, 2) * ( I4_WIDTH(maxval(matrix))+2 )

  call CSV_FILE_OPEN_WRITE (csv_file_name, funit, csv_file_status_here)
  if (.not. csv_file_status_here) then
    if (present(csv_file_status)) csv_file_status=csv_file_status_here
    return
  end if

  COLNAMES_WRT: if (write_colnames) then
    ! determine the maximum length of column names
    do i=LBndj,UBndj
      len_col(i) = len_trim(colnames(i))
    end do
    csv_record=repeat(" ", (abs(UBndj-LBndj)+1)*(maxval(len_col)+4))
    call CSV_RECORD_APPEND_ARRAY_S(csv_record, colnames_output)
    call CSV_FILE_RECORD_WRITE(csv_file_name, funit, csv_record, &
                               csv_file_status_here)
    deallocate(colnames_here)
    deallocate(colnames_output)
    deallocate(len_col)
    if (.not. csv_file_status_here) then
      if (present(csv_file_status)) csv_file_status=csv_file_status_here
      return
    end if
  end if COLNAMES_WRT

  do i=LBndi, UBndi

    csv_record=repeat(" ", max_size_record) ! allocate empty record of max len
    do j=LBndj, UBndj
      call CSV_RECORD_APPEND_I4 (csv_record, matrix(i,j))
    end do

    call CSV_FILE_RECORD_WRITE (csv_file_name, funit, csv_record, &
                                csv_file_status_here)
    if (.not. csv_file_status_here) then
      if (present(csv_file_status)) csv_file_status=csv_file_status_here
      return
    end if

  end do
  call CSV_FILE_CLOSE(csv_file_name, funit, csv_file_status_here)
  if (.not. csv_file_status_here) then
      if (present(csv_file_status)) csv_file_status=csv_file_status_here
    return
  end if

end subroutine CSV_MATRIX_WRITE_I4

!-------------------------------------------------------------------------------

subroutine CSV_MATRIX_WRITE_R4 (matrix, csv_file_name, colnames, csv_file_status)
!*******************************************************************************
! CSV_MATRIX_WRITE_R4
! PURPOSE: Writes a matrix of real type to a CSV data file
! CALL PARAMETERS:
!    Matrix, 2-dimensional, of real type
!    Character CSV_FILE_NAME, the name of the file.
!    Logical CSV_FILE_STATUS, .TRUE. if successfull, no errors
! Modified by Sergey Budaev
!*******************************************************************************

  implicit none

  ! Calling parameters
  real, dimension(:,:), intent(in) :: matrix
  character (len=*), intent(in) :: csv_file_name
  character (len=*), dimension(:), optional, intent(in) :: colnames
  logical, optional, intent(out) :: csv_file_status

  ! Local variables, copies of optionals
  character (len=255), dimension(:), allocatable :: colnames_here
  logical :: csv_file_status_here

  ! Local variables
  character (len=255), dimension(:), allocatable :: colnames_output
  integer, dimension(:), allocatable             :: len_col
  logical :: write_colnames
  integer :: funit
  character (len=:), allocatable :: csv_record
  integer :: i, j, LBndi, Ubndi, Lbndj, Ubndj
  integer :: max_size_record

  ! Subroutine name for DEBUG LOGGER
  character (len=*), parameter :: PROCNAME = "CSV_MATRIX_WRITE_R4"

  !-----------------------------------------------------------------------------

  ! May need initialise logical on some platforms/compilers
  if (present(csv_file_status)) csv_file_status = .TRUE.


  LBndi=lbound(matrix, 1)   ! Determining bounds for out matrix
  UBndi=ubound(matrix, 1)
  LBndj=lbound(matrix, 2)
  UBndj=ubound(matrix, 2)

  COLNAMES_CHK: if (present(colnames)) then
    write_colnames=.TRUE.
    allocate(colnames_here(lbound(colnames,1):ubound(colnames,1)))
    allocate(colnames_output(LBndj:UBndj))
    allocate(len_col(LBndj:UBndj))
    colnames_here=colnames
    call CSV_COLNAMES(colnames_here, colnames_output)
  else COLNAMES_CHK
    write_colnames=.FALSE.
  end if COLNAMES_CHK

  ! Assess the maximum size of the whole record in advance, we
  ! cannot make record allocatable, now implemented CVS_GUESS_RECORD_LENGTH
  max_size_record = size(matrix, 2) * ( I4_WIDTH(int(maxval(matrix)))+14 )

  call CSV_FILE_OPEN_WRITE (csv_file_name, funit, csv_file_status_here)
  if (.not. csv_file_status_here) then
    if (present(csv_file_status)) csv_file_status=csv_file_status_here
    return
  end if

  COLNAMES_WRT: if (write_colnames) then
    ! determine the maximum length of column names
    do i=LBndj,UBndj
      len_col(i) = len_trim(colnames(i))
    end do
    csv_record=repeat(" ", (abs(UBndj-LBndj)+1)*(maxval(len_col)+4))
    call CSV_RECORD_APPEND_ARRAY_S(csv_record, colnames_output)
    call CSV_FILE_RECORD_WRITE(csv_file_name, funit, csv_record, &
                               csv_file_status_here)
    deallocate(colnames_here)
    deallocate(colnames_output)
    deallocate(len_col)
    if (.not. csv_file_status_here) then
      if (present(csv_file_status)) csv_file_status=csv_file_status_here
      return
    end if
  end if COLNAMES_WRT

  do i=LBndi, UBndi

    csv_record=repeat(" ", max_size_record) ! allocate empty record of max len

    do j=LBndj, UBndj
      call CSV_RECORD_APPEND_R4 (csv_record, matrix(i,j))
    end do

    call CSV_FILE_RECORD_WRITE (csv_file_name, funit, csv_record, &
                                csv_file_status_here)
    if (.not. csv_file_status_here) then
      if (present(csv_file_status)) csv_file_status=csv_file_status_here
      return
    end if

  end do
  call CSV_FILE_CLOSE(csv_file_name, funit, csv_file_status_here)
  if (.not. csv_file_status_here) then
    if (present(csv_file_status)) csv_file_status=csv_file_status_here
    return
  end if

end subroutine CSV_MATRIX_WRITE_R4

!-------------------------------------------------------------------------------

subroutine CSV_MATRIX_WRITE_R8 (matrix, csv_file_name, colnames, csv_file_status)
!*******************************************************************************
! CSV_MATRIX_WRITE_R8
! PURPOSE: Writes a matrix of real (kind 8), double, to a CSV data file
! CALL PARAMETERS:
!    Matrix, 2-dimensional, of reals (kind 8)
!    Character CSV_FILE_NAME, the name of the file.
!    Logical CSV_FILE_STATUS, .TRUE. if successfull, no errors
! Modified by Sergey Budaev
!*******************************************************************************

  implicit none

  ! Calling parameters
  real (kind=8), dimension(:,:), intent(in) :: matrix
  character (len=*), intent(in) :: csv_file_name
  character (len=*), dimension(:), optional, intent(in) :: colnames
  logical, optional, intent(out) :: csv_file_status

  ! Local variables, copies of optionals
  character (len=255), dimension(:), allocatable :: colnames_here
  logical :: csv_file_status_here

  ! Local variables
  character (len=255), dimension(:), allocatable :: colnames_output
  integer, dimension(:), allocatable             :: len_col
  logical :: write_colnames
  integer :: funit
  character (len=:), allocatable :: csv_record
  integer :: i, j, LBndi, Ubndi, Lbndj, Ubndj
  integer :: max_size_record

  ! Subroutine name for DEBUG LOGGER
  character (len=*), parameter :: PROCNAME = "CSV_MATRIX_WRITE_R8"

  !-----------------------------------------------------------------------------

  ! May need initialise logical on some platforms/compilers
  if (present(csv_file_status)) csv_file_status = .TRUE.


  LBndi=lbound(matrix, 1)   ! Determining bounds for out matrix
  UBndi=ubound(matrix, 1)
  LBndj=lbound(matrix, 2)
  UBndj=ubound(matrix, 2)

  COLNAMES_CHK: if (present(colnames)) then
    write_colnames=.TRUE.
    allocate(colnames_here(lbound(colnames,1):ubound(colnames,1)))
    allocate(colnames_output(LBndj:UBndj))
    allocate(len_col(LBndj:UBndj))
    colnames_here=colnames
    call CSV_COLNAMES(colnames_here, colnames_output)
  else COLNAMES_CHK
    write_colnames=.FALSE.
  end if COLNAMES_CHK

  ! Assess the maximum size of the whole record in advance, we
  ! cannot make record allocatable, now implemented CVS_GUESS_RECORD_LENGTH
  max_size_record = size(matrix, 2) * ( I4_WIDTH(int(maxval(matrix)))+18 )

  call CSV_FILE_OPEN_WRITE (csv_file_name, funit, csv_file_status_here)
  if (.not. csv_file_status_here) then
    if (present(csv_file_status)) csv_file_status=csv_file_status_here
    return
  end if

  COLNAMES_WRT: if (write_colnames) then
    ! determine the maximum length of column names
    do i=LBndj,UBndj
      len_col(i) = len_trim(colnames(i))
    end do
    csv_record=repeat(" ", (abs(UBndj-LBndj)+1)*(maxval(len_col)+4))
    call CSV_RECORD_APPEND_ARRAY_S(csv_record, colnames_output)
    call CSV_FILE_RECORD_WRITE(csv_file_name, funit, csv_record, &
                               csv_file_status_here)
    deallocate(colnames_here)
    deallocate(colnames_output)
    deallocate(len_col)
    if (.not. csv_file_status_here) then
      if (present(csv_file_status)) csv_file_status=csv_file_status_here
      return
    end if
  end if COLNAMES_WRT

  do i=LBndi, UBndi

    csv_record=repeat(" ", max_size_record) ! allocate empty record of max len
    do j=LBndj, UBndj
      call CSV_RECORD_APPEND_R8 (csv_record, matrix(i,j))
    end do

    call CSV_FILE_RECORD_WRITE (csv_file_name, funit, csv_record, &
                                csv_file_status_here)
    if (.not. csv_file_status_here) then
      if (present(csv_file_status)) csv_file_status=csv_file_status_here
    return
    end if

  end do
  call CSV_FILE_CLOSE(csv_file_name, funit, csv_file_status_here)
  if (.not. csv_file_status_here) then
      if (present(csv_file_status)) csv_file_status=csv_file_status_here
    return
  end if

end subroutine CSV_MATRIX_WRITE_R8

!-------------------------------------------------------------------------------

subroutine CSV_MATRIX_WRITE_S (matrix, csv_file_name, colnames, csv_file_status)
!*******************************************************************************
! CSV_MATRIX_WRITE_S
! PURPOSE: Writes a matrix of character strings to a CSV data file
! CALL PARAMETERS:
!    Matrix, 2-dimensional, of character strings
!    Character CSV_FILE_NAME, the name of the file.
!    Logical CSV_FILE_STATUS, .TRUE. if successfull, no errors
! Modified by Sergey Budaev
!*******************************************************************************

  implicit none

  ! Calling parameters
  character (len=*), dimension(:,:), intent(in) :: matrix
  character (len=*), intent(in) :: csv_file_name
  character (len=*), dimension(:), optional, intent(in) :: colnames
  logical, optional, intent(out) :: csv_file_status

  ! Local variables, copies of optionals
  character (len=255), dimension(:), allocatable :: colnames_here
  logical :: csv_file_status_here

  ! Local variables
  character (len=255), dimension(:), allocatable :: colnames_output
  integer, dimension(:), allocatable             :: len_col
  logical :: write_colnames
  integer :: funit
  character (len=:), allocatable :: csv_record
  integer :: i, j, LBndi, Ubndi, Lbndj, Ubndj
  integer :: max_size_record, max_size_record_count

  ! Subroutine name for DEBUG LOGGER
  character (len=*), parameter :: PROCNAME = "CSV_MATRIX_WRITE_S"

  !-----------------------------------------------------------------------------

  ! May need initialise logical on some platforms/compilers
  if (present(csv_file_status)) csv_file_status = .TRUE.


  LBndi=lbound(matrix, 1)   ! Determining bounds for out matrix
  UBndi=ubound(matrix, 1)
  LBndj=lbound(matrix, 2)
  UBndj=ubound(matrix, 2)

  COLNAMES_CHK: if (present(colnames)) then
    write_colnames=.TRUE.
    allocate(colnames_here(lbound(colnames,1):ubound(colnames,1)))
    allocate(colnames_output(LBndj:UBndj))
    allocate(len_col(LBndj:UBndj))
    colnames_here=colnames
    call CSV_COLNAMES(colnames_here, colnames_output)
  else COLNAMES_CHK
    write_colnames=.FALSE.
  end if COLNAMES_CHK

  ! Assess the maximum size of the whole record in advance, we
  ! cannot make record allocatable
  max_size_record=0
  max_size_record_count=0
  do i=LBndi, UBndi
    do j=LBndj, UBndj
      max_size_record_count=max_size_record_count+len(matrix(i,j))+1
    end do
    max_size_record=max(max_size_record,max_size_record_count)
  end do

  call CSV_FILE_OPEN_WRITE (csv_file_name, funit, csv_file_status_here)
  if (.not. csv_file_status_here) then
    if (present(csv_file_status)) csv_file_status=csv_file_status_here
    return
  end if

  COLNAMES_WRT: if (write_colnames) then
    ! determine the maximum length of column names
    do i=LBndj,UBndj
      len_col(i) = len_trim(colnames(i))
    end do
    csv_record=repeat(" ", (abs(UBndj-LBndj)+1)*(maxval(len_col)+4))
    call CSV_RECORD_APPEND_ARRAY_S(csv_record, colnames_output)
    call CSV_FILE_RECORD_WRITE(csv_file_name, funit, csv_record, &
                               csv_file_status_here)
    deallocate(colnames_here)
    deallocate(colnames_output)
    deallocate(len_col)
    if (.not. csv_file_status_here) then
      if (present(csv_file_status)) csv_file_status=csv_file_status_here
      return
    end if
  end if COLNAMES_WRT

  do i=LBndi, UBndi

    csv_record=repeat(" ", max_size_record) ! allocate empty record of max len
    do j=LBndj, UBndj
      call CSV_RECORD_APPEND_S (csv_record, matrix(i,j))
    end do

    call CSV_FILE_RECORD_WRITE (csv_file_name, funit, csv_record, &
                                csv_file_status_here)
    if (.not. csv_file_status_here) then
      if (present(csv_file_status)) csv_file_status=csv_file_status_here
    return
    end if

  end do
  call CSV_FILE_CLOSE(csv_file_name, funit, csv_file_status_here)
  if (.not. csv_file_status_here) then
      if (present(csv_file_status)) csv_file_status=csv_file_status_here
    return
  end if

end subroutine CSV_MATRIX_WRITE_S

!-------------------------------------------------------------------------------

subroutine CSV_ARRAY_WRITE_I4 (array, csv_file_name, vertical, csv_file_status)
!*******************************************************************************
! CSV_MATRIX_WRITE_I4
! PURPOSE: Writes an array of integers to a CSV data file
! CALL PARAMETERS:
!    Array of integers
!    Character CSV_FILE_NAME, the name of the file.
!    Logical VERTICAL, if .TRUE. (default if absent) write single column
!    Logical CSV_FILE_STATUS, .TRUE. if successfull, no errors
! Modified by Sergey Budaev
!*******************************************************************************

  implicit none

  ! Calling parameters
  integer, dimension(:), intent(in) :: array        ! was matrix
  character (len=*), intent(in) :: csv_file_name
  logical, optional, intent(in) :: vertical
  logical, optional, intent(out) :: csv_file_status

  ! Local variables, copies of optionals
  logical :: vertical_here
  logical :: csv_file_status_here

  ! Local variables
  integer :: funit
  character (len=:), allocatable :: csv_record
  integer :: i, LBndi, Ubndi
  integer :: max_size_record

  ! Subroutine name for DEBUG LOGGER
  character (len=*), parameter :: PROCNAME = "CSV_ARRAY_WRITE_I4"

  !-----------------------------------------------------------------------------

  ! May need initialise logical on some platforms/compilers
  if (present(csv_file_status)) csv_file_status = .TRUE.


  LBndi=lbound(array, 1)      ! Determining bounds for out array
  UBndi=ubound(array, 1)

  if (present(vertical)) then ! If write array "vertically" (single column)
    vertical_here=vertical
  else
    vertical_here=.TRUE.
  end if

  if (vertical_here) then
    ! Write vertically, i.e. in single column

    ! Assess the maximum size of the whole record in advance, we
    ! cannot make record allocatable, now implemented CVS_GUESS_RECORD_LENGTH
    max_size_record = ( I4_WIDTH(maxval(array))+2 )

    call CSV_FILE_OPEN_WRITE (csv_file_name, funit, csv_file_status_here)
    if (.not. csv_file_status_here) then
      if (present(csv_file_status)) csv_file_status=csv_file_status_here
      return
    end if

    do i=LBndi, UBndi

      csv_record=repeat(" ", max_size_record) ! allocate empty record of max len
      call CSV_RECORD_APPEND_I4 (csv_record, array(i))  ! write by rows (down)

      call CSV_FILE_RECORD_WRITE (csv_file_name, funit, csv_record, &
                                  csv_file_status_here)
      if (.not. csv_file_status_here) then
        if (present(csv_file_status)) csv_file_status=csv_file_status_here
      return
      end if

    end do
    call CSV_FILE_CLOSE(csv_file_name, funit, csv_file_status_here)
    if (.not. csv_file_status_here) then
        if (present(csv_file_status)) csv_file_status=csv_file_status_here
      return
    end if

  else ! Write horizontally, i.e. in a single row.

    ! now implemented CVS_GUESS_RECORD_LENGTH
    max_size_record = size(array) * ( I4_WIDTH(maxval(array))+4 )

    call CSV_FILE_OPEN_WRITE (csv_file_name, funit, csv_file_status_here)
    if (.not. csv_file_status_here) then
      if (present(csv_file_status)) csv_file_status=csv_file_status_here
      return
    end if

    csv_record=repeat(" ", max_size_record) ! allocate empty record of max len

    do i=LBndi, UBndi
      call CSV_RECORD_APPEND_I4 (csv_record, array(i))  ! write by cols (right)
    end do

    call CSV_FILE_RECORD_WRITE (csv_file_name, funit, csv_record, &
                                  csv_file_status_here)
    if (.not. csv_file_status_here) then
      if (present(csv_file_status)) csv_file_status=csv_file_status_here
      return
    end if

    call CSV_FILE_CLOSE(csv_file_name, funit, csv_file_status_here)
    if (.not. csv_file_status_here) then
        if (present(csv_file_status)) csv_file_status=csv_file_status_here
      return
    end if

  end if

end subroutine CSV_ARRAY_WRITE_I4

!-------------------------------------------------------------------------------

subroutine CSV_ARRAY_WRITE_R4 (array, csv_file_name, vertical, csv_file_status)
!*******************************************************************************
! CSV_ARRAY_WRITE_R4
! PURPOSE: Writes an array of integers to a CSV data file
! CALL PARAMETERS:
!    Array of reals
!    Character CSV_FILE_NAME, the name of the file.
!    Logical CSV_FILE_STATUS, .TRUE. if successfull, no errors
! Modified by Sergey Budaev
!*******************************************************************************

  implicit none

  ! Calling parameters
  real, dimension(:), intent(in) :: array        ! was matrix
  character (len=*), intent(in) :: csv_file_name
  logical, optional, intent(in) :: vertical
  logical, optional, intent(out) :: csv_file_status

  ! Local variables, copies of optionals
  logical :: vertical_here
  logical :: csv_file_status_here

  ! Local variables
  integer :: funit
  character (len=:), allocatable :: csv_record
  integer :: i, LBndi, Ubndi
  integer :: max_size_record

  ! Subroutine name for DEBUG LOGGER
  character (len=*), parameter :: PROCNAME = "CSV_ARRAY_WRITE_R4"

  !-----------------------------------------------------------------------------

  ! May need initialise logical on some platforms/compilers
  if (present(csv_file_status)) csv_file_status = .TRUE.


  LBndi=lbound(array, 1)      ! Determining bounds for out array
  UBndi=ubound(array, 1)

  if (present(vertical)) then ! If write array "vertically" (single column)
    vertical_here=vertical
  else
    vertical_here=.TRUE.
  end if

  if (vertical_here) then
    ! Write vertically, i.e. in single column

    ! Assess the maximum size of the whole record in advance, we
    ! cannot make record allocatable, now implemented CVS_GUESS_RECORD_LENGTH
    max_size_record = I4_WIDTH(int(maxval(abs(array))))+14

    call CSV_FILE_OPEN_WRITE (csv_file_name, funit, csv_file_status_here)
    if (.not. csv_file_status_here) then
      if (present(csv_file_status)) csv_file_status=csv_file_status_here
      return
    end if

    do i=LBndi, UBndi

      csv_record=repeat(" ", max_size_record) ! allocate empty record of max len
      call CSV_RECORD_APPEND_R4 (csv_record, array(i))  ! write by rows (down)

      call CSV_FILE_RECORD_WRITE (csv_file_name, funit, csv_record, &
                                  csv_file_status_here)
      if (.not. csv_file_status_here) then
        if (present(csv_file_status)) csv_file_status=csv_file_status_here
      return
      end if

    end do
    call CSV_FILE_CLOSE(csv_file_name, funit, csv_file_status_here)
    if (.not. csv_file_status_here) then
        if (present(csv_file_status)) csv_file_status=csv_file_status_here
      return
    end if

  else ! Write horizontally, i.e. in a single row.

    ! now implemented CVS_GUESS_RECORD_LENGTH
    max_size_record = size(array) * (I4_WIDTH(int(maxval(abs(array))))+14)

    call CSV_FILE_OPEN_WRITE (csv_file_name, funit, csv_file_status_here)
    if (.not. csv_file_status_here) then
      if (present(csv_file_status)) csv_file_status=csv_file_status_here
      return
    end if

    csv_record=repeat(" ", max_size_record) ! allocate empty record of max len

    do i=LBndi, UBndi
      call CSV_RECORD_APPEND_R4 (csv_record, array(i))  ! write by cols (right)
    end do

    call CSV_FILE_RECORD_WRITE (csv_file_name, funit, csv_record, &
                                  csv_file_status_here)
    if (.not. csv_file_status_here) then
      if (present(csv_file_status)) csv_file_status=csv_file_status_here
      return
    end if

    call CSV_FILE_CLOSE(csv_file_name, funit, csv_file_status_here)
    if (.not. csv_file_status_here) then
        if (present(csv_file_status)) csv_file_status=csv_file_status_here
      return
    end if

  end if

end subroutine CSV_ARRAY_WRITE_R4

!-------------------------------------------------------------------------------

subroutine CSV_ARRAY_WRITE_R8 (array, csv_file_name, vertical, csv_file_status)
!*******************************************************************************
! CSV_ARRAY_WRITE_R8
! PURPOSE: Writes an array of integers to a CSV data file
! CALL PARAMETERS:
!    Arrays of double precision reals (kind 8)
!    Character CSV_FILE_NAME, the name of the file.
!    Logical CSV_FILE_STATUS, .TRUE. if successfull, no errors
! Modified by Sergey Budaev
!*******************************************************************************

  implicit none

  ! Calling parameters
  real (kind=8), dimension(:), intent(in) :: array        ! was matrix
  character (len=*), intent(in) :: csv_file_name
  logical, optional, intent(in) :: vertical
  logical, optional, intent(out) :: csv_file_status

  ! Local variables, copies of optionals
  logical :: vertical_here
  logical :: csv_file_status_here

  ! Local variables
  integer :: funit
  character (len=:), allocatable :: csv_record
  integer :: i, LBndi, Ubndi
  integer :: max_size_record

  ! Subroutine name for DEBUG LOGGER
  character (len=*), parameter :: PROCNAME = "CSV_ARRAY_WRITE_R8"

  !-----------------------------------------------------------------------------

  ! May need initialise logical on some platforms/compilers
  if (present(csv_file_status)) csv_file_status = .TRUE.


  LBndi=lbound(array, 1)      ! Determining bounds for out array
  UBndi=ubound(array, 1)

  if (present(vertical)) then ! If write array "vertically" (single column)
    vertical_here=vertical
  else
    vertical_here=.TRUE.
  end if

  if (vertical_here) then
    ! Write vertically, i.e. in single column

    ! Assess the maximum size of the whole record in advance, we
    ! cannot make record allocatable, now implemented CVS_GUESS_RECORD_LENGTH
    max_size_record = I4_WIDTH(int(maxval(abs(array))))+14

    call CSV_FILE_OPEN_WRITE (csv_file_name, funit, csv_file_status_here)
    if (.not. csv_file_status_here) then
      if (present(csv_file_status)) csv_file_status=csv_file_status_here
      return
    end if

    do i=LBndi, UBndi

      csv_record=repeat(" ", max_size_record) ! allocate empty record of max len
      call CSV_RECORD_APPEND_R8 (csv_record, array(i))  ! write by rows (down)

      call CSV_FILE_RECORD_WRITE (csv_file_name, funit, csv_record, &
                                  csv_file_status_here)
      if (.not. csv_file_status_here) then
        if (present(csv_file_status)) csv_file_status=csv_file_status_here
      return
      end if

    end do
    call CSV_FILE_CLOSE(csv_file_name, funit, csv_file_status_here)
    if (.not. csv_file_status_here) then
        if (present(csv_file_status)) csv_file_status=csv_file_status_here
      return
    end if

  else ! Write horizontally, i.e. in a single row.

    ! now implemented CVS_GUESS_RECORD_LENGTH
    max_size_record = size(array) * (I4_WIDTH(int(maxval(abs(array))))+14)

    call CSV_FILE_OPEN_WRITE (csv_file_name, funit, csv_file_status_here)
    if (.not. csv_file_status_here) then
      if (present(csv_file_status)) csv_file_status=csv_file_status_here
      return
    end if

    csv_record=repeat(" ", max_size_record) ! allocate empty record of max len

    do i=LBndi, UBndi
      call CSV_RECORD_APPEND_R8 (csv_record, array(i))  ! write by cols (right)
    end do

    call CSV_FILE_RECORD_WRITE (csv_file_name, funit, csv_record, &
                                  csv_file_status_here)
    if (.not. csv_file_status_here) then
      if (present(csv_file_status)) csv_file_status=csv_file_status_here
      return
    end if

    call CSV_FILE_CLOSE(csv_file_name, funit, csv_file_status_here)
    if (.not. csv_file_status_here) then
        if (present(csv_file_status)) csv_file_status=csv_file_status_here
      return
    end if

  end if

end subroutine CSV_ARRAY_WRITE_R8

!-------------------------------------------------------------------------------

subroutine CSV_ARRAY_WRITE_S (array, csv_file_name, vertical, csv_file_status)
!*******************************************************************************
! CSV_ARRAY_WRITE_S
! PURPOSE: Writes an array of integers to a CSV data file
! CALL PARAMETERS:
!    Array of strings
!    Character CSV_FILE_NAME, the name of the file.
!    Logical CSV_FILE_STATUS, .TRUE. if successfull, no errors
! Modified by Sergey Budaev
!*******************************************************************************

  implicit none

  ! Calling parameters
  character (len=*), dimension(:), intent(in) :: array        ! was matrix
  character (len=*), intent(in) :: csv_file_name
  logical, optional, intent(in) :: vertical
  logical, optional, intent(out) :: csv_file_status

  ! Local variables, copies of optionals
  logical :: vertical_here
  logical :: csv_file_status_here

  ! Local variables
  integer :: funit
  character (len=:), allocatable :: csv_record
  integer :: i, LBndi, Ubndi
  integer :: max_size_record

  ! Subroutine name for DEBUG LOGGER
  character (len=*), parameter :: PROCNAME = "CSV_ARRAY_WRITE_S"

  !-----------------------------------------------------------------------------

  ! May need initialise logical on some platforms/compilers
  if (present(csv_file_status)) csv_file_status = .TRUE.


  LBndi=lbound(array, 1)      ! Determining bounds for out array
  UBndi=ubound(array, 1)

  if (present(vertical)) then ! If write array "vertically" (single column)
    vertical_here=vertical
  else
    vertical_here=.TRUE.
  end if

  if (vertical_here) then
    ! Write vertically, i.e. in single column

    ! Assess the maximum size of the whole record in advance, we
    ! cannot make record allocatable
    max_size_record=4
    do i=LBndi, UBndi
      max_size_record=max(max_size_record, len_trim(array(i))) + 4
    end do

    call CSV_FILE_OPEN_WRITE (csv_file_name, funit, csv_file_status_here)
    if (.not. csv_file_status_here) then
      if (present(csv_file_status)) csv_file_status=csv_file_status_here
      return
    end if

    do i=LBndi, UBndi

      csv_record=repeat(" ", max_size_record) ! allocate empty record of max len
      call CSV_RECORD_APPEND_S (csv_record, array(i))  ! write by rows (down)

      call CSV_FILE_RECORD_WRITE (csv_file_name, funit, csv_record, &
                                  csv_file_status_here)
      if (.not. csv_file_status_here) then
        if (present(csv_file_status)) csv_file_status=csv_file_status_here
      return
      end if

    end do
    call CSV_FILE_CLOSE(csv_file_name, funit, csv_file_status_here)
    if (.not. csv_file_status_here) then
        if (present(csv_file_status)) csv_file_status=csv_file_status_here
      return
    end if

  else ! Write horizontally, i.e. in a single row.

    ! Assess the maximum size of the whole record in advance, we
    ! cannot make record allocatable
    max_size_record=8
    do i=LBndi, UBndi
      max_size_record = max_size_record + len_trim(array(i)) + 4
    end do

    call CSV_FILE_OPEN_WRITE (csv_file_name, funit, csv_file_status_here)
    if (.not. csv_file_status_here) then
      if (present(csv_file_status)) csv_file_status=csv_file_status_here
      return
    end if

    csv_record=repeat(" ", max_size_record) ! allocate empty record of max len

    do i=LBndi, UBndi
      call CSV_RECORD_APPEND_S (csv_record, array(i))  ! write by cols (right)
    end do

    call CSV_FILE_RECORD_WRITE (csv_file_name, funit, csv_record, &
                                  csv_file_status_here)
    if (.not. csv_file_status_here) then
      if (present(csv_file_status)) csv_file_status=csv_file_status_here
      return
    end if

    call CSV_FILE_CLOSE(csv_file_name, funit, csv_file_status_here)
    if (.not. csv_file_status_here) then
        if (present(csv_file_status)) csv_file_status=csv_file_status_here
      return
    end if

  end if

end subroutine CSV_ARRAY_WRITE_S

!-------------------------------------------------------------------------------

function CSV_GUESS_RECORD_LEN_I4 (n_values, max_target_value) &
    result (record_size_guess)
!*******************************************************************************
! CSV_GUESS_RECORD_LEN_I4
! PURPOSE: Guesses the maximum length of the CSV character record
! CALL PARAMETERS:
!     Integer maximum number of values in the record, a target value, its
!     exact value is not important, only type (integer or real) and number of
!     characters matters.
! Author Sergey Budaev
!*******************************************************************************

  ! Function value
  integer :: record_size_guess

  ! Calling parameters
  integer :: n_values
  integer :: max_target_value

  ! Subroutine name for DEBUG LOGGER
  character (len=*), parameter :: PROCNAME = "CSV_GUESS_RECORD_LEN_I4"

  !-----------------------------------------------------------------------------

  record_size_guess = n_values * (I4_WIDTH(max_target_value)+4 )

end function CSV_GUESS_RECORD_LEN_I4

!-------------------------------------------------------------------------------

function CSV_GUESS_RECORD_LEN_R4 (n_values, max_target_value) &
    result (record_size_guess)
!*******************************************************************************
! CSV_GUESS_RECORD_LEN_R4
! PURPOSE: Guesses the maximum length of the CSV character record
! CALL PARAMETERS:
!     Integer maximum number of values in the record, a target value, its
!     exact value is not important, only type (integer or real) and number of
!     characters matters. This is the real(type=4) version.
! Author Sergey Budaev
!*******************************************************************************

  ! Function value
  integer :: record_size_guess

  ! Calling parameters
  integer :: n_values
  real :: max_target_value

  ! Subroutine name for DEBUG LOGGER
  character (len=*), parameter :: PROCNAME = "CSV_GUESS_RECORD_LEN_R4"

  !-----------------------------------------------------------------------------

  record_size_guess = n_values * (I4_WIDTH(int(max_target_value))+14)

end function CSV_GUESS_RECORD_LEN_R4

!-------------------------------------------------------------------------------

function CSV_GUESS_RECORD_LEN_R8 (n_values, max_target_value) &
    result (record_size_guess)
!*******************************************************************************
! CSV_GUESS_RECORD_LEN_R8
! PURPOSE: Guesses the maximum length of the CSV character record
! CALL PARAMETERS:
!     Integer maximum number of values in the record, a target value, its
!     exact value is not important, only type (integer or real) and number of
!     characters matters. This is the real(type=8) version.
! Author Sergey Budaev
!*******************************************************************************

  ! Function value
  integer :: record_size_guess

  ! Calling parameters
  integer :: n_values
  real(kind=8) :: max_target_value

  ! Subroutine name for DEBUG LOGGER
  character (len=*), parameter :: PROCNAME = "CSV_GUESS_RECORD_LEN_R8"

  !-----------------------------------------------------------------------------

  record_size_guess = n_values * (I4_WIDTH(int(max_target_value))+14)

end function CSV_GUESS_RECORD_LEN_R8

!-------------------------------------------------------------------------------

subroutine CSV_COLNAMES(names_in, names_out)
!*******************************************************************************
! CSV_COLNAMES
! PURPOSE: Composes column names across two string arrays, copies from names_in
!          to names_out and fill possible insufficient names with "COL_XXX"
! CALL PARAMETERS:
! NOTE: Fortran does not pass exact limits to subroutines with assumed shape
!          arrays, they use just the default start index (1). I discovered
!          this only during debugging this sub. It works ok assuming both arrays
!          start from 1 though. Do transform args to pointer?
! Author Sergey Budaev
!*******************************************************************************

  ! Calling parameters
  character (len=*), dimension (:), intent(in) :: names_in
  character (len=*), dimension (:), intent(out) :: names_out

  ! Local variables
  integer :: LBndi_in, UBndi_in   ! bounds for col names
  integer :: LBndi_out, UBndi_out !   arrays
  integer :: LBndi_whl, UBndi_whl !   and full names array
  integer :: maxdigits_whl        ! length of whole array
  integer :: i                    ! counters
  character (len=255), dimension(:), allocatable :: names_whole

  ! Subroutine name for DEBUG LOGGER
  character (len=*), parameter :: PROCNAME = "CSV_COLNAMES"

  !-----------------------------------------------------------------------------

  LBndi_in=lbound(names_in, 1)      ! Determining bounds for out array
  UBndi_in=ubound(names_in, 1)
  LBndi_out=lbound(names_out, 1)    ! exact bounds do not seem to be passed
  UBndi_out=ubound(names_out, 1)    ! only shape & size important

  LBndi_whl=min(LBndi_in,LBndi_out)           ! set size for the whole array of
  UBndi_whl=max(UBndi_in,UBndi_out)           !  names..

  allocate(names_whole(LBndi_whl:UBndi_whl))  ! and allocate whole names array

  maxdigits_whl=max(abs(LBndi_whl),abs(UBndi_whl)) ! max number of digits

  do i=LBndi_whl, UBndi_whl
    names_whole(i) ="COL_" // STR_ITOA_LZ(i, maxdigits_whl)
  end do

  do i=LBndi_in, UBndi_in
    names_whole(i) = names_in(i)
  end do

  do i=LBndi_out, UBndi_out
    names_out(i) = names_whole(i)
  end do

end subroutine CSV_COLNAMES

!-------------------------------------------------------------------------------
! Private objects follow:
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

function STR_ITOA(i, formatstr) result (ToStrA)  ! PRIVATE HERE, from UTILS
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

function STR_ITOA_LZ(i, maxi) result (ToStrA)  ! PRIVATE HERE, from UTILS
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
    ToStrA = "-" // repeat("0", fwidth-iwidth+1) // STR_ITOA(abs(i))
  else
    ToStrA = repeat("0", fwidth-iwidth) // STR_ITOA(i)
  end if

end function STR_ITOA_LZ

!-------------------------------------------------------------------------------

function CLEANUP(instring) result (cleaned)  ! PRIVATE HERE, from UTILS
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

function READLINE (aunit, InLine, trimmed) result (OK)
!*******************************************************************************
! READLINE
! PURPOSE: Read a line of arbitrary length from an input unit.
! CALL PARAMETERS:
!    Integer unit number.
!    Output allocatable string
!    Optional logical indicator flag to trim the output strings
!    Return read success: TRUE if read successful
!
! Obtained from Stackoverflow.com, author unknown (wordy)
!  (answered Feb 22 '14 at 11:23):
! URL: https://stackoverflow.com/questions/14765382/
!             reading-a-character-string-of-unknown-length
!
! NOTES:
!     Here's a function for Fortran 2003, which sets an allocatable string
!     (InLine) of exactly the length of the input string (optionally trimmed),
!     or returns .false. if end of file.!
!     For example to do something with all lines in a file with unit "aunit"
!       do:
!     .........................................................
!     character(LEN=:), allocatable :: InLine
!     ....
!     do while (ReadLine(aunit, InLine))
!       [.. something with InLine]
!     end do
!     .........................................................
!*******************************************************************************

integer, intent(IN) :: aunit
character(LEN=:), allocatable, optional :: InLine
logical, intent(in), optional :: trimmed

integer, parameter :: line_buf_len= 1024*4
character(LEN=line_buf_len) :: InS
logical :: OK, set
integer status, size

OK = .false.
set = .true.

do
    read (aunit,'(a)',advance='NO',iostat=status, size=size) InS
    OK = .not. IS_IOSTAT_END(status)
    if (.not. OK) return
    if (present(InLine)) then
        if (set) then
            InLine = InS(1:size)
            set=.false.
        else
            InLine = InLine // InS(1:size)
        end if
    end if
    if (IS_IOSTAT_EOR(status)) exit
end do

if (present(trimmed) .and. present(InLine)) then
    if (trimmed) InLine = trim(adjustl(InLine))
end if

end function READLINE

!-------------------------------------------------------------------------------

function CSV_MATRIX_READ_R4 (csv_file_name, csv_file_status, &
                  include_incomplete_records, missing_code) result (matrix_out)
!*******************************************************************************
! CSV_MATRIX_READ_R4
! PURPOSE: Reads a matrix of real type from a CSV data file
! RETURNS:
!    Allocatable matrix of the data. The sizes of the dimensions can be
!      determined using intrinsic procedures (e.g. size). If the data file was
!      not read successfully, the return matrix is allocated to zero size (0,0).
!    The output matrix **must** be declared as:
!       real, dimension(:,:), allocatable :: MATRIX
! CALL PARAMETERS:
!    Character CSV_FILE_NAME, the name of the file.
!    Logical CSV_FILE_STATUS, .TRUE. if successfull, no errors
!    Optional logical flag to include incomplete records when the number of
!       columns in the data file is shorter than determined. If set to TRUE,
!       missing data values will be substituted by missing data code (default
!       -9999.0)
!    Optional missing data code that substitutes the default -9999.0
!
! Author: Sergey Budaev
!*******************************************************************************

  ! Calling parameters
  real, dimension(:,:), allocatable :: matrix_out
  character (len=*), intent(in) :: csv_file_name
  logical, optional, intent(out) :: csv_file_status
  logical, optional, intent(in) :: include_incomplete_records
  real, optional, intent(in) :: missing_code

  ! Local variables, operations with files
  integer :: file_unit, error_iflag
  logical :: file_status

  ! Local variables, operations with the data array.
  real, dimension(:,:), allocatable :: matrix
  integer :: i, lines_infile, iline, icase, jfield, line_data_buff_length
  character(len=:), allocatable :: line_data_buff
  integer, parameter :: LEN_CSV_FIELD = 64 ! Single field within record.
  integer, parameter :: MIN_FIELD = 2 ! Minimum length of a data field
  character(len=LEN_CSV_FIELD), dimension(:), allocatable  ::                 &
                                                          line_data_substrings
  integer :: line_data_nflds
  real, allocatable, dimension(:) :: matrix_row
  logical :: include_incomplete
  real ::missing_code_here
  logical, allocatable, dimension(:) :: include_vector
  real, parameter :: MISSING = -9999.0
  logical :: not_a_data_row
  integer ::nvars
  integer ::ncases

  ! Delimiters for data fields:
  character, parameter :: TAB =achar(9)
  character(len=*), parameter :: SDELIM = " ," // TAB

  ! Check optional parameters: flag to include incomplete records
  ! (substitutes them with missing data code)
  if (present(include_incomplete_records)) then
    include_incomplete = include_incomplete_records
  else
    include_incomplete = .FALSE.
  end if

  ! Check optional parameters: Optional missing data code
  ! for incomplete records.
  if(present(missing_code)) then
    missing_code_here = missing_code
  else
    missing_code_here = MISSING
  end if

  file_status = .TRUE.

  ! First, get a free unit number.
  file_unit=GET_FREE_FUNIT(file_status, MAX_UNIT)
  if (.NOT. file_status) then ! File opening error, exit straight away.
    allocate(matrix_out(0,0)) ! But have to allocate the output return matrix.
    if (present(csv_file_status)) csv_file_status = .FALSE.
    return
  end if

  ! Second, calculate the number of *numeric-only* lines in the file.
  ncases = CSV_FILE_LINES_COUNT(  csv_file_name=csv_file_name,                &
                                  numeric_only=.TRUE.,                        &
                                  csv_file_status=file_status )
  if (.NOT. file_status) then ! File opening error, try to close and exit
    goto 1000
  end if

  ! Then try to open the CSV file.
  call CSV_FILE_OPEN_READ (csv_file_name, file_unit, file_status)
  if (.NOT. file_status) then ! File opening error, try to close and exit
    goto 1000
  end if

  iline = 0
  icase = 0
  nvars = 0

  ! Now cycle through the data lines using the non-advancing READLINE
  do while ( READLINE(file_unit, line_data_buff, .TRUE.) )

    iline = iline + 1

    line_data_buff_length = len_trim(line_data_buff)

    ! An approximate upper bound for the number of data fields in the
    ! current record is len_trim(line_data_buff)/MIN_FIELD), so we can now
    ! allocate the temporary array of substrings.
    ! This is also the number of variables.
    allocate(line_data_substrings(line_data_buff_length/MIN_FIELD))

    call PARSE ( line_data_buff, SDELIM, line_data_substrings, line_data_nflds )

    ! We have to allocate the array of row values and initialise it for correct
    ! processing by the VALUE subroutine.
    allocate(matrix_row(line_data_nflds))
    matrix_row = missing_code_here

    ! Only non-empty lines are considered data.
    if ( line_data_buff_length>0 ) then
      if (nvars > line_data_nflds ) then
        if (.not. include_incomplete ) then
          ncases = ncases - 1
          not_a_data_row = .TRUE.
        end if
      else
        not_a_data_row = .FALSE.
      end if
    else
      not_a_data_row = .TRUE.
    end if

    do jfield=1, line_data_nflds
      if (IS_NUMERIC(line_data_substrings(jfield))) then
        call VALUE( trim(line_data_substrings(jfield)), matrix_row(jfield),   &
                    error_iflag)
      else
        matrix_row(jfield) = missing_code_here
        not_a_data_row = .TRUE.
      end if
    end do

    if (.not. not_a_data_row .and. line_data_buff_length>0 ) then
      icase = icase + 1
      if (icase>1) include_vector(icase) = .TRUE.
    end if
    ! If this is the first numeric case, we can assess the number of variables
    ! and allocate the output data matrix.
    if (icase == 1) then
      if (.not. allocated(matrix)) then  ! if we get there but matrix allocated
        nvars = line_data_nflds          ! then  we got the second incomplete
        allocate(matrix(ncases, nvars))  ! record, but icase not yet updated to
        allocate(include_vector(ncases)) ! to 2
        include_vector = .FALSE.
        include_vector(icase) = .TRUE.
      end if
    end if

    if (.not. not_a_data_row) then
      if (line_data_nflds<nvars) then
        matrix(icase,1:line_data_nflds) = matrix_row(1:line_data_nflds)
        matrix(icase,line_data_nflds+1:nvars) = missing_code_here
      else
        matrix(icase,1:nvars) = matrix_row(1:nvars)
      end if
    end if

    deallocate(matrix_row)
    deallocate(line_data_substrings)

  end do

  allocate(matrix_out(ncases, nvars)) ! We can now allocate the return matrix.

  do i=1, ncases
    if(include_vector(i)) matrix_out(i,1:nvars) = matrix(i,1:nvars)
  end do

  if (present(csv_file_status)) csv_file_status = .TRUE.
  return

      ! File error, return after attempting to close straight away.
1000  call CSV_FILE_CLOSE(csv_file_unit=file_unit)
      allocate(matrix_out(0,0)) ! But have to allocate the output return matrix.
      if (present(csv_file_status)) csv_file_status = .FALSE.
      return

end function CSV_MATRIX_READ_R4

end module CSV_IO
