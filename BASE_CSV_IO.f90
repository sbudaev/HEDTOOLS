module CSV_IO
!*******************************************************************************
! CSV_IO
! PURPOSE: This module provides high level functions for input and output in
!          CSV (Comma Seperated Values) file format along with a few other
!          basic utilities. CSV is accessible by LibreOffice and MS Excel.
! VERSION AND DATE: 0.3, 2015/11/30
! CONTENTS:
!    TODO: **** bla bla bla
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

implicit none

! Public constants
integer, public, parameter :: MAX_UNIT=99       ! Maximum unit number (in old
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
! DERIVED TYP for CSV FILE HANDLE
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
! zoutput%name= trim(directory) // "file_" // TOSTR(number) // ".txt"
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

! Generic interfaces for whole array/matrix operations with arbitrary data types

interface CSV_RECORD_APPEND

  module procedure CSV_RECORD_APPEND_I4
  module procedure CSV_RECORD_APPEND_R4
  module procedure CSV_RECORD_APPEND_R8
  module procedure CSV_RECORD_APPEND_S

end interface CSV_RECORD_APPEND

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

private :: LOG_DBG  ! This wrapper DEBUG LOG is used only for debugging this
                    ! module. It may not or may use the module LOGGER, if not,
                    ! (normally)  it can be used as a stand-alone module in
                    ! other projects...

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

function CSV_FILE_LINES_COUNT (csv_file_name, csv_file_status) &
        result (line_num)
!*******************************************************************************
! CSV_FILE_LINE_COUNT
! PURPOSE: counts the number of lines in a CSV file.
! CALL PARAMETERS:
!    Optional character CSV_FILE_NAME, the name of the file.
!    Optional integer LINE_NUM, the number of lines.
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
  character (len=*), intent(in) :: csv_file_name
  logical, optional, intent(out) :: csv_file_status

  ! Local variables, copies of optionals
  logical :: csv_file_status_here

  ! Local variables
  integer :: ierror
  integer :: input_status
  integer :: input_unit
  character (len=:), allocatable :: line

  ! Subroutine name for DEBUG LOGGER
  character (len=*), parameter :: PROCNAME = "CSV_FILE_LINES_COUNT"

  !-----------------------------------------------------------------------------

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

  do
    read (input_unit, '(a)', iostat = input_status) line
    if (input_status /= 0) then
      ierror=line_num
      exit  ! leave the do loop here
    end if
    line_num=line_num + 1
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

subroutine CSV_MATRIX_WRITE_I4 (matrix, csv_file_name, csv_file_status)
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
  logical, optional, intent(out) :: csv_file_status

  ! Local variables, copies of optionals
  logical :: csv_file_status_here

  ! Local variables
  integer :: funit
  character (len=:), allocatable :: csv_record
  integer :: i, j, LBndi, Ubndi, Lbndj, Ubndj
  integer :: max_size_record

  ! Subroutine name for DEBUG LOGGER
  character (len=*), parameter :: PROCNAME = "CSV_MATRIX_WRITE_I4"

  !-----------------------------------------------------------------------------

  LBndi=lbound(matrix, 1)   ! Determining bounds for out matrix
  UBndi=ubound(matrix, 1)
  LBndj=lbound(matrix, 2)
  UBndj=ubound(matrix, 2)

  ! Assess the maximum size of the whole record in advance, we
  ! cannot make record allocatable
  max_size_record = size(matrix, 2) * ( I4_WIDTH(maxval(matrix))+2 )

  call CSV_FILE_OPEN_WRITE (csv_file_name, funit, csv_file_status_here)
  if (.not. csv_file_status_here) then
    if (present(csv_file_status)) csv_file_status=csv_file_status_here
    return
  end if

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

subroutine CSV_MATRIX_WRITE_R4 (matrix, csv_file_name, csv_file_status)
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
  logical, optional, intent(out) :: csv_file_status

  ! Local variables, copies of optionals
  logical :: csv_file_status_here

  ! Local variables
  integer :: funit
  character (len=:), allocatable :: csv_record
  integer :: i, j, LBndi, Ubndi, Lbndj, Ubndj
  integer :: max_size_record

  ! Subroutine name for DEBUG LOGGER
  character (len=*), parameter :: PROCNAME = "CSV_MATRIX_WRITE_R4"

  !-----------------------------------------------------------------------------

  LBndi=lbound(matrix, 1)   ! Determining bounds for out matrix
  UBndi=ubound(matrix, 1)
  LBndj=lbound(matrix, 2)
  UBndj=ubound(matrix, 2)

  ! Assess the maximum size of the whole record in advance, we
  ! cannot make record allocatable
  max_size_record = size(matrix, 2) * ( I4_WIDTH(int(maxval(matrix)))+14 )

  call CSV_FILE_OPEN_WRITE (csv_file_name, funit, csv_file_status_here)
  if (.not. csv_file_status_here) then
    if (present(csv_file_status)) csv_file_status=csv_file_status_here
    return
  end if

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

subroutine CSV_MATRIX_WRITE_R8 (matrix, csv_file_name, csv_file_status)
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
  logical, optional, intent(out) :: csv_file_status

  ! Local variables, copies of optionals
  logical :: csv_file_status_here

  ! Local variables
  integer :: funit
  character (len=:), allocatable :: csv_record
  integer :: i, j, LBndi, Ubndi, Lbndj, Ubndj
  integer :: max_size_record

  ! Subroutine name for DEBUG LOGGER
  character (len=*), parameter :: PROCNAME = "CSV_MATRIX_WRITE_R8"

  !-----------------------------------------------------------------------------

  LBndi=lbound(matrix, 1)   ! Determining bounds for out matrix
  UBndi=ubound(matrix, 1)
  LBndj=lbound(matrix, 2)
  UBndj=ubound(matrix, 2)

  ! Assess the maximum size of the whole record in advance, we
  ! cannot make record allocatable
  max_size_record = size(matrix, 2) * ( I4_WIDTH(int(maxval(matrix)))+18 )

  call CSV_FILE_OPEN_WRITE (csv_file_name, funit, csv_file_status_here)
  if (.not. csv_file_status_here) then
    if (present(csv_file_status)) csv_file_status=csv_file_status_here
    return
  end if

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

subroutine CSV_MATRIX_WRITE_S (matrix, csv_file_name, csv_file_status)
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
  logical, optional, intent(out) :: csv_file_status

  ! Local variables, copies of optionals
  logical :: csv_file_status_here

  ! Local variables
  integer :: funit
  character (len=:), allocatable :: csv_record
  integer :: i, j, LBndi, Ubndi, Lbndj, Ubndj
  integer :: max_size_record, max_size_record_count

  ! Subroutine name for DEBUG LOGGER
  character (len=*), parameter :: PROCNAME = "CSV_MATRIX_WRITE_S"

  !-----------------------------------------------------------------------------

  LBndi=lbound(matrix, 1)   ! Determining bounds for out matrix
  UBndi=ubound(matrix, 1)
  LBndj=lbound(matrix, 2)
  UBndj=ubound(matrix, 2)

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

subroutine CSV_ARRAY_WRITE_I4 (array, csv_file_name, csv_file_status)
!*******************************************************************************
! CSV_MATRIX_WRITE_I4
! PURPOSE: Writes an array of integers to a CSV data file
! CALL PARAMETERS:
!    Array of integers
!    Character CSV_FILE_NAME, the name of the file.
!    Logical CSV_FILE_STATUS, .TRUE. if successfull, no errors
! Modified by Sergey Budaev
!*******************************************************************************

  implicit none

  ! Calling parameters
  integer, dimension(:), intent(in) :: array        ! was matrix
  character (len=*), intent(in) :: csv_file_name
  logical, optional, intent(out) :: csv_file_status

  ! Local variables, copies of optionals
  logical :: csv_file_status_here

  ! Local variables
  integer :: funit
  character (len=:), allocatable :: csv_record
  integer :: i, LBndi, Ubndi
  integer :: max_size_record

  ! Subroutine name for DEBUG LOGGER
  character (len=*), parameter :: PROCNAME = "CSV_ARRAY_WRITE_I4"

  !-----------------------------------------------------------------------------

  ! Assess the maximum size of the whole record in advance, we
  ! cannot make record allocatable
  ! TODO: make option to write array in a single row
  !max_size_record = size(array) * ( I4_WIDTH(maxval(array))+1 )
  max_size_record = ( I4_WIDTH(maxval(array))+2 )

  LBndi=lbound(array, 1)   ! Determining bounds for out array
  UBndi=ubound(array, 1)

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

end subroutine CSV_ARRAY_WRITE_I4

!-------------------------------------------------------------------------------

subroutine CSV_ARRAY_WRITE_R4 (array, csv_file_name, csv_file_status)
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
  logical, optional, intent(out) :: csv_file_status

  ! Local variables, copies of optionals
  logical :: csv_file_status_here

  ! Local variables
  integer :: funit
  character (len=:), allocatable :: csv_record
  integer :: i, LBndi, Ubndi
  integer :: max_size_record

  ! Subroutine name for DEBUG LOGGER
  character (len=*), parameter :: PROCNAME = "CSV_ARRAY_WRITE_R4"

  !-----------------------------------------------------------------------------

  ! Assess the maximum size of the whole record in advance, we
  ! cannot make record allocatable
  ! TODO: make option to write array in a single row
  !max_size_record = size(array) * ( I4_WIDTH(int(maxval(array)))+10 )
  max_size_record = I4_WIDTH(int(maxval(array)))+14

  LBndi=lbound(array, 1)   ! Determining bounds for out array
  UBndi=ubound(array, 1)

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

end subroutine CSV_ARRAY_WRITE_R4

!-------------------------------------------------------------------------------

subroutine CSV_ARRAY_WRITE_R8 (array, csv_file_name, csv_file_status)
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
  logical, optional, intent(out) :: csv_file_status

  ! Local variables, copies of optionals
  logical :: csv_file_status_here

  ! Local variables
  integer :: funit
  character (len=255) :: csv_record   ! TODO: make allocatable + allocate to necessary size
  integer :: i, LBndi, Ubndi
  integer :: max_size_record

  ! Subroutine name for DEBUG LOGGER
  character (len=*), parameter :: PROCNAME = "CSV_ARRAY_WRITE_R8"

  !-----------------------------------------------------------------------------
  ! Assess the maximum size of the whole record in advance, we
  ! cannot make record allocatable
  ! TODO: make option to write array in a single row
  max_size_record = size(array) * ( I4_WIDTH(int(maxval(array)))+1 )
  max_size_record = I4_WIDTH(int(maxval(array)))+18

  LBndi=lbound(array, 1)   ! Determining bounds for out array
  UBndi=ubound(array, 1)

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

end subroutine CSV_ARRAY_WRITE_R8

!-------------------------------------------------------------------------------

subroutine CSV_ARRAY_WRITE_S (array, csv_file_name, csv_file_status)
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
  logical, optional, intent(out) :: csv_file_status

  ! Local variables, copies of optionals
  logical :: csv_file_status_here

  ! Local variables
  integer :: funit
  character (len=:), allocatable :: csv_record
  integer :: i, LBndi, Ubndi
  integer :: max_size_record

  ! Subroutine name for DEBUG LOGGER
  character (len=*), parameter :: PROCNAME = "CSV_ARRAY_WRITE_S"

  !-----------------------------------------------------------------------------

  LBndi=lbound(array, 1)   ! Determining bounds for out array
  UBndi=ubound(array, 1)

  ! Assess the maximum size of the whole record in advance, we
  ! cannot make record allocatable
  max_size_record=0
  do i=LBndi, UBndi
    max_size_record=max_size_record+len(array(i))+1
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

end subroutine CSV_ARRAY_WRITE_S

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

end module CSV_IO
