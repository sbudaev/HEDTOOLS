module ERRORS
!*******************************************************************************
! PURPOSE:
! Generic error message handler The routines in this module do not halt program
! execution; they merely store error messages for subsequent
! retrieval.
!
! Design by J.L. Schafer
!
! CONTENTS:
!
! EXAMPLE:
!
! NOTES:
!
!*******************************************************************************
implicit none

private

public :: error_type
public :: ERR_RESET, ERR_HANDLE, ERR_MSG_PRESENT, ERR_GET_MSGS

! max width of any single error message line
integer, parameter :: err_msg_width = 70

!-------------------------------------------------------------------
! Private type for a single node in the linked list sequence
type :: msg_line_type
  character (len=err_msg_width) :: line = ""
  type(msg_line_type), pointer :: next => null()
end type msg_line_type

!-------------------------------------------------------------------
! Public type for holding a linked list of messages sequence
type :: error_type
  private             ! content is private
  logical :: msg_present = .false.
  type(msg_line_type), pointer :: head => null(), tail => null()
end type error_type

!-------------------------------------------------------------------
contains

subroutine ERR_RESET( err )
!*******************************************************************************
! ERR_RESET
! PURPOSE:
!   Public: deletes all messages from the list
! CALL PARAMETERS: derived error_type code
! NOTES:
!*******************************************************************************

  implicit none

  type(error_type), intent(inout) :: err
  type(msg_line_type), pointer :: current_line

  if( .not. err%msg_present ) return

  do
    current_line => err%head
    err%head => err%head%next
    deallocate( current_line )
    if( .not.associated(err%head) ) exit
  end do

  nullify(err%tail)
  err%msg_present = .false.

end subroutine ERR_RESET

!-------------------------------------------------------------------------------

logical function ERR_MSG_PRESENT( err )
!*******************************************************************************
! ERR_MSG_PRESENT
! PURPOSE:
!   Public: Queries the error_type to see if a message is present
! CALL PARAMETERS: derived error_type code
! RETURNS:
! NOTES:
!*******************************************************************************

implicit none

type(error_type), intent(inout) :: err

  err_msg_present = err%msg_present

end function ERR_MSG_PRESENT

!-------------------------------------------------------------------------------

subroutine INSERT_MSG_LINE( text_line, err )
!*******************************************************************************
! INSERT_MSG_LINE
! PURPOSE:
!   Inserts a new message line at the end of the list
! CALL PARAMETERS: derived error_type code
! RETURNS:
! NOTES:
!*******************************************************************************

  implicit none

  ! declare arguments

  character(len=*), intent(in) :: text_line
  type(error_type), intent(inout) :: err

  ! begin
  if( .not. err%msg_present ) then
    ! begin a linked list
    allocate( err%head )
    err%tail => err%head
    err%head%line = text_line
    err%msg_present = .true.
  else
    ! add a node to the list; point tail to the new node
    allocate( err%tail%next )
    err%tail => err%tail%next
    err%tail%line = text_line
  end if

end subroutine INSERT_MSG_LINE

!-------------------------------------------------------------------------------

subroutine ERR_HANDLE( err, err_code, called_from, file_name, &
                        line_no, object_name, custom_1, custom_2, custom_3 )
!*******************************************************************************
! ERR_HANDLE
! PURPOSE:
!   Public: Stores a message in the error handler
! CALL PARAMETERS: derived error_type code
! RETURNS:
! NOTES:
! Definition of err_code :TODO:
! 0 = no error
!     0 = no error
! 1-99: I/O errors
!     1 = file could not be opened for read-access
!     2 = file could not be opened for write-access
!     3 = error in reading file
!     4 = error in writing to file
!     5 = error in reading file: EOR/EOF encountered
!     6 = file open for write-access could not be closed
!     7 = file open for read-access could not be closed
! 100-199: numerical errors
!   100 = attempted division by zero
!   101 = attempted logarithm of non-positive number
!   102 = argument to exp function too large
!   103 = attempted square root of negative number
!   110 = general overflow
!   111 = general underflow
!   112 = integer overflow
!   113 = integer underflow
!   114 = inexact result
! 200-299: memory errors
!   200 = unable to dynamically allocate memory for object
!   201 = unable to deallocate memory for object
! 300-399: array dimension errors
!   300 = non-square matrix encountered where square matrix expected
!   301 = dimensions of matrix arguments not conformable
! 1000: other error
!  1000 = reserved for custom error messages
!*******************************************************************************

implicit none

! declare required arguments

type(error_type), intent(inout) :: err

integer, intent(in) :: err_code

! declare optional arguments

character (len=*), optional :: called_from, file_name, &
        object_name, custom_1, custom_2, custom_3

integer, optional :: line_no

! local variables94

character(len=12) :: ichar

! begin error code definitions
  select case(err_code)
  case(0)
    call INSERT_MSG_LINE( &
    "No errors", err)
  ! -- I/O errors ------------------------------------------
  case(1)
    call INSERT_MSG_LINE( &
    "Error 1: File could not be opened for read-access", err)
  case(2)
    call INSERT_MSG_LINE( &
    "Error 2: File could not be opened for write-access", err)
  case(3)
    call INSERT_MSG_LINE( &
    "Error 3: File read error", err)
  case(4)
    call INSERT_MSG_LINE( &
    "Error 4: File write error", err)
  case(5)
    call INSERT_MSG_LINE( &
    "Error 5: End of file premature", err)
  case(6)
    call INSERT_MSG_LINE( &
    "Error 6: Cannot close file for write access", err)
  case(7)
    call INSERT_MSG_LINE( &
    "Error 7: Cannot close file for read-access", err)
  ! --- 100-199: numerical errors --------------------------
  case(100)
    call INSERT_MSG_LINE( &
    "Error 100: Attempted division by zero", err)
  case(101)
    call INSERT_MSG_LINE( &
    "Error 101: Logarithm of negative number", err)
  case(102)
    call INSERT_MSG_LINE( &
    "Error 102: Argument too large", err)
  case(103)
    call INSERT_MSG_LINE( &
    "Error 103: Argument too small", err)
  case(104)
    call INSERT_MSG_LINE( &
    "Error 104: Illegal negative argument", err)
  ! --- 200-299: memory errors -----------------------------
  case(200)
    call INSERT_MSG_LINE( &
    "Unable to dynamically allocate memory for object", err)
  case(201)
    call INSERT_MSG_LINE( &
    "Error 201: Unable to deallocate memory for object", err)
  case(300)
    call INSERT_MSG_LINE( &
    "Error 300: Non-square matrix encountered where square expected", err)
  case(301)
    call INSERT_MSG_LINE( &
    "Error 301: Dimensions of matrix arguments not conformable", err)
  ! --- custom error message -------------------------------
  case(1000)
    call INSERT_MSG_LINE( &
    "Error 1000: Not trapped yet", err)
  ! anything else
  case default
    call INSERT_MSG_LINE("Error XXX: Unknown error code.", err)
  end select

  ! append other optional information if present
  if( present(custom_1) ) &
    call INSERT_MSG_LINE(custom_1, err)

  if( present(custom_2) ) &
    call INSERT_MSG_LINE(custom_2, err)

  if( present(custom_3) ) &
    call INSERT_MSG_LINE(custom_3, err)

  if(present(file_name)) &
    call INSERT_MSG_LINE("FILE: " // trim(file_name), err)

  if(present(line_no)) then
    write(ichar,"(I12)") line_no
    ichar = adjustl(ichar)
    call INSERT_MSG_LINE("LINE: " // trim(ichar), err)
  end if

  if(present(object_name)) &
    call INSERT_MSG_LINE(trim(object_name), err)

  if(present(called_from)) &
    call INSERT_MSG_LINE("OCCURRED IN: " // &
    trim(called_from), err)

end subroutine ERR_HANDLE

!-------------------------------------------------------------------------------

subroutine ERR_GET_MSGS(err, msg_string)
!*******************************************************************************
! ERR_GET_MSGS
! PURPOSE:
!   Public: Retrieves all stored messages as a single character
!   string, with message lines separated by platform-appropriate
!   ASCII carriage control characters.
!   Values for platform may be "UNIX", "MAC" or "PC"
!   CALL PARAMETERS:
!
!*******************************************************************************

implicit none

! required arguments
type(error_type), intent(inout) :: err
character(len=*), intent(out) :: msg_string

! optional arguments
!character(len=*), intent(in), optional :: platform

! local variables
character(len=4) :: plat
integer :: posn
logical :: first_time
type(msg_line_type), pointer :: cur_line

  ! determine platform
!  if( present(platform) ) then
!    plat = platform
!  else
!    plat = "PC"
!  end if

  ! clean out msg_string
  msg_string = ""
  ! step through the linked list, appending the lines
  first_time = .true.
  cur_line => err%head

  do

    if( .not.associated(cur_line) ) exit

    posn = len_trim(msg_string)

    if( (posn+3) >= len(msg_string) ) exit ! out of space

    posn = posn + 1

    if( .not.first_time) then
!      select case(plat)
!      case("UNIX")
!        ! Separate lines with LF
!        msg_string(posn:) = achar(10)
!        posn = posn + 1
!      case("MAC")
!        ! Separate lines with CR
!        msg_string(posn:) = achar(13)
!        posn = posn + 1
!      case default
!        msg_string(posn:) = achar(13) // achar(10)
!        posn = posn + 296
!      end select
      msg_string(posn:) = new_line("A")
      posn = posn + 1
    end if

    msg_string(posn:) = trim(cur_line%line)
    first_time = .false.
    cur_line => cur_line%next

  end do

end subroutine ERR_GET_MSGS

end module ERRORS
!-------------------------------------------------------------------
