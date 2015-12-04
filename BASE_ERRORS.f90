!*******************************************************************************
! Error-handling and reporting modules::
!
!   ASSERT: provides an interface to check for assertions and
!     generate corresponding errors.
!
!   EXCEPTION: Provides services to generate different levels of exceptions
!     and display the message of the exception.
!
!   THROWABLE: Abstraction of a stack of exceptions.
!
!   ERRORS: Generic error message handler that stores error messages into a
!     single string
!*******************************************************************************

module EXCEPTION
!*******************************************************************************
! EXCEPTION --
!
!   Provides services to generate different levels of exceptions
!   and display the message of the exception.
!   Five levels of exceptions can be generated, from the lowest
!   to the highest level :
!   - information, warning : just print a message and continue
!   - error, fatal_error and failure : prints a message and stop the execution
!
! OVERVIEW
!
!   Simple use-case
!
!   Suppose that one would like to compute the square root of one
!   real value. The "compute_sqrt" function takes one positive real argument,
!   and if the argument is negative, one cannot compute the square root so
!   that one would generate an error. In the following example, extracted from
!   the unit tests included in the project, one uses the static method
!   "exception_raiseError" to display a user-friendly message and stop the
!   execution of the program
!
!    function compute_sqrt ( value ) result ( root )
!      use EXCEPTION
!      implicit none
!      real, intent(in) :: value
!      real :: root
!      if ( value < 0. ) then
!        call exception_raiseError ( "Value is negative in compute_sqrt" )
!      else
!        root = sqrt ( value )
!      endif
!   end function compute_sqrt
!
!   real :: root
!   root = compute_sqrt ( -1. )
!
!   In the previous example, the standard output is written so that the
!   following message appears on screen :
!
!   Error.
!   Message: Value is negative in compute_sqrt
!
!   Controlling the execution
!
!   The client code can control the behaviour of the component each time
!   an exception is raised.
!   The default behaviour is to stop the execution. This can be modified
!   by calling "exception_setstoponerror" in order to continue the execution,
!   even if error, fatal error or failure exceptions are raised.
!
!   In the following example, the static method "exception_setstoponerror" is
!   called so that an error does not interrupt the execution.
!
!   call exception_setstoponerror ( .false. )
!   call exception_raiseError ( "There is an error, but the execution will &
!                             continue." )
!
! Controlling output
!
!   The default behaviour is to write messages onto the standard output
!   each time an exception is raised.
!   This can be modified in two ways :
!   * the first possibility is to disable the writing of the messages
!     with "exception_logactive". This feature might be useful in the case
!     where a component has known bugs but generates lots of unwanted
!     exceptions messages.
!   * the second possibility is to connect the component to an existing unit
!     with "exception_setlogunit", so that the messages are written
!     on the given logical unit number.
!     This allows for example to write on an existing log file, may be the log
!     file manage by the LOGGER component included in the project.
!
!   In the following example, the client code first disables all output,
!   set "stoponerror" to false and generates an error which is not displayed
!   and does not interrupt the execution.
!
!   call exception_setstoponerror ( .false. )
!   call exception_logactive ( .false. )
!   call exception_raiseError ( "This message will not be displayed and the &
!                             execution will continue." )
!   call exception_logactive ( .true. )
!   call exception_raiseError ( "This message WILL be displayed and the &
!                             execution will continue." )
!
!   In the following example, the client code connects the EXCEPTION
!   component to an existing unit so that the exception messages are written
!   onto a client log file.
!
!     log_fileunit = 12
!     call exception_setstoponerror ( .false. )
!     open ( log_fileunit , FILE= "log_file.log" )
!     call exception_setlogunit ( log_fileunit )
!     call exception_raiseError ( "This message will be written in &
!                             log_file.log and the execution will continue." )
!     call exception_setlogunit ( 0 )
!     call exception_raiseError ( "This message will be written on standard &
!                             output and the execution will continue." )
!     close ( log_fileunit )
!
!   In the following example, the client code connects the EXCEPTION &
!                             component to
!   the logfile manage by LOGGER. This way, the exception messages are
!   collected in the unique log file of the client code.
!
!     call log_startup ( "log_file.log" , append=.true. )
!     call log_cget ( "logfileunit" , log_fileunit )
!     call exception_setstoponerror ( .false. )
!     call exception_setlogunit ( log_fileunit )
!     call exception_raiseError ( "This message will be written in &
!                             log_file.log and the execution will continue." )
!     call log_shutdown ()
!
! Pseudo-catch
!
!   The client code can use a pseudo-catch system which provides
!   a simple way to manage exceptions which are raised at a lower
!   level in the call stack. This allows to provide special
!   treatments when exceptions are generated, without modifiying
!   all lower level subroutines/function, but just by inserting
!   exception management when needed.
!   Suppose that you have a subroutine which source code is :
!     subroutine yoursubroutine ()
!       use EXCEPTION, only : exception_raiseFatalError
!       implicit none
!       [...]
!       call exception_raiseFatalError ( "Wrong blabla in yoursubroutine" )
!       [...]
!     end subroutine yoursubroutine
!   When calling the subroutine "yoursubroutine", one may wonder if exceptions
!   have been generated so that these errors may be processed, or not.
!   One can use the "exception_catch" service to compute the status
!   of one subroutine and manage that status :
!     use EXCEPTION, only : exception_catch, &
!         EXCEPTION_INFORMATION, &
!         EXCEPTION_WARNING &
!         EXCEPTION_ERROR &
!         EXCEPTION_FATAL_ERROR &
!         EXCEPTION_FAILURE
!     integer :: status
!     call exception_catch ( yoursubroutine , status )
!     select case ( status )
!     case ( EXCEPTION_INFORMATION )
!        write(6,*) "Information"
!     case ( EXCEPTION_WARNING )
!        write(6,*) "Warning"
!     case ( EXCEPTION_ERROR , EXCEPTION_FATAL_ERROR , EXCEPTION_FAILURE )
!        write(6,*) "Fatal error"
!     case default
!        write(6,*) "No problem, continue."
!     end select
!
! TODO
!   - design a more powerful exception management system, which manages
!   exceptions through the call stack
!
! Copyright (c) 2008 Michael Baudin
!
! $Id: EXCEPTION.f90,v 1.4 2008/06/18 10:35:22 relaxmike Exp $
!*******************************************************************************
  implicit none

  private

  ! Default unit used for LOGGER output
  integer, parameter, private :: MAX_UNIT = 299
  !
  ! Public methods
  !
  public :: exception_getcounter
  public :: exception_getlogunit
  public :: exception_initcounter
  public :: exception_islogactive
  public :: exception_logactive
  public :: exception_raiseError
  public :: exception_raiseFailure
  public :: exception_raiseFatalError
  public :: exception_raiseInformation
  public :: exception_raiseWarning
  public :: exception_report
  public :: exception_setlogunit
  public :: exception_setstoponerror
  public :: exception_catch
  !
  ! Tags to manage error
  !
  integer, parameter, public :: EXCEPTION_OK = 0
  integer, parameter, public :: EXCEPTION_INFORMATION = 1
  integer, parameter, public :: EXCEPTION_WARNING = 2
  integer, parameter, public :: EXCEPTION_ERROR = 3
  integer, parameter, public :: EXCEPTION_FATAL_ERROR = 4
  integer, parameter, public :: EXCEPTION_FAILURE = 5
  integer, parameter, public :: EXCEPTION_SIZE = 5
  !
  ! Set to true to stop the execution after a stoping error.
  !
  logical :: exception_stoponerror = .true.
  !
  ! Counters for the different levels of errors
  !
  integer :: exception_nbinformation = 0
  integer :: exception_nbwarning = 0
  integer :: exception_nberror = 0
  integer :: exception_nbfatalerror = 0
  integer :: exception_nbfailure = 0
  !
  ! Maximum number of characters in a message generated by the exception
  !
  integer , parameter :: EXCEPTION_MAXIMUM_LENGTH = 500
  !
  ! Set exception_log_active to true to activate the exception logging
  ! to the unit # exception_log_unit
  !
  integer :: exception_log_unit = MAX_UNIT
  logical :: exception_log_unit_active = .false.
  !
  ! Set exception_log_active to true to display the exception messages
  !
  logical :: exception_log_active = .true.
contains
  !
  ! exception_raiseFatalError --
  !   Generates a fatal error message and stops the execution.
  !
  subroutine exception_raiseFatalError ( message )
    character(len=*), intent(in) :: message
    exception_nbfatalerror = exception_nbfatalerror + 1
    call exception_print(EXCEPTION_FATAL_ERROR, message )
    call exception_stop ()
  end subroutine exception_raiseFatalError
  !
  ! exception_raiseError --
  !   Generates an error message and stops the execution
  !
  subroutine exception_raiseError ( message )
    character(len=*), intent(in) :: message
    exception_nberror = exception_nberror + 1
    call exception_print(EXCEPTION_ERROR, message )
    call exception_stop ()
  end subroutine exception_raiseError
  !
  ! exception_raiseWarning --
  !   Generates a warning message
  !
  subroutine exception_raiseWarning ( message )
    character(len=*), intent(in) :: message
    exception_nbwarning = exception_nbwarning + 1
    call exception_print(EXCEPTION_WARNING, message )
  end subroutine exception_raiseWarning
  !
  ! exception_raiseInformation --
  !   Generates an information message.
  !
  subroutine exception_raiseInformation ( message )
    character(len=*), intent(in) :: message
    exception_nbinformation = exception_nbinformation + 1
    call exception_print(EXCEPTION_INFORMATION, message )
  end subroutine exception_raiseInformation
  !
  ! exception_raiseFailure --
  !   Generates a failure message and stops the execution
  !
  subroutine exception_raiseFailure ( message )
    character(len=*), intent(in) :: message
    exception_nbfailure = exception_nbfailure + 1
    call exception_print(EXCEPTION_FAILURE, message )
    call exception_stop ()
  end subroutine exception_raiseFailure
  !
  ! exception_print --
  !   Prints a message on the standard output about
  !   the current exception type, the origin of the error and the
  !   message of the error.
  ! Arguments :
  !   exceptionCode : an integer exception code between : EXCEPTION_INFORMATION,
  !     EXCEPTION_WARNING, EXCEPTION_ERROR, EXCEPTION_FATAL_ERROR, EXCEPTION_FAILURE
  !   origin : the name of the element generating the exception
  !   message : a message to display
  !
  subroutine exception_print ( exceptionCode , message )
    integer, intent(in) :: exceptionCode
    character(len=*), intent(in) :: message
    character ( len = EXCEPTION_MAXIMUM_LENGTH ) :: msg
    select case (exceptionCode)
    case (EXCEPTION_INFORMATION)
       write ( msg , * ) "Information."
    case (EXCEPTION_WARNING)
       write(msg,*) "Warning."
    case (EXCEPTION_ERROR)
       write(msg,*) "Error."
    case (EXCEPTION_FATAL_ERROR)
       write(msg,*) "Fatal error."
    case (EXCEPTION_FAILURE)
       write(msg,*) "Failure."
    case default
       write(msg,*) "Bad value for the exception code."
    end select
    call exception_logmsg ( msg )
    write(msg,*) "Message: ", trim(message)
    call exception_logmsg ( msg )
  end subroutine exception_print
  !
  ! exception_stop --
  !   Stop the execution after an exception has been raised.
  ! Note :
  !   In debug mode, put a breakpoint here allows to see the exception
  !   before stopping.
  !
  subroutine exception_stop ()
    if ( exception_stoponerror ) then
       STOP
    endif
  end subroutine exception_stop
  !
  ! exception_setstoponerror --
  !   Configure the component to manage the behaviour when an exception
  !   is generated.
  ! Arguments :
  !   stoponerror : if true, then when an error, a fatal_error or a failure
  !     is generated, then stop the execution.
  !     If .false., then the same kinds of exceptions do not stop the
  !     execution
  !
  subroutine exception_setstoponerror ( stoponerror )
    logical, intent(in) :: stoponerror
    exception_stoponerror = stoponerror
  end subroutine exception_setstoponerror
  !
  ! exception_logmsg --
  !   Write the given message onto the default unit.
  !
  subroutine exception_logmsg ( message )
    use, intrinsic :: ISO_FORTRAN_ENV
    character(len=*), intent(in) :: message
    if ( exception_log_active ) then
       if ( exception_log_unit_active ) then
          write(exception_log_unit,*) trim(message)
       else
          write(ERROR_UNIT,*) trim(message)
       endif
    endif
  end subroutine exception_logmsg
  !
  ! exception_initcounter --
  !   Reset to 0 all the exceptions counters
  !
  subroutine exception_initcounter ()
    exception_nbinformation = 0
    exception_nbwarning = 0
    exception_nberror = 0
    exception_nbfatalerror = 0
    exception_nbfailure = 0
  end subroutine exception_initcounter
  !
  ! exception_getcounter --
  !   Returns an array of size EXCEPTION_SIZE indicating the number
  !   of type of exceptions.
  ! Arguments :
  !   excepcounters ( 1:EXCEPTION_SIZE) : array of integers
  !     excepcounters ( iexcept ) is the total number of exceptions
  !     of type #iexcept generated since the begining of the execution.
  !     The possible values for iexcept are the following.
  !     - counter ( EXCEPTION_INFORMATION ) : total number of information exceptions raised
  !     - counter ( EXCEPTION_WARNING ) : total number of warning exceptions raised
  !     - counter ( EXCEPTION_ERROR ) : total number of error exceptions raised
  !     - counter ( EXCEPTION_FATAL_ERROR ) : total number of fatal error exceptions raised
  !     - counter ( EXCEPTION_FAILURE ) : total number of failure exceptions raised
  !
  subroutine exception_getcounter ( counter )
    integer, dimension(1:EXCEPTION_SIZE), intent(out) :: counter
    counter ( EXCEPTION_INFORMATION ) = exception_nbinformation
    counter ( EXCEPTION_WARNING ) = exception_nbwarning
    counter ( EXCEPTION_ERROR ) = exception_nberror
    counter ( EXCEPTION_FATAL_ERROR ) = exception_nbfatalerror
    counter ( EXCEPTION_FAILURE ) = exception_nbfailure
  end subroutine exception_getcounter
  !
  ! exception_report --
  !   Writes on the current exception unit number a report
  !   which details the number of exceptions of all types
  !   since the begining of the execution or the last reset of
  !   all counters.
  !
  subroutine exception_report ()
    character ( len = 300 ) :: message
    call exception_logmsg ( "Exception report" )
    write ( message , * ) "Number of informations :" , exception_nbinformation
    call exception_logmsg ( message )
    write ( message , * ) "Number of warnings :" , exception_nbwarning
    call exception_logmsg ( message )
    write ( message , * ) "Number of errors :" , exception_nberror
    call exception_logmsg ( message )
    write ( message , * ) "Number of fatal errors :" , exception_nbfatalerror
    call exception_logmsg ( message )
    write ( message , * ) "Number of failures :" , exception_nbfailure
    call exception_logmsg ( message )
  end subroutine exception_report
  !
  ! exception_setlogunit --
  !   Set the unit number onto which the messages are output.
  !   If the unitnumber is negative or 0, the messages are written
  !   to the standard output (unit *).
  ! Arguments :
  !   unitnumber : the unit number
  ! Note :
  !   A unitnumber equals to 6 is generally the standard output,
  !   but this may depend on the fortran compiler.
  !
  subroutine exception_setlogunit ( unitn )
    integer, optional, intent(in) :: unitn
    integer :: unitnumber

    if (present(unitn)) then
      unitnumber=unitn
    else
      unitnumber=MAX_UNIT
    end if

    if ( unitnumber <= 0 ) then
       exception_log_unit_active = .false.
       exception_log_unit = unitnumber
    else
       exception_log_unit_active = .true.
       exception_log_unit = unitnumber
    endif
  end subroutine exception_setlogunit
  !
  ! exception_getlogunit --
  !   Returns the positive unit number onto which the messages are output,
  !   if enabled or a negative integer if the feature is disabled.
  ! Arguments :
  !
  !
  function exception_getlogunit ( ) result ( logunit )
    integer :: logunit
    logunit = exception_log_unit
  end function exception_getlogunit
  !
  ! exception_logactive --
  !   If the boolean argument bool is true, enable the logging
  !   of the exceptions messages.
  !   If the boolean argument bool is false, disable the logging
  !   of the exceptions messages.
  ! Arguments :
  !   bool : if true, active the logging of the messages
  !
  subroutine exception_logactive ( bool )
    logical, intent(in) :: bool
    exception_log_active = bool
  end subroutine exception_logactive
  !
  ! exception_islogactive --
  !   Returns .true. if the current exception messages are written,
  !   either on standard output or into a log file.
  !
  function exception_islogactive ( ) result ( islogactive )
    logical :: islogactive
    islogactive = exception_log_active
  end function exception_islogactive
  !
  ! exception_catch --
  !   Calls the given subroutine and returns the integer associated
  !   with last exception, higher level, code or 0 if no exception
  !   was raised.
  ! Argument :
  !   callback : the subroutine to call back
  !   status : the integer corresponding to the last exception, if any.
  ! Caution !
  !   The internal algorithm is based on the exception counters,
  !   which implies that any call to exception_initcounter in the client
  !   code can result in a wrong status.
  !
  subroutine exception_catch ( callback , status )
    interface interfacecallback
       subroutine callback ()
       end subroutine callback
    end interface interfacecallback
    integer, intent(out) :: status
    integer, dimension(1:EXCEPTION_SIZE) :: counter_before
    integer, dimension(1:EXCEPTION_SIZE) :: counter_after
    integer :: exception_code
    !
    ! Get the counters before
    !
    call exception_getcounter ( counter_before )
    !
    ! Call the callback
    !
    call callback ()
    !
    ! Get the counters after
    !
    call exception_getcounter ( counter_after )
    !
    ! Compare before / after and set the corresponding status.
    !
    ! Process the loop, from the highest level of exception to the lowest.
    status = EXCEPTION_OK
    do exception_code = EXCEPTION_SIZE , 1 , -1
       if ( counter_after ( exception_code ) > counter_before ( exception_code ) ) then
          status = exception_code
          exit
       endif
    enddo
  end subroutine exception_catch
end module EXCEPTION

!-------------------------------------------------------------------------------

module ASSERT
!*******************************************************************************
! ASSERT --
!
!   This module provides an interface to check for assertions and
!   generate corresponding errors.
!
!   This component is based on the EXCEPTION module.
!   The main service of the component is the subroutine "assert_assert"
!   which check that the given logical variable is true. The following
!   is a sample use. If the condition is not .true., then an exception
!   of error type is raised by the EXCEPTION module.
!
!      use ASSERT, only : assert_assert
!      integer :: x , y
!      x = 2
!      y = x**2
!      call assert_assert ( y == 4 , "Wrong power value." )
!
!   The user can choose a more specific type of exception raised
!   by choosing the correspondant assert_failure, assert_fatalError,
!   assert_error, assert_warning or assert_information service.
!
!   The component can influence performances because of the
!   internal computation performed to check the assertions, which may be
!   undesirable in optimized mode.
!   Therefore, the assertions are checked only if the pre-processing
!   macro _ASSERT_ENABLE is defined. If it is undefined, there is no
!   impact on performances.
!
!   The component can be dynamically enabled or disabled with
!   assert_setenabled ; one can see if the component is currently
!   enabled with assert_getenabled.
!
!   The user can check the number of assertions that were successful
!   and the number of assertions which failed with assert_getcounters,
!   and reset the internal counters with assert_initcounters.
!
!   To control more precisely the behaviour of the ASSERT component
!   when an exception occurs, the user can directly configure EXCEPTION.
!
! Copyright (c) 2008 Michael Baudin
!
! $Id: ASSERT.f90,v 1.2 2008/05/06 08:39:47 relaxmike Exp $
!*******************************************************************************
  use EXCEPTION, only : &
       exception_raiseError , &
       exception_raiseFailure , &
       exception_raiseFatalError , &
       exception_raiseInformation, &
       exception_raiseWarning
  implicit none
  private
  !
  ! Public methods
  !
  public :: assert_fatalError
  public :: assert_error
  public :: assert_warning
  public :: assert_information
  public :: assert_failure
  public :: assert_getenabled
  public :: assert_setenabled
  public :: assert_assert
  public :: assert_getcounters
  public :: assert_initcounters
  !
  ! Set to true to enable assertion checking
  !
  logical :: assert_enabled = .true.
  !
  ! Counter of the number of assertions that were true, false, total
  !
  integer :: assert_numbertrue = 0
  integer :: assert_numberfalse = 0
  integer :: assert_numbertotal = 0
  !
  ! Flag for the counters
  !
  integer, parameter, public :: ASSERT_INDEX_NUMBERTRUE = 1
  integer, parameter, public :: ASSERT_INDEX_NUMBERFALSE = 2
  integer, parameter, public :: ASSERT_INDEX_NUMBERTOTAL = 3
  integer, parameter, public :: ASSERT_INDEX_MAX = 3

contains
  !
  ! assert_assert --
  !   Assert that the given test is true.
  !   If not .true., raise an assert error.
  !
  subroutine assert_assert ( test , message )
    implicit none
    logical         , intent(in) :: test
    character(len=*), intent(in) :: message
    call assert_error ( test , message )
  end subroutine assert_assert
  !
  ! assert_getenabled --
  !   Returns .true. if the assertions are enabled
  !
  logical function assert_getenabled ( )
    assert_getenabled = assert_enabled
  end function assert_getenabled
  !
  ! assert_setenabled --
  !   Enable (or disable) the assertion checking if the given boolean is true,
  !   (or .false.).
  !
  subroutine assert_setenabled ( bool )
    logical , intent(in) :: bool
    assert_enabled = bool
  end subroutine assert_setenabled
  !
  ! assert_fatalError --
  !   Assert that the given test is true.
  !   If not .true., raise a fatal error.
  !
  subroutine assert_fatalError ( test , message )
    implicit none
    logical         , intent(in) :: test
    character(len=*), intent(in) :: message
!#ifdef _ASSERT_ENABLE
    logical :: istrue
    istrue = assert_istrue ( test )
    if (.not. istrue) then
       call exception_raiseFatalError ( message )
    endif
!#else
    continue
!#endif
  end subroutine assert_fatalError
  !
  ! assert_error --
  !   Assert that the given test is true.
  !   If not .true., raise an error.
  !
  subroutine assert_error ( test , message )
    implicit none
    logical         , intent(in) :: test
    character(len=*), intent(in) :: message
!#ifdef _ASSERT_ENABLE
    logical :: istrue
    istrue = assert_istrue ( test )
    if (.not. istrue) then
       call exception_raiseError ( message )
    endif
!#else
    continue
!#endif
  end subroutine assert_error
  !
  ! assert_warning --
  !   Assert that the given test is true.
  !   If not .true., raise a warning.
  !
  subroutine assert_warning ( test , message )
    implicit none
    logical         , intent(in) :: test
    character(len=*), intent(in) :: message
!#ifdef _ASSERT_ENABLE
    logical :: istrue
    istrue = assert_istrue ( test )
    if (.not. istrue) then
       call exception_raiseWarning ( message )
    endif
!#else
    continue
!#endif
  end subroutine assert_warning
  !
  ! assert_information --
  !   Assert that the given test is true.
  !   If not .true., raise an information.
  !
  subroutine assert_information ( test , message )
    implicit none
    logical         , intent(in) :: test
    character(len=*), intent(in) :: message
!#ifdef _ASSERT_ENABLE
    logical :: istrue
    istrue = assert_istrue ( test )
    if (.not. istrue) then
       call exception_raiseInformation ( message )
    endif
!#else
    continue
!#endif
  end subroutine assert_information
  !
  ! assert_failure --
  !   Assert that the given test is true.
  !   If not .true., raise a failure.
  !
  subroutine assert_failure ( test , message )
    implicit none
    logical         , intent(in) :: test
    character(len=*), intent(in) :: message
!#ifdef _ASSERT_ENABLE
    logical :: istrue
    istrue = assert_istrue ( test )
    if (.not. istrue) then
       call exception_raiseFailure ( message )
    endif
!#else
    continue
!#endif
  end subroutine assert_failure
  !
  ! assert_countersupdate --
  !   Check if the condition is true or false and update the counters
  !
  subroutine assert_countersupdate ( test )
    logical         , intent(in) :: test
    assert_numbertotal = assert_numbertotal + 1
    if ( test ) then
       assert_numbertrue = assert_numbertrue + 1
    else
       assert_numberfalse = assert_numberfalse + 1
    endif
  end subroutine assert_countersupdate
  !
  ! assert_istrue --
  !   Returns .true. if the assert component is disabled.
  !   If the assert component is enabled, return .true. or .false.
  !   depending on the value of test.
  ! Side effects:
  !   Updates the assert counters with a call to assert_countersupdate.
  !
  logical function assert_istrue ( test )
    logical         , intent(in) :: test
    assert_istrue = .true.
!#ifdef _ASSERT_ENABLE
    if ( assert_enabled ) then
       call assert_countersupdate ( test )
       if ( .not.test ) then
          assert_istrue = .false.
       endif
    endif
!#endif
  end function assert_istrue
  !
  ! assert_getcounters --
  !   Fill an array of size 3 with the current values of the counters :
  !     counter ( ASSERT_INDEX_NUMBERTRUE ) : number of true assertions
  !     counter ( ASSERT_INDEX_NUMBERFALSE ) : number of false assertions
  !     counter ( ASSERT_INDEX_NUMBERTOTAL ) : number of total assertions
  !
  subroutine assert_getcounters ( counters )
    integer , dimension(1:ASSERT_INDEX_MAX) , intent(out) :: counters
    counters ( ASSERT_INDEX_NUMBERTRUE ) = assert_numbertrue
    counters ( ASSERT_INDEX_NUMBERFALSE ) = assert_numberfalse
    counters ( ASSERT_INDEX_NUMBERTOTAL ) = assert_numbertotal
  end subroutine assert_getcounters
  !
  ! assert_initcounters --
  !   Initializes the assertions counters
  !
  subroutine assert_initcounters ( )
    assert_numbertrue = 0
    assert_numberfalse = 0
    assert_numbertotal = 0
  end subroutine assert_initcounters
end module ASSERT

!-------------------------------------------------------------------------------

module THROWABLE
!*******************************************************************************
! THROWABLE --
!
!   Abstraction of a stack of exceptions.
!   A throwable object is a made of a message (a string)
!   and a cause (a throwable object).
!
!   A throwable object made be created with no message and no cause,
!   with a message but without a cause, with a cause but without
!   a message and with both a cause and a message.
!
!   The stack trace associated with the current object can
!   be displayed with throwable_printStackTrace, either on the
!   standard output or a given unit number or with a callback
!   display subroutine. The typical output is :
!
!     My throw 3
!     Caused by:My throw 2
!     Caused by:My throw 1
!
!   The throwable_exists service allows to know if
!   the throwable object has been created, that is, if
!   an exception has occured. The message associated can be
!   retrieved with throwable_getmessage and the cause
!   may be directly accessed with throwable_getcause.
!
! Copyright (c) 2008 Michael Baudin
!
! $Id: THROWABLE.f90,v 1.1 2008/04/09 07:29:23 relaxmike Exp $
!*******************************************************************************

  implicit none

  ! Default unit used for LOGGER output
  integer, parameter, private :: MAX_UNIT = 299

  private
  !
  ! Public methods
  !
  public :: T_THROWABLE
  public :: throwable_exists
  public :: throwable_free
  public :: throwable_getcause
  public :: throwable_getmessage
  public :: throwable_iscause
  public :: throwable_new
  public :: throwable_printStackTrace
  public :: throwable_write
  !
  ! Maximum number of characters in a message generated by the exception
  !
  integer , parameter :: THROWABLE_MAXIMUM_LENGTH = 500
  !
  ! Derived-type to manage throwable objects
  !
  type T_THROWABLE
     private
     logical :: exists = .false.
     character ( len = THROWABLE_MAXIMUM_LENGTH ) :: message
     type ( T_THROWABLE ), pointer :: cause
  end type T_THROWABLE
  !
  ! Interface for constructors
  !
  interface throwable_new
     module procedure throwable_new_empty
     module procedure throwable_new_cause
     module procedure throwable_new_message
     module procedure throwable_new_cause_message
  end interface throwable_new
  interface throwable_printStackTrace
     module procedure throwable_printStackTrace_onunit
     module procedure throwable_printStackTrace_callback
  end interface throwable_printStackTrace
contains
  !
  ! throwable_new_empty --
  !   Constructor
  !
  subroutine throwable_new_empty ( this )
    type ( T_THROWABLE ) , pointer :: this
    allocate ( this )
    this % exists = .true.
    this % cause => NULL ()
    this % message = ""
  end subroutine throwable_new_empty
  !
  ! throwable_new_cause --
  !   Constructor
  !
  subroutine throwable_new_cause ( this , cause )
    type ( T_THROWABLE ) , pointer :: this
    type ( T_THROWABLE ), pointer :: cause
    call throwable_new_empty ( this )
    this % cause => cause
    this % message = ""
  end subroutine throwable_new_cause
  !
  ! throwable_new_message --
  !   Constructor
  !
  subroutine throwable_new_message ( this , message )
    type ( T_THROWABLE ) , pointer :: this
    character ( len = *) , intent ( in ) :: message
    call throwable_new_empty ( this )
    this % cause => NULL ()
    this % message = message
  end subroutine throwable_new_message
  !
  ! throwable_new_cause_message --
  !   Constructor
  !
  subroutine throwable_new_cause_message ( this , cause , message )
    type ( T_THROWABLE ) , pointer :: this
    type ( T_THROWABLE ), pointer :: cause
    character ( len = *) , intent ( in ) :: message
    call throwable_new_empty ( this )
    this % cause => cause
    this % message = message
  end subroutine throwable_new_cause_message
  !
  ! throwable_free --
  !   Destructor
  !
  recursive subroutine throwable_free ( this )
    type ( T_THROWABLE ) , pointer :: this
    logical :: causeassociated
    causeassociated = associated ( this % cause )
    if ( causeassociated ) then
       call throwable_free ( this % cause )
    endif
    this % exists = .false.
    deallocate ( this )
  end subroutine throwable_free
  !
  ! throwable_iscause --
  !   Returns .true. if the current throwable object has a cause.
  !
  logical function throwable_iscause ( this )
    type ( T_THROWABLE ) , pointer :: this
    throwable_iscause = associated ( this % cause )
  end function throwable_iscause
  !
  ! throwable_getcause --
  !   Get the cause of the current throwable object
  !
  subroutine throwable_getcause ( this , cause )
    type ( T_THROWABLE ) , pointer :: this
    type ( T_THROWABLE ) , pointer :: cause
    cause => this % cause
  end subroutine throwable_getcause
  !
  ! throwable_getmessage --
  !   Get the message of the current throwable object
  !
  subroutine throwable_getmessage ( this , message )
    type ( T_THROWABLE ) , pointer :: this
    character ( len = *) , intent ( out ) :: message
    message = this % message
  end subroutine throwable_getmessage
  !
  ! throwable_write --
  !   Writes a short description of this throwable on the given unit.
  !
  subroutine throwable_write ( this , unitn )
    type ( T_THROWABLE ) , pointer :: this
    integer , optional, intent ( in ) :: unitn
    integer  :: unitnumber
    if (present(unitn)) then
      unitnumber=unitn
    else
      unitnumber=MAX_UNIT
    end if
    write ( unitnumber , * ) "Throwable:"
    write ( unitnumber , * ) trim ( this % message )
  end subroutine throwable_write
  !
  ! throwable_printStackTrace_onunit --
  !   Prints this throwable and its backtrace to the given unit number.
  ! Arguments :
  !   unitnumber : if present, the messages are written on this unit number.
  !     If not provided, the messages are written on standard output.
  !
  subroutine throwable_printStackTrace_onunit ( this , unitnumber )
    type ( T_THROWABLE ) , pointer :: this
    integer , intent ( in ), optional :: unitnumber
    type ( T_THROWABLE ) , pointer  :: current
    logical :: iscause
    character ( len = THROWABLE_MAXIMUM_LENGTH ) :: message
    write ( message , * ) trim(this % message)
    call printmsg ( message )
    !
    ! Prints the stack causes
    !
    current => this
    iscause = throwable_iscause ( current )
    do while ( iscause )
       current => current % cause
       write ( message , * ) "Caused by:", trim ( current % message )
       call printmsg ( message )
       iscause = throwable_iscause ( current )
    end do
  contains
    subroutine printmsg ( message )
      character (len=*) , intent(in) :: message
      if ( present ( unitnumber ) ) then
         write ( unitnumber , * ) trim ( message )
      else
         write ( MAX_UNIT , * ) trim ( message )
      endif
    end subroutine printmsg
  end subroutine throwable_printStackTrace_onunit
  !
  ! throwable_printStackTrace_callback --
  !   Prints this throwable and its backtrace and pass the message to
  !   the given callback
  !
  subroutine throwable_printStackTrace_callback ( this , callback )
    type ( T_THROWABLE ) , pointer :: this
    external callback
    ! The following is regular fortran but generates an "Internal error" with IVF 8
    !!$    interface interfacecallback
    !!$       subroutine callback ( message )
    !!$         implicit none
    !!$         character ( len = * ) , intent(in) :: message
    !!$       end subroutine callback
    !!$    end interface interfacecallback
    type ( T_THROWABLE ) , pointer  :: current
    logical :: iscause
    character ( len = THROWABLE_MAXIMUM_LENGTH ) :: message
    write ( message , * ) trim ( this % message )
    call callback ( message )
    !
    ! Prints the stack causes
    !
    current => this
    iscause = throwable_iscause ( current )
    do while ( iscause )
       current => current % cause
       write ( message , * ) "Caused by:", trim ( current % message )
       call callback ( message )
       iscause = throwable_iscause ( current )
    end do
  end subroutine throwable_printStackTrace_callback
  !
  ! throwable_exists --
  !   Returns true if the current throwable object is allocated.
  !
  logical function throwable_exists ( this )
    type ( T_THROWABLE ) , pointer :: this
    logical :: isassociated
    isassociated = associated ( this )
    if ( isassociated ) then
       throwable_exists = this % exists
    else
       throwable_exists = .false.
    endif
  end function throwable_exists
end module THROWABLE

!-------------------------------------------------------------------------------

module ERRORS
!*******************************************************************************
! ERRORS --
!
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
    "ERR: File could not be opened", err)
  case(2)
    call INSERT_MSG_LINE( &
    "ERR: File read error", err)
  case(3)
    call INSERT_MSG_LINE( &
    "ERR: File write error", err)
  case(4)
    call INSERT_MSG_LINE( &
    "ERR: End of file premature", err)
  case(5)
    call INSERT_MSG_LINE( &
    "ERR: Cannot close file", err)
  ! --- 100-199: numerical errors --------------------------
  case(100)
    call INSERT_MSG_LINE( &
    "ERR: Attempted division by zero", err)
  case(101)
    call INSERT_MSG_LINE( &
    "ERR: Logarithm of negative number", err)
  case(102)
    call INSERT_MSG_LINE( &
    "ERR: Argument too large", err)
  case(103)
    call INSERT_MSG_LINE( &
    "ERR: Argument too small", err)
  case(104)
    call INSERT_MSG_LINE( &
    "ERR: Illegal negative argument", err)
  ! --- 200-299: memory errors -----------------------------
  case(200)
    call INSERT_MSG_LINE( &
    "ERR: Unable to allocate memory for object", err)
  case(201)
    call INSERT_MSG_LINE( &
    "ERR: Unable to deallocate memory for object", err)
  case(300)
    call INSERT_MSG_LINE( &
    "ERR: Non-square matrix encountered", err)
  case(301)
    call INSERT_MSG_LINE( &
    "ERR: Dimensions of matrix not conformable", err)
  ! --- custom error message -------------------------------
  case(1000)
    call INSERT_MSG_LINE( &
    "ERR: Not trapped yet", err)
  ! anything else
  case default
    call INSERT_MSG_LINE("Error XXX: Unknown error", err)
  end select

  ! append other optional information if present
  if( present(custom_1) ) &
    call INSERT_MSG_LINE(custom_1, err)

  if( present(custom_2) ) &
    call INSERT_MSG_LINE(custom_2, err)

  if( present(custom_3) ) &
    call INSERT_MSG_LINE(custom_3, err)

  if(present(file_name)) &
    call INSERT_MSG_LINE("File: " // trim(file_name), err)

  if(present(line_no)) then
    write(ichar,"(I12)") line_no
    ichar = adjustl(ichar)
    call INSERT_MSG_LINE("Line: " // trim(ichar), err)
  end if

  if(present(object_name)) &
    call INSERT_MSG_LINE(trim(object_name), err)

  if(present(called_from)) &
    call INSERT_MSG_LINE("Occurred in: " // &
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

    if( .not. associated(cur_line) ) exit

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
