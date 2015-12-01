!*******************************************************************************
! MODEL_PROTO
! PURPOSE:
!   A prototype model that shows how to work with the HEDTOOLS modules
!
! NOTES:
!   1. All global parameters are now declared within a module (or more than one
!      modules). Modules may or may not be seperate files. They may also contain
!      local subroutines, functions, derived type declarations and everythin.
!      This is particularly useful for ththe next object-oriented models...
!
! Normally use make to build. If make unavailable, use manual build as follows
! (for GNU fortran)::
! gfortran -g -c ../BASE_LOGGER.f90 ../BASE_CSV_IO.f90 ../BASE_UTILS.f90 MODEL_PROTO.f90
! gfortran -g -o ZZZ MODEL_PROTO.f90 ../BASE_CSV_IO.f90 ../BASE_UTILS.f90 ../BASE_LOGGER.f90
!*******************************************************************************

module COMMONDATA
!*******************************************************************************
! COMMONDATA module
! PURPOSE:
!   Declare global constants for the model. This supersedes the COMMONFISH
!   included common block that was used in previous versions.
!*******************************************************************************

  !----------- PARAMETER SECTION -----------------------------------------------

  character (len=*), parameter, public :: MODEL_NAME = "HED00_PROTO"

  ! sets the model in sebug mode, in this way we can use conditions to write
  ! more information and data to the log... or just show log on the screen
  ! if we use preprocessor, then preprocessor commands / pragmas
  ! could also be used, such as
  ! #defile IS_DEBUG
  logical, parameter, public :: IS_DEBUG=.TRUE.

  ! various other parameters...
  integer, parameter, public :: Cinds = 3
  integer, parameter, public :: Cfmothers = 10, Cmaxlife = 5000
  integer, parameter, public :: CgNR = 5,Cxy = 2, Cdip = 5, CNRcomp = 5
  integer, parameter, public :: CgMEM = 2

  !----------- VARIABLE/ARRAYS/MATRICES SECTION --------------------------------

  ! genome for neuronal responses CgNR responses,genes in NR,diploid,CNR
  ! components)
  real, public :: genomeNR(Cinds,CgNR,Cxy,Cdip,CNRcomp)

  ! genome for modulatory genes CgMOD genes,diploid)
  real genomeMEM(Cinds,CgMEM,Cdip)

  ! genome for memory genes !(CgMEM genes,diploid)
  real genomeMOD(Cinds,Cdip)

  !--------- MODULE DEBUG LOGGER -----------------------------------------------
  ! Module name for the DEBUG LOGGER: every function/sub must also have
  ! the PROCNAME parameter referring to its name. This is done for the Debug
  ! Logger module. Each module must also have a DEBUG Logger subroutine, that
  ! is a wrapper to module LOGGER (or perhaps any other that is being used)
  !   procedure name PROCNAME
  character (len=*), private, parameter :: MODNAME = "COMMONDATA"

!----------- CONTAINED SUBROUTINES SECTION -------------------------------------
contains  ! Note that these contained subroutines may be separate, but in such
          ! case they should contain "use COMMONDATA" to get access to global
          ! objects

subroutine LOGGER_INIT()
! Wrapper to initialise the system logger, called only once at the start
! hidden into subroutine to avoid clutter in the main program

  use LOGGER      ! We need it to get access to LOGGER
  use BASE_UTILS  ! need this for access to base utils

  implicit none

  ! Subroutine name for DEBUG LOGGER
  character (len=*), parameter :: PROCNAME = "LOGGER_INIT"

  ! We first initialise the log and set log file name
  call LOG_STARTUP (MODEL_NAME // "-LOG.log")

  call LOG_CONFIGURE("timestamp", .true.)      ! Produce timestamps in the log
  call LOG_CONFIGURE("writeonstdout" , .true.) ! Output log on screen AND file

  call LOG_CONFIGURE("level_string_volume", "chapter" )  ! Set log level
  call LOG_DELIMITER()                                   ! Issue log delimiter

  ! Send informative messages to the log..
  ! Note that there is a specific subroutine LOG_DBG for producing
  ! debug messages. It is reasonable to do an additional wrapper
  ! for normal informative non-debug messages, if all of them should
  ! contain specific prefix (e.g. INFO::)
  call LOG_MSG("We are now starting to rubn our prototype model")
  call LOG_MSG("Model name: " // MODEL_NAME)
  call LOG_MSG("Model parameters:")
  call LOG_MSG("Individuals: " // TOSTR(Cinds))
  call LOG_MSG("Maximum run: " // TOSTR(Cmaxlife))
  call LOG_MSG("Mothers: " // TOSTR(Cfmothers))
  call LOG_MSG("")
  call LOG_MSG("Any other arbitrary messages can be produced..")
  call LOG_DELIMITER()

  call LOG_MSG("We can switch off logging on the screen (stdout) using")
  call LOG_MSG("  the configure subroutine.")

  call LOG_CONFIGURE("writeonstdout" , .false.) ! do not write on screen further

  call LOG_DELIMITER()
  call LOG_MSG("From now, log messages will go only to the log file and not")
  call LOG_MSG("  to the screen (stdout)...")
  call LOG_DELIMITER()

end subroutine LOGGER_INIT

subroutine LOG_DBG(message_string, procname, modname)
!*******************************************************************************
! LOG_DBG
! PURPOSE: This subroutine is a wrapper for writing debug messages by the
! module LOGGER.
!*******************************************************************************

  use LOGGER

  implicit none

  ! Calling parameters
  character(len=*), intent(in) :: message_string
  character (len=*), optional, intent(in) :: procname
  character (len=*), optional, intent(in) :: modname

  ! Local variables
  character (len=:), allocatable :: prefix_msg

  !-----------------------------------------------------------------------------

  if (IS_DEBUG) then

    ! We first generate the message prefix
    if (present(procname)) then
      if (present(modname)) then
        prefix_msg="DEBUG:: MODULE:" // modname // "PROCEDURE: " // procname // ":: "
      else
        prefix_msg="PROCEDURE: " // procname // ":: "
      end if
    else
      if (present(modname)) then
        prefix_msg="DEBUG:: MODULE:" // modname // ":: "
      else
        prefix_msg="DEBUG:: "
      end if
    end if

    call LOG_MSG( prefix_msg // message_string )  ! use module LOGGER mandatory

  end if

end subroutine LOG_DBG


subroutine FATAL_ERROR(message)
!-----------------------------------------------------------------------------
! We may need an error trapping module...
! It may be  be produced later, presumably using something using IEEE
! assertions, that could trap numerical errors/esceptions etc...
! now, just trivial
!-----------------------------------------------------------------------------

  use LOGGER
  use BASE_UTILS

  implicit none

  ! Subroutine name for DEBUG LOGGER
  character (len=*), parameter :: PROCNAME = "FATAL_ERROR"

  character(len=*) :: message

  call LOG_CONFIGURE("writeonstdout" , .true.)  ! Enable log to the screen
  call LOG_DELIMITER()                          ! Write delimiter
  call LOG_MSG("Encountered an error; Type: " // message // " will stop now...")

    ! May duplicate message to standard error
  call STDERR("PROGRAM ERROR, will terminate now...")

  stop 255 ! produce exit code so that a model call script could locate error

end subroutine FATAL_ERROR

subroutine CHECK_ERROR_FILE ( file_status_flag, fatal )
!--------------------------------------------------------------------
! Wrapper subroutine to check if CSV output resulted in an error
!--------------------------------------------------------------------

  use LOGGER
  use BASE_UTILS

  implicit none

  ! Subroutine name for DEBUG LOGGER
  character (len=*), parameter :: PROCNAME = "CHECK_ERROR_FILE"

  logical, intent(in) :: file_status_flag
  logical, optional, intent(in) :: fatal

  if (.NOT. file_status_flag) then
    ! If everything ok, write confirmation to the log
    call LOG_DBG("File operation returned " // TOSTR(file_status_flag) &
                  // " status.", PROCNAME )
    ! enable log output to the the screen as well as to the file
    call LOG_CONFIGURE("writeonstdout" , .true.)
    ! and log it
    call LOG_DELIMITER()
    call LOG_DBG("ERROR:: File operation returned error status " // &
                  TOSTR(file_status_flag), PROCNAME)
    ! If the subroutine is called with the fatal flag = .true., stop execution
    if (present(fatal)) then
      if (fatal) then
        call LOG_DBG("We will go to FATAL_ERROR procedure now...")
        call LOG_DELIMITER()
        call FATAL_ERROR ("Fatal file output error")
      end if
    end if
  end if

end subroutine CHECK_ERROR_FILE

!-------------------------------------------------------------------------------

end module COMMONDATA

!*******************************************************************************
!                            M A I N    P R O G R A M
!*******************************************************************************

program MODEL_PROTO

  !-----[ DECLARATION SECTION ]-------------------------------------------------
  ! We first declare which modules the model is going to use

  use BASE_UTILS    ! Basic utils
  use CSV_IO        ! CSV data handling
  use LOGGER        ! System logger

  use COMMONDATA    ! use our global data / objects module

  implicit none     ! This is standard thing...

  ! Subroutine name for DEBUG LOGGER
  character (len=*), parameter :: MODNAME = "MODEL_PROTO_MAIN"
  character (len=*), parameter :: PROCNAME = "MODEL_PROTO_MAIN"

  ! Now we declare various model constants and variables...

  integer, parameter :: BIGNUM = 100, SMALLNUM = 10

  integer :: i, j, k

  real, dimension (BIGNUM,SMALLNUM) :: RAND_A

  ! We need a name and unit for CSV output file written  with CSV_IO module
  character(len=:), allocatable :: name_for_csv_out
  integer :: unit_for_csv_out

  ! Note that we may use an error status variable to flag file output errors,
  ! mainly in CSV_IO. Initialise it with a TRUE value as there are portability
  ! issues in non-passing logical optional parameter on some
  ! platforms / compiler combinations...
  logical :: no_error_status=.TRUE.

  ! -----[ EXECUTION SECTION ]--------------------------------------------------
  ! We now start the model...

  ! First, initialise the model logger. Logger is quite configurable and
  ! is managed by LOG_CONFIGURE ()
  ! It is probably better to hide all this logger init code into a subroutine
  ! to avoid cluttering ...
  call LOGGER_INIT()

  ! We also have options to write some information on the standard output
  ! and standard error devices. This does not go to log. It can be considered
  ! as normal output of the program

  ! STDOUT accepts optional number of string arguments, every one
  ! of it goes as a seperete line. STDOUT is intended for all routine screen
  ! output, LOGGER is for debug and service messages that are normally hidden
  call STDOUT( &
      "----------------------------------------------------------------",&
      " Starting running the model : " // MODEL_NAME, &
      " Every argument goes to separate line...", &
      " We may also output numerical data of any type", &
      " Using conversion function TOSTR, e.g. Pi=" // TOSTR(3.1415926), &
      " ", &
      " Note that the above example used concatenation to attach number", &
      " ", &
      " We can also use optional format spec (standard Fortran rules)", &
      " For example: Pi=" // TOSTR(3.1415926, "(f4.2)"), &
      "   ToStr accepts any data types, e.g. integer: one=" // TOSTR(1), &
      " ", &
      "----------------------------------------------------------------" )

  ! There is a similar function for producing output to stderr. It works
  ! almost identically as STDOUT, but the output is going to stderr.
  ! By default, everything also goes to the screen terminal.
  ! It was made for convenience, as in most operating systems we could
  ! redirect output from stdout and stderr to different places
  ! e.g. MODEL_PROTO 2> file_001.log
  call STDERR("+--------------------------------------------------+",&
              "| This output is going to standard error device... |",&
              "| By default, it is also screen...                 |",&
              "| But may be redirected separately when needed     |",&
              "+--------------------------------------------------+")


  ! Start some calculations ----------------------------------------------------

  ! But first show how we can produce debug messages
  ! of course, we can hide this code to a wrapper subroutine
  call LOG_DBG("About to go into CALCULATE_RANDOM_MODEL...", PROCNAME)

  ! Call some calculations...
  call CALCULATE_RANDOM_MODEL()

  ! We can Log debug messages as much as possible...
  call LOG_DBG("Exited from CALCULATE_RANDOM_MODEL...", PROCNAME)

  call LOG_DBG("About to Initialise random seed...", PROCNAME)

  call RANDOM_SEED_INIT() ! Standard utility to init random seed from BASE_UTIL

  ! Do some calculations...

  ! The good programming style for modelling assumes
  ! hiding as much as possible to subroutines or functioins
  ! so that we could easily see the high-level structure of the model
  ! and avoid most of the clutter....

  call LOG_DBG("Starting to cycle over i ...", PROCNAME)

  do i=1, ubound(RAND_A, 1)

    call LOG_DBG("Starting to cycle over j...", PROCNAME)

    do j=1, ubound(RAND_A, 2)

      call random_number(RAND_A(i,j))

      call LOG_DBG( "Writing row " // TOSTR(i) // ", col " // TOSTR(j) // &
                  ", Value=" // TOSTR(RAND_A(i,j)) , PROCNAME)

    end do

  end do

  call LOG_DBG("Exit cycles over j and j...", PROCNAME)

  name_for_csv_out = MODEL_NAME // "-data-randon.csv" ! CSV file name

  ! We can use high-level functions to save data matrices
  call LOG_DBG("We are going to output RAND_A to " // name_for_csv_out, &
        PROCNAME)

  ! Here is the actual data save function, it can handle one-dimensional arrays
  !  and two-dimensional matrices of any type (integer,real,double,character)
  call CSV_MATRIX_WRITE(matrix=RAND_A, csv_file_name=name_for_csv_out, &
                        csv_file_status=no_error_status)

  ! Error handling...  and debug logging...
  call LOG_DBG("Write status for RAND_A file " // name_for_csv_out &
                // " is " // TOSTR(no_error_status) // ".", PROCNAME)

  call CHECK_ERROR_FILE(no_error_status)

  call LOG_CONFIGURE("writeonstdout" , .true.) ! Log to screen again enabled

  call LOG_CONFIGURE("timestamp", .false.)     ! config the log timestamp OFF

  call LOG_MSG("We are finishing our calculations now...")

  call STDOUT("************************************************",&
              "*          CALCULATIONS FINISHED               *",&
              "************************************************")

  call LOG_SHUTDOWN ()  ! close logger

  stop 0  ! explicit stop, not really needed


!----[ end of the program ]-----------------------------------------------------
end program MODEL_PROTO

!-------------------------------------------------------------------------------
! External and other subroutines follow...
! Note: the use of modules is more convenient as we won't need interface blocks
! which may be mandatory in some cases...
!-------------------------------------------------------------------------------

subroutine CALCULATE_RANDOM_MODEL ()

!----[ DECLARATION SECTION ]----------------------------------------------------
  use COMMONDATA    ! This is to get access to the global data and objects
  use BASE_UTILS    ! for Basic utils
  use CSV_IO        ! for CSV data handling
  use LOGGER        ! for the Logger

  implicit none

  ! Subroutine name for DEBUG LOGGER
  character (len=*), parameter :: PROCNAME = "CALCULATE_RANDOM_MODEL"

  integer :: i, j, k, l, m, o  ! local variables for this subroutine

  character(len=:), allocatable :: CSV_FILE_APPEND_DATA_NAME  ! CSV file
  integer :: CSV_FILE_APPEND_DATA_UNIT                        !     unit
  logical :: CSV_WRITTEN_OK                                   !     no-error

  real :: RandomNumber

  ! Need this for temporary keeping of the current file record.  But note that
  ! it cannot be declared allocatable as it will get empty value with zero
  ! for a new record. length. Although it is possible to explicitly reallocate
  ! it with the necessary values each time
  character (len=:), allocatable :: RECORD_CSV

  !----[ CALCULATION SECTION ]--------------------------------------------------

  ! Write / log what is being done to log if DEBUGging
  call LOG_DBG("Entering subroutine CALCULATE_RANDOM_MODEL...", PROCNAME)

  do i=1, Cinds     ! We will here (1) )cycle over the dimensions of
                    ! the multidimensional array; (2) set its value;
    do j=1, CgNR    ! (3) write the value to CSV output files
                    ! using low-level routines outputting single numbers
      do k=1, Cxy   ! in numerous individual files i x j x k
                    ! CSV output commands are enclosed in comments
                    ! Note: CSV write block
                    ! ...
                    ! end CSV block

        ! Note: CSV write block
        ! So, to write data to CSV output we do the following:
        ! 1. generate CSV file name using number to string function TOSTR
        CSV_FILE_APPEND_DATA_NAME="data_genomeNR_" // TOSTR(i) // "_" // &
          TOSTR(j) // "_" // TOSTR(k) // ".csv"
        ! Note: CSV
        ! 2. open CSV file for writing
        call CSV_FILE_OPEN_WRITE (CSV_FILE_APPEND_DATA_NAME, &
                          CSV_FILE_APPEND_DATA_UNIT, CSV_WRITTEN_OK)
        !    2.1. handle possible CSV error via wrapper
        call CHECK_ERROR_FILE (CSV_WRITTEN_OK)
        ! end CSV block

        do l=1, Cdip

          ! Note: CSV block
          ! We will begin a new CSV record (row) for writing to file, so
          ! 3. Nullify current CSV record (row) to empty
          RECORD_CSV=repeat(" ", 255)
          ! end CSV block

          do m=1, CNRcomp
            ! do calculations... get it ransom
            call random_number( genomeNR(i,j,k,l,m) )

            ! And we can of course log everything
            call LOG_DBG("Set value =" // &
                          TOSTR(genomeNR(i,j,k,l,m)), PROCNAME)

            ! Note: CSV block
            ! 4. for CSV output we cycle over m and append each number to the
            !    same record
            call CSV_RECORD_APPEND ( RECORD_CSV, genomeNR(i,j,k,l,m) )
            ! end CSV block

          end do

          ! IMPORTANT NOTE::
          ! Yes, TOSTR accepts vectors, here we use slices to print the first
          ! three elements of the last index calculated at previous cycle "m".
          ! Reals also accepts Fortran formats and integer array constructors
          ! like (/(i, i=1,3, 1)/) also work; char(9) is TAB:
          call STDOUT( &
            "Data ::" // TOSTR(i) // "," // TOSTR(j) // "," // TOSTR(k) &
            // "," // TOSTR(l) // char(9) // TOSTR( (/(o, o=1,3, 1)/) ) // &
            " :: " // TOSTR(genomeNR(i,j,k,l,1:3), "(f4.2)") )
                      ! This outputs such things:
                      ! Data ::10,10,2,20  1 2 3 ::  0.92 0.35 0.09

          call LOG_DBG("About to write record " // TOSTR(m) // ", of size " &
                        // TOSTR(CSV_RECORD_SIZE(RECORD_CSV)) , PROCNAME)
          ! Note: CSV block
          ! 5. write l-th CSV record
          !    Note that we use named optional parameters and can refer to the
          !    CSV file by either name or unit, named parameters may go in any
          !    order
          call CSV_FILE_RECORD_WRITE (record=RECORD_CSV, &
                                      csv_file_name=CSV_FILE_APPEND_DATA_NAME,&
                                      csv_file_status=CSV_WRITTEN_OK)
          ! Note: CSV block
          ! 5.1. again handle  possible CSV error via wrapper
          call CHECK_ERROR_FILE (CSV_WRITTEN_OK)
          ! end CSV block

        end do ! end l

        ! Note: CSV block
        ! 6. close the current CSV file for writing
        call CSV_FILE_CLOSE( csv_file_name=CSV_FILE_APPEND_DATA_NAME, &
                             csv_file_status=CSV_WRITTEN_OK )
        !    6.1. again handle  possible CSV error via wrapper
        call CHECK_ERROR_FILE (CSV_WRITTEN_OK)
        ! end CSV block

      end do ! end k

    end do ! end j

  end do ! end i

  call LOG_DBG("Now we are about to go out of CALCULATE_RANDOM_MODEL...", &
                PROCNAME)

end subroutine CALCULATE_RANDOM_MODEL









