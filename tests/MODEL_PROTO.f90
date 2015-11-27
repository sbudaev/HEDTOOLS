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
! Buld commands for GNU fortran
! gfortran -g -c ../BASE_CSV_IO.f90 ../BASE_UTILS.f90 ../BASE_LOGGER.f90
! gfortran -g -c MODEL_PROTO.f90
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
  integer, parameter, public :: Cinds = 10
  integer, parameter, public :: Cfmothers = 10, Cmaxlife = 5000
  integer, parameter, public :: CgNR = 10,Cxy = 2, Cdip = 20, CNRcomp = 10
  integer, parameter, public :: CgMEM = 2

  !----------- VARIABLE/ARRAYS/MATRICES SECTION --------------------------------

  ! genome for neuronal responses CgNR responses,genes in NR,diploid,CNR
  ! components)
  real, public :: genomeNR(Cinds,CgNR,Cxy,Cdip,CNRcomp)

  ! genome for modulatory genes CgMOD genes,diploid)
  real genomeMEM(Cinds,CgMEM,Cdip)

  ! genome for memory genes !(CgMEM genes,diploid)
  real genomeMOD(Cinds,Cdip)

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

  ! We first initialise the log and set log file name
  call LOG_STARTUP (MODEL_NAME // "-log.txt")

  call LOG_CONFIGURE("timestamp", .true.)      ! Produce timestamps in the log
  call LOG_CONFIGURE("writeonstdout" , .true.) ! Output log on screen and file

  call LOG_CONFIGURE("level_string_volume", "chapter" )  ! Set log level
  call LOG_DELIMITER()                                   ! Issue log delimiter

  ! Send messages to the log
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

subroutine LOG_DBG(message_string)
! This subroutine is a wrapper for writing debug messages
! It is hidden into a subroutine to avoid extra clutter in the
! main program...

  use LOGGER      ! we need it get access to logger

  implicit none

  character(len=*), intent(in) :: message_string

  if (IS_DEBUG) call LOG_MSG(message_string)

end subroutine LOG_DBG

subroutine FATAL_ERROR(message)
  !-----------------------------------------------------------------------------
  ! We may need an error trapping module...
  ! It may be  be produced later, presumably using something using IEEE
  ! assertions, that could trap numerical errors/esceptions etc...
  ! now, just trivial

  use LOGGER
  use BASE_UTILS

  implicit none

  character(len=*) :: message

  call LOG_CONFIGURE("writeonstdout" , .true.)  ! Enable log to the screen
  call LOG_DELIMITER()                          ! Write delimiter
  call LOG_MSG("Encountered an error; Type: " // message // " will stop now...")

    ! May duplicate message to standard error
  call STDERR("PROGRAM ERROR, will terminate now...")

  stop 255 ! produce exit code so that a model call script could locate error

end subroutine FATAL_ERROR

subroutine CHECK_ERROR_FILE ( file_status_flag )
  ! Wrapper subroutine to check if CSV output resulted in an error

  use LOGGER
  use BASE_UTILS
  implicit none

  logical, intent(in) :: file_status_flag

  if (file_status_flag) then
    ! If everything ok, write confirmation to the log
    call LOG_DBG("File operation returned " // TOSTR(file_status_flag) &
                  // " status." )
  else
    ! enable log output to the the screen as well as to the file
    call LOG_CONFIGURE("writeonstdout" , .true.)
    ! and log it
    call LOG_DELIMITER()
    call LOG_DBG("ERROR:: File operation returned error status " // &
                  TOSTR(file_status_flag))
    call LOG_DBG("We will go to FATAL_ERROR procedure now...")
    call LOG_DELIMITER()
    call FATAL_ERROR ("File output error")
  end if

end subroutine CHECK_ERROR_FILE

end module COMMONDATA

!-------------------------------------------------------------------------------

program MODEL_PROTO

  ! We first declare which modules the model is going to use

  use BASE_UTILS    ! Basic utils
  use CSV_IO        ! CSV data handling
  use LOGGER        ! System logger

  use COMMONDATA    ! use our global data / objects module

  implicit none     ! This is standard thing...

  ! Now we declare various model constants and variables...

  integer, parameter :: BIGNUM = 100, SMALLNUM = 10

  integer :: i, j, k

  real, dimension (BIGNUM,SMALLNUM) :: RAND_A

  real, dimension (BIGNUM,SMALLNUM) :: RAND_B

  real, dimension (BIGNUM) :: RAND_C

  ! We need a name and unit for CSV output file written  with CSV_IO module
  character(len=:), allocatable :: name_for_csv_out
  integer :: unit_for_csv_out

  logical :: no_error_status  ! Note that we may use an error status variable to
                              ! flag file output errors, mainly in CSV_IO

  ! ----------------------------------------------------------------------------
  ! We now start the model...

  ! First, initialise the model logger. Logger is quite configurable and
  ! is managed by LOG_CONFIGURE ()
  ! It is probably better to hide all this logger init code into a subroutine
  ! to avoid cluttering ...
  call LOGGER_INIT()

  call LOG_DELIMITER() ! It is good to write delimiter lines to the log to focus
                       ! attention on more important things

  ! We also have options to write some information on the standard output
  ! and standard error devices. This does not go to log. It can be considered
  ! as normal output of the program

  ! STDOUT accepts optional number of string arguments, every one
  ! of it goes as a seperete line. STDOUT is intended for all routine screen
  ! output, LOGGER is for debug and service messages that are normally hidden
  call STDOUT("----------------------------------------------------------------",&
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
  ! of course, we can hide this code to
  call LOG_DBG("About to go into INIT_DATA...") ! Log only in DEDUG mode using
                                                ! wrapper subroutine
  ! Call some calculations...
  call INIT_DATA()

  call LOG_DBG("Exited from INIT_DATA...")      ! We can Log debug messages as
                                                ! much as possible...

  call LOG_DBG("About to Initialise random seed...") ! Log only in DEDUG mode

  call RANDOM_SEED_INIT() ! Standard utility to init random seed from BASE_UTIL

  ! Do some calculations...

  ! The good programming style for modelling assumes
  ! hiding as much as possible to subroutines or functioins
  ! so that we could easily see the high-level structure of the model
  ! and avoid most of the clutter....

  call LOG_DBG("Starting to cycle over i ...") ! Log only in DEDUG mode

  do i=1, ubound(RAND_A, 1)

    call LOG_DBG("Starting to cycle over j...")

    do j=1, ubound(RAND_A, 2)

      call random_number(RAND_A(i,j))

      call LOG_DBG( "Writing" // TOSTR(i) // ", " // TOSTR(j) // "=" &
                  // TOSTR(RAND_A(i,j)) )

    end do

  end do

  call LOG_DBG("Exit cycles over j and j...")

  name_for_csv_out = MODEL_NAME // "-data-randon.csv" ! CSV file name

  ! We can use high-level functions to save data matrices
  call LOG_DBG("We are going to output RAND_A to " // name_for_csv_out)

  ! here is the actual function, it can handle one-dimensional arrays and
  ! two-dimensional matrices of any type (integer, real, double, and character
  call CSV_MATRIX_WRITE(RAND_A, name_for_csv_out, no_error_status)

  ! Error handling...  and debug logging...
  call LOG_DBG("Write status for RAND_A file " // name_for_csv_out &
                // " is " // TOSTR(no_error_status) // ".")

  call CHECK_ERROR_FILE(no_error_status)

  call LOG_CONFIGURE("writeonstdout" , .true.) ! Log to screen again enabled

  call LOG_MSG("We are finishing our calculations now...")

  call STDOUT("************************************************",&
              "           CALCULATIONS FINISHED                ",&
              "************************************************")

  call LOG_SHUTDOWN ()  ! close logger

  stop 0  ! explicit stop, not really needed

!-------------------------------------------------------------------------------
! end of the program
!-------------------------------------------------------------------------------

contains ! Now, subroutines local to the MODEL_PROTO may follow...
! We include subroutines after contains within the main program so that
! they have access to program-private objects and do not need to USE XXX

subroutine CALC_SOMETHING (A, B, C)
! This is just an arbitrary local subroutine within the model definition
! module it should be accessible to all subroutines (if not declared private)

integer, intent(in) :: A, B
integer, intent(out) :: C

C=A+B ! trivial thing here

end subroutine CALC_SOMETHING

end program MODEL_PROTO

!-------------------------------------------------------------------------------
! External and other subroutines follow...
!-------------------------------------------------------------------------------

subroutine INIT_DATA ()

  use COMMONDATA    ! This is to get access to the global data and objects
  use BASE_UTILS    ! for Basic utils
  use CSV_IO        ! for CSV data handling
  use LOGGER        ! for the Logger

  implicit none

  integer :: i, j, k, l, m, o  ! local variables for this subroutine

  character(len=:), allocatable :: CSV_FILE_APPEND_DATA_NAME  ! CSV file
  integer :: CSV_FILE_APPEND_DATA_UNIT                        !     unit
  logical :: CSV_WRITTEN_OK                                   !     no-error

  real :: RandomNumber

  ! Need this for temporary keeping of the current file record.  But note that
  ! it cannot be declared allocatable as it will get empty value with zero
  ! for a new record. length. Although it is possible to explicitly reallocate
  ! it with the necessary values each time
  character (len=255) :: RECORD_CSV

  ! Write / log what is being done to log if DEBUGging
  call LOG_DBG("Entering subroutine INIT_DATA...")

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
          RECORD_CSV=""
          ! end CSV block

          do m=1, CNRcomp
            ! do calculations... get it ransom
            call random_number( genomeNR(i,j,k,l,m) )

            ! And we can of course log everything
            call LOG_DBG("Set value =" // &
                          TOSTR(genomeNR(i,j,k,l,m)))

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
          ! like (/(i, i=1,3, 1)/) also work:
          call STDOUT("Data " // TOSTR( (/(o, o=1,3, 1)/) ) // " :: " &
                      // TOSTR(genomeNR(i,j,k,l,1:3), "(f4.2)") )
                      ! This outputs such things:
                      ! Data  1 2 3 ::  0.92 0.35 0.09

          call LOG_DBG("About to write record " // TOSTR(m) // ", of size" &
                        // TOSTR(CSV_RECORD_SIZE(RECORD_CSV)) )
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

  call LOG_DBG("Now we are about to go out of INIT_DATA...")

end subroutine INIT_DATA









