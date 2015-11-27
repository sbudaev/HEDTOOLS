! Test program for module CSV_IO
! VERSION AND DATE: 1.3, 2015/11/24
! Compile for test this way
! include "../BASE_CSV_IO.f90"
!-------------------------------------------------------------------------------
! Compile the module and the test binary, and run the binary:
! ** for GNU platform
! gfortran -g -c ../BASE_CSV_IO.f90 ../BASE_UTILS.f90 ../BASE_LOGGER.f90
! gfortran -g -o ZZZ test_CSV_IO.f90 ../BASE_CSV_IO.f90 ../BASE_UTILS.f90 ../BASE_LOGGER.f90 && ./ZZZ
! ** for Oracle platform
! f95 -g -c ../BASE_CSV_IO.f90 ../BASE_UTILS.f90 ../BASE_LOGGER.f90
! f95 -g -o ZZZ test_CSV_IO.f90 ../BASE_CSV_IO.f90 ../BASE_UTILS.f90 ../BASE_LOGGER.f90 && ./ZZZ

program TEST_CSV_IO

 ! --------------------------[ Initialisations etc...]--------------------------

 use CSV_IO                   ! for CSV output
 use BASE_UTILS               ! for string conversion, here NUMTOSTR
 use LOGGER                   ! Use the execution logger facility

 implicit none

 ! File related variables
 character(len=:), allocatable :: FILE_NAME_CSV1, FILE_NAME_CSV2  ! Names
 integer :: FILE_UNIT_CSV1, FILE_UNIT_CSV2                        ! Units

 logical :: FSTAT_CSV  ! This is a logical error flag showing if subroutine call
                       ! was successful (True) or there was an error (False)

 integer :: i, j, k    ! indices etc...

 ! Data matrix for CSV file
 real, dimension(5,5) :: ARRAY_X = reshape ((/ 1.,  2.,  3.,  4.,  5.9904, &
                                               6.,  7.,  8.,  9. ,10.0221, &
                                              11., 12., 13., 14., 15.0301, &
                                              16., 17., 18., 19., 20.0478, &
                                              21., 22., 23., 24., 25.9987  /), &
                                                                     (/5,5/))

 real, dimension(20) :: ARRAY_Y = [ &
                   1.1,  2.2,  3.3,  4.4,  5.5,  6.6,  7.7,  8.8,  9.9, 10.1, &
                  11.1, 12.2, 13.3, 14.4, 15.5, 16.6, 17.7, 18.8, 19.9, 20.9 ]

 real, dimension (100,10) :: MATRIX

 real, dimension(10) :: ARRAY_Z

 ! We need to declare the character variable to store CSV record
 character (len=255) :: RECORD_CSV

 !==============================================================================

 print *, "Running program"

 call LOG_STARTUP ( "test_CSV_logger.log" )        ! Init. Log file
 call LOG_CONFIGURE ( "writeonstdout" , .true. )   ! do/not write on screen
 call LOG_CONFIGURE ( "timestamp" , .true. )       ! do write timestamps
 call LOG_CONFIGURE ( "level_string_volume" , "chapter" )
 call LOG_DELIMITER ( )                             ! issue log delimiter
 call LOG_MSG( "This also uses the logging utility" )
 call LOG_MSG( "  Logger can use either STDOUT and FILE..." )

 call LOG_CONFIGURE ( "writeonstdout" , .false. ) ! do not write on screen further
 call LOG_DELIMITER ( )

 FILE_NAME_CSV1 = "ZZZ_FILE_TEST_1.csv"   ! this is our output file
 FILE_UNIT_CSV1 = -1                      ! We use invalid file unit, so it is
                                          ! initially generated automatically
                                          ! by the CSV_FILE_OPEN_WRITE sub.
                                          ! Had we used valid unit, it were used
                                          ! instead of generated one...

 call LOG_MSG ( "Use CSV file " // FILE_NAME_CSV1 // " and unit " // &
      TOSTR(FILE_UNIT_CSV1) )
 !******************************************************************************

 ! Open CSV fie for writing, This opens the output file physically.
 ! Note that  if file exists it will be overwritten...
 call CSV_FILE_OPEN_WRITE (FILE_NAME_CSV1, FILE_UNIT_CSV1, FSTAT_CSV)
 if (.not. FSTAT_CSV) goto 1000

 call LOG_MSG ("Opened the output file for writing, got unit " // TOSTR(FILE_UNIT_CSV1) )

 ! we can get the file unit from the file name; most CSV IO routines accept
 ! either csv_file_name or csv_file_unit optional parameters. If both
 ! are included, unit has precedence.
 i=GET_FILE_UNIT(csv_file_name="ZZZ_FILE_TEST_1.csv", csv_file_status=FSTAT_CSV)
 if (.not. FSTAT_CSV) goto 1000

 call LOG_MSG ("Run illustration of how to get a free file unit, got " // TOSTR(i) )


 ! We can first write an optional header line for the CSV file,
 ! using
 !          call CSV_FILE_HEADER_WRITE
 ! but postpone this a little to show include some more information into
 ! this header

 ! -------------------[ Writing headers for data columns ]----------------------
 ! Initialise the first element of the CSV record (first column).
 ! For every record, arbitrary numbers and strings can be appended
 ! with CSV_RECORD_APPEND subroutine with generic interfaces. i.e.
 ! it accepts the first parameter of the types string, integer and real
 !
 ! NOTE that we do not yet physically write the data to the file,
 ! only produce record data. Physical output is managed by
 !   call CSV_FILE_RECORD_WRITE ()
 RECORD_CSV="" ; call CSV_RECORD_APPEND(RECORD_CSV, "ROW_NAMES")
 ! Append numbered column names - construct variable names. Note that
 ! string values to append may contain numbers, but these should be converted
 ! to string type and appended using standard // operator, in this case
 ! "VAR_" // NUMTOSTR(i)) converts to VAR_1, VAR_2 ...
 do i=1, 5
   call CSV_RECORD_APPEND(RECORD_CSV, "VAR_" // NUMTOSTR(i))
 end do
 !And append an additional arbitrary column header
 call CSV_RECORD_APPEND(RECORD_CSV, "INTEGER")

 ! Add final column header for the last text column
 call CSV_RECORD_APPEND(RECORD_CSV, "Final text column")

 ! Print the size of this record. We use dedicated function for this...
 print *, "The size of this record is: ", CSV_RECORD_SIZE (RECORD_CSV), "columns"

 call LOG_MSG ( "Created the headers for the CSV file columns, total " &
               // TOSTR(CSV_RECORD_SIZE(RECORD_CSV)) // "Columns in this file" )

 ! -----------------[ Writing File header at the first row ]--------------------
 ! Write CSV file header. We postponed the file header after the column names
 ! to illustrate how we could count the number of CSV fields...
 ! So we (1) make column headers (but don't write them physically to the file);
 ! (2) count how many columns we got; (3) physically write header containing
 ! the number of columns; (4) physically write the column headers produced
 ! at (1).
 call CSV_FILE_HEADER_WRITE(csv_file_name=FILE_NAME_CSV1, &
      header="Example header; full timestamp: " // TIMESTAMP_FULL() // &
      ". Total " // NUMTOSTR(CSV_RECORD_SIZE (RECORD_CSV)) // " columns.", &
      csv_file_status=FSTAT_CSV)
 if (.not. FSTAT_CSV) goto 1000

 ! -----------------[ Write the first record of data (column ]------------------
 ! -----------------[ names to the physical file.            ]------------------
 ! CSV record of row IDs is now ready to be written to the file
 call CSV_FILE_RECORD_WRITE (csv_file_name=FILE_NAME_CSV1, record=RECORD_CSV, &
      csv_file_status=FSTAT_CSV)
  if (.not. FSTAT_CSV) goto 1000

 ! --------------[ Write the actual table of numbers to the CSV file]-----------
 ! We will write a data file with row and column headers to the CSV file...
 do i=1, 5

   ! Initialise the first column of each record
   RECORD_CSV=""
   ! and construct row names for this record subsequently
   call CSV_RECORD_APPEND( RECORD_CSV, "ROW_" // NUMTOSTR(i) )
   ! Now we are ready to fill the rest of the record
   ! it will contain our variables by rows (reverse from Fortran convention)...
   do j=1, 5
     call CSV_RECORD_APPEND( RECORD_CSV, ARRAY_X(j,i) )
   end do

   ! We now append an integer to the record
    call CSV_RECORD_APPEND( RECORD_CSV, 122 )

    ! And finally append another text field
    call CSV_RECORD_APPEND( RECORD_CSV, "Final txt string row=" // NUMTOSTR(i) )

   ! We are now ready to write the complete record to the file and loop
   ! to the next row (record) of data values, note that either file unit or
   ! name can be used
   call CSV_FILE_RECORD_WRITE (csv_file_unit=FILE_UNIT_CSV1, record=RECORD_CSV, &
      csv_file_status=FSTAT_CSV)
   if (.not. FSTAT_CSV) goto 1000

 end do

 call LOG_MSG ( "File " // FILE_NAME_CSV1 // "written, but not yet closed" )

 ! Close CSV file
 call CSV_FILE_CLOSE(csv_file_name=FILE_NAME_CSV1, csv_file_status=FSTAT_CSV)
 if (.not. FSTAT_CSV) goto 1000

 call LOG_MSG ( "File closed" )
 call LOG_SHUTDOWN ()

 ! ----------------------[Count lines of an arbitrary CSV file]-----------------
 ! Just any ine is counted, not distinguishing between header, column names etc.
 ! In case of an error reading file, it results in -1 and error status.
 print *, "Check file: ", CSV_FILE_LINES_COUNT (FILE_NAME_CSV1, &
            FSTAT_CSV), "lns; status:", FSTAT_CSV

 !******************************************************************************
 !---------------[Write another file, without clutter of comments]--------------

 FILE_NAME_CSV2 = "ZZZ_FILE_TEST_2.csv"
 FILE_UNIT_CSV2 = -1  ! Note that any invalid unit is "autocorrected" following
                      ! CSV_FILE_OPEN_WRITE sub

 call CSV_FILE_OPEN_WRITE (FILE_NAME_CSV2, FILE_UNIT_CSV2, FSTAT_CSV)
 if (.not. FSTAT_CSV) goto 1000

 RECORD_CSV='"ROWS"' ! If first string is manually set, don't forget to quote it
 do i=1, 20          ! or first initialise as "" and then do CSV_RECORD_APPEND
   call CSV_RECORD_APPEND( RECORD_CSV, "VAR_" // NUMTOSTR(i) )
 end do
 call CSV_FILE_RECORD_WRITE (csv_file_name=FILE_NAME_CSV2, &
        record=RECORD_CSV, csv_file_status=FSTAT_CSV)
 if (.not. FSTAT_CSV) goto 1000

 do i=1, 1000
   RECORD_CSV="" ! first element of the numeric record = case name
   call CSV_RECORD_APPEND( RECORD_CSV, "ROW_" // NUMTOSTR(i) )

   do j=1, 20
     call CSV_RECORD_APPEND( RECORD_CSV, ARRAY_Y(j) )
   end do
   call CSV_FILE_RECORD_WRITE (csv_file_name=FILE_NAME_CSV2, &
        record=RECORD_CSV, csv_file_status=FSTAT_CSV)
   if (.not. FSTAT_CSV) goto 1000

 end do

 call CSV_FILE_CLOSE(csv_file_name=FILE_NAME_CSV2, csv_file_status=FSTAT_CSV)
 if (.not. FSTAT_CSV) goto 1000

 !******************************************************************************
 !------------[ Now use the higher level sub to write whole matrix ]------------

 ! Construct matrix, the matrix can be integer, real, double or character
 do j=lbound(MATRIX, 2), ubound(MATRIX, 2)      ! we first loop over rows that
   do i=lbound(MATRIX, 1), ubound(MATRIX, 1)    ! is a little faster in Fortran
      MATRIX (i,j) = real(i)+100*real(j)+.0024
   end do
 end do

 ! Save matrix
 call CSV_MATRIX_WRITE ( MATRIX, "ZZZ_FILE_TEST_3.csv", FSTAT_CSV)
 if (.not. FSTAT_CSV) goto 1000

 !******************************************************************************
 !-------------[ We can write 1-d arrays too ]----------------------------------

 do i=lbound(ARRAY_Z, 1), ubound(ARRAY_Z, 1)
      ARRAY_Z=i+0.003
 end do

 ! save array
 call CSV_MATRIX_WRITE ( ARRAY_Z, "ZZZ_FILE_TEST_4.csv", FSTAT_CSV)
 if (.not. FSTAT_CSV) goto 1000

 !--------------------------[ End of program ]----------------------------------

 print *, TIMESTAMP_FULL()
 stop 0

1000 print *, "FILE ERROR"
     stop 1


end program TEST_CSV_IO
