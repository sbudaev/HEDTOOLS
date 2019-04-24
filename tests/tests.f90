module m_tests
contains
  !> This is called whenever test fails. In this case, the test program
  !! terminates with the exit code 255.
  subroutine fail_test(name)
    character(*), intent(in) :: name
    character(*), parameter :: MESSAGE = "Fail test "
    print *, MESSAGE, name
    stop 255
  end subroutine fail_test
  !> This is called whenever test passes
  subroutine pass_test(name)
    character(*), intent(in) :: name
    print *, "Passed ", name
  end subroutine pass_test
end module m_tests

!===============================================================================

program tests_hedtools
use m_tests

 print *, "*** Tests started ***"
 call test_CSV_IO()
 call test_STRINGS()
 print *, "*** Tests completed ***"

contains

subroutine test_CSV_IO
  use CSV_IO

  integer :: unum
  logical :: fstat
  integer, parameter :: ROWS=100, COLS=20
  real, dimension(ROWS,COLS) :: DATA_OUT, DATA_IN
  character(len=*), parameter :: TESTNAME="test_CSV_IO"

  print *, "Test: ", TESTNAME

  if (.not. CHECK_UNIT_VALID(1) .eqv. .TRUE.) &
                            call fail_test("CHECK_UNIT_VALID 1")
  if (.not. CHECK_UNIT_VALID(0) .eqv. .FALSE.) &
                            call fail_test("CHECK_UNIT_VALID 0")
  if (.not. CHECK_UNIT_VALID(-1) .eqv. .FALSE.) &
                            call fail_test("CHECK_UNIT_VALID -1")
  if (.not. CHECK_UNIT_VALID(500) .eqv. .FALSE.) &
                            call fail_test("CHECK_UNIT_VALID 500")

  ! This file does not exist so far
  if (.not. CHECK_FILE_OPEN("i_dont_exist.csv") .eqv. .FALSE.) &
                            call fail_test("CHECK_FILE_OPEN non-exist")

  ! Opening new file for writing CHECK_FILE_OPEN returns TRUE
  call CSV_OPEN_WRITE("test_file_1.csv", unum, fstat)
  if (.not. fstat .eqv. .TRUE.) call fail_test("Exist file write T")
  if (.not. CHECK_FILE_OPEN("test_file_1.csv") .eqv. .TRUE.) &
                            call fail_test("CHECK_FILE_OPEN exist T")

  ! Opening non-existing file for reading CHECK_FILE_OPEN returns FALSE
  call CSV_OPEN_READ("i_dont_exist.csv", unum, fstat)
  if (.not. fstat .eqv. .FALSE.) call fail_test("Non-exist read file F")
  if (.not. CHECK_FILE_OPEN("i_dont_exist.csv") .eqv. .FALSE.) &
                            call fail_test("CHECK_FILE_OPEN non-exist F")

  DATA_OUT = 1.1
  call CSV_MATRIX_WRITE(DATA_OUT, "file_1.csv" )
  DATA_IN = CSV_MATRIX_READ("file_1.csv")
  if ( any(DATA_OUT /= DATA_IN) ) call fail_test("DATA IN /= OUT")

end subroutine test_CSV_IO

subroutine test_STRINGS
  use BASE_STRINGS

  character(len=255) :: test_str_01, test_str_02
  character(len=24), dimension(10) :: substrings
  character(len=*), dimension(5), parameter ::  checksubstring = [ "This  ", &
                                                                   "is    ", &
                                                                   "a     ", &
                                                                   "test  ", &
                                                                   "string" ]
  integer :: n_parts, inumber, ierr
  real :: rnumber
  character(len=*), parameter :: TESTNAME="test_STRINGS", DELIMS=",:;"
  integer :: ipos, imatch
  character :: char

  print *, "Test: ", TESTNAME

  ! Test PARSE()
  test_str_01 = "This is a test string"
  call PARSE(test_str_01, " ,:", substrings, n_parts)
  if ( .not. n_parts == 5 ) call fail_test("Wrong N of substrings in PARSE")
  ! Note: any called without trimming, might result mismatch due to spaces with
  !       some compiler
  if ( any( substrings(1:5)/=checksubstring(1:5) ) )                          &
                            call fail_test( "Error in PARSE strings" )

  ! Test COMPACT()
  test_str_01 = "Test   string     with      wide gaps"
  call COMPACT(test_str_01)
  if ( .not. trim(test_str_01) == "Test string with wide gaps" )              &
                            call fail_test("Error in COMPACT")

  ! Test REMOVESP()
  test_str_01 = "Test   string     with      wide gaps"
  call REMOVESP(test_str_01)
  if ( .not. trim(test_str_01) == "Teststringwithwidegaps" )                  &
                            call fail_test("Error in REMOVESP")

  ! Test VALUE()
  test_str_01 = "001"
  call VALUE(test_str_01, inumber, ierr)
  if ( .not. (inumber==1 .and. ierr==0) )                                     &
                            call fail_test("Error in VALUE with integer")

  test_str_01 = "1000"
  call VALUE(test_str_01, inumber, ierr)
  if ( .not. (inumber==1000 .and. ierr==0) )                                  &
                            call fail_test("Error in VALUE with integer")

  test_str_01 = "-1000"
  call VALUE(test_str_01, inumber, ierr)
  if ( .not. (inumber==-1000 .and. ierr==0) )                                 &
                            call fail_test("Error in VALUE with integer")

  test_str_01 = "2.345"
  call VALUE(test_str_01, rnumber, ierr)
  if ( .not. (rnumber==2.345 .and. ierr==0) )                                 &
                            call fail_test("Error in VALUE with real")

  test_str_01 = "-1000.123"
  call VALUE(test_str_01, rnumber, ierr)
  if ( .not. (rnumber==-1000.123 .and. ierr==0) )                             &
                            call fail_test("Error in VALUE with real")

  test_str_01 = "ERROR_VALUE_2.345"
  call VALUE(test_str_01, rnumber, ierr)
  if ( ierr==0 ) call fail_test("Error in VALUE with error reporting")

  ! Test SHIFTSTR()
  test_str_01 = "Test string"
  call SHIFTSTR(test_str_01, 4)  ! 1234-----------
  if ( .not. trim(test_str_01) == "    Test string" )                         &
                            call fail_test("Error in SHIFTSTR")

  ! Test INSERTSTR()
  test_str_01 = "Test string"
  call INSERTSTR(test_str_01, " a small", 5)
  !                               '12345+++++++-------'
  if ( .not. trim(test_str_01) == "Test a small string" )                     &
                            call fail_test("Error in INSERTSTR")

  ! Test DELSUBSTR()
  test_str_01 = "Test a small string"
  call DELSUBSTR(test_str_01, " a small")
  if ( .not. trim(test_str_01) == "Test string" )                             &
                            call fail_test("Error in DELSUBSTR")

  ! Test DELSUBSTR(), only the first substring is deleted
  test_str_01 = "Test a small string, which is a small string"
  call DELSUBSTR(test_str_01, " a small")
  if ( .not. trim(test_str_01) == "Test string, which is a small string" )    &
                            call fail_test("Error in DELSUBSTR, repeated")

  ! Test DELALL(), only the first substring is deleted
  test_str_01 = "Test a small string, which is a small string zero"
  call DELALL(test_str_01, " a small")
  if ( .not. trim(test_str_01) == "Test string, which is string zero" )       &
                            call fail_test("Error in DELALL")

  ! Test UPPERCASE()
  test_str_01 = "Test a small string"
  if ( .not. trim(UPPERCASE(test_str_01)) == "TEST A SMALL STRING" )          &
                            call fail_test("Error in UPPERCASE")

  if ( .not. trim(UPPERCASE("some text")) == "SOME TEXT" )                    &
                            call fail_test("Error in UPPERCASE inline")

  ! Test LOWERCASE()
  test_str_01 = "Test a Small striNg"
  if ( .not. trim(LOWERCASE(test_str_01)) == "test a small string" )          &
                            call fail_test("Error in LOWERCASE")

  if ( .not. trim(LOWERCASE("Some TeXt")) == "some text" )                    &
                            call fail_test("Error in LOWERCASE inline")

  ! Test MACH(), only the first substring is deleted
  !              1234567890123-5-789012345678-0-23456789012345678901234
  test_str_01 = "Test a string {a small string} brackets close at 15-30"
  call MATCH(test_str_01, 15, imatch)
  if ( .not. imatch == 30 ) call fail_test("Error in MATCH {}")

  test_str_01 = "Test a string <a small string> brackets close at 15-30"
  call MATCH(test_str_01, 15, imatch)
  if ( .not. imatch == 30 ) call fail_test("Error in MATCH <>")

  test_str_01 = "Test a string (a small string) brackets close at 15-30"
  call MATCH(test_str_01, 15, imatch)
  if ( .not. imatch == 30 ) call fail_test("Error in MATCH ()")

  test_str_01 = "Test a string [a small string] brackets close at 15-30"
  call MATCH(test_str_01, 15, imatch)
  if ( .not. imatch == 30 ) call fail_test("Error in MATCH []")

  ! Test TRIMZERO
  test_str_01 = "0.00400"
  call TRIMZERO(test_str_01)
  if ( .not. trim(test_str_01) == "0.004" ) call fail_test("Error in TRIMZERO")

  test_str_01 = "10.0400"
  call TRIMZERO(test_str_01)
  if ( .not. trim(test_str_01) == "10.04" ) call fail_test("Error in TRIMZERO")

  test_str_01 = "20.30000"
  call TRIMZERO(test_str_01)
  if ( .not. trim(test_str_01) == "20.3" ) call fail_test("Error in TRIMZERO")

  ! Test IS_LETTER, Note that UTF character set is not supported
  if ( .not. IS_LETTER("A") .eqv. .TRUE. ) call fail_test("Error in IS_LETTER")
  if ( .not. IS_LETTER("z") .eqv. .TRUE. ) call fail_test("Error in IS_LETTER")
  if ( .not. IS_LETTER("1") .eqv. .FALSE. ) call fail_test("Error in IS_LETTER")

  ! Test IS_DIGIT, Note that UTF character set is not supported
  if ( .not. IS_DIGIT("1") .eqv. .TRUE. ) call fail_test("Error in IS_DIGIT")
  if ( .not. IS_DIGIT("9") .eqv. .TRUE. ) call fail_test("Error in IS_DIGIT")
  if ( .not. IS_DIGIT("A") .eqv. .FALSE. ) call fail_test("Error in IS_DIGIT")


  ! Test SPLIT(), only the first substring is deleted
  test_str_01 = "First part, of a string, that is long"
  call SPLIT(test_str_01, ",", test_str_02, char)
  if ( .not. ( trim(test_str_01) == "of a string, that is long" .and.         &
               trim(test_str_02) == "First part" .and. char == "," )  )       &
                            call fail_test("Error in SPLIT")

  test_str_01 = "word1 word2 word3"
  call SPLIT(test_str_01, " ,:", test_str_02, char)
  if ( .not. ( trim(test_str_01) == "word2 word3" .and.                       &
               trim(test_str_02) == "word1" .and. char == " " )  )            &
                            call fail_test("Error in SPLIT")

  test_str_01 = "part1 part2 part3"
  call SPLIT(test_str_01, " ,:", test_str_02)
  if ( .not. ( trim(test_str_01) == "part2 part3" .and.                       &
               trim(test_str_02) == "part1" )  )                              &
                            call fail_test("Error in SPLIT")

  ! Test REMOVEBKSL
  test_str_01 = "pt1_\_pt2"
  call REMOVEBKSL(test_str_01)
  if ( .not. test_str_01 == "pt1__pt2" ) call fail_test("Error in REMOVEBKSL")

  ! Test IS_NUMERIC
  if ( .not. IS_NUMERIC("1.2", .TRUE.) .eqv. .TRUE. )                         &
                            call fail_test("Error in IS_NUMERIC")
  if ( .not. IS_NUMERIC(" 1.2", .TRUE.) .eqv. .TRUE. )                        &
                            call fail_test("Error in IS_NUMERIC")
  if ( .not. IS_NUMERIC(" 1.2", .FALSE.) .eqv. .FALSE. )                      &
                            call fail_test("Error in IS_NUMERIC")
  if ( .not. IS_NUMERIC("DD", .FALSE.) .eqv. .FALSE. )                        &
                            call fail_test("Error in IS_NUMERIC")
  if ( .not. IS_NUMERIC("  ", .TRUE.) .eqv. .FALSE. )                         &
                            call fail_test("Error in IS_NUMERIC")

end subroutine test_STRINGS


end program tests_hedtools
