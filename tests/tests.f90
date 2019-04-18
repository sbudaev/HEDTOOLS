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

 call test_CSV_IO()
 call test_STRINGS()
 print *, "Tests completed"

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

  character(len=255) :: test_str_01
  character(len=24), dimension(10) :: substrings
  character(len=*), dimension(5), parameter ::  checksubstring = [ "This  ", &
                                                                   "is    ", &
                                                                   "a     ", &
                                                                   "test  ", &
                                                                   "string" ]
  integer :: n_parts
  character(len=*), parameter :: TESTNAME="test_STRINGS"

  print *, "Test: ", TESTNAME

  test_str_01 = "This is a test string"

  ! Test PARSE()
  call PARSE(test_str_01, " ,:", substrings, n_parts)
  if ( .not. n_parts == 5 ) call fail_test("Wrong N of substrings in PARSE")
  if ( any( substrings(1:5)/=checksubstring(1:5) ) )                          &
                            call fail_test( "Error in PARSE strings" )

end subroutine test_STRINGS


end program tests_hedtools
