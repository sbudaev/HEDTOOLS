module m_tests

! Definition of the double (kind=8) and quadruple (kind=16) precision
integer, parameter, public :: SP = selected_real_kind(6,   37)
integer, parameter, public :: DP = selected_real_kind(15,  307)
integer, parameter, public :: QP = selected_real_kind(33, 4931)

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

! Functions for testing FUNCZERO. Note that they are external to the test
! program tests_hedtools to meet Oracle f95 and perhaps older compilers
! that require function passing as an argument to be module procedure or
! explicit interface.
function zf_linear_r4(x) result (y)
  use m_tests
  real(SP), intent(in) :: x
  real(SP) :: y
  y = x + 1
end function zf_linear_r4

function zf_linear_r8(x) result (y)
  use m_tests
  real(DP), intent(in) :: x
  real(DP) :: y
  y = x + 1
end function zf_linear_r8

function zf_qubic_r4(x) result (y)
  use m_tests
  real(SP), intent(in) :: x
  real(SP) :: y
  y = x ** 3 - 2.0
end function zf_qubic_r4

function zf_qubic_r8(x) result (y)
  use m_tests
  real(DP), intent(in) :: x
  real(DP) :: y
  y = x ** 3 - 2.0
end function zf_qubic_r8

function zf_wave0_r4(x) result (y)
  use m_tests
  real(SP), intent(in) :: x
  real(SP) :: y
  y = sin(x) * 4.0
end function zf_wave0_r4

function zf_wave0_r8(x) result (y)
  use m_tests
  real(DP), intent(in) :: x
  real(DP) :: y
  y = sin(x) * 4.0
end function zf_wave0_r8

!===============================================================================

program tests_hedtools
use m_tests

  ! Oracle f95 requires function to be EXTERNAL, a module procedure
  ! or declared in an  interface block to be an argument.
  interface

    function zf_linear_r4(x) result (y)
      use m_tests
      real(SP), intent(in) :: x
      real(SP) :: y
    end function zf_linear_r4

    function zf_linear_r8(x) result (y)
      use m_tests
      real(DP), intent(in) :: x
      real(DP) :: y
    end function zf_linear_r8

    function zf_qubic_r4(x) result (y)
      use m_tests
      real(SP), intent(in) :: x
      real(SP) :: y
    end function zf_qubic_r4

    function zf_qubic_r8(x) result (y)
      use m_tests
      real(DP), intent(in) :: x
      real(DP) :: y
    end function zf_qubic_r8

    function zf_wave0_r4(x) result (y)
      use m_tests
      real(SP), intent(in) :: x
      real(SP) :: y
    end function zf_wave0_r4

    function zf_wave0_r8(x) result (y)
      use m_tests
      real(DP), intent(in) :: x
      real(DP) :: y
    end function zf_wave0_r8

  end interface

 print *, "*** Tests started ***"
 call test_CSV_IO()
 call test_CSV_IO_low()
 call test_STRINGS()
 call test_BASE_UTILS_zerofun()
 call test_BASE_UTILS_qsort()
 call test_BASE_UTILS_cspline_SP()
 call test_BASE_UTILS_cspline_DP()
 call test_BASE_UTILS_solve_line_SP()
 call test_BASE_UTILS_solve_line_DP()
 call test_BASE_RANDOM_RNORM_DP()
 print *, "*** Tests completed ***"

contains

subroutine test_CSV_IO
  use CSV_IO

  integer :: unum
  logical :: fstat
  integer, parameter :: ROWS=100, COLS=20
  real(SP), dimension(ROWS,COLS) :: DATA_OUT, DATA_IN
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

subroutine test_CSV_IO_low
  use CSV_IO
  use BASE_UTILS, only : TOSTR
  integer :: i, j, num
  ! This c_file defines file handle for CSV
  type(csv_file) :: c_file
  character(255) :: c_record
  character(len=*), parameter :: TESTNAME="test_CSV_IO_low"

  print *, "Test: ", TESTNAME
  num = 0

  ! File handle must at least define the file name
  c_file%name = "test_2.csv"

  call CSV_OPEN_WRITE(c_file) 

  ! Create and write the varable names
  c_record=""
  do i = 1, 5
    call CSV_RECORD_APPEND(c_record, "VAR_" // TOSTR(i))
  end do
  call CSV_RECORD_WRITE(c_record, c_file)

  ! Write rows
  do i =1, 10
    c_record = ""
    ! Write columns within each row
    do j = 1, 5
      ! we output values of num
      num = num + 1
      call CSV_RECORD_APPEND(c_record, num) 
    end do
    call CSV_RECORD_WRITE(c_record, c_file)
  end do

  call CSV_CLOSE(c_file)

end subroutine test_CSV_IO_low

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
  real(SP) :: rnumber
  character(len=*), parameter :: TESTNAME="test_STRINGS", DELIMS=",:;"
  integer :: imatch
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

  ! Test MATCH(), only the first substring is deleted
  !              1234567890123-5-789012345678-0-23456789012345678901234
  test_str_01 = "Test a string {a small string} brackets close at 15-30"
  imatch = MATCH(test_str_01, 15)
  if ( .not. imatch == 30 ) call fail_test("Error in MATCH {}")

  test_str_01 = "Test a string <a small string> brackets close at 15-30"
  imatch = MATCH(test_str_01, 15)
  if ( .not. imatch == 30 ) call fail_test("Error in MATCH <>")

  test_str_01 = "Test a string (a small string) brackets close at 15-30"
  imatch = MATCH(test_str_01, 15)
  if ( .not. imatch == 30 ) call fail_test("Error in MATCH ()")

  test_str_01 = "Test a string [a small string] brackets close at 15-30"
  imatch = MATCH(test_str_01, 15)
  if ( .not. imatch == 30 ) call fail_test("Error in MATCH []")

  imatch = MATCH("Test a string [a small string] with square brackets", 15)
  if ( .not. imatch == 30 ) call fail_test("Error in MATCH [] inline")

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

subroutine test_BASE_UTILS_zerofun()
  use BASE_UTILS

  real(SP) :: Y, A, B, Y0
  real(DP) :: DY, DA, DB, DY0

  real(SP), parameter :: eps_f = 1.0e-12_SP
  real(DP), parameter :: deps_f = 1.0e-22_DP

  real(SP), parameter :: INVALID = -9999.0_SP
  real(DP), parameter :: DINVALID = -9999.0_DP

  character(len=*), parameter :: TESTNAME="test_BASE_UTILS_zerofun"

  print *, "Test: ", TESTNAME

  A = 1.0_SP; B = 2.0_SP; Y0=INVALID ! No zero of f(x)=X+1  within [1,2]
  Y = ZEROFUN(A, B, zf_linear_r4, eps_f)
  if (.not. Y == Y0 ) call fail_test("Error in FUNCZERO 01")

  A = -2.0_SP; B =  1.0_SP; Y0 = -1.0_SP
  Y = ZEROFUN(A, B, zf_linear_r4, eps_f)
  if (.not. Y == Y0 ) call fail_test("Error in FUNCZERO 02")

  A = -2.0_SP; B =  2.0_SP; Y0 = 2.0_SP**(1.0_SP/3.0_SP)
  Y = ZEROFUN(A, B, zf_qubic_r4, eps_f)
  if (.not. abs(Y - Y0) < eps_f ) call fail_test("Error in FUNCZERO 03")

  A = -2.0_SP; B =  3.0_SP; Y0 = 0.0_SP
  Y = ZEROFUN(A, B, zf_wave0_r4, eps_f)
  if (.not. abs(Y - Y0) < eps_f ) call fail_test("Error in FUNCZERO 04")

  DA = 1.0_DP; DB = 2.0_DP; DY0=DINVALID ! No zero of f(x)=X+1  within [1,2]
  DY = ZEROFUN(DA, DB, zf_linear_r8, deps_f)
  if (.not. DY == DY0 ) call fail_test("Error in FUNCZERO 01/DB")

  DA = -2.0_DP; DB =  2.0_DP; DY0 = 2.0_DP**(1.0_DP/3.0_DP)
  DY = ZEROFUN(DA, DB, zf_qubic_r8, deps_f)
  if (.not. abs(DY - DY0) < deps_f ) call fail_test("Error in FUNCZERO 03")

  DA = -2.0_DP; DB =  3.0_DP; DY0 = 0.0_DP
  DY = ZEROFUN(DA, DB, zf_wave0_r8, deps_f)
  if (.not. abs(DY - DY0) < deps_f ) call fail_test("Error in FUNCZERO 04/DB")

end subroutine test_BASE_UTILS_zerofun

subroutine test_BASE_UTILS_qsort

use BASE_UTILS

  integer, dimension(10) :: intarray = [1,4,6,1,0,0,8,3,4,5]
  integer, dimension(10) :: truesort = [0,0,1,1,3,4,4,5,6,8]
  integer, dimension(10) :: truersor = [8,6,5,4,4,3,1,1,0,0]

  real(SP), dimension(10) :: realarray = [1._SP,4.0_SP,6.0_SP,1.0_SP,0.0_SP,0.0_SP,8.0_SP,3.0_SP,4.0_SP,5.0_SP]
  real(SP), dimension(10) :: rtruesort = [0._SP,0.0_SP,1.0_SP,1.0_SP,3.0_SP,4.0_SP,4.0_SP,5.0_SP,6.0_SP,8.0_SP]
  real(SP), dimension(10) :: truerrsor = [8._SP,6.0_SP,5.0_SP,4.0_SP,4.0_SP,3.0_SP,1.0_SP,1.0_SP,0.0_SP,0.0_SP]

  character(len=*), parameter :: TESTNAME="test_BASE_UTILS_qsort"
  print *, "Test: ", TESTNAME

  call ARRAY_QSORT(intarray)
  if ( any(intarray /= truesort) )                                            &
      call fail_test("QSORT failed: integer array")

  call ARRAY_QSORT(intarray, .TRUE.)
  if ( any(intarray /= truersor) )                                            &
      call fail_test("QSORT failed: reverse integer array")

  call ARRAY_QSORT(realarray)
  if ( any(realarray /= rtruesort) )                                          &
      call fail_test("QSORT failed: real SP array")

  call ARRAY_QSORT(realarray, .TRUE.)
  if ( any(realarray /= truerrsor) )                                          &
      call fail_test("QSORT failed: reverse real SP array")

end subroutine test_BASE_UTILS_qsort

subroutine test_BASE_UTILS_cspline_SP

use BASE_UTILS

  real(SP), dimension(4), parameter :: data_x =   [1.0,  2.0,  5.0,  9.0]
  real(SP), dimension(4), parameter :: data_y =   [5.5, 14.5, 25.0, 12.0]
  real(SP), dimension(4), parameter :: interp_x = [1.0,  4.0,  7.0,  9.0]

  real(SP), dimension( size(interp_x) ) :: interp_y

  real(SP), dimension(4), parameter :: interp_y_check =                       &
                            [ 5.50000000, 23.9419632, 21.3303566, 12.0000000 ]

  character(len=*), parameter :: TESTNAME="test_BASE_UTILS_cspline_SP"
  print *, "Test: ", TESTNAME

  call CSPLINE( data_x, data_y, interp_x, interp_y)
  if ( any(interp_y /= interp_y_check) )                                      &
                              call fail_test("CSPLINE Failed: kind SP")

  if ( CSPLINE_SCALAR(data_x, data_y, 4.0_SP) /= interp_y_check(2) )          &
                              call fail_test("CSPLINE SCALAR Failed: kind SP")


end subroutine test_BASE_UTILS_cspline_SP

subroutine test_BASE_UTILS_cspline_DP

use BASE_UTILS

  real(DP), dimension(4), parameter :: data_x =   [1.0_DP,  2.0_DP,  5.0_DP,  9.0_DP]
  real(DP), dimension(4), parameter :: data_y =   [5.5_DP, 14.5_DP, 25.0_DP, 12.0_DP]
  real(DP), dimension(4), parameter :: interp_x = [1.0_DP,  4.0_DP,  7.0_DP,  9.0_DP]

  real(DP), dimension( size(interp_x) ) :: interp_y

  real(DP), dimension(4), parameter :: interp_y_check =                       &
                              [ 5.5000000000000000_DP, 23.941964285714285_DP, &
                                21.330357142857146_DP, 12.000000000000000_DP ]

  character(len=*), parameter :: TESTNAME="test_BASE_UTILS_cspline_DP"
  print *, "Test: ", TESTNAME

  call CSPLINE( data_x, data_y, interp_x, interp_y)
  if ( any(interp_y /= interp_y_check) )                                      &
                                    call fail_test("CSPLINE Failed: kind DP")

  if ( CSPLINE_SCALAR(data_x, data_y, 4.0_DP) /= interp_y_check(2) )          &
                              call fail_test("CSPLINE SCALAR Failed: kind DP")

end subroutine test_BASE_UTILS_cspline_DP

subroutine test_BASE_UTILS_solve_line_SP

use BASE_UTILS

  real(SP) :: k,  b,  x1,  x2,  y1,  y2
  real(SP) :: ks, bs, x1s, x2s, y1s, y2s
  character(len=*), parameter :: TESTNAME="test_BASE_UTILS_solve_line_SP"
  real(SP), parameter :: eps_s = 1.0e-6_SP
  real(DP), parameter :: eps_d = 1.0e-6_DP

  print *, "Test: ", TESTNAME

  k = 2.2_SP
  b = 14.5_SP

  x1 = 1.0_SP
  y1 = k * x1 + b

  x2 = 14.0_SP
  y2 = k * x2 + b

  x1s = x1
  y1s = y1
  
  x2s = x2
  y2s = y2
 
  call SOLVE_LINE(ks, bs, x1s, y1s, x2s, y2s)

  if ( abs (k - ks) > eps_s  ) call fail_test("SOLVE_LINE failed, SP")
  if ( abs (b - bs) > eps_s  ) call fail_test("SOLVE_LINE failed, SP")

end subroutine test_BASE_UTILS_solve_line_SP

subroutine test_BASE_UTILS_solve_line_DP

use BASE_UTILS

  real(DP) :: k,  b,  x1,  x2,  y1,  y2
  real(DP) :: ks, bs, x1s, x2s, y1s, y2s
  character(len=*), parameter :: TESTNAME="test_BASE_UTILS_solve_line_DP"
  real(SP), parameter :: eps_s = 1.0e-6_SP
  real(DP), parameter :: eps_d = 1.0e-6_DP

  print *, "Test: ", TESTNAME

  k = 22.2_DP
  b = 64.5_DP

  x1 = 1.0_DP
  y1 = k * x1 + b

  x2 = 14.0_DP
  y2 = k * x2 + b

  x1s = x1
  y1s = y1
  
  x2s = x2
  y2s = y2
 
  call SOLVE_LINE(ks, bs, x1s, y1s, x2s, y2s)

  if ( abs (k - ks) > eps_d  ) call fail_test("SOLVE_LINE failed, DP")
  if ( abs (b - bs) > eps_d  ) call fail_test("SOLVE_LINE failed, DP")

end subroutine test_BASE_UTILS_solve_line_DP

subroutine test_BASE_RANDOM_RNORM_DP

use BASE_RANDOM

  character(len=*), parameter :: TESTNAME="test_BASE_RANDOM_RNORM_DP"

  real(DP), allocatable, dimension(:) :: X
  real(SP), parameter :: eps_s = 0.5_SP
  real(DP), parameter :: eps_d = 0.5_DP

 integer :: i

  print *, "Test: ", TESTNAME

  allocate(X(100))

  do i=1, size(X)
    X(i) = RNORM()
  end do
  
  ! print *, 'Mean:', sum(X)/size(X)

  ! Check X in R
  ! z <- read.csv('rnorm_output.csv')
  ! head(z)
  ! mean(z$RNORM)
  ! sd(z$RNORM)
  open(100, file="rnorm_output.csv", status="new", action="write")
  
  write(100, *) 'RNORM'

  do i=1, size(X)
    write(100, *) X(i)
  end do
  
  if ( abs (sum(X)/size(X) - 0.0_DP) > eps_d  ) call fail_test("RNORM mean failed, DP")


end subroutine test_BASE_RANDOM_RNORM_DP


end program tests_hedtools
