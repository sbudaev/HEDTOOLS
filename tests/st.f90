! This is a small program to test the library
! buiild: 
!   $FC st.f90 hedtools.lib
! e.g. with gfortran:
!    gfortran st.f90 -I.. ../hedutils.a -o test
! Note that -I (include) option defines the path to the mod files
!
! Warning: If the program does not find the shared library in the 
! currect directory, set the LD_LIBRARY_PATH to refer to the current
! directory:
!       LD_LIBRARY_PATH=$LD_LIBRARY_PATH:. ./test
!----------------------------------------------------------------------
program test_csv

  use CSV_IO
  use BASE_UTILS, only : TOSTR
  integer :: i, j, num
  ! This c_file defines file handle for CSV
  type(csv_file) :: c_file
  character(255) :: c_record
  character(len=*), parameter :: TESTNAME="test_CSV_IO"

  print *, "Test: ", TESTNAME
  num = 0

  ! File handle must at least define the file name
  c_file%name = "test_2.csv"
  print *, "Output CSV file: ", trim(c_file%name)

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

end program test_csv

