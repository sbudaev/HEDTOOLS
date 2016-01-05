! Scratch util for little tests, always changing

! for Solaris Studio::
! of95 -g -c ../BASE_CSV_IO.f90 ../BASE_UTILS.f90
! of95 -g -o zzz test_SCRATCH.f90 ../BASE_UTILS.f90 ../BASE_CSV_IO.f90 && ./zzz

! gfortran -g -c ../BASE_CSV_IO.f90 ../BASE_UTILS.f90
! gfortran -g -o zzz test_SCRATCH.f90 ../BASE_UTILS.f90 ../BASE_CSV_IO.f90 && ./zzz


program TEST_SCRATCH

  use BASE_UTILS
  use CSV_IO

  type (csv_file) :: zzz

  character (len=:), allocatable :: aaa
  character (len=:), allocatable :: bbb

  real, dimension (10) :: MATRX=3.1415926
  integer, dimension (10) :: IMAT=300

  aaa = "file_name_001"
  bbb = "_"
  zzz%name= bbb // aaa // bbb // TOSTR(120) // ".csv"

  print *, trim(zzz%name), ":"
  print *, trim(zzz%name) // "_GGG"

  call CSV_MATRIX_WRITE(MATRX, zzz%name)

  zzz%unit=24

  VALID: if (CHECK_UNIT_VALID(zzz%unit)) then
    print *, "Unit valid"
  end if VALID

  print *, ">>", TOSTR(MATRX), "<<"
  print *, ">>", TOSTR(MATRX, "(f4.2)"), "<<"
  print *, ">>", TOSTR(IMAT), "<<"
  print *, ">>", TOSTR(IMAT, "(I4)"), "<<"

end program TEST_SCRATCH
