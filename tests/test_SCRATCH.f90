! Scratch util for little tests, always changing

! for Solaris Studio::
! of95 -g -c ../BASE_CSV_IO.f90 ../BASE_UTILS.f90
! of95 -g -o zzz test_SCRATCH.f90 ../BASE_UTILS.f90 ../BASE_CSV_IO.f90 && ./zzz

! gfortran -g -c ../BASE_CSV_IO.f90 ../BASE_UTILS.f90
! gfortran -g -o zzz test_SCRATCH.f90 ../BASE_UTILS.f90 ../BASE_CSV_IO.f90 && ./zzz


program TEST_SCRATCH

  use BASE_UTILS
  use CSV_IO

  character (len=255), dimension(1:10) :: NAMES = ["aav01","aav02","aav03",&
                        "aav04","aav05","aav06","aav07","aav08","aav09","aav10"]
  !real (kind=4), dimension(1:100,1:20 ) :: MATRIX
  real, dimension(1:100,1:300 ) :: MATRIX
  !character (len=255), dimension(1:100,1:35 ) :: MATRIX
  !integer, dimension(1:10) :: ARRAY
  character (len=10), dimension(1:40)  :: NOUT
  logical :: flag = .TRUE.

!  call CSV_COLNAMES(NAMES,NOUT)

!  print *, NAMES
!  print *, NOUT

  MATRIX=121.23456
  !MATRIX="AAA BBB CCC DDD"
  !ARRAY=149

  NAMES=[("a" // TOSTR(i,10),i=1,10)]

  call CSV_MATRIX_WRITE(matrix=MATRIX, colnames=NAMES, csv_file_name="zzz1.csv", csv_file_status=flag)
  print *, flag

  call CSV_MATRIX_WRITE(matrix=MATRIX, csv_file_name="zzz2.csv", csv_file_status=flag)
  print *, flag



end program TEST_SCRATCH
