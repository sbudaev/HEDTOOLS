! Scratch util for little tests, always changing

! for Solaris Studio::
! of95 -g -c ../BASE_CSV_IO.f90 ../BASE_UTILS.f90
! of95 -g -o zzz test_SCRATCH.f90 ../BASE_UTILS.f90 ../BASE_CSV_IO.f90 && ./zzz

! gfortran -g -c ../BASE_CSV_IO.f90 ../BASE_UTILS.f90
! gfortran -g -o zzz test_SCRATCH.f90 ../BASE_UTILS.f90 ../BASE_CSV_IO.f90 && ./zzz


program TEST_SCRATCH

  use BASE_UTILS
  use CSV_IO

  character (len=255) :: REC
  integer :: i
  real, dimension(6) :: RARR = [0.1,0.2,0.3,0.4,0.5,0.6]

  REC=""
  print *, ">>", trim(REC), "<<", (RARR(i),i=1,4)

  i=5
  call CSV_RECORD_APPEND( REC, (/(RARR(i), i=1,6)/) )
  print *, ">>", trim(REC), "<<"

  call CSV_RECORD_APPEND( REC, RARR(1:4) )
  print *, ">>", trim(REC), "<<"

end program TEST_SCRATCH
