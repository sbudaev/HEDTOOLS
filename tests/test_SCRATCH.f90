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
  character (len=4) :: XXX = "1234"
  integer, dimension(6) :: IARR = [1,2,3,4,5,6]
  real, dimension(6) :: RARR = [0.1,0.2,0.3,0.4,0.5,0.6]
  
  REC=""
  print *, ">>", trim(REC), "<<"
  
  call CSV_RECORD_APPEND (REC, "one")
  print *, ">>", trim(REC), "<<"
  
  call CSV_RECORD_APPEND (REC, "two", "three", "TTT", "ZZZ_1", "MMM", XXX)  
  print *, ">>", trim(REC), "<<"
  
  call CSV_RECORD_APPEND(REC, 1.2, 1.3)
  print *, ">>", trim(REC), "<<"
  
  call CSV_RECORD_APPEND(REC, IARR)
  print *, ">>", trim(REC), "<<"
  
  call CSV_RECORD_APPEND(REC, RARR)
  print *, ">>", trim(REC), "<<"
  
  call CSV_RECORD_APPEND(REC, ["A","B","C"])
  print *, ">>", trim(REC), "<<"
  
  call CSV_RECORD_APPEND(REC, [102.1,102.2,109.])
  print *, ">>", trim(REC), "<<"

end program TEST_SCRATCH
