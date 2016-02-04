! Scratch util for little tests, always changing

! for Solaris Studio::
! of95 -g -c ../BASE_CSV_IO.f90 ../BASE_UTILS.f90
! of95 -g -o zzz test_SCRATCH.f90 ../BASE_UTILS.f90 ../BASE_CSV_IO.f90 && ./zzz

! gfortran -g -c ../BASE_CSV_IO.f90 ../BASE_UTILS.f90
! gfortran -g -o zzz test_SCRATCH.f90 ../BASE_UTILS.f90 ../BASE_CSV_IO.f90 && ./zzz


program TEST_SCRATCH

  use BASE_UTILS
  use CSV_IO

  character (len=25500) :: REC
  integer :: i
  real, dimension(6) :: RARR = [0.1,0.2,0.3,0.4,0.5,0.6]
  character (len=4), dimension(6) :: STARR=["a1","a2","a3","a4","a5","a6"]

  REC=""
  print *, ">>", trim(REC), "<<", (RARR(i),i=1,4)

  i=5
  ! using old Fortran arbitrary array constructor
  call CSV_RECORD_APPEND( REC, (/(RARR(i), i=1,6)/) )  
  print *, ">>", trim(REC), "<<"

  REC=""
  ! Using array slices
  call CSV_RECORD_APPEND( REC, RARR(1:4) )
  print *, ">>", trim(REC), "<<"
  
  REC=""
  ! using new fortran array constructor
  call CSV_RECORD_APPEND( REC, [(RARR(i), i=1,4), 200.3, 14.077 ] )
  print *, ">>", trim(REC), "<<"
  
  REC=""
  ! using new fortran array constructor
  call CSV_RECORD_APPEND( REC, [(i,i=1,10)] )
  print *, ">>", trim(REC), "<<"
  
  REC=""
  ! using old fortran array constructor
  call CSV_RECORD_APPEND( REC, (/(i,i=100,1000,100)/) )
  print *, ">>", trim(REC), "<<"
  
  REC=""
  ! using string array
  call CSV_RECORD_APPEND( REC, STARR )
  print *, ">>", trim(REC), "<<"
    
  REC=""
  ! using fortran array constructor to create strings
  ! Note that implicit constructor sets the constructed string length 
  ! at first invocation. So, set maximum length first (7 rather than 5)
  call CSV_RECORD_APPEND( REC, [ "VAR_001", ("VAR_" // TOSTR(i),i=2,100) ] )
  print *, ">>", trim(REC), "<<"
  

end program TEST_SCRATCH
