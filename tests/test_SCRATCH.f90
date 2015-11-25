! Scratch util for little tests, always changing

! for Solaris Studio::
! of95 -g -c ../BASE_CSV_IO.f90 ../BASE_UTILS.f90
! of95 -o zzz test_SCRATCH.f90 ../BASE_UTILS.f90

program TEST_SCRATCH

  use BASE_UTILS

  real :: ZZ = 12.45

  print *, TOSTR(ZZ), ": - real"

  !call STDOUT( "jbdfjhbdfvhjbfvhj" // TOSTR(12) // ":" )

  !call STDERR( "Error message " // TOSTR(ZZ) // ":" )


end program TEST_SCRATCH
