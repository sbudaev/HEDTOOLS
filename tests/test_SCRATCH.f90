! Scratch util for little tests, always changing

! for Solaris Studio::
! of95 -g -c ../BASE_CSV_IO.f90 ../BASE_UTILS.f90
! of95 -o zzz test_SCRATCH.f90 ../BASE_UTILS.f90

program TEST_SCRATCH

  use BASE_UTILS

  real(8) :: ZZ = 12.455565465654654_8
  character (len=120) :: SS
  
  call STDOUT( "Output number=" // TOSTR(12) // ":" // TOSTR(2.4_8, "(f4.2)")  )

  call STDERR( "Error code=" // TOSTR(ZZ) // ":" )
  
  
end program TEST_SCRATCH
