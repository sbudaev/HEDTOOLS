! Scratch util for little tests, always changing

! for Solaris Studio::
! of95 -g -c ../BASE_CSV_IO.f90 ../BASE_UTILS.f90
! of95 -o zzz test_SCRATCH.f90 ../BASE_UTILS.f90

! gfortran -g -c ../BASE_CSV_IO.f90 ../BASE_UTILS.f90
! gfortran -g -o zzz test_SCRATCH.f90 ../BASE_UTILS.f90 && ./zzz


program TEST_SCRATCH

  use BASE_UTILS

  real(8) :: ZZ = 12.455565465654654_8
  character (len=120) :: SS

  real, dimension (10) :: YY = [1.,2.,3.,4.,5.,6.,7.,8.,9.,10.]
  real(8), dimension (10) :: BB = [1._8,2._8,3._8,4._8,5._8,6._8,7._8,8._8,9._8,10._8]
  integer, dimension(10) :: VV = [1,2,3,4,5,6,7,8,9,10]

  print *, "Array=", TOSTR (YY, "(f6.2)"), ":---"
  print *, "Array=", TOSTR (BB(1:3), "(f6.2)"), ":---"
  print *, "Array=", TOSTR (VV(3:6)), ":---"

  print *, (/(i, i=1,10)/)

!  call STDOUT( "Output number=" // TOSTR(12) // ":" // TOSTR(2.4_8, "(f4.2)")  )

!  call STDERR( "Error code=" // TOSTR(ZZ) // ":" )


end program TEST_SCRATCH
