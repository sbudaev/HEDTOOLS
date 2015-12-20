! Scratch util for little tests, always changing

! for Solaris Studio::
! of95 -g -c ../BASE_CSV_IO.f90 ../BASE_UTILS.f90
! of95 -o zzz test_SCRATCH.f90 ../BASE_UTILS.f90

! gfortran -g -c ../BASE_CSV_IO.f90 ../BASE_UTILS.f90
! gfortran -g -o zzz test_SCRATCH.f90 ../BASE_UTILS.f90 ../BASE_CSV_IO.f90 && ./zzz


program TEST_SCRATCH

  use BASE_UTILS
! use CSV_IO

  type :: out_file
    character (len=255) :: name_file ! CSV file name
    integer :: unit_file                        ! file unit (internal mostly)
    logical :: succeed_file                     ! last access success flag
  end type out_file

  type (out_file) :: zzz

  character (len=255) :: xxx = "R123456789"
  character (len=255) :: yyy

  yyy = "XXX hhh" // xxx
  zzz%name_file= trim(yyy) // "test xxx zzz" // TOSTR(120)

  print *, zzz%name_file


end program TEST_SCRATCH
