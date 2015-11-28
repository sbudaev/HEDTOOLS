! Scratch util for little tests, always changing

! for Solaris Studio::
! of95 -g -c ../BASE_CSV_IO.f90 ../BASE_UTILS.f90
! of95 -o zzz test_SCRATCH.f90 ../BASE_UTILS.f90

! gfortran -g -c ../BASE_CSV_IO.f90 ../BASE_UTILS.f90
! gfortran -g -o zzz test_SCRATCH.f90 ../BASE_UTILS.f90 ../BASE_CSV_IO.f90 && ./zzz


program TEST_SCRATCH

!  use BASE_UTILS
!  use CSV_IO

  real(8) :: ZZ = 12.455565465654654_8
  character (len=120) :: SS

  real, dimension (10) :: YY = [1.,2.,3.,4.,5.,6.,7.,8.,9.,10.]
  real(8), dimension (10) :: BB = [1._8,2._8,3._8,4._8,5._8,6._8,7._8,8._8,9._8,10._8]
  integer, dimension(10) :: VV = [1,2,3,4,5,6,7,8,9,10]

  integer :: CSV_VV = 1
  integer :: CSV_UNIT, o

  real(kind=8) :: BBB= 3.0_8 !14159265553434786348

  character (len=:), allocatable :: RECORD_CSV
  !character (len=255) :: RECORD_CSV

  !allocate (character (len=255) :: RECORD_CSV  )
  !RECORD_CSV=repeat(" ", 100)

  !RECORD_CSV=""

  !print *,">>>>", RECORD_CSV,"<<<<", len(RECORD_CSV), len_trim(RECORD_CSV)

  print *, ">>>", char(9), "<<<"


  print *, BBB, real(int(BBB, kind=8))
  if (  BBB == real(int(BBB, kind=8))  ) print *, "YES"


  stop

!  !print *, (maxval(VV))

!  stop

!  RECORD_CSV=repeat(" ", 5 * (i4_width(5)+1) )

!  print *,">>>>", RECORD_CSV,"<<<<", len(RECORD_CSV), len_trim(RECORD_CSV)

!!  print *, "Array=", TOSTR (YY, "(f6.2)"), ":---"
!!  print *, "Array=", TOSTR (BB(1:3), "(f6.2)"), ":---"
!!  print *, "Array=", TOSTR (VV(3:6)), ":---"

!!  print *, (/(i, i=1,10)/)

!  call CSV_FILE_OPEN_WRITE ("zzz_csv_int.csv", CSV_UNIT)

!  !RECORD_CSV=" "

!  call CSV_RECORD_APPEND( RECORD_CSV, 1 )
!  call CSV_RECORD_APPEND( RECORD_CSV, 2 )
!  call CSV_RECORD_APPEND( RECORD_CSV, 3 )
!  call CSV_RECORD_APPEND( RECORD_CSV, 5 )

!  call CSV_FILE_RECORD_WRITE (csv_file_unit=CSV_UNIT, record=RECORD_CSV)

!  call CSV_FILE_CLOSE(csv_file_unit=CSV_UNIT)



end program TEST_SCRATCH
