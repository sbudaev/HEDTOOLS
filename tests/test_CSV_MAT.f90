program RANDOM_MATRICES
! Test CSV_IO matrix output by producing many random matrices
! with different ranges

! gfortran -g -c ../BASE_CSV_IO.f90 ../BASE_UTILS.f90
! gfortran -g -o ZZZ test_CSV_MAT.f90 ../BASE_CSV_IO.f90 ../BASE_UTILS.f90

use, intrinsic :: ISO_FORTRAN_ENV
use CSV_IO
use BASE_UTILS

implicit none

integer :: N_MAT = 100                          ! N of random matrices
integer :: i, j, m, M_ROWS, M_COLS              ! Rows/cols
real, dimension (:,:), allocatable :: MATRIX    ! Matrix itself, random

real :: M_LOWER, M_UPPER                  ! Upper and lower limits of Random

character (len=:), allocatable :: Fileout
logical :: f_status = .TRUE.

call RANDOM_SEED_INIT()

do m=0, N_MAT-1     ! Loop for matrices

  M_LOWER=m*0.0001  !  Lower value for range
  M_UPPER=m*1000.   !  Upper value for range

  M_ROWS=m*100+4
  M_COLS=m*4+4

  allocate(MATRIX(M_ROWS,M_COLS))

  write(OUTPUT_UNIT, *) "Generating matrix:", m, ", size ", M_ROWS, "x", M_COLS
  write(OUTPUT_UNIT, *) "            Range:", M_LOWER, ":", M_UPPER
  write(OUTPUT_UNIT, *) "--"

  do i=1, M_ROWS
    do J=1, M_COLS
      call random_number( MATRIX(i,j) )
      MATRIX(i,j) = M_LOWER + MATRIX(i,j) * (M_UPPER - M_LOWER)
    end do
  end do

  ! Save matrices to CSV
  Fileout="out_file_" // TOSTR(m) // ".csv"
  call CSV_MATRIX_WRITE( matrix=MATRIX, &
                         csv_file_name=Fileout, &
                         csv_file_status=f_status )
  write(OUTPUT_UNIT, *) "Writing file: ", Fileout

  if(.NOT. f_status) write(ERROR_UNIT,*) "Produced file writing error, ", f_status

  deallocate(MATRIX)

end do

write(OUTPUT_UNIT, *) "All done"

end program RANDOM_MATRICES
