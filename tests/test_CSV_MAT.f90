program RANDOM_MATRICES
! Test CSV_IO matrix output by producing many random matrices
! with different ranges

! gfortran -g -c ../BASE_CSV_IO.f90 ../BASE_UTILS.f90 ../BASE_ERRORS.f90
! gfortran -g -o ZZZ test_CSV_MAT.f90 ../BASE_CSV_IO.f90 ../BASE_UTILS.f90 ../BASE_ERRORS.f90

use, intrinsic :: ISO_FORTRAN_ENV
use CSV_IO
use BASE_UTILS
use ERRORS

implicit none

integer :: N_MAT = 10                           ! N of random matrices
integer :: i, j, m, M_ROWS, M_COLS              ! Rows/cols
real, dimension (:,:), allocatable :: MATRIX    ! Matrix itself, random

real :: M_LOWER, M_UPPER                  ! Upper and lower limits of Random

character (len=:), allocatable :: Fileout
logical :: f_status = .TRUE.

! For error message handler
type(error_type) :: handle_err  ! error handler from module ERRORS
character(len=256) :: error_msg_string
character (len=*), parameter :: MODNAME = "RANDOM_MATRICES"
character (len=*), parameter :: PROCNAME = "RANDOM_MATRICES"
! ---

call RANDOM_SEED_INIT()

LOOP_MAT: do m=0, N_MAT-1     ! Loop for matrices

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
  Fileout="/out_file_" // TOSTR(m) // ".csv"
  call CSV_MATRIX_WRITE( matrix=MATRIX, &
                         csv_file_name=Fileout, &
                         csv_file_status=f_status )
  write(OUTPUT_UNIT, *) "Writing file: ", Fileout

  !if(.NOT. f_status) write(ERROR_UNIT,*) "Produced file writing error, ", f_status
  if(.NOT. f_status) then
    call ERR_HANDLE ( err=handle_err, err_code=2, &
                      called_from = PROCNAME //" in module "// MODNAME, &
                      custom_1="LOOP_MAT", file_name= Fileout )
    !exit LOOP_MAT
  end if

  deallocate(MATRIX)

end do LOOP_MAT

! Emulate error
call ERR_HANDLE ( err=handle_err, err_code=1000, custom_1="emulated error", called_from = PROCNAME //" in "// MODNAME )

! Output error messages
if( ERR_MSG_PRESENT(handle_err) ) then
  call ERR_GET_MSGS(handle_err, error_msg_string)
  print *, trim(error_msg_string), ":"
end if

write(OUTPUT_UNIT, *) "All done"

call ERR_RESET(handle_err)

if( ERR_MSG_PRESENT(handle_err) ) then
  call ERR_GET_MSGS(handle_err, error_msg_string)
  print *, trim(error_msg_string), ":"
end if

end program RANDOM_MATRICES
