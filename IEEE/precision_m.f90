! Module define_precision provides the kind value needed
! to define the precision of a complete package along
! with definitions of all commonly used precisions
module precision
! Some Fortran compilers don't yet support int32, and int64.
!  use, intrinsic :: iso_fortran_env, only: int32, int64
  use, intrinsic :: iso_c_binding, only: c_double, c_float, c_int
  ! ..
  ! .. Intrinsic Functions ..
  intrinsic kind, selected_real_kind
  private :: kind, selected_real_kind

  ! .. Parameters .. to define the standard precisions
  integer, parameter :: skind = kind(0.0e0)
  integer, parameter :: dkind = kind(0.0d0)
  integer, parameter :: qkind = selected_real_kind(30) ! Quad precision
  integer, parameter :: kind80 = selected_real_kind(18) ! 80 bit reals
! The last 2 above may return a kind of -1 if not supported.
!  integer, parameter :: ikind = int32 ! Integer kind
!  integer, parameter :: ikind64 = int64 ! To use 64 bit integers
  integer, parameter :: cdkind = c_double ! C double_precision
  integer, parameter :: crkind = c_float ! C float
  integer, parameter :: cikind = c_int ! C integer
  integer, parameter :: rk = dkind
!  integer, parameter :: ik = ikind
! To change the default package precision to
! another precision change the parameter assignment
! to rk above to the precsion desired,
! and recompile the complete package.
! ik is not used as the NAG compiler does not like: integer(ik) :: ...
end module precision
