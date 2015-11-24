!*******************************************************************************
! PURPOSE:
! Fortran Character String Utilities
! A collection of string manipulation routines is contained in the module
! ‘strings’ found in the file stringmod.f90. To obtain this module as well as
! some other string utilities, go to the website
!     http://www.gbenthien.net/strings/index.html.
! To use the routines in the module ‘strings’ the user needs to add the
! statement
!     Use strings
! to the top of the program. These routines were developed primarily to aid
! in the reading and manipulation of input data from an ASCII text file.
!*******************************************************************************

module precision

! Real kinds

integer, parameter :: kr4=selected_real_kind(6,37)   ! single precision real
integer, parameter :: kr8=selected_real_kind(15,307) ! double precision real

! Integer kinds

integer, parameter :: ki4=selected_int_kind(9)       ! single precision integer
integer, parameter :: ki8=selected_int_kind(18)      ! double precision integer

!Complex kinds

integer, parameter :: kc4 = kr4                      ! single precision complex
integer, parameter :: kc8 = kr8                      ! double precision complex

end module precision
