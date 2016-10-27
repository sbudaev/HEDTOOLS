program INTERPOLATE
use BASE_UTILS
use BASE_RANDOM
use CSV_IO
!use plplot   ! PLPLOT

integer :: i

integer, parameter :: FUNCT_N = 7
real, dimension(FUNCT_N) :: xx = [0.0, 0.2, 0.60, 0.80, 0.900, 1.0, 2.5]
real, dimension(FUNCT_N) :: yy = [1.0, 0.3, 0.04, 0.01, 0.001, 0.0, 0.0]

integer, parameter :: INTERP = 100
real, dimension(INTERP)  :: xi
real, dimension(INTERP)  :: outvecli, outveclg
real, dimension(INTERP)  :: outvecdd

!call RAND_ARRAY(xi, minval(xx), maxval(xx))
call RAND_ARRAY(xi, 0., 1.)

do i=1, 100
  xi(i) = real(i/100.0)
  outvecdd(i) = DDPINTERPOL( xx, yy, xi(i) )
  !print *, xi(i)
end do

outvecli = LIN_INTERPOL_VECTOR( xx, yy, xi )

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! Use PGPLOT to draw plot of data. Use GRAPHLIB=-lpgplot for build,
!     output devices: /XSERVE /XWINDOW /PS
!if (pgopen('/XWINDOW') .lt. 1) stop
!call pgenv(0., 1., 0., 1.,  0,  0)
!call pglab('x', 'y', 'Linear piecewise interpolation')
!call pgline(100, xi, outvecli)
!call pgclos
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
! Use PLPLOT to draw plot of data. Use for build:
! GRAPHLIB=-lplplotf95d -lplplotf95cd -lplf95demolibd -I/usr/lib/fortran/modules/plplot
! NOTE: should use the real(kind=plflt) type, won't work with default reals.
!       Can use real(real_xxx,plflt) intrinsic function for conversion.
!call plparseopts( PL_PARSE_FULL )
!call plinit
!call plenv( 0._plflt, 1._plflt, 0._plflt, 1._plflt, 0, 0 )
!call pllab('x', 'y', 'Linear piecewise interpolation')
!call plline( real(xi,plflt), real(outvecli,plflt) )
!call plend
!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! Use PGPLOT to draw plot of data. Use GRAPHLIB=-lpgplot for build,
!     output devices: /XSERVE /XWINDOW /PS
if (pgopen('/XWINDOW') .lt. 1) stop
call pgenv(0., 1., 0., 1.,  0,  0)
call pglab('x', 'y', 'Nonparameteric, DDP')
call pgline(100, xi, outvecdd)    ! DD interpolation
call pgline(100, xi, outvecli)    ! Linear interpolation
call pgclos

! Print PostScript file
if (pgopen('/PS') .lt. 1) stop
call pgenv(0., 1., 0., 1.,  0,  0)
call pglab('x', 'y', 'Nonparameteric, DDP')
call pgline(100, xi, outvecdd)  ! DD interpolation
call pgline(100, xi, outvecli)  ! Linear interpolation
call pgclos
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

! Plot commands for R, based on csv data output
! zzz <- read.csv("zzz_linear.csv")
! plot(zzz$X, zzz$Y, type="l")
! plot(zzz$X, zzz$Y, type="p")
call CSV_MATRIX_WRITE ( reshape(                             &
                          [ xi,                              &
                            outvecli],                       &
                          [INTERP, 2]),                      &
                          "zzz_linear.csv" ,                 &
                          ["X","Y"  ]                        &
                          )

outveclg =  LAGR_INTERPOL_VECTOR( xx, yy, xi )

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! Use PGPLOT to draw plot of data. Use GRAPHLIB=-lpgplot for build,
!     output devices: /XSERVE /XWINDOW /PS
if (pgopen('/XWINDOW') .lt. 1) stop
call pgenv(0., 1., 0., 1.,  0,  0)
call pglab('x', 'y', 'Nonparametric, Lagrange')
call pgline(100, xi, outveclg)
call pgclos
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

! Plot commands for R, based on csv data output
! zzz <- read.csv("zzz_nonlin_lag.csv")
! plot(zzz$X, zzz$Y, type="l")
call CSV_MATRIX_WRITE ( reshape(                             &
                          [ xi,                              &
                            outveclg],                       &
                          [INTERP, 2]),                      &
                          "zzz_nonlin_lag.csv" ,             &
                          ["X","Y"  ]                        &
                          )

! Plot commands for R, based on csv data output
! zzz <- read.csv("zzz_nonlin_dd.csv")
! plot(zzz$X, zzz$Y, type="l")
call CSV_MATRIX_WRITE ( reshape(                             &
                          [ xi,                              &
                            outvecdd],                       &
                          [INTERP, 2]),                      &
                          "zzz_nonlin_dd.csv" ,              &
                          ["X","Y"  ]                        &
                          )

end program INTERPOLATE
