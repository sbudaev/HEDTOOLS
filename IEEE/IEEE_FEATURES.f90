MODULE IEEE_FEATURES !gfortran
  ! Timeline 11/21/2012 08:20 AM 
  ! This is a shell module that indicates the opportunities for
  ! support of the IEEE features.  This version for gfortran 
  ! indicates support for all the standard features.
  ! See Draft Fortran 2008 standard, Section 14.2.
  !------------------------------------------------------------------------------
  !The module IEEE_FEATURES defines the type IEEE FEATURES TYPE, 
  !for expressing the need for particular IEC 60559:1989 features.
  !Its only possible values are those of named constants defned in
  !the module:
  !IEEE DATATYPE, IEEE DENORMAL, IEEE DIVIDE, IEEE HALTING, IEEE INEXACT FLAG, IEEE INF,
  !IEEE INVALID FLAG, IEEE NAN, IEEE ROUNDING, IEEE SQRT, and IEEE UNDERFLOW FLAG.
  !------------------------------------------------------------------------------
  TYPE, PUBLIC :: IEEE_FEATURES_TYPE
    ! Althought the standard nameed objects with this type are visible,
    ! the component, SUPPORT, is private.  This suppresses direct access
    ! to that component.
    LOGICAL, PRIVATE :: SUPPORT=.TRUE.
  END TYPE IEEE_FEATURES_TYPE


  TYPE(IEEE_FEATURES_TYPE), SAVE :: &
    IEEE_DATATYPE,&
    IEEE_DENORMAL,&
    IEEE_DIVIDE,&
    IEEE_HALTING,&
    IEEE_INEXACT_FLAG,&
    IEEE_INF,&
    IEEE_INVALID_FLAG,&
    IEEE_NAN,&
    IEEE_ROUNDING,&
    IEEE_SQRT,&
    IEEE_UNDERFLOW_FLAG,&
    IEEE_X87_ACCURACY ! This is not standard.  Added to indicate
  ! access to the accuracy available in the x87.

  ! This provides visibility of the features supported.
  ! For this gfortran version, all the features are supported.
  ! There is no option but to have them all supported.
  PUBLIC &
    IEEE_DATATYPE,&
    IEEE_DENORMAL,&
    IEEE_DIVIDE,&
    IEEE_HALTING,&
    IEEE_INEXACT_FLAG,&
    IEEE_INF,&
    IEEE_INVALID_FLAG,&
    IEEE_NAN,&
    IEEE_ROUNDING,&
    IEEE_SQRT,&
    IEEE_UNDERFLOW_FLAG,&
    IEEE_X87_ACCURACY
END MODULE IEEE_FEATURES ! R. J. Hanson
