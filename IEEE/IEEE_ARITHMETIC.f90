MODULE IEEE_ARITHMETIC
  USE IEEE_EXCEPTIONS
  IMPLICIT NONE
  ! Timeline 8/4/2013 9:00 AM
  ! See Draft Fortran 2008 standard, Section 14.2.
  !------------------------------------------------------------------------------
  ! The type IEEE_CLASS_TYPE, for identifying a class of 
  ! foating-point values. Its only possible values are
  ! those of named constants defined in the module: IEEE_SIGNALING_NAN,
  ! IEEE_QUIET_NAN, IEEE_NEGATIVE_INF, IEEE_NEGATIVE_NORMAL,
  ! IEEE_NEGATIVE_DENORMAL, IEEE_NEGATIVE_ZERO, IEEE_POSITIVE_ZERO,
  ! IEEE_POSITIVE_DENORMAL, IEEE_POSITIVE_NORMAL, IEEE_POSITIVE_INF,
  ! and IEEE_OTHER_VALUE.
  !
  ! The type IEEE_ROUND_TYPE, for identifying a particular rounding mode.
  ! Its only possible values are those of named constants defined in the module:
  ! IEEE_NEAREST, IEEE_TO_ZERO, IEEE_UP, and IEEE_DOWN for the IEEE modes,
  ! and IEEE_OTHER for any other mode.
  !
  ! The operator == for two values of one of these types to return
  ! .TRUE. if the values are the same and .FALSE. otherwise.
  ! The operator /= for two values of one of these types to return
  ! .TRUE. if the values differ and .FALSE. otherwise.
  !------------------------------------------------------------------------------
  PRIVATE
  ! Logical parameter value used for .TRUE.
  LOGICAL, PARAMETER :: TRUE=.TRUE.

  !Tables of class type examples.
  INTEGER, PARAMETER :: ISCLASS(10) = &
    [&
    Z'7F800000',& ! becomes IEEE_NEGATIVE_INF
    Z'3F800000',& ! becomes IEEE_NEGATIVE_NORMAL, -1.E0
    Z'00400000',& ! becomes IEEE_NEGATIVE_DENORMAL, -tiny(1.E0)/2
    Z'00000000',& ! becomes IEEE_NEGATIVE_ZERO, -0.E0
    Z'00000000',& ! IEEE_POSITIVE_ZERO,  0.E0
    Z'00400000',& ! IEEE_POSITIVE_DENORMAL, tiny(1.E0)/2
    Z'3F800000',& ! IEEE_POSITIVE_NORMAL,  1.E0 
    Z'7F800000',& ! IEEE_POSITIVE_INF
    Z'7FC00000',& ! IEEE_QUIET_NAN
    Z'7F800001' & ! IEEE_SIGNALING_NAN
    ]
  INTEGER, PARAMETER :: IDCLASS(2*10) = &
    [&
    Z'00000000',Z'7FF00000',& ! becomes IEEE_NEGATIVE_INF
    Z'00000000',Z'3FF00000',& ! becomes IEEE_NEGATIVE_NORMAL, -1.D0
    Z'00000000',Z'00080000',& ! becomes IEEE_NEGATIVE_DENORMAL, -tiny(1.D0)/2
    Z'00000000',Z'00000000',& ! becomes IEEE_NEGATIVE_ZERO, -0.D0
    Z'00000000',Z'00000000',& ! IEEE_POSITIVE_ZERO,  0.D0
    Z'00000000',Z'00080000',& ! IEEE_POSITIVE_DENORMAL, tiny(1.D0)/2
    Z'00000000',Z'3FF00000',& ! IEEE_POSITIVE_NORMAL,  1.D0 
    Z'00000000',Z'7FF00000',& ! IEEE_POSITIVE_INF
    Z'00000000',Z'7FF80000',& ! IEEE_QUIET_NAN
    Z'00000001',Z'7FF00000' & ! IEEE_SIGNALING_NAN
    ]
  ! Integer pointer to little, big end after transfer of double
  ! floating point value to a pair of integers.
  INTEGER, PARAMETER :: IEND=2, IBEG=1
  TYPE, PUBLIC :: IEEE_CLASS_TYPE
    INTEGER, PRIVATE :: TYPE
  END TYPE IEEE_CLASS_TYPE
  ! The values assigned here are an enumeration that is used
  ! to identify the type of the formats for IEEE floating
  ! point representations.
  TYPE(IEEE_CLASS_TYPE), PUBLIC :: &
    IEEE_NEGATIVE_INF     =IEEE_CLASS_TYPE(1),&
    IEEE_NEGATIVE_NORMAL  =IEEE_CLASS_TYPE(2),&
    IEEE_NEGATIVE_DENORMAL=IEEE_CLASS_TYPE(3),&
    IEEE_NEGATIVE_ZERO    =IEEE_CLASS_TYPE(4),&
    IEEE_POSITIVE_ZERO    =IEEE_CLASS_TYPE(5),&
    IEEE_POSITIVE_DENORMAL=IEEE_CLASS_TYPE(6),&
    IEEE_POSITIVE_NORMAL  =IEEE_CLASS_TYPE(7),&
    IEEE_POSITIVE_INF     =IEEE_CLASS_TYPE(8),&
    IEEE_QUIET_NAN        =IEEE_CLASS_TYPE(9),&
    IEEE_SIGNALING_NAN    =IEEE_CLASS_TYPE(10),&
    IEEE_OTHER_VALUE      =IEEE_CLASS_TYPE(11)

  TYPE, PUBLIC :: IEEE_ROUND_TYPE
    INTEGER, PRIVATE :: MODE=0
  END TYPE IEEE_ROUND_TYPE
  ! The values assigned here, in the low order two bits of the component, are
  ! the settings in the MMXCSR and x87 CW registers for those rounding types.
  TYPE(IEEE_ROUND_TYPE), PUBLIC :: &
    IEEE_NEAREST=IEEE_ROUND_TYPE(0),&
    IEEE_TO_ZERO=IEEE_ROUND_TYPE(3),&
    IEEE_UP     =IEEE_ROUND_TYPE(2),&
    IEEE_DOWN   =IEEE_ROUND_TYPE(1),&
    IEEE_OTHER  =IEEE_ROUND_TYPE(0)

  TYPE, PUBLIC :: IEEE_X87_PRECISION_TYPE
    INTEGER, PRIVATE :: MODE=0
  END TYPE IEEE_X87_PRECISION_TYPE
  ! These values are for setting the precision of the x87 arithmetic.
  ! They are defined and used only for the x87 and not the SSE.
  TYPE(IEEE_X87_PRECISION_TYPE), PUBLIC :: &
    IEEE_SINGLE=IEEE_X87_PRECISION_TYPE(0),&
    IEEE_UNUSED=IEEE_X87_PRECISION_TYPE(1),&
    IEEE_DOUBLE=IEEE_X87_PRECISION_TYPE(2),&
    IEEE_DOUBLE_EXTENDED=IEEE_X87_PRECISION_TYPE(3)
  PUBLIC :: IEEE_GET_X87_PRECISION_MODE,&
    IEEE_SET_X87_PRECISION_MODE


  PUBLIC :: IEEE_SUPPORT_UNDERFLOW_CONTROL,&
    IEEE_SUPPORT_STANDARD,&
    IEEE_SUPPORT_SQRT,&
    IEEE_SUPPORT_ROUNDING,&
    IEEE_SUPPORT_NAN,&
    IEEE_SUPPORT_INF,&
    IEEE_SUPPORT_DIVIDE,&
    IEEE_SUPPORT_DENORMAL,&
    IEEE_SUPPORT_IO,&
    IEEE_SUPPORT_DATATYPE

  PUBLIC :: IEEE_GET_UNDERFLOW_MODE,&
    IEEE_SET_UNDERFLOW_MODE,&
    IEEE_GET_ROUNDING_MODE,&
    IEEE_SET_ROUNDING_MODE,&
    IEEE_IS_NAN, &
    IEEE_IS_FINITE,&
    IEEE_IS_NEGATIVE,&
    IEEE_IS_NORMAL,&
    IEEE_COPY_SIGN,&
    IEEE_CLASS,&
    IEEE_VALUE,&
    IEEE_UNORDERED,&
    IEEE_LOGB,&
    IEEE_SCALB,&
    IEEE_RINT,&
    IEEE_REM,&
    IEEE_NEXT_AFTER,&
    IEEE_SELECTED_REAL_KIND,&
    IEEE_FLAG_TYPE,&
    IEEE_INVALID,&
    IEEE_OVERFLOW,&
    IEEE_DIVIDE_BY_ZERO,&
    IEEE_UNDERFLOW,&
    IEEE_INEXACT,&
    IEEE_GET_FLAG,&
    IEEE_SET_FLAG,&
    IEEE_GET_HALTING_MODE,&
    IEEE_SET_HALTING_MODE,&
    OPERATOR(==),&
    OPERATOR(/=)
  ! Define the comparison operations ==, /= for the
  ! CLASS, ROUNDING and X87_PRECISION types.  Outputs
  ! are a LOGICAL with values .TRUE. and .FALSE.
  INTERFACE OPERATOR(==)
    MODULE PROCEDURE CLASS_TYPE_EQUAL, &
      ROUNDING_MODE_EQUAL,&
      X87_PRECISION_MODE_EQUAL
  END INTERFACE OPERATOR(==)

  INTERFACE OPERATOR(/=)
    MODULE PROCEDURE CLASS_TYPE_NOT_EQUAL, &
      ROUNDING_MODE_NOT_EQUAL,&
      X87_PRECISION_MODE_NOT_EQUAL
  END INTERFACE OPERATOR(/=)

  ! Define the generic interfaces for the 
  ! functions that classify types.
  INTERFACE IEEE_NEXT_AFTER
    MODULE PROCEDURE S_NEXT_AFTER, D_NEXT_AFTER
  END INTERFACE IEEE_NEXT_AFTER

  INTERFACE IEEE_REM
    MODULE PROCEDURE SS_REM, SD_REM, DS_REM, DD_REM
  END INTERFACE IEEE_REM

  INTERFACE IEEE_RINT
    MODULE PROCEDURE S_RINT, D_RINT
  END INTERFACE IEEE_RINT

  INTERFACE IEEE_SCALB
    MODULE PROCEDURE S_SCALB, D_SCALB
  END INTERFACE IEEE_SCALB

  INTERFACE IEEE_LOGB
    MODULE PROCEDURE S_LOGB, D_LOGB
  END INTERFACE IEEE_LOGB

  INTERFACE IEEE_UNORDERED
    MODULE PROCEDURE S_UNORDERED, D_UNORDERED
  END INTERFACE IEEE_UNORDERED

  INTERFACE IEEE_VALUE
    MODULE PROCEDURE S_VALUE, D_VALUE
  END INTERFACE IEEE_VALUE

  INTERFACE IEEE_CLASS
    MODULE PROCEDURE S_CLASS, D_CLASS
  END INTERFACE IEEE_CLASS

  INTERFACE IEEE_COPY_SIGN
    MODULE PROCEDURE S_COPY_SIGN, D_COPY_SIGN
  END INTERFACE IEEE_COPY_SIGN

  INTERFACE IEEE_IS_NORMAL
    MODULE PROCEDURE S_IS_NORMAL, D_IS_NORMAL
  END INTERFACE IEEE_IS_NORMAL

  INTERFACE IEEE_IS_NEGATIVE
    MODULE PROCEDURE S_IS_NEGATIVE, D_IS_NEGATIVE
  END INTERFACE IEEE_IS_NEGATIVE

  INTERFACE IEEE_IS_FINITE
    MODULE PROCEDURE S_IS_FINITE, D_IS_FINITE
  END INTERFACE IEEE_IS_FINITE

  INTERFACE IEEE_IS_NAN
    MODULE PROCEDURE S_IS_NAN, D_IS_NAN
  END INTERFACE IEEE_IS_NAN
  ! Define the generic interfaces for the support
  ! inquiry functions required by the standard.
  INTERFACE IEEE_SUPPORT_DATATYPE
    MODULE PROCEDURE &
      N_IEEE_SUPPORT_DATATYPE,&
      S_IEEE_SUPPORT_DATATYPE,&
      S1_IEEE_SUPPORT_DATATYPE,&
      S2_IEEE_SUPPORT_DATATYPE,&
      S3_IEEE_SUPPORT_DATATYPE,&
      S4_IEEE_SUPPORT_DATATYPE,&
      S5_IEEE_SUPPORT_DATATYPE,&
      S6_IEEE_SUPPORT_DATATYPE,&
      S7_IEEE_SUPPORT_DATATYPE,&
      D_IEEE_SUPPORT_DATATYPE, &
      D1_IEEE_SUPPORT_DATATYPE,&
      D2_IEEE_SUPPORT_DATATYPE,&
      D3_IEEE_SUPPORT_DATATYPE,&
      D4_IEEE_SUPPORT_DATATYPE,&
      D5_IEEE_SUPPORT_DATATYPE,&
      D6_IEEE_SUPPORT_DATATYPE,&
      D7_IEEE_SUPPORT_DATATYPE
  END INTERFACE IEEE_SUPPORT_DATATYPE

  INTERFACE IEEE_SUPPORT_IO
    MODULE PROCEDURE &
      N_IEEE_SUPPORT_IO,&
      S_IEEE_SUPPORT_IO,&
      S1_IEEE_SUPPORT_IO,&
      S2_IEEE_SUPPORT_IO,&
      S3_IEEE_SUPPORT_IO,&
      S4_IEEE_SUPPORT_IO,&
      S5_IEEE_SUPPORT_IO,&
      S6_IEEE_SUPPORT_IO,&
      S7_IEEE_SUPPORT_IO,&
      D_IEEE_SUPPORT_IO, &
      D1_IEEE_SUPPORT_IO,&
      D2_IEEE_SUPPORT_IO,&
      D3_IEEE_SUPPORT_IO,&
      D4_IEEE_SUPPORT_IO,&
      D5_IEEE_SUPPORT_IO,&
      D6_IEEE_SUPPORT_IO,&
      D7_IEEE_SUPPORT_IO
  END INTERFACE IEEE_SUPPORT_IO

  INTERFACE IEEE_SUPPORT_DENORMAL
    MODULE PROCEDURE &
      N_IEEE_SUPPORT_DENORMAL,&
      S_IEEE_SUPPORT_DENORMAL,&
      S1_IEEE_SUPPORT_DENORMAL,&
      S2_IEEE_SUPPORT_DENORMAL,&
      S3_IEEE_SUPPORT_DENORMAL,&
      S4_IEEE_SUPPORT_DENORMAL,&
      S5_IEEE_SUPPORT_DENORMAL,&
      S6_IEEE_SUPPORT_DENORMAL,&
      S7_IEEE_SUPPORT_DENORMAL,&
      D_IEEE_SUPPORT_DENORMAL, &
      D1_IEEE_SUPPORT_DENORMAL,&
      D2_IEEE_SUPPORT_DENORMAL,&
      D3_IEEE_SUPPORT_DENORMAL,&
      D4_IEEE_SUPPORT_DENORMAL,&
      D5_IEEE_SUPPORT_DENORMAL,&
      D6_IEEE_SUPPORT_DENORMAL,&
      D7_IEEE_SUPPORT_DENORMAL
  END INTERFACE IEEE_SUPPORT_DENORMAL

  INTERFACE IEEE_SUPPORT_DIVIDE
    MODULE PROCEDURE &
      N_IEEE_SUPPORT_DIVIDE,&
      S_IEEE_SUPPORT_DIVIDE,&
      S1_IEEE_SUPPORT_DIVIDE,&
      S2_IEEE_SUPPORT_DIVIDE,&
      S3_IEEE_SUPPORT_DIVIDE,&
      S4_IEEE_SUPPORT_DIVIDE,&
      S5_IEEE_SUPPORT_DIVIDE,&
      S6_IEEE_SUPPORT_DIVIDE,&
      S7_IEEE_SUPPORT_DIVIDE,&
      D_IEEE_SUPPORT_DIVIDE, &
      D1_IEEE_SUPPORT_DIVIDE,&
      D2_IEEE_SUPPORT_DIVIDE,&
      D3_IEEE_SUPPORT_DIVIDE,&
      D4_IEEE_SUPPORT_DIVIDE,&
      D5_IEEE_SUPPORT_DIVIDE,&
      D6_IEEE_SUPPORT_DIVIDE,&
      D7_IEEE_SUPPORT_DIVIDE
  END INTERFACE IEEE_SUPPORT_DIVIDE

  INTERFACE IEEE_SUPPORT_INF
    MODULE PROCEDURE &
      N_IEEE_SUPPORT_INF,&
      S_IEEE_SUPPORT_INF,&
      S1_IEEE_SUPPORT_INF,&
      S2_IEEE_SUPPORT_INF,&
      S3_IEEE_SUPPORT_INF,&
      S4_IEEE_SUPPORT_INF,&
      S5_IEEE_SUPPORT_INF,&
      S6_IEEE_SUPPORT_INF,&
      S7_IEEE_SUPPORT_INF,&
      D_IEEE_SUPPORT_INF, &
      D1_IEEE_SUPPORT_INF,&
      D2_IEEE_SUPPORT_INF,&
      D3_IEEE_SUPPORT_INF,&
      D4_IEEE_SUPPORT_INF,&
      D5_IEEE_SUPPORT_INF,&
      D6_IEEE_SUPPORT_INF,&
      D7_IEEE_SUPPORT_INF
  END INTERFACE IEEE_SUPPORT_INF

  INTERFACE IEEE_SUPPORT_NAN
    MODULE PROCEDURE &
      N_IEEE_SUPPORT_NAN,&
      S_IEEE_SUPPORT_NAN,&
      S1_IEEE_SUPPORT_NAN,&
      S2_IEEE_SUPPORT_NAN,&
      S3_IEEE_SUPPORT_NAN,&
      S4_IEEE_SUPPORT_NAN,&
      S5_IEEE_SUPPORT_NAN,&
      S6_IEEE_SUPPORT_NAN,&
      S7_IEEE_SUPPORT_NAN,&
      D_IEEE_SUPPORT_NAN, &
      D1_IEEE_SUPPORT_NAN,&
      D2_IEEE_SUPPORT_NAN,&
      D3_IEEE_SUPPORT_NAN,&
      D4_IEEE_SUPPORT_NAN,&
      D5_IEEE_SUPPORT_NAN,&
      D6_IEEE_SUPPORT_NAN,&
      D7_IEEE_SUPPORT_NAN
  END INTERFACE IEEE_SUPPORT_NAN

  INTERFACE IEEE_SUPPORT_ROUNDING
    MODULE PROCEDURE &
      N_IEEE_SUPPORT_ROUNDING,&
      S_IEEE_SUPPORT_ROUNDING,&
      S1_IEEE_SUPPORT_ROUNDING,&
      S2_IEEE_SUPPORT_ROUNDING,&
      S3_IEEE_SUPPORT_ROUNDING,&
      S4_IEEE_SUPPORT_ROUNDING,&
      S5_IEEE_SUPPORT_ROUNDING,&
      S6_IEEE_SUPPORT_ROUNDING,&
      S7_IEEE_SUPPORT_ROUNDING,&
      D_IEEE_SUPPORT_ROUNDING, &
      D1_IEEE_SUPPORT_ROUNDING,&
      D2_IEEE_SUPPORT_ROUNDING,&
      D3_IEEE_SUPPORT_ROUNDING,&
      D4_IEEE_SUPPORT_ROUNDING,&
      D5_IEEE_SUPPORT_ROUNDING,&
      D6_IEEE_SUPPORT_ROUNDING,&
      D7_IEEE_SUPPORT_ROUNDING
  END INTERFACE IEEE_SUPPORT_ROUNDING

  INTERFACE IEEE_SUPPORT_SQRT
    MODULE PROCEDURE &
      N_IEEE_SUPPORT_SQRT,&
      S_IEEE_SUPPORT_SQRT,&
      S1_IEEE_SUPPORT_SQRT,&
      S2_IEEE_SUPPORT_SQRT,&
      S3_IEEE_SUPPORT_SQRT,&
      S4_IEEE_SUPPORT_SQRT,&
      S5_IEEE_SUPPORT_SQRT,&
      S6_IEEE_SUPPORT_SQRT,&
      S7_IEEE_SUPPORT_SQRT,&
      D_IEEE_SUPPORT_SQRT, &
      D1_IEEE_SUPPORT_SQRT,&
      D2_IEEE_SUPPORT_SQRT,&
      D3_IEEE_SUPPORT_SQRT,&
      D4_IEEE_SUPPORT_SQRT,&
      D5_IEEE_SUPPORT_SQRT,&
      D6_IEEE_SUPPORT_SQRT,&
      D7_IEEE_SUPPORT_SQRT
  END INTERFACE IEEE_SUPPORT_SQRT

  INTERFACE IEEE_SUPPORT_STANDARD
    MODULE PROCEDURE &
      N_IEEE_SUPPORT_STANDARD,&
      S_IEEE_SUPPORT_STANDARD,&
      S1_IEEE_SUPPORT_STANDARD,&
      S2_IEEE_SUPPORT_STANDARD,&
      S3_IEEE_SUPPORT_STANDARD,&
      S4_IEEE_SUPPORT_STANDARD,&
      S5_IEEE_SUPPORT_STANDARD,&
      S6_IEEE_SUPPORT_STANDARD,&
      S7_IEEE_SUPPORT_STANDARD,&
      D_IEEE_SUPPORT_STANDARD, &
      D1_IEEE_SUPPORT_STANDARD,&
      D2_IEEE_SUPPORT_STANDARD,&
      D3_IEEE_SUPPORT_STANDARD,&
      D4_IEEE_SUPPORT_STANDARD,&
      D5_IEEE_SUPPORT_STANDARD,&
      D6_IEEE_SUPPORT_STANDARD,&
      D7_IEEE_SUPPORT_STANDARD
  END INTERFACE IEEE_SUPPORT_STANDARD

  INTERFACE IEEE_SUPPORT_UNDERFLOW_CONTROL
    MODULE PROCEDURE &
      N_IEEE_SUPPORT_UNDERFLOW_CONTROL,&
      S_IEEE_SUPPORT_UNDERFLOW_CONTROL,&
      S1_IEEE_SUPPORT_UNDERFLOW_CONTROL,&
      S2_IEEE_SUPPORT_UNDERFLOW_CONTROL,&
      S3_IEEE_SUPPORT_UNDERFLOW_CONTROL,&
      S4_IEEE_SUPPORT_UNDERFLOW_CONTROL,&
      S5_IEEE_SUPPORT_UNDERFLOW_CONTROL,&
      S6_IEEE_SUPPORT_UNDERFLOW_CONTROL,&
      S7_IEEE_SUPPORT_UNDERFLOW_CONTROL,&
      D_IEEE_SUPPORT_UNDERFLOW_CONTROL, &
      D1_IEEE_SUPPORT_UNDERFLOW_CONTROL,&
      D2_IEEE_SUPPORT_UNDERFLOW_CONTROL,&
      D3_IEEE_SUPPORT_UNDERFLOW_CONTROL,&
      D4_IEEE_SUPPORT_UNDERFLOW_CONTROL,&
      D5_IEEE_SUPPORT_UNDERFLOW_CONTROL,&
      D6_IEEE_SUPPORT_UNDERFLOW_CONTROL,&
      D7_IEEE_SUPPORT_UNDERFLOW_CONTROL

  END INTERFACE IEEE_SUPPORT_UNDERFLOW_CONTROL
  ! End of interfaces for the generic routines that support
  ! inquiry functions required by the standard.
CONTAINS
  ! Module routine for IEEE_NEXT_AFTER -
  FUNCTION S_NEXT_AFTER(X,Y)RESULT(Z)
    REAL(KIND(1.E0)), INTENT(IN) :: X, Y
    REAL(KIND(1.E0)) Z

    INTEGER, SAVE :: IX, K
    !$OMP THREADPRIVATE(IX,K)
    DO ! This is really a BLOCK construct
      IF(X /= X) THEN ! If either x or y == NaN
        Z=X           ! result is one of the input NaNs.
        EXIT
      END IF
      IF(Y /= Y) THEN
        Z=Y
        EXIT
      END IF
      IF(X == Y) THEN ! If x == y, result is x.
        Z=X           ! No exceptions raised.
        EXIT
      END IF
      IF(X == 0.E0) THEN ! x=0 leads to smallest denormalized
        ! result, in the direction of y
        Z=transfer(1,Z)
        IF(Y < 0.E0) Z=-Z
        CALL IEEE_SET_FLAG(IEEE_INEXACT, .TRUE.)
        EXIT
      END IF
      IF(IEEE_IS_FINITE(X)) THEN
        IX=transfer(X,IX) ! Transfer X to integer
        ! so bits can be added.
        IF(X == 0.E0)  IX=IBCLR(IX,31)
        IF(X < Y) THEN
          K=1
        ELSE
          K=-1
        END IF
        IF(X < 0.E0) K=-K
        IX=IX+K ! Add bit in least significant position.
        Z=transfer(IX,Z) ! Transfer back to floating point.
        ! Result had a change from input, so set INEXACT.
        CALL IEEE_SET_FLAG(IEEE_INEXACT, .TRUE.)
        ! A finite result could have OVERFLOWed and become infinite.
        IF(.not. IEEE_IS_FINITE(Z)) &
          CALL IEEE_SET_FLAG(IEEE_OVERFLOW, .TRUE.)
        EXIT
      ELSE
        Z=X ! Input is infinite, thus so is output.
        ! No OVERFLOW nor INEXACT flags signal.
        EXIT
      END IF
    END DO

  END FUNCTION  S_NEXT_AFTER

  FUNCTION D_NEXT_AFTER(X,Y)RESULT(Z)
    ! Module routine for IEEE_NEXT_AFTER -
    REAL(KIND(1.D0)), INTENT(IN) :: X, Y
    REAL(KIND(1.D0)) Z
    INTEGER, SAVE :: IX(2), K
    !$OMP THREADPRIVATE(IX,K)

    DO ! This is really a BLOCK construct
      IF(X /= X) THEN ! If either x or y == NaN
        Z=X           ! result is one of the input NaNs.
        EXIT
      END IF
      IF(Y /= Y) THEN
        Z=Y
        EXIT
      END IF
      IF(X == Y) THEN ! If x == y, result is x.
        Z=X           ! No exceptions raised.
        EXIT
      END IF
      IF(X == 0.D0) THEN ! x=0 leads to smallest denormalized
        ! result, in the direction of y
        IX(IBEG)=1; IX(IEND)=0
        Z=transfer(IX,Z)
        IF(Y < 0.D0) Z=-Z
        CALL IEEE_SET_FLAG(IEEE_INEXACT, .TRUE.)
        EXIT
      END IF
      IF(IEEE_IS_FINITE(X)) THEN
        IX=transfer(X,IX,2) ! Transfer X to integers
        ! so bits can be added.
        IF(X < Y) THEN
          K=1
        ELSE
          K=-1
        END IF
        IF(X < 0.D0) K=-K
        ! Add bit in least significant position, with carry possible.
        IF(isign(1,IX(IBEG)) /= isign(1,IX(IBEG)+1)) &
          IX(IEND)=IX(IEND)+K
        IX(IBEG)=IX(IBEG)+K

        Z=transfer(IX,Z) ! Transfer back to floating point.
        ! Result had a change from input, so set INEXACT.
        CALL IEEE_SET_FLAG(IEEE_INEXACT, .TRUE.)
        ! A finite result could have OVERFLOWed and become infinite.
        IF(.not. IEEE_IS_FINITE(Z)) &
          CALL IEEE_SET_FLAG(IEEE_OVERFLOW, .TRUE.)
        EXIT
      ELSE
        Z=X ! Input is infinite, so is output.
        ! No OVERFLOW nor INEXACT flags signal.
        EXIT
      END IF
    END DO
  END FUNCTION  D_NEXT_AFTER

  !ELEMENTAL FUNCTION SS_REM(X,Y) RESULT(Z)
  FUNCTION SS_REM(X,Y) RESULT(Z)
    ! Module routine for IEEE_REM -
    REAL(KIND(1.E0)), INTENT(IN) :: X, Y
    REAL(KIND(1.E0)) Z
    !$OMP CRITICAL
    Z=DD_REM(REAL(X,kind(1.D0)),REAL(Y,KIND(1.D0)))
    !$OMP END CRITICAL
  END FUNCTION SS_REM

  FUNCTION SD_REM(X,Y) RESULT(Z)
    ! Module routine for IEEE_REM -
    REAL(KIND(1.E0)), INTENT(IN) :: X
    REAL(KIND(1.D0)), INTENT(IN) :: Y
    REAL(KIND(1.D0)) Z
    !$OMP CRITICAL
    Z=DD_REM(REAL(X,KIND(1.D0)),Y)
    !$OMP END CRITICAL
  END FUNCTION SD_REM

  FUNCTION DS_REM(X,Y) RESULT(Z)
    ! Module routine for IEEE_REM -
    REAL(KIND(1.D0)), INTENT(IN) :: X
    REAL(KIND(1.E0)), INTENT(IN) :: Y
    REAL(KIND(1.D0)) Z
    !$OMP CRITICAL
    Z=DD_REM(X,REAL(Y,KIND(1.D0)))
    !$OMP END CRITICAL
  END FUNCTION DS_REM

  FUNCTION DD_REM(X,Y) RESULT(Z)
    ! Module routine for IEEE_REM -
    REAL(KIND(1.D0)), INTENT(IN) :: X, Y
    REAL(KIND(1.D0)) Z
    INTERFACE 
      PURE SUBROUTINE FREM(X,Y,Z) BIND(C,NAME='__GFORTRAN__frem')
        USE ISO_C_BINDING
        REAL(C_DOUBLE), INTENT(IN) :: X,Y
        REAL(C_DOUBLE), INTENT(OUT) :: Z
      END SUBROUTINE FREM
    END INTERFACE
    ! The above routine is a C code using in-line assembler.
    ! It uses the x87 and executes the built-in
    ! instruction fprem1.  This instruction is
    ! a little complicated to use.  The x87
    ! state is saved and then restored after the
    ! operation has completed.  There is a test
    ! and branch until the operation completes.
    REAL(KIND(1.D0)) :: DX, DY
    DX=X
    DY=Y
    ! Need a critical section here.  The frem C function
    ! has a local store of the state to an array that
    ! is not thread-private.
    !$OMP CRITICAL
    call frem(DX,DY,Z)
    !$OMP END CRITICAL

  END FUNCTION DD_REM

  FUNCTION S_RINT(X) RESULT(Y) !elemental
    ! Module routine for IEEE_RINT -
    REAL(KIND(1.E0)), INTENT(IN) :: X
    REAL(KIND(1.E0)) Y

    TYPE(IEEE_ROUND_TYPE) ROUND
    REAL(KIND(1.E0)), SAVE :: Z
    INTEGER, SAVE :: IX
    !$OMP THREADPRIVATE(Z,IX)
    Z=int(ABS(X)) ! Exclude the fractional part of X.
    DO ! This is really a BLOCK construct
      Y=Z
      IF(ABS(X)==Z) THEN ! If X is an integer, exit
        EXIT
      END IF
      CALL IEEE_GET_ROUNDING_MODE(ROUND)
      IF(X > 0.E0) THEN
        IF(ROUND == IEEE_DOWN .or. ROUND == IEEE_TO_ZERO) THEN
          EXIT
        END IF
        IF(ROUND==IEEE_UP) THEN
          Y=Y+1.E0
          EXIT
        END IF
        IX=Z
        ! In case of a tie use the nearest even integer.
        IF((ROUND == IEEE_NEAREST .and. (X-Z == 0.5E0)) &
          .and. btest(IX,0)) THEN
          Y=Y+1.E0
          EXIT
        END IF
      ELSE
        IF(ROUND == IEEE_UP .or. ROUND == IEEE_TO_ZERO) THEN
          EXIT
        END IF
        IF(ROUND==IEEE_DOWN) THEN
          Y=Y+1.E0
          EXIT
        END IF
        IF((ROUND == IEEE_NEAREST .and. (X-Z == 0.5E0)) &
          ! In case of a tie use the nearest even integer.
          .and. btest(IX,0)) THEN
          Y=Y+1.E0
          EXIT
        END IF
      END IF
      EXIT
    END DO
    IF(X < 0.E0) Y=-Y
  END FUNCTION S_RINT

  FUNCTION D_RINT(X) RESULT(Y) !elemental
    ! Module routine for IEEE_RINT -
    REAL(KIND(1.D0)), INTENT(IN) :: X
    REAL(KIND(1.D0)) Y

    TYPE(IEEE_ROUND_TYPE) ROUND
    REAL(KIND(1.D0)),SAVE :: Z
    INTEGER, SAVE :: IX
    !$OMP THREADPRIVATE(Z,IX)
    Z=int(ABS(X)) ! Exclude the fractional part of X.
    DO ! This is really a BLOCK construct
      Y=Z
      IF(ABS(X)==Z) THEN ! If X is an integer, exit
        EXIT
      END IF
      CALL IEEE_GET_ROUNDING_MODE(ROUND)
      IF(X > 0.D0) THEN
        IF(ROUND == IEEE_DOWN .or. ROUND == IEEE_TO_ZERO) THEN
          EXIT
        END IF
        IF(ROUND==IEEE_UP) THEN
          Y=Y+1.D0
          EXIT
        END IF
        IX=Z
        ! In case of a tie use the nearest even integer.
        IF((ROUND == IEEE_NEAREST .and. (X-Z == 0.5D0)) &
          .and. btest(IX,0)) THEN
          Y=Y+1.D0
          EXIT
        END IF
      ELSE
        IF(ROUND == IEEE_UP .or. ROUND == IEEE_TO_ZERO) THEN
          EXIT
        END IF
        IF(ROUND==IEEE_DOWN) THEN
          Y=Y+1.D0
          EXIT
        END IF
        IF((ROUND == IEEE_NEAREST .and. (X-Z == 0.5D0)) &
          ! In case of a tie use the nearest even integer.
          .and. btest(IX,0)) THEN
          Y=Y+1.D0
          EXIT
        END IF
      END IF
      EXIT
    END DO
    IF(X < 0.D0) Y=-Y
  END FUNCTION D_RINT

  FUNCTION S_SCALB(X, I) RESULT(Y)
    ! Module routine for IEEE_SCALB -
    REAL(KIND(1.E0)), INTENT(IN) :: X
    INTEGER, INTENT(IN) :: I
    REAL(KIND(1.E0)) Y

    REAL(KIND(1.E0)), SAVE :: T
    INTEGER, SAVE :: IX, IEX, J
    !$OMP THREADPRIVATE(T, IX, IEX, J)
    DO ! This is really a BLOCK construct
      IF(X /= X) THEN
        ! Input value is NaN.  Return quiet NaN as result.
        y=transfer(ISCLASS(9),y) ! Get quiet NaN from table
        EXIT
      END IF
      IF(X == 0.E0) THEN
        ! Input value is 0.  Return x as the result.
        Y=X
        EXIT
      END IF
      IF(IEEE_CLASS(ABS(X)) == IEEE_POSITIVE_INF) THEN
        ! Input value is (+-) Inf.  Return (+/-) Inf as result.
        Y=X ! Return (+/-) Inf
        EXIT
      END IF
      ! Values remain, with some significance.  Pick off the biased exponent
      ! as an integer.
      IX=transfer(X,IX)
      IEX=0
      ! Get the 8 bits of exponent in low order of IEX.
      CALL MVBITS(IX,23,8,IEX,0)
      ! Add I to get the candidate exponent.
      IEX=IEX+I
      IF(IEX >= 255) THEN
        ! Scaled abs result would be larger than huge(x).
        ! Set result to signed Inf and signal OVERFLOW.
        IEX=ISCLASS(8) ! Bit pattern for +Inf
        IF(BTEST(IX,31)) IEX=IBSET(IEX,31) ! Give Inf result the sign of x
        CALL IEEE_SET_FLAG(IEEE_OVERFLOW, .TRUE.) ! Set overflow flag
        Y=TRANSFER(IEX,Y) ! Set result
        EXIT
      END IF
      IF(IEX < 23) THEN ! Result will be denormalized or zero.
        ! Some input precision may be lost.

        ! Things are complicated here.  The value of x could be
        ! denormalized as input.  Multiplications by 2 or .5 are based 
        ! on the sign of I. Each multiply is exact.
        IF(I < 0) THEN
          T=0.5E0
        ELSE
          T=2.E0
        END IF
        y=x
        DO J=1,IABS(I)
          Y=Y*T
          ! This catches a denormalized result that underflows to zero.
          if(abs(y) == 0.e0) EXIT
        END DO
        CALL IEEE_SET_FLAG(IEEE_UNDERFLOW, .TRUE.) ! Set underflow flag.
        EXIT
      END IF
      ! Have a normal result.  Move updated exponent into place.
      ! No loss of relative accuracy occurs.
      CALL MVBITS(IEX,0,8,IX,23)
      y=transfer(IX,y) ! Set result
      EXIT
    END DO
  END FUNCTION S_SCALB

  FUNCTION D_SCALB(X, I) RESULT(Y)
    ! Module routine for IEEE_SCALB -
    REAL(KIND(1.D0)), INTENT(IN) :: X
    INTEGER, INTENT(IN) :: I
    REAL(KIND(1.D0)) Y

    REAL(KIND(1.D0)), SAVE :: T
    INTEGER, SAVE :: IX(2), IY(2), IEX, J
    !$OMP THREADPRIVATE(T, IX, IEX, J)

    DO ! This is really a BLOCK construct
      IF(X /= X) THEN
        ! Input value is NaN.  Return quiet NaN as result.
        y=transfer(IDCLASS(9*2-1:2*9),y) ! Get quiet NaN from table
        EXIT
      END IF
      IF(X == 0.D0) THEN
        ! Input value is 0.  Return x as result.
        y=x
        EXIT
      END IF
      IF(IEEE_CLASS(ABS(X)) == IEEE_POSITIVE_INF) THEN
        ! Input value is (+-) Inf.  Return Inf as result.
        Y=X ! Return (+/-) Inf
        EXIT
      END IF
      ! Normal values remain.  Pick off the biased exponent
      ! as an integer.
      IX=transfer(X,IX,2)
      IEX=0
      ! Get the 11 bits of exponent in low order of IEX.
      CALL MVBITS(IX(IEND),20,11,IEX,0)
      ! Add I to get the candidate exponent.
      IEX=IEX+I
      IF(IEX >= 2047) THEN
        ! Scaled abs result would be larger than huge(x).
        ! Set result to signed Inf and signal OVERFLOW.
        IY=IDCLASS(8*2-1:8*2) ! Bit pattern for +Inf
        IF(BTEST(IX(IEND),31)) IY(IEND)=IBSET(IY(IEND),31) ! Give Inf result the sign of x
        CALL IEEE_SET_FLAG(IEEE_OVERFLOW, .TRUE.) ! Set overflow flag
        y=transfer(IY,y) ! Set result
        EXIT
      END IF
      IF(IEX < 23+32) THEN ! Result will become denormalized or zero.
        ! Some precision may be lost.
        ! Things are complicated here.  The value of x could be
        ! denormalized as input.  Multiplications by 2 or .5 are based 
        ! on the sign of I. Each multiply is exact.
        IF(I < 0) THEN
          T=0.5E0
        ELSE
          T=2.E0
        END IF
        y=x
        DO J=1,IABS(I)
          Y=Y*T
          ! This catches a denormalized result that underflows to zero.
          if(abs(y) == 0.D0) EXIT
        END DO
        CALL IEEE_SET_FLAG(IEEE_UNDERFLOW, .TRUE.) ! Set underflow flag.
        EXIT
      END IF
      ! Have a normal result.  Move exponent into place.
      ! No loss of relative accuracy occurs in this case.
      CALL MVBITS(IEX,0,11,IX(IEND),20)
      Y=TRANSFER(IX,Y)
      EXIT
    END DO
  END FUNCTION D_SCALB

  FUNCTION S_LOGB(X)RESULT(Y)
    ! Module routine for IEEE_LOGB -
    REAL(KIND(1.E0)), INTENT(IN) :: X
    REAL(KIND(1.E0)) Y
    INTEGER, SAVE :: IX, IEX
    !$OMP THREADPRIVATE(IX,IEX)
    IEX=0
    DO ! This is really a BLOCK construct
      IF(X /= X) THEN
        ! Input value is NaN.  Return quiet NaN as result.
        y=transfer(ISCLASS(9),y) ! Get quiet NaN from table
        EXIT
      END IF
      IF(X == 0.E0) THEN
        ! Input value is 0.  Return -Inf as result.
        y=transfer(ibset(ISCLASS(1),31),y) ! Get -Inf from table
        ! Signal DIVIDE_BY_ZERO:
        CALL IEEE_SET_FLAG(IEEE_DIVIDE_BY_ZERO, .TRUE.)
        EXIT
      END IF
      IF(IEEE_CLASS(ABS(X)) == IEEE_POSITIVE_INF) THEN
        ! Input value is +- Inf.  Return Inf as result.
        y=transfer(ISCLASS(8),y) ! Get +Inf from table
        EXIT
      END IF
      ! Normal values remain.  Pick off the unbiased exponent
      ! as an integer.  Then convert it to floating point.
      IX=transfer(X,IX)
      ! Get the 8 bits of exponent in low order of IEX.
      CALL MVBITS(IX,23,8,IEX,0)
      ! Remove exponent bias with subtraction of 127.
      y=IEX-127 ! Conversion to single with =.
      EXIT
    END DO
!!$OMP END CRITICAL
  END FUNCTION S_LOGB

  FUNCTION D_LOGB(X)RESULT(Y)
    ! Module routine for IEEE_LOGB -
    REAL(KIND(1.D0)), INTENT(IN) :: X
    REAL(KIND(1.D0)) Y
    INTEGER, SAVE :: IX(2), IEX
    !$OMP THREADPRIVATE(IX,IEX)

    IEX=0
    DO ! This is really a BLOCK construct
      IF(X /= X) THEN
        ! Input value is NaN.  Return quiet NaN as result.
        y=transfer(IDCLASS(9*2-1:2*9),y) ! Get quiet NaN from table
        EXIT
      END IF
      IF(X == 0.E0) THEN
        ! Input value is 0.  Return -Inf as result.
        y=transfer([IDCLASS(1),ibset(IDCLASS(2),31)],y) ! Get -Inf from table
        ! Signal DIVIDE_BY_ZERO:
        CALL IEEE_SET_FLAG(IEEE_DIVIDE_BY_ZERO, .TRUE.)
        EXIT
      END IF
      IF(IEEE_CLASS(ABS(X)) == IEEE_POSITIVE_INF) THEN
        ! Input value is +- Inf.  Return Inf as result.
        y=transfer(IDCLASS(8*2-1:8*2),y) ! Get +Inf from table
        EXIT
      END IF
      ! Normal values remain.  Pick off the unbiased exponent
      ! as an integer.  Then convert it to floating point.
      IX=transfer(X,IX,2)
      ! Get the 11 bits of exponent in low order of IEX.
      CALL MVBITS(IX(IEND),20,11,IEX,0)
      ! Remove exponent bias with subtraction of 1023.
      y=IEX-1023 ! Conversion to double with =.
      EXIT
    END DO
  END FUNCTION D_LOGB

  elemental FUNCTION S_UNORDERED(X,Y)RESULT(YES_NO)
    ! Module routine for IEEE_UNORDERED -
    REAL(KIND(1.E0)), INTENT(IN) :: X,Y
    LOGICAL YES_NO
    ! If either x or y is NaN, result is .TRUE.
    ! Result is .FALSE. otherwise.
    YES_NO=(X /= X) .OR. (Y /= Y)
  END FUNCTION S_UNORDERED

  elemental FUNCTION D_UNORDERED(X,Y)RESULT(YES_NO)
    ! Module routine for IEEE_UNORDERED -
    REAL(KIND(1.D0)), INTENT(IN) :: X,Y
    LOGICAL YES_NO
    ! If either x or y is NaN, result is .TRUE.
    ! Result is .FALSE. otherwise.
    YES_NO=(X /= X) .OR. (Y /= Y)
  END FUNCTION D_UNORDERED

  elemental FUNCTION S_VALUE(X, CLASS) RESULT(Y)
    ! Module routine for IEEE_VALUE -
    REAL(KIND(1.E0)), INTENT(IN) :: X
    TYPE(IEEE_CLASS_TYPE), INTENT(IN) :: CLASS
    REAL(KIND(1.E0)) Y
    IF(CLASS % TYPE > 4) THEN
      Y=TRANSFER(ISCLASS(CLASS % TYPE),Y)
    ELSE
      Y=TRANSFER(ibset(ISCLASS(CLASS % TYPE),31),Y)
    END IF
  END FUNCTION S_VALUE

  elemental FUNCTION D_VALUE(X, CLASS) RESULT(Y)
    ! Module routine for IEEE_VALUE -
    REAL(KIND(1.D0)), INTENT(IN) :: X
    TYPE(IEEE_CLASS_TYPE), INTENT(IN) :: CLASS
    REAL(KIND(1.D0)) Y
    IF(CLASS % TYPE > 4) THEN
      Y=TRANSFER(IDCLASS(2*CLASS % TYPE-1:2*CLASS % TYPE),Y)
    ELSE
      Y=TRANSFER([IDCLASS(2*CLASS % TYPE-1),ibset(IDCLASS(2*CLASS % TYPE),31)],Y)
    END IF
  END FUNCTION D_VALUE

  FUNCTION S_CLASS(X)RESULT(CLASS)
    ! Module routine for IEEE_CLASS -
    REAL(KIND(1.E0)), INTENT(IN) :: X
    TYPE(IEEE_CLASS_TYPE) CLASS
    INTEGER, PARAMETER :: IINF=Z'7F800000'
    INTEGER, PARAMETER :: IQLO=Z'7FC00000'
    INTEGER, PARAMETER :: IQHI=Z'7FFFFFFF'
    INTEGER, SAVE :: IX
    LOGICAL, SAVE :: NEG
    !$OMP THREADPRIVATE(IX,NEG)

    DO ! This is really a BLOCK construct
      ! Transfer to an integer for checking bits.
      IX=transfer(X,IX)
      ! Test for NaN, and then signalling or quiet NaN:
      IF(X /= X) THEN
        ! Have a NaN here. See if it signals.
        ! Non-zero bits in the alternate mask for a NaN is a signal.
        ! These bits will not be set by hardware exceptions.  But software
        ! can set them.
        IX=IBCLR(IX,31)
        IF(IQLO <= IX .and. IX <= IQHI) THEN
          CLASS=IEEE_QUIET_NAN
        ELSE 
          CLASS=IEEE_SIGNALING_NAN
        END IF
        EXIT
      END IF
      ! Get logical equivalent for sign of X.
      NEG=BTEST(IX,31)
      ! Test for an infinity.  
      IF(IAND(IX,IINF) == IINF) THEN
        ! Test if the value is - or + infinity.
        IF(NEG) THEN
          CLASS=IEEE_NEGATIVE_INF
        ELSE
          CLASS=IEEE_POSITIVE_INF
        END IF
        EXIT
      END IF
      ! Test for zero.
      IF(ABS(X) == 0.E0) THEN
        ! Test if the value is - or + zero.
        IF(NEG) THEN
          CLASS=IEEE_NEGATIVE_ZERO
        ELSE
          CLASS=IEEE_POSITIVE_ZERO
        END IF
        EXIT
      END IF
      ! Test for denormal.
      IF(ABS(X) < TINY(X)) THEN
        ! Test if the value is - or +.
        IF(NEG) THEN
          CLASS=IEEE_NEGATIVE_DENORMAL
        ELSE
          CLASS=IEEE_POSITIVE_DENORMAL
        END IF
        EXIT
      END IF
      ! The other possibility is a normal.
      IF(NEG) THEN
        CLASS=IEEE_NEGATIVE_NORMAL
      ELSE
        CLASS=IEEE_POSITIVE_NORMAL
      END IF
      EXIT
    END DO

  end function S_CLASS

  function D_CLASS(x)RESULT(CLASS)
    ! Module routine for IEEE_CLASS -
    REAL(KIND(1.D0)), INTENT(IN) :: X
    TYPE(IEEE_CLASS_TYPE) CLASS
    INTEGER, PARAMETER :: INAN=Z'7FF80000',&
      IINF=Z'7FF00000'
    REAL(KIND(1.D0)), PARAMETER :: DMIN=REAL(INAN,KIND(1.D0)),&
      DMAX=REAL(HUGE(1),KIND(1.D0))+1.D0
    INTEGER, SAVE :: IX(2)
    LOGICAL, SAVE :: NEG
    REAL(KIND(1.D0)), SAVE :: TEMP
    !$OMP THREADPRIVATE(IX,NEG,TEMP)

    DO ! This is really a BLOCK construct
      ! Transfer to an integer for checking bits.
      IX=transfer(X,IX,2)
      ! Test for NaN, and then signalling or quiet NaN:
      IF(X /= X) THEN
        ! Have a NaN here. See if it signals.
        ! Non-zero bits in the alternate mask for a NaN is a signal.
        ! These bits will not be set by hardware exceptions.  But software
        ! can set them.
        IX(IEND)=IBCLR(IX(IEND),31)
        TEMP=REAL(IX(IEND), KIND(1.D0))
        IF(DMIN <= TEMP .and. &
          TEMP <= DMAX) THEN
          CLASS=IEEE_QUIET_NAN
        ELSE
          CLASS=IEEE_SIGNALING_NAN
        END IF
        EXIT
      END IF
      ! Get logical equivalent for sign of X.
      NEG=BTEST(IX(IEND),31)
      ! Test for an infinity.  
      IF(IAND(IX(IEND),IINF) == IINF) THEN
        ! Test if the value is - or + infinity.
        IF(NEG) THEN
          CLASS=IEEE_NEGATIVE_INF
        ELSE
          CLASS=IEEE_POSITIVE_INF
        END IF
        EXIT
      END IF
      ! Test for zero.
      IF(ABS(X) == 0.D0) THEN
        ! Test if the value is - or + zero.
        IF(NEG) THEN
          CLASS=IEEE_NEGATIVE_ZERO
        ELSE
          CLASS=IEEE_POSITIVE_ZERO
        END IF
        EXIT
      END IF
      ! Test for denormal.
      IF(ABS(X) < TINY(X)) THEN
        ! Test if the value is - or +.
        IF(NEG) THEN
          CLASS=IEEE_NEGATIVE_DENORMAL
        ELSE
          CLASS=IEEE_POSITIVE_DENORMAL
        END IF
        EXIT
      END IF
      ! The other possibility is a normal.
      IF(NEG) THEN
        CLASS=IEEE_NEGATIVE_NORMAL
      ELSE
        CLASS=IEEE_POSITIVE_NORMAL
      END IF
      EXIT
    END DO
  end function D_CLASS

  function S_COPY_SIGN(X,Y)RESULT(T)
    ! Module routine for IEEE_COPY_SIGN -
    REAL(KIND(1.E0)), INTENT(IN) :: X,Y
    REAL(KIND(1.E0)) T
    INTEGER, SAVE :: IX, IY
    !$OMP THREADPRIVATE(IX,IY)

    ! Transfer X,Y to integers so the
    ! bits can be examined.
    IX=transfer(X,IX)
    IY=transfer(Y,IY)
    ! Clear the sign that was input in X.
    IX=IBCLR(IX,31)
    ! If the sign of Y is negative, replace the
    ! sign of X.  If Y is positive the sign
    ! of X is positive, thanks to clearing
    ! the sign of X.
    IF(BTEST(IY,31)) IX=IBSET(IX,31)
    ! Transer the resulting integer representation
    ! to an output floating point value.
    T=transfer(IX,X)
  end function S_COPY_SIGN

  function D_COPY_SIGN(X,Y)RESULT(T)
    ! Module routine for IEEE_COPY_SIGN -
    REAL(KIND(1.D0)), INTENT(IN) :: X,Y
    REAL(KIND(1.D0)) T
    INTEGER, SAVE :: IX(2), IY(2)
    !$OMP THREADPRIVATE(IX,IY)
    ! Transfer X,Y to integers so the
    ! bits can be examined.
    IX=transfer(X,IX,2)
    IY=transfer(Y,IY,2)
    ! Clear the sign that was input in X.
    IX(IEND)=IBCLR(IX(IEND),31)
    ! If the sign of Y is negative, replace the
    ! sign of X.  If Y is positive the sign
    ! of X is positive, thanks to clearing
    ! the sign of X.
    IF(BTEST(IY(IEND),31)) IX=IBSET(IX(IEND),31)
    ! Transer the resulting integer representation
    ! to an output floating point value.
    T=transfer(IX,X)
  end function D_COPY_SIGN

  function S_IS_NORMAL(X)RESULT(YES_NO)
    ! Module routine for IEEE_IS_NORMAL -
    REAL(KIND(1.E0)), INTENT(IN) :: X
    LOGICAL YES_NO
    INTEGER, PARAMETER :: IINF=Z'7F800000'
    INTEGER, SAVE :: IX
    !$OMP THREADPRIVATE(IX)

    DO ! This is really a BLOCK construct
      ! If X is NaN, return .FALSE.
      YES_NO= (X /= X)
      IF(YES_NO) EXIT
      ! Number is not a NaN.  Transfer to an integer so the
      ! bits can be examined.
      IX=transfer(X,IX)
      ! Now X is not a NaN but may be (+/-) Infinity.
      YES_NO= (iand(IX,IINF)==IINF)
      IF(YES_NO) EXIT
      ! Number if not a NaN, not infinite, so check
      ! for denormalized.
      YES_NO=ABS(X) < TINY(X)
      IF(YES_NO) THEN
        ! Value could be zero, which is a normal value.
        ! But this is complemented after the block exit.
        IF(X == 0.E0) YES_NO=.FALSE.
        EXIT
      END IF
      ! The last possiblity is that the value is a normalized
      ! non-zero value.  But YES_NO = .FALSE. here.
      EXIT
    END DO
    ! The flag is complemented to correspond to the value required.
    YES_NO=.NOT. YES_NO
  end function S_IS_NORMAL

  function D_IS_NORMAL(X)RESULT(YES_NO)
    ! Module routine for IEEE_IS_NORMAL -
    REAL(KIND(1.D0)), INTENT(IN) :: X
    LOGICAL YES_NO
    INTEGER, PARAMETER :: IINF=Z'7FF00000'
    INTEGER, SAVE :: IX(2)
    !$OMP THREADPRIVATE(IX)
    DO ! This is really a BLOCK construct
      ! If X is NaN, return .FALSE.
      YES_NO= (X /= X)
      IF(YES_NO) EXIT
      ! Number is not a NaN.  Transfer to an integer so the
      ! bits can be examined.
      IX=transfer(X,IX,2)
      ! Now X is not a NaN but may be (+/-) Infinity.
      YES_NO= (iand(IX(IEND),IINF)==IINF)
      IF(YES_NO) EXIT
      ! Number if not a NaN, not infinite, so check
      ! for denormalized.
      YES_NO=ABS(X) < TINY(X)
      IF(YES_NO) THEN
        ! Value could be zero, which is a normal value.
        ! But this is complemented after the block exit.
        IF(X == 0.D0) YES_NO=.FALSE.
        EXIT
      END IF
      ! The last possiblity is that the value is a normalized
      ! non-zero value.  But YES_NO = .FALSE. here.
      EXIT
    END DO
    ! The flag is complemented to correspond to the value required.
    YES_NO=.NOT. YES_NO
  end function D_IS_NORMAL

  function S_IS_NEGATIVE(X)RESULT(YES_NO)
    ! Module routine for IEEE_IS_NEGATIVE -
    REAL(KIND(1.E0)), INTENT(IN) :: X
    LOGICAL YES_NO
    INTEGER, SAVE :: IX
    !$OMP THREADPRIVATE(IX)

    YES_NO= (X /= X)
    DO ! This is really a BLOCK construct
      ! If X is NaN, return .FALSE.
      IF(YES_NO) EXIT
      ! Number is not a NaN.  Transfer to integer so the
      ! bits can be examined.
      IX=transfer(X,IX)
      ! Now X is not a NaN but may be (+/-) Infinity.
      ! See if bit 31 is set, indicating a negative value.
      YES_NO= .NOT. BTEST(IX,31)
      EXIT
    END DO
    YES_NO=.NOT.YES_NO
  end function S_IS_NEGATIVE

  function D_IS_NEGATIVE(X)RESULT(YES_NO)
    ! Module routine for IEEE_IS_NEGATIVE -
    REAL(KIND(1.D0)), INTENT(IN) :: X
    LOGICAL YES_NO
    INTEGER, SAVE :: IX(2)
    !$OMP THREADPRIVATE(IX)

    YES_NO= (X /= X)
    DO ! This is really a BLOCK construct
      ! If X is NaN, return .FALSE.
      IF(YES_NO) EXIT
      ! Number is not a NaN.  Transfer to integer so the
      ! bits can be examined. Note that the order may be
      ! reversed:  What was high lands in IX(IB), including
      ! the sign and exponent.
      IX=transfer(X,IX,2)
      ! Now X is not a NaN  but may be (+/-) Infinity.
      ! See if bit 31 is set, indicating a negative value.
      YES_NO= .NOT. BTEST(IX(IEND),31)
      EXIT
    END DO
    YES_NO=.NOT.YES_NO

  end function D_IS_NEGATIVE

  function S_IS_FINITE(X)RESULT(YES_NO)
    ! Module routine for IEEE_IS_FINITE -
    REAL(KIND(1.E0)), INTENT(IN) :: X
    LOGICAL YES_NO
    INTEGER, PARAMETER :: IINF=Z'7F800000'
    INTEGER, SAVE :: IX
    !$OMP THREADPRIVATE(IX)

    YES_NO= (X /= X)
    DO ! This is really a BLOCK construct
      ! If X is NaN, return .FALSE.
      IF(YES_NO) EXIT
      ! Number is not a NaN.  Transfer to integer so the
      ! bits can be examined.
      IX=transfer(X,IX)
      ! See if number is (+/-) Infinity.
      YES_NO=iand(IX,IINF)==IINF
      ! If X is infinite then set output.
      IF(YES_NO) EXIT
      ! Now X is not a NaN and not (+/-) Infinity.
      EXIT
    END DO
    YES_NO=.NOT.YES_NO
  end function S_IS_FINITE

  function D_IS_FINITE(X)RESULT(YES_NO)
    ! Module routine for IEEE_IS_FINITE -
    REAL(KIND(1.D0)), INTENT(IN) :: X
    LOGICAL YES_NO
    INTEGER, PARAMETER :: IINF=Z'7FF00000'
    INTEGER, SAVE :: IX(2)
    !$OMP THREADPRIVATE(IX)

    DO ! This is really a BLOCK construct
      ! If X is NaN, return .FALSE.
      YES_NO= (X /= X)
      IF(YES_NO) EXIT
      ! Number is not a NaN.  Transfer to integer so the
      ! bits can be examined. Note that the order may be
      ! reversed:  What was high lands in IX(IB), including
      ! the exponent.
      IX=transfer(X,IX,2)
      ! See if number is (+/-) Infinity.
      YES_NO=iand(IX(IEND),IINF)==IINF
      ! Now X is not a NaN and not (+/-) Infinity.
      EXIT
    END DO
    YES_NO=.NOT.YES_NO
  end function D_IS_FINITE

  elemental function S_IS_NAN(X)RESULT(YES_NO)
    ! Module routine for IEEE_IS_NAN -
    REAL(kind(1.E0)), INTENT(IN) :: X
    LOGICAL YES_NO
    YES_NO = (X /= X)
  end function S_IS_NAN
  elemental function D_IS_NAN(X)RESULT(YES_NO)
    ! Module routine for IEEE_IS_NAN -
    REAL(kind(1.D0)), INTENT(IN) :: X
    LOGICAL YES_NO
    YES_NO = (X /= X)
  end function D_IS_NAN

  ! Start functions (they follow in groups of 3) that support
  ! inquiries about IEEE features available.
  FUNCTION N_IEEE_SUPPORT_DATATYPE() RESULT(YES_NO)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION S_IEEE_SUPPORT_DATATYPE(Y) RESULT(YES_NO)         
  REAL(KIND(1.E0)) Y
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION S1_IEEE_SUPPORT_DATATYPE(Y) RESULT(YES_NO)         
  REAL(KIND(1.E0))Y(:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION S2_IEEE_SUPPORT_DATATYPE(Y) RESULT(YES_NO)         
  REAL(KIND(1.E0))Y(:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION S3_IEEE_SUPPORT_DATATYPE(Y) RESULT(YES_NO)         
  REAL(KIND(1.E0))Y(:,:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION S4_IEEE_SUPPORT_DATATYPE(Y) RESULT(YES_NO)         
  REAL(KIND(1.E0))Y(:,:,:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION S5_IEEE_SUPPORT_DATATYPE(Y) RESULT(YES_NO)         
  REAL(KIND(1.E0))Y(:,:,:,:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION S6_IEEE_SUPPORT_DATATYPE(Y) RESULT(YES_NO)         
  REAL(KIND(1.E0))Y(:,:,:,:,:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION S7_IEEE_SUPPORT_DATATYPE(Y) RESULT(YES_NO)         
  REAL(KIND(1.E0))Y(:,:,:,:,:,:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION D_IEEE_SUPPORT_DATATYPE(Y) RESULT(YES_NO)         
  REAL(KIND(1.D0)) Y
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION D1_IEEE_SUPPORT_DATATYPE(Y) RESULT(YES_NO)         
  REAL(KIND(1.D0))Y(:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION D2_IEEE_SUPPORT_DATATYPE(Y) RESULT(YES_NO)         
  REAL(KIND(1.D0))Y(:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION D3_IEEE_SUPPORT_DATATYPE(Y) RESULT(YES_NO)         
  REAL(KIND(1.D0))Y(:,:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION D4_IEEE_SUPPORT_DATATYPE(Y) RESULT(YES_NO)         
  REAL(KIND(1.D0))Y(:,:,:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION D5_IEEE_SUPPORT_DATATYPE(Y) RESULT(YES_NO)         
  REAL(KIND(1.D0))Y(:,:,:,:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION D6_IEEE_SUPPORT_DATATYPE(Y) RESULT(YES_NO)         
  REAL(KIND(1.D0))Y(:,:,:,:,:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION D7_IEEE_SUPPORT_DATATYPE(Y) RESULT(YES_NO)         
  REAL(KIND(1.D0))Y(:,:,:,:,:,:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 

FUNCTION N_IEEE_SUPPORT_IO() RESULT(YES_NO)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION S_IEEE_SUPPORT_IO(Y) RESULT(YES_NO)               
  REAL(KIND(1.E0)) Y
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION S1_IEEE_SUPPORT_IO(Y) RESULT(YES_NO)               
  REAL(KIND(1.E0))Y(:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION S2_IEEE_SUPPORT_IO(Y) RESULT(YES_NO)               
  REAL(KIND(1.E0))Y(:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION S3_IEEE_SUPPORT_IO(Y) RESULT(YES_NO)               
  REAL(KIND(1.E0))Y(:,:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION S4_IEEE_SUPPORT_IO(Y) RESULT(YES_NO)               
  REAL(KIND(1.E0))Y(:,:,:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION S5_IEEE_SUPPORT_IO(Y) RESULT(YES_NO)               
  REAL(KIND(1.E0))Y(:,:,:,:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION S6_IEEE_SUPPORT_IO(Y) RESULT(YES_NO)               
  REAL(KIND(1.E0))Y(:,:,:,:,:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION S7_IEEE_SUPPORT_IO(Y) RESULT(YES_NO)               
  REAL(KIND(1.E0))Y(:,:,:,:,:,:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION D_IEEE_SUPPORT_IO(Y) RESULT(YES_NO)               
  REAL(KIND(1.D0)) Y
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION D1_IEEE_SUPPORT_IO(Y) RESULT(YES_NO)               
  REAL(KIND(1.D0))Y(:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION D2_IEEE_SUPPORT_IO(Y) RESULT(YES_NO)               
  REAL(KIND(1.D0))Y(:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION D3_IEEE_SUPPORT_IO(Y) RESULT(YES_NO)               
  REAL(KIND(1.D0))Y(:,:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION D4_IEEE_SUPPORT_IO(Y) RESULT(YES_NO)               
  REAL(KIND(1.D0))Y(:,:,:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION D5_IEEE_SUPPORT_IO(Y) RESULT(YES_NO)               
  REAL(KIND(1.D0))Y(:,:,:,:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION D6_IEEE_SUPPORT_IO(Y) RESULT(YES_NO)               
  REAL(KIND(1.D0))Y(:,:,:,:,:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION D7_IEEE_SUPPORT_IO(Y) RESULT(YES_NO)               
  REAL(KIND(1.D0))Y(:,:,:,:,:,:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 

FUNCTION N_IEEE_SUPPORT_DENORMAL() RESULT(YES_NO)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION S_IEEE_SUPPORT_DENORMAL(Y) RESULT(YES_NO)         
  REAL(KIND(1.E0)) Y
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION S1_IEEE_SUPPORT_DENORMAL(Y) RESULT(YES_NO)         
  REAL(KIND(1.E0))Y(:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION S2_IEEE_SUPPORT_DENORMAL(Y) RESULT(YES_NO)         
  REAL(KIND(1.E0))Y(:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION S3_IEEE_SUPPORT_DENORMAL(Y) RESULT(YES_NO)         
  REAL(KIND(1.E0))Y(:,:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION S4_IEEE_SUPPORT_DENORMAL(Y) RESULT(YES_NO)         
  REAL(KIND(1.E0))Y(:,:,:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION S5_IEEE_SUPPORT_DENORMAL(Y) RESULT(YES_NO)         
  REAL(KIND(1.E0))Y(:,:,:,:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION S6_IEEE_SUPPORT_DENORMAL(Y) RESULT(YES_NO)         
  REAL(KIND(1.E0))Y(:,:,:,:,:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION S7_IEEE_SUPPORT_DENORMAL(Y) RESULT(YES_NO)         
  REAL(KIND(1.E0))Y(:,:,:,:,:,:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION D_IEEE_SUPPORT_DENORMAL(Y) RESULT(YES_NO)         
  REAL(KIND(1.D0)) Y
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION D1_IEEE_SUPPORT_DENORMAL(Y) RESULT(YES_NO)         
  REAL(KIND(1.D0))Y(:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION D2_IEEE_SUPPORT_DENORMAL(Y) RESULT(YES_NO)         
  REAL(KIND(1.D0))Y(:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION D3_IEEE_SUPPORT_DENORMAL(Y) RESULT(YES_NO)         
  REAL(KIND(1.D0))Y(:,:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION D4_IEEE_SUPPORT_DENORMAL(Y) RESULT(YES_NO)         
  REAL(KIND(1.D0))Y(:,:,:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION D5_IEEE_SUPPORT_DENORMAL(Y) RESULT(YES_NO)         
  REAL(KIND(1.D0))Y(:,:,:,:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION D6_IEEE_SUPPORT_DENORMAL(Y) RESULT(YES_NO)         
  REAL(KIND(1.D0))Y(:,:,:,:,:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION D7_IEEE_SUPPORT_DENORMAL(Y) RESULT(YES_NO)         
  REAL(KIND(1.D0))Y(:,:,:,:,:,:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 

FUNCTION N_IEEE_SUPPORT_DIVIDE() RESULT(YES_NO)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION S_IEEE_SUPPORT_DIVIDE(Y) RESULT(YES_NO)           
  REAL(KIND(1.E0)) Y
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION S1_IEEE_SUPPORT_DIVIDE(Y) RESULT(YES_NO)           
  REAL(KIND(1.E0))Y(:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION S2_IEEE_SUPPORT_DIVIDE(Y) RESULT(YES_NO)           
  REAL(KIND(1.E0))Y(:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION S3_IEEE_SUPPORT_DIVIDE(Y) RESULT(YES_NO)           
  REAL(KIND(1.E0))Y(:,:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION S4_IEEE_SUPPORT_DIVIDE(Y) RESULT(YES_NO)           
  REAL(KIND(1.E0))Y(:,:,:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION S5_IEEE_SUPPORT_DIVIDE(Y) RESULT(YES_NO)           
  REAL(KIND(1.E0))Y(:,:,:,:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION S6_IEEE_SUPPORT_DIVIDE(Y) RESULT(YES_NO)           
  REAL(KIND(1.E0))Y(:,:,:,:,:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION S7_IEEE_SUPPORT_DIVIDE(Y) RESULT(YES_NO)           
  REAL(KIND(1.E0))Y(:,:,:,:,:,:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION D_IEEE_SUPPORT_DIVIDE(Y) RESULT(YES_NO)           
  REAL(KIND(1.D0)) Y
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION D1_IEEE_SUPPORT_DIVIDE(Y) RESULT(YES_NO)           
  REAL(KIND(1.D0))Y(:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION D2_IEEE_SUPPORT_DIVIDE(Y) RESULT(YES_NO)           
  REAL(KIND(1.D0))Y(:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION D3_IEEE_SUPPORT_DIVIDE(Y) RESULT(YES_NO)           
  REAL(KIND(1.D0))Y(:,:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION D4_IEEE_SUPPORT_DIVIDE(Y) RESULT(YES_NO)           
  REAL(KIND(1.D0))Y(:,:,:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION D5_IEEE_SUPPORT_DIVIDE(Y) RESULT(YES_NO)           
  REAL(KIND(1.D0))Y(:,:,:,:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION D6_IEEE_SUPPORT_DIVIDE(Y) RESULT(YES_NO)           
  REAL(KIND(1.D0))Y(:,:,:,:,:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION D7_IEEE_SUPPORT_DIVIDE(Y) RESULT(YES_NO)           
  REAL(KIND(1.D0))Y(:,:,:,:,:,:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 

FUNCTION N_IEEE_SUPPORT_INF() RESULT(YES_NO)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION S_IEEE_SUPPORT_INF(Y) RESULT(YES_NO)              
  REAL(KIND(1.E0)) Y
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION S1_IEEE_SUPPORT_INF(Y) RESULT(YES_NO)              
  REAL(KIND(1.E0))Y(:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION S2_IEEE_SUPPORT_INF(Y) RESULT(YES_NO)              
  REAL(KIND(1.E0))Y(:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION S3_IEEE_SUPPORT_INF(Y) RESULT(YES_NO)              
  REAL(KIND(1.E0))Y(:,:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION S4_IEEE_SUPPORT_INF(Y) RESULT(YES_NO)              
  REAL(KIND(1.E0))Y(:,:,:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION S5_IEEE_SUPPORT_INF(Y) RESULT(YES_NO)              
  REAL(KIND(1.E0))Y(:,:,:,:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION S6_IEEE_SUPPORT_INF(Y) RESULT(YES_NO)              
  REAL(KIND(1.E0))Y(:,:,:,:,:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION S7_IEEE_SUPPORT_INF(Y) RESULT(YES_NO)              
  REAL(KIND(1.E0))Y(:,:,:,:,:,:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION D_IEEE_SUPPORT_INF(Y) RESULT(YES_NO)              
  REAL(KIND(1.D0)) Y
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION D1_IEEE_SUPPORT_INF(Y) RESULT(YES_NO)              
  REAL(KIND(1.D0))Y(:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION D2_IEEE_SUPPORT_INF(Y) RESULT(YES_NO)              
  REAL(KIND(1.D0))Y(:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION D3_IEEE_SUPPORT_INF(Y) RESULT(YES_NO)              
  REAL(KIND(1.D0))Y(:,:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION D4_IEEE_SUPPORT_INF(Y) RESULT(YES_NO)              
  REAL(KIND(1.D0))Y(:,:,:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION D5_IEEE_SUPPORT_INF(Y) RESULT(YES_NO)              
  REAL(KIND(1.D0))Y(:,:,:,:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION D6_IEEE_SUPPORT_INF(Y) RESULT(YES_NO)              
  REAL(KIND(1.D0))Y(:,:,:,:,:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION D7_IEEE_SUPPORT_INF(Y) RESULT(YES_NO)              
  REAL(KIND(1.D0))Y(:,:,:,:,:,:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 

FUNCTION N_IEEE_SUPPORT_NAN() RESULT(YES_NO)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION S_IEEE_SUPPORT_NAN(Y) RESULT(YES_NO)              
  REAL(KIND(1.E0)) Y
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION S1_IEEE_SUPPORT_NAN(Y) RESULT(YES_NO)              
  REAL(KIND(1.E0))Y(:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION S2_IEEE_SUPPORT_NAN(Y) RESULT(YES_NO)              
  REAL(KIND(1.E0))Y(:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION S3_IEEE_SUPPORT_NAN(Y) RESULT(YES_NO)              
  REAL(KIND(1.E0))Y(:,:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION S4_IEEE_SUPPORT_NAN(Y) RESULT(YES_NO)              
  REAL(KIND(1.E0))Y(:,:,:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION S5_IEEE_SUPPORT_NAN(Y) RESULT(YES_NO)              
  REAL(KIND(1.E0))Y(:,:,:,:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION S6_IEEE_SUPPORT_NAN(Y) RESULT(YES_NO)              
  REAL(KIND(1.E0))Y(:,:,:,:,:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION S7_IEEE_SUPPORT_NAN(Y) RESULT(YES_NO)              
  REAL(KIND(1.E0))Y(:,:,:,:,:,:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION D_IEEE_SUPPORT_NAN(Y) RESULT(YES_NO)              
  REAL(KIND(1.D0)) Y
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION D1_IEEE_SUPPORT_NAN(Y) RESULT(YES_NO)              
  REAL(KIND(1.D0))Y(:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION D2_IEEE_SUPPORT_NAN(Y) RESULT(YES_NO)              
  REAL(KIND(1.D0))Y(:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION D3_IEEE_SUPPORT_NAN(Y) RESULT(YES_NO)              
  REAL(KIND(1.D0))Y(:,:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION D4_IEEE_SUPPORT_NAN(Y) RESULT(YES_NO)              
  REAL(KIND(1.D0))Y(:,:,:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION D5_IEEE_SUPPORT_NAN(Y) RESULT(YES_NO)              
  REAL(KIND(1.D0))Y(:,:,:,:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION D6_IEEE_SUPPORT_NAN(Y) RESULT(YES_NO)              
  REAL(KIND(1.D0))Y(:,:,:,:,:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION D7_IEEE_SUPPORT_NAN(Y) RESULT(YES_NO)              
  REAL(KIND(1.D0))Y(:,:,:,:,:,:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 

FUNCTION N_IEEE_SUPPORT_ROUNDING() RESULT(YES_NO)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION S_IEEE_SUPPORT_ROUNDING(Y) RESULT(YES_NO)         
  REAL(KIND(1.E0)) Y
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION S1_IEEE_SUPPORT_ROUNDING(Y) RESULT(YES_NO)         
  REAL(KIND(1.E0))Y(:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION S2_IEEE_SUPPORT_ROUNDING(Y) RESULT(YES_NO)         
  REAL(KIND(1.E0))Y(:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION S3_IEEE_SUPPORT_ROUNDING(Y) RESULT(YES_NO)         
  REAL(KIND(1.E0))Y(:,:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION S4_IEEE_SUPPORT_ROUNDING(Y) RESULT(YES_NO)         
  REAL(KIND(1.E0))Y(:,:,:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION S5_IEEE_SUPPORT_ROUNDING(Y) RESULT(YES_NO)         
  REAL(KIND(1.E0))Y(:,:,:,:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION S6_IEEE_SUPPORT_ROUNDING(Y) RESULT(YES_NO)         
  REAL(KIND(1.E0))Y(:,:,:,:,:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION S7_IEEE_SUPPORT_ROUNDING(Y) RESULT(YES_NO)         
  REAL(KIND(1.E0))Y(:,:,:,:,:,:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION D_IEEE_SUPPORT_ROUNDING(Y) RESULT(YES_NO)         
  REAL(KIND(1.D0)) Y
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION D1_IEEE_SUPPORT_ROUNDING(Y) RESULT(YES_NO)         
  REAL(KIND(1.D0))Y(:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION D2_IEEE_SUPPORT_ROUNDING(Y) RESULT(YES_NO)         
  REAL(KIND(1.D0))Y(:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION D3_IEEE_SUPPORT_ROUNDING(Y) RESULT(YES_NO)         
  REAL(KIND(1.D0))Y(:,:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION D4_IEEE_SUPPORT_ROUNDING(Y) RESULT(YES_NO)         
  REAL(KIND(1.D0))Y(:,:,:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION D5_IEEE_SUPPORT_ROUNDING(Y) RESULT(YES_NO)         
  REAL(KIND(1.D0))Y(:,:,:,:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION D6_IEEE_SUPPORT_ROUNDING(Y) RESULT(YES_NO)         
  REAL(KIND(1.D0))Y(:,:,:,:,:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION D7_IEEE_SUPPORT_ROUNDING(Y) RESULT(YES_NO)         
  REAL(KIND(1.D0))Y(:,:,:,:,:,:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 

FUNCTION N_IEEE_SUPPORT_SQRT() RESULT(YES_NO)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION S_IEEE_SUPPORT_SQRT(Y) RESULT(YES_NO)             
  REAL(KIND(1.E0)) Y
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION S1_IEEE_SUPPORT_SQRT(Y) RESULT(YES_NO)             
  REAL(KIND(1.E0))Y(:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION S2_IEEE_SUPPORT_SQRT(Y) RESULT(YES_NO)             
  REAL(KIND(1.E0))Y(:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION S3_IEEE_SUPPORT_SQRT(Y) RESULT(YES_NO)             
  REAL(KIND(1.E0))Y(:,:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION S4_IEEE_SUPPORT_SQRT(Y) RESULT(YES_NO)             
  REAL(KIND(1.E0))Y(:,:,:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION S5_IEEE_SUPPORT_SQRT(Y) RESULT(YES_NO)             
  REAL(KIND(1.E0))Y(:,:,:,:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION S6_IEEE_SUPPORT_SQRT(Y) RESULT(YES_NO)             
  REAL(KIND(1.E0))Y(:,:,:,:,:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION S7_IEEE_SUPPORT_SQRT(Y) RESULT(YES_NO)             
  REAL(KIND(1.E0))Y(:,:,:,:,:,:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION D_IEEE_SUPPORT_SQRT(Y) RESULT(YES_NO)             
  REAL(KIND(1.D0)) Y
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION D1_IEEE_SUPPORT_SQRT(Y) RESULT(YES_NO)             
  REAL(KIND(1.D0))Y(:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION D2_IEEE_SUPPORT_SQRT(Y) RESULT(YES_NO)             
  REAL(KIND(1.D0))Y(:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION D3_IEEE_SUPPORT_SQRT(Y) RESULT(YES_NO)             
  REAL(KIND(1.D0))Y(:,:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION D4_IEEE_SUPPORT_SQRT(Y) RESULT(YES_NO)             
  REAL(KIND(1.D0))Y(:,:,:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION D5_IEEE_SUPPORT_SQRT(Y) RESULT(YES_NO)             
  REAL(KIND(1.D0))Y(:,:,:,:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION D6_IEEE_SUPPORT_SQRT(Y) RESULT(YES_NO)             
  REAL(KIND(1.D0))Y(:,:,:,:,:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION D7_IEEE_SUPPORT_SQRT(Y) RESULT(YES_NO)             
  REAL(KIND(1.D0))Y(:,:,:,:,:,:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 

FUNCTION N_IEEE_SUPPORT_STANDARD() RESULT(YES_NO)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION S_IEEE_SUPPORT_STANDARD(Y) RESULT(YES_NO)         
  REAL(KIND(1.E0)) Y
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION S1_IEEE_SUPPORT_STANDARD(Y) RESULT(YES_NO)         
  REAL(KIND(1.E0))Y(:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION S2_IEEE_SUPPORT_STANDARD(Y) RESULT(YES_NO)         
  REAL(KIND(1.E0))Y(:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION S3_IEEE_SUPPORT_STANDARD(Y) RESULT(YES_NO)         
  REAL(KIND(1.E0))Y(:,:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION S4_IEEE_SUPPORT_STANDARD(Y) RESULT(YES_NO)         
  REAL(KIND(1.E0))Y(:,:,:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION S5_IEEE_SUPPORT_STANDARD(Y) RESULT(YES_NO)         
  REAL(KIND(1.E0))Y(:,:,:,:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION S6_IEEE_SUPPORT_STANDARD(Y) RESULT(YES_NO)         
  REAL(KIND(1.E0))Y(:,:,:,:,:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION S7_IEEE_SUPPORT_STANDARD(Y) RESULT(YES_NO)         
  REAL(KIND(1.E0))Y(:,:,:,:,:,:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION D_IEEE_SUPPORT_STANDARD(Y) RESULT(YES_NO)         
  REAL(KIND(1.D0)) Y
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION D1_IEEE_SUPPORT_STANDARD(Y) RESULT(YES_NO)         
  REAL(KIND(1.D0))Y(:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION D2_IEEE_SUPPORT_STANDARD(Y) RESULT(YES_NO)         
  REAL(KIND(1.D0))Y(:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION D3_IEEE_SUPPORT_STANDARD(Y) RESULT(YES_NO)         
  REAL(KIND(1.D0))Y(:,:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION D4_IEEE_SUPPORT_STANDARD(Y) RESULT(YES_NO)         
  REAL(KIND(1.D0))Y(:,:,:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION D5_IEEE_SUPPORT_STANDARD(Y) RESULT(YES_NO)         
  REAL(KIND(1.D0))Y(:,:,:,:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION D6_IEEE_SUPPORT_STANDARD(Y) RESULT(YES_NO)         
  REAL(KIND(1.D0))Y(:,:,:,:,:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION D7_IEEE_SUPPORT_STANDARD(Y) RESULT(YES_NO)         
  REAL(KIND(1.D0))Y(:,:,:,:,:,:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 

FUNCTION N_IEEE_SUPPORT_UNDERFLOW_CONTROL() RESULT(YES_NO)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION S_IEEE_SUPPORT_UNDERFLOW_CONTROL(Y) RESULT(YES_NO)
  REAL(KIND(1.E0)) Y
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION S1_IEEE_SUPPORT_UNDERFLOW_CONTROL(Y) RESULT(YES_NO)
  REAL(KIND(1.E0))Y(:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION S2_IEEE_SUPPORT_UNDERFLOW_CONTROL(Y) RESULT(YES_NO)
  REAL(KIND(1.E0))Y(:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION S3_IEEE_SUPPORT_UNDERFLOW_CONTROL(Y) RESULT(YES_NO)
  REAL(KIND(1.E0))Y(:,:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION S4_IEEE_SUPPORT_UNDERFLOW_CONTROL(Y) RESULT(YES_NO)
  REAL(KIND(1.E0))Y(:,:,:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION S5_IEEE_SUPPORT_UNDERFLOW_CONTROL(Y) RESULT(YES_NO)
  REAL(KIND(1.E0))Y(:,:,:,:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION S6_IEEE_SUPPORT_UNDERFLOW_CONTROL(Y) RESULT(YES_NO)
  REAL(KIND(1.E0))Y(:,:,:,:,:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION S7_IEEE_SUPPORT_UNDERFLOW_CONTROL(Y) RESULT(YES_NO)
  REAL(KIND(1.E0))Y(:,:,:,:,:,:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION D_IEEE_SUPPORT_UNDERFLOW_CONTROL(Y) RESULT(YES_NO)
  REAL(KIND(1.D0)) Y
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION D1_IEEE_SUPPORT_UNDERFLOW_CONTROL(Y) RESULT(YES_NO)
  REAL(KIND(1.D0))Y(:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION D2_IEEE_SUPPORT_UNDERFLOW_CONTROL(Y) RESULT(YES_NO)
  REAL(KIND(1.D0))Y(:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION D3_IEEE_SUPPORT_UNDERFLOW_CONTROL(Y) RESULT(YES_NO)
  REAL(KIND(1.D0))Y(:,:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION D4_IEEE_SUPPORT_UNDERFLOW_CONTROL(Y) RESULT(YES_NO)
  REAL(KIND(1.D0))Y(:,:,:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION D5_IEEE_SUPPORT_UNDERFLOW_CONTROL(Y) RESULT(YES_NO)
  REAL(KIND(1.D0))Y(:,:,:,:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION D6_IEEE_SUPPORT_UNDERFLOW_CONTROL(Y) RESULT(YES_NO)
  REAL(KIND(1.D0))Y(:,:,:,:,:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 
FUNCTION D7_IEEE_SUPPORT_UNDERFLOW_CONTROL(Y) RESULT(YES_NO)
  REAL(KIND(1.D0))Y(:,:,:,:,:,:,:)
  LOGICAL :: YES_NO 
  YES_NO=TRUE
END FUNCTION 

  ! End of functions (in groups of 3) that support
  ! inquiries about IEEE features available.

  ! Two groups of functions that support comparisons
  ! == and /= for IEEE_CLASS and IEEE_ROUND_TYPE
  elemental function class_type_equal(type1, type2)
    type(ieee_class_type), intent(in) :: type1,type2
    logical class_type_equal
    ! Compare the component values to assign equality.
    class_type_equal = (type1 % type == type2 % type)
  end function class_type_equal

  elemental function rounding_mode_equal(type1, type2)
    type(ieee_round_type), intent(in) :: type1,type2
    logical rounding_mode_equal
    ! Compare the component values to assign equality.
    rounding_mode_equal = (type1 % mode == type2 % mode)
  end function rounding_mode_equal

  elemental function class_type_not_equal(type1, type2)
    type(ieee_class_type), intent(in) :: type1,type2
    logical class_type_not_equal
    ! Compare the component values to assign not equal.
    class_type_not_equal = (type1 % type /= type2 % type)
  end function class_type_not_equal

  elemental function rounding_mode_not_equal(type1, type2)
    type(ieee_round_type), intent(in) :: type1,type2
    logical rounding_mode_not_equal
    ! Compare the component values to assign not equal.
    rounding_mode_not_equal = (type1 % mode /= type2 % mode)
  end function rounding_mode_not_equal

  elemental function x87_precision_mode_equal(type1, type2)
    type(ieee_x87_precision_type), intent(in) :: type1,type2
    logical x87_precision_mode_equal
    ! Compare the component values to assign logical equality.
    x87_precision_mode_equal = (type1 % mode == type2 % mode)
  end function x87_precision_mode_equal

  elemental function x87_precision_mode_not_equal(type1, type2)
    type(ieee_x87_precision_type), intent(in) :: type1,type2
    logical x87_precision_mode_not_equal
    ! Compare the component values to assign logical equality.
    x87_precision_mode_not_equal = (type1 % mode /= type2 % mode)
  end function x87_precision_mode_not_equal
  ! End of functions that support comparisons
  ! == and /= for IEEE_CLASS and IEEE_ROUND_TYPE
  ! and optionally IEEE_X87_PRECISION_TYPE.

  ! Set the IEEE rounding mode.
  subroutine ieee_set_rounding_mode(round_value)
    type(ieee_round_type), intent(in) :: round_value
    integer, SAVE :: SSEFLAGS, X87FLAGS, S, X
    !$OMP THREADPRIVATE(SSEFLAGS, X87FLAGS, S, X)
!!$OMP CRITICAL
    SSEFLAGS=GETSWSSE()
    X87FLAGS=GETCWX87()
    S=SSEFLAGS
    X=X87FLAGS
    !  TEMPLATE: call mvbits(from=, from_pos, len, to=, to_pos)
    ! Rounding settings for SSE are at bits 13,14.
    call mvbits(round_value % mode, 0, 2, S, 13)
    ! Rounding settings for x87 are at bits 10,11.
    call mvbits(round_value % mode, 0, 2, X, 10)
    ! Change flag setting if this is needed.
    if(s /= SSEFLAGS) CALL SETCWSSE(s)
    if(x /= X87FLAGS) CALL SETCWX87(x)
!!$OMP END CRITICAL
  end subroutine ieee_set_rounding_mode

  ! Set the IEEE rounding mode.
  subroutine ieee_get_rounding_mode(round_value)
    type(ieee_round_type), intent(out) :: round_value
    integer, SAVE :: SSEFLAGS, X87FLAGS
    !$OMP THREADPRIVATE(SSEFLAGS, X87FLAGS)
    SSEFLAGS=GETSWSSE()
    X87FLAGS=GETCWX87()
    ! It is possible that the x87 and SSE have different rounding
    ! modes set.  This code gets both but uses only the mode
    ! from the x87 as the output.  But this is only an issue
    ! if a setting of the rounding modes were initialized
    ! as different - unlikely and weird.  The setting of the
    ! rounding modes keeps both modes the same, as shown below here.

    !  TEMPLATE: call mvbits(from=, from_pos, len, to=, to_pos)
    ! Rounding settings for SSE are at bits 13,14.
    call mvbits(SSEFLAGS, 13, 2, round_value % mode, 0)
    ! Rounding settings for x87 are at bits 10,11.
    call mvbits(X87FLAGS, 10, 2, round_value % mode, 0)
  end subroutine ieee_get_rounding_mode

  subroutine ieee_get_underflow_mode(gradual)
    logical, intent(out) :: gradual
    integer,save :: SSEFLAGS, X87FLAGS
    !$OMP THREADPRIVATE(SSEFLAGS, X87FLAGS)
    ! Get the x87 and SSE control words.
    SSEFLAGS=GETSWSSE()
    X87FLAGS=GETCWX87()
    ! The setting for gradual underflow is bit 6, numbered from the right.
    gradual = .not.(btest(SSEFLAGS,6) .and. btest(X87FLAGS,6))
  end subroutine ieee_get_underflow_mode

  subroutine ieee_set_underflow_mode(gradual)
    logical, intent(in) :: gradual
    integer,save :: SSEFLAGS, X87FLAGS, S, X
    !$OMP THREADPRIVATE(SSEFLAGS, X87FLAGS, S, X)
    ! Get the x87 and SSE control words.
!!$OMP CRITICAL
    SSEFLAGS=GETSWSSE()
    X87FLAGS=GETCWX87()
    S=SSEFLAGS
    X=X87FLAGS

    ! If gradual = .TRUE., set units for gradual underflow.
    ! Else gradual underflow ceases.
    IF(gradual) THEN
      S=IBCLR(S,6)
      X=IBCLR(X,6)
    ELSE
      S=IBSET(S,6)
      X=IBSET(X,6)
    END IF
    ! Change flag setting if this is needed.
    if(s /= SSEFLAGS) CALL SETCWSSE(s)
    if(x /= X87FLAGS) CALL SETCWX87(x)
!!$OMP END CRITICAL
  end subroutine ieee_set_underflow_mode

  ! Get the x87 accuracy mode.
  ! This is not part of the IEEE module standards.
  subroutine ieee_get_x87_precision_mode(precision_value)
    type(ieee_x87_precision_type), intent(out) :: precision_value
    integer,save :: X87FLAGS
    !$OMP THREADPRIVATE(X87FLAGS)
    X87FLAGS=GETCWX87()
    precision_value % mode=0
    !  TEMPLATE: call mvbits(from=, from_pos, len, to=, to_pos)
    ! Precision settings are at bits 8,9.
    call mvbits(X87FLAGS, 8, 2, precision_value % mode, 0)
  end subroutine ieee_get_x87_precision_mode

  ! Set the x87 accuracy mode.
  ! This is not part of the IEEE module standards.
  subroutine ieee_set_x87_precision_mode(precision_value)
    type(ieee_x87_precision_type), intent(in) :: precision_value
    integer,save :: X87FLAGS, X
    !$OMP THREADPRIVATE(X87FLAGS, X)
!!$OMP CRITICAL
    X87FLAGS=GETCWX87()
    X=X87FLAGS

    !  TEMPLATE: call mvbits(from=, from_pos, len, to=, to_pos)
    ! Precision settings are at bits 8,9.
    call mvbits(precision_value % mode, 0, 2, X, 8)
    ! Change flag setting if this is needed.
    if(x /= X87FLAGS) CALL SETCWX87(x)
!!$OMP END CRITICAL
  end subroutine ieee_set_x87_precision_mode

  function IEEE_SELECTED_REAL_KIND(P,R,RADIX)RESULT(KIND)
    INTEGER, INTENT(IN), OPTIONAL :: P,R,RADIX
    INTEGER KIND
    KIND=SELECTED_REAL_KIND(P,R)
  end function IEEE_SELECTED_REAL_KIND

END MODULE IEEE_ARITHMETIC ! R. J. Hanson, F. T. Krogh
