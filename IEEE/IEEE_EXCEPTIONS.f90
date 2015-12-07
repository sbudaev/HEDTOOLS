MODULE IEEE_EXCEPTIONS
  USE ISO_C_BINDING, ONLY: C_INT

  IMPLICIT NONE
  ! Timeline 8/4/2013 10:00 AM 
  ! See Draft Fortran 2008 standard, Section 14.2.
  !------------------------------------------------------------------------------
  !    IEEE_FLAG_TYPE is for identifying a particular exception 
  !    Its only possible values are those of
  !    named constants defned in the module: IEEE_INVALID, IEEE_OVERFLOW,
  !    IEEE_DIVIDE_BY_ZERO, IEEE_UNDERFLOW, and IEEE_INEXACT. The module 
  !    also defines the array named constants IEEE_USUAL = [ IEEE_OVERFLOW,
  !    IEEE_DIVIDE_BY_ZERO, IEEE_INVALID ] and IEEE_ALL = [ IEEE_USUAL,
  !    IEEE_UNDERFLOW, IEEE_INEXACT ].
  !    IEEE_STATUS_TYPE is for representing the floating-point status.
  !------------------------------------------------------------------------------

  TYPE IEEE_FLAG_TYPE
    INTEGER, PRIVATE :: BIT_POSITION
  END TYPE IEEE_FLAG_TYPE

  INTEGER,PARAMETER,PRIVATE :: &
    INVALID=       0,&
!!!    DENORMALIZED=  1,& ! Not one of the standard flags,
                       ! but is present in the x87.
    DIVIDE_BY_ZERO=2,&
    OVERFLOW=      3,&
    UNDERFLOW=     4,&
    INEXACT=       5

  TYPE(IEEE_FLAG_TYPE), PUBLIC :: &
    IEEE_INVALID=IEEE_FLAG_TYPE(INVALID),&
    IEEE_OVERFLOW=IEEE_FLAG_TYPE(OVERFLOW),&
    IEEE_DIVIDE_BY_ZERO=IEEE_FLAG_TYPE(DIVIDE_BY_ZERO),&
    IEEE_UNDERFLOW=IEEE_FLAG_TYPE(UNDERFLOW),&
    IEEE_INEXACT=IEEE_FLAG_TYPE(INEXACT)

  TYPE(IEEE_FLAG_TYPE), PUBLIC:: IEEE_USUAL(3)=&
    [IEEE_FLAG_TYPE(OVERFLOW),IEEE_FLAG_TYPE(DIVIDE_BY_ZERO),&
    IEEE_FLAG_TYPE(INVALID)]

  TYPE(IEEE_FLAG_TYPE), PUBLIC :: IEEE_ALL(5)=&
    [IEEE_FLAG_TYPE(OVERFLOW),IEEE_FLAG_TYPE(DIVIDE_BY_ZERO),&
    IEEE_FLAG_TYPE(INVALID),IEEE_FLAG_TYPE(UNDERFLOW),&
    IEEE_FLAG_TYPE(INEXACT)]

  TYPE, BIND(C) :: IEEE_STATUS_TYPE
    ! This array of integers is long enough to hold the x87 state
    ! and also hold the SSE state.
    ! This requires 108 FPU state bytes and 512 SSE state bytes.
    ! Every aspect of both units is saved, not just the control and status.
    INTEGER(C_INT),PRIVATE :: STATEX87SSE(128+27) ! Note: space for 27 ints for FPU and 128 for SSE.
  END TYPE IEEE_STATUS_TYPE

  INTERFACE IEEE_SUPPORT_FLAG
    ! The inquiry function IEEE_SUPPORT_FLAG has several REAL types of optional arguments.
    MODULE PROCEDURE &
      N_IEEE_SUPPORT_FLAG,&
      S_IEEE_SUPPORT_FLAG,&
      S1_IEEE_SUPPORT_FLAG,&
      S2_IEEE_SUPPORT_FLAG,&
      S3_IEEE_SUPPORT_FLAG,&
      S4_IEEE_SUPPORT_FLAG,&
      S5_IEEE_SUPPORT_FLAG,&
      S6_IEEE_SUPPORT_FLAG,&
      S7_IEEE_SUPPORT_FLAG,&
      D_IEEE_SUPPORT_FLAG, &
      D1_IEEE_SUPPORT_FLAG,&
      D2_IEEE_SUPPORT_FLAG,&
      D3_IEEE_SUPPORT_FLAG,&
      D4_IEEE_SUPPORT_FLAG,&
      D5_IEEE_SUPPORT_FLAG,&
      D6_IEEE_SUPPORT_FLAG,&
      D7_IEEE_SUPPORT_FLAG
  END INTERFACE IEEE_SUPPORT_FLAG

  INTERFACE IEEE_SET_FLAG
    ! This subroutine supports a single pair, a pair of rank-1 arguments,
    ! or a rank-1 array with a single logical applied to all:
    ! CALL IEEE_SET_FLAG(FLAG, FLAG_VALUES).
    MODULE PROCEDURE SCLR_IEEE_SET_FLAG, ARRA_IEEE_SET_FLAG, &
      ARRA_SCLR_IEEE_SET_FLAG
  END INTERFACE IEEE_SET_FLAG

  ! Interface to the C codes that get and set the flags and state.
  INTERFACE
    ! These are interfaces for the inter-language calls to C:
    ! The status and control register in the X87, and the MXSCR register
    ! for the SSE units, are read or written.
    ! Note that full integer (32 bits) are used for the arguments but 
    ! the least significant 16 bits contain the information.
    ! The subroutine SETSWX87() is performed by doing a complete state
    ! save, modifying that saved state, and then restoring the state.
    PURE FUNCTION GETSWSSE()BIND(C,NAME='__GFORTRAN__get_regi_in_c_sse')
      USE ISO_C_BINDING
      INTEGER(C_INT) :: GETSWSSE ! Read MXSCR register.
    END FUNCTION GETSWSSE

    PURE SUBROUTINE SETCWSSE(CWORD) BIND(C,NAME='__GFORTRAN__set_regi_in_c_sse')
      USE ISO_C_BINDING
      INTEGER(C_INT), INTENT(IN) :: CWORD    ! Write MXSCR register.
    END SUBROUTINE SETCWSSE

    PURE FUNCTION GETCWX87()BIND(C,NAME='__GFORTRAN__get_cw_in_c_x87')
      USE ISO_C_BINDING
      INTEGER(C_INT) :: GETCWX87 ! Read X87 control register.
    END FUNCTION GETCWX87

    PURE SUBROUTINE SETCWX87(CWORD) BIND(C,NAME='__GFORTRAN__set_cw_in_c_x87')
      USE ISO_C_BINDING
      INTEGER(C_INT), INTENT(IN) :: CWORD   ! Write X87 control register.
    END SUBROUTINE SETCWX87

    PURE FUNCTION GETSWX87()BIND(C,NAME='__GFORTRAN__get_sw_in_c_x87')
      USE ISO_C_BINDING
      INTEGER(C_INT) :: GETSWX87 ! Read X87 status register.
    END FUNCTION GETSWX87

    PURE SUBROUTINE SETSWX87(SWORD) BIND(C,NAME='__GFORTRAN__set_sw_in_c_x87')
      USE ISO_C_BINDING
      INTEGER(C_INT),INTENT(OUT) :: SWORD  ! Write x87 status register.
      ! Requires a state save then restore.
    END SUBROUTINE SETSWX87

    SUBROUTINE GET_STATES(SAVE_SPACE) BIND(C,NAME='__GFORTRAN__get_states')
      USE ISO_C_BINDING
      INTEGER(C_INT), INTENT(OUT) :: SAVE_SPACE(*)
      ! Save x87 and sse state
    END SUBROUTINE GET_STATES

    SUBROUTINE SET_STATES(SAVE_SPACE) BIND(C,NAME='__GFORTRAN__set_states')
      USE ISO_C_BINDING
      INTEGER(C_INT), INTENT(IN) :: SAVE_SPACE(*)
      ! Restore x87 and sse state
    END SUBROUTINE SET_STATES
  END INTERFACE
CONTAINS

  SUBROUTINE IEEE_GET_STATUS(STATUS_VALUE)
    TYPE(IEEE_STATUS_TYPE), INTENT(OUT) :: STATUS_VALUE
    CALL GET_STATES(status_value % STATEX87SSE)
  END SUBROUTINE IEEE_GET_STATUS

  SUBROUTINE IEEE_SET_STATUS(STATUS_VALUE)
    TYPE(IEEE_STATUS_TYPE), INTENT(IN) :: STATUS_VALUE
    CALL SET_STATES(STATUS_VALUE % STATEX87SSE)
  END SUBROUTINE IEEE_SET_STATUS

  ! This group of elemental subroutines return the values of the
  ! signaling exceptions and the current halting modes for each
  ! possible exception.

  SUBROUTINE IEEE_GET_FLAG(FLAG, YES_NO)
    TYPE(IEEE_FLAG_TYPE), INTENT(IN) :: FLAG
    LOGICAL, INTENT(OUT) :: YES_NO
    ! Here we provide a basic design feature of the IEEE modules:
    ! Enabled exception flags are read for both the X87 and SSE.
    ! The bits returned are combined with an OR operation.
    ! This is necessary because floating point arithmetic can
    ! be mixed between the x87 and SSE (and other) arithmetic units.
    INTEGER, SAVE :: BP, SSEFLAGS, X87FLAGS
    !$OMP THREADPRIVATE(BP, SSEFLAGS, X87FLAGS)

    BP = FLAG % BIT_POSITION
    ! Get the flags from x87 and MXSCR register.
    SSEFLAGS=GETSWSSE()
    X87FLAGS=GETSWX87()
    ! Extract a bit (as .True., .False.) from each of two flags, at position BP.
    ! The intrinsic function btest() has .True. or .False. as the result.
    ! It is necessary to OR both flag postitions:  Computation can occur
    ! either in the x87 or the SSE.  Exceptions flagged in the x87 do not set
    ! the same exception flags in the SSE.
    YES_NO=btest(SSEFLAGS,BP) .or. btest(X87FLAGS,BP)
  END SUBROUTINE IEEE_GET_FLAG

  SUBROUTINE SCLR_IEEE_SET_FLAG(FLAG, FLAG_VALUE)
    TYPE(IEEE_FLAG_TYPE), INTENT(IN) :: FLAG
    LOGICAL, INTENT(IN) :: FLAG_VALUE
    ! Ascend scalar arguments to rank-1 arrays.
    ! Then call the array routine.
    CALL ARRA_IEEE_SET_FLAG([FLAG], [FLAG_VALUE])
  END SUBROUTINE SCLR_IEEE_SET_FLAG

  SUBROUTINE ARRA_SCLR_IEEE_SET_FLAG(FLAG, FLAG_VALUE)
    TYPE(IEEE_FLAG_TYPE), INTENT(IN) :: FLAG(:)
    LOGICAL, INTENT(IN) :: FLAG_VALUE
    LOGICAL :: FLAG_VALUE_DUPLICATES(size(FLAG))
    ! Ascend scalar argument to rank-1 arrays.
    ! Then call the array routine.
    FLAG_VALUE_DUPLICATES=FLAG_VALUE
    CALL ARRA_IEEE_SET_FLAG(FLAG, FLAG_VALUE_DUPLICATES)
  END SUBROUTINE ARRA_SCLR_IEEE_SET_FLAG

  SUBROUTINE ARRA_IEEE_SET_FLAG(FLAG, FLAG_VALUE)
    TYPE(IEEE_FLAG_TYPE), INTENT(IN) :: FLAG(:)
    LOGICAL, INTENT(IN) :: FLAG_VALUE(:)
!!!    LOGICAL :: LOCAL_FLAG
    INTEGER, SAVE :: BP, SSEFLAGS, X87FLAGS, I, S, X
    !$OMP THREADPRIVATE(BP, SSEFLAGS, X87FLAGS, I, S, X)
    ! Get the flags from x87 status word and MXSCR register.

    SSEFLAGS=GETSWSSE()
    X87FLAGS=GETSWX87()
    S=SSEFLAGS
    X=X87FLAGS
    DO I=1,size(FLAG)
      BP = FLAG(I) % BIT_POSITION
      ! If flag_value(I) is .TRUE. set the bit accordingly.
      ! This version does not insist that values of IEEE_FLAG_TYPE
      ! be unique.  The last one on this list prevails, in case of repeats.
      IF(FLAG_VALUE(I)) THEN
        S=IBSET(S,BP)
        X=IBSET(X,BP)
      ELSE
        S=IBCLR(S,BP)
        X=IBCLR(X,BP)
      END IF

    END DO

    ! If flags should change, reset their states.
    IF(S /= SSEFLAGS)CALL SETCWSSE(S)
    IF(X /= X87FLAGS)CALL SETSWX87(X) ! This is the most expensive one.
  END SUBROUTINE ARRA_IEEE_SET_FLAG
  
  SUBROUTINE IEEE_GET_HALTING_MODE (FLAG, YES_NO)
    TYPE(IEEE_FLAG_TYPE), INTENT(IN) :: FLAG
    LOGICAL, INTENT(OUT) :: YES_NO
    ! Here we provide a basic design feature of the IEEE modules:
    ! Enabled exception flags are read for both the X87 and SSE.
    ! The bits returned are combined with an AND operation.
    ! This is necessary because floating point arithmetic can
    ! be mixed between the x87 and SSE arithmetic units.
    ! A setting of the halting mode uses the x87 control word
    ! OR the MXSCR register.
    INTEGER, SAVE :: SSEFLAGS, X87FLAGS, BP
    !$OMP THREADPRIVATE(SSEFLAGS, X87FLAGS, BP)

    BP = FLAG % BIT_POSITION
    ! Get the flags from x87 and MXSCR register.
    SSEFLAGS=GETSWSSE()
    X87FLAGS=GETCWX87()
    ! Extract a bit (as .True., .False.) from each flag set, at position BP.
    ! Note the offset of 7 bits for the SSE register.
    YES_NO=.not.(btest(SSEFLAGS,BP+7) .or. btest(X87FLAGS,BP))
  END SUBROUTINE IEEE_GET_HALTING_MODE

  SUBROUTINE IEEE_SET_HALTING_MODE(FLAG, HALTING)
    TYPE(IEEE_FLAG_TYPE), INTENT(IN) :: FLAG
    LOGICAL, INTENT(IN) :: HALTING
    INTEGER, SAVE :: BP, SSEFLAGS, X87FLAGS, S, X
    !$OMP THREADPRIVATE(BP, SSEFLAGS, X87FLAGS, S, X)
    BP = FLAG % BIT_POSITION
    ! The component of FLAG corresponding to the HALTING flag
    ! must be 0,2,3,4, or 5.
    SSEFLAGS=GETSWSSE() ! Get current setting of halting flags
    X87FLAGS=GETCWX87()
    S=SSEFLAGS
    X=X87FLAGS
    IF(HALTING) THEN    ! Either clear or set a bit.
      ! Note the offset of 7 bits for the SSE register.
      S=IBCLR(S,BP+7) 
      X=IBcLR(X,BP)
    ELSE                ! Not halting is a 1 bit;
      ! halting is a 0 bit
      S=IBSET(S,BP+7)
      X=IBSET(X,BP)
    END IF
    ! If flags should change, reset their states.
    IF(S /= SSEFLAGS) CALL SETCWSSE(S) ! Set the control words
    IF(X /= X87FLAGS) CALL SETCWX87(X) ! with the new halting mode
  END SUBROUTINE IEEE_SET_HALTING_MODE

  ! This group of functions support whether or not a program
  ! may halt on an IEEE floating point exception.
  FUNCTION IEEE_SUPPORT_HALTING(FLAG) RESULT(YES_NO)
    TYPE(IEEE_FLAG_TYPE), INTENT(IN) :: FLAG
    LOGICAL :: YES_NO
    INTEGER, SAVE :: BP
    !$OMP THREADPRIVATE(BP)

    BP = FLAG % BIT_POSITION
    ! The component of FLAG corresponding to the HALTING flag
    ! must be 0,2,3,4, or 5.
    YES_NO=(BP >= 0 .and. BP <= 5) .and. (BP /= 1)
  END FUNCTION IEEE_SUPPORT_HALTING

  ! These next functions support the generic interface that allows
  ! the usage LOGICAL = IEEE_SUPPORT(FLAG, [X}), where [X] is optional
  ! and can be any type REAL value.  Basically support is as inclusive
  ! as possible, with no alternatives.
!  FUNCTION S_IEEE_SUPPORT_FLAG(FLAG, X) RESULT(YES_NO)
!    TYPE(IEEE_FLAG_TYPE), INTENT(IN) :: FLAG
!    REAL(KIND(1.E0)), INTENT(IN) :: X
!    LOGICAL :: YES_NO
!    YES_NO=IEEE_SUPPORT_HALTING(FLAG)
!  END FUNCTION S_IEEE_SUPPORT_FLAG
!
!  FUNCTION S1_IEEE_SUPPORT_FLAG(FLAG, X) RESULT(YES_NO)
!    TYPE(IEEE_FLAG_TYPE), INTENT(IN) :: FLAG
!    REAL(KIND(1.E0)), INTENT(IN) :: X(:)
!    LOGICAL :: YES_NO
!    YES_NO=IEEE_SUPPORT_HALTING(FLAG)
!  END FUNCTION S1_IEEE_SUPPORT_FLAG
!
!  FUNCTION S2_IEEE_SUPPORT_FLAG(FLAG, X) RESULT(YES_NO)
!    TYPE(IEEE_FLAG_TYPE), INTENT(IN) :: FLAG
!    REAL(KIND(1.E0)), INTENT(IN) :: X(:,:)
!    LOGICAL :: YES_NO
!    YES_NO=IEEE_SUPPORT_HALTING(FLAG)
!  END FUNCTION S2_IEEE_SUPPORT_FLAG
!
!  FUNCTION S3_IEEE_SUPPORT_FLAG(FLAG, X) RESULT(YES_NO)
!    TYPE(IEEE_FLAG_TYPE), INTENT(IN) :: FLAG
!    REAL(KIND(1.E0)), INTENT(IN) :: X(:,:,:)
!    LOGICAL :: YES_NO
!    YES_NO=IEEE_SUPPORT_HALTING(FLAG)
!  END FUNCTION S3_IEEE_SUPPORT_FLAG
!
!  FUNCTION S4_IEEE_SUPPORT_FLAG(FLAG, X) RESULT(YES_NO)
!    TYPE(IEEE_FLAG_TYPE), INTENT(IN) :: FLAG
!    REAL(KIND(1.E0)), INTENT(IN) :: X(:,:,:,:)
!    LOGICAL :: YES_NO
!    YES_NO=IEEE_SUPPORT_HALTING(FLAG)
!  END FUNCTION S4_IEEE_SUPPORT_FLAG
!
!  FUNCTION S5_IEEE_SUPPORT_FLAG(FLAG, X) RESULT(YES_NO)
!    TYPE(IEEE_FLAG_TYPE), INTENT(IN) :: FLAG
!    REAL(KIND(1.E0)), INTENT(IN) :: X(:,:,:,:,:)
!    LOGICAL :: YES_NO
!    YES_NO=IEEE_SUPPORT_HALTING(FLAG)
!  END FUNCTION S5_IEEE_SUPPORT_FLAG
!
!  FUNCTION S6_IEEE_SUPPORT_FLAG(FLAG, X) RESULT(YES_NO)
!    TYPE(IEEE_FLAG_TYPE), INTENT(IN) :: FLAG
!    REAL(KIND(1.E0)), INTENT(IN) :: X(:,:,:,:,:,:)
!    LOGICAL :: YES_NO
!    YES_NO=IEEE_SUPPORT_HALTING(FLAG)
!  END FUNCTION S6_IEEE_SUPPORT_FLAG
!
!  FUNCTION S7_IEEE_SUPPORT_FLAG(FLAG, X) RESULT(YES_NO)
!    TYPE(IEEE_FLAG_TYPE), INTENT(IN) :: FLAG
!    REAL(KIND(1.E0)), INTENT(IN) :: X(:,:,:,:,:,:,:)
!    LOGICAL :: YES_NO
!    YES_NO=IEEE_SUPPORT_HALTING(FLAG)
!  END FUNCTION S7_IEEE_SUPPORT_FLAG
!
!  FUNCTION D_IEEE_SUPPORT_FLAG(FLAG, Y) RESULT(YES_NO)
!    TYPE(IEEE_FLAG_TYPE), INTENT(IN) :: FLAG
!    REAL(KIND(1.D0)), INTENT(IN) :: Y
!    LOGICAL :: YES_NO
!    YES_NO=IEEE_SUPPORT_HALTING(FLAG)
!  END FUNCTION D_IEEE_SUPPORT_FLAG
!  FUNCTION D1_IEEE_SUPPORT_FLAG(FLAG, X) RESULT(YES_NO)
!    TYPE(IEEE_FLAG_TYPE), INTENT(IN) :: FLAG
!    REAL(KIND(1.D0)), INTENT(IN) :: X(:)
!    LOGICAL :: YES_NO
!    YES_NO=IEEE_SUPPORT_HALTING(FLAG)
!  END FUNCTION D1_IEEE_SUPPORT_FLAG
!
!  FUNCTION D2_IEEE_SUPPORT_FLAG(FLAG, X) RESULT(YES_NO)
!    TYPE(IEEE_FLAG_TYPE), INTENT(IN) :: FLAG
!    REAL(KIND(1.D0)), INTENT(IN) :: X(:,:)
!    LOGICAL :: YES_NO
!    YES_NO=IEEE_SUPPORT_HALTING(FLAG)
!  END FUNCTION D2_IEEE_SUPPORT_FLAG
!
!  FUNCTION D3_IEEE_SUPPORT_FLAG(FLAG, X) RESULT(YES_NO)
!    TYPE(IEEE_FLAG_TYPE), INTENT(IN) :: FLAG
!    REAL(KIND(1.D0)), INTENT(IN) :: X(:,:,:)
!    LOGICAL :: YES_NO
!    YES_NO=IEEE_SUPPORT_HALTING(FLAG)
!  END FUNCTION D3_IEEE_SUPPORT_FLAG
!
!  FUNCTION D4_IEEE_SUPPORT_FLAG(FLAG, X) RESULT(YES_NO)
!    TYPE(IEEE_FLAG_TYPE), INTENT(IN) :: FLAG
!    REAL(KIND(1.D0)), INTENT(IN) :: X(:,:,:,:)
!    LOGICAL :: YES_NO
!    YES_NO=IEEE_SUPPORT_HALTING(FLAG)
!  END FUNCTION D4_IEEE_SUPPORT_FLAG
!
!  FUNCTION D5_IEEE_SUPPORT_FLAG(FLAG, X) RESULT(YES_NO)
!    TYPE(IEEE_FLAG_TYPE), INTENT(IN) :: FLAG
!    REAL(KIND(1.D0)), INTENT(IN) :: X(:,:,:,:,:)
!    LOGICAL :: YES_NO
!    YES_NO=IEEE_SUPPORT_HALTING(FLAG)
!  END FUNCTION D5_IEEE_SUPPORT_FLAG
!
!  FUNCTION D6_IEEE_SUPPORT_FLAG(FLAG, X) RESULT(YES_NO)
!    TYPE(IEEE_FLAG_TYPE), INTENT(IN) :: FLAG
!    REAL(KIND(1.D0)), INTENT(IN) :: X(:,:,:,:,:,:)
!    LOGICAL :: YES_NO
!    YES_NO=IEEE_SUPPORT_HALTING(FLAG)
!  END FUNCTION D6_IEEE_SUPPORT_FLAG
!
!  FUNCTION D7_IEEE_SUPPORT_FLAG(FLAG, X) RESULT(YES_NO)
!    TYPE(IEEE_FLAG_TYPE), INTENT(IN) :: FLAG
!    REAL(KIND(1.D0)), INTENT(IN) :: X(:,:,:,:,:,:,:)
!    LOGICAL :: YES_NO
!    YES_NO=IEEE_SUPPORT_HALTING(FLAG)
!  END FUNCTION D7_IEEE_SUPPORT_FLAG

FUNCTION N_IEEE_SUPPORT_FLAG(FLAG) RESULT(YES_NO)
    TYPE(IEEE_FLAG_TYPE), INTENT(IN) :: FLAG
    LOGICAL :: YES_NO
    YES_NO=IEEE_SUPPORT_HALTING(FLAG)
END FUNCTION N_IEEE_SUPPORT_FLAG
FUNCTION S_IEEE_SUPPORT_FLAG(FLAG, Y) RESULT(YES_NO)
  TYPE(IEEE_FLAG_TYPE), INTENT(IN) :: FLAG
  REAL(KIND(1.E0)) Y
  LOGICAL :: YES_NO
  YES_NO=IEEE_SUPPORT_HALTING(FLAG)
END FUNCTION 
FUNCTION S1_IEEE_SUPPORT_FLAG(FLAG, Y) RESULT(YES_NO)
  TYPE(IEEE_FLAG_TYPE), INTENT(IN) :: FLAG
  REAL(KIND(1.E0))Y(:)
  LOGICAL :: YES_NO
  YES_NO=IEEE_SUPPORT_HALTING(FLAG)
END FUNCTION 
FUNCTION S2_IEEE_SUPPORT_FLAG(FLAG, Y) RESULT(YES_NO)
  TYPE(IEEE_FLAG_TYPE), INTENT(IN) :: FLAG
  REAL(KIND(1.E0))Y(:,:)
  LOGICAL :: YES_NO
  YES_NO=IEEE_SUPPORT_HALTING(FLAG)
END FUNCTION 
FUNCTION S3_IEEE_SUPPORT_FLAG(FLAG, Y) RESULT(YES_NO)
  TYPE(IEEE_FLAG_TYPE), INTENT(IN) :: FLAG
  REAL(KIND(1.E0))Y(:,:,:)
  LOGICAL :: YES_NO
  YES_NO=IEEE_SUPPORT_HALTING(FLAG)
END FUNCTION 
FUNCTION S4_IEEE_SUPPORT_FLAG(FLAG, Y) RESULT(YES_NO)
  TYPE(IEEE_FLAG_TYPE), INTENT(IN) :: FLAG
  REAL(KIND(1.E0))Y(:,:,:,:)
  LOGICAL :: YES_NO
  YES_NO=IEEE_SUPPORT_HALTING(FLAG)
END FUNCTION 
FUNCTION S5_IEEE_SUPPORT_FLAG(FLAG, Y) RESULT(YES_NO)
  TYPE(IEEE_FLAG_TYPE), INTENT(IN) :: FLAG
  REAL(KIND(1.E0))Y(:,:,:,:,:)
  LOGICAL :: YES_NO
  YES_NO=IEEE_SUPPORT_HALTING(FLAG)
END FUNCTION 
FUNCTION S6_IEEE_SUPPORT_FLAG(FLAG, Y) RESULT(YES_NO)
  TYPE(IEEE_FLAG_TYPE), INTENT(IN) :: FLAG
  REAL(KIND(1.E0))Y(:,:,:,:,:,:)
  LOGICAL :: YES_NO
  YES_NO=IEEE_SUPPORT_HALTING(FLAG)
END FUNCTION 
FUNCTION S7_IEEE_SUPPORT_FLAG(FLAG, Y) RESULT(YES_NO)
  TYPE(IEEE_FLAG_TYPE), INTENT(IN) :: FLAG
  REAL(KIND(1.E0))Y(:,:,:,:,:,:,:)
  LOGICAL :: YES_NO
  YES_NO=IEEE_SUPPORT_HALTING(FLAG)
END FUNCTION 
FUNCTION D_IEEE_SUPPORT_FLAG(FLAG, Y) RESULT(YES_NO)
  TYPE(IEEE_FLAG_TYPE), INTENT(IN) :: FLAG
  REAL(KIND(1.D0)) Y
  LOGICAL :: YES_NO
  YES_NO=IEEE_SUPPORT_HALTING(FLAG)
END FUNCTION 
FUNCTION D1_IEEE_SUPPORT_FLAG(FLAG, Y) RESULT(YES_NO)
  TYPE(IEEE_FLAG_TYPE), INTENT(IN) :: FLAG
  REAL(KIND(1.D0))Y(:)
  LOGICAL :: YES_NO
  YES_NO=IEEE_SUPPORT_HALTING(FLAG)
END FUNCTION 
FUNCTION D2_IEEE_SUPPORT_FLAG(FLAG, Y) RESULT(YES_NO)
  TYPE(IEEE_FLAG_TYPE), INTENT(IN) :: FLAG
  REAL(KIND(1.D0))Y(:,:)
  LOGICAL :: YES_NO
  YES_NO=IEEE_SUPPORT_HALTING(FLAG)
END FUNCTION 
FUNCTION D3_IEEE_SUPPORT_FLAG(FLAG, Y) RESULT(YES_NO)
  TYPE(IEEE_FLAG_TYPE), INTENT(IN) :: FLAG
  REAL(KIND(1.D0))Y(:,:,:)
  LOGICAL :: YES_NO
  YES_NO=IEEE_SUPPORT_HALTING(FLAG)
END FUNCTION 
FUNCTION D4_IEEE_SUPPORT_FLAG(FLAG, Y) RESULT(YES_NO)
  TYPE(IEEE_FLAG_TYPE), INTENT(IN) :: FLAG
  REAL(KIND(1.D0))Y(:,:,:,:)
  LOGICAL :: YES_NO
  YES_NO=IEEE_SUPPORT_HALTING(FLAG)
END FUNCTION 
FUNCTION D5_IEEE_SUPPORT_FLAG(FLAG, Y) RESULT(YES_NO)
  TYPE(IEEE_FLAG_TYPE), INTENT(IN) :: FLAG
  REAL(KIND(1.D0))Y(:,:,:,:,:)
  LOGICAL :: YES_NO
  YES_NO=IEEE_SUPPORT_HALTING(FLAG)
END FUNCTION 
FUNCTION D6_IEEE_SUPPORT_FLAG(FLAG, Y) RESULT(YES_NO)
  TYPE(IEEE_FLAG_TYPE), INTENT(IN) :: FLAG
  REAL(KIND(1.D0))Y(:,:,:,:,:,:)
  LOGICAL :: YES_NO
  YES_NO=IEEE_SUPPORT_HALTING(FLAG)
END FUNCTION 
FUNCTION D7_IEEE_SUPPORT_FLAG(FLAG, Y) RESULT(YES_NO)
  TYPE(IEEE_FLAG_TYPE), INTENT(IN) :: FLAG
  REAL(KIND(1.D0))Y(:,:,:,:,:,:,:)
  LOGICAL :: YES_NO
  YES_NO=IEEE_SUPPORT_HALTING(FLAG)
END FUNCTION

  ! Emd of group for support of generic IEEE_SUPPORT_FLAG.
END MODULE IEEE_EXCEPTIONS ! R. J. Hanson
