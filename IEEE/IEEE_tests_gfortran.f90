PROGRAM IEEE_TESTS
  ! Timeline 8/4/2013 9:00 AM 
  USE, NON_INTRINSIC :: IEEE_FEATURES
  USE, NON_INTRINSIC :: IEEE_ARITHMETIC
  USE, NON_INTRINSIC :: IEEE_EXCEPTIONS

  USE MESSY_M   ! For output only in the test program
  USE PRECISION ! For KIND parameters with test output only
  IMPLICIT NONE

  ! These are tests for the gfortran version of the 
  ! (intrinsic) IEEE modules.  During development the output
  ! was carefully compared with the Intel XE versions,
  ! and this established "correctness" when compared
  ! to the standard.  ! See Draft Fortran 2008 standard,
  ! Section 14.2.
  ! For testing purposes the are classified as NON_INTRINSIC
  ! modules.

  TYPE(IEEE_FLAG_TYPE) ATYPE, ARRAY_TYPE(5)
  TYPE(IEEE_STATUS_TYPE) SAVE_SPOT
  TYPE(IEEE_CLASS_TYPE) CLASS_TYPE
  ! For setting the x87 to compute with single or double extended
  ! accuracy.  The default is double accuracy.
  TYPE(IEEE_X87_PRECISION_TYPE) PRECISION_VALUE
  CHARACTER(LEN=10) :: VERSION='gfortran'
  TYPE(MESSY_TY) E
  REAL(SKIND) SA,SB,SC,SD
  REAL(DKIND) DA,DB,DC,DD
  LOGICAL ALLPASS, FLAG_VALUEA, FLAG_VALUEB, FLAG_VALUEC, ARRAY_FLAGS(5)
  INTEGER I

  ! These bit patterns are transered to floating point
  ! values and used for testing IEEE_CLASS.
  INTEGER, PARAMETER :: ISCLASS(10) = &
    (/&
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
    /)
  INTEGER, PARAMETER :: IDCLASS(2*10) = &
    (/&
    Z'00000000',Z'7FF00000',& ! becomes IEEE_NEGATIVE_INF
    Z'00000000',Z'3FF00000',& ! becomes IEEE_NEGATIVE_NORMAL, -1.D0
    Z'00000000',Z'00080000',& ! becomes IEEE_NEGATIVE_DENORMAL, -tiny(1.D0)/2
    Z'00000000',Z'00000000',& ! becomes IEEE_NEGATIVE_ZERO, -0.D0
    Z'00000000',Z'00000000',& ! IEEE_POSITIVE_ZERO,  0.D0
    Z'00000000',Z'00080000',& ! IEEE_POSITIVE_DENORMAL, tiny(1.D0)/2
    Z'00000000',Z'3FF00000',& ! IEEE_POSITIVE_NORMAL,  1.D0 
    Z'00000000',Z'7FF00000',& ! IEEE_POSITIVE_INF
    Z'00000000',Z'7FF80000',& ! IEEE_QUIET_NAN
    Z'00000001',Z'7FF00000'&  ! IEEE_SIGNALING_NAN
    /)
  CHARACTER(LEN=22),PARAMETER :: CLASS_NAMES(10) = &
    (/&
    'IEEE_NEGATIVE_INF     ',&
    'IEEE_NEGATIVE_NORMAL  ',&
    'IEEE_NEGATIVE_DENORMAL',&
    'IEEE_NEGATIVE_ZERO    ',&
    'IEEE_POSITIVE_ZERO    ',&
    'IEEE_POSITIVE_DENORMAL',&
    'IEEE_POSITIVE_NORMAL  ',&
    'IEEE_POSITIVE_INF     ',&
    'IEEE_QUIET_NAN        ',&
    'IEEE_SIGNALING_NAN    '&
    /)
  INTERFACE ! Calls to x87 code for computing d=sqrt(a/b+c).
    FUNCTION S87(A, B, C) BIND(C, NAME='fraobpc')
      USE ISO_C_BINDING
      REAL(C_FLOAT), INTENT(IN) :: A, B, C
      REAL(C_FLOAT) S87
    END FUNCTION S87
    FUNCTION D87(A, B, C) BIND(C, NAME='draobpc')
      USE ISO_C_BINDING
      REAL(C_DOUBLE), INTENT(IN) :: A, B, C
      REAL(C_DOUBLE) D87
    END FUNCTION D87
    FUNCTION GETPI() BIND(C,NAME='getpi')
      USE ISO_C_BINDING
      REAL(C_DOUBLE) GETPI
    END FUNCTION GETPI
  END INTERFACE
  ! Look for tests that PASS.  Print summary.
  ALLPASS=.TRUE.
  ! Enable gradual underflow.
  CALL IEEE_SET_UNDERFLOW_MODE(.TRUE.)
  ! Don't stop on INEXACT.
  CALL IEEE_SET_HALTING_MODE(IEEE_INEXACT,.FALSE.)
  ! Save the x87 and SSE startup state. This will be restored
  ! after each major test.
  CALL IEEE_GET_STATUS(SAVE_SPOT)

  !----------------------------------------------------------------------
  ATYPE=IEEE_INVALID
  ARRAY_TYPE=IEEE_ALL
  ! A call is made to an x87 routine the computes d=sqrt(a/b+c).
  ! With different values of a,b, and c all the exceptions can
  ! be triggered this way.
  SA=0;SB=0;SC=0
  FLAG_VALUEB=.FALSE.
  ARRAY_FLAGS=.TRUE.
  CALL IEEE_GET_HALTING_MODE(ATYPE, FLAG_VALUEA)
  CALL IEEE_SET_HALTING_MODE(ATYPE, FLAG_VALUEB) ! Turn off HALTING on INVALID
  ARRAY_FLAGS=FLAG_VALUEA
  CALL IEEE_SET_FLAG(ARRAY_TYPE, ARRAY_FLAGS)
  CALL IEEE_SET_FLAG(ATYPE, FLAG_VALUEB) ! Quiet the flag INVALID
  SD=S87(SA,SB,SC) ! Compute using x87 with values that trigger INVALID
  CALL IEEE_GET_FLAG(ATYPE, FLAG_VALUEA)
  ! The IEEE_INVALID flag is tested: 
  DA=SA;DB=SB;DC=SC;DD=SD
  IF(FLAG_VALUEA) THEN
    call messy(E,'$N$D4 Computing (w/x87) D=sqrt(A/B+C), D=$R, A=$R, B=$R, C=$R, correctly&
      & signals $N IEEE_INVALID using '//VERSION, &
      rdat=[DD,DA,DB,DC])
  ELSE
    call messy(E,'$N$D4 Computing (w/x87) D=sqrt(A/B+C), D=$R, A=$R, B=$R, C=$R,&
      & incorrectly DOES NOT signal $N IEEE_INVALID using '//VERSION, &
      rdat=[DD,DA,DB,DC])
        ALLPASS=.FALSE.
  END IF

  CALL IEEE_SET_FLAG(ATYPE, FLAG_VALUEB)         ! Quiet the flag INVALID
  CALL IEEE_SET_STATUS(SAVE_SPOT)                ! Restore starting state
  CALL IEEE_SET_HALTING_MODE(ATYPE, FLAG_VALUEB) ! Turn off HALTING on INVALID
  SD=SQRT(SA/SB+SC) ! Compute using SSE with values that trigger INVALID
  CALL IEEE_GET_FLAG(ATYPE, FLAG_VALUEA)
  FLAG_VALUEA=FLAG_VALUEA .and. IEEE_IS_NAN(SD)
  ! The IEEE_INVALID flag is tested: 
  IF(FLAG_VALUEA) THEN
    call messy(E,'$N$D4 Computing (w/SSE) D=sqrt(A/B+C), D=$R, A=$R, B=$R, C=$R, correctly&
      & signals $N IEEE_INVALID using '//VERSION, &
      rdat=[DD,DA,DB,DC])
  ELSE
    call messy(E,'$N$D4 Computing (w/SSE) D=sqrt(A/B+C), D=$R, A=$R, B=$R, C=$R,&
      & incorrectly DOES NOT signal $N IEEE_INVALID using '//VERSION, &
      rdat=[DD,DA,DB,DC])
        ALLPASS=.FALSE.
  END IF
  CALL IEEE_SET_FLAG(ATYPE, FLAG_VALUEB) ! Quiet the flag INVALID
  CALL IEEE_SET_STATUS(SAVE_SPOT)        ! Restore starting state
  !----------------------------------------------------------------------
  ATYPE=IEEE_DIVIDE_BY_ZERO
  CALL IEEE_SET_HALTING_MODE(ATYPE, FLAG_VALUEB) ! Turn off HALTING on DIVIDE_BY_ZERO
  SA=1;SB=0;SC=0
  DA=SA;DB=SB;DC=SC;DD=SD
  DD=D87(DA,DB,DC) ! Compute using x87 with values that trigger DIVIDE_BY_ZERO
  CALL IEEE_GET_FLAG(ATYPE, FLAG_VALUEA)
  FLAG_VALUEA=FLAG_VALUEA .and. (.NOT. IEEE_IS_FINITE(SD))
  ! The IEEE_DIVIDE_BY_ZERO flag is tested: 
  
  IF(FLAG_VALUEA) THEN
    call messy(E,'$N$D4 Computing (w/x87) D=sqrt(A/B+C), D=$R, A=$R, B=$R, C=$R,&
      & correctly $N signals IEEE_DIVIDE_BY_ZERO using '//VERSION, &
      rdat=[DD,DA,DB,DC])
  ELSE
    call messy(E,'$N$D4 Computing (w/x87) D=sqrt(A/B+C), D=$R, A=$R, B=$R, C=$R,&
      & incorrectly $N DOES NOT signal IEEE_DIVIDE_BY_ZERO using '//VERSION, &
      rdat=[DD,DA,DB,DC])
        ALLPASS=.FALSE.
  END IF
  CALL IEEE_SET_FLAG(ATYPE, FLAG_VALUEB) ! Quiet the flag IEEE_DIVIDE_BY_ZERO
  CALL IEEE_SET_STATUS(SAVE_SPOT)        ! Restore starting state
  CALL IEEE_SET_HALTING_MODE(ATYPE, FLAG_VALUEB) ! Turn off HALTING on DIVIDE_BY_ZERO
  SD=SQRT(SA/SB+SC) ! Compute using SSE with values that trigger DIVIDE_BY_ZERO
  CALL IEEE_GET_FLAG(ATYPE, FLAG_VALUEA)
  ! The IEEE_INVALID flag is tested: 
  IF(FLAG_VALUEA) THEN
    call messy(E,'$N$D4 Computing (w/SSE) D=sqrt(A/B+C), D=$R, A=$R, B=$R, C=$R,&
      & correctly $N signals IEEE_DIVIDE_BY_ZERO using '//VERSION, &
      rdat=[DD,DA,DB,DC])
  ELSE
    call messy(E,'$N$D4 Computing (w/SSE) D=sqrt(A/B+C), D=$R, A=$R, B=$R, C=$R,&
      & incorrectly $N DOES NOT signal IEEE_DIVIDE_BY_ZERO using '//VERSION, &
      rdat=[DD,DA,DB,DC])
        ALLPASS=.FALSE.
  END IF

  CALL IEEE_SET_FLAG(ATYPE, FLAG_VALUEB) ! Quiet the flag IEEE_DIVIDE_BY_ZERO
  CALL IEEE_SET_STATUS(SAVE_SPOT)        ! Restore starting state
  !----------------------------------------------------------------------
  ATYPE=IEEE_OVERFLOW
  CALL IEEE_SET_HALTING_MODE(ATYPE, FLAG_VALUEB) ! Turn off HALTING on OVERFLOW
  SA=HUGE(SA);SB=TINY(SB);SC=0
  SD=S87(SA,SB,SC) ! Compute using x87 with values that trigger OVERFLOW
  CALL IEEE_GET_FLAG(ATYPE, FLAG_VALUEA)
  FLAG_VALUEA=FLAG_VALUEA 
  ! The IEEE_OVERFLOW flag is tested: 
  DA=SA;DB=SB;DC=SC;DD=SD
  IF(FLAG_VALUEA) THEN
    call messy(E,'$N$D4 Computing (w/x87) D=sqrt(A/B+C), D=$R, A=$R, B=$R, C=$R,&
      & incorrectly $N signals IEEE_OVERFLOW using '//VERSION, &
      rdat=[DD,DA,DB,DC])
        ALLPASS=.FALSE.
  ELSE
    call messy(E,'$N$D4 Computing (w/x87) D=sqrt(A/B+C), D=$R, A=$R, B=$R, C=$R,&
      & correctly $N DOES NOT signal IEEE_OVERFLOW using '//VERSION, &
      rdat=[DD,DA,DB,DC])
  END IF
  CALL IEEE_SET_FLAG(ATYPE, FLAG_VALUEB) ! Quiet the flag IEEE_OVERFLOW
  CALL IEEE_SET_STATUS(SAVE_SPOT)        ! Restore starting state
  CALL IEEE_SET_HALTING_MODE(ATYPE, FLAG_VALUEB) ! Turn off HALTING on OVERFLOW
  SD=SQRT(SA/SB+SC) ! Compute using SSE with values that trigger OVERFLOW
  CALL IEEE_GET_FLAG(ATYPE, FLAG_VALUEA)
  FLAG_VALUEA=FLAG_VALUEA .and. (.NOT. IEEE_IS_FINITE(SD))
  ! The IEEE_OVERFLOW flag is tested: 
  IF(FLAG_VALUEA) THEN
    call messy(E,'$N$D4 Computing (w/SSE) D=sqrt(A/B+C), D=$R, A=$R, B=$R, C=$R,&
      & correctly $N signals IEEE_OVERFLOW using '//VERSION, &
      rdat=[DD,DA,DB,DC])
  ELSE
    call messy(E,'$N$D4 Computing (w/SSE) D=sqrt(A/B+C), D=$R, A=$R, B=$R, C=$R,&
      & incorrectly $N DOES NOT signal IEEE_OVERFLOW using '//VERSION, &
      rdat=[DD,DA,DB,DC])
        ALLPASS=.FALSE.
  END IF
  CALL IEEE_SET_FLAG(ATYPE, FLAG_VALUEB) ! Quiet the flag IEEE_OVERFLOW
  CALL IEEE_SET_STATUS(SAVE_SPOT)        ! Restore starting state
  !----------------------------------------------------------------------
  ATYPE=IEEE_UNDERFLOW
  CALL IEEE_SET_HALTING_MODE(ATYPE, FLAG_VALUEB) ! Turn off HALTING on UNDERFLOW
  SA=TINY(SA);SB=HUGE(SB);SC=0
  SD=S87(SA,SB,SC) ! Compute using x87 with values that trigger UNDERFLOW
  CALL IEEE_GET_FLAG(ATYPE, FLAG_VALUEA)
  ! The IEEE_UNDERFLOW flag is tested: 
  DA=SA;DB=SB;DC=SC;DD=SD
  IF(FLAG_VALUEA) THEN
    call messy(E,'$N$D4 Computing (w/x87) D=sqrt(A/B+C), D=$R, A=$R, B=$R, C=$R,&
      & correctly $N signals IEEE_UNDERFLOW using '//VERSION, &
      rdat=[DD,DA,DB,DC])
  ELSE
    call messy(E,'$N$D4 Computing (w/x87) D=sqrt(A/B+C), D=$R, A=$R, B=$R, C=$R,&
      & incorrectly $N DOES NOT signal IEEE_UNDERFLOW using '//VERSION, &
      rdat=[DD,DA,DB,DC])
        ALLPASS=.FALSE.
  END IF

  CALL IEEE_SET_FLAG(ATYPE, FLAG_VALUEB) ! Quiet the flag IEEE_UNDERFLOW
  CALL IEEE_SET_STATUS(SAVE_SPOT)        ! Restore starting state
  SD=SQRT(SA/SB+SC) ! Compute using SSE with values that trigger INVALID
  CALL IEEE_GET_FLAG(ATYPE, FLAG_VALUEA)
  ! The IEEE_UNDERFLOW flag is tested: 
  IF(FLAG_VALUEA) THEN
    call messy(E,'$N$D4 Computing (w/SSE) D=sqrt(A/B+C), D=$R, A=$R, B=$R, C=$R,&
      & correctly $N signals IEEE_UNDERFLOW using '//VERSION, &
      rdat=[DD,DA,DB,DC])
  ELSE
    call messy(E,'$N$D4 Computing (w/SSE) D=sqrt(A/B+C), D=$R, A=$R, B=$R, C=$R,&
      & incorrectly $N DOES NOT signal IEEE_UNDERFLOW using '//VERSION, &
      rdat=[DD,DA,DB,DC])
        ALLPASS=.FALSE.
  END IF
  CALL IEEE_SET_FLAG(ATYPE, FLAG_VALUEB) ! Quiet the flag IEEE_UNDERFLOW
  CALL IEEE_SET_STATUS(SAVE_SPOT)        ! Restore starting state
  !----------------------------------------------------------------------
  ATYPE=IEEE_INEXACT
  CALL IEEE_SET_HALTING_MODE(ATYPE, FLAG_VALUEB) ! Turn off HALTING on INEXACT
  SA=2;SB=1;SC=0
  SD=S87(SA,SB,SC) ! Compute using x87 with values that trigger INAXACT.
  CALL IEEE_GET_FLAG(ATYPE, FLAG_VALUEA)
  ! The IEEE_INEXACT flag is tested: 
  DA=SA;DB=SB;DC=SC;DD=SD
  IF(FLAG_VALUEA) THEN
    call messy(E,'$N$D4 Computing (w/x87) D=sqrt(A/B+C), D=$R, A=$R, B=$R, C=$R,&
      & correctly $N signals IEEE_INEXACT using '//VERSION, &
      rdat=[DD,DA,DB,DC])
  ELSE
    call messy(E,'$N$D4 Computing (w/x87) D=sqrt(A/B+C), D=$R, A=$R, B=$R, C=$R,&
      & incorrectly $N DOES NOT signal IEEE_INEXACT using '//VERSION, &
      rdat=[DD,DA,DB,DC])
        ALLPASS=.FALSE.
  END IF

  CALL IEEE_SET_FLAG(ATYPE, FLAG_VALUEB) ! Quiet the flag IEEE_INEXACT
  CALL IEEE_SET_STATUS(SAVE_SPOT)        ! Restore starting state
  SD=SQRT(SA/SB+SC) ! Compute using SSE with values that trigger INEXACT
  CALL IEEE_GET_FLAG(ATYPE, FLAG_VALUEA)
  ! The IEEE_INEXACT flag is tested: 
  IF(FLAG_VALUEA) THEN
    call messy(E,'$N$D4 Computing (w/SSE) D=sqrt(A/B+C), D=$R, A=$R, B=$R, C=$R,&
      & correctly $N signals IEEE_INEXACT using '//VERSION, &
      rdat=[DD,DA,DB,DC])
  ELSE
    call messy(E,'$N$D4 Computing (w/SSE) D=sqrt(A/B+C), D=$R, A=$R, B=$R, C=$R,&
      & incorrectly $N DOES NOT signal IEEE_INEXACT using '//VERSION, &
      rdat=[DD,DA,DB,DC])
        ALLPASS=.FALSE.
  END IF
  CALL IEEE_SET_FLAG(ATYPE, FLAG_VALUEB) ! Quiet the flag IEEE_INEXACT
  CALL IEEE_SET_STATUS(SAVE_SPOT)        ! Restore starting state
  ATYPE=IEEE_INVALID
  CALL IEEE_SET_HALTING_MODE(ATYPE, FLAG_VALUEB) ! Turn off HALTING on INVALID
  !----------------------------------------------------------------------
  ! Test the subroutines from IEEE_ARITHMETIC.
  DO I=1,10
    ! Transfer the bit pattern so the number is now floating point.
    ! Test single precision IEEE_CLASS() first.
    IF(I > 4) THEN
      SA=transfer(ISCLASS(I),SA)
    ELSE
      SA=transfer(ibset(ISCLASS(I),31),SA)
    END IF
    CLASS_TYPE=IEEE_CLASS(SA)
    SELECT CASE(I)
    CASE (1)
      FLAG_VALUEA=(CLASS_TYPE == IEEE_NEGATIVE_INF)
    CASE (2)
      FLAG_VALUEA=(CLASS_TYPE == IEEE_NEGATIVE_NORMAL)
    CASE (3)
      FLAG_VALUEA=(CLASS_TYPE == IEEE_NEGATIVE_DENORMAL)
    CASE (4)
      FLAG_VALUEA=(CLASS_TYPE == IEEE_NEGATIVE_ZERO)
    CASE (5)
      FLAG_VALUEA=(CLASS_TYPE == IEEE_POSITIVE_ZERO)
    CASE (6)
      FLAG_VALUEA=(CLASS_TYPE == IEEE_POSITIVE_DENORMAL)
    CASE (7)
      FLAG_VALUEA=(CLASS_TYPE == IEEE_POSITIVE_NORMAL)
    CASE (8)
      FLAG_VALUEA=(CLASS_TYPE == IEEE_POSITIVE_INF)
    CASE (9)
      FLAG_VALUEA=(CLASS_TYPE == IEEE_QUIET_NAN)
      FLAG_VALUEA=(CLASS_TYPE /= IEEE_SIGNALING_NAN) .and. FLAG_VALUEA
    CASE (10)
      FLAG_VALUEA=(CLASS_TYPE == IEEE_SIGNALING_NAN)
      FLAG_VALUEA=(CLASS_TYPE /= IEEE_QUIET_NAN) .and. FLAG_VALUEA
    END SELECT
    IF(.NOT. FLAG_VALUEA) EXIT
  END DO
  IF(FLAG_VALUEA) THEN
    call messy(E,'$N All single precision types are correctly classified$N&
      &  using IEEE_CLASS() and the overloaded comparisons ==, /=.')
  ELSE
    call messy(E,'$NClassification of the single precision data = $R should be classified as $N'&
      //trim(CLASS_NAMES(I))//', but it was not.',rdat=[real(SA,dkind)])
        ALLPASS=.FALSE.
  END IF
  DO I = 1,9
    IF(I > 4) THEN
      DA=transfer(IDCLASS(2*I-1:2*I),DA)
    ELSE
      DA=transfer([IDCLASS(2*I-1),ibset(IDCLASS(2*I),31)],DA)
    END IF
    CLASS_TYPE=IEEE_CLASS(DA)
    SELECT CASE(I)
    CASE (1)
      FLAG_VALUEA=(CLASS_TYPE == IEEE_NEGATIVE_INF)
    CASE (2)
      FLAG_VALUEA=(CLASS_TYPE == IEEE_NEGATIVE_NORMAL)
    CASE (3)
      FLAG_VALUEA=(CLASS_TYPE == IEEE_NEGATIVE_DENORMAL)
    CASE (4)
      FLAG_VALUEA=(CLASS_TYPE == IEEE_NEGATIVE_ZERO)
    CASE (5)
      FLAG_VALUEA=(CLASS_TYPE == IEEE_POSITIVE_ZERO)
    CASE (6)
      FLAG_VALUEA=(CLASS_TYPE == IEEE_POSITIVE_DENORMAL)
    CASE (7)
      FLAG_VALUEA=(CLASS_TYPE == IEEE_POSITIVE_NORMAL)
    CASE (8)
      FLAG_VALUEA=(CLASS_TYPE == IEEE_POSITIVE_INF)
    CASE (9)
      FLAG_VALUEA=(CLASS_TYPE == IEEE_QUIET_NAN)
      FLAG_VALUEA=(CLASS_TYPE /= IEEE_SIGNALING_NAN) .and. FLAG_VALUEA
    CASE (10)
      FLAG_VALUEA=(CLASS_TYPE == IEEE_SIGNALING_NAN)
      FLAG_VALUEA=(CLASS_TYPE /= IEEE_QUIET_NAN) .and. FLAG_VALUEA
    END SELECT
    IF(.NOT. FLAG_VALUEA) EXIT
  END DO
  IF(FLAG_VALUEA) THEN
    call messy(E,'$N All double precision types are correctly classified$N&
      &  using IEEE_CLASS() and the overloaded comparisons ==, /=.')
  ELSE
    call messy(E,'$NClassification of the double precision data = $R should be classified as $N'&
      //trim(CLASS_NAMES(I))//', but it was not.',rdat=[DA])
        ALLPASS=.FALSE.
  END IF
  !----------------------------------------------------------------------
  ! Test IEEE_COPY_SIGN().
  SA=transfer(ibset(ISCLASS(2),31),SA)
  SB=transfer(ISCLASS(7),SB)
  FLAG_VALUEA=(IEEE_COPY_SIGN(SA,SB) > 0) .and. (IEEE_COPY_SIGN(SB,SA) < 0)
  IF(FLAG_VALUEA) THEN
    call messy(E,'$N A single precision sign copy of 1. to -1.$N&
      &  and then -1. to 1. is correct.')
  ELSE
    call messy(E,'$N A single precision sign copy of 1. to -1.$N&
      &  and then -1. to 1. is NOT correct.')
        ALLPASS=.FALSE.
  END IF

  DA=transfer([IDCLASS(3),ibset(IDCLASS(4),31)],DA)
  DB=transfer(IDCLASS(2*7-1:2*7),DB)
  FLAG_VALUEA=(IEEE_COPY_SIGN(DA,DB) > 0) .and. (IEEE_COPY_SIGN(DB,DA) < 0)
  IF(FLAG_VALUEA) THEN
    call messy(E,'$N A double precision sign copy of 1. to -1.$N&
      &  and then -1. to 1. is correct.')
  ELSE
    call messy(E,'$N A double precision sign copy of 1. to -1.$N&
      &  and then -1. to 1. is NOT correct.')
        ALLPASS=.FALSE.
  END IF

  ! Test classification of finite types, IEEE_IS_FINITE().
  FLAG_VALUEA=.TRUE.
  ATYPE=IEEE_INVALID
  CALL IEEE_SET_HALTING_MODE(ATYPE, FLAG_VALUEB) ! Turn off HALTING on INVALID
  DO I = 1,10
    IF(I > 4) THEN
      SA=transfer(ISCLASS(I),SA)
    ELSE
      SA=transfer(ibset(ISCLASS(I),31),SA)
    END IF

    FLAG_VALUEB=IEEE_IS_FINITE(SA)
    IF(I==1 .or. I > 7) FLAG_VALUEB=.not. FLAG_VALUEB
    FLAG_VALUEA=FLAG_VALUEA .and. FLAG_VALUEB
    IF(FLAG_VALUEA) CYCLE
    call messy(E,'$N A FINITE VALUE classification is NOT correct$N&
      &  for the single precision $R, of class: '//trim(CLASS_NAMES(I)),&
      rdat=[real(SA,dkind)])
        ALLPASS=.FALSE.
    EXIT
  END DO
  IF(FLAG_VALUEA) THEN
    call messy(E,'$N All single precision FINITE VALUE classifications are correct.$N')
  END IF
  FLAG_VALUEA=.TRUE.
  DO I = 1,10
    IF(I > 4) THEN
      DA=transfer(IDCLASS(2*I-1:2*I),DA)
    ELSE
      DA=transfer([IDCLASS(2*I-1),ibset(IDCLASS(2*I),31)],DA)
    END IF
    FLAG_VALUEB=IEEE_IS_FINITE(DA)
    IF(I==1 .or. I > 7) FLAG_VALUEB=.not. FLAG_VALUEB
    FLAG_VALUEA=FLAG_VALUEA .and. FLAG_VALUEB
    IF(FLAG_VALUEA) CYCLE
    call messy(E,'$N A FINITE VALUE classification is NOT correct$N&
      &  for the double precision $R, of class: '//trim(CLASS_NAMES(I)),&
      rdat=[DA])
        ALLPASS=.FALSE.
    EXIT
  END DO
  IF(FLAG_VALUEA) THEN
    call messy(E,'$N All double precision FINITE VALUE classifications are correct.$N')
  END IF
  !----------------------------------------------------------------------
  ! Test tests of NaNs:
  FLAG_VALUEA=.TRUE.
  DO I=1,10
    IF(I > 4) THEN
      SA=transfer(ISCLASS(I),SA)
    ELSE
      SA=transfer(ibset(ISCLASS(I),31),SA)
    END IF
    FLAG_VALUEB=IEEE_IS_NAN(SA)
    IF(I < 9) FLAG_VALUEB=.not. FLAG_VALUEB
    FLAG_VALUEA=FLAG_VALUEA .and. FLAG_VALUEB
    IF(FLAG_VALUEA) CYCLE
    call messy(E,'$N A NaN VALUE classification is NOT correct$N&
      &  for the single precision $R, of class: '//trim(CLASS_NAMES(I)),&
      rdat=[real(SA,dkind)])
        ALLPASS=.FALSE.
    EXIT
  END DO
  IF(FLAG_VALUEA) THEN
    call messy(E,'$N All single precision NaN VALUE classifications are correct.$N')
  END IF
  FLAG_VALUEA=.TRUE.
  DO I=1,10
    IF(I > 4) THEN
      DA=transfer(IDCLASS(2*I-1:2*I),DA)
    ELSE
      DA=transfer([IDCLASS(2*I-1),ibset(IDCLASS(2*I),31)],DA)
    END IF
    FLAG_VALUEB=IEEE_IS_NAN(DA)
    IF(I < 9) FLAG_VALUEB=.not. FLAG_VALUEB
    FLAG_VALUEA=FLAG_VALUEA .and. FLAG_VALUEB
    IF(FLAG_VALUEA) CYCLE
    call messy(E,'$N A NaN VALUE classification is NOT correct$N&
      &  for the double precision $R, of class: '//trim(CLASS_NAMES(I)),&
      rdat=[DA])
        ALLPASS=.FALSE.
    EXIT
  END DO
  IF(FLAG_VALUEA) THEN
    call messy(E,'$N All double precision NaN VALUE classifications are correct.$N')
  END IF
  !----------------------------------------------------------------------
  ! Tests of negative values.
  FLAG_VALUEA=.TRUE.
  DO I=1,10
    IF(I > 4) THEN
      SA=transfer(ISCLASS(I),SA)
    ELSE
      SA=transfer(ibset(ISCLASS(I),31),SA)
    END IF
    FLAG_VALUEB=IEEE_IS_NEGATIVE(SA)
    IF(I > 4) FLAG_VALUEB=.not. FLAG_VALUEB
    FLAG_VALUEA=FLAG_VALUEA .and. FLAG_VALUEB
    IF(FLAG_VALUEA) CYCLE
    call messy(E,'$N A NEGATIVE VALUE classification is NOT correct$N&
      &  for the single precision $R, of class: '//trim(CLASS_NAMES(I)),&
      rdat=[real(SA,dkind)])
        ALLPASS=.FALSE.
    EXIT
  END DO
  IF(FLAG_VALUEA) THEN
    call messy(E,'$N All single precision NEGATIVE VALUE classifications are correct.$N')
  END IF
  FLAG_VALUEA=.TRUE.
  DO I=1,10
    IF(I > 4) THEN
      DA=transfer(IDCLASS(2*I-1:2*I),DA)
    ELSE
      DA=transfer([IDCLASS(2*I-1),ibset(IDCLASS(2*I),31)],DA)
    END IF
    FLAG_VALUEB=IEEE_IS_NEGATIVE(DA)
    IF(I > 4) FLAG_VALUEB=.not. FLAG_VALUEB
    FLAG_VALUEA=FLAG_VALUEA .and. FLAG_VALUEB
    IF(FLAG_VALUEA) CYCLE
    call messy(E,'$N A NEGATIVE VALUE classification is NOT correct$N&
      &  for the double precision $R, of class: '//trim(CLASS_NAMES(I)),&
      rdat=[DA])
        ALLPASS=.FALSE.
    EXIT
  END DO
  IF(FLAG_VALUEA) THEN
    call messy(E,'$N All double precision NEGATIVE VALUE classifications are correct.$N')
  END IF
  !----------------------------------------------------------------------
  ! Tests of normal values.
  FLAG_VALUEA=.TRUE.
  DO I=1,10
    IF(I > 4) THEN
      SA=transfer(ISCLASS(I),SA)
    ELSE
      SA=transfer(ibset(ISCLASS(I),31),SA)
    END IF
    FLAG_VALUEB=IEEE_IS_NORMAL(SA)
    IF(I /= 2 .and. I /=4 .and. I /= 5 .and. I /= 7) FLAG_VALUEB=.NOT. FLAG_VALUEB
    FLAG_VALUEA=FLAG_VALUEA .and. FLAG_VALUEB
    IF(FLAG_VALUEA) CYCLE
    call messy(E,'$N A NORMAL VALUE classification is NOT correct$N&
      &  for the single precision $R, of class: '//trim(CLASS_NAMES(I)),&
      rdat=[real(SA,dkind)])
        ALLPASS=.FALSE.
    EXIT
  END DO
  IF(FLAG_VALUEA) THEN
    call messy(E,'$N All single precision NORMAL VALUE classifications are correct.$N')
  END IF
  FLAG_VALUEA=.TRUE.
  DO I=1,10
    IF(I > 4) THEN
      DA=transfer(IDCLASS(2*I-1:2*I),DA)
    ELSE
      DA=transfer([IDCLASS(2*I-1),ibset(IDCLASS(2*I),31)],DA)
    END IF
    FLAG_VALUEB=IEEE_IS_NORMAL(DA)
    IF(I /= 2 .and. I /=4 .and. I /= 5 .and. I /= 7) FLAG_VALUEB=.NOT. FLAG_VALUEB
    FLAG_VALUEA=FLAG_VALUEA .and. FLAG_VALUEB
    IF(FLAG_VALUEA) CYCLE
    call messy(E,'$N A NORMAL VALUE classification is NOT correct$N&
      &  for the double precision $R, of class: '//trim(CLASS_NAMES(I)),&
      rdat=[DA])
        ALLPASS=.FALSE.
    EXIT
  END DO
  IF(FLAG_VALUEA) THEN
    call messy(E,'$N All double precision NORMAL VALUE classifications are correct.$N')
  END IF
  !----------------------------------------------------------------------
  ! Tests of LOGB:
  CALL IEEE_SET_STATUS(SAVE_SPOT)        ! Restore starting state
  SB=2.E0
  SA=IEEE_LOGB(SB)
  SD=0.E0
  ATYPE=IEEE_DIVIDE_BY_ZERO
  FLAG_VALUEB=.FALSE.
  CALL IEEE_SET_HALTING_MODE(ATYPE, FLAG_VALUEB) ! Turn off HALTING on DIVIDIE_BY_ZERO
  SC=IEEE_LOGB(SD)
  CALL IEEE_GET_FLAG(IEEE_DIVIDE_BY_ZERO, FLAG_VALUEA)
  IF(.not. FLAG_VALUEA) THEN
    call messy(E,'$N Single precision LOGB(0.E0)=-Inf, and IEEE_DIVIDE_BY_ZERO$N&
      &should signal.  It DOES NOT.  F2003 standard violation.')
        ALLPASS=.FALSE.
  END IF
  CALL IEEE_SET_FLAG(IEEE_DIVIDE_BY_ZERO, FLAG_VALUEA)
  FLAG_VALUEA=(SA == 1.E0) .and. &
    (IEEE_CLASS(SC) == IEEE_NEGATIVE_INF)
  IF(FLAG_VALUEA) THEN
    call messy(E,'$N Single precision LOGB returned correct results with &
      &inputs of ($R and $R).$N',rdat=[real(sb,dkind),real(sd,dkind)])
  ELSE
    call messy(E,'$N Single precision LOGB returned INCORRECT results with &
      &inputs of ($R and $R).$N',rdat=[real(sb,dkind),real(sd,dkind)])
        ALLPASS=.FALSE.
  END IF

  CALL IEEE_SET_STATUS(SAVE_SPOT)        ! Restore starting state
  DB=2.D0
  DA=IEEE_LOGB(DB)
  DD=0.D0
  ATYPE=IEEE_DIVIDE_BY_ZERO
  CALL IEEE_SET_HALTING_MODE(ATYPE, FLAG_VALUEB) ! Turn off HALTING on DIVIDIE_BY_ZERO
  DC=IEEE_LOGB(DD)
  CALL IEEE_GET_FLAG(IEEE_DIVIDE_BY_ZERO, FLAG_VALUEA)
  IF(.not. FLAG_VALUEA) THEN
    call messy(E,'$N Double precision LOGB(0.D0)=-Inf, and IEEE_DIVIDE_BY_ZERO$N&
      &should signal.  It DOES NOT.  F2003 standard violation.')
        ALLPASS=.FALSE.
  END IF
  FLAG_VALUEA=(DA == 1.D0) .and. &
    (IEEE_CLASS(DC) == IEEE_NEGATIVE_INF)
  IF(FLAG_VALUEA) THEN
    call messy(E,'$N Double precision LOGB returned correct results with &
      &inputs of ($R and $R).$N',rdat=[db,dd])
  ELSE
    call messy(E,'$N Double precision LOGB returned INCORRECT results with &
      &inputs of ($R and $R).$N',rdat=[db,dd])
        ALLPASS=.FALSE.
  END IF
  !----------------------------------------------------------------------
  ! Tests for IEEE_NEXT_AFTER, single.
  CALL IEEE_SET_STATUS(SAVE_SPOT)        ! Restore starting state
  SB=0.E0
  SC=IEEE_VALUE(SC, IEEE_POSITIVE_INF)
  ATYPE=IEEE_INVALID
  CALL IEEE_SET_HALTING_MODE(ATYPE, FLAG_VALUEB) ! Turn off HALTING on INVALID
  SA=IEEE_NEXT_AFTER(SB, SC) ! Get next of 0 toward +Inf
  CALL IEEE_GET_FLAG(IEEE_INEXACT, FLAG_VALUEA) ! INEXACT must signal
  IF(.not. FLAG_VALUEA) THEN
    call messy(E,'$N Single precision NEXT_AFTER(0.E0,+Inf)=1, and IEEE_INEXACT$N&
      &should signal.  It DOES NOT.  F2003 standard violation.')
        ALLPASS=.FALSE.
  END IF
  FLAG_VALUEA=(SA > SB) ! Go down after a NEXT_AFTER up.
  SA=IEEE_NEXT_AFTER(SA,IEEE_VALUE(SC, IEEE_NEGATIVE_INF))
  FLAG_VALUEA=FLAG_VALUEA .and. (SA == 0.E0)
  IF(FLAG_VALUEA) THEN
    call messy(E,'$N Single precision NEXT_AFTER(x,y) returned correct results with &
      &inputs of x=$R and y=$R.$N',rdat=[real(sb,dkind),real(sc,dkind)])
  ELSE
    call messy(E,'$N Single precision NEXT_AFTER(x,y) returned INCORRECT results with &
      &inputs of x=$R and y=$R.$N',rdat=[real(sb,dkind),real(sc,dkind)])
        ALLPASS=.FALSE.
  END IF

  CALL IEEE_SET_STATUS(SAVE_SPOT)        ! Restore starting state
  SC=IEEE_VALUE(SC, IEEE_NEGATIVE_INF)
  SA=IEEE_NEXT_AFTER(SB, SC) ! Get next after of 0 toward -Inf
  CALL IEEE_GET_FLAG(IEEE_INEXACT, FLAG_VALUEA) ! INEXACT must signal
  IF(.not. FLAG_VALUEA) THEN
    call messy(E,'$N Single precision NEXT_AFTER(0.E0,-Inf)=-1, and IEEE_INEXACT$N&
      &should signal.  It DOES NOT.  F2003 standard violation.')
        ALLPASS=.FALSE.
  END IF
  FLAG_VALUEA=(SA < SB) ! Go up after a NEXT_AFTER down.
  SA=IEEE_NEXT_AFTER(SA,IEEE_VALUE(SC, IEEE_POSITIVE_INF))
  FLAG_VALUEA=FLAG_VALUEA .and. (SA == 0.E0)

  IF(FLAG_VALUEA) THEN
    call messy(E,'$N Single precision NEXT_AFTER(x,y) returned correct results with &
      &inputs of x=$R and y=$R.$N',rdat=[real(sb,dkind),real(sc,dkind)])
  ELSE
    call messy(E,'$N Single precision NEXT_AFTER(x,y) returned INCORRECT results with &
      &inputs of x=$R and y=$R.$N',rdat=[real(sb,dkind),real(sc,dkind)])
        ALLPASS=.FALSE.
  END IF
  CALL IEEE_SET_STATUS(SAVE_SPOT)        ! Restore starting state
  SB=HUGE(SA)
  ATYPE=IEEE_OVERFLOW
  CALL IEEE_SET_HALTING_MODE(ATYPE, FLAG_VALUEB) ! Turn off HALTING on OVERFLOW
  SA=IEEE_NEXT_AFTER(SB, IEEE_VALUE(SA, IEEE_POSITIVE_INF))
  CALL IEEE_GET_FLAG(IEEE_INEXACT,  FLAG_VALUEA) ! INEXACT must signal
  IF(.not. FLAG_VALUEA) THEN
    call messy(E,'$N Single precision NEXT_AFTER(HUGE(0.E0),+Inf)=+Inf, and IEEE_INEXACT$N&
      &should signal.  It DOES NOT.  F2003 standard violation.')
        ALLPASS=.FALSE.
  END IF
  CALL IEEE_GET_FLAG(IEEE_OVERFLOW, FLAG_VALUEB) ! OVERFLOW must signal
  IF(.not. FLAG_VALUEB) THEN
    call messy(E,'$N Single precision NEXT_AFTER(HUGE(0.E0),+Inf)=+Inf, and IEEE_OVERFLOW$N&
      &should signal.  It DOES NOT.  F2003 standard violation.')
        ALLPASS=.FALSE.
  END IF
  FLAG_VALUEA=(SA > SB) .and. &
    .not. IEEE_IS_FINITE(SA)
  IF(FLAG_VALUEA) THEN
    call messy(E,'$N Single precision NEXT_AFTER(HUGE(0.E0),+Inf), correctly &
      &has an INFINITE value.')
  ELSE
    call messy(E,'$NDouble precision NEXT_AFTER(HUGE(0.E0),+Inf), DOES NOT HAVE&
      & an INFINITE value, but it should.')
        ALLPASS=.FALSE.
  END IF
  !----------------------------------------------------------------------
  ! Tests for IEEE_NEXT_AFTER, double
  CALL IEEE_SET_STATUS(SAVE_SPOT)        ! Restore starting state
  DB=0.D0
  DC=IEEE_VALUE(DC, IEEE_POSITIVE_INF)
  DA=IEEE_NEXT_AFTER(DB, DC) ! Get next of 0 toward +Inf
  CALL IEEE_GET_FLAG(IEEE_INEXACT, FLAG_VALUEA) ! INEXACT must signal
  IF(.not. FLAG_VALUEA) THEN
    call messy(E,'$N Double precision NEXT_AFTER(0.D0,+Inf)=+Inf, and IEEE_INEXACT$N&
      &should signal.  It DOES NOT.  F2003 standard violation.')
        ALLPASS=.FALSE.
  END IF
  FLAG_VALUEA = (DA > DB)
  DA=IEEE_NEXT_AFTER(DA,IEEE_VALUE(DC, IEEE_NEGATIVE_INF))
  FLAG_VALUEA=FLAG_VALUEA .and. (DA == 0.D0)
  IF(FLAG_VALUEA) THEN
    call messy(E,'$N Double precision NEXT_AFTER(x,y) returned correct results with &
      &inputs of x=$R and y=$R.$N',rdat=[DB,DC])
  ELSE
    call messy(E,'$N Double precision NEXT_AFTER(x,y) returned INCORRECT results with &
      &inputs of x=$R and y=$R.$N',rdat=[DB,DC])
        ALLPASS=.FALSE.
  END IF

  CALL IEEE_SET_STATUS(SAVE_SPOT)        ! Restore starting state
  DC=IEEE_VALUE(DC, IEEE_NEGATIVE_INF)
  DA=IEEE_NEXT_AFTER(DB, DC) ! Get next of 0 toward -Inf
  CALL IEEE_GET_FLAG(IEEE_INEXACT, FLAG_VALUEA) ! INEXACT must signal
  IF(.not. FLAG_VALUEA) THEN
    call messy(E,'$N Double precision NEXT_AFTER(0.D0,-Inf)=-1, and IEEE_INEXACT$N&
      &should signal.  It DOES NOT.  F2003 standard violation.')
        ALLPASS=.FALSE.
  END IF
  FLAG_VALUEA=(DA < DB)
  DA=IEEE_NEXT_AFTER(DA,IEEE_VALUE(DC, IEEE_POSITIVE_INF))
  FLAG_VALUEA=FLAG_VALUEA .and. (DA == 0.D0)

  IF(FLAG_VALUEA) THEN
    call messy(E,'$N Double precision NEXT_AFTER(x,y) returned correct results with &
      &inputs of x=$R and y=$R.$N',rdat=[DB,DC])
  ELSE
    call messy(E,'$N Double precision NEXT_AFTER(x,y) returned INCORRECT results with &
      &inputs of x=$R and y=$R.$N',rdat=[DB,DC])
        ALLPASS=.FALSE.
  END IF

  CALL IEEE_SET_STATUS(SAVE_SPOT)        ! Restore starting state
  DB=HUGE(DA)
  ATYPE=IEEE_OVERFLOW
  FLAG_VALUEB=.FALSE.
  CALL IEEE_SET_HALTING_MODE(ATYPE, FLAG_VALUEB) ! Turn off HALTING on OVERFLOW
  SA=IEEE_NEXT_AFTER(DB, IEEE_VALUE(DA, IEEE_POSITIVE_INF))
  CALL IEEE_GET_FLAG(IEEE_INEXACT,  FLAG_VALUEA) ! INEXACT must signal
  IF(.not. FLAG_VALUEA) THEN
    call messy(E,'$N Double precision NEXT_AFTER(HUGE(0.D0),+Inf)=+Inf, and IEEE_INEXACT$N&
      &should signal.  It DOES NOT.  F2003 standard violation.')
        ALLPASS=.FALSE.
  END IF
  CALL IEEE_GET_FLAG(IEEE_OVERFLOW, FLAG_VALUEB) ! OVERFLOW must signal
  IF(.not. FLAG_VALUEB) THEN
    call messy(E,'$N Double precision NEXT_AFTER(HUGE(0.D0),+Inf)=+Inf, and IEEE_OVERFLOW$N&
      &should signal.  It DOES NOT.  F2003 standard violation.')
        ALLPASS=.FALSE.
  END IF
  FLAG_VALUEA=(SA > SB) .and. &
    .not. IEEE_IS_FINITE(SA)
  IF(FLAG_VALUEA) THEN
    call messy(E,'$N Double precision NEXT_AFTER(HUGE(0.D0),+Inf), correctly &
      &has an INFINITE value.')
  ELSE
    call messy(E,'$N Double precision NEXT_AFTER(HUGE(0.D0),+Inf), DOES NOT HAVE&
      & an INFINITE value, but it should.')
        ALLPASS=.FALSE.
  END IF
  !----------------------------------------------------------------------
  ! Tests for IEEE_REM, double. (All precison mixes call the double version.)
  ! So only double is tested.
  ! Simultaneously check rounding modes using all possible settings.
  CALL IEEE_SET_STATUS(SAVE_SPOT)        ! Restore starting state

  ! Optionally check default precision settings for x87 FPU.
  CALL IEEE_GET_X87_PRECISION_MODE(PRECISION_VALUE)
  CALL IEEE_SET_X87_PRECISION_MODE(IEEE_DOUBLE_EXTENDED)
  IF(PRECISION_VALUE == IEEE_DOUBLE_EXTENDED) THEN
    call messy(E,'$N NOTE: The default X87 precision is set for&
      & DOUBLE_EXTENDED, or 64 bits of working precision.')
  ELSE IF (PRECISION_VALUE == IEEE_DOUBLE) THEN
    call messy(E,'$N NOTE: The default X87 precision is set for DOUBLE,&
      & or 53 bits of working precision.')
  ELSE
    call messy(E,'$N NOTE: The default X87 precision is set for SINGLE,&
      & or 23 bits of working precision.')
  END IF
  ! Restore default x87 precision.
  CALL IEEE_SET_X87_PRECISION_MODE(PRECISION_VALUE)
  CALL IEEE_SET_ROUNDING_MODE(IEEE_DOWN)
  DA=GETPI() ! Value of \pi from X87 internal store.
  CALL IEEE_SET_ROUNDING_MODE(IEEE_UP)
  DB=GETPI()
  IF(DB > DA) THEN ! Round down should yield value < round up, here.
    call messy(E,'$N Double precision rounding mode IEEE_DOWN&
      & for \pi ~= $R is < value with IEEE_UP.',rdat=[DA])
  ELSE
    call messy(E,'$N Double precision rounding mode IEEE_DOWN&
      & for \pi ~= $R is INCORRECTLY >= value with IEEE_UP.',rdat=[DA])
        ALLPASS=.FALSE.
  END If
  CALL IEEE_SET_ROUNDING_MODE(IEEE_TO_ZERO)
  DC=GETPI()
  CALL IEEE_SET_ROUNDING_MODE(IEEE_NEAREST) ! This has no effect on IEEE_REM().
  IF(DC == DA) THEN ! Round down should agree with round to zero, here.
    call messy(E,'$N Double precision rounding mode IEEE_DOWN&
      & for \pi ~= $R agrees with IEEE_TO_ZERO.',rdat=[DA])
  ELSE
    call messy(E,'$N Double precision rounding mode IEEE_DOWN&
      & for \pi ~= $R INCOREECTLY disagrees with IEEE_TO_ZERO.',rdat=[DA])
        ALLPASS=.FALSE.
  END IF
  
  DB=GETPI()
  DA=4.D0
  DA=IEEE_REM(DA,DB)
  DC=5.D0
  DC=IEEE_REM(5.D0,DB)
  IF(DA+DB == 4.D0) THEN
    call messy(E,'$N Double precision a=IEEE_REM(4, \pi) correctly satisfies&
      & a + \pi==4.')
  ELSE
    call messy(E,'$N Double precision a=IEEE_REM(4, \pi) INCORRECTLY&
      & does not satisfy a + \pi==4.')
        ALLPASS=.FALSE.
  END IF
  IF(DC+2*DB == 5.D0) THEN
    call messy(E,'$N Double precision a=IEEE_REM(5, \pi) correctly satisfies&
      & a + 2 * \pi==5.')
  ELSE
    call messy(E,'$N Double precision a=IEEE_REM(5, \pi) INCORRECTLY&
      & does not satisfy a + 2 * \pi==5.')
        ALLPASS=.FALSE.
  END IF
  !----------------------------------------------------------------------
  CALL IEEE_SET_STATUS(SAVE_SPOT)        ! Restore starting state
  CALL IEEE_SET_ROUNDING_MODE(IEEE_NEAREST)
  ! Tests for IEEE_RINT, single.
  SB=1.5E0
  SA=IEEE_RINT(SB) ! With rounding to nearest, should == 2.
  FLAG_VALUEA=(SA==2.E0)
  SB=0.5E0
  SA=IEEE_RINT(SB) ! With rounding to nearest, should == 0.
  FLAG_VALUEA=(SA==0.E0)

  CALL IEEE_SET_ROUNDING_MODE(IEEE_TO_ZERO)
  SA=IEEE_RINT(SB) ! With rounding to zero, should == 0.
  FLAG_VALUEA=(SA==0.E0) .and. FLAG_VALUEA

  CALL IEEE_SET_ROUNDING_MODE(IEEE_DOWN)
  SA=IEEE_RINT(SB) ! With rounding down, should == 0.
  FLAG_VALUEA=(SA==0.E0) .and. FLAG_VALUEA

  CALL IEEE_SET_ROUNDING_MODE(IEEE_UP)
  SA=IEEE_RINT(SB) ! With rounding up, should == 1.
  FLAG_VALUEA=(SA==1.E0) .and. FLAG_VALUEA
  IF(FLAG_VALUEA) THEN
    call messy(E,'$N Single precision IEEE_RINT of 0.5, 1.5 is correct for$N&
      & rounding modes: IEEE_NEAREST, IEEE_DOWN, IEEE_UP and IEEE_TO_ZERO.')
  ELSE
    call messy(E,'$N Single precision IEEE_RINT of 0.5, 1.5 is INCORRECT for$N&
      & one of the rounding modes: IEEE_NEAREST, IEEE_DOWN, IEEE_UP and IEEE_TO_ZERO.')
        ALLPASS=.FALSE.
  END If
  !----------------------------------------------------------------------
  CALL IEEE_SET_STATUS(SAVE_SPOT)        ! Restore starting state
  CALL IEEE_SET_ROUNDING_MODE(IEEE_NEAREST)
  ! Tests for IEEE_RINT, double.
  DB=1.5D0
  DA=IEEE_RINT(DB) ! With rounding to nearest, should == 2.
  FLAG_VALUEA=(DA==2.D0)
  DB=0.5D0
  DA=IEEE_RINT(DB) ! With rounding to nearest, should == 0.
  FLAG_VALUEA=(DA==0.D0)

  CALL IEEE_SET_ROUNDING_MODE(IEEE_TO_ZERO)
  DA=IEEE_RINT(DB) ! With rounding to zero, should == 0.
  FLAG_VALUEA=(DA==0.D0) .and. FLAG_VALUEA

  CALL IEEE_SET_ROUNDING_MODE(IEEE_DOWN)
  DA=IEEE_RINT(DB) ! With rounding down, should == 0.
  FLAG_VALUEA=(DA==0.E0) .and. FLAG_VALUEA

  CALL IEEE_SET_ROUNDING_MODE(IEEE_UP)
  DA=IEEE_RINT(DB) ! With rounding up, should == 1.
  FLAG_VALUEA=(DA==1.D0) .and. FLAG_VALUEA
  IF(FLAG_VALUEA) THEN
    call messy(E,'$N Double precision IEEE_RINT of 0.5, 1.5 is correct for$N&
      & rounding modes: IEEE_NEAREST, IEEE_DOWN, IEEE_UP and IEEE_TO_ZERO.')
  ELSE
    call messy(E,'$N Double precision IEEE_RINT of 0.5, 1.5 is INCORRECT for$N&
      & one of the rounding modes: IEEE_NEAREST, IEEE_DOWN, IEEE_UP and IEEE_TO_ZERO.')
        ALLPASS=.FALSE.
  END If
  !----------------------------------------------------------------------
  CALL IEEE_SET_STATUS(SAVE_SPOT)        ! Restore starting state
  ! Tests for IEEE_SCALB, single.
  SB=0.E0
  SA=IEEE_SCALB(SB,1)
  FLAG_VALUEA=(SA==0.E0) ! Scaling 0 by any power is 0.
  SB=1.E0
  SA=IEEE_SCALB(SB,-1) ! Scale 1 by 2**-1 = .5
  FLAG_VALUEA=(SA==0.5E0) .and. FLAG_VALUEA
  SA=IEEE_SCALB(SA,1)  ! Scale .5 by 2**1 = 1
  FLAG_VALUEA=(SA==1.E0) .and. FLAG_VALUEA

  call ieee_get_underflow_mode(FLAG_VALUEC) ! Save gradual underflow setting
  call ieee_set_underflow_mode(.FALSE.) ! No gradual underflow 
  ! This next result may not be represented exactly.
  SA = IEEE_SCALB(TINY(SB), -3)
  ! The underflow exception should signal.
  CALL IEEE_GET_FLAG(IEEE_UNDERFLOW, FLAG_VALUEB)
  IF(FLAG_VALUEB) THEN
    call messy(E,'$N Single precision a=IEEE_SCALB(TINY(0.E0),-3)$N&
      & correctly signals IEEE_UNDERFLOW, with gradual underflow disabled.')
  ELSE
    call messy(E,'$N Single precision a=IEEE_SCALB(TINY(0.E0),-3)$N&
      & should signal IEEE_UNDERFLOW, with gradual underflow disabled.$N&
      & It DOES NOT.  F2003 standard violation.')
        ALLPASS=.FALSE.
  END IF
  ! This next result is compared with the IEEE_SCALB result.
  SC=TINY(SB)*0.125E0
  ! Since gradual underflow is not enabled, the
  ! logical value of (SC < SA) should be .FALSE.
  FLAG_VALUEA=(.not.(SC < SA)) .and. FLAG_VALUEA
  IF(FLAG_VALUEA) THEN
    call messy(E,'$N Single precision IEEE_SCALB of 1.0 is correct.$N&
      & IEEE_SCALB(TINY(0.E0),-3) with gradual underflow disabled is correct.')
  ELSE
    call messy(E,'$N Single precision IEEE_SCALB of 1.0 is INCORRECT or$N&
      & IEEE_SCALB(TINY(0.E0),-3) with gradual underflow disabled is INCORRECT.')
        ALLPASS=.FALSE.
  END If

  call ieee_set_underflow_mode(.TRUE.) ! There is gradual underflow enabled 
  ! This next result should be represented exactly.
  SA = IEEE_SCALB(TINY(SB), -3)
  ! The underflow exception should signal.
  CALL IEEE_GET_FLAG(IEEE_UNDERFLOW, FLAG_VALUEB)
  IF(FLAG_VALUEB) THEN
    call messy(E,'$N Single precision a=IEEE_SCALB(TINY(0.E0),-3)$N&
      & correctly signals IEEE_UNDERFLOW, with gradual underflow enabled.')
  ELSE
    call messy(E,'$N Single precision a=IEEE_SCALB(TINY(0.E0),-3)$N&
      & should signal IEEE_UNDERFLOW, with gradual underflow enabled.$N&
      & It DOES NOT.  F2003 standard violation.')
        ALLPASS=.FALSE.
  END IF
  ! This next result is compared with the IEEE_SCALB result.
  SC=TINY(SB)*0.125E0
  ! Since gradual underflow is enabled, the
  ! logical value of (SC == SA) should be .TRUE.
  FLAG_VALUEA=(SC == SA) .and. FLAG_VALUEA
  IF(FLAG_VALUEA) THEN
    call messy(E,'$N Single precision IEEE_SCALB of 1.0 is correct.$N&
      & IEEE_SCALB(TINY(0.E0),-3) with gradual underflow enabled is correct.')
  ELSE
    call messy(E,'$N Single precision IEEE_SCALB of 1.0 is INCORRECT or$N&
      & IEEE_SCALB(TINY(0.E0),-3) with gradual underflow enabled is INCORRECT.')
        ALLPASS=.FALSE.
  END If
  !----------------------------------------------------------------------
  CALL IEEE_SET_STATUS(SAVE_SPOT)        ! Restore starting state
  ! Tests for IEEE_SCALB, double.
  DB=0.D0
  DA=IEEE_SCALB(DB,1)
  FLAG_VALUEA=(DA==0.D0) ! Scaling 0 by any power is 0.
  DB=1.D0
  DA=IEEE_SCALB(DB,-1) ! Scale 1 by 2**-1 = .5
  FLAG_VALUEA=(DA==0.5D0) .and. FLAG_VALUEA
  DA=IEEE_SCALB(DA,1)  ! Scale .5 by 2**1 = 1
  FLAG_VALUEA=(DA==1.D0) .and. FLAG_VALUEA

  call ieee_set_underflow_mode(.FALSE.) ! No gradual underflow 
  ! This next result may not be represented exactly.
  DA = IEEE_SCALB(TINY(DB), -3)
  ! The underflow exception should signal.
  CALL IEEE_GET_FLAG(IEEE_UNDERFLOW, FLAG_VALUEB)
  IF(FLAG_VALUEB) THEN
    call messy(E,'$N Double precision a=IEEE_SCALB(TINY(0.D0),-3)$N&
      & signals IEEE_UNDERFLOW, with gradual underflow disabled.')
  ELSE
    call messy(E,'$N Double precision a=IEEE_SCALB(TINY(0.D0),-3)$N&
      & should signal IEEE_UNDERFLOW, with gradual underflow disabled.$N&
      & It DOES NOT.  F2003 standard violation.')
        ALLPASS=.FALSE.
  END IF
  ! This next result is compared with the IEEE_SCALB result.
  DC=TINY(DB)*0.125D0
  ! Since gradual underflow is not enabled, the
  ! logical value of (SC < SA) should be .FALSE.
  FLAG_VALUEA=(.not.(DC < DA)) .and. FLAG_VALUEA
  IF(FLAG_VALUEA) THEN
    call messy(E,'$N Double precision IEEE_SCALB of 1.0 is correct.$N&
      & IEEE_SCALB(TINY(0.D0),-3) with gradual underflow disabled is correct.')
  ELSE
    call messy(E,'$N Double precision IEEE_SCALB of 1.0 is INCORRECT or$N&
      & IEEE_SCALB(TINY(0.D0),-3) with gradual underflow disabled is INCORRECT.')
        ALLPASS=.FALSE.
  END If

  call ieee_set_underflow_mode(.TRUE.) ! There is gradual underflow enabled 
  ! This next result should be represented exactly.
  DA = IEEE_SCALB(TINY(DB), -3)
  ! The underflow exception should signal.
  CALL IEEE_GET_FLAG(IEEE_UNDERFLOW, FLAG_VALUEB)
  IF(FLAG_VALUEB) THEN
    call messy(E,'$N Double precision a=IEEE_SCALB(TINY(0.D0),-3)$N&
      & signals IEEE_UNDERFLOW, with gradual underflow enabled.')
  ELSE
    call messy(E,'$N Double precision a=IEEE_SCALB(TINY(0.D0),-3)$N&
      & should signal IEEE_UNDERFLOW, with gradual underflow enabled.$N&
      & It DOES NOT.  F2003 standard violation.')
        ALLPASS=.FALSE.
  END IF

  ! This next result is compared with the IEEE_SCALB result.
  DC=TINY(DB)*0.125D0
  ! Since gradual underflow is enabled, the
  ! logical value of (SC == SA) should be .TRUE.
  FLAG_VALUEA=(DC == DA) .and. FLAG_VALUEA
  IF(FLAG_VALUEA) THEN
    call messy(E,'$N Double precision IEEE_SCALB of 1.0 is correct.$N&
      & IEEE_SCALB(TINY(0.D0),-3) with gradual underflow enabled is correct.')
  ELSE
    call messy(E,'$N Double precision IEEE_SCALB of 1.0 is INCORRECT or$N&
      & IEEE_SCALB(TINY(0.D0),-3) with gradual underflow enabled is INCORRECT.')
        ALLPASS=.FALSE.
  END If
  call ieee_set_underflow_mode(FLAG_VALUEC) ! Restore default underflow mode 
  !----------------------------------------------------------------------
  CALL IEEE_SET_STATUS(SAVE_SPOT)        ! Restore starting state
  ! Summary, indicating if all tests pass or else examine details.
  IF(ALLPASS) THEN
     call messy(E,'$N*** All Tests PASSED for non-intrinsic IEEE modules.$N')
  ELSE
    call messy(E,'$N*** Some Tests DID NOT PASS for non-intrinsic IEEE modules. &
    & Look for the details in the output lists.$N')
  END IF
END PROGRAM IEEE_TESTS
