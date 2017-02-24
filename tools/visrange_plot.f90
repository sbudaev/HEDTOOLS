!> Produce a plot of the relationship between the fish size
!! and its visual range for the predator
!! make SRC=visrange_plot.f90 OUT=visrange_plot
!! $Id$
!-------------------------------------------------------------------------------
module PRECIS                     !> @note Module copies data from `COMMONDATA`.

  ! PRECISION CONTROL FOR REAL TYPE AND IEEE FLOAT MATH in the model

  !> Standard precision for real data type. We first define 32, 64 and
  !! 128 bit real kinds.
  !! @warning `HEDTOOLS` cannot accept precision higher than kind 8 so far.
  !!          So 128 bit reals are for example only here. Have to implement
  !!          higher precision `HEDTOOLS` routines if they are really used.
  integer, parameter, public :: S_PREC_32  = selected_real_kind( 6,   37)
  integer, parameter, public :: D_PREC_64  = selected_real_kind(15,  307)
  integer, parameter, public :: Q_PREC_128 = selected_real_kind(33, 4931)

  !> @brief   Definition of the **standard** real type precision (*SRP*).
  !> @detail  `SRP` is defined as the standard precision that should normally
  !!          be used for all real variables and constants. **SRP** stands for
  !!          **Standard Real Precision** (Naming note: const name should be
  !!          short to not produce too long real definitions, e.g.
  !!          `real(SRP) :: alpha`).
  !! @warning All float (and other) constants should ideally be defined in the
  !!          definition section of COMMONDATA or another module, **not** in
  !!          the code. It is for easier maintainability and precision control.
  !! @warning **All standard real variables** should be defined as:
  !!          `real(SRP) :: real_var`.
  !!          **Literal constants** should normally add `_SRP`: `1.234_SRP`
  !!          (although it is less crucial).
  integer, parameter, public :: SRP = S_PREC_32

  !> Definition of the **high** real precision (**HRP**).  This real type kind
  !! is used in pieces where a higher level of FPU precision is required, e.g.
  !! to avoid overflow/underflow and similar errors.
  integer, parameter, public :: HRP = Q_PREC_128 ! D_PREC_64

  !> In some (perhaps quite rare) cases of exponentiation we may also need huge
  !! integers, those in 64 bit would probably be enough. So whenever we need
  !! such a big integer, declare it as: `integer(LONG) :: bignum`
  !! @warning HEDTOOLS **do not** currently work with these `LONG`
  !!          kind integers. So they are only for "internal"-calculation use.
  !!          Alternatively, use the intrinsic function `int` to convert to the
  !!          default integer type inline before use, e.g.:
  !!          `TOSTR(int(max_permutations))`.
  integer, parameter, public :: LONG = selected_int_kind(16)

end module PRECIS

module CONST                      !> @note Module copies data from `COMMONDATA`.

  use PRECIS
  implicit none

  !> Maximum above-surface light intensity at midday, DAYLIGHT=500.0.
  !! @note Can be deterministic or stochastic.
  real(SRP), parameter, public :: DAYLIGHT=500._SRP

  !> Some parameters should never be below zero. But when set at random, can
  !! still get under zero values. In such cases they should be set a minimum,
  !! an "essentially zero" value. Here set as `epsilon` function, i.e. the
  !! smallest `real` number E such that @f$ 1 + E > 1 @f$
  !! E.g. such values can be treated as `max(ZERO, function())`
  real(SRP), parameter, public :: ZERO = epsilon(0._SRP) ! 32bit 1.19209290E-07

  !> The length of standard character string labels. We use labels for various
  !! objects, like alleles, perceptual and neural components / bundles etc.
  !! For simplicity, they all have the same length. It should be big enough to
  !! fit the longest whole label
  integer, parameter, public :: LABEL_LENGTH = 14

  !> Beam attenuation coefficient of water (m-1),BEAMATT = 1.0.
  real(SRP), parameter, public :: BEAMATT=1._SRP

  !> Saturation parameter of eye (Ke) (uE m-2 s-1), EYESAT=500.0.
  real(SRP), parameter, public :: EYESAT=500._SRP

  !> Inherent contrast of prey, CONTRAST =1.0.
  real(SRP), parameter, public :: PREYCONTRAST_DEFAULT = 1._SRP

  !> Area of prey (m2), PREYAREA = 3.E-6.
  real(SRP), parameter, public :: PREYAREA_DEFAULT=3.E-6_SRP

  !> Dimensionless descriptor of fish eye quality, VISCAP=1.6E6.
  real(SRP), parameter, public :: VISCAP=1.6E6_SRP

  !> @brief  The **PI** number.
  !! @detail Pi number @f$ \pi = 4.0 \cdot tg(1.0) @f$ [`4.*atan(1.)`],
  !!         numerically equal to `PI=3.1415926_SRP`.
  real(SRP), parameter :: PI=4._SRP*atan(1._SRP)

end module CONST

!=============================================================================
!> The subroutines srgetr, easyr and deriv should naturally form a
!! submodule, but it is not used here as submodules are a F2008 feature
!! not supported by all compiler systems.
module AKVISRANGE ! Dag AKsnes VISual RANGEe utilities.

  ! WARNING: The HRP 128 precision model should be used, anything smaller
  !          can produce FPU overflows! Precision is defined in COMMONDATA.
  !integer, parameter, public :: Q_PREC_128 = selected_real_kind(33, 4931)
  !integer, parameter, public :: HRP = Q_PREC_128 ! D_PREC_64

  use PRECIS
  use CONST
  use BASE_UTILS
  implicit none

  contains

  !-----------------------------------------------------------------------------
  !> Obtain visual range by solving the non-linear equation
  !! by means of Newton-Raphson iteration and derivation in
  !! subroutine `deriv`. Initial value is calculated in `easyr`.
  !! The calculation is based on the model described in Aksnes &
  !! Utne (1997) Sarsia 83:137-147.
  !! @note Programmed and tested 29 January 2001 Dag L Aksnes.
  !! @note This subroutine is left with only the most crucial changes.
  !!       (a) added `HRP` precision specifier (128 bit precision model)
  !!           to real type specifiers and `_HRP` for literal constants;
  !!       (b) restored diagnostic IER output from archival Hed11.f90.
  !!       (c) added explicit `intent` and declared the procedures as
  !!           `pure` that is required for being parallel-friendly.
  !! @note In the new code it should be wrapped into an OO friendly function.
  elemental subroutine srgetr(r, c, C0, Ap, Vc, Ke, Eb, IER)
  ! Input parameters
  !     RST       : start value of r calculated by `easyr`
  !     c         : beam attenuation coefficient (m-1)
  !     C0        : prey inherent contrast
  !     Ap        : prey area (m^2)
  !     Vc        : parameter characterising visual capacity (d.l.)
  !                 this parameter is denoted E' in Aksnes & Utne
  !     Ke        : saturation parameter (uE m-2 s-1)
  !     Eb        : background irradiance at depth DEPTH
  ! Output parameters
  !     F1     : function value of equation in `deriv`
  !     FDER   : the derivative of the function
  !     r      : the predator's visual range (when F1=0)
  !     IER    : = 1, No convergence after IEND steps. Error return.
  !              = 2, Return in case of zero divisor.
  !              = 3, r out of allowed range (negative)
  !              = 0, valid r returned
  real(HRP), intent(in)   :: c, C0, Ap, Vc, Ke, Eb
  real(HRP), intent(out)  :: r
  integer, optional, intent(out) :: IER

  real(HRP) :: AS, EPS, RST, TOL, TOLF, F1, FDER, DX
  integer   :: IEND, I

  !.............................................................................

    ! Initial guess of visual range (RST)
    call easyr(RST,C0,Ap,Vc,Ke,Eb)

    ! Upper boundary of allowed error of visual range.
    ! SB: The error cannot be smaller than the epsilon parameter
    !     setting limits on real type numerical precision
    !     for the CPU FPU.
    EPS = max(.000001_SRP, ZERO)
    ! Maximum number of iteration steps
    IEND = 100

    ! Prepare iteration
    r = RST
    TOL = r
    !> @note `deriv` is **kind 8 real** type. We need extra precision here
    !!       to avoid overflow FPU errors.
    call deriv(r,F1,FDER,c,C0,Ap,Vc,Ke,Eb)
    TOLF = 100._SRP * EPS

    ! Start iteration expmt
    ! @warning Cannot probably be converted to `do concurrent` due to exits
    !!         from the loop.
    do 6 I = 1, IEND
      if (F1 == 0._SRP) goto 7

      ! Equation is not satisfied by r
      if (FDER == 0._SRP) goto 8

      ! Iteration is possible
      DX = F1/FDER
      r = r-DX

      ! Test on allowed range
      if (r .LT. 0._SRP) goto 9

      TOL = r
      !> @note `deriv` is **kind 8 real** type. We need extra precision here
      !!       to avoid overflow FPU errors.
      call deriv(r,F1,FDER,c,C0,Ap,Vc,Ke,Eb)

      ! Test on satisfactory accuracy
      TOL = EPS
      AS = abs(r)
      if ((AS-1._SRP) > 0) TOL = TOL*AS
      TOL = TOL*AS
      if ((abs(DX)-TOL) > 0._SRP) goto 6
      if ((abs(F1)-TOLF) .LE. 0._SRP) goto 7
  6   continue

      ! No convergence after IEND steps. Error return.
      if (present(IER)) IER = 1
  7   return
      ! Return in case of zero divisor
  8   if (present(IER)) IER = 2
      return
      ! r out of allowed range (negative)
  9   if (present(IER)) IER = 3
      return

  end subroutine srgetr

  !-----------------------------------------------------------------------------
  !> Obtain a first estimate of visual range by using a simplified
  !! expression of visual range.
  !! @note This subroutine is left almost intact, only (a) added
  !!       `SRP` for real type (`SRP` is *standard realprecision*
  !!       and is defined in COMMONDATA).
  elemental subroutine easyr(r, C0, Ap, Vc, Ke, Eb)
    real(HRP), intent(out) :: r
    real(HRP), intent(in)  :: C0, Ap, Vc, Ke, Eb
    real(HRP) :: R2
    ! See the calling routine `srgetr` for explanation of parameters
    R2 = abs(C0)*Ap*Vc*Eb/(Ke+Eb)
    r  = sqrt(R2)
    return
  end subroutine easyr

  !-----------------------------------------------------------------------------
  !> Derivation of equation for visual range of a predator.
  !! @note This is a high precision version. But higher precision alone is
  !!       **not** sufficient to prevent numerical exponentiation overflow.
  !! @note This subroutine is left almost intact, only (a) added
  !!       `HRP` for literal constants and real type (`HRP` is the *high
  !!       real precision* and is defined in COMMONDATA), (b) added numerical
  !!       overflow safeguard code based on `MAX_LOG` exponentiability limit;
  !!       added logging of overflow using `LOG_MSG`.
  elemental subroutine deriv(r, F1, FDER, c, C0, Ap, Vc, Ke, Eb)
    ! Input parameters
    !     See explanation in calling routine
    ! Output parameters
    !     F1     : function value of equation in `deriv`
    !     FDER   : the derivative of the function
    !     r      : the predator's visual range (when F1=0)
    !
    !    The function and the derivative is calculated on the basis of the
    !    log-transformed expression
    real(HRP), intent(inout) :: r
    real(HRP), intent(out)   :: F1, FDER
    real(HRP), intent(in)    :: c, C0, Ap, Vc, Ke, Eb

    real(HRP) :: FR1, FR2

    !> `MAX_LOG` is a parameter determining the safe limit of `exp` function
    !! overflow in the current float point precision model, well below this.
    !! We cannot calculate precise exponent of a value exceeding this
    !! parameter. The maximum possible exponentiation-able value is set
    !! to the maximum **128-bit** real value kind `Q_PREC_128`, this bottom
    !! line value would set a safe limit for **64-bit** `HRP` calculations.
    !! A benefit of this approach is that it doesn't require IEEE exception
    !! handling with not fully portable optional IEEE modules.
    !! @note  The real 128 bit limit (Q_PREC_128) is sufficient to calculate
    !!        visual range up to the fish length of approximately 700 cm
    !!        (area 153.9 cm2). At this level, the maximum visual range is
    !!        1343.34 cm. This would be sufficient for non-whales.
    real(HRP), parameter :: HUGE_REAL = huge(0.0_HRP)
    real(HRP), parameter :: MAX_LOG = log(HUGE_REAL)

    !> PROCNAME is the procedure name for logging and debugging (with MODNAME).
    character(len=*), parameter :: PROCNAME = "(deriv)"

    FR2=log(abs(C0)*Ap*Vc)

    !> @note    Here `exp(c*r)` can result in huge FPU overflow (Infinity),
    !!          so we now check if the exponent is likely to be so high and
    !!          **if yes**, just set the huge ceiling.
    !! @warning The resulting calculations are then most probably grossly
    !!          wrong but we nonetheless avoid FPU runtime error: incorrect
    !!          arithmetic calculations.
    if (c*r < MAX_LOG) then
      FR1=log(((Ke+Eb)/Eb)*r*r*exp(c*r))
    else
      FR1=HUGE_REAL
      !> Subroutine `LOG_MSG` is defined in `LOGGER` module. It reports
      !! numerical overflow errors.
      !! @warning `LOG_MSG` is **disabled** here as it cannot be **pure** and
      !!          hampers purifying `deriv` for parallel processing.
      !!          A negative side is no error report logging. But the `HRP`
      !!          64 bit precision model seems to work okay. There is also
      !!          a guarantee against overflow and gross errors with the
      !!          `HUGE_REAL` parameter.
      !call LOG_MSG (  "ERROR in " // PROCNAME // ":  FR1 overflow "  //      &
      !                "exceeding " // TOSTR(HUGE_REAL) // " limit :" //      &
      !                " c*r =" // TOSTR(c*r)  )
    end if

    F1 = FR1-FR2
    FDER = c + 2.0_HRP/r
    return

  end subroutine deriv

end module AKVISRANGE

!=============================================================================

module MODCOMPS                 !> @note Module copies data from `COMMONDATA`.

  use PRECIS
  use CONST
  use AKVISRANGE
  use BASE_UTILS
  implicit none

  !-----------------------------------------------------------------------------
  !> @brief  Convert cm to m.
  !! @detail This is needed because some of the the sizes are expressed
  !!         in cm but certain functions (e.g. visual range estimator SRGETR)
  !!         require parameters in m. E.g. FOOD_ITEM_SIZE_DEFAULT is set
  !!         around 0.5 cm while SRGETR requires prey area in m².
  interface cm2m
    module procedure cm2m_r
    module procedure cm2m_i
  end interface cm2m

  !> @brief  Convert m to cm.
  !! @detail This is needed because some of the the sizes are expressed
  !!         in cm but certain functions (e.g. visual range estimator SRGETR)
  !!         require parameters in m. Therefore, conversion back from visual
  !!         range function should use these functions.
  interface m2cm
    module procedure m2cm_r
    module procedure m2cm_i
  end interface m2cm

  !> Calculate visual range of predator  using Dag Aksnes's procedures
  !! `srgetr`, `easyr` and `deriv`.
  interface visual_range
    module procedure visual_range_scalar
    module procedure visual_range_vector
  end interface visual_range

  contains
  !-----------------------------------------------------------------------------
  !> @brief  Convert cm to m.
  !! @note   This version gets real type argument.
  !! @detail This is needed because some of the the sizes are expressed
  !!         in cm but certain functions (e.g. visual range estimator SRGETR)
  !!         require parameters in m. E.g. FOOD_ITEM_SIZE_DEFAULT is set
  !!         around 0.5 cm while SRGETR requires prey area in m².
  elemental function cm2m_r(value_cm) result(value_m)
    real(SRP), intent(in) :: value_cm           !> @param value_cm value in cm
    real(SRP) :: value_m                        !> @returns value in m
    real(SRP), parameter :: CONV_F = 0.01_SRP   !> conversion factor

    value_m = value_cm * CONV_F

  end function cm2m_r

  !-----------------------------------------------------------------------------
  !> @brief  Convert cm to m.
  !! @note   This version gets integer argument (albeit returns real).
  elemental function cm2m_i(value_cm) result(value_m)
    integer, intent(in) :: value_cm             !> @param value_cm value in cm
    real(SRP) :: value_m                        !> @returns value in m
    real(SRP), parameter :: CONV_F = 0.01_SRP   !> conversion factor

    value_m = real(value_cm, SRP) * CONV_F

  end function cm2m_i
  !-----------------------------------------------------------------------------
  !> @brief  Convert m to cm.
  !! @note   This version gets real type argument.
  elemental function m2cm_r(value_m) result(value_cm)
    real(SRP), intent(in) :: value_m           !> @param value_cm value in cm
    real(SRP) :: value_cm                        !> @returns value in m
    real(SRP), parameter :: CONV_F = 100.0_SRP   !> conversion factor

    value_cm = value_m * CONV_F

  end function m2cm_r

  !-----------------------------------------------------------------------------
  !> @brief  Convert m to cm.
  !! @note   This version gets integer argument (albeit returns real).
  elemental function m2cm_i(value_m) result(value_cm)
    integer, intent(in) :: value_m             !> @param value_cm value in cm
    real(SRP) :: value_cm                        !> @returns value in m
    real(SRP), parameter :: CONV_F = 100.0_SRP   !> conversion factor

    value_cm = real(value_m, SRP) * CONV_F

  end function m2cm_i

  !-----------------------------------------------------------------------------
  !> @brief  Calculate a circle area.
  !! @detail Calculate the area of the circle.
  !! @note   Food items are viewed as circular objects with the default
  !!         **radius** equal to the **size** `FOOD_ITEM_SIZE_DEFAULT` or
  !!         `FOOD_ITEM_MEAN_SIZE`.
  elemental function carea(R) result (area_circ)
    real(SRP), intent(in) :: R
    real(SRP) :: area_circ

    area_circ = PI * R * R

  end function carea

  !-----------------------------------------------------------------------------
  !> Wrapper for calculating *visual range of a fish predator* using
  !! the Dag Aksnes's procedures `srgetr`, `easyr` and `deriv`.
  !! @warning The measurement unit here is meter. Might need conversion if
  !!          other units are used elsewhere!
  !! @note    Note that this is a scalar version
  !! @note    Example call:
  !!             `visual_range( light_depth( 30., light_surface(100,.TRUE.) ) )`
  function visual_range_scalar(irradiance, prey_area, prey_contrast)          &
                        result(visual_range_calculate)

    !> PROCNAME is the procedure name for logging and debugging (with MODNAME).
    character(len=*), parameter :: PROCNAME = "(visual_range_scalar)"

    !> @param irradiance background irradiance at specific depth
    real(SRP), intent(in) :: irradiance

    !> @param prey_area prey area, m^2
    real(SRP), optional, intent(in) :: prey_area

    !> @param prey_contrast optional prey inherent contrast or default
    !!        parameter if not present.
    real(SRP), optional, intent(in) :: prey_contrast

    !> @return Returns visual range of the fish predator
    real(SRP) :: visual_range_calculate

    !> Local high precision value of `visual_range_calculate`, explicitly
    !! converted to `SRP` at the end.
    real(HRP) :: visual_range_HRP_here

    !> Local error flag
    integer :: error_flag

    !> Local error message
    character(len=LABEL_LENGTH), parameter, dimension(3) ::                   &
            error_msg = ["NO_CONVERGENCE", "DIVISION_ZERO ", "NEGATIVE_RANGE"]

    !> Local copies of optional parameters
    real(SRP) :: prey_area_here, prey_contrast_here

    !> First, check if background irradiance is near zero and
    !! return zero visual range.
    if(irradiance < ZERO) then
      visual_range_calculate = 0.0_SRP
      return
    end if

    if (present(prey_area)) then
      prey_area_here=prey_area
    else
      prey_area_here=PREYAREA_DEFAULT
    end if

    !> Initialise the error flag.
    error_flag = 0

    if (present(prey_contrast)) then
      prey_contrast_here=prey_contrast
    else
      prey_contrast_here=PREYCONTRAST_DEFAULT
    end if

    !> @note Note that `srgetr` and the whole computational backend are now
    !!       in `HRP` precision to avoid numerical overflow errors.
    call srgetr(  visual_range_HRP_here,                                    &
                  real(BEAMATT,HRP), real(prey_contrast_here,HRP),          &
                  real(prey_area_here,HRP), real(VISCAP,HRP),               &
                  real(EYESAT,HRP), real(irradiance,HRP), error_flag  )

    !> @note The visual range calculation backend `srgetr` seems
    !!       computationally suboptimal and with large object size
    !!       leads to numerical overflow, so the visual range is -Infinity.
    !!       This is corrected in two steps. (1) in `deriv`, a problematic
    !!       part of the computation now checks proactively for potential
    !!       overflow (comparing to `log(huge())` for the current FPU
    !1       precision level), so no NaNs or Infinity are produced and
    !!       no FPU invalid arithmetic errors occur; (2) Because the
    !!       returned visual range is still erroneous (huge negative value),
    !!       a crude approximation based on `easyr` is used as the final
    !!       value. These cases are reported to the logger.
    if (error_flag /= 0) then
      print *, "ERROR in " , PROCNAME ,                                 &
                   ": (srgetr) issued error code " ,                         &
                   TOSTR(error_flag) , " :: " , error_msg(error_flag) ,    &
                   ". Object area (prey_area)=" , TOSTR(prey_area) ,        &
                   ", object contrast (prey_contrast)=" ,                    &
                      (prey_contrast) ,                                 &
                   ". Visual range calculated as " ,                         &
                      (visual_range_HRP_here) , " m"
      !> If the visual range value returned by `srgetr` is negative, we reset
      !! it to an initial approximation by `easyr`. If the easyr approximation
      !! still returns underflow value, set it to `ZERO`, an essentially zero,
      !! with the `SRP` precision.
      if (visual_range_HRP_here < ZERO ) then
        !> @note Note that `easyr` and the whole computational backend are now
        !!       in `HRP` precision to avoid numerical overflow errors.
        call easyr( visual_range_HRP_here,                                    &
                    real(prey_contrast_here,HRP),real(prey_area_here,HRP),    &
                    real(VISCAP,HRP), real(EYESAT,HRP), real(irradiance,HRP) )
        print *, "ERROR in " , PROCNAME , ":  Visual range "  ,        &
                     "recalculated using (easyr) approximation: " ,          &
                     (visual_range_HRP_here) , " m"
        if (visual_range_HRP_here < ZERO) then
          visual_range_HRP_here = ZERO
          print *, "ERROR in " , PROCNAME , ":  Visual range "  ,     &
                     "recalculated using *easyr* approximation still " ,     &
                     "below ZERO, set to " , (visual_range_HRP_here) , &
                     " m"
        end if
      end if
    end if

    !> finally, do explicit conversion from `HRP` to `SRP`.
    visual_range_calculate = real(visual_range_HRP_here, SRP)

  end function visual_range_scalar

  !-----------------------------------------------------------------------------
  !> Wrapper for calculating *visual range of a fish predator* using
  !! the Dag Aksnes's procedures `srgetr`, `easyr` and `deriv`.
  !! @warning The measurement unit here is meter. Might need conversion if
  !!          other units are used elsewhere!
  !! @note This is a vector version, `prey_area` is mandatory and also defines
  !!       the vector size for all other vector parameters including the
  !!       returned function value vector.
  !! @note This is useful for selecting among a swarm of prey with different
  !!       sizes when vector is processed.
  function visual_range_vector(irradiance, prey_area, prey_contrast)          &
                        result(visual_range_calculate)

   !> PROCNAME is the procedure name for logging and debugging (with MODNAME).
    character(len=*), parameter :: PROCNAME = "(visual_range_vector)"

    !> @param irradiance background irradiance at specific depth
    real(SRP), intent(in) :: irradiance

    !> @param prey_area prey area, m^2
    !! @note  Mandagtory parameter.
    real(SRP), dimension(:), intent(in) :: prey_area

    !> @param prey_contrast optional prey inherent contrast or default
    !!        parameter if not present.
    real(SRP), optional, dimension(size(prey_area)), intent(in) :: prey_contrast

    !> @return Returns visual range of the fish predator
    real(SRP), dimension(size(prey_area)) :: visual_range_calculate

    !> Local copies of optional parameters
    real(SRP), dimension(size(prey_area)) :: prey_contrast_here

    !> Local counter
    integer :: i

    if (present(prey_contrast)) then
      prey_contrast_here=prey_contrast
    else
      prey_contrast_here=PREYCONTRAST_DEFAULT
    end if

    do i=1, size(visual_range_calculate)
      visual_range_calculate(i) = visual_range_scalar(irradiance,             &
                                      prey_area(i), prey_contrast_here(i))
    end do

  end function visual_range_vector

  !-----------------------------------------------------------------------------
  !> New parallel-ready visual range function making use the elemental
  !! computation backend. NOTE: It is simplified, e.g. no error reporting is
  !! done, debugging the old code has shown it works okay up to the MAX_LOG
  !! limit.
  elemental function visual_range_new(irradiance, prey_area, prey_contrast)    &
                                                result (visual_range_calculate)

    !> @param irradiance background irradiance at specific depth
    real(SRP), intent(in) :: irradiance

    !> @param prey_area prey area, m^2
    real(SRP), optional, intent(in) :: prey_area

    !> @param prey_contrast optional prey inherent contrast or default
    !!        parameter if not present.
    real(SRP), optional, intent(in) :: prey_contrast

    !> @return Returns visual range of the fish predator
    real(SRP) :: visual_range_calculate

    !> Local high precision value of `visual_range_calculate`, explicitly
    !! converted to `SRP` at the end.
    real(HRP) :: visual_range_HRP_here

    !> Local copies of optional parameters
    real(SRP) :: prey_area_here, prey_contrast_here

    if (present(prey_area)) then
      prey_area_here=prey_area
    else
      prey_area_here=PREYAREA_DEFAULT
    end if

    if (present(prey_contrast)) then
      prey_contrast_here=prey_contrast
    else
      prey_contrast_here=PREYCONTRAST_DEFAULT
    end if

    !> @note Note that `srgetr` and the whole computational backend are now
    !!       in `HRP` precision to avoid numerical overflow errors.
    call srgetr(  visual_range_HRP_here,                                    &
                  real(BEAMATT,HRP), real(prey_contrast_here,HRP),          &
                  real(prey_area_here,HRP), real(VISCAP,HRP),               &
                  real(EYESAT,HRP), real(irradiance,HRP)   )


    !> finally, do explicit conversion from `HRP` to `SRP`.
    visual_range_calculate = real(visual_range_HRP_here, SRP)

  end function visual_range_new

end module MODCOMPS

!=============================================================================
! make GRAPHLIB=-lpgplot
program VISRANGE_PLOT

use, intrinsic :: ISO_FORTRAN_ENV, only : ERROR_UNIT
use PRECIS
use CONST
use AKVISRANGE
use MODCOMPS
use BASE_UTILS
use CSV_IO

implicit none

  integer, parameter :: MAXSCALE=100
  real(SRP), dimension(MAXSCALE) :: visrange, object_length, object_area
  real(SRP) :: irradiance
  integer :: i

  ! Output devices. See PGPLOT docs or set ? for runtime choice.
  ! NOTE: /XWINDOW is the standard native X11 graphics device for Unix systems.
  !       /GW is the native graphics device for Microsoft Windows, requires
  !       separate GrWin/GrWinC library.
  character(len=*), parameter :: DEV_UNDEF = '?',        EXT_UNDEF = "" ! not file
  character(len=*), parameter :: DEV_XWIN  = '/XWINDOW', EXT_XWIN  = "" ! not file
  character(len=*), parameter :: DEV_GW    = '/GW',      EXT_GW    = "" ! not file
  character(len=*), parameter :: DEV_PS    = '/PS',      EXT_PS    = ".ps"
  character(len=*), parameter :: DEV_PSV   = '/VPS',     EXT_VPS   = ".ps"
  character(len=*), parameter :: DEV_PNG   = '/PNG',     EXT_PNG   = ".png"
  character(len=*), parameter :: DEV_TPNG  = '/TPNG',    EXT_TPNG  = ".png"
  character(len=*), parameter :: DEV_GIF   = '/GIF',     EXT_GIF   = ".gif"
  character(len=*), parameter :: DEV_VGIF  = '/VGIF',    EXT_VGIF  = ".gif"
  character(len=*), parameter :: DEV_HPGL  = '/HPGL',    EXT_HPGL  = ".hpgl"
  character(len=*), parameter :: DEV_HPGL2 = '/HPGL2',   EXT_HPGL2 = ".hpplot"
  ! Output device.
  character(len=:), allocatable :: output_dev

  integer :: pgopen ! pgplot function is integer type.

  ! Exit codes.
  integer, parameter :: EXIT_CODE_CLEAN = 0
  integer, parameter :: EXIT_CODE_ERROR = 1

  !> @warning Note that Oracle f95 does not tolerate column names array
  !!          constructor within the `CSV_MATRIX_WRITE` call **inline**.
  !!          So do colimn names seperately.
  character(len=*), dimension(3), parameter :: COLNAMES =                     &
                                            ["LENGTH  ","AREA    ","VISRANGE"]


  ! The default graphic output device is different for different platforms.
  if (PLATFORM_IS_WINDOWS()) then
    output_dev  = DEV_GW   ! Default output device is GrWin on Microsoft Windows.
  else
    output_dev  = DEV_XWIN ! Default output device is X11.
  end if

  !> Define fixed output device manually.
  !output_dev = DEV_UNDEF
  !output_dev = DEV_PS
  !output_dev = DEV_PNG

  print *, "Output graphical device: ", output_dev

  !> Background irradiance is the ambient illumination.
  irradiance =  DAYLIGHT !/2.0

!  !=============================================================================
!  !> NOTE: This is the old **loop-based** calculation procedure, **disabled**.
!  do i = 1, MAXSCALE
!    !object_length(i) = (PREDATOR_BODY_SIZE/100.0_SRP) * real(i, SRP)
!    object_length(i) = (700.0/real(MAXSCALE,SRP)) * real(i, SRP)
!    ! Calculate visual range for the predator
!    object_area(i) = carea( cm2m( object_length(i) ) )
!    !visrange(i) = m2cm (  visual_range (                                      &
!    !      irradiance = irradiance,                                            &
!    !      prey_area = object_area(i),                                         &
!    !      prey_contrast = PREYCONTRAST_DEFAULT ) )
!    visrange(i) = m2cm( visrange_m( object_area(i) ) )
!    !print *, object_length(i), object_area(i), visrange(i)
!  end do
!  !=============================================================================

  !=============================================================================
  !> NOTE: This is the new **whole-array-based** procedure making use the
  !!       elemental functions.
  object_length = (700.0/real(MAXSCALE,SRP)) *                                &
                                      [( real(i,SRP), i=1,size(object_length) )]
  object_area = carea( cm2m( object_length ) )
  visrange = m2cm( visrange_m_new( object_area ) )
  !=============================================================================

  !> Save raw data to CSV
  call CSV_MATRIX_WRITE ( reshape( [object_length, object_area, visrange],    &
                                   [size(object_length), 3]  ),               &
                         "visrange_output_raw_" // TOSTR(irradiance) // ".csv",&
                         COLNAMES  )

  !=============================================================================
  ! Produce the plot itself -- using PGPLOT library.

  if (pgopen(output_dev) .lt. 1) then
    write(ERROR_UNIT,*) "ERROR: Cannot open output device ", output_dev
    stop EXIT_CODE_ERROR
  end if

  call pgenv( minval(object_length), maxval(object_length),                   &
              minval(visrange), maxval(visrange), 0, 0 )

  call pglab('Object radius, cm', 'Visual range, cm', 'Irradiance=' //        &
              TOSTR(irradiance))

  call pgline( MAXSCALE, object_length, visrange )  ! plot line

  call pgpt( 1, [50.], [m2cm(visrange_m_new(carea(cm2m(50.))))], 8 )   ! plot dots

  call pgclos

  !=============================================================================

contains

  function visrange_m(area) result (fn_val)
    real(SRP), intent(in) :: area
    real(SRP) :: fn_val

    fn_val =   visual_range ( irradiance = irradiance,                        &
                              prey_area = area,                               &
                              prey_contrast = PREYCONTRAST_DEFAULT )

  end function visrange_m

  !.............................................................................

  elemental function visrange_m_new(area) result (fn_val)
    real(SRP), intent(in) :: area
    real(SRP) :: fn_val

    fn_val =   visual_range_new ( irradiance = irradiance,                    &
                                  prey_area = area,                           &
                                  prey_contrast = PREYCONTRAST_DEFAULT )

  end function visrange_m_new

end program VISRANGE_PLOT

