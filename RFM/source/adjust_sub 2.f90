MODULE ADJUST_SUB
CONTAINS
SUBROUTINE ADJUST ( TEM, PRE, PPA, AMT, ANTE, CNTE )
!
! VERSION
!   01MAY17 AD F90 conversion. Checked.
!
! DESCRIPTION
!   Adjust line parameters for path conditions
!   Called by SPCWID, SPCFIN
!   The line strength and width at 296K (TEMREF) and 1 atm (PREF) read from 
!   the HITRAN line data file are adjusted for atmospheric path conditions. 
!   The doppler halfwidth is also calculated. These parameters are then used 
!   in the lineshape formulation. 
!
!   Adjusts data in HITCOM and loads into ADJCOM
! 
!   Line mixing data and temperature dependence interpolation due to 
!   L.L.Strow (private communication with Dave Edwards)
!
!   Line strengths for different isotopes of a gas on the HITRAN data base 
!   weighted by atmospheric abundance of the particular isotope. Absolute 
!   strengths may be obtained by dividing by this abundance i.e. 
!   STREN/ABUN(IDGAS,ISO) where IDGAS is the gas ID and ISO the isotope ID. 
!   This will be important when performing calculations for other planetary 
!   atmospheres.
!
! VARIABLE KINDS
    USE KIND_DAT 
!
! GLOBAL DATA
    USE ADJCOM_DAT ! Path-adjusted line data
    USE HITCOM_DAT ! HITRAN line data
    USE IDXCON_DAT ! RFM/HITRAN indices for specific molecules
    USE PHYCON_DAT, ONLY: C2, PREREF, RGAS, TEMREF, VLIGHT ! Physical constants
    USE FLGCOM_DAT, ONLY: MIXFLG ! T = use line-mixing
!
! SUBROUTINES
    USE NTECLC_SUB ! Calculate various non-LTE parameters for line
    USE QTFCT_FNC  ! Calculate total internal partition sum
    USE YMIX_FNC   ! Calculate line-mixing y-coefficient
!
  IMPLICIT NONE
!
! ARGUMENTS
    REAL(R4), INTENT(IN)  :: TEM  ! Path temperature [K]
    REAL(R4), INTENT(IN)  :: PRE  ! Path pressure [atm]
    REAL(R4), INTENT(IN)  :: PPA  ! Path partial pressure [atm]
    REAL(R4), INTENT(IN)  :: AMT  ! Path amount [kmol/cm2]
    REAL(R4), INTENT(OUT) :: ANTE ! Non-lte factor for k abs
    REAL(R4), INTENT(OUT) :: CNTE ! Non-lte factor for c abs
!
! LOCAL CONSTANTS
    REAL(R4), PARAMETER :: R2 = 2.0 * LOG(2.0) * RGAS ! 2ln2 k N = 11526.3
!
! LOCAL VARIABLES
    REAL(R4) :: BR     ! Broadening factor at Standard Temp.
    REAL(R4) :: SQ     ! Ratio of tps@296K/tps@path_temp
    REAL(R8) :: ANLTE  ! Non-LTE Correction factor for k absorption
    REAL(R8) :: CNLTE  ! Non-LTE Correction factor for c absorption
    REAL(R8) :: GAMMA  ! exp ( -hcv/kT )
    REAL(R8) :: GAMREF ! exp ( -hcv/kT_ref )
    REAL(R8) :: SB     ! exp( -hcE_l/kT_path ) / exp( -hcE_l/kT_ref )
    REAL(R8) :: SE     ! Ratio of stimulated emission @path/@ref
!
! EXECUTABLE CODE -------------------------------------------------------------
!
! Save path parameters - could be required by CHISHP
  TEMADJ = TEM
  PREADJ = PRE
  PPAADJ = PPA
!
  WNOADJ = HIT%WNO + DBLE ( PRE * HIT%TSP ) ! Pressure shift (often 0)
!
! Convert for line width in cm-1 at 296K and 1atm.
  IF ( HIT%SBR .EQ. 0.0 ) THEN                 ! If self-broad.HW is zero...
    IF ( HIT%IDM .EQ. IDXH2O ) THEN                   ! ID=1 = Water vapour 
      HIT%SBR = 5.0 * HIT%ABR
      BR = ( HIT%ABR * ( PRE - PPA ) + HIT%SBR * PPA ) / PREREF
    ELSE                     ! HIT%SBR = HIT%ABR for want of something better
      BR = HIT%ABR * PRE / PREREF  
    ENDIF
  ELSE
    BR = ( HIT%ABR * ( PRE - PPA ) + HIT%SBR * PPA ) / PREREF
  ENDIF
  WIDADJ = BR * ( TEMREF / TEM )**HIT%ABC
!
! Calculate Doppler half-width at half-max HWHM in cm-1. 
  DOPADJ = SNGL ( HIT%WNO / VLIGHT ) * SQRT ( R2 * TEM / HIT%WGT )
!
! Calculate the line mixing y coefficient (only CO2 lines at present)
  IF ( MIXFLG .AND. HIT%IDM .EQ. IDXCO2 ) THEN          ! ID= 2 = CO2
    YMXADJ = YMIX ( TEM, PRE, PPA )
  ELSE
    YMXADJ = 0.0
  ENDIF
!
! Convert for line strength in cm-1.(mol.cm-2)-1 at 296K.
!
! Boltzman factor for lower state energy
  SB = DEXP ( DBLE(HIT%ELS) * C2 * DBLE(TEM-TEMREF)/DBLE(TEM*TEMREF) )
!
! Stimulated emission 
  GAMMA = DEXP ( -C2 * HIT%WNO / DBLE ( TEM ) )
  GAMREF = DEXP ( -C2 * HIT%WNO / DBLE ( TEMREF ) )
  SE = ( 1.D0 - GAMMA ) / ( 1.D0 - GAMREF )
!
! Nonlte calculation of absorption coefficient modifiers
  SQ = 1.0                       
  IF ( HIT%IUV .NE. 0 .OR. HIT%ILV .NE. 0 ) THEN
!    IF ( PTH(IPTH)%IVJ .GT. 0 ) CALL PTBVIB ( PTH(IPTH)%IVJ, .TRUE. ) 
    CALL NTECLC ( PRE, TEM, GAMMA, ANLTE, CNLTE, SQ )
!    IF ( PTH(IPTH)%IVJ .GT. 0 ) CALL PTBVIB ( PTH(IPTH)%IVJ, .FALSE. ) 
  ELSE
    ANLTE = 1.0D0
    CNLTE = 1.0D0       
    SQ = QTFCT ( HIT%IDM, HIT%IDI, TEM )
  ENDIF
!
! SB can be larger than allowed for SNGL (eg for mid-IR NO lines in HITRAN2012)
! so combine all factors together before converting to SNGL in the hope that it
! will be small enough to fit into STRADJ
  STRADJ = SNGL ( AMT * HIT%STR * SB * SE * SQ )   ! Bug#112
  ANTE = SNGL ( ANLTE )
  CNTE = SNGL ( CNLTE )
!
END SUBROUTINE ADJUST
END MODULE ADJUST_SUB
