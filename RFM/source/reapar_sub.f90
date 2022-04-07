MODULE REAPAR_SUB
CONTAINS
SUBROUTINE REAPAR ( EOF, FAIL, ERRMSG ) 
!
! VERSION
!   10SEP19 AD Checked.
!   04MAY18 AD Bug#5 correct Isotope#ID after reading as Z1
!   17NOV17 AD Original.
!
! DESCRIPTION
!   Read record from HITRAN ASCII line parameter file
!   Called by REAHIT
!
! VARIABLE KINDS
    USE KIND_DAT
!
! GLOBAL DATA
    USE GASCOM_DAT ! Molecule and isotope data
    USE HFLCOM_DAT ! HITRAN file data
    USE HITCOM_DAT ! HITRAN line data
    USE REJCOM_DAT ! Minimum line strength limits
    USE PHYCON_DAT, ONLY: AVOG   ! Avogradro's number [kmol/cm2]
    USE RFMLUN_DAT, ONLY: LUNHIT ! LUN for HITRAN file
!
! SUBROUTINES
    USE C11INT_FNC ! Write integer as left-adjusted string
    USE IDXGAS_FNC ! Index in GAS arrays of molecule,isotope
    USE SETNTE_SUB ! Set IUSNTE,ILSNTE in HITCOM
    USE USEQAL_FNC ! Set TRUE if listed in line molecule qualifiers
    USE VALISO_FNC ! Check recognised isotope
!    
  IMPLICIT NONE
!
! ARGUMENTS      
    LOGICAL,       INTENT(OUT) :: EOF    ! Set TRUE if end-of-file reached
    LOGICAL,       INTENT(OUT) :: FAIL   ! Set TRUE if a fatal error is detected
    CHARACTER(80), INTENT(OUT) :: ERRMSG ! Error message written if FAIL is TRUE
!
! LOCAL CONSTANTS
    INTEGER(I4), PARAMETER :: FIXIDI(0:15) = &
              (/ 10, 1, 2, 3, 4, 5, 6, 7, 8, 9, 11, 12, 13, 14, 15, 16 /)
! LOCAL VARIABLES
    INTEGER(I4) :: IDI   ! Isotope ID read from .par file
    INTEGER(I4) :: IGAS  ! Index of molecule in GASCOM
    INTEGER(I4) :: IOS   ! Saved value of IOSTAT for error messages
    REAL(R4)    :: TPROB ! Transition probability [Debyes2].
    REAL(R8)    :: DSTR  ! Original HITRAN linestrength
!
! EXECUTABLE CODE -------------------------------------------------------------
!
  FAIL = .FALSE.
  IOS = 0
  HIT%ILS = 0
  HIT%IUS = 0
!
! Find next useful record in file. 
  DO 
    IRCHFL = IRCHFL + 1
    READ ( LUNHIT, 1001, IOSTAT=IOS, ERR=900, END=900 ) HIT%IDM, IDI, &
      HIT%WNO, DSTR, TPROB, HIT%ABR, HIT%SBR, HIT%ELS, HIT%ABC, HIT%TSP
1001 FORMAT( I2, Z1, F12.6, F10.3, E10.3, F5.2, F5.2, F10.4, F4.1, F8.5 )
    HIT%STR = SNGL ( DSTR * AVOG ) 
    HIT%IDI = FIXIDI(IDI)    ! Convert to correct numbering
    WNOHFL = HIT%WNO
    IF ( IGSMOL(HIT%IDM) .EQ. 0 ) CYCLE
    IF ( .NOT. VALISO ( IGSMOL(HIT%IDM), HIT%IDI ) ) CYCLE
    IGAS = IDXGAS ( HIT%IDM, HIT%IDI )
    IF ( USEREJ ) THEN
      IF ( HIT%STR .LT. STRREJ(IGAS) ) CYCLE
    END IF
    IF ( GAS(IGAS)%QAL ) THEN
      IF ( .NOT. USEQAL ( HIT%IDM, HIT%IDI, HIT%ILS, HIT%IUS ) ) CYCLE
    END IF
    HIT%IGS = IGAS
    HIT%WGT = GAS(IGAS)%WGT(HIT%IDI)
    IF ( GAS(IGAS)%NTE ) CALL SETNTE
!
    EXIT    ! next required line loaded
  END DO
!
900 CONTINUE
  EOF = IOS .LT. 0
  FAIL = IOS .GT. 0 
  IF ( FAIL ) WRITE ( ERRMSG, * ) 'F-REAPAR: Failed to read Rec#' // &
    TRIM(C11INT(IRCHFL)) // ' in HITRAN File. IOSTAT=', IOS
!
END SUBROUTINE REAPAR
END MODULE REAPAR_SUB

