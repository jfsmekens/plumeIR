MODULE REAHIT_SUB
CONTAINS
SUBROUTINE REAHIT ( EOF, FAIL, ERRMSG ) 
!
! VERSION
!   13JUN17 AD Add REAPAR
!   01MAY17 AD F90 version. Checked.
!
! DESCRIPTION
!   Read record from HITRAN line data file
!   Called by REACYC, SPCWID.
!
! VARIABLE KINDS
    USE KIND_DAT
!
! GLOBAL DATA
    USE GASCOM_DAT ! Molecule and isotope data
    USE HFLCOM_DAT ! HITRAN file data
    USE HITCOM_DAT ! HITRAN line data
    USE REJCOM_DAT ! Minimum line strength limits
    USE RFMLUN_DAT, ONLY: LUNHIT ! LUN for HITRAN file
!
! SUBROUTINES
    USE C11INT_FNC ! Write integer as left-adjusted string
    USE IDXGAS_FNC ! Index in GAS arrays of molecule,isotope
    USE REAPAR_SUB ! Read record from HITRAN ASCII line parameter file
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
! LOCAL VARIABLES
    INTEGER(I4)  :: IDXMOL ! HITRAN index of molecule
    INTEGER(I4)  :: IGAS   ! Index of molecule in GASCOM
    INTEGER(I4)  :: IOS    ! Saved value of IOSTAT for error messages
    INTEGER(I4)  :: IREC   ! Record# 
    INTEGER(I4)  :: LSTAT  ! Status of transition information.
    REAL(R4)     :: TPROB  ! Transition probability [Debyes2].
    CHARACTER(9) :: SPARE9 ! Spare bytes in binary file record
!
! EXECUTABLE CODE -------------------------------------------------------------
!
  FAIL = .FALSE.
  EOF = .NOT. OPNHFL           ! Flag for no HITRAN data file being used
  IF ( EOF ) RETURN 
!
  IF ( PARHFL ) THEN
    CALL REAPAR ( EOF, FAIL, ERRMSG )
    RETURN
  END IF
!
  IOS = 0
!
! Find next useful record in file. 
  DO 
    IREC = MINVAL ( IFPHFL )
    EOF = IREC .EQ. IR2HFL
    IF ( EOF ) EXIT
!
    READ ( LUNHIT, REC=IREC, IOSTAT=IOS, ERR=900 ) LSTAT
    IF ( ABS ( LSTAT ) .NE. 10 ) THEN
      FAIL = .TRUE.
      WRITE ( ERRMSG, * ) 'F-REAHIT: Unexpected LSTAT=', LSTAT 
    END IF
!
    READ ( LUNHIT, REC=IREC, IOSTAT=IOS, ERR=900 ) &     ! Load /HITCOM/
      LSTAT, HIT%IDM, HIT%IDI, HIT%WNO, HIT%STR,  TPROB, & 
      HIT%ABR, HIT%SBR, HIT%ELS, HIT%ABC, HIT%TSP, HIT%IUS, HIT%ILS, &
      HIT%ULQ, HIT%BLQ, SPARE9, IFWDPT
    IDXMOL = HIT%IDM
    IFPHFL(IDXMOL) = IREC + IFWDPT  ! Set forward pointer for the next call 
    IF ( .NOT. VALISO ( IGSMOL(IDXMOL), HIT%IDI ) ) CYCLE
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
  IRCHFL = IREC
!
900 CONTINUE
  FAIL = IOS .NE. 0 
  IF ( FAIL ) WRITE ( ERRMSG, * ) 'F-REAHIT: Failed to read Rec#' // &
    TRIM(C11INT(IREC)) // ' in HITRAN File. IOSTAT=', IOS
!
END SUBROUTINE REAHIT
END MODULE REAHIT_SUB

