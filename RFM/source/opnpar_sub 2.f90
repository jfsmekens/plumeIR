MODULE OPNPAR_SUB
CONTAINS
SUBROUTINE OPNPAR ( NAMHIT, FAIL, ERRMSG )
!
! VERSION
!   17NOV17 AD Original. Checked.
!
! DESCRIPTION
!   Open HITRAN ASCII line parameter file and check contents
!   Called once by OPNHIT if file not recognised as binary.
!
! VARIABLE KINDS
    USE KIND_DAT
!
! GLOBAL DATA
    USE GASCOM_DAT ! Molecule and isotope data
    USE HFLCOM_DAT ! HITRAN file data
    USE QALCOM_DAT ! Band/isotope selection qualifiers
    USE FLGCOM_DAT, ONLY: MIXFLG, NTEFLG ! Flags for line mixing and non-LTE
    USE RFMCON_DAT, ONLY: FWIND  ! Window [cm-1] for widemesh calc
    USE RFMLUN_DAT, ONLY: LUNHIT ! LUN for HITRAN file
    USE SPCCOM_DAT, ONLY: WMXSPC ! Upper Wavenumber reqd for any range
!
! SUBROUTINES
    USE C11INT_FNC ! Write integer as left-adjusted string
    USE C9REAL_GEN ! Write real number as C*9 string
    USE WRTLOG_SUB ! Write text message to log file
!
  IMPLICIT NONE
!
! ARGUMENTS
    CHARACTER(*),  INTENT(IN)  :: NAMHIT ! Name of HITRAN file
    LOGICAL,       INTENT(OUT) :: FAIL   ! Set TRUE if a fatal error is detected
    CHARACTER(80), INTENT(OUT) :: ERRMSG ! Error message if FAIL is TRUE
!
! LOCAL VARIABLES
    INTEGER(I4) :: IDXISO ! HITRAN index of isotope
    INTEGER(I4) :: IDXMOL ! HITRAN index of molecule
    INTEGER(I4) :: IGAS   ! Counter for required molecules
    INTEGER(I4) :: IOS    ! Saved value of IOSTAT for error messages
!
! EXECUTABLE CODE -------------------------------------------------------------
!
! Send message to LOG file saying which file is about to be opened
  CALL WRTLOG ( 'I-OPNPAR: Unrecognised binary structure ' // &
                '- try opening as ASCII .par file' )
!
  CLOSE ( LUNHIT ) 
  OPEN ( UNIT=LUNHIT, FILE=NAMHIT, STATUS='OLD', ACTION='READ', IOSTAT=IOS )
  IF ( IOS .NE. 0 ) THEN
    FAIL = .TRUE.
    WRITE ( ERRMSG, * ) 'F-OPNPAR: Open failure on HITRAN file. IOSTAT=', IOS
    RETURN
  END IF
  PARHFL = .TRUE.
! 
! Certain options rely on encoded global/local quantum numbers created by
! HITBIN program so not available in original HITRAN
  FAIL = .TRUE.
  IF ( MIXFLG ) THEN
    ERRMSG = 'F-OPNPAR: MIX flag requires binary HITRAN file'
  ELSE IF ( NTEFLG ) THEN
    ERRMSG = 'F-OPNPAR: NTE flag requires binary HITRAN file'
  ELSE IF ( NQAL .GT. 0 ) THEN
    IF ( ANY ( QAL%ILS .NE. 0 ) .OR. ANY ( QAL%IUS .NE. 0 ) ) THEN
      ERRMSG = 'F-OPNPAR: Vib level selection requires binary HITRAN file'
    ELSE
      FAIL = .FALSE.
    END IF
  ELSE
    FAIL = .FALSE.
  END IF
  IF ( FAIL ) RETURN

  IFPHFL = 0
! Flag all required molecules as -1 until first line found in HITRAN file
  DO IGAS = 1, NGAS
    IDXMOL = GAS(IGAS)%IDM
    IF ( IDXMOL .LE. MAXPTR ) IFPHFL(IDXMOL) = -1
  END DO
!
! Read first record
  READ ( LUNHIT, '(I2,Z1,F12.6)', ERR=900, IOSTAT=IOS ) IDXMOL, IDXISO, WNLHFL
  IF ( IDXMOL .LE. MAXPTR ) IFPHFL(IDXMOL) = 1
!
  WNUHFL = WNLHFL
! Don't need to read to the end of the file, just to max required wavenumber
  DO WHILE ( WNUHFL .LT. WMXSPC + FWIND .OR. ANY ( IFPHFL .EQ. -1 ) ) 
    READ ( LUNHIT, '(I2,Z1,F12.6)', IOSTAT=IOS, END=900, ERR=900 ) &
      IDXMOL, IDXISO, WNUHFL
    IF ( IDXMOL .LE. MAXPTR ) IFPHFL(IDXMOL) = 1
  END DO
  CALL WRTLOG ( 'I-OPNPAR: HITRAN .par file has wavenumber range ' & 
                // TRIM ( C9REAL(WNLHFL) ) // 'to >=' &
                // TRIM ( C9REAL(WNUHFL) ) // ' cm-1' ) 
900 CONTINUE
  FAIL = IOS .NE. 0 
  IF ( FAIL ) &
    ERRMSG = 'F-OPNPAR: Read failure on HITRAN file. IOSTAT=' // C11INT(IOS)
!
END SUBROUTINE OPNPAR
END MODULE OPNPAR_SUB
