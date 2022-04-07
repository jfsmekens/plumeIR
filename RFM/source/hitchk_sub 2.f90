MODULE HITCHK_SUB
CONTAINS
SUBROUTINE HITCHK
!
! VERSION
!   01MAY17 AD F90 version. Tested.
!
! DESCRIPTION
!   Check HITRAN binary line data file contents
!   Called once by DRVHIT.
!
! VARIABLE KINDS
    USE KIND_DAT
!
! GLOBAL DATA
    USE GASCOM_DAT ! Molecule and isotope data
    USE HFLCOM_DAT ! HITRAN file data
    USE SPCCOM_DAT ! Spectral range data
    USE RFMCON_DAT, ONLY: FWIND  ! Window [cm-1] for widemesh calc
    USE RFMLUN_DAT, ONLY: LUNHIT ! LUN for HITRAN binary file
!
! SUBROUTINES
    USE C9REAL_GEN ! Write real number as C*9 string
    USE WRTLOG_SUB ! Write text message to log file
!
  IMPLICIT NONE
!
! LOCAL VARIABLES
    INTEGER(I4)   :: IDXMOL ! HITRAN/RFM index of molecule
    INTEGER(I4)   :: IGAS   ! Counter for required species
    CHARACTER(80) :: LOGMSG ! Message sent to log file
!
! EXECUTABLE CODE -------------------------------------------------------------
!
! Check if any required molecules are line molecules
  IF ( ALL ( IGSMOL(1:MAXHLN) .EQ. 0 ) ) THEN
    CALL WRTLOG ( 'W-HITCHK: Ignore HITRAN file - no line molecules reqd' ) 
    CLOSE ( LUNHIT )
    OPNHFL = .FALSE.
    RETURN
  END IF
!
! Check that file contains some data for required molecules (even if out of
! spectral range) 
  DO IGAS = 1, NGAS
    IDXMOL = GAS(IGAS)%IDM
    IF ( IDXMOL .LT. MAXPTR ) GAS(IGAS)%HIT = IFPHFL(IDXMOL) .GT. 0
  END DO

! Check HITRAN wavenumber range overlaps required spectral range(s). 
  IF ( WNLHFL .GT. WMXSPC .OR. WNUHFL .LT. WMNSPC ) THEN
    CALL WRTLOG ( 'W-HITCHK: Ignore HITRAN file - outside reqd waveno range' )
    CALL WRTLOG ( LOGMSG ) 
    CLOSE ( LUNHIT )
    OPNHFL = .FALSE.
    GAS%HIT = .FALSE.   ! No molecules can use HITRAN data
    RETURN
  END IF
!
! Check HITRAN range sufficient to include widemesh margins
  IF ( WNLHFL .GT. MAX ( WMNSPC-FWIND, 0.0D0 ) .OR. &
       WNUHFL .LT. WMXSPC+FWIND ) THEN 
    LOGMSG = 'W-HITCHK: HITRAN file may not include all lines within ' // &
             TRIM ( C9REAL(FWIND) ) // ' cm-1 of output'
    CALL WRTLOG ( LOGMSG ) 
  END IF
!
END SUBROUTINE HITCHK
END MODULE HITCHK_SUB
