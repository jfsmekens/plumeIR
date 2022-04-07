MODULE INIPAR_SUB
CONTAINS
SUBROUTINE INIPAR ( WNOREQ, FAIL, ERRMSG )
!
! VERSION
!   13JUN17 AD Original. Checked.
!
! DESCRIPTION    
!   Initialise the HITRAN ASCII line parameter file
!   Called by INIHFL if ASCII HITRAN file.
!
! VARIABLE KINDS
    USE KIND_DAT  
!
! GLOBAL DATA
    USE HFLCOM_DAT ! HITRAN file data
    USE RFMLUN_DAT, ONLY: LUNHIT ! LUN for HITRAN binary file
!
  IMPLICIT NONE
!
! ARGUMENTS      
    REAL(R8),      INTENT(IN)  :: WNOREQ ! Initial wavenumber
    LOGICAL,       INTENT(OUT) :: FAIL   ! Set TRUE if a fatal error is detected
    CHARACTER(80), INTENT(OUT) :: ERRMSG ! Error message written if FAIL is TRUE
!
! LOCAL VARIABLES
    INTEGER(I4) :: IOS  ! Value of IOSTAT for error messages
    REAL(R8)    :: WNUM ! Wavenumber of HITRAN record
!
! EXECUTABLE CODE -------------------------------------------------------------
!
  REWIND ( LUNHIT, IOSTAT=IOS, ERR=900 ) 
  IRCHFL = 0
  WNOHFL = -1.0D0
!
  DO      
    READ ( LUNHIT, '(3X,F12.6)', IOSTAT=IOS, ERR=900 ) WNUM
    IF ( WNUM .GT. WNOREQ ) EXIT
    IRCHFL = IRCHFL + 1
    WNOHFL = WNUM
  END DO
!
  BACKSPACE ( LUNHIT, IOSTAT=IOS, ERR=900 ) 
!
900 CONTINUE
  FAIL = IOS .NE. 0
  IF ( FAIL ) WRITE ( ERRMSG, * ) &
    'F-INIPAR: Failed to read HITRAN .par file. IOSTAT=', IOS
!
END SUBROUTINE INIPAR
END MODULE INIPAR_SUB
