MODULE DRVNAM_SUB
CONTAINS
SUBROUTINE DRVNAM ( LUNDRV, KEY, FAIL, ERRMSG )
!
! VERSION
!   08NOV17 AD F90 conversion of inpnam.for. Tested
!
! DESCRIPTION
!   Read RFM driver table user-defined filename
!   Called by RFMDRV following *ABS, *BBT etc markers in Driver table.
!
! VARIABLE KINDS
    USE KIND_DAT
!
! GLOBAL DATA
    USE NAMCOM_DAT ! RFM output filenames
    USE LENREC_DAT ! Max length of input text record
!
! SUBROUTINES
    USE ENDCHK_SUB ! Check end of Driver Table section has been reached
    USE NXTFLD_SUB ! Load next field from section of driver file
    USE WRTLOG_SUB ! Write text message to log file
!
  IMPLICIT NONE
!
! ARGUMENTS
    INTEGER(I4),   INTENT(IN)  :: LUNDRV ! LUN for Driver File
    CHARACTER(4),  INTENT(IN)  :: KEY    ! Section header, eg '*RAD'
    LOGICAL,       INTENT(OUT) :: FAIL   ! Set TRUE if a fatal error is detected
    CHARACTER(80), INTENT(OUT) :: ERRMSG ! Error message written if FAIL is TRUE
!
! LOCAL VARIABLES
    INTEGER(I4)       :: LENGTH ! No.characters in FIELD
    CHARACTER(LENREC) :: FILNAM ! Field read from driver table
!
! EXECUTABLE CODE --------------------------------------------------------------
!
! Read first and only expected field in driver table section
  CALL NXTFLD ( LUNDRV, FILNAM, LENGTH, FAIL, ERRMSG )
  IF ( FAIL ) RETURN
  IF ( LENGTH .EQ. 0 ) THEN
    FAIL = .TRUE.
    ERRMSG = 'F-DRVNAM: No filename supplied in ' // KEY // ' section'
    RETURN
  ELSE IF ( LENGTH .GT. LENNAM ) THEN
    FAIL = .TRUE.
    ERRMSG = 'F-DRVNAM: Name longer than LENOUT in outcom_dat.f90'
    RETURN
  END IF
!
  CALL WRTLOG ( 'I-DRVNAM: User-supplied ' // KEY(2:4) // ' filename: ' &
                // FILNAM )
!
! Save as appropriate common variable in outcom.inc
  SELECT CASE ( KEY ) 
  CASE ( '*ABS' ) ; ABSNAM = FILNAM
  CASE ( '*BBT' ) ; BBTNAM = FILNAM
  CASE ( '*COO' ) ; COONAM = FILNAM
  CASE ( '*OPT' ) ; OPTNAM = FILNAM
  CASE ( '*PRF' ) ; PRFNAM = FILNAM
  CASE ( '*PTH' ) ; PTHNAM = FILNAM
  CASE ( '*RAD' ) ; RADNAM = FILNAM
  CASE ( '*RJT' ) ; RJTNAM = FILNAM
  CASE ( '*TAB' ) ; TABNAM = FILNAM
  CASE ( '*TRA' ) ; TRANAM = FILNAM
  CASE ( '*WID' ) ; WIDNAM = FILNAM
  CASE DEFAULT 
    WRITE (*,*) 'F-DRVNAM: Unrecognised KEY=' // KEY
    STOP 'F-DRVNAM: Logical Error'
  END SELECT
!
! Check that there are no more fields in this Driver file section
  CALL ENDCHK ( LUNDRV, KEY, FAIL, ERRMSG )
!
END SUBROUTINE DRVNAM
END MODULE DRVNAM_SUB
