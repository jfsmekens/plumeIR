MODULE SFCLEV_SUB
CONTAINS
SUBROUTINE SFCLEV ( PARAM, VALUE, FAIL, ERRMSG )
!
! VERSION
!   05AUG19 AD Check new surface is not above observer level
!   05APR19 AD Change condition for resetting TAN%IAT
!   14DEC17 AD F90 original.
!
! DESCRIPTION
!   Read SFC height or pressure 
!   Called by DRVSFC if 'HGTSFC' or 'PRESFC' in *SFC section of driver file.
!   Inserts surface level into existing profile and assigns IATSFC.
!
! VARIABLE KINDS
    USE KIND_DAT
!
! GLOBAL DATA
    USE ATMCOM_DAT ! Atmospheric profile data
    USE TANCOM_DAT ! Tangent path data
    USE OBSCOM_DAT, ONLY: IATOBS ! Observer level (or 0 if unspecified)
!
! SUBROUTINES
    USE ADDATM_SUB ! Add extra level to atm profiles in ATMCOM
    USE C9REAL_GEN ! Write real number as C*9 string
    USE WRTLOG_SUB ! Write text message to log file
!
  IMPLICIT NONE 
!
! ARGUMENTS
    CHARACTER(*),  INTENT(IN)  :: PARAM  ! 'HGTSFC' or 'PRESFC'
    CHARACTER(*),  INTENT(IN)  :: VALUE  ! String containing surface value 
    LOGICAL,       INTENT(OUT) :: FAIL   ! T=A fatal error was detected
    CHARACTER(80), INTENT(OUT) :: ERRMSG ! Error message if FAIL is TRUE
!
! LOCAL VARIABLES      
    INTEGER(I4) :: IOS ! Saved value of IOSTAT for error message
!
! EXECUTABLE CODE --------------------------------------------------------------
!
  SELECT CASE ( PARAM )
  CASE ( 'HGTSFC' ) 
    READ ( VALUE, *, IOSTAT=IOS, ERR=900 ) HGTSFC
    IF ( HGTSFC .LT. HGTATM(1) .OR. HGTSFC .GT. HGTATM(NATM ) ) THEN
      FAIL = .TRUE.
      ERRMSG = 'F-SFCLEV: Surface Height=' // C9REAL(HGTSFC) // &
               ' not within profile'
      RETURN
    END IF
    CALL ADDATM ( HGTSFC, .TRUE., IATSFC ) 
    CALL WRTLOG ( 'I-SFCLEV: Setting Surface Altitude = ' // C9REAL(HGTSFC) )
  CASE ( 'PRESFC' ) 
    READ ( VALUE, *, IOSTAT=IOS, ERR=900 ) PRESFC
    IF ( PRESFC .GT. PREATM(1) .OR. PRESFC .LT. PREATM(NATM ) ) THEN
      FAIL = .TRUE.
      ERRMSG = 'F-SFCLEV: Surface Pressure=' // C9REAL(PRESFC) // &
               ' not within profile'
      RETURN
    END IF
    CALL ADDATM ( PRESFC, .FALSE., IATSFC ) 
    CALL WRTLOG ( 'I-SFCLEV: Setting Surface Pressure = ' // C9REAL(PRESFC) )
  CASE DEFAULT
    STOP 'F-SFCLEV: Logical error'
  END SELECT
!
! If observer altitude/pressure has already been specified, check that the new 
! surface is not above the observer level
  IF ( IATOBS .GT. 0 ) THEN
    IF ( IATSFC .GT. IATOBS ) THEN
      FAIL = .TRUE.
      ERRMSG = 'F-SFCLEV: New surface above specified observer position'
      RETURN
    END IF
  END IF
!
! For surface-intersecting paths, reset to new surface
  WHERE ( TAN(:)%SFC ) 
    TAN(:)%IAT = IATSFC
    TAN(:)%HGT = HGTSFC
  END WHERE
!
900 CONTINUE
  FAIL = IOS .NE. 0 
  IF ( FAIL ) WRITE ( ERRMSG, * ) 'F-SFCLEV: Failed to read ' // PARAM // &
    ' value. IOSTAT=', IOS
!
END SUBROUTINE SFCLEV
END MODULE SFCLEV_SUB
