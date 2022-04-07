MODULE OPNHIT_SUB
CONTAINS
SUBROUTINE OPNHIT ( NAMHIT, FAIL, ERRMSG )
!
! VERSION
!   12JUN17 AD Allow for ASCII files. Checked.
!   O1MAY17 AD F90 conversion. 
!
! DESCRIPTION
!   Open HITRAN binary line data file and read header
!   Called once by DRVHIT
!
! VARIABLE KINDS
    USE KIND_DAT
!
! GLOBAL DATA
    USE HFLCOM_DAT ! HITRAN file data
    USE RFMLUN_DAT, ONLY: LUNHIT ! LUN for HITRAN file
!
! SUBROUTINES
    USE C11INT_FNC ! Write integer as left-adjusted string
    USE LEXIST_FNC ! Check if file exists
    USE OPNPAR_SUB ! Open HITRAN ASCII line parameter file and check contents
    USE WRTLOG_SUB ! Write text message to log file
!
  IMPLICIT NONE
!
! ARGUMENTS
    CHARACTER(*),  INTENT(IN)  :: NAMHIT ! Name of HITRAN file
    LOGICAL,       INTENT(OUT) :: FAIL   ! Set TRUE if a fatal error is detected
    CHARACTER(80), INTENT(OUT) :: ERRMSG ! Error message if FAIL is TRUE
!
! LOCAL CONSTANTS
    INTEGER(I4), PARAMETER :: I22DUM(22) = 0 ! Dummy array to find RECL
!
! LOCAL VARIABLES
    INTEGER(I4)   :: IDUMMY ! Dummy integer(i4) for read
    INTEGER(I4)   :: IFMT   ! Binary file format identifier
    INTEGER(I4)   :: IOS    ! Saved value of IOSTAT for error messages
    INTEGER(I4)   :: IPTOFF = 0 ! Offset for reading forward pointers
    INTEGER(I4)   :: IPTR   ! Pointer counter 
    INTEGER(I4)   :: IREC   ! Record counter
    INTEGER(I4)   :: LSTAT  ! Used to identify HITRAN Fwd.Pointer block
    INTEGER(I4)   :: RECLEN ! RECL parameter for opening file (22 or 88)
    REAL(R8)      :: DDUMMY ! Dummy R*8
    CHARACTER(48) :: LABEL  ! Label read from HITRAN file
    CHARACTER(80) :: LOGMSG ! Message sent to log file
!
! EXECUTABLE CODE -------------------------------------------------------------
!
! Send message to LOG file saying which file is about to be opened
  CALL WRTLOG ( 'I-OPNHIT: Opening HITRAN File: '//NAMHIT )
!
  IF ( .NOT. LEXIST ( NAMHIT ) ) THEN 
    FAIL = .TRUE.
    ERRMSG = 'F-OPNHIT: file not found'
    RETURN
  END IF
! 
  INQUIRE ( IOLENGTH=RECLEN ) I22DUM
  LOGMSG = 'I-OPNHIT: Opening file using RECL=' // C11INT(RECLEN)
  CALL WRTLOG ( LOGMSG )
!
  OPEN ( UNIT=LUNHIT, FILE=NAMHIT, STATUS='OLD', ACTION='READ', &
         ACCESS='DIRECT', RECL=RECLEN, IOSTAT=IOS )
  IF ( IOS .NE. 0 ) THEN
    FAIL = .TRUE.
    WRITE ( ERRMSG, * ) 'F-OPNHIT: Open failure on HITRAN file. IOSTAT=', IOS
    RETURN
  END IF
  OPNHFL = .TRUE.
!
! Read header and extract first/last record#
  READ ( LUNHIT, REC=1, IOSTAT=IOS, ERR=900 ) LSTAT, IFMT, IR1HFL, IR2HFL, LABEL
! 
! Print File label to LOG file
  CALL WRTLOG ( 'I-OPNHIT: HITRAN File Label='//LABEL )
!
! Check format - data generated with old version of HITBIN had IFMT=56,
! newer data has IFMT=1 but format is the same.
  IF ( IFMT .NE. 1 .AND. IFMT .NE. 56 ) THEN
    CALL WRTLOG ( 'W-OPNHIT: Unexpected format ID in rec#1 of binary file, ' &
                  // 'value=' // C11INT(IFMT) )
    CALL OPNPAR ( NAMHIT, FAIL, ERRMSG ) 
    RETURN
  END IF
!
! Find wavenumber range by reading first,last records
  READ ( LUNHIT, REC=IR1HFL, IOSTAT=IOS, ERR=900 ) LSTAT, IDUMMY, IDUMMY, WNLHFL
  READ ( LUNHIT, REC=IR2HFL, IOSTAT=IOS, ERR=900 ) LSTAT, IDUMMY, IDUMMY, WNUHFL
!
! Initialise pointers for all different line species to point to last
! record in file
  IFPHFL = 0
!
! Read Forward Pointers from 1st n records from start of file
  IREC = IR1HFL
  READ ( LUNHIT, REC=IREC, ERR=900, IOSTAT=IOS ) LSTAT
  IF ( LSTAT .NE. -7 ) THEN                 ! Check this is FP record
    ERRMSG = 'F-OPNHIT: Expected Fwd Ptr (LSTAT=-7) for Rec#' // &
             TRIM ( C11INT(IREC) ) // ', got LSTAT=' // C11INT(LSTAT)
    FAIL = .TRUE.
    RETURN
  END IF
!
  IPTOFF = 0
  DO 
    IF ( IPTOFF+14 .GT. MAXPTR ) THEN
      FAIL = .TRUE.
      ERRMSG = 'F-OPNHIT: No.forward ptrs in HITRAN file >MAXPTR=' // &
               TRIM ( C11INT(MAXPTR) ) // ' in hflcom.f90' 
      RETURN
    END IF
    READ ( LUNHIT, REC=IREC, ERR=900, IOSTAT=IOS ) &
      LSTAT, IDUMMY, IDUMMY, DDUMMY, ( IFPHFL(IPTR), IPTR=IPTOFF+1, IPTOFF+14 )
! Binary format has rel.forward pointer set to last rec in file for no data,
! but here use IFPHFL=0 as flag indicating no molec.data within file
    DO IPTR = IPTOFF+1, IPTOFF+14
      IF ( IREC + IFPHFL(IPTR) .EQ. IR2HFL ) IFPHFL(IPTR) = 0
    END DO
    IPTOFF = IPTOFF + 14
    IREC = IREC + 1
    READ ( LUNHIT, REC=IREC, ERR=900, IOSTAT=IOS ) LSTAT
    IF ( LSTAT .NE. -7 ) EXIT
  END DO
! 
900 CONTINUE
  FAIL = IOS .NE. 0 
  IF ( FAIL ) &
    ERRMSG = 'F-OPNHIT: Read failure on HITRAN file. IOSTAT=' // C11INT(IOS)
!
END SUBROUTINE OPNHIT
END MODULE OPNHIT_SUB
