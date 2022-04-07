MODULE XSCFIL_SUB
CONTAINS
SUBROUTINE XSCFIL ( NAMXSC, FAIL, ERRMSG )
!
! VERSION
!   17JAN18 AD BACKSPACE rather than REWIND in case RFM format has comment recs
!   20NOV17 AD should really check that .xsc file has correct format at first
!   23JUN17 AD Allow for original HITRAN format files
!   01MAY17 AD F90 version. Checked.
!
! DESCRIPTION
!   Check if .xsc file required
!   Called by DRVXSC for each filename in *XSC section, and XSCDEF if wildcard.
!
! VARIABLE KINDS
    USE KIND_DAT
!
! GLOBAL DATA
    USE GASCOM_DAT ! Molecule and isotope data
    USE SPCCOM_DAT ! Spectral range data
    USE RFMLUN_DAT, ONLY: LUNTMP ! LUN for temporarily open files
!
! SUBROUTINES
    USE IDXGAS_FNC ! Index in GAS arrays of molecule,isotope
    USE MOLIDX_SUB ! Give molecule name for HITRAN/RFM index, or vice-versa
    USE OPNFIL_SUB ! Open input file
    USE REAXSC_SUB ! Load X/S data into XSCCOM
    USE SCNXSC_SUB ! Scan HITRAN format .xsc file
    USE WRTLOG_SUB ! Write text message to log file
!   
  IMPLICIT NONE 
!
! ARGUMENTS
    CHARACTER(*),  INTENT(IN)  :: NAMXSC ! Name of .xsc file 
    LOGICAL,       INTENT(OUT) :: FAIL   ! Set TRUE if a fatal error is detected
    CHARACTER(80), INTENT(OUT) :: ERRMSG ! Error message if FAIL is TRUE
!
! LOCAL VARIABLES
    LOGICAL       :: RFMFMT ! T=file in RFM format, F=original HITRAN format
    LOGICAL       :: USEFIL ! T=file contains at least one reqd dataset
    INTEGER(I4)   :: IDXMOL ! HITRAN/RFM Molecule ID
    INTEGER(I4)   :: IGAS   ! Index of molecule in GASCOM
    INTEGER(I4)   :: IOS    ! Saved value of IOSTAT for error message
    INTEGER(I4)   :: IPT    ! Counter for abs.coeff. data points
    INTEGER(I4)   :: IRNG   ! Counter for spc.ranges in orig.fmt xsc file
    INTEGER(I4)   :: IXST   ! Counter for (p,T) datasets
    INTEGER(I4)   :: NPT    ! No. abs.coeff points in (p,T) dataset
    INTEGER(I4)   :: NRNG   ! No. spc.ranges in original format .xsc file
    INTEGER(I4)   :: NXST   ! No. (p,T) datasets in spectral range
    REAL(R4)      :: RDUM   ! Dummy variable for read
    REAL(R8)      :: WNL    ! Lower wavenumber [cm-1] of spectral range
    REAL(R8)      :: WNU    ! Upper wavenumber [cm-1] of spectral range
    CHARACTER(10) :: FMT    ! .xsc file format string
    CHARACTER(20) :: C20    ! First 20 characters of first record in file
    CHARACTER(20) :: MOLEC  ! Right adjusted name of molec in orig.fmt file 
    CHARACTER(80) :: MESSGE ! Message sent to log file
    INTEGER(I4), ALLOCATABLE :: NTAB(:) ! No. p,T tabulations in each spc.range
    REAL(R8),    ALLOCATABLE :: WMAX(:) ! Upper Wno [cm-1] of each spc. range
    REAL(R8),    ALLOCATABLE :: WMIN(:) ! Lower Wno [cm-1] of each spc. range
!
! EXECUTABLE CODE --------------------------------------------------------------
!
  USEFIL = .FALSE.
!
  CALL OPNFIL ( LUNTMP, NAMXSC, FAIL, ERRMSG )
  IF ( FAIL ) RETURN
!
! Check if RFM format or original format
  READ ( LUNTMP, '(A20)', IOSTAT=IOS, ERR=900 ) C20
  RFMFMT = C20(1:3) .EQ. '***'
  BACKSPACE ( LUNTMP ) 
!
  IF ( .NOT. RFMFMT ) THEN
    MOLEC = ADJUSTL(C20)
    IDXMOL = 0
    CALL MOLIDX ( IDXMOL, MOLEC )  
    IF ( IDXMOL .GT. 0 ) THEN
      IGAS = IDXGAS ( IDXMOL )
    ELSE 
      IGAS = 0
    END IF
    IF ( IGAS .EQ. 0 ) THEN       ! molecule not required
      NRNG = 0                    ! suppress reading file in main loop
    ELSE
      CALL SCNXSC ( LUNTMP, C20, NRNG, WMIN, WMAX, NTAB, FAIL, ERRMSG )
      IF ( FAIL ) RETURN
    END IF
    IRNG = 0
  END IF
!
  DO
    IF ( RFMFMT ) THEN
      READ ( LUNTMP, '(3X,I7,I10,A10,2F10.4)', IOSTAT=IOS, ERR=900 ) &
        IDXMOL, NXST, FMT, WNL, WNU
      IF ( IOS .LT. 0 ) EXIT
      IF ( INDEX ( FMT, 'HITRAN2K' ) .EQ. 0 ) THEN
        FAIL = .TRUE.
        ERRMSG = 'F-XSCFIL: .xsc file is not HITRAN2k format'
        RETURN
      END IF
      IGAS = IDXGAS ( IDXMOL )
      IF ( IGAS .EQ. 0 ) EXIT    ! Molecule not required
    ELSE 
      IRNG = IRNG + 1
      IF ( IRNG .GT. NRNG ) EXIT
      NXST = NTAB(IRNG)
      WNL  = WMIN(IRNG)
      WNU  = WMAX(IRNG)
    END IF
!
    GAS(IGAS)%XSC = .TRUE.     ! At least one x/s file identified for molec.
!
! Check if overlap with any required RFM spectral range
    IF ( ANY ( WNU .GE. SPC%WXL .AND. WNL .LE. SPC%WXU ) ) THEN
      WRITE ( MESSGE, '(A,2F12.4,A)' ) &
        'I-XSCFIL: Loading ' // ( GAS(IGAS)%COD ) // ' data for', &
        WNL, WNU, ' [cm-1]' 
      CALL WRTLOG ( MESSGE ) 
      CALL REAXSC ( LUNTMP, IGAS, NXST, FAIL, ERRMSG ) 
      IF ( FAIL ) RETURN
      USEFIL = .TRUE.
    ELSE
      DO IXST = 1, NXST
        READ ( LUNTMP, '(40X,I7)', IOSTAT=IOS, ERR=900) NPT
        READ ( LUNTMP, '(10E10.3)', IOSTAT=IOS, ERR=900 ) ( RDUM, IPT = 1, NPT )
      END DO
    END IF
  END DO
!
  CLOSE ( LUNTMP )
!
  IF ( .NOT. USEFIL ) THEN
    IF ( IGAS .EQ. 0 ) THEN
      CALL WRTLOG ('W-XSCFIL: File ignored - Molecule not required')
    ELSE
      CALL WRTLOG ('W-XSCFIL: File ignored - spectral range(s) not required')
    END IF
  END IF
!
900 CONTINUE
  FAIL = IOS .GT. 0 
  IF ( FAIL ) WRITE ( ERRMSG, * ) &
    'F-XSCFIL: Error reading X/S file. IOSTAT=', IOS
!
END SUBROUTINE XSCFIL
END MODULE XSCFIL_SUB

