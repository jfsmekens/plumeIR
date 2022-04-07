MODULE INIHFL_SUB
CONTAINS
SUBROUTINE INIHFL ( WNOREQ, FAIL, ERRMSG )
!
! VERSION
!   13JUN17 AD Add INIPAR
!   01MAY17 AD F90 conversion. Checked.
!
! DESCRIPTION    
!   Initialise the HITRAN line data file
!   Called by SPCWID and SPCFIN once for each spectral range.
!   Set the forward pointers for each required gas
!   and set current record to required wavenumber
!
! VARIABLE KINDS
    USE KIND_DAT  
!
! GLOBAL DATA
    USE HFLCOM_DAT ! HITRAN file data
    USE HITCOM_DAT ! HITRAN line data
    USE RFMLUN_DAT, ONLY: LUNHIT ! LUN for HITRAN binary file
!
! SUBROUTINES
    USE INIPAR_SUB ! Initialise the HITRAN ASCII line parameter file
!
  IMPLICIT NONE
!
! ARGUMENTS      
    REAL(R8),      INTENT(IN)  :: WNOREQ ! Initial wavenumber
    LOGICAL,       INTENT(OUT) :: FAIL   ! Set TRUE if a fatal error is detected
    CHARACTER(80), INTENT(OUT) :: ERRMSG ! Error message written if FAIL is TRUE
!
! LOCAL VARIABLES
    INTEGER(I4)  :: FPTEMP(14) ! Forward pointers from HITRAN file
    INTEGER(I4)  :: I          ! Dummy read
    INTEGER(I4)  :: IDG1       ! HITRAN gas ID for start of Fwd Pointer block
    INTEGER(I4)  :: IDXMOL     ! HITRAN gas ID
    INTEGER(I4)  :: IOS        ! Value of IOSTAT for error messages
    INTEGER(I4)  :: IPTR       ! Counter for forward pointers
    INTEGER(I4)  :: IREC       ! Record#
    INTEGER(I4)  :: K,L        ! Record#
    INTEGER(I4)  :: LSTAT      ! Status of transition information.
    INTEGER(I4)  :: NGSREQ     ! Number of different gases required from file
    REAL(R4)     :: TPROB      ! Transition probability [Debyes2].
    REAL(R8)     :: DDUMMY     ! Dummy variable for read
    REAL(R8)     :: WNUM       ! Wavenumber of HITRAN record
    CHARACTER(9) :: SPARE9     ! Spare bytes in binary file record
!
! EXECUTABLE CODE -------------------------------------------------------------
!
  WNOHFL = 0.0D0         ! Set when actual data is read in by REACYC
  IF ( PARHFL ) THEN
    CALL INIPAR ( WNOREQ, FAIL, ERRMSG )
    RETURN
  END IF
!       
! Binary search for 1st record: follows code in the IBM SLUP routine:
! K is first record, L is last. Only need to read WNUM for search
  K = IR1HFL
  L = IR2HFL
  DO WHILE ( K + 1 .LT. L )       
    IREC = (K + L) / 2
    READ ( LUNHIT, REC=IREC, IOSTAT=IOS, ERR=900 ) I, I, I, WNUM
    IF ( WNUM .LT. WNOREQ ) THEN   ! Reqd WN higher, try half way up to L
      K = IREC 
    ELSE                     ! Reqd WN lower or =, try half way down to K
      L = IREC 
    ENDIF
  END DO
!
! If K & L differ by 1, have found required location where the K .LT. DWNLOW 
! and L .GE. DWNLOW, Choose record L (note K<L). If there is more than 1 
! record at exactly DWNLOW, will finish pointing to first. 
! Forward pointer block are labelled with wavenumber of next line to exploit 
! this. 
!
  IRCHFL = L
!
! Now set the initial forward pointers for each gas
! First determine which HITRAN Gas IDs are required from this file
! (IDG < MAXPTR for all reqd line molecules checked earlier in HITFIL)
  IFPHFL = IR2HFL ! initialise all fwd pointers to end-of-file
!
  NGSREQ = COUNT ( USEIDG ) 
!
! Step back until all pointers are set 
  IREC = IRCHFL
!
! It is possible that IREC is positioned at the start of the forward pointer
! block at the start of the file, so need to advance to the end of this block
! so that pointers for each molecule can be read from the block
  DO
    READ ( LUNHIT, REC=IREC, IOSTAT=IOS, ERR=900 ) LSTAT, IDXMOL
    IF ( LSTAT .NE. -7 ) EXIT
    IREC = IREC + 1
  END DO
!
  DO WHILE ( NGSREQ .GT. 0 ) 
    IREC = IREC - 1
! There should be a forward pointer for every gas 
    IF ( IREC .EQ. 0 ) STOP 'F-INIHFL: Logical error'
    READ ( LUNHIT, REC=IREC, IOSTAT=IOS, ERR=900 ) LSTAT, IDXMOL
    IF ( LSTAT .GE. 10 .AND. USEIDG(IDXMOL) ) THEN  ! Found a molec. line rec.
      READ ( LUNHIT, REC=IREC, IOSTAT=IOS, ERR=900 ) &       ! Load /HITCOM/
        LSTAT, HIT%IDM, HIT%IDI, HIT%WNO, HIT%STR,  TPROB, &
        HIT%ABR, HIT%SBR, HIT%ELS, HIT%ABC, HIT%TSP, HIT%IUS, HIT%ILS, &
        HIT%ULQ, HIT%BLQ, SPARE9, IFWDPT
      IF ( IFPHFL(IDXMOL) .EQ. IR2HFL ) THEN
        NGSREQ = NGSREQ - 1
        IFPHFL(IDXMOL) = IREC + IFWDPT
      END IF
    ELSE IF ( LSTAT .EQ. -7 ) THEN         ! Found a forward pointer record
      READ ( LUNHIT, REC=IREC, ERR=900, IOSTAT=IOS ) &
        LSTAT, IDG1, I, DDUMMY, ( FPTEMP(IPTR), IPTR = 1, 14 )
      DO IPTR = 1, 14
        IDXMOL = IDG1 + IPTR - 1
        IF ( USEIDG(IDXMOL) ) THEN
          IF ( IFPHFL(IDXMOL) .EQ. IR2HFL ) THEN
            NGSREQ = NGSREQ - 1
            IFPHFL(IDXMOL) = IREC + FPTEMP(IPTR)
          END IF
        END IF
      END DO
    END IF   
  END DO          
!
! Also use this opportunity to initialise numbers which will then remain zero
! if LTE calculation being used
  HIT%IUV = 0
  HIT%ILV = 0
!
900 CONTINUE
  FAIL = IOS .NE. 0
  IF ( FAIL ) WRITE ( ERRMSG, * ) &
    'F-INIHFL: Failed to read HITRAN file, rec#:', IREC, '. IOSTAT=', IOS
!
END SUBROUTINE INIHFL
END MODULE INIHFL_SUB
