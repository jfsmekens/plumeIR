MODULE SCNXSC_SUB
CONTAINS
SUBROUTINE SCNXSC ( LUN, MOLEC, NSPC, WMIN, WMAX, NTAB, FAIL, ERRMSG )
!
! VERSION
!   21JUN17 AD Original. Checked.
!
! DESCRIPTION
!   Scan HITRAN format .xsc file
!   Called by XSCFIL if .xsc file identified as original HITRAN format.
!
! VARIABLE KINDS
    USE KIND_DAT
!
  IMPLICIT NONE 
!
! ARGUMENTS
    INTEGER(I4),   INTENT(IN)  :: LUN     ! LUN for accessing file
    CHARACTER(20), INTENT(IN)  :: MOLEC   ! Expected Molecule name
    INTEGER(I4),   INTENT(OUT) :: NSPC    ! No. spectral ranges in file
    REAL(R8), ALLOCATABLE, &
                   INTENT(OUT) :: WMIN(:) ! Lower wno [cm-1] of spectral range
    REAL(R8), ALLOCATABLE, &
                   INTENT(OUT) :: WMAX(:) ! Upper wno [cm-1] of spectral range
    INTEGER(I4), ALLOCATABLE, &
                   INTENT(OUT) :: NTAB(:) ! No p,T tabulations for each spc.rng
    LOGICAL,       INTENT(OUT) :: FAIL    ! Set TRUE if fatal error is detected
    CHARACTER(80), INTENT(OUT) :: ERRMSG  ! Error message if FAIL is TRUE
!
! LOCAL VARIABLES
    INTEGER(I4)   :: I      ! Counter for abs.coeff values
    INTEGER(I4)   :: IOS    ! Saved value of IOSTAT for error message
    INTEGER(I4)   :: N      ! No. abs.coeff values within each tabulation
    REAL(R4)      :: R      ! Dummy variable for reading abs.coeff values
    REAL(R8)      :: W1     ! Lower Wno [cm-1] for particular tabulation
    REAL(R8)      :: W2     ! Upper Wno [cm-1] for particular tabulation
    CHARACTER(20) :: NMOLEC ! Molecule name for each tabulation
    INTEGER(I4), ALLOCATABLE :: NSAV(:) ! Saved NTAB during reallocation
    REAL(R8),    ALLOCATABLE :: WSAV(:) ! Saved WMIN, WMAX during reallocation
!
! EXECUTABLE CODE --------------------------------------------------------------
!
  NSPC = 0
  ALLOCATE ( WMIN(1), WMAX(1), NTAB(1) ) 
  DO
    READ ( LUN, '(A20,2F10.4,I7)', END=100, ERR=900, IOSTAT=IOS ) &
      NMOLEC, W1, W2, N
    IF ( NMOLEC .NE. MOLEC ) THEN
      FAIL = .TRUE.
      ERRMSG = 'F-SCNXSC: Inconsistent Molecule names in file: ' // &
               TRIM ( MOLEC ) // '&' // TRIM ( NMOLEC ) 
      RETURN
    END IF 
!
    IF ( NSPC .EQ. 0  ) THEN    ! Flag for reading first tabulation
      NSPC = 1
      WMIN(1) = W1
      WMAX(1) = W2
      NTAB(1) = 0
    END IF
!
    IF ( W1 .GT. WMAX(NSPC) .OR. W2 .LT. WMIN(NSPC) ) THEN ! New spectral range
      NSPC = NSPC + 1
      CALL MOVE_ALLOC ( NTAB, NSAV ) 
      ALLOCATE ( NTAB(NSPC) ) 
      NTAB(1:NSPC-1) = NSAV
      NTAB(NSPC) = 1
      CALL MOVE_ALLOC ( WMIN, WSAV ) 
      ALLOCATE ( WMIN(NSPC) ) 
      WMIN(1:NSPC-1) = WSAV
      WMIN(NSPC) = W1
      CALL MOVE_ALLOC ( WMAX, WSAV ) 
      ALLOCATE ( WMAX(NSPC) ) 
      WMAX(1:NSPC-1) = WSAV
      WMAX(NSPC) = W2
!
    ELSE                                      ! same spectral range
      NTAB(NSPC) = NTAB(NSPC) + 1
      WMIN(NSPC) = MIN ( W1, WMIN(NSPC) ) 
      WMAX(NSPC) = MAX ( W2, WMAX(NSPC) ) 
    END IF
!
    READ ( LUN, '(10E10.3)', IOSTAT=IOS, ERR=900 ) ( R, I = 1, N )
  END DO
!
100 CONTINUE
  REWIND ( LUN ) 
!
900 CONTINUE
  FAIL = IOS .GT. 0 
  IF ( FAIL ) WRITE ( ERRMSG, * ) &
    'F-SCNXSC: Error reading X/S file. IOSTAT=', IOS
!
END SUBROUTINE SCNXSC
END MODULE SCNXSC_SUB

