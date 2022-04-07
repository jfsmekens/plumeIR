MODULE REAXSC_SUB
CONTAINS
SUBROUTINE REAXSC ( LUN, IGAS, NXT, FAIL, ERRMSG )
!
! VERSION
!   08FEB19 AD Bug#16: Extract no.triangles NTRI from TRIANG.
!   01MAY17 AD F90 conversion. Checked.
! 
! DESCRIPTION    
!   Load X/S data into XSCCOM
!   Called by XSCFIL for every required X/S dataset 
!
! VARIABLE KINDS
    USE KIND_DAT 
!
! GLOBAL DATA
    USE XSCCOM_DAT ! Tabulated Cross-Section data
!
! SUBROUTINES
    USE TRIANG_SUB ! 2-D interpolation of irregular grid using triangulation
!
  IMPLICIT NONE
!
! ARGUMENTS      
    INTEGER(I4),   INTENT(IN)  :: LUN    ! LUN for reading X/S data
    INTEGER(I4),   INTENT(IN)  :: IGAS   ! Index of absorber in GASCOM
    INTEGER(I4),   INTENT(IN)  :: NXT    ! No. (p,T) tabulations to read
    LOGICAL,       INTENT(OUT) :: FAIL   ! Set TRUE if a fatal error is detected
    CHARACTER(80), INTENT(OUT) :: ERRMSG ! Error message written if FAIL is TRUE
!
! LOCAL VARIABLES
    INTEGER(I4) :: IOF  ! Offset for each (p,T) tabulation in ABS array
    INTEGER(I4) :: IOS  ! Saved value of IOSTAT for error messages
    INTEGER(I4) :: IPT  ! Counter for data points
    INTEGER(I4) :: IXT  ! Counter for X/S (p,T) tables
    INTEGER(I4) :: NPT  ! No. of X/S data points in current (p,T) table
    INTEGER(I4) :: NTRI ! No. triangles found
    REAL(R8)    :: WN1  ! Lower wno limit [cm-1] of x/s tabulation
    REAL(R8)    :: WN2  ! Upper wno limit [cm-1] of x/s tabulation
    INTEGER(I4),  ALLOCATABLE :: IDXTRI(:,:) ! Triangle coordinates
    REAL(R4),     ALLOCATABLE :: ABSDAT(:)   ! Abs.Coeff values stored locally
    REAL(R4),     ALLOCATABLE :: ABSSAV(:)   ! Saved ABSDAT during reallocation
    TYPE(XSCTYP), ALLOCATABLE :: XSCSAV(:)   ! Saved XSC during reallocation
!
! EXECUTABLE CODE -------------------------------------------------------------
!
  IF ( ALLOCATED ( XSC ) ) CALL MOVE_ALLOC ( XSC, XSCSAV ) 
  NXSC = NXSC + 1
  ALLOCATE ( XSC(NXSC) ) 
  IF ( ALLOCATED ( XSCSAV ) ) XSC(1:NXSC-1) = XSCSAV
!
  XSC(NXSC)%IGS = IGAS
  XSC(NXSC)%NXT = NXT                                ! No.(p,T) tables
  ALLOCATE ( XSC(NXSC)%IOF(NXT) )
  ALLOCATE ( XSC(NXSC)%NPT(NXT) )
  ALLOCATE ( XSC(NXSC)%PRE(NXT) ) 
  ALLOCATE ( XSC(NXSC)%TEM(NXT) )
  ALLOCATE ( XSC(NXSC)%WN1(NXT) )
  ALLOCATE ( XSC(NXSC)%DWN(NXT) )
!
  IOF = 0
  DO IXT = 1, NXT
    READ ( LUN, '(20X,2F10.4,I7,F7.2,F6.1)', IOSTAT=IOS, ERR=900) &
      WN1, WN2, NPT, XSC(NXSC)%TEM(IXT), XSC(NXSC)%PRE(IXT) 
    XSC(NXSC)%WN1(IXT) = WN1
    XSC(NXSC)%IOF(IXT) = IOF
    XSC(NXSC)%NPT(IXT) = NPT
    XSC(NXSC)%DWN(IXT) = ( WN2 - XSC(NXSC)%WN1(IXT) ) / ( NPT - 1.0D0 )
    IF ( IXT .EQ. 1 ) THEN
      ALLOCATE ( ABSDAT(NPT*(1+NXT)) )   ! Assign larger than expected size
      XSC(NXSC)%WNL = WN1
      XSC(NXSC)%WNU = WN2
    ELSE IF ( SIZE(ABSDAT) .LT. IOF + NPT ) THEN
      CALL MOVE_ALLOC ( ABSDAT, ABSSAV )
      ALLOCATE ( ABSDAT(IOF+NPT) )
      ABSDAT(1:IOF) = ABSSAV(1:IOF)
      XSC(NXSC)%WNL = MIN ( XSC(NXSC)%WNL, WN1 ) 
      XSC(NXSC)%WNU = MAX ( XSC(NXSC)%WNU, WN2 ) 
    END IF
    READ ( LUN, '(10E10.3)', IOSTAT=IOS, ERR=900 ) &
      ( ABSDAT(IPT), IPT = IOF + 1, IOF + NPT )
    IOF = IOF + NPT    
  END DO
  XSC(NXSC)%NXP = IOF
  ALLOCATE ( XSC(NXSC)%ABS(IOF) ) 
  XSC(NXSC)%ABS = ABSDAT(1:IOF)
!
  CALL TRIANG ( NXT, XSC(NXSC)%TEM, XSC(NXSC)%PRE, NTRI, IDXTRI )
  XSC(NXSC)%NTRI = NTRI
  ALLOCATE ( XSC(NXSC)%ITRI(NTRI,3) )
  XSC(NXSC)%ITRI = IDXTRI
!
900 CONTINUE
  FAIL = IOS .NE. 0
  IF ( FAIL ) WRITE ( ERRMSG, * ) &
    'F-REAXSC: Read error on .xsc file. IOSTAT=', IOS
!
END SUBROUTINE REAXSC
END MODULE REAXSC_SUB
