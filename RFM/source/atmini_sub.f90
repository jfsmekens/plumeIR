MODULE ATMINI_SUB
CONTAINS
SUBROUTINE ATMINI ( LEVGRD, PREGRD )
!
! VERSION
!   21JUN17 AD Allow argument to be lnp or alt.
!   01MAY17 AD F90 original. Checked.
!
! DESCRIPTION
!   Initialise atmospheric profile data in ATMCOM
!   Called once by ATMGRD.
!
! VARIABLE KINDS
    USE KIND_DAT
!
! GLOBAL DATA
    USE ATMCOM_DAT ! Atmospheric profile data
    USE GASCOM_DAT ! Molecule and isotope data
    USE FLGCOM_DAT, ONLY: LINFLG ! T = Assume VMR varies linearly with altitude
!
  IMPLICIT NONE
!
! ARGUMENTS
    REAL(R4), INTENT(IN) :: LEVGRD(:) ! Profile grid levels (km or ln(p) [mb])
    LOGICAL,  INTENT(IN) :: PREGRD    ! T=pressure grid, F=height grid
!
! EXECUTABLE CODE -------------------------------------------------------------
!
  NATM = SIZE ( LEVGRD )
  FIXPRE = PREGRD
!
  ALLOCATE ( HGTATM(NATM) ) 
  ALLOCATE ( TEMATM(NATM) ) 
  ALLOCATE ( PREATM(NATM) ) 
!
  IF ( FIXPRE ) THEN
    PREATM = LEVGRD 
    SETPRE = .TRUE.
    PRESFC = PREATM(1)
    ALLOCATE ( LNPATM(NATM) ) 
    LNPATM = LOG ( PREATM ) 
    LEVATM => LNPATM
  ELSE
    HGTATM = LEVGRD
    SETHGT = .TRUE.
    HGTTOA = HGTATM(NATM)
    HGTSFC = HGTATM(1)
    LEVATM => HGTATM
  END IF
!
  NVMR = NGAS
  ALLOCATE ( SETVMR(NVMR) ) ; SETVMR = .FALSE.
  ALLOCATE ( LINVMR(NVMR) ) ; LINVMR = LINFLG
  IAXVMR = IAXGAS
  IF ( IAXVMR .GT. 0 ) LINVMR(IAXVMR) = .FALSE.  ! Aer always linear interp.
  ALLOCATE ( NAMVMR(NVMR) ) ; NAMVMR = GAS%COD
  ALLOCATE ( NTEVMR(NVMR) ) ; NTEVMR = GAS%NTE
!
  ALLOCATE ( VMRATM(NATM,NVMR) )
!
END SUBROUTINE ATMINI
END MODULE ATMINI_SUB
