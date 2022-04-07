MODULE INILBL_SUB
CONTAINS
SUBROUTINE INILBL
!
! VERSION
!   01MAY17 AD F90 original. Checked.
!
! DESCRIPTION  
!   Initialise line-by-line calc segments
!   Called by SPCINI at start of each spectral range
!   Only needs to be called once unless LUTs are used, which are an alternative
!   source of absorption data to line-by-line calculations.
!
! VARIABLE KINDS
    USE KIND_DAT  
!
! GLOBAL DATA
    USE CLCCOM_DAT ! Calculated path segments
    USE GASCOM_DAT ! Molecule and isotope data
    USE WIDCOM_DAT, ONLY: NLBL, IDXLBL ! line-by-line calc paths
    USE HFLCOM_DAT, ONLY: USEIDG ! T=need FP for this molecule
!
  IMPLICIT NONE
!
! LOCAL VARIABLES
    INTEGER(I4) :: ICLC   ! Index of calculated path segment
    INTEGER(I4) :: IDXMOL ! HITRAN/RFM index of (line) molecule
    INTEGER(I4) :: IGAS   ! Counter for absorbers
    INTEGER(I4) :: ILBL   ! Index of line-by-line calc.path segment
!
! EXECUTABLE CODE -------------------------------------------------------------
!
  USEIDG = .FALSE.
  CLC%LBL = .FALSE.
  DO IGAS = 1, NGAS
    IF ( GAS(IGAS)%HIT ) THEN
      IDXMOL = GAS(IGAS)%IDM
      USEIDG(IDXMOL) = .TRUE.
      WHERE ( CLC%IGS .EQ. IGAS ) CLC%LBL = .TRUE.
    ELSE IF ( GAS(IGAS)%CTM ) THEN
! Also need to flag continuum-only gases for LBL calc since continuum is
! interpolated to widemesh and them subsequently interpolated to finemesh in
! the same way as line wing contributions
      WHERE ( CLC%IGS .EQ. IGAS ) CLC%LBL = .TRUE.
    END IF
  END DO
!
  NLBL = COUNT ( CLC%LBL ) 
  IF ( ALLOCATED ( IDXLBL ) ) DEALLOCATE ( IDXLBL ) 
  ALLOCATE ( IDXLBL(NLBL) ) 
  ILBL = 0
  DO ICLC = 1, NCLC
    IF ( CLC(ICLC)%LBL ) THEN
      ILBL = ILBL + 1
      IDXLBL(ILBL) = ICLC
    END IF
  END DO
!  
END SUBROUTINE INILBL
END MODULE INILBL_SUB
