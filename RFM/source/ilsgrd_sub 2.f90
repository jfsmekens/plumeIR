MODULE ILSGRD_SUB
CONTAINS
SUBROUTINE ILSGRD ( IFNOUT, IP1ILS, IP2ILS, FNCILS, IGRD1, IGRD2, FNCIRR ) 
!
! VERSION
!   01MAY17 AD F90 original. Checked.
!
! DESCRIPTION
!   Calculate ILS fn for irregular grid
!   Called by SPCILS if ILS or AVG flags enabled with irregular grid
!
! VARIABLE KINDS
    USE KIND_DAT
!
! GLOBAL DATA
    USE GRDCOM_DAT ! Irregular grid
!
  IMPLICIT NONE
!
! ARGUMENTS
    INTEGER(I4), INTENT(IN)    :: IFNOUT    ! Output pt on fine grid
    INTEGER(I4), INTENT(IN)    :: IP1ILS    ! Lower bound of ILS function
    INTEGER(I4), INTENT(IN)    :: IP2ILS    ! Upper bound of ILS function
    REAL(R8),    INTENT(IN)    :: FNCILS(IP1ILS:IP2ILS) ! ILS function
    INTEGER(I4), INTENT(INOUT) :: IGRD1     ! Lower irreg grid pt for convol.
    INTEGER(I4), INTENT(OUT)   :: IGRD2     ! Upper irreg grid pt for convol. 
    REAL(R8), ALLOCATABLE, &
                 INTENT(OUT)   :: FNCIRR(:) ! Convolution weights for irr.grid
!
! LOCAL VARIABLES
    INTEGER(I4) :: IFIN    ! Counter for fine grid points
    INTEGER(I4) :: IFINGL  ! Fine grid index of lower irreg grid point
    INTEGER(I4) :: IFINGU  ! Fine grid index of upper irreg grid point
    INTEGER(I4) :: IFINL   ! Index of lower fine grid point
    INTEGER(I4) :: IFINU   ! Index of upper fine grid point
    INTEGER(I4) :: IGRD    ! Counter for irreg.grid points
    INTEGER(I4) :: IPILS   ! Counter for ILS function fine grid points
    REAL(R8)    :: A       ! Interpolation weight 
!
! EXECUTABLE CODE -------------------------------------------------------------
!
! Index of fine grid point at lower limit of ILS function
  IFINL = IFNOUT + IP1ILS
  IFINU = IFNOUT + IP2ILS
!
! If grd point corresponding to IGRD1 is above IFINL then reset to start of grid
  IF ( IGRD1 .GT. NGRD ) IGRD1 = 1        ! ensure GRD(IGRD1) can be accessed
  IF ( GRD(IGRD1)%IFN .GT. IFINL ) IGRD1 = 1     
!
! Increase IGRD1 until next grid point lies above IFINL (so IGRD1 .le. IFINL)
  DO WHILE ( GRD(IGRD1+1)%IFN .LE. IFINL ) 
    IGRD1 = IGRD1 + 1
  END DO 
! 
  IGRD2 = IGRD1 + 1
  DO WHILE ( GRD(IGRD2)%IFN .LT. IFINU )
    IGRD2 = IGRD2 + 1                     ! end with IGRD2 .ge. IFINU
  END DO
!
  IF ( ALLOCATED ( FNCIRR ) ) DEALLOCATE ( FNCIRR ) 
  ALLOCATE ( FNCIRR(IGRD1:IGRD2) ) 
  FNCIRR = 0.0D0
!
! Indices on full fine grid of lower,upper irreg grid points
  IFINGL = GRD(IGRD1)%IFN
  IFINGU = GRD(IGRD1+1)%IFN
!
  IGRD = IGRD1
  DO IPILS = IP1ILS, IP2ILS       ! Loop over ILS function width
    IFIN = IFNOUT + IPILS         ! Index on fine grid of ILS fn point
    A = DBLE ( ( IFIN - IFINGL ) / ( IFINGU - IFINGL ) )
    FNCIRR(IGRD) = FNCIRR(IGRD) + (1.0-A) * FNCILS(IPILS)
    FNCIRR(IGRD+1) = FNCIRR(IGRD+1) + A * FNCILS(IPILS)
    IF ( IFIN .EQ. IFINGU .AND. IPILS .LT. IP2ILS ) THEN ! next irreg grid pt
      IGRD = IGRD + 1
      IFINGL = IFINGU
      IFINGU = GRD(IGRD+1)%IFN
    END IF
  END DO
!
  IF ( IGRD2 .NE. IGRD+1 ) STOP 'F-ILSGRD: Logical error'
!
END SUBROUTINE ILSGRD
END MODULE ILSGRD_SUB

