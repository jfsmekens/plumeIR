MODULE FLXEFN_SUB
CONTAINS
SUBROUTINE FLXEFN ( ABSLEV, IATM )
!
! VERSION
!   01MAY17 AD F90 conversion. Checked.
!
! DESCRIPTION
!   Calculate absorption coefficient at atmospheric level
!   Called by SPCFLX for each output level with TRA flag.
!
! VARIABLE KINDS
    USE KIND_DAT
!
! GLOBAL DATA
    USE PTHCOM_DAT ! Path segment data
    USE ATMCOM_DAT, ONLY: NVMR ! No. different absorbers
    USE FINCOM_DAT, ONLY: ABSFIN ! Fine mesh absorption
!
! SUBROUTINES
    USE IDXPTH_FNC ! Index in PTHCOM of tan/atm/gas/dir
!
  IMPLICIT NONE
!
! ARGUMENTS
    INTEGER(I4), INTENT(IN)  :: IATM      ! Index of atmospheric profile level
    REAL(R8),    INTENT(OUT) :: ABSLEV(:) ! Absorption coefficient [/cm2]
!
! LOCAL VARIABLES
    INTEGER(I4) :: ICLC ! Index of calculated path
    INTEGER(I4) :: IPTH ! Path counter
    INTEGER(I4) :: IVMR ! Absorber counter
!
! EXECUTABLE CODE -------------------------------------------------------------
!
! Initialise 
  ABSLEV = 0.0D0
!
! Sum over absorbing species
  DO IVMR = 1, NVMR
    IPTH = IDXPTH ( 1, IATM, IVMR, 2 )   ! Special case of IDIR=2
    IF ( IPTH .EQ. 0 ) STOP 'F-FLXEFN: Logical error'
    ICLC = PTH(IPTH)%ICL
    ABSLEV = ABSLEV + DBLE ( ABSFIN(:,ICLC) * PTH(IPTH)%AMT ) 
  END DO
!
END SUBROUTINE FLXEFN
END MODULE FLXEFN_SUB
