MODULE ADDPTH_SUB
CONTAINS
SUBROUTINE ADDPTH ( PTB, IPTH, ORG, JPTH )
!
! VERSION
!   01MAY17 AD F90 version. Checked.
!
! DESCRIPTION
!   Add perturbed path segment
!   Called by JACPTH
!
! GLOBAL DATA
    USE PTBCON_DAT ! Jacobian perturbation sizes
    USE PTHCOM_DAT, ONLY: PTHTYP ! Path data type
!
! SUBROUTINES
    USE ADDCLC_SUB ! Set calculated paths
!
  IMPLICIT NONE
!
! ARGUMENTS
    TYPE(PTHTYP), INTENT(IN)   :: PTB    ! Perturbed path
    INTEGER(I4),  INTENT(IN)   :: IPTH   ! Index of perturbed path
    TYPE(PTHTYP), ALLOCATABLE, &
                INTENT(INOUT)  :: ORG(:) ! Set of unperturbed paths
    INTEGER(I4),  INTENT(OUT)  :: JPTH   ! Index of new path
!
! LOCAL CONSTANTS
    REAL(R4), PARAMETER :: SIGPTH = 0.01 ! Fraction of ptb requiring new path
!
! LOCAL VARIABLES
    LOGICAL :: NEWCLC ! Set T if new calc path also required
    TYPE(PTHTYP), ALLOCATABLE :: PTHSAV(:) ! Saved ORG during reallocation
!
! EXECUTABLE CODE ------------------------------------------------------------
!
  NEWCLC = ( ABS ( PTB%PRE - ORG(IPTH)%PRE ) .GT. &
             ABS ( SIGPTH * PTBPRE * ORG(IPTH)%PRE )       ) .OR. &
           ( ABS ( PTB%TEM - ORG(IPTH)%TEM ) .GT. &
             ABS ( SIGPTH * PTBTEM )                       )
  IF ( NEWCLC .OR. &
           ( ABS ( PTB%AMT - ORG(IPTH)%AMT ) .GT. &
             ABS ( SIGPTH * PTBVMR * ORG(IPTH)%AMT )       ) ) THEN
    JPTH = SIZE ( ORG ) 
    CALL MOVE_ALLOC ( ORG, PTHSAV ) 
    JPTH = JPTH + 1
    ALLOCATE ( ORG(JPTH) )
    ORG(1:JPTH-1) = PTHSAV
    ORG(JPTH) = PTB
    IF ( NEWCLC ) THEN
      CALL ADDCLC ( ORG(JPTH), .TRUE. )
    ELSE
      ORG(JPTH)%ICL = ORG(IPTH)%ICL
    END IF
  ELSE
    JPTH = 0
  END IF
!
END SUBROUTINE ADDPTH
END MODULE ADDPTH_SUB
