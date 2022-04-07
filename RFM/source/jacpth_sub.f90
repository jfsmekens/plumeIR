MODULE JACPTH_SUB
CONTAINS
SUBROUTINE JACPTH
!
! VERSION
!   05MAR19 AD Split LIMPTH into LIMPTH and GRAPTH. Add JACSKY
!   04FEB19 AD Allow for 'los' Jacobian
!   30MAY18 AD Bug#7: Correct message listing no.new tan.paths
!   02MAY18 AD Bug#1: Add JACFOV
!   01MAY17 AD F90 conversion of rfmptb.for. Checked.
!
! DESCRIPTION
!   Set up list of pertubations required for RFM Jacobian calc
!   Called once by RFMPTH if JAC option enabled.
!
! VARIABLE KINDS
    USE KIND_DAT
!
! GLOBAL DATA
    USE FLGCOM_DAT ! Option flags
    USE JACCOM_DAT ! Jacobian data
    USE PTHCOM_DAT ! Path segment data
    USE TANCOM_DAT ! Tangent path data
    USE CLCCOM_DAT, ONLY: NCLC ! No. calculated path segments
!
! SUBROUTINES
    USE ADDPTH_SUB ! Add perturbed path segment
    USE ADDTAN_SUB ! Add tangent ray path
    USE C11INT_FNC ! Write integer as left-adjusted string
    USE FLXPTH_SUB ! Set up paths for flux calculations
    USE GRAPTH_SUB ! Set 2D Limb-viewing paths
    USE HOMPTH_SUB ! Set Homogeneous paths
    USE JACFOV_SUB ! Assign tan paths for Jacobians after FOV convolution
    USE JACSKY_SUB ! Allocate diffuse sky tan paths for Jacobians
    USE JTPPTH_FNC ! T=path corresponds to Jacobian Tan.Pt perturbation
    USE LIMPTH_SUB ! Set Limb-viewing paths
    USE PTBATM_SUB ! Perturb/unperturb atmospheric profiles for Jacobian calc
    USE VRTPTH_SUB ! Set Vertical-viewing paths
    USE WRTLOG_SUB ! Write text message to log file
!
  IMPLICIT NONE
!
! LOCAL VARIABLES
    INTEGER(I4)   :: IJAC   ! Jacobian element counter
    INTEGER(I4)   :: IPTH   ! Counter for unperturbed paths
    INTEGER(I4)   :: ITAN   ! Unperturbed tangent ray indices
    INTEGER(I4)   :: JPTH   ! Index of any new added Jacobian path
    INTEGER(I4)   :: LCLC   ! Saved value of NCLC on entry
    INTEGER(I4)   :: LPTH   ! Saved value of NPTH on entry
    INTEGER(I4)   :: LTAN   ! Saved value of MTAN on entry
    INTEGER(I4)   :: MPTH   ! Size of NEWPTH
    INTEGER(I4)   :: MTANJ  ! Saved updated MTAN
    CHARACTER(80) :: MESSGE ! Text message for log file
    TYPE(PTHTYP), ALLOCATABLE :: NEWPTH(:) ! Unptb paths plus Jacob.paths
!
! EXECUTABLE CODE ------------------------------------------------------------
!
  LCLC = NCLC
  LPTH = NPTH
  MPTH = NPTH
  CALL MOVE_ALLOC ( PTH, NEWPTH ) 
  NTNJAC = MTAN
  ALLOCATE ( ITNJAC(NTNJAC,NJAC) )
  ITNJAC = 0
  LTAN = MTAN   
!
  DO IJAC = 1, NJAC
    SELECT CASE ( JAC(IJAC)%JDX )
    CASE ( JDXTEM, JDXPRE, 1:JDXVMR )   ! Atmospheric perturbation
      CALL PTBATM ( IJAC ) ! Also undo any previous atm profile perturbation
! Recalculate paths in PTH for perturbed atmosphere
! Path calculations use MTAN, but MTAN also being updated as new Jacobian
! tangent rays added, so temporarily reset MTAN to original value
      MTANJ = MTAN
      MTAN = LTAN  ! reset for perturbed path calculation
      IF ( HOMFLG ) THEN
        CALL HOMPTH
      ELSE IF ( FLXFLG ) THEN
        CALL FLXPTH
      ELSE IF ( NADFLG .OR. ZENFLG ) THEN
        CALL VRTPTH
      ELSE IF ( GRAFLG ) THEN
        CALL GRAPTH
      ELSE
        CALL LIMPTH
      END IF
      MTAN = MTANJ
! Loop over all paths to see which have changed signficantly
      DO IPTH = 1, NPTH
        IF ( JTPFLG .AND. .NOT. JTPPTH ( IPTH, IJAC ) ) CYCLE
        CALL ADDPTH ( PTH(IPTH), IPTH, NEWPTH, JPTH ) 
        IF ( JPTH .EQ. 0 ) CYCLE          ! No significant change in path
        MPTH = JPTH
        ITAN = PTH(IPTH)%ITN
        IF ( ITNJAC(ITAN,IJAC) .EQ. 0 ) THEN  ! Assign new ray path
          CALL ADDTAN ( ITAN, .TRUE. )    ! increments MTAN
          ITNJAC(ITAN,IJAC) = MTAN
          TAN(MTAN)%JDX = JAC(IJAC)%JDX
        END IF
        NEWPTH(JPTH)%ITN = ITNJAC(ITAN,IJAC)
      END DO       
      DEALLOCATE ( PTH ) 
    CASE ( JDXSFE, JDXSFT )  ! Assign new ray paths for any surface ray
      DO ITAN = 1, LTAN
        IF ( TAN(ITAN)%SFC ) THEN
          CALL ADDTAN ( ITAN, .TRUE. )    ! increments MTAN
          ITNJAC(ITAN,IJAC) = MTAN
          TAN(MTAN)%JDX = JAC(IJAC)%JDX
        END IF
      END DO  
    CASE DEFAULT
      IF ( JAC(IJAC)%COD .NE. 'los' ) STOP 'F-JACPTH: Logical error'
    END SELECT
  END DO
!
! Undo last atmospheric profile perturbation (if any)
  CALL PTBATM ( 0 ) 
!
! Move all unperturbed and perturbed paths back to PTH 
  CALL MOVE_ALLOC ( NEWPTH, PTH ) 
  NPTH = MPTH
!
! Ensure tan paths are assigned after FOV convolution
  IF ( FOVFLG ) CALL JACFOV
!
  IF ( SFCFLG ) CALL JACSKY
!
! Construct info message detailing no.tangent paths required
  MESSGE = 'I-JACPTH: ' // TRIM ( C11INT(MTAN-NTNJAC) ) // & 
           ' extra tan.paths reqd for Jac.Calcs. New total=' // C11INT(MTAN) 
  CALL WRTLOG ( MESSGE )
!
  MESSGE = 'I-JACPTH: ' // TRIM ( C11INT(NCLC-LCLC) ) // & 
           ' extra Clc.paths reqd for Jac.Calcs. New total=' // C11INT(NCLC)
  CALL WRTLOG ( MESSGE )
!
  MESSGE = 'I-JACPTH: ' // TRIM ( C11INT(NPTH-LPTH) ) // & 
           ' extra C-G.paths reqd for Jac.Calcs. New total=' // C11INT(NPTH)
  CALL WRTLOG ( MESSGE )
!
END SUBROUTINE JACPTH
END MODULE JACPTH_SUB

