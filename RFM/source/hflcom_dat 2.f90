MODULE HFLCOM_DAT
!
! VERSION
!   13JUN17 AD Add PARHFL
!   01MAY17 AD F90 conversion. Checked.
!
! DESCRIPTION
!   HITRAN file data
!   Initially loaded by OPNHIT, updated by various routines.
!
! VARIABLE KINDS
    USE KIND_DAT
!
  IMPLICIT NONE
  SAVE
!
! GLOBAL CONSTANTS
    INTEGER(I4), PARAMETER :: MAXPTR = 56 ! No. fwd pointers  in HIT .bin file
!
! GLOBAL VARIABLES
    LOGICAL     :: USEIDG(MAXPTR)   ! T=need FP for this molecule
    LOGICAL     :: OPNHFL = .FALSE. ! T = file open
    LOGICAL     :: PARHFL = .FALSE. ! T = HITRAN ASCII .par file
    INTEGER(I4) :: IR1HFL           ! First data record of File
    INTEGER(I4) :: IR2HFL           ! Last data record of File
    INTEGER(I4) :: IRCHFL           ! Current record no 
    INTEGER(I4) :: IFPHFL(MAXPTR)   ! Forward pointer for each molecule
    REAL(R8)    :: WNLHFL           ! Lowest WNO in file
    REAL(R8)    :: WNUHFL           ! Highest WNO in file
    REAL(R8)    :: WNOHFL           ! WNO of last line loaded into cyc.buffer
!
END MODULE HFLCOM_DAT
