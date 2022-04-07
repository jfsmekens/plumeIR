MODULE PLANCK_FNC
CONTAINS
PURE FUNCTION PLANCK ( TEM, WNOLST )
!
! VERSION
!   01MAY17 AD F90 conversion of F77 subroutine. Checked.
!
! DESCRIPTION
!   Planck Function
!   General purpose module.
!
! VARIABLE KINDS
    USE KIND_DAT 
!
! GLOBAL DATA
    USE PHYCON_DAT, ONLY: C1,C2 ! Radiation constants
!
  IMPLICIT NONE
!
! ARGUMENTS 
    REAL(R4), INTENT(IN) :: TEM       ! Temperature [K]
    REAL(R8), INTENT(IN) :: WNOLST(:) ! List of wavenumbers [cm-1]
!
! FUNCTION TYPE
    REAL(R8) :: PLANCK ( SIZE(WNOLST) ) ! Function array same size as WNOLST
!
! EXECUTABLE CODE -------------------------------------------------------------
!
  IF ( TEM .GT. TINY(1.0) ) THEN
    PLANCK = C1 * WNOLST**3 / ( EXP ( C2 * WNOLST / TEM ) - 1.0D0 ) 
  ELSE
    PLANCK = 0.0D0
  END IF
!
END FUNCTION PLANCK
END MODULE PLANCK_FNC
