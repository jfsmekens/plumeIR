MODULE PTBCON_DAT
!
! VERSION
!   26DEC18 AD F90 conversion
!
! DESCRIPTION
!   Jacobian perturbation sizes
!
! VARIABLE KINDS
    USE KIND_DAT
!
  IMPLICIT NONE
!
! GLOBAL CONSTANTS
    REAL(R4), PARAMETER :: PTBEXT = 1.0E-4 ! Extinction ptb [/km]
    REAL(R4), PARAMETER :: PTBLOS = 0.001  ! LOS perturbation [km]
!    REAL(R4), PARAMETER :: PTBNTE = 1.0    ! Vib Temp ptb [K]
!    REAL(R4), PARAMETER :: PTBOFF = 1.0    ! Radiometric offset ptb [r.u.]
!    REAL(R4), PARAMETER :: PTBPOI = 1.0    ! LOS Pointing Jacobian units [km]
    REAL(R4), PARAMETER :: PTBPRE = 0.01   ! Fraction for pressure perturbation
    REAL(R4), PARAMETER :: PTBSFE = 0.01   ! Amount [0-1] for sfc emissivity ptb
!    REAL(R4), PARAMETER :: PTBSFP = 0.01   ! Fraction for sfc pressure ptb
    REAL(R4), PARAMETER :: PTBSFT = 1.0    ! Surface Temp ptb [K]
    REAL(R4), PARAMETER :: PTBSRC = 0.01   ! Fraction for Source Fn perturbation
    REAL(R4), PARAMETER :: PTBTEM = 1.0    ! Temperature ptb [K]
    REAL(R4), PARAMETER :: PTBVMR = 0.01   ! Fraction for VMR perturbation
!
END MODULE PTBCON_DAT
