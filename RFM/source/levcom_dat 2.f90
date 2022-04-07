MODULE LEVCOM_DAT
!
! VERSION
!   01MAY17 AD F90 original. Checked.
!
! DESCRIPTION
!   Intermediate output levels
!   Loaded by DRVCHK.
!
! VARIABLE KINDS
    USE KIND_DAT
!
  IMPLICIT NONE
  SAVE
!
  TYPE :: LEVTYP
    INTEGER(I4) :: IAT ! Atmospheric profile level
    INTEGER(I4) :: IDR ! Ray direction -1=toa downwards +1=surface/tan pt up
    REAL(R4)    :: HGT ! Altitude [km]
  END TYPE LEVTYP
!
! GLOBAL VARIABLES
    TYPE(LEVTYP), ALLOCATABLE :: LEV(:)
!
    INTEGER(I4)              :: NLEV = 0    ! No. output levels
    INTEGER(I4), ALLOCATABLE :: ITNLEV(:,:) ! [MTAN,NLEV] Indices of lev. rays
!
END MODULE LEVCOM_DAT

