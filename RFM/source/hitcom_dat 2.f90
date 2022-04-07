MODULE HITCOM_DAT
!
! VERSION
!   24FEB17 AD F90 version. Checked.
!
! DESCRIPTION
!   HITRAN line data 
!   Data Type representing structure of HITRAN record.
!
! VARIABLE KINDS
    USE KIND_DAT
!
  IMPLICIT NONE
  SAVE
!
  TYPE :: HITTYP
    INTEGER(I4)  :: IDM ! HITRAN Gas ID
    INTEGER(I4)  :: IDI ! Isotope Number (1=most abundant,2=2nd,3 etc)
    INTEGER(I4)  :: IGS ! Index of molec,iso in GAS
    INTEGER(I4)  :: IUS ! Upper state global quanta index.
    INTEGER(I4)  :: ILS ! Lower state global quanta index
    INTEGER(I4)  :: IUV ! Index of Vib.Temp profile affecting upper level
    INTEGER(I4)  :: ILV ! Index of Vib.Temp profile affecting lower level
    REAL(R4)     :: STR ! Line strength  [cm-1./(kg.moles.cm-2)]@296K
    REAL(R4)     :: ABR ! Air-broad halfwidth  (HWHM) [cm-1/atm] @ 296K
    REAL(R4)     :: SBR ! Self-broad halfwidth (HWHM) [cm-1/atm] @ 296K.
    REAL(R4)     :: ELS ! Lower-state energy [cm-1]
    REAL(R4)     :: ABC ! Coeff.of temp.dependence of air-broadened HW
    REAL(R4)     :: TSP ! Transition shift due to pressure
    REAL(R4)     :: WGT ! Molecular weight [atomic units]
    REAL(R8)     :: WNO ! Cyclic buf of line wnos. [cm-1]
    CHARACTER(9) :: BLQ ! Lower State local quanta
    CHARACTER(9) :: ULQ ! Upper State local quanta
  END TYPE HITTYP
!
! GLOBAL VARIABLES
    TYPE(HITTYP) :: HIT
    TYPE(HITTYP), ALLOCATABLE :: CYC(:)  ! Cyclic buffer
!
    INTEGER(I4) :: IFWDPT ! Forward pointer on data line
    INTEGER(I4) :: NCYC   ! Current size of CYC array
    INTEGER(I4) :: NLIN   ! No.lines currently stored
    INTEGER(I4) :: ICYC1  ! Index for lowest wavenumber line
!
END MODULE HITCOM_DAT
