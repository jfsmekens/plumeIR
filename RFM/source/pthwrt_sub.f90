MODULE PTHWRT_SUB
CONTAINS
SUBROUTINE PTHWRT ( FAIL, ERRMSG )
!
! VERSION
!   05AUG19 AD Change ALTOBS to HGTOBS
!   14DEC17 AD F90 conversion of rfmpth.for. Checked.
!
! DESCRIPTION
!   Write RFM path diagnostics
!   Called once by RFMPTH if PTH flag selected.
!
! VARIABLE KINDS
    USE KIND_DAT
!
! GLOBAL DATA
    USE ATMCOM_DAT ! Atmospheric profile data
    USE FLGCOM_DAT ! Option flags
    USE HDRCOM_DAT ! Output header data
    USE NAMCOM_DAT ! RFM output filenames
    USE OBSCOM_DAT ! Observer location data
    USE PTHCOM_DAT ! Path segment data
    USE TANCOM_DAT ! Tangent path data
    USE PHYADJ_DAT, ONLY: RADCRV ! Local radius of curvature [km]
    USE PHYCON_DAT, ONLY: ATMB, DG2RAD ! [mb]/[atm], [rad]/[deg] conv.factors
    USE RFMLUN_DAT, ONLY: LUNTMP ! LUN for temporarily open files
    USE SFCCOM_DAT, ONLY: RFLSFC ! T=reflective surface, F=no reflection
!
! SUBROUTINES
    USE C9REAL_GEN ! Write real number as C*9 string
    USE IDXPTH_FNC ! Index in PTHCOM of tan/atm/gas/dir
    USE MAKNAM_SUB ! Construct filename for RFM output files
    USE NAMGAS_FNC ! Return molecule name + (iso) associated with GASCOM index
    USE WRTLOG_SUB ! Write text message to log file
!
 IMPLICIT NONE
!
! ARGUMENTS
    LOGICAL,       INTENT(OUT) :: FAIL   ! Set TRUE if a fatal error occurs
    CHARACTER(80), INTENT(OUT) :: ERRMSG ! Error message written if FAIL is TRUE
!
! LOCAL VARIABLES
    INTEGER(I4)       :: IATM   ! Atmospheric layer counter
    INTEGER(I4)       :: IATM1, IATM2 ! Limits for atmospheric level counter
    INTEGER(I4)       :: ICLC   ! Flag for whether LBL calc.performed on path
    INTEGER(I4)       :: IDIR   ! Direction pointer
    INTEGER(I4)       :: IOS    ! Value of IOSTAT saved for error messages.
    INTEGER(I4)       :: IPTH   ! Counter for RFM paths
    INTEGER(I4)       :: ITAN   ! Tangent height counter
    INTEGER(I4)       :: IVMR   ! Absorber counter
    INTEGER(I4)       :: NSEG1, NSEG2 ! No. segments for each tangent path.
    REAL(R4)          :: AMTSUM ! Total absorber mass [kmol/cm^2]
    REAL(R4)          :: HGT    ! Lowest altitude for path segment
    REAL(R4)          :: RAYSUM ! Total pathlength [km]
    CHARACTER(LENNAM) :: FILNAM ! Name of file actually opened (incl. RUNID)
    CHARACTER(80)     :: REC    ! Text record written out
    CHARACTER(7)      :: STASTR = 'UNKNOWN' ! Status for OPEN statements
    CHARACTER(9)      :: TANSTR ! Tangent path info
!
! EXECUTABLE CODE -------------------------------------------------------------
!
  IF ( NEWFLG ) STASTR = 'NEW'
!
  DO ITAN = 1, NTAN
!
! Construct filename and open file
    CALL MAKNAM ( PTHNAM, FILNAM, ITAN=ITAN )
    CALL WRTLOG ( 'I-PTHWRT: Opening output file: ' // FILNAM ) 
    OPEN ( UNIT=LUNTMP, FILE=FILNAM, STATUS=STASTR, ACTION='WRITE', &
             IOSTAT=IOS, ERR=900 )
!
    TANSTR = C9REAL ( TAN(ITAN)%USR ) 
!
! Write File Header 
! Homogenenous path calculation
    IF ( HOMFLG ) THEN 
      REC = '! Path diagnostics calculated for Length =' // TANSTR // &
            ' [km] by RFM v.' // VIDHDR
      WRITE ( LUNTMP, '(A)', IOSTAT=IOS, ERR=900 ) REC
      WRITE ( LUNTMP, '(A)', IOSTAT=IOS, ERR=900 ) TXTHDR
      IATM1 = 1
      IATM2 = 1
      IDIR  = 1
      NSEG1 = 1
      NSEG2 = 0
! Plane parallel atmosphere calculation
    ELSE IF ( ZENFLG .OR. NADFLG ) THEN
      IF ( USRELE ) THEN
        REC = '! Path diagnostics calculated for Ele.Ang=' // TANSTR // &
              ' [dg] by RFM v.' // VIDHDR
      ELSE
        REC = '! Path diagnostics calculated for AirMass=' // TANSTR // &
              '      by RFM v.' // VIDHDR
      END IF
      WRITE ( LUNTMP, '(A)', IOSTAT=IOS, ERR=900 ) REC
      WRITE ( LUNTMP, '(A)', IOSTAT=IOS, ERR=900 ) TXTHDR
      IF ( ZENFLG ) THEN
        IATM1 = IATSFC
        IATM2 = NATM - 1
        IF ( OBSFLG ) IATM1 = MIN ( IATOBS, NATM-1 )
        IDIR = 1
        NSEG1 = IATM2 - IATM1 + 1
        NSEG2 = 0
      ELSE                              ! NADFLG
        IATM1 = NATM - 1
        IATM2 = IATSFC
        IF ( OBSFLG .AND. .NOT. RFLSFC ) IATM1 = MIN ( IATOBS-1, NATM-1 ) 
        IDIR = -1
        NSEG1 = IATM1 - IATM2 + 1
        NSEG2 = 0
      END IF
! Limb-viewing geometry
    ELSE       
      IF ( USRELE ) THEN       ! user-specified elevation angle
        REC = '! Path diagnostics calculated for Ele.Ang=' // TANSTR // &
              ' [dg] by RFM v.' // VIDHDR
      ELSE IF ( USRGEO ) THEN  ! user-specified geometric tangent height
        REC = '! Path diagnostics calculated for Geo.Tan=' // TANSTR // &
              ' [km] by RFM v.' // VIDHDR
      ELSE                     ! user-specified refracted tangent height 
        REC = '! Path diagnostics calculated for Tan Hgt=' // TANSTR // &
              ' [km] by RFM v.' // VIDHDR
      END IF
      WRITE ( LUNTMP, '(A)', IOSTAT=IOS, ERR=900 ) REC
      WRITE ( LUNTMP, '(A)', IOSTAT=IOS, ERR=900 ) TXTHDR
! For limb-viewing, write out extra couple of header records giving additional
! information on viewing/tangent point geometry
      WRITE ( LUNTMP, '(A)', IOSTAT=IOS, ERR=900 ) &
        '!  Rfr.Tan   Geo.Tan   Tan.Zen   Tan.Psi   Rad.Crv   ' // &
        'Obs.Ele   Obs.Alt   Obs.Psi'
      REC = '!'
      WRITE ( REC(2:10), '(F9.3)' ) TAN(ITAN)%HGT
      WRITE ( REC(11:20), '(F10.3)' ) TAN(ITAN)%GEO
      WRITE ( REC(21:30), '(F10.3)' ) ASIN ( TAN(ITAN)%SZN ) / DG2RAD
      IF ( GRAFLG ) WRITE ( REC(31:40), '(F10.3)' ) TAN(ITAN)%PSI
      WRITE ( REC(41:50), '(F10.3)' ) RADCRV
      IF ( OBSFLG ) THEN
        WRITE ( REC(51:60), '(F10.3)' ) TAN(ITAN)%ELE
        WRITE ( REC(61:70), '(F10.3)' ) HGTOBS
        IF ( GRAFLG ) WRITE ( REC(71:80), '(F10.3)' ) PSIOBS
      END IF
      WRITE ( LUNTMP, '(A)' ) REC
      IATM1 = NATM-1
      IATM2 = TAN(ITAN)%IAT
      IDIR = -1
      NSEG1 = IATM1 - IATM2 + 1  ! top of atmosphere to tangent level
      NSEG2 = 0                              ! Bug#80 fix: add this line
      IF ( GRAFLG ) THEN  ! Allow diagnostics for both down and up parts 
        IF ( OBSFLG ) THEN
          IATM1 = MIN ( IATOBS-1, NATM-1 ) 
          IF ( TAN(ITAN)%IAT .EQ. IATOBS ) THEN ! Upview, skip downward loop
            IATM1 = IATOBS
            IATM2 = IATM1 + 1               
          ELSE
            NSEG2 = NSEG1
            NSEG1 = IATM1 - IATM2 + 1  ! Add obs to tan.level
          END IF
        ELSE
          NSEG2 = NSEG1
        END IF
      END IF
    END IF
!
    WRITE ( LUNTMP, * ) NVMR, NSEG1, NSEG2, ' = NGas, NSeg1, NSeg2'
!
! For each absorber....
    DO IVMR = 1, NVMR
      WRITE ( LUNTMP, '(A)', IOSTAT=IOS, ERR=900 ) NAMGAS(IVMR)
      IF ( GRAFLG ) THEN
        WRITE ( LUNTMP, '(A)', IOSTAT=IOS, ERR=900 ) &
          'Lev  Zlow[km]  Psi[dg] Temp[K]  Press[mb]  VMR[ppv]' // &
          '   Amt[kmol/cm2] Len.[km] Clc'
      ELSE
        WRITE ( LUNTMP, '(A)', IOSTAT=IOS, ERR=900 ) &
          'Lev  Zlow[km]  Zen[dg] Temp[K]  Press[mb]  VMR[ppv]' // &
          '   Amt[kmol/cm2] Len.[km] Clc'
      END IF
!
      DO              ! loop over path directions 100 CONTINUE
        RAYSUM = 0.0
        AMTSUM = 0.0
        DO IATM = IATM1, IATM2, IDIR
          IPTH = IDXPTH ( ITAN, IATM, IVMR, IDIR )
!          IF ( PTH(IPTH)%CLC ) THEN   ! Not defined in RFM F90
!            ICLC = 1
!          ELSE
            ICLC = 0
!          END IF
          IF ( IATM .EQ. TAN(ITAN)%IAT ) THEN
            HGT = TAN(ITAN)%HGT
          ELSE
           HGT = HGTATM(IATM)
          END IF
          WRITE ( LUNTMP, '(I3, 3F9.3, 1P3E12.5, 0PF10.3, I3)', &
                  IOSTAT=IOS, ERR=900 )  &
            IATM, HGT, PTH(IPTH)%PSI, PTH(IPTH)%TEM, PTH(IPTH)%PRE*ATMB, &
            PTH(IPTH)%PPA/PTH(IPTH)%PRE, PTH(IPTH)%AMT, PTH(IPTH)%RAY, ICLC
          RAYSUM = RAYSUM + PTH(IPTH)%RAY
          AMTSUM = AMTSUM + PTH(IPTH)%AMT
        END DO
        WRITE ( LUNTMP, '(48X,A,1PE12.5,0PF10.3)', IOSTAT=IOS, ERR=900 ) &
          'Total:', AMTSUM, RAYSUM
        IF ( GRAFLG ) THEN
          IF ( IDIR .EQ. -1 ) THEN ! Repeat from tan.pt to t.o.a.
            IDIR = +1
            IATM1 = TAN(ITAN)%IAT
            IATM2 = NATM - 1       
          ELSE                     ! IDIR=+1, ie tan.pt to t.o.a.
            IDIR = -1              ! Reset downward path for next IVMR
            IATM1 = NATM - 1
            IATM2 = TAN(ITAN)%IAT
            EXIT
          END IF            
        ELSE
          EXIT      ! only one direction unless GRAFLG
        END IF            
      END DO
    END DO
    CLOSE ( LUNTMP, IOSTAT=IOS, ERR=900 )
  END DO
!
900 CONTINUE
  FAIL = IOS .NE. 0
  IF ( FAIL ) WRITE ( ERRMSG, * ) &
    'F-PTHWRT: I/O failure on output file. IOSTAT=', IOS
!
END SUBROUTINE PTHWRT
END MODULE PTHWRT_SUB

