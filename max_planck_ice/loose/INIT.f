      SUBROUTINE INIT(INTYP, bathy, 
     1     DCVM, WUP, COSGAM, RTC, STC, QTOC,
     2     SNOFLG,
     3     TICE, TICM,
     4     U, V, SURTYP, SURFWIN, 
     5     QS, QT, QH, QSB, QTB, QHB, QDS, QDT, QHSTO,
     6     HS, HT, QV, QRHO, QW, FW, IEN, MLFIX, 
     7     H, A, HSN)
C=======================================================================
      IMPLICIT none
      INCLUDE "icegrid.inc"
C     Include physical constants, rheology parameters, and mixed layer
C       parameters
      INCLUDE "physical.inc"
      INCLUDE "rheology.inc"
      INCLUDE "oml.inc"
C=======================================================================
      COMMON/CORR/FM(0:L,0:M), F(L,M), COSPHI(0:L,0:M), SINPHI(0:L,0:M)
      REAL FM, F, COSPHI, SINPHI

      COMMON/IPARM/H0,HNU,HNU2,ARMIN,ARMAX,HMIN
      REAL H0, HNU, HNU2, ARMIN, ARMAX, HMIN

      COMMON/DRV/DXSQ,DYSQ,SX2,SY2,SXY
      REAL DXSQ, DYSQ, SX2, SY2, SXY

      COMMON/STP/T,NTMES,NRST,NRREC,NPLT,NSTAT,IIC,NFLD,NSTA
      REAL T
      INTEGER NTMES, NRST, NRREC, NPLT, NSTAT, IIC, NFLD, NSTA

      COMMON/COORD/PM(0:L,0:M), PN(0:L,0:M), DNDX(L,M), DMDY(L,M)
      REAL PM, PN, DNDX, DMDY

      REAL U(L, M, 3), V(L, M, 3)
      REAL H(0:L,0:M,2), A(0:L,0:M,2), HSN(0:L,0:M,2)

      COMMON/FRWND/CDWIN,SINWIN,COSWIN,UWIN(L,M), VWIN(L,M)
      REAL CDWIN, SINWIN, COSWIN, UWIN, VWIN

      COMMON/FRWAT/SINWAT,COSWAT,UWAT(L,M), VWAT(L,M)
      REAL SINWAT, COSWAT, UWAT, VWAT

      REAL TICE(0:L, 0:M)
      REAL TICM(0:L, 0:M, NLEVEL)
C     Commons viscp and relaxp moved to rheology.inc
      COMMON/MASK/VM(L,M), HM(0:L,0:M), OM(0:L,0:M), FLM(0:L,0:M,2)
      REAL VM, HM, OM, FLM

      REAL QS(0:L,0:M), QT(0:L,0:M), QH(0:L,0:M)
      REAL QSB(0:L,0:M), QTB(0:L,0:M), QHB(0:L,0:M)
      REAL QDS(0:L,0:M), QDT(0:L,0:M), QHSTO(0:L,0:M)
      REAL HS(0:L,0:M), HT(0:L,0:M), QV(0:L,0:M)
      REAL QRHO(0:L,0:M), QW(0:L,0:M), FW(0:L,0:M)
      INTEGER IEN(0:L,0:M), MLFIX

      REAL PI, RAD
      REAL SURTYP, SURFWIN
      COMMON/TAU/CD(0:L,0:M), SINBET(0:L,0:M), COSBET(0:L,0:M),
     1 BETA(0:L,0:M), TAUX(L,M), TAUY(L,M)
      REAL CD, SINBET, COSBET, BETA, TAUX, TAUY

      REAL DCVM, WUP, COSGAM, RTC, STC, QTOC
      REAL SNOFLG
C     Make a bathymetric array, pass as argument
      REAL bathy(0:L, 0:M)
      INTEGER INTYP
C=======================================================================
      REAL TI0, QH0, QS0, QT0, QHB0, QDT0, QDS0
      REAL THWIN, THWAT
      REAL tempor(0:L, 0:M)
      CHARACTER*120 runparms
      REAL PHI, XL, YL, FLAGM, QHSMAX
      INTEGER I, J, K
C-----------------------------------------------------------------------
C  SPECIFY ZONAL AND ANNUAL MEAN OF CLOUDINESS (ACC.TO V.LOON (1972))
C  Replaced with calls to read in from MRF output.  BG 11/92
C-----------------------------------------------------------------------
C     Open input run files
      OPEN (10, FILE='MASK', FORM='FORMATTED', STATUS='OLD')
      OPEN (12, FILE='tsdeep', FORM='UNFORMATTED', STATUS='OLD')
      OPEN (13, FILE='tsshal', FORM='UNFORMATTED', STATUS='OLD')
      READ (*,9009) runparms
      OPEN (21, FILE=runparms, FORM='FORMATTED', STATUS='OLD')
      OPEN (27, FILE='bathy', FORM='UNFORMATTED', STATUS='OLD')
 9009 FORMAT (A120)
  
C     Open output run files
      OPEN (14, FILE='RESTARTn', FORM='UNFORMATTED', STATUS='UNKNOWN')
      OPEN (15, FILE='thick', FORM='UNFORMATTED', STATUS='UNKNOWN')
      OPEN (16, FILE='FORT.16', FORM='FORMATTED', STATUS='UNKNOWN')
      OPEN (17, FILE='conc', FORM='UNFORMATTED', STATUS='UNKNOWN')
      OPEN (18, FILE='FORT.18', FORM='FORMATTED', STATUS='UNKNOWN')
      OPEN (19, FILE='vels', FORM='UNFORMATTED', STATUS='UNKNOWN')
      OPEN (22, FILE='tml', FORM='UNFORMATTED', STATUS='UNKNOWN')
      OPEN (23, FILE='sml', FORM='UNFORMATTED', STATUS='UNKNOWN')
      OPEN (24, FILE='hml', FORM='UNFORMATTED', STATUS='UNKNOWN')
      OPEN (25, FILE='atm.flux', FORM='UNFORMATTED', STATUS='UNKNOWN')
      OPEN (26, FILE='oce.flux', FORM='UNFORMATTED', STATUS='UNKNOWN')
C-----------------------------------------------------------------------
C  SET RUN LENGTH and OUTPUT parameters
C-----------------------------------------------------------------------
      READ (21, 9001) INTYP
      IF (INTYP .EQ. 1) THEN
        OPEN (20, FILE='metout', FORM='UNFORMATTED', STATUS='NEW')
       ELSE
        OPEN (20, FILE='metout', FORM='UNFORMATTED', STATUS='OLD')
      ENDIF
      READ (21, 9001) NTMES
      READ (21, 9001) NRST
      READ (21, 9001) NSTAT
      READ (21, 9001) NSTA
      READ (21, 9001) NPLT
      READ (21, 9001) NFLD
      READ (21, 9001) NRREC

C     Now try to open restart file.  If there isn't one, the reset NRREC
C       so as to do a cold start
      OPEN ( 9, FILE='RESTARTo', FORM='UNFORMATTED', STATUS='OLD', 
     1                  ERR=9999)
      GO TO 9998
 9999 CONTINUE
      NRREC = 0
 9998 CONTINUE
C-----------------------------------------------------------------------
C  DECIDE ABOUT INCLUSION OF SNOW
C-----------------------------------------------------------------------
C**REMARK: -SNOFLG=0.: NO SNOW
C          -SNOFLG=1.: INCLUDE SNOW LAYER
      READ (21, 9002) SNOFLG
C-----------------------------------------------------------------------
C  DECIDE ABOUT INCLUDING THE VARIABLE OML MODEL
C-----------------------------------------------------------------------
C**REMARK: -MLFIX=1 : FIXED MIXED LAYER
C          -MLFIX=0 : PROGNOSTIC MIXED LAYER
      READ (21, 9001) MLFIX
C-----------------------------------------------------------------------
C  DECIDE WHICH AIR-ICE INTERACTION TO USE
C-----------------------------------------------------------------------
C          -SURTYP = 0. : BULK AERODYNAMICS
C          -SURTYP = 1. : ASL MODEL (LOUIS, 1979)
C          -SURTYP = 2. : ABL MODEL (KOCH, 1988)
      READ (21, 9002) SURTYP
C-----------------------------------------------------------------------
C  USE OF SURFACE OR UPPER LAYER WINDS
C-----------------------------------------------------------------------
C**REMARK: -SURFWIN=1.: SURFACE WINDS (FIXED WIND TURNING)
C          -SURFWIN=0.: UPPER LAYER WINDS
      READ (21, 9002) SURFWIN
C-----------------------------------------------------------------------
C  SET PARAMETERS FOR SPHERICAL COORDINATES AND CORIOLIS FORCE
C-----------------------------------------------------------------------
      PI=4.*ATAN(1.)
      RAD=PI/180.
C  SET CORIOLIS PARAMETER:
      IF (PTYPE .EQ. 1 .OR. PTYPE .EQ. 2) THEN
        DO 1 J=0,M
          DO 2 I = 0, L
            COSPHI(I, J)=COS((LATMIN+DLAT*J)*RAD)
            SINPHI(I, J)=SIN((LATMIN+DLAT*J)*RAD)
            FM(I, J)=F0*SINPHI(I, J)
    2     CONTINUE
    1   CONTINUE
        DO 3 J=1,M
          DO 4 I = 1, L
            F(I,J)=F0*SIN((LATMIN-DLAT/2.+DLAT*J)*RAD)
    4     CONTINUE
    3   CONTINUE
       ELSE IF (PTYPE .EQ. 3) THEN
C       Polar stereographic grid
        DO 5 J = 0, M
          DO 6 I = 0, L
            PHI = SQRT( (DX*(I-POLEI))**2 + (DY*(J-POLEJ))**2 )
     1        / DXDEG - 90.
            PHI = SIGN(PHI,LATMIN) 
            COSPHI(I,J) = COS(RAD*PHI)
            SINPHI(I,J) = SIN(RAD*PHI)
            FM(I,J)     = F0*SINPHI(I,J)
    6     CONTINUE
    5   CONTINUE
        DO 7 J = 1, M
          DO 8 I = 1, L
            PHI = SQRT( (DX*(I-0.5-POLEI))**2 + (DY*(J-0.5-POLEJ))**2 )
     1        / DXDEG - 90.
            PHI = SIGN(PHI,LATMIN) 
            F(I,J)=F0*SIN(PHI*RAD)
    8     CONTINUE
    7   CONTINUE

        ELSE
         STOP 'Grid type out of range'
       ENDIF       
C-----------------------------------------------------------------------
C  DETERMINE GRID SIZE AND TIME STEP
C-----------------------------------------------------------------------
C**REMARK: -DX=5.5560E+05 FOR SPHERICAL COORDINATES
C          -DX=2.3481E+05 FOR CARTESIAN COORDINATES
      XL=DX*FLOAT(LM)
      DXSQ=DX*DX
      YL=DY*FLOAT(MM)
      DYSQ=DY*DY
      SX2=0.5/DXSQ
      SY2=0.5/DYSQ
      SXY=0.25/(DX*DY)
      T=0.0
C-----------------------------------------------------------------------
C  SET COORDINATE MAPPING FACTORS
C-----------------------------------------------------------------------
C**REMARK: -CARTESIAN COORDINATES: PM=1 AND DX=DX AT MEAN LATITUDE
C          -SPHERICAL COORDINATES: PM=1./COSPHI(I,J) AND DX=DX AT LAT=0
C          -Polar Stereographic:   PM=2./(1+ABS(SINPHI)) AND DX=DX AT LAT=90
      IF (PTYPE .EQ. 1) THEN
C       Cartesian Coordinates
        DO 209 J = 0, M
          DO 210 I = 0, L
            PM(I,J) = 1.
            PN(I,J) = 1.
  210     CONTINUE
  209   CONTINUE
       ELSE IF (PTYPE .EQ. 2) THEN
C        Spherical coordinates
         DO 211 J = 0, M
           DO 212 I = 0, L
             PM(I,J) = 1./COSPHI(I,J)
             PN(I,J) = 1.
  212      CONTINUE
  211    CONTINUE
       ELSE IF (PTYPE .EQ. 3) THEN
C        Polar Stereographic
         DO 213 J = 0, M
           DO 214 I = 0, L
             PM(I,J)=2./(1.+ABS(SINPHI(I,J)))  !Polar Stereographic
             PN(I,J)=2./(1.+ABS(SINPHI(I,J)))  !Polar Stereographic
  214      CONTINUE
  213   CONTINUE
       ELSE 
          STOP 'Grid type out of range'
      ENDIF

C     The following terms are the x and y components of the centripetal
C       accelerations.  They are being ignored for now.  They derive
C       from the difference between u.grad(u) and grad(u^2/2)+curl(u)
C       cross u when non-cartesian coordinates are used.  RELCON
C       is the only routine which uses these.
      DO 216 J=1,M
      DO 216 I=1,L
       DNDX(I,J)=0.
       DMDY(I,J)=0.
  216 CONTINUE
C-----------------------------------------------------------------------
C  SET UP ICE AND SNOW PARAMETERS
C-----------------------------------------------------------------------
      READ (21, 9002) H0
      READ (21, 9002) ARMIN
      READ (21, 9002) ARMAX
      READ (21, 9002) HMIN
      READ (21, 9002) HNU
      HNU = HNU * DX
      HNU2=DX**2*HNU
C-----------------------------------------------------------------------
C  SET UP SURFACE HEAT AND MOMENTUM TRANSFER PARAMETERS
C-----------------------------------------------------------------------
C**FOR SURFACE WINDS TURNING ANGLE EQUAL TO ZERO:
      READ (21, 9002) CDWIN
      READ (21, 9002) THWIN
      SINWIN = SIN(THWIN*PI/180.)
      COSWIN = COS(THWIN*PI/180.)
      READ (21, 9002) THWAT
      SINWAT = SIN(THWAT*PI/180.)
      COSWAT = COS(THWAT*PI/180.)
C-----------------------------------------------------------------------
C  SET OML PARAMETERS
C-----------------------------------------------------------------------
      FLAGM=FLOAT(MLFIX)
      READ (21, 9002) QHSMAX
      DCVM=EXP(-QHSMAX/QHS)
      READ (21, 9002) WUP
      IF (MLFIX.EQ.1) WUP=0.0
      READ (21, 9002) COSGAM
      READ (21, 9002) RTC
      RTC = RTC * 8.64E4/DT
      READ (21, 9002) STC
      STC = STC * 8.64E4/DT
      READ (21, 9002) QTOC
      READ (27) bathy
C-----------------------------------------------------------------------
C  SET UP PARAMETERS FOR THE RELAX SUBROUTINE, moved to physical.inc
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C  INITIALIZE HORIZONTAL BOUNDARY CONDITIONS
C-----------------------------------------------------------------------
      CALL BCSINIT(VM, HM, OM, FLM)
C-----------------------------------------------------------------------
C  Rationalize the boundary condition file versus the bathymetry file.
C  If either says there is water present, put water at the point.
C  Work over 1-(N-1).  Use bathy, HM, OM (HM and OM are identical).
C  Bob Grumbine 10 October 1995.
      DO 200 J = 1, M-1
        DO 101 I = 1, L-1
          IF (OM(i,j) .EQ. 1. .AND. bathy(i,j) .LE. 5.0) THEN
            PRINT *,'Mask is water, bathy is land ',i,j
            bathy(i,j) = 1.0
            OM(i,j) = 0.0
            HM(i,j) = 0.0 
          ENDIF
          IF (OM(i,j) .EQ. 0. .AND. bathy(i,j) .GT. 5.0) THEN
              PRINT *,'Mask is land, bathy is water ',i,j,bathy(i,j)
              OM(i,j) = 1.
              HM(i,j) = 1.
            bathy(i,j) = 1.0 
          ENDIF
  101   CONTINUE
  200 CONTINUE
C-----------------------------------------------------------------------
C  INITIALIZE THE SI, OML AND ABL VARIABLES (OPT. FROM RESTART TAPE)
C-----------------------------------------------------------------------
      IF (NRREC .NE. 0) THEN
       DO 420 K=1,NRREC
         READ (9) tempor
         DO 9800 j = 0, M
           DO 9801 i = 0, L
             H(i,j, 1) = tempor(i,j)
             H(i,j, 2) = tempor(i,j)
 9801      CONTINUE
 9800    CONTINUE
         
         READ (9) tempor
         DO 9802 j = 0, M
           DO 9803 i = 0, L
             A(i,j, 1) = tempor(i,j)
             A(i,j, 2) = tempor(i,j)
 9803      CONTINUE
 9802    CONTINUE
        
         READ (9) tempor
         DO 9804 j = 0, M
           DO 9805 i = 0, L
             HSN(i,j, 1) = tempor(i,j)
             HSN(i,j, 2) = tempor(i,j)
             IF (tempor(i,j) .LT. 0) THEN
               STOP 'Found a negative snow thickness on read in'
             ENDIF
 9805      CONTINUE
 9804    CONTINUE

         READ (9) TICE
         READ (9) QT
         READ (9) QS
         READ (9) QH
         READ (9) QTB
         READ (9) QSB
         READ (9) QHB
         READ (9) QDT
         READ (9) QDS
         READ (9) TICM
         READ (9) U
         READ (9) V
 420   CONTINUE
      ELSE
        READ (21, 9002) TI0
        READ (21, 9002) QH0
        READ (21, 9002) QS0
        READ (21, 9002) QT0
        READ (21, 9002) QHB0
        READ (21, 9002) QDT0
        READ (21, 9002) QDS0
        DO 280 J=1,M
        DO 280 I=1,L
         U(I,J,1)=0.
         V(I,J,1)=0.
  280 CONTINUE
        DO 281 J=0,M
        DO 281 I=0,L
         H(I,J,1)= 0.
         A(I,J,1)= 0.
         HSN(I,J,1)=0.
         TICE(I,J)= TI0
         QH(I,J)= QH0
         QT(I,J)= QT0
         QS(I,J)= QS0
         QHB(I,J)= bathy(i,j)
         QDT(I,J)= MIN(QDT0, bathy(i,j)) !Note that QD are thermocline thick-
         QDS(I,J)= MIN(QDS0, bathy(i,j)) ! nesses.  Must be less than bathy.
         IF (I*J .NE. 0) TAUX(I,J)=0.
         IF (I*J .NE. 0) TAUY(I,J)=0.
  281   CONTINUE
        READ (12) QTB
        READ (12) QSB
        READ (13) QT
        READ (13) QS
      
        IF (PTYPE .EQ. 1 .OR. PTYPE .EQ. 2) THEN
          DO 283 J=0, M
          DO 283 I=0,L
            H(I,J,1) = OM(I,J)*(1. - (I/FLOAT(L/2)-1.)**2)*
     1                         (1. - (J/FLOAT(M/2)-1.)**2)*0.0
            A(I,J,1) = OM(I,J)*(1. - (I/FLOAT(L/2)-1.)**2)*
     1                         (1. - (J/FLOAT(M/2)-1.)**2)*0.0
            HSN(I,J,1) = OM(I,J)*(1. - (I/FLOAT(L/2)-1.)**2)*
     1                         (1. - (J/FLOAT(M/2)-1.)**2)*0.0
  283     CONTINUE
         ELSE
          DO 285 J = 0, M
          DO 285 I = 0, L
            H(I,J,1) = OM(I,J)*(1. - (I/FLOAT(L/2)-1.)**2)*
     1                         (1. - (J/FLOAT(M/2)-1.)**2)*0.0
            A(I,J,1) = OM(I,J)*(1. - (I/FLOAT(L/2)-1.)**2)*
     1                         (1. - (J/FLOAT(M/2)-1.)**2)*0.0
            HSN(I,J,1) = OM(I,J)*(1. - (I/FLOAT(L/2)-1.)**2)*
     1                         (1. - (J/FLOAT(M/2)-1.)**2)*0.0
  285     CONTINUE
        ENDIF

        DO 282 K=1,NLEVEL
        DO 282 J=0,M
        DO 282 I=0,L
         TICM(I,J,K)=TICE(I,J)
  282   CONTINUE
      ENDIF
C-----------------------------------------------------------------------
C  READ IN FIELDS THAT ARE CONSTANT IN TIME
C-----------------------------------------------------------------------
        DO 1000 J = 1, M-2
          DO 1100 I = 2, LM
            UWAT(I,J) = 0.0
            VWAT(I,J) = 0.0
 1100     CONTINUE
 1000   CONTINUE

      DO 60 J=1,M
       UWAT(1,J)=UWAT(LM,J)
       VWAT(1,J)=VWAT(LM,J)
       UWAT(L,J)=UWAT(2,J)
       VWAT(L,J)=VWAT(2,J)
   60 CONTINUE
      DO 61 I=1,L
       UWAT(I,M)=UWAT(I,MM)
       VWAT(I,M)=VWAT(I,MM)
   61 CONTINUE
C-----------------------------------------------------------------------
C  DERIVE INITIAL OML VARIABLES
C-----------------------------------------------------------------------
      DO 80 J=0,M
      DO 80 I=0,L
       QDS(I,J)=QDS(I,J)*(1.-FLAGM)
       QDT(I,J)=QDT(I,J)*(1.-FLAGM)
C      Degree content of water column
       HT(I,J)=(QT(I,J)-QTB(I,J))*(QH(I,J)+QDT(I,J))+QTB(I,J)*QHB(I,J)
C      Salt content of water column
       HS(I,J)=(QS(I,J)-QSB(I,J))*(QH(I,J)+QDS(I,J))+QSB(I,J)*QHB(I,J)
   80 CONTINUE
C-----------------------------------------------------------------------
C  WRITE CURRENT CONTROL PARAMETERS
C-----------------------------------------------------------------------
      WRITE(16,403) L, M, MLFIX, SURTYP, SURTYP, SURFWIN, DX, DY, 
     1  DT, H0, PSTAR, WT,
     3  ZOI, SINWIN, COSWIN, QTOC, CDWIN, CDWAT

      RETURN


  100 WRITE(*,400)
  403 FORMAT (
     1 ' L      = ',I12,    ' M      = ',I12,    ' MLFIX  = ',I12,/,
     2 ' SURTYP = ',1PE12.3,' SURTYP = ',1PE12.3,' SURFWIN= ',1PE12.3,/,
     3 ' DX     = ',1PE12.3,' DY     = ',1PE12.3,' DT     = ',1PE12.3,/,
     4 ' H0     = ',1PE12.3,' PSTAR  = ',1PE12.3,' WT     = ',1PE12.3,/,
     5 ' ZOI    = ',1PE12.3,' SINWIN = ',1PE12.3,' COSWIN = ',1PE12.3,/,
     6 ' QTOC   = ',1PE12.3,' CDWIN  = ',1PE12.3,' CDWAT  = ',1PE12.3)
  802 FORMAT (18F6.2)
  901 FORMAT (5E13.4)
  400 FORMAT ('1  READ ERROR IN INIT ')
 9001 FORMAT (I4)
 9002 FORMAT (E13.6)

      STOP
      END
