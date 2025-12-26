         PROGRAM MAKBND
C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C MAIN PROGRAM: MKBND126     CREATE values along boundaries of ngm/eta
C   PRGMMR: ROGERS           ORG: NP22        DATE: 1998-10-19
C
C ABSTRACT: READS IN spectral COEFFICIENT FORM (KMAX LEVELS OF
C   T126 COEFFICIENTS) AND CONVERTS IT TO A IMAX X JMAX "basis" grid
C   for interpolation to the boundary points of the NGM or ETA models
C
C PROGRAM HISTORY LOG:
C   90-05-21  DIMEGO/PAN         CRAY CODE WITH A NEW
C                                SUBROUTINE INTERFACE FOR TRANSFORMS
C   99-02-05  ROGERS             GENERALIZED VERSION WHICH WILL WORK
C                                FOR ANY GLOBAL MODEL RESOLUTION
C                                ADDED PTETABC CODE (INTERPOLATES ETA
C                                BOUNDARY POINTS FROM SIGMA TO ETA)
C                                AS A SUBROUTINE
C USAGE:
C   INPUT FILES:
C     UNIT11    - sigma COEFFICIENTS FROM THE GDAS or aviation
C               - ON KMAX SIGMA LEVELS
C
C   OUTPUT FILES:
C     UNIT6    - DIAGNOSTICS AND PRINT OUTPUT
C     UNIT51   - output FILE OF lateral boundary values
C
C   SUBPROGRAMS CALLED:
C     UNIQUE:    - COF2GRD  COF2xx   DZTOUV   PPZT     PPUV
C                  CMPIND   CMPWND   TRANSI   TRANVI
C                  PLN2H    SUMSHS   SUMVHS   FFTidim
C
C     LIBRARY:
C       SPLIB    - SPTRAN SPTRANV
C
C   EXIT STATES:
C     COND =   0 - SUCCESSFUL RUN
C
C   REMARKS: SEE COMMENT CARDS FOLLOWING DOCBLOCK
C
C ATTRIBUTES:
C   LANGUAGE: STANDARD FORTRAN
C   MACHINE:
C
C$$$
CC
C      VARIABLES IN NAMELIST RGRID READ INTO MAIN PROGRAM
CC
C   POLA    - TYPE OF baSIS GRID  (DEFAULT=FALSE)
C   NORTH   - HEMISPHERE SWITCH FOR baSIS GRID  (DEFAULT=TRUE)
C   ALONVT  - REFERENCE LONGITUDE (+W)      FOR POLA = TRUE
C             STARTING LATITUDE   (+N)      FOR POLA = FALSE
C   POLEI   - I INDEX OF POLE               FOR POLA = TRUE 
C             INCREMENT FOR LATITUDE        FOR POLA = FALSE
C   POLEJ   - J INDEX OF POLE               FOR POLA = TRUE
C             STARTING LONGITUDE  (+W)      FOR POLA = FALSE GRNWCH=360
C   XMESHL  - MESH LENGTH (KM) AT 60N       FOR POLA = TRUE
C             INCREMENT FOR LONGITUDE       FOR POLA = FALSE
C   SI2     - LMAXP1 SIGMA INTERFACE DEFINITIONS
CC
C
C   ASSUME MAXIMUM # GLOBAL LEVELS = 50
C
      INCLUDE "parmlbc"
C-----------------------------------------------------------------------
                             D I M E N S I O N
     & HPTLAT(KB), HPTLON(KB), UPTLAT(KB), UPTLON(KB)
     &,VPTLAT(KB), VPTLON(KB)
C
                             D I M E N S I O N
     & UATHPTS(KB,KMAX), VATHPTS(KB,KMAX),
     & TATHPTS(KB,KMAX), QATHPTS(KB,KMAX),
     &     PSATHPTS(KB), ZSATHPTS(KB),
     & UATUPTS(KB,KMAX), VATUPTS(KB,KMAX), TATUPTS(KB,KMAX),
     &     PSATUPTS(KB), ZSATUPTS(KB),
     & UATVPTS(KB,KMAX), VATVPTS(KB,KMAX), TATVPTS(KB,KMAX),
     &     PSATVPTS(KB), ZSATVPTS(KB)
      DIMENSION ALL(245)
C
      DIMENSION XX(IMAX,JMAX,KMAX,4)
      DIMENSION TEMP(IMAX,JMAX,KMAX),QTEMP(IMAX,JMAX,KMAX)
      DIMENSION SI(KMAX+1),SL(KMAX),IDATE(4)
      DIMENSION PS(IMAX,JMAX,3)
C
      PARAMETER(IDIM=IMAX,JDIM=JMAX,
     1          NVAR=4,NPOINT=3)
C
      DIMENSION TMN0(KMAX), AREA(KMAX)
C
      COMMON /SIGLTB/ B(LBDIM,KMAX,NVAR,NPOINT),GLON(LBDIM,NPOINT)
     1 ,SLVL(KMAX+1),SLYR(KMAX),PSFC(LBDIM,NPOINT),ZSFC(LBDIM,NPOINT)
     2 ,DTMN(KMAX)
C
      EQUIVALENCE (SI(1),SLVL(1))
      EQUIVALENCE (SL(1),SLYR(1))
      EQUIVALENCE (GLON(1,1),UPTLON(1))
      EQUIVALENCE (GLON(1,2),VPTLON(1))
      EQUIVALENCE (GLON(1,3),HPTLON(1))
      EQUIVALENCE (PSFC(1,1),PSATUPTS(1))
      EQUIVALENCE (PSFC(1,2),PSATVPTS(1))
      EQUIVALENCE (PSFC(1,3),PSATHPTS(1))
      EQUIVALENCE (ZSFC(1,1),ZSATUPTS(1))
      EQUIVALENCE (ZSFC(1,2),ZSATVPTS(1))
      EQUIVALENCE (ZSFC(1,3),ZSATHPTS(1))
      EQUIVALENCE (B(1,1,1,1),UATUPTS(1,1))
      EQUIVALENCE (B(1,1,2,1),VATUPTS(1,1))
      EQUIVALENCE (B(1,1,3,1),TATUPTS(1,1))
      EQUIVALENCE (B(1,1,1,2),UATVPTS(1,1))
      EQUIVALENCE (B(1,1,2,2),VATVPTS(1,1))
      EQUIVALENCE (B(1,1,3,2),TATVPTS(1,1))
      EQUIVALENCE (B(1,1,1,3),UATHPTS(1,1))
      EQUIVALENCE (B(1,1,2,3),VATHPTS(1,1))
      EQUIVALENCE (B(1,1,3,3),TATHPTS(1,1))
      EQUIVALENCE (B(1,1,4,3),QATHPTS(1,1))
C
      LOGICAL POLA,NORTH
      COMMON /GRID/ POLA,NORTH,ALONVT,POLEI,POLEJ,XMESHL
      NAMELIST/RGRID/POLA,NORTH,ALONVT,POLEI,POLEJ,XMESHL
      EQUIVALENCE (YLATS,ALONVT) , (DLAT,POLEI)
      EQUIVALENCE (XLONW,POLEJ) , (DLON,XMESHL)
C
      NAMELIST/PRMFLD/NTIMES
C
      DIMENSION CON(24),XLIM(4),YLIM(4)
      CHARACTER*4 YTITLE(20) , BLANK
      LOGICAL IACROS,XREV,YREV,TICK,ALLCON
      CHARACTER*1 XTITLE(30) , BLNK
      DATA BLANK/'    '/ , BLNK/' '/
      COMMON /BLK3/ MI,LJ,LID,IACROS,XREV,YREV,TICK,ALLCON,XLIM,YLIM,
     + XRG,YRG,IXD,IYD,NPRL,NPRC,NLV,CON,XTITLE,YTITLE
C
      COMMON /GRIDS/ ALAT( IMAX , JMAX ),ALON( IMAX , JMAX )
C
      CALL W3TAGB('MKBND   ',1998,0292,0082,'NP22   ')
      print*,'start of mkbnd'
C
      WRITE(6,1)
    1 FORMAT('1  WELCOME TO THE GENERAL BOUNDARY VALUE PROCESSOR',/,
     + ' WITH NEW TRANSFORMs -ETA/NGM- CRAY VERSION December 6,1990',//)
      MI = IMAX
      LID = MI
      LJ = JMAX
      DO I=1,30
        XTITLE(I) = BLNK
      ENDDO
      DO I=1,20
        YTITLE(I) = BLANK
      ENDDO
C
C  YC=NPRC/IMAX and XC=NPRL/JMAX
C   THESE ARE FOR half grid NPRC=68 and NPRL=41 small screen
C
      YC = (68.) / FLOAT(IMAX)
      XL = (41.) / FLOAT(JMAX)
      CON(1) = 1.0
      CON(2) = 1.0
      ICT = 3
      DEGRAD = 3.14159265 / 180.
C
C  SET PRINT FOR 12 VALUES PER LINE
C
      ISIX =  IMAX / 12 + 1
C     NAMELIST/RGRID/POLA,NORTH,ALONVT,POLEI,POLEJ,XMESHL
c     LOGICAL POLA,NORTH
      POLA = .FALSE.
      NORTH = .TRUE.
      ALONVT = 0.5
      POLEI  = 1.0
      POLEJ  = 360.
      XMESHL = 1.0
      READ(5,RGRID)
      WRITE(6,4)ALONVT,POLEI,POLEJ,XMESHL
    4 FORMAT('0  &RGRID  LIMITS ',4G12.5)
      NTIMES = 1
      READ(5,PRMFLD)
      WRITE(6,PRMFLD)
      NUNIT = 51
C
      IF( KBETA.NE.KB )
     &  CALL ngmPTS(HPTLAT,HPTLON,NHPT,
     &           UPTLAT,UPTLON,NUPT,
     &           VPTLAT,VPTLON,NVPT,
     &           NTIMES,NUNIT,DLAM0)
C
      IF( KBETA.EQ.KB )
     &  CALL etaPTS(HPTLAT,HPTLON,NHPT,
     &           UPTLAT,UPTLON,NUPT,
     &           VPTLAT,VPTLON,NVPT)
C
      DO 9800 NT = 1,NTIMES
        IFCTHR = (NT-1) * 6
        ITIME  = (IFCTHR-12) * 3600
C
C      READ and process the SIGGES COEFFICIENTS
C      FIRST READ THE SECOND RECORD TO GET GLOBAL MODEL
C      SPECS WHICH ARE SENT TO THE COF2GRD ROUTINE
C
      LUN1= 11 + (NT -1)
      READ(LUN1)  
      READ(LUN1)HOUR,IDATE,SI,SL
C
      JCAP = 62
      JCAP1 = JCAP + 1
      JCAP2 = JCAP + 2
      NC = JCAP1 * JCAP2
c     KMAX = ALL(203)
      KMAXP1 = KMAX + 1
      KDMAX = KMAX - 4
      MAXK   =  KMAX
      MAXKP1 = MAXK + 1
      MAXKM1 = MAXK - 1
      MAXKM2 = MAXK - 2
      MAXKM3 = MAXK - 3
c     DO K = 1, KMAXP1
c       SI(K) = ALL(K)
c     ENDDO
c     DO K = 1, KMAX
c       SL(K) = ALL(K+KMAXP1)
c     ENDDO
c     IF(ALL(204).EQ.1.) THEN
      JROMB = 0
c     ELSE
c       JROMB = 1
c     ENDIF
      WRITE(6,100) LUN1,IDATE,HOUR
  100 FORMAT(' DATE OF COEFFICIENT FILE ',I4,4I5,F10.3)
      WRITE(6,110) SI
      WRITE(6,110) SL
  110 FORMAT(10G12.5)
      WRITE(6,*)JCAP,NC,KMAX,JROMB 
C
      MWAVE = JCAP
      KDIMQ = KDMAX
      KDIM = KMAX
      KDIMP = KMAXP1
C
c     CALL COF2GRD(LUN1,NC,JROMB,JCAP,XX,PS)
      CALL COF2GRD(LUN1,NC,JROMB,JCAP,XX,PS)
      print *,'ok after cof2grd'
C
C
c     CALL EZSKCH (PS(1,1,1), ICT, XL, YC)                   
c     CALL EZSKCH (PS(1,1,2), ICT, XL, YC)  
c     CALL EZSKCH (XX(1,1,1,1), ICT, XL, YC)                           
c     CALL EZSKCH (XX(1,1,2,1), ICT, XL, YC)                           
c     CALL EZSKCH (XX(1,1,1,2), ICT, XL, YC)                         
c     CALL EZSKCH (XX(1,1,2,2), ICT, XL, YC)                         
c     CALL EZSKCH (XX(1,1,1,3), ICT, XL, YC)                    
c     CALL EZSKCH (XX(1,1,1,4), ICT, XL, YC)  
c     CALL EZSKCH (XX(1,1,2,3), ICT, XL, YC)                    
c     CALL EZSKCH (XX(1,1,2,4), ICT, XL, YC)  
C
C  the XX array is now complete
C     var: 1-T  2-U  3-V  4-q
C
C***********************************************************************
C SET UP LAT,LON OF THE IMAX x JMAX "basis" grid
C***********************************************************************
C
      DO 521 J=1, JMAX
      YJ = J
      DO 520 I=1, IMAX
      XI = I
      CALL IJ2LL(XI,YJ,YLAT,WLON)
      ALAT(I,J) = YLAT
      ALON(I,J) = WLON
  520 CONTINUE
  521 CONTINUE
      print *,'ok after ij2ll'
C
C***********************************************************************
C adjust virtual to sensible temperature for NGM and
C SET UP the mean (sensible) temperature for NGM adjustment
C***********************************************************************
C
       IF( KB.NE.KBETA ) THEN
       XKAPA = 287.05e0 / 1005.0e0
       A0 = 6371220.0
       DEGREE = 90. / ASIN(1.0)
C
C -------- CHANGE VIRTUAL TEMP TO TEMP ----
       DO K=1,KDIMQ
        DO J=1,JDIM
         DO I=1,IDIM
          TEMP(I,J,K)=XX(I,J,K,1)
          QTEMP(I,J,K)=XX(I,J,K,4)
         ENDDO
        ENDDO
       ENDDO
       DO K=1,KDIMQ
        DO J=1,JDIM
         DO I=1,IDIM
c         XX(I,J,K,1)=XX(I,J,K,1)/(1.0+0.602*XX(I,J,K,4))
          XX(I,J,K,1)=TEMP(I,J,K)/(1.0+0.602*QTEMP(I,J,K))
         ENDDO
        ENDDO
       ENDDO
       print *,'ok after t to tv'
C -------- DO TEMPERATURE ADJUSTMENT -----
        DATA ITADJ/0/
        IF( ITADJ .EQ. 0 ) THEN
          ITADJ=1
          PRINT *,' ------ INITIAL TEMPERATURE (MEAN) ----'
          DO 420 K=1,KDIM
            TSUM=0.0e0
            ASUM=0.0e0
            DO 410 J=1,JDIM
              DPHI=DLAT*A0/DEGREE
              RLAT=ALAT(1,J)/DEGREE
              DLAM=DLON*A0*COS(RLAT)/DEGREE
              DAREA=DLAM*DPHI
              DO 400 I=1,IDIM
                TSUM=TSUM+XX(I,J,K,1)*DAREA
                ASUM=ASUM+DAREA
 400          CONTINUE
 410        CONTINUE
            TMN0(K)=TSUM/ASUM
            AREA(K)=ASUM
            DTMN(K)=0.0e0
            PRINT *,' K= ',K,' TMEAN=',TMN0(K)
 420      CONTINUE
        ELSE
          PRINT *,' ------ ADJUSTED TEMPERATURE ----'
          DO 460 K=1,KDIM
            TSUM=0.0e0
            DO 450 J=1,JDIM
              DPHI=DLAT*A0/DEGREE
              RLAT=ALAT(1,J)/DEGREE
              DLAM=DLON*A0*COS(RLAT)/DEGREE
              DAREA=DLAM*DPHI
              DO 440 I=1,IDIM
                TSUM=TSUM+XX(I,J,K,1)*DAREA
 440          CONTINUE
 450        CONTINUE
            TTM=TSUM/AREA(K)
            DTMN(K)=TMN0(K)-TTM
            PRINT *,' K=',K,' TMEAN=',TMN0(K),' T=',TTM,' DT=',DTMN(K)
 460      CONTINUE
        ENDIF
       ENDIF
C***********************************************************************
C  interpolate to the mass points (HPTS)
C***********************************************************************
C
      IFIRST = 0
C  surface pressure at mass points 
      CALL LL2pts(PS(1,1,1),PSATHPTS,
     &            NHPT,HPTLON,HPTLAT,IFIRST)
c     print 5444,(PSATHPTS(I),I=1,NHPT,150)
c5444 FORMAT(' some pts ',10e12.5)
C  topography at mass points 
      CALL LL2pts(PS(1,1,2),ZSATHPTS,
     &            NHPT,HPTLON,HPTLAT,IFIRST)
c     print 5444,(ZSATHPTS(I),I=1,NHPT,150)
      DO 522 K=1,KMAX
C  temperature at mass points and level k
      CALL LL2pts(XX(1,1,K,1),TATHPTS(1,K),
     &            NHPT,HPTLON,HPTLAT,IFIRST)
c     print 5444,(TATHPTS(I,K),I=1,NHPT,150)
C  u (zonal) wind component at mass points and level k
      CALL LL2pts(XX(1,1,K,2),UATHPTS(1,K),
     &            NHPT,HPTLON,HPTLAT,IFIRST)
c     print 5444,(UATHPTS(I,K),I=1,NHPT,150)
C  v (meridional) wind component at mass points and level k
      CALL LL2pts(XX(1,1,K,3),VATHPTS(1,K),
     &            NHPT,HPTLON,HPTLAT,IFIRST)
c     print 5444,(VATHPTS(I,K),I=1,NHPT,150)
C  specific humidity at mass points and level k
      CALL LL2pts(XX(1,1,K,4),QATHPTS(1,K),
     &            NHPT,HPTLON,HPTLAT,IFIRST)
c     print 5444,(QATHPTS(I,K),I=1,NHPT,150)
  522 CONTINUE
C
C***********************************************************************
C  interpolate to the u-comp wind points (UPTS)
C***********************************************************************
C
      IFIRST = 0
C  surface pressure at u-comp wind points 
      CALL LL2pts(PS(1,1,1),PSATUPTS,
     &            NUPT,UPTLON,UPTLAT,IFIRST)
c     print 5444,(PSATUPTS(I),I=1,NUPT,150)
C  topography at u-comp wind points 
      CALL LL2pts(PS(1,1,2),ZSATUPTS,
     &            NUPT,UPTLON,UPTLAT,IFIRST)
c     print 5444,(ZSATUPTS(I),I=1,NUPT,150)
      DO 523 K=1,KMAX
C  temperature at u-comp wind points and level k
      CALL LL2pts(XX(1,1,K,1),TATUPTS(1,K),
     &            NUPT,UPTLON,UPTLAT,IFIRST)
c     print 5444,(TATUPTS(I,K),I=1,NUPT,150)
C  u (zonal) wind component at u-comp wind points and level k
      CALL LL2pts(XX(1,1,K,2),UATUPTS(1,K),
     &            NUPT,UPTLON,UPTLAT,IFIRST)
c     print 5444,(UATUPTS(I,K),I=1,NUPT,150)
C  v (meridional) wind component at u-comp wind points and level k
      CALL LL2pts(XX(1,1,K,3),VATUPTS(1,K),
     &            NUPT,UPTLON,UPTLAT,IFIRST)
c     print 5444,(VATUPTS(I,K),I=1,NUPT,150)
  523 CONTINUE
C
C***********************************************************************
C  interpolate to the v-comp wind points (VPTS)
C***********************************************************************
C
      IFIRST = 0
C  surface pressure at v-comp wind points 
      CALL LL2pts(PS(1,1,1),PSATVPTS,
     &            NVPT,VPTLON,VPTLAT,IFIRST)
c     print 5444,(PSATVPTS(I),I=1,NVPT,150)
C  topography at v-comp wind points 
      CALL LL2pts(PS(1,1,2),ZSATVPTS,
     &            NVPT,VPTLON,VPTLAT,IFIRST)
c     print 5444,(ZSATVPTS(I),I=1,NVPT,150)
      DO 524 K=1,KMAX
C  temperature at v-comp wind points and level k
      CALL LL2pts(XX(1,1,K,1),TATVPTS(1,K),
     &            NVPT,VPTLON,VPTLAT,IFIRST)
c     print 5444,(TATVPTS(I,K),I=1,NVPT,150)
C  u (zonal) wind component at v-comp wind points and level k
      CALL LL2pts(XX(1,1,K,2),UATVPTS(1,K),
     &            NVPT,VPTLON,VPTLAT,IFIRST)
c     print 5444,(UATVPTS(I,K),I=1,NVPT,150)
C  v (meridional) wind component at v-comp wind points and level k
      CALL LL2pts(XX(1,1,K,3),VATVPTS(1,K),
     &            NVPT,VPTLON,VPTLAT,IFIRST)
c     print 5444,(VATVPTS(I,K),I=1,NVPT,150)
  524 CONTINUE
      IF( KBETA.EQ.KB ) THEN
        CALL WRTETA(TATHPTS,UATUPTS,VATUPTS,QATHPTS,PSATHPTS,
     &   ZSATHPTS,NUNIT)
      ELSE
        CALL WRITER(NUPT,NVPT,NHPT,NUNIT,ITIME,DLAM0,
     1   JCAP)
      END IF
 9800 CONTINUE
C
C   INTERPOLATE TO ETA VERTICAL COORDINATE
C
      CALL PETABCS(HPTLAT,HPTLON,VPTLAT,VPTLON,SI,SL)
C
      CALL W3TAGE('MKBND   ')
        STOP
        END
