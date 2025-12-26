      SUBROUTINE MIXDIAG
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    MIXDIAG     PRINTS OUT COLUMNAR VALUES OF VARIABLES
C   PRGMMR: J TUCCILLO       ORG: W/NMC4     DATE: 90-06-17
C
C ABSTRACT:     PRINT OUT COLUMNAR VALUES OF POTENTIAL TEMPERATURE,
C   .           AND RELATIVE HUMIDITY, TOGETHER WITH SURFACE
C   .           ENERGY FLUXES, ETC.
C   .                PRINT IS PERFORMED AFTER EVERY 12 CALLS.
C   .           LOCATION OF SELECTED POINTS ( NDIAG IN NUMBER )
C   .           IS SPECIFIED BY DATA STMNTS
C   .           FOR 'NGSTN', 'ISTN' AND 'JSTN'
C   .
C
C PROGRAM HISTORY LOG:
C   90-06-17  J TUCCILLO
C
C USAGE:  CALL MIXDIAG
C   OUTPUT FILES:
C     FT06F001  - FOR PRINTOUT
C
C   SUBPROGRAMS CALLED:
C     LIBRARY:
C       COMMON   - COMCONST
C                  COMBLANK
C
C REMARKS:
C   .
C   - - - - - - - - I N P U T   V A R I A B L E S  - - - - - - - - -
C   .
C   NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C   -----       ----------------------------------        ---------
C   .
C   VBL          FORECAST VARIABLES                       COMMON
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN
C   MACHINE:  CRAY Y-MP
C
C$$$
C
      INCLUDE 'parmodel'
C
C        NDIAG IS THE NUMBER OF GRID POINTS FOR WHICH PRINT WILL
C                BE MADE.
      PARAMETER ( NDIAG = 6 )
C...TRANSLATED BY FPP 3.00Z36 11/09/90  15:00:34  
C...SWITCHES: OPTON=I47,OPTOFF=VAE0
C
C     COMMON BLOCK /COMCONST/ CONTAINS GRID-RELATED PARAMETERS,
C          AND A FEW OTHER COMMON CONSTANTS.
      COMMON /COMCONST/ IJMAX, KM, LVLTOT, IJRAD, LOFCLDS(2,4),
     1                  ICALLRAD, IPHYSPL, NGRDUSE, NH,
     2                  NTIME, ITIME, NSTEPS,
     3                  IMG(INGRDUSE), JMG(INGRDUSE),
     4                  IAG(INGRDUS1), JAG(INGRDUS1),
     5                  IBG(INGRDUS1), JBG(INGRDUS1),
     6                  IADDRG(INIADDRS, INGRDUSE),
     7                  NPTSFH(2, INGRDUSE),
     8                  KUMULUS, LGRIDPPT, KLIFT1, KLIFT2, IBUCKET,
     9                  XPOLEH(INGRDUSE), YPOLEH(INGRDUSE), RADIUS,
     1                  DELSIG(IKM), PR(IKM), PRESS(IKM),
     2                  SIGINT(IKMP1),
     3                  DTOVDX, ANGVEL,
     4                  SIGMACC, SIGMAGSP, SIGMADHQ, CRITCONV,
     5                  SATDEL, RHFACTOR, QBOUND,
     6                  ANEM, BLKDR, CHARN, CONAUST, DDORF, PKATO,
     7                  SCALEHT, SIGDOT, DLAMNGM
C
      COMMON            SCR     (IIJMAX,  INSCR),
     1                  SCRGEOG (IIJMAX,  INSCRGEO),
     2                  SCR3    (IIJKMAX, INSCR3),
     3                  FILLER  (INFILLER),
     4                  VBL     (INVBL),
     5                  BITGRDH (IIJMAX, 2, INGRDUSE),
     6                  BITGRDU (IIJMAX, 2, INGRDUSE),
     7                  BITGRDV (IIJMAX, 2, INGRDUSE),
     8                  BITSEA  (IIJMAX, INGRDUSE),
     9                  BITSNO  (IIJMAX, INGRDUSE),
     1                  BITWVL  (IIJMAX, INGRDUSE)
C
      LOGICAL BITGRDH, BITGRDU, BITGRDV, BITSEA, BITSNO, BITWVL
C
C                   LOCAL ARRAYS
      DIMENSION NGSTN(NDIAG),ISTN(NDIAG),JSTN(NDIAG),TIME(12),X(24),
     1  THETA(IKM,12,NDIAG),     RH(IKM,12,NDIAG),
     2  H(12,NDIAG),      CLHI(12,NDIAG),     CLME(12,NDIAG),
     3  CLLO(12,NDIAG),   TGND(12,NDIAG),     TSS(12,NDIAG),
     4  QGND(12,NDIAG),   AVM(12,NDIAG),      EX(12,NDIAG),
     5  FLSH(12,NDIAG),   FLLH(12,NDIAG),     DSOL(12,NDIAG),
     6  ULWG(12,NDIAG),   DLWG(12,NDIAG),     ULWT(12,NDIAG),
     7  IP(IKM,NDIAG)
C
C              STATION LOCATIONS
      DATA NGSTN / 3, 3, 3, 3, 3, 3/ ,
     1      ISTN /87,84,73,69,46,48/ ,
     2      JSTN /37,42,26,40,28,43/
C
C          CALL COUNTER = 0,1,2,3,--12,0,1,2,3--12,0--ETC
      DATA ICALL/0/
C           CONSTANTS FOR VAPOR PRESSURE (OVER WATER)
      DATA AA/5.0065E0/, BB/24.84573E0/
C
C--------------------------------------------------------------
C            ADVANCE ICALL UPON ENTRANCE
C
C--------------------------------------------------------------
C            ADVANCE ICALL UPON ENTRANCE
      ICALL = ICALL + 1
C           RECORD TIME IN HOURS
      TIME(ICALL) = FLOAT(ITIME) / 3600.E0
C             SOME CONSTANTS
      CKAPPA = 287.05E0 / 1005.E0
      EPS = 287.05E0 / 461.5E0
C
C            RECORD VALUES
      DO 20 N = 1, NDIAG
C            ADDRESS PARAMETERS
      NG = NGSTN(N)
C
C***********************************************************************
C     A TEMPORARY FIX OF THE BOOBY TRAP.
      IF (NG .NE. NGRDUSE) RETURN
C***********************************************************************
C
      IM = IMG(NG)
      JM = JMG(NG)
      IJ = IM * JM
      I = ISTN(N)
      J = JSTN(N)
C          CONSTANT TO ACCESS FIELDS IN ARRAY VBL
      IAD = I - 1 + (J-1)*IM
C           SURFACE PRESSURE IN BARS
      HH = VBL(IADDRG(5,NG) + IAD )
      H(ICALL,N) = HH
      HREC = 1.E0 / HH
      HKAP = HH**CKAPPA
C            PREPARE TO SAVE THETA AND REL HUMID
      IADT = IADDRG(3,NG) + IAD
      IADQ = IADDRG(4,NG) + IAD
C
CMIC$ DO ALL VECTOR SHARED(KM, IJ, IADT, HREC, ICALL, N, HKAP, AA, BB, 
CMIC$1   HH, IADQ, EPS, VBL, THETA, PR, PRESS, RH, IP) PRIVATE(K, INC, 
CMIC$2   TH, PKAPPA, TEMP, TR, ES, P, Q, REL)
        DO 10 K=1,KM
        INC = (K-1)*IJ
        TH= VBL(IADT+INC) * HREC
        THETA(K,ICALL,N) = TH
C         GET TEMPERATURE AND SAT VAPOR PRESS ( IN CENTIBARS )
        PKAPPA = 2.E0 * PR(K) * HKAP
        TEMP = TH * PKAPPA
        TR = 273.16E0 / TEMP
        ES = 0.611 * ( TR**AA ) * EXP ( BB * (1.E0 - TR ) )
C             PRESSURE IN CENTIBARS
        P = HH * PRESS(K) * 100.E0
C            RELATIVE HUMIDITY OVER WATER
        Q = VBL(IADQ+INC) * HREC
        REL = Q * ( P -ES ) / ( (1.E0 - Q) * EPS * ES )
        RH(K,ICALL,N) = REL
C           RECORD LATEST PRESSURE AT LEVEL K IN MILLIBARS
        IP(K,N) = INT( 10.E0 * P )
C
   10   CONTINUE
C
C           SAVE 'SURFACE QUANTITIES '
      CLHI(ICALL,N) = VBL(IADDRG(30,NG) + IAD )
      CLME(ICALL,N) = VBL(IADDRG(29,NG) + IAD )
      CLLO(ICALL,N) = VBL(IADDRG(28,NG) + IAD )
      FLSH(ICALL,N) = VBL(IADDRG(36,NG) + IAD ) * 1.E3
      FLLH(ICALL,N) = VBL(IADDRG(37,NG) + IAD ) * 2.5E9
      ULWG(ICALL,N) = VBL(IADDRG(25,NG) + IAD ) * 1.E3
      ULWT(ICALL,N) = VBL(IADDRG(26,NG) + IAD ) * 1.E3
      DLWG(ICALL,N) = VBL(IADDRG(15,NG) + IAD ) * 1.E3
      DSOL(ICALL,N) = VBL(IADDRG(16,NG) + IAD ) * 1.E3
      TGND(ICALL,N) = VBL(IADDRG( 6,NG) + IAD ) - 273.16E0
      TSS(ICALL,N)  = VBL(IADDRG(21,NG) + IAD ) - 273.16E0
      QGND(ICALL,N) = VBL(IADDRG( 7,NG) + IAD )
      AVM(ICALL,N)  = VBL(IADDRG(18,NG) + IAD )
      EX(ICALL,N)   = VBL(IADDRG(19,NG) + IAD )
C
   20 CONTINUE
C
C               IS IT TIME TO PRINT?
      IF( ICALL .LT. 12 ) RETURN
C           YES, FIRST RECYCLE ICALL
      ICALL = 0
C
      PRINT 34
   34 FORMAT(1H1,40X,'DIAGNOSTICS FROM MIXDIAG')
      DO 100 N = 1,NDIAG
      PRINT 35,N,ISTN(N),JSTN(N),NGSTN(N)
   35 FORMAT(1H0,20X,' FOR POINT',I3,' AT  I,J=',
     1  2I4, ' ON GRID',I4,'.  PLOT IS  THETA . REL HUMID  ')
      PRINT 40,TIME
   40 FORMAT(1H0,3X,'TIME=',12(F8.2,2X) )
C
        DO 60 KK=1,KM
        K = KM + 1 - KK
CMIC$ DO ALL VECTOR SHARED(X) PRIVATE(IQ2)
        DO 1001 IQ2 = 1, 24
          X(IQ2) = 0.E0
1001    CONTINUE
        L = 1
CMIC$ DO ALL VECTOR SHARED(K, N, L, THETA, X, RH) PRIVATE(LL)
      DO 50 LL = 1, 12
         X(L+(LL-1)*2) = THETA(K,LL,N)
         X(L-1+LL*2) = RH(K,LL,N)
   50 CONTINUE
        PRINT 55,IP(K,N),X
   55   FORMAT(1H ,2X,I4,2X,12(F6.1,F4.2) )
C
   60 CONTINUE
C           PRINT 'SURFACE QUANTITIES'
      PRINT 61,(H(L,N),L=1,12)
   61 FORMAT(1H0,4X,'H',2X,12(F8.3,2X) )
      PRINT 62,(CLHI(L,N),L=1,12)
   62 FORMAT(1H ,'HICLD ',12(F8.3,2X) )
      PRINT 63,(CLME(L,N),L=1,12)
   63 FORMAT(1H ,'MDCLD ',12(F8.3,2X) )
      PRINT 64,(CLLO(L,N),L=1,12)
   64 FORMAT(1H ,'LOCLD ',12(F8.3,2X) )
      PRINT 65,(TGND(L,N),L=1,12)
   65 FORMAT(1H ,'GND T ',12(F8.3,2X) )
      PRINT 66,( TSS(L,N),L=1,12)
   66 FORMAT(1H ,'SUB T ',12(F8.3,2X) )
      PRINT 67,(QGND(L,N),L=1,12)
   67 FORMAT(1H ,'GND Q ',12(F8.3,2X) )
      PRINT 68,( AVM(L,N),L=1,12)
   68 FORMAT(1H ,'MSTAV ',12(F8.3,2X) )
      PRINT 69,(  EX(L,N),L=1,12)
   69 FORMAT(1H ,'EXCOF ',12(E8.3,2X) )
      PRINT 70,(FLSH(L,N),L=1,12)
   70 FORMAT(1H ,'SHFLX ',12(F8.3,2X) )
      PRINT 71,(FLLH(L,N),L=1,12)
   71 FORMAT(1H ,'LHFLX ',12(F8.3,2X) )
      PRINT 72,(DSOL(L,N),L=1,12)
   72 FORMAT(1H ,'DSOLG ',12(F8.3,2X) )
      PRINT 73,(DLWG(L,N),L=1,12)
   73 FORMAT(1H ,'DLWVG ',12(F8.3,2X) )
      PRINT 74,(ULWG(L,N),L=1,12)
   74 FORMAT(1H ,'ULWVG ',12(F8.3,2X) )
      PRINT 75,(ULWT(L,N),L=1,12)
   75 FORMAT(1H ,'ULWVT ',12(F8.3,2X) )
C
  100 CONTINUE
C
      RETURN
C
      END
