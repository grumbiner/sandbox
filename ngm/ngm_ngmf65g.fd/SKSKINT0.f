      SUBROUTINE SKSKINT0 ( NPAIR,NG,LADD3,LEN31,ITMAX,LPR )
C
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    SKSKINT0    COMPUTE INITIAL SURFACE TEMPERATURE
C   PRGMMR: J TUCCILLO       ORG: W/NMC40   DATE: 90-06-18
C
C ABSTRACT:    COMPUTES INITIAL SURFACE TEMPERATURE
C   .
C
C PROGRAM HISTORY LOG:
C   90-06-17  J TUCCILLO
C
C USAGE:  CALL SKSKINT0 ( NPAIR,NG,LADD3,LEN31,ITMAX,LPR )
C   INPUT ARGUMENT LIST:
C     NPAIR    - 1 FOR THE FIRST OF A PAIR OF CALLS TO
C                THIS GRID, 2 FOR THE SECOND OF A PAIR.
C     NG       - GRID NUMBER (OUTER HEMISPHERIC GRID HAS
C                NG EQUAL TO 1.)
C     LADD3    - THE LOCATION OF THE FIRST FORECAST POINT
C                IN A TWO-DIMENSIONAL ARRAY FOR THIS GRID
C                AND NPAIR.
C     LEN31    - THE LENGTH OF THE VECTOR ENCOMPASSING
C                ALL FORECAST POINTS IN TWO DIMENSIONS
C                FOR THIS GRID AND NPAIR.
C     ITMAX    - MAXIMUM NUMBER OF ITERATES FOR SOLVING
C                THE SURFACE ENERGY BUDGET WITH RIGHT
C                HAND SIDE EQUAL TO ZERO.
C     LPR      - LOGICAL FOR DEBUG PRINTS
C
C   OUTPUT FILES:
C     FT06F001 - FOR PRINTOUT
C
C   SUBPROGRAMS CALLED:
C     UNIQUE     - VAPTAB
C                  P2KAP
C                  SFCEXCH
C     LIBRARY:
C       COMMON   - COMCONST
C                  COMBLANK
C                  COMDIAG
C
C REMARKS:
C   .
C   - - - - - - - - I N P U T   V A R I A B L E S  - - - - - - - - -
C   .
C   NAMES      MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C   -----      ----------------------------------        ---------
C   .
C   BITGRDH    BIT VECTOR INDICATING THE LOCATION        /COMVBLS/
C   .          OF LEGITIMATE FORECAST POINTS.
C   .
C   VBL        ARRAY CONTAINING ALL FORECAST-TYPE        /COMVBLS/
C   .          VARIABLES FOR ALL GRIDS.
C   .
C   - - - - - - - - O U T P U T   V A R I A B L E S - - - - - - - - - -
C   .
C   NAMES      MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C   -----      ----------------------------------        ---------
C   .
C   VBL        ARRAY CONTAINING ALL FORECAST-TYPE        /COMVBLS/
C   .          VARIABLES FOR ALL GRIDS.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN
C   MACHINE:  CRAY Y-MP
C
C$$$
C
      INCLUDE 'parmodel'
C...TRANSLATED BY FPP 3.00Z36 11/09/90  14:44:02  
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
C     COMMON BLOCK /COMDIAG/ CONTAINS QUANTITIES SAVED FOR
C          DIAGNOSTIC PURPOSES.
      COMMON /COMDIAG/  KADIAB( IKM, INGRDUSE ),
     1                  NPTSDHQ (2, INGRDUSE),
     2                  KKCLOUD2(2, INGRDUSE), NPTSBUOY(2, INGRDUSE),
     3                  NPTSWATR(2, INGRDUSE), NPTSCC  (2, INGRDUSE),
     4                  KKGSP2  (2, INGRDUSE),
     5                  KKGSE2  (2, INGRDUSE),
     6                  NPTSSAT (IKM, 2, INGRDUSE),
     7                  NPTSEVAP(IKM, 2, INGRDUSE),
     8                  NPTSGSP (2, INGRDUSE), NUMPROF,
     9                  PPTCC   (2, INGRDUSE), PPTGSP  (2, INGRDUSE),
     1                  ALATPR  (IMAXPROF),    ALONPR  (IMAXPROF),
     2                  DESCRP  (IMAXPROF)
C
      CHARACTER*24 DESCRP
C
      DIMENSION H1     ( IIJMAX ), TMP  ( IIJMAX ),
     *          ASWG   ( IIJMAX ), TG1  ( IIJMAX ), F1     ( IIJMAX ),
     *          FP1    ( IIJMAX ), WORK1( IIJMAX ), AMOIS1 ( IIJMAX ),
     *          CG1    ( IIJMAX ), QG1  ( IIJMAX ), TSS1   ( IIJMAX ),
     *          ALWD   ( IIJMAX ), T1   ( IIJMAX ), Q1     (IIJMAX  ),
     *          EC1    ( IIJMAX ), SPD  ( IIJMAX ), DENSITY(IIJMAX  ),
     *          CD     ( IIJMAX ), SP1  ( IIJMAX ), BIT1   ( IIJMAX ),
     *          TGM1   ( IIJMAX ), FM1  ( IIJMAX ), TEMPER1( IIJMAX )
C
      LOGICAL    BIT1
      LOGICAL    LPR
C
      EQUIVALENCE ( SCR( 1 , 1 ) , H1      ( 1 ) ) ,
     *            ( SCR( 1 , 2 ) , ASWG    ( 1 ) ) ,
     *            ( SCR( 1 , 3 ) , T1      ( 1 ) ) ,
     *            ( SCR( 1 , 4 ) , Q1      ( 1 ) ) ,
     *            ( SCR( 1 , 5 ) , TG1     ( 1 ) ) ,
     *            ( SCR( 1 , 6 ) , F1      ( 1 ) ) ,
     *            ( SCR( 1 , 7 ) , FP1     ( 1 ) ) ,
     *            ( SCR( 1 , 8 ) , WORK1   ( 1 ) ) ,
     *            ( SCR( 1 , 9 ) , AMOIS1  ( 1 ) ) ,
     *            ( SCR( 1 , 10) , CG1     ( 1 ) ) ,
     *            ( SCR( 1 , 11) , QG1     ( 1 ) ) ,
     *            ( SCR( 1 , 12) , TSS1    ( 1 ) ) ,
     *            ( SCR( 1 , 13) , ALWD    ( 1 ) ) ,
     *            ( SCR( 1 , 14) , EC1     ( 1 ) ) ,
     *            ( SCR( 1 , 15) , SP1     ( 1 ) ) ,
     *            ( SCR( 1 , 16) , SPD     ( 1 ) ) ,
     *            ( SCR( 1 , 17) , DENSITY ( 1 ) ) ,
     *            ( SCR( 1 , 18) , CD      ( 1 ) )
      EQUIVALENCE ( SCR( 1 , 19) , TGM1    ( 1 ) ),
     *            ( SCR( 1 , 20) , FM1     ( 1 ) ),
     *            ( SCR( 1 , 21) , TEMPER1 ( 1 ) ),
     *            ( SCR( 1 , 22) , TMP     ( 1 ) ),
     *            ( SCR( 1 , 23) , BIT1    ( 1 ) )
C
      PARAMETER ( C1 = 100.E0 / 287.05E0 )
C
      PARAMETER (	
     *     CP       = 1005.E0   ,
     *     ALHEAT   = 2.5E+06   ,
     *     RV       = 461.5E0   ,
     *     STEFEPS  = 5.612E-8  ,
     *     EPS      = 0.622E0   ,
     *     ONEMEPS  = 0.378E0   ,
     *     SNOTEMP  = 273.E0    ,
     *     ATD      = 1.0E0     ,
     *     CONV     = 0.1E0     ,
     *     CNGMTOW  = 1000.E0  )
      INTEGER I
      REAL COFD1,COFD2,COFD3,COFD4,COFN1,COFN2,COFN3
C
C***********************************************************************
C
      PARAMETER (COFD1 = 1., COFD2 = 5.44053037, COFD3 = 2.27693825, 
     1   COFD4 = -0.0869930591, COFN1 = 0.34757549, COFN2 = 4.36732956, 
     2   COFN3 = 3.91557032)
      IM = IMG(NG)
      JM = JMG(NG)
      IJ = IM * JM
C
      LENUPDT = 0
      DO 88890 IQ2=1,LEN31
         BIT1(IQ2)=BITGRDH(LADD3+IQ2-1,NPAIR,NG).AND..NOT.BITSEA
     *   (LADD3+IQ2-1,NG)
          IF ( BIT1( IQ2 ) ) LENUPDT = LENUPDT + 1
88890 CONTINUE
C
C     ADDRESSES IN VBL
C
      MADDU   = IADDRG( 1, NG)
      MADDV   = IADDRG( 2, NG)
      MADDT   = IADDRG( 3, NG)
      MADDQ   = IADDRG( 4, NG)
      MADDH   = IADDRG( 5, NG)
      MADDTG  = IADDRG( 6, NG)
      MADDQG  = IADDRG( 7, NG)
      MADDLWG = IADDRG(15, NG)
      MADDSWG = IADDRG(16, NG)
      MADDMA  = IADDRG(18, NG)
      MADDEC  = IADDRG(19, NG)
      MADDTSS = IADDRG(21, NG)
      MADDCG  = IADDRG(22, NG)
C
      JADDH   = MADDH   - 1 + LADD3
      JADDQ   = MADDQ   - 1 + LADD3
      JADDTSS = MADDTSS - 1 + LADD3
      JADDCG  = MADDCG  - 1 + LADD3
      JADDSWG = MADDSWG - 1 + LADD3
      JADDLWG = MADDLWG - 1 + LADD3
      JADDTG  = MADDTG  - 1 + LADD3
      JADDQG  = MADDQG  - 1 + LADD3
      JADDEC  = MADDEC  - 1 + LADD3
      JADDMA  = MADDMA  - 1 + LADD3
C
C     INVERSE H AND HKAPPA * INVERSE H  ( FULL GRID )
C
C*****  Code Expanded From Routine:  P2KAP
CMIC$ DO ALL VECTOR SHARED(IJ, MADDH, VBL, WORK1) PRIVATE(I)
      DO 77001 I = 1, IJ
         WORK1(I) = (0.34757549+VBL(I+MADDH-1)*(4.36732956+VBL(I+MADDH-1
     1      )*3.91557032))/(1.+VBL(I+MADDH-1)*(5.44053037+VBL(I+MADDH-1)
     2      *(2.27693825+VBL(I+MADDH-1)*(-0.0869930591))))
77001 CONTINUE
C*****  End of Code Expanded From Routine:  P2KAP
C     COMPUTE WIND SPEED OF BOTTOM LAYER
CMIC$ DO ALL VECTOR SHARED(IJ, MADDH, MADDU, IM, MADDV, MADDT, C1, VBL, 
CMIC$1   WORK1, SPD, TMP, DENSITY) PRIVATE(IQ2, HINV, HKAPPA1S)
      DO 88900 IQ2=1,IJ
         HINV   =1.0E0/VBL(MADDH+IQ2-1)
         HKAPPA1S=WORK1(IQ2)*HINV
C
C     U-VELOCITY OF BOTTOM LAYER AT H POINTS
C
         SPD(IQ2) = SQRT (
     *          (0.5E0*(VBL(MADDU+IQ2-1)+VBL(MADDU-IM+IQ2-1))
     *         *HINV   ) **2 +
C     V-VELOCITY OF BOTTOM LAYER AT H POINTS
C
     *          (0.5E0*(VBL(MADDV+IQ2-1)+VBL(MADDV+IQ2-2))*
     *         HINV   ) **2 )
C
C     AIR TEMPERATURE
C
         TMP(IQ2)=VBL(MADDT+IQ2-1)*HKAPPA1S
C
C     DENSITY OF BOTTOM LAYER
C
         DENSITY(IQ2)=C1*VBL(MADDH+IQ2-1) / TMP(IQ2)
88900 CONTINUE
C
C     COMPRESS
C
      IN = 1
      DO 7001 IQ2 = 1, LEN31
         IF ( BIT1(IQ2) ) THEN
            T1(IN)        = TMP  ( LADD3   + IQ2 - 1 )
            SP1(IN)       = SPD  ( LADD3   + IQ2 - 1 )
            H1(IN)        = VBL  ( JADDH   + IQ2 - 1 )
            Q1(IN)        = VBL  ( JADDQ   + IQ2 - 1 )
            TSS1(IN)      = VBL  ( JADDTSS + IQ2 - 1 )
            CG1(IN)       = VBL  ( JADDCG  + IQ2 - 1 )
            AMOIS1(IN)    = VBL  ( JADDMA  + IQ2 - 1 )
            ALWD (IN)     = VBL  ( JADDLWG + IQ2 - 1 )
            ASWG(IN)      = VBL  ( JADDSWG + IQ2 - 1 )
            TG1(IN)       = VBL  ( JADDTG  + IQ2 - 1 )
            IN = IN + 1
         END IF
7001  CONTINUE
C
CMIC$ DO ALL VECTOR SHARED(LENUPDT, Q1, H1) PRIVATE(IQ2)
      DO 89030 IQ2=1,LENUPDT
         Q1(IQ2)=Q1(IQ2)/H1(IQ2)
         H1(IQ2)=CNGMTOW*H1(IQ2)
89030 CONTINUE
C
C     COMPUTE LOCATION FOR DIAGNOSTIC PRINT
C
      DO 666 IT = 1 , ITMAX
C
      CALL SFCEXCH ( NG, SPD, DENSITY, CD )
      CALL VAPTAB ( TG1 , QG1 , LENUPDT , 1 )
C
      IN = 1
      DO 4001 IQ2 = 1, LEN31
         IF ( BIT1(IQ2) ) THEN
            EC1(IN) = VBL ( JADDEC  + IQ2 - 1 )
            IN = IN + 1
         END IF
4001  CONTINUE
C
CMIC$ DO ALL VECTOR SHARED(LENUPDT, ANGVEL, QG1, H1, ASWG, ALWD, TG1, T1
CMIC$1   , EC1, Q1, AMOIS1, TSS1, CG1, F1) PRIVATE(IQ2, QG1S, QG1SS)
      DO 89050 IQ2=1,LENUPDT
C
         QG1S=QG1(IQ2)*10.E0
         QG1SS=EPS * QG1S / ( H1(IQ2)-ONEMEPS*QG1S)
C
         F1(IQ2)=(ASWG(IQ2)+ALWD(IQ2))*CNGMTOW -
     *           STEFEPS*(TG1(IQ2)**4) -
     *           (CP*CNGMTOW)*(TG1(IQ2)-T1(IQ2))*EC1(IQ2) -
     *           (ALHEAT*CNGMTOW)*(QG1SS-Q1(IQ2))*
     *           EC1(IQ2) * AMOIS1(IQ2) -
     *           (TG1(IQ2)-TSS1(IQ2))*CG1(IQ2)*ANGVEL
C
89050 CONTINUE
C
      IF ( IT .EQ. 1 ) THEN
CMIC$ DO ALL VECTOR SHARED(LENUPDT,F1,FP1,FM1,TG1,TGM1)PRIVATE(IQ2)
      DO 89230 IQ2=1,LENUPDT
         FP1(IQ2)=F1(IQ2)*(-1.E0/150.E0)
         FM1(IQ2)=F1(IQ2)
         TGM1(IQ2)=TG1(IQ2)
89230 CONTINUE
      ELSE
CMIC$ DO ALL VECTOR SHARED(LENUPDT, F1, FM1, TG1, TGM1, FP1) PRIVATE(IQ2
CMIC$1   , WORK1S)
      DO 89260 IQ2=1,LENUPDT
         WORK1S = F1(IQ2) - FM1(IQ2)
         IF ( ABS(WORK1S) .GT. 0.001E0 ) THEN
             FP1(IQ2)=F1(IQ2)*(TG1(IQ2)-TGM1(IQ2))/WORK1S
         ELSE
             FP1(IQ2) = 0.E0
         END IF
         FM1(IQ2)=F1(IQ2)
         TGM1(IQ2)=TG1(IQ2)
89260 CONTINUE
      END IF
C
C     LETS KEEP THE TEMPERATURE CHANGE WITHIN REASONABLE LIMITS
C
CMIC$ DO ALL VECTOR SHARED(LENUPDT, TG1, FP1) PRIVATE(IQ2)
      DO 89350 IQ2=1,LENUPDT
         TG1(IQ2) = TG1(IQ2) - AMIN1(AMAX1(FP1(IQ2),-6.0),6.0)
89350 CONTINUE
C
      IN = 1
      DO 8001 IQ2 = 1, LEN31
         IF ( BIT1(IQ2) ) THEN
            VBL(JADDTG+IQ2-1) = TG1(IN)
            IN = IN + 1
         END IF
8001  CONTINUE
C
      NUMBNC = 0
      DO 3002 IQ2 = 1, LENUPDT
         IF ( ABS(FP1(IQ2)) .GT. CONV ) NUMBNC = NUMBNC + 1
3002  CONTINUE
C
      IF ( NUMBNC .EQ. 0 ) GOTO 667
 
C
666   CONTINUE
C
C     IF WE GET TO HERE WE HAVENT CONVERGED...
C     SHZBOT ... KEEP GOING, ANY WEIRD INITIAL TEMPS WONT PERSIST
C     FOR LONG ONCE WE START THE TIME DIFFERENCING.
C
C
667   CONTINUE
C
CMIC$ DO ALL VECTOR SHARED(LEN31, LADD3, NG, JADDTG, BIT1, BITSNO, VBL)
CMIC$1    PRIVATE(IQ2)
      DO 89370 IQ2=1,LEN31
        IF ( BIT1(IQ2).AND.BITSNO(LADD3+IQ2-1,NG).AND.
     *       VBL(JADDTG+IQ2-1).GT.SNOTEMP ) THEN
              VBL(JADDTG+IQ2-1)=SNOTEMP
        END IF
89370 CONTINUE
C
      CALL VAPTAB ( VBL(JADDTG), QG1, LEN31, 1 )
CMIC$ PARALLEL SHARED(LEN31, JADDQG, BIT1, QG1, VBL, JADDH) PRIVATE(IQ2
CMIC$1   , QG1S)
CMIC$ DO PARALLEL VECTOR
      DO 89380 IQ2=1,LEN31
         QG1S=QG1(IQ2)*10.E0
         QG1(IQ2)= EPS * QG1S/
     *   (VBL(JADDH+IQ2-1)*CNGMTOW-ONEMEPS*QG1S)
89380 CONTINUE
C
CMIC$ DO PARALLEL VECTOR
      DO 5002 IQ2 = 1, LEN31
         IF ( BIT1(IQ2) ) THEN
            VBL(JADDQG+IQ2-1) = QG1(IQ2)
         END IF
5002  CONTINUE
CMIC$ END PARALLEL
C
C
      RETURN
      END
