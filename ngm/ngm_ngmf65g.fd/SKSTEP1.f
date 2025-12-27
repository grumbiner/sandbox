      SUBROUTINE SKSTEP1 ( NPAIR,NG,LADD3,LEN31,DTTT,LPR )
C
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:     SKSTEP1    TO TIMEMARCH SURFACE TEMPERATURE
C   PRGMMR: J TUCCILLO       ORG: W/NMC4     DATE: 90-06-17
C
C ABSTRACT: SOLVES THE SURFACE ENERGY BUDGET FOR THE PREDICTION
C   .       OF THE SKIN TEMPERATURE. RECALL THAT SKIN TEMPERATURE
C   .       IS DEFINED AS THE TEMPERATURE A PASSIVE RADIOMETER FROM
C   .       SPACE WOULD SEE.
C   .
C
C PROGRAM HISTORY LOG:
C   90-06-17  J TUCCILLO
C
C USAGE:  CALL SKSTEP1 ( NPAIR, NG, LADD3, LEN31, DTTT, LPR )
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
C     DTTT     - TIMESTEP ON THE A GRID
C     LPR      - LOGICAL FOR DEBUG PRINTS
C
C   OUTPUT FILES:
C     FT06F001 - FOR PRINTOUT
C
C   SUBPROGRAMS CALLED:
C     UNIQUE:    - P2KAP
C                  VAPTAB
C                  SFCEXCH
C     LIBRARY:
C       COMMON   - COMCONST
C                  COMBLANK
C                  COMDIAG
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
C   SOURCE STATEMENTS: XXXX
C
C$$$
C
      INCLUDE 'parmodel'
C...TRANSLATED BY FPP 3.00Z36 11/09/90  14:44:39  
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
     *          HKAPPA1( IIJMAX ), TG1  ( IIJMAX ), F1     ( IIJMAX ),
     *          FP1    ( IIJMAX ), WORK1( IIJMAX ), AMOIS1 ( IIJMAX ),
     *          CG1    ( IIJMAX ), QG1  ( IIJMAX ), TSS1   ( IIJMAX ),
     *          TGSQR1 ( IIJMAX ), T1   ( IIJMAX ), Q1     (IIJMAX  ),
     *          EC1    ( IIJMAX ), SPD  ( IIJMAX ), DENSITY(IIJMAX  ),
     *          CD     ( IIJMAX ), SP1  ( IIJMAX ), BIT1   ( IIJMAX )
C
      LOGICAL BIT1
C
      LOGICAL    LPR
C
      EQUIVALENCE ( SCR( 1 , 1 ) , H1      ( 1 ) ) ,
     *            ( SCR( 1 , 2 ) , HKAPPA1 ( 1 ) ) ,
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
     *            ( SCR( 1 , 13) , TGSQR1  ( 1 ) ) ,
     *            ( SCR( 1 , 14) , EC1     ( 1 ) ) ,
     *            ( SCR( 1 , 15) , SP1     ( 1 ) ) ,
     *            ( SCR( 1 , 16) , SPD     ( 1 ) ) ,
     *            ( SCR( 1 , 17) , DENSITY ( 1 ) ) ,
     *            ( SCR( 1 , 18) , CD      ( 1 ) ) ,
     *            ( SCR( 1 , 19) , TMP     ( 1 ) ) ,
     *            ( SCR( 1 , 20) , BIT1    ( 1 ) )
C
      PARAMETER (
     *     CP       = 1005.E0  ,
     *     ALHEAT   = 2.5E+06  ,
     *     RV       = 461.5E0  ,
     *     STEFEPS  = 5.612E-8 ,
     *     EPS      = 0.622E0  ,
     *     ONEMEPS  = 0.378E0  ,
     *     SNOTEMP  = 273.E0   ,
     *     CNGMTOW  = 1000.E0  ,
     *     C1       = 100.E0 / 287.05E0 )
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
         IF ( BIT1(IQ2) ) LENUPDT = LENUPDT + 1
88890 CONTINUE
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
C     INVERSE H AND HKAPPA * INVERSE H  ( FULL GRID )
C
C*****  Code Expanded From Routine:  P2KAP
CMIC$ DO ALL VECTOR SHARED(IJ, MADDH, VBL, HKAPPA1) PRIVATE(I)
      DO 77001 I = 1, IJ
         HKAPPA1(I) = (0.34757549+VBL(I+MADDH-1)*(4.36732956+VBL(I+MADDH
     1      -1)*3.91557032))/(1.+VBL(I+MADDH-1)*(5.44053037+VBL(I+MADDH-
     2      1)*(2.27693825+VBL(I+MADDH-1)*(-0.0869930591))))
77001 CONTINUE
C*****  End of Code Expanded From Routine:  P2KAP
C
CMIC$ DO ALL VECTOR SHARED(IJ, MADDH, MADDU, IM, MADDV, MADDT, C1, VBL, 
CMIC$1   HKAPPA1, SPD, TMP, DENSITY) PRIVATE(IQ2, H1S, HKAPPA1S)
      DO 88900 IQ2=1,IJ
         H1S=1.0E0/VBL(MADDH+IQ2-1)
         HKAPPA1S=HKAPPA1(IQ2)*H1S
C
C     U-VELOCITY OF BOTTOM LAYER AT H POINTS
C
         SPD(IQ2)=SQRT(
     *   (0.5E0*(VBL(MADDU+IQ2-1)+VBL(MADDU-IM+IQ2-1))*H1S)**2 +
C
C     V-VELOCITY OF BOTTOM LAYER AT H POINTS
C
     *   (0.5E0*(VBL(MADDV+IQ2-1)+VBL(MADDV+IQ2-2))*H1S)**2 )
C
C     AIR TEMPERATURE
C
         TMP(IQ2)=VBL(MADDT+IQ2-1)*HKAPPA1S
C
C     DENSITY OF BOTTOM LAYER
C
         DENSITY(IQ2)=C1*VBL(MADDH+IQ2-1)/TMP(IQ2)
88900 CONTINUE
C
C      COMPUTE SFC EXCHANGE
C
      CALL SFCEXCH ( NG, SPD, DENSITY, CD )
C
C     COMPRESS OUT LAND POINT QUANTITIES
C
      JADDH   = MADDH   - 1 + LADD3
      JADDQ   = MADDQ   - 1 + LADD3
      JADDTSS = MADDTSS - 1 + LADD3
      JADDCG  = MADDCG  - 1 + LADD3
      JADDTG  = MADDTG  - 1 + LADD3
      JADDQG  = MADDQG  - 1 + LADD3
      JADDSWG = MADDSWG - 1 + LADD3
      JADDLWG = MADDLWG - 1 + LADD3
      JADDEC  = MADDEC  - 1 + LADD3
      JADDMA  = MADDMA  - 1 + LADD3
C
      IN = 1
      DO 2002 IQ2 = 1, LEN31
         IF ( BIT1(IQ2) ) THEN
            H1      ( IN )  = VBL  ( IQ2 + JADDH   - 1 )
            T1      ( IN )  = TMP  ( IQ2 + LADD3   - 1 )
            SP1     ( IN )  = SPD  ( IQ2 + LADD3   - 1 )
            Q1      ( IN )  = VBL  ( IQ2 + JADDQ   - 1 )
            TSS1    ( IN )  = VBL  ( IQ2 + JADDTSS - 1 )
            CG1     ( IN )  = VBL  ( IQ2 + JADDCG  - 1 )
            TG1     ( IN )  = VBL  ( IQ2 + JADDTG  - 1 )
            QG1     ( IN )  = VBL  ( IQ2 + JADDQG  - 1 )
            F1      ( IN )  = VBL  ( IQ2 + JADDSWG - 1 )
            WORK1   ( IN )  = VBL  ( IQ2 + JADDLWG - 1 )
            EC1     ( IN )  = VBL  ( IQ2 + JADDEC  - 1 )
            AMOIS1  ( IN )  = VBL  ( IQ2 + JADDMA  - 1 )
            IN = IN + 1
         END IF
2002  CONTINUE
C
CMIC$ DO ALL VECTOR SHARED(LENUPDT, ANGVEL, DTTT, TG1, F1, WORK1, T1, 
CMIC$1   EC1, QG1, Q1, H1, AMOIS1, CG1, TSS1) PRIVATE(IQ2)
      DO 89050 IQ2=1,LENUPDT
C
C            SHORTWAVE DOWN AND LONGWAVE DOWN
C
         TG1(IQ2) = TG1(IQ2) +
     *             ((F1(IQ2)+WORK1(IQ2))*CNGMTOW -
     *     STEFEPS*(TG1(IQ2)**4) -
C
C            SENSIBLE HEAT FLUX
C
     *     (CP*CNGMTOW)*(TG1(IQ2)-T1(IQ2))*EC1(IQ2) -
C
C            LATENT HEAT FLUX
C
     *     (ALHEAT*CNGMTOW)*(QG1(IQ2)-(Q1(IQ2)/H1(IQ2)))*
     *     EC1(IQ2)*AMOIS1(IQ2) -
C
C            GROUND STORAGE TERM
C
     *      CG1(IQ2)*(TG1(IQ2)-TSS1(IQ2))*ANGVEL )  /
C
C            D (LONGWAVE UP) / D (TG)
C
     *              ((1.E0/DTTT)*CG1(IQ2)+
     *               (4.E0*STEFEPS)*(TG1(IQ2)**3) +
C
C            D (SENSIBLE HEAT FLUX) / D (TG)
C
     *        (CP*CNGMTOW)*EC1(IQ2) +
C
C            D (LATENT HEAT FLUX) / D (TG)
C
     *       (CNGMTOW*ALHEAT*ALHEAT/RV)*
     *       AMOIS1(IQ2)*EC1(IQ2)*QG1(IQ2)/(TG1(IQ2)**2)
C
C            D (GROUND STORAGE TERM) / D (TG)
C
     *       +ANGVEL*CG1(IQ2) )
89050 CONTINUE
C
      IN = 1
      DO 7001 IQ2 = 1, LEN31
         IF ( BIT1(IQ2) ) THEN
            VBL(IQ2+JADDTG-1 ) = TG1(IN)
            IN = IN + 1
         END IF
7001  CONTINUE
CMIC$ DO ALL VECTOR SHARED(LEN31, LADD3, NG, JADDTG, BIT1, BITSNO, VBL)
CMIC$1    PRIVATE(IQ2)
      DO 89310 IQ2=1,LEN31
         IF ( BIT1(IQ2) .AND. BITSNO(IQ2+LADD3-1, NG) .AND.
     *        VBL(JADDTG+IQ2-1) .GT. SNOTEMP ) THEN
            VBL(JADDTG+IQ2-1)=SNOTEMP
         END IF
89310 CONTINUE
C
      CALL VAPTAB ( VBL(JADDTG), QG1, LEN31, 1 )
CMIC$ PARALLEL SHARED(LEN31, JADDQG, BIT1, QG1, VBL, JADDH) PRIVATE(IQ2
CMIC$1   , QG1S)
CMIC$ DO PARALLEL VECTOR
      DO 89320 IQ2=1,LEN31
         QG1S=QG1(IQ2)*10.E0
         QG1(IQ2)=EPS*QG1S/
     *   (CNGMTOW*VBL(JADDH+IQ2-1)-ONEMEPS*QG1S)
89320 CONTINUE
C
CMIC$ DO PARALLEL VECTOR
      DO 8001 IQ2 = 1, LEN31
         IF ( BIT1(IQ2) ) THEN
            VBL(IQ2+JADDQG-1 ) = QG1(IQ2)
         END IF
8001  CONTINUE
CMIC$ END PARALLEL
C
C
      RETURN
      END
