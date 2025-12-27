      SUBROUTINE RDSTEP1( NPAIR,NG,LADD3,LEN31,JD,GMT,IDMEAN,ISW )
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    RDSTEP1     CALLS RADIATION ROUTINES, SAVES RESULTS
C   PRGMMR: J TUCCILLO       ORG: W/NMC40    DATE: 90-06-19
C
C ABSTRACT:  COORDINATES THE COMPUTATION OF CLOUDS AND LONG AND SHORT
C   .        WAVE RADIATION. IF DEBUG IS ON THEN MANY PRINT
C   .        STATEMENTS WILL BE EXECUTED.
C   .
C PROGRAM HISTORY LOG:
C   90-06-19  J TUCCILLO
C
C USAGE:  CALL RDSTEP1( NPAIR,NG,LADD3,LEN31,JD,GMT,IDMEAN,ISW )
C   INPUT ARGUMENT LIST:
C     NPAIR    -  1 FOR THE FIRST OF A PAIR OF CALLS TO
C                 THIS GRID, 2 FOR THE SECOND OF A PAIR.
C     NG       -  GRID NUMBER (OUTER HEMISPHERIC GRID HAS
C                 NG EQUAL TO 1.)
C     LADD3    -  THE LOCATION OF THE FIRST FORECAST POINT
C                 IN A TWO-DIMENSIONAL ARRAY FOR THIS GRID
C                 AND NPAIR.
C     LEN31    -  THE LENGTH OF THE VECTOR ENCOMPASSING
C                 ALL FORECAST POINTS IN TWO DIMENSIONS
C                 FOR THIS GRID AND NPAIR.
C     JD       -  NUMBER OF DAYS FROM 1 JANUARY INCLUSIVE
C     GMT      -  CURRENT TIME
C     IDMEAN   -  FLAG TO PLACE SUN AT LOCAL NOON
C     ISW      -  FLAG TO INDICATE WHICH RADIATION
C                 CALCULATIONS TO DO 1-SW, 2-LW, 3-BOTH
C
C   SUBPROGRAMS CALLED:
C     UNIQUE:    -
C                  P2KAP
C                  VAPTAB
C                  O3CLIMO
C                  RDCLDGEN
C                  RDCLDEXP
C                  SWRADIAT
C                  LWRADIAT
C                  COSZEN
C     LIBRARY:
C
C REMARKS:
C   .
C   - - - - - - - - I N P U T   V A R I A B L E S  - - - - - - - - -
C   .
C   NAMES      MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C   -----      ----------------------------------        ---------
C   .
C   BITGRDH    BIT VECTOR INDICATING THE LOCATION        /COMVBLS/
C   .             OF LEGITIMATE FORECAST POINTS.
C   .
C   DELSIG     SIGMA THICKNESSES FOR EACH                /COMCONST/
C   .             LAYER.
C   .
C   IADDRG     STARTING ADDRESSES IN VBL FOR EACH TYPE   /COMCONST/
C   .             OF DATA FOR EACH GRID.
C   .
C   IMG,JMG    HORIZONTAL DIMENSIONS OF ALL GRIDS.       /COMCONST/
C   .
C   KM         NUMBER OF HORIZONTAL LEVELS FOR ALL       /COMCONST/
C   .             GRIDS.
C   .
C   NPTSFH     THE ARRAY OF THE NUMBER OF FORECAST       /COMCONST/
C   .             POINTS FOR EACH GRID AND NPAIR.
C   .
C   PR         HALF OF (1-SIGMA) TO THE KAPPA POWER.     /COMCONST/
C   .
C   PRESS      PRESS(K) * SURFACE PRESSURE IS THE        /COMCONST/
C   .             PRESSURE OF LAYER K.
C   .
C   SIGINT     SIGMA VALUES AT THE KM + 1 INTERFACES.    /COMCONST/
C   .
C   VBL        ARRAY CONTAINING ALL FORECAST-TYPE        /COMVBLS/
C   .             VARIABLES FOR ALL GRIDS.
C   .             THE H, HTH, HQ, MA  AND CPR  FIELDS ARE NEEDED
C   .             AS INPUT.
C   .
C   - - - - - - - - O U T P U T   V A R I A B L E S - - - - - - - - - -
C   .
C   NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C   -----       ----------------------------------        ---------
C   .
C   VBL        ARRAY CONTAINING ALL FORECAST-TYPE        /COMVBLS/
C   '             VARIABLES FOR ALL GRIDS.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN
C   MACHINE:  CRAY Y-MP
C
C$$$
C
      INCLUDE 'parmodel'
C
      PARAMETER (
     *     CP       = 1005.E0  ,
     *     S0       = 1367.E0  ,
     *     EPS      = 0.10E0   ,
     *     DT       = 1.E0     ,
     *     CPRCONST = 1.E0     ,
     *     CWTONGM  = 0.001E0  ,
     *     CNGMTOW  = 1000.E0  ,
     *     ALSSNO   = 0.75E0   ,
     *     Z0SNO    = 0.10E0  )
C...TRANSLATED BY FPP 3.00Z36 11/09/90  15:00:41  
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
      DIMENSION ALLSCR ( IMAXSCR          )
      DIMENSION BLK1   ( IIJMAX , IBLK1D  )
      DIMENSION BLK2   ( IIJRAD , IBLK2D  )
C
      DIMENSION
     *          T1       ( IIJMAX , IKM   )    ,
     *          Q1       ( IIJMAX , IKM   )    ,
     *          H1       ( IIJMAX         )    ,
     *          TG1      ( IIJMAX         )    ,
     *          TSA1     ( IIJMAX         )    ,
     *          XLAT1    ( IIJMAX         )    ,
     *          CPR1     ( IIJMAX         )    ,
     *          HINV1    ( IIJMAX         )    ,
     *          HKAPPA1  ( IIJMAX         )    ,
     *          HKAPHI1  ( IIJMAX         )    ,
     *          HHKAPI1  ( IIJMAX         )    ,
     *          ALS1     ( IIJMAX         )    ,
     *          AMA1     ( IIJMAX         )    ,
     *          RADNET1  ( IIJMAX         )    ,
     *          COSZ1    ( IIJMAX         )    ,
     *          WORK     ( IIJMAX         )
C
      DIMENSION
     *          TL       ( IIJRAD , IKM   )    ,
     *          QL       ( IIJRAD , IKM   )    ,
     *          PL       ( IIJRAD , IKM   )    ,
     *          DPL      ( IIJRAD , IKM   )    ,
     *          O3L      ( IIJRAD , IKM   )    ,
     *          CLD      ( IIJRAD , IKM   )    ,
     *          CLU      ( IIJRAD , IKM   )    ,
     *          PI       ( IIJRAD , IKMP1 )    ,
     *          CLDA     ( IIJRAD ,     3 )    ,
     *          CLUA     ( IIJRAD         )    ,
     *          ICLDW    ( IIJRAD ,     3 )    ,
     *          ICLUW    ( IIJRAD ,     2 )    ,
     *          CLDW     ( IIJRAD ,     3 )    ,
     *          CLUW     ( IIJRAD ,     2 )    ,
     *          TG       ( IIJRAD         )    ,
     *          TSA      ( IIJRAD         )    ,
     *          COSZ     ( IIJRAD         )    ,
     *          CPR      ( IIJRAD         )    ,
     *          ALS      ( IIJRAD         )
C
      DIMENSION
     *          ATLX     ( IIJRAD , IKM   )    ,
     *          ASL      ( IIJRAD , IKM   )    ,
     *          RS       ( IIJRAD         )    ,
     *          RSCLR    ( IIJRAD         )    ,
     *          SS       ( IIJRAD         )    ,
     *          ULTCLR   ( IIJRAD         )    ,
     *          ULWBOT   ( IIJRAD         )    ,
     *          SWINC    ( IIJRAD         )    ,
     *          DLWBOT   ( IIJRAD         )    ,
     *          DSWCLR   ( IIJRAD         )    ,
     *          ULWTOP   ( IIJRAD         )    ,
     *          DSWTOP   ( IIJRAD         )    ,
     *          SSCLR    ( IIJRAD         )
C
      DIMENSION
     *          TEMP1    ( IIJRAD , IKM   )    ,
     *          TEMP2    ( IIJRAD , IKM   )    ,
     *          IW1      ( IIJRAD , IKM   )    ,
     *          IW2      ( IIJRAD , IKM   )
C
      DIMENSION BIT1     ( IIJMAX         )
C
      LOGICAL BIT1
C
      LOGICAL NOZ      ,
     *        LFRST    ,
     *        DEBUG    ,
     *        LSMLBLK  ,
     *        LSWR     ,
     *        LLWR     ,
     *        LPRINT
C
C     DATA NOZ / .TRUE. /
      INTEGER I1X,NZP1,KSTLO,KSPLO,KSTMD,KSPMD,KSTHI,KSPHI,K1X,IQ2W1X,J1
     .X,I2X,J2X,I3X
      REAL COFD1,COFD2,COFD3,COFD4,COFN1,COFN2,COFN3
      PARAMETER (COFD1 = 1., COFD2 = 5.44053037, COFD3 = 2.27693825, 
     1   COFD4 = -0.0869930591, COFN1 = 0.34757549, COFN2 = 4.36732956, 
     2   COFN3 = 3.91557032)
C
c     DATA NOZ / .TRUE. /
      DATA NOZ / .FALSE. /
      DATA LFRST / .TRUE. /
      DATA DEBUG / .FALSE. /
      DATA IOUTUPRT/6/
C
      EQUIVALENCE ( ALLSCR(1) , SCR(1,1) )
C
      EQUIVALENCE ( ALLSCR(IRADWRK + 1 ) , BLK1(1,1)  ) ,
     *            ( BLK1(1,   1        ) , T1(1,1)    ) ,
     *            ( BLK1(1, 1*IKM + 1  ) , Q1(1,1)    ) ,
     *            ( BLK1(1, 2*IKM + 1  ) , H1(1)      ) ,
     *            ( BLK1(1, 2*IKM + 2  ) , TG1(1)     ) ,
     *            ( BLK1(1, 2*IKM + 3  ) , TSA1(1)    ) ,
     *            ( BLK1(1, 2*IKM + 4  ) , XLAT1(1)   ) ,
     *            ( BLK1(1, 2*IKM + 5  ) , CPR1(1)    ) ,
     *            ( BLK1(1, 2*IKM + 6  ) , HINV1(1)   ) ,
     *            ( BLK1(1, 2*IKM + 7  ) , HKAPPA1(1) ) ,
     *            ( BLK1(1, 2*IKM + 8  ) , HKAPHI1(1) ) ,
     *            ( BLK1(1, 2*IKM + 9  ) , HHKAPI1(1) ) ,
     *            ( BLK1(1, 2*IKM + 10 ) , ALS1(1)    ) ,
     *            ( BLK1(1, 2*IKM + 11 ) , AMA1(1)    ) ,
     *            ( BLK1(1, 2*IKM + 12 ) , RADNET1(1) ) ,
     *            ( BLK1(1, 2*IKM + 13 ) , COSZ1(1)   ) ,
     *            ( BLK1(1, 2*IKM + 14 ) , WORK(1)    )
C
      EQUIVALENCE ( ALLSCR (IRADWRK + IBLK1WRK + 1 ),BLK2(1,1))
C
      EQUIVALENCE ( BLK2(1,           1) , TL(1,1)     ) ,
     *            ( BLK2(1,  1*IKM +  1) , QL(1,1)     ) ,
     *            ( BLK2(1,  2*IKM +  1) , PL(1,1)     ) ,
     *            ( BLK2(1,  3*IKM +  1) , DPL(1,1)    ) ,
     *            ( BLK2(1,  4*IKM +  1) , O3L(1,1)    ) ,
     *            ( BLK2(1,  5*IKM +  1) , CLD(1,1)    ) ,
     *            ( BLK2(1,  6*IKM +  1) , CLU(1,1)    ) ,
     *            ( BLK2(1,  7*IKM +  1) , ATLX(1,1)   ) ,
     *            ( BLK2(1,  8*IKM +  1) , ASL(1,1)    ) ,
     *            ( BLK2(1,  9*IKM +  1) , TEMP1(1,1)  ) ,
     *            ( BLK2(1, 10*IKM +  1) , TEMP2(1,1)  ) ,
     *            ( BLK2(1, 11*IKM +  1) , IW1(1,1)    ) ,
     *            ( BLK2(1, 13*IKM +  1) , IW2(1,1)    ) ,
     *            ( BLK2(1, 15*IKM +  1) , PI(1,1)     ) ,
     *            ( BLK2(1, 16*IKM +  2) , CLDA(1,1)   ) ,
     *            ( BLK2(1, 16*IKM +  5) , CLUA(1)     )
C
      EQUIVALENCE ( BLK2(1, 16*IKM +  6) , ICLDW(1,1)  ) ,
     *            ( BLK2(1, 16*IKM + 12) , ICLUW(1,1)  ) ,
     *            ( BLK2(1, 16*IKM + 16) , CLDW(1,1)   ) ,
     *            ( BLK2(1, 16*IKM + 19) , CLUW(1,1)   ) ,
     *            ( BLK2(1, 16*IKM + 21) , TG(1)       ) ,
     *            ( BLK2(1, 16*IKM + 22) , TSA(1)      ) ,
     *            ( BLK2(1, 16*IKM + 23) , COSZ(1)     ) ,
     *            ( BLK2(1, 16*IKM + 24) , CPR(1)      ) ,
     *            ( BLK2(1, 16*IKM + 25) , ALS(1)      ) ,
     *            ( BLK2(1, 16*IKM + 26) , RS(1)       ) ,
     *            ( BLK2(1, 16*IKM + 27) , RSCLR(1)    ) ,
     *            ( BLK2(1, 16*IKM + 28) , SS(1)       ) ,
     *            ( BLK2(1, 16*IKM + 29) , ULTCLR(1)   ) ,
     *            ( BLK2(1, 16*IKM + 30) , ULWBOT(1)   ) ,
     *            ( BLK2(1, 16*IKM + 31) , SWINC(1)    ) ,
     *            ( BLK2(1, 16*IKM + 32) , DLWBOT(1)   ) ,
     *            ( BLK2(1, 16*IKM + 33) , DSWCLR(1)   ) ,
     *            ( BLK2(1, 16*IKM + 34) , ULWTOP(1)   ) ,
     *            ( BLK2(1, 16*IKM + 35) , DSWTOP(1)   ) ,
     *            ( BLK2(1, 16*IKM + 36) , SSCLR(1)    )
C
C***********************************************************************
C
C     SET LOGICALS FOR WHICH RADIATION TO DO
C
      IF ( ISW .EQ. 1 ) THEN
         LSWR = .TRUE.
         LLWR = .FALSE.
      ELSE IF ( ISW .EQ. 2 ) THEN
         LSWR = .FALSE.
         LLWR = .TRUE.
      ELSE IF ( ISW .EQ. 3 ) THEN
         LSWR = .TRUE.
         LLWR = .TRUE.
      END IF
C
      IF ( LFRST ) THEN
C
         LFRST = .FALSE.
         ISCRAMT = IMAXSCR
         IWRKAMT = IRADWRK + IBLK1WRK + IBLK2WRK
C
         IF ( ISCRAMT .LT. IWRKAMT) THEN
            WRITE(IOUTUPRT,1001) ISCRAMT,IWRKAMT
1001        FORMAT(' WORK SPACE PROBLEM IN RDSTEP1',/,
     *             ' ISCRAMT =',I10,/,
     *             ' IWRKAMT =',I10 )
           CALL EXIT(16)
         ELSE
C           ILEFT = ISCRAMT - IWRKAMT
C           WRITE(IOUTUPRT,1002) ISCRAMT,IWRKAMT,ILEFT
C1002       FORMAT(' WORK SPACE OK IN RDSTEP1',/,
C    *             ' ISCRAMT =',I10,/,
C    *             ' IWRKAMT =',I10,/,
C    *             ' THERE ARE ',I10,2X,' WORDS STILL AVAILABLE')
         END IF
C
      END IF
C
C
      MADDT    = IADDRG( 3, NG)
      MADDQ    = IADDRG( 4, NG)
      MADDH    = IADDRG( 5, NG)
      MADDTG   = IADDRG( 6, NG)
      MADDALB  = IADDRG( 8, NG)
      MADDSWR  = IADDRG(14, NG)
      MADDDLB  = IADDRG(15, NG)
      MADDDSB  = IADDRG(16, NG)
      MADDCPR  = IADDRG(17, NG)
      MADDMA   = IADDRG(18, NG)
      MADDEC   = IADDRG(19, NG)
      MADDZ0   = IADDRG(20, NG)
      MADDULB  = IADDRG(25, NG)
      MADDULT  = IADDRG(26, NG)
      MADDCAC  = IADDRG(27, NG)
      MADDCAL  = IADDRG(28, NG)
      MADDCAM  = IADDRG(29, NG)
      MADDCAH  = IADDRG(30, NG)
      MADDCLL  = IADDRG(31, NG)
      MADDCLM  = IADDRG(32, NG)
      MADDCLH  = IADDRG(33, NG)
      MADDCLB  = IADDRG(34, NG)
      MADDCLT  = IADDRG(35, NG)
      MADDLWR  = IADDRG(38, NG)
      MADDSWO  = IADDRG(40, NG)
      MADDSWI  = IADDRG(41, NG)
C
      IM = IMG(NG)
      JM = JMG(NG)
      IJ = IM * JM
      NUMCOL1 = NPTSFH(NPAIR, NG)
      KMP1 = KM + 1
C
C              COMPUTE THE NUMBER OF BLOCKS WE WILL PROCESS
C
      NUMBLKS = NUMCOL1 / IJRAD
      NUMLAST = NUMCOL1 - NUMBLKS * IJRAD
      IF ( NUMLAST  .NE. 0 ) THEN
         NUMBLKS = NUMBLKS + 1
         LSMLBLK = .TRUE.
      ELSE
         LSMLBLK = .FALSE.
      END IF
C
      IF ( DEBUG ) THEN
         PRINT 4881
4881     FORMAT(//////)
         PRINT 4882 , NG , NUMCOL1
4882     FORMAT(' *****GRID NUMB ',2X,I5,' FORECAST POINTS=',2X,I5)
         PRINT 4883
4883     FORMAT(///)
         PRINT 1000 , NUMBLKS , NUMLAST , LSMLBLK
1000     FORMAT(' NUMBLKS=',2X,I5,/,
     *          ' NUMLAST =',2X,I5,/,
     *          ' LSMLBLK =',2X,L2 )
C
         PRINT 57799 , JD , GMT
57799    FORMAT(' JD , GMT =',2X,I5,2X,F15.5)
      END IF
C
C  COMPUTE COS OF THE ZENITH ANGLE AND LATITUDES
C  (USE WORK,H1,T1 AS TEMPORARY WORK SPACE)
C
      CALL COSZEN(NG,GMT,FLOAT(JD),H1,T1(1,1),T1(1,2),T1(1,3),T1(1,4),
     *            WORK , IDMEAN )
C
C  COMPRESS OUT SOLAR ZENITH ANGLE, LATITUDES, AND H
C
      JADDH = MADDH - 1 + LADD3
      IN = 1
      DO 8001 IQ2 = 1, LEN31
         IF ( BITGRDH(IQ2+LADD3-1,NPAIR,NG) ) THEN
            COSZ1(IN) = WORK(IQ2+LADD3-1)
            XLAT1(IN) = H1(IQ2+LADD3-1)
            H1(IN) = VBL(IQ2+JADDH-1)
            IN = IN + 1
         END IF
8001  CONTINUE
C
C         COMPRESS  THETA, Q
C
      JADDT = MADDT - 1 + LADD3 - IJ
      JADDQ = MADDQ - 1 + LADD3 - IJ
C
CMIC$ PARALLEL SHARED(NUMCOL1, H1, HKAPPA1, KM, JADDT, IJ, JADDQ, LEN31
CMIC$1   , LADD3, NPAIR, NG, BITGRDH, VBL, T1, Q1) PRIVATE(I1X, IN, K, 
CMIC$2   IQ2)
C
CMIC$ DO PARALLEL
      DO 1 K = 1, KM
         IN = 1
         DO 77014 IQ2 = 1, LEN31
            IF (BITGRDH(LADD3+IQ2-1,NPAIR,NG)) THEN
               T1(IN,K) = VBL(JADDT+IJ*K+IQ2-1)
               Q1(IN,K) = VBL(JADDQ+IJ*K+IQ2-1)
               IN = IN + 1
            ENDIF
77014    CONTINUE
    1 CONTINUE
C*****  Code Expanded From Routine:  P2KAP
CMIC$ DO PARALLEL VECTOR
      DO 77001 I1X = 1, NUMCOL1
         HKAPPA1(I1X) = (0.34757549+H1(I1X)*(4.36732956+H1(I1X)*
     1      3.91557032))/(1.+H1(I1X)*(5.44053037+H1(I1X)*(2.27693825+H1(
     2      I1X)*(-0.0869930591))))
77001 CONTINUE
CMIC$ END PARALLEL
C*****  End of Code Expanded From Routine:  P2KAP
CMIC$ PARALLEL SHARED(KM, NUMCOL1, PR, T1, HKAPHI1, Q1, HINV1, H1, 
CMIC$1   HKAPPA1, HHKAPI1) PRIVATE(K, C1, IQ2W6E)
CMIC$ DO PARALLEL VECTOR
      DO 88890 IQ2W6E=1,NUMCOL1
         HINV1(IQ2W6E)=1.0E0/H1(IQ2W6E)
         HKAPHI1(IQ2W6E)=HKAPPA1(IQ2W6E)*HINV1(IQ2W6E)
         HHKAPI1(IQ2W6E)=H1(IQ2W6E)/HKAPPA1(IQ2W6E)
88890 CONTINUE
C
CMIC$ DO PARALLEL
      DO  2   K = 1 , KM
         C1    = 2.0E0 * PR(K)
      DO 88920 IQ2W6E=1,NUMCOL1
         T1(IQ2W6E,K)=T1(IQ2W6E,K)*HKAPHI1(IQ2W6E)*C1
         Q1(IQ2W6E,K)=AMAX1((Q1(IQ2W6E,K)*HINV1(IQ2W6E)), 1.E-06 )
         if(t1(IQ2W6E,K).lt.150.0.or.t1(IQ2W6E,K).gt.325.) then
           print *,'t1 ',IQ2W6E,K,T1(IQ2W6E,K)
         endif
         if(q1(IQ2W6E,K).lt.-1.e-4.or.q1(IQ2W6E,K).gt.30.e-3) then
           print *,'q1 ',IQ2W6E,K,q1(IQ2W6E,K)
         endif
88920 CONTINUE
 2    CONTINUE
CMIC$ END PARALLEL
      C1    = 1.0 / (2.0E0 * PR(1))
CMIC$ DO ALL VECTOR SHARED(NUMCOL1, C1, T1, TSA1) PRIVATE(IQ2W6E)
      DO 88930 IQ2W6E=1,NUMCOL1
         TSA1(IQ2W6E)=T1(IQ2W6E,1) * C1
88930 CONTINUE
C
C            COMPRESS OUT TG, AMA, ALS, AND CPR
C
      JADDTG  = MADDTG  - 1 + LADD3
      JADDZ0  = MADDZ0  - 1 + LADD3
      JADDALB = MADDALB - 1 + LADD3
      JADDMA  = MADDMA  - 1 + LADD3
      JADDCPR = MADDCPR - 1 + LADD3
C
CMIC$ DO ALL VECTOR SHARED(LEN31, JADDALB, LADD3, NG, JADDZ0, VBL, WORK
CMIC$1   , BITSNO) PRIVATE(IQ2W6E)
      DO 88970 IQ2W6E = 1, LEN31
         WORK(IQ2W6E) = VBL(JADDALB+IQ2W6E-1)
         IF (BITSNO(LADD3+IQ2W6E-1,NG)) WORK(IQ2W6E) = WORK(IQ2W6E) + (
     1      0.75E0-WORK(IQ2W6E))*(1.E0-VBL(JADDZ0+IQ2W6E-1)/(VBL(JADDZ0+
     2      IQ2W6E-1)+0.10E0))
88970 CONTINUE
C
      IN = 1
      DO 5001 IQ2 = 1, LEN31
         IF ( BITGRDH(IQ2+LADD3-1,NPAIR,NG) ) THEN
            ALS1(IN) = WORK(IQ2)
            TG1(IN)  = VBL(IQ2+JADDTG-1)
            CPR1(IN) = VBL(IQ2+JADDCPR-1)
            IN = IN + 1
         END IF
5001  CONTINUE
C
CMIC$ PARALLEL SHARED(NUMCOL1, CPR1, H1, IJRAD, KM, ICLUW) PRIVATE(
CMIC$1   IQ2W6E)
CMIC$ DO PARALLEL VECTOR
      DO 88990 IQ2W6E=1,IJRAD
         ICLUW(IQ2W6E,1)=1
         ICLUW(IQ2W6E,2)=KM
88990 CONTINUE
C
C     CONVERT FROM NGM UNITS OF PSTAR(BARS) TO MBS
C
CMIC$ DO PARALLEL VECTOR
      DO 89030 IQ2W6E=1,NUMCOL1
         CPR1(IQ2W6E)=CPRCONST
         H1(IQ2W6E)=H1(IQ2W6E)*CNGMTOW
89030 CONTINUE
CMIC$ END PARALLEL
C
C===================================================================
C
C              LOOP OVER NUMBLKS TO COMPUTE RADIATION
C
C===================================================================
C
      ISTS   = 1
      ISTCNTS = 1
C
      DO 666 KBLK = 1 , NUMBLKS
C
             IF(DEBUG) THEN
                LPRINT = .TRUE.
             ELSE
                LPRINT = .FALSE.
             END IF
C
             IF ( ( KBLK .EQ. NUMBLKS ) .AND.  LSMLBLK  )  THEN
                NUMB = NUMLAST
             ELSE
                NUMB = IJRAD
             END IF
C
            IF (LPRINT) THEN
             PRINT 4885 , KBLK
4885         FORMAT(///,' ****************** KBLK = ',2X,I5,' ******',
     *              '*************',/)
             END IF
C
C                 STARTING ADDRESS AND LENGTH FOR DECOMPRESS
C
         IF ( LPRINT) THEN
            PRINT 59932 , ISTCNTS
59932       FORMAT(' ISTCNTS = ',2X,I5)
         END IF
C
         IF (KBLK.NE.NUMBLKS) THEN
C
             LENDCOM = 0
             ICOUNT  = 0
             ISTCNT = ISTCNTS
             ISTDCOM = ISTCNTS + LADD3 - 1
             DO 3001 I=ISTCNT,LEN31
                II=LADD3 + I -1
                LENDCOM = LENDCOM  + 1
                IF (BITGRDH(II,NPAIR,NG)) ICOUNT = ICOUNT + 1
                IF (ICOUNT .EQ. (NUMB + 1)) THEN
                    ISTCNTS = II - LADD3 + 1
                    LENDCOM = LENDCOM - 1
                    GOTO 3002
                END IF
3001        CONTINUE
3002        CONTINUE
C
         ELSE
C
            ISTDCOM = ISTCNTS + LADD3 - 1
            LENDCOM = LEN31 - ISTCNTS + 1
C
         END IF
C
         IF ( LPRINT) THEN
            PRINT 59933 , ISTDCOM , LENDCOM
59933       FORMAT(' ISTDCOM , LENDCOM = ',2X,I5,2X,I5)
         END IF
C
C
CMIC$ DO ALL VECTOR SHARED(NUMB, ISTS, TG1, TSA1, TSA, TG, CPR1, CPR, 
CMIC$1   ALS1, ALS, COSZ1, COSZ, XLAT1, RS) PRIVATE(IQ2W6E)
c     tgmax=1.e-35
c     tgmin=1.e35
c     almax=1.e-35
c     almin=1.e35
      DO 89040 IQ2W6E = 1, NUMB
         IF (TG1(ISTS+IQ2W6E-1) .GT. TSA1(ISTS+IQ2W6E-1)) THEN
            TSA(IQ2W6E) = TSA1(ISTS+IQ2W6E-1)
         ELSE
            TSA(IQ2W6E) = TG1(ISTS+IQ2W6E-1)
         ENDIF
         TG(IQ2W6E) = TG1(ISTS+IQ2W6E-1)
         CPR(IQ2W6E) = CPR1(ISTS+IQ2W6E-1)
         ALS(IQ2W6E) = ALS1(ISTS+IQ2W6E-1)
         COSZ(IQ2W6E) = COSZ1(ISTS+IQ2W6E-1)
         RS(IQ2W6E) = XLAT1(ISTS+IQ2W6E-1)
c        tgmax=amax1(TG(IQ2W6E),tgmax)
c        tgmin=amin1(TG(IQ2W6E),tgmin)
c        almax=amax1(als(IQ2W6E),almax)
c        almin=amin1(als(IQ2W6E),almin)
89040 CONTINUE
c     print *,'kblk,k,tgmax,tgmin,almax,almin ',
c    1    kblk,k,tgmax,tgmin,almax,almin
C
      IF (LPRINT)  THEN
         PRINT 8002 , COSZ(1),ALS(1),CPR(1),TG(1),TSA(1),
     *                H1(ISTS),RS(1)
8002     FORMAT(' COSZ     = ',2X,F15.5,/,
     *          ' ALS      = ',2X,F15.5,/,
     *          ' CPR      = ',2X,F15.5,/,
     *          ' TG       = ',2X,F15.5,/,
     *          ' TSA      = ',2X,F15.5,/,
     *          ' H        = ',2X,F15.5,/,
     *          ' LAT      = ',2X,F15.5)
      END IF
C
      IF ( NOZ ) THEN
CMIC$ DO ALL VECTOR SHARED(IJRAD, KM, O3L) PRIVATE(IQ2)
         DO 3901 IQ2 = 1, IJRAD * KM
            O3L(IQ2,1) = 0.E0
3901     CONTINUE
      ELSE
      CALL O3CLIMO (JD,GMT,NUMB,PRESS,RS,O3L,
     *              TEMP1(1,1),TEMP1(1,2),TEMP1(1,3),TEMP1(1,4))
      END IF
C
CMIC$ PARALLEL SHARED(NUMB, PI, KM, KMP1, ISTS, H1, SIGINT, T1, TL, Q1, 
CMIC$1   QL, PRESS, PL, DELSIG, DPL) PRIVATE(IQ2W6E, K, KK)
CMIC$ DO PARALLEL
             DO 667 K = 1 , KM
c         tmaxx = -1.e+35
c         tminn = 1.e+35
c         qmaxx = -1.e+35
c         qminn = 1.e+35
                KK = KMP1 - K
      DO 89110 IQ2W6E=1,NUMB
         TL(IQ2W6E,KK)=T1(ISTS+IQ2W6E-1,K)
         QL(IQ2W6E,KK)=Q1(ISTS+IQ2W6E-1,K)
         PL(IQ2W6E,KK)=H1(ISTS+IQ2W6E-1)*PRESS(K)
         DPL(IQ2W6E,KK)=H1(ISTS+IQ2W6E-1)*DELSIG(K)
c        tmaxx = amax1(TL(IQ2W6E,KK),tmaxx)
c        tminn = amin1(TL(IQ2W6E,KK),tminn)
c        qmaxx = amax1(QL(IQ2W6E,KK),qmaxx)
c        qminn = amin1(QL(IQ2W6E,KK),qminn)
89110 CONTINUE
c        print *, ' kblk,k,tmaxx,tminn,qmaxx,qminn= ',
c    1     kblk,k,tmaxx,tminn,qmaxx,qminn
 667         CONTINUE
C
CMIC$ DO PARALLEL
      DO 6648 K=1,KM
         KK = ( KMP1 + 1) - K
         DO 89150 IQ2W6E=1,NUMB
           PI(IQ2W6E,KK)=H1(ISTS+IQ2W6E-1)*(1.E0-SIGINT(K))
89150    CONTINUE
 6648 CONTINUE
CMIC$ DO PARALLEL VECTOR
      DO 89160 IQ2W6E=1,NUMB
         PI(IQ2W6E,1)=5.0E0
89160 CONTINUE
CMIC$ END PARALLEL
C
C                COMPUTE CLOUDS
C
             CALL RDCLDGEN( TL, QL, PL, CPR, CLDA, CLUA, ICLDW, ICLUW,
     *       CLD, TEMP2, LOFCLDS, IJRAD, KM, LPRINT )
C*****  Code Expanded From Routine:  RDCLDEXP
      NZP1 = KM + 1
      KSTLO = NZP1 - LOFCLDS(1,2)
      KSPLO = NZP1 - LOFCLDS(2,2)
      KSTMD = NZP1 - LOFCLDS(1,3)
      KSPMD = NZP1 - LOFCLDS(2,3)
      KSTHI = NZP1 - LOFCLDS(1,4)
      KSPHI = NZP1 - LOFCLDS(2,4)
CMIC$ PARALLEL SHARED(KM, IJRAD, ICLUW, CLUA, CLU, KSTHI, KSPHI, ICLDW, 
CMIC$1   CLDA, CLD, KSTMD, KSPMD, KSTLO, KSPLO) PRIVATE(K1X, IQ2W1X)
CMIC$ DO PARALLEL
C
C
      DO 77002 K1X = 1, KM
         DO 77003 IQ2W1X = 1, IJRAD
            CLD(IQ2W1X,K1X) = CLD(IQ2W1X,K1X)*0.E0
            CLU(IQ2W1X,K1X) = CLU(IQ2W1X,K1X)*0.E0
77003    CONTINUE
77002 CONTINUE
CMIC$ DO PARALLEL
C
      DO 77004 K1X = KSTLO, KSPLO, -1
         DO 77005 IQ2W1X = 1, IJRAD
            IF(ICLDW(IQ2W1X,1).EQ.K1X)CLD(IQ2W1X,K1X)=CLDA(IQ2W1X,1)
77005    CONTINUE
77004 CONTINUE
CMIC$ DO PARALLEL
C
      DO 77006 K1X = KSTMD, KSPMD, -1
         DO 77007 IQ2W1X = 1, IJRAD
            IF(ICLDW(IQ2W1X,2).EQ.K1X)CLD(IQ2W1X,K1X)=CLDA(IQ2W1X,2)
77007    CONTINUE
77006 CONTINUE
CMIC$ DO PARALLEL
C
      DO 77008 K1X = KSTHI, KSPHI, -1
         DO 77009 IQ2W1X = 1, IJRAD
            IF(ICLDW(IQ2W1X,3).EQ.K1X)CLD(IQ2W1X,K1X)=CLDA(IQ2W1X,3)
77009    CONTINUE
77008 CONTINUE
CMIC$ DO PARALLEL
C
      DO 77010 K1X = 1, KM
         DO 77011 IQ2W1X = 1, IJRAD
            IF (ICLUW(IQ2W1X,1).GE.K1X .AND. ICLUW(IQ2W1X,2).LE.K1X) CLU
     1         (IQ2W1X,K1X) = CLUA(IQ2W1X)
77011    CONTINUE
77010 CONTINUE
CMIC$ END PARALLEL
C*****  End of Code Expanded From Routine:  RDCLDEXP
             IF (LPRINT) THEN
               DO 1667 K = 1 , KM
                  KK = KMP1 - K
                  PRINT 7730 , KK,TL(1,KK),QL(1,KK),PL(1,KK),O3L(1,KK),
     *                         CLD(1,KK),CLU(1,KK),PRESS(K)
 7730             FORMAT(' KKK,TL,QL,PL,O3L,CLD,CLU,SIGL=',2X,I5,2X,
     *                F10.4,2X,E15.5,2X,F10.4,2X,E15.5,3(F10.4,2X))
 1667          CONTINUE
             END IF
C
C                 COMPUTE LONGWAVE AND SHORTWAVE RADIATION
C
      IF ( LLWR )
     *       CALL LWRADIAT ( DT , NOZ , PL ,PI ,TL , TG , TSA , QL ,
     *                     O3L , CLD , CLU , ATLX , RS , ULWBOT ,
     *                     DLWBOT , ULWTOP , RSCLR , ULTCLR )
      IF ( LSWR )
     *       CALL SWRADIAT ( DT , NOZ , S0 , ALS , COSZ , PI , DPL ,
     *                     TL , QL , O3L , CLD , CLU , SWINC , DSWCLR ,
     *                     DSWTOP , SSCLR , SS , ASL )
C
C         PLACE SW LEAVING TOP OF ATMOSPHERE IN RADNET1
C
      IF ( LSWR )THEN
CMIC$ DO ALL VECTOR SHARED(NUMB, SWINC, DSWTOP, RADNET1) PRIVATE(IQ2W6E)
      DO 89190 IQ2W6E=1,NUMB
         RADNET1(IQ2W6E)=SWINC(IQ2W6E)-DSWTOP(IQ2W6E)
89190 CONTINUE
      END IF
C
              IF (LPRINT) THEN
                 C1=24.E0 * 3600.E0
                 DO 9002 K=1,KM
                    AP1 = ATLX(1,K) * C1
                    AP2 = ASL(1,K)  * C1
                    PRINT 8005 , K,PL(1,K),CLD(1,K),CLU(1,K),AP1,AP2
8005                FORMAT(' K,PRES,CLD,CLU,LWRADIAT,SWRADIAT=',2X,I5,
     *                     2X,3(F10.4,2X),2X,2(E15.5,2X))
9002             CONTINUE
C
                 PRINT 8006 , DLWBOT(1),ULWBOT(1),ULWTOP(1),SS(1),
     *                        RADNET1(1),SWINC(1)
8006             FORMAT(' DLWBOT  =',2X,F20.5,/,
     *                  ' ULWBOT  =',2X,F20.5,/,
     *                  ' ULWTOP  =',2X,F20.5,/,
     *                  ' SS      =',2X,F20.5,/,
     *                  ' SWOUT   =',2X,F20.5,/,
     *                  ' SWINC   =',2X,F20.5)
              END IF
C
      C1 = 3600.E0 * CWTONGM
      IF ( LSWR ) THEN
CMIC$ DO ALL VECTOR SHARED(NUMB, C1, RADNET1, SWINC, SS) PRIVATE(IQ2W6E)
      DO 89200 IQ2W6E=1,NUMB
         RADNET1(IQ2W6E)=C1*RADNET1(IQ2W6E)
         SWINC(IQ2W6E)=C1*SWINC(IQ2W6E)
         SS(IQ2W6E)=SS(IQ2W6E)*CWTONGM
89200 CONTINUE
      END IF
C
      IF ( LLWR ) THEN
CMIC$ DO ALL VECTOR SHARED(NUMB, ULWBOT, DLWBOT, ULWTOP) PRIVATE(IQ2W6E)
      DO 89230 IQ2W6E=1,NUMB
         ULWBOT(IQ2W6E)=ULWBOT(IQ2W6E)*CWTONGM
         DLWBOT(IQ2W6E)=DLWBOT(IQ2W6E)*CWTONGM
         ULWTOP(IQ2W6E)=ULWTOP(IQ2W6E)*CWTONGM
89230 CONTINUE
      END IF
C
CMIC$ PARALLEL SHARED(IJRAD, KMP1, ICLUW, ICLDW) PRIVATE(IQ2W6E)
CMIC$ DO PARALLEL VECTOR
      DO 89260 IQ2W6E=1,IJRAD*3
         ICLDW(IQ2W6E,1)=KMP1-ICLDW(IQ2W6E,1)
89260 CONTINUE
CMIC$ DO PARALLEL VECTOR
      DO 89270 IQ2W6E=1,IJRAD*2
         ICLUW(IQ2W6E,1)=KMP1-ICLUW(IQ2W6E,1)
89270 CONTINUE
CMIC$ END PARALLEL
C*****  Code Expanded From Routine:  RDHGATHR
      J1X = IJRAD*3
CMIC$ DO ALL VECTOR SHARED(J1X, ICLDW, PRESS, CLDW) PRIVATE(I2X)
      DO 77012 I2X = 1, J1X
         CLDW(I2X,1) = PRESS(ICLDW(I2X,1))
77012 CONTINUE
C*****  End of Code Expanded From Routine:  RDHGATHR
C*****  Code Expanded From Routine:  RDHGATHR
      J2X = IJRAD*2
CMIC$ DO ALL VECTOR SHARED(J2X, ICLUW, PRESS, CLUW) PRIVATE(I3X)
      DO 77013 I3X = 1, J2X
         CLUW(I3X,1) = PRESS(ICLUW(I3X,1))
77013 CONTINUE
C*****  End of Code Expanded From Routine:  RDHGATHR
CMIC$ PARALLEL SHARED(IJRAD, CLUA, CLUW, CLDA, CLDW, ISTS, H1) PRIVATE(
CMIC$1   IQ2W6E)
CMIC$ DO PARALLEL VECTOR
      DO 89280 IQ2W6E=1,IJRAD
         CLDW(IQ2W6E,1)=H1(ISTS+IQ2W6E-1)*CLDW(IQ2W6E,1)
         CLDW(IQ2W6E,2)=H1(ISTS+IQ2W6E-1)*CLDW(IQ2W6E,2)
         CLDW(IQ2W6E,3)=H1(ISTS+IQ2W6E-1)*CLDW(IQ2W6E,3)
89280 CONTINUE
CMIC$ DO PARALLEL VECTOR
      DO 89310 IQ2W6E=1,IJRAD*3
         CLDW(IQ2W6E,1)=CLDW(IQ2W6E,1)*CWTONGM
89310 CONTINUE
CMIC$ DO PARALLEL VECTOR
      DO 89320 IQ2W6E=1,IJRAD
         CLUW(IQ2W6E,1)=H1(ISTS+IQ2W6E-1)*CLUW(IQ2W6E,1)
         CLUW(IQ2W6E,2)=H1(ISTS+IQ2W6E-1)*CLUW(IQ2W6E,2)
89320 CONTINUE
CMIC$ DO PARALLEL VECTOR
      DO 89340 IQ2W6E=1,IJRAD*2
         CLUW(IQ2W6E,1)=CLUW(IQ2W6E,1)*CWTONGM
89340 CONTINUE
CMIC$ DO PARALLEL VECTOR
      DO 89350 IQ2W6E=1,IJRAD*3
       IF ( CLDA(IQ2W6E,1) .LE. 0.E0 ) THEN
         CLDW(IQ2W6E,1)=0.E0
       END IF
89350 CONTINUE
CMIC$ DO PARALLEL VECTOR
      DO 89360 IQ2W6E=1,IJRAD
       IF ( CLUA(IQ2W6E) .EQ. 0.E0 ) THEN
         CLUW(IQ2W6E,1)=0.E0
         CLUW(IQ2W6E,2)=0.E0
       END IF
89360 CONTINUE
CMIC$ END PARALLEL
C
       IF ( LLWR ) THEN
             JADDB = MADDULB - 1 + ISTDCOM
             JADDT = MADDULT - 1 + ISTDCOM
             IN = 1
             DO 7001 IQ2 = 1, LENDCOM
                IF ( BITGRDH(IQ2+ISTDCOM-1, NPAIR, NG) ) THEN
                   VBL(IQ2+JADDB-1) = ULWBOT(IN)
                   VBL(IQ2+JADDT-1) = ULWTOP(IN)
                   IN = IN + 1
                END IF
7001         CONTINUE
      END IF
C
      IN = 1
      DO 7006 IQ2 = 1, LENDCOM
         IF ( BITGRDH(IQ2+ISTDCOM-1, NPAIR, NG) ) THEN
            VBL(IQ2+MADDCAC+ISTDCOM-2) = CLUA(IN)
            VBL(IQ2+MADDCAL+ISTDCOM-2) = CLDA(IN,1)
            VBL(IQ2+MADDCAM+ISTDCOM-2) = CLDA(IN,2)
            VBL(IQ2+MADDCAH+ISTDCOM-2) = CLDA(IN,3)
            VBL(IQ2+MADDCLL+ISTDCOM-2) = CLDW(IN,1)
            VBL(IQ2+MADDCLM+ISTDCOM-2) = CLDW(IN,2)
            VBL(IQ2+MADDCLH+ISTDCOM-2) = CLDW(IN,3)
            VBL(IQ2+MADDCLB+ISTDCOM-2) = CLUW(IN,1)
            VBL(IQ2+MADDCLT+ISTDCOM-2) = CLUW(IN,2)
            IN = IN + 1
          END IF
7006    CONTINUE
C
        JADDSWR = MADDSWR - 1 + ISTDCOM - IJ
        JADDLWR = MADDLWR - 1 + ISTDCOM - IJ
        DO  8   K = 1 , KM
            KINV = KMP1 - K
            C6 = 0.5E0 / PR(K)
CMIC$ DO ALL VECTOR SHARED(NUMB,ISTS,C6,HHKAPI1,WORK)PRIVATE(IQ2W6E)
      DO 89380 IQ2W6E=1,NUMB
         WORK(IQ2W6E)=HHKAPI1(ISTS+IQ2W6E-1)*C6
89380 CONTINUE
                 IF ( LSWR ) THEN
                    JADDSWR = JADDSWR + IJ
CMIC$ DO ALL VECTOR SHARED(NUMB, KINV, ASL, WORK) PRIVATE(IQ2W6E)
      DO 89390 IQ2W6E=1,NUMB
         ASL(IQ2W6E,KINV)=ASL(IQ2W6E,KINV)*WORK(IQ2W6E)
89390 CONTINUE
                    IN = 1
                    DO 7008 IQ2 = 1, LENDCOM
                       IF ( BITGRDH(IQ2+ISTDCOM-1,NPAIR,NG) ) THEN
                          VBL(IQ2+JADDSWR-1) = ASL(IN,KINV)
                          IN = IN + 1
                       END IF
7008                CONTINUE
                 END IF
                 IF ( LLWR ) THEN
                    JADDLWR = JADDLWR + IJ
CMIC$ DO ALL VECTOR SHARED(NUMB, KINV, ATLX, WORK) PRIVATE(IQ2W6E)
      DO 89400 IQ2W6E=1,NUMB
         ATLX(IQ2W6E,KINV)=ATLX(IQ2W6E,KINV)*WORK(IQ2W6E)
89400 CONTINUE
                    IN = 1
                    DO 7009 IQ2 = 1, LENDCOM
                       IF ( BITGRDH(IQ2+ISTDCOM-1,NPAIR,NG) ) THEN
                          VBL(IQ2+JADDLWR-1) = ATLX(IN,KINV)
                          IN = IN + 1
                       END IF
7009                CONTINUE
                 END IF
 8           CONTINUE
C
             IF ( LLWR ) THEN
                JADDH = MADDDLB - 1 + ISTDCOM
                    IN = 1
                    DO 7019 IQ2 = 1, LENDCOM
                       IF ( BITGRDH(IQ2+ISTDCOM-1,NPAIR,NG) ) THEN
                          VBL(IQ2+JADDH-1) = DLWBOT(IN)
                          IN = IN + 1
                       END IF
7019                CONTINUE
             END IF
             IF ( LSWR ) THEN
                JADDH = MADDDSB - 1 + ISTDCOM
                    IN = 1
                    DO 7029 IQ2 = 1, LENDCOM
                       IF ( BITGRDH(IQ2+ISTDCOM-1,NPAIR,NG) ) THEN
                          VBL(IQ2+JADDH-1) = SS(IN)
                          IN = IN + 1
                       END IF
7029                CONTINUE
             END IF
C
C
C            ACCUMULATE SOLAR TERMS FOR ALBEDO CALCULATION
C
      IF ( LSWR ) THEN
         JADDH = MADDSWO - 1 + ISTDCOM
         IN = 1
         DO 4441 IQ2 = 1, LENDCOM
            IF ( BITGRDH(IQ2+ISTDCOM-1,NPAIR,NG) ) THEN
               WORK(IN) = VBL(IQ2+JADDH-1)
               IN = IN + 1
            END IF
4441     CONTINUE
CMIC$ DO ALL VECTOR SHARED(NUMB, WORK, RADNET1) PRIVATE(IQ2W6E)
      DO 89410 IQ2W6E=1,NUMB
         WORK(IQ2W6E)=WORK(IQ2W6E)+RADNET1(IQ2W6E)
89410 CONTINUE
         IN = 1
         DO 9001 IQ2 = 1, LENDCOM
            IF ( BITGRDH(IQ2+ISTDCOM-1,NPAIR,NG) ) THEN
               VBL(IQ2+JADDH-1) = WORK(IN)
               IN = IN + 1
               END IF
9001     CONTINUE
C
         JADDH = MADDSWI - 1 + ISTDCOM
         IN = 1
         DO 7776 IQ2 = 1, LENDCOM
            IF ( BITGRDH(IQ2+ISTDCOM-1,NPAIR,NG) ) THEN
               WORK(IN) = VBL(IQ2+JADDH-1)
               IN = IN + 1
            END IF
7776     CONTINUE
CMIC$ DO ALL VECTOR SHARED(NUMB, WORK, SWINC) PRIVATE(IQ2W6E)
      DO 89420 IQ2W6E=1,NUMB
         WORK(IQ2W6E)=WORK(IQ2W6E)+SWINC(IQ2W6E)
89420 CONTINUE
         IN = 1
         DO 5555 IQ2 = 1, LENDCOM
            IF ( BITGRDH(IQ2+ISTDCOM-1,NPAIR,NG) ) THEN
               VBL(IQ2+JADDH-1) = WORK(IN)
               IN = IN + 1
               END IF
5555     CONTINUE
C
      END IF
C
      ISTS = ISTS + NUMB
C
 666  CONTINUE
C
C
      RETURN
      END
