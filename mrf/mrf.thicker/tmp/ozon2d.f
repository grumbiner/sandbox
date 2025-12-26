      SUBROUTINE OZON2D(SIGL,QO3,SFCP,XLAT,RSIN1,RCOS1,RCOS2)
CFPP$ NOCONCUR R
C ...  COMPUTE MODEL LYR O3 PROFILE FROM THE ORIGINAL GFDL DATA
      PARAMETER (L= 28 )
      PARAMETER (NL=81,NLP1=NL+1,LNGTH=37*NL)
      DIMENSION SIGL( 28 ),PSM( 256 )
      DIMENSION QO3( 256 , 28 ),SFCP( 256 ),XLAT( 256 )
      DIMENSION JJROW( 256 ),                           TTHAN( 256 )
      DIMENSION QO3O3( 256 ,NL)
      COMMON /RDFSAV/ EMISP,EMIST,XLATT,XLATP,Q19001,HP98,H3M6,
     1     HP75,H6M2,HP537,H74E1,H15E1,Q14330,HP2,TWENTY,HNINE,
     2     DEGRAD,HSIGMA,DAYSEC,RCO2
C...    **************************************************************
C--   SEASONAL CLIMATOLOGIES OF O3 FROM O3INTN
      COMMON /SEASO3/
C-       ...WINTER....  ...SPRING....  ...SUMMER....  ....FALL.....
     1   DDUO3N(37,NL), DDO3N2(37,NL), DDO3N3(37,NL), DDO3N4(37,NL)
     2  , PRGFDL(NL)
C...    **************************************************************
C...       BEGIN HERE .....
      RNDG = 180./ 3.141593E+0
      DO 10 I=1, 256
       TH2=HP2*XLAT(I)*RNDG
       JJROW(I)=Q19001-TH2
       TTHAN(I)=(19-JJROW(I))-TH2
   10 CONTINUE
C....   SEASONAL AND SPATIAL INTERPOLATION DONE BELOW.
      DO 20 K=1,NL
      DO 20 I=1, 256
        DO3V = DDUO3N(JJROW(I),K) + RSIN1*DDO3N2(JJROW(I),K)
     1             +RCOS1*DDO3N3(JJROW(I),K)
     2             +RCOS2*DDO3N4(JJROW(I),K)
        DO3VP = DDUO3N(JJROW(I)+1,K) + RSIN1*DDO3N2(JJROW(I)+1,K)
     1             +RCOS1*DDO3N3(JJROW(I)+1,K)
     2             +RCOS2*DDO3N4(JJROW(I)+1,K)
C...   NOW LATITUDINAL INTERPOLATION, AND
C          CONVERT O3 INTO MASS MIXING RATIO(ORIGINAL DATA MPY BY 1.E4)
C      FLIP VERTICAL COORDINATE TOO...
        QO3O3(I,NL+1-K) = 1. E -4 * (DO3V+TTHAN(I)*(DO3VP-DO3V))
   20 CONTINUE
C...    VERTICAL (LINEAR IN LN P) INTERPOLATE FOR EACH GRIDPOINT
      NUMITR = 0
      ILOG = NL
   21 CONTINUE
      ILOG = (ILOG+1)/2
        IF(ILOG.EQ.1) GO TO 22
        NUMITR = NUMITR + 1
        GO TO 21
   22 CONTINUE
      DO 60 K=1,L
        NHALF=(NL+1)/2
        DO 30 I=1, 256
          JJROW(I) = NHALF
          PSM(I) = SFCP(I) * SIGL(K)
   30   CONTINUE
        DO 40 IT=1,NUMITR
          NHALF=(NHALF+1)/2
          DO 40 I=1, 256
            IF(PSM(I).LT.PRGFDL(JJROW(I))) THEN
              JJROW(I) = JJROW(I) + NHALF
            ELSE IF(PSM(I).GE.PRGFDL(JJROW(I)-1)) THEN
              JJROW(I) = JJROW(I) - NHALF
            ENDIF
            JJROW(I) = MIN(JJROW(I),NL)
            JJROW(I) = MAX(JJROW(I),2)
   40   CONTINUE
        DO 50 I=1, 256
          IF(PSM(I).GT.PRGFDL(1)) THEN
            QO3(I,K) = QO3O3(I,1)
          ELSE IF(PSM(I).LT.PRGFDL(NL)) THEN
            QO3(I,K) = QO3O3(I,NL)
          ELSE
            APLO = ALOG(PRGFDL(JJROW(I)-1))
            APUP = ALOG(PRGFDL(JJROW(I)))
            QO3(I,K) = QO3O3(I,JJROW(I)) + (ALOG(PSM(I))-APUP) /
     1                 (APLO-APUP) *
     2                 (QO3O3(I,JJROW(I)-1)-QO3O3(I,JJROW(I)))
          ENDIF
   50   CONTINUE
   60 CONTINUE
      RETURN
      END
