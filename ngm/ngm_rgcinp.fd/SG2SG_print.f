      SUBROUTINE SG2SG(IJMLB,CH)
C$$$  SUBROUTINE DOCUMENTATION BLOCK
C
C SUBROUTINE:  SG2SG   INTERPOLATE VALUES BETWEEN TWO SIGMA SURFACES.
C   AUTHOR: HANN-MING HENRY JUANG             DATE: 89-09-18
C
C
C ABSTRACT: USE CUBIC INTERPOLATION UNDER TENSION.
C
C PROGRAM HISTORY LOG:
C   89-09-12  HANN-MING HENRY JUANG  wrote original code
C   90-07-01  KEN MITCHELL CONVERTED TO CRAY Y-MP
C   91-05-17  Geoff DiMego fixed program so that NO extrapolation beyond
C             top or bottom of bounding-model's domain (in vertical)
C
C USAGE:
C   INPUT ARGUMENTS:
C
C     IJMLB    -   TOTAL LENGTH OF DATA IN IJ FOR LTBL
C     CH       -   WHICH VARIABLE TO BE PERFORMED IN NGM BOX.
C                  FOR EXAMPLES:
C                  CH='UVTQ' FOR ALL VARIABLES
C                  CH=' V  ' FOR V ONLY
C                  CH='  TQ' FOR T AND Q
C
C
C REMARKS:       NONE AT THIS TIME.
C
C ATTRIBUTES:
C   LANGUAGE:    FORTRAN 77
C   SOURCE STATEMENTS: ABOUT 250.
C
C$$$
C
C
      IMPLICIT REAL (A-H, O-Z)
C
C
      INCLUDE 'parmodel'
C
C
      COMMON /CONSTS/ IH, IQ, IT, IU, IV, IZ,
     1                NLAT, LEVH, LEVS, LONF, NSIGSTP,
     2                SLAT( INLAT ), STRIPARA( INSIGSTP ,  INLAT )
C
C       *****   END OF INCLUDED COMMON BLK CONSTS   *****
C
C
      LOGICAL  BITSEA, BITSNO, BITWVL
C
      COMMON /CONSTN/ IMG( INGRDUSE ), JMG( INGRDUSE ),
     1                IAG( INGRDUS1 ), JAG( INGRDUS1 ),
     2                IBG( INGRDUS1 ), JBG( INGRDUS1 ),
     3                IADDRG( INIADDRS ,  INGRDUSE ),
     4                INDEXHU,  INDEXHV,  INDEXHTH, INDEXHQ,
     5                INDEXH,   INDEXST,  INDEXSQ,  INDEXALB,
     6                INDEXSLP, INDEXPSI, INDEXCD,  INDEXCC,  INDEXGSP,
     7                INDEXRAD, INDEXDLW, INDEXDSW, INDEXCPR, INDEXMA,
     8                INDEXSEC, INDEXZ0, INDEXTSS,  INDEXCG,  INDEXDHQ,
     9                INDEXTSD, KM, NH, NGRDUSE,
     1                SPECS( INSPECS ), DELSIG( IKM ),
     2                POVH( IKM ), PIOVHK( IKM ),
     3                CMUU  ( INGRDUSE ),
     4                XPOLEH( INGRDUSE ), YPOLEH( INGRDUSE ),
     5                XPOLEU( INGRDUSE ), YPOLEU( INGRDUSE ),
     6                XPOLEV( INGRDUSE ), YPOLEV( INGRDUSE ),
     7                DLAMNGM,
     8                BITSEA( IIJMAX ,  INGRDUSE ),
     9                BITSNO( IIJMAX ,  INGRDUSE ),
     1                BITWVL( IIJMAX ,  INGRDUSE )
C
C       ****   END OF INCLUDED COMMON BLOCK CONSTN   ***
C
      PARAMETER (K15=SELECTED_REAL_KIND(15))
      REAL(KIND=K15) FACT,ALNPS
C
      COMMON    VBL( INVBL )
C
      PARAMETER(IJLB=2000,KS=64,KSP=KS+1,KMX= IKM ,KMP1=KMX+1)
      COMMON /LTBL/ ULB(IJLB,KS),VLB(IJLB,KS),TLB(IJLB,KS)
     1             ,QLB(IJLB,KS),HLB(IJLB),HLBNEW(IJLB)
     2             ,SLYR(KS),ZSLB(IJLB),LBUO,LBVO,LBCH
     3             ,LSCRU(IJLB), LSCRV(IJLB), LSCRH(IJLB)
C
      DIMENSION TMP(IJLB,KS),PRESS( IKM ),SIGINT(KMP1),PR( IKM )
     1         ,PRS(KS)
      CHARACTER*4 CH
      REAL CKAPPAP
C
      DATA INP/0/
      IF( INP .EQ. 0 ) THEN
      INP = 1
C
      PRINT *,' INITIATE SIGMA LAYER COMPUTATION'
C
      CKAPPAI = 1005.E0 / 287.05E0
      CKAPPA  = 1.0E0 / CKAPPAI
      CKAPPAP = REAL( CKAPPA + 1.0E0 )
      PRINT *,' CKAPPA CKAPPAI CKAPPAP ',CKAPPA,CKAPPAI,CKAPPAP
C
C              CALCULATE THE INTERFACE VALUES OF SIGMA.
      SIGINT(1) = 0.0E0
C
      DO 1800 K=1, IKM
         SIGINT(K+1) = SIGINT(K) + DELSIG(K)
 1800 CONTINUE
C
      SIGINT(KM+1) = 1.0E0
      PRINT *,' SIGINT ',(SIGINT(K),K=1,KM+1)
C
C              COMPUTE PR(K), WHICH WHEN MULTIPLIED BY 2 TIMES
C              H ** KAPPA YIELDS PI, THE EXNER FUNCTION.
      PR(1) = 1.0E0
C
      DO 1900 K=2, IKM
         ONEMSIG = 1.E0 - REAL (SIGINT(K) )
         PR(K) = REAL( ONEMSIG ** CKAPPAP )
 1900 CONTINUE
C
      C1 = 1.0E0 / (2.E0 * REAL (CKAPPAP) )
C
      DO 2000 K=1, IKM -1
         PR(K) = C1 * (PR(K) - PR(K+1) ) / DELSIG(K)
 2000 CONTINUE
C
      PR(KM) = C1 * PR(KM) / DELSIG(KM)
      PRINT *,' PR ',(PR(K),K=1,KM)
C
C              CALCULATE PRESS(K), WHICH WHEN MULTIPLIED BY
C              H (THE SURFACE PRESSURE) YIELDS THE LAYER PRESSURE.
      DO 2100 K=1, IKM
         PRESS(K) = REAL ((REAL (2.0E0 * PR(K)))
     1                 ** (REAL (CKAPPAI)))
 2100 CONTINUE
C
      PRINT 2200, (PRESS(K),K=1, IKM )
 2200 FORMAT(' ** SIGMA LAYER **',/(1X,10G13.6))
C
      ENDIF
C
C
 2222 FORMAT(' BEFORE SG2SG K=',I5,1X,8G13.6)
 3333 FORMAT(' AFTER SG2SG  K=',I5,1X,8G13.6)
C
      IF( CH(1:1) .EQ. 'U' ) THEN
C
C     PRINT * , ' LINEAR INTERPOLATION AT VARIABLE U. '
C
      DO 110 I=1,IJMLB
      DO 100 K=1,KS
      PRS(K)=HLB(I)*SLYR(K)
 100  CONTINUE
      DO 110 K=1, IKM
      PSTAR = HLBNEW(I) * PRESS(K)
      CALL GETBMT(LM,PSTAR,PRS,KS)
        IF( LM .EQ. KS ) LM = KS - 1
      LP = LM + 1
      FACT = ALOG(REAL(PRS(LP))) - ALOG(REAL(PRS(LM)))
      FACT = (ALOG(REAL(PRS(LP))) - ALOG(REAL(PSTAR))) / FACT
C   avoid extrapolation below 'bounding-model's terrain
        IF( PSTAR.gt.PRS(LM) ) LP = LM
C   avoid extrapolation above 'bounding-model's top-level
        IF( PSTAR.lt.PRS(LP) ) FACT = 0.0
      TMP(I,K) = ULB(I,LP) + ( ULB(I,LM) - ULB(I,LP) ) * FACT
 110  CONTINUE
      DO K=1,KS
        PRINT 2222,K,(ULB(I,K),I=837,844)
      ENDDO
      DO K=1,IKM
        PRINT 3333,K,(TMP(I,K),I=837,844)
      ENDDO
      DO 120 K=1, IKM
      DO 120 I=1,IJMLB
      ULB(I,K) = TMP(I,K)
 120  CONTINUE
C
      ENDIF
C
      IF( CH(2:2) .EQ. 'V' ) THEN
C
C     PRINT * , ' LINEAR INTERPOLATION AT VARIABLE V. '
      DO 210 I=1,IJMLB
      DO 200 K=1,KS
      PRS(K) = HLB(I) * SLYR(K)
 200  CONTINUE
      DO 210 K=1, IKM
      PSTAR = HLBNEW(I) * PRESS(K)
      CALL GETBMT(LM,PSTAR,PRS,KS)
        IF( LM .EQ. KS ) LM = KS - 1
      LP = LM + 1
      FACT = ALOG(REAL(PRS(LP))) - ALOG(REAL(PRS(LM)))
      FACT = (ALOG(REAL(PRS(LP))) - ALOG(REAL(PSTAR))) / FACT
C   avoid extrapolation below 'bounding-model's terrain
        IF( PSTAR.gt.PRS(LM) ) LP = LM
C   avoid extrapolation above 'bounding-model's top-level
        IF( PSTAR.lt.PRS(LP) ) FACT = 0.0
      TMP(I,K) = VLB(I,LP) + ( VLB(I,LM) - VLB(I,LP) ) * FACT
 210  CONTINUE
      DO K=1,KS
        PRINT 2222,K,(VLB(I,K),I=837,844)
      ENDDO
      DO K=1,IKM
        PRINT 3333,K,(TMP(I,K),I=837,844)
      ENDDO
      DO 220 K=1, IKM
      DO 220 I=1,IJMLB
      VLB(I,K) = TMP(I,K)
 220  CONTINUE
C
      ENDIF
C
      IF( CH(3:3) .EQ. 'T' ) THEN
C
C     PRINT * , '       INTERPOLATION AT VARIABLE T. '
C
      DO 310 I=1,IJMLB
      DO 300 K=1,KS
      PRS(K) = HLB(I) * SLYR(K)
 300  CONTINUE
      DO 310 K=1, IKM
      PSTAR = HLBNEW(I) * PRESS(K)
      CALL GETBMT(LM,PSTAR,PRS,KS)
        IF( LM.ge.2 .AND. LM.lt.KS ) THEN
C     PRINT * , ' CUBIC INTERPOLATION AT VARIABLE T. '
      LP = LM + 1
      LB = LM - 1
      CALL GETABC(A,B,C,TLB(I,LB),TLB(I,LM),TLB(I,LP),
     1                  PRS(  LB),PRS(  LM),PRS(  LP))
      ALNPS = ALOG( REAL(PSTAR) )
      TMP(I,K) = A * ALNPS * ALNPS + B * ALNPS + C
        ELSE
C     PRINT * , ' LINEAR INTERPOLATION AT VARIABLE T. '
        IF( LM .EQ. KS ) LM = KS - 1
      LP = LM + 1
      FACT = ALOG(REAL(PRS(LP))) - ALOG(REAL(PRS(LM)))
      FACT = (ALOG(REAL(PRS(LP))) - ALOG(REAL(PSTAR))) / FACT
C   avoid extrapolation below 'bounding-model's terrain
        IF( PSTAR.gt.PRS(LM) ) LP = LM
C   avoid extrapolation above 'bounding-model's top-level
        IF( PSTAR.lt.PRS(LP) ) FACT = 0.0
      TMP(I,K) = TLB(I,LP) + ( TLB(I,LM) - TLB(I,LP) ) * FACT
        ENDIF
 310  CONTINUE
      DO K=1,KS
        PRINT 2222,K,(TLB(I,K),I=837,844)
      ENDDO
      DO K=1,IKM
        PRINT 3333,K,(TMP(I,K),I=837,844)
      ENDDO
      DO 320 K=1, IKM
      DO 320 I=1,IJMLB
      TLB(I,K) = TMP(I,K)
 320  CONTINUE
C
      ENDIF
C
      IF( CH(4:4) .EQ. 'Q' ) THEN
C
C     PRINT * , '       INTERPOLATION AT VARIABLE Q. '
C
      DO 410 I=1,IJMLB
      DO 400 K=1,KS
      PRS(K) = HLB(I) * SLYR(K)
 400  CONTINUE
      DO 410 K=1,KM
      PSTAR = HLBNEW(I) * PRESS(K)
      CALL GETBMT(LM,PSTAR,PRS,KS)
        IF( LM.ge.2 .AND. LM.lt.KS ) THEN
C     PRINT * , ' CUBIC INTERPOLATION AT VARIABLE Q. '
      LP = LM + 1
      LB = LM - 1
      CALL GETABC(A,B,C,QLB(I,LB),QLB(I,LM),QLB(I,LP),
     1                  PRS(  LB),PRS(  LM),PRS(  LP))
      ALNPS = ALOG( REAL(PSTAR) )
      TMP(I,K) = A * ALNPS * ALNPS + B *ALNPS + C
        ELSE
C     PRINT * , ' LINEAR INTERPOLATION AT VARIABLE Q. '
        IF( LM .EQ. KS ) LM = KS - 1
      LP = LM + 1
      FACT = ALOG(REAL(PRS(LP))) - ALOG(REAL(PRS(LM)))
      FACT = (ALOG(REAL(PRS(LP))) - ALOG(REAL(PSTAR))) / FACT
C   avoid extrapolation below 'bounding-model's terrain
        IF( PSTAR.gt.PRS(LM) ) LP = LM
C   avoid extrapolation above 'bounding-model's top-level
        IF( PSTAR.lt.PRS(LP) ) FACT = 0.0
      TMP(I,K) = QLB(I,LP) + ( QLB(I,LM) - QLB(I,LP) ) * FACT
        ENDIF
 410  CONTINUE
      DO K=1,KS
        PRINT 2222,K,(QLB(I,K),I=837,844)
      ENDDO
      DO K=1,IKM
        PRINT 3333,K,(TMP(I,K),I=837,844)
      ENDDO
      DO 420 K=1, IKM
      DO 420 I=1,IJMLB
      IF( TMP(I,K).LT.0.0E0 ) TMP(I,K)=0.0E0
      QLB(I,K) = TMP(I,K)
 420  CONTINUE
C
      ENDIF
C
C
      RETURN
      END
