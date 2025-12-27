      SUBROUTINE GETHN(NGLB, IJMLB, CH)
C$$$  SUBROUTINE DOCUMENTATION BLOCK
C
C SUBROUTINE:  GETHN       GET GROUND PRESSURE FROM HYDROSTATIC.
C   AUTHOR: HANN-MING HENRY JUANG             DATE: 89-04-18
C
C
C ABSTRACT: GET GROUND PRESSURES IN DIFFERENT POINTS, SAY U POINT,
C   V POINT OR H POINT FOR NESTED GRID MODEL (NGM).
C
C PROGRAM HISTORY LOG:
C
C USAGE:
C   INPUT ARGUMENTS:
C
C     NGLB     -   WHICH GRID TO BE PERFORMED FOR LATERAL
C                  BOUNDARY DATA.
C     IJMLB    -   NUMBER OF GRID POINTS TO DO LATERAL
C                  BOUNDARY COMPUTATION.
C     CH       -   WHICH POINT TO BE PERFORMED IN NGM BOX.
C                  CH='U' AT U POINT.
C                  CH='V' AT V POINT.
C                  CH='H' AT H POINT.
C
C
C
C REMARKS:       NONE AT THIS TIME.
C
C ATTRIBUTES:
C   LANGUAGE:      CDC FORTRAN WITH CYBER 205 EXTENSIONS.
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
C
      COMMON    VBL( INVBL )
C
      PARAMETER(IJLB=2000,KS=64)
      COMMON /LTBL/ ULB(IJLB,KS),VLB(IJLB,KS),TLB(IJLB,KS)
     1             ,QLB(IJLB,KS),HLB(IJLB),HLBNEW(IJLB)
     2             ,SLYR(KS), ZSLB(IJLB),LBUO,LBVO,LBCH
     3             ,LSCRU(IJLB), LSCRV(IJLB), LSCRH(IJLB)
C
      DIMENSION ZSTAR(IJLB,3),
     1          TEMP1(IJLB),TEMP2(IJLB),TEMP3(IJLB),
     2          TEMT1(IJLB),TEMT2(IJLB),TEMT3(IJLB)
C
      CHARACTER*1 CH
C
C          GET GROUND HEIGHT (N=10) FOR NGLB GRID.
C
      DATA INP /0/
      IF( INP .EQ. 0 ) THEN
      INP = 1
C
      IMNG = IMG(NGLB)
      JMNG = JMG(NGLB)
      IJNG= IMNG * JMNG
      JADDZ= IADDRG(10, NGLB) - 1
C
      PRINT *,' * CHECK * NGLB IM JM IJM JADDZ IJMLB ', NGLB,IMNG,
     1       JMNG,IJNG,JADDZ,IJMLB
C
      CP = 1004.6E0
      XR = 287.05E0
      XG = 9.8062E0
      CKAPPA = XR / CP
      RKAPPA = 1.0E0 / CKAPPA
      CSUBPOG = CP / XG
      GOCSUBP = XG / CP
      GOR = XG / XR
C
C
C           COMPRESS THE GROUND HEIGHT TO LTBL POINT
C
C - U -
       DO 101 IU=1,LBUO
           NZ =  LSCRU(IU) + JADDZ
           ZSTARU = 0.5E0 * ( VBL(NZ) + VBL(NZ + IMNG) )
           J =(LSCRU(IU)-1)/IMNG + 1
           IF( J .EQ. JMNG ) THEN
            ZSTARU = VBL(NZ)
           ENDIF
           ZSTAR(IU,1) = ZSTARU   * CSUBPOG
 101   CONTINUE
C - V -
       DO 102 IV=1,LBVO
           NZ = LSCRV(IV) + JADDZ
           ZSTARV = 0.5E0 * ( VBL(NZ) + VBL(NZ + 1 ) )
           I = MOD( LSCRV(IV), IMNG )
           IF( I .EQ. 0 ) THEN
            ZSTARV = VBL(NZ)
           ENDIF
           ZSTAR(IV,2) = ZSTARV   * CSUBPOG
 102   CONTINUE
C - H -
       DO 103 IH=1,LBCH
           NZ = LSCRH(IH) + JADDZ
           ZSTAR(IH,3) = VBL(NZ)  * CSUBPOG
 103   CONTINUE
       PRINT *,' ZSTAR AT U ',(ZSTAR(I,1),I=1,LBUO,150)
       PRINT *,' ZSTAR AT V ',(ZSTAR(I,2),I=1,LBVO,150)
       PRINT *,' ZSTAR AT H ',(ZSTAR(I,3),I=1,LBCH,150)
C
       ENDIF
C ---------------------------------------------------------------
C
       IF( CH .EQ. 'U' ) NP = 1
       IF( CH .EQ. 'V' ) NP = 2
       IF( CH .EQ. 'H' ) NP = 3
C
        PRINT *,' HLB    ',(HLB(I),I=1,IJMLB,150)
        PRINT *,' ZSLB   ',(ZSLB(I),I=1,IJMLB,150)
C
       DO 150  I = 1 , IJMLB
C
C FIRST LAYER P AND T
        TEMP1(I)=HLB(I)*SLYR(1)
c       IF(MOD(I,150).EQ.0) print *,i,TEMP1(I),SLYR(1),HLB(I)
        TEMT1(I)=(TEMP1(I)*0.001E0)**CKAPPA
c       IF(MOD(I,150).EQ.0) print *,i,TEMP1(I),TEMT1(I),CKAPPA
        TEMT1(I)=TLB(I,1)*TEMT1(I)
C SECOND LAYER P AND T
        TEMP2(I)=HLB(I)*SLYR(2)
        TEMT2(I)=(TEMP2(I)*0.001E0)**CKAPPA
        TEMT2(I)=TLB(I,2)*TEMT2(I)
C RATIO OF 0.5*(T2-T1)/ALOG(P2/P1) ----- TEMP3
c       IF(MOD(I,150).EQ.0)print *,i,HLB(I),SLYR(1),SLYR(2),
c    1    TLB(I,1),TEMT1(I),TEMT2(I),TEMP1(I),TEMP2(I)
        TEMT3(I)=TEMP2(I)/TEMP1(I)
c       IF(MOD(I,150).EQ.0)print *,i,TEMP1(I),TEMP2(I),TEMT3(I)
        TEMP3(I)=LOG(TEMT3(I))
c       IF(MOD(I,150).EQ.0)print *,i,TEMP3(I),TEMT3(I)
        TEMP3(I)=(TEMT2(I)-TEMT1(I))/TEMP3(I)
c       IF(MOD(I,150).EQ.0)print *,i,TEMP3(I),TEMT2(I),TEMT1(I)
        TEMP3(I)=0.5E0*TEMP3(I)
c       IF(MOD(I,150).EQ.0)print *,i,TEMP3(I)
C TERM OF ALOG(PG/(P1*P2)) ------------- TEMP2
        TEMT3(I)=TEMP1(I)*TEMP2(I)
        TEMT3(I)=HLB(I)/TEMT3(I)
        TEMP2(I)=LOG(TEMT3(I))
C MEAN OF T1 AND T2 -------------------- TEMP1
        TEMP1(I)=0.5E0*(TEMT1(I)+TEMT2(I))
C G(ZG-ZS)/R --------------------------- TEMT1
        TEMT1(I)=GOR*(ZSLB(I)-ZSTAR(I,NP))
C
  150 CONTINUE
C
C -------------------------------------------------------------
C EXPERIMENT SHOW TWO OR THREE ITERATION GET DP < 0.05
C SO WE CAN DO ITERATION BY VECTORIZATION
C       DO 100 I=1,IJMLB
C       PS = HLB(I)
C150    TM = TEMP1(I)+TEMP3(I)*(LOG(PS)+TEMP2(I))
C       PS0 = HLB(I)*EXP(TEMT1(I)/TM)
CCXX    PRINT *, ' PS PS0 ',PS,PS0
C       DP = ABS(PS-PS0)
C       IF( DP .LE. 0.05 ) GO TO 200
C       PS = PS0
C       GO TO 150
C200    HLBNEW(I) = PS0
C100    CONTINUE
C --------------------------------------------------------------
C DO THREE ITERATIONS
C
      DO 250  I = 1, IJMLB
C
C 1ST
C TM
        TEMT2(I)=LOG(HLB(I))
c       IF(MOD(I,150).EQ.0) print *,i,TEMT2(I),TEMP1(I),
c    1    TEMP2(I),TEMP3(I)
        TEMT2(I)=TEMT2(I)+TEMP2(I)
c       IF(MOD(I,150).EQ.0) print *,i,TEMT2(I),TEMP2(I)
        TEMT2(I)=TEMP3(I)*TEMT2(I)
c       IF(MOD(I,150).EQ.0) print *,i,TEMT2(I),TEMP3(I)
        TEMT2(I)=TEMP1(I)+TEMT2(I)
c       IF(MOD(I,150).EQ.0) print *,i,TEMT2(I),TEMP1(I)
C PS
        TEMT2(I)=TEMT1(I)/TEMT2(I)
c       IF(MOD(I,150).EQ.0) print *,i,TEMT2(I)
        TEMT3(I)=EXP(TEMT2(I))
c       IF(MOD(I,150).EQ.0) print *,i,TEMT3(I)
        TEMT3(I)=HLB(I)*TEMT3(I)
c       IF(MOD(I,150).EQ.0) THEN
c        print *,'1st iter ',i,HLB(I),TEMT1(I),TEMT2(I),
c    1     TEMT3(I)
c       endif
C 2ND
C TM
        TEMT2(I)=LOG(TEMT3(I))
        TEMT2(I)=TEMT2(I)+TEMP2(I)
        TEMT2(I)=TEMP3(I)*TEMT2(I)
        TEMT2(I)=TEMP1(I)+TEMT2(I)
C PS
        TEMT2(I)=TEMT1(I)/TEMT2(I)
        TEMT3(I)=EXP(TEMT2(I))
        TEMT3(I)=HLB(I)*TEMT3(I)
c       IF(MOD(I,150).EQ.0) THEN
c        print *,'2nd iter ',i,HLB(I),TEMT1(I),TEMT2(I),
c    1     TEMT3(I)
c       endif
C 3RD
C TM
        TEMT2(I)=LOG(TEMT3(I))
        TEMT2(I)=TEMT2(I)+TEMP2(I)
        TEMT2(I)=TEMP3(I)*TEMT2(I)
        TEMT2(I)=TEMP1(I)+TEMT2(I)
C PS
        TEMT2(I)=TEMT1(I)/TEMT2(I)
        TEMT3(I)=EXP(TEMT2(I))
        HLBNEW(I)=HLB(I)*TEMT3(I)
c       IF(MOD(I,150).EQ.0) THEN
c        print *,'3nd iter ',i,HLB(I),TEMT1(I),TEMT2(I),
c    1     TEMT3(I),hlbnew(i)
c       endif
C
 250  CONTINUE
C
        PRINT *,' ZSTAR  ',(ZSTAR(I,NP),I=1,IJMLB,150)
        PRINT *,' HLBNEW ',(HLBNEW(I),I=1,IJMLB,150)
C
      RETURN
      END
