      SUBROUTINE BNDPTS(ZLATB,ZLONB,NHPT,
     &                 ULATB,ULONB,NUPT,
     &                 VLATB,VLONB,NVPT)
C
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM: BNDPTS        FINDS LAT/LONG OF ETA BOUNDARY POINTS
C
C   PRGMMR: E. ROGERS       ORG: W/NMC22    DATE: 90-06-19
C
C ABSTRACT: COMPUTES THE EARTH LATITUDE AND LONGITUDE OF ETA GRID
C   POINTS (BOTH H AND V POINTS) AND STRIPS OFF THE BOUNDARY POINTS
C
C PROGRAM HISTORY LOG:
C   90-06-19  E.ROGERS
C
C USAGE:    CALL BNDPTS(ZLATB,ZLONB,NHPT,ULATB,ULONB,NUPT)
C   INPUT ARGUMENT LIST:
C     NONE
C
C   OUTPUT ARGUMENT LIST:
C     ZLATB   -LATITUDE OF H BOUNDARY POINTS IN RADIANS (NEG=S)
C     ZLONB   -LONGITUDE OF H BOUNDARY POINTS IN RADIANS (E)
C     NHPT    -NUMBER OF H BOUNDARY POINTS
C     ULATB   -LATITUDE OF U BOUNDARY POINTS IN RADIANS (NEG=S)
C     ULONB   -LONGITUDE OF U BOUNDARY POINTS IN RADIANS (E)
C     NUPT    -NUMBER OF U BOUNDARY POINTS
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  CRAY
C
        INCLUDE "parmeta.res"
                                                      P A R A M E T E R
     & (IMJM=IM*JM-JM/2,JMP1=JM+1,IMM1=IM-1,IMJMM1=IMJM-1
     &, KHL00=1,KHH00=IM*JM-JM/2
     &, KNE=IM,KNW=IM-1,KSW=-IM,KSE=-IM+1
     &, KSL=IMJM+KSE
     &, KSLM1=KSL-1,KB=JM+2*IM-3)
C-----------------------------------------------------------------------
                                                     D I M E N S I O N
     & KHL0  (JM),KHH0  (JM), KVL0 (JM), KVH0 (JM)
C
     &,GLATH (IMJM),GLONH (IMJM),GLATV (IMJM),GLONV (IMJM)
C
     &,HLAT (IMJM),HLON (IMJM),VLAT (IMJM),VLON (IMJM)
     &,ZLATB(KB), ZLONB(KB), ULATB(KB), ULONB(KB)
C-----------------------------------------------------------------------
                                                           D A T A
     &  LIST/03/
C--------------HORIZONTAL GRID CONSTANTS-STANDARD RESOLUTION----------
         include "griddef"
c                                                          D A T A
c    & TLM0D/-97.0/,TPH0D/41.0/,WBD/-35./,SBD/-25./
c    &,DLMD/.212121212/,DPHD/.2/
C--------------UNIVERSAL CONSTANTS--------------------------------------
                                                           D A T A
     & PI/3.141592654/
C-----------------------------------------------------------------------
 1000 FORMAT(' K=',I4,' GLAT=',E12.5,' GLON=',E12.5,' ELAT=',E12.5
     &,' ELON=',E12.5,' WLON=',E12.5)
C----------------------------------------------------------------------
C----------------------------------------------------------------------
      DTR=PI/180.
      TPH0=TPH0D*DTR
      WB=WBD*DTR
      SB=SBD*DTR
      DLM=DLMD*DTR
      DPH=DPHD*DTR
      TDLM=DLM+DLM
      TDPH=DPH+DPH
C
      STPH0=SIN(TPH0)
      CTPH0=COS(TPH0)
C
      DO 100 J=1,JM
      KHL0(J)=IM*(J-1)-(J-1)/2+1
      KHH0(J)=IM*J-J / 2
      IF(MOD(J,2).EQ.0)THEN
        KVL0(J)=KHL0(J)-1
        KVH0(J)=KHH0(J)
      ELSE
        KVL0(J)=KHL0(J)
        KVH0(J)=KHH0(J)-1
      ENDIF
C       WRITE(LIST,20) J,KHL0(J),KHH0(J),KVL0(J),KVH0(J)
C  20 FORMAT(2X,5I10)
  100 CONTINUE
C-----------------------------------------------------------------------
C---COMPUTE GEOGRAPHIC LAT AND LONG OF ETA GRID POINTS (H & V POINTS)---
C-----------------------------------------------------------------------
      TPHH=SB-DPH
      DO 200 J=1,JM
      KHL=KHL0(J)
      KHH=KHH0(J)
      KVL=KVL0(J)
      KVH=KVH0(J)
C
      TLMH=WB-TDLM+MOD(J+1,2)*DLM
      TPHH=TPHH+DPH
      TLMV=WB-TDLM+MOD(J,2)*DLM
      TPHV=TPHH
      STPH=SIN(TPHH)
      CTPH=COS(TPHH)
      STPV=SIN(TPHV)
      CTPV=COS(TPHV)
C----------------------------------------------------------------------
C---------- COMPUTE EARTH LATITUDE/LONGITUDE OF H POINTS --------------
C----------------------------------------------------------------------
      DO 201 K=KHL,KHH
      TLMH=TLMH+TDLM
      SPHH=CTPH0*STPH+STPH0*CTPH*COS(TLMH)
      GLATH(K)=ASIN(SPHH)
      CLMH=CTPH*COS(TLMH)/(COS(GLATH(K))*CTPH0)
     1           -TAN(GLATH(K))*TAN(TPH0)
      IF(CLMH.GT.1.)CLMH=1.
      FACTH=1.
      IF(TLMH.GT.0.)FACTH=-1.
      GLONH(K)=-TLM0D*DTR+FACTH*ACOS(CLMH)
C          IF(K.EQ.1.OR.MOD(K,50).EQ.0)THEN
C           WRITE(LIST,99995)J,K,GLATH(K),GLONH(K)
C9995       FORMAT(2X,2(I6,1X),2(E12.5,1X))
C          ENDIF
C
C    CONVERT INTO DEGREES AND EAST LONGITUDE
C
      HLAT(K)=GLATH(K)/DTR
      HLON(K)=360.-GLONH(K)/DTR
      IF(HLON(K).GT.360.)HLON(K)=HLON(K)-360.
  201 CONTINUE
C----------------------------------------------------------------------
C---------- COMPUTE EARTH LATITUDE/LONGITUDE OF V POINTS --------------
C----------------------------------------------------------------------
      DO 202 K=KVL,KVH
      TLMV=TLMV+TDLM
      SPHV=CTPH0*STPV+STPH0*CTPV*COS(TLMV)
      GLATV(K)=ASIN(SPHV)
      CLMV=CTPV*COS(TLMV)/(COS(GLATV(K))*CTPH0)
     1         -TAN(GLATV(K))*TAN(TPH0)
      IF(CLMV.GT.1.)CLMV=1.
      FACTV=1.
      IF(TLMV.GT.0.)FACTV=-1.
      GLONV(K)=-TLM0D*DTR+FACTV*ACOS(CLMV)
C          IF(K.EQ.1.OR.MOD(K,50).EQ.0)THEN
C           WRITE(LIST,99995)J,K,GLATV(K),GLONV(K)
C          ENDIF
C
C    CONVERT INTO DEGREES AND EAST LONGITUDE
C
      VLAT(K)=GLATV(K)/DTR
      VLON(K)=360.-GLONV(K)/DTR
      IF(VLON(K).GT.360.)VLON(K)=VLON(K)-360.
  202 CONTINUE
  200 CONTINUE
C
C     DO 210 K=KHL00,KHH00
C      IF(K.LT.100.OR.MOD(K,100).EQ.0)THEN
C        WRITE(LIST,88888)K,HLAT(K),HLON(K),VLAT(K),VLON(K)
C8888    FORMAT(2X,I5,1X,4(E12.5,1X))
C      ENDIF
C 210 CONTINUE
C
C------------- THE SEPARATION OF THE BOUNDARY VALUES -------------------
C
C       SOUTH BOUNDARY-H POINTS
C
55545 FORMAT(2X,' START=',I6,' END = ',I6)
55546 FORMAT(2X,' START=',I6,' END = ',I6, ' INC = ',I6)
      N=1
      WRITE(LIST,55545)N,IM
      DO 541 K=1,IM
      ZLATB(N)=HLAT(K)
      ZLONB(N)=HLON(K)
      N=N+1
 541  CONTINUE
C
C       NORTH BOUNDARY-H POINTS
C
      WRITE(LIST,55545)KSL,IMJM
      DO 543 K=KSL,IMJM
      ZLATB(N)=HLAT(K)
      ZLONB(N)=HLON(K)
      N=N+1
 543  CONTINUE
C
C       WEST BOUNDARY-H POINTS
C
      KHSTWB=2*KNE
      KHENWB=KSL+KSW+KSE
      KHICWB=KNW+KNE
      WRITE(LIST,55546)KHSTWB,KHENWB,KHICWB
      DO 545 K=KHSTWB,KHENWB,KHICWB
      ZLATB(N)=HLAT(K)
      ZLONB(N)=HLON(K)
      N=N+1
 545  CONTINUE
C
C       EAST BOUNDARY-H POINTS
C
      KHSTEB=(2*KNE)+KNW
      KHENEB=IMJM+KSW+KSE
      KHICEB=KNE+KNW
      WRITE(LIST,55546)KHSTEB,KHENEB,KHICEB
      DO 547 K=KHSTEB,KHENEB,KHICEB
      ZLATB(N)=HLAT(K)
      ZLONB(N)=HLON(K)
       N=N+1
 547  CONTINUE
      NHPT=N-1
C
C       SOUTH BOUNDARY-V POINTS
C
      N=1
      WRITE(LIST,55545)N,IMM1
      DO 549 K=1,IMM1
      ULATB(N)=VLAT(K)
      ULONB(N)=VLON(K)
C           VLATB(N)=VLAT(K)
C           VLONB(N)=VLON(K)
      N=N+1
 549  CONTINUE
C
C       NORTH BOUNDARY-V POINTS
C
      WRITE(LIST,55545)KSL,IMJMM1
      DO 551 K=KSL,IMJMM1
      ULATB(N)=VLAT(K)
      ULONB(N)=VLON(K)
C           VLATB(N)=VLAT(K)
C           VLONB(N)=VLON(K)
 550  CONTINUE
      N=N+1
 551  CONTINUE
C
C       WEST BOUNDARY-V POINTS
C
      KVSTWB=IM
      KVENWB=KSL+KSW
      KVICWB=KNE+KNW
      WRITE(LIST,55546)KVSTWB,KVENWB,KVICWB
      DO 553 K=KVSTWB,KVENWB,KVICWB
      ULATB(N)=VLAT(K)
      ULONB(N)=VLON(K)
C           VLATB(N)=VLAT(K)
C           VLONB(N)=VLON(K)
      N=N+1
 553  CONTINUE
C
C       EAST BOUNDARY-V POINTS
C
      KVSTEB=KNE+KNW
      KVENEB=KSLM1
      KVICEB=KNE+KNW
      WRITE(LIST,55546)KVSTEB,KVENEB,KVICEB
      DO 555 K=KVSTEB,KVENEB,KVICEB
      ULATB(N)=VLAT(K)
      ULONB(N)=VLON(K)
C           VLATB(N)=VLAT(K)
C           VLONB(N)=VLON(K)
      N=N+1
 555  CONTINUE
      NUPT=N-1
C     NVPT=NUPT
      WRITE(LIST,77776)NHPT,NUPT
77776 FORMAT(2X,3(I6,1X))
C
C     DO 77777 N=1,KB
C      WRITE(LIST,77778)N,ZLATB(N),ZLONB(N),ULATB(N),ULONB(N)
C7778  FORMAT(2X,I6,4(E12.5,1X))
C7777 CONTINUE
      RETURN
      END
