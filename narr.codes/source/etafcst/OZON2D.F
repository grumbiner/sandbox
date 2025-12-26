                             SUBROUTINE OZON2D
     1           (LK,POZN,XLAT,RSIN1,RCOS1,RCOS2,QO3)
C***********************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .     
C SUBPROGRAM:    OZON2D      COMPUTE OZONE IN MODEL COLUMNS
C   PRGRMMR: KATZ/CAMPANA    ORG: W/NP22     DATE: ??-??-??
C     
C ABSTRACT:
C     OZON2D COMPUTES THE OZONE MIXING RATIO IN EACH GRID BOX
C     OF COLUMNS WITHIN THE MODEL DOMAIN
C     
C PROGRAM HISTORY LOG:
C   ??-??-??  KATZ/KC    - ORIGINATOR
C   96-07-26  BLACK      - MODIFIED FOR ETA MODEL
C   98-10-28  BLACK      - MODIFIED FOR DISTRIBUTED MEMORY
C     
C USAGE: CALL OZON2D FROM SUBROUTINE RADTN
C   INPUT ARGUMENT LIST:
C     LK:    NUMBER OF LAYERS IN COLUMNS
C     XLAT:  GEODETIC LATITUDE OF GRID COLUMNS IN DEGREES
C     RSIN1: INFORMATION RELATING TO POSITION OF EARTH IN ITS ORBIT
C     RCOS1: INFORMATION RELATING TO POSITION OF EARTH IN ITS ORBIT
C     RCOS2: INFORMATION RELATING TO POSITION OF EARTH IN ITS ORBIT
C  
C   OUTPUT ARGUMENT LIST: 
C     QO3:  OZONE MIXING RATIO AT MIDLAYERS OF MODEL COLUMNS
C     
C   OUTPUT FILES:
C     NONE
C     
C   SUBPROGRAMS CALLED:
C  
C     UNIQUE:
C        NONE
C  
C     LIBRARY:
C        NONE
C  
C   COMMON BLOCKS: SEASO3
C   
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE : IBM SP
C$$$  
C----------------------------------------------------------------------
                           P A R A M E T E R
     1 (NL=81,NLP1=NL+1,LNGTH=37*NL,RTD=57.2957795)
C----------------------------------------------------------------------
      INCLUDE "parmeta"
      INCLUDE "mpp.h"
      INCLUDE "SEASO3.comm"
#include "sp.h"
C----------------------------------------------------------------------
                           D I M E N S I O N
     1 QO3(IDIM1:IDIM2,LK),POZN(IDIM1:IDIM2,LK),XLAT(IDIM1:IDIM2)
     2,QO3O3(IDIM1:IDIM2,NL),JJROW(IDIM1:IDIM2),TTHAN(IDIM1:IDIM2)
C----------------------------------------------------------------------
      DO I=MYIS,MYIE
        TH2=0.2*XLAT(I)
        JJROW(I)=19.001-TH2
        TTHAN(I)=(19-JJROW(I))-TH2
      ENDDO
C
C***  SEASONAL AND SPATIAL INTERPOLATION DONE BELOW.
C
      DO K=1,NL
      DO I=MYIS,MYIE
        DO3V=XDUO3N(JJROW(I),K)+RSIN1*XDO3N2(JJROW(I),K)
     1             +RCOS1*XDO3N3(JJROW(I),K)
     2             +RCOS2*XDO3N4(JJROW(I),K)
        DO3VP=XDUO3N(JJROW(I)+1,K)+RSIN1*XDO3N2(JJROW(I)+1,K)
     1              +RCOS1*XDO3N3(JJROW(I)+1,K)
     2              +RCOS2*XDO3N4(JJROW(I)+1,K)
C
C***  NOW LATITUDINAL INTERPOLATION
C***  AND CONVERT O3 INTO MASS MIXING RATIO (ORIG DATA MPY BY 1.E4)
C 
        QO3O3(I,K)=1.E-4*(DO3V+TTHAN(I)*(DO3VP-DO3V))
      ENDDO
      ENDDO
C***
C***  VERTICAL INTERPOLATION FOR EACH GRIDPOINT (LINEAR IN LN P)
C***
      NUMITR=0
      ILOG=NL
   20 CONTINUE
      ILOG=(ILOG+1)/2
        IF(ILOG.EQ.1)GO TO 25
        NUMITR=NUMITR+1
        GO TO 20
   25 CONTINUE
C
      DO 60 K=1,LK
C
      NHALF=(NL+1)/2
      DO I=MYIS,MYIE
        JJROW(I)=NHALF
      ENDDO
C
      DO 40 IT=1,NUMITR
      NHALF=(NHALF+1)/2
      DO I=MYIS,MYIE
        IF(POZN(I,K).LT.PRGFDL(JJROW(I)-1))THEN
          JJROW(I)=JJROW(I)-NHALF
        ELSEIF(POZN(I,K).GE.PRGFDL(JJROW(I)))THEN
          JJROW(I)=JJROW(I)+NHALF
        ENDIF
        JJROW(I)=MIN(JJROW(I),NL)
        JJROW(I)=MAX(JJROW(I),2)
      ENDDO
   40 CONTINUE
C
      DO 50 I=MYIS,MYIE
      IF(POZN(I,K).LT.PRGFDL(1))THEN
        QO3(I,K)=QO3O3(I,1)
      ELSE IF(POZN(I,K).GT.PRGFDL(NL))THEN
        QO3(I,K)=QO3O3(I,NL)
      ELSE
        APLO=ALOG(PRGFDL(JJROW(I)-1))
        APHI=ALOG(PRGFDL(JJROW(I)))
        QO3(I,K)=QO3O3(I,JJROW(I))+(ALOG(POZN(I,K))-APHI)/
     1             (APLO-APHI)*
     2             (QO3O3(I,JJROW(I)-1)-QO3O3(I,JJROW(I)))
      ENDIF
   50 CONTINUE
C
   60 CONTINUE
C----------------------------------------------------------------------
      RETURN
      END
