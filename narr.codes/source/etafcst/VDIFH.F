C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      SUBROUTINE VDIFH(LMHK,KTM,DTQ2,THZ0,QZ0,AKHS,CT,CKLQ
     &,                T,Q,AKH,APE,Z)
C     ******************************************************************
C     *                                                                *
C     *            VERTICAL DIFFUSION OF MASS VARIABLES                *
C     *                                                                *
C     ******************************************************************
C-----------------------------------------------------------------------
      INCLUDE "parmeta"
      INCLUDE "mpp.h"
#include "sp.h"
C-----------------------------------------------------------------------
                             P A R A M E T E R
     &(LP1=LM+1,LM1=LM-1)
C-----------------------------------------------------------------------
                             D I M E N S I O N
     & T     (LM),Q     (LM)
                             D I M E N S I O N
     & AKH   (LM1)
     &,APE   (LM)
     &,Z     (LP1)
                             D I M E N S I O N
     & CM    (LM1),CR    (LM1),RST   (LM1),RSQ   (LM1)
     &,DTOZ  (LM1),AKCT  (LM1)
C-----------------------------------------------------------------------
C***********************************************************************
      DTDIF=DTQ2/FLOAT(KTM)
      LMHM=LMHK-1
      LMHP=LMHK+1
C-----------------------------------------------------------------------
          DO 100 L=1,LMHM
      DTOZ(L)=DTDIF/(Z(L)-Z(L+1))
      CR(L)=-DTOZ(L)*AKH(L)
      AKCT(L)=AKH(L)*(Z(L)-Z(L+2))*0.5*CT
 100  CONTINUE
C
      CM(1)=DTOZ(1)*AKH(1)+1.
C-----------------------------------------------------------------------
                             DO 300 KT=1,KTM
C-----------------------------------------------------------------------
      RST(1)=-AKCT(1)*DTOZ(1)+T(1)*APE(1)
      RSQ(1)=Q(1)
C-----------------------------------------------------------------------
          DO 110 L=2,LMHM
      DTOZL=DTOZ(L)
      CF=-DTOZL*AKH(L-1)/CM(L-1)
      CM(L)=-CR(L-1)*CF+(AKH(L-1)+AKH(L))*DTOZL+1.
      RST(L)=-RST(L-1)*CF+(AKCT(L-1)-AKCT(L))*DTOZL+T(L)*APE(L)
      RSQ(L)=-RSQ(L-1)*CF+Q(L)
 110  CONTINUE
C-----------------------------------------------------------------------
      DTOZS=DTDIF/(Z(LMHK)-Z(LMHP))
      AKHH=AKH(LMHM)
C
      CF=-DTOZS*AKHH/CM(LMHM)
      AKQS=AKHS*CKLQ
C
      CMB=CR(LMHM)*CF
      CMTB=-CMB+(AKHH+AKHS)*DTOZS+1.
      CMQB=-CMB+(AKHH+AKQS)*DTOZS+1.
C
      RSTB=-RST(LMHM)*CF+(AKCT(LMHM)-AKHS*CT)*DTOZS
     &                  +T(LMHK)*APE(LMHK)
      RSQB=-RSQ(LMHM)*CF+Q(LMHK)
C-----------------------------------------------------------------------
      T(LMHK)=(DTOZS*AKHS*THZ0+RSTB)/(APE(LMHK)*CMTB)
      Q(LMHK)=(DTOZS*AKQS*QZ0 +RSQB)/CMQB
C-----------------------------------------------------------------------
          DO 120 L=LMHM,1,-1
      RCML=1./CM(L)
      T(L)=(-CR(L)*T(L+1)*APE(L+1)+RST(L))*RCML/APE(L)
      Q(L)=(-CR(L)*Q(L+1)         +RSQ(L))*RCML
 120  CONTINUE
C-----------------------------------------------------------------------
 300                         CONTINUE
C-----------------------------------------------------------------------
                             RETURN
                             END

