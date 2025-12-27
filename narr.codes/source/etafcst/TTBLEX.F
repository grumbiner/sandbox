      SUBROUTINE TTBLEX(TREF,TTBL,ITB,JTB,KNUM,IARR,JARR
     1,                 PDSL,AETAL,HTML,PT,PL,QQ,PP,RDP,THE0
     2,                 STHE,RDTHE,THESP,IPTB,ITHTB)
C     ******************************************************************
C     *                                                                *
C     *         EXTRACT TEMPERATURE OF THE MOIST ADIABAT FROM          *
C     *                    THE APPROPRIATE TTBL                        *
C     *                                                                *
C     ******************************************************************
C----------------------------------------------------------------------
      INCLUDE "parmeta"
      INCLUDE "mpp.h"
#include "sp.h"
C----------------------------------------------------------------------
      parameter(IMJM_LOC=IDIM2*JDIM2)
C----------------------------------------------------------------------
                             P A R A M E T E R
     & (IMJM=IM*JM-JM/2,IMXJM=IM*JM)
C----------------------------------------------------------------------
                             D I M E N S I O N
     1 TREF(IDIM1:IDIM2,JDIM1:JDIM2)
     2,TTBL(JTB,ITB),IARR(IMJM_LOC),JARR(IMJM_LOC)
     3,PDSL(IDIM1:IDIM2,JDIM1:JDIM2)
     4,QQ(IDIM1:IDIM2,JDIM1:JDIM2)
     5,PP(IDIM1:IDIM2,JDIM1:JDIM2),THE0(ITB)
     6,STHE(ITB),THESP(IDIM1:IDIM2,JDIM1:JDIM2)
     7,IPTB(IDIM1:IDIM2,JDIM1:JDIM2)
     8,ITHTB(IDIM1:IDIM2,JDIM1:JDIM2)
     9,HTML(IDIM1:IDIM2,JDIM1:JDIM2)
C-----------------------------------------------------------------------
      DO 500 KK=1,KNUM
C--------------SCALING PRESSURE & TT TABLE INDEX------------------------
      I=IARR(KK)
      J=JARR(KK)
      PK=PDSL(I,J)*AETAL+PT
      TPK=(PK-PL)*RDP
      QQ(I,J)=TPK-AINT(TPK)
      IPTB(I,J)=INT(TPK)+1
C--------------KEEPING INDICES WITHIN THE TABLE-------------------------
      IF(IPTB(I,J).LT.1)THEN
        IPTB(I,J)=1
        QQ(I,J)=0.
      ENDIF
      IF(IPTB(I,J).GE.ITB)THEN
        IPTB(I,J)=ITB-1
        QQ(I,J)=0.
      ENDIF
C--------------BASE AND SCALING FACTOR FOR THE--------------------------
      IPTBK=IPTB(I,J)
      BTHE00K=THE0(IPTBK)
      STHE00K=STHE(IPTBK)
      BTHE10K=THE0(IPTBK+1)
      STHE10K=STHE(IPTBK+1)
C--------------SCALING THE & TT TABLE INDEX-----------------------------
      BTHK=(BTHE10K-BTHE00K)*QQ(I,J)+BTHE00K
      STHK=(STHE10K-STHE00K)*QQ(I,J)+STHE00K
      TTHK=(THESP(I,J)-BTHK)/STHK*RDTHE
      PP(I,J)=TTHK-AINT(TTHK)
      ITHTB(I,J)=INT(TTHK)+1
C--------------KEEPING INDICES WITHIN THE TABLE-------------------------
      IF(ITHTB(I,J).LT.1)THEN
        ITHTB(I,J)=1
        PP(I,J)=0.
      ENDIF
      IF(ITHTB(I,J).GE.JTB)THEN
        ITHTB(I,J)=JTB-1
        PP(I,J)=0.
      ENDIF
C--------------TEMPERATURE AT FOUR SURROUNDING TT TABLE PTS.------------
      ITH=ITHTB(I,J)
      IP=IPTB(I,J)
      T00K=TTBL(ITH  ,IP  )
      T10K=TTBL(ITH+1,IP  )
      T01K=TTBL(ITH  ,IP+1)
      T11K=TTBL(ITH+1,IP+1)
C--------------PARCEL TEMPERATURE-------------------------------------
      TREF(I,J)=(T00K+(T10K-T00K)*PP(I,J)+(T01K-T00K)*QQ(I,J)
     1         +(T00K-T10K-T01K+T11K)*PP(I,J)*QQ(I,J))*HTML(I,J)
  500 CONTINUE
C
      RETURN
      END
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
