      SUBROUTINE TTBLEX(TREF,TTBL,ITB,JTB,KARR,PDSL,AETAL,HTML
     1,                 PT,PL,QQ,PP,RDP,THE0,STHE,RDTHE,THESP
     2,                 IPTB,ITHTB)
CFPP$ NOCONCUR R
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .
C SUBPROGRAM:    TTBLEX      COMPUTES T ALONG A MOIST ADIABAT
C   PRGRMMR: BLACK           ORG: W/NP2      DATE: ??-??-??
C
C ABSTRACT:
C     THIS ROUTINE COMPUTES THE TEMPERATURE ALONG A MOIST
C     ADIABAT GIVEN THE SATURATION POTENTIAL TEMPERATURE
C     AND THE PRESSURE
C   .
C
C PROGRAM HISTORY LOG:
C   ??-??-??  T BLACK - ORIGINATOR
C   98-06-12  T BLACK - CONVERSION FROM 1-D TO 2-D
C   00-01-04  JIM TUCCILLO - MPI VERSION
C
C   OUTPUT FILES:
C     NONE
C
C   SUBPROGRAMS CALLED:
C     UTILITIES:
C       NONE
C
C   ATTRIBUTES:
C     LANGUAGE: FORTRAN
C----------------------------------------------------------------------
      INCLUDE "parmeta"
      INCLUDE "CTLBLK.comm"
C----------------------------------------------------------------------
                             D I M E N S I O N
     1 TREF(IM,JM),TTBL(JTB,ITB),KARR(IM,JM),PDSL(IM,JM),HTML(IM,JM)
     2,QQ(IM,JM),PP(IM,JM),THE0(ITB),STHE(ITB),THESP(IM,JM)
     3,IPTB(IM,JM),ITHTB(IM,JM)
C-----------------------------------------------------------------------
!$omp  parallel do
!$omp& private(bthe00k,bthe10k,bthk,ip,iptbk,ith,pk,sthe00k,sthe10k,
!$omp&         sthk,t00k,t01k,t10k,t11k,tpk,tthk)
      DO 500 J=JSTA_M,JEND_M
      DO 500 I=2,IM-1
      IF(KARR(I,J).GT.0)THEN
C--------------SCALING PRESSURE & TT TABLE INDEX------------------------
        PK=PDSL(I,J)*AETAL+PT
        TPK=(PK-PL)*RDP
        QQ(I,J)=TPK-AINT(TPK)
        IPTB(I,J)=INT(TPK)+1
C--------------KEEPING INDICES WITHIN THE TABLE-------------------------
        IF(IPTB(I,J).LT.1)THEN
          IPTB(I,J)=1
          QQ(I,J)=0.
        ENDIF
C
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
C
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
     1           +(T00K-T10K-T01K+T11K)*PP(I,J)*QQ(I,J))*HTML(I,J)
      ENDIF
  500 CONTINUE
C
      RETURN
      END
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
