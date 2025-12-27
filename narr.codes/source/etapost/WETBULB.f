      SUBROUTINE WETBULB(T,Q,PDSL,HTM,KARR,TWET)
C
C     FILE: WETBULB.f
C     WRITTEN: 10 SEPTEMBER 1993, MICHAEL BALDWIN
C     REVISIONS: 
C     CONVERSION TO 2-D: 12 JUNE 1998 (T BLACK)
C     MPI VERSION: 04 Jan 2000 ( JIM TUCCILLO )
C
C-----------------------------------------------------------------------
C     ROUTINE TO COMPUTE WET BULB TEMPERATURES USING THE LOOK UP TABLE
C     APPROACH THAT IS USED IN CUCNVC
C  
C     FOR A GIVEN POINT K AND LAYER L:
C      THETA E IS COMPUTED FROM THETA AND Q BY LIFTING THE PARCEL TO
C      ITS SATURATION POINT.
C      THEN THE WET BULB TEMPERATURE IS FOUND BY FOLLOWING THE THETA E
C      DOWN TO THE ORIGINAL PRESSURE LEVEL (USING SUBROUTINE TTBLEX).     
C
C   
C
      LOGICAL   UNIS,UNIL,OCT90
      INCLUDE "parmeta"
      INCLUDE "parm.tbl"
      PARAMETER(IMJM=IM*JM-JM/2,LP1=LM+1)
      INCLUDE "cuparm"
      INCLUDE "PHYS.comm"
      INCLUDE "CTLBLK.comm"
C-----------------------------------------------------------------------
C  LIST OF VARIABLES NEEDED
C    PARAMETERS:
C      INCLUDED IN "cuparm" AND "parm.tbl"
C    INPUT:
C      T,Q,HTM (3-D),PDSL,KARR (2-D),KNUM
C    OUTPUT: 
C      TWET (3-D)
C    INTERNAL:
C      THESP,KLRES,KHRES,QQ,PP,IPTB,ITHTB
C      PLUS SOME INCLUDED IN COMMON BLOCK "PHYS"
C    SUBROUTINES CALLED:
C      TTBLEX
C
      DIMENSION THESP(IM,JM),TWET(IM,JM,LM),T(IM,JM,LM),Q(IM,JM,LM)
      DIMENSION PDSL(IM,JM),HTM(IM,JM,LM),KARR(IM,JM)
                             D I M E N S I O N
     1 KLRES(IM,JM),KHRES(IM,JM)
     2,QQ(IM,JM),PP(IM,JM)
     3,IPTB(IM,JM),ITHTB(IM,JM)
C
C--------------COMPUTE WET BULB TEMPERATURES----------------------------
!$omp  parallel do
!$omp& private(apebtk,apespk,bqk,bqs00k,bqs10k,iq,iqtbk,it,ittb1,ittbk,
!$omp&         karr,khres,klres,knumh,knuml,p00k,p01k,p10k,p11k,ppk,
!$omp&         presk,qbtk,qqk,sqk,sqs00k,sqs10k,tbtk,thesp,tpspk,
!$omp&         tqk,tthbtk,tthk)
C-----------------------------------------------------------------------
                             DO 300 L=1,LM
      DO 125 J=JSTA,JEND
      DO 125 I=1,IM
      IF(KARR(I,J).GT.0)THEN
        IF (HTM(I,J,L).LT.1.0) THEN
          THESP(I,J)=273.15
          GOTO 125
        ENDIF
        TBTK  =T(I,J,L)
        QBTK  =Q(I,J,L)
        PRESK=PDSL(I,J)*AETA(L)+PT
        APEBTK=(H10E5/PRESK)**CAPA
        IF(QBTK.LT.EPSQ) QBTK=HTM(I,J,L)*EPSQ
C--------------SCALING POTENTIAL TEMPERATURE & TABLE INDEX--------------
        TTHBTK  =TBTK*APEBTK
        TTHK    =(TTHBTK-THL)*RDTH
        QQK     =TTHK-AINT(TTHK)
        ITTB1   =INT(TTHK)+1
C--------------KEEPING INDICES WITHIN THE TABLE-------------------------
        IF(ITTB1.LT.1) THEN
          ITTB1  =1
          QQK    =D00
        ENDIF
C
        IF(ITTB1.GE.JTB) THEN
        ITTB1  =JTB-1
        QQK    =D00
        ENDIF
C--------------BASE AND SCALING FACTOR FOR SPEC. HUMIDITY---------------
        ITTBK=ITTB1
        BQS00K=QS0(ITTBK)
        SQS00K=SQS(ITTBK)
        BQS10K=QS0(ITTBK+1)
        SQS10K=SQS(ITTBK+1)
C--------------SCALING SPEC. HUMIDITY & TABLE INDEX---------------------
        BQK=(BQS10K-BQS00K)*QQK+BQS00K
        SQK=(SQS10K-SQS00K)*QQK+SQS00K
        TQK=(QBTK-BQK)/SQK*RDQ
        PPK=TQK-AINT(TQK)
        IQTBK=INT(TQK)+1
C--------------KEEPING INDICES WITHIN THE TABLE-------------------------
        IF(IQTBK.LT.1) THEN
          IQTBK =1
          PPK   =D00
        ENDIF
C
        IF(IQTBK.GE.ITB) THEN
          IQTBK=ITB-1
          PPK  =D00
        ENDIF
C--------------SATURATION PRESSURE AT FOUR SURROUNDING TABLE PTS.-------
        IQ=IQTBK
        IT=ITTB1
        P00K=PTBL(IQ  ,IT  )
        P10K=PTBL(IQ+1,IT  )
        P01K=PTBL(IQ  ,IT+1)
        P11K=PTBL(IQ+1,IT+1)
C--------------SATURATION POINT VARIABLES AT THE BOTTOM-----------------
        TPSPK=P00K+(P10K-P00K)*PPK+(P01K-P00K)*QQK
     1          +(P00K-P10K-P01K+P11K)*PPK*QQK
        APESPK=(H10E5/TPSPK)**CAPA
        THESP(I,J)=TTHBTK*EXP(ELOCP*QBTK*APESPK/TTHBTK)
      ENDIF
  125 CONTINUE
C--------------SCALING PRESSURE & TT TABLE INDEX------------------------
      KNUML=0
      KNUMH=0
C
      DO 280 J=JSTA,JEND
      DO 280 I=1,IM
      KLRES(I,J)=0
      KHRES(I,J)=0
C
      IF(KARR(I,J).GT.0)THEN
        PRESK=PDSL(I,J)*AETA(L)+PT
C
        IF(PRESK.LT.PLQ)THEN
          KNUML=KNUML+1
          KLRES(I,J)=1
        ELSE
          KNUMH=KNUMH+1
          KHRES(I,J)=1
        ENDIF
      ENDIF
 280  CONTINUE
C***
C***  COMPUTE PARCEL TEMPERATURE ALONG MOIST ADIABAT FOR PRESSURE<PL
C**
      IF(KNUML.GT.0)THEN
        CALL TTBLEX(TWET(1,1,L),TTBL,ITB,JTB,KLRES,PDSL,AETA(L)
     1,             HTM(1,1,L),PT,PL,QQ,PP,RDP,THE0,STHE,RDTHE,THESP
     2,             IPTB,ITHTB)
      ENDIF
C***
C***  COMPUTE PARCEL TEMPERATURE ALONG MOIST ADIABAT FOR PRESSURE>PL
C**
      IF(KNUMH.GT.0)THEN
        CALL TTBLEX(TWET(1,1,L),TTBLQ,ITBQ,JTBQ,KHRES,PDSL,AETA(L)
     1,             HTM(1,1,L),PT,PLQ,QQ,PP,RDPQ,THE0Q,STHEQ,RDTHEQ
     2,             THESP,IPTB,ITHTB)
      ENDIF
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
  300 CONTINUE
      RETURN
      END
