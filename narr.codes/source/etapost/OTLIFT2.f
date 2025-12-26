      SUBROUTINE OTLIFT2(T500,SLINDX)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .     
C SUBPROGRAM:    OTLIFT2     COMPUTES SFC TO 500MB LIFTED INDEX
C   PRGRMMR: TREADON         ORG: W/NP2      DATE: 93-03-10       
C     
C ABSTRACT:
C     THIS ROUTINE COMPUTES A SURFACE TO 500MB LIFTED INDEX.
C     THE LIFTED PARCEL IS FROM THE FIRST ATMOSPHERIC ETA 
C     LAYER (IE, THE ETA LAYER CLOSEST TO THE MODEL GROUND).
C     THE LIFTED INDEX IS THE DIFFERENCE BETWEEN THIS PARCEL'S
C     TEMPERATURE AT 500MB AND THE AMBIENT 500MB TEMPERATURE.
C   .     
C     
C PROGRAM HISTORY LOG:
C   ??-??-??  ??? - SUBROUTINE OTLIFT IN ETA MODEL.
C   93-03-10  RUSS TREADON - ADAPTED OTLIFT FOR USE WITH NEW POST. 
C   98-06-18  T BLACK      - CONVERSION FROM 1-D TO 2-D
C   00-01-04  JIM TUCCILLO - MPI VERSION
C     
C USAGE:    CALL OTLIFT2(T500,SLINDX)
C   INPUT ARGUMENT LIST:
C     T500     - 500MB AMBIENT TEMPERATURE.
C
C   OUTPUT ARGUMENT LIST: 
C     SLINDX   - LIFTED INDEX.
C     
C   OUTPUT FILES:
C     NONE
C     
C   SUBPROGRAMS CALLED:
C     UTILITIES:
C       NONE
C     LIBRARY:
C       COMMON   - CTLBLK
C                  LOOPS
C                  MASKS
C                  PHYS
C                  VRBLS
C     
C   ATTRIBUTES:
C     LANGUAGE: FORTRAN
C     MACHINE : CRAY C-90
C$$$  
C     
C     SET LOCAL PARAMETERS.
      PARAMETER
     & (D00=0.E0,H10E5=100000.E0,H5E4=5.E4,CAPA=0.28589641
     &, D8202=.820231E0)
      PARAMETER
     & (ELIVW=2.72E6,CP=1004.6E0,ELOCP=ELIVW/CP)
C     
C     INCLUDE GLOBAL PARAMETERS.
      INCLUDE "parmeta"
      INCLUDE "parm.tbl"
C     
C     SET DEPENDENT PARAMETERS.
      PARAMETER
     & (JAM=6+2*(JM-10),LP1=LM+1)
C     
C     DECLARE VARIABLES.
      LOGICAL RUN,FIRST,RESTRT,SIGMA
      DIMENSION
     & ITTB  (IM,JM),IQTB  (IM,JM),IPTB  (IM,JM),ITHTB (IM,JM)
     &,PDSL  (IM,JM),TBT   (IM,JM),QBT   (IM,JM)
     &,APEBT (IM,JM),TTHBT (IM,JM),TTH   (IM,JM),PP    (IM,JM)
     &,BQS00 (IM,JM),SQS00 (IM,JM),BQS10 (IM,JM),SQS10 (IM,JM)
     &,BQ    (IM,JM),SQ    (IM,JM),TQ    (IM,JM),QQ    (IM,JM)
     &,P00   (IM,JM),P10   (IM,JM),P01   (IM,JM),P11   (IM,JM)
     &,TPSP  (IM,JM),APESP (IM,JM),TTHES (IM,JM)
     &,PSP   (IM,JM),THBT  (IM,JM),THESP (IM,JM)
     &,P     (IM,JM),TP    (IM,JM),BTH   (IM,JM),STH   (IM,JM)
     &,BTHE00(IM,JM),STHE00(IM,JM),BTHE10(IM,JM),STHE10(IM,JM)
     &,T00   (IM,JM),T10   (IM,JM),T01   (IM,JM),T11   (IM,JM)
     &,PARTMP(IM,JM),T500  (IM,JM),SLINDX(IM,JM)
C     
C     INCLUDE COMMON BLOCKS.
      INCLUDE "CTLBLK.comm"
      INCLUDE "LOOPS.comm"
      INCLUDE "MASKS.comm"
      INCLUDE "PHYS.comm"
      INCLUDE "VRBLS.comm"
C
       EQUIVALENCE
     & (ITTB  (1,1),IPTB  (1,1)                           )
     &,(IQTB  (1,1),ITHTB (1,1)                           )
     &,(QQ    (1,1),APESP (1,1),TTHES (1,1)               )
     &,(TTH   (1,1),TQ    (1,1),TP    (1,1)               )
       EQUIVALENCE
     & (BQS00 (1,1),P00   (1,1),BTHE00(1,1),T00   (1,1)   )
     &,(SQS00 (1,1),P10   (1,1),STHE00(1,1),T10   (1,1)   )
     &,(BQS10 (1,1),P01   (1,1),BTHE10(1,1),T01   (1,1)   )
     &,(SQS10 (1,1),P11   (1,1),STHE10(1,1),T11   (1,1)   )
     &,(BQ    (1,1)          ,BTH   (1,1)                 )
     &,(SQ    (1,1)          ,STH   (1,1)                 )
C     
C     
C***********************************************************************
C     START OTLIFT2 HERE
C     
C     INTIALIZE LIFTED INDEX ARRAY TO ZERO.
      DO J=JSTA,JEND
      DO I=1,IM
        SLINDX(I,J)=D00
      ENDDO
      ENDDO
C--------------FIND EXNER AT LOWEST LEVEL-------------------------------
      DO J=JSTA_M2,JEND_M2
      DO I=2,IM-1
        LBTM=LMH(I,J)
        TBT(I,J)=T(I,J,LBTM)
        QBT(I,J)=Q(I,J,LBTM)
        PDSL(I,J)=PD(I,J)*RES(I,J)
        APEBT(I,J)=PDSL(I,J)*AETA(LBTM)+PT
        APEBT(I,J)=(H10E5/APEBT(I,J))**CAPA
      ENDDO
      ENDDO
C--------------SCALING POTENTIAL TEMPERATURE & TABLE INDEX--------------
      DO J=JSTA_M2,JEND_M2
      DO I=2,IM-1
        TTHBT(I,J)=TBT(I,J)*APEBT(I,J)
        TTH(I,J)=(TTHBT(I,J)-THL)*RDTH
        QQ(I,J)=TTH(I,J)-AINT(TTH(I,J))
        ITTB(I,J)=INT(TTH(I,J))+1
      ENDDO
      ENDDO
C--------------KEEPING INDICES WITHIN THE TABLE-------------------------
      DO J=JSTA_M2,JEND_M2
      DO I=2,IM-1
        IF(ITTB(I,J).LT.1)THEN
          ITTB(I,J)=1
          QQ(I,J)=D00
        ENDIF
          IF(ITTB(I,J).GE.JTB)THEN
          ITTB(I,J)=JTB-1
          QQ(I,J)=D00
        ENDIF
      ENDDO
      ENDDO
C--------------BASE AND SCALING FACTOR FOR SPEC. HUMIDITY---------------
      DO J=JSTA_M2,JEND_M2
      DO I=2,IM-1
        ITTBK=ITTB(I,J)
        BQS00(I,J)=QS0(ITTBK)
        SQS00(I,J)=SQS(ITTBK)
        BQS10(I,J)=QS0(ITTBK+1)
        SQS10(I,J)=SQS(ITTBK+1)
      ENDDO
      ENDDO
C--------------SCALING SPEC. HUMIDITY & TABLE INDEX---------------------
      DO J=JSTA_M2,JEND_M2
      DO I=2,IM-1
        BQ(I,J)=(BQS10(I,J)-BQS00(I,J))*QQ(I,J)+BQS00(I,J)
        SQ(I,J)=(SQS10(I,J)-SQS00(I,J))*QQ(I,J)+SQS00(I,J)
        TQ(I,J)=(QBT(I,J)-BQ(I,J))/SQ(I,J)*RDQ
        PP(I,J)=TQ(I,J)-AINT(TQ(I,J))
        IQTB(I,J)=INT(TQ(I,J))+1
      ENDDO
      ENDDO
C--------------KEEPING INDICES WITHIN THE TABLE-------------------------
      DO J=JSTA_M2,JEND_M2
      DO I=2,IM-1
        IF(IQTB(I,J).LT.1)THEN
          IQTB(I,J)=1
          PP(I,J)=D00
        ENDIF
        IF(IQTB(I,J).GE.ITB)THEN
          IQTB(I,J)=ITB-1
          PP(I,J)=D00
        ENDIF
      ENDDO
      ENDDO
C--------------SATURATION PRESSURE AT FOUR SURROUNDING TABLE PTS.-------
      DO J=JSTA_M2,JEND_M2
      DO I=2,IM-1
        IQ=IQTB(I,J)
        IT=ITTB(I,J)
        P00(I,J)=PTBL(IQ,IT)
        P10(I,J)=PTBL(IQ+1,IT)
        P01(I,J)=PTBL(IQ,IT+1)
        P11(I,J)=PTBL(IQ+1,IT+1)
      ENDDO
      ENDDO
C--------------SATURATION POINT VARIABLES AT THE BOTTOM-----------------
      DO J=JSTA_M2,JEND_M2
      DO I=2,IM-1
        TPSP(I,J)=P00(I,J)+(P10(I,J)-P00(I,J))*PP(I,J)
     1                    +(P01(I,J)-P00(I,J))*QQ(I,J)
     2        +(P00(I,J)-P10(I,J)-P01(I,J)+P11(I,J))*PP(I,J)*QQ(I,J)
        IF(TPSP(I,J).LE.D00)TPSP(I,J)=H10E5
        APESP(I,J)=(H10E5/TPSP(I,J))**CAPA
        TTHES(I,J)=TTHBT(I,J)*EXP(ELOCP*QBT(I,J)*APESP(I,J)/TTHBT(I,J))
      ENDDO
      ENDDO
C-----------------------------------------------------------------------
      DO J=JSTA_M2,JEND_M2
      DO I=2,IM-1
        PSP(I,J)=TPSP(I,J)
        THBT(I,J)=TTHBT(I,J)
        THESP(I,J)=TTHES(I,J)
      ENDDO
      ENDDO
C-----------------------------------------------------------------------
 190  CONTINUE
C--------------SCALING PRESSURE & TT TABLE INDEX------------------------
      DO J=JSTA_M2,JEND_M2
      DO I=2,IM-1
        P (I,J)=H5E4
        TP(I,J)=(P(I,J)-PL)*RDP
        QQ(I,J)=TP(I,J)-AINT(TP(I,J))
        IPTB(I,J)=INT(TP(I,J))+1
      ENDDO
      ENDDO
C--------------KEEPING INDICES WITHIN THE TABLE-------------------------
      DO J=JSTA_M2,JEND_M2
      DO I=2,IM-1
        IF(IPTB(I,J).LT.1)THEN
          IPTB(I,J)=1
          QQ(I,J)=D00
        ENDIF
        IF(IPTB(I,J).GE.ITB)THEN
          IPTB(I,J)=ITB-1
          QQ(I,J)=D00
        ENDIF
      ENDDO
      ENDDO
C--------------BASE AND SCALING FACTOR FOR THE--------------------------
      DO J=JSTA_M2,JEND_M2
      DO I=2,IM-1
        IPTBK=IPTB(I,J)
        BTHE00(I,J)=THE0(IPTBK)
        STHE00(I,J)=STHE(IPTBK)
        BTHE10(I,J)=THE0(IPTBK+1)
        STHE10(I,J)=STHE(IPTBK+1)
      ENDDO
      ENDDO
C--------------SCALING THE & TT TABLE INDEX-----------------------------
      DO J=JSTA_M2,JEND_M2
      DO I=2,IM-1
        BTH(I,J)=(BTHE10(I,J)-BTHE00(I,J))*QQ(I,J)+BTHE00(I,J)
        STH(I,J)=(STHE10(I,J)-STHE00(I,J))*QQ(I,J)+STHE00(I,J)
        TTH(I,J)=(THESP(I,J)-BTH(I,J))/STH(I,J)*RDTHE
        PP(I,J)=TTH(I,J)-AINT(TTH(I,J))
        ITHTB(I,J)=INT(TTH(I,J))+1
      ENDDO
      ENDDO
C--------------KEEPING INDICES WITHIN THE TABLE-------------------------
      DO J=JSTA_M2,JEND_M2
      DO I=2,IM-1
        IF(ITHTB(I,J).LT.1)THEN
          ITHTB(I,J)=1
          PP(I,j)=D00
        ENDIF
        IF(ITHTB(I,J).GE.JTB)THEN
          ITHTB(I,J)=JTB-1
          PP(I,J)=D00
        ENDIF
      ENDDO
      ENDDO
C--------------TEMPERATURE AT FOUR SURROUNDING TT TABLE PTS.------------
      DO J=JSTA_M2,JEND_M2
      DO I=2,IM-1
        ITH=ITHTB(I,J)
        IP=IPTB(I,J)
        T00(I,J)=TTBL(ITH,IP)
        T10(I,J)=TTBL(ITH+1,IP)
        T01(I,J)=TTBL(ITH,IP+1)
        T11(I,J)=TTBL(ITH+1,IP+1)
      ENDDO
      ENDDO
C--------------PARCEL TEMPERATURE AT 500MB----------------------------
      DO J=JSTA_M2,JEND_M2
      DO I=2,IM-1
        IF(TPSP(I,J).GE.H5E4)THEN
          PARTMP(I,J)=(T00(I,J)+(T10(I,J)-T00(I,J))*PP(I,J)
     1                         +(T01(I,J)-T00(I,J))*QQ(I,J)
     2        +(T00(I,J)-T10(I,J)-T01(I,J)+T11(I,J))*PP(I,J)*QQ(I,J))
        ELSE
          PARTMP(I,J)=TBT(I,J)*APEBT(I,J)*D8202
        ENDIF
C--------------LIFTED INDEX---------------------------------------------
        SLINDX(I,J)=T500(I,J)-PARTMP(I,J)
      ENDDO
      ENDDO
C
      RETURN
      END
