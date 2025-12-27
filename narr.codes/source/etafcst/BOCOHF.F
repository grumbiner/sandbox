C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
                             SUBROUTINE BOCOHF
C     ******************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .     
C SUBPROGRAM:    BOCOHF      UPDATE MASS POINTS ON BOUNDARY
C   PRGRMMR: JANJIC          ORG: W/NP22     DATE: 94-03-08
C     
C ABSTRACT:
C     TEMPERATURE, SURFACE PRESSURE, AND TKE ARE
C     UPDATED ON THE DOMAIN BOUNDARY BY APPLYING THE
C     PRE-COMPUTED TENDENCIES AT EACH TIME STEP.
C     THE MOISTURE VARIABLES ARE EXCLUDED SINCE THIS
C     ROUTINE IS USED WITH THE LYNCH DIGITAL FILTER.
C     
C PROGRAM HISTORY LOG:
C   87-??-??  MESINGER   - ORIGINATOR
C   95-03-25  BLACK      - CONVERSION FROM 1-D TO 2-D in HORIZONTAL
C   96-12-13  BLACK      - FINAL MODIFICATION FOR NESTED RUNS
C   98-10-30  BLACK      - MODIFIED FOR DISTRIBUTED MEMORY
C   99-08-25  BLACK      - REMOVED MOISTURE VARIABLES
C   00-09-14  BLACK      - MODIFIED FOR DIRECT ACCESS READ
C     
C USAGE: CALL BOCOH FROM MAIN PROGRAM EBU
C   INPUT ARGUMENT LIST:
C     NONE
C  
C   OUTPUT ARGUMENT LIST: 
C     NONE
C     
C   OUTPUT FILES:
C     NONE
C     
C   SUBPROGRAMS CALLED:
C  
C     UNIQUE: NONE
C  
C     LIBRARY: NONE
C  
C   COMMON BLOCKS: CTLBLK
C                  MASKS
C                  VRBLS
C                  PVRBLS
C                  CLDWTR
C                  BOCO
C                  MAPOT
C   
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE : IBM SP
C$$$  
C     ******************************************************************
C-----------------------------------------------------------------------
      INCLUDE "parmeta"
      INCLUDE "mpif.h"
      INCLUDE "mpp.h"
#include "sp.h"
C-----------------------------------------------------------------------
                             P A R A M E T E R
     & (IMJM=IM*JM-JM/2,LB=2*IM+JM-3,LP1=LM+1)
                             P A R A M E T E R
     & (ISIZ1=2*LB,ISIZ2=2*LB*LM)
C-----------------------------------------------------------------------
                             L O G I C A L
     & RUN,FIRST,RESTRT,SIGMA,NEST
C-----------------------------------------------------------------------
      INCLUDE "CTLBLK.comm"
C-----------------------------------------------------------------------
      INCLUDE "MASKS.comm"
C-----------------------------------------------------------------------
      INCLUDE "VRBLS.comm"
C-----------------------------------------------------------------------
      INCLUDE "PVRBLS.comm"
C-----------------------------------------------------------------------
      INCLUDE "CLDWTR.comm"
C-----------------------------------------------------------------------
      INCLUDE "BOCO.comm"
C-----------------------------------------------------------------------
      INCLUDE "MAPOT.comm"
C-----------------------------------------------------------------------
C***********************************************************************
C--------------READ FRESH BOUNDARY DATA IF NECESSARY--------------------
      IF(NTSD-1.EQ.NBOCO)THEN
        IF(MYPE.EQ.0.AND.NEST)THEN
          NREC=NINT((NTSD-1)*ABS(DT)/3600.)+2
          READ(NBC,REC=NREC)BCHR
     1,                   ((PDB(K,N),K=1,LB),N=1,2)
     2,                   (((TB(K,L,N),K=1,LB),L=1,LM),N=1,2)
     3,                   (((QB(K,L,N),K=1,LB),L=1,LM),N=1,2)
     4,                   (((UB(K,L,N),K=1,LB),L=1,LM),N=1,2)
     5,                   (((VB(K,L,N),K=1,LB),L=1,LM),N=1,2)
     6,                  (((Q2B(K,L,N),K=1,LB),L=1,LM),N=1,2)
     7,                 (((CWMB(K,L,N),K=1,LB),L=1,LM),N=1,2)
        ENDIF
        IF(MYPE.EQ.0.AND..NOT.NEST)THEN
          READ(NBC)PDB
          READ(NBC)TB
          READ(NBC)QB
          READ(NBC)UB
          READ(NBC)VB
          READ(NBC)Q2B
          READ(NBC)CWMB
        ENDIF
C
        CALL MPI_BCAST(PDB,ISIZ1,MPI_REAL,0,MPI_COMM_COMP,IRTN)
        CALL MPI_BCAST(TB,ISIZ2,MPI_REAL,0,MPI_COMM_COMP,IRTN)
cccc    CALL MPI_BCAST(QB,ISIZ2,MPI_REAL,0,MPI_COMM_COMP,IRTN)
        CALL MPI_BCAST(UB,ISIZ2,MPI_REAL,0,MPI_COMM_COMP,IRTN)
        CALL MPI_BCAST(VB,ISIZ2,MPI_REAL,0,MPI_COMM_COMP,IRTN)
        CALL MPI_BCAST(Q2B,ISIZ2,MPI_REAL,0,MPI_COMM_COMP,IRTN)
cccc    CALL MPI_BCAST(CWMB,ISIZ2,MPI_REAL,0,MPI_COMM_COMP,IRTN)
C***
C***    FIND NEXT BOUNDARY CONDITION READ
C***
        IF(NTSD.LT.NTSTM)THEN
          IF(MYPE.EQ.0.AND.NEST)BCHR=BCHR+1    ! This assumes 1-hrly BC's
          IF(MYPE.EQ.0.AND..NOT.NEST)READ(NBC)BCHR
          CALL MPI_BCAST(BCHR,1,MPI_REAL,0,MPI_COMM_COMP,IRTN)
          NBOCO=INT(BCHR*TSPH+0.5)
        ENDIF
C
      ENDIF
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      IIM=IM-MY_IS_GLB+1
      JJM=JM-MY_JS_GLB+1
C--------------------------------------------------------------
C***
C***  UPDATE THE SURFACE PRESSURE
C***
C--------------------------------------------------------------
      N=1
      DO 101 I=1,IM
      PDB(N,1)=PDB(N,1)+PDB(N,2)*DT
      IF(MY_JS_GLB.EQ.1.AND.I.GE.MY_IS_GLB-ILPAD1.
     1                  AND.I.LE.MY_IE_GLB+IRPAD1)THEN
        II=I-MY_IS_GLB+1
        PD(II,1)=PDB(N,1)
      ENDIF
      N=N+1
  101 CONTINUE
C
      DO 102 I=1,IM
      PDB(N,1)=PDB(N,1)+PDB(N,2)*DT
      IF(MY_JE_GLB.EQ.JM.AND.I.GE.MY_IS_GLB-ILPAD1.
     1                   AND.I.LE.MY_IE_GLB+IRPAD1)THEN
        II=I-MY_IS_GLB+1
        PD(II,JJM)=PDB(N,1)
      ENDIF
      N=N+1
  102 CONTINUE
C
      DO 103 J=3,JM-2,2
      PDB(N,1)=PDB(N,1)+PDB(N,2)*DT
      IF(MY_IS_GLB.EQ.1.AND.J.GE.MY_JS_GLB-JBPAD1.
     1                  AND.J.LE.MY_JE_GLB+JTPAD1)THEN
        JJ=J-MY_JS_GLB+1
        PD(1,JJ)=PDB(N,1)
      ENDIF
      N=N+1
  103 CONTINUE
C
      DO 104 J=3,JM-2,2
      PDB(N,1)=PDB(N,1)+PDB(N,2)*DT
      IF(MY_IE_GLB.EQ.IM.AND.J.GE.MY_JS_GLB-JBPAD1.
     1                   AND.J.LE.MY_JE_GLB+JTPAD1)THEN
        JJ=J-MY_JS_GLB+1
        PD(IIM,JJ)=PDB(N,1)
      ENDIF
      N=N+1
  104 CONTINUE
C--------------------------------------------------------------
C***
C***  UPDATE THE 3-D MASS VARIABLES
C***
C--------------------------------------------------------------
                             DO 115 L=1,LM
C--------------------------------------------------------------
      N=1
      DO 111 I=1,IM
      TB(N,L,1)=TB(N,L,1)+TB(N,L,2)*DT
      Q2B(N,L,1)=Q2B(N,L,1)+Q2B(N,L,2)*DT
      IF(MY_JS_GLB.EQ.1.AND.I.GE.MY_IS_GLB-ILPAD1.
     1                  AND.I.LE.MY_IE_GLB+IRPAD1)THEN
        II=I-MY_IS_GLB+1
        T(II,1,L)=TB(N,L,1)
        Q2(II,1,L)=Q2B(N,L,1)
      ENDIF
      N=N+1
  111 CONTINUE
C
      DO 112 I=1,IM
      TB(N,L,1)=TB(N,L,1)+TB(N,L,2)*DT
      Q2B(N,L,1)=Q2B(N,L,1)+Q2B(N,L,2)*DT
      IF(MY_JE_GLB.EQ.JM.AND.I.GE.MY_IS_GLB-ILPAD1.
     1               AND.I.LE.MY_IE_GLB+IRPAD1)THEN
        II=I-MY_IS_GLB+1
        T(II,JJM,L)=TB(N,L,1)
        Q2(II,JJM,L)=Q2B(N,L,1)
      ENDIF
      N=N+1
  112 CONTINUE
C
      DO 113 J=3,JM-2,2
      TB(N,L,1)=TB(N,L,1)+TB(N,L,2)*DT
      Q2B(N,L,1)=Q2B(N,L,1)+Q2B(N,L,2)*DT
      IF(MY_IS_GLB.EQ.1.AND.J.GE.MY_JS_GLB-JBPAD1.
     1                  AND.J.LE.MY_JE_GLB+JTPAD1)THEN
        JJ=J-MY_JS_GLB+1
        T(1,JJ,L)=TB(N,L,1)
        Q2(1,JJ,L)=Q2B(N,L,1)
      ENDIF
      N=N+1
  113 CONTINUE
C
      DO 114 J=3,JM-2,2
      TB(N,L,1)=TB(N,L,1)+TB(N,L,2)*DT
      Q2B(N,L,1)=Q2B(N,L,1)+Q2B(N,L,2)*DT
      IF(MY_IE_GLB.EQ.IM.AND.J.GE.MY_JS_GLB-JBPAD1.
     1                   AND.J.LE.MY_JE_GLB+JTPAD1)THEN
        JJ=J-MY_JS_GLB+1
        T(IIM,JJ,L)=TB(N,L,1)
        Q2(IIM,JJ,L)=Q2B(N,L,1)
      ENDIF
      N=N+1
  114 CONTINUE
C
  115 CONTINUE
C--------------------------------------------------------------------
C
C------- SPACE INTERPOLATION OF PD AND T AT THE INNER BOUNDARY ------
C
C--------------------------------------------------------------------
      IF(IBROW.EQ.1)THEN
        DO 121 I=MYIS,MYIE1
        SHTM=HTM(I,1,LM)+HTM(I+1,1,LM)+HTM(I,3,LM)+HTM(I+1,3,LM)
        PD(I,2)=(PD(I,1)*HTM(I,1,LM)+PD(I+1,1)*HTM(I+1,1,LM)
     1          +PD(I,3)*HTM(I,3,LM)+PD(I+1,3)*HTM(I+1,3,LM))/SHTM
  121   CONTINUE
      ENDIF
C
      IF(ITROW.EQ.1)THEN
        DO 122 I=MYIS,MYIE1
        SHTM=HTM(I,JJM-2,LM)+HTM(I+1,JJM-2,LM)+HTM(I,JJM,LM)
     1                                        +HTM(I+1,JJM,LM)
        PD(I,JJM-1)=(PD(I,JJM-2)*HTM(I,JJM-2,LM)
     1             +PD(I+1,JJM-2)*HTM(I+1,JJM-2,LM)
     2             +PD(I,JJM)*HTM(I,JJM,LM)
     3             +PD(I+1,JJM)*HTM(I+1,JJM,LM))/SHTM
  122   CONTINUE
      ENDIF
C
      IF(ILCOL.EQ.1)THEN
        DO 123 J=4,JM-3,2
        IF(MY_IS_GLB.EQ.1.AND.J.GE.MY_JS_GLB-JBPAD1.
     1                    AND.J.LE.MY_JE_GLB+JTPAD1)THEN
          JJ=J-MY_JS_GLB+1
          SHTM=HTM(1,JJ-1,LM)+HTM(2,JJ-1,LM)+HTM(1,JJ+1,LM)
     1                                      +HTM(2,JJ+1,LM)
          PD(1,JJ)=(PD(1,JJ-1)*HTM(1,JJ-1,LM)
     1             +PD(2,JJ-1)*HTM(2,JJ-1,LM)
     2             +PD(1,JJ+1)*HTM(1,JJ+1,LM)
     3             +PD(2,JJ+1)*HTM(2,JJ+1,LM))/SHTM
        ENDIF
  123   CONTINUE
      ENDIF
C
      IF(IRCOL.EQ.1)THEN
        DO 124 J=4,JM-3,2
        IF(MY_IE_GLB.EQ.IM.AND.J.GE.MY_JS_GLB-JBPAD1.
     1                     AND.J.LE.MY_JE_GLB+JTPAD1)THEN
          JJ=J-MY_JS_GLB+1
          SHTM=HTM(IIM-1,JJ-1,LM)+HTM(IIM,JJ-1,LM)
     1        +HTM(IIM-1,JJ+1,LM)+HTM(IIM,JJ+1,LM)
          PD(IIM-1,JJ)=(PD(IIM-1,JJ-1)*HTM(IIM-1,JJ-1,LM)
     1                 +PD(IIM,JJ-1)*HTM(IIM,JJ-1,LM)
     2                 +PD(IIM-1,JJ+1)*HTM(IIM-1,JJ+1,LM)
     3                 +PD(IIM,JJ+1)*HTM(IIM,JJ+1,LM))/SHTM
        ENDIF
  124   CONTINUE
      ENDIF
C
C
C--------------------------------------------------------------------
                             DO 135 L=1,LM
C--------------------------------------------------------------------
      IF(IBROW.EQ.1)THEN
        DO 131 I=MYIS,MYIE1
        RHTM=1./(HTM(I,1,L)+HTM(I+1,1,L)+HTM(I,3,L)+HTM(I+1,3,L))
        T(I,2,L)=(T(I,1,L)*HTM(I,1,L)+T(I+1,1,L)*HTM(I+1,1,L)
     1           +T(I,3,L)*HTM(I,3,L)+T(I+1,3,L)*HTM(I+1,3,L))*RHTM
        Q2(I,2,L)=(Q2(I,1,L)*HTM(I,1,L)+Q2(I+1,1,L)*HTM(I+1,1,L)
     1            +Q2(I,3,L)*HTM(I,3,L)+Q2(I+1,3,L)*HTM(I+1,3,L))*RHTM
  131   CONTINUE
      ENDIF
C
      IF(ITROW.EQ.1)THEN
        DO 132 I=MYIS,MYIE1
        RHTM=1./(HTM(I,JJM-2,L)+HTM(I+1,JJM-2,L)
     1          +HTM(I,JJM,L)+HTM(I+1,JJM,L))
        T(I,JJM-1,L)=(T(I,JJM-2,L)*HTM(I,JJM-2,L)
     1              +T(I+1,JJM-2,L)*HTM(I+1,JJM-2,L)
     2              +T(I,JJM,L)*HTM(I,JJM,L)
     3              +T(I+1,JJM,L)*HTM(I+1,JJM,L))*RHTM
        Q2(I,JJM-1,L)=(Q2(I,JJM-2,L)*HTM(I,JJM-2,L)
     1               +Q2(I+1,JJM-2,L)*HTM(I+1,JJM-2,L)
     2               +Q2(I,JJM,L)*HTM(I,JJM,L)
     3               +Q2(I+1,JJM,L)*HTM(I+1,JJM,L))*RHTM
  132   CONTINUE
      ENDIF
C
      IF(ILCOL.EQ.1)THEN
        DO 133 J=4,JM-3,2
        IF(MY_IS_GLB.EQ.1.AND.J.GE.MY_JS_GLB-JBPAD1.
     1                    AND.J.LE.MY_JE_GLB+JTPAD1)THEN
          JJ=J-MY_JS_GLB+1
          RHTM=1./(HTM(1,JJ-1,L)+HTM(2,JJ-1,L)
     1            +HTM(1,JJ+1,L)+HTM(2,JJ+1,L))
          T(1,JJ,L)=(T(1,JJ-1,L)*HTM(1,JJ-1,L)
     1              +T(2,JJ-1,L)*HTM(2,JJ-1,L)
     2              +T(1,JJ+1,L)*HTM(1,JJ+1,L)
     3              +T(2,JJ+1,L)*HTM(2,JJ+1,L))*RHTM
          Q2(1,JJ,L)=(Q2(1,JJ-1,L)*HTM(1,JJ-1,L)
     1               +Q2(2,JJ-1,L)*HTM(2,JJ-1,L)
     2               +Q2(1,JJ+1,L)*HTM(1,JJ+1,L)
     3               +Q2(2,JJ+1,L)*HTM(2,JJ+1,L))*RHTM
        ENDIF
  133   CONTINUE
      ENDIF
C
      IF(IRCOL.EQ.1)THEN
        DO 134 J=4,JM-3,2
        IF(MY_IE_GLB.EQ.IM.AND.J.GE.MY_JS_GLB-JBPAD1.
     1                     AND.J.LE.MY_JE_GLB+JTPAD1)THEN
          JJ=J-MY_JS_GLB+1
          RHTM=1./(HTM(IIM-1,JJ-1,L)+HTM(IIM,JJ-1,L)
     1            +HTM(IIM-1,JJ+1,L)+HTM(IIM,JJ+1,L))
          T(IIM-1,JJ,L)=(T(IIM-1,JJ-1,L)*HTM(IIM-1,JJ-1,L)
     1                +T(IIM,JJ-1,L)*HTM(IIM,JJ-1,L)
     2                +T(IIM-1,JJ+1,L)*HTM(IIM-1,JJ+1,L)
     2                +T(IIM,JJ+1,L)*HTM(IIM,JJ+1,L))*RHTM
          Q2(IIM-1,JJ,L)=(Q2(IIM-1,JJ-1,L)*HTM(IIM-1,JJ-1,L)
     1                 +Q2(IIM,JJ-1,L)*HTM(IIM,JJ-1,L)
     2                 +Q2(IIM-1,JJ+1,L)*HTM(IIM-1,JJ+1,L)
     2                 +Q2(IIM,JJ+1,L)*HTM(IIM,JJ+1,L))*RHTM
        ENDIF
  134   CONTINUE
      ENDIF
  135                        CONTINUE
C--------------------------------------------------------------------
C--------------------------------------------------------------------
                             RETURN
                             END
