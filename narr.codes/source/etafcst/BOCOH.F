C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
                             SUBROUTINE BOCOH 
C     ******************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .     
C SUBPROGRAM:    BOCOH       UPDATE MASS POINTS ON BOUNDARY
C   PRGRMMR: JANJIC          ORG: W/NP22     DATE: 94-03-08
C     
C ABSTRACT:
C     TEMPERATURE, SPECIFIC HUMIDITY, AND SURFACE PRESSURE
C     ARE UPDATED ON THE DOMAIN BOUNDARY BY APPLYING THE
C     PRE-COMPUTED TENDENCIES AT EACH TIME STEP.
C     
C PROGRAM HISTORY LOG:
C   87-??-??  MESINGER   - ORIGINATOR
C   95-03-25  BLACK      - CONVERSION FROM 1-D TO 2-D in HORIZONTAL
C   96-12-13  BLACK      - FINAL MODIFICATION FOR NESTED RUNS
C   98-10-30  BLACK      - MODIFIED FOR DISTRIBUTED MEMORY
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
     & RUN,FIRST,RESTRT,SIGMA
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
c     if(mype.eq.13) then
c      print*,'At the start of BOCOH'
c      do l=1,60
c       print*,'l,t(40,77,l)=',l,t(40,77,l)
c      enddo
c     endif
C--------------READ FRESH BOUNDARY DATA IF NECESSARY--------------------
      IF(NTSD-1.EQ.NBOCO)THEN
c       print*,'am i in if block?'
        IF(MYPE.EQ.0)THEN
c         print*,'nbc=',nbc
          print*,'BOCOH: read fresh boundary data NTSD=',NTSD
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
        CALL MPI_BCAST(QB,ISIZ2,MPI_REAL,0,MPI_COMM_COMP,IRTN)
        CALL MPI_BCAST(UB,ISIZ2,MPI_REAL,0,MPI_COMM_COMP,IRTN)
        CALL MPI_BCAST(VB,ISIZ2,MPI_REAL,0,MPI_COMM_COMP,IRTN)
        CALL MPI_BCAST(Q2B,ISIZ2,MPI_REAL,0,MPI_COMM_COMP,IRTN)
        CALL MPI_BCAST(CWMB,ISIZ2,MPI_REAL,0,MPI_COMM_COMP,IRTN)
C***
C***    FIND NEXT BOUNDARY CONDITION READ
C***
        IF(NTSD.LT.NTSTM)THEN
          IF(MYPE.EQ.0)READ(NBC)BCHR
          CALL MPI_BCAST(BCHR,1,MPI_REAL,0,MPI_COMM_COMP,IRTN)
          NBOCO=INT(BCHR*TSPH+0.5)
          IF(MYPE.EQ.0)THEN
          print*,'NEXT read BCHR, NBOCO = ', BCHR, NBOCO
          END IF
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
c     if(mype.eq.13) then
c      print*,'1st break of BOCOH'
c      do l=1,60
c       print*,'l,t(40,77,l)=',l,t(40,77,l)
c      enddo
c     endif
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
c     if(mype.eq.13) then
c      print*,'2nd break of BOCOH'
c      do l=1,60
c       print*,'l,t(40,77,l)=',l,t(40,77,l)
c      enddo
c     endif
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
      QB(N,L,1)=QB(N,L,1)+QB(N,L,2)*DT
      Q2B(N,L,1)=Q2B(N,L,1)+Q2B(N,L,2)*DT
      CWMB(N,L,1)=CWMB(N,L,1)+CWMB(N,L,2)*DT                        
      IF(MY_JS_GLB.EQ.1.AND.I.GE.MY_IS_GLB-ILPAD1.
     1                  AND.I.LE.MY_IE_GLB+IRPAD1)THEN
        II=I-MY_IS_GLB+1
        T(II,1,L)=TB(N,L,1)
        Q(II,1,L)=QB(N,L,1)
        Q2(II,1,L)=Q2B(N,L,1)
        CWM(II,1,L)=CWMB(N,L,1)                                                 
      ENDIF
      N=N+1
  111 CONTINUE
C
c     if(mype.eq.13) then
c      print*,'Before 112 block'
c      do l=1,60
c       print*,'l,t(40,77,l)=',l,t(40,77,l)
c      enddo
c     endif

      DO 112 I=1,IM
c     if(mype.eq.13) then
c     print*,'i,n,l,tb(n,l,1),tb(n,l,2),dt=',
c    *  i,n,l,tb(n,l,1),tb(n,l,2),dt
c     endif
      TB(N,L,1)=TB(N,L,1)+TB(N,L,2)*DT
      QB(N,L,1)=QB(N,L,1)+QB(N,L,2)*DT
      Q2B(N,L,1)=Q2B(N,L,1)+Q2B(N,L,2)*DT
      CWMB(N,L,1)=CWMB(N,L,1)+CWMB(N,L,2)*DT                       
      IF(MY_JE_GLB.EQ.JM.AND.I.GE.MY_IS_GLB-ILPAD1.
     1               AND.I.LE.MY_IE_GLB+IRPAD1)THEN
        II=I-MY_IS_GLB+1
c       if(mype.eq.13.and.ii.eq.40.and.jjm.eq.77) then
c        print*,'n,l=',n,l
c        print*,'tb(n,l,1)=',tb(n,l,1)
c       endif 
        T(II,JJM,L)=TB(N,L,1)
        Q(II,JJM,L)=QB(N,L,1)
        Q2(II,JJM,L)=Q2B(N,L,1)
        CWM(II,JJM,L)=CWMB(N,L,1)                                                 
      ENDIF
      N=N+1
  112 CONTINUE
C
c     if(mype.eq.13) then
c      print*,'Before 113 block'
c      do l=1,60
c       print*,'l,t(40,77,l)=',l,t(40,77,l)
c      enddo
c     endif

      DO 113 J=3,JM-2,2
      TB(N,L,1)=TB(N,L,1)+TB(N,L,2)*DT
      QB(N,L,1)=QB(N,L,1)+QB(N,L,2)*DT
      Q2B(N,L,1)=Q2B(N,L,1)+Q2B(N,L,2)*DT
      CWMB(N,L,1)=CWMB(N,L,1)+CWMB(N,L,2)*DT                       
      IF(MY_IS_GLB.EQ.1.AND.J.GE.MY_JS_GLB-JBPAD1.
     1                  AND.J.LE.MY_JE_GLB+JTPAD1)THEN
        JJ=J-MY_JS_GLB+1
        T(1,JJ,L)=TB(N,L,1)
        Q(1,JJ,L)=QB(N,L,1)
        Q2(1,JJ,L)=Q2B(N,L,1)
        CWM(1,JJ,L)=CWMB(N,L,1)                                                 
      ENDIF
      N=N+1
  113 CONTINUE
C
c     if(mype.eq.13) then
c      print*,'Before 114 block'
c      do l=1,60
c       print*,'l,t(40,77,l)=',l,t(40,77,l)
c      enddo
c     endif

      DO 114 J=3,JM-2,2
      TB(N,L,1)=TB(N,L,1)+TB(N,L,2)*DT
      QB(N,L,1)=QB(N,L,1)+QB(N,L,2)*DT
      Q2B(N,L,1)=Q2B(N,L,1)+Q2B(N,L,2)*DT
      CWMB(N,L,1)=CWMB(N,L,1)+CWMB(N,L,2)*DT                       
      IF(MY_IE_GLB.EQ.IM.AND.J.GE.MY_JS_GLB-JBPAD1.
     1                   AND.J.LE.MY_JE_GLB+JTPAD1)THEN
        JJ=J-MY_JS_GLB+1
        T(IIM,JJ,L)=TB(N,L,1)
        Q(IIM,JJ,L)=QB(N,L,1)
        Q2(IIM,JJ,L)=Q2B(N,L,1)
        CWM(IIM,JJ,L)=CWMB(N,L,1)                                                 
      ENDIF
      N=N+1
  114 CONTINUE
C
  115 CONTINUE
c     if(mype.eq.13) then
c      print*,'3rd break of BOCOH'
c      do l=1,60
c       print*,'l,t(40,77,l)=',l,t(40,77,l)
c      enddo
c     endif
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
c     if(mype.eq.13) then
c      print*,'4th break of BOCOH'
c      do l=1,60
c       print*,'l,t(40,77,l)=',l,t(40,77,l)
c      enddo
c     endif
C
C
C--------------------------------------------------------------------
                             DO 135 L=1,LM
C--------------------------------------------------------------------
c     if(mype.eq.13) then
c     print*,'l,ibrow,itrow,ircol,ilcol=',l,ibrow,itrow,ircol,ilcol
c     endif
      IF(IBROW.EQ.1)THEN
        DO 131 I=MYIS,MYIE1
        RHTM=1./(HTM(I,1,L)+HTM(I+1,1,L)+HTM(I,3,L)+HTM(I+1,3,L))
        T(I,2,L)=(T(I,1,L)*HTM(I,1,L)+T(I+1,1,L)*HTM(I+1,1,L)
     1           +T(I,3,L)*HTM(I,3,L)+T(I+1,3,L)*HTM(I+1,3,L))*RHTM
        Q(I,2,L)=(Q(I,1,L)*HTM(I,1,L)+Q(I+1,1,L)*HTM(I+1,1,L)
     1           +Q(I,3,L)*HTM(I,3,L)+Q(I+1,3,L)*HTM(I+1,3,L))*RHTM
        Q2(I,2,L)=(Q2(I,1,L)*HTM(I,1,L)+Q2(I+1,1,L)*HTM(I+1,1,L)
     1            +Q2(I,3,L)*HTM(I,3,L)+Q2(I+1,3,L)*HTM(I+1,3,L))*RHTM
        CWM(I,2,L)=(CWM(I,1,L)*HTM(I,1,L)+CWM(I+1,1,L)*HTM(I+1,1,L)
     1             +CWM(I,3,L)*HTM(I,3,L)+CWM(I+1,3,L)*HTM(I+1,3,L))
     2             *RHTM
  131   CONTINUE
      ENDIF
C
      IF(ITROW.EQ.1)THEN
c       print*,'inside itrow.eq.1 block'
c       print*,'myis,myie1=',myis,myie1
c       print*,'jjm=',jjm
        DO 132 I=MYIS,MYIE1
        RHTM=1./(HTM(I,JJM-2,L)+HTM(I+1,JJM-2,L)
     1          +HTM(I,JJM,L)+HTM(I+1,JJM,L))
c       if(i.eq.40.and.jjm-1.eq.76.and.mype.eq.13) then
c       if(i.eq.40.and.mype.eq.13) then
c        print*,'i,jjm,l=',i,jjm,l
c        print*,'T(I,JJM-2,L)=',T(I,JJM-2,L)
c        print*,'HTM(I,JJM-2,L)=',HTM(I,JJM-2,L)
c        print*,'T(I+1,JJM-2,L)=',T(I+1,JJM-2,L)
c        print*,'HTM(I+1,JJM-2,L)=',HTM(I+1,JJM-2,L)
c        print*,'T(I,JJM,L)=',T(I,JJM,L)
c        print*,'HTM(I,JJM,L)=',HTM(I,JJM,L)
c        print*,'T(I+1,JJM,L)=',T(I+1,JJM,L)
c        print*,'HTM(I+1,JJM,L)=',HTM(I+1,JJM,L)
c        print*,'rhtm=',rhtm
c       endif
        T(I,JJM-1,L)=(T(I,JJM-2,L)*HTM(I,JJM-2,L)
     1              +T(I+1,JJM-2,L)*HTM(I+1,JJM-2,L)
     2              +T(I,JJM,L)*HTM(I,JJM,L)
     3              +T(I+1,JJM,L)*HTM(I+1,JJM,L))*RHTM
        Q(I,JJM-1,L)=(Q(I,JJM-2,L)*HTM(I,JJM-2,L)
     1              +Q(I+1,JJM-2,L)*HTM(I+1,JJM-2,L)
     2              +Q(I,JJM,L)*HTM(I,JJM,L)
     3              +Q(I+1,JJM,L)*HTM(I+1,JJM,L))*RHTM
        Q2(I,JJM-1,L)=(Q2(I,JJM-2,L)*HTM(I,JJM-2,L)
     1               +Q2(I+1,JJM-2,L)*HTM(I+1,JJM-2,L)
     2               +Q2(I,JJM,L)*HTM(I,JJM,L)
     3               +Q2(I+1,JJM,L)*HTM(I+1,JJM,L))*RHTM
        CWM(I,JJM-1,L)=(CWM(I,JJM-2,L)*HTM(I,JJM-2,L)
     1                +CWM(I+1,JJM-2,L)*HTM(I+1,JJM-2,L)
     2                +CWM(I,JJM,L)*HTM(I,JJM,L)
     3                +CWM(I+1,JJM,L)*HTM(I+1,JJM,L))*RHTM
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
          Q(1,JJ,L)=(Q(1,JJ-1,L)*HTM(1,JJ-1,L)
     1              +Q(2,JJ-1,L)*HTM(2,JJ-1,L)
     2              +Q(1,JJ+1,L)*HTM(1,JJ+1,L)
     3              +Q(2,JJ+1,L)*HTM(2,JJ+1,L))*RHTM
          Q2(1,JJ,L)=(Q2(1,JJ-1,L)*HTM(1,JJ-1,L)
     1               +Q2(2,JJ-1,L)*HTM(2,JJ-1,L)
     2               +Q2(1,JJ+1,L)*HTM(1,JJ+1,L)
     3               +Q2(2,JJ+1,L)*HTM(2,JJ+1,L))*RHTM
          CWM(1,JJ,L)=(CWM(1,JJ-1,L)*HTM(1,JJ-1,L)
     1                +CWM(2,JJ-1,L)*HTM(2,JJ-1,L)
     2                +CWM(1,JJ+1,L)*HTM(1,JJ+1,L)
     3                +CWM(2,JJ+1,L)*HTM(2,JJ+1,L))*RHTM
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
c       if(iim-1.eq.40.and.jj.eq.77.and.mype.eq.13) then
c         print*,'l=',l
c         print*,'T(IIM-1,JJ-1,L)=',T(IIM-1,JJ-1,L)
c         print*,'HTM(IIM-1,JJ-1,L)=',HTM(IIM-1,JJ-1,L)
c         print*,'T(IIM,JJ-1,L)=',T(IIM,JJ-1,L)
c         print*,'HTM(IIM,JJ-1,L)=',HTM(IIM,JJ-1,L)
c         print*,'T(IIM-1,JJ+1,L)=',T(IIM-1,JJ+1,L)
c         print*,'HTM(IIM-1,JJ+1,L)=',HTM(IIM-1,JJ+1,L)
c         print*,'T(IIM,JJ+1,L)=',T(IIM,JJ+1,L)
c         print*,'HTM(IIM,JJ+1,L)=',HTM(IIM,JJ+1,L)
c         print*,'rhtm=',rhtm
c       endif
          T(IIM-1,JJ,L)=(T(IIM-1,JJ-1,L)*HTM(IIM-1,JJ-1,L)
     1                +T(IIM,JJ-1,L)*HTM(IIM,JJ-1,L)
     2                +T(IIM-1,JJ+1,L)*HTM(IIM-1,JJ+1,L)
     2                +T(IIM,JJ+1,L)*HTM(IIM,JJ+1,L))*RHTM
          Q(IIM-1,JJ,L)=(Q(IIM-1,JJ-1,L)*HTM(IIM-1,JJ-1,L)
     1                +Q(IIM,JJ-1,L)*HTM(IIM,JJ-1,L)
     2                +Q(IIM-1,JJ+1,L)*HTM(IIM-1,JJ+1,L)
     2                +Q(IIM,JJ+1,L)*HTM(IIM,JJ+1,L))*RHTM
          Q2(IIM-1,JJ,L)=(Q2(IIM-1,JJ-1,L)*HTM(IIM-1,JJ-1,L)
     1                 +Q2(IIM,JJ-1,L)*HTM(IIM,JJ-1,L)
     2                 +Q2(IIM-1,JJ+1,L)*HTM(IIM-1,JJ+1,L)
     2                 +Q2(IIM,JJ+1,L)*HTM(IIM,JJ+1,L))*RHTM
          CWM(IIM-1,JJ,L)=(CWM(IIM-1,JJ-1,L)*HTM(IIM-1,JJ-1,L)
     1                  +CWM(IIM,JJ-1,L)*HTM(IIM,JJ-1,L)
     2                  +CWM(IIM-1,JJ+1,L)*HTM(IIM-1,JJ+1,L)
     2                  +CWM(IIM,JJ+1,L)*HTM(IIM,JJ+1,L))*RHTM
        ENDIF
  134   CONTINUE
      ENDIF
  135                        CONTINUE
c     if(mype.eq.13) then
c      print*,'At the end of BOCOH'
c      do l=1,60
c       print*,'l,t(40,77,l)=',l,t(40,77,l)
c      enddo
c     endif
C--------------------------------------------------------------------
C--------------------------------------------------------------------
                             RETURN
                             END
