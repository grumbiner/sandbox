C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
                             SUBROUTINE BOCOV
C     ******************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .     
C SUBPROGRAM:    BOCOV       UPDATE WIND POINTS ON BOUNDARY
C   PRGRMMR: JANJIC          ORG: W/NP22     DATE: 94-03-08
C     
C ABSTRACT:
C     U AND V COMPONENTS OF THE WIND ARE UPDATED ON THE
C     DOMAIN BOUNDARY BY APPLYING THE PRE-COMPUTED
C     TENDENCIES AT EACH TIME STEP.  AN EXTRAPOLATION FROM
C     INSIDE THE DOMAIN IS USED FOR THE COMPONENT TANGENTIAL
C     TO THE BOUNDARY IF THE NORMAL COMPONENT IS OUTWARD.
C     
C PROGRAM HISTORY LOG:
C   87-??-??  MESINGER   - ORIGINATOR
C   95-03-25  BLACK      - CONVERSION FROM 1-D TO 2-D IN HORIZONTAL
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
C                  BOCO
C   
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE : IBM SP
C$$$  
C***********************************************************************
                             P A R A M E T E R
     & (D06666=.06666666)
C-----------------------------------------------------------------------
      INCLUDE "parmeta"
      INCLUDE "mpp.h"
#include "sp.h"
C-----------------------------------------------------------------------
                             P A R A M E T E R
     & (IMJM=IM*JM-JM/2,LB=2*IM+JM-3)
C-----------------------------------------------------------------------
                             L O G I C A L
     & RUN,FIRST,RESTRT,SIGMA
C-----------------------------------------------------------------------
      INCLUDE "CTLBLK.comm"
C-----------------------------------------------------------------------
      INCLUDE "MASKS.comm"
C-----------------------------------------------------------------------
      INCLUDE "VRBLS.comm"
C----------------------------------------------------------------------
      INCLUDE "BOCO.comm"
C***********************************************************************
C------------- TIME INTERPOLATION OF U AND V AT THE OUTER BOUNDARY -----
      IIM=IM-MY_IS_GLB+1
      JJM=JM-MY_JS_GLB+1
C-----------------------------------------------------------------------
                             DO 115 L=1,LM
C-----------------------------------------------------------------------
      N=1
      DO 111 I=1,IM-1
      UB(N,L,1)=UB(N,L,1)+UB(N,L,2)*DT
      VB(N,L,1)=VB(N,L,1)+VB(N,L,2)*DT
      IF(MY_JS_GLB.EQ.1.AND.I.GE.MY_IS_GLB-ILPAD1.
     1                  AND.I.LE.MY_IE_GLB+IRPAD1)THEN
        II=I-MY_IS_GLB+1
        U(II,1,L)=UB(N,L,1)
        V(II,1,L)=VB(N,L,1)
      ENDIF
      N=N+1
  111 CONTINUE
C
      DO 112 I=1,IM-1
      UB(N,L,1)=UB(N,L,1)+UB(N,L,2)*DT
      VB(N,L,1)=VB(N,L,1)+VB(N,L,2)*DT
      IF(MY_JE_GLB.EQ.JM.AND.I.GE.MY_IS_GLB-ILPAD1.
     1                   AND.I.LE.MY_IE_GLB+IRPAD1)THEN
        II=I-MY_IS_GLB+1
        U(II,JJM,L)=UB(N,L,1)
        V(II,JJM,L)=VB(N,L,1)
      ENDIF
      N=N+1
  112 CONTINUE
C
      DO 113 J=2,JM-1,2
      UB(N,L,1)=UB(N,L,1)+UB(N,L,2)*DT
      VB(N,L,1)=VB(N,L,1)+VB(N,L,2)*DT
      IF(MY_IS_GLB.EQ.1.AND.J.GE.MY_JS_GLB-JBPAD1.
     1                  AND.J.LE.MY_JE_GLB+JTPAD1)THEN
        JJ=J-MY_JS_GLB+1
        U(1,JJ,L)=UB(N,L,1)
        V(1,JJ,L)=VB(N,L,1)
      ENDIF
      N=N+1
  113 CONTINUE
C
      DO 114 J=2,JM-1,2
      UB(N,L,1)=UB(N,L,1)+UB(N,L,2)*DT
      VB(N,L,1)=VB(N,L,1)+VB(N,L,2)*DT
      IF(MY_IE_GLB.EQ.IM.AND.J.GE.MY_JS_GLB-JBPAD1.
     1                   AND.J.LE.MY_JE_GLB+JTPAD1)THEN
        JJ=J-MY_JS_GLB+1
        U(IIM,JJ,L)=UB(N,L,1)
        V(IIM,JJ,L)=VB(N,L,1)
      ENDIF
      N=N+1
  114 CONTINUE
  115                        CONTINUE
C-----------------------------------------------------------------------
C--------------EXTRAPOLATION OF TANGENTIAL VELOCITY AT OUTFLOW POINTS---
C-----------------------------------------------------------------------
                             DO 125 L=1,LM
C-----------------------------------------------------------------------
      IF(IBROW.EQ.1)THEN
        DO 121 I=MYIS1_P1,MYIE2_P1
        IF(V(I,1,L).LT.0.)U(I,1,L)=(VTM(I,5,L)+1.)*U(I,3,L)
     1                             -VTM(I,5,L)    *U(I,5,L)
  121   CONTINUE
      ENDIF
C
      IF(ITROW.EQ.1)THEN
c       if(mype.eq.13) print*,'myis1_p1,myie2_p1=',
c    *    myis1_p1,myie2_p1
        DO 122 I=MYIS1_P1,MYIE2_P1
c       if(mype.eq.13) print*,'i,jjm,l=',i,jjm,l
        IF(V(I,JJM,L).GT.0.)
     1      U(I,JJM,L)=(VTM(I,JJM-4,L)+1.)*U(I,JJM-2,L)
     2                 -VTM(I,JJM-4,L)    *U(I,JJM-4,L)
  122   CONTINUE
      ENDIF
C
      DO 123 J=4,JM-3,2
      IF(ILCOL.EQ.1)THEN
        IF(MY_IS_GLB.EQ.1.AND.J.GE.MY_JS_GLB-JBPAD1.
     1                    AND.J.LE.MY_JE_GLB+JTPAD1)THEN
          JJ=J-MY_JS_GLB+1
          IF(U(1,JJ,L).LT.0.)
     1        V(1,JJ,L)=(VTM(3,JJ,L)+1.)*V(2,JJ,L)
     2                  -VTM(3,JJ,L)    *V(3,JJ,L)
        ENDIF
      ENDIF
  123 CONTINUE
C
      DO 124 J=4,JM-3,2
      IF(IRCOL.EQ.1)THEN
        IF(MY_IE_GLB.EQ.IM.AND.J.GE.MY_JS_GLB-JBPAD1.
     1                     AND.J.LE.MY_JE_GLB+JTPAD1)THEN
          JJ=J-MY_JS_GLB+1
          IF(U(IIM,JJ,L).GT.0.)
     1        V(IIM,JJ,L)=(VTM(IIM-2,JJ,L)+1.)*V(IIM-1,JJ,L)
     1                    -VTM(IIM-2,JJ,L)    *V(IIM-2,JJ,L)
        ENDIF
      ENDIF
  124 CONTINUE
  125                        CONTINUE
C-----------------------------------------------------------------------
C------------- SPACE INTERPOLATION OF U AND V AT THE INNER BOUNDARY ----
C-----------------------------------------------------------------------
                             DO 140 L=1,LM
C-----------------------------------------------------------------------
      IF(IBROW.EQ.1.AND.ILCOL.EQ.1)THEN
        U(2,2,L)=D06666*(4.*(U(1,1,L)+U(2,1,L)+U(2,3,L))
     1                     + U(1,2,L)+U(1,4,L)+U(2,4,L))
        V(2,2,L)=D06666*(4.*(V(1,1,L)+V(2,1,L)+V(2,3,L))
     1                      +V(1,2,L)+V(1,4,L)+V(2,4,L))
      ENDIF
C
      IF(IBROW.EQ.1.AND.IRCOL.EQ.1)THEN
        U(IIM-1,2,L)=D06666*(4.*(U(IIM-2,1,L)+U(IIM-1,1,L)
     1                          +U(IIM-2,3,L))
     2                          +U(IIM,2,L)+U(IIM,4,L)+U(IIM-1,4,L))
        V(IIM-1,2,L)=D06666*(4.*(V(IIM-2,1,L)+V(IIM-1,1,L)
     1                          +V(IIM-2,3,L))
     2                          +V(IIM,2,L)+V(IIM,4,L)+V(IIM-1,4,L))
      ENDIF
C
      IF(ITROW.EQ.1.AND.ILCOL.EQ.1)THEN
        U(2,JJM-1,L)=D06666*(4.*(U(1,JJM,L)+U(2,JJM,L)+U(2,JJM-2,L))
     1                          +U(1,JJM-1,L)+U(1,JJM-3,L)
     2                          +U(2,JJM-3,L))
        V(2,JJM-1,L)=D06666*(4.*(V(1,JJM,L)+V(2,JJM,L)+V(2,JJM-2,L))
     1                          +V(1,JJM-1,L)+V(1,JJM-3,L)
     2                          +V(2,JJM-3,L))
      ENDIF
C
      IF(ITROW.EQ.1.AND.IRCOL.EQ.1)THEN
        U(IIM-1,JJM-1,L)=
     1    D06666*(4.*(U(IIM-2,JJM,L)+U(IIM-1,JJM,L)+U(IIM-2,JJM-2,L))
     2               +U(IIM,JJM-1,L)+U(IIM,JJM-3,L)+U(IIM-1,JJM-3,L))
        V(IIM-1,JJM-1,L)=
     1    D06666*(4.*(V(IIM-2,JJM,L)+V(IIM-1,JJM,L)+V(IIM-2,JJM-2,L))
     2               +V(IIM,JJM-1,L)+V(IIM,JJM-3,L)+V(IIM-1,JJM-3,L))
      ENDIF
C-----------------------------------------------------------------------
C--------------SPACE INTERPOLATION OF U AND V AT THE INNER BOUNDARY-----
C-----------------------------------------------------------------------
      IF(IBROW.EQ.1)THEN
        DO 131 I=MYIS2,MYIE2
        U(I,2,L)=(U(I-1,1,L)+U(I,1,L)+U(I-1,3,L)+U(I,3,L))*0.25
        V(I,2,L)=(V(I-1,1,L)+V(I,1,L)+V(I-1,3,L)+V(I,3,L))*0.25
  131   CONTINUE
      ENDIF
C
      IF(ITROW.EQ.1)THEN
        DO 132 I=MYIS2,MYIE2
        U(I,JJM-1,L)=(U(I-1,JJM-2,L)+U(I,JJM-2,L)
     1               +U(I-1,JJM,L)+U(I,JJM,L))*0.25
        V(I,JJM-1,L)=(V(I-1,JJM-2,L)+V(I,JJM-2,L)
     1               +V(I-1,JJM,L)+V(I,JJM,L))*0.25
  132   CONTINUE
      ENDIF
C
      DO 133 J=3,JM-2,2
      IF(ILCOL.EQ.1)THEN
        IF(MY_IS_GLB.EQ.1.AND.J.GE.MY_JS_GLB-JBPAD1.
     1                    AND.J.LE.MY_JE_GLB+JTPAD1)THEN
          JJ=J-MY_JS_GLB+1
          U(1,JJ,L)=(U(1,JJ-1,L)+U(2,JJ-1,L)
     1              +U(1,JJ+1,L)+U(2,JJ+1,L))*0.25
          V(1,JJ,L)=(V(1,JJ-1,L)+V(2,JJ-1,L)
     1              +V(1,JJ+1,L)+V(2,JJ+1,L))*0.25
        ENDIF
      ENDIF
  133 CONTINUE
C
      IF(IRCOL.EQ.1)THEN
        DO 134 J=3,JM-2,2
        IF(MY_IE_GLB.EQ.IM.AND.J.GE.MY_JS_GLB-JBPAD1.
     1                     AND.J.LE.MY_JE_GLB+JTPAD1)THEN
          JJ=J-MY_JS_GLB+1
          U(IIM-1,JJ,L)=0.25*(U(IIM-1,JJ-1,L)+U(IIM,JJ-1,L)
     1                     +U(IIM-1,JJ+1,L)+U(IIM,JJ+1,L))
          V(IIM-1,JJ,L)=0.25*(V(IIM-1,JJ-1,L)+V(IIM,JJ-1,L)
     1                     +V(IIM-1,JJ+1,L)+V(IIM,JJ+1,L))
        ENDIF
  134   CONTINUE
      ENDIF
C-----------------------------------------------------------------------
  140                        CONTINUE
C-----------------------------------------------------------------------
                             RETURN
                             END
