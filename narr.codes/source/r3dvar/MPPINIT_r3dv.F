C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
                          SUBROUTINE MPPINIT      
C     ******************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .
C SUBPROGRAM:    RADTN       THE OUTER RADIATION DRIVER
C   PRGRMMR: BLACK           ORG: W/NP22     DATE: 98-10-28
C
C ABSTRACT:
C     MPPINIT DETERMINES ALL RELEVANT VALUES FOR DIMENSIONS OF THE
C     DISTRIBUTED SUBDOMAINS AND THEIR HALOES.
C
C PROGRAM HISTORY LOG:
C   97-??-??  MEYS       - ORIGINATOR
C   97-??-??  BLACK      - CHANGES MADE FOR CLARITY
C   98-10-29  BLACK      - REWRITTEN FOR CLARITY
!   99-8-30   parrish    - addapted for dynamic memory in r3dv, using pointers
C
C USAGE: CALL RADTN FROM MAIN PROGRAM EBU
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
C     UNIQUE:
C        NONE
C
C     LIBRARY:
C        NONE
C
C   COMMON BLOCKS: MPPCOM
C                  GLB_TABLE
C                  TEMPCOM
C                  TOPO
C                  MAPPINGS
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE : IBM SP
C$$$
C-----------------------------------------------------------------------
       INCLUDE "PARMETA.comm"
       INCLUDE "mpp.h"
       INCLUDE "mpif.h"
      include "my_comm.h"
!#include "sp.h"
C-----------------------------------------------------------------------
       INTEGER ISTAT(MPI_STATUS_SIZE)
C-----------------------------------------------------------------------
C***********************************************************************
C-----------------------------------------------------------------------
C***
C***  INPES AND JNPES ARE THE NUMBER OF PEs REQUESTED IN X AND Y.
C***  ICHUNK AND JCHUNK ARE THE FIRST GUESS OF THE NUMBER
C***  OF I's AND J's IN EACH SUBDOMAIN OBTAINED BY SIMPLY 
C***  DIVIDING THE GLOBAL DIMENSIONS BY THE NUMBER OF PE's 
C***  REQUESTED IN EACH DIRECTION.
C***  
      ICHUNK=IM/INPES
      JCHUNK=JM/JNPES
      IICHUNK=ICHUNK+1
C***
C***  COMPUTE THE GLOBAL START AND END INDEX VALUES
C***  FOR I (MY_IS_GLB,MY_IE_GLB) AND J (MY_JS_GLB,MY_JE_GLB)
C***  ON EACH PE.
C***  IN GENERAL, THE NUMBER OF POINTS IN EACH DIRECTION
C***  WILL NOT DIVIDE EVENLY WITH INPES AND JNPES.  THE
C***  LOGIC BELOW GIVES ONE EXTRA POINT TO AS MANY OF THE
C***  EARLIEST PEs IN EACH DIRECTION AS IT TAKES TO USE
C***  UP THE REMAINDER POINTS (ITAIL AND JTAIL, WHICH ARE
C***  COMPUTED IN parmeta).
C***
      IPE=0
      MY_JS_CALC=1
      JNCHUNKS=0
C
      DO J=1,JNPES
        JCHUNK_CALC=JCHUNK
        IF(J.LE.JTAIL)JCHUNK_CALC=JCHUNK+1
        JNCHUNKS=JNCHUNKS+JCHUNK_CALC
        MY_JE_CALC=JNCHUNKS
        MY_IS_CALC=1
        NCHUNKS=0
C
        DO I=1,INPES
          ICHUNK_CALC=ICHUNK
          IF(I.LE.ITAIL)ICHUNK_CALC=ICHUNK+1
          NCHUNKS=NCHUNKS+ICHUNK_CALC
          MY_IE_CALC=NCHUNKS 
          IF(MYPE.EQ.IPE)THEN
            MY_IS_GLB=MY_IS_CALC
            MY_IE_GLB=MY_IE_CALC
            MY_JS_GLB=MY_JS_CALC
            MY_JE_GLB=MY_JE_CALC
          ENDIF
          MY_IS_CALC=MY_IE_CALC+1 
          IPE=IPE+1
        ENDDO
C
        MY_JS_CALC=MY_JE_CALC+1
      ENDDO
C----------------------------------------------------------------------
C***
C***  ILPADx IS THE INCREMENT INTO THE LEFT HALO OF A SUBDOMAIN.
C***  IRPADx IS THE INCREMENT INTO THE RIGHT HALO OF A SUBDOMAIN.
C***  ILPADx IS ALWAYS 0 FOR SUBDOMAINS ALONG THE WEST GLOBAL BOUNDARY.
C***  IRPADx IS ALWAYS 0 FOR SUBDOMAINS ALONG THE EAST GLOBAL BOUNDARY.
C***
C***  ILCOL IS A FLAG TELLING WHETHER OR NOT A SUBDOMAIN IS ON THE
C***  WEST (LEFT) GLOBAL BOUNDARY.
C***
C***  IS_INCx_BND AND IE_INCx_BND ARE INCREMENTS FROM THE LOCAL 
C***  STARTING OR ENDING I VALUE AWAY FROM THE LOCAL BOUNDARY INTO THE
C***  SURBDOMAIN.  THEY ARE NONZERO ONLY FOR SUBDOMAINS ON THE WESTERN
C***  AND EASTERN GLOBAL BOUNDARIES.
C***
      ILPAD1=1
      ILPAD2=2
      ILPAD3=3
      ILPAD4=4
      IRPAD1=1
      IRPAD2=2
      IRPAD3=3
      IRPAD4=4
C
      ILCOL=0
      IRCOL=0
C
      IS_INC1_BND=0
      IS_INC2_BND=0
      IE_INC1_BND=0
      IE_INC2_BND=0
C
      IF(MOD(MYPE,INPES).EQ.0)THEN             !WESTERNMOST SUBDOMAINS
        ILPAD1=0
        ILPAD2=0
        ILPAD3=0
        ILPAD4=0
        ILCOL=1
        IS_INC1_BND=1
        IS_INC2_BND=2
      end if                           !   dp 10/25/99
      IF(MOD(MYPE,INPES).EQ.INPES-1)THEN   !EASTERNMOST SUBDOMAINS   ! dp 10/25/99
        IRPAD1=0
        IRPAD2=0
        IRPAD3=0
        IRPAD4=0
        IRCOL=1
        IE_INC1_BND=1
        IE_INC2_BND=2
        MY_IE_GLB=IM
      ENDIF
C----------------------------------------------------------------------
C***
C***  NOW DO THE SAME FOR THE J DIRECTION
C***
      JBPAD1=1
      JBPAD2=2
      JBPAD3=3
      JBPAD4=4
      JBPAD5=5
      JTPAD1=1
      JTPAD2=2
      JTPAD3=3
      JTPAD4=4
      JTPAD5=5
C
      IBROW=0
      ITROW=0
C
      JS_INC1_BND=0
      JS_INC2_BND=0
      JS_INC3_BND=0
      JS_INC4_BND=0
      JS_INC5_BND=0
      JE_INC1_BND=0
      JE_INC2_BND=0
      JE_INC3_BND=0
      JE_INC4_BND=0
      JE_INC5_BND=0
C
      IF(MYPE/INPES.EQ.0)THEN              !SOUTHERNMOST SUBDOMAINS
        JBPAD1=0
        JBPAD2=0
        JBPAD3=0
        JBPAD4=0
        JBPAD5=0
        IBROW=1
        JS_INC1_BND=1
        JS_INC2_BND=2
        JS_INC3_BND=3
        JS_INC4_BND=4
        JS_INC5_BND=5
      end if                   !  dp 10/25/99
      IF(MYPE/INPES.EQ.JNPES-1)THEN    !NORTHERNMOST SUBDOMAINS ! dp 10/25/99
        JTPAD1=0
        JTPAD2=0
        JTPAD3=0
        JTPAD4=0
        JTPAD5=0
        ITROW=1
        JE_INC1_BND=1
        JE_INC2_BND=2
        JE_INC3_BND=3
        JE_INC4_BND=4
        JE_INC5_BND=5
        MY_JE_GLB=JM
      ENDIF
C----------------------------------------------------------------------
C***
C***  THE FOLLOWING ARE THE LOCAL LIMITS OF I AND J IN EACH SUBDOMAIN
C***
      MY_IS_LOC=1
      MY_IE_LOC=MY_IE_GLB-MY_IS_GLB+1
      MY_JS_LOC=1
      MY_JE_LOC=MY_JE_GLB-MY_JS_GLB+1
C----------------------------------------------------------------------
C***
C***  EACH PE WILL NOW FILL ITS OWN SECTIONS OF THE GLOBAL-TO-LOCAL
C***  TRANSLATION ARRAYS (DIMENSIONED GLOBALLY) AND LOCAL-TO-GLOBAL
C***  TRANSLATION ARRAYS (DIMENSIONED LOCALLY)
C***
      ILOC=0
      DO I=MY_IS_GLB-1,MY_IE_GLB+1
        G2LI(I)=ILOC
        L2GI(ILOC)=I
        ILOC=ILOC+1
      ENDDO
C
      JLOC=0
      DO J=MY_JS_GLB-1,MY_JE_GLB+1
        G2LJ(J)=JLOC
        L2GJ(JLOC)=J
        JLOC=JLOC+1
      ENDDO
C----------------------------------------------------------------------
C***
C***  EACH PE WILL NOW FILL THE ARRAY CALLED MY_NEB WHICH HOLDS THE
C***  NUMBER OF THE 8 PEs THAT ARE ITS NEIGHBORS: NORTH(1), EAST(2),
C***  SOUTH(3), WEST(4), NORTHEAST(5), SOUTHEAST(6), SOUTHWEST(7),
C***  AND NORTHWEST(8).  THE VALUE IN THE ARRAY WILL BE -1 FOR THOSE
C***  NEIGHBORS THAT DO NOT EXIST BECAUSE THEY ARE BEYOND THE
C***  GLOBAL DOMAIN BOUNDARY.
C***
      IPE=0
      DO J=1,JNPES
      DO I=1,INPES
        ITEMP(I,J)=IPE
        IF(IPE.EQ.MYPE) THEN
          MYI=I
          MYJ=J
        ENDIF
        IPE=IPE+1
        ENDDO
      ENDDO
C
      MY_N=-1
      IF(MYJ+1.LE.JNPES)MY_N=ITEMP(MYI,MYJ+1)
C
      MY_E=-1
      IF(MYI+1.LE.INPES)MY_E=ITEMP(MYI+1,MYJ)
C
      MY_S=-1
      IF(MYJ-1.GE.1)MY_S=ITEMP(MYI,MYJ-1)
C
      MY_W=-1
      IF(MYI-1.GE.1)MY_W=ITEMP(MYI-1,MYJ)
C
      MY_NE=-1
      IF((MYI+1.LE.INPES).AND.(MYJ+1.LE.JNPES))
     1    MY_NE=ITEMP(MYI+1,MYJ+1)
C
      MY_SE=-1
      IF((MYI+1.LE.INPES).AND.(MYJ-1.GE.1))
     1    MY_SE=ITEMP(MYI+1,MYJ-1)
C
      MY_SW=-1
      IF((MYI-1.GE.1).AND.(MYJ-1.GE.1))
     1    MY_SW=ITEMP(MYI-1,MYJ-1)
C
      MY_NW=-1
      IF((MYI-1.GE.1).AND.(MYJ+1.LE.JNPES))
     1    MY_NW=ITEMP(MYI-1,MYJ+1)
C
      MY_NEB(1)=MY_N
      MY_NEB(2)=MY_E
      MY_NEB(3)=MY_S
      MY_NEB(4)=MY_W
      MY_NEB(5)=MY_NE
      MY_NEB(6)=MY_SE
      MY_NEB(7)=MY_SW
      MY_NEB(8)=MY_NW
C----------------------------------------------------------------------
C*** 
C***  GENERATE THE TABLES (DIMENSIONED INPES*JNPES) THAT HOLD THE
C***  STARTING AND ENDING VALUES OF I AND J FOR EACH PE IN TERMS
C***  OF BOTH THE GLOBAL AND THE LOCAL DOMAINS.
C***
      CALL INDTABLE
C----------------------------------------------------------------------
C***
C***  CREATE ABBREVIATED NAMES FOR LOOP LIMITS.
C***
      MYIS    =MY_IS_LOC
      MYIS_P1 =MY_IS_LOC-ILPAD1
      MYIS_P2 =MY_IS_LOC-ILPAD2
      MYIS_P3 =MY_IS_LOC-ILPAD3
      MYIS_P4 =MY_IS_LOC-ILPAD4
C
      MYIS1   =MY_IS_LOC+IS_INC1_BND
      MYIS1_P1=MY_IS_LOC+IS_INC1_BND-ILPAD1
      MYIS1_P2=MY_IS_LOC+IS_INC1_BND-ILPAD2
      MYIS1_P3=MY_IS_LOC+IS_INC1_BND-ILPAD3
C
      MYIS2   =MY_IS_LOC+IS_INC2_BND
C***
      MYIE    =MY_IE_LOC 
      MYIE_P1 =MY_IE_LOC+IRPAD1
      MYIE_P2 =MY_IE_LOC+IRPAD2
      MYIE_P3 =MY_IE_LOC+IRPAD3
      MYIE_P4 =MY_IE_LOC+IRPAD4
C
      MYIE1   =MY_IE_LOC-IE_INC1_BND          !The size of these increments
      MYIE1_P1=MY_IE_LOC-IE_INC1_BND+IRPAD1   !is zero unless the subdomain
      MYIE1_P2=MY_IE_LOC-IE_INC1_BND+IRPAD2   !lies along a global boundary
      MYIE1_P3=MY_IE_LOC-IE_INC1_BND+IRPAD3   !in which case the increment
C                                             !is indicated by the number
      MYIE2   =MY_IE_LOC-IE_INC2_BND          !following 'INC'.
      MYIE2_P1=MY_IE_LOC-IE_INC2_BND+IRPAD1
C***
C***
      MYJS    =MY_JS_LOC
      MYJS_P1 =MY_JS_LOC-JBPAD1
      MYJS_P2 =MY_JS_LOC-JBPAD2
      MYJS_P3 =MY_JS_LOC-JBPAD3
      MYJS_P4 =MY_JS_LOC-JBPAD4
      MYJS_P5 =MY_JS_LOC-JBPAD5
C
      MYJS1   =MY_JS_LOC+JS_INC1_BND
      MYJS1_P1=MY_JS_LOC+JS_INC1_BND-JBPAD1
      MYJS1_P2=MY_JS_LOC+JS_INC1_BND-JBPAD2
      MYJS1_P3=MY_JS_LOC+JS_INC1_BND-JBPAD3
      MYJS1_P4=MY_JS_LOC+JS_INC1_BND-JBPAD4
      MYJS1_P5=MY_JS_LOC+JS_INC1_BND-JBPAD5
C
      MYJS2   =MY_JS_LOC+JS_INC2_BND
      MYJS2_P1=MY_JS_LOC+JS_INC2_BND-JBPAD1
      MYJS2_P2=MY_JS_LOC+JS_INC2_BND-JBPAD2
      MYJS2_P4=MY_JS_LOC+JS_INC2_BND-JBPAD4
      MYJS2_P5=MY_JS_LOC+JS_INC2_BND-JBPAD5
C
      MYJS3   =MY_JS_LOC+JS_INC3_BND
      MYJS3_P1=MY_JS_LOC+JS_INC3_BND-JBPAD1
      MYJS3_P4=MY_JS_LOC+JS_INC3_BND-JBPAD4
C
      MYJS4   =MY_JS_LOC+JS_INC4_BND
      MYJS4_P4=MY_JS_LOC+JS_INC4_BND-JBPAD4
C
      MYJS5   =MY_JS_LOC+JS_INC5_BND
      MYJS5_P2=MY_JS_LOC+JS_INC5_BND-JBPAD2
C***
      MYJE    =MY_JE_LOC
      MYJE_P1 =MY_JE_LOC+JTPAD1
      MYJE_P2 =MY_JE_LOC+JTPAD2
      MYJE_P3 =MY_JE_LOC+JTPAD3
      MYJE_P4 =MY_JE_LOC+JTPAD4
      MYJE_P5 =MY_JE_LOC+JTPAD5
C
      MYJE1   =MY_JE_LOC-JE_INC1_BND
      MYJE1_P1=MY_JE_LOC-JE_INC1_BND+JTPAD1
      MYJE1_P2=MY_JE_LOC-JE_INC1_BND+JTPAD2
      MYJE1_P3=MY_JE_LOC-JE_INC1_BND+JTPAD3
      MYJE1_P4=MY_JE_LOC-JE_INC1_BND+JTPAD4
      MYJE1_P5=MY_JE_LOC-JE_INC1_BND+JTPAD5
C
      MYJE2   =MY_JE_LOC-JE_INC2_BND
      MYJE2_P1=MY_JE_LOC-JE_INC2_BND+JTPAD1
      MYJE2_P2=MY_JE_LOC-JE_INC2_BND+JTPAD2
      MYJE2_P4=MY_JE_LOC-JE_INC2_BND+JTPAD4
      MYJE2_P5=MY_JE_LOC-JE_INC2_BND+JTPAD5
C
      MYJE3   =MY_JE_LOC-JE_INC3_BND 
      MYJE3_P1=MY_JE_LOC-JE_INC3_BND+JTPAD1
      MYJE3_P4=MY_JE_LOC-JE_INC3_BND+JTPAD4
      MYJE3_P5=MY_JE_LOC-JE_INC3_BND+JTPAD5
C
      MYJE4   =MY_JE_LOC-JE_INC4_BND
      MYJE4_P4=MY_JE_LOC-JE_INC4_BND+JTPAD4
      MYJE4_P5=MY_JE_LOC-JE_INC4_BND+JTPAD5
C
      MYJE5   =MY_JE_LOC-JE_INC5_BND
      MYJE5_P2=MY_JE_LOC-JE_INC5_BND+JTPAD2
!         print *,' in mppinit, mype,ilcol,ircol,ibrow,itrow=',
!    *             mype,ilcol,ircol,ibrow,itrow
C
C-----------------------------------------------------------------
C*****************************************************************
      RETURN
      END
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
C-----------------------------------------------------------------
                      SUBROUTINE INDTABLE 
C-----------------------------------------------------------------
C***
C***  THIS ROUTINE GENERATES THE TABLES THAT WILL GIVE THE 
C***  STARTING AND ENDING VALUES OF I AND J FOR EACH PE ON
C***  THE GLOBAL AND LOCAL DOMAINS.  EACH PE WILL HAVE A COPY
C***  OF THE FULL TABLES.  THE ARGUMENT USED IS SIMPLY THE
C***  NUMBER OF THE PE FOR WHICH THESE VALUES ARE DESIRED.
C***
C-----------------------------------------------------------------
      INCLUDE "PARMETA.comm"
      INCLUDE "mpif.h"
      include "my_comm.h"
      INCLUDE "mpp.h"
C-----------------------------------------------------------------
C
      IS_LOC_TABLE(MYPE)=MY_IS_LOC
      JS_LOC_TABLE(MYPE)=MY_JS_LOC
      IE_LOC_TABLE(MYPE)=MY_IE_LOC
      JE_LOC_TABLE(MYPE)=MY_JE_LOC
C
      IS_GLB_TABLE(MYPE)=MY_IS_GLB
      IE_GLB_TABLE(MYPE)=MY_IE_GLB
      JS_GLB_TABLE(MYPE)=MY_JS_GLB
      JE_GLB_TABLE(MYPE)=MY_JE_GLB
C
      DO IPE=0,NPES-1
        CALL MPI_BCAST(IS_LOC_TABLE(IPE),1,MPI_INTEGER,IPE,
     1                 my_comm,IRTN)
        CALL MPI_BCAST(JS_LOC_TABLE(IPE),1,MPI_INTEGER,IPE,
     1                 my_comm,IRTN)
        CALL MPI_BCAST(IE_LOC_TABLE(IPE),1,MPI_INTEGER,IPE,
     1                 my_comm,IRTN)
        CALL MPI_BCAST(JE_LOC_TABLE(IPE),1,MPI_INTEGER,IPE,
     1                 my_comm,IRTN)
C
        CALL MPI_BCAST(IS_GLB_TABLE(IPE),1,MPI_INTEGER,IPE,
     1                 my_comm,IRTN)
        CALL MPI_BCAST(JS_GLB_TABLE(IPE),1,MPI_INTEGER,IPE,
     1                 my_comm,IRTN)
        CALL MPI_BCAST(IE_GLB_TABLE(IPE),1,MPI_INTEGER,IPE,
     1                 my_comm,IRTN)
        CALL MPI_BCAST(JE_GLB_TABLE(IPE),1,MPI_INTEGER,IPE,
     1                 my_comm,IRTN)
      ENDDO
C
      CALL MPI_BARRIER(my_comm,IRTN)
C------------------------------------------------------------------
C***
C***  ALL OF THE PEs CAN NOW GENERATE A COMPLETE TABLE OF THE 
C***  NUMBER OF GRID POINTS IN THE I DIRECTION THAT ARE ON
C***  ALL OTHER PEs.  THIS WILL BE USED IN THE MESINGER MSLP
C***  REDUCTION AS WELL AS IN THE BROADCAST BELOW.
C***
      DO IPE=0,NPES-1
        ICHUNKTAB(IPE)=IE_LOC_TABLE(IPE)-IS_LOC_TABLE(IPE)+1
      ENDDO
C***
C***  SET UP A MAP OF THE GLOBAL DOMAIN THAT GIVES THE PE THAT
C***  OWNS EACH POINT.
C***  (THIS APPEARS TO BE VESTIGIAL)
C***
C
C***  FIRST EACH PE FILLS IN ITS SECTION OF THE ARRAY
C
      DO JGLB=JS_GLB_TABLE(MYPE),JE_GLB_TABLE(MYPE)
      DO IGLB=IS_GLB_TABLE(MYPE),IE_GLB_TABLE(MYPE)
        ITEMP(IGLB,JGLB)=MYPE
      ENDDO
      ENDDO
C
C***  NEXT, ALL PEs EXCHANGE THEIR SECTIONS SO EVERYONE HAS 
C***  A FULL MAP
C
      DO IPE=0,NPES-1
      DO JGLB=JS_GLB_TABLE(IPE),JE_GLB_TABLE(IPE)
        CALL MPI_BCAST(ITEMP(IS_GLB_TABLE(IPE),JGLB),ICHUNKTAB(IPE)
     1,                MPI_INTEGER,IPE,my_comm,IRECV)
      ENDDO
      ENDDO
C
      CALL MPI_BARRIER(my_comm,IRTN)
C********************************************************************
       RETURN
       END

