C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
                             SUBROUTINE BOCOoutan
C     ******************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .     
C SUBPROGRAM:    BOCOoutan   generate boundary lookalike file NBC
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
      INCLUDE "mpif.h"
      include "my_comm.h"
      INCLUDE "PARMETA.comm"
      INCLUDE "PARMTBL.comm"
      INCLUDE "mpp.h"
!#include "sp.h"
C-----------------------------------------------------------------------
!                            P A R A M E T E R
!    & (IMJM=IM*JM-JM/2,LB=2*IM+JM-3,LP1=LM+1)
!                            P A R A M E T E R
!    & (ISIZ1=2*LB,ISIZ2=2*LB*LM)
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
      INTEGER ISTAT(MPI_STATUS_SIZE)
C***********************************************************************
      IIM=IM-MY_IS_GLB+1
      JJM=JM-MY_JS_GLB+1

         test_bmissing=.1*huge(test_bmissing)
         pdb(:,1)=test_bmissing
         tb(:,:,1)=test_bmissing
         qb(:,:,1)=test_bmissing
         ub(:,:,1)=test_bmissing
         vb(:,:,1)=test_bmissing

C--------------------------------------------------------------
C***
C***  generate surface pressure boundary
C***
C--------------------------------------------------------------

      N=1
      DO 101 I=1,IM
      IF(MY_JS_GLB.EQ.1.AND.I.GE.MY_IS_GLB-ILPAD1.
     1                  AND.I.LE.MY_IE_GLB+IRPAD1)THEN
        II=I-MY_IS_GLB+1
        PDB(N,1)=pd(ii,1)
      ENDIF
      N=N+1
  101 CONTINUE
C
      DO 102 I=1,IM
      IF(MY_JE_GLB.EQ.JM.AND.I.GE.MY_IS_GLB-ILPAD1.
     1                   AND.I.LE.MY_IE_GLB+IRPAD1)THEN
        II=I-MY_IS_GLB+1
        PDB(N,1)=pd(ii,jjm)
      ENDIF
      N=N+1
  102 CONTINUE
C
      DO 103 J=3,JM-2,2
      IF(MY_IS_GLB.EQ.1.AND.J.GE.MY_JS_GLB-JBPAD1.
     1                  AND.J.LE.MY_JE_GLB+JTPAD1)THEN
        JJ=J-MY_JS_GLB+1
        PDB(N,1)=pd(1,jj)
      ENDIF
      N=N+1
  103 CONTINUE
C
      DO 104 J=3,JM-2,2
      IF(MY_IE_GLB.EQ.IM.AND.J.GE.MY_JS_GLB-JBPAD1.
     1                   AND.J.LE.MY_JE_GLB+JTPAD1)THEN
        JJ=J-MY_JS_GLB+1
        PDB(N,1)=pd(iim,jj)
      ENDIF
      N=N+1
  104 CONTINUE
C--------------------------------------------------------------
C***
C***  generate t and q boundaries
C***
C--------------------------------------------------------------
                             DO 115 L=1,LM
C--------------------------------------------------------------
      N=1
      DO 111 I=1,IM
      IF(MY_JS_GLB.EQ.1.AND.I.GE.MY_IS_GLB-ILPAD1.
     1                  AND.I.LE.MY_IE_GLB+IRPAD1)THEN
        II=I-MY_IS_GLB+1
        TB(N,L,1)=t(ii,1,l)
        QB(N,L,1)=q(ii,1,l)
      ENDIF
      N=N+1
  111 CONTINUE
C
      DO 112 I=1,IM
      IF(MY_JE_GLB.EQ.JM.AND.I.GE.MY_IS_GLB-ILPAD1.
     1               AND.I.LE.MY_IE_GLB+IRPAD1)THEN
        II=I-MY_IS_GLB+1
        TB(N,L,1)=t(ii,jjm,l)
        QB(N,L,1)=q(ii,jjm,l)
      ENDIF
      N=N+1
  112 CONTINUE
C
      DO 113 J=3,JM-2,2
      IF(MY_IS_GLB.EQ.1.AND.J.GE.MY_JS_GLB-JBPAD1.
     1                  AND.J.LE.MY_JE_GLB+JTPAD1)THEN
        JJ=J-MY_JS_GLB+1
        TB(N,L,1)=t(1,jj,l)
        QB(N,L,1)=q(1,jj,l)
      ENDIF
      N=N+1
  113 CONTINUE
C
      DO 114 J=3,JM-2,2
      IF(MY_IE_GLB.EQ.IM.AND.J.GE.MY_JS_GLB-JBPAD1.
     1                   AND.J.LE.MY_JE_GLB+JTPAD1)THEN
        JJ=J-MY_JS_GLB+1
        TB(N,L,1)=t(iim,jj,l)
        QB(N,L,1)=q(iim,jj,l)
      ENDIF
      N=N+1
  114 CONTINUE
C
  115 CONTINUE
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C------------- generate boundary u and v
C-----------------------------------------------------------------------
                             DO 1150 L=1,LM
C-----------------------------------------------------------------------
      N=1
      DO 1110 I=1,IM-1
      IF(MY_JS_GLB.EQ.1.AND.I.GE.MY_IS_GLB-ILPAD1.
     1                  AND.I.LE.MY_IE_GLB+IRPAD1)THEN
        II=I-MY_IS_GLB+1
        UB(N,L,1)=u(ii,1,l)
        VB(N,L,1)=v(ii,1,l)
      ENDIF
      N=N+1
 1110 CONTINUE
C
      DO 1120 I=1,IM-1
      IF(MY_JE_GLB.EQ.JM.AND.I.GE.MY_IS_GLB-ILPAD1.
     1                   AND.I.LE.MY_IE_GLB+IRPAD1)THEN
        II=I-MY_IS_GLB+1
        UB(N,L,1)=u(ii,jjm,l)
        VB(N,L,1)=v(ii,jjm,l)
      ENDIF
      N=N+1
 1120 CONTINUE
C
      DO 1130 J=2,JM-1,2
      IF(MY_IS_GLB.EQ.1.AND.J.GE.MY_JS_GLB-JBPAD1.
     1                  AND.J.LE.MY_JE_GLB+JTPAD1)THEN
        JJ=J-MY_JS_GLB+1
        UB(N,L,1)=u(1,jj,l)
        VB(N,L,1)=v(1,jj,l)
      ENDIF
      N=N+1
 1130 CONTINUE
C
      DO 1140 J=2,JM-1,2
      IF(MY_IE_GLB.EQ.IM.AND.J.GE.MY_JS_GLB-JBPAD1.
     1                   AND.J.LE.MY_JE_GLB+JTPAD1)THEN
        JJ=J-MY_JS_GLB+1
        UB(N,L,1)=u(iim,jj,l)
        VB(N,L,1)=v(iim,jj,l)
      ENDIF
      N=N+1
 1140 CONTINUE
 1150                        CONTINUE

!--- gather together all the pieces and fill in the holes

         if(mype.ne.0) then
          call mpi_send(pdb(1,1),lb,mpi_real,0,mype,
     *                  my_comm,isend)
         else
          do ipe=1,npes-1
           call mpi_recv(pdb(1,2),lb,mpi_real,ipe,ipe,
     *                  my_comm,istat,irecv)
           do i=1,lb
            if(pdb(i,2).ne.test_bmissing) pdb(i,1)=pdb(i,2)
           end do
          end do
         end if

         if(mype.ne.0) then
          call mpi_send(tb(1,1,1),lb*lm,mpi_real,0,mype,
     *                  my_comm,isend)
         else
          do ipe=1,npes-1
           call mpi_recv(tb(1,1,2),lb*lm,mpi_real,ipe,ipe,
     *                  my_comm,istat,irecv)
           do i=1,lb
            if(tb(i,1,2).ne.test_bmissing) tb(i,:,1)=tb(i,:,2)
           end do
          end do
         end if

         if(mype.ne.0) then
          call mpi_send(qb(1,1,1),lb*lm,mpi_real,0,mype,
     *                  my_comm,isend)
         else
          do ipe=1,npes-1
           call mpi_recv(qb(1,1,2),lb*lm,mpi_real,ipe,ipe,
     *                  my_comm,istat,irecv)
           do i=1,lb
            if(qb(i,1,2).ne.test_bmissing) qb(i,:,1)=qb(i,:,2)
           end do
          end do
         end if

         if(mype.ne.0) then
          call mpi_send(ub(1,1,1),lb*lm,mpi_real,0,mype,
     *                  my_comm,isend)
         else
          do ipe=1,npes-1
           call mpi_recv(ub(1,1,2),lb*lm,mpi_real,ipe,ipe,
     *                  my_comm,istat,irecv)
           do i=1,lb
            if(ub(i,1,2).ne.test_bmissing) ub(i,:,1)=ub(i,:,2)
           end do
          end do
         end if

         if(mype.ne.0) then
          call mpi_send(vb(1,1,1),lb*lm,mpi_real,0,mype,
     *                  my_comm,isend)
         else
          do ipe=1,npes-1
           call mpi_recv(vb(1,1,2),lb*lm,mpi_real,ipe,ipe,
     *                  my_comm,istat,irecv)
           do i=1,lb
            if(vb(i,1,2).ne.test_bmissing) vb(i,:,1)=vb(i,:,2)
           end do
          end do
         end if

!---------------------------------
!-----  write out NBC file
!---------------------------------
       if(mype.eq.0) then
        pdb(:,2)=0.
        tb(:,:,2)=0.
        qb(:,:,2)=0.
        ub(:,:,2)=0.
        vb(:,:,2)=0.
        NBC=53
          rewind nbc
        TBOCO = 6.0
        WRITE(NBC) RUN,IDAT,IHRST,TBOCO
        WRITE(NBC) PDB,TB,QB,UB,VB
        WRITE(NBC) PDB,TB,QB,UB,VB
        WRITE(NBC) PDB,TB,QB,UB,VB
          close(nbc)
       end if

                             RETURN
                             END
