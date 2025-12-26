C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
                            SUBROUTINE HZADV2
C     ******************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .
C SUBPROGRAM:    HZADV2      HORIZONTAL ADVECTION OF VAPOR AND CLOUD
C   PRGRMMR: JANJIC          ORG: W/NP22     DATE: 96-07-19
C
C ABSTRACT:
C     HZADV2 CALCULATES THE CONTRIBUTION OF THE HORIZONTAL ADVECTION
C     TO THE TENDENCIES OF SPECIFIC HUMIDITY AND CLOUD WATER AND
C     THEN UPDATES THOSE VARIABLES.  AN ANTI-FILTERING TECHNIQUE 
C     IS USED.
C
C PROGRAM HISTORY LOG:
C   96-07-19  JANJIC   - ORIGINATOR
C   98-11-02  BLACK    - MODIFIED FOR DISTRIBUTED MEMORY
C   99-03-17  TUCCILLO - INCORPORATED MPI_ALLREDUCE FOR GLOBAL SUM
C
C USAGE: CALL HZADV1 FROM MAIN PROGRAM EBU
C   INPUT ARGUMENT LIST:
C       NONE
C
C   OUTPUT ARGUMENT LIST
C       NONE
C
C   OUTPUT FILES:
C       NONE
C   SUBPROGRAMS CALLED:
C
C     UNIQUE: NONE
C
C     LIBRARY: NONE
C
C   COMMON BLOCKS: CTLBLK
C                  LOOPS
C                  MASKS
C                  DYNAM
C                  CONTIN
C                  VRBLS
C                  PVRBLS
C                  CLDWTR
C                  INDX
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE : IBM SP
C$$$
C***********************************************************************
                             P A R A M E T E R
     &(EPSQ=2.E-12
     &,FF1=0.52500,FF2=-0.64813,FF3=0.24520,FF4=-0.12189)
C-----------------------------------------------------------------------
      INCLUDE "PARMETA.comm"
      INCLUDE "mpp.h"
      INCLUDE "mpif.h"
      include "my_comm.h"
!#include "sp.h"
C-----------------------------------------------------------------------
!                            P A R A M E T E R
!    & (IM1=IM-1,JAM=6+2*(JM-10)
!    &, IMJM=IM*JM-JM/2
!    &, JAMD=(JAM*2-10)*3,LP1=LM+1) 
C-----------------------------------------------------------------------
                             L O G I C A L
     & RUN,FIRST,RESTRT,SIGMA
C-----------------------------------------------------------------------
      INCLUDE "CTLBLK.comm"
C-----------------------------------------------------------------------
      INCLUDE "LOOPS.comm"
C-----------------------------------------------------------------------
      INCLUDE "MASKS.comm"
C-----------------------------------------------------------------------
      INCLUDE "DYNAM.comm"
C-----------------------------------------------------------------------
      INCLUDE "CONTIN.comm"
C-----------------------------------------------------------------------
      INCLUDE "VRBLS.comm"
C-----------------------------------------------------------------------
      INCLUDE "PVRBLS.comm"
C-----------------------------------------------------------------------
      INCLUDE "CLDWTR.comm"
C-----------------------------------------------------------------------
      INCLUDE "INDX.comm"
C-----------------------------------------------------------------------
                             D I M E N S I O N
     & IFPA(IDIM1:IDIM2,JDIM1:JDIM2,LM),IFQA(IDIM1:IDIM2,JDIM1:JDIM2,LM) 
     &,IFPF(IDIM1:IDIM2,JDIM1:JDIM2,LM),IFQF(IDIM1:IDIM2,JDIM1:JDIM2,LM)
     &,JFPA(IDIM1:IDIM2,JDIM1:JDIM2,LM),JFQA(IDIM1:IDIM2,JDIM1:JDIM2,LM)
     &,JFPF(IDIM1:IDIM2,JDIM1:JDIM2,LM),JFQF(IDIM1:IDIM2,JDIM1:JDIM2,LM)
     &,AFP (IDIM1:IDIM2,JDIM1:JDIM2,LM),AFQ (IDIM1:IDIM2,JDIM1:JDIM2,LM)
     &,Q1  (IDIM1:IDIM2,JDIM1:JDIM2,LM),DQST(IDIM1:IDIM2,JDIM1:JDIM2,LM)
     &,W1  (IDIM1:IDIM2,JDIM1:JDIM2,LM),DWST(IDIM1:IDIM2,JDIM1:JDIM2,LM)
     &,DARE(IDIM1:IDIM2,JDIM1:JDIM2),   DVOL(IDIM1:IDIM2,JDIM1:JDIM2,LM)
     &,EMH (IDIM1:IDIM2,JDIM1:JDIM2)
c    &,QLIM(IDIM1:IDIM2,JDIM1:JDIM2),WLIM  (IDIM1:IDIM2,JDIM1:JDIM2)
C-----------------------------------------------------------------------
                             R E A L
     & GSUMS(4,LM),XSUMS(4,LM)

C-----------------------------------------------------------------------
                             I N T E G E R
     & ISTAT(MPI_STATUS_SIZE)
C
C***********************************************************************
      ENH=FLOAT(IDTAD)*DT/(08.*DY)
C
      DO J=MYJS_P2,MYJE_P2
      DO I=MYIS_P1,MYIE_P1
        EMH (I,J)=FLOAT(IDTAD)*DT/(08.*DX(I,J))
        DARE(I,J)=HBM2(I,J)*DX(I,J)*DY
      ENDDO
      ENDDO
C
C***********************************************************************
C-----------------------------------------------------------------------
!$omp  parallel do 
!$omp& private(dqstij,dvolp,dwstij,htmijl,jfp,jfq,pp,qp,
!$omp&         sumnq,sumnw,sumpq,sumpw,tta,ttb)
C
C-----------------------------------------------------------------------
      DO L=1,LM
C-----------------------------------------------------------------------
C***********************************************************************
        DO 200 J=MYJS_P2,MYJE_P2
        DO 200 I=MYIS_P1,MYIE_P1
        DVOL(I,J,L)=DARE(I,J)*PDSL(I,J)*DETA(L)
        HTMIJL=HTM(I,J,L)
        Q  (I,J,L)=AMAX1(Q  (I,J,L),EPSQ)*HTMIJL
        CWM(I,J,L)=AMAX1(CWM(I,J,L),EPSQ)*HTMIJL
        Q1  (I,J,L)=Q  (I,J,L)
        W1  (I,J,L)=CWM(I,J,L)
  200   CONTINUE
C-----------------------------------------------------------------------
        SUMPQ=0.
        SUMNQ=0.
        SUMPW=0.
        SUMNW=0.
C
        DO 300 J=MYJS2_P1,MYJE2_P1
        DO 300 I=MYIS1_P1,MYIE1_P1
C
        DVOLP=DVOL(I,J,L)*HBM3(I,J)
        TTA=(U(I,J-1,L)+U(I+IHW(J),J,L)+U(I+IHE(J),J,L)+U(I,J+1,L))
     2     *HBM2(I,J)*EMH(I,J)
        TTB=(V(I,J-1,L)+V(I+IHW(J),J,L)+V(I+IHE(J),J,L)+V(I,J+1,L))
     2     *HBM2(I,J)*ENH
C
        PP=-TTA-TTB
        QP= TTA-TTB
C
        JFP=INT(SIGN(1.,PP))
        JFQ=INT(SIGN(1.,QP))
C
        IFPA(I,J,L)=IHE(J)+I+( JFP-1  )/2
        IFQA(I,J,L)=IHE(J)+I+(-JFQ-1  )/2
C
        JFPA(I,J,L)=       J+JFP
        JFQA(I,J,L)=       J+JFQ
C
        IFPF(I,J,L)=IHE(J)+I+(-JFP-1  )/2
        IFQF(I,J,L)=IHE(J)+I+( JFQ-1  )/2
C
        JFPF(I,J,L)=       J-JFP
        JFQF(I,J,L)=       J-JFQ
C
        PP=ABS(PP)*HTM(I,J,L)*HTM(IFPA(I,J,L),JFPA(I,J,L),L)
        QP=ABS(QP)*HTM(I,J,L)*HTM(IFQA(I,J,L),JFQA(I,J,L),L)
C
        AFP (I,J,L)=(((FF4*PP+FF3)*PP+FF2)*PP+FF1)*PP
        AFQ (I,J,L)=(((FF4*QP+FF3)*QP+FF2)*QP+FF1)*QP
C
        DQSTIJ=(Q  (IFPA(I,J,L),JFPA(I,J,L),L)-Q  (I,J,L))*PP
     2        +(Q  (IFQA(I,J,L),JFQA(I,J,L),L)-Q  (I,J,L))*QP
        DWSTIJ=(CWM(IFPA(I,J,L),JFPA(I,J,L),L)-CWM(I,J,L))*PP
     2        +(CWM(IFQA(I,J,L),JFQA(I,J,L),L)-CWM(I,J,L))*QP
C
        DQST(I,J,L)=DQSTIJ
        DWST(I,J,L)=DWSTIJ
C
  300   CONTINUE
C***
C***  GLOBAL SUM FOR CONSERVATION
C***
        DO 310 J=MYJS2,MYJE2
        DO 310 I=MYIS1,MYIE1
C
        DVOLP=DVOL(I,J,L)*HBM3(I,J)
        DQSTIJ=DQST(I,J,L)*DVOLP
        DWSTIJ=DWST(I,J,L)*DVOLP
C
        IF(DQSTIJ.GT.0.)THEN
          SUMPQ=SUMPQ+DQSTIJ
        ELSE
          SUMNQ=SUMNQ+DQSTIJ
        ENDIF
C
        IF(DWSTIJ.GT.0.)THEN
          SUMPW=SUMPW+DWSTIJ
        ELSE
          SUMNW=SUMNW+DWSTIJ
        ENDIF
C
  310   CONTINUE
C
C-----------------------------------------------------------------------
        XSUMS(1,L)=SUMPQ
        XSUMS(2,L)=SUMNQ
        XSUMS(3,L)=SUMPW
        XSUMS(4,L)=SUMNW
C
      ENDDO               ! END OF LM LOOP
C-----------------------------------------------------------------------
C
C***  GLOBAL REDUCTION
C
      CALL MPI_ALLREDUCE(XSUMS,GSUMS,4*LM,MPI_REAL,MPI_SUM,
     1                     my_comm,IRECV)
      IF(IRECV.NE.0)THEN
        PRINT*, ' RETURN CODE FROM 1st ALLREDUCE IN HZADV2 = ',IRECV
        STOP
      ENDIF
C
C***  END OF GLOBAL REDUCTION
C
C-----------------------------------------------------------------------
!$omp  parallel do 
!$omp& private(d2pqq,d2pqw,dqstij,dvolp,dwstij,
!$omp&         q00,q0q,q1ij,qp0,qstij,rfacq,rfacw,
!$omp&         rfqij,rfwij,sumnq,sumnw,sumpq,sumpw,
!$omp&         w00,w0q,w1ij,wp0,wstij)
C-----------------------------------------------------------------------
      DO L=1,LM
C-----------------------------------------------------------------------
C
        SUMPQ=GSUMS(1,L)
        SUMNQ=GSUMS(2,L)
        SUMPW=GSUMS(3,L)
        SUMNW=GSUMS(4,L)
C
C--------------FIRST MOMENT CONSERVING FACTOR---------------------------
        IF(SUMPQ.GT.1.)THEN
          RFACQ=-SUMNQ/SUMPQ
        ELSE
          RFACQ=1.
        ENDIF
C
        IF(SUMPW.GT.1.)THEN
          RFACW=-SUMNW/SUMPW
        ELSE
          RFACW=1.
        ENDIF
C
        IF(RFACQ.LT.0.9.OR.RFACQ.GT.1.1)RFACQ=1.
        IF(RFACW.LT.0.9.OR.RFACW.GT.1.1)RFACW=1.
C--------------IMPOSE CONSERVATION ON ADVECTION-------------------------
        IF(RFACQ.LT.1.)THEN
          DO J=MYJS2_P1,MYJE2_P1
          DO I=MYIS1_P1,MYIE1_P1
            DQSTIJ=DQST(I,J,L)
            RFQIJ=HBM3(I,J)*(RFACQ-1.)+1.
            IF(DQSTIJ.LT.0.)DQSTIJ=DQSTIJ/RFQIJ
            Q1(I,J,L)=Q(I,J,L)+DQSTIJ
          ENDDO
          ENDDO
        ELSE
          DO J=MYJS2_P1,MYJE2_P1
          DO I=MYIS1_P1,MYIE1_P1
            DQSTIJ=DQST(I,J,L)
            RFQIJ=HBM3(I,J)*(RFACQ-1.)+1.
            IF(DQSTIJ.GE.0.)DQSTIJ=DQSTIJ*RFQIJ
            Q1(I,J,L)=Q(I,J,L)+DQSTIJ
          ENDDO
          ENDDO
        ENDIF 
C-----------------------------------------------------------------------
        IF(RFACW.LT.1.)THEN
          DO J=MYJS2_P1,MYJE2_P1
          DO I=MYIS1_P1,MYIE1_P1
            DWSTIJ=DWST(I,J,L)
            RFWIJ=HBM3(I,J)*(RFACW-1.)+1.
            IF(DWSTIJ.LT.0.)DWSTIJ=DWSTIJ/RFWIJ
            W1(I,J,L)=CWM(I,J,L)+DWSTIJ
          ENDDO
          ENDDO
        ELSE
          DO J=MYJS2_P1,MYJE2_P1
          DO I=MYIS1_P1,MYIE1_P1
            DWSTIJ=DWST(I,J,L)
            RFWIJ=HBM3(I,J)*(RFACW-1.)+1.
            IF(DWSTIJ.GE.0.)DWSTIJ=DWSTIJ*RFWIJ
            W1(I,J,L)=CWM(I,J,L)+DWSTIJ
          ENDDO
          ENDDO
        ENDIF
C--------------ANTI-FILTERING STEP--------------------------------------
        SUMPQ=0.
        SUMNQ=0.
        SUMPW=0.
        SUMNW=0.
C--------------ANTI-FILTERING LIMITERS----------------------------------
        DO 330 J=MYJS2,MYJE2
        DO 330 I=MYIS1,MYIE1
C
        DVOLP=DVOL(I,J,L)
        Q1IJ =Q1(I,J,L)
        W1IJ =W1(I,J,L)
C
        D2PQQ=((Q1(IFPA(I,J,L),JFPA(I,J,L),L)-Q1IJ                   )
     2        -(Q1IJ                   -Q1(IFPF(I,J,L),JFPF(I,J,L),L))
     3        *HTM(IFPF(I,J,L),JFPF(I,J,L),L))*AFP(I,J,L)
     4       +((Q1(IFQA(I,J,L),JFQA(I,J,L),L)-Q1IJ                   )
     5        -(Q1IJ                   -Q1(IFQF(I,J,L),JFQF(I,J,L),L))
     6        *HTM(IFQF(I,J,L),JFQF(I,J,L),L))*AFQ(I,J,L)
C
        D2PQW=((W1(IFPA(I,J,L),JFPA(I,J,L),L)-W1IJ                   )
     2        -(W1IJ                   -W1(IFPF(I,J,L),JFPF(I,J,L),L))
     3        *HTM(IFPF(I,J,L),JFPF(I,J,L),L))*AFP(I,J,L)
     4       +((W1(IFQA(I,J,L),JFQA(I,J,L),L)-W1IJ                   )
     5        -(W1IJ                   -W1(IFQF(I,J,L),JFQF(I,J,L),L))
     6        *HTM(IFQF(I,J,L),JFQF(I,J,L),L))*AFQ(I,J,L)
C
        QSTIJ=Q1IJ-D2PQQ
        WSTIJ=W1IJ-D2PQW
C
        Q00=Q  (I          ,J          ,L)
        QP0=Q  (IFPA(I,J,L),JFPA(I,J,L),L)
        Q0Q=Q  (IFQA(I,J,L),JFQA(I,J,L),L)
C
        W00=CWM(I          ,J          ,L)
        WP0=CWM(IFPA(I,J,L),JFPA(I,J,L),L)
        W0Q=CWM(IFQA(I,J,L),JFQA(I,J,L),L)
C
        QSTIJ=AMAX1(QSTIJ,AMIN1(Q00,QP0,Q0Q))
        QSTIJ=AMIN1(QSTIJ,AMAX1(Q00,QP0,Q0Q))
        WSTIJ=AMAX1(WSTIJ,AMIN1(W00,WP0,W0Q))
        WSTIJ=AMIN1(WSTIJ,AMAX1(W00,WP0,W0Q))
C
        DQSTIJ=QSTIJ-Q1IJ
        DWSTIJ=WSTIJ-W1IJ
C
        DQST(I,J,L)=DQSTIJ
        DWST(I,J,L)=DWSTIJ
C
        DQSTIJ=DQSTIJ*DVOLP
        DWSTIJ=DWSTIJ*DVOLP
C
        IF(DQSTIJ.GT.0.)THEN
          SUMPQ =SUMPQ+DQSTIJ
        ELSE
          SUMNQ =SUMNQ+DQSTIJ
        ENDIF
C
        IF(DWSTIJ.GT.0.)THEN
          SUMPW =SUMPW+DWSTIJ
        ELSE
          SUMNW =SUMNW+DWSTIJ
        ENDIF
C
  330   CONTINUE
C-----------------------------------------------------------------------
        XSUMS(1,L)=SUMPQ
        XSUMS(2,L)=SUMNQ
        XSUMS(3,L)=SUMPW
        XSUMS(4,L)=SUMNW
C
      ENDDO               ! END OF LM LOOP
C-----------------------------------------------------------------------
C
C***  GLOBAL REDUCTION
C
      CALL MPI_ALLREDUCE(XSUMS,GSUMS,4*LM,MPI_REAL,MPI_SUM,
     1                     my_comm,IRECV)
      IF(IRECV.NE.0)THEN
         PRINT*, ' RETURN CODE FROM 2nd ALLREDUCE IN HZADV2 = ',IRECV
         STOP
      ENDIF
C
C***  END OF GLOBAL REDUCTION
C
C-----------------------------------------------------------------------
C
!$omp  parallel do
!$omp& private(dqstij,dwstij,htmijl,rfacq,rfacw,rfqij,rfwij,
!$omp&         sumnw,sumnq,sumpq,sumpw)
C-----------------------------------------------------------------------
      DO L=1,LM
C
        SUMPQ=GSUMS(1,L)
        SUMNQ=GSUMS(2,L)
        SUMPW=GSUMS(3,L)
        SUMNW=GSUMS(4,L)
C
C--------------FIRST MOMENT CONSERVING FACTOR---------------------------
        IF(SUMPQ.GT.1.)THEN
          RFACQ=-SUMNQ/SUMPQ
        ELSE
          RFACQ=1.
        ENDIF
C
        IF(SUMPW.GT.1.)THEN
          RFACW=-SUMNW/SUMPW
        ELSE
          RFACW=1.
        ENDIF
C
        IF(RFACQ.LT.0.9.OR.RFACQ.GT.1.1)RFACQ=1.
        IF(RFACW.LT.0.9.OR.RFACW.GT.1.1)RFACW=1.
C--------------IMPOSE CONSERVATION ON ANTI-FILTERING--------------------
        IF(RFACQ.LT.1.)THEN
          DO J=MYJS2,MYJE2
          DO I=MYIS1,MYIE1
            DQSTIJ=DQST(I,J,L)
            RFQIJ=HBM2(I,J)*(RFACQ-1.)+1.
            IF(DQSTIJ.GE.0.)   DQSTIJ=DQSTIJ*RFQIJ
            Q  (I,J,L)=Q1(I,J,L)+DQSTIJ
          ENDDO
          ENDDO
        ELSE
          DO J=MYJS2,MYJE2
          DO I=MYIS1,MYIE1
            DQSTIJ=DQST(I,J,L)
            RFQIJ=HBM2(I,J)*(RFACQ-1.)+1.
            IF(DQSTIJ.LT.0.)   DQSTIJ=DQSTIJ/RFQIJ
            Q  (I,J,L)=Q1(I,J,L)+DQSTIJ
          ENDDO
          ENDDO
        ENDIF
C-----------------------------------------------------------------------
        IF(RFACW.LT.1.)THEN
          DO J=MYJS2,MYJE2
          DO I=MYIS1,MYIE1
            DWSTIJ=DWST(I,J,L)
            RFWIJ=HBM2(I,J)*(RFACW-1.)+1.
            IF(DWSTIJ.GE.0.)   DWSTIJ=DWSTIJ*RFWIJ
            CWM(I,J,L)=W1(I,J,L)+DWSTIJ
          ENDDO
          ENDDO
        ELSE
          DO J=MYJS2,MYJE2
          DO I=MYIS1,MYIE1
            DWSTIJ=DWST(I,J,L)
            RFWIJ=HBM2(I,J)*(RFACW-1.)+1.
            IF(DWSTIJ.LT.0.)   DWSTIJ=DWSTIJ/RFWIJ
            CWM(I,J,L)=W1(I,J,L)+DWSTIJ
          ENDDO
          ENDDO
        ENDIF
C
C-----------------------------------------------------------------------
C
        DO J=MYJS,MYJE
        DO I=MYIS,MYIE
          HTMIJL=HTM(I,J,L)
          Q  (I,J,L)=AMAX1(Q  (I,J,L),EPSQ)*HTMIJL
          CWM(I,J,L)=AMAX1(CWM(I,J,L),EPSQ)*HTMIJL
        ENDDO
        ENDDO
C-----------------------------------------------------------------------
C
      ENDDO       ! END OF LM LOOP
C
C-----------------------------------------------------------------------
                             RETURN
                             END
