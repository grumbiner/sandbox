CFPP$ NOCONCUR R
      SUBROUTINE FFT777(A,W,TRIGS,IFAX,INC,JUMP,N,LOT,ISIGN)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    FFT777      PERFORMS MULTIPLE CRAY FFTS.
C   PRGMMR: JOSEPH SELA      ORG: W/NMC23    DATE: 91-03-11
C
C ABSTRACT: PERFORMS MULTIPLE CRAY FAST FOURIER TRANSFORMS.
C
C PROGRAM HISTORY LOG:
C   86-12-28  CRAY RESEARCH
C
C USAGE:    CALL FFT777(A,W,TRIGS,IFAX,INC,JUMP,N,LOT,ISIGN)
C
C REMARKS:  CRAY DOCUMENTATION FOLLOWS.
C-----------------------------------------------------------------------
C
C     MULTIPLE REAL FAST FOURIER TRANSFORM FOR X-MP  (12-28-86)
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C
C     CALL QPASS/RPASS - NONCYCLIC.
C
C-----------------------------------------------------------------------
C
C     INPUT:
C     -----
C
C     A(*)     CONTAINS THE DATA TO BE TRANSFORMED.
C     WORK(*)  IS A WORK AREA; MINIMUM SIZE = 2*N*MIN(LOT,64).
C     TRIGS(*) CONTAINS THE TRIG FUNCTION VALUES; MINUMUM SIZE = 2*N.
C     IFAX(*)  CONTAINS THE FACTORS OF N.
C     INC      IS THE INCREMENT WITHIN EACH DATA VECTOR.
C     JUMP     IS THE INCREMENT BETWEEN THE START OF EACH DATA VECTOR.
C     N        IS THE LENGTH OF THE DATA VECTORS.
C     LOT      IS THE NUMBER OF DATA VECTORS.
C     ISIGN    = +1 FOR TRANSFORM FROM SPECTRAL TO GRIDPOINT.
C              = -1 FOR TRANSFORM FROM GRIDPOINT TO SPECTRAL.
C
C-----------------------------------------------------------------------
C
C     OUTPUT:
C     ------
C
C     A(*)     CONTAINS THE TRANSFORMED DATA
C     WORK(*)  IS UNDEFINED
C
C     ALL OTHER ARGUMENTS ARE UNCHANGED
C
C-----------------------------------------------------------------------
C
C     COMMENTS:
C     --------
C
C     TRIGS(*) AND IFAX(*) CAN BE INITIALIZED BY:
C              CALL SET777(TRIGS,IFAX,N) .
C
C     N MUST BE GREATER THAN ONE AND FACTORIZABLE:
C              N = (2**P) * (3**Q) * (4**R) * (5**S) * (6**T) * (8) ,
C     WHERE P, Q, R, S AND T ARE INTEGERS.
C
C     A(*) SHOULD BE DIMENSIONED (N+2)*LOT OR GREATER.
C
C     WHEN ISIGN=-1, THE REAL (GRIDPOINT) INPUT VALUES FOR EACH DATA
C     VECTOR,
C              A(0),A(1),A(2), ... ,A(N-1),A(N),A(N+1)
C     SHOULD BE STORED IN ARRAY A(*) WITH SKIP INCREMENT=INC.  THE LAST
C     TWO VALUES HAVE NO MEANING AND MAY BE UNDEFINED.  THE COMPUTED
C     OUTPUT VECTOR IS
C              B(0),C(0),B(1),C(1), ... ,B(N/2),C(N/2)
C     WHERE (B(I),C(I)) FORMS THE I-TH FOURIER (SPECTRAL) COEFFICIENT.
C     WHEN ISIGN=+1, THE INPUT AND OUTPUT DATA FORMATS ARE REVERSED.
C
C     THE SKIP INCREMENTS, INC AND JUMP, SHOULD BE DEFINED IN TERMS OF
C     THE REAL (GRIDPOINT) DATA VECTORS.
C
C     VECTORIZATION IS ACHIEVED BY DOING THE TRANSFORMS IN PARALLEL,
C     WITH VECTOR LENGTH = MIN(LOT,64).  BEST PERFORMANCE IS OBTAINED
C     WHEN THE SKIP INCREMENT, JUMP, IS A GOOD MEMORY STRIDE ON THE
C     X-MP.
C
C-----------------------------------------------------------------------
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN, CFT77.
C   MACHINE:  CRAY Y-MP.
C
C$$$
      DIMENSION A(1),W(1),TRIGS(2*N),IFAX(13)
      DATA LDOMAX /64/
      IB=1
      IC=1+LDOMAX*N
      NFAX=IFAX(1)
      IF(ISIGN.EQ.+1) GO TO 100
C     ------------------------------------------------------------------
C     REAL-TO-COMPLEX TRANSFORM (ISIGN=-1)
C     ------------------------------------------------------------------
      IA=1
      DO 90 M=1,LOT,LDOMAX
        LDO=MIN(LOT-M+1,LDOMAX)
        K=1+NFAX
        LA=N/IFAX(K)
        IF(K.EQ.2) THEN
          IW=0
          DO 20 J=1,N*INC,INC
          JA=J-1
          DO 10 L=1,LDO
          W(IW+L)=A(IA+JA)
          JA=JA+JUMP
   10     CONTINUE
          IW=IW+LDO
   20     CONTINUE
          CALL QPASS(W(1),A(IA),TRIGS,
     *               LDO,INC,1,JUMP,LDO,N,IFAX(K),LA,IERR)
C         IF(IERR.GT.0) GO TO 200
          GO TO 50
        ENDIF
C       ----------------------------------------------------------------
        CALL QPASS(A(IA),W(IB),TRIGS,
     *             INC,LDO,JUMP,1,LDO,N,IFAX(K),LA,IERR)
C       IF(IERR.GT.0) GO TO 200
        IF(K.EQ.3) GO TO 40
C       ----------------------------------------------------------------
   30   K=K-1
        LA=LA/IFAX(K)
        CALL QPASS(W(IB),W(IC),TRIGS,
     *             LDO,LDO,1,1,LDO,N,IFAX(K),LA,IERR)
C       IF(IERR.GT.0) GO TO 200
        ID=IB
        IB=IC
        IC=ID
        IF(K.NE.3) GO TO 30
C       ----------------------------------------------------------------
   40   K=K-1
        LA=LA/IFAX(K)
        CALL QPASS(W(IB),A(IA),TRIGS,
     *             LDO,INC,1,JUMP,LDO,N,IFAX(K),LA,IERR)
C       IF(IERR.GE.0) GO TO 200
C       ----------------------------------------------------------------
   50   IA=IA+LDO*JUMP
   90 CONTINUE
      RETURN
C     ------------------------------------------------------------------
C     COMPLEX-TO-REAL TRANSFORM (ISIGN=-1)
C     ------------------------------------------------------------------
  100 IA=1
      DO 190 M=1,LOT,LDOMAX
        LDO=MIN(LOT-M+1,LDOMAX)
        K=2
        LA=1
        IF(NFAX.EQ.1) THEN
          CALL RPASS(A(IA),W(1),TRIGS,
     *               INC,LDO,JUMP,1,LDO,N,IFAX(K),LA,IERR)
C         IF(IERR.GT.0) GO TO 200
          IW=0
          DO 120 J=1,N*INC,INC
          JA=J-1
          DO 110 L=1,LDO
          A(IA+JA)=W(IW+L)
          JA=JA+JUMP
  110     CONTINUE
          IW=IW+LDO
  120     CONTINUE
          GO TO 150
        ENDIF
C       ----------------------------------------------------------------
        CALL RPASS(A(IA),W(IB),TRIGS,
     *             INC,LDO,JUMP,1,LDO,N,IFAX(K),LA,IERR)
C       IF(IERR.GT.0) GO TO 200
        IF(NFAX.EQ.2) GO TO 140
C       ----------------------------------------------------------------
  130   LA=LA*IFAX(K)
        K=K+1
        CALL RPASS(W(IB),W(IC),TRIGS,
     *             LDO,LDO,1,1,LDO,N,IFAX(K),LA,IERR)
C       IF(IERR.GT.0) GO TO 200
        ID=IB
        IB=IC
        IC=ID
        IF(K.LT.NFAX) GO TO 130
C       ----------------------------------------------------------------
  140   LA=LA*IFAX(K)
        K=K+1
        CALL RPASS(W(IB),A(IA),TRIGS,
     *             LDO,INC,1,JUMP,LDO,N,IFAX(K),LA,IERR)
C       IF(IERR.GT.0) GO TO 200
C       ----------------------------------------------------------------
C       FILL IN ZEROS AT END
  150   JA=IA+INC*N
CDIR$ IVDEP
        DO 160 L=1,LDO
        A(JA)=0.0
        A(JA+INC)=0.0
        JA=JA+JUMP
  160   CONTINUE
        IA=IA+LDO*JUMP
  190 CONTINUE
      RETURN
C     ------------------------------------------------------------------
  200 CONTINUE
C     STOP 'FFT777'
      RETURN
C     ------------------------------------------------------------------
      END
