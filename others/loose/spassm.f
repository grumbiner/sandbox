C
      SUBROUTINE SPASSM(A,B,C,D,TRIGS,INC1,INC2,INC3,INC4,LOT,N,IFAC,LA,
     $                  W1,W2,W3,W4,W5,W6)
C     Mark Iredell 2 May 1995
C
C
C*    IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION A(N),B(N),C(N),D(N),TRIGS(N)
      DIMENSION W1(LOT),W2(LOT),W3(LOT),W4(LOT),W5(LOT),W6(LOT)
C*    DATA SIN60/0.866025403784437D0/
      DATA SIN60/0.8660254          /
C
      M=N/IFAC
      IINK=M*INC1
      JINK=LA*INC2
      JUMP=(IFAC-1)*JINK
      IBASE=0
      JBASE=0
      IGO=IFAC-1
      IF(IGO.GT.4) RETURN
      GO TO (10,50,90),IGO
C
   10 CONTINUE
      IA=1
      JA=1
      IB=IA+IINK
      JB=JA+JINK
      DO 20 L=1,LA
      I=IBASE
      J=JBASE
      DO 15 IJK=1,LOT
      C(JA+J)=A(IA+I)+A(IB+I)
      D(JA+J)=B(IA+I)+B(IB+I)
      I=I+INC3
      J=J+INC4
   15 CONTINUE
      I=IBASE
      J=JBASE
      DO 16 IJK=1,LOT
      C(JB+J)=A(IA+I)-A(IB+I)
      D(JB+J)=B(IA+I)-B(IB+I)
      I=I+INC3
      J=J+INC4
   16 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
   20 CONTINUE
      IF(LA.EQ.M) RETURN
      LA1=LA+1
      JBASE=JBASE+JUMP
      DO 40 K=LA1,M,LA
      KB=K+K-2
      C1=TRIGS(KB+1)
      S1=TRIGS(KB+2)
      DO 30 L=1,LA
      I=IBASE
      J=JBASE
      DO 25 IJK=1,LOT
      C(JA+J)=A(IA+I)+A(IB+I)
      D(JA+J)=B(IA+I)+B(IB+I)
      I=I+INC3
      J=J+INC4
   25 CONTINUE
      I=IBASE
      DO 26 IJK=1,LOT
      W1(IJK)=A(IA+I)-A(IB+I)
      W2(IJK)=B(IA+I)-B(IB+I)
      I=I+INC3
   26 CONTINUE
      J=JBASE
      DO 27 IJK=1,LOT
      C(JB+J)=C1*W1(IJK)
      D(JB+J)=S1*W1(IJK)
      J=J+INC4
   27 CONTINUE
      J=JBASE
      DO 28 IJK=1,LOT
      C(JB+J)=C(JB+J)-S1*W2(IJK)
      D(JB+J)=D(JB+J)+C1*W2(IJK)
      J=J+INC4
   28 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
   30 CONTINUE
      JBASE=JBASE+JUMP
   40 CONTINUE
      RETURN
C
   50 CONTINUE
      IA=1
      JA=1
      IB=IA+IINK
      JB=JA+JINK
      IC=IB+IINK
      JC=JB+JINK
      DO 60 L=1,LA
      I=IBASE
      DO 55 IJK=1,LOT
      W1(IJK)=A(IB+I)+A(IC+I)
      W2(IJK)=B(IB+I)+B(IC+I)
      W3(IJK)=A(IB+I)-A(IC+I)
      W4(IJK)=B(IB+I)-B(IC+I)
      I=I+INC3
   55 CONTINUE
      DO 51 IJK=1,LOT
      W3(IJK)=SIN60*W3(IJK)
      W4(IJK)=SIN60*W4(IJK)
   51 CONTINUE
      I=IBASE
      J=JBASE
      DO 56 IJK=1,LOT
      C(JA+J)=A(IA+I)+W1(IJK)
      D(JA+J)=B(IA+I)+W2(IJK)
      I=I+INC3
      J=J+INC4
   56 CONTINUE
      DO 57 IJK=1,LOT
C*    W1(IJK)=0.5D0*W1(IJK)
C*    W2(IJK)=0.5D0*W2(IJK)
      W1(IJK)=0.5  *W1(IJK)
      W2(IJK)=0.5  *W2(IJK)
   57 CONTINUE
      I=IBASE
      DO 52 IJK=1,LOT
      W1(IJK)=A(IA+I)-W1(IJK)
      W2(IJK)=B(IA+I)-W2(IJK)
      I=I+INC3
   52 CONTINUE
      J=JBASE
      DO 58 IJK=1,LOT
      C(JB+J)=W1(IJK)-W4(IJK)
      D(JB+J)=W2(IJK)+W3(IJK)
      J=J+INC4
   58 CONTINUE
      J=JBASE
      DO 59 IJK=1,LOT
      C(JC+J)=W1(IJK)+W4(IJK)
      D(JC+J)=W2(IJK)-W3(IJK)
      J=J+INC4
   59 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
   60 CONTINUE
      IF(LA.EQ.M) RETURN
      LA1=LA+1
      JBASE=JBASE+JUMP
      DO 80 K=LA1,M,LA
      KB=K+K-2
      KC=KB+KB
      C1=TRIGS(KB+1)
      S1=TRIGS(KB+2)
      C2=TRIGS(KC+1)
      S2=TRIGS(KC+2)
      DO 70 L=1,LA
      I=IBASE
      DO 61 IJK=1,LOT
      W1(IJK)=A(IB+I)+A(IC+I)
      W2(IJK)=B(IB+I)+B(IC+I)
      W3(IJK)=A(IB+I)-A(IC+I)
      W4(IJK)=B(IB+I)-B(IC+I)
      I=I+INC3
   61 CONTINUE
      DO 71 IJK=1,LOT
      W3(IJK)=SIN60*W3(IJK)
      W4(IJK)=SIN60*W4(IJK)
   71 CONTINUE
      I=IBASE
      J=JBASE
      DO 62 IJK=1,LOT
      C(JA+J)=A(IA+I)+W1(IJK)
      D(JA+J)=B(IA+I)+W2(IJK)
      I=I+INC3
      J=J+INC4
   62 CONTINUE
      DO 63 IJK=1,LOT
C*    W1(IJK)=0.5D0*W1(IJK)
C*    W2(IJK)=0.5D0*W2(IJK)
      W1(IJK)=0.5  *W1(IJK)
      W2(IJK)=0.5  *W2(IJK)
   63 CONTINUE
      I=IBASE
      DO 72 IJK=1,LOT
      W1(IJK)=A(IA+I)-W1(IJK)
      W2(IJK)=B(IA+I)-W2(IJK)
      I=I+INC3
   72 CONTINUE
      DO 64 IJK=1,LOT
      W5(IJK)=W1(IJK)-W4(IJK)
      W6(IJK)=W2(IJK)+W3(IJK)
   64 CONTINUE
      J=JBASE
      DO 65 IJK=1,LOT
      C(JB+J)=C1*W5(IJK)
      D(JB+J)=S1*W5(IJK)
      J=J+INC4
   65 CONTINUE
      J=JBASE
      DO 68 IJK=1,LOT
      C(JB+J)=C(JB+J)-S1*W6(IJK)
      D(JB+J)=D(JB+J)+C1*W6(IJK)
      J=J+INC4
   68 CONTINUE
      DO 66 IJK=1,LOT
      W5(IJK)=W1(IJK)+W4(IJK)
      W6(IJK)=W2(IJK)-W3(IJK)
   66 CONTINUE
      J=JBASE
      DO 67 IJK=1,LOT
      C(JC+J)=C2*W5(IJK)
      D(JC+J)=S2*W5(IJK)
      J=J+INC4
   67 CONTINUE
      J=JBASE
      DO 69 IJK=1,LOT
      C(JC+J)=C(JC+J)-S2*W6(IJK)
      D(JC+J)=D(JC+J)+C2*W6(IJK)
      J=J+INC4
   69 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
   70 CONTINUE
      JBASE=JBASE+JUMP
   80 CONTINUE
      RETURN
C
   90 CONTINUE
      IA=1
      JA=1
      IB=IA+IINK
      JB=JA+JINK
      IC=IB+IINK
      JC=JB+JINK
      ID=IC+IINK
      JD=JC+JINK
      DO 100 L=1,LA
      I=IBASE
      DO 91 IJK=1,LOT
      W1(IJK)=A(IA+I)+A(IC+I)
      W2(IJK)=A(IB+I)+A(ID+I)
      W3(IJK)=B(IA+I)+B(IC+I)
      W4(IJK)=B(IB+I)+B(ID+I)
      I=I+INC3
   91 CONTINUE
      J=JBASE
      DO 92 IJK=1,LOT
      C(JA+J)=W1(IJK)+W2(IJK)
      D(JA+J)=W3(IJK)+W4(IJK)
      J=J+INC4
   92 CONTINUE
      J=JBASE
      DO 93 IJK=1,LOT
      C(JC+J)=W1(IJK)-W2(IJK)
      D(JC+J)=W3(IJK)-W4(IJK)
      J=J+INC4
   93 CONTINUE
      I=IBASE
      DO 94 IJK=1,LOT
      W1(IJK)=A(IA+I)-A(IC+I)
      W2(IJK)=A(IB+I)-A(ID+I)
      W3(IJK)=B(IA+I)-B(IC+I)
      W4(IJK)=B(IB+I)-B(ID+I)
      I=I+INC3
   94 CONTINUE
      J=JBASE
      DO 95 IJK=1,LOT
      C(JB+J)=W1(IJK)-W4(IJK)
      D(JB+J)=W3(IJK)+W2(IJK)
      J=J+INC4
   95 CONTINUE
      J=JBASE
      DO 96 IJK=1,LOT
      C(JD+J)=W1(IJK)+W4(IJK)
      D(JD+J)=W3(IJK)-W2(IJK)
      J=J+INC4
   96 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  100 CONTINUE
      IF(LA.EQ.M) RETURN
      LA1=LA+1
      JBASE=JBASE+JUMP
      DO 120 K=LA1,M,LA
      KB=K+K-2
      KC=KB+KB
      KD=KC+KB
      C1=TRIGS(KB+1)
      S1=TRIGS(KB+2)
      C2=TRIGS(KC+1)
      S2=TRIGS(KC+2)
      C3=TRIGS(KD+1)
      S3=TRIGS(KD+2)
      DO 110 L=1,LA
      I=IBASE
      DO 101 IJK=1,LOT
      W1(IJK)=A(IA+I)+A(IC+I)
      W2(IJK)=A(IB+I)+A(ID+I)
      W3(IJK)=B(IA+I)+B(IC+I)
      W4(IJK)=B(IB+I)+B(ID+I)
      I=I+INC3
  101 CONTINUE
      J=JBASE
      DO 102 IJK=1,LOT
      C(JA+J)=W1(IJK)+W2(IJK)
      D(JA+J)=W3(IJK)+W4(IJK)
      J=J+INC4
  102 CONTINUE
      DO 103 IJK=1,LOT
      W1(IJK)=W1(IJK)-W2(IJK)
      W3(IJK)=W3(IJK)-W4(IJK)
  103 CONTINUE
      J=JBASE
      DO 104 IJK=1,LOT
      C(JC+J)=C2*W1(IJK)
      D(JC+J)=S2*W1(IJK)
      J=J+INC4
  104 CONTINUE
      J=JBASE
      DO 105 IJK=1,LOT
      C(JC+J)=C(JC+J)-S2*W3(IJK)
      D(JC+J)=D(JC+J)+C2*W3(IJK)
      J=J+INC4
  105 CONTINUE
      I=IBASE
      DO 111 IJK=1,LOT
      W1(IJK)=A(IA+I)-A(IC+I)
      W2(IJK)=A(IB+I)-A(ID+I)
      W3(IJK)=B(IA+I)-B(IC+I)
      W4(IJK)=B(IB+I)-B(ID+I)
      I=I+INC3
  111 CONTINUE
      DO 112 IJK=1,LOT
      W5(IJK)=W1(IJK)-W4(IJK)
      W6(IJK)=W3(IJK)+W2(IJK)
  112 CONTINUE
      J=JBASE
      DO 113 IJK=1,LOT
      C(JB+J)=C1*W5(IJK)
      D(JB+J)=S1*W5(IJK)
      J=J+INC4
  113 CONTINUE
      J=JBASE
      DO 117 IJK=1,LOT
      C(JB+J)=C(JB+J)-S1*W6(IJK)
      D(JB+J)=D(JB+J)+C1*W6(IJK)
      J=J+INC4
  117 CONTINUE
      DO 114 IJK=1,LOT
      W1(IJK)=W1(IJK)+W4(IJK)
      W3(IJK)=W3(IJK)-W2(IJK)
  114 CONTINUE
      J=JBASE
      DO 115 IJK=1,LOT
      C(JD+J)=C3*W1(IJK)
      D(JD+J)=S3*W1(IJK)
      J=J+INC4
  115 CONTINUE
      J=JBASE
      DO 116 IJK=1,LOT
      C(JD+J)=C(JD+J)-S3*W3(IJK)
      D(JD+J)=D(JD+J)+C3*W3(IJK)
      J=J+INC4
  116 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  110 CONTINUE
      JBASE=JBASE+JUMP
  120 CONTINUE
      RETURN
C
C
      END
