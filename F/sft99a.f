C
      SUBROUTINE SFT99A(A,WORK,TRIGS,INC,JUMP,N,LOT,W1,W2,W3,W4,W5,W6)
C     Mark Iredell 7 April 1994
C
C
C*    IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION A(N),WORK(N),TRIGS(N)
      DIMENSION W1(LOT),W2(LOT),W3(LOT),W4(LOT),W5(LOT),W6(LOT)
C*    DATA DP2,DM2/2.D0,-2.D0/
      DATA DP2,DM2/2.  ,-2.  /
C
      NH=N/2
      NX=N+1
      INK=INC+INC
C
      IA=1
      IB=N*INC+1
      JA=1
      JB=2
      DO 11 L=1,LOT
      WORK(JA)=A(IA)+A(IB)
      IA=IA+JUMP
      IB=IB+JUMP
      JA=JA+NX
   11 CONTINUE
      IA=1
      IB=N*INC+1
      DO 12 L=1,LOT
      WORK(JB)=A(IA)-A(IB)
      IA=IA+JUMP
      IB=IB+JUMP
      JB=JB+NX
   12 CONTINUE
C
      IABASE=2*INC+1
      IBBASE=(N-2)*INC+1
      JABASE=3
      JBBASE=N-1
C
      DO 30 K=3,NH,2
      IA=IABASE
      IB=IBBASE
      JA=JABASE
      JB=JBBASE
      C=TRIGS(N+K)
      S=TRIGS(N+K+1)
      DO 21 L=1,LOT
      W1(L)=A(IA)+A(IB)
      W2(L)=A(IA)-A(IB)
      W3(L)=A(IA+INC)+A(IB+INC)
      W4(L)=A(IA+INC)-A(IB+INC)
      IA=IA+JUMP
      IB=IB+JUMP
   21 CONTINUE
      DO 22 L=1,LOT
      W5(L)=S*W2(L)
      W6(L)=C*W2(L)
   22 CONTINUE
      DO 23 L=1,LOT
      W5(L)=W5(L)+C*W3(L)
      W6(L)=W6(L)-S*W3(L)
   23 CONTINUE
      DO 24 L=1,LOT
      WORK(JA)=W1(L)-W5(L)
      JA=JA+NX
   24 CONTINUE
      DO 25 L=1,LOT
      WORK(JB)=W1(L)+W5(L)
      JB=JB+NX
   25 CONTINUE
      JA=JABASE
      DO 26 L=1,LOT
      WORK(JA+1)=W6(L)+W4(L)
      JA=JA+NX
   26 CONTINUE
      JB=JBBASE
      DO 27 L=1,LOT
      WORK(JB+1)=W6(L)-W4(L)
      JB=JB+NX
   27 CONTINUE
      IABASE=IABASE+INK
      IBBASE=IBBASE-INK
      JABASE=JABASE+2
      JBBASE=JBBASE-2
   30 CONTINUE
C
      IF(IABASE.NE.IBBASE) GO TO 50
      IA=IABASE
      JA=JABASE
      DO 41 L=1,LOT
      WORK(JA)=DP2*A(IA)
      IA=IA+JUMP
      JA=JA+NX
   41 CONTINUE
      IA=IABASE
      JA=JABASE
      DO 42 L=1,LOT
      WORK(JA+1)=DM2*A(IA+INC)
      IA=IA+JUMP
      JA=JA+NX
   42 CONTINUE
C
   50 CONTINUE
      RETURN
C
      END
