      SUBROUTINE SFAX(IFAX,N,MODE)
C     Mark Iredell 7 April 1994
C
C
      DIMENSION IFAX(10)
C
      NN=N
      IF(IABS(MODE).EQ.1) GO TO 10
      IF(IABS(MODE).EQ.8) GO TO 10
      NN=N/2
      IF((NN+NN).EQ.N) GO TO 10
      IFAX(1)=-99
      RETURN
C
   10 CONTINUE
      K=1
   20 CONTINUE
      IF(MOD(NN,4).NE.0) GO TO 30
      K=K+1
      IFAX(K)=4
      NN=NN/4
      IF(NN.EQ.1) GO TO 80
      GO TO 20
   30 CONTINUE
      IF(MOD(NN,2).NE.0) GO TO 40
      K=K+1
      IFAX(K)=2
      NN=NN/2
      IF(NN.EQ.1) GO TO 80
   40 CONTINUE
      IF(MOD(NN,3).NE.0) GO TO 50
      K=K+1
      IFAX(K)=3
      NN=NN/3
      IF(NN.EQ.1) GO TO 80
      GO TO 40
   50 CONTINUE
      L=5
      INC=2
   60 CONTINUE
      IF(MOD(NN,L).NE.0) GO TO 70
      K=K+1
      IFAX(K)=L
      NN=NN/L
      IF(NN.EQ.1) GO TO 80
      GO TO 60
   70 CONTINUE
      L=L+INC
      INC=6-INC
      GO TO 60
   80 CONTINUE
      IFAX(1)=K-1
      NFAX=IFAX(1)
      IF(NFAX.EQ.1) GO TO 110
      DO 100 II=2,NFAX
      ISTOP=NFAX+2-II
      DO 90 I=2,ISTOP
      IF(IFAX(I+1).GE.IFAX(I)) GO TO 90
      ITEM=IFAX(I)
      IFAX(I)=IFAX(I+1)
      IFAX(I+1)=ITEM
   90 CONTINUE
  100 CONTINUE
  110 CONTINUE
      RETURN
C
      END
