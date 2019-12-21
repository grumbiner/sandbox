 
      SUBROUTINE RPERM(P,N)
C   Unknown, 2 May 1995.
C===Generate a random permutation, P, of the first N integers.
C   (equivalent to sampling WITHOUT REPLACEMENT).
C   Adaptation of Knuth Volume 2, Algorithm 3.4.2P.
      INTEGER N,P(N), K,J,I,IPJ,ITEMP,M
      REAL U(100)
      DO 1 I=1,N
1     P(I)=I
C---Generate up to 100 U(0,1) numbers at a time.
      DO 3 I=1,N,100
        M=MIN(N-I+1,100)
        CALL RAND(U,M)
        DO 2 J=1,M
          IPJ=I+J-1
C---INT(U)=TRUNCATION(U)
          K=INT(U(J)*(N-IPJ+1))+IPJ
          ITEMP=P(IPJ)
          P(IPJ)=P(K)
2       P(K)=ITEMP
3     CONTINUE
      RETURN
      END
