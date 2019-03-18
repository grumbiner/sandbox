      SUBROUTINE IRAND(S,N,LOW,HI)
C===Generate a random integer sequence: S(1),S(2), ... ,S(N)
C   such that each element is in the closed interval <LOW,HI> and
C   sampled WITH REPLACEMENT.                            HDK, JUNE 1971.
      INTEGER N,S(N),LOW,HI,IX,I
      REAL U(1)
      DOUBLE PRECISION X
      DO 1 I=1,N
        CALL RAND(U,1)
C---Use DP arithmetic to effect a more precise transformation.
        X=DBLE((HI+1)-LOW)*U(1) + DBLE(LOW)
C---INT(X)=TRUNCATION(X)
        IX=INT(X)
        IF(X.LT.0 .AND. IX.NE.X) IX=INT(X-1.D0)
        S(I)=IX
  1   CONTINUE
      RETURN
      END
