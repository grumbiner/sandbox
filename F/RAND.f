C=======================================================================
      SUBROUTINE RAND(U,N)
      INTEGER N,X,Y,Z
      REAL U(N),V
      COMMON/RANDOM/X,Y,Z
      IF(N.LE.0) RETURN
      DO 1 I=1,N
        X=171*MOD(X,177)-2*(X/177)
        IF(X.LT.0) X=X+30269
        Y=172*MOD(Y,176)-35*(Y/176)
        IF(Y.LT.0) Y=Y+30307
        Z=170*MOD(Z,178)-63*(Z/178)
        IF(Z.LT.0) Z=Z+30323
        V=X/30269.0 + Y/30308.0 + Z/30323.0
1       U(I)=V-INT(V)
      RETURN
      END
