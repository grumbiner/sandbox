      PROGRAM repro
C     Re-process the scores
      IMPLICIT none
      INTEGER i, j, k
      REAL delm, sigm, sigo, iam, r2m
      REAL delo, sigoo, sigo2, iao, r2o
      REAL x1, x2, x3, x4, x5

      i = 0
 1000 CONTINUE
        READ (*, 9001, END=2000) j, delm, sigm, sigo, iam, r2m, 
     1                        x1, x2, x3, x4, 
     2                           k, delo, sigoo, sigo2, iao, r2o, 
     3                        sigoo, sigo2, iao 
CD        WRITE (*, 9001 ) j, delm, sigm, sigo, iam, r2m, delo, 
CD     1                        sigoo, sigo2, iao, r2o
CD        PRINT *,i, r2m, r2o, r2m-r2o
        WRITE (*,9002) i, r2m, r2o, r2m-r2o
 9002   FORMAT (I5, 3F6.3)
        i = i + 1
        GO TO 1000

 2000 CONTINUE
CORIG 9001 FORMAT (18x, I6, 1X, 3F6.3, 3x, F6.3, F6.3, 2x, 2F6.3, 2F6.3)
 9001 FORMAT (18x, I6, 1X, 3F6.3, F6.3, F6.3, 2F6.3, 2F6.3, 
     1             I6, 1X, 3F6.3, F6.3, F6.3, 2F6.3, 2F6.3)

      STOP
      END
