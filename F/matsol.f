C*************************************************----------++++++++++!!
      SUBROUTINE MATSOL(xmax, ymax, a, b, piv)
C
C     This part does the LU factorization.
C
C     Robert Grumbine 6 June 1994

      IMPLICIT none

      INTEGER xmax, ymax

      REAL*8 a(2*xmax*ymax, -2*xmax: 2*xmax), 
     1       b(2*xmax*ymax)
C
C     Local variables.
C
      INTEGER ii, i, j
      INTEGER k, n
      REAL*8 piv(2*xmax*ymax, -2*xmax: -1)

      SAVE 

      k = 2*xmax
      n = 2*xmax*ymax
C
C     Initialize the matrix to hold the pivots in temporarily.
C
      DO 100 ii = 1, n
        DO 110 j = -k, -1
          piv(ii, j) = 0.D0
C          PRINT *,'100 loop i, j, piv', ii, j, piv(ii,j)
  110   CONTINUE
  100 CONTINUE
C
C     Decompose the matrix
C
      DO 1000 ii = 1, n-1
CC        IF (DABS(a(ii, 0)) .LT. 1.D-10) THEN
        IF (ABS(a(ii, 0)) .LT. 1.D-10) THEN
          WRITE (*, 9001) ii
          WRITE (10, 9001) ii
        ENDIF
        DO 1010 i = 1, MIN(k, n-ii)
          piv(ii+i, -i) = a(ii+i, -i)/ a(ii, 0)
          DO 1020 j = 1, k
            a(ii+i, j-i) = a(ii+i, j-i) -a(ii, j)*piv(ii+i, -i)
 1020     CONTINUE
 1010   CONTINUE
 1000 CONTINUE
C
C     Now put the pivots on the lower diagonal of the matrix.
C
      DO 2000 ii = 1, n
        DO 2010 j = -k, -1
          a(ii, j) = piv(ii, j)
 2010   CONTINUE
 2000 CONTINUE

 9001 FORMAT (' WARNING!! a small pivot (.le. 1.d-10) was found on step
     1', I5,' of the decomposition. Accuracy will be impaired.')
 
      RETURN
      END
