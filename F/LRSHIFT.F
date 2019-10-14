      PROGRAM lrshift 
      REAL a(3,3), temp, sum
      INTEGER n, iters

C     Set up A.
C     (or read in, have passed in, etc.)

      n = 3
      iters = 0

 1000 CONTINUE
        iters = iters + 1

C       Subtract r from the diagonal of a.
        r = a(n, n)
        DO 10 I = 1, n
          a(i, i) = a(i, i) - r
   10   CONTINUE

C       Make the L-U decomposition, with no pivoting.
        CALL nopiv(a, n)

C       Compute U*L
        DO 100 i = 1, n
          DO 200 j = 1, n
            IF (i .EQ. j) THEN
              temp = a(i, j)
              DO 300 k = j+1, n
                temp = temp + a(i, k)*a(k, j)
  300         CONTINUE
             ELSE
              temp = 0.0
              DO 310 k = i, n
                temp = temp + a(i, k)*a(k, j)
  310         CONTINUE
            ENDIF

            a(i, j) = temp

  200     CONTINUE
  100   CONTINUE

C       Add r to the diagonal of a.
        DO 20 i = 1, n
          a(i, i) = a(i, i) + r
   20   CONTINUE

C       Check for convergence.
        sum = 0.0
        DO 400 i = 2, n
          DO 500 j = 1, i-1
            sum = sum + abs(a(i, j))
  500     CONTINUE
  400   CONTINUE
        IF (sum .GT. 1.E-10 .AND. iters .LE. 35) GO TO 1000
C       This would be a good place to replace constants with
C         parameters.

C     Write out the eigenvalues.
      Write (6, 9001) (a(i, i), i=1, n)
      WRITE (6, 9002) iters
 9001 FORMAT ('1The eigervalues are:'/5F20.12)
 9002 FORMAT (//' It took',I3,' iterations')

      STOP
      END