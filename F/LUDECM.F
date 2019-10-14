      PROGRAM ludecm
C     Solve an n*n system of equations and unknowns by L-U
C       decomposition.
C     Assume a full matrix, no need for pivoting, and n<=20.
C     LW=B  UX=W  X = solution.
C     Decompose so that ones are on the main diagonal of L.
C     B holds the initial B and the final solution x.
C     W is a temproary storage area. (It isn't required, but is
C       used for clarity.)
C     A is used for the original matrix and then for L-U.

      REAL a(20, 20), b(20), w(20)

C     Read in initial values from a file.
      READ (8, 1) n
      DO 10 i = 1, n
        READ (8, 2) (a(i, j), j=1, n)
   10 CONTINUE
      READ (8, 2) (b(i), i=1, n)
    1 FORMAT (I5)
    2 FORMAT (8F10.5)

      WRITE (6, 3) n
      WRITE (7, 3) n
    3 FORMAT (' The problem is to solbe the ',I2,' square matrix:')
      DO 20 i = 1, n
        WRITE (6, 4) (a(i, j), j=1, n)
        WRITE (7, 4) (a(i, j), j=1, n)
   20 CONTINUE
      WRITE (6, 5) (b(i), i=1, n)
      WRITE (7, 5) (b(i), i=1, n)
    4 FORMAT (1X, 8F10.5)
    5 FORMAT (' The b vector is ',8F10.5)

C     Decompose the matrix, store l in the original matrix.
      DO 30 ii = 1, n-1
        DO 40 i = ii+1, n
        a(i, ii) = a(i, ii)/ a(ii, ii)
          DO 50 j = ii+1, n
            a(i, j) = a(i, j) -a(ii, j)*a(i, ii)
   50     CONTINUE
   40   CONTINUE
   30 CONTINUE

C     Do the forward solution through L.
      w(1) = b(1)
      DO 60 ii = 2, n
        w(ii) = b(ii)
        DO 70 j = 1, ii-1
          w(ii) = w(ii) - a(ii, j)*w(j)
   70   CONTINUE
   60 CONTINUE

C     Do the back substitution through u.
      b(n) = w(n) / a(n,n)
      DO 80 i = n-1, 1, -1
        b(i) = w(i)
        DO 90 j = n, i+1, -1
          b(i) = b(i)-b(j)*a(i, j)
   90   CONTINUE
        b(i) = b(i) / a(i, i)
   80 CONTINUE

      STOP
      END 
