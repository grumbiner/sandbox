      PROGRAM shuffl
C     Program to randomize the order of elements in a vector.
C     Robert Grumbine 4-8-87.

      REAL x(10000), y(10000)
      INTEGER i, j, n, tot
      DOUBLE PRECISION drand48

      PRINT *,'How many data points are there?'
      READ (*,9001) tot
      CALL readin (x, tot, 10)

      n = tot
      DO 1000 i = 1, tot
        j    = INT (n*drand48() + 1. )
        y(i) = x( j )
        x(j) = x( n )
        n = n - 1
 1000 CONTINUE

      PRINT *,'What do you want to call the output file?'
      CALL ritout(y, tot, 11)

 9001 FORMAT (I5)

      END
