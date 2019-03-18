      PROGRAM dirich
C     Program to generate data on a dirichlet kernal.
C     Robert Grumbine 15 December 1994

      IMPLICIT none

      INTEGER i, n
      REAL pi, d(10000)

      PRINT *,'How many points do you want?'
      READ (*,9001) n

      pi   = 3.141592654
      DO 1000 i = 1, n
        d(i) = SNGL( dsin(DBLE(FLOAT(i)*pi/1000.)) /
     1             (DBLE(n)* dsin(DBLE(FLOAT(i)*pi/(1000.*FLOAT(n))))) )
 1000 CONTINUE

      PRINT *,'What do you want to call the output file?'
      CALL ritout(d, n, 10)

 9001 FORMAT (I5)

      END
