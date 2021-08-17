      PROGRAM mrand
C     Add random noise to an input vector, avoiding the
C     end points.
      INTEGER nmax
      PARAMETER (nmax = 6410)
      REAL x(nmax), a(nmax), r(nmax)
      INTEGER i, n, seed
      REAL tmp, RAN2

      OPEN (10, FILE='bering', FORM='FORMATTED', STATUS='OLD')
      OPEN (11, FILE='bering.dat', FORM='FORMATTED', STATUS='UNKNOWN')
      OPEN (12, FILE='forin.dat', FORM='FORMATTED', STATUS='UNKNOWN')

CD      PRINT *,'Finished opening files'
      seed = -1
      tmp = RAN2(seed)
      i = 0
 1000 CONTINUE
        i = i + 1
        READ (10, 9001, END=2000) a(i), r(i)
CD        WRITE (*, *) i, a(i), r(i)
        GO TO 1000

 2000 CONTINUE
      PRINT *,'Finished reading in data'

      n = i - 1

      CALL mkurand(x, n, 0.05, seed)
      PRINT *,'Returned from mkurand'

      DO 3000 i = 2, n-1
        a(i) = a(i) + x(i)
 3000 CONTINUE
      
      DO 4000 i = 1, n
        WRITE (11, 9001) a(i), r(i) 
        WRITE (12, 9001) a(i), r(i) 
 4000 CONTINUE
 
 9001 FORMAT (F6.4, 2x, F9.6)

      STOP 
      END
