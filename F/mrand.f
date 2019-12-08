      PROGRAM mrand
C     Add random noise to an input vector, avoiding the
C     end points.
      IMPLICIT none
      INTEGER nmax
      PARAMETER (nmax = 410)
      REAL x(nmax), a(nmax), r(nmax)
      INTEGER i, n

      OPEN (10, FILE='bering', FORM='FORMATTED', STATUS='OLD')
      OPEN (11, FILE='bering.dat', FORM='FORMATTED', STATUS='NEW')
      OPEN (12, FILE='forin.dat', FORM='FORMATTED', STATUS='NEW')

      i = 0
 1000 CONTINUE
        i = i + 1
CD        READ (10, 9001, END=2000) a(i), r(i)
        READ (10, *, ERR=2000, END=2000) a(i), r(i)
        WRITE (*, 9001 ) a(i), r(i)
        GO TO 1000

 2000 CONTINUE
      n = i - 1

      PRINT *,'n = ',n
      PRINT *,'calling mkuran'
      CALL mkuran(x, n, 0.05, 1)
      PRINT *,'returned from mkuran'

CD      DO 3000 i = 0.2*n, 0.8*n
      DO 3000 i = 2, n-1
        a(i) = a(i) + x(i)
 3000 CONTINUE
      
      DO 4000 i = 1, n
        WRITE (*, 9001) a(i), r(i) 
        WRITE (11, 9001) a(i), r(i) 
        WRITE (12, 9001) a(i), r(i) 
 4000 CONTINUE
 
 9001 FORMAT (F6.4, 2x, F9.6)

      STOP 
      END
