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
        PRINT *,i
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
      SUBROUTINE mkuran(x, n, rsize, seed)   
C     Subroutine to creat a uniformly distributed random variable
C       vector.  
C     Bob Grumbine 12/09/93.

      IMPLICIT none

      INTEGER n, seed
      REAL x(n), rsize, ran2

      INTEGER i

      PRINT *,'starting loop in mkuran'
      DO 1000 i = 1, n
        x(i) = rsize*(ran2(seed)-0.5)*2.
        PRINT *,'i, x(i) ',i,x(i)
 1000 CONTINUE

      RETURN
      END
      REAL FUNCTION ran2(IDUM)
C     Single linear congruential random number generator.
C     From Numerical Recipes.
C     Bob Grumbine 7 April 1994.

      IMPLICIT none

      INTEGER IDUM
      INTEGER M, IA, IC
      PARAMETER (M = 714025)
      PARAMETER (IA = 1366)
      PARAMETER (IC = 150899)
      REAL RM
      PARAMETER (RM = 1./M)
      INTEGER J, IFF, IY

      INTEGER IR(97)
C     The save statement is necessary, as Num. Recipes assumes
C       that variables are saved between calls, which they
C       aren't on all machines.  BG 1/22/94.
      SAVE 
      
      DATA IFF /0/
      
      IF (IDUM .LT. 0 .OR. IFF .EQ. 0) THEN
        IFF = 1
        IDUM = MOD(IC-IDUM,M)
        DO 11 J = 1, 97
          IDUM = MOD(IA*IDUM+IC, M)
          IR(J) = IDUM
   11   CONTINUE
        IDUM = MOD(IA*IDUM+IC, M)
        IY = IDUM
      ENDIF

      J = 1 + (97*IY)/M
      IF (J .GT. 97 .OR. J .LT. 1) THEN
        PRINT *,'Error on J in RAN2.  J = ',J
	STOP
      ENDIF
      IY = IR(J)
      ran2 = IY*RM
      IDUM=MOD(IA*IDUM+IC,M)
      IR(J) = IDUM

      RETURN
      END
