      PROGRAM regres
C     estimate various regression lines for x-y data
C     Robert Grumbine 27 September 1994

      IMPLICIT none

      INTEGER nmax
      PARAMETER (nmax = 9999)
      REAL x(nmax), y(nmax)

      REAL xt(nmax), yt(nmax)
      REAL bnot, bwon, avg
      INTEGER npts
      INTEGER i
      REAL corxy

      PRINT *,'How many data points are there?'
      READ (*,9001) npts
      PRINT *,' x file'
      CALL readin(x, npts, 10)
      PRINT *,' y file'
      CALL readin(y, npts, 11)
      IF (npts .LE. 10) 
     1  WRITE (*,9002) (x(i) , y(i), i = 1, npts)
 9001 FORMAT (I4)
 9002 FORMAT (F8.3)

      DO 1000 i = 1, npts
        xt(i) = x(i)
        yt(i) = y(i)
 1000 CONTINUE
      IF (npts .LE. 10) 
     1  WRITE (*,9003) (xt(i) , yt(i), i = 1, npts)
      CALL fit(xt, yt, npts, bnot, bwon, corxy)
      PRINT *,'Linear fit is:', bnot, bwon
      PRINT *,'Correlation was (r**2) ', corxy**2
      PRINT *,' '
 
      DO 2000 i = 1, npts
        xt(i) = x(i)
        yt(i) = ALOG(y(i))
 2000 CONTINUE
      IF (npts .LE. 10) 
     1  WRITE (*,9003) (xt(i) , yt(i), i = 1, npts)
      CALL fit(xt, yt, npts, bnot, bwon, corxy)
      PRINT *,'y = exp(a+bx) fit is:', bnot, bwon
      PRINT *,'Correlation was (r**2) ', corxy**2

      DO 3000 i = 1, npts
        xt(i) = ALOG(ABS(x(i)))
        yt(i) = ALOG(y(i))
 3000 CONTINUE
      IF (npts .LE. 10) 
     1  WRITE (*,9003) (xt(i) , yt(i), i = 1, npts)
      CALL fit(xt, yt, npts, bnot, bwon, corxy)
      PRINT *,' Took abs(x) first'
      PRINT *,'y = c*x**b fit is:', EXP(bnot), bwon
      PRINT *,'Correlation was (r**2) ', corxy**2

 9003 FORMAT (2E13.6)
      END
