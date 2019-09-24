      PROGRAM inverse
C     Program to create inversions of sample data.
C     For support of the data inversion report.
C     11-23-93  Version 1: Apply a smoothness constraint 
C                            to a scalar field in time.
C     11-23-93  Version 2: Put gaps into the observing series.
C     11-24-93  Version 3: Synthetically smooth very gappy series --
C                            average the true, 'observed', and smoothed
C                            while using full data in computing smoothed.
C     11-24-93  Version 4: Determine boundary values from time series,
C                            rather than by specification.
C     11-26-93  Version 5: Remove unused variables.  Make gappy versus
C                            full series correlation computation automatic.
C
C     Subroutines:
C       Mksmpl - 11-23-93 Make a time series for a single point.
C              - 11-24-93 -- option for a sinusoidal series.
C       Mkrand - 11-23-93 Add random noise to a vector.
C       Wght1  - 11-23-93 Create terms in weighting matrix for inversion
C              - 11-24-93 -- modified for determining boundary values
C       Tridig - 11-23-93 Invert Tri-diagonal matrix (Num. Recipes)
C       Correl - XX-XX-XX BG correlation functions.
C       Sample - 11-23-93 Determine whether data was successfully sampled.
C       Synth  - 11-24-93 Construct a synthetic series based on gappiness,
C                           If original series is half gaps, new series has
C                           time resolution of 2dt.

      IMPLICIT none
      
      INTEGER tzero, tline, thi
      PARAMETER (tzero = 4)
      PARAMETER (tline = 20)
      PARAMETER (thi   = 136)

      REAL dt, epsilon, rsize, gaps
      PARAMETER (dt = 1.0)
      INTEGER m, n, i, j
      PARAMETER (n = thi)
      REAL x(n), y(n), z(n), w(n)
      REAL x2(n), y2(n), z2(n), w2(n)
      REAL a(n), b(n), c(n), r(n)

      REAL rxy, rxz, ryz, ran2
      REAL sxy, sxz, syz
      REAL d1, d2, d3, d4, ia
      CHARACTER*60 fname
      INTEGER npts, nser, seed

      PRINT *,'What size random errors would you like?'
      READ (*,9001) rsize
      PRINT *,'What magnitude of epsilon would you like?'
      READ (*,9001) epsilon
      PRINT *,'What is the probability of a gap occurring?'
      READ (*,9001) gaps
 9001 FORMAT (E13.6)
      PRINT *,'How many series to average?'
      READ (*,9010) nser
 9010 FORMAT (I3)

      PRINT *,'What is the name of the output file?'
      READ (*,9009) fname
      OPEN (10, FILE=fname, FORM='FORMATTED', STATUS='NEW')
 9009 FORMAT (A60) 
   
      seed = -1
      ia = RAN2(seed)
      sxy = 0.
      sxz = 0.
      syz = 0.

      DO 9998 j = 1, nser
        
        CALL mksmpl(x, n, tzero, tline, thi, 2)
        CALL veceq(x, y, n)
        CALL mkrand(x, y, n, rsize, seed)
        CALL sample(w, n, gaps, seed)
        
        CALL wght1(y, a, b, c, r, dt, epsilon, n, w, 2)
        CALL tridig(a, b, c, r, z, n)
        
        CALL synth(x, x2, w, n, gaps, .TRUE., m, w2)
        CALL synth(y, y2, w, n, gaps, .FALSE., m, w2)
        CALL synth(z, z2, w, n, gaps, .TRUE., m, w2)

        DO 9999 i = 1, n
          WRITE (10,9002) i, x(i), y(i), z(i)
 9999   CONTINUE
 9002   FORMAT (I4, 3F7.3)

        IF (m .EQ. n) THEN
          CALL correl(x, y, n, rxy, d1, d2, d3, d4)
          WRITE (*,9003) rxy, d1, d2, d3, d4
          CALL correl(x, z, n, rxz, d1, d2, d3, d4)
          WRITE (*,9003) rxz, d1, d2, d3, d4
          CALL correl(y, z, n, ryz, d1, d2, d3, d4)
          WRITE (*,9003) ryz, d1, d2, d3, d4
          PRINT *,' '
         ELSE
          npts = 0
          DO 1000 i = 1, m
            IF (w2(i) .NE. 0) THEN
              npts = npts + 1
              x(npts) = x2(i)
              y(npts) = y2(i)
              z(npts) = z2(i)
            ENDIF
 1000     CONTINUE
          CALL correl(x, y, npts, rxy, d1, d2, d3, d4)
          WRITE (*,9003) rxy, d1, d2, d3, d4, FLOAT(npts)/FLOAT(m), m
          CALL correl(x, z, npts, rxz, d1, d2, d3, d4)
          WRITE (*,9003) rxz, d1, d2, d3, d4, FLOAT(npts)/FLOAT(m), m
          CALL correl(y, z, npts, ryz, d1, d2, d3, d4)
          WRITE (*,9003) ryz, d1, d2, d3, d4, FLOAT(npts)/FLOAT(m), m
          PRINT *,' '
        ENDIF

        sxy = sxy+rxy
        sxz = sxz+rxz
        syz = syz+ryz

 9998 CONTINUE
      
      PRINT *,' '
      WRITE (*,9003) sxy/nser, sxz/nser, syz/nser
      
 9003 FORMAT (6F7.3, I4)

      STOP
      END
      SUBROUTINE synth(x, x2, w, n, gaps, useall, m, w2)
C     Subroutine to construct a synthetic time series, subsampled
C       proportional to 1/(1-gaps).  w determines whether there is
C       a gap at a given time.  
C       Useall is a logical which controls whether the full time
C        series (ignoring gaps) is to be used.
      INTEGER n, m
      REAL gaps, x(n), x2(n), w(n), w2(n)
      LOGICAL useall

      INTEGER i, j, k, nsamp, resamp

      resamp = INT(1./(1.-gaps))
      m = n / resamp

      IF (resamp .EQ. 1) THEN
        DO 1 i = 1, n
          x2(i) = x(i)
          w2(i) = 1.0
   1    CONTINUE
        RETURN
      ENDIF

C     Note, must deal with series which are not integral multiples.
C     Currently ignoring. 11-24-93 BG      
      IF (useall) THEN
        DO 1000 i = 1, n, resamp
          k = 1+i/resamp
          x2(k) = 0.
          DO 1100 j = 1, resamp
            x2(k) = x2(k)+x(i-1+j)
 1100     CONTINUE
          x2(k) = x2(k)/resamp
 1000   CONTINUE
      
       ELSE
        DO 2000 i = 1, n, resamp
          k = 1+i/resamp
          x2(k) = 0.
          w2(k) = 0
          nsamp = 0
          DO 2100 j = 1, resamp
            IF (w(i+j-1) .NE. 0) THEN
              nsamp = nsamp + 1
              x2(k) = x2(k) + x(i+j-1)
              w2(k) = 1.
            ENDIF
 2100     CONTINUE
          IF (nsamp .NE. 0) THEN
            x2(k) = x2(k) / nsamp
          ENDIF
 2000   CONTINUE

      ENDIF



      RETURN
      END

      SUBROUTINE sample(w, n, gaps, seed)
      INTEGER n
      REAL w(n), gaps
      INTEGER i, seed

      DO 1000 i = 1, n
        IF (RAN2(seed) .GT. gaps) THEN
          w(i) = 1.
         ELSE
          w(i) = 0.
        ENDIF
 1000 CONTINUE
      
      RETURN
      END
      SUBROUTINE mksmpl(x, n, tzero, tline, thi, type)
      INTEGER n, type
      REAL x(n)
      INTEGER i, j
      INTEGER tzero, tline, thi
      REAL pi

      IF (type .EQ. 1) THEN
        DO 1000 i = 1, tzero
          x(i) = 0.0
 1000   CONTINUE
        j = tzero
        DO 1100 i = j+1, j+tline
          x(i) = (i-j)*0.99/FLOAT(tline)
 1100   CONTINUE
        j = j+tline
        DO 1200 i = j+1, j+thi
          x(i) = 0.99
 1200   CONTINUE
        j = j+thi
        DO 1300 i = j+1, j+tline
          x(i) = 0.99 - (i-j)*0.99/FLOAT(tline)
 1300   CONTINUE
        j = j+tline
        DO 1400 i = j+1, j+tzero
          x(i) = 0.0
 1400   CONTINUE

       ELSE IF (type .EQ. 2) THEN
        pi = ATAN(1.)*4.
        DO 2000 i = 1, thi       
          x(i) = 0.95 + 0.05 * sin(2.*pi*(i-1)/FLOAT(tline) ) 
 2000   CONTINUE
       ELSE
        PRINT *,'Type of sample curve out of range '
        STOP
      ENDIF
      
      RETURN
      END
      SUBROUTINE mkrand(x, y, n, rsize, seed)   
      INTEGER n, seed
      REAL x(n), y(n), rsize

      INTEGER i

      DO 1000 i = 1, n
        y(i) = x(i) + rsize*(RAN2(seed)-0.5)*2.
 1000 CONTINUE

      RETURN
      END
      FUNCTION RAN2(IDUM)
C     Single linear congruential random number generator.
C     From Numerical Recipes.
      INTEGER IDUM
      INTEGER M, IA, IC
      PARAMETER (M = 714025)
      PARAMETER (IA = 1366)
      PARAMETER (IC = 150899)
      REAL RM
      PARAMETER (RM = 1./M)
      INTEGER J, IFF, IY

      INTEGER IR(97)
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
      IF (J .GT. 97 .OR. J .LT. 1) PAUSE
      IY = IR(J)
      RAN2 = IY*RM
      IDUM=MOD(IA*IDUM+IC,M)
      IR(J) = IDUM
      RETURN
      END

      SUBROUTINE wght1(y, a, b, c, r, dt, epsilon, n, w, type)
C     11-23-93 Fill the vectors with their weights for the
C        constrained problem.  Y is input vector, a, b, c
C        are the diagonals, and r is the forcing vector in Ax = r.
C     11-23-93 Extended to work with gappy time series.
C     11-24-93 Added 'type' to handle which type of boundary treatment.
C               1 -> use 0 boundary values.
C               2 -> use natural variational conditions.
C     11-26-93 Work with computing boundary values in a gappy time
C               series.

      INTEGER n, type
      REAL y(n), r(n), a(n), b(n), c(n), w(n)
      REAL dt, epsilon

      REAL e2dt
      INTEGER i

      e2dt = (epsilon*dt)**2
      IF (type .EQ. 1) THEN
        a(1) = 0.
        b(1) = 1.
        c(1) = 0.
        r(1) = 0.95
        a(n) = 0.
        b(n) = 1.
        c(n) = 0.
        r(n) = 0.95
       ELSE IF (type .EQ. 2) THEN
C       9000 loops handle gaps near ends of series.        
        a(1) = 0.
        b(1) = -1.
        c(1) = 1.        
        DO 9000 i = 2, n
          IF (w(i) .NE. 0.) THEN
            r(1) = (y(i)-y(1))/(1+2*epsilon*epsilon)/(i-1)
            GO TO 9001
          ENDIF
 9000   CONTINUE
 9001   CONTINUE
        a(n) = -1.
        b(n) = 1.
        c(n) = 0.
        DO 9100 i = n-1, 1, -1
          IF (w(i) .NE. 0.) THEN
            r(n) = (y(n)-y(i))/(1+2*epsilon*epsilon)/(n-i)
            GO TO 9101
          ENDIF
 9100   CONTINUE
 9101   CONTINUE
       ELSE
        PRINT *,'Type of boundary treatment out of range'
        STOP
      ENDIF

      DO 1000 i = 2, n-1
        IF (w(i) .NE. 0.) THEN  
          a(i) = e2dt
          b(i) = -1. -2.*e2dt
          c(i) = e2dt
          r(i) = -y(i)
         ELSE
          a(i) = e2dt
          b(i) = -2.*e2dt
          c(i) = e2dt
          r(i) = 0
        ENDIF
 1000 CONTINUE

      RETURN
      END

      SUBROUTINE tridig(a, b, c, r, u, n)
C     Subroutine to solve a tri-diagonal matrix problem
C       Au = r, where the diagonals of A are a, b, c
C     From Numerical Recipes
      INTEGER nmax 
      PARAMETER (nmax = 300*450)
      REAL gam(nmax), a(n), b(n), c(n), r(n), u(n)
      REAL bet

      IF (nmax .LT. n) THEN
        PRINT *,'Need more work space'
        STOP
      ENDIF
      IF (b(1) .EQ. 0) STOP

      bet = b(1)
      u(1) = r(1)/bet
      DO 1000 j = 2, N
        gam(j) = c(j-1)/bet
        bet    = b(j)-a(j)*gam(j)
        IF (bet .EQ. 0.) STOP
        u(j) = (r(j)-a(j)*u(j-1))/bet
 1000 CONTINUE
      DO 1100 j = n-1, 1, -1
        u(j) = u(j)-gam(j+1)*u(j+1)
 1100 CONTINUE

      RETURN
      END

      SUBROUTINE veceq(x, y, n)
      INTEGER n
      REAL x(n), y(n)
      INTEGER i
      DO 1000 i = 1, n
        y(i) = x(i)
 1000 CONTINUE
      RETURN
      END
      SUBROUTINE correl(x, y, k, r2, xbar, ybar, sig2x, sig2y)
C     Compute various statistical parameters between two 
C       vectors.

      IMPLICIT none

      INTEGER k
      REAL x(k), y(k)
      REAL r2, xbar, ybar, sig2x, sig2y
      
      REAL sumx, sumx2, sumxy
      REAL sx, sy, x2, y2, xy

      sx = sumx(x, k)
      sy = sumx(y, k)
      x2 = sumx2(x, k)
      y2 = sumx2(y, k)
      xy = sumxy(x, y, k)

      xbar = sx/k
      ybar = sy/k
      sig2x = (k*x2-sx*sx)/k/(k-1)
      sig2y = (k*y2-sy*sy)/k/(k-1)
      r2    = (k*xy - sx*sy)/ SQRT( (k*x2-sx*sx)*(k*y2-sy*sy) )

      RETURN
      END 
      FUNCTION sumx(x,n)
      IMPLICIT none
      INTEGER n
      REAL x(n)
      REAL sumx

      DOUBLE PRECISION sum
      INTEGER i
      sum = 0.
      DO 1000 i = 1, n
        sum = sum + DBLE(x(i))
 1000 CONTINUE
      sumx = SNGL(sum)
      RETURN
      END

      FUNCTION sumx2(x, n)
      IMPLICIT none
      INTEGER n
      REAL x(n)
      REAL sumx2

      DOUBLE PRECISION sum
      INTEGER i
      sum = 0.0
      DO 2000 i = 1, n
        sum = sum + DBLE(x(i)*x(i))
 2000 CONTINUE
      sumx2 = SNGL(sum)

      RETURN
      END

      FUNCTION sumxy(x, y, n)
C     Compute the sum of x(i)*y(i)
      INTEGER n
      REAL x(n), y(n), sumxy

      INTEGER i
      REAL sum

      sum = 0.0
      DO 1000 i = 1, n
        sum = sum + x(i)*y(i)
 1000 CONTINUE

      sumxy = sum
      RETURN
      END
      SUBROUTINE agree(r, rbar, x, n, ia)
C     Compute the index of agreement between two vectors
      REAL r(n), x(n)
      REAL rbar, ia

      REAL sr2, sx2, srx, spx
      INTEGER i

      sr2 = 0.0
      sx2 = 0.0
      srx = 0.0
      spx = 0.0
      rbar = 0.0

      DO 100 i = 0, n
        rbar = rbar + r(i)
  100 CONTINUE
      rbar = rbar / FLOAT (n)

      DO 1000 i = 0, n
          sr2 = sr2 + r(i)*r(i)
          sx2 = sx2 + x(i)*x(i)
          srx = srx + r(i)*x(i)
          spx = spx + (ABS(x(i)-rbar) + ABS(r(i)-rbar) )**2
 1000 CONTINUE

      IF (spx .NE. 0.0) THEN
        ia = 1.0 - (sx2 - 2.*srx + sr2) / spx
       ELSE 
        ia = 0.0
      ENDIF

      RETURN
      END 
