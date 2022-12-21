      PROGRAM iter
C     Program to act as a front end to the harmonic analysis routines.
C     Robert Grumbine 8-25-86.
C     Version to individually compute the components of frequencies
C       which are too closely spaced for matrix handling. 

      IMPLICIT none

      INTEGER m, n
      REAL a(6000), b(6000), freq(6000)
      REAL x(11000), y(11000)

      INTEGER i, j
      CHARACTER*40 fname
      LOGICAL yes
      REAL f, start, stop, step

      REAL pi
      PARAMETER (pi = 3.141592654)

      REAL a1, a2, a3, a4, a5
      REAL alpha, beta, r

      PRINT *,'How many data points are there?'
      READ (*,9001) m

      PRINT *,'At what frequency do you want to start?'
      READ (*,9002) start
      PRINT *,'At what freq. do you want to end?'
      READ (*,9002) stop
      PRINT *,'What interval do you want to step with?'
      READ (*,9002) step 

      n = INT( (stop-start)/step )
      IF (n .GE. 6000) THEN
        step = (stop-start)/6000
        n = 6000
        PRINT *,'Your step size was too small, now using ',step
      ENDIF

      OPEN(10, FILE="seout", FORM="FORMATTED", STATUS="OLD")

      DO i = 1, m
        !READ(10,*) j, a1, a2, a3, a4, a5, x(i)
        y(i) = i
        READ(10,*) j, x(i)
      ENDDO

      f = start
      DO 1000 i = 1, n
        freq(i) = f * 2. * pi
        f       = f + step
 1000 CONTINUE

C     Find to best fit amplitudes for the given frequencies.
      CALL detrend(y, x, m, alpha, beta, r)
      PRINT *,'detrended ',alpha, beta, r 

      i = 1
      CALL harmrm(x, m, 0, a(1), b(1), 1)
      PRINT *,'a b = ',a(1), b(1)
      x = x - a(1)

      DO 1500 i = 2, n
        CALL harmrn(x, m, freq(i), a(i), b(i), 1)
 1500 CONTINUE

C     Write out results:
      PRINT *,'What do you want to call the harmonic data file?'
      READ (*,9003) fname
      OPEN (12, FILE=fname, FORM='FORMATTED', STATUS='UNKNOWN')

      DO 2000 i = 1, n
        WRITE (*,9002) freq(i)/2./pi, (a(i)*a(i)+b(i)*b(i))**.5,
     1              ATAN2(b(i),a(i))*180./pi 
        WRITE (12,9002) freq(i)/2./pi, (a(i)*a(i)+b(i)*b(i))**.5,
     1              ATAN2(b(i),a(i))*180./pi 
 2000 CONTINUE
      CLOSE(12)

C     Subtract the analyzed components from the data.
      x = x + a(1) !add back in because demod will remove it too
      CALL demod (a, b, freq, 7, x, m)
      DO i = 1, m
        PRINT *,i,x(i)
      ENDDO 

 9001 FORMAT (I5)   

 9002 FORMAT (3E15.7)

 9003 FORMAT (A40)

      END
