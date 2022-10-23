      PROGRAM hiter
C     Program to act as a front end to the harmonic analysis routines.
C     Robert Grumbine 8-25-86.
C     Version to individually compute the components of frequencies
C       which are too closely spaced for matrix handling. 

      INTEGER m, n
      INTEGER maxfreqs
      PARAMETER (maxfreqs = 5000)
      REAL a(maxfreqs), b(maxfreqs), freq(maxfreqs)
CD      REAL x(10000)

      INTEGER i
      CHARACTER*40 fname
      LOGICAL yes
      REAL f, start, stop, step

      REAL pi
      PARAMETER (pi = 3.141592654)
C 2006/06/06 for iers data
      INTEGER npts
      PARAMETER (npts = 6136)
      REAL stepsize
      PARAMETER (stepsize = 2.5)
      REAL x(npts), y(npts), utc(npts), lod(npts) 
      INTEGER yy, dd, jd
      CHARACTER*4 mo
      DOUBLE PRECISION sum

      PRINT *,'At what frequency do you want to start?'
      READ (*,9002) start
      PRINT *,'At what freq. do you want to end?'
      READ (*,9002) stop
      PRINT *,'What interval do you want to step with?'
      READ (*,9002) step 
      n = INT( (stop-start)/step )
      IF (n .GE. maxfreqs) THEN
        STOP "would have tried to run with too many frequencies"
      ENDIF

      OPEN (10, FILE="selected", FORM="FORMATTED", STATUS="OLD")
      OPEN (11, FILE="radius.decimated", FORM="FORMATTED", STATUS="OLD")
      DO i = 1, npts
        READ(10, *) x(i)
        READ(11, *) y(i)
        x(i) = x(i)*y(i)
      ENDDO
      m = npts

      f = start
      DO 1000 i = 1, n
        freq(i) = f * 2. * pi
        f       = f + step
 1000 CONTINUE

C     Write out results:
      PRINT *,'What do you want to call the harmonic data file?'
      READ (*,9003) fname
      OPEN (12, FILE=fname, FORM='FORMATTED', STATUS='NEW')

C     Find to best fit amplitudes for the given frequencies.
      sum = 0.0
      DO i = 1, npts
        sum = sum + x(i)
      ENDDO
      sum = sum / npts
      PRINT *,'mean was ',sum
      DO i = 1, npts
        x(i) = x(i) - sum
      ENDDO

      DO 1500 i = 1, n
        CALL harmrn(x, m, freq(i), a(i), b(i), 1)
 1500 CONTINUE
      DO 2000 i = 1, n
        WRITE (12,9002) freq(i)/2./pi, (a(i)*a(i)+b(i)*b(i))**.5,
     1              ATAN2(b(i),a(i))*180./pi, 2.*pi/freq(i) * stepsize
 2000 CONTINUE

!9002 FORMAT (3E15.7)
 9002 FORMAT (E15.7, E15.7, E15.7, F14.6)

 9003 FORMAT (A40)

      END
