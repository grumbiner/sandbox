      PROGRAM hiter
C     Program to act as a front end to the harmonic analysis routines.
C     Robert Grumbine 8-25-86.
C     Version to individually compute the components of frequencies
C       which are too closely spaced for matrix handling. 

      INTEGER m, n
      INTEGER maxfreqs
      PARAMETER (maxfreqs = 6000)
      REAL a(maxfreqs), b(maxfreqs), freq(maxfreqs)

      INTEGER i
      CHARACTER*40 fname
      LOGICAL yes
      REAL f, start, step

      REAL pi
      PARAMETER (pi = 3.141592654)
C 2006/06/06 for iers data
      INTEGER npts
C      PARAMETER (npts = 45*365)
      PARAMETER (npts = 2327)
      REAL r(npts), x(npts), y(npts), utc(npts), lod(npts) 
      INTEGER yy, dd, jd, j
      CHARACTER*4 mo
      DOUBLE PRECISION sum, stopval
      REAL stepsize
      PARAMETER (stepsize = 0.05)

 9002 FORMAT (3E15.7)
      PRINT *,'At what frequency do you want to start?'
      READ (*,9002) start
      PRINT *,'At what freq. do you want to end?'
      READ (*,9002) stopval
      PRINT *,'What interval do you want to step with?'
      READ (*,9002) step 

!      start = start / stepsize
!      stopval = stopval / stepsize
!      step  = step / stepsize

      n = INT( (stopval-start)/step )
      IF (n .GE. maxfreqs) THEN
        STOP "would have tried to run with too many frequencies"
      ENDIF


      m = 43*365
      m = 15429
      m = 2327

      OPEN (10, FILE="cycle", FORM="FORMATTED", STATUS="OLD")
      DO i = 1, m
        READ(10,*) j, x(i), y(i)
      ENDDO

      f = start
      DO 1000 i = 1, n
        freq(i) = f * 2. * pi
        f       = f + step
 1000 CONTINUE

      CALL analy(x, m, freq, n)
      CALL analy(y, m, freq, n)
!      CALL analy(r, m, freq, n)
!      CALL analy(lod, m, freq, n)
!      CALL analy(utc, m, freq, n)

!      DO i = 1, m
!        lod(i) = lod(i)*r(i)
!      ENDDO
!      CALL analy(lod, m, freq, n)

      STOP
      END

      SUBROUTINE analy(x, m, freq, n)
      IMPLICIT none
      INTEGER m, n
      REAL x(m), freq(n)
      DOUBLE PRECISION sum
      CHARACTER*40 fname
      INTEGER i
      REAL pi
      PARAMETER (pi = 3.141592654)
      INTEGER maxfreqs
      PARAMETER (maxfreqs = 5000)
      REAL a(maxfreqs), b(maxfreqs)
      
C     Write out results:
      PRINT *,'What do you want to call the harmonic data file?'
      READ (*,9003) fname
      OPEN (12, FILE=fname, FORM='FORMATTED', STATUS='NEW')

C     Find to best fit amplitudes for the given frequencies.
      sum = 0.0
      DO i = 1, m
        sum = sum + x(i)
      ENDDO
      sum = sum / m
      PRINT *,'mean was ',sum
      DO i = 1, m
        x(i) = x(i) - sum
      ENDDO

      DO 1500 i = 1, n
        CALL harmrn(x, m, freq(i), a(i), b(i), 1)
CD        CALL harmrm(x, m, freq(i), a(i), b(i), 1)
 1500 CONTINUE
      DO 2000 i = 1, n
CD        WRITE (*,9002) freq(i)/2./pi, (a(i)*a(i)+b(i)*b(i))**.5,
CD     1              ATAN2(b(i),a(i))*180./pi 
        WRITE (12,9002) freq(i)/2./pi, (a(i)*a(i)+b(i)*b(i))**.5,
     1              ATAN2(b(i),a(i))*180./pi 
 2000 CONTINUE

 9002 FORMAT (3E15.7)

 9003 FORMAT (A40)

      RETURN
      END
