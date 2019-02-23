      PROGRAM hiter
C     Program to act as a front end to the harmonic analysis routines.
C     Robert Grumbine 8-25-86.
C     Version to individually compute the components of frequencies
C       which are too closely spaced for matrix handling. 

      INTEGER m, n
      REAL a(400), b(400), freq(400)
      REAL x(10000)

      INTEGER i
      CHARACTER*40 fname
      LOGICAL yes
      REAL f, start, stop, step

      REAL pi
      PARAMETER (pi = 3.141592654)

      PRINT *,'How many data points are there?'
      READ (*,9001) m

      PRINT *,'At what frequency do you want to start?'
      READ (*,9002) start
      PRINT *,'At what freq. do you want to end?'
      READ (*,9002) stop
      PRINT *,'What interval do you want to step with?'
      READ (*,9002) step 

      n = INT( (stop-start)/step )

      CALL readin(x, m)

      f = start
      DO 1000 i = 1, n
        freq(i) = f * 2. * pi
        f       = f + step
 1000 CONTINUE

C     Find to best fit amplitudes for the given frequencies.
      DO 1500 i = 1, n
        CALL harmrn(x, m, freq(i), a(i), b(i), 1)
 1500 CONTINUE

C     Write out results:
      PRINT *,'What do you want to call the harmonic data file?'
      READ (*,9003) fname
      OPEN (12, FILE=fname, FORM='FORMATTED', STATUS='NEW')

      DO 2000 i = 1, n
        WRITE (*,9002) freq(i)/2./pi, (a(i)*a(i)+b(i)*b(i))**.5,
     1              ATAN2(b(i),a(i))*180./pi 
        WRITE (12,9002) freq(i)/2./pi, (a(i)*a(i)+b(i)*b(i))**.5,
     1              ATAN2(b(i),a(i))*180./pi 
 2000 CONTINUE

C     Subtract the analyzed components from the data.
      PRINT *,'Do you want to subtract the best fit harmonics?'
      IF (yes(.FALSE.)) THEN 
        CALL demod (a, b, freq, n, x, m)

        PRINT *,'What do you want to call the residual file?'
        CALL ritout(m, x, 11)

      ENDIF

 9001 FORMAT (I5)   

 9002 FORMAT (3E15.7)

 9003 FORMAT (A40)

      END
