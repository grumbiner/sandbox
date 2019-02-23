      PROGRAM hcust
C     Program to act as a front end to the harmonic analysis routines.
C     Robert Grumbine 8-25-86.
C     Customized 11-6-86 to be for a 'small' number of data points,
C       and to examine multiple files at the same set of frequencies.  BG

      IMPLICIT none

      INTEGER m, n
      REAL a(400), b(400), freq(0:400)
      REAL x(10000)

      INTEGER i, unit
      CHARACTER*60 fname
      LOGICAL yes

      REAL pi
      PARAMETER (pi = 3.141592654)

      unit = 12

      PRINT *,'How many frequencies do you want to analyze?'
      READ (*,9001) n

      freq(0) = 0.0
      DO 1000 i = 1, n
        PRINT *,'Enter the frequency (cph)'
        READ (*,9002) freq(i)
        freq(i) = freq(i) * 2.*pi
 1000 CONTINUE

 3000 CONTINUE
C       Repeat until loop, use the given set of frequencies for all files.
        unit = unit + 1
 
        PRINT *,'How many data points are there?'
        READ (*,9001) m
  
        CALL readin(x, m)

C       Find to best fit amplitudes for the given frequencies.
        IF (freq(1) .EQ. 0.0) THEN
          CALL harmrm(x, m, freq, a, b, n)
         ELSE
          CALL harmrn(x, m, freq, a, b, n)
        ENDIF

C       Write out results:
        PRINT *,'What do you want to call the harmonic data file?'
        READ (*,9003) fname
        OPEN (unit, FILE=fname, FORM='FORMATTED', STATUS='NEW')

        DO 2000 i = 1, n
          WRITE (*,9002) freq(i)/2./pi, (a(i)*a(i)+b(i)*b(i))**.5,
     1              ATAN2(b(i),a(i)) 
          WRITE (unit,9002) freq(i)/2./pi, (a(i)*a(i)+b(i)*b(i))**.5,
     1              ATAN2(b(i),a(i)) 
 2000   CONTINUE

C       Subtract the analyzed components from the data.
        PRINT *,'Do you want to subtract the best fit harmonics?'
        IF (yes(.FALSE.)) THEN 
          CALL demod (a, b, freq, n, x, m)

          PRINT *,'What do you want to call the residual file?'
          CALL ritout(x, m, 11)
  
        ENDIF

        CLOSE (unit, STATUS='KEEP')
        PRINT *,'Do you want to process another file?'
      IF (yes(.FALSE.)) GO TO 3000

 9001 FORMAT (I5)   

 9002 FORMAT (3E14.6)

 9003 FORMAT (A60)

      END
