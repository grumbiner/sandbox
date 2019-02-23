      PROGRAM iriter
C     Program to act as a front end to the harmonic analysis routines.
C     Robert Grumbine 8-25-86.
C     Customized 11-6-86 to be for a 'small' number of data points,
C       and to examine multiple files at the same set of frequencies.  BG
C     Version to analyze irregularly spaced data points.  3-2-87 BG

      INTEGER m, n
      REAL a(400), b(400), freq(400)
      REAL x(10000), time(10000)

      INTEGER i, unit
      CHARACTER*60 fname
      LOGICAL yes
      REAL start, end, step, f
      REAL ta(400), tb(400), tf(400)
      REAL cond(10000), rcond

      REAL pi
      PARAMETER (pi = 3.141592654)

      unit = 12

C     PRINT *,'How many frequencies do you want to analyze?'
C     READ (*,9001) n

C     DO 1000 i = 1, n
C       PRINT *,'Enter the frequency (cph)'
C       READ (*,9002) freq(i)
C       freq(i) = freq(i) * 2.*pi
C1000 CONTINUE

 3000 CONTINUE
C       Repeat until loop, use the given set of frequencies for all files.
        unit = unit + 1
       
        PRINT *,'How many data points are there?'
        READ (*,9001) m
  
        CALL readin(x   , m)
        CALL readin(time, m)

C       Find to best fit amplitudes for the given frequencies.
        PRINT *,'What is the first frequency?'
        READ (*,9002) start
        PRINT *,'What is the final frequency?'
        READ (*,9002) end
        PRINT *,'What is the step?'
        READ (*,9002) step

        start = start*2.*pi
        end   = end  *2.*pi
        step  = step *2.*pi

        DO 1000 f = start, end, step
          freq(1) = f
          tf( INT((f-start)/step + 1.) ) = f
          CALL harmin(x, time, m, freq, a, b, 1, rcond)
          ta( INT((f-start)/step + 1.) ) = a(1)
          tb( INT((f-start)/step + 1.) ) = b(1)
          cond( INT((f-start)/step + 1.) ) = -ALOG10(rcond)
          WRITE (*,9002) f/2./pi, (a(1)*a(1)+b(1)*b(1))**.5,
     1              ATAN2(b(1),a(1)), -ALOG10(rcond) 
 1000   CONTINUE
 
C       Write out results:
        PRINT *,'What do you want to call the harmonic data file?'
        READ (*,9003) fname
        OPEN (unit, FILE=fname, FORM='FORMATTED', STATUS='NEW')

        DO 2000 i = 1, INT( (end-start)/step )
          WRITE (*,9002) tf(i)/2./pi, (ta(i)*ta(i)+tb(i)*tb(i))**.5,
     1              ATAN2(tb(i),ta(i)), cond(i) 
          WRITE (unit,9002) tf(i)/2./pi, (ta(i)*ta(i)+tb(i)*tb(i))**.5,
     1              ATAN2(tb(i),ta(i)), cond(i) 
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

 9002 FORMAT (4E14.6)

 9003 FORMAT (A60)

      END
