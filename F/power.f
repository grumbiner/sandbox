      PROGRAM power
C     Program to find fft of an input data vector
C     Robert Grumbine 27 September 1994

      IMPLICIT none

C     jdp must be greater than or equal to the number of data points
C       you will be analyzing.
      INTEGER jdp
      PARAMETER (jdp = 9200)

C     Arrays for the transform routine
      REAL x(jdp), tr(jdp), wk(3*jdp+150)
      COMPLEX hk(jdp)
      INTEGER iwk(3*jdp+150)

C     Arrays used here to hold spectrum information.
      REAL ampl(jdp), phase(jdp), freq(jdp)
C     REAL varian(jdp)

C     Detrend says whether of not to use linear detrending.
      LOGICAL detren, yes
      REAL a0, a1, avg
      INTEGER i

      REAL delta, fish
      INTEGER n, filtno
      CHARACTER*40 fname

C$EMA  wk, iwk, ampl, phase, freq

C     Read in the number of points to find the spectrum for.
      PRINT *,'How many data points are there?'      
      READ (*,9003) n 
C     Ensure that this is an even number: 
      n = (n/2)*2 

      PRINT *,'What is the time step?'
      READ (*,9001) delta 

      CALL readin(tr, n)

C     Call dtrnd if requested to subtract the best fit line from the data.
      PRINT *,'Do you want demeaning done?'
      detren = yes(.FALSE.)
      IF (detren) THEN
        DO 1000 i = 1, n
          x(i) = FLOAT(i)
 1000   CONTINUE
        CALL detrnd(x, tr, n, a0, a1, avg, detren)
      ENDIF

      PRINT *,'Would you like to filter the data?'
      IF (yes(.FALSE.)) THEN
        PRINT *,'Which filter would you like to use?'
        PRINT *,'      1 = Hann filter '
        PRINT *,'      2 = Split cosine filter'
        READ (*,9003) filtno
        CALL filt(tr, n, filtno)
      ENDIF

C     Find the transform of the input vector.
      CALL ftrc(tr, hk, 1.E-10, n, iwk, wk)

C     compute spectral information:
      DO 2000 i = 1, n/2+1
        ampl(i)   = (REAL(hk(i))**2+AIMAG(hk(i))**2)**.5
        phase(i)  = ATAN2( AIMAG(hk(i)), REAL(hk(i)) )
        freq(i)   = (i-1)/(n*delta)
C       varian(i) = ampl(i)*ampl(i)
 2000 CONTINUE

      fish = 0.0
      DO 3000 i = 2, n/2+1
        fish = ampl(i)*ampl(i) + fish
 3000 CONTINUE
      PRINT *,'The total variance was',fish

      PRINT *,'Do you want the output unformatted?'
      IF (yes(.TRUE.)) THEN

        PRINT *,'What do you want to call the amplitude file?'
        CALL ritout(ampl, n/2+1, 11)

        PRINT *,'What do you want to call the phase file?'
        CALL ritout(phase, n/2+1, 12)

        PRINT *,'What do you want to call the frequency file?'
        CALL ritout(freq, n/2+1, 13)

       ELSE

C       Write out info.
        PRINT *,'What do you want to call the output file?'
        READ (*,9009) fname
        OPEN (11, FILE = fname, FORM='FORMATTED', STATUS='NEW')
 
        WRITE (11, 9002)
     1    freq(1),CHAR(9), ampl(1),CHAR(9), phase(1)
        DO 4000 i = 2, n/2 + 1
          WRITE (11, 9002)
     1    freq(i),CHAR(9), ampl(i),CHAR(9), phase(i) 
 4000   CONTINUE

      ENDIF

 9001 FORMAT (E16.7)
 
 9002 FORMAT (E12.6,A1,F12.6,A1,F10.5)
 
 9003 FORMAT (I10)

 9004 FORMAT (6I6)

 9005 FORMAT (L14)
 
 9009 FORMAT (A40)

      END
