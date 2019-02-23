C***********************************************************----------!!
      PROGRAM superm
C     Program to analyze all statistically significant frequencies in
C       a data record.
C     BG 8-26-86.
C     Multiple pass.
C     Extended to process multiple files 10-2-86.
C     Amplitude test modified to consider amplitudes greater than a speci-
C       fied as a fraction of the total variance 10-2-86.
C     Extended so that the mean may be analyzed as well.  This removes need
C       for preliminary detrending.  12-8-86

C     jdp must be greater than or equal to the number of data points
C       you will be analyzing.
      INTEGER jdp
      PARAMETER (jdp = 10000)

C     Arrays for the transform routine
      REAL wk(3*jdp+150)
      COMPLEX hk(jdp)
      INTEGER iwk(3*jdp+150)

C     Global variables: 
      REAL orig(jdp), amp(jdp), phase(jdp), freq(jdp)
      LOGICAL detren, yes
      INTEGER i, j
      REAL x(jdp), y(jdp), raw(jdp) 
      INTEGER nfft, size
      CHARACTER*60 fname

C     Spectral analysis variables:
      REAL delta, varold, varnew
      INTEGER filtno
      REAL a0, a1, avg
      LOGICAL filtr

C     variables for significance and tidal testing:
      REAL minmag, minamp, tide, per, gsig, lsig
      REAL sfreq(400), tsfreq(400)
      INTEGER len, nsfreq, tnsfrq
      INTEGER k1, k2, k3
      LOGICAL locmx, match, newfrq
      REAL toler

C     variables for harmonic analysis:
      REAL hfreq(400), a(400), b(400)
      REAL pi
      INTEGER nhfreq
      REAL ftemp

      PARAMETER (pi = 3.141592654)

CHP$EMA  wk, iwk, hk 

C     Statement function used in determining whether a frequency is at
C       a local maximum in amplitude.
C     Add .01 to index before truncation to provide pseudo-rounding.
      locmx(per) = ((amp(INT(FLOAT(size)*per+1.001)) .GT.
     1                       amp(INT(FLOAT(size)*per+2.001)) ).AND.
     2              (amp(INT(FLOAT(size)*per+1.001)) .GT.
     3                       amp( INT(FLOAT(size)*per+.001)) )    ) 


C***********************************************************----------!!
C     Get variables which will not be changing between different file 
C        analyses.

      PRINT *,'What is the time step?'
      READ (*,9001) delta 

      PRINT *,'Do you want detrending done?'
      detren = yes(.FALSE.)

C     Get control for significance testing.
      PRINT *,'What (global) significance level do you want to use?'
      READ (*,9001) gsig

      PRINT *,'How many frequencies do you want to analyze at a time?'
      READ (*,9003) len
      PRINT *,'What (local) significance level do you want to use?'
      READ (*,9001) lsig

      PRINT *,'What is the minimum magnitude (as a fraction of the total
     1 variance) you want to use?'
      READ (*,9001) minmag

      PRINT *,'Would you like to filter the data?'
      filtr = yes(.FALSE.)

      PRINT *,'What tolerance do you want for tidal frequencies?'
      READ (*,9001) toler

C     Repeat until point for multiple file processing.
C     Some variables must be read in for each new file.
 9999 CONTINUE

C     Read in the number of points to find the spectrum for.
      PRINT *,'How many data points are there?'      
      READ (*,9003) size 
      CALL readin(orig, size)

      PRINT *,'What do you want to call the signif. frequency file?'
      READ (*,9006) fname
      OPEN (11, FILE = fname, FORM='FORMATTED', STATUS='NEW')

C     Get variables for harmonic analysis.
      PRINT *,'What do you want to call the harmonic data file?'
      READ (*,9006) fname
      OPEN (12, FILE=fname, FORM='FORMATTED', STATUS='NEW')

      varold = 1.E36 
      nhfreq = 0

C     Transfer data to a different array, where it may be changed during
C       the program.
      DO 1000 i = 1, size
        raw(i) = orig(i)
 1000 CONTINUE

      IF (filtr) THEN
        PRINT *,'Which filter would you like to use?'
        PRINT *,'      1 = Hann filter '
        PRINT *,'      2 = Split cosine filter'
        READ (*,9003) filtno
        CALL filt(raw, size, filtno)
      ENDIF

C***********************************************************----------!!
C     This is the place to loop (repeat forever loop, must jump out).
 1    CONTINUE

C     Call dtrnd if requested to subtract the best fit line from the data.
      IF (detren) THEN
        DO 1100 i = 1, size
          x(i) = FLOAT(i)
 1100   CONTINUE
        CALL detrnd(x, raw, size, a0, a1, avg, detren)
      ENDIF

C     Find the transform of the input vector.
      nfft = size/2 + 1
      CALL ftrc(raw, hk, 1.E-10, 2*(size/2), iwk, wk)

C     compute spectral information:
      DO 1200 i = 1, nfft
        amp(i)    = (REAL(hk(i))**2+AIMAG(hk(i))**2)**.5
C       phase(i)  = ATAN2( AIMAG(hk(i)), REAL(hk(i)) )
        freq(i)   = FLOAT(i-1)/(FLOAT(size)*delta)
 1200 CONTINUE

      varnew = 0.0
      DO 1300 i = 1, nfft 
        varnew = amp(i)*amp(i) + varnew
 1300 CONTINUE
      PRINT *,'The total variance was',varnew

C     If the change in variance is below the minimum magnitude specified
C       for a single frequency, go to the harmonic analysis.
      IF ( (varold-varnew)/varold .LT. minmag) GO TO 2

C***********************************************************----------!!
C     Perform global test of significance.  If no points are significant,
C       the entire series is random.
      DO 2000 i = 1, nfft
        x(i) = freq(i)
        y(i) = amp(i)
 2000 CONTINUE

      PRINT *,'global significance'
      CALL signif(x, y, nfft, gsig, .TRUE., sfreq, nsfreq, .TRUE.)
      IF (nsfreq .EQ. 0) THEN
        PRINT *,'The remainder of the series is random about a linear sp
     1ectral trend'
        GO TO 2
      ENDIF 

C     Local significance testing.
C     Modified 12-9-87 to go through all tests for each subsection alone.
C       The goal of this is to allow for a subroutine to search for the
C       frequency of maximum response (in a small interval near the fourier
C       frequency) and demodulate using it.  BG
CD     PRINT *,'local significance'
      newfrq = .FALSE.
      DO 2100 j = 1, INT ( nfft/len )
        tnsfrq = 0 
        PRINT *,'local test # ',j
        DO 2110 i = 1, len
          y(i) =  amp(i + (j-1)*len)
          x(i) = freq(i + (j-1)*len)
 2110   CONTINUE

CD       PRINT *, 'Ready to call signif.'
        CALL signif(x, y, len, lsig, .TRUE., sfreq, nsfreq, .FALSE.)
        DO 2120 i = 1, nsfreq
          tsfreq(i + tnsfrq) = sfreq(i)
 2120   CONTINUE
        tnsfrq = tnsfrq + nsfreq
        IF (nsfreq .EQ. 0) THEN
          PRINT *,'no sig. freqs in this subsection'
          GO TO 2100
        ENDIF

C       Now test to see if the 'significant frequencies' are local maxima.
        nsfreq = 0
CD       PRINT *,'Entering test of local max.'
        DO 2200 i = 1, tnsfrq
          IF ( tsfreq(i) .EQ. 0.0) THEN
            nsfreq = nsfreq + 1
            sfreq(nsfreq) = tsfreq(i)
            GO TO 2200
          ENDIF
          IF ( locmx(tsfreq(i)) ) THEN
            nsfreq = nsfreq + 1
            sfreq(nsfreq) = tsfreq(i)
          ENDIF
 2200   CONTINUE

CD       PRINT *,'passed test for local maxima'
        IF (nsfreq .EQ. 0) THEN
          PRINT *, 'No remaining frequency is a local maximum' 
          GO TO 2100
        ENDIF

C       Test to ensure that the magnitude of the frequency is greater than
C         the cutoff value.
        tnsfrq = nsfreq
        nsfreq = 0
        minamp = .5*SQRT(minmag*varnew)
CD       PRINT *,minamp,' = minamp', tnsfrq,'  = tnsfrq'
        DO 2300 i = 1, tnsfrq
          IF (amp( INT(FLOAT(size)*tsfreq(i))+ 1) .GE. minamp) THEN
            nsfreq = nsfreq + 1
            sfreq(nsfreq) = tsfreq(i)
          ENDIF
 2300   CONTINUE
CD       PRINT *,'past test for minimum amplitude'
        IF (nsfreq .EQ. 0) THEN 
        PRINT *, 'No frequency has a large enough amplitude to be physic
     1ally meaningful in this subsection.'
          GO TO 2100
        ENDIF

C       Now look for frequencies which are acceptably near tidal frequencies.
C       Write out frequencies for debugging purposes.
CD       PRINT *,'calling tidefr'
        DO 2400 i = 1, nsfreq
          CALL tidefr(sfreq(i), k1, k2, k3, match, tide, toler)
          sfreq(i) = tide
          WRITE(* ,9007) tide, k1, k2, k3, match
          WRITE(11,9007) tide, k1, k2, k3, match
 2400   CONTINUE

C***********************************************************----------!!
C       This is the place to check for the best frequency to analyze.


C       Find to best fit amplitudes for the given frequencies.
C       Convert to radians for the analysis.
        DO 3000 i = 1, nsfreq
          sfreq(i) = sfreq(i)*2.*pi
 3000   CONTINUE
        DO 3001 i = 1, nsfreq
          x(i) = FLOAT(i)
 3001   CONTINUE
C       sort so that the mean may be detected if present.
        CALL sort(x, sfreq, nsfreq, 2, 1)
        PRINT *,'calling the harmonic analysis routines'
        IF (sfreq(nsfreq) .EQ. 0.0) THEN
C         swap so that the mean is in the first location.
          ftemp         = sfreq(1)
          sfreq(1)      = 0.0
          sfreq(nsfreq) = ftemp
          PRINT *,'Analysis includes the mean'
          CALL harmrm(orig, size, sfreq, a, b, nsfreq)
         ELSE
C         The mean is not statistically significant
          CALL harmrn(orig, size, sfreq, a, b, nsfreq)
        ENDIF

C       Write out results and transfer frequencies to hfreq for global 
C         analysis at the end of the program:
        DO 3100 i = 1, nsfreq
          nhfreq = nhfreq + 1
          hfreq(nhfreq) = sfreq(i)
          WRITE (* ,9008) sfreq(i)/2./pi, a(i), b(i)
          WRITE (12,9008) sfreq(i)/2./pi, a(i), b(i)
 3100   CONTINUE

C       Subtract the analyzed components from the data.
        CALL demod (a, b, sfreq, nsfreq, raw, size)

C       Find the transform of the input vector.
C       Must recompute for any section that has a significant frequency
        CALL ftrc(raw, hk, 1.E-10, 2*(size/2), iwk, wk)

C       compute spectral information:
        DO 5000 i = 1, nfft
          amp(i)    = (REAL(hk(i))**2+AIMAG(hk(i))**2)**.5
C         phase(i)  = ATAN2( AIMAG(hk(i)), REAL(hk(i)) )
          freq(i)   = FLOAT(i-1)/(FLOAT(size)*delta)
 5000   CONTINUE
        newfrq = .TRUE.

 2100 CONTINUE

      IF ( .NOT. newfrq ) THEN
        PRINT *,'The remaining spectrum is due to noise + continuum.'
        GO TO 2
      ENDIF

C     Add a shut off for the case that further analysis gives no imporvement, 
C       or even makes the fit worse.  BG 12-9-87
      IF (varold .LE. varnew) GO TO 2
C     Update the variance value:
      varold = varnew

C     Go back to the start to see if there is anything left.
      GO TO 1

C***********************************************************----------!!

C     Do the global harmonic analysis on the original data series.
 2    CONTINUE

C     Before analyzing the frequencies, ensure that there are no duplicate
C       frequencies in the list.  If there are any, the analysis will fail,
C       and the program will crash.
      DO 4001 i = 1, nhfreq
        x(i) = FLOAT(i)
 4001 CONTINUE
      CALL sort(x, hfreq, nhfreq, 2, 1)

      nsfreq = 0
      DO 4002 i = 1, nhfreq-1 
        IF (hfreq(i) .NE. hfreq(i+1)) THEN
          nsfreq = nsfreq + 1
          sfreq(nsfreq) = hfreq(i)
        ENDIF
 4002 CONTINUE
      nsfreq = nsfreq+1
      sfreq(nsfreq) = hfreq(i)


      PRINT *,'Starting global analysis, detrending.'
      DO 4010 i = 1, size
        x(i) = FLOAT(i)
 4010 CONTINUE
      CALL detrnd(x, orig, size, a0, a1, avg, detren)

      PRINT *,'Starting global analysis, harmonic analysis.'
      IF (sfreq(nsfreq) .EQ. 0.0) THEN
C       swap so that the mean is in the first location.
        ftemp         = sfreq(1)
        sfreq(1)      = 0.0
        sfreq(nsfreq) = ftemp
        PRINT *,'Analysis includes the mean'
        CALL harmrm(orig, size, sfreq, a, b, nsfreq)
       ELSE
C       The mean is not statistically significant
        CALL harmrn(orig, size, sfreq, a, b, nsfreq)
      ENDIF

      PRINT *,'Starting global analysis, demodulation.'
      CALL demod(a, b, sfreq, nsfreq, orig, size)

      WRITE (* ,9009) a0, a1, avg, detren
      WRITE (12,9009) a0, a1, avg, detren

      DO 4000 i = 1, nsfreq
        WRITE (* ,9008) a(i), b(i), sfreq(i)/2./pi,
     1                 (a(i)*a(i)+b(i)*b(i))**.5, ATAN2(b(i),a(i))
        WRITE (12,9008) sfreq(i)/2./pi,
     1                 (a(i)*a(i)+b(i)*b(i))**.5, ATAN2(b(i),a(i))
 4000 CONTINUE

      PRINT *,'What do you want to call the residual file?'
      CALL ritout(raw, size, 13)

      CLOSE (11, STATUS='KEEP')
      CLOSE (12, STATUS='KEEP')
      CLOSE (13, STATUS='KEEP')

      PRINT *,'Would you like to analyze another file?'
      IF (yes(.FALSE.)) GO TO 9999

C***********************************************************----------!!

 9001 FORMAT (E14.6)
 
 9002 FORMAT (F14.6,3E14.6)
 
 9003 FORMAT (I10)

 9004 FORMAT (6I6)

 9005 FORMAT (L14)
 
 9006 FORMAT (A60)

 9007 FORMAT (E14.7,3I4,L3)

 9008 FORMAT (E13.6,5F10.4)

 9009 FORMAT (' The best fit line through the data is',E13.6,'+',E13.6,
     1         '*i.'/'The average was',E13.6,'.  Was detrending done?',
     2         L7)

      END
