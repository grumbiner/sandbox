      PROGRAM sigdri
C     Program to drive the routines which test amplitudes for statistical
C       significance.
C     Written by Robert Grumbine 8-31-86.

      INTEGER n, ntot
      REAL amp(5000), freq(5000), x(5000), y(5000)
 
      INTEGER i, j, len, tnsfrq, nsfreq
      INTEGER k1, k2, k3
      LOGICAL subtr, match
      REAL alpmax, minmag, per, tide
      REAL sfreq(500), tsfreq(500)
      CHARACTER*60 fname 
       
      LOGICAL locmx
C     Locmx is a statement function to test to see if a frequency is a
C       local maximum in amplitude. 
      locmx(per) = ((amp(INT(FLOAT(ntot)*per+1)) .GT.
     1                       amp(INT(FLOAT(ntot)*per)+2)) .AND.
     2              (amp(INT(FLOAT(ntot)*per+1)) .GT.
     3                       amp(INT(FLOAT(ntot)*per)  ))      ) 

C     Read in the amplitudes and frequencies of the FFT'ed data.
      PRINT *,'How long was the data record?'
      READ (*,9001) ntot

      PRINT *,'How many frequencies are there?'
      READ (*,9001) n

      PRINT *,'What is the name of the amplitude file?'
      CALL readin(amp, n)

      PRINT *,'What is the name of the frequency file'
      CALL readin(freq, n)

C     Perform global test of significance.  If no points are significant,
C       the entire series is random.
      PRINT *,'What (global) significance level do you want to use?'
      READ (*,9002) alpmax
      DO 1000 i = 1, n
        x(i) = freq(i)
        y(i) = amp(i)
 1000 CONTINUE

      CALL signif(x, y, n, alpmax, .FALSE., sfreq, nsfreq)
      IF (nsfreq .EQ. 0) STOP 'The series is random.'
 
C     Get control for local significance testing.
      PRINT *,'How many frequencies do you want to analyze at a time?'
      READ (*,9001) len
      PRINT *,'What (local) significance level do you want to use?'
      READ (*,9002) alpmax

C     The 2000 loop pulls out the next (len) data points, and analyzes 
C       them for significant frequencies.
      tnsfrq = 0 
      DO 2000 j = 1, INT ( n/len )
        DO 2010 i = 1, len
          y(i) =  amp(i + (j-1)*len)
          x(i) = freq(i + (j-1)*len)
 2010   CONTINUE
CD       PRINT *, 'Ready to call signif.'
        CALL signif(x, y, len, alpmax, .TRUE., sfreq, nsfreq)
        DO 2020 i = 1, nsfreq
          tsfreq(i + tnsfrq) = sfreq(i)
 2020   CONTINUE
        tnsfrq = tnsfrq + nsfreq
 2000 CONTINUE

      IF (tnsfrq .EQ. 0) THEN
        PRINT *,'The entire spectrum is due to noise + continuum.'
        STOP
      ENDIF

C     Now test to see if the 'significant frequencies' are local maxima.
C     If a frequency is not a local max., then it cannot be significant,
C       at least on the first pass through.
      nsfreq = 0
CD     PRINT *,'Entering test of local max.'
      DO 3000 i = 1, tnsfrq
CD       WRITE (*,9002) tsfreq(i)
CD       WRITE(*,9002) amp(INT(tsfreq(i)*FLOAT(ntot)+0.) ),
CD    1                amp(INT(tsfreq(i)*FLOAT(ntot)+1.) ),
CD    2                amp(INT(tsfreq(i)*FLOAT(ntot)+2.) )
        IF ( tsfreq(i) .EQ. 0.0) THEN
          nsfreq = nsfreq + 1
          sfreq(nsfreq) = tsfreq(i)
          GO TO 3000
        ENDIF
        IF ( locmx(tsfreq(i)) ) THEN
          nsfreq = nsfreq + 1
          sfreq(nsfreq) = tsfreq(i)
        ENDIF
 3000 CONTINUE
CD     PRINT *,'passed test for local maxima'
      IF (nsfreq .EQ. 0) STOP 'No frequency is a local maximum' 


C     This is the point to check to see if any of the frequencies are
C       harmonics of each other.
C       Nothing is currently done about this possibility.

C     Test to ensure that the magnitude of the frequency is greater than
C       the cutoff value.
      PRINT *,'What is the minimum magnitude you want to use?'
      READ (*,9002) minmag
C     Minmag should be in terms of a fraction of the total variance.

      tnsfrq = nsfreq
      nsfreq = 0
      DO 5000 i = 1, tnsfrq
        IF (amp( INT(FLOAT(ntot)*tsfreq(i))+1 ) .GE. minmag) THEN
          nsfreq = nsfreq + 1
          sfreq(nsfreq) = tsfreq(i)
        ENDIF
 5000 CONTINUE
      IF (nsfreq .EQ. 0) STOP 'No frequencies are large enough to be 
     1physically meaningful.'

C     This is the point to call for harmonic analysis of the remaining
C       frequencies.

C     Now check to see if any frequencies are acceptably near tidal 
C       frequencies.
C     Write out frequencies for debugging purposes.
      PRINT *,'What do you want to call the signif. frequency file?'
      READ (*,9004) fname
      OPEN (12, FILE = fname, FORM='FORMATTED', STATUS='NEW')
      DO 6000 i = 1, nsfreq
        CALL tidefr(sfreq(i), k1, k2, k3, match, tide)
        sfreq(i) = tide
        WRITE(*,9003) k1, k2, k3, match, tide
        WRITE(12,9003) k1, k2, k3, match, tide
 6000 CONTINUE
CD     PRINT *,'passed check for tidal frequencies.'


 9001 FORMAT(I5)

 9002 FORMAT (3E15.7)

 9003 FORMAT (3I4,L3,E14.7)

 9004 FORMAT (A60)

      END
