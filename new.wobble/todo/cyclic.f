      PROGRAM cyclic
C Do a cyclic/iterative descent through a time series, looking for
C   periods of large amplitude.
C Build off the ideas of hiter series of program (begun in 1987 MS Thesis)
C Read in data to vector
C
C Input provides a frequency resolution and range over which to conduct
C   harmonic analysis.
C The iterative aspect begins with those results -- the largest amplitude
C   term found, and any at least as large as half that size, are then
C   subtracted from the series and the process is continued.
C
C This version of the program is tailored to the orbital analysis problem
C   in 'Source of the Chandler Wobble', R. Grumbine, submitted 2010.
 
      IMPLICIT none

C Variables for the iterative (hiter) frequency search/removal
      INTEGER maxfreqs, nfreqs
      PARAMETER (maxfreqs = 128000)
      DOUBLE PRECISION a(maxfreqs), b(maxfreqs), freq(maxfreqs)
      DOUBLE PRECISION amplitude(maxfreqs), phase(maxfreqs)
      DOUBLE PRECISION first, last, step

C Mathematics
      DOUBLE PRECISION pi

C Data series
      DOUBLE PRECISION, ALLOCATABLE :: x(:)
      INTEGER npts
      DOUBLE PRECISION dt
      CHARACTER*80 fname, fbase

C Misc
      INTEGER i, iter, itmax, fmax
      DOUBLE PRECISION sum, mean

C -------- Begin executable section -----------
C Initialize:
      pi = DABS(DACOS(-1.D0))

      !PRINT *,'File with instructions?'
      READ (*,*) fname
      OPEN (10, FILE=fname, FORM="FORMATTED", STATUS="OLD")
      READ (10,*) dt
      READ (10,*) npts
      ALLOCATE (x(npts))

      !Frequencies in cycles per day
      READ (10,*) first
      READ (10,*) last
      READ (10,*) step
      nfreqs = NINT((last-first)/step)
      IF (nfreqs .GE. maxfreqs-1) THEN
        PRINT *,"would have tried to run with too many frequencies ", 
     1                nfreqs," vs ",maxfreqs
        STOP
      ENDIF
      DO i = 1, nfreqs
        freq(i) = (first + (i-1)*step)*2.D0*pi*dt
      ENDDO

      READ (10,*) fname
      OPEN (11, FILE=fname, FORM="FORMATTED", STATUS="OLD")

      !Read in data and ensure mean is zero:
      sum = 0.D0
      DO i = 1, npts
        READ (11, *) x(i)
        sum = sum + x(i)
      ENDDO
      mean = sum / npts
      !PRINT *,'mean was ',mean
      x = x - mean




C Now run through the frequencies one at a time and find the amplitudes:
!to move to writing harmonics to data file, subtracted data to another file.
C also to move this stretch to a subroutine
      WRITE (*, *) 0, ",", mean,
     1             ",",   ATAN2(b(i),a(i))*180./pi,",",  99999.
      READ (10,*) itmax
      READ (10,*) fbase

      DO iter = 1, itmax
        WRITE (fname, 9002) iter
 9002   FORMAT (I2)
        !OPEN (11 + iter, FILE=fname, FORM="FORMATTED", STATUS="UNKNOWN")
        OPEN (11 + iter, FORM="FORMATTED", STATUS="UNKNOWN")

!Now step through frequency space and find best harmonic fits
        DO i = 1, nfreqs
          CALL harmrn(x, npts, freq(i), a(i), b(i), 1)
          WRITE (11+iter, *) freq(i)/2./pi/dt,",",  
     1                 SQRT(a(i)*a(i)+b(i)*b(i)), ",",  
     2                 ATAN2(b(i),a(i))*180./pi,",",  2.D0*pi/freq(i)*dt
          amplitude(i) = SQRT(a(i)*a(i)+b(i)*b(i))
          phase(i)     = ATAN2(b(i),a(i))*180./pi
        ENDDO

! Print out large amplitude components
        PRINT *,'iter ',iter,' max amplitude = ',MAXVAL(amplitude)
        DO i = 1, nfreqs
          IF (amplitude(i) .GE. 0.5 * MAXVAL(amplitude) ) THEN
            IF (amplitude(i) .EQ. MAXVAL(amplitude) ) fmax = i
            WRITE (*, *) freq(i)/2./pi/dt,",",
     1                 amplitude(i),",", phase(i),",", 
     2                 2.D0*pi*dt/freq(i)
          ENDIF
        ENDDO

! subtract out the maximum amplitude component:
        PRINT *,iter,' iter, extracting ',freq(fmax)/2./pi/dt, 
     1             amplitude(fmax), 
     1             phase(fmax), 2.*pi*dt/freq(fmax)
        DO i = 1, npts
             x(i) = x(i) - a(fmax)*dcos(DBLE(i)*freq(fmax) )
     1                   - b(fmax)*dsin(DBLE(i)*freq(fmax) )
        ENDDO

        CLOSE (11+iter)
      ENDDO

      END
