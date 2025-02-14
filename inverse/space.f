      PROGRAM space
C     Program to create inversions of real data maps.
C     For support of the data inversion report.
C     Base version from acor3.for 12-08-93.
C     Variant to work in space rather than time.
C
C     Subroutines:
C       Wght1  - 11-23-93 Create terms in weighting matrix for inversion
C              - 11-24-93 -- modified for determining boundary values
C              - 11-30-93 -- modified for autocorrelated errors.
C              - 12-08-93 -- boundary condition implementation fixed.
C       Tridig - 11-23-93 Invert Tri-diagonal matrix (Num. Recipes)
C       Correl - XX-XX-XX BG correlation functions.
C       Sample - 11-23-93 Determine whether data was successfully sampled.
C       Synth  - 11-24-93 Construct a synthetic series based on gappiness,
C                           If original series is half gaps, new series has
C                           time resolution of 2dt.
C       Readat - 12-09-93 Read in a month's worth of nsidc maps      
C       Flag   - 12-09-93 Flag points for data vs. no data
C       Locdat - 12-09-93 Find number of data points, 
C                           and start/finish of series
C       Trimmed -12-09-93 Recopy series so points 1 and ndat have data.
C       Writ   - 12-09-93 Write out the month of reanalyzed maps

      IMPLICIT none
      
      REAL dt, epsilon
      PARAMETER (dt = 1.0)
      INTEGER nday, nx, ny
      PARAMETER (nday =   1)
      PARAMETER (nx   = 304)
      PARAMETER (ny   = 448)
      REAL y(ny), z(ny), w(ny), out(ny)
      CHARACTER*1 x(nday, nx, ny)
      REAL a(ny), b(ny), c(ny), r(ny)

      INTEGER i, j, k
      INTEGER i0, in, ndat

      CHARACTER*60 base, tag
      SAVE

      OPEN (10, FILE='remin', FORM='FORMATTED', STATUS='OLD')

      PRINT *,'What magnitude of epsilon would you like?'
      READ (10,9001) epsilon
 9001 FORMAT (E13.6)
 9010 FORMAT (I3)

      PRINT *,'What is the base name of the output file?'
      READ (10,9009) base
      PRINT *,'What is the file tag?'
      READ (10,9009) tag
 9009 FORMAT (A60) 
   
C     Read in the maps
      CALL readat(x, nday, nx, ny, base, tag)

      DO 9998 j = 1, ny

C         Transfer the data for a point through time to a vector
          DO 1000 k = 1, nx
            y(k) = FLOAT(ICHAR(x(1,k,j)))
 1000     CONTINUE
          CALL flag(y, w, nx)
          CALL locdat(w, nx, i0, in, ndat)
          
          IF (ndat .LE. INT(0.1*FLOAT(nx)) ) THEN
            PRINT *,'Too few data points, i,j= ',k,j,ndat
            DO 1010 k = 1, nx
              z(k) = 168.            !168 = land
 1010       CONTINUE
           ELSE
CD            PRINT *,'j = ',j
CD            PRINT *,'i0, in, nx, ndat ',i0, in, nx, ndat
            CALL trimmed(y, z, w, i0, in, nx)
CD            PRINT *,'i0, in, nx, ndat ',i0, in, nx, ndat

C           Find the variational optimal series
            CALL wght1(z, a, b, c, r, dt, epsilon, ndat, w, 2)
            CALL tridig(a, b, c, r, out, ndat)
            IF (i0 .GT. 1) THEN
              DO 1020 k = 1, i0-1
                z(k) = 157.
 1020         CONTINUE
            ENDIF
            IF (in .LT. nx) THEN
              DO 1030 k = in+1, nx
                z(k) = 157.
 1030         CONTINUE
            ENDIF
            DO 1040 k = 1, ndat
              z(k+i0-1) = out(k)
 1040       CONTINUE
            
          ENDIF

          DO 2000 k = 1, nx
            WRITE (*,9019) ICHAR(x(1,k,j)), INT(z(k)+0.5)
 2000     CONTINUE
 9019     FORMAT (2I4)
           
          DO 1100 k = 1, nx
            x(1,k,j) = CHAR(INT(z(k)+0.5))
 1100     CONTINUE

 9997   CONTINUE       
 9998 CONTINUE

      PRINT *,'What is the base name for the output files?'
      READ (10,9009) base
      PRINT *,'What is the tag for the output files?'
      READ (10,9009) tag
      CALL writ(x, 1, nx, ny, base, tag)

      STOP
      END
