      PROGRAM inverse
C     Program to create inversions of real data maps.
C     For support of the data inversion report.
C     Base version from acor3.for 12-08-93.
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
      PARAMETER (nday =  31)
      PARAMETER (nx   = 304)
      PARAMETER (ny   = 448)
      REAL y(nday), z(nday), w(nday), out(nday)
      CHARACTER*1 x(nday, nx, ny)
      REAL a(nday), b(nday), c(nday), r(nday)

      INTEGER i, j, k
      INTEGER i0, in, ndat

      CHARACTER*60 base, tag

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
        DO 9997 i = 1, nx

C         Transfer the data for a point through time to a vector
          DO 1000 k = 1, nday
            y(k) = FLOAT(ICHAR(x(k,i,j)))
 1000     CONTINUE
          CALL flag(y, w, nday)
          CALL locdat(w, nday, i0, in, ndat)
        
          IF (ndat .EQ. 0) THEN
            DO 1010 k = 1, nday
              z(k) = 168.            !168 = land
 1010       CONTINUE
           ELSE
            CALL trimmed(y, z, w, i0, in, nday)
         
C           Find the variational optimal series
            CALL wght1(z, a, b, c, r, dt, epsilon, ndat, w, 2)
            CALL tridig(a, b, c, r, out, ndat)
            IF (i0 .GT. 1) THEN
              DO 1020 k = 1, i0-1
                z(k) = 157.
 1020         CONTINUE
            ENDIF
            IF (in .LT. ndat) THEN
              DO 1030 k = in+1, nday
                z(k) = 157.
 1030         CONTINUE
            ENDIF
            DO 1040 k = 1, ndat
              z(k+i0-1) = out(k)
 1040       CONTINUE
            
          ENDIF

          DO 1100 k = 1, nday
            x(k,i,j) = CHAR(INT(z(k)+0.5))
 1100     CONTINUE

 9997   CONTINUE       
 9998 CONTINUE

      PRINT *,'What is the base name for the output files?'
      READ (10,9009) base
      PRINT *,'What is the tag for the output files?'
      READ (10,9009) tag
      CALL writ(x, nday, nx, ny, base, tag)

 9003 FORMAT (6F7.3, I4)

      STOP
      END

      SUBROUTINE locdat(w, nday, i0, in, ndat)
C     Locdat locates the start and finish of a time series in which there
C       may be missing points.
      INTEGER nday, ndat, i0, in
      REAL w(nday)

      INTEGER i

      ndat = 0
      i0   = 0
      DO 1000 i = 1, nday
        IF (w(nday) .NE. 0) THEN
          IF (i0 .EQ. 0) i0 = i
          in = i
          ndat = ndat + 1
        ENDIF
 1000 CONTINUE
 
      RETURN
      END

      SUBROUTINE trimmed(y, z, w, i0, in, nday)
C     Transfer series to start and finish with real data.
      INTEGER i0, in, nday
      REAL y(nday), z(nday), w(nday), w2(3000)

      INTEGER i

      DO 1000 i = i0, in
        z(i-i0+1) = y(i)
        w2(i-i0+1) = w(i)
 1000 CONTINUE
      DO 2000 i = 1, in-i0+1
        w(i) = w2(i)
 2000 CONTINUE

      RETURN
      END

      SUBROUTINE flag(y, w, nday)
C     Flag (put zero weights in w) those days for which there is
C       no data.  Currently do not distinguish between points which
C       have no data and those which are land.  12-09-93
      INTEGER nday
      REAL y(nday), w(nday)

      INTEGER i

      DO 1000 i = 1, nday
        IF (y(nday) .GT. 100.) THEN
          w(i) = 0.0
         ELSE
          w(i) = 1.0
        ENDIF
 1000 CONTINUE

      RETURN
      END
          
      SUBROUTINE synth(x, x2, w, n, gaps, useall, m, w2)
C     Subroutine to construct a synthetic time series, subsampled
C       proportional to 1/(1-gaps).  w determines whether there is
C       a gap at a given time.  
C       Useall is a logical which controls whether the full time
C        series (ignoring gaps) is to be used.
      INTEGER n, m
      REAL gaps, x(n), x2(n), w(n), w2(n)
      LOGICAL useall

      INTEGER i, j, k, nsamp, resamp

      resamp = INT(1./(1.-gaps))
      m = n / resamp

      IF (resamp .EQ. 1) THEN
        DO 1 i = 1, n
          x2(i) = x(i)
          w2(i) = 1.0
   1    CONTINUE
        RETURN
      ENDIF

C     Note, must deal with series which are not integral multiples.
C     Currently ignoring. 11-24-93 BG      
      IF (useall) THEN
        DO 1000 i = 1, n, resamp
          k = 1+i/resamp
          x2(k) = 0.
          DO 1100 j = 1, resamp
            x2(k) = x2(k)+x(i-1+j)
 1100     CONTINUE
          x2(k) = x2(k)/resamp
 1000   CONTINUE
      
       ELSE
        DO 2000 i = 1, n, resamp
          k = 1+i/resamp
          x2(k) = 0.
          w2(k) = 0
          nsamp = 0
          DO 2100 j = 1, resamp
            IF (w(i+j-1) .NE. 0) THEN
              nsamp = nsamp + 1
              x2(k) = x2(k) + x(i+j-1)
              w2(k) = 1.
            ENDIF
 2100     CONTINUE
          IF (nsamp .NE. 0) THEN
            x2(k) = x2(k) / nsamp
          ENDIF
 2000   CONTINUE

      ENDIF

      RETURN
      END

      SUBROUTINE wght1(y, a, b, c, r, dt, epsilon, n, w, type)
C     11-23-93 Fill the vectors with their weights for the
C        constrained problem.  Y is input vector, a, b, c
C        are the diagonals, and r is the forcing vector in Ax = r.
C     11-23-93 Extended to work with gappy time series.
C     11-24-93 Added 'type' to handle which type of boundary treatment.
C               1 -> use 0 boundary values.
C               2 -> use natural variational conditions.
C     11-26-93 Work with computing boundary values in a gappy time
C               series.
C     11-30-93 Handle autocorrelated noise.
C     11-30-93 Fix error, e2dt should be epsilon/dt, not times.
C     12-08-93 Fix error, boundary condition is 1+epsilon^2, not 2*epsilon^2

      INTEGER n, type
      REAL y(n), r(n), a(n), b(n), c(n), w(n)
      REAL dt, epsilon

      REAL e2dt, e2
      INTEGER i

      e2dt = (epsilon/dt)**2
      e2 = epsilon*epsilon

      IF (type .EQ. 1) THEN
        a(1) = 0.
        b(1) = 1.
        c(1) = 0.
        r(1) = 0.95
        a(n) = 0.
        b(n) = 1.
        c(n) = 0.
        r(n) = 0.95
       ELSE IF (type .EQ. 2) THEN
C       9000 loops handle gaps near ends of series.        
        a(1) = 0.
        b(1) = -1.
        c(1) = 1.        
        DO 9000 i = 2, n
          IF (w(i) .NE. 0.) THEN
            r(1) = (y(i)-y(1))/(1+e2)/(i-1)
            GO TO 9001
          ENDIF
 9000   CONTINUE
 9001   CONTINUE
        a(n) = -1.
        b(n) = 1.
        c(n) = 0.
        DO 9100 i = n-1, 1, -1
          IF (w(i) .NE. 0.) THEN
            r(n) = (y(n)-y(i))/(1+e2)/(n-i)
            GO TO 9101
          ENDIF
 9100   CONTINUE
 9101   CONTINUE
       ELSE
        PRINT *,'Type of boundary treatment out of range'
        STOP
      ENDIF

      DO 1000 i = 2, n-1
          a(i) =  e2dt 
          b(i) = -w(i) - 2.*e2dt
          c(i) =  e2dt 
          r(i) = -w(i)*y(i)
 1000 CONTINUE

      RETURN
      END
      
      SUBROUTINE tridig(a, b, c, r, u, n)
C     Subroutine to solve a tri-diagonal matrix problem
C       Au = r, where the diagonals of A are a, b, c
C     From Numerical Recipes
      INTEGER nmax 
      PARAMETER (nmax = 4*304*448)
      REAL gam(nmax), a(n), b(n), c(n), r(n), u(n)
      REAL bet

      IF (nmax .LT. n) THEN
        PRINT *,'Need more work space'
        STOP
      ENDIF
      IF (b(1) .EQ. 0) STOP

      bet = b(1)
      u(1) = r(1)/bet
      DO 1000 j = 2, N
        gam(j) = c(j-1)/bet
        bet    = b(j)-a(j)*gam(j)
        IF (bet .EQ. 0.) STOP
        u(j) = (r(j)-a(j)*u(j-1))/bet
 1000 CONTINUE
      DO 1100 j = n-1, 1, -1
        u(j) = u(j)-gam(j+1)*u(j+1)
 1100 CONTINUE

      RETURN
      END

      SUBROUTINE veceq(x, y, n)
      INTEGER n
      REAL x(n), y(n)
      INTEGER i
      DO 1000 i = 1, n
        y(i) = x(i)
 1000 CONTINUE
      RETURN
      END
      SUBROUTINE correl(x, y, k, r2, xbar, ybar, sig2x, sig2y)
C     Compute various statistical parameters between two 
C       vectors.

      IMPLICIT none

      INTEGER k
      REAL x(k), y(k)
      REAL r2, xbar, ybar, sig2x, sig2y
      
      REAL sumx, sumx2, sumxy
      REAL sx, sy, x2, y2, xy

      sx = sumx(x, k)
      sy = sumx(y, k)
      x2 = sumx2(x, k)
      y2 = sumx2(y, k)
      xy = sumxy(x, y, k)

      xbar = sx/k
      ybar = sy/k
      sig2x = (k*x2-sx*sx)/k/(k-1)
      sig2y = (k*y2-sy*sy)/k/(k-1)
      r2    = (k*xy - sx*sy)/ SQRT( (k*x2-sx*sx)*(k*y2-sy*sy) )

      RETURN
      END 
      FUNCTION sumx(x,n)
      IMPLICIT none
      INTEGER n
      REAL x(n)
      REAL sumx

      DOUBLE PRECISION sum
      INTEGER i
      sum = 0.
      DO 1000 i = 1, n
        sum = sum + DBLE(x(i))
 1000 CONTINUE
      sumx = SNGL(sum)
      RETURN
      END

      FUNCTION sumx2(x, n)
      IMPLICIT none
      INTEGER n
      REAL x(n)
      REAL sumx2

      DOUBLE PRECISION sum
      INTEGER i
      sum = 0.0
      DO 2000 i = 1, n
        sum = sum + DBLE(x(i)*x(i))
 2000 CONTINUE
      sumx2 = SNGL(sum)

      RETURN
      END

      FUNCTION sumxy(x, y, n)
C     Compute the sum of x(i)*y(i)
      INTEGER n
      REAL x(n), y(n), sumxy

      INTEGER i
      REAL sum

      sum = 0.0
      DO 1000 i = 1, n
        sum = sum + x(i)*y(i)
 1000 CONTINUE

      sumxy = sum
      RETURN
      END
      SUBROUTINE agree(r, rbar, x, n, ia)
C     Compute the index of agreement between two vectors
      REAL r(n), x(n)
      REAL rbar, ia

      REAL sr2, sx2, srx, spx
      INTEGER i

      sr2 = 0.0
      sx2 = 0.0
      srx = 0.0
      spx = 0.0
      rbar = 0.0

      DO 100 i = 0, n
        rbar = rbar + r(i)
  100 CONTINUE
      rbar = rbar / FLOAT (n)

      DO 1000 i = 0, n
          sr2 = sr2 + r(i)*r(i)
          sx2 = sx2 + x(i)*x(i)
          srx = srx + r(i)*x(i)
          spx = spx + (ABS(x(i)-rbar) + ABS(r(i)-rbar) )**2
 1000 CONTINUE

      IF (spx .NE. 0.0) THEN
        ia = 1.0 - (sx2 - 2.*srx + sr2) / spx
       ELSE 
        ia = 0.0
      ENDIF

      RETURN
      END
      SUBROUTINE mkrand(x, y, n, rsize, seed)   
      INTEGER n, seed
      REAL x(n), y(n), rsize

      INTEGER i

      DO 1000 i = 1, n
        y(i) = x(i) + rsize*(RAN2(seed)-0.5)*2.
 1000 CONTINUE

      RETURN
      END
      FUNCTION RAN2(IDUM)
C     Single linear congruential random number generator.
C     From Numerical Recipes.
      INTEGER IDUM
      INTEGER M, IA, IC
      PARAMETER (M = 714025)
      PARAMETER (IA = 1366)
      PARAMETER (IC = 150899)
      REAL RM
      PARAMETER (RM = 1./M)
      INTEGER J, IFF, IY

      INTEGER IR(97)
      DATA IFF /0/
      IF (IDUM .LT. 0 .OR. IFF .EQ. 0) THEN
        IFF = 1
        IDUM = MOD(IC-IDUM,M)
        DO 11 J = 1, 97
          IDUM = MOD(IA*IDUM+IC, M)
          IR(J) = IDUM
   11   CONTINUE
        IDUM = MOD(IA*IDUM+IC, M)
        IY = IDUM
      ENDIF

      J = 1 + (97*IY)/M
      IF (J .GT. 97 .OR. J .LT. 1) PAUSE
      IY = IR(J)
      RAN2 = IY*RM
      IDUM=MOD(IA*IDUM+IC,M)
      IR(J) = IDUM
      RETURN
      END
      
      SUBROUTINE blnkrm(infile)
      INTEGER i, j
      CHARACTER*122 infile, outfile

      j = 0
      DO 1000 i = 1, 122
        IF (infile(i:i) .NE. ' ') THEN
          j = j + 1
          outfile(j:j) = infile(i:i)
        ENDIF
 1000 CONTINUE
      DO 1100 i = j+1, 122
        outfile(i:i) = ' '
 1100 CONTINUE

      infile = outfile
      PRINT *,'in blnkrm, outfile = ',outfile

      RETURN
      END

      SUBROUTINE readat(x, nz, nnx, nny, base, tag)
      IMPLICIT none

      INTEGER nx, ny, lrec
      INTEGER nnx, nny, nz
      PARAMETER (nx = 304)
      PARAMETER (ny = 448)
      PARAMETER (lrec = 3040)

      CHARACTER*1 line(nx), x(nz, nx, ny)

      CHARACTER*60 base, tag
      CHARACTER*122 infile
      INTEGER i, j, iunit, day, ier
C================================================================
      iunit = 31           
      DO 1000 day = 1, nz
       IF (day .LT. 10) THEN
         WRITE (infile,9001) base, day, tag
 9001    FORMAT (A60,'0',I1,A60)
       ELSE
         WRITE (infile,9002) base, day, tag
 9002    FORMAT (A60,I2,A60)
       ENDIF
       CALL blnkrm(infile)
       OPEN (unit=iunit,file=infile,status='old',      
     +      form='binary',recl=lrec,iostat=ier)       
             if (ier.ne.0) then                   
                 print *,infile, ' not accessible!'  
             endif                                
        DO 2000 j = 1, ny
          READ (iunit) line
          DO 2100 i = 1, nx
            x(day, i, j) = line(i)
 2100     CONTINUE
 2000   CONTINUE


        CLOSE (iunit)
 1000 CONTINUE

      RETURN
      END

      SUBROUTINE writ(x, nz, nnx, nny, base, tag)
      IMPLICIT none

      INTEGER nx, ny, lrec
      INTEGER nnx, nny, nz
      PARAMETER (nx = 304)
      PARAMETER (ny = 448)
      PARAMETER (lrec = 3040)

      CHARACTER*1 line(nx), x(nz, nx, ny)
      CHARACTER*60 base, tag
      CHARACTER*122 infile
      INTEGER i, j, iunit, day, ier
C================================================================

      DO 1000 day = 1, nz
       iunit = 20+day
       IF (day .LT. 10) THEN
         WRITE (infile,9001) base, day, tag
 9001    FORMAT (A60,'0',I1,A60)
       ELSE
         WRITE (infile,9002) base, day, tag
 9002    FORMAT (A60,I2,A60)
       ENDIF
       CALL blnkrm(infile)
       OPEN (unit=iunit,file=infile,status='new',
     +      form='binary', iostat=ier)       
             if (ier.ne.0) then                   
                 print *,infile, ' not accessible!'  
             endif                                
        DO 1100 j = 1, ny
          DO 1200 i = 1, nx
            line(i) = x(day, i, j) 
 1200     CONTINUE
          WRITE (iunit) line
 1100   CONTINUE

        CLOSE (iunit, STATUS='KEEP')
 1000 CONTINUE

      RETURN
      END
