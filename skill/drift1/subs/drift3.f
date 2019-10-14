      SUBROUTINE drift3(odist, odir, dist1, dir1, dist2, dir2, npts,
     1                  obar, errbar, errsd, err1bar, err2bar, t)
C     routine to compute the error of two different forecasts relative
C       to observed drift distances, then score (via t statistic) the
C       difference in forecast errors.
C     Robert Grumbine 24 February 1998

      IMPLICIT none

      INTEGER nmax, npts
      PARAMETER (nmax = 20000)
      REAL odist(npts), odir(npts)
      REAL dist1(npts), dir1(npts)
      REAL dist2(npts), dir2(npts)
      REAL obar, delbar, delsd, t

      INTEGER i
      REAL x0(nmax), y0(nmax)
      REAL x1(nmax), y1(nmax)
      REAL x2(nmax), y2(nmax)
      REAL err1(nmax), err2(nmax)
      REAL errvar, errbar, errsd
      REAL err1bar, err2bar

      IF (npts .GT. nmax) THEN
        PRINT *,'npts greater than nmax allowed in drift3',nmax, npts
        RETURN
      ENDIF

      CALL vectorize(odist, odir, x0, y0, npts)
      CALL vectorize(dist1, dir1, x1, y1, npts)
      CALL vectorize(dist2, dir2, x2, y2, npts)

      DO 1000 i = 1, npts
        err1(i) = SQRT( (x1(i) - x0(i))**2 + (y1(i) - y0(i))**2 )
        err2(i) = SQRT( (x2(i) - x0(i))**2 + (y2(i) - y0(i))**2 )
 1000 CONTINUE

      obar    = 0.
      errbar = 0.
      errvar = 0.
      err1bar = 0.
      err2bar = 0.
      DO 1100 i = 1, npts
        err1bar = err1bar + err1(i)
        err2bar = err2bar + err2(i)
        errbar = errbar + (err2(i) - err1(i))
        errvar = errvar + (err2(i) - err1(i))**2
        obar    = obar + odist(i)
 1100 CONTINUE
      obar    = obar    / npts
      err1bar = err1bar / npts
      err2bar = err2bar / npts
      errbar = errbar / npts
      errvar = errvar / npts
      errsd = SQRT(errvar - errbar**2)
      t      = errbar / (errsd/SQRT(FLOAT(npts)) )
 
      RETURN
      END
