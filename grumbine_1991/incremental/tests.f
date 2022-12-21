C***********************************************************----------!!
      PROGRAM tests
C     Conduct tests of the source term functions.
C     Examine both accuracy and speed.
      IMPLICIT none

      INTEGER nx, ny
      PARAMETER (nx = 40)
      PARAMETER (ny = 40)

      REAL qsum(nx, ny), dx, dy, dt
      REAL ss(nx, ny), sd(nx, ny), qss(nx, ny), qsd(nx, ny)
      REAL we(nx, ny), h(nx, ny)
      INTEGER t1, i, j, tstep, loy
      REAL qsfmax, qsfref, qsm
      PARAMETER (qsfmax = 3.47E-5)
      PARAMETER (qsfref = 2.E-6)
      PARAMETER (qsm    = -5.E-6)
      REAL rwin, rspr, rsum, rfll, value
      PARAMETER (rwin = 0.)
      PARAMETER (rspr = 5. /12.)
      PARAMETER (rsum = 8. /12.)
      PARAMETER (rfll = 10./12.)
      INTEGER strwin, strspr, strsum, strfll
      REAL xcen, xlen, ycen, ylen
      PARAMETER (xcen = 5.4E5)
      PARAMETER (xlen = 4.0E5)
      PARAMETER (ycen = 6.0E4)
      PARAMETER (ylen = 4.0E4)

      dx = 2.E4
      dy = 2.E4
      dt = 6.E4
      loy = INT(365.2422*86400./dt)/12+1
      loy = loy*12
      dt  = 365.2422*86400./FLOAT(loy)
      PRINT *,'nsteps, dt ',loy, dt
      strwin = rwin*loy
      strspr = rspr*loy
      strsum = rsum*loy
      strfll = rfll*loy

      CALL arset1(ss, 0.0, nx, ny)
      CALL arset1(sd, -0.1, nx, ny)
      CALL arset1(qss, 0.0, nx, ny)
      CALL arset1(qsd, 0.0, nx, ny)
      CALL arset1(we,  0.0, nx, ny)
      CALL arset1(h, 500.0, nx, ny)
      CALL arset1(qsum, 0., nx, ny)

CT      t1 = LONG(362)
      DO 1000 tstep = 0, loy-1
        CALL source(ss, sd, qss, qsd, we, h, nx, ny, tstep,
     1             qsfmax, qsfref, qsm, loy, xcen, xlen, ycen, ylen,
     2             strwin, strspr, strsum, strfll, dx, dy, dt)
        CALL arsum(qsum, qss, nx, ny)
 1000 CONTINUE
CT      PRINT *,'Time ', LONG(362)-t1
      CALL integ2(qsum, nx, ny, dx, dy, value)
      PRINT *,'Total integral ',value
      CALL integ2(sd, nx, ny, dx, dy, value)
      PRINT *,'Total sd ', value

      END
