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

      DO 1000 tstep = 0, loy-1
        CALL source(ss, sd, qss, qsd, we, h, nx, ny, tstep,
     1             qsfmax, qsfref, qsm, loy, xcen, xlen, ycen, ylen,
     2             strwin, strspr, strsum, strfll, dx, dy, dt)
        CALL arsum(qsum, qss, nx, ny)
 1000 CONTINUE

      CALL integ2(qsum, nx, ny, dx, dy, value)
      PRINT *,'Total integral ',value
      CALL integ2(sd, nx, ny, dx, dy, value)
      PRINT *,'Total sd ', value

      PAUSE
      END
C***********************************************************----------!!
      SUBROUTINE source(ss, sd, qss, qsd, we, h, nx, ny, tstep,
     1             qsfmax, qsfref, qsm, loy, xcen, xlen, ycen, ylen,
     2             strwin, strspr, strsum, strfll, delx, dely, delt)
C     Subroutine to extrapolate the salinization forcing to the next
C       time step.  BG 3-25-88.
      IMPLICIT none

      INTEGER nx, ny
      REAL ss(nx, ny), sd(nx, ny), qss(nx, ny), qsd(nx, ny)
      REAL we(nx, ny), h(nx, ny)
      INTEGER tstep, loy
      REAL xcen, xlen, ycen, ylen
      INTEGER strspr, strsum, strfll, strwin
      REAL qsfmax, qsfref, qsm, delt, delx, dely

      INTEGER i, j, t

      REAL pi
      PARAMETER (pi = 3.141592654)
      REAL xref, yref, dx, dy, sigx, sigy
      REAL a, qfix
      REAL yterm, xpart, qssum

      SAVE a, qfix

      t = MOD(tstep,loy)
      xref = xcen
      yref = ycen
      sigx = xlen
      sigy = ylen

      xpart = 0.5/sigx/sigx

      IF (tstep .EQ. 0) THEN
        qfix = 0.0
        a = 0.0
        CALL qstart(ss, qss, qsd, we, h, nx, ny, a,
     1                 qsfmax, qsfref, qsm,
     2   loy, xcen, xlen, ycen, ylen,
     3   strwin, strspr, strsum, strfll, delx, dely, delt)
      ENDIF
      IF (t .EQ. 0) THEN
        DO 100 j = 1, ny
          DO 110 i = 1, nx
            qss(i,j) = we(i,j)*sd(i,j)
  110     CONTINUE
  100   CONTINUE
        CALL integ2(qss, nx, ny, delx, dely, qfix)
        qfix = qfix/delx/dely/h(1,1)/(loy*0.60)
      ENDIF

      IF ((t .GE. strwin) .AND. (t .LT. strspr)) THEN
        DO 1000 j = 1, ny
          dy = dely*FLOAT(j)
          yterm = (dy - yref)*(dy - yref)*0.5/sigy/sigy
          DO 1010 i = 1, nx
            dx = delx*FLOAT(i)
            qss(i,j) = qsfref + qfix + qsfmax*
     1        EXP( -(dx-xref)*(dx-xref)*xpart - yterm )
 1010     CONTINUE
 1000   CONTINUE
       ELSEIF (t .GE. strspr .AND. t .LT. strsum) THEN
C       spring, qss = qsd = 0.0
        CALL arset1(qss, 0.0, nx, ny)
       ELSEIF (t .GE. strsum .AND. t .LT. strfll) THEN
C       Summer
        DO 2000 j = 1, ny
          qssum = qsm - a*FLOAT(j)*dely
          DO 2010 i = 1, nx
            qss(i,j) = qssum
 2010     CONTINUE
 2000   CONTINUE
       ELSE
C       fall, do nothing
        CALL arset1(qss, 0.0, nx, ny)
      ENDIF

      DO 3000 j = 1, ny
        DO 3100 i = 1, nx
          qsd(i,j) = qss(i,j)
 3100   CONTINUE
 3000 CONTINUE

      RETURN
      END
C***********************************************************----------!!
C     Version from simp.in.f
      SUBROUTINE integ2(g, nx, ny, dx, dy, value)
C     Perform a 2-d integration
      IMPLICIT none
      INTEGER nx, ny
      REAL g(nx, ny), dx, dy, value
      DOUBLE PRECISION tempor(4000), summer

      INTEGER i, j

      DO 1000 j = 2, ny-1
        tempor(j) = 0.0
        DO 1100 i = 3, nx-2, 2
          tempor(j) = tempor(j) + 4.*g(i,j)+2.*g(i+1,j)
 1100   CONTINUE
        tempor(j) = (tempor(j) + g(2,j)+g(nx-1,j))/3.
 1000 CONTINUE

      summer = 0.
      DO 1200 j = 3, ny-2, 2
        summer = summer + 4.*tempor(j)+2.*tempor(j+1)
 1200 CONTINUE
      summer = (summer + tempor(2)+tempor(ny-1) )/3.

      value = summer*dx*dy

      RETURN
C***********************************************************----------!!
      ENTRY integ22(g, nx, ny, dx, dy, value)
C     Perform a 2-d integration of sqaured field

      DO 2000 j = 1, ny
        tempor(j) = 0.0
        DO 2100 i = 2, nx-1, 2
          tempor(j) = tempor(j) + 4.*g(i,j)*g(i,j)
     1                           +2.*g(i+1,j)*g(i+1,j)
 2100   CONTINUE
        tempor(j) = (tempor(j) + g(1,j)*g(1,j)+g(nx,j)*g(nx,j))/3.
 2000 CONTINUE

      summer = 0.
      DO 2200 j = 2, ny-1, 2
        summer = summer + 4.*tempor(j)+2.*tempor(j+1)
 2200 CONTINUE
      summer = (summer + tempor(1)+tempor(ny) )/3.

      value = summer*dx*dy
      RETURN
      END
C***********************************************************----------!!
      SUBROUTINE arset1(x, val, nx, ny)
      INTEGER nx, ny
      REAL x(nx*ny), val
      INTEGER i

      DO 1000 i = 1, nx*ny
        x(i) = val
 1000 CONTINUE

      RETURN
      END
      SUBROUTINE arsum(x, y, nx, ny)
      INTEGER nx, ny
      REAL x(nx*ny), y(nx*ny)
      INTEGER i
      DO 1000 i = 1, nx*ny
        x(i) = x(i)+y(i)
 1000 CONTINUE
      RETURN
      END
      SUBROUTINE arsum2(x, y, nx, ny)
      INTEGER nx, ny
      REAL x(nx,ny), y(nx,ny)
      INTEGER i, j
      DO 1100 j = 1, ny
      DO 1000 i = 1, nx
        x(i,j) = x(i,j) + y(i,j)
 1000 CONTINUE
 1100 CONTINUE

      RETURN
      END
C***********************************************************----------!!
      SUBROUTINE qstart(ss, qss, qsd, we, h, nx, ny, a,
     1         qsfmax, qsfref, qsm , loy, xcen, xlen, ycen, ylen,
     2 strwin, strspr, strsum, strfll, delx, dely, delt   )
C     Subroutine to extrapolate the salinization forcing to the next
C       time step.  BG 3-25-88.
      IMPLICIT none

      INTEGER nx, ny
      REAL ss(nx, ny), qss(nx, ny), qsd(nx, ny), we(nx, ny), h(nx, ny)
      INTEGER loy
      REAL xcen, xlen, ycen, ylen
      INTEGER strspr, strsum, strfll, strwin
      REAL qsfmax, qsfref, qsm, delt, delx, dely

      INTEGER i, j, t

      REAL pi
      PARAMETER (pi = 3.141592654)
      REAL xref, yref, dx, dy, sigx, sigy
      REAL a, sumq, sumw
      REAL yterm, xpart, qssum

      xref = xcen
      yref = ycen
      sigx = xlen
      sigy = ylen
      xpart = 0.5/sigx/sigx

      CALL arset1(qss, 0.0, nx, ny)

      DO 9000 t = 0, loy-1
      IF ((t .GE. strwin) .AND. (t .LT. strspr)) THEN
        DO 1000 j = 1, ny
          dy = dely*FLOAT(j)
          yterm = (dy - yref)*(dy - yref)*0.5/sigy/sigy
          DO 1010 i = 1, nx
            dx = delx*FLOAT(i)
            qss(i,j) = qss(i,j) + qsfref + qsfmax*
     1        EXP( -(dx-xref)*(dx-xref)*xpart - yterm )
 1010     CONTINUE
 1000   CONTINUE
       ELSEIF (t .GE. strsum .AND. t .LT. strfll) THEN
        DO 2000 j = 1, ny
          qssum = qsm
C                   - a*FLOAT(j)*dely
          DO 2010 i = 1, nx
            qss(i,j) = qss(i,j)+qssum
 2010     CONTINUE
 2000   CONTINUE
       ELSE
C       fall or spring, do nothing
      ENDIF
 9000 CONTINUE

      CALL integ2(qss, nx, ny, delx, dely, sumq)

      DO 3000 j = 1, ny
        DO 3100 i = 1, nx
          qss(i,j) = j*dely
 3100   CONTINUE
 3000 CONTINUE
      CALL integ2(qss, nx, ny, delx, dely, sumw)
      a = sumq/sumw/FLOAT(strfll-strsum+0)
      PRINT *,'net source ', sumq/delx/dely, a

      RETURN
      END
