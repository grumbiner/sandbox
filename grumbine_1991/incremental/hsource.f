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
      EN
