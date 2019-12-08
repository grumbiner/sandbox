C***********************************************************----------!!
      SUBROUTINE qstart(ss, qss, qsd, we, h, nx, ny, a, loy, 
     1              xcen, xlen, ycen, ylen, 
     2              qsfmax, qsfref, qsm, delx, dely,
     3              strspr, strsum, strfll, strwin, delt        )
C     Subroutine to extrapolate the salinization forcing to the next
C       time step.  Robert Grumbine 3-25-1988.
      IMPLICIT none

      INTEGER nx, ny
      REAL ss(nx, ny), qss(nx, ny), qsd(nx, ny), we(nx, ny), h(nx, ny)
      INTEGER loy
      INTEGER xcen, xlen, ycen, ylen
      INTEGER strspr, strsum, strfll, strwin
      REAL qsfmax, qsfref, qsm, delt, delx, dely

      INTEGER i, j, t

      REAL pi
      PARAMETER (pi = 3.141592654)
      REAL xref, yref, dx, dy, sigx, sigy
      REAL a, sumq, sumw, qfix
      REAL yterm, xpart, qssum

      xref = FLOAT(xcen)
      yref = FLOAT(ycen)
      sigx = FLOAT(xlen)
      sigy = FLOAT(ylen)

      xpart = 0.5/sigx/sigx

      DO 400 j = 1, ny
        DO 410 i = 1, nx
          qss(i,j) = 0.0
  410   CONTINUE
 400  CONTINUE

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
       ELSEIF (t .GE. strspr .AND. t .LT. strsum) THEN
C       spring, qss = qsd = 0.0
       ELSEIF (t .GE. strsum .AND. t .LT. strfll) THEN
        DO 2000 j = 1, ny
          qssum = qsm - a*FLOAT(j)*dely
          DO 2010 i = 1, nx
            qss(i,j) = qss(i,j)+qsm
 2010     CONTINUE
 2000   CONTINUE
       ELSE
C       fall, do nothing
      ENDIF
 9000 CONTINUE

      CALL summer(qss, nx, ny, sumq)
      CALL summer(we, nx, ny, sumw)
      sumw = sumw*(-0.1*FLOAT(loy))/h(nx/2, ny/2)
      sumq = (sumq + sumw)

      DO 3000 j = 1, ny
        DO 3100 i = 1, nx
          qss(i,j) = j*dely
 3100   CONTINUE
 3000 CONTINUE
      CALL summer(qss, nx, ny, sumw)
      a = sumq/sumw/FLOAT(strfll-strsum-0)
      PRINT *,'net source ', sumq/delx/dely, a

      RETURN
      END
