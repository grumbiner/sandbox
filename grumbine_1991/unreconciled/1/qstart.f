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
