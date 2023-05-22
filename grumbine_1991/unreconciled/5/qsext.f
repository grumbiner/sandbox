C***********************************************************----------!!
      SUBROUTINE qsext(ss, qss, qsd, we, h, nnx, nny, tstep, loy,
     1              xcen, xlen, ycen, ylen,
     2              qsfmax, qsfref, qsm, delx, dely,
     3              strspr, strsum, strfll, strwin, delt        )
C     Subroutine to extrapolate the salinization forcing to the next
C       time step.  BG 3-25-88.
      IMPLICIT none
      INCLUDE "grid.inc"

      REAL ss(nx, ny), qss(nx, ny), qsd(nx, ny), we(nx, ny), h(nx, ny)
      INTEGER tstep, loy
      INTEGER xcen, xlen, ycen, ylen
      INTEGER strspr, strsum, strfll, strwin
      REAL qsfmax, qsfref, qsm, delt, delx, dely

      INTEGER nnx, nny

      INTEGER i, j, t

      REAL pi
      PARAMETER (pi = 3.141592654)
      REAL xref, yref, dx, dy, sigx, sigy
      REAL a, sumq, sumw, qfix
      REAL yterm, xpart, qssum

      SAVE a, qfix

      t = MOD(tstep,loy)
      xref = FLOAT(xcen)
      yref = FLOAT(ycen)
      sigx = FLOAT(xlen)
      sigy = FLOAT(ylen)

      xpart = 0.5/sigx/sigx

      IF (tstep .EQ. 0) THEN
        qfix = 0.0
        a = 0.0
        CALL qstart(ss, qss, qsd, we, h, nnx, nny, a, loy,
     1              xcen, xlen, ycen, ylen,
     2              qsfmax, qsfref, qsm, delx, dely,
     3              strspr, strsum, strfll, strwin, delt        )
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
        DO 1100 j = 1, ny
          DO 1110 i = 1, nx
            qss(i,j) = 0.0
 1110     CONTINUE
 1100   CONTINUE
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
        DO 2100 j = 1, ny
          DO 2110 i = 1, nx
            qss(i,j) = 0.0
 2110     CONTINUE
 2100   CONTINUE
      ENDIF

      qsd = qss

      RETURN
      END
