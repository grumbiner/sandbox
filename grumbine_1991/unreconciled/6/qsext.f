      SUBROUTINE qsext(qss, qsd, h, nx, ny, tstep, loy,
     1                 xcen, xlen, ycen, ylen,
     2                 qsfmax, qsfref, qsm, delx, dely,
     2                 strspr, strsum, strfll, strwin               )
C     Subroutine to extrapolate the salinization forcing to the next
C       time step.  BG 3-25-88.

      INTEGER nx, ny, tstep
      REAL qss(nx, ny), qsd(nx, ny), h(nx, ny)
      REAL delx, dely

      INTEGER i, j, t

      INTEGER xcen, ycen, xlen, ylen
C     find the center and size of the enhanced freezing region,
C       Assuming a rectangle of size xlen by xlen centered on xcen, ycen
C       Then over a length 1/2 xlen to the left and right have a linear
C       and over a length of ylen taper linearly to the background rates
      REAL qsfmax, qsfref
C     The box and background freezing rates.
      REAL qsm
C     The melting rate (uniform over domain)
      INTEGER strspr, strsum, strfll, strwin, loy
C     Start of spring, summer, fall, winter, and length of the year
C       For now, discontinuous behavior - winter has freezing, fall
C       spring have nore melt or freeze, and summer has melting

      REAL pi
      PARAMETER (pi = 3.141592654)
      REAL xref, yref, dx, dy, sigx, sigy
      REAL xv, yv

      t = MOD(tstep,loy)
      xref = FLOAT(xcen)
      yref = FLOAT(ycen)
      sigx = FLOAT(xlen)
      sigy = FLOAT(ylen)

      IF ((t .GE. strwin) .AND. (t .LT. strspr)) THEN
C     Note that this assumes that time is told within a year.
C     Wintertime
CD      PRINT *,'winter'
        DO 1000 j = 1, ny
          dy = dely*FLOAT(j)
          DO 1010 i = 1, nx
            dx = delx*FLOAT(i)
            IF (dx .LT. xref-sigx ) THEN
              xv = 0.0
             ELSE IF (dx .GT. xref-sigx
     1          .AND. dx .LT. xref-sigx/2. ) THEN
              xv = (dx - (xref-sigx))*2./sigx
             ELSE IF (dx .GT. xref+sigx/2.
     1          .AND. dx .LT. xref+sigx    ) THEN
              xv = 1. - (dx -xref-sigx/2. )*2./sigx
             ELSE IF (dx .GE. xref-sigx/2.
     1          .AND. dx .LE. xref+sigx/2.) THEN
              xv = 1.0
             ELSE
              xv = 0.0
            ENDIF
            IF (dy .LE. yref+sigy) THEN
              yv = 1.0
             ELSE IF (dy .GT. yref+sigy
     1          .AND. dy .LE. yref+2.*sigy) THEN
              yv = 1. - (dy - yref - sigy)/sigy
             ELSE
              yv = 0.
            ENDIF
            qss(i,j) = qsfref + xv*yv*qsfmax
            qsd(i,j) = qss(i,j)
 1010     CONTINUE
 1000   CONTINUE
       ELSEIF (t .GE. strspr .AND. t .LT. strsum) THEN
C       spring, qss = qsd = 0.0
        DO 1100 j = 1, ny
          DO 1110 i = 1, nx
            qss(i,j) = 0.0
            qsd(i,j) = 0.0
 1110     CONTINUE
 1100   CONTINUE
       ELSEIF (t .GE. strsum .AND. t .LT. strfll) THEN
        DO 2000 j = 1, ny
          DO 2010 i = 1, nx
            qss(i,j) = qsm
            qsd(i,j) = qsm
 2010     CONTINUE
 2000   CONTINUE
       ELSE
C       fall, do nothing
        DO 2100 j = 1, ny
          DO 2110 i = 1, nx
            qss(i,j) = 0.0
            qsd(i,j) = 0.0
 2110     CONTINUE
 2100   CONTINUE
      ENDIF

      RETURN
      END
