C***********************************************************----------!!
      SUBROUTINE qsext(qss, qsd, h, nx, ny, tstep, loy,
     1                 xcen, xlen, ycen, ylen,
     2                 qsfmax, qsfref, qsm, delx, dely,
     3                 strspr, strsum, strfll, strwin            )
C     Subroutine to extrapolate the salinization forcing to the next
C       time step.  BG 3-25-88.

      INTEGER nx, ny, tstep
      REAL qss(nx, ny), qsd(nx, ny), h(nx, ny)
      REAL delx, dely

      INTEGER i, j, t

      INTEGER xcen, ycen, xlen, ylen
      REAL qsfmax, qsfref
      REAL qsm
      INTEGER strspr, strsum, strfll, strwin, loy

      REAL pi
      PARAMETER (pi = 3.141592654)
      REAL xref, yref, dx, dy, sigx, sigy
      REAL a, sumq
      SAVE a

      t = MOD(tstep,loy)
      xref = FLOAT(xcen)
      yref = FLOAT(ycen)
      sigx = FLOAT(xlen)
      sigy = FLOAT(ylen)

      IF (t .EQ. 1) THEN
C       compute the required slope for salt conservation.
        sumq = 0.0
        DO 100 j = 2, ny-1
          dy = dely*FLOAT(j)
          DO 101 i = 2, nx-1
            dx = delx*FLOAT(i)
            sumq = sumq + qsfref + qsfmax*
     1      EXP((-1.)*(( dx-xref )**2/2./sigx**2
     2                +( dy-yref )**2/2./sigy**2 ))
 101      CONTINUE
 100    CONTINUE
        sumq = sumq*FLOAT(strspr-strwin)/FLOAT(strfll-strsum)/
     1         FLOAT(nx*ny-2*nx-2*ny+4)
        sumq = sumq + qsm
        a    = sumq*2./FLOAT(ny+1)/dely
        PRINT *,'linear melting parameter = ',a
      ENDIF

      IF ((t .GE. strwin) .AND. (t .LT. strspr)) THEN
        DO 1000 j = 1, ny
          DO 1010 i = 1, nx
            dx = delx*FLOAT(i)
            dy = dely*FLOAT(j)
            qss(i,j) = qsfref + qsfmax*
     1      EXP((-1.)*(( dx-xref )**2/2./sigx**2
     2                +( dy-yref )**2/2./sigy**2 ))
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
            qss(i,j) = qsm - a*FLOAT(j)*dely
            qsd(i,j) = qss(i,j)
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
