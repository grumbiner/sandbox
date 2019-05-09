C***********************************************************----------!!
      SUBROUTINE qsext(ss, qss, qsd, we, h, nx, ny, tstep, loy,
     1                 xcen, xlen, ycen, ylen,
     2                 qsfmax, qsfref, qsm, delx, dely,
     3                 strspr, strsum, strfll, strwin, delt         )
C     Subroutine to extrapolate the salinization forcing to the next
C       time step.  BG 3-25-88.
      IMPLICIT none

      INTEGER nx, ny, tstep
      REAL qss(nx, ny), qsd(nx, ny)
      REAL we(nx, ny), h(nx, ny), ss(nx, ny)
      REAL delx, dely

      INTEGER i, j, t

      INTEGER xcen, ycen, xlen, ylen
      REAL qsfmax, qsfref
      REAL qsm, delt
      INTEGER strspr, strsum, strfll, strwin, loy

      REAL pi
      PARAMETER (pi = 3.141592654)
      REAL xref, yref, dx, dy, sigx, sigy
      REAL a, sumq, sumw
      DOUBLE PRECISION qfix
      REAL yterm, xpart, qssum
      SAVE a, qfix

      t = MOD(tstep,loy)
      xref = FLOAT(xcen)
      yref = FLOAT(ycen)
      sigx = FLOAT(xlen)
      sigy = FLOAT(ylen)

      xpart = 0.5/sigx/sigx

      IF (t .EQ. 1) THEN
C       compute the required slope for salt conservation.
        sumq = 0.0
        sumw = 0.0
        DO 100 j = 1, ny
          dy = dely*FLOAT(j)
          yterm = (dy-yref)*(dy-yref)*0.5/sigy/sigy
          DO 101 i = 1, nx
            dx = delx*FLOAT(i)
            sumq = sumq + qsfref + qsfmax*
     1        DEXP( DBLE(-( dx-xref )*( dx-xref )*xpart
     2             - yterm                      ) )
            sumw = sumw + we(i,j)
 101      CONTINUE
 100    CONTINUE

        sumw = sumw*(-0.1*2.*FLOAT(loy))/FLOAT(strfll-strsum+1)
        sumq = sumq*FLOAT(strspr-strwin+1)/FLOAT(strfll-strsum+1)
        sumq = sumq + sumw + qsm*FLOAT(nx*ny)

        a    = sumq*2./FLOAT(ny*(ny+1))/FLOAT(nx)/dely
        PRINT *,'linear melting parameter = ',a

C       Compute the fix required for salt conservation interannually.
        CALL summer(ss, nx, ny, qfix)
        qfix = -qfix*h(nx/2,ny/2)/DBLE(strspr-strwin+1)
     1             /DBLE((nx-2)*(ny-2))/delt
        PRINT *,'qfix correction',qfix
CD        WRITE (1, 9001) qfix
      ENDIF
 9001 FORMAT ('qfix correction',E13.6)

      IF ((t .GE. strwin) .AND. (t .LT. strspr)) THEN
        DO 1000 j = 1, ny
          dy = dely*FLOAT(j)
          yterm = (dy - yref)*(dy - yref)*0.5/sigy/sigy
          DO 1010 i = 1, nx
            dx = delx*FLOAT(i)
            qss(i,j) = qsfref + SNGL(qfix) + qsfmax*
     1        DEXP( DBLE(-(dx-xref)*(dx-xref)*xpart - yterm ) )
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
          qssum = qsm - a*FLOAT(j)*dely
          DO 2010 i = 1, nx
            qss(i,j) = qssum
            qsd(i,j) = qssum
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
