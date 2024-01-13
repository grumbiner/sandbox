      SUBROUTINE qcext(qcs, qcd, cs, cd, qss, h, nx, ny, tstep, loy,
     1  xmin, xmax, ymin, ymax, lambda, c0, qfmax, qfref, qm,
     2  strspr, strsum, strfll, strwin, dcadt, delt   )
C     High forcing specification changed to a rectangle 12-19-88, BG.
C     Forcing for chemicals, taken from qsext.f of 1-24-89.

      INTEGER nx, ny, tstep
      REAL qcs(nx, ny), qcd(nx, ny), qss(nx, ny)
      REAL cs(nx, ny), cd(nx, ny), h(nx, ny)
      INTEGER i, j, t

      INTEGER xmin, xmax, ymin, ymax
      REAL qfmax, qfref, qm, lambda, c0
      REAL dcadt, delt
      INTEGER strspr, strsum, strfll, strwin, loy
C     Start of spring, summer, fall, winter, and length of the year

      REAL pi, time
      PARAMETER (pi = 3.141592654)

      t = MOD(tstep,loy)
      IF ((t .GE. strwin) .AND. (t .LT. strspr)) THEN
C     Wintertime
CD      PRINT *,'winter'
        DO 1000 j = 1, ny
          DO 1010 i = 1, nx
            qcs(i,j) = lambda*(c0*exp(dcadt*delt*FLOAT(tstep))
     1                         -cs(i,j)-cd(i,j) )/h(i,j)
            qcs(i,j) = qcs(i,j)*qss(i,j)/(qfmax+qfref)
            qcd(i,j) = qcs(i,j)
 1010     CONTINUE
 1000   CONTINUE
       ELSE
        DO 1100 j = 1, ny
          DO 1110 i = 1, nx
            qcs(i,j) = lambda*(c0*exp(dcadt*delt*FLOAT(tstep))
     1                         -cs(i,j)-cd(i,j) )/h(i,j)
            qcd(i,j) = qcs(i,j)
 1110     CONTINUE
 1100   CONTINUE
      ENDIF

      RETURN
      END
