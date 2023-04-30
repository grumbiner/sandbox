      SUBROUTINE qsext(qss, qsd, h, nx, ny, tstep, loy,
     1                 xcen, xlen, ycen, ylen,
     2                 qsfmax, qsfref, qsm, delx, dely,
     3                 strspr, strsum, strfll, strwin               )
C     Subroutine to extrapolate the salinization forcing to the next
C       time step.  BG 3-25-88.
C     Version to use the forcing function developed by Killworth in
C       1974 paper.  3-16-89.
      INTEGER nx, ny, tstep
      REAL qss(nx, ny), qsd(nx, ny), h(nx, ny)
      REAL delx, dely

      INTEGER xcen, ycen, xlen, ylen
C     xcen, ycen, and xlen are all dummy variables now.
C     ylen gives the upper limit on the y extension of the forcing
      REAL qsfmax, qsfref, qsm
C     qsfmax is the forcing, when present.  qsm must == 0.
      INTEGER strspr, strsum, strfll, strwin, loy
C     Start of spring, summer, fall, winter, and length of the year
C     strspr and strfll are dummies, go straight from freezing to meltin

      INTEGER i, j, t
      REAL dy

CD    PRINT *,'entered kqext'
      t = MOD(tstep,loy)
CD    PRINT *,'making test of season, t, tstep, loy=', t, tstep, loy
CD    PRINT *,'strspr', strspr
CD    PRINT *,'strwin', strwin
      IF ((t .GE. strwin) .AND. (t .LT. strspr)) THEN
C     Note that this assumes that time is told within a year.
C     Wintertime
CD      PRINT *,'winter'
        DO 1000 j = 1, ylen
          DO 1010 i = 1, nx
            qss(i,j) = qsfmax * (1. - FLOAT(j)/(FLOAT(ylen)))**2
            qsd(i,j) = qss(i,j)
 1010     CONTINUE
 1000   CONTINUE
        DO 1001 j = ylen+1, ny
          DO 1011 i = 1, nx
            qss(i,j) = 0.0
            qsd(i,j) = 0.0
 1011     CONTINUE
 1001   CONTINUE
       ELSEIF (t .GE. strspr .AND. t .LT. strsum) THEN
C       spring, qss = qsd = 0.0
CD      PRINT *,'spring'
        DO 1100 j = 1, ny
          DO 1110 i = 1, nx
            qss(i,j) = 0.0
            qsd(i,j) = 0.0
 1110     CONTINUE
 1100   CONTINUE
       ELSEIF (t .GE. strsum .AND. t .LT. strfll) THEN
CD      PRINT *,'summer'
        DO 2000 j = 1, ny
          DO 2010 i = 1, nx
            qss(i,j) = qsm
            qsd(i,j) = qsm
 2010     CONTINUE
 2000   CONTINUE
       ELSE
CD      PRINT *,'fall'
C       fall, do nothing
        DO 2100 j = 1, ny
          DO 2110 i = 1, nx
            qss(i,j) = 0.0
            qsd(i,j) = 0.0
 2110     CONTINUE
 2100   CONTINUE
      ENDIF

CD    PRINT *,'leaving kqext.f'
      RETURN
      END
