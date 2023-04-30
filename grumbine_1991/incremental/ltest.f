      PROGRAM ltest
C     Test the laplacean sbr
      INTEGER nx, ny
      PARAMETER (nx = 42)
      PARAMETER (ny = 42)
      REAL diffu(nx, ny), g(nx, ny), dx, dy, ah
      PARAMETER (dx = 1.)
      PARAMETER (dy = 1.)
      PARAMETER (ah = 1.)

      INTEGER i, j, power, t1
      REAL eps

      PRINT *,'What is the size of epsilon?'
      READ (*,9001) eps
 9001 FORMAT (E13.6)
      PRINT *,'What power would you like?'
      READ (*,9002) power
 9002 FORMAT (I2)

      DO 1000 j = 1, ny
        DO 1100 i  = 1, nx
          g(i,j) = FLOAT(i**power)+FLOAT(j**power)
 1100   CONTINUE
 1000 CONTINUE

CT      t1 = LONG(362)
      DO 1 i = 1, 250
        CALL laplac(diffu, g, nx, ny, dx, dy, ah)
   1  CONTINUE
CT      PRINT *,'Time for 250 iters of 40*40 laplacean ',LONG(362)-t1
      DO 2000 j = 2, ny-1
        DO 2100 i = 2, nx-1
          IF (ABS(diffu(i,j)/power/(power-1)/
     1      (FLOAT(i**(power-2))+FLOAT(j**(power-2)))-1.)
     1       .GT. eps) THEN
        PRINT *,i,j,diffu(i,j), 1. - diffu(i,j)/
     1 power/(power-1)/(FLOAT(i**(power-2))+FLOAT(j**(power-2)))
          ENDIF
 2100   CONTINUE
 2000 CONTINUE

      END
