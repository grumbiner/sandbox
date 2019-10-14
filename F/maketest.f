      PROGRAM writ
      INTEGER nx, ny
      PARAMETER (nx = 360)
      PARAMETER (ny = 180)
      REAL sst(nx, ny), salt(nx, ny)
      INTEGER i,j
      
      DO 1000 j = 1, ny
        DO 1100 i = 1, nx
          sst(i,j) = 279.0
          salt(i,j) = 34.7
 1100   CONTINUE
 1000 CONTINUE

      WRITE (10) sst
      WRITE (10) salt

      STOP
      END
