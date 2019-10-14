
      SUBROUTINE enter(atmp, atot, nx, ny, nk, k)
      INTEGER nx, ny, nk, k
      REAl atmp(nx, ny), atot(nx, ny, nk)
      INTEGER i, j 
      DO 1000 j = 1, ny
        DO 1001 i = 1, nx
          atot(i,j,k) = atmp(i,j)
 1001   CONTINUE
 1000 CONTINUE
      RETURN
      END
