      SUBROUTINE acopin(ain, atmp, nx, ny, nday, k)
C     Copy an array in to the appropriate part of a 3d array
      INTEGER nx, ny, nday, k
      REAL atmp(nx, ny), ain(nx, ny, nday)
      INTEGER i, j

      DO 1000 j = 1, ny
        DO 1100 i = 1, nx
          ain(i, j, k) = atmp(i,j)
 1100   CONTINUE
 1000 CONTINUE

      RETURN
      END

      SUBROUTINE acopout(ain, atmp, nx, ny, nday, k)
C     Copy an array out from the appropriate part of a 3d array
      INTEGER nx, ny, nday, k
      REAL atmp(nx, ny), ain(nx, ny, nday)
      INTEGER i, j

      DO 1000 j = 1, ny
        DO 1100 i = 1, nx
          atmp(i,j) = ain(i,j,k)
 1100   CONTINUE
 1000 CONTINUE

      RETURN
      END
