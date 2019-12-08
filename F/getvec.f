      SUBROUTINE getvec(a, vec, nx, ny, nday, i, j)
C     get a time varying element from a 3d array
      INTEGER nx, ny, nday, i, j
      REAL a(nx, ny, nday), vec(nday)

      INTEGER k

      DO 1000 k = 1, nday
        vec(k) = a(i,j,k)
 1000 CONTINUE

      RETURN
      END
