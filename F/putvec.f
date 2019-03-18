      SUBROUTINE putvec(a, vec, nx, ny, nday, i, j)
C     put a time varying element into a 3d array
      INTEGER nx, ny, nday, i, j
      REAL a(nx, ny, nday), vec(nday)

      INTEGER k

      DO 1000 k = 1, nday
        a(i,j,k) = vec(k)
 1000 CONTINUE

      RETURN
      END
