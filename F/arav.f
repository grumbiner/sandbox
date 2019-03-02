      SUBROUTINE arav(z, m, ts, n, zz, l, p)
C     Subroutine to average an array for some convenient interval.

      INTEGER l, m, n, p, ts
      REAL z(m, n), zz(l, p)
 
      INTEGER i, j, k, up, av
      REAL ftmp

      PRINT *,'How many steps averaging do you want?'
      READ (*,9001) av
      up = ts/av
      DO 1000 k = 1, up 
        DO 1010 j = 1, n
          ftmp = 0.0
          DO 1020 i = 1, av
            ftmp = ftmp + z(i+(k-1)*av,j)
 1020     CONTINUE
          zz(k,j) = ftmp/FLOAT(av)
 1010   CONTINUE
 1000 CONTINUE

 9001 FORMAT (I5)

      RETURN
      END
