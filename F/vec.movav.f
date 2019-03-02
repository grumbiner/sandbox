      SUBROUTINE vecav(z, m, zz, n)
C     Subroutine to average a vector for some convenient period.

      INTEGER m, n
      REAL z(m), zz(n)
 
      INTEGER i, j, k, ts, av
      REAL ftmp

      PRINT *,'How many steps averaging do you want?'
      READ (*,9001) av
      ts = m/av
      DO 1000 k = 1, ts 
        ftmp = 0.0
        DO 1010 i = 1, av
          ftmp = ftmp + z(i+(k-1)*av)
 1010   CONTINUE
        zz(k) = ftmp/FLOAT(av)
 1000 CONTINUE

 9001 FORMAT (I5)

      RETURN
      END
