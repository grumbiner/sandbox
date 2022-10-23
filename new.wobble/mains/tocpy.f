      PROGRAM cpy
      IMPLICIT none
      REAL yr
      PARAMETER (yr = 365.256)
      REAL t1, a1, t2, a2, t3, a3
      INTEGER i

      DO i = 1, 10001
        READ (*,*) t1, a1, t2, a2, t3, a3
        t1 = t1 * yr
        WRITE (*,*) t1, a1, a2, a3
      ENDDO

      END 
