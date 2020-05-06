      PROGRAM a

      REAL rlon(0:121, 0:121, 0:5), rlat(0:121, 0:121, 0:5)
      INTEGER mask(0:121, 0:121, 0:5)
      INTEGER i, j, k

      OPEN (10, FILE="LONLATMASK", FORM="UNFORMATTED",STATUS="OLD")
      READ (10) rlon, rlat, mask

      DO k = 0, 5
      DO j = 0, 121
      DO i = 0, 121
        WRITE (*,*) rlon(i,j,k), rlat(i,j,k), mask(i,j,k)
      ENDDO
      ENDDO
      ENDDO

      STOP
      END
