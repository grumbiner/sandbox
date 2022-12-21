      PROGRAM alpha
      REAL x1, x2, x3, x4, x5
      INTEGER i
      REAL y1, y2, y3

      OPEN (10, FILE="iers.clean", FORM="FORMATTED")
      OPEN (11, FILE="range.adj", FORM="FORMATTED")
    
      DO i = 1, 16377
        READ (10,*), x1, x2, x3, x4, x5
        READ (11,*) y1, y2, y3
        WRITE (*,9001), x1, x2, x3, x4, x5, y2, y3
      ENDDO

 9001 FORMAT (7E22.13)

      STOP
      END
  
