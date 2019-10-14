      SUBROUTINE mksmpl(x, n, tzero, tline, thi, type)
      INTEGER n, type
      REAL x(n)
      INTEGER i, j
      INTEGER tzero, tline, thi, lthi
      REAL pi

      lthi = thi - 2*tzero - 2*tline
      IF (type .EQ. 1) THEN
        DO 1000 i = 1, tzero
          x(i) = 0.0
 1000   CONTINUE
        j = tzero
        DO 1100 i = j+1, j+tline
          x(i) = (i-j)*0.99/FLOAT(tline)
 1100   CONTINUE
        j = j+tline
        DO 1200 i = j+1, j+lthi
          x(i) = 0.99
 1200   CONTINUE
        j = j+lthi
        DO 1300 i = j+1, j+tline
          x(i) = 0.99 - (i-j)*0.99/FLOAT(tline)
 1300   CONTINUE
        j = j+tline
        DO 1400 i = j+1, j+tzero
          x(i) = 0.0
 1400   CONTINUE

       ELSE IF (type .EQ. 2) THEN
        pi = ATAN(1.)*4.
        DO 2000 i = 1, thi       
          x(i) = 0.95 + 0.05 * sin(2.*pi*(i-1)/FLOAT(tline) ) 
 2000   CONTINUE
       ELSE
        PRINT *,'Type of sample curve out of range '
        STOP
      ENDIF
      
      RETURN
      END
