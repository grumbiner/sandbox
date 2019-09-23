
      SUBROUTINE scorer(imag1, imag2, nx, ny, level,
     1                  a11, a12, a21, a22, s1, s2, s3, s4)
      INTEGER nx, ny
      INTEGER*1 imag1(ny, nx), imag2(ny, nx)
      INTEGER a11, a12, a21, a22
      INTEGER level , i, j
      REAL lam1, lam2, s1, s2, s3, s4

      a11 = 0
      a12 = 0
      a21 = 0
      a22 = 0
      DO 1010 j = 1, nx
      DO 1000 i = 1, ny
        IF (imag2(i,j) .GT. 5 .AND. imag2(i,j) .LE. 100
     1 .AND. imag1(i,j) .GT. 5 .AND. imag2(i,j) .LE. 100 ) THEN
          IF(imag1(i,j) .GT. level) THEN
            IF (imag2(i,j) .GT. level) THEN
              a11 = a11+1
             ELSE
              a12 = a12+1
            ENDIF
           ELSE
            IF (imag2(i,j) .GT. level) THEN
              a21 = a21+1
             ELSE
              a22 = a22+1
            ENDIF
          ENDIF
         ELSE
C         Non-ice point, even potentially.  Skip.
        ENDIF
 1000 CONTINUE
 1010 CONTINUE
C     Now Print out scoring info
      lam1 =
     1  FLOAT(a11+a22) + SQRT((a11+a22)**2-4.*(a11*a22-a12*a22))
      lam2 =
     1  FLOAT(a11+a22) - SQRT((a11+a22)**2-4.*(a11*a22-a12*a22))

      s1 = FLOAT(a11)/FLOAT(a11+a21+a12)
      s2 = FLOAT(a11*a22-a21*a12)/FLOAT(a11*a22+1)
      s3 = lam2/lam1
      s4 = (a11+a22-a12-a21)/(a11+a12+a21+a22)

      RETURN
      END
