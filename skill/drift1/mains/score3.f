      PROGRAM idiff
C     Compute a forescast score given two 8 bit graphic images.
C     For use with the imaging display programs for sea ice.

      INTEGER nx, ny, north, south
CD      PARAMETER (nx(1) = 224)
CD      PARAMETER (ny(1) = 152)
CD      PARAMETER (nx(2) = 166)
CD      PARAMETER (ny(2) = 158)
      PARAMETER (nx    = 224)
      PARAMETER (ny    = 152)
      PARAMETER (north = 1  )
      PARAMETER (south = 2  )

      INTEGER*1 imag1(ny,nx), imag2(ny, nx), imag3(ny, nx)

      CHARACTER*60 fname
      INTEGER i, j, level
      INTEGER a11, a12, a21, a22
      REAL s1, s2, s3, s4

 9001 FORMAT (A60)

      PRINT *,'What is the name of the first file?'
      READ (*,9001) fname
      OPEN (10,FILE=fname, FORM='BINARY', STATUS='OLD')
      PRINT *,'What is the name of the second file?'
      READ (*,9001) fname
      OPEN (11,FILE=fname, FORM='BINARY', STATUS='OLD')
      PRINT *,'What is the name of the climate file?'
      READ (*,9001) fname
      OPEN (13,FILE=fname, FORM='BINARY', STATUS='OLD')

      PRINT *,'What do you want to call the scores file?'
      READ (*,9001) fname
      OPEN (12, FILE=fname, FORM='FORMATTED', STATUS='NEW')

 9002 FORMAT (A1)
CD      PRINT *,'Reading imag1'
      READ (10) imag1
CD      PRINT *,'Reading imag2'
      READ (11) imag2
      READ (13) imag3

      DO 1000 level = 15, 95, 5
        CALL scorer(imag1, imag2, nx, ny, level,
     1                   a11, a12, a21, a22, s1, s2, s3, s4)
        WRITE (*,9010) level, a11, a12, a21, a22, 
     1  s1, s2, s3, s4
        WRITE (12,9010) level, a11, a12, a21, a22, 
     1  s1, s2, s3, s4
        CALL scorer(imag1, imag3, nx, ny, level, 
     1                   a11, a12, a21, a22, s1, s2, s3, s4)
        WRITE (*,9010) level, a11, a12, a21, a22, 
     1  s1, s2, s3, s4
        WRITE (12,9010) level, a11, a12, a21, a22, 
     1    s1, s2, s3, s4
 1000 CONTINUE

 9010 FORMAT (I3,4I5,4F8.4)

      END

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
