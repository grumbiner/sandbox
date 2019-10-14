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
