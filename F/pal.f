      PROGRAM pal
C     Create a palette for use with imdisp
C     Be very simple-minded and cycle over the first 216 
C       units as nested rgb, and the last 40 colors as 
C       gray-scale.
C     Robert Grumbine 13 Feb 1996

      IMPLICIT none

      INTEGER ncol, ngray, colstp
      PARAMETER (ncol = 6)
      PARAMETER (colstp = 51)
      PARAMETER (ngray = 40)
      INTEGER i, j, k, line, gray

      OPEN (10, FILE='palett.dat', FORM='FORMATTED', STATUS='NEW')
      line = -1
      DO 1000 k = 0, ncol-1
        DO 1010 j = 0, ncol-1
          DO 1020 i = 0, ncol-1
          line = line+1
          WRITE (*,9001) line, i*colstp, j*colstp, k*colstp
          WRITE (10,9001) line, i*colstp, j*colstp, k*colstp
 9001     FORMAT (' ',I4,3(', ',I4))
 1020     CONTINUE
 1010   CONTINUE
 1000 CONTINUE

      line = line+1
      DO 1100 i = line, 255
        gray = ((i-line)*255)/ngray
        WRITE (*,9001) i, gray, gray, gray
        WRITE (10,9001) i, gray, gray, gray
 1100 CONTINUE

      RETURN
      END

