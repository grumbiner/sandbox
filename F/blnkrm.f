      SUBROUTINE blnkrm(infile)
C     Remove embedded blanks in the input line, and then
C       pad out to end of line with blanks.
C     Bob Grumbine 1/22/94.

      IMPLICIT none

      INTEGER i, j
      CHARACTER*122 infile, outfile

      j = 0
      DO 1000 i = 1, 122
        IF (infile(i:i) .NE. ' ') THEN
          j = j + 1
          outfile(j:j) = infile(i:i)
        ENDIF
 1000 CONTINUE
      DO 1100 i = j+1, 122
        outfile(i:i) = ' '
 1100 CONTINUE

      infile = outfile

      RETURN
      END
