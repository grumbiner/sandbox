      SUBROUTINE break(outch, brakpt, length)
C     Subroutine to break a long string of output into groups 
C       which don't wrap across line breaks.
C     Robert Grumbine 11-9-88

C     Arguments:
      CHARACTER*350 outch
      INTEGER length, brakpt

C     Local variables:
      INTEGER i, j, k, n
      CHARACTER*350 line
C***********************************************************

C       Make line break at a space
        IF (outch(brakpt:brakpt) .NE. ' ') THEN
CD        PRINT *,'padding?'
          k = 0 
 3000     CONTINUE
          k = k + 1
          IF (outch(brakpt-k:brakpt-k) .NE. ' ') GO TO 3000
          DO 3010 j = 1, brakpt-k 
             line(j:j) = outch(j:j)
 3010     CONTINUE
          DO 3020 i = j, brakpt
            line(i:i) = ' '
 3020     CONTINUE
          DO 3030 j = brakpt+1, 350
            line(j:j) = outch(j-k:j-k)
 3030     CONTINUE
          DO 3040 j = 1, 350-k
            outch(j:j) = line(j:j)
 3040     CONTINUE
          length = length+k
        ENDIF

      RETURN
      END
