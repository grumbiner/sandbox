      PROGRAM fanaly
C     Program to combine the results of many significance analysis
C       runs.
C     1) count all occurrences of precisely the same frequency + combine.
C     2) delete any frequency which occurs less than a specified number
C          times AND is not within a specified frequency range of another
C          frequency
C     3) combine frequencies which are within the specified limit of each
C          other and delete those which occur less than the specified
C          number of times.
C     Robert Grumbine

      IMPLICIT none

      REAL freqs(10000), occur(10000,2)
      REAL dummy(10000), occur2(10000, 2)
      INTEGER n, i, m, oft, n2
      REAL toler

      PRINT *,'How many lines of data are there?'
      READ (*,9001) n

      CALL readin(freqs, n)

C     Sort frequencies
      DO 1000 i = 1, n
        dummy(i) = FLOAT(i)
 1000 CONTINUE

      CALL sort(dummy, freqs, n, 0, 1) 
D     WRITE (*,9002) (freqs(i), dummy(i), i = 1, n)

C     Collect all occurrences of the same frequency.
      DO 1100 i = 1, n
        occur(i, 1) = 0.0
        occur(1, 2) = 0.0
        occur2(i, 1) = 0.0
        occur2(i, 2) = 0.0
 1100 CONTINUE

      m = 1
      occur(1, 1) = freqs(1)
      occur(1, 2) = 1.0
      DO 2000 i = 2, n
        IF (freqs(i) .EQ. freqs(i-1)) THEN 
          occur(m, 2) = occur(m, 2) + 1.0
         ELSE
          m = m + 1
          occur(m, 1) = freqs(i)
          occur(m, 2) = 1.0
        ENDIF
 2000 CONTINUE

C     Print out the table of frequency and occurrence:
      PRINT *,'There were ',m,' different frequencies'
      WRITE (*,9002) (occur(i, 1), occur(i, 2), i = 1, m)

C     Now delete frequencies which occur less often than required, but
C        are not within some tolerance of any other frequency.
      PRINT *,'What is the frequency of occurrence requirement?'
      READ (*,9001) oft
      PRINT *,'What is the frequency isolation requirement?'
      READ (*,9003) toler

      n2 = 0
      IF ((occur(1, 2) .LT. oft) .AND.
     1 (ABS(occur(1, 1)-occur(2, 1)) .GT. toler)) THEN
C       The first frequency does not pass the frequency of occurrence 
C         test and is not within toler of the next frequency, do not
C         include it in the new table.
       ELSE
        n2 = 1
        occur2(1, 1) = occur(1, 1) 
        occur2(1, 2) = occur(1, 2)
      ENDIF
      DO 3000 i = 2, m-1
        IF ( (occur(i, 2) .LT. oft) .AND.
     1       ( ABS(occur(i, 1) - occur(i-1, 1)) .GT. toler) .AND.
     2       ( ABS(occur(i, 1) - occur(i+1, 1)) .GT. toler) )  THEN
C       Skip to next
         ELSE
          n2 = n2+1
          occur2(n2, 1) = occur(i, 1)
          occur2(n2, 2) = occur(i, 2)
        ENDIF
 3000 CONTINUE

      PRINT *,'There are < ',n2,' frequencies remaining.'
      PRINT *,'The candidates are:'
      WRITE (*,9002) (occur2(i, 1), occur2(i, 2), i = 1, n2)

 9001 FORMAT (I5)

 9002 FORMAT (E13.6, I4)

 9003 FORMAT (E13.6)

      END

