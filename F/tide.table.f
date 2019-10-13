      PROGRAM table
C     Program to read in all tidal information and tabulate it.
C     Robert Grumbine 27 September 1994

      IMPLICIT NONE

      INTEGER nfiles, nfreqs
      PARAMETER (nfiles = 23)
      PARAMETER (nfreqs = 33)

C     Define data variables:
      CHARACTER*80 header
      CHARACTER*4  names(nfiles)
      CHARACTER*9  doods(nfreqs)
      REAL         freqs(nfreqs), datums(nfreqs, nfiles, 7)

C     Miscellaneous variables:
      INTEGER i, j, k 
      INTEGER k1, k2, k3
      LOGICAL match
      CHARACTER*60 fname
      CHARACTER*1  polar
      REAL freq, major, minor, eccen, alpha, beta
      REAL tide

      DO 1000 i = 1, nfiles
        READ (*,9001) names(i)
 9001   FORMAT (A4)
 1000 CONTINUE

C     Repeat-until loop to read in every data file:
      j = 0
 2000 CONTINUE
        j = j + 1
        READ (*,9002) fname
        OPEN (10, FILE=fname, FORM='FORMATTED', STATUS='OLD')
 9002   FORMAT (A60)
        READ (10, 9003) header
        DO 2100 i = 1, nfreqs
          READ (10, 9004) freq, major, minor, eccen, alpha, beta, polar
 9003     FORMAT (A80)
 9004     FORMAT (11X, F8.6, 2F7.2, F7.3, 2F10.1, 7X, A1)
          datums(i, j, 1) = freq
          datums(i, j, 2) = major
          datums(i, j, 3) = minor
          datums(i, j, 4) = eccen
          datums(i, j, 5) = alpha
          datums(i, j, 6) = beta
          IF (polar .EQ. 'a') THEN
            datums(i, j, 7) = 1.0
           ELSE
            datums(i, j, 7) = 0.0
          ENDIF
 2100   CONTINUE

        CLOSE(10, STATUS='KEEP')
        IF (j .LT. nfiles) GO TO 2000

      DO 3000 i = 1, nfreqs
        freqs(i) = datums(i, 1, 1)
        CALL tidefr ( freqs(i), k1, k2, k3, match, tide, 0.0)
        WRITE (doods(i), '(3I3)') k1, k2, k3
 3000 CONTINUE

      DO 4000 k = 2, 7
C       Get table header:
        READ (*, 9003) header
        WRITE (*, 9003) header 
        WRITE (*, 9006) (names(i), i = 1, nfiles)
 9006   FORMAT (18X, 23A5)

        DO 4100 i = 1, nfreqs
         IF (k .LT. 5 .OR. k .GT. 6) THEN
           WRITE (*,9005) doods(i), freqs(i), 
     1                    (datums(i, j, k), j = 1, nfiles) 
 9005      FORMAT (A9, F9.6, 23F5.2)
         ELSE
           WRITE (*,9007) doods(i), freqs(i), 
     1                    (datums(i, j, k), j = 1, nfiles) 
 9007      FORMAT (A9, F9.6, 23F5.0)
        ENDIF
 4100   CONTINUE
 4000 CONTINUE

      END
