      PROGRAM af
C     Read in air-force snow and ice map, write out an ice map
C       for comparison purposes.
C     Bob Grumbine 6 December 1994

      IMPLICIT none

      CHARACTER*1024 header
      INTEGER*2 ice(512, 512)
      INTEGER*2 line(512)
      CHARACTER*1 map(512, 512)

      INTEGER i, j

      READ (20) header
      READ (20) header

      DO 1000 j = 1, 512
        READ (20) line
        DO 1100 i = 1, 512
          ice(i,j) = line(i)
 1100   CONTINUE
 1000 CONTINUE

      DO 2000 j = 1, 512
        DO 2100 i = 1, 512
          IF (ice(i,j) .EQ. 0) THEN
            map(i,j) = CHAR(0)
           ELSE IF (ice(i,j) .EQ. 4090) THEN
            map(i,j) = CHAR(100)
           ELSE
            map(i,j) = CHAR(0)
          ENDIF
 2100   CONTINUE
 2000 CONTINUE

      WRITE (21) map

      STOP
      END

      
