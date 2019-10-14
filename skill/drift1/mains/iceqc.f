      PROGRAM iceqc
C     Read in the ocean wave model land-ice map and reformat for
C      the NIC.
C     Bob Grumbine 4 April 1994.

      IMPLICIT none

      LOGICAL*1 q(150, 59), redone(59, 150)
      INTEGER i, j

      READ (10, 8) q
   8  FORMAT (50L1)

      DO 1000 j = 1, 59
        DO 1100 i = 1, 150
          redone(j, i) = q(i,j)
 1100   CONTINUE
 1000 CONTINUE

      WRITE (11, 9) redone
   9  FORMAT (59L1)

      STOP
      END


