      PROGRAM bvsm
C     Intercompare bathymetry and land mask file on northern hemisphere
C       127 km grid.
C     Robert Grumbine
C     9 November 1995

      IMPLICIT none

      INTEGER i, j, nx, ny
      PARAMETER (nx = 77)
      PARAMETER (ny = 93)
      REAL bathy(nx, ny)
      REAL mask(nx, ny)
      REAL inmask(nx, ny)
      CHARACTER*9 formp

      OPEN (10, FILE='bathy', FORM='UNFORMATTED', STATUS='OLD')
      OPEN (11, FILE='mask', FORM='FORMATTED', STATUS='OLD')
      READ (10) bathy

 9001 FORMAT ('(', I3,'G1.0)' )
      WRITE (formp, 9001) nx
      READ (11, formp) ((mask(i,j), i=1,nx), j = 1, ny)

      DO 1000 j = 1, ny
        DO 1100 i = 1, nx
          IF (mask(i,j) .EQ. 1. .AND. bathy(i,j) .LE. 1.0) THEN
            PRINT *,'mask is water, bathy is land ',i-1, j-1
          ENDIF
          IF (mask(i,j) .EQ. 0. .AND. bathy(i,j) .GT. 1.0) THEN
            PRINT *,'mask is land, bathy is water ',i-1, j-1, bathy(i,j)
          ENDIF
 1100   CONTINUE
 1000 CONTINUE 

      STOP
      END
