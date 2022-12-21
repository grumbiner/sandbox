      PROGRAM tsmap
C     Read in the Great Lakes grid and write out fields for
C       ice forecast model.  Need to flip the vertical.
C     Bob Grumbine 9 July 1995.

      IMPLICIT none

      INTEGER NX_GLK, NY_GLK
      PARAMETER (NX_GLK = 85)
      PARAMETER (NY_GLK = 85)

      INTEGER imask(NY_GLK,NX_GLK)
      REAL bathy(NY_GLK,NX_GLK)
      REAL bref(0:5)

      INTEGER i, j
  
      bref(0) = 1.0
      bref(1) = 20.0
      bref(2) = 50.0
      bref(3) = 100.0
      bref(4) = 150.0
      bref(5) = 200.0

      DO 100 j = 1, 85
        READ (10, 9001) (imask(i,j),i=1,NX_GLK)
  100 CONTINUE
 9001 FORMAT (85I1)

      DO 1000 j = 1, NY_GLK
        DO 1100 i = 1, NX_GLK

          bathy(i,j) = bref( imask(i,j) )

 1100   CONTINUE
 1000 CONTINUE
    
      WRITE (13) bathy

      STOP
      END
