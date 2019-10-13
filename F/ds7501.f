      SUBROUTINE ds7501(topo, unit, fname)
C     Read in NCAR data set 750.1, earth topography on a 1 degree
C       grid, at half-integer latitude/longitudes, with 1, 1 at
C       89.5 S, 0 E.
C     Robert Grumbine 6 June 1994

      IMPLICIT none
      
      INTEGER elev(360,180)
      REAL topo(360, 180)
      INTEGER unit, k, i, j
      CHARACTER*72 fname
      
      OPEN (unit, FILE=fname, FORM='FORMATTED', STATUS='OLD')
      
      DO 100 j = 1, 180
	DO 110 k = 0, 29
          READ (10, 1000) (elev(i+12*k,j),i=1,12)
  110   CONTINUE
  100 CONTINUE

      DO 200 j = 1, 180
        DO 210 i = 1, 360
          topo(i,j) = FLOAT(elev(i,j))
  210   CONTINUE
  200 CONTINUE

 1000 FORMAT (12(I5,1X))
      
      RETURN
      END
