      SUBROUTINE getskpts(lat, long, nskile, iunit, fname)
C     Read in the skiles points for/from virtual drift programs.
C     Robert Grumbine 4 April 1994.

      IMPLICIT none

      INTEGER nskile, iunit
      REAL lat(3*nskile), long(3*nskile)
      INTEGER i, dum
      CHARACTER*60 fname
      
      OPEN (iunit, FILE=fname, FORM='FORMATTED', STATUS='OLD')
      DO 1000 i = 1, nskile
        READ (iunit, *) dum, lat(i), long(i)
 1000 CONTINUE
      CLOSE (iunit, STATUS='KEEP')
      
      RETURN
      END
