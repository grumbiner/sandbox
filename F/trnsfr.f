      SUBROUTINE trnsfr(tempor, idim, jdim,
     1    a, tlong, tlat, x, nlong, nlat,
     2    long1, lat1, long2, lat2, dlong, dlat, mult  )
C     Transfer the data from a global gaussian grid onto
C       a subsection of the sphere.  BG 3/6/92.
C     Bob Grumbine 7 April 1994.

      IMPLICIT none

      INTEGER idim, jdim, tlat, tlong, nlat, nlong
      REAL lat1, long1, lat2, long2, dlat, dlong
      REAL a(tlong, tlat)
      REAL x(nlong, nlat)
      REAL tempor(idim, jdim)
      REAL mult

C     Local variables. 
      INTEGER i, j
      INTEGER tmin, tmax, gmin, gmax

C     Translate onto the global spherical grid
      CALL GAU2L(tempor, idim, jdim, a, tlong, tlat)

C     Extract the desired subset of the grid.
C     The NMC grid has 1,1 at 0 E, 90 N, increasing eastward and
C       southward.
C     In specifying the lat/long box, I've taken lat1 as the
C       southernmost point, rather than northernmost.
      gmin = INT(long1/dlong) + 1
      tmin = INT((90.-lat1)/dlat)+1
      gmax = INT(long2/dlong)
      tmax = INT((90.-lat2)/dlat)+1

      DO 4000 j = tmax, tmin
        DO 4010 i = gmin, gmax
          x (i-gmin+1, tmin-j+1) = a(i,j)
 4010   CONTINUE
 4000 CONTINUE

C     Rescale multiplicatively
      IF (mult .EQ. 1.0) GO TO 5000
      DO 4020 j = 1, nlat
        DO 4030 i = 1, nlong
          x(i,j) = x(i,j)*mult
 4030   CONTINUE
 4020 CONTINUE

 5000 CONTINUE

      RETURN
      END
