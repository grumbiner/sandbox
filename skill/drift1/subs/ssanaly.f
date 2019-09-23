      SUBROUTINE ssanaly(dist1, dir1, dist2, dir2, npts, ia, r2, vcor)
C     Conduct simple analyses of displacement fields
C     1- correlation between 1 and 2 displacements
C     2  index of agreement
C     3  vector correlation.
C     Bob Grumbine 21 April 1994.
C     Revised for generality in working with buoys as well as forecast models.
C     Bob Grumbine 10 April 1995.
C     Variant ssanaly derived from sanaly for use with a single vector.
C     Bob Grumbine 10 April 1995.

      IMPLICIT none

      INTEGER npts, scorefile

C     Parameters for reading data in
      REAL dir1(npts), dist1(npts)
      REAL dir2(npts), dist2(npts)
      REAL x1(npts), y1(npts)
      REAL x2(npts), y2(npts)

      REAL ia, r2, vcor, iagree, rbar
    
      ia = iagree(dist1, dist2, npts)
   
      CALL correl(dist1, dist2, npts, r2, 
     1                      rbar, rbar, rbar, rbar)

      CALL vectorize(dist1, dir1, x1, y1, npts)
      CALL vectorize(dist2, dir2, x2, y2, npts)

      CALL vcc(x1, y1, x2, y2, npts, vcor)

CD      WRITE (scorefile,9002) ia, r2, vcor

CD 9002 FORMAT (3F6.3)
   
      RETURN
      END
