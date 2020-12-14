      PROGRAM bgmapvec
C     Plot vector velocities on a background geographic map using
C       NCAR Graphics
C     Note that this does _not_ handle the fact that the velocity
C       Grid is offset from the scalar grid.
C     Robert Grumbine 14 January 1994.

      IMPLICIT none

      INCLUDE "icegrid.inc"
      REAL x(L,M), y(L,M)
      INTEGER i, j, iframe, nframe
      REAL lat(50000), lon(50000)
      REAL xlon(50000), ylat(50000)

      REAL clevel
      INTEGER idot, ier
      CHARACTER*60 fname
      REAL xmin1, xmax1, ymin1, ymax1
      REAL rlat, rlon, rot
      INTEGER jproj
      COMMON /xytrn/ nm, nn, itrans
      INTEGER nm, nn, itrans, nbuoy
      REAL ub, ul, ur, ut
      REAL fl, fb, fr, ft
      REAL t1, t2

      nm = L
      nn = M
      itrans = 3

      CALL OPNGKS

      READ (*,9001) xmin1
      READ (*,9001) xmax1
      READ (*,9001) ymin1
      READ (*,9001) ymax1
      READ (*,9003) jproj
      READ (*,9001) rlat
      READ (*,9001) rlon
      READ (*,9002) fname
      READ (*,9003) nframe
      OPEN (UNIT=10, FILE=fname, FORM='UNFORMATTED', 
     1  STATUS='OLD')

      clevel = 0.

      DO 1000 iframe = 1, nframe
        CALL gselnt (0)
        CALL SET(0.0, 1.0, 0.0, 1.0, 0., 1., 0., 1., 1)


        ier = 0  
        idot  = 0
        CALL SUPMAP(jproj, rlat, rlon, rot,
     1       ymin1, xmin1, ymax1, xmax1, +2, 5, 1, idot, ier)
        IF (ier .NE. 0) STOP 'supmap error'
        CALL GETSET (fl,fr,fb,ft,ul,ur,ub,ut,1)

CD        CALL getboy (lat, lon, nbuoy, xlon, ylat)
CD        DO 1000 j = 1, nbuoy
CD          t1 = xlon(j) * (ur - ul) + ul 
CD          t2 = ylat(j) * (ut - ub) + ub
CD          CALL WTSTR(t1, t2, 'X', 1, 0, 0)
CD 1000   CONTINUE

        READ (10) x
        READ (10) y
C       The following can be used to zero out slow vectors.
C        Not currently used
CD        DO 2000 j = 1, M+1
CD          DO 2100 i = 1, L+1
CD            IF (ABS(y(i,j)) .LT. clevel/2.) y(i,j) = 0.
CD 2100     CONTINUE
CD 2000   CONTINUE

        CALL EZVEC(x, y, L, M)

        CALL FRAME

 1000 CONTINUE

      CALL CLSGKS
  
 9001 FORMAT (E13.6)
 9002 FORMAT (A60)
 9003 FORMAT (I3)
 
      STOP 
      END                                                        
