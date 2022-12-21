      PROGRAM a
C     From Rachel Teboulle's  mldplot.f
C     Robert Grumbine 14 January 1994
C     Plot 'x's at buoy locations on an overlay of meteorological
C       contours of some sort (x data array)

      IMPLICIT none

      INCLUDE "icegrid.inc"
      REAL x(L+1, M+1, 34)
      REAL y(L+1, M+1)
      REAL lat(50000), lon(50000)
      INTEGER i, j
      REAL xlon(50000), ylat(50000)

      REAL clevel, xlo, xhi
      INTEGER idot, ier, ihgh, icn, linsol, jframe, nset
      CHARACTER*60 fname
      REAL xmin1, xmax1, ymin1, ymax1
      REAL rlat, rlon, rot
      INTEGER jproj
      COMMON /xytrn/ nm, nn, itrans
      INTEGER nm, nn, itrans, nbuoy
      REAL ub, ul, ur, ut
      REAL fl, fb, fr, ft
      REAL t1, t2

      nm = L+1
      nn = M+1
      itrans = 3

      CALL OPNGKS

      READ (*,9002) fname
      OPEN (UNIT=10, FILE=fname, FORM='UNFORMATTED', 
     1  STATUS='OLD')

      clevel = 2.5
      xlo = 263.15
      xhi = 305.
      READ (10) x

      READ (*,9001) xmin1
      READ (*,9001) xmax1
      READ (*,9001) ymin1
      READ (*,9001) ymax1
      READ (*,9003) jproj
      READ (*,9001) rlat
      READ (*,9001) rlon

      ihgh  = 0
      linsol = 0
      idot  = 0
      CALL gselnt (0)
      CALL SET(0.0, 1.0, 0.0, 1.0, 0., 1., 0., 1., 1)
        i = 32
        CALL trans(y, x, i)
        ier = 0  
        CALL SUPMAP(jproj, rlat, rlon, rot,
     1       ymin1, xmin1, ymax1, xmax1, +2, 5, 1, idot, ier)
        IF (ier .NE. 0) STOP 'supmap error'
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
        call getset (fl,fr,fb,ft,ul,ur,ub,ut,1)

        CALL getboy (lat, lon, nbuoy, xlon, ylat)
        DO 2000 j = 1, nbuoy
          t1 = xlon(j) * (ur - ul) + ul 
          t2 = ylat(j) * (ut - ub) + ub
          CALL WTSTR(t1, t2, 'X', 1, 0, 0)
CD          CALL POINTS(t1, t2, 1, ICHAR('X'), 0)
C        
 2000   CONTINUE

        nset = 1
        CALL CONREC (y,L+1,L+1,M+1,xlo,xhi,clevel,nset,ihgh,linsol)

        CALL FRAME
   9  CONTINUE

      CALL CLSGKS
  
 9001 FORMAT (E13.6)
 9002 FORMAT (A60)
 9003 FORMAT (I3)
 
      STOP 
      END                                                        
