      PROGRAM buoycont
C     From Rachel Teboulle's  mldplot.f
      IMPLICIT none

      INCLUDE "icegrid.inc"
      REAL x(L+1, M+1, 34)
      REAL y(L+1, M+1), y2(L+1, M+1)
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

      READ (*,9001) xmin1
      READ (*,9001) xmax1
      READ (*,9001) ymin1
      READ (*,9001) ymax1
      READ (*,9003) jproj
      READ (*,9001) rlat
      READ (*,9001) rlon

      CALL gselnt (0)
      CALL SET(0.0, 1.0, 0.0, 1.0, 0., 1., 0., 1., 1)


        ier = 0  
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

        READ (*,9002) fname
        OPEN (UNIT=10, FILE=fname, FORM='UNFORMATTED', 
     1    STATUS='OLD')

        clevel = 0.
        xlo = 0.
        xhi = 0.
        READ (*,9001) clevel
        READ (*,9001) xlo
        READ (*,9001) xhi
        nset = 1
        ihgh  = 0
        linsol = 0
        idot  = 0
        i = 32
        READ (10) y
        DO 2000 j = 1, M+1
          DO 2100 i = 1, L+1
            IF (ABS(y(i,j)) .LT. clevel/2.) y(i,j) = 0.
 2100     CONTINUE
 2000   CONTINUE

        CALL CONREC (y,L+1,L+1,M+1,xlo,xhi,clevel,nset,ihgh,linsol)

        CALL FRAME

      CALL CLSGKS
  
 9001 FORMAT (E13.6)
 9002 FORMAT (A60)
 9003 FORMAT (I3)
 
      STOP 
      END                                                        
