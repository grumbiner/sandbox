      PROGRAM contour
C     From Rachel Teboulle's  mldplot.f
      IMPLICIT none

      INCLUDE "oldgrid.inc"
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
 
      SUBROUTINE trans(y, x, point)
      INCLUDE "oldgrid.inc"
      REAL x(0:L,0:M,34), y(0:L,0:M)
      INTEGER point
      INTEGER i, j
      
      DO 1000 j = 0, M
        DO 1100 i = 0, L
          y(i,j) = x(i,j,point)
 1100   CONTINUE
 1000 CONTINUE

      RETURN
      END
      FUNCTION FX(x,y)
C     Convert a grid point pair to latitude and longitude
      INCLUDE "oldgrid.inc"
      REAL phi, xlon, ylat, fxlon, ydum
      COMMON /xytrn/ nm, nn, itrans
      INTEGER nm, nn, itrans

      nm = L+1
      nn = M+1
      itrans = 3

      phi = -90. + 
     1  SQRT(dx**2*(x-1-polei)**2 + dy**2*(y-1-polej)**2)/dxdeg
      ylat = SIGN(phi, latmin)
      xlon = ATAN2(y-1-polej , x-1-polei) * 180. / (ATAN(1.)*4.)

      call maptrn(ylat, xlon, fxlon, ydum)
      fx = fxlon

      RETURN
      END 
      FUNCTION FY(x,y)
C     Convert a grid point pair to latitude and longitude
      INCLUDE "oldgrid.inc"
      REAL phi, xlon, ylat, fylat, ydum
      COMMON /xytrn/ nm, nn, itrans
      INTEGER nm, nn, itrans

      nm = L+1
      nn = M+1
      itrans = 3

      phi = -90. + 
     1  SQRT(dx**2*(x-1-polei)**2 + dy**2*(y-1-polej)**2)/dxdeg
      ylat = SIGN(phi, latmin)
      xlon = ATAN2(y-1-polej , x-1-polei) * 180. / (ATAN(1.)*4.)

      call maptrn(ylat, xlon, ydum, fylat)
      fy = fylat

      RETURN
      END 
      SUBROUTINE getboy(lat, lon, nbuoy, x, y)
      REAL lat(50000), lon(50000)
      REAL x(50000), y(50000)
      INTEGER nbuoy
      CHARACTER*60 fname
      INCLUDE "oldgrid.inc"
      REAL pi, r, toler, arcdis
      PARAMETER (toler = 20.0)
 
      READ (*,9001) fname
      OPEN (12, FILE=fname, FORM='FORMATTED', STATUS='OLD')
     
      i = 0
 1000 CONTINUE
        READ (12, 9002, END=1100) lat(i+1), lon(i+1)
CD        WRITE (*,9002) lat(i+1), lon(i+1)
        lon(i+1) = 360. - lon(i+1)
C       Convert from degrees west of buoys to degrees east of
C       rest of world
        IF (i+1 .GE. 2) THEN
          IF (arcdis(lon(i+1), lat(i+1), lon(i), lat(i)) .GT. toler) 
     1     THEN
            i = i + 1
            GO TO 1000
           ELSE
            GO TO 1000
          ENDIF
         ELSE
          i = i + 1
          GO TO 1000
        ENDIF
 1100 CONTINUE
      nbuoy = i
      PRINT *,'nbuoy = ', nbuoy

      pi = 4.*ATAN(1.)
      DO 2000 i = 1, nbuoy
        r = DXDEG*(90. - lat(i) )
        x(i) = polei + COS(lon(i)*pi/180.)*r/DX + 1
        y(i) = polej + SIN(lon(i)*pi/180.)*r/DY + 1
 2000 CONTINUE
      DO 2001 i = 1, nbuoy
        x(i) = x(i) / FLOAT(L+1)
        y(i) = y(i) / FLOAT(M+1)
CD        PRINT *,'x, y ',x(i), y(i)
        IF (x(i) .GT. 1. .OR. y(i) .GT. 1.) THEN
          PRINT *,'x, y, out of bounds ',x(i), y(i), lon(i), lat(i)
          x(i) = 0.0
          y(i) = 0.0
        ENDIF
 2001 CONTINUE
 
 9001 FORMAT (A60)
 9002 FORMAT (7x, 9x, F5.1, F6.1)

      RETURN
      END
      FUNCTION arcdis (long1, lat1, long2, lat2)
C     Function to compute the distance between two latitude, longitude
C       points in nautical miles.
C     Algorithm from Doug MacAyeal, The University of Chicago, 1985.

      IMPLICIT none

      REAL ab, ac, bc, arcdis, arg
      REAL lat1, long1, lat2, long2

      REAL pi, rdpdg, kmtonm, rearth
      PARAMETER (pi = 3.141592654)
      PARAMETER (rdpdg = pi / 180.)
      PARAMETER (kmtonm = 1.852)
      PARAMETER (rearth = 6730.949)

C     Bullet-proofing: Check that lats are within +- 90, longs
C       longs are within +- 360.
      IF (ABS(lat1) .GT. 90. .OR. ABS(lat2) .GT. 90.) THEN
        PRINT *,'Latitudes are off the planet.'
        PRINT *,'No fix within arcdis.'
        PRINT *,'WARNING error.'
      ENDIF
      IF (ABS(long1) .GT. 360. .OR. ABS(long2) .GT. 360.) THEN
        PRINT *,'Longitudes are outside range.'
        PRINT *,'No fix within arcdis.'
        PRINT *,'WARNING long1, long2 = ',long1, long2
      ENDIF

C     Operational Code      
C     Special case included because trig round off can give identical
C      points a nonzero separating distance. BG 6/3/93.
      IF (long1 .EQ. long2 .AND. lat1 .EQ. lat2) THEN
        arcdis = 0.0
        RETURN
      ENDIF

      ab = (90.-lat1)*rdpdg
      ac = (90.-lat2)*rdpdg
      bc = ABS((long1-long2)*rdpdg)
C     arg introduced 6/3/93 to proof against the special case that
C       round-off errors in trig produce a value for the argument 
C       which is out of bounds for the arc-cosine function.  This
C       should be rare if not totally impossible.
      arg = COS(ab)*COS(ac)+SIN(ab)*SIN(ac)*COS(bc)
      IF (arg .GT.  1.0) THEN 
        PRINT *,'Out of bounds arcdis ',lat1, long1, lat2, long2
        arg =  1.0
      ENDIF
      IF (arg .LT. -1.0) THEN 
        PRINT *,'Out of bounds arcdis ',lat1, long1, lat2, long2
        arg = -1.0
      ENDIF
      arcdis = ACOS(arg)*rearth

      arcdis = arcdis/kmtonm

C     Bullet-proofing: Verify that distance is less than circumference
C       of the earth.
      IF (arcdis .GT. 2.*pi*rearth/kmtonm) THEN
        PRINT *,'Failure in computation of distance on earth.'
        PRINT *,'Derived distance = ',arcdis,' nautical miles.'
        PRINT *,'FATAL error'
        STOP
      ENDIF

      RETURN
      END
