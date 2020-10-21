C===========================================================----------++
      SUBROUTINE regrid(lats, longs, npts, tin, tav, def)
C     Author Bob Grumbine
C     LAST MODIFIED 21 June 1994.
C     Translate from 1 degree grid to polar stereo grid.

      IMPLICIT none

      INCLUDE "icegrid.inc"
      INTEGER npts
      REAL lats(LP*MP), longs(LP*MP), tin(360,180), tav(LP*MP)
      REAL def
C===========================================================----------++

      REAL ri(LP*MP), rj(LP*MP)

      INTEGER i1(LP*MP), i2(LP*MP), j1(LP*MP), j2(LP*MP)
      REAL long1(LP*MP), long2(LP*MP), lat1(LP*MP), lat2(LP*MP)
      REAL d1(LP*MP), d2(LP*MP), d3(LP*MP), d4(LP*MP)
      REAL w1(LP*MP), w2(LP*MP), w3(LP*MP), w4(LP*MP)
      REAL ws(LP*MP), dist, wt, ttemp
      INTEGER lm1(LP*MP), ln2(LP*MP), lm3(LP*MP), lm4(LP*MP)
      INTEGER rri, rrj, kl
      REAL tlong, tlat

      INTEGER i, it, j, k, pts(LP*MP)
      REAL arcdis
      REAL radius
      PARAMETER (radius = 220.)
      INTEGER range
      PARAMETER (range = 2)
 
      IF (npts .GT. LP*MP) STOP 'Too many npts '
      DO 1000 k = 1, npts
        ri(k) =  0.5+longs(k)
        rj(k) = 90.5+lats(k)
        ri(k) =  0.0+longs(k)
        rj(k) = 90.0+lats(k)
        i1(k) = INT(ri(k))
        i2(k) = i1(k)+1
        j1(k) = INT(rj(k))
        j2(k) = j1(k)+1
        lat1(k) = -90.5+j1(k)
        long1(k) = -0.5+i1(k)
        lat2(k) = -90.5+j2(k)
        long2(k) = -0.5+i2(k)
C       Do some pre-checking on latitude and longitude bounds
        IF (long1(k) .GT. 360.) long1(k) = long1(k)-360.
        IF (long2(k) .GT. 360.) long2(k) = long2(k)-360.
        IF (lat1(k)  .GT.  90.) lat1(k)  =  90.
        IF (lat2(k)  .GT.  90.) lat2(k)  =  90.
        IF (lat1(k)  .LT. -90.) lat1(k)  = -90.
        IF (lat2(k)  .LT. -90.) lat2(k)  = -90.
 1000 CONTINUE

      DO 2000 k = 1, npts
CD        PRINT *,'2000, k= ',k, i1(k), i2(k), j1(k), j2(k),
CD     1      longs(k), lats(k)

        d1(k) = arcdis(long1(k), lat1(k), longs(k), lats(k))
        d2(k) = arcdis(long2(k), lat1(k), longs(k), lats(k))
        d3(k) = arcdis(long1(k), lat2(k), longs(k), lats(k))
        d4(k) = arcdis(long2(k), lat2(k), longs(k), lats(k))
        IF (tin(i1(k), j1(k)) .LT. -3.) THEN
          lm1(k) = 0
         ELSE
          lm1(k) = 1
        ENDIF
        IF (tin(i2(k), j1(k)) .LT. -3.) THEN
          ln2(k) = 0
         ELSE
          ln2(k) = 1
        ENDIF
        IF (tin(i1(k), j2(k)) .LT. -3.) THEN
          lm3(k) = 0
         ELSE
          lm3(k) = 1
        ENDIF
        IF (tin(i2(k), j2(k)) .LT. -3.) THEN
          lm4(k) = 0
         ELSE
          lm4(k) = 1
        ENDIF
 2000 CONTINUE

      DO 3000 k = 1, npts
        pts(k) = lm1(k)+ln2(k)+lm3(k)+lm4(k)
        w1(k) = EXP(-4.*d1(k)*d1(k)/radius/radius)
        w2(k) = EXP(-4.*d2(k)*d2(k)/radius/radius)
        w3(k) = EXP(-4.*d3(k)*d3(k)/radius/radius)
        w4(k) = EXP(-4.*d4(k)*d4(k)/radius/radius)
        IF (pts(k) .EQ. 0) THEN
CD          PRINT *,k, i1(k), j1(k), longs(k), lats(k)
        ENDIF
 3000 CONTINUE

      DO 4000 k = 1, npts
        IF (pts(k) .NE. 0) THEN
          tav(k) = (w1(k)*lm1(k)*tin(i1(k),j1(k))
     1           +  w2(k)*ln2(k)*tin(i2(k),j1(k))
     2           +  w3(k)*lm3(k)*tin(i1(k),j2(k))
     3           +  w4(k)*lm4(k)*tin(i2(k),j2(k))  )
     4 / (w1(k)*lm1(k)+w2(k)*ln2(k)+w3(k)*lm3(k)+w4(k)*lm4(k) )
          ELSE
           tav(k) = def
        ENDIF
 4000 CONTINUE

C     Now sweep through for points that are ocean by mask, but not
C      within 1 grid point of a Levitus point.
      DO 5000 k = 1, npts
        IF (pts(k) .NE. 0) GO TO 5000
        rri = INT(ri(k)+0.5)
        rrj = INT(rj(k)+0.5)
        kl  = 0
        DO 5100 j = rrj-range, rrj+range
          DO 5110 i = rri-range, rri+range
            IF (i .LT. 1) THEN 
              it = i+360
             ELSE
              it = i
            ENDIF
            IF (i .GT. 360) THEN
              it = i - 360
             ELSE
              it = i
            ENDIF
            IF (j .LT. 1 .OR. j .GT. 180) GO TO 5110
            IF (tin(it, j) .GT. -3.) THEN
              kl = kl+1
              tlong = -0.5+i
              IF (tlong .GT. 360.) tlong = tlong-360.
              tlat = -90.5+j
              dist  = arcdis(tlong, tlat, longs(k), lats(k))
              ws(kl) = EXP(-4.*dist*dist/radius/radius)
            ENDIF
 5110     CONTINUE
 5100   CONTINUE
        IF (kl .EQ. 0) THEN
          PRINT *,'No data available point at ',longs(k),lats(k)
          tav(k) = def
         ELSE
          kl = 0
          wt = 0.0
          ttemp = 0.0
          DO 5200 j = rrj-range, rrj+range
            DO 5210 i = rri-range, rri+range
              IF (tin(i, j) .GT. -3.) THEN
                kl = kl+1
                wt = wt + ws(kl)
                ttemp = ttemp + ws(kl)*tin(i, j)
              ENDIF
 5210      CONTINUE
 5200    CONTINUE
         tav(k) = ttemp/wt 
        ENDIF
 5000 CONTINUE

      RETURN
      END  
