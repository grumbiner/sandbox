      SUBROUTINE sk2out(x0, y0, dx, dy, skpt, npts, time)
C     Print out the forecasts for the skiles2 program virtual drift.
C     Bob Grumbine 4 April 1994.
C     Print of Alaskan points brought in to local routine.
C     Bob Grumbine 22 March 1996.


      IMPLICIT none

      INTEGER npts, skpt(npts)
      REAL x0(npts), y0(npts), dx(npts), dy(npts)
      INTEGER time
      CHARACTER*8 DATE

      REAL arcdis, wdir, xp, yp, dxnm, dynm, dummy, kmtonm
      PARAMETER (kmtonm = 1. /  1.852 )
      REAL xstl, ystl, akrad
      PARAMETER (xstl = -170.0)
      PARAMETER (ystl =  65.0 )
      PARAMETER (akrad = 2500.0)

      INTEGER k

      IF (MOD(time,24) .NE. 0 ) RETURN

      WRITE (60,9002) time
      WRITE (61,9002) time
      WRITE (62,9002) time
 9002 FORMAT (I3,'-Hour Forecast ice drift')
      WRITE (60,9003)
      WRITE (61,9003)
      WRITE (62,9003)
 9003 FORMAT ('Atmosphere only driving')
      WRITE (60,9004) DATE()
      WRITE (61,9004) DATE()
      WRITE (62,9004) DATE()
 9004 FORMAT ('          Day Zero = ',A8)
      WRITE (60,9005) 
      WRITE (61,9005) 
      WRITE (62,9005) 

      WRITE (60,9001) 
      WRITE (61,9011) 
      WRITE (62,9011) 
      DO 1000 k = 1, npts
        xp = x0(k)+dx(k)
        yp = y0(k)+dy(k)
        dxnm = kmtonm * arcdis(x0(k), y0(k), xp, y0(k))
        dynm = kmtonm * arcdis(x0(k), y0(k), x0(k), yp)
        dxnm = SIGN(dxnm, x0(k)-xp)
        dynm = SIGN(dynm, y0(k)-yp)

C       BG output file
        WRITE (60,9010) skpt(k), x0(k), y0(k),
     3  wdir  (-dxnm, -dynm, dummy),
     2  kmtonm * arcdis(x0(k),y0(k),xp,yp)

C       Operational output file
        IF (k .LE. 207) THEN 
          WRITE (61,9012) skpt(k), wdir(-dxnm, -dynm, dummy),
     1      kmtonm * arcdis(x0(k),y0(k),xp,yp)
          IF (arcdis(xstl, ystl, x0(k), y0(k) ) .LE. akrad) THEN
            WRITE (62,9012) skpt(k), wdir(-dxnm, -dynm, dummy),
     1      kmtonm * arcdis(x0(k),y0(k),xp,yp)
          ENDIF

         ELSE
          IF (k .EQ. 208) WRITE (61,9001)
          IF (k .EQ. 208) WRITE (62,9001)
          WRITE (61,9010) skpt(k), x0(k), y0(k),
     1    wdir  (-dxnm, -dynm, dummy),
     2    kmtonm * arcdis(x0(k),y0(k),xp,yp)
          IF (arcdis(xstl, ystl, x0(k), y0(k) ) .LE. akrad) THEN
            WRITE (62,9010) skpt(k), x0(k), y0(k),
     1      wdir  (-dxnm, -dynm, dummy),
     2      kmtonm * arcdis(x0(k),y0(k),xp,yp)
          ENDIF
        ENDIF
 1000 CONTINUE

      WRITE (60,9005) 
      WRITE (61,9005) 
      WRITE (62,9005) 
      WRITE (60,9005) 
      WRITE (61,9005) 
      WRITE (62,9005) 

 9001 FORMAT ('Point ',' Initial location ',
     1    '   Dir  Dist(nm)')
 9011 FORMAT ('Point ','  Dir   Dist(nm)')

 9010 FORMAT (I4,3X,2F8.3,2X,2F6.1)
 9012 FORMAT (I4,3X,2F6.1)
 9005 FORMAT (' ')

      RETURN
      END 
