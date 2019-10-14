C================================================================   
      SUBROUTINE getedg(lat, long, nskile, ntot, iunit)
      INTEGER iunit, nskile, ntot
      REAL lat(3*nskile), long(3*nskile)
      
      INTEGER i, dummy
      REAL tlat, tlong
      INTEGER ilat, ilong
      CHARACTER*60 header
      
      REWIND(iunit)
      DO 1000 i = 1, 5
        READ (iunit,9001) header
 1000 CONTINUE
      DO 1100 i = 1, nskile
        READ (iunit, 9001) header
 1100 CONTINUE
      READ (iunit,9001) header
 9001 FORMAT (A60)
CD      PRINT *,'In getedg, last header = '
CD      PRINT *,header 

      i = 0
 2000 CONTINUE
         i = i + 1
         READ(iunit, 9002, END=2100, ERR=2100) tlong, tlat
CD         PRINT *,'tlat, tlong ',tlat, tlong
         IF (tlat .GT. 0.) THEN
           lat(nskile+i) = tlat
           long(nskile+i) = tlong
          ELSE
           GO TO 2100
         ENDIF
         GO TO 2000
 2100 CONTINUE
      ntot = nskile+i-1
     
CD      PRINT *,'ntot = ',ntot, lat(ntot), long(ntot)
 
 9002 FORMAT (7x, 2F8.3)
      REWIND (iunit)

      RETURN
      END
