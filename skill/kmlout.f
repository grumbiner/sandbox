      SUBROUTINE kmlout(lon, lat, npts, dir, dist, hours, date, unit)
!Robert Grumbine
!6 November 2014

      IMPLICIT none
   
      INTEGER npts, hours, date, unit
      REAL lat(npts), lon(npts), dir(npts), dist(npts)

      INTEGER i
      CHARACTER*80 fname
      REAL tlon, tlat, tdir, tdist


      WRITE (fname,1001) hours
 1001 FORMAT ("seaice_drift_",I3.3,".kml")
      OPEN (unit, FILE=fname, FORM="FORMATTED", STATUS="NEW")


!Print out the kml header file information
      WRITE (unit,8999) "<?xml version=""1.0"" encoding=""UTF-8""?>"
      WRITE (unit,8998) 
     1   "<kml xmlns=""http://earth.google.com/kml/2.2"">"
      WRITE (unit,*) "<Document>"

      WRITE (unit,*) "<Folder>"
      WRITE (unit,*) "<name>Sea Ice Drift distance (nm) and direction,"
      WRITE (unit,*) hours
      WRITE (unit,*) " hours from ",date
      WRITE (unit,*) " </name>"
      WRITE (unit,*) "  <LookAt>"
      WRITE (unit,*) "   <longitude>-170</longitude>"
      WRITE (unit,*) "    <latitude>63.3</latitude>"
      WRITE (unit,*) "    <range>4000000</range>"
      WRITE (unit,*) "  </LookAt>"
 8999 FORMAT (A38)
 8998 FORMAT (A45)


! Now start printing out the pushphins with information on the ice
!    points.
      DO i = 1, npts
        IF (lon(i) .GT. 180.) THEN 
           tlon = lon(i) - 360.
        ELSE
           tlon = lon(i)
        ENDIF
        IF (tlon .LT. -180) THEN
           tlon = tlon + 360.
        ENDIF

        WRITE (unit,*) "<Placemark> <name>"
        WRITE (unit,9001) dist(i)
        WRITE (unit, *) " nm, "
        WRITE (unit,9002) dir(i)
        WRITE (unit, *) " degrees </name>"
        WRITE (unit,*) "  <Point> <coordinates>"
        WRITE (unit,9003) tlon, lat(i)
        WRITE (unit,*) " 0 </coordinates> </Point> </Placemark>"

      ENDDO 
 9001 FORMAT (F5.1)
 9002 FORMAT (F5.0)
 9003 FORMAT (F8.3,", ",F7.3,", ")

!Now close off the folder, document:
      WRITE (unit,*) "  </Folder>"
      WRITE (unit,*) "</Document>"
      WRITE (unit,*) "</kml>"


      CLOSE (unit)

      RETURN
      END
