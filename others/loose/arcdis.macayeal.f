      FUNCTION arcdis (lat1, long1, lat2, long2)
C     Function to compute the distance between two latitude, longitude
C       points in kilometers.
C     By Doug MacAyeal.

      REAL ab, ac, bc, pi, arcdis
      REAL lat1, long1, lat2, long2

      pi =3.1415927
      ab=((90.-lat1)*pi)/180.
      ac=((90.-lat2)*pi)/180.
      bc=abs(((long1-long2)*pi)/180.)
      arcdis=ACOS(COS(ab)*COS(ac)+SIN(ab)*SIN(ac)*COS(bc))
     1   *6378.245

      RETURN
      END
