      PROGRAM distance
      IMPLICIT none
      REAL arcdis
      REAL lat1, lon1, lat2, lon2, lat3, lon3
      REAL d12, d13, d23


      DO WHILE (1 .EQ. 1)
        READ (*,*) lat1, lon1, lat2, lon2, lat3, lon3
        d12 = arcdis(lon1, lat1, lon2, lat2)
        d13 = arcdis(lon1, lat1, lon3, lat3)
        d23 = arcdis(lon2, lat2, lon3, lat3)
        WRITE (*,*) d12, d13, d23
      ENDDO

      END
