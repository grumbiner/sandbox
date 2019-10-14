      PROGRAM struc
C     Test of derived types in fortran
      TYPE buoy
         CHARACTER*80 name
         REAL lat, lon
         INTEGER time
         REAL slp, sst, wind
      END TYPE BUOY
      TYPE (BUOY) list(5)

      INTEGER i, c

      DO i = 1, 5
        list(i)%time = i
      ENDDO

      DO i = 1, 5
        PRINT *,'i, time ', i, list(i)%time 
      ENDDO

      i = c(list)

      END 
