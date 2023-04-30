      PROGRAM avg
      INTEGER navg
      INTEGER ndata
      PARAMETER (ndata = 6136*10)
      PARAMETER (navg = 4)
      DOUBLE PRECISION x(ndata)
      
      INTEGER i, j
      DOUBLE PRECISION acc

      OPEN (10, FILE="radius.filtered", FORM="FORMATTED", STATUS="OLD")

      acc = 0.0
      DO i = 1, ndata
        READ (10, *) x(i) 
        acc = acc + x(i)
        IF (MOD(i,4) .EQ. 0) THEN
          WRITE (*,*) acc/navg
          acc = 0
        ENDIF
      ENDDO
  
      STOP
      END
