      PROGRAM decimate
      INTEGER nin, nout, rate
      PARAMETER (rate = 10)
      PARAMETER (nin = 61364)
      PARAMETER (nout = nin / rate)
      INTEGER nx, ny
      PARAMETER (nx = 144)
      PARAMETER (ny =  73)
      REAL x(nx, ny), avg(nx, ny)

      INTEGER i, j, k

      OPEN(10, FILE="bin.all", FORM="UNFORMATTED", STATUS="OLD")
      OPEN(11, FILE="bin.decimated", FORM="UNFORMATTED", STATUS="NEW")
   
      DO i = 1, nout
        DO l = 1, ny
        DO k = 1, nx
          avg(k,l) = 0.0
        ENDDO
        ENDDO

        DO j = 1, rate
          READ (10) x

          DO l = 1, ny
          DO k = 1, nx
            avg(k,l) = avg(k,l) + x(k,l)
          ENDDO
          ENDDO
        ENDDO

        DO l = 1, ny
        DO k = 1, nx
          avg(k,l) = avg(k,l) / rate
        ENDDO
        ENDDO

        WRITE (11) avg
      ENDDO

      STOP
      END
