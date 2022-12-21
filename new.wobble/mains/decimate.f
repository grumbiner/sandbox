      PROGRAM decimate
      INTEGER nin, nout, rate
      PARAMETER (rate = 10)
      PARAMETER (nin = 61364)
      PARAMETER (nout = nin / rate)
      REAL xin(nin), xout(nout)

      INTEGER i, j, k

      OPEN(10, FILE="selected", FORM="FORMATTED", STATUS="OLD")
      OPEN(11, FILE="decimated.out", FORM="FORMATTED", STATUS="NEW")
      DO i = 1, nin
        READ(10,*) xin(i)
      ENDDO
   
      DO i = 0, nout-1
        xout(i+1) = 0.0
        DO j = 0, rate-1
          xout(i+1) = xout(i+1) + xin(i*rate + j + 1)
        ENDDO
        xout(i+1) = xout(i+1)/rate
        WRITE (11, *) xout(i+1)
      ENDDO

      STOP
      END
