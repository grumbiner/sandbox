      SUBROUTINE filt(x, n, filtno)
C     Subroutine to filter data using various types of filter.
C     Robert Grumbine 1987

      INTEGER n, filtno
      REAL x(n)

      INTEGER i, m
      REAL pi, percen
      PARAMETER (pi = 3.141592654)

C     Select the filter:
      IF (filtno .EQ. 1) THEN
C       Hanned filter (Bloomfield, p. 84)
        DO 1000 i = 1, n
          x(i) = x(i)*(.5 - .5*cos(2.*pi*(FLOAT(i)-.5)/FLOAT(n)))
 1000   CONTINUE

       ELSE IF (filtno .EQ. 2) THEN
C       Split cosine bell filter.
        PRINT *,'What percentage of the data do you want tapered?'
        READ (*,9001) percen
        m = INT(percen*n)
        PRINT *,'m=', m
        DO 2000 i = 1, m
          x(i) = x(i)*.5*(1.-cos(pi*(FLOAT(i)-1.5)/FLOAT(m)) ) 
 2000   CONTINUE
        DO 2010 i = n-m+1, n
          x(i) = x(i)*.5*(1.-cos(pi*(FLOAT(n)-FLOAT(i)-.5)/FLOAT(m)) )
 2010   CONTINUE

       ELSE
        PRINT *,'filtno out of range'

      ENDIF

 9001 FORMAT (E14.6)
      
      RETURN
      END
