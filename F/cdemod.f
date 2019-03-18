      PROGRAM cdemod
C     Program for complex demodulation:
C     Robert Grumbine 15 December 1994

      IMPLICIT none

      REAL x(10000), a(10000), b(10000)
      REAL r(10000), theta(10000)
      REAL freq, pi 
      INTEGER i, n

      PARAMETER (pi = 3.141592654)

      PRINT *,'How many data points are there?'
      READ (*,9001) n
      CALL readin(x, n, 10)
     
      PRINT *, 'What frequency would you like to use?'
      READ (*,9002) freq
 
      DO 1000 i = 1, n
        a(i) = x(i) * sin(freq*FLOAT(i/n)*2.*pi)
        b(i) = x(i) * cos(freq*FLOAT(i/n)*2.*pi)
        r(i) = (a(i)**2 + b(i)**2)**.5
        theta(i) = ATAN2(a(i),b(i))
 1000 CONTINUE

      PRINT *,'What would you like to call the amplitude file?'
      CALL ritout(r, n, 11)
      PRINT *,'What would you like to call the phase file?'
      CALL ritout(theta, n, 12)

 9001 FORMAT (I4)

 9002 FORMAT (E14.6)

      END
