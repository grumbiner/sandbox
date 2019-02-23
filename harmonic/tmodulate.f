      PROGRAM modul
C     Program to give examples of tidal modulation in 'unresolved' 
C       harmonics.
C     Uses the notation of GWP notes for tides 381 class.
C     Robert Grumbine 27 September 1994

      IMPLICIT NONE

      REAL pi, omega1, omega2, omega3, omega4 
      PARAMETER (pi     = 3.1415927E+0)
      PARAMETER (omega1 = 1.5250452E-3*2.*pi)
      PARAMETER (omega2 = 1.1407960E-4*2.*pi)
      PARAMETER (omega3 = 1.2894721E-5*2.*pi)
      PARAMETER (omega4 = 6.1290280E-6*2.*pi)

      REAL gamma, f(10000), u(10000) 
      REAL domega
      INTEGER m1, m2, m3, m4
      INTEGER i, n
     
      PRINT *,'What is the ratio of the equilibrium amplitudes?'
      READ (*,9001) gamma
      PRINT *,'Please enter the modulation vector, one number per line'
      READ (*,9002) m1
      READ (*,9002) m2
      READ (*,9002) m3
      READ (*,9002) m4
      domega = m1*omega1 + m2*omega2 + m3*omega3 + m4*omega4
      PRINT *,'The synodic period is ',2.*pi/domega,' hours'
      PRINT *,'How many hours of results would you like?'
      READ (*,9002) n

      DO 1000 i = 1, n
        f(i) = (1. + 2.*gamma*cos(domega*FLOAT(i)) + gamma**2) **.5
        u(i) = ASIN( (gamma/f(i)) * sin(domega*FLOAT(i)) )
 1000 CONTINUE

      PRINT *,'What would you like to call the f file?'
      CALL ritout(f, n, 11)
      PRINT *,'What would you like to call the u file?'
      CALL ritout(u, n, 11)

 9001 FORMAT (E13.6)

 9002 FORMAT (I4)
 
      END
