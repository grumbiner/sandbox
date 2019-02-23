      PROGRAM beat

C     Program to generate the sum of two sinusoids.
C     Robert Grumbine 27 September 1994

      IMPLICIT none

      REAL omega1, omega2, omega3, omega4, pi
      INTEGER i, n
      REAL sum(9000), time(9000)
      DOUBLE PRECISION drand48

      PARAMETER (omega1 =   25.0000)
      PARAMETER (omega2 =   12.0000)
      PARAMETER (omega3 =  354.367 )
      PARAMETER (omega4 = 4382.905 ) 
      PARAMETER (pi     = 3.141592654)

      REAL freq(4), a(4), b(4)
      REAL t
      INTEGER m

      m = 4
      freq(1) = 2.*pi/omega1
      freq(2) = 2.*pi/omega2
      freq(3) = 2.*pi/omega3
      freq(4) = 2.*pi/omega4

      t = .0
      DO 1000 i = 1, 9000
        t = t + SNGL(drand48()) 
        time(i) = t
        sum(i)  =  0.0 
     1           + 2.5  * sin(2.*pi*t/omega1) 
     2           + 5.0  * sin(2.*pi*t/omega2)
C    3           + 0.4 * sin(2.*pi*t/omega3)
C    4           + 0.2 * sin(2.*pi*t/omega4)
C    5           + SNGL(drand48()) 
 1000 CONTINUE

C     PRINT *,'calling harmonic analysis' 
C     CALL harmon(sum, 9000, freq, a, b, m)

C     WRITE (*,9001) (freq(i), a(i), b(i), i=1, m)
 9001 FORMAT (3e15.7)
      
      OPEN (10, FILE='btest', FORM='UNFORMATTED', STATUS='NEW')
      WRITE (10) sum
      CLOSE (10, STATUS='KEEP')

      OPEN (10, FILE='time', FORM='UNFORMATTED', STATUS='NEW')
      WRITE (10) time
      CLOSE (10, STATUS='KEEP')

      END
