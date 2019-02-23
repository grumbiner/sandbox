      PROGRAM beat
C     Program to generate the 'speed' of two random numbers.
C     Robert Grumbine 27 September 1994

      IMPLICIT none

      REAL omega1, omega2, omega3, omega4, pi
      INTEGER i, n
      REAL sum(100000)
      REAL t, s, time, lrand
      PARAMETER (omega1 = 12.421)
      PARAMETER (omega2 = 12.000)
      PARAMETER (omega3 = 354.00)
      PARAMETER (omega4 = 4368.0) 
      PARAMETER (pi     = 3.141592654)

      REAL freq(4), a(4), b(4)
      INTEGER m, type 
      CHARACTER*60 fname

C$EMA sum

      lrand(t) = AMOD( 25173.*t +13849., 65536.) / 65536.

      CALL srand(1)

      m = 4
      freq(1) = 2.*pi/omega1
      freq(2) = 2.*pi/omega2
      freq(3) = 2.*pi/omega3
      freq(4) = 2.*pi/omega4

      t = 3.

      PRINT *,'What type of randomness do you want?'
      READ (*,9002) type
      DO 1000 i = 1, 100000
       IF (type .EQ. 1) THEN
        time = FLOAT(i) - 1.
        sum(i) =   0.1 * sin(2.*pi*time/omega1) 
     *           + 0.1 * sin(2.*pi*time/omega2)
     1           + 0.1 * sin(2.*pi*time/omega3)
     2           + 0.1 * sin(2.*pi*time/omega4)
     3           + (rand()) -.5
       ELSE IF (type .EQ. 2) THEN
        sum(i) = lrand(t) - .5
        t      = sum(i)*65536.
       ELSE IF (type .EQ. 3) THEN
        sum(i) = (rand()) - .0
       ELSE IF (type .EQ. 4) THEN
        t = (rand()) 
        IF (t .LE. .0002) THEN
          sum(i) = 3.5
         ELSEIF ( t .LE. .0013) THEN
          sum(i) = 3.0
         ELSEIF ( t .LE. .0062) THEN
          sum(i) = 2.5
         ELSEIF ( t .LE. .0228) THEN
          sum(i) = 2.0 
         ELSEIF ( t .LE. .0668) THEN
          sum(i) = 1.5
         ELSEIF ( t .LE. .1587) THEN
          sum(i) = 1.0
         ELSEIF ( t .LE. .3087) THEN
          sum(i) =  .5
         ELSEIF ( t .LE. .6195) THEN
          sum(i) =  .0
         ELSEIF ( t .LE. .8143) THEN
          sum(i) =-0.5
         ELSEIF ( t .LE. .9332) THEN
          sum(i) =-1.0
         ELSEIF ( t .LE. .9772) THEN
          sum(i) =-1.5
         ELSEIF ( t .LE. .9938) THEN
          sum(i) =-2.0
         ELSEIF ( t .LE. .9987) THEN
          sum(i) =-2.5
         ELSEIF ( t .LE. .9998) THEN
          sum(i) =-3.0
         ELSE
          sum(i) =-3.5
        ENDIF
        ELSE IF (type .EQ. 5) THEN 
         t =  ((rand())**2.+(rand())**2.)**.5-.7
         s =  ((rand())**2.+(rand())**2.)**.5 -.7
         sum(i) = (t**2.+ s**2.)**.5
      ENDIF
 1000 CONTINUE

C     PRINT *,'calling harmonic analysis' 
C     CALL harmon(sum, 9000, freq, a, b, m)

C     WRITE (*,9001) (freq(i), a(i), b(i), i=1, m)
     
      PRINT *,'What do you want to call the file?'
      READ (*,9003) fname 
      OPEN (10, FILE=fname, FORM='UNFORMATTED', STATUS='NEW')
      WRITE (10) sum
      CLOSE (10, STATUS='KEEP')
      WRITE (*, 9001) sum

 9001 FORMAT (10F4.2)

 9002 FORMAT (I5)

 9003 FORMAT (A60)

      END
