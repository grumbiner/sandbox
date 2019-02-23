      PROGRAM dummy
! Set up a dummy analysis and data file for testing scaling of 
!   Cressman-like program
      IMPLICIT none

      INTEGER nx, ny
      PARAMETER (nx = 360*2)
      PARAMETER (ny = 180*2)
      REAL first(nx, ny)

      INTEGER i, maxobs
      PARAMETER (maxobs = 1000*1000*10)
      REAL lat(maxobs), lon(maxobs), obs(maxobs)

      REAL rgrand, ran2

      OPEN (10, FILE="first", FORM="UNFORMATTED", STATUS="UNKNOWN")
      WRITE (10) first
      CLOSE (10)

      lat = 0.0
      lon = 0.0
      obs = 0.0
      OPEN (11, FILE="obs",FORM="UNFORMATTED", STATUS="UNKNOWN")
      DO i = 1, maxobs
        lat(i) = rgrand(-90.0, 90.0)
        lon(i) = rgrand(0.0,  360.0)
        obs(i) = rgrand(-5.0,  5.0)
        WRITE (11) lat(i), lon(i), obs(i)
      ENDDO
      PRINT *,'lat ', maxval(lat), minval(lat)
      PRINT *,'lon ', maxval(lon), minval(lon)
      PRINT *,'obs ', maxval(obs), minval(obs)

      CLOSE(11)

      STOP
      END
      REAL FUNCTION rgrand(low, high)
!  Pseudo-random number generator to get uniformly distributed between
!    low and high bound
      IMPLICIT none
      REAL ran2, low, high, rand, x
      INTEGER seed
      SAVE seed

      x = ran2(seed)
      rgrand = low + (high-low)*x
      !PRINT *,x, low, high, rgrand
      RETURN
      END

      REAL FUNCTION ran2(IDUM)
C     Single linear congruential random number generator.
C     From Numerical Recipes.
C     Bob Grumbine 7 April 1994.

      IMPLICIT none

      INTEGER IDUM
      INTEGER M, IA, IC
      PARAMETER (M = 714025)
      PARAMETER (IA = 1366)
      PARAMETER (IC = 150899)
      REAL RM
      PARAMETER (RM = 1./M)
      INTEGER J, IFF, IY

      INTEGER IR(97)
C     The save statement is necessary, as Num. Recipes assumes
C       that variables are saved between calls, which they
C       aren't on all machines.  BG 1/22/94.
      SAVE

      DATA IFF /0/

      IF (IDUM .LT. 0 .OR. IFF .EQ. 0) THEN
        IFF = 1
        IDUM = MOD(IC-IDUM,M)
        DO 11 J = 1, 97
          IDUM = MOD(IA*IDUM+IC, M)
          IR(J) = IDUM
   11   CONTINUE
        IDUM = MOD(IA*IDUM+IC, M)
        IY = IDUM
      ENDIF

      J = 1 + (97*IY)/M
      IF (J .GT. 97 .OR. J .LT. 1) THEN
        PRINT *,'Error on J in RAN2.  J = ',J
        STOP
      ENDIF
      IY = IR(J)
      ran2 = IY*RM
      IDUM=MOD(IA*IDUM+IC,M)
      IR(J) = IDUM


      RETURN
      END
