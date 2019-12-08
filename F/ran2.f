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
