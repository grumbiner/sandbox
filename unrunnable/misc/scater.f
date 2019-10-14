C***********************************************************----------!!
      SUBROUTINE scater(x, y, n)
C     Subroutine to make a scatter plot.
C     Bob Grumbine 6-25-86.

      INTEGER n
      REAL x(n), y(n)
	CHARACTER*4 ch

      INTEGER i
C     Get hold of the MacIntosh drawing routines
      INCLUDE HD.I40:MacFortran020:Include Files:quickdraw.inc
      INTEGER toolbx

C      ch ='   *'
C     Plot marks.
      DO 1000 i = 1, n
        CALL toolbx(MOVETO, INT(x(i)),INT(y(i)))
	  CALL toolbx(DRAWCHAR,'   *')
 1000 CONTINUE

      RETURN
      END
