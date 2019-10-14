      SUBROUTINE ctofloat(ch, fl, nx, ny)
C     Convert a character array to a floating point array
C     Robert Grumbine
C     Last Modified 6 March 1996

      IMPLICIT none

      INTEGER i, j, nx, ny
      REAL fl(nx, ny)
      CHARACTER*1 ch(nx, ny)

      DO 1000 j = 1, ny
      DO 1000 i = 1, nx
        fl(i,j) = FLOAT( ICHAR(ch(i,j)) )
        IF (fl(i,j) .lt. 0.) THEN
          PRINT *,'lev 4 ',i,j,ch(i,j), ichar(ch(i,j))
        ENDIF
 1000 CONTINUE
CD      PRINT *,'floats = ',fl

      RETURN
      END
