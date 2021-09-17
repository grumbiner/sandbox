      SUBROUTINE imprep(x, fout, nx, ny, xlo, xhi)
C     Prepare a character array for raster display from real 2d input.
C     Bob Grumbine 6 April 1994.
C     LAST MODIFIED 16 August 1994


      IMPLICIT none

      INTEGER nx, ny
      REAL x(nx, ny)
      CHARACTER*1 fout(nx,ny)
      REAL xlo, xhi

      INTEGER i, j

      DO 1000 j = 1, ny
        DO 1010 i = 1, nx
          fout(i,j) = CHAR(INT( (x(i,j)-xlo)/(xhi-xlo)*255.)  )
 1010   CONTINUE
 1000 CONTINUE

      RETURN
      END
