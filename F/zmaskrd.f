      SUBROUTINE zmaskrd(zmask, nnx, nny, unit)
C     Read in the 1 degree by 1 degree mask of Joe Sela's.
C     Bob Grumbine 2 June 1994.

      IMPLICIT none

      INTEGER nx, ny
      INTEGER nnx, nny, unit
      PARAMETER (nx = 360)
      PARAMETER (ny = 181)
      REAL zmask(nx, ny)

      IF (nx .NE. nnx) THEN
        PRINT *,'Dimension mismatch in nx ',nx, nnx
        PRINT *,'Dimension mismatch? in ny ',ny, nny
        STOP
      ENDIF
      IF (ny .NE. nny) THEN
        PRINT *,'Dimension mismatch in ny ',ny, nny
        STOP
      ENDIF

C     Requires that zmask be appropriately attached/assigned elsewhere.
C     100 = ocean, 0 = land?

      READ (unit) zmask

      RETURN
      END
