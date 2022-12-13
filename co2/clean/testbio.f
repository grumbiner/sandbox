
      PROGRAM tstbio
C     Test conservation and equality in the biological source/sink
C       routine.  12/8/91.

      IMPLICIT none

      INTEGER nlayer, pchem
      PARAMETER (nlayer = 76)
      PARAMETER (pchem = 13)

      DOUBLE PRECISION x(nlayer), q(nlayer)
      DOUBLE PRECISION deltaz, deltat, produc

      INTEGER i, j
      DOUBLE PRECISION a, b

      DO 1000 i = 1, nlayer
        x(i) = 0.0
        q(i) = 0.0
 1000 CONTINUE

      deltaz = 50.0
      produc = 50.0
      deltat = 50.0
      CALL biosrc(pchem, x, q, nlayer, deltaz, deltat, produc)

      a = 0.0
      b = 0.0
      DO 2000 i = 1, nlayer
        a = a + q(i)
        b = b + x(i)
 2000 CONTINUE
      PRINT *, a, b, produc

      END
