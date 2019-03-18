      LOGICAL FUNCTION noice(A, nx, ny, ns, lstep)
C     Function returns true if there is some ice on the indicated
C       (lstep) time step, false otherwise.  Useful for avoiding
C       computing ice velocities when there is no ice.
C     Robert Grumbine 15 October 1996.

      IMPLICIT none

      INTEGER nx, ny, ns, lstep
      REAL A(nx, ny, ns)
      INTEGER i, j
      LOGICAL temp

      temp = .TRUE.
      DO 1000 j = 1, ny
        DO 1000 i = 1, nx
          IF (A(i,j,lstep) .NE. 0) THEN
            noice=.FALSE.
            RETURN
          ENDIF
 1000   CONTINUE

      noice = temp
      RETURN
      END
