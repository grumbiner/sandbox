C*************************************************----------++++++++++!!
      SUBROUTINE utrop2(psi, we, delx, ahm, beta, f, href)
      IMPLICIT none
      INCLUDE "grid.inc"
      REAL psi(nx, ny), we(nx, ny)
      REAL delx, ahm, beta, f, href

      INTEGER k
      PARAMETER (k = 2)
      DOUBLE PRECISION a(nx,-k:k), b(nx), w(nx), piv(nx, -k:-1)

      INTEGER i, j
      DOUBLE PRECISION gamma, delta

C     This is the portion where the solution is found.
C     Now initialize the matrices for matsol.  Aside - b not used here
CT      PRINT *,'Start time ',LONG(362)
      DO 2000 j = -k, k
        DO 2010 i = 1, nx
          a(i,j) = 0.D0
 2010   CONTINUE
 2000 CONTINUE
C     Now compute the constants used in filling a
      gamma = -DBLE(ahm/beta/(delx**4))
      delta =  1.D0/DBLE(delx)/12.D0
C     Changed to a forward difference in beta 7-18-90.
C     Interior portion of the grid.
      DO 2100 i = 1+k, nx-k
        a(i,-2) =  gamma+delta
        a(i,-1) = -4.D0*gamma-8.D0*delta
        a(i, 0) =  6.D0*gamma
        a(i, 1) = -4.D0*gamma+8.D0*delta
        a(i, 2) =  gamma-delta
 2100 CONTINUE
C     Now apply boundary condition at i = 1, stream fn. = 0 and
C       normal grad = 0.  (psi = 0 and no slip.)
      a(1, 0)  =  1.D0
      a(2,-1)  = -1.D0
      a(2, 0)  =  1.D0
C     Apply the same conditions at i = nx
      a(nx, 0)   =  1.D0
      a(nx-1, 0) = -1.D0
      a(nx-1, 1) =  1.D0
C     Note that must have b(1) = b(nx) = 0.0 also.
C     Now make the de-composition. Piv is used within matsol only
      CALL matsol(k, nx, a, b, piv)
CT      PRINT *,'Finished matrix fill and invert',LONG(362)
C     Ready now to start loading up the forcing and retrieving the soln.
      j = 1
      DO 3210 i = 1, nx
        psi(i,j) = 0.0
 3210 CONTINUE
      DO 3000 j = 2, ny
        b(1)    = 0.D0
        b(2)    = 0.D0
        b(nx)   = 0.D0
        b(nx-1) = 0.D0
        DO 3100 i = 3, nx-2
          b(i) = DBLE(we(i,j)*f/href/beta)
 3100   CONTINUE
        CALL BAKSOL(k, nx, a, b, w)
        DO 3200 i = 1, nx
          psi(i,j) = SNGL(b(i))
 3200   CONTINUE
 3000 CONTINUE
CT      PRINT *,'Finished finding psi',LONG(362)

      RETURN
      END
