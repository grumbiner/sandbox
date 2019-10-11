      SUBROUTINE adv2xy(UT, VT, SS, QS, DELX, DELY, DELT,
     1                  TSTEP)
C     SUBROUTINE TO Compute simple advection to the next step.
      IMPLICIT none
      INCLUDE "grid.inc"
      
      INTEGER  TSTEP
      REAL UT(nx, ny), VT(nx, ny)
      REAL QS(nx, ny), SS(nx, ny)
      REAL DELX, DELY, DELT, DREF, SREF

      REAL FSS(nx, ny), FSSTAR(nx, ny)
      REAL DX2, DY2, DXTDX, DYTDY

      INTEGER I, J, K, L

      DX2   = DELX+DELX
      DY2   = DELY+DELY
      DXTDX = DELX*DELX
      DYTDY = DELY*DELY

      CALL arset(FSS, nx, ny, 0.0)
      CALL arset(FSSTAR, nx, ny, 0.0)

C     Use an Euler Backward scheme.
      DO 1000 J = 2, ny-1
        DO 1010 I = 2, nx-1

          FSS(I,J) =
     1     - (  (UT(I+1,J)*SS(I+1,J)-UT(I-1,J)*SS(I-1,J))
     2        + (VT(I,J+1)*SS(I,J+1)-VT(I,J-1)*SS(I,J-1))  )/DX2

          FSS(I,J) = SS(I,J)+DELT*FSS(I,J)

 1010   CONTINUE
 1000 CONTINUE

      DO 2000 J = 2, ny-1
        DO 2100 I = 2, nx-1

             FSSTAR(I,J) =
     1     - (  (UT(I+1,J)*FSS(I+1,J)-UT(I-1,J)*FSS(I-1,J))
     2        + (VT(I,J+1)*FSS(I,J+1)-VT(I,J-1)*FSS(I,J-1))  )/DX2

 2100   CONTINUE
 2000 CONTINUE

C     EXTRAPOLATE THE INTERIOR VALUES:
      DO 3000 J = 2, ny-1
        DO 3010 I = 2, nx-1
          SS(I,J) = SS(I,J) + DELT*FSSTAR(I,J)
 3010   CONTINUE
 3000 CONTINUE

C     BC:
      DO 4000 I = 2, nx-1
        SS(I,1)  = SS(I,2)
        SS(I,ny) = SS(I,ny-1)
 4000 CONTINUE
      DO 4010 J = 2, ny-1
        SS(nx,J) = SS(nx-1,J)
        SS(1,J)  = SS(2,J)
 4010 CONTINUE 
C     Note that strictly speaking the corner points are irrelevant.
      SS(1,1)   = SS(2,2)
      SS(1,ny)  = SS(2,ny-1)
      SS(nx,1)  = SS(nx-1,2)
      SS(nx,ny) = SS(nx-1, ny-1)

      DO 5000 J = 1, ny
        DO 5100 I = 1, nx
          IF (SS(I,J) .LE. 0.0) THEN
            SS(I,J) = 0.0
          ENDIF
          IF (SS(I,J) .GT. 1.0) THEN
            SS(I,J) = 1.0
          ENDIF
 5100   CONTINUE
 5000 CONTINUE
 9001 FORMAT (18F5.2)

      RETURN
      END
