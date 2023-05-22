C***********************************************************----------!!
      SUBROUTINE UVEXT (UC, VC, SS, H,
     1                  RHOREF, G, F, beta, ahm, delx, dely)
C     EXTRAPOLATE U, V TO THE NEXT TIME LEVEL
C     COMPUTATION OF COMMON CONSTANTS ADDED PRIOR TO 5-26-88.
C     VERSION REWRITTEN FOR GEOSTROPHY, A LA DERIVATION. 4-5-89.
C       MUCH COMMENTED PROGRAM DELETED 4-5-89.
C     Unused variables deleted 7-16-90.
C     Per reviewer notes, overspecified boundary conditions removed.--
C       Note though that the bc. on SS effectively produce the same
C       result.  7-16-90.

      IMPLICIT none
      INCLUDE "grid.inc"
      REAL UC(nx, ny), VC(nx, ny)
      REAL SS(nx, ny)
      REAL H(nx, ny)
      REAL RHOREF, F, G
      REAL delx, dely
      REAL beta, ahm

      REAL RHOS1P, RHOS(nx, ny)
      INTEGER I, J

C     PARAMS FOR SPEEDIER NUMERICS
      REAL DX2, DY2, G8RREF, bcorr

C     COMPUTE PARAMS FOR SPEEDIER NUMERICS
      DX2    = 2.*delx
      DY2    = 2.*dely
      G8RREF = G*H(1,1)/4./RHOREF/F

C     COMPUTE THE DENSITY FIELD BEFORE ENTERING THE EXTRAPOLATION.
C     THIS REDUCES THE NUMBER OF CALLS TO THE DENSITY FUNCTION BY
C       ALMOST A FACTOR OF 4.  8-4-88.
      DO 900 J = 1, ny
        DO 910 I = 1, nx
            RHOS(I,J) = RHOS1P( SS(I,J), 0.0, 0.0)
  910   CONTINUE
  900 CONTINUE

C     COMPUTE THE GEOSTROPHIC VELOCITY (Thermal Wind Relation)
      DO 1000 J = 2, ny-1
        DO 1010 I = 2, nx-1

          UC(I,J) = +G8RREF*( RHOS(I,J+1) - RHOS(I,J-1) )/DY2
          VC(I,J) = -G8RREF*( RHOS(I+1,J) - RHOS(I-1,J) )/DX2

 1010   CONTINUE
 1000 CONTINUE

C     Now compute the free slip velocities along boundaries.
      J = 1
      DO 1100 I = 2, nx-1
        UC(I,J) = +G8RREF*( RHOS(I,J+1) - RHOS(I,J) )/dely
        VC(I,J) = -G8RREF*( RHOS(I+1,J) - RHOS(I-1,J) )/DX2
 1100 CONTINUE
      J = ny
      DO 1200 I = 2, nx-1
        UC(I,J) = +G8RREF*( RHOS(I,J) - RHOS(I,J-1) )/dely
        VC(I,J) = -G8RREF*( RHOS(I+1,J) - RHOS(I-1,J) )/DX2
 1200 CONTINUE
      I = 1
      DO 1300 J = 2, ny-1
        UC(I,J) = +G8RREF*( RHOS(I,J+1) - RHOS(I,J-1) )/DY2
        VC(I,J) = -G8RREF*( RHOS(I+1,J) - RHOS(I,J) )/delx
 1300 CONTINUE
      I = nx
      DO 1400 J = 2, ny-1
        UC(I,J) = +G8RREF*( RHOS(I,J+1) - RHOS(I,J-1) )/DY2
        VC(I,J) = -G8RREF*( RHOS(I,J) - RHOS(I-1,J) )/delx
 1400 CONTINUE
      UC(1,1)   = +G8RREF*( RHOS(1 ,2)  - RHOS(1 ,1) )/dely
      UC(1,ny)  = +G8RREF*( RHOS(1 ,ny) - RHOS(1 ,ny-1) )/dely
      UC(nx,1)  = +G8RREF*( RHOS(nx,2)  - RHOS(nx,1) )/dely
      UC(nx,ny) = +G8RREF*( RHOS(nx,ny) - RHOS(nx,ny-1) )/dely
      VC(1,1)   = -G8RREF*( RHOS(2 ,1)  - RHOS(1 ,1) )/delx
      VC(1,ny)  = -G8RREF*( RHOS(2 ,ny) - RHOS(1 ,ny) )/delx
      VC(nx,1)  = -G8RREF*( RHOS(nx,1)  - RHOS(nx-1,1) )/delx
      VC(nx,ny) = -G8RREF*( RHOS(nx,ny) - RHOS(nx-1,ny) )/delx

C     Apply the beta correction to the velocities.
      DO 2000 J = 1, ny
        bcorr = 1. + beta*FLOAT(ny/2-J)*dely/F
        DO 2100 I = 1, nx
          UC(I,J) = UC(I,J)/bcorr
          VC(I,J) = VC(I,J)/bcorr
 2100   CONTINUE
 2000 CONTINUE

C     NOW CONSIDER THE BOUNDARY CONDITIONS
C       7-16-90
C         AT I = 1         U = 0.0
C         AT I = nx        U = 0.0
C         AT J = 1         V = 0.0
C       8-2  J = ny        V = 0.0
      DO 3000 I = 1, nx
C       BC ON V AT THE Y BOUNDARIES
        VC(I,1)  = 0.0
        VC(I,ny) = 0.0
 3000 CONTINUE

      DO 3010 J = 2, ny-1
C       BC on U at the x boundaries
C       Interior solution, no knowledge of the boundary conditions
        UC(1,  J) = +G8RREF*( RHOS(1 ,J+1) - RHOS(1 ,J-1) )/dely
        UC(nx, J) = +G8RREF*( RHOS(nx,J+1) - RHOS(nx,J-1) )/dely
 3010 CONTINUE

C     Compute the diffusive correction to the solution.
C       BG 1/10/91.
      PRINT *,'Calling ucdiff'
      CALL ucdiff(UC, VC, ahm, F, beta, delx, dely)

      RETURN
      END
