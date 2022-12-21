      SUBROUTINE ucdiff(uref, vref, ahm, f, beta, delx, dely)
C     Subroutine to iteratively implement diffusion in the baroclinic
C       flow field, initially determined by thermal wind relation.

      IMPLICIT none
      INCLUDE "grid.inc"
      REAL uref(nx, ny), vref(nx, ny)
      REAL ahm, f, beta, delx, dely

      REAL ua(nx, ny), va(nx, ny)
      REAL rmsref, rmsa
      REAL uppe, ae, be, compue, uppw, aw, bw, compuw, lambda
      INTEGER i, j

      CALL ARSET(ua, nx, ny, 0.0)

C     Boundary Layers
      DO 1000 j = 2, ny-1
        lambda = 1./SQRT(ABS(ahm/2./(f+beta*dely*FLOAT(ny/2-j))))
        uppw = (uref(3,j)-2.*uref(2,j)+uref(1,j))/delx/delx
        aw   = -uref(1,j)
        bw   =  uref(1,j)+uppw/lambda/lambda
        uppe = (uref(nx,j)-2.*uref(nx-1,j)+uref(nx-2,j))/delx/delx
        ae   = -uref(nx,j)
        be   =  uref(nx,j)+uppe/lambda/lambda
        DO 1100 i = 1, nx
          compuw = lambda*delx*FLOAT(i-1)
          compue = lambda*delx*FLOAT(nx-i)
          ua(i,j) = exp(-compuw) * (aw*cos(compuw)+bw*sin(compuw))
     1            + exp(-compue) * (ae*cos(compue)+be*sin(compue))
 1100   CONTINUE
 1000 CONTINUE

      rmsref = 0.0
      rmsa   = 0.0
      DO 1300 j = 2, ny-1
        DO 1310 i = 2, nx-1
          rmsref = rmsref + uref(i,j)*uref(i,j) + vref(i,j)*vref(i,j)
          rmsa = rmsa + ua(i,j)*ua(i,j)
 1310   CONTINUE
 1300 CONTINUE
      rmsref = SQRT(rmsref)/FLOAT((nx-2)*(ny-2))
      rmsa   = SQRT(rmsa)  /FLOAT((nx-2)*(ny-2))
      IF (rmsref .NE. 0.0) THEN
        WRITE (*,9001) 0, rmsa, rmsref, rmsa/rmsref
        WRITE (1,9001) 0, rmsa, rmsref, rmsa/rmsref
CD        PRINT *, aw, bw, uppw, lambda, compuw
       ELSE
        WRITE (*,9001) 0, rmsa, rmsref
      ENDIF

      DO 5000 j = 2, ny-1
        DO 5100 i = 1, nx
          uref(i,j) = ua(i,j)+uref(i,j)
 5100   CONTINUE
 5000 CONTINUE

 9001 FORMAT (I3,4E15.6)

      RETURN
      END
