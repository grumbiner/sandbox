      SUBROUTINE filtconc(conc, nx, ny, sst, err, snx, sny, tml, tmlmax)
C     Apply sst filter to the concentrations.  Should be redundant, 
C       but no need to take the chance.
C     Robert Grumbine
C     Last Modified 26 April 1996

      IMPLICIT none

      INTEGER i, j, nx, ny, snx, sny
      REAL conc(nx, ny), tml(nx, ny)
      REAL sst(snx, sny), err(snx, sny)
      REAL tmlmax
     
      DO 1000 j = 1, ny
      DO 1000 i = 1, nx
        IF (tml(i,j) .GT. tmlmax .AND. conc(i,j) .NE. 0. ) THEN
CD          IF (conc(i,j) .LT. 128./100.) THEN
CD            WRITE (*,9001) i, j, tml(i,j), conc(i,j)
CD          ENDIF
          conc(i,j) = 0.0
        ENDIF
 1000 CONTINUE
 9001 FORMAT ('fconc ', 2I5, 2F9.2)
    
      RETURN
      END 
