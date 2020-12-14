      SUBROUTINE filtsst(sst, err, snx, sny, 
     1                   tshal, tdeep, sshal, sdeep, 
     2                   tml, sml, tmld, smld, nx, ny, weight, errunit)
C     Check the observed sst versus a climatological value.
C     Note that the sst and error grid is on 1 degree grid.
      IMPLICIT none

      INTEGER i, j, nx, ny, snx, sny, errunit
      REAL sst(snx, sny), err(snx, sny)
      REAL tshal(nx, ny), tdeep(nx, ny), sshal(nx, ny), sdeep(nx, ny)
      REAL tml(nx, ny), sml(nx, ny)
      REAL tmld(nx, ny), smld(nx, ny)
      INCLUDE "icegrid.inc"
      INTEGER lli, llj
      REAL llat, llon, weight, tfreez
      LOGICAL errout
      REAL delta1(LP, MP), delta2(LP, MP)

      DO 1000 j = 1, ny
        DO 1100 i = 1, nx
          tml(i,j)   = AMAX1(tfreez(sml(i,j)),   tml(i,j) )
          tmld(i,j)  = AMAX1(tfreez(smld(i,j)),  tmld(i,j) )
          tshal(i,j) = AMAX1(tfreez(sshal(i,j)), tshal(i,j) )
          tdeep(i,j) = AMAX1(tfreez(sdeep(i,j)), tdeep(i,j) )
          delta1(i,j) = 0.
          delta2(i,j) = 0.
 1100   CONTINUE
 1000 CONTINUE

      errout = .FALSE.

      DO 2000 j = 0, ny-1
        DO 2100 i = 0, nx-1
          CALL mapxy(i*dx+xorig, j*dy + yorig, llat, llon, 
     1               slat, slon, sgn, SQRT(eccen2), rearth)
          llj = INT(  (90. - llat + 0.5) + 0.5 )
          IF (llon .GE. 0.) THEN
            lli =  llon + 0.5
          ELSE
            lli =  (360. + llon) + 0.5
          ENDIF

          IF (ABS(-273.15 + sst(lli,llj) - tshal(i+1,j+1)) .GT. 10.0
     1       .AND. -273.15 + sst(lli, llj) .GT. -1.70
     2       .AND. tshal(i+1, j+1) .GT. -1.8 ) THEN
            WRITE (*,9001) llon, llat, -273.15 + sst(lli,llj), 
     1                            tshal(i+1,j+1)
            sst(lli,llj) = tshal(i+1,j+1) + 273.15
            errout = .TRUE.
          ENDIF
          delta1(i+1, j+1) = tshal(i+1,j+1) - sst(lli, llj)
          delta2(i+1, j+1) = tml(i+1, j+1) - sst(lli, llj)

          tml(i+1,j+1) = (1. - weight)*tml(i+1,j+1) + 
     1                   weight*(sst(lli, llj)-273.15)
 2100   CONTINUE
 2000 CONTINUE

      PRINT *,'Finished checking sst vs. climatology'

      IF (errout) THEN
        WRITE (errunit) delta1
        WRITE (errunit) delta2
        DO 8000 j = 1, ny
        DO 8000 i = 1, nx 
          delta1(i,j) = tshal(i,j) - tml(i,j)
 8000   CONTINUE
        WRITE (errunit) delta1
      ENDIF

 9001 FORMAT ('sst clim ',2F7.2, 3F9.2)

      RETURN
      END 
