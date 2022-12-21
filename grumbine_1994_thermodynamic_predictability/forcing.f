      SUBROUTINE forcing(ta, swdown, lwd, lwu, rh, fw, unitm, unitfw)
C     Read from the meteorology file, and the ocean flux file,
C       various variables for sea ice thermodynamics.  
C     Currently, air temperature, downwelling shortwave, ocean flux,
C       and atmospheric water content.
C     Robert Grumbine 15 September 1994.

      INCLUDE "icegrid.inc"
      INCLUDE "physical.inc"

      REAL ta(0:L, 0:M), swdown(0:L, 0:M), fw(0:L, 0:M), rh(0:L, 0:M)
      REAL tsfc(0:L, 0:M), mask(0:L, 0:M), fwice(0:L, 0:M)
      REAL lwd(0:L, 0:M), lwu(0:L, 0:M)
      REAL tmps(0:L, 0:M), uwin(L, M), vwin(L, M)
      INTEGER unitm, unitfw
      REAL fwguess
      fwguess(i,j) = RHOWAT*4.E3*CSENS*
     1           3.E-2*AMAX1(2., SQRT(uwin(i,j)**2+vwin(i,j)**2) )
     1               * (ta(i,j) + 1.84 - TMELT )


C     Use brute force reading.

C     This is the listing in order of the variables currently in the
C       meteorology file.

      READ (unitm) ta
      READ (unitm) tmps
      READ (unitm) rh
      READ (unitm) tsfc
      READ (unitm) swdown
      READ (unitm) lwd
      READ (unitm) lwu
      READ (unitm) precip
      READ (unitm) mask
      READ (unitm) uwin
      READ (unitm) vwin

CD      READ (unitfw) fwice
      CALL arset(fwice, LP, MP, 0.0) 
      DO 1000 j = 0, M
        DO 1100 i = 0, L

          ta(i,j) = ta(i,j) + TMELT
          tsfc(i,j) = tsfc(i,j) + TMELT

          IF (mask(i,j) .LT. 1.5) THEN
            fw(i,j) = fwguess(i,j)
           ELSE
            fw(i,j) = fwice(i,j)
          ENDIF

          IF (tsfc(i,j) .GT. TMELT + 10.0 ) THEN
            fw(i,j) = rhoice * vapl * 1. / 86400.  
          ENDIF

 1100   CONTINUE
 1000 CONTINUE

      RETURN
      END
