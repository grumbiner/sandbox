      PROGRAM f0
C     Compute the potential sea ice freezing rate, for ice
C      of minimum thickness only.
C     Robert Grumbine 14 September 1994.
C     Compute the potential freezing rate for ice in the thinnest 
C       category only, and heating/cooling rate for the ocean
C     Required input is unit 11 with Ta, SWdown, LWdown, LWup, Relative 
C       Humidity, FW - ua is considered known.
C     Output is to unit 12, freezing rate in meters per day or cooling 
C       rate in degrees per day Computed at each model grid point
C     Future: ua should be a grid read in from forcing.

      IMPLICIT none

      INCLUDE "icegrid.inc"
      REAL hmin, delh
      PARAMETER (hmin = 0.10)
      PARAMETER (delh = 0.0 )

      REAL hs, hi
 
      REAL tf, ks, ki, ei, alpha, rhoi, rhoa, lf, lv, c
      REAL cdq, cd, ua, cp, sigma, taus, taui, i0

      REAL ta(0:L, 0:M), swdown(0:L, 0:M), fw(0:L, 0:M), rh(0:L, 0:M)
      REAL oqextra(0:L, 0:M), qextra(0:L, 0:M), temp(0:L, 0:M)
      REAL lwd(0:L, 0:M), lwu(0:L, 0:M)
      REAL grow(0:L, 0:M), ogrow(0:L, 0:M)
      REAL avger(0:L, 0:M), oavg(0:L, 0:M)

      INTEGER nstep, i, j, k

      CALL init( nstep, 
     1  tf, ks, ki, ei, alpha, rhoi, rhoa, lf, lv, c,
     2  cdq, cd, ua, cp, sigma, taus, taui, i0  )

      hs = 0.00
 
      DO 100 j = 0, M
        DO 110 i = 0, L
          temp(i,j) = tf
          grow(i,j) = 0.0
          ogrow(i,j) = 0.0
          avger(i,j) = 0.0
          oavg(i,j) = 0.0
  110   CONTINUE
  100 CONTINUE

      DO 1000 i = 1, nstep
        PRINT *,'step ',i
        REWIND (11)    
        CALL forcing(ta, swdown, lwd, lwu, rh, fw, 30+i, 11)

        CALL temper(
     1    ta, tf, ks, ki, ei, alpha, rhoi, rhoa, lf, lv, c,
     2    swdown, lwd, lwu, 
     3    cdq, cd, ua, cp, sigma, taus, taui, fw, rh, i0,
     4    hi, hs, temp, qextra, hmin, delh, L+1, M+1  )

        CALL otemper(
     1    ta, tf, ks, ki, ei, alpha, rhoi, rhoa, lf, lv, c,
     2    swdown, lwd, lwu, 
     3    cdq, cd, ua, cp, sigma, taus, taui, fw, rh, i0,
     4    hi, hs, temp, oqextra, 0.0, delh, L+1, M+1  )

        CALL grower(
     1    ta, tf, ks, ki, ei, alpha, rhoi, rhoa, lf, lv, c,
     2    swdown, lwd, lwu, 
     3    cdq, cd, ua, cp, sigma, taus, taui, fw, rh, i0,
     4    hi, hs, temp, qextra, grow, hmin, delh, L+1, M+1  )

        CALL grower(
     1    ta, tf, ks, ki, ei, alpha, rhoi, rhoa, lf, lv, c,
     2    swdown, lwd, lwu, 
     3    cdq, cd, ua, cp, sigma, taus, taui, fw, rh, i0,
     4    hi, hs, temp, oqextra, ogrow, 0.0, delh, L+1, M+1  )

        DO 1010 k = 0, M
        DO 1010 j = 0, L
          avger(j,k) = avger(j,k) + grow(j,k)
          oavg(j,k)  = oavg(j,k)  + ogrow(j,k)
 1010   CONTINUE

 1000 CONTINUE

      DO 2010 k = 0, M
      DO 2010 j = 0, L
        avger(j,k) = avger(j,k) / FLOAT(nstep)
        avger(j,k) = avger(j,k) * 100.0 * 86400.
        oavg(j,k)  = oavg(j,k)  / FLOAT(nstep)
        oavg(j,k)  = oavg(j,k)  * 100.0 * 86400.
 2010 CONTINUE

      DO 2000 j = 0, M
        DO 2100 i = 0, L
          IF (avger(i,j) .LT. -127.0 ) avger(i,j) = -127.0
          IF (avger(i,j) .GT.  127.0 ) avger(i,j) =  127.0
          IF (oavg (i,j) .LT. -127.0 ) oavg (i,j) = -127.0
          IF (oavg (i,j) .GT.  127.0 ) oavg (i,j) =  127.0
 2100   CONTINUE
 2000 CONTINUE

      WRITE (12) avger
      WRITE (12) oavg

      STOP
      END
