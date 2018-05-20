      PROGRAM icethk
C     Compute the thickness evolution of predetermined floes
      IMPLICIT none

      REAL hmin, hmax, delh
      PARAMETER (hmin = 0.00)
      PARAMETER (hmax = 4.00)
      PARAMETER (delh = 0.02)
      INTEGER nx, ny
      PARAMETER (nx = ((hmax - hmin)/delh) )
      PARAMETER (ny = ((hmax - hmin)/delh) )

      REAL temp
      REAL grow, pred
      CHARACTER*60 fname
      REAL hs, hi, qextra
 
      REAL ta, tf, ks, ki, ei, alpha, rhoi, rhoa, lf, lv, c
      REAL swdown, cdq, cd, ua, cp, sigma, taus, taui, fw, qa, i0

      INTEGER i, j

      REAL thick1, thick2, delt, delref, runlen
      PARAMETER (delt = 3600.)

      CALL init(
     1  ta, tf, ks, ki, ei, alpha, rhoi, rhoa, lf, lv, c,
     2  swdown, cdq, cd, ua, cp, sigma, taus, taui, fw, qa, i0  )

      READ(*,9001) thick1
      READ(*,9001) thick2
      READ(*,9001) runlen
      hs = 0.00
      delref = ABS(thick1-thick2)

      DO 1000 i = 1, INT(runlen)
      hi = thick1 
          CALL temper(
     1  ta, tf, ks, ki, ei, alpha, rhoi, rhoa, lf, lv, c,
     2  swdown, cdq, cd, ua, cp, sigma, taus, taui, fw, qa, i0,
     3  hi, hs, temp, qextra   )

          CALL grower(
     1  ta, tf, ks, ki, ei, alpha, rhoi, rhoa, lf, lv, c,
     2  swdown, cdq, cd, ua, cp, sigma, taus, taui, fw, qa, i0,
     3  hi, hs, temp, qextra, grow   )
      
          CALL preder(
     1  ta, tf, ks, ki, ei, alpha, rhoi, rhoa, lf, lv, c,
     2  swdown, cdq, cd, ua, cp, sigma, taus, taui, fw, qa, i0,
     3  hi, hs, temp, qextra, pred   )
      thick1 = thick1 + delt*grow

      hi = thick2 
          CALL temper(
     1  ta, tf, ks, ki, ei, alpha, rhoi, rhoa, lf, lv, c,
     2  swdown, cdq, cd, ua, cp, sigma, taus, taui, fw, qa, i0,
     3  hi, hs, temp, qextra   )

          CALL grower(
     1  ta, tf, ks, ki, ei, alpha, rhoi, rhoa, lf, lv, c,
     2  swdown, cdq, cd, ua, cp, sigma, taus, taui, fw, qa, i0,
     3  hi, hs, temp, qextra, grow   )
      
        thick2 = thick2 + delt*grow

        WRITE (*,9002) i, pred/3600., 
     1  thick1, thick2, ABS(thick1-thick2)/delref

 1000 CONTINUE

 9001 FORMAT (E13.6)
 9002 FORMAT (I4, F8.2, 2F7.4, F7.4)

      STOP
      END
