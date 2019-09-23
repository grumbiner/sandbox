      PROGRAM therm
C     Derive sea ice themodynamics

      IMPLICIT none

      REAL hmin, hmax, delh
      PARAMETER (hmin = 0.10)
      PARAMETER (hmax = 10.0)
      PARAMETER (delh = 0.01)
      INTEGER nx
      PARAMETER (nx = ((hmax - hmin)/delh) )

      REAL temp(0:nx)
      REAL grow(0:nx)

      CHARACTER*60 fname
      REAL hs, hi, qextra
 
      REAL ta, tf, ks, ki, ei, alpha, rhoi, rhoa, lf, lv, c
      REAL swdown, cdq, cd, ua, cp, sigma, taus, taui, fw, qa, i0

      INTEGER i, j

      CALL init(
     1  ta, tf, ks, ki, ei, alpha, rhoi, rhoa, lf, lv, c,
     2  swdown, cdq, cd, ua, cp, sigma, taus, taui, fw, qa, i0  )

      hs = 0.00
 
      DO 200 i = 0, nx
        temp(i) = tf
        grow(i) = 0.0
  200 CONTINUE

          CALL temper(
     1  ta, tf, ks, ki, ei, alpha, rhoi, rhoa, lf, lv, c,
     2  swdown, cdq, cd, ua, cp, sigma, taus, taui, fw, qa, i0,
     3  hi, hs, temp, qextra, hmin, delh, nx  )

          CALL grower(
     1  ta, tf, ks, ki, ei, alpha, rhoi, rhoa, lf, lv, c,
     2  swdown, cdq, cd, ua, cp, sigma, taus, taui, fw, qa, i0,
     3  hi, hs, temp, qextra, grow, hmin, delh, nx  )


C     Will probably need to rescale these terms.


CD      WRITE (*,9001) ((hmin+i*delh, temp(i)-tf, 
CD     1                grow(i)*100.*86400.),i=0,nx)

 9001 FORMAT (F6.3, F6.1, F8.3)

      STOP
      END
