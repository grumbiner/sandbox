      PROGRAM therm
C     Derive sea ice themodynamics

      IMPLICIT none

      REAL hmin, hmax, delh
      PARAMETER (hmin = 0.10)
      PARAMETER (hmax = 10.0)
      PARAMETER (delh = 0.0001)
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

      DO 1000 i = 0, nx
        hi = i*delh + hmin
          IF(hi .EQ. 0.0) THEN
            temp(i) = tf
            grow(i) = 0.0
            GO TO 2000
          ENDIF

          CALL temper(
     1  ta, tf, ks, ki, ei, alpha, rhoi, rhoa, lf, lv, c,
     2  swdown, cdq, cd, ua, cp, sigma, taus, taui, fw, qa, i0,
     3  hi, hs, temp(i), qextra   )

          CALL grower(
     1  ta, tf, ks, ki, ei, alpha, rhoi, rhoa, lf, lv, c,
     2  swdown, cdq, cd, ua, cp, sigma, taus, taui, fw, qa, i0,
     3  hi, hs, temp(i), qextra, grow(i)   )

 2000   CONTINUE

CD        WRITE (*,9001) hi, temp(i)-tf, 
CD     1                  grow(i)*100.*86400.

 1010   CONTINUE
 1000 CONTINUE

C     Will probably need to rescale these terms.
      DO 3100 i = 0, nx
        grow(i) = grow(i)*100.*86400.
 3100 CONTINUE


 9001 FORMAT (F6.3, F6.1, F8.3)

      STOP
      END
