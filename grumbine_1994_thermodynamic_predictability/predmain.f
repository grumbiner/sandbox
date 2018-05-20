      PROGRAM predictability
C     Derive terms of interest in thermodynamic predictability of
C       sea ice, as defined by bg paper.
      IMPLICIT none

      REAL hmin, hmax, delh
      PARAMETER (hmin = 0.00)
      PARAMETER (hmax = 4.00)
      PARAMETER (delh = 0.02)
      INTEGER nx, ny
      PARAMETER (nx = ((hmax - hmin)/delh) )
      PARAMETER (ny = ((hmax - hmin)/delh) )

      REAL temp(0:nx, 0:ny)
      REAL grow(0:nx, 0:ny)
      REAL pred(0:nx, 0:ny)
      CHARACTER*60 fname
      REAL hs, hi, qextra
 
      REAL ta, tf, ks, ki, ei, alpha, rhoi, rhoa, lf, lv, c
      REAL swdown, cdq, cd, ua, cp, sigma, taus, taui, fw, qa, i0

      INTEGER i, j

      CALL init(
     1  ta, tf, ks, ki, ei, alpha, rhoi, rhoa, lf, lv, c,
     2  swdown, cdq, cd, ua, cp, sigma, taus, taui, fw, qa, i0  )
 
      DO 100 j = 0, ny
        DO 200 i = 0, nx
          temp(i,j) = tf
          grow(i,j) = 0.0
          pred(i,j) = 0.0
  200   CONTINUE
  100 CONTINUE

      DO 1000 i = 0, ny
        hi = i*delh
        DO 1010 j = 0, i
          hs = j*delh/4.
          IF(hi .EQ. 0.0) THEN
            temp(i,j) = tf
            grow(i,j) = 0.0
            pred(i,j) = 0.0
            GO TO 2000
          ENDIF

          CALL temper(
     1  ta, tf, ks, ki, ei, alpha, rhoi, rhoa, lf, lv, c,
     2  swdown, cdq, cd, ua, cp, sigma, taus, taui, fw, qa, i0,
     3  hi, hs, temp(i,j), qextra   )

          CALL grower(
     1  ta, tf, ks, ki, ei, alpha, rhoi, rhoa, lf, lv, c,
     2  swdown, cdq, cd, ua, cp, sigma, taus, taui, fw, qa, i0,
     3  hi, hs, temp(i,j), qextra, grow(i,j)   )

          CALL preder(
     1  ta, tf, ks, ki, ei, alpha, rhoi, rhoa, lf, lv, c,
     2  swdown, cdq, cd, ua, cp, sigma, taus, taui, fw, qa, i0,
     3  hi, hs, temp(i,j), qextra, pred(i,j)   )
          IF (grow(i,j) .LT. 0.0) THEN
CD            pred(i,j) = MIN(pred(i,j), ABS(hi/grow(i,j)) )
          ENDIF
 2000   CONTINUE

        IF (hs .EQ. 0.) WRITE (*,9001) hs, hi, temp(i,j)-tf, 
     1                  grow(i,j)*100.*86400., pred(i,j)/86400.,
     2     pred(i,j)/86400./365.2422
CD        WRITE (14,9001) hs, hi, temp(i,j)-tf, 
CD     1                  grow(i,j)*100.*86400., pred(i,j)/86400.

 1010   CONTINUE
 1000 CONTINUE

C     Will probably need to rescale these terms.
      DO 3000 j = 0, ny
        DO 3100 i = 0, nx
          grow(i,j) = grow(i,j)*100.*86400.
          pred(i,j) = pred(i,j)/86400.
 3100   CONTINUE
 3000 CONTINUE

      WRITE (10) temp
      WRITE (11) grow
      WRITE (12) pred

 9001 FORMAT (F6.3, F5.2, F6.1, F6.2,2x, F12.2, F8.3)

      STOP
      END
