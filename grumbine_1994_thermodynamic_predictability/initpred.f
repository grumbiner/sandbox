      SUBROUTINE init(
     1  ta, tf, ks, ki, ei, alpha, rhoi, rhoa, lf, lv, c,
     2  swdown, cdq, cd, ua, cp, sigma, taus, taui, fw, qa, i0  )

C     Get parameters for predictability tests.
      IMPLICIT none

      CHARACTER*60 fname
 
      REAL ta, tf, ks, ki, ei, alpha, rhoi, rhoa, lf, lv, c
      REAL swdown, cdq, cd, ua, cp, sigma, taus, taui, fw, qa, i0

      PRINT *,'What would you like to call the temperature file?'
      READ (*,9001) fname
      OPEN (UNIT=10, FILE=fname, FORM='UNFORMATTED', STATUS='NEW')
      PRINT *,'What would you like to call the growth file?'
      READ (*,9001) fname
      OPEN (UNIT=11, FILE=fname, FORM='UNFORMATTED', STATUS='NEW')
      PRINT *,'What would you like to call the predictability file?'
      READ (*,9001) fname
      OPEN (UNIT=12, FILE=fname, FORM='UNFORMATTED', STATUS='NEW')
      PRINT *,'What would you like to call the debug file?'
      READ (*,9001) fname
      OPEN (UNIT=13, FILE=fname, FORM='FORMATTED', STATUS='NEW')
    
      READ (*,9002) ta
      READ (*,9002) tf
      READ (*,9002) ks
      READ (*,9002) ki
      READ (*,9002) ei
      READ (*,9002) alpha
      READ (*,9002) rhoi
      READ (*,9002) rhoa
      READ (*,9002) lf
      READ (*,9002) lv
      READ (*,9002) c

      READ (*,9002) swdown
      READ (*,9002) cdq
      READ (*,9002) cd
      READ (*,9002) ua
      READ (*,9002) cp
      READ (*,9002) sigma
      READ (*,9002) taus
      READ (*,9002) taui
      READ (*,9002) fw
      READ (*,9002) qa
      READ (*,9002) i0

 9001 FORMAT (A60)
 9002 FORMAT (E13.6)

      RETURN
      END
