      SUBROUTINE init( nstep,
     1  tf, ks, ki, ei, alpha, rhoi, rhoa, lf, lv, c,
     2  cdq, cd, ua, cp, sigmat, taus, taui, i0  )

C     Get parameters for predictability tests.
      IMPLICIT none

      INCLUDE "physical.inc"

      CHARACTER*60 fname
      INTEGER i, nstep
 
      REAL tf, ks, ki, ei, alpha, rhoi, rhoa, lf, lv, c
      REAL cdq, cd, ua, cp, sigmat, taus, taui, i0

      PRINT *,'How many weather steps are there?'
      READ (*,9003) nstep
 
      DO 1 i = 1, nstep
        PRINT *,'What is the name of the meteo file?'
        READ (*,9001) fname
        OPEN (UNIT=30+i, FILE=fname, FORM='UNFORMATTED', STATUS='OLD')
   1  CONTINUE
      PRINT *,'What is the name of the ocean flux file?'
      READ (*,9001) fname
      OPEN (UNIT=11, FILE=fname, FORM='UNFORMATTED', STATUS='OLD')
      PRINT *,'What would you like to call the freezing rate file?'
      READ (*,9001) fname
CD      OPEN (UNIT=12, FILE=fname, FORM='UNFORMATTED', STATUS='NEW')
      PRINT *,'What would you like to call the debug file?'
      READ (*,9001) fname
CD      OPEN (UNIT=13, FILE=fname, FORM='FORMATTED', STATUS='NEW')
    
      READ (*,9002) alpha
      READ (*,9002) c
      READ (*,9002) ua
      READ (*,9002) taus
      READ (*,9002) taui
      READ (*,9002) i0

C     Parameters derivable from the physical include file.
      tf = tmelt + tfrez
      ks = consn
      ki = con
      ei = epsilw
      rhoi = rhoice
      rhoa = rhoair
      lf   = lfuse
      lv   = vapl
      cdq = clat
      cd  = csens
      cp = cpair
      sigmat = sigma

 9001 FORMAT (A60)
 9002 FORMAT (E13.6)
 9003 FORMAT (I2)

      RETURN
      END
