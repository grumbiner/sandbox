      SUBROUTINE SNFRAC (SNEQV,IVEG,SNCOVR)

!      IMPLICIT NONE
      
C ----------------------------------------------------------------------
C SUBROUTINE SNFRAC
C ----------------------------------------------------------------------
C CALCULATE SNOW FRACTION (0 -> 1)
C SNEQV   SNOW WATER EQUIVALENT (M)
C IVEG    VEGETATION TYPE
C SNCOVR  FRACTIONAL SNOW COVER
C SNUP    THRESHOLD SNEQV DEPTH ABOVE WHICH SNCOVR=1
C SALP    TUNING PARAMETER
C ----------------------------------------------------------------------
      REAL SNEQV,SALP,SNUP(13),SNCOVR,RSNOW

      DATA SALP /2.6/
      DATA SNUP /0.080, 0.080, 0.080, 0.080, 0.080, 0.080,
     &  	 0.040, 0.040, 0.040, 0.040, 0.025, 0.040,
     &  	 0.025/
     
C ----------------------------------------------------------------------
C SNUP IS VEG-CLASS DEPENDENT SNOWDEPTH THRESHHOLD ABOVE WHICH SNOCVR=1.
C ----------------------------------------------------------------------
        IF (SNEQV .LT. SNUP(IVEG)) THEN
          RSNOW = SNEQV/SNUP(IVEG)
          SNCOVR = 1. - (EXP(-SALP*RSNOW) - RSNOW*EXP(-SALP))
        ELSE
          SNCOVR = 1.0
        ENDIF
        SNCOVR = MAX(0.,MIN(SNCOVR,1.))

      RETURN
      END
