      SUBROUTINE tsexam(t, s, t2, s2, mask)
C     Check the ts files generated for the ice model for
C       outliers and physical consistency (stability)
C     Author: Bob Grumbine
C     LAST MODIFIED: 28 September 1994.

      IMPLICIT none

      INCLUDE "oml.inc"
      INCLUDE "physical.inc"
      INCLUDE "icegrid.inc"

      REAL t(LP, MP), s(LP, MP)
      REAL t2(LP, MP), s2(LP, MP)
      INTEGER mask(LP, MP)
      INTEGER i, j
      REAL sumt, sums, sout, tout, sout2, tout2, sumt2, sums2
      REAL meant, means, meant2, means2
      INTEGER count
      CHARACTER*60 fname
      REAL rho1, rho2, eps

 9001 FORMAT (A60)

C     Compute the means
      sumt = 0.
      sums = 0.
      sumt2 = 0.
      sums2 = 0.
      count = 0
      DO 1000 j = 1, MP
        DO 1100 i  = 1, LP
          IF (mask(i,j) .NE. 0) THEN
            count = count + 1
            sumt = sumt + t(i,j)
            sums = sums + s(i,j)
            sumt2 = sumt2 + t2(i,j)
            sums2 = sums2 + s2(i,j)
          ENDIF
 1100   CONTINUE
 1000 CONTINUE
      meant = sumt/FLOAT(count)
      means = sums/FLOAT(count)
      meant2 = sumt2/FLOAT(count)
      means2 = sums2/FLOAT(count)
      PRINT *,'Mean t and mean s are ',meant, means, meant2, means2
 
C     Now look for outliers
      PRINT *,'What is the outlier bound for upper level t?'
      READ (*,9002) tout
      PRINT *,'What is the outlier bound for upper level s?'
      READ (*,9002) sout
      PRINT *,'What is the outlier bound for lower level t?'
      READ (*,9002) tout2
      PRINT *,'What is the outlier bound for lower level s?'
      READ (*,9002) sout2
      PRINT *,'What would you like to call the outlier file?'
      READ (*,9001) fname
      OPEN (11, FILE=fname, FORM='FORMATTED', STATUS='NEW')
            
      DO 2000 j = 1, MP
        DO 2100 i = 1, LP
          IF (mask(i,j) .NE. 0) THEN
          IF (ABS(t(i,j)-meant) .GT. tout .OR. 
     1        ABS(s(i,j)-means) .GT. sout .OR. 
     1        ABS(t2(i,j)-meant2) .GT. tout2 .OR. 
     1        ABS(s2(i,j)-means2) .GT. sout2) THEN
CD              WRITE (*,9003) i, j, t(i,j), s(i,j), t2(i,j), s2(i,j)
              WRITE (11, 9003) i, j, t(i,j), s(i,j), t2(i,j), s2(i,j)
          ENDIF
          ENDIF
 2100   CONTINUE
 2000 CONTINUE


C     Now conduct an outlier modification -- set to nearest permitted
C       value.
      PRINT *,'Conducting outlier modification '
      DO 4000 j = 1, MP
        DO 4100 i = 1, LP
          IF (ABS(t(i,j)-meant) .GT. tout) THEN
            IF (t(i,j) .GT. meant) THEN
              t(i,j) = meant+tout
             ELSE
              t(i,j) = meant-tout
            ENDIF
          ENDIF
          IF (ABS(s(i,j)-means) .GT. sout) THEN
            IF (s(i,j) .GT. means) THEN
              s(i,j) = means+sout
             ELSE
              s(i,j) = means-sout
            ENDIF
          ENDIF
          IF (ABS(t2(i,j)-meant2) .GT. tout2) THEN
            IF (t2(i,j) .GT. meant2) THEN
              t2(i,j) = meant2+tout2
             ELSE
              t2(i,j) = meant2-tout2
            ENDIF
          ENDIF
          IF (ABS(s2(i,j)-means2) .GT. sout2) THEN
            IF (s2(i,j) .GT. means2) THEN
              s2(i,j) = means2+sout2
             ELSE
              s2(i,j) = means2-sout2
            ENDIF
          ENDIF
 4100   CONTINUE
 4000 CONTINUE

C     Now stabilize the ocean.  Consider deep level perfect, and 
C      adjust salinity.
      PRINT *,'Starting static stability check'
      PRINT *,'What is the minimum stability period '
      READ (*,9002) eps
      eps = (HMLREF*RHOWAT/GRAV)*3.141592654**2/eps/eps
      PRINT *,'Delta rho is ',eps
      eps = eps/RHOWAT 
C     Division necessary because of definition of rho1,2
      DO 3000 j = 1, MP
        DO 3100 i = 1, LP
          IF (mask(i,j) .NE. 0) THEN
            rho1 = -betat* t(i,j) + betas*s(i,j) + gammat*t(i,j)*t(i,j)
            rho2 = -betat*t2(i,j) + betas*s2(i,j) + 
     1                                 gammat*t2(i,j)*t2(i,j)
            IF (rho1+eps .GT. rho2) THEN
              WRITE (11, 9003) i, j, t(i,j), s(i,j), t2(i,j), s2(i,j),
     1           RHOWAT*rho1, RHOWAT*rho2, RHOWAT*(rho1-rho2)
C            Change the salinity.
            s(i,j) = s2(i,j) - eps/betas - betat*(t2(i,j)-t(i,j))/betas
     1                       +gammat/betas * (t2(i,j)**2 - t(i,j)**2)
          ENDIF
          ENDIF
 3100   CONTINUE
 3000 CONTINUE

 9002 FORMAT (E13.6)
 9003 FORMAT (2I4, 6F8.3, F7.3)

      RETURN
      END 
