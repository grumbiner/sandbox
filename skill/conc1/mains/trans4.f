      PROGRAM trans
C     Construct vectors of observation, null forecaster, persistence

      IMPLICIT none
      INCLUDE "skile.inc"

      INTEGER mmask(0:L, 0:m)
      REAL smask(0:LP*divisor-1, 0:mp*divisor-1)
      REAL mconc(0:l, 0:m), sconc(0:lp*divisor-1, 0:mp*divisor-1)
      REAL spconc(0:lp*divisor-1, 0:mp*divisor-1)
      INTEGER mermask(0:lp*divisor - 1, 0:mp*divisor - 1 )
      REAL climo(0:L-1,0:M-1)

      INTEGER i, j, k, nf

      DO 100 j = 1, M
        READ (10,9001) (mmask(i,j),i=1,l)
  100 CONTINUE
      DO 200 j = 0, M
        READ (10,9001) (mmask(i,j),i=0,l)
  200 CONTINUE
 9001 FORMAT(120I1)

      READ (13) mconc
      READ (16) climo

C      CALL cread(pconc, nf, LP*divisor, MP*divisor, "fort.14")
C      CALL cread(cconc, nf, LP*divisor, MP*divisor, "fort.12")
      READ (12) sconc
      READ (14) spconc
 9100 FORMAT (A7)
CD      CALL cread(cmask, nf, LP*divisor, MP*divisor, fname)
      READ (11) smask 

C     Create the merged land mask
      DO 2000 j = 0, mp*divisor - 1
        DO 2100 i = 0, lp*divisor - 1
          IF (smask(i,j) .EQ. 157 .OR. smask(i,j) .EQ. 195
     1           .OR. mmask(i/divisor, j/divisor) 
     2                                 .EQ. 0) THEN
            mermask(i,j) = 157
          ELSE
            mermask(i,j) = 0
          ENDIF
 2100   CONTINUE
 2000 CONTINUE

C     Now create the vector with ice (where both report a non-zero ice
C       concentration, and the obs aren't missing or bad.).  Along the
C       way, cap the observations to 100 percent.

      k = 0
      DO 3000 j = 0, mp*divisor - 1
        DO 3100 i = 0, lp*divisor - 1
C         The following limits comparison to points poleward of 50.
          IF ( SQRT((i - polei*divisor)**2 + (j - polej*divisor)**2 ) 
     1      .GT. 175) THEN
            GO TO 3100
          ENDIF
          IF (mermask(i,j) .NE. 157 .AND. 
     1      ( ( sconc(i,j) .LT. 1.28 .AND. 
     2          sconc(i,j) .GT. 0.         ) .OR.
     3        (mconc(i/divisor, j/divisor) .LT. 1.28 .AND.
     4         mconc(i/divisor, j/divisor) .GT. 1.E-3      )  .OR. 
     4        (climo(i, j) .LT. 1.28 .AND.
     5         climo(i, j) .GT. 1.E-3      )  .OR. 
     6        (spconc(i, j) .LT. 1.28 .AND. 
     7         spconc(i, j) .GT. 1.E-3      )   ) 
     8   ) THEN
            k = k + 1
            WRITE (*,9009) AMIN1(1.00, sconc(i,j)/100.), 
     1          AMIN1(1.00, spconc(i,j)/100.), 
     2          AMIN1(1.00, climo(i,j)/100.),
     3          mconc(i/divisor, j/divisor) 
          ELSE
CD            PRINT *,i,j,mermask(i,j), sconc(i,j), mconc(i/divisor,j/divisor)
          ENDIF
 3100   CONTINUE
 3000 CONTINUE

 9009 FORMAT(4F7.2)

      STOP
      END
