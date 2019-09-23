CD    assign -a $HOME3/icemodel/running/$pole/MASK fort.10
CD    assign -a $HOME3/ssmi/${p}land.map -s unblocked fort.11
CD    assign -a /ombptmp/ice/analy/fifteenth/b3${pole}.$tag -s unblocked fort.12
CD    ssign -a mod5.conc
      PROGRAM iscore
C     Compute scores for the ice model against the passive microwave analysis
C     Bob Grumbine 9 February 1996

      IMPLICIT none
      INCLUDE "skile.inc"

      INTEGER mmask(0:L, 0:m), smask(0:LP*divisor-1, 0:mp*divisor-1)
      REAL mconc(0:l, 0:m), sconc(0:lp*divisor-1, 0:mp*divisor-1)
      REAL spconc(0:lp*divisor-1, 0:mp*divisor-1)
      INTEGER mermask(0:lp*divisor - 1, 0:mp*divisor - 1 )

      CHARACTER*1 cmask(0:LP*divisor-1, 0:mp*divisor-1)
      CHARACTER*1 cconc(0:LP*divisor-1, 0:mp*divisor-1)
      CHARACTER*1 pconc(0:LP*divisor-1, 0:mp*divisor-1)
      
      REAL ssvec(lp*mp*divisor*divisor), mvec(lp*mp*divisor*divisor)

      INTEGER i, j, k

      DO 100 j = 1, M
        READ (10,9001) (mmask(i,j),i=1,l)
  100 CONTINUE
      DO 200 j = 0, M
        READ (10,9001) (mmask(i,j),i=0,l)
  200 CONTINUE
 9001 FORMAT(120I1)

      READ (14) pconc
      READ (13) mconc
      READ (12) cconc
      READ (11) cmask

      DO 1000 j = 0, MP*divisor - 1
        DO 1100 i = 0, LP*divisor - 1
           smask(i,j) = ICHAR(cmask(i,j))
           sconc(i,j) = FLOAT(ICHAR(cconc(i,j))) / 100.
           spconc(i,j) = FLOAT(ICHAR(pconc(i,j))) / 100.
 1100   CONTINUE
 1000 CONTINUE

C     Create the merged land mask
      DO 2000 j = 0, mp*divisor - 1
        DO 2100 i = 0, lp*divisor - 1
          IF (smask(i,j) .EQ. 157 .OR. mmask(i/divisor, j/divisor) 
     1                                 .EQ. 0) THEN
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
          IF (mermask(i,j) .NE. 157 .AND. 
     1      ( ( sconc(i,j) .LT. 1.28 .AND. sconc(i,j) .GT. 0.) .OR.
     2        (mconc(i/divisor, j/divisor) .LT. 1.28 .AND.
     3        mconc(i/divisor, j/divisor) .GT. 1.E-3 )   )  
     4        .AND. sconc(i,j) .LT. 1.28 ) THEN
            k = k + 1
            ssvec(k) = AMIN1(1.00, sconc(i,j))
            mvec(k) = mconc(i/divisor,j/divisor)
          ELSE
            PRINT *,i,j,mermask(i,j), sconc(i,j), 
     1               mconc(i/divisor,j/divisor)
          ENDIF
 3100   CONTINUE
 3000 CONTINUE

CD      PRINT *,'npts = ',k
      IF (k .LE. 1) THEN
        PRINT *,'No matchups'
      ELSE
        CALL scorer(ssvec, mvec, k, 5)
      ENDIF

      k = 0
      DO 4000 j = 0, mp*divisor - 1
        DO 4100 i = 0, lp*divisor - 1
          IF (mermask(i,j) .NE. 157 .AND. 
     1      ( ( sconc(i,j) .LT. 1.28 .AND. sconc(i,j) .GT. 0.) .OR.
     2        (spconc(i, j) .LT. 1.28 .AND. spconc(i, j) .GT. 1.E-3)  )
     4        .AND. sconc(i,j) .LT. 1.28 .AND. spconc(i,j) .LT. 1.28) 
     5     THEN
            k = k + 1
            ssvec(k) = AMIN1(1.00, sconc(i,j))
            mvec(k) =  AMIN1(1.00, spconc(i,j))
            PRINT *,i,j,mermask(i,j), sconc(i,j), spconc(i,j)
           ELSE
            PRINT *,i,j,mermask(i,j), sconc(i,j), spconc(i,j)
          ENDIF
 4100   CONTINUE
 4000 CONTINUE

CD      PRINT *,'npts = ',k
      IF (k .LE. 1) THEN
        PRINT *,'No matchups'
      ELSE
        CALL scorer(ssvec, mvec, k, 5)
      ENDIF
      STOP
      END
