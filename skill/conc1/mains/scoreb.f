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
CD            PRINT *,i,j,mermask(i,j), sconc(i,j), mconc(i/divisor,j/divisor)
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
     5    THEN
            k = k + 1
            ssvec(k) = AMIN1(1.00, sconc(i,j))
            mvec(k) =  AMIN1(1.00, spconc(i,j))
CD            PRINT *,i,j,mermask(i,j), sconc(i,j), spconc(i,j)
          ELSE
CD            PRINT *,i,j,mermask(i,j), sconc(i,j), spconc(i,j)
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

      SUBROUTINE scorer(obs, model, npts, unit)
      IMPLICIT none

      INTEGER unit, npts
      REAL obs(npts), model(npts), tmp(180000)

      REAL ia, r2, sig2x, sig2y, xbar, ybar
      REAL rms, sigdel, delbar
      REAL sumx, sumx2, iagree

      INTEGER i

      DO 1000 i = 1, npts
CD        PRINT *,'i, obs, model ',i,obs(i), model(i)
        tmp(i) = model(i) - obs(i)
 1000 CONTINUE
      
CD      PRINT *,'passed loop '
      delbar = sumx(tmp, npts)
CD      PRINT *,'delsum = ',delbar
      rms   = sumx2(tmp, npts)
CD      PRINT *,'rmssum = ',rms
      sigdel = (FLOAT(npts)*rms - delbar*delbar)/
     1            FLOAT(npts)/FLOAT(npts-1)
CD      PRINT *,'sigdel square ',sigdel
      delbar = delbar/FLOAT(npts)
      rms    = SQRT(rms/FLOAT(npts))
      sigdel = SQRT(sigdel)
CD      PRINT *,'del, rms, sig ',delbar, rms, sigdel

      ia = iagree(obs, model, npts)
CD      PRINT *,'ia = ', ia

      CALL correl(obs, model, npts, r2, xbar, ybar, sig2x, sig2y)
CD      PRINT *,'r2 = ', r2

      WRITE (*, 9001) npts, delbar, rms, sigdel, ia, r2,
     1      xbar, ybar, sqrt(sig2x), sqrt(sig2y)
 9001 FORMAT (I7, 1X, 3F6.3, 3x, F6.3, F6.3, 2x, 2F6.3, 2F6.3)

      RETURN
      END
