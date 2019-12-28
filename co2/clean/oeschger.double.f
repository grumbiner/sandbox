      PROGRAM oeschger
C     Attempt to simulate the results of the Oeschger model
C        as presented in Broecker and Peng.
      IMPLICIT none
      
      DOUBLE PRECISION secpyr
      PARAMETER (secpyr = 8.64D4*365.2422)
      INTEGER nlayer
      PARAMETER (nlayer = 76)
      DOUBLE PRECISION deltaz, deltat
      PARAMETER (deltaz = 50.D0)
      PARAMETER (deltat = secpyr/4.D0)
      
      DOUBLE PRECISION phos(nlayer), carbon(nlayer)
      DOUBLE PRECISION w(nlayer), kappa(nlayer)
      DOUBLE PRECISION qp(nlayer), s
      
      DOUBLE PRECISION decay, sink, qatm
      PARAMETER (decay = 1.D0/577.D0)
CD      PARAMETER (sink  = 0.085)
      PARAMETER (qatm  = 3.D0/secpyr)
      
      INTEGER i, j, lenrun
      DOUBLE PRECISION fp(nlayer), fc(nlayer), alpha
      DOUBLE PRECISION f1(nlayer), f2(nlayer), f3(nlayer)
      DOUBLE PRECISION sum1, sum2, sum3, stock
      CHARACTER*1 zo(nlayer)
      CHARACTER*60 fname
      
C     Initialize the carbon and phosphate concentrations
C       Concentrations are in Moles per cubic meter
      phos(1) = 0.D0
      phos(2) = 0.D0
      carbon(1) = 0.212D0
      carbon(2) = 0.212D0
      DO 100 i = 3, nlayer
        phos(i)   = 2.0D-3
        carbon(i) = 2.12D-1
  100 CONTINUE
  
C     Initialize the diffusivity (meters squared per second)
      DO 200 i = 1, 15
        kappa(i) = 1.6D-4
  200 CONTINUE
      DO 210 i = 16, nlayer
        kappa(i) = 0.5D-4
  210 CONTINUE
  
C     Initialize the vertical upwelling (meters per second)
      DO 300 i = 1, 20
        w(i) = 0.D0
  300 CONTINUE
      DO 310 i = 21, nlayer
        w(i) = 4.4D0/secpyr
  310 CONTINUE
  
C     Open the output file for ncsa.
      PRINT *,'What is the output file name?'
      READ (*,9001) fname
      OPEN (10, FILE=fname, FORM='FORMATTED', STATUS='NEW')
C     Determine the length of the run:
      PRINT *,'How long a run (approx 8 years per second)'
      READ (*,9003) lenrun
      OPEN (11, FILE='julie', FORM='FORMATTED', STATUS='NEW')
      PRINT *,'What sinking portion would you like?'
      READ (*,9004) sink
 9004 FORMAT (D13.6)
 
C     Now ready to compute the evolution of the carbon and phosphate
C       distributions.  Equilibration time is something of a mystery.
C       Start out with a 50 year run
      PRINT *,'time at start', LONG(362)
      sum2 = 0.D0
      alpha = decay/(1.-EXP(-decay*(deltaz*FLOAT(nlayer)-2.D0*deltaz)))
      DO 1000 j = 1, lenrun*INT(secpyr/deltat)
        s = -sum2*deltaz
CD        stock = (1.-sink*deltat)*stock
CD     1           +s*deltat/2.D0/deltaz/deltaz
CD        PRINT *,'standing crop',stock
CD        WRITE (11, 9004) stock
        DO 1100 i = 3, nlayer-2

          f1(i) = EXP(-(DBLE(i)-1.5D0)*deltaz*decay)*alpha*s*sink
CD          f2(i) = (kappa(i+1)*(phos(i+1)-phos(i))-
CD     1             kappa(i  )*(phos(i)-phos(i-1))  )
CD     1         /deltaz/deltaz
          f2(i) = kappa(i)*(-phos(i+2)+16.D0*phos(i+1)
     1        -30.D0*phos(i)-phos(i-2)+16.D0*phos(i-1)  )
     1         /deltaz/deltaz/24.D0
          f3(i) = - w(i)*(phos(i+1)-phos(i))/deltaz
          fp(i) = f1(i) + f2(i) + f3(i)
     
CD          fc(i) = EXP(-(DBLE(i)-1.5D0)*deltaz*decay)*alpha*s*105.D0*sink
CD     1 + (kappa(i+1)*(carbon(i+1)-carbon(i))
CD     1     -kappa(i)*(carbon(i)-carbon(i-1)))
CD     1      /deltaz/deltaz
CD     2 - w(i)*(carbon(i+1)-carbon(i))/deltaz
     
 1100   CONTINUE
 
          i = nlayer-1
          f1(i) = EXP(-(DBLE(i)-1.5D0)*deltaz*decay)*alpha*s*sink
          f2(i) = kappa(i)*(phos(i)-2.*phos(i-1)+phos(i-2))
     1   /deltaz/deltaz
          f3(i) = - w(i)*(phos(i)-phos(i-1))/deltaz
          fp(i) = f1(i) + f2(i) + f3(i)
          
          fc(i) = EXP(-(DBLE(i)-1.5D0)*deltaz*decay)*alpha*s*105.D0*sink
     1 + kappa(i)*(carbon(i+1)-2.*carbon(i)+carbon(i-1))/deltaz/deltaz
     2 - w(i)*(carbon(i)-carbon(i-1))/deltaz

C       Extrapolate the upper layers
        carbon(1) = carbon(1) + deltat*
     1                         (qatm/deltaz/2.D0 - 105.D0*s/2.D0/deltaz)
        carbon(2) = carbon(2) + deltat*
     1                         (qatm/deltaz/2. - 105.*s/2./deltaz)
     2   + kappa(2)*(carbon(1)-2.D0*carbon(2)+carbon(3))
     3             /deltaz/deltaz*deltat
        carbon(1) = 0.5D0*(carbon(1)+carbon(2))
        carbon(2) = carbon(1)
CD        PRINT *,j, carbon(1), carbon(nlayer/2), carbon(nlayer)
C       Extrapolate the interior
        DO 1200 i = 3, nlayer-1
          phos(i)   = phos(i)   + deltat * fp(i)
CD          carbon(i) = carbon(i) + deltat * fc(i)
 1200   CONTINUE
C       Apply the conservation conditions, phos, carb at bottom = phos, carb
C           at BW input level
        phos(nlayer) = phos(INT(1000.D0/deltaz))
CD        carbon(nlayer) = carbon(INT(1000./deltaz))
C       Now apply a no flux diffusion condition
        phos(nlayer-1)   = 0.5D0*(phos(nlayer)+phos(nlayer-1))
        carbon(nlayer-1) = 0.5D0*(carbon(nlayer-1)+carbon(nlayer))
        phos(nlayer)     = phos(nlayer-1)
        carbon(nlayer)   = carbon(nlayer-1)
CD        DO 1210 i = 3, nlayer-1
CD          WRITE (10, 9005) fp(i), CHAR(9), 
CD     1          f1(i), CHAR(9), f2(i), CHAR(9), f3(i)
CD 1210   CONTINUE
       
        IF (MOD((j-1),100*INT(secpyr/deltat)) .EQ. 0) THEN
          WRITE (10, 9002) (i,CHAR(9),phos(i),CHAR(9),
     1                              carbon(i),i=1, nlayer)
CD          WRITE (*, 9002) (i,CHAR(9),phos(i),CHAR(9),
CD     1                              carbon(i),i=1, nlayer)
        ENDIF
        
        stock = 0.D0
        sum1  = 0.D0
        sum2  = 0.D0
        sum3  = 0.D0
        DO 2000 i = 3, nlayer-1
          stock = stock + fp(i)
          sum1  = sum1  + f1(i)
          sum2  = sum2  + f2(i)
          sum3  = sum3  + f3(i)
 2000   CONTINUE
        PRINT *,'total forcing', stock/FLOAT(nlayer)
        WRITE (11, 9005) stock, CHAR(9), 
     1                    sum1, CHAR(9), sum2, CHAR(9), sum3
 1000 CONTINUE

      PRINT *,'time at end', LONG(362)
      PAUSE
      CLOSE(10, STATUS='KEEP')
      
 9001 FORMAT (A60)
 9002 FORMAT (I2,A1,E12.6,A1,E12.6)
 9003 FORMAT (I5, BN)
 9005 FORMAT (4(D12.6,A1))
      
      END