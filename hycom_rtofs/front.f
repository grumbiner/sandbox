      PROGRAM front
      implicit none

C     Serve as a front end to the 3 layer thermodynamic code of
C     Winton, 2000.

C     For the ice code:
C     Ice variables
      REAL hs, hi, ts, t1, t2
      REAL tmelt, bmelt
C     Forcing variables
      REAL hf, sol, hfd, fb, snow
C     Generic
      REAL dt
C     physics
      REAL thinice, rhoi, lf
      PARAMETER (rhoi = 905) !winton value
      PARAMETER (lf   = 3.34e5) !
      PARAMETER (thinice = 0.01)

C     From the meteorological model:
      INTEGER nx, ny
      PARAMETER (nx = 768)
      PARAMETER (ny = 384)
      REAL swdown, swup
      REAL lwdown, lwup
      REAL prate  ! kg/m^2/s
      REAL shtfl, lhtfl  !net sensible and latent heat fluxes
C     Meteorology a local flux computation would want:
C       Sensible heat:
      REAL ugrd, vgrd, t2m
C       Latent heat:
      REAL spfh, press
C       Radiation
      REAL albedo
C       Boundary Layer depth
      REAL hpbl

C     Required external (to ice code) function:
      REAL dfluxdtemp

C     Set up some parameters
      PARAMETER (dt = 3600.*6) !6 hourly info from flux file 
      PARAMETER (fb = 2)       !canonical arctic ocean-ice flux
      
      INTEGER i, years
      REAL sum, sum_rad, sum_sen

C     Get meteorological input
      OPEN(10, FILE="metinput", FORM="unformatted")

C     No prior ice information, set up cold thin ice start:
      hs =  0.0334
      hi =  0.0675
      t1 = -1.8
      t2 = -1.8
      ts =  0.0

      DO years = 1, 6
      sum = 0.0
      sum_rad = 0.0
      sum_sen = 0.0
      DO i = 1, 1460
        READ (10) shtfl, lhtfl, t2m, lwdown, lwup, swup, swdown, prate,
     1            ugrd, vgrd, spfh, press, hpbl, albedo

C     Compute the dflux/dtemp term, pass as hfd
        hfd = dfluxdtemp(hi, hs, ugrd, vgrd, ts+273.15, spfh, press, 
     1                   albedo, swdown)

        snow = prate / 330.  !sbr wants m/s, prate is kg/m^2/s
        sol  = - (swdown - swup + lwdown - lwup)
        hf   = - (shtfl + lhtfl) 
        sum  = sum + sol + hf
        sum_rad = sum_rad + sol
        sum_sen = sum_sen + hf
        IF (hi .LT. thinice) THEN
          t1 = -1.8
          t2 = -1.8
          ts =  0.0
          hs = hs + snow*dt
          hi = hi + hf/rhoi/lf*dt
          IF (hi .LT. 0.0) THEN
            hi = 0.
            hs = 0.
          ENDIF
C          IF (hs .LT. 0.0) THEN
C            hs = 0.
C            PRINT *,'reset hs'
C          ENDIF
        ELSE
          CALL ice3lay(hs, hi, t1, t2, ts, hf, sol, hfd, fb,
     &                   snow, dt, tmelt, bmelt)
        ENDIF

      WRITE(*,9000) years, i, hs, hi, t1, t2, ts, t2m, hf, hfd
      ENDDO
CD        PRINT *,'Net for year ',years,' = ',sum/1460,sum_rad/1460, 
CD     1                 sum_sen/1460
        REWIND(10)
      ENDDO
 9000 FORMAT (I2,I5,F7.4,F7.4,F7.2,F6.2, F6.2, 3F7.2, F7.2)

      STOP
      END

C     Based on Grumbine 1994 code
C     Expects Kelvin temperature
      REAL FUNCTION dfluxdtemp(hi, hs, ugrd, vgrd, ts, spfh, press,
     1                    albedo, swdown)
      IMPLICIT NONE
      REAL hi, hs, ugrd, vgrd, ts, spfh, press, albedo, swdown

      REAL ks, ki, rhoa, cdq, lv, cd, cp, ei, sigma
      PARAMETER (sigma  = 5.67051E-8 )   !Stefan-Boltzmann constant
CORIG      PARAMETER (ki     = 2.1656)
      PARAMETER (ki     = 2.03)  !as used by winton
      PARAMETER (ks     = 0.31)
      PARAMETER (rhoa   = 1.29)
      PARAMETER (cdq    = 1.75E-3)
      PARAMETER (cd     = 1.75E-3)
      PARAMETER (lv     = 2.5008E6 )  !Gill 0C
      PARAMETER (cp     = 1004.6)     !NMC Handbook
      PARAMETER (ei     = 0.97) 

      REAL ua, t
      REAL qs, dqdt, dfdt

      qs(t) = 0.622*6.11/1013.25*EXP(LOG(10.)*9.5*(t-273.16)/(t-7.66))
      dqdt(t) = qs(t)*(273.16-7.66)/(t-7.66)**2*LOG(10.)*9.5
CORIG      dfdt(t) = + ks*ki/(ks*hi+ki*hs)
C     Winton sbr computes thermal conduction term itself
      dfdt(t) = 
     1           + rhoa*cdq*lv*ua*dqdt(t)
     2           + rhoa*cd*cp*ua
     3           + 4.*ei*sigma*t**3

        ua = sqrt(ugrd*ugrd + vgrd*vgrd)
        dfluxdtemp = dfdt(ts)

      RETURN
      END
