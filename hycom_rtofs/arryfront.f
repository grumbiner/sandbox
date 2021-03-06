      PROGRAM front
      implicit none

C     Serve as a front end to the 3 layer thermodynamic code of
C     Winton, 2000.

C     From the meteorological model:
      INTEGER nx, ny
      PARAMETER (nx = 768)
      PARAMETER (ny = 384)
      REAL swdown(nx, ny), swup(nx, ny)
      REAL lwdown(nx, ny), lwup(nx, ny)
      REAL prate(nx, ny)  ! kg/m^2/s
      REAL shtfl(nx, ny), lhtfl(nx, ny)  !net sensible and latent heat fluxes
C     Meteorology a local flux computation would want:
C       Sensible heat:
      REAL ugrd(nx, ny), vgrd(nx, ny), t2m(nx, ny)
C       Latent heat:
      REAL spfh(nx, ny), press(nx, ny)
C       Radiation
      REAL albedo(nx, ny)
C       Boundary Layer depth
      REAL hpbl(nx, ny)


C     For the ice code:
C     Ice variables
      REAL hs(nx, ny), hi(nx, ny), ts(nx, ny), t1(nx, ny), t2(nx, ny)
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


C     Required external (to ice code) function:
      REAL dfluxdtemp

C     Set up some parameters
      PARAMETER (dt = 3600.*6) !6 hourly info from flux file 
      PARAMETER (fb = 2)       !canonical arctic ocean-ice flux
      
      INTEGER i, j, tau, years
      REAL sum_rad(nx, ny), sum_sen(nx, ny)

C     Get meteorological input
      OPEN(10, FILE="metinput", FORM="unformatted")

C     No prior ice information, set up cold thin ice start:
      hs =  0.0
      hi =  0.0
      t1 = -1.8
      t2 = -1.8
      ts =  0.0

      DO years = 1, 1
      sum_rad = 0.0
      sum_sen = 0.0
      DO tau = 1, 124 + 120 + 124
CD        PRINT *,'i, tau ',years, tau
        READ (10) shtfl
        READ (10) lhtfl
        READ (10) t2m
        READ (10) lwdown
        READ (10) lwup
        READ (10) swup
        READ (10) swdown
        READ (10) prate
        READ (10) ugrd
        READ (10) vgrd
        READ (10) spfh
        READ (10) press
        READ (10) hpbl
        READ (10) albedo
        DO j = 1, ny
        DO i = 1, nx

C     Compute the dflux/dtemp term, pass as hfd
        hfd = dfluxdtemp(hi(i,j), hs(i,j), ugrd(i,j), vgrd(i,j), 
     1                ts(i,j)+273.15)

        snow = prate(i,j) / 330.  !sbr wants m/s, prate is kg/m^2/s
        sol  = - (swdown(i,j) - swup(i,j) + lwdown(i,j) - lwup(i,j))
        hf   = - (shtfl(i,j) + lhtfl(i,j)) 
        sum_rad(i,j) = sum_rad(i,j) + sol
        sum_sen(i,j) = sum_sen(i,j) + hf
        IF (hi(i,j) .LT. thinice) THEN
          t1(i,j) = -1.8
          t2(i,j) = -1.8
          ts(i,j) =  0.0
          hs(i,j) = hs(i,j) + snow*dt
          hi(i,j) = hi(i,j) + hf/rhoi/lf*dt
          IF (hi(i,j) .LT. 0.0) THEN
            hi(i,j) = 0.
            hs(i,j) = 0.
          ENDIF
        ELSE
          CALL ice3lay(hs(i,j), hi(i,j), t1(i,j), t2(i,j), ts(i,j), hf, 
     1                 sol, hfd, fb, snow, dt, tmelt, bmelt)
        ENDIF

        ENDDO !i
        ENDDO !j

      ENDDO !tau
        REWIND(10)
      ENDDO !years
 9000 FORMAT (I2,I5,F7.4,F7.4,F7.2,F6.2, F6.2, 3F7.2, F7.2)

      OPEN(12,FILE="output",FORM="unformatted")
      WRITE(12) sum_rad, sum_sen, hs, hi
      CLOSE(12)

      STOP
      END

C     Based on Grumbine 1994 code
C     Expects Kelvin temperature
      REAL FUNCTION dfluxdtemp(hi, hs, ugrd, vgrd, ts)
      IMPLICIT NONE
      REAL hi, hs, ugrd, vgrd, ts

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
