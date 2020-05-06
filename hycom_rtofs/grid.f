      PROGRAM front
      implicit none

C     Serve as a front end to the 3 layer thermodynamic code of
C     Winton, 2000.

C     From the meteorological model:
      INTEGER nx, ny
      PARAMETER (nx = 216)
      PARAMETER (ny =  97)
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
C       Land mask, 1 = land
      REAL land(nx, ny)
      REAL oldice(nx, ny), newice(nx, ny)


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
      
      INTEGER yearmax, steps(12),mon(12)
      INTEGER i, j, tau, month, years
      REAL sum_rad(nx, ny), sum_sen(nx, ny)
      CHARACTER*80 fname
      DATA steps/124,120,121,124,116,124,120,124,120,124,122,120/
      DATA mon  /10,  11, 12, 01, 02, 03, 04, 05, 06, 07, 08, 09/
      PARAMETER (yearmax = 1)

      INTEGER count(nx, ny)
      REAL imax(nx, ny), smax(nx, ny)

      INTEGER first(nx, ny), last(nx, ny), days(nx, ny)
      REAL vcount(nx*ny), vdays(nx*ny)
      INTEGER dayno
      REAL alpha_sw, alpha_lw, beta_rad
      REAL alpha_sh, alpha_lh, beta_bndy
      REAL r2, xbar, ybar, sig2x, sig2y 
      INTEGER k

C     No prior ice information, set up cold thin ice start:
      hs =  0.0
      hi =  0.0
      t1 = -1.8
      t2 = -1.8
      ts =  0.0

      OPEN (11, FILE="params",FORM="formatted")
      READ (11,*) alpha_sw, alpha_lw, beta_rad, 
     1            alpha_sh, alpha_lh, beta_bndy
      CLOSE(11)

      OPEN(10, FILE="land", FORM="unformatted")
      READ(10) land
      CLOSE(10)
CD      OPEN(10, FILE="first", FORM="unformatted")
CD      READ(10) first
CD      CLOSE(10)
CD      OPEN(10, FILE="last", FORM="unformatted")
CD      READ(10) last
CD      CLOSE(10)
      OPEN(10, FILE="days", FORM="unformatted")
      READ(10) days
      CLOSE(10)
      oldice = 0
      newice = 0

      DO years = 1, yearmax
        dayno = 0
        sum_rad = 0.0
        sum_sen = 0.0
        count = 0
        imax  = 0.0
        smax  = 0.0
CD      DO month = 1, 9  ! run through end of June
      DO month = 1, 8  ! run through end of May
C     Get meteorological input
      IF (mon(month) .LT. 10) THEN
        WRITE(fname,9001) mon(month)
      ELSE
        WRITE(fname,9002) mon(month)
      ENDIF
 9001 FORMAT("metinput.0",I1)
 9002 FORMAT("metinput.",I2)
      OPEN(10, FILE=fname, FORM="unformatted")
 9010 FORMAT("obsd.",I1)
 9011 FORMAT("obsd.",I2)
 9012 FORMAT("obsd.",I3)

      DO tau = 1, steps(month)
        IF ( mod(tau-1, 4) .EQ. 0) THEN
          dayno = dayno + 1
          IF (dayno .LT. 10) WRITE(fname,9010) dayno
          IF (dayno .GE. 10 .AND. dayno .LT. 100 ) THEN
                 WRITE(fname,9011) dayno
          ENDIF
          IF (dayno .GE. 100) WRITE(fname,9012) dayno
          OPEN(11,FILE=fname,FORM="unformatted")
          oldice = newice
          READ(11) newice
          CLOSE(11)
        ENDIF

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

C     If dayno is the same as the date of first appearance of ice, set ice
C       to 10 cm and cold.
CD          IF (first(i,j) .EQ. dayno) THEN
C         If ice today, but not yesterday, initialize
          IF (newice(i,j) .GT. 0. .AND. oldice(i,j) .EQ. 0) THEN
            hs(i,j) =  0.0
            hi(i,j) =  0.10
            t1(i,j) = -1.8
            t2(i,j) = -1.8
            ts(i,j) =  0.0
          ENDIF

C         Skip land points and not-yet iced points:
          IF (land(i,j) .GT. 128 .OR. hi(i,j) .EQ. 0.0 ) CYCLE 
CD          IF (albedo(i,j) .LT. 0.5 .AND. newice(i,j) .GT. 0.15) THEN
CD            PRINT *,'albedo ',i,j,dayno,albedo(i,j),newice(i,j)
CD          ENDIF

C     Compute the dflux/dtemp term, pass as hfd
          hfd = dfluxdtemp(hi(i,j), hs(i,j), ugrd(i,j), vgrd(i,j), 
     1                      ts(i,j)+273.15)

          snow = prate(i,j) / 330.  !sbr wants m/s, prate is kg/m^2/s
          sol  = - ( alpha_sw*(swdown(i,j) - swup(i,j)) + 
     1               alpha_lw*(lwdown(i,j) - lwup(i,j)) - beta_rad   )
          hf   = - (alpha_sh*shtfl(i,j) + alpha_lh*lhtfl(i,j) 
     1                - beta_bndy) 
          sum_rad(i,j) = sum_rad(i,j) + sol
          sum_sen(i,j) = sum_sen(i,j) + hf
          IF (hi(i,j) .LT. thinice) THEN
            t1(i,j) = -1.8
            t2(i,j) = -1.8
            ts(i,j) =  0.0
            hs(i,j) = hs(i,j) + snow*dt
            hi(i,j) = hi(i,j) + hf/rhoi/lf*dt
            IF (hs(i,j) .LT. 0.0) THEN
              hs(i,j) = 0.
            ENDIF
            IF (hi(i,j) .LE. 0.0) THEN
              hi(i,j) = 0.
              hs(i,j) = 0.
            ENDIF
          ELSE
            CALL ice3lay(hs(i,j), hi(i,j), t1(i,j), t2(i,j), ts(i,j), 
     1                   hf, sol, hfd, fb, snow, dt, tmelt, bmelt)
          ENDIF

          IF (hi(i,j) .GT. 0) THEN
            count(i,j) = count(i,j) + 1
            imax(i,j) = max(imax(i,j), hi(i,j) )
            smax(i,j) = max(smax(i,j), hs(i,j) )
          ENDIF

        ENDDO !i
        ENDDO !j


      ENDDO !tau
        CLOSE(10)
      ENDDO !months
      WRITE(fname,9003) years
 9003 FORMAT("output.0",I1)
      OPEN(12,FILE=fname,FORM="unformatted")
      count = count / 4.
      WRITE(12) sum_rad, sum_sen, hs, hi, imax, smax, count
      CLOSE(12)
      ENDDO !years
 9000 FORMAT (I2,I5,F7.4,F7.4,F7.2,F6.2, F6.2, 3F7.2, F7.2)

      k = 1
      DO j = 1, ny
      DO i = 1, nx
        IF (count(i,j) .NE. 0 .OR. days(i,j) .NE. 0) THEN
          vcount(k) = count(i,j)
          vdays(k) = days(i,j)
          k = k + 1
CD          WRITE (*,9005) i, j, count(i,j), days(i,j)
        ENDIF
      ENDDO
      ENDDO
      k = k - 1
      CALL correl(vcount, vdays, k, r2, xbar, ybar, sig2x, sig2y)
      WRITE (*,9006) alpha_sw, alpha_lw, beta_rad, alpha_sh, alpha_lh,
     1                beta_bndy,
     1                r2, (xbar-ybar)/ybar, xbar, 
     2                 (sig2x-sig2y)/sig2y, sig2x 
 9005 FORMAT(2I4,2I5)
 9006 FORMAT(2(2F7.3,F7.1),F7.3,F7.3,F7.2,F7.3,F7.1)


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
