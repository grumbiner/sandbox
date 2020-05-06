      SUBROUTINE extrap(w, t, rho, nz, dz, dt)
C     extrapolate the variables to the next time step
C     Robert Grumbine 2 May 1995

      IMPLICIT NONE

      INTEGER nz
      REAL w(nz), t(nz), rho(nz)
      REAL dz, dt
      
      REAL gee, are, pnot, cp
      PARAMETER (gee  = 9.81)
      PARAMETER (are  = 287.04)
      PARAMETER (pnot = 1.01325E5)
      PARAMETER (cp   = 1004.)
      REAL tref, rhoref, pref
      PARAMETER (tref = 253.)
      
      REAL ch, ua, hb
      PARAMETER (ch = 1.E-3)
      PARAMETER (ua = 10.)
      PARAMETER (hb = 10.)
      
      REAL c1(500), c2(500), c3(500)
      REAL a1, a2, a3
      REAL sum1, sum2, sum3
      REAL f1, f2, f3
      INTEGER i, j, k
													
      f1(k) = +gee*t(k)/tref - are*(t(k+1)-t(k))/dz
     1 -(rho(k+1)-rho(k))/dz*(are*pnot)**2/pnot*exp(+gee*k*dz/are/tref)
     2 +gee*rho(k)/(pnot*exp(-gee*k*dz/are/tref)/are/tref)
      f2(k) = ch*ua*exp(-k*dz/hb)*(20.-t(k))/(1.+are/cp)/hb
      f3(k) = (w(k+1)+w(k))/2.*pnot*gee*
     1                   exp(-gee*k*dz/are/tref)/(are*tref)**2
  
      DO 1000 i = 2, nz-1
        c1(i) = f1(i) - w(i)*(w(i+1)-w(i-1))/2./dz
        c2(i) = f2(i) - w(i)*(t(i+1)-t(i))/dz
        c3(i) = f3(i) - w(i)*(rho(i+1)-rho(i))/dz
 1000 CONTINUE
      i = 1
      c1(i) = f1(i) - w(i)*w(i+1)/2./dz
      c2(i) = f2(i) - w(i)*(t(i+1)-t(i))/dz
      c3(i) = f3(i) - w(i)*(rho(i+1)-rho(i))/dz

      i = 1
      w(i)   = w(i)   + dt*c1(i)
      t(i)   = t(i)   + dt*c2(i)
      rho(i) = rho(i) + dt*c3(i)
      sum1 = w(1)/2
      sum2 = t(1)/2
      sum3 = rho(1)/2
      DO 2000 i = 2, nz-1
        w(i)   = w(i)   + dt*c1(i)
        t(i)   = t(i)   + dt*c2(i)
        rho(i) = rho(i) + dt*c3(i)
        sum1 = sum1 + w(i)
        sum2 = sum2 + t(i)
        sum3 = sum3 + rho(i)
 2000 CONTINUE
      WRITE (6,9001) sum1, sum2, sum3
 9001 FORMAT (' Integrate w, t, rho is: ', 3E13.6)
 
      a1 = 0.0
      a2 = 0.0
      a3 = 0.0
      DO 3000 i = 1, nz
        a1 = MAX(a1,   ABS(w(i)))
        a2 = MAX(a2,   ABS(t(i)))
        a3 = MAX(a3, ABS(rho(i)))
 3000 CONTINUE
      WRITE (6,9002) a1, a2, a3
 9002 FORMAT (' The max absolute values of w, t, rho were: ',3E13.6)
 
      a1 = 0.0
      a2 = 0.0
      a3 = 0.0
      DO 3100 i = 1, nz
        a1 = MAX(a1, ABS(c1(i)))
        a2 = MAX(a2, ABS(c2(i)))
        a3 = MAX(a3, ABS(c3(i)))
 3100 CONTINUE
      WRITE (6,9003) a1, a2, a3
 9003 FORMAT (' The max absolute values of change in w, t, rho were: '
     1                ,3E13.6)
 
      RETURN
      END
