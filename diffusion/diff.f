      PROGRAM diff
      IMPLICIT none

      INTEGER nz
      REAL dz
      PARAMETER (nz = 100)
CD      PARAMETER (dz = 0.1)
      REAL kappa, rho
      PARAMETER (kappa = 2.2)
      PARAMETER (rho   = 917.0)
      REAL cp, lf, tfreez
      PARAMETER (cp  = 4.e3)
      PARAMETER (lf  = 334e3)
      PARAMETER (tfreez = -1.8)
      REAL dt
      REAL thick

C Temperature kept at 2 time levels
      REAL t(nz,2)
      REAL frad(nz)
      REAL q0, sum, s0
      PARAMETER (q0 = -3e2)

      INTEGER i,k, n, np, nday, perday

      READ (*,*) nday
      READ (*,*) dt
      perday = 86400/dt
      dz = 0.01

      DO k = 1, 2
      DO i = 1, nz
        t(i,k) = tfreez
      ENDDO
      ENDDO
      s0 = 0
      DO i = 1, nz
        s0 = s0 + t(i,1)*rho*cp*dz
      ENDDO
      PRINT *,'initial heat ',s0

      READ (*,*) frad(1)
      DO i = 2, nz
        frad(i) = frad(i-1)*(1.-1.*dz)
      ENDDO

      np = 1
      n = 2
      thick = nz*dz
      DO k = 1, nday*perday
        i = n
        n = np
        np = i
        t(1,np) = t(1,n) + (q0*dt)/(rho*cp*dz)
     1        - (t(1,n)-t(2,n))*(kappa*dt)/(rho*cp*dz*dz)
     2        + dt*(frad(1)-frad(2))/(rho*cp*dz)
        DO i = 2, nz-1
          t(i,np) = t(i,n) + (kappa*dt)/(rho*cp*dz*dz) 
     1               * (t(i+1,n)-2.*t(i,n)+t(i-1,n))
     2        + dt*(frad(i-1)-frad(i+1))/(rho*cp*dz*2)
        ENDDO
        t(nz,np) = tfreez
        thick = thick + (t(nz,np)-t(nz-1,np))*(kappa/dz)/(rho*lf)*dt
        dz = thick/nz
C       Note that this is being done without redistribution of T,
C         fails energy conservation
        IF (MOD(k*dt,60.) .EQ. 0) THEN
          sum = 0
          DO i = 1, nz
            sum = sum + t(i,np)*dz*rho*cp
          ENDDO
          PRINT *,k*dt,t(1,np), t(nz/2,np), (sum-s0), thick
        ENDIF
      ENDDO

      PRINT *,q0,(t(1,np)-t(nz,np))*kappa/((nz-1)*dz), 
     1           (t(1,np)-t(2,np))*kappa/dz
      DO i = 1, nz
        PRINT *, i, t(i,np), frad(i)
      ENDDO


      STOP
      END
