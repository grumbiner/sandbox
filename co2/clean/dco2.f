C*************************************************----------++++++++++!!
      FUNCTION dco2(tco2, alk, temp, ssalt, depth, yinit, zinit)
C     Compute the concentration of co2 (Moles/kg) given
C       the total co2, alkalinity, temperature, salinity,
C       and depth.
C     8/10/91. BG.
C     Revise to a two-parameter Newton's method, with the last step's
C       concentrations of HCO3- and CO3-- as the first guess for this
C       step
      IMPLICIT none

      REAL revell
      COMMON /carbon/ revell

      REAL yinit, zinit
      DOUBLE PRECISION dco2
      DOUBLE PRECISION tco2, alk, temp, depth, salt
      REAL ssalt

      DOUBLE PRECISION yold, zold, y, z, tk

C     Physical chemistry equilibrium reaction parameters
      DOUBLE PRECISION  k1,  k2, kprime
      DOUBLE PRECISION pk1, pk2, pk10, pk20
      DOUBLE PRECISION tb, kb, nkb, nkb0

      DOUBLE PRECISION alpha, beta
      PARAMETER (alpha = 12.1D-6) ! total borate to salinity scaling

C     Statement functions in Newton's method solution	  
      DOUBLE PRECISION f1, f2, df1dy, df1dz, df2dy, df2dz
      DOUBLE PRECISION delta
      
C     Iteration parameters
      INTEGER itmax, iters
      DOUBLE PRECISION dely, delz, epsy, epsz
      PARAMETER (itmax = 15)
      PARAMETER (epsy = 1.E-9)
      PARAMETER (epsz = 1.E-9)
      DOUBLE PRECISION hplus
	
      hplus(y,z) = k2*y/z  
      f1(y,z) = z + y + y*y/kprime/z - tco2
      f2(y,z) = y + 2.*z + tb*kb*z/(kb*z+k2*y) - alk

      df1dy(y,z) = 1. + 2.*y/kprime/z
      df1dz(y,z) = 1. - y*y/z/z/kprime

      df2dy(y,z) = 1. - tb*kb*z*k2/(kb*z+k2*y)/(kb*z+k2*y)
      df2dz(y,z) = 2. + tb*kb     /(kb*z+k2*y) 
     1                - tb*kb*z*kb/(kb*z+k2*y)/(kb*z+k2*y)

CD      delta(y,z) = df1dy(y,z)*df2dz(y,z)-df1dz(y,z)*df2dy(y,z)
  
      beta = DLOG(10.D0)
  
      tk = temp+273.15D0
      salt = DBLE(ssalt)
      tb   = salt*alpha

CD      PRINT *, tk, salt, tco2, alk

C     1987 Unesco values for equilibrium reactions:
      pk10 =  6320.81/tk - 126.3405 + 19.568* LOG(tk)
      pk20 =  5143.69/tk -  90.1833 + 14.613* LOG(tk)
      nkb0 = -8966.90/tk + 148.0248 - 24.4344*LOG(tk)

      pk1 = pk10 + 0.0068*salt +
     1 (19.894 - 840.39/tk - 3.0189*LOG(tk))*SQRT(salt)

      pk2 = pk20 + 0.0217*salt +
     1 (17.176 - 690.59/tk - 2.6719*LOG(tk))*SQRT(salt)

      nkb = nkb0 - 0.01767*salt +
     1 (0.5998 -  75.25/tk )*SQRT(salt)

      k1 = DEXP(-beta*pk1)
      k2 = DEXP(-beta*pk2)
      kb = DEXP(nkb)
      kprime = k1/k2

C     Now conduct the Newton's method iterations
      yold = DBLE(yinit)
      zold = DBLE(zinit)
      iters = 0

 1000 CONTINUE
        iters = iters+1
        delta = df1dy(yold,zold)*df2dz(yold,zold) - 
     1          df1dz(yold,zold)*df2dy(yold,zold)
        dely = -(f1(yold,zold)*df2dz(yold,zold) 
     1         - f2(yold,zold)*df1dz(yold,zold) ) /delta
        delz = -(f2(yold,zold)*df1dy(yold,zold) 
     1         - f1(yold,zold)*df2dy(yold,zold) ) /delta
        yold = yold + dely
        zold = zold + delz
CD        PRINT *,iters, dely, delz, yold, zold, 
CD     1    f1(yold, zold), f2(yold, zold)
       
      IF ((ABS(dely) .GT. epsy .OR. ABS(delz) .GT. epsz )
     1                       .AND. (iters .LT. itmax))  GO TO 1000
      IF (iters .GE. itmax) THEN
        PRINT *,iters, dely, delz, yold, zold,
     1    f1(yold, zold), f2(yold, zold)
      ENDIF
CD      PRINT *,iters

C     Now put the answer into the function variable
      yinit = yold
      zinit = zold
      dco2  = yold*yold/kprime/zold

C     Compute an approximation to the revelle factor:
CD      revell = tco2/yold* (- 2./df1dy(yold,zold)
CD     1             +  yold/zold/df1dz(yold,zold)     )
CD      revell = tco2/dco2*(2.*yold/kprime/zold/df1dy(yold,zold) 
CD     1           - yold*yold/kprime/zold/zold/df1dz(yold,zold) )
CD      revell = tco2 / zinit

      RETURN
      END