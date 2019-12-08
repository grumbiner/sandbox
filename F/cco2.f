C*************************************************----------++++++++++!!
      FUNCTION dco2(tco2, alk, temp, rsalt, depth)
C     Compute the concentration of co2 (Moles/kg) given
C       the total co2, alkalinity, temperature, salinity,
C       and depth.
C     8/10/91. BG.
C     Fourth iteration added 10/24/92. BG
      IMPLICIT none

      REAL revell
      COMMON /carbon/ revell


      DOUBLE PRECISION dco2
      DOUBLE PRECISION tco2, alk, temp, depth, salt
      REAL rsalt

      DOUBLE PRECISION  k1,  k2, tk
      DOUBLE PRECISION x, eps, z, dummy
      DOUBLE PRECISION pk1, pk2, pk10, pk20
      DOUBLE PRECISION beta, tempor
      DOUBLE PRECISION tb, kb, nkb, nkb0
      DOUBLE PRECISION ystar, co2old, enew, co2str

      beta = DLOG(10.D0)
      tk = temp+273.15D0
      salt = DBLE(rsalt)
      tb   = rsalt*12.1E-6

C     1987 Unesco values:
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
      dummy = 1. - 4.*k2/k1

C     Make a first guess at the borate contribution to alkalinity.
      eps = tb/(1. + (k2/kb)*8.)

      z = alk - eps

      x = tco2 - SQRT(tco2*tco2-(2.*tco2-z)*z*dummy)
      x = x / dummy

      co2str = k2*2.*x*x/(alk-x-eps)/k1
      ystar  = tco2 - x - co2str

CD      PRINT *,'eps, kb, k2, k2/kb',
CD     1         eps, kb, k2, k2/kb

C     Second Guess.  Good to about <1% in co2, 10% in borate.
      enew = tb/(1. + (k2/kb)*x/ystar)
      z    = alk - enew

      x = tco2 - SQRT(tco2*tco2-(2.*tco2-z)*z*dummy)
      x = x / dummy

      co2str = k2*2.*x*x/(alk-x-enew)/k1
      ystar  = tco2 - x - co2str

C     Third try.  Good to < 0.3% in co2, 1% in borate
      enew = tb/(1. + (k2/kb)*x/ystar)
      z    = alk - enew

      x = tco2 - SQRT(tco2*tco2-(2.*tco2-z)*z*dummy)
      x = x / dummy

      co2str = k2*2.*x*x/(alk-x-enew)/k1
      ystar  = tco2 - x - co2str

C     Fourth try.  Good to ? in co2, ? in borate
      enew = tb/(1. + (k2/kb)*x/ystar)
      z    = alk - enew

      x = tco2 - SQRT(tco2*tco2-(2.*tco2-z)*z*dummy)
      x = x / dummy

      co2str = k2*2.*x*x/(alk-x-enew)/k1
      ystar  = tco2 - x - co2str

C     Fourth try.  Good to ? in co2, ? in borate
      enew = tb/(1. + (k2/kb)*x/ystar)
      z    = alk - enew

      x = tco2 - SQRT(tco2*tco2-(2.*tco2-z)*z*dummy)
      x = x / dummy

      co2str = k2*2.*x*x/(alk-x-enew)/k1
      ystar  = tco2 - x - co2str

C     Fourth try.  Good to ? in co2, ? in borate
      enew = tb/(1. + (k2/kb)*x/ystar)
      z    = alk - enew

      x = tco2 - SQRT(tco2*tco2-(2.*tco2-z)*z*dummy)
      x = x / dummy

      co2str = k2*2.*x*x/(alk-x-enew)/k1
      ystar  = tco2 - x - co2str
C     Fourth try.  Good to ? in co2, ? in borate
      enew = tb/(1. + (k2/kb)*x/ystar)
      z    = alk - enew

      x = tco2 - SQRT(tco2*tco2-(2.*tco2-z)*z*dummy)
      x = x / dummy

      co2str = k2*2.*x*x/(alk-x-enew)/k1
      ystar  = tco2 - x - co2str

C     Fourth try.  Good to ? in co2, ? in borate
      enew = tb/(1. + (k2/kb)*x/ystar)
      z    = alk - enew

      x = tco2 - SQRT(tco2*tco2-(2.*tco2-z)*z*dummy)
      x = x / dummy

      co2str = k2*2.*x*x/(alk-x-enew)/k1
      ystar  = tco2 - x - co2str

C     Fourth try.  Good to ? in co2, ? in borate
      enew = tb/(1. + (k2/kb)*x/ystar)
      z    = alk - enew

      x = tco2 - SQRT(tco2*tco2-(2.*tco2-z)*z*dummy)
      x = x / dummy

      co2str = k2*2.*x*x/(alk-x-enew)/k1
      ystar  = tco2 - x - co2str

C     Now put the answer into the function variable
      dco2 = co2str

C     Compute an approximation to the revelle fastor:
      revell = tco2/ystar

      RETURN
      END