      SUBROUTINE airtau(ua, va, a, b, mass, nbods)
C     Compute air-ice stress BG 1/21/92.
      IMPLICIT none

      INTEGER nbods
      REAL ua(nbods), va(nbods), mass(nbods), a(nbods), b(nbods)

      REAL rhoa, ca, phi
      REAL pi
      PARAMETER (pi = 3.141592654)
      PARAMETER (rhoa = 1.2)
      PARAMETER (ca   = 3.E-3)
      PARAMETER (phi  = 20./90.*pi/2.)

      INTEGER i

      DO 1000 i = 1 , nbods
        a(i) = rhoa*ca*SQRT(ua(i)*ua(i)+va(i)*va(i))*
     1          (ua(i)*cos(phi)-va(i)*sin(phi))
        b(i) = rhoa*ca*SQRT(ua(i)*ua(i)+va(i)*va(i))*
     2          (va(i)*cos(phi)+ua(i)*sin(phi))
 1000 CONTINUE

      DO 2000 i = 1, nbods
        a(i) = a(i)/mass(i)
        b(i) = b(i)/mass(i)
 2000 CONTINUE

      RETURN
      END
      SUBROUTINE coriol(x, y, u, v, a, b, nbods)
C     Coriolis deflection

      IMPLICIT none
      INTEGER nbods
      REAL x(nbods), y(nbods), u(nbods), v(nbods)
      REAL a(nbods), b(nbods)

      INTEGER i
      REAL f, beta
      REAL lat, pi
      REAL omega, radius
      PARAMETER (omega = 7.292E-5)
      PARAMETER (radius = 6.371E6)
      PARAMETER (pi    = 3.141592654)
C     Note that the following assumes a fixed reference latitude.
C       This should be generalized.
      PARAMETER (lat   = 67.0)

      f    = 2.*omega*SIN(lat*pi/180.)
      beta = f/radius/TAN(lat*pi/180.)
CD      PRINT *,'f, beta ', f, beta

      DO 1000 i = 1, nbods
        a(i) = -(f + beta*y(i))*v(i)
        b(i) =  (f + beta*y(i))*u(i)
 1000 CONTINUE

      RETURN
      END
C********!*********!*********!*********!*********!*********!---------!++
      SUBROUTINE diagns(x, y, u, v, rot, step, mass, rinert, nbods)
C     Compute diagnostics for an n-body simulation.

      IMPLICIT none
      INTEGER nbods, step
      REAL x(nbods), y(nbods), u(nbods), v(nbods), rot(nbods)
      REAL mass(nbods), rinert(nbods)

      INTEGER i
      REAL xbar, ubar, rotbar, ybar, vbar
      REAL sum(10)

      DO 100 i = 1, 5
        sum(i) = 0.0
  100 CONTINUE
      DO 110 i = 1, nbods
        sum(1) = sum(1) + x(i)
        sum(2) = sum(2) + y(i)
        sum(3) = sum(3) + u(i)
        sum(4) = sum(4) + v(i)
        sum(5) = sum(5) + rot(i)
  110 CONTINUE
      DO 120 i = 1, 5
        sum(i) = sum(i)/nbods
  120 CONTINUE
      xbar = sum(1)
      ybar = sum(2)
      ubar = sum(3)
      vbar = sum(4)
      rotbar = sum(5)

      DO 199 i = 1, 7
        sum(i) = 0.0
  199 CONTINUE

      DO 200 i = 1, nbods
        sum(1) = sum(1) + mass(i)*x(i)
        sum(2) = sum(2) + mass(i)*y(i)
        sum(3) = sum(3) + mass(i)*u(i)
        sum(4) = sum(4) + mass(i)*v(i)
        sum(5) = sum(5) + rinert(i)*rot(i)
  200 CONTINUE

C     Mean Kinetic energy
      DO 210 i = 1, nbods
        sum(6) = sum(6) + mass(i)*(u(i)*u(i)+v(i)*v(i))
  210 CONTINUE

C     Thermal kinetic energy
      DO 220 i = 1, nbods
        sum(7) = sum(7) + mass(i)*((u(i)-ubar)**2+(v(i)-vbar)**2)
  220 CONTINUE

C     DO 230 i = 1, 7
        sum(i) = sum(i)/nbods
  230 CONTINUE

      WRITE (*,9001) step, sum(1)/1.E7, sum(2)/1.E7, sum(3)/1.E4,
     1     sum(4)/1.E4, sum(5)/1.E4,
     2     CHAR(9),sum(6)/1.E4, CHAR(9),sum(7)/1.E4
      WRITE (10,9001) step, sum(1)/1.E7, sum(2)/1.E7, sum(3)/1.E4,
     1     sum(4)/1.E4, sum(5)/1.E4,
     2     CHAR(9),sum(6)/1.E4, CHAR(9),sum(7)/1.E4
CD      PRINT *,xbar, ybar, ubar, vbar, rotbar

C 9001 FORMAT (I4, 2F7.1, 2F5.2, E9.3, 2(A1,F9.2))
 9001 FORMAT (I8, 2F8.2, 3F10.5,2(A1,F10.5))

      RETURN
      END
      SUBROUTINE extrap(xnm1, xn, xnp1, ynm1, yn, ynp1,
     1                  unm1, un, unp1, vnm1, vn, vnp1,
     2                  rotnm1, rotn, rotnp1,
     3                  rad, thick, mass, rinert, deltat,
     4                  ua, va, uo, vo, dtopo, nbods)
      IMPLICIT none

      INTEGER nbods
      REAL deltat
      REAL xnm1(nbods), ynm1(nbods), unm1(nbods), vnm1(nbods)
      REAL rotnm1(nbods)
      REAL xn(nbods), yn(nbods), un(nbods), vn(nbods)
      REAL rotn(nbods)
      REAL xnp1(nbods), ynp1(nbods), unp1(nbods), vnp1(nbods)
      REAL rotnp1(nbods)
      REAL mass(nbods), rad(nbods), thick(nbods), rinert(nbods)
      REAL ua(nbods), va(nbods), uo(nbods), vo(nbods)
      REAL dtopo(nbods)

      INTEGER nmax
      PARAMETER (nmax = 1024*128)
C    !This should be an include, rather than a statement.
      REAL fa(nmax), fb(nmax), ttorq(nmax)
      INTEGER i

      CALL forces(fa, fb, nbods, xn, yn, un, vn, rotn,
     1  mass, rad, thick,
     2  ua, va, uo, vo, dtopo)
      CALL torqs (ttorq, nbods, rotn, mass, rad, thick, rinert,
     1            xn, vn, un, vn)

C     Do the extrapolation
      DO 1000 i = 1, nbods
        xnp1(i)   = xnm1(i)   + 2.*deltat*un(i)
        ynp1(i)   = ynm1(i)   + 2.*deltat*vn(i)
        unp1(i)   = unm1(i)   + 2.*deltat*fa(i)
        vnp1(i)   = vnm1(i)   + 2.*deltat*fb(i)
 1000 CONTINUE

CD      DO 2000 i = 1, nbods
CD        rotnp1(i) = rotnm1(i) + 2.*deltat*ttorq(i)
CD 2000 CONTINUE

      RETURN
      END
      SUBROUTINE finter(x, y, u, v, rot, a, b, nbods)
C     Compute the floe-floe interaction forces in linear
C       momentum exchange.  BG 1/21/92.  --postpone for later.

      INTEGER nbods
      REAL x(nbods), y(nbods), u(nbods), v(nbods), rot(nbods)
      REAL a(nbods), b(nbods)

      INTEGER i

      DO 1000 i = 1, nbods
        a(i) = 0.0
        b(i) = 0.0
 1000 CONTINUE

      RETURN
      END
      SUBROUTINE forces(fa, fb, nbods, xn, yn, un, vn, rotn,
     1                  mass, rad, thick,
     2                  ua, va, uo, vo, dtopo)

      IMPLICIT none
      INTEGER nbods
      REAL xn(nbods), yn(nbods), un(nbods), vn(nbods), rotn(nbods)
      REAL mass(nbods), rad(nbods), thick(nbods)
      REAL ua(nbods), va(nbods), uo(nbods), vo(nbods), dtopo(nbods)
      REAL fa(nbods), fb(nbods)

      INTEGER i
      INTEGER nmax
      PARAMETER (nmax = 1024*128)
      REAL a(nmax), b(nmax)
      REAL xmax, ymax

      DO 100 i = 1, nbods
        fa(i) = 0.0
        fb(i) = 0.0
  100 CONTINUE

      CALL airtau(ua, va, a, b, mass, nbods)
      DO 200 i = 1, nbods
        fa(i) = fa(i) + a(i)
        fb(i) = fb(i) + b(i)
  200 CONTINUE
C     Check for what the maximum forces are
CD      xmax = 0.
CD      ymax = 0.
CD      DO 1000 i = 1, nbods
CD        xmax = AMAX1(xmax, a(i) )
CD        ymax = AMAX1(ymax, b(i) )
CD 1000 CONTINUE
CD      PRINT *,'max forces in x, y in airtau ', xmax, ymax

      CALL ocetau(uo, vo, un, vn, mass, a, b, nbods)
      DO 300 i = 1, nbods
        fa(i) = fa(i) + a(i)
        fb(i) = fb(i) + b(i)
  300 CONTINUE
C     Check for what the maximum forces are
CD      xmax = 0.
CD      ymax = 0.
CD      DO 1100 i = 1, nbods
CD        xmax = AMAX1(xmax, ABS(a(i)) )
CD        ymax = AMAX1(ymax, ABS(b(i)) )
CD 1100 CONTINUE
CD      PRINT *,'max forces in x, y in ocetau ', xmax, ymax

      CALL formdr(uo, vo, un, vn, a, b, rad, thick, mass, nbods)
      DO 400 i = 1, nbods
        fa(i) = fa(i) + a(i)
        fb(i) = fb(i) + b(i)
  400 CONTINUE
C     Check for what the maximum forces are
CD      xmax = 0.
CD      ymax = 0.
CD      DO 1200 i = 1, nbods
CD        xmax = AMAX1(xmax, a(i) )
CD        ymax = AMAX1(ymax, b(i) )
CD 1200 CONTINUE
CD      PRINT *,'max forces in x, y in formdr ', xmax, ymax

      CALL coriol(xn, yn, un, vn, a, b, nbods)
      DO 500 i = 1, nbods
        fa(i) = fa(i) + a(i)
        fb(i) = fb(i) + b(i)
  500 CONTINUE
C     Check for what the maximum forces are
CD      xmax = 0.
CD      ymax = 0.
CD      DO 1300 i = 1, nbods
CD        xmax = AMAX1(xmax, ABS(a(i)) )
CD        ymax = AMAX1(ymax, ABS(b(i)) )
CD 1300 CONTINUE
CD      PRINT *,'max forces in x, y in coriol', xmax, ymax

      CALL topog (xn, yn, dtopo, a, b, nbods)
      DO 600 i = 1, nbods
        fa(i) = fa(i) + a(i)
        fb(i) = fb(i) + b(i)
  600 CONTINUE
C     Check for what the maximum forces are
CD      xmax = 0.
CD      ymax = 0.
CD      DO 1400 i = 1, nbods
CD        xmax = AMAX1(xmax, a(i) )
CD        ymax = AMAX1(ymax, b(i) )
CD 1400 CONTINUE
CD      PRINT *,'max forces in x, y in topog ', xmax, ymax

      CALL finter(xn, yn, un, vn, rotn, a, b, nbods)
      DO 700 i = 1, nbods
        fa(i) = fa(i) + a(i)
        fb(i) = fb(i) + b(i)
  700 CONTINUE
C     Check for what the maximum forces are
CD      xmax = 0.
CD      ymax = 0.
CD      DO 1500 i = 1, nbods
CD        xmax = AMAX1(xmax, a(i) )
CD        ymax = AMAX1(ymax, b(i) )
CD 1500 CONTINUE
CD      PRINT *,'max forces in x, y in finter', xmax, ymax

C     Check for what the maximum forces are
CD      xmax = 0.
CD      ymax = 0.
CD      DO 1600 i = 1, nbods
CD        xmax = AMAX1(xmax, ABS(fa(i)) )
CD        ymax = AMAX1(ymax, ABS(fb(i)) )
CD 1600 CONTINUE
CD      PRINT *,'max forces in x, y ', xmax, ymax

      RETURN
      END
      SUBROUTINE formdr(uo, vo, ui, vi, a, b,
     1                  rad, thick, mass, nbods)
C     Compute the form drag addition to stress on the ice floes,
C       after Steele, et al., 1989.
      IMPLICIT none

      INTEGER nbods
      REAL uo(nbods), vo(nbods), ui(nbods), vi(nbods)
      REAL a(nbods), b(nbods), rad(nbods), thick(nbods), mass(nbods)

      REAL rho, lf, gamma
      PARAMETER (rho = 1025.)
      PARAMETER (lf = 10.)
C     lf should be computed from the ice information ! BG 1/21/92.

      INTEGER i

      DO 1000 i = 1, nbods
C        PRINT *,'formdr, i= ',i
        gamma = (1.-(thick(i)/lf))**2
        a(i) = 0.5*gamma*rho*(thick(i)/rad(i))*
     1         SQRT( (uo(i)-ui(i))**2+(vo(i)-vi(i))**2 )*
     2         (uo(i)-ui(i))
        b(i) = 0.5*gamma*rho*(thick(i)/rad(i))*
     1         SQRT( (uo(i)-ui(i))**2+(vo(i)-vi(i))**2 )*
     2         (vo(i)-vi(i))
 1000 CONTINUE

      DO 2000 i = 1, nbods
        a(i) = a(i)/mass(i)
        b(i) = b(i)/mass(i)
 2000 CONTINUE

      RETURN
      END
      SUBROUTINE frstdt(xnm1, xn, ynm1, yn, unm1, un,
     1                  vnm1, vn, rotnm1, rotn,
     2                  rad, thick, mass, rinert, deltat,
     3                  ua, va, uo, vo, dtopo, nbods)

      INTEGER nbods
      REAL deltat
      REAL xnm1(nbods), ynm1(nbods), unm1(nbods), vnm1(nbods)
      REAL rotnm1(nbods)
      REAL xn(nbods), yn(nbods), un(nbods), vn(nbods)
      REAL rotn(nbods)
      REAL mass(nbods), rad(nbods), thick(nbods), rinert(nbods)
      REAL ua(nbods), va(nbods), uo(nbods), vo(nbods)
      REAL dtopo(nbods)

      INTEGER nmax
      PARAMETER (nmax = 1024*128)
C    !This should be an include, rather than a statement.
      REAL fa(nmax), fb(nmax), ttorq(nmax)

      CALL forces(fa, fb, nbods, xnm1, ynm1, unm1, vnm1,
     1            rotnm1, mass, rad, thick,
     2            ua, va, uo, vo, dtopo)
      CALL torqs (ttorq, nbods, rotnm1, mass, rad, thick, rinert,
     1            xnm1, ynm1, unm1, vnm1)

C     Do the extrapolation
      DO 1000 i = 1, nbods
        xn(i) = xnm1(i) + 1.*deltat*unm1(i)
        yn(i) = ynm1(i) + 1.*deltat*vnm1(i)
        un(i) = unm1(i) + 1.*deltat*fa(i)
        vn(i) = vnm1(i) + 1.*deltat*fb(i)
        rotn(i) = rotnm1(i) + 1.*deltat*ttorq(i)
 1000 CONTINUE

      RETURN
      END
      PROGRAM nbody
C     Main program for conducting n-body simulations.
C     Begun 1/14/92 by BG.

      IMPLICIT none
      INTEGER nmax
      PARAMETER (nmax = 1024*128)

      REAL xa(nmax), xb(nmax), xc(nmax)
      REAL ya(nmax), yb(nmax), yc(nmax)
      REAL ua(nmax), ub(nmax), uc(nmax)
      REAL va(nmax), vb(nmax), vc(nmax)
      REAL rota(nmax), rotb(nmax), rotc(nmax)
      REAL rad(nmax), thick(nmax), mass(nmax), rinert(nmax)
      REAL uatm(nmax), vatm(nmax), uoce(nmax), voce(nmax)
      REAL dtopo(nmax)

      REAL deltat
      INTEGER ndt, nbods
      INTEGER i

      CALL setup(xa, xb, xc, ya, yb, yc,
     1           ua, ub, uc, va, vb, vc,
     2           rota, rotb, rotc,
     3           rad, thick, mass, rinert,
     4           deltat, ndt,
     5           uatm, vatm, uoce, voce, dtopo,
     6                             nbods, nmax  )

      CALL frstdt(xa, xb, ya, yb, ua, ub, va, vb, rota, rotb,
     1           rad, thick, mass, rinert, deltat,
     2           uatm, vatm, uoce, voce, dtopo, nbods     )
      CALL diagns(xb, yb, ub, vb, rotb, 0, mass, rinert, nbods)

C********!*********!*********!*********!*********!*********!---------!++
      DO 1000 i = 1, ndt/3

        CALL extrap(xa, xb, xc, ya, yb, yc,
     1              ua, ub, uc, va, vb, vc,
     2              rota, rotb, rotc,
     3              rad, thick, mass, rinert,
     4              deltat,
     5              uatm, vatm, uoce, voce, dtopo, nbods)

        CALL diagns(xc, yc, uc, vc, rotc, 3*i-2, mass, rinert, nbods)

        CALL extrap(xb, xc, xa, yb, yc, ya,
     1              ub, uc, ua, vb, vc, va,
     2              rotb, rotc, rota,
     3           rad, thick, mass, rinert,
     4           deltat,
     2           uatm, vatm, uoce, voce, dtopo, nbods)

        CALL diagns(xa, ya, ua, va, rota, 3*i-1, mass, rinert, nbods)

        CALL extrap(xc, xa, xb, yc, ya, yb,
     1              uc, ua, ub, vc, va, vb,
     2              rotc, rota, rotb,
     3           rad, thick, mass, rinert,
     4           deltat,
     2           uatm, vatm, uoce, voce, dtopo, nbods)

        CALL diagns(xb, yb, ub, vb, rotb, 3*i, mass, rinert, nbods)

 1000 CONTINUE
      PRINT *,LONG(362)
      
      PAUSE
      END
      SUBROUTINE ocetau(uo, vo, ui, vi, mass, a, b, nbods)
C     Compute ocean-ice stress BG 1/21/92.
      IMPLICIT none

      INTEGER nbods
      REAL uo(nbods), vo(nbods), a(nbods), b(nbods)
      REAL ui(nbods), vi(nbods), mass(nbods)

      REAL rhoo, co, phi
      REAL pi
      PARAMETER (pi = 3.141592654)
      PARAMETER (rhoo = 1.025E3)
      PARAMETER (co   = 6.E-3)
      PARAMETER (phi  = 20.*pi/180.)

      INTEGER i

      DO 1000 i = 1 , nbods
        a(i) = rhoo*co*SQRT((ui(i)-uo(i))**2+(vi(i)-vo(i))**2)*
     1          ((uo(i)-ui(i))*cos(phi)-(vo(i)-vi(i))*sin(phi))
        b(i) = rhoo*co*SQRT((ui(i)-uo(i))**2+(vi(i)-vo(i))**2)*
     1          ((vo(i)-vi(i))*cos(phi)+(uo(i)-ui(i))*sin(phi))
 1000 CONTINUE

      DO 2000 i = 1, nbods
        a(i) = a(i)/mass(i)
        b(i) = b(i)/mass(i)
 2000 CONTINUE

      RETURN
      END
      SUBROUTINE omdrag(rot, rinert, nbods, a)
C     Compute the rotational drag on a rotating floe (disk)
C       in a viscous fluid (ocean).  BG 1/21/92.
C     Prescribe a damping time.  BG 1/21/92.

      IMPLICIT none
      INTEGER nbods
      REAL rot(nbods), rinert(nbods), a(nbods)

      REAL alpha
      PARAMETER (alpha = -1.E-4)

      INTEGER i

      DO 1000 i = 1, nbods
        a(i) = alpha*rot(i)
 1000 CONTINUE

      RETURN
      END
      FUNCTION rand(x)
C     Generate uniformly random numbers on the
C       real interval -1, 1 using a linear congruential
C       generator.  Parameters taken from Numerical
C       recipes, p. 198.
C     BG 1/22/92.

      INTEGER m, a, c
      PARAMETER (m = 134456)
      PARAMETER (a = 205   )
      PARAMETER (c = 29573 )

      INTEGER x
      REAL rand

      x = MOD(a*x+c, m)
      rand = (FLOAT(x)/FLOAT(m)-0.5)*2.
CD      PRINT *,'rand', rand

      RETURN
      END
      SUBROUTINE setup(xa, xb, xc, ya, yb, yc,
     1                 ua, ub, uc, va, vb, vc,
     2                 rota, rotb, rotc,
     3                 rad, thick, mass, rinert,
     4                 deltat, ndt,
     5                 uatm, vatm, uoce, voce, dtopo,
     6                                   nbods, nmax )

      IMPLICIT none
      INTEGER nmax
      REAL xa(nmax), xb(nmax), xc(nmax)
      REAL ya(nmax), yb(nmax), yc(nmax)
      REAL ua(nmax), ub(nmax), uc(nmax)
      REAL va(nmax), vb(nmax), vc(nmax)
      REAL rota(nmax), rotb(nmax), rotc(nmax)
      REAL rad(nmax), thick(nmax), mass(nmax), rinert(nmax)
      REAL uatm(nmax), vatm(nmax), uoce(nmax), voce(nmax)
      REAL dtopo(nmax)

      REAL deltat
      INTEGER ndt, nbods

      REAL rnot, hnot, trun
      REAL rhoi, pi
      PARAMETER (rhoi = 917.)
      PARAMETER (pi  = 3.141592654)

      REAL speed, nops
C      PARAMETER (speed = 0.1E6) ! Mac IIx
C      PARAMETER (speed = 30.E6) ! Cray Y-MP (flops)
      PARAMETER (speed = 1800.E6) ! Macpro ~2014
      PARAMETER (nops  = 200. ) !linear term in operations/step/body
      INTEGER i
      REAL xmax, umax, rotmax
      REAL rand
      INTEGER seed

 100  PRINT *,'How many bodies?'
      READ (*,9001) nbods
      IF (nbods .GT. nmax) THEN
        PRINT *,'You have taken more than the limit.'
        PRINT *,'Please enter a number less than ',nmax
        GO TO 100
      ENDIF
      PRINT *,'DT = ?'
      READ (*,9002) deltat
      PRINT *,'How long a simulation?'
      READ (*,9002) trun
      ndt = INT(0.5 + trun/deltat)

      PRINT *,'Reference floe radius?'
      READ (*,9002) rnot
      PRINT *,'Reference thickness?'
      READ (*,9002) hnot
      DO 1000 i = 1, nbods
        rad(i)    = rnot
        thick(i)  = hnot
        mass(i)   = pi*rnot*rnot*hnot*rhoi
        rinert(i) = mass(i) *.5*pi*rnot*rnot
 1000 CONTINUE

C     Now set up the random locations, speeds, and rotations
      seed = 1
      PRINT *,'What is the bound of the square?'
      READ (*,9002) xmax
      WRITE (*,9002) xmax
      PRINT *,'What is the bound on speed?'
      READ (*,9002) umax
      WRITE (*,9002) umax
      PRINT *,'What is the bound on rotation?'
      READ (*,9002) rotmax
      WRITE (*,9002) rotmax
      DO 2000 i = 1, nbods
        xa(i) = xmax*rand(seed)
        ya(i) = xmax*rand(seed)
        ua(i) = umax*rand(seed)
        va(i) = umax*rand(seed)
CD        rota(i) = rotmax*rand(seed)
        rota(i) = rotmax
        IF (ABS(xa(i)) .GT. xmax) THEN
          PRINT *,'initialization failure in randomness'
          STOP
        ENDIF
        IF (ABS(ya(i)) .GT. xmax) THEN
          PRINT *,'initialization failure in randomness'
          STOP
        ENDIF
        IF (ABS(ua(i)) .GT. umax) THEN
          PRINT *,'initialization failure in randomness'
          STOP
        ENDIF
        IF (ABS(va(i)) .GT. umax) THEN
          PRINT *,'initialization failure in randomness'
          STOP
        ENDIF
        IF (ABS(rota(i)) .GT. rotmax) THEN
          PRINT *,'initialization failure in randomness'
          STOP
        ENDIF
 2000 CONTINUE
      DO 2100 i = 1, nbods
        xb(i) = 0.0
        yb(i) = 0.0
        ub(i) = 0.0
        vb(i) = 0.0
        rotb(i) = 0.0
        xc(i) = 0.0
        yc(i) = 0.0
        uc(i) = 0.0
        vc(i) = 0.0
        rotc(i) = 0.0
 2100 CONTINUE
C     Now zero out the cells greater than the number of bodies to
C       be modelled.
      DO 2200 i = nbods+1, nmax
        xa(i) = 0.0
        ya(i) = 0.0
        ua(i) = 0.0
        va(i) = 0.0
        rota(i) = 0.0
        xb(i) = 0.0
        yb(i) = 0.0
        ub(i) = 0.0
        vb(i) = 0.0
        rotb(i) = 0.0
        xc(i) = 0.0
        yc(i) = 0.0
        uc(i) = 0.0
        vc(i) = 0.0
        rotc(i) = 0.0
 2200 CONTINUE

C     Establish the ocean and atmospheric current fields:
      DO 3000 i = 1, nbods
        uatm(i) = 0.0
        vatm(i) = 0.0
        uoce(i) = 0.0
        voce(i) = 0.0
 3000 CONTINUE

C     Establish the sea surface topography
      DO 3010 i = 1, nbods
        dtopo(i) = 0.0
 3010 CONTINUE

      PRINT *,'Estimated time of execution (s):'
      PRINT *, nbods*nops*ndt/speed
      OPEN (10, FILE='nbody.output', FORM='FORMATTED', STATUS='NEW')
      PRINT *, LONG(362)

 9001 FORMAT (I8)
 9002 FORMAT (E13.6)

      RETURN
      END
      SUBROUTINE tinter(rot, rinert, x, y, u, v, nbods, a)
C     Compute the floe-floe exchange of angular momentum.
C     BG 1/21/92.

      INTEGER nbods
      REAL rot(nbods), rinert(nbods)
      REAL x(nbods), y(nbods), u(nbods), v(nbods)
      REAL a(nbods)

C     Postpone for now.
      INTEGER i

      DO 1000 i = 1, nbods
       a(i) = 0.0
 1000 CONTINUE

      RETURN
      END
      SUBROUTINE topog(x, y, dtopo, a, b, nbods)
C     Compute the geoid deformation acceleration on the ice floes.
C       Follow Thorndike and Colony, and use the rhoi/rhoo deflator.

      INTEGER nbods
      REAL dtopo(nbods), x(nbods), y(nbods)
      REAL a(nbods), b(nbods)

      REAL rhoo, rhoi, g
      PARAMETER (rhoo = 1025.)
      PARAMETER (rhoi = 917. )
      PARAMETER (g    = 9.8  )

      INTEGER i
C     Don't have an algorithm yet.  Set to zero.

      DO 1000 i = 1, nbods
        a(i) = g*rhoi/rhoo*0.0
        b(i) = g*rhoi/rhoo*0.0
 1000 CONTINUE

      RETURN
      END
C*********!*********!*********!*********!*********!*********!_________!++
      SUBROUTINE torqs(ttorq, nbods, rot, mass, rad, thick, rinert,
     1                 x, y, u, v)
C     Compute the torques acting on the floes.
C     BG 1/21/92.

      IMPLICIT none
      INTEGER nbods
      REAL ttorq(nbods)
      REAL rot(nbods), mass(nbods), rad(nbods), thick(nbods)
      REAL rinert(nbods)
      REAL x(nbods), y(nbods), u(nbods), v(nbods)

      INTEGER nmax
      PARAMETER (nmax = 1024*128)

      INTEGER i
      REAL a(nmax), tmax, tmin

      DO 100 i = 1, nbods
        ttorq(i) = 0.0
  100 CONTINUE

      CALL omdrag(rot, rinert, nbods, a)
      DO 1000 i = 1, nbods
        ttorq(i) = ttorq(i) + a(i)
 1000 CONTINUE

CD      tmax = 0.0
CD      tmin = 0.0
CD      DO 1010 i = 1, nbods
CD        tmax = AMAX1(tmax, a(i))
CD        tmin = AMIN1(tmax, a(i))
CD 1010 CONTINUE
CD      PRINT *,'torq max, min', tmax, tmin

      CALL tinter(rot, rinert, x, y, u, v, nbods, a)
      DO 1100 i = 1, nbods
        ttorq(i) = ttorq(i) + a(i)
 1100 CONTINUE

      RETURN
      END
