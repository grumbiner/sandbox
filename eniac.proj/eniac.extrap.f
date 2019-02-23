C***********************************************************!! 
	SUBROUTINE extrap(z, zi, k)
C       Robert Grumbine 2 May 1995
C       For eniac emulation program

        IMPLICIT none
	
C	Non-dimensional constants
	INTEGER p, q, n, ip, jp
	PARAMETER (p = 18)
	PARAMETER (q = 15)
	PARAMETER (n = 24)
	PARAMETER (ip = 9)
	PARAMETER (jp = 13)

C	Declare Dimensional Constants
	REAL g, radius, omega, ds, dt
	PARAMETER (g      = 9.81)
	PARAMETER (radius = 6.37E6)
	PARAMETER (omega  = 7.292E-5)
	PARAMETER (ds     = 7.36E5)
	PARAMETER (dt     = omega*86400/n)
	
C	Rescale feet (input dimension of Z's) to meters
	REAL fttom
	PARAMETER (fttom = 3.048)
	REAL pi
	PARAMETER (pi = 3.141592654)
	
C	Declare temporary arrays -
C		These were used because of the memory constraint on the ENIAC
	REAL s(0:p,0:p), t(0:q,0:q), v(0:p,0:q)
	REAL r(0:p,0:q), f(0:p,0:q), h(0:p,0:q)
	REAL a(0:p,0:q), b(0:p,0:q)
	REAL alpha(0:p,0:q), beta(0:p,0:q)
	REAL zp(0:p,0:q), zetap(0:p,0:q)
	
C	Declare data, physical info.
	REAL z(0:p,0:q), zeta(0:p,0:q), eta(0:p,0:q)
	
C	Declare computational variables
	INTEGER i, j, k, l, m
	REAL zi(0:p,0:q)
	
	SAVE s, t, v, r, f, h, a, b, alpha, beta, zp, zetap
	SAVE zeta, eta
	
	DO 1010 j = 1, q-1
	  DO 1020 i = 1, p-1
	    eta(i,j) = f(i,j)+h(i,j)*zeta(i,j)
 1020   CONTINUE
 1010 CONTINUE

	  DO 1101 i = 1, p-1
	    IF (z(i+1,0) .LT. z(i-1,0)) THEN
	      eta(i,j) = f(i,j)+h(i,j)*zeta(i,j)
          ENDIF
 	    IF (z(i+1,q) .GT. z(i-1,q)) THEN
	      eta(i,j) = f(i,j)+h(i,j)*zeta(i,j)
          ENDIF
 1101   CONTINUE

	  DO 1111 j = 1, q-1
	    IF (z(0,j+1).GT.z(0,j-1)) THEN
	      eta(i,j) = f(i,j)+h(i,j)*zeta(i,j)
          ENDIF
	    IF (z(p,j+1).LT.z(p,j-1)) THEN 
	      eta(i,j) = f(i,j)+h(i,j)*zeta(i,j)
          ENDIF
 1111   CONTINUE
 
 	DO 1200 j = 1, q-1
	  DO 1210 i = 1, p-1
	    zetap(i,j) = 0.5*( (eta(i+1,j)-eta(i-1,j))
     1				*(z(i,j+1)-z(i,j-1))
     2			 -   (eta(i,j+1)-eta(i,j-1))
     3				*(z(i+1,j)-z(i-1,j)) )
 1210   CONTINUE
 1200 CONTINUE
 
 	DO 1300 j = 1, q-1
	  DO 1310 i = 1, p-1
	    alpha(i,j) = 0.0
	    DO 1320 l = 1, p-1
	      alpha(i,j)=alpha(i,j)+s(l,i)*zetap(l,j)
 1320     CONTINUE
 1310   CONTINUE
 1300 CONTINUE
 
 	DO 1400 j = 1, q-1
	  DO 1410 i = 1, p-1
	    a(i,j) = 0.0
	    DO 1420 m = 1, q-1
	      a(i,j) = a(i,j) + t(m,j)*alpha(i,m)
 1420     CONTINUE
 1410   CONTINUE
 1400 CONTINUE
 
 	DO 1500 j = 1, q-1
	  DO 1510 i = 1, p-1
	    b(i,j) = -a(i,j)/v(i,j)
 1510   CONTINUE
 1500 CONTINUE
 	
	DO 1600 j = 1, q-1
	  DO 1610 i = 1, p-1
	    beta(i,j) = 0.0
	    DO 1620 m = 1, q-1
	      beta(i,j) = beta(i,j)+t(m,j)*b(i,m)
 1620     CONTINUE
 1610   CONTINUE
 1600 CONTINUE
 
 	DO 1700 j = 1, q-1
	  DO 1710 i = 1, p-1
	    zp(i,j) = 0.0
	    DO 1720 l = 1, p-1
	      zp(i,j) = zp(i,j)+s(l,i)*beta(l,j)
 1720     CONTINUE
 1710   CONTINUE
 1700 CONTINUE

C	SECTION FOR CARRYING FORWARD THE EXTRAPOLATION 
 	IF (k.EQ.0) THEN
	  DO 1800 j= 1, q-1
	    DO 1810 i = 1, p-1
	      z(i,j)    = z(i,j)+dt*zp(i,j)
		zeta(i,j) = zeta(i,j)+dt*zetap(i,j)
 1810     CONTINUE
 1800   CONTINUE
 	 ELSE
	  DO 1820 j = 1, q-1
	    DO 1830 i = 1, p-1
	      z(i,j)    = z(i,j)+2.*dt*zp(i,j)
		zeta(i,j) = zeta(i,j)+2.*dt*zetap(i,j)
 1830     CONTINUE
 1820   CONTINUE
	ENDIF

C	Apply the inflow/outflow conditions	
	DO 1900 j = 1, q-1
	  IF (z(0,j+1) .GT. z(0,j-1)) THEN
	    z(0,j)    = z(0,j)
	    zeta(0,j) = 2.*zeta(1,j)-zeta(2,j)
	   ELSE
	    z(0,j)    = z(0,j)
	    zeta(0,j) = zeta(0,j)
	  ENDIF
	  IF (z(p,j+1) .LT. z(p,j-1)) THEN
	    z(p,j)    = z(p,j)
	    zeta(p,j) = 2.*zeta(p-1,j)-zeta(p-2,j)
	   ELSE
	    z(p,j)    = z(p,j)
	    zeta(p,j) = zeta(p,j)
	  ENDIF
 1900 CONTINUE
 	DO 1910 i = 1, p-1
	  IF (z(i+1,0) .LT. z(i-1,0)) THEN
 	    z(i,0)    = z(i,0)
	    zeta(i,0) = 2.*zeta(i,1)-zeta(i,2)
	   ELSE
	    z(i,0)    = z(i,0)
	    zeta(i,0) = zeta(i,0)
	  ENDIF
	  IF (z(i+1,q) .GT. z(i-1,q)) THEN
	    z(i,q)    = z(i,q)
	    zeta(i,q) = 2.*zeta(i,q-1)-zeta(i,q-2)
	  ENDIF
 1910 CONTINUE
 
      RETURN
	
	ENTRY setup(z, zi, k)
		
C	Rescale the heights to metric
	DO 1 j = 0, 15
	  DO 2 i = 0, 18
	    z(i,j)  = z(i,j)/fttom
	    zi(i,j) = z(i,j)
    2   CONTINUE
    1 CONTINUE

C	Prepare the data decks - used due to memory constraints
	DO 10 i = 1, p-1
	  DO 11 l = 1, p-1
	    s(l,i) = SIN(pi*i*l/p)
   11   CONTINUE
   10 CONTINUE
   
   	DO 20 j = 1, q-1
	  DO 21 m = 1, q-1
	    t(m,j) = SIN(pi*m*j/q)
   21   CONTINUE
   20 CONTINUE
   
      DO 30 i = 0, p
	  DO 31 j = 0, q
	    v(i,j) = p*q*(SIN(pi*i/(2*p))**2
     1		    + SIN(pi*j/(2*q))**2 )
   31   CONTINUE
   30 CONTINUE
   
   	DO 40 i = 0, p
	  DO 41 j = 0, q
	    r(i,j) = (ds/(2*radius))**2
     1           *((i-ip)**2+(j-jp)**2)
   41   CONTINUE
   40 CONTINUE
   
      DO 50 i = 0, p
	  DO 51 j = 0, q
	    f(i,j) = (1.-r(i,j))/(1.+r(i,j))
   51   CONTINUE
   50 CONTINUE
   
   	DO 60 i = 0, p
	  DO 61 j = 0, q
          h(i,j) = g/(2*omega*ds)**2 
     1          * (1+r(i,j))**3/(1.-r(i,j))
   61   CONTINUE
   60 CONTINUE

	DO 70 i = 1, p-1
	  DO 71 j = 1, q-1
	    zeta(i,j) = z(i+1,j)+z(i,j+1)+z(i-1,j)+z(i,j-1)
     1                - 4.*z(i,j)
   71   CONTINUE
   70 CONTINUE
   
      DO 72 j = 0, q
	  zeta(0,j) = 2.*zeta(1,j)  - zeta(2,j)
	  zeta(p,j) = 2.*zeta(p-1,j)- zeta(p-2,j)
   72 CONTINUE
      DO 73 i = 0,p
	  zeta(i,0) = 2.*zeta(i,1)   - zeta(i,2)
	  zeta(i,q) = 2.*zeta(i,q-1) - zeta(i,q-2)
   73 CONTINUE
   
      DO 80 j = 0, q
	  DO 81 i = 0, p
	    eta(i,j) = f(i,j)+h(i,j)*zeta(i,j)
   81   CONTINUE
   80 CONTINUE
   
C     END OF THE PRELIMINARY SET UP SECTION
C***********************************************************!!
	RETURN
	END
