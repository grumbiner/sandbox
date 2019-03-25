       PROGRAM ENIAC
C      Emulate the first successful numerical weather prediction.
C      It was run on the ENIAC during 1948 and 1949.
C      The results and method were written up in Tellus 2, p. 237 1950.
C      Robert Grumbine 2 May 1995 

C      Non-dimensional constants
      INTEGER p, q, n, ip, jp
      PARAMETER (p = 18)
      PARAMETER (q = 15)
      PARAMETER (n = 24)
      PARAMETER (ip = 9)
      PARAMETER (jp = 13)

C      Declare Dimensional Constants
      REAL g, radius, omega, ds, dt
      PARAMETER (g      = 9.81)
      PARAMETER (radius = 6.37E6)
      PARAMETER (omega  = 7.292E-5)
      PARAMETER (ds     = 7.36E5)
      PARAMETER (dt     = omega*86400/n)
      
C      Rescale feet (input dimension of Z's) to meters
      REAL fttom
      PARAMETER (fttom = 3.048)
      REAL pi
      PARAMETER (pi = 3.141592654)
      
C     Declare temporary arrays -
C       These were used because of the memory constraint on the ENIAC
      REAL s(0:p,0:p), t(0:q,0:q), v(0:p,0:q)
      REAL r(0:p,0:q), f(0:p,0:q), h(0:p,0:q)
      REAL a(0:p,0:q), b(0:p,0:q)
      REAL alpha(0:p,0:q), beta(0:p,0:q)
      REAL zp(0:p,0:q), zetap(0:p,0:q)
      
C      Declare data, physical info.
      REAL z(0:p,0:q), zeta(0:p,0:q), eta(0:p,0:q)
      
C      Declare computational variables
      INTEGER i, j, k, l, m
      
C***********************************************************!!
C      Set up the initial conditions deck
      DATA (z(0,j),j=0,15) /910, 899, 900, 885, 880, 890, 895,
     1 885, 895, 920, 910, 890, 870, 860, 850, 840/
      DATA (z(1,j),j=0,15) /907, 900, 895, 880, 870, 885, 890,
     1 890, 900, 925, 920, 900, 880, 870, 840, 830/
      DATA (z(2,j),j=0,15) /905, 901, 890, 877, 865, 865, 880,
     1 885, 900, 920, 930, 910, 885, 860, 820, 800/
      DATA (z(3,j),j=0,15) /908, 901, 888, 870, 847, 830, 830,
     1 850, 890, 900, 900, 890, 860, 800, 760, 720/
      DATA (z(4,j),j=0,15) /910, 905, 888, 871, 840, 790, 760,
     1 780, 860, 865, 830, 800, 750, 720, 680, 680/
      DATA (z(5,j),j=0,15) /915, 910, 898, 880, 850, 795, 740,
     1 740, 800, 830, 770, 720, 685, 670, 670, 670/
      DATA (z(6,j),j=0,15) /918, 919, 915, 900, 865, 810, 750,
     1 730, 750, 740, 730, 690, 655, 665, 665, 665/
      DATA (z(7,j),j=0,15) /930, 931, 926, 915, 883, 825, 770,
     1 725, 700, 680, 670, 660, 660, 655, 655, 660/
      DATA (z(8,j),j=0,15) /932, 945, 942, 932, 900, 850, 780,
     1 720, 660, 610, 600, 620, 640, 650, 655, 665/
      DATA (z(9,j),j=0,15) /935, 950, 950, 942, 924, 890, 800,
     1 710, 640, 600, 590, 580, 600, 645, 660, 675/
      DATA (z(10,j),j=0,15) /933, 940, 945, 942, 926, 890, 810,
     1 720, 650, 600, 580, 560, 620, 655, 670, 685/
      DATA (z(11,j),j=0,15) /931, 934, 939, 935, 897, 880, 830,
     1 750, 690, 620, 600, 600, 630, 670, 685, 700/
      DATA (z(12,j),j=0,15) /925, 926, 925, 920, 890, 865, 830,
     1 760, 710, 670, 620, 620, 650, 690, 700, 710/
      DATA (z(13,j),j=0,15) /920, 919, 896, 903, 875, 845, 810,
     1 760, 735, 730, 710, 720, 720, 740, 730, 730/
      DATA (z(14,j),j=0,15) /915, 913, 889, 897, 860, 825, 790,
     1 780, 790, 830, 840, 850, 820, 780, 760, 740/
      DATA (z(15,j),j=0,15) /912, 910, 885, 891, 860, 830, 820,
     1 840, 860, 890, 900, 900, 850, 790, 770, 755/
      DATA (z(16,j),j=0,15) /908, 908, 885, 900, 880, 870, 880,
     1 890, 910, 925, 920, 900, 870, 800, 780, 765/
      DATA (z(17,j),j=0,15) /905, 907, 886, 905, 890, 900, 910,
     1 920, 920, 920, 900, 890, 870, 840, 800, 780/
      DATA (z(18,j),j=0,15) /903, 906, 886, 910, 900, 920, 920,
     1 925, 915, 910, 890, 880, 860, 850, 820, 810/
C***********************************************************!!      
      
C      BEGIN THE EXECUTION HERE
C      This file is not comparable to anything in the original program.
      OPEN (UNIT=1, FILE="ENIAC.OUT", FORM="FORMATTED")
      
C      Rescale the heights to metric
      DO 1 j = 0, 15
        DO 2 i = 0, 18
          z(i,j)  = z(i,j)/fttom
    2   CONTINUE
    1 CONTINUE

      zeta = 0.0

C      Prepare the data decks - used due to memory constraints
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
     1                + SIN(pi*j/(2*q))**2 )
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
C      BEGIN THE ITERATIVE SOLUTION OF THE EQUATIONS
      DO 2000 k = 0, 3*n-1
      
      DO 1010 j = 1, q-1
        DO 1020 i = 1, p-1
          eta(i,j) = f(i,j)+h(i,j)*zeta(i,j)
 1020   CONTINUE
 1010 CONTINUE

      DO 1101 i = 1, p-1
        j = 0
        IF (z(i+1,0) .LT. z(i-1,0)) THEN
          eta(i,j) = f(i,j)+h(i,j)*zeta(i,j)
        ENDIF
        j = q
        IF (z(i+1,q) .GT. z(i-1,q)) THEN
          eta(i,j) = f(i,j)+h(i,j)*zeta(i,j)
        ENDIF
 1101 CONTINUE

      DO 1111 j = 1, q-1
        i = 0
        IF (z(0,j+1).GT.z(0,j-1)) THEN
          eta(i,j) = f(i,j)+h(i,j)*zeta(i,j)
        ENDIF
        i = p
        IF (z(p,j+1).LT.z(p,j-1)) THEN 
          eta(i,j) = f(i,j)+h(i,j)*zeta(i,j)
        ENDIF
 1111 CONTINUE
 
       DO 1200 j = 1, q-1
        DO 1210 i = 1, p-1
          zetap(i,j) = 0.5*( (eta(i+1,j)-eta(i-1,j))
     1                        *(z(i,j+1)-z(i,j-1))
     2                   -   (eta(i,j+1)-eta(i,j-1))
     3                        *(z(i+1,j)-z(i-1,j)) )
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

C      SECTION FOR CARRYING FORWARD THE EXTRAPOLATION 
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

C      Apply the inflow/outflow conditions      
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
       
CD      WRITE (*,9003) k
CD      WRITE (*,9001) ((z(i,j),j=0,q),i=0,p)
CD      WRITE (*,9002)
      WRITE (1,9003) k
      WRITE (1,9001) ((z(i,j),j=0,q),i=0,p)
      WRITE (1,9002)
 9001 FORMAT (16F7.2)
 9002 FORMAT (' ')
 9003 FORMAT ('k = ',I3)
      
 2000 CONTINUE
 
C***********************************************************!! 

      END
