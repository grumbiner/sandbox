C averager.f
C This program will square the 2nd and the 3rd columns out of 3 columns of 
C numbers and put it into a file with three columns of numbers.

C Subprograms needed: subra.f subrb.f subrc.f subrd.f subre.f subrf.f subrg.f

      PROGRAM AB test
      IMPLICIT NONE
 
      REAL b(24),c(24),e(24),f(24),h(24),j(24)
      REAL l(24),m(24),p(24),q(24),s(24),u(24),w(24),x(24) 
      REAL a(24),d(24),g(24),k(24),o(24),r(24),v(24)
      REAL squar1b(24),squar1a(24)
      REAL squar2b(24),squar2a(24)
      REAL squar3b(24),squar3a(24)
      REAL squar4b(24),squar4a(24)
      REAL squar5b(24),squar5a(24)
      REAL squar6b(24),squar6a(24)
      REAL squar7b(24),squar7a(24)
      INTEGER i,n


 
      CALL GET1 (a,b,c,n)
      CALL GET2 (d,e,f,n)
      CALL GET3 (g,h,j,n)
      CALL GET4 (k,l,m,n)
      CALL GET5 (o,p,q,n)
      CALL GET6 (r,s,u,n)
      CALL GET7 (v,w,x,n)

      OPEN (1, FILE = 'newcorr.9704.024')
      OPEN (2, FILE = 'newcorr.9704.048')
      OPEN (3, FILE = 'newcorr.9704.072')
      OPEN (4, FILE = 'newcorr.9704.096')
      OPEN (5, FILE = 'newcorr.9704.120')
      OPEN (6, FILE = 'newcorr.9704.144')
      OPEN (7, FILE = 'newcorr.9704.168')
      

      DO 1000 i = 1,n
         squar1a(i) = b(i)*b(i)
         squar1b(i) = c(i)*c(i)
      WRITE (1,9005) a(i),squar1a(i), squar1b(i)
 1000 CONTINUE

      DO 1100 i = 1,n
         squar2a(i) = e(i)*e(i)
         squar2b(i) = f(i)*f(i)
      WRITE (2,9005) d(i),squar2a(i), squar2b(i)
 1100 CONTINUE

      DO 1200 i = 1,n
         squar3a(i) = h(i)*h(i)
         squar3b(i) = j(i)*j(i)
      WRITE (3,9005) g(i),squar3a(i), squar3b(i)
 1200 CONTINUE

      DO 1300 i = 1,n
         squar4a(i) = l(i)*l(i)
         squar4b(i) = m(i)*m(i)
      WRITE (4,9005) k(i),squar4a(i), squar4b(i)
 1300 CONTINUE

      DO 1400 i = 1,n
         squar5a(i) = p(i)*p(i)
         squar5b(i) = q(i)*q(i)
      WRITE (5,9005) o(i),squar5a(i), squar5b(i)
 1400 CONTINUE

      DO 1500 i = 1,n
         squar6a(i) = s(i)*s(i)
         squar6b(i) = u(i)*u(i)
      WRITE (6,9005) r(i),squar6a(i), squar6b(i)
 1500 CONTINUE

      DO 1600 i = 1,n
         squar7a(i) = w(i)*w(i)
         squar7b(i) = x(i)*x(i)
      WRITE (7,9005) v(i),squar7a(i), squar7b(i)
 1600 CONTINUE

 9005 FORMAT (3F9.3)

      END
