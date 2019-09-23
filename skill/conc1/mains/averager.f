C This program averages numbers from 7 different files
C and make a separate file with 2 columns of 7 numbers.
C averager.f
      PROGRAM AB test
      IMPLICIT NONE
 
      REAL a(24),b(24),c(24)
      REAL d(24),e(24),f(24)
      REAL g(24),h(24),j(24)
      REAL k(24),l(24),m(24)
      REAL o(24),p(24),q(24)
      REAL r(24),s(24),u(24)
      REAL v(24),w(24),x(24) 
      REAL average
      INTEGER n


 
      CALL GET1 (a,b,c,n)
      CALL GET2 (d,e,f,n)
      CALL GET3 (g,h,j,n)
      CALL GET4 (k,l,m,n)
      CALL GET5 (o,p,q,n)
      CALL GET6 (r,s,u,n)
      CALL GET7 (v,w,x,n)

      OPEN (7, FILE = 'ss.south')
      
      WRITE (7,9005) average (b,n), average (c,n)
      WRITE (7,9005) average (e,n), average (f,n)
      WRITE (7,9005) average (h,n), average (j,n)
      WRITE (7,9005) average (l,n), average (m,n)
      WRITE (7,9005) average (p,n), average (q,n)
      WRITE (7,9005) average (s,n), average (u,n)
      WRITE (7,9005) average (w,n), average (x,n)

 9005 FORMAT (' ',2F9.4)

      END

      REAL FUNCTION average (Y,n)
      INTEGER i,n  
      REAL Y(n), aver

      aver = 0.

      DO 4000 i = 1,n
         aver = aver + Y(i) 
 4000 CONTINUE

      average = aver/n

      RETURN 
      END
