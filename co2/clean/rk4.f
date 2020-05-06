C*************************************************----------++++++++++!!
      SUBROUTINE rk4(y, dydx, n, x, h, yout, derivs)
C     From Numerical Recipes.  Typography and declarations
C       changed to conform with BG usage.
      IMPLICIT NONE
  
      INTEGER nmax, n
      PARAMETER (nmax = 23)

      REAL y(n),    dydx(n),  yout(n)
      REAL yt(nmax),dyt(nmax),dym(nmax)
      REAL h, hh, h6, xh, x
  
      INTEGER i
      EXTERNAL derivs
 
      hh=h*0.5
      h6=h/6.
      xh=x+hh
  
      DO 11 i=1,n
        yt(i)=y(i)+hh*dydx(i)
11    CONTINUE

      CALL derivs(xh,yt,dyt)
      DO 12 i=1,n
        yt(i)=y(i)+hh*dyt(i)
12    CONTINUE

      CALL derivs(xh,yt,dym)
      DO 13 i=1,n
        yt(i)=y(i)+h*dym(i)
        dym(i)=dyt(i)+dym(i)
13    CONTINUE

      CALL derivs(x+h,yt,dyt)
      DO 14 i=1,n
        yout(i)=y(i)+h6*(dydx(i)+dyt(i)+2.*dym(i))
14    CONTINUE

      RETURN
      END
