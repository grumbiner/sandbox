      FUNCTION erf(x)
C     By Doug MacAyeal, from Abramowitz and Stegun
      REAL*8 p, a1, a2, a3, a4, a5
      PARAMETER (p=.3275911d0,a1=.254829592d0,a2=-.284496736d0
     1    ,a3=1.421413741d0,a4=-1.453152027d0,a5=1.061405429d0)
      REAL*8 erf,x
      REAL*8 t
      IF (x.gt.0.) THEN
        t=1.d0/(1.d0+p*x)
        erf=1.0d0-(a1*t+a2*t**2+a3*t**3+a4*t**4+a5*t**5)*dexp(-x**2)
       ELSE
        t=1.d0/(1.d0-p*x)
        erf=-1.0d0+(a1*t+a2*t**2+a3*t**3+a4*t**4+a5*t**5)*dexp(-x**2)
      END IF
      RETURN
      END
