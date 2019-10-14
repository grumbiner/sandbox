      PROGRAM conaly
C     Make an analytic estimate of the time of convection,
C       given the forcing and starting postition of the test
C       water column.
C     8-31-88.

      REAL alpha, beta, qr, qm, pi, h, dels, delt
      REAL tguess, tmax, tmin, dummy
      REAL f, t, ftmin, ftmax
      REAL u, v, sigx, sigy, delx, dely
      REAL dxmin, dxmax, ddx
      INTEGER i
      PARAMETER (pi = 3.141592654)

CD    PRINT *,'What is the maximum polynya freezing rate?'
      READ (*,9001) qm
CD    WRITE (*,9001) qm
CD    PRINT *,'What is the backgroud freezing rate?'
      READ (*,9001) qr
CD    WRITE (*,9001) qr
CD    PRINT *,'What is the models time step?'
      READ (*,9001) delt
CD    WRITE (*,9001) delt
CD    PRINT *,'What is the depth of the water column?'
      READ (*,9001) h
CD    WRITE (*,9001) h
CD    PRINT *,'What is the stratification?'
      READ (*,9001) dels
CD    WRITE (*,9001) dels

CD    PRINT *,'What is U?'
      READ (*,9001) u
CD    WRITE (*,9001) u
CD    PRINT *,'What is V?'
      READ (*,9001) v
CD    WRITE (*,9001) v
CD    PRINT *,'What is sigx?'
      READ (*,9001) sigx
CD    WRITE (*,9001) sigx
CD    PRINT *,'What is sigy?'
      READ (*,9001) sigy
CD    WRITE (*,9001) sigy

CD    PRINT *,'What is delx?'
      READ (*,9001) delx 
CD    WRITE (*,9001) delx 
CD    PRINT *,'What is dely?'
      READ (*,9001) dely
CD    WRITE (*,9001) dely
      PRINT *,'What is delta x max?'
      READ (*,9001) dxmax
      PRINT *,'What is delta x min?'
      READ (*,9001) dxmin
      PRINT *,'What is the increment for delta x?'
      READ (*,9001) ddx

      alpha = SQRT(0.5*( (u/sigx)**2 + (v/sigy)**2 ) )
      PRINT *,'alpha',alpha
      beta = (u*delx/sigx**2 + v*dely/sigy**2)/(2.*alpha)
      tmin = h*dels/qm/2.
      tmax = h*dels/qr
      ftmin  = f(tmin, alpha, beta, qr, qm, pi, h,
     1                dels, delx, dely, sigx, sigy)
      ftmax  = f(tmax, alpha, beta, qr, qm, pi, h, 
     1                dels, delx, dely, sigx, sigy)
      PRINT *,'tmin, f(tmin)', tmin/delt, ftmin
      PRINT *,'tmax, f(tmax)', tmax/delt, ftmax

      DO 3000 delx = dxmin, dxmax, ddx
        i = 0
        beta = (u*delx/sigx**2 + v*dely/sigy**2)/(2.*alpha)
CD      PRINT *,'beta',beta
        tmin = h*dels/qm/2.
        tmax = h*dels/qr

 1000   CONTINUE
          i      = i + 1
          tguess = 0.5*(tmin+tmax)
          dummy  = f(tguess, alpha, beta, qr, qm, pi, h,
     1                      dels, delx, dely, sigx, sigy)
          IF (dummy*ftmin .GT. 0.0) THEN
            tmin  = tguess
            ftmin = dummy
           ELSE 
            tmax  = tguess
            ftmax = dummy
          ENDIF
          IF (    (ABS(dummy) .LT. 1.E-4)
     1                   .OR. (i .GE. 20)
     2                   .OR. (tmax-tmin .LT. 0.E4)  )    THEN
             GO TO 2000
           ELSE 
             GO TO 1000
          ENDIF

 2000   CONTINUE
        WRITE (*,9002) i, delx, delx+u*tguess, tguess/delt, dummy
 3000 CONTINUE

 9001 FORMAT (E14.6)
   
 9002 FORMAT (' i, tguess, f(tguess)',I3,3E10.4,F9.5)
 
      END 
