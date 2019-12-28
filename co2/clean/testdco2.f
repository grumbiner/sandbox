      PROGRAM testco2
C     test the co2 function
      IMPLICIT none

      DOUBLE PRECISION cco2, dco2, pco2
      DOUBLE PRECISION tco2, alk, temp, depth
      DOUBLE PRECISION c2, p2, c1, p1
      REAL yinit, zinit
      REAL rsalt

CD      OPEN (10, FILE='ccout', FORM='FORMATTED', STATUS='NEW')

      depth =  0.D0
      rsalt = 35.0
      PRINT *,'What is tco2?'
      READ (*,9002) tco2
 9002 FORMAT (D13.6)

CD      tco2 = 2139.D-6
      alk  = 2330.D-6
      revell = 10.
      DO 1000 temp = 0., 28.
        yinit = tco2*(1.-1./revell)
        zinit = tco2/revell
        c1 = cco2(tco2, alk, temp, rsalt, depth)
        p1 = pco2(c1, temp, rsalt, depth)*1.E6
        c2 = dco2(tco2, alk, temp, rsalt, depth, yinit, zinit)
        p2 = pco2(c2, temp, rsalt, depth)*1.E6
        c1 = c1*1.E6
        c2 = c2*1.E6
CD        WRITE (10,9001) 
CD     1 temp, CHAR(9), p1, CHAR(9), p2, CHAR(9), tco2/zinit
        WRITE (*,9001) 
     1 temp, CHAR(9), p1, CHAR(9), p2, CHAR(9), tco2/zinit
CD        PRINT *,temp, c1*1.E6, c2*1.E6, p1*1.E6, p2*1.E6, tco2/zinit
 1000 CONTINUE

 9001 FORMAT (F5.1,5(A1,F10.4))

      PAUSE
      END
