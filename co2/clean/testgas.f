
      PROGRAM testgas
      IMPLICIT none

      INTEGER gas
      DOUBLE PRECISION xu, gasses, pref, temper, wind
      DOUBLE PRECISION dummy, cco2, d1, d2
      INTEGER i

      wind = 6.0
      pref = 350.D-6
      gas  = 16
CD      PRINT *,'Starting loop'

      DO 1000 i = 0, 22
CD        PRINT *,'in loop', i
        temper = DBLE(i)
        xu   = cco2(2000.D-6, 2300.D-6, temper, 34.7, 0.D0)
CD        PRINT *,'temperature',temper
        dummy = gasses(gas, xu, pref, temper, 34.7, wind)
CD        PRINT *,'back from gasses',dummy
       d1 = dummy*86400.*365.2422
        d2 = d1*1027.8
        WRITE (*,9001) xu, d1, d2, temper
 1000 CONTINUE

 9001 FORMAT (3E13.6, F5.1)

      END