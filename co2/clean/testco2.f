
      PROGRAM testco2
C     test the co2 function
      IMPLICIT none

      DOUBLE PRECISION bco2, cco2, pco2, n2co2
      DOUBLE PRECISION tco2, alk, temp, depth
      DOUBLE PRECISION c1, c2, p1, p2
      REAL rsalt

      OPEN (10, FILE='ccout', FORM='FORMATTED', STATUS='NEW')

      depth =  0.D0
      rsalt = 34.7
      tco2 = 1961.D-6
      alk  = 2276.D-6

      DO 1000 temp = 0., 30.
        c1 = bco2(tco2, alk, temp, rsalt, depth)
        c2 = cco2(tco2, alk, temp, rsalt, depth)
        p1 = pco2(c1, temp, rsalt, depth)
        p2 = pco2(c2, temp, rsalt, depth)
        WRITE (10,9001) temp, CHAR(9), c1, CHAR(9), p1,
     1                        CHAR(9), c2, CHAR(9), p2
        WRITE (*,9001) temp, CHAR(9), c1, CHAR(9), p1,
     1                        CHAR(9), c2, CHAR(9), p2
 1000 CONTINUE

 9001 FORMAT (F5.1,2(A1,E13.5))

      END
