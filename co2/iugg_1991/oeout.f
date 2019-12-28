C*************************************************----------++++++++++!!
      SUBROUTINE oeout(x, y, nlayer, chemu, 
     1                 step, outoft, secpyr, deltat, deltaz,
     2                 xatm, temp, wind, regions, time)

      IMPLICIT none

      INTEGER nlayer, chemu, step, outoft, regions
      DOUBLE PRECISION x(nlayer, chemu, regions)
      DOUBLE PRECISION y(nlayer, chemu, regions)
      DOUBLE PRECISION secpyr, deltat, deltaz
      DOUBLE PRECISION xatm(chemu), temp(regions), wind(regions)
      REAL time

      DOUBLE PRECISION sum1
      INTEGER i, j, k, m, dz

      dz = INT(deltaz)
      IF (MOD((step-1),INT(DBLE(outoft)*secpyr/deltat)) .EQ. 0) THEN
        DO 3000 m = 1, regions
          DO 1000 k = 1, chemu
            sum1 = 0.D0
            DO 2000 i = 2, nlayer-1
              sum1 = sum1 + x(i, k, m)
 2000       CONTINUE
            WRITE (9*m+k, 9002) (dz*i-dz/2,CHAR(9),x(i,k,m),
     1                       CHAR(9),sum1, i=1, nlayer)
 1000     CONTINUE
 3000   CONTINUE
      ENDIF

      IF (MOD((step-1),INT(DBLE(outoft)*secpyr/deltat/10)).EQ. 0) THEN
C       Atmospheric output 10* as often as profiles.
        WRITE (1, 9003) xatm(2), xatm(3), xatm(4),
     1                temp(1), temp(2), temp(3),
     2                wind(1), wind(2), wind(3), time
        WRITE (*, 9003) xatm(2), xatm(3), xatm(4),
     1                temp(1), temp(2), temp(3),
     2                wind(1), wind(2), wind(3), time
      ENDIF

 9002 FORMAT (I4,A1,E13.6,A1,E13.6)
 
 9003 FORMAT (3E12.5, 3F5.1, 3F5.1, F8.0)
 
      RETURN
      END
