C*************************************************----------++++++++++!!
      SUBROUTINE geolco2(x, pchem, nlayer, chemu, region,
     1                   csdept, ishelf, rsalt, deltaz,
     2                   time)

      IMPLICIT none

      INTEGER nlayer, chemu, region
      INTEGER pchem(chemu)
      DOUBLE PRECISION x(nlayer, chemu, region)
      DOUBLE PRECISION deltaz
      REAL csdept, ishelf, time, rsalt

      INCLUDE "chmptr.inc"

      LOGICAL consrv(nchem)
      INTEGER ntimes
      PARAMETER (ntimes = 20)
      REAL depth(ntimes), shelf(ntimes), dates(ntimes)
      REAL refdep
      PARAMETER (refdep = 3750.0)

      INTEGER i, j, k, itime
      DOUBLE PRECISION dznew, height, table
      DOUBLE PRECISION sum, mult, ic(nchem), fract

      REAL lenna, lenso, area(3), rho(3)

      SAVE consrv, depth, dates, shelf, area, rho, ic
      INCLUDE "arrho.inc"

C*************************************************----------++++++++++!!
C     Now enforce conservation of the selected variables, and
C       evaluate the evolution of the ice shelf location, ocean depth,
C       and ocean salinity.

      height = table(time, depth)
      ishelf = table(time, shelf)
      dznew = (refdep+height)/(FLOAT(nlayer-1))

C     Put the new deltaz, salinity in.
      deltaz = dznew
      rsalt = ic(salt)*refdep/(refdep+height)
      csdept = 500.0 + height

      RETURN
      END
