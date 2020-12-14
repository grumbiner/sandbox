      FUNCTION FY(x,y)
C     Convert a grid point pair to latitude and longitude for mapping
C       conrec onto a supmap.
C     Bob Grumbine 16 June 1994.

      IMPLICIT none

      INCLUDE "icegrid.inc"
      REAL FY, x, y
      REAL xlon, ylat, fylat, ydum
      COMMON /xytrn/ nm, nn, itrans
      INTEGER nm, nn, itrans

      nm = L+1
      nn = M+1
      itrans = 3

      CALL mapxy((x-1)*dx+xorig, (y-1)*dy+yorig, 
     1      ylat, xlon, slat, slon, sgn, 
     2            eccen2**0.5, rearth)

      CALL maptrn(ylat, xlon, ydum, fylat)
      fy = fylat

      RETURN
      END 
