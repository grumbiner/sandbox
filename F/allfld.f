C********(*********(*********(*********(*********(*********(---------!++
      SUBROUTINE allfld(fin, fout)
C     Create a file with all the fluxfile fields on the polar grid.
C     fin specifies the input file number, fout is the output file.
C     nlong, nlat should be redundant.
C     Bob Grumbine 8 April 1994.

      IMPLICIT none

      INTEGER fin, fout

      INCLUDE "icegrid.inc"
      INCLUDE "mgrid.inc"

      INTEGER iic, i, j, k, nlong, nlat, nfield
      PARAMETER (nlong = 360/dlonm)
      PARAMETER (nlat  = 180./dlatm + 1)
      PARAMETER (nfield = 36)
      REAL all(nlong, nlat, nfield), polar(0:L, 0:M, nfield)
      REAL out(0:L, 0:M)
 
      DO 1000 IIC = 1, 1
        CALL allflx(all, fin)
        DO 1100 k = 3, 36
          CALL terp(all(1,1,k), polar(0,0,k), 1)
          DO 1200 j = 0, M
            DO 1200 i = 0, L
              out(i,j) = polar(i,j,k)
 1200     CONTINUE
          WRITE (fout) out
 1100   CONTINUE
 1000 CONTINUE
      REWIND (fout)

      RETURN
      END 
