      PROGRAM toanse
      IMPLICIT none
      INTEGER nx, ny, nt
      PARAMETER (nx = 192)
      PARAMETER (ny =  94)
      PARAMETER (nt = 68668)
      REAL x(nx, ny)
      REAL y(nt)
      DOUBLE PRECISION sumx, sumy, sumx2, sumy2

      INTEGER i, j, k, l, m
      CHARACTER*80 fname
      

      READ(*,*) l
      READ (*,*) m
      WRITE(fname,9002) l, m
 9002 FORMAT("extracted",I3,"_",I2)
      sumx = 0.D0
      sumy = 0.D0
      sumx2 = 0.D0
      sumy2 = 0.D0

      OPEN(10, FORM="UNFORMATTED")
      OPEN(11, FILE=fname, FORM="UNFORMATTED", STATUS="OLD")
      READ(11) y

      DO k = 1, nt
        READ (10) x
        WRITE(*,9001) 0.25*k,"	",x(l,m),"	",y(k)
        sumx = sumx + x(l,m)
        sumy = sumy + y(k)
        sumx2 = sumx2 + x(l,m)*x(l,m)
        sumy2 = sumy2 + y(k)*y(k)
      ENDDO
 9001 FORMAT(F8.2,A1,F6.2,A1,F6.2)
      PRINT *,"x sumx sumx2 var =",sumx/nt, sqrt(sumx2/nt), 
     1                 sqrt((sumx2 - sumx*sumx/nt)/nt) 
      PRINT *,"y sumy sumy2 var =",sumy/nt, sqrt(sumy2/nt), 
     1                 sqrt((sumy2 - sumy*sumy/nt)/nt) 

      END
