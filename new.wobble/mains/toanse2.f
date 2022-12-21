      PROGRAM toanse
      IMPLICIT none
      INTEGER nx, ny, nt
      PARAMETER (nx = 192)
      PARAMETER (ny =  94)
      PARAMETER (nt = 68668)
      DOUBLE PRECISION y(nt)
      DOUBLE PRECISION sumy, sumy2

      INTEGER i, j, k, l, m
      CHARACTER*80 fname
      

      sumy = 0.D0
      sumy2 = 0.D0
      OPEN(11, FILE="r_and_rdot", FORM="FORMATTED", STATUS="OLD")

      DO k = 1, nt
        READ (11,*) sumy, y(k), sumy2
        WRITE(*,9001) 0.25*k-0.25,"	",y(k) - 1.
      ENDDO
 9001 FORMAT(F8.2,A1,E13.6)

      END
