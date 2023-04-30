      PROGRAM toanse
      IMPLICIT none
      INTEGER nx, ny, nt
      PARAMETER (nx = 194)
      PARAMETER (ny =  92)
      PARAMETER (nt = 68668)
      REAL x(nx, ny)
      REAL y(nt)

      INTEGER i, j
      
      OPEN(10, FORM="UNFORMATTED")
      READ(10) y

      DO i = 1, nt
        WRITE(*,9001) 0.25*i,"	",y(i)
      ENDDO
 9001 FORMAT(F8.2,A1,F6.2)

      END
