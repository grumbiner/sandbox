      PROGRAM front
      implicit none

C     Serve as a front end to the 3 layer thermodynamic code of
C     Winton, 2000.

C     From the meteorological model:
      INTEGER nx, ny
      PARAMETER (nx = 768)
      PARAMETER (ny = 384)

C     For the ice code:
C     Ice variables
      REAL hs(nx, ny), hi(nx, ny), ts(nx, ny), t1(nx, ny), t2(nx, ny)
C     physics
      REAL thinice, rhoi, lf
      PARAMETER (rhoi = 905) !winton value
      PARAMETER (lf   = 3.34e5) !
      PARAMETER (thinice = 0.01)

      INTEGER i, j, tau, month, years
      REAL sum_rad(nx, ny), sum_sen(nx, ny)

      INTEGER count(nx, ny), latcount(ny)
      REAL imax(nx, ny), smax(nx, ny), land(nx, ny)

      OPEN(10, FILE="output", FORM="unformatted")
      READ(10) sum_rad, sum_sen, hs, hi, imax, smax, count
      CLOSE(10)
      OPEN(11, FILE="land", FORM="unformatted")
      READ(11) land
      CLOSE(11)

      sum_rad = sum_rad / 1460
      sum_sen = sum_sen / 1460

      DO j = 1, ny
        latcount(j) = 0
      DO i = 1, nx
        IF (land(i,j) .NE. 1.0 .AND. count(i,j) .GT. 0) THEN
          latcount(j) = latcount(j) + 1
CD          WRITE (*,9001),i,j,sum_rad(i,j), sum_sen(i,j), hs(i,j), 
CD     1                   hi(i,j), imax(i,j), smax(i,j), count(i,j)
        ENDIF
      ENDDO
      ENDDO
 9001 FORMAT (2I4,6F8.2,I5)
      DO j = 1, ny
        PRINT *,'lat ',j,' count = ',latcount(j)
      ENDDO


      STOP
      END
