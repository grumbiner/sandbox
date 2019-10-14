      SUBROUTINE getxyz1(x, y, z, clim, i)
C     Subroutine to return vectors with
C       ice concentrations from three consecutive days
C       to John's regression program.
C     Robert Grumbine 27 November 1996

      INTEGER nx, ny
      PARAMETER (nx = 385)
      PARAMETER (ny = 465)

      REAL x(nx*ny), y(nx*ny), z(nx*ny), clim(nx*ny)
      REAL xt, yt, zt, ct
      INTEGER i

      OPEN (99, FILE="test.out", FORM="FORMATTED", STATUS="OLD")

      i = 0
 1000 CONTINUE
        READ (99, *, END=2000) xt, yt, ct, zt
        IF (xt .LT. 128 .AND. yt .LT. 128 .AND. zt .LT. 128) THEN
          i = i + 1
          x(i) = xt
          y(i) = yt
          clim(i) = ct
          z(i) = zt
        ENDIF
        GO TO 1000

 2000 CONTINUE

      PRINT *,"i = ",i
 
      RETURN
      END
