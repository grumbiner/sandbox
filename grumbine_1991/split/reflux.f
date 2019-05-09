C***********************************************************----------!!
      SUBROUTINE reflux(vt, vc, ss, sd, h, flm, fls,
     1                          scrit, nx, ny, dx)
C     Recompute fluxes using a bottom water definition
      IMPLICIT none

      INTEGER nx, ny
      REAL vt(nx, ny), vc(nx, ny), ss(nx, ny), sd(nx, ny)
      REAL h(nx, ny), flm(ny), fls(ny)
      REAL dx, scrit
      INTEGER i, j
      REAL iconst

      iconst = dx*0.5*h(1,1)
      DO 1000 j = 1, ny
        flm(j) = 0.0
        fls(j) = 0.0
        DO 1010 i = 1, nx
C         Lower layer
          IF (vt(i,j)-vc(i,j) .GT. 0.0) THEN
            IF (ss(i,j)-sd(i,j) .GT. scrit) THEN
              flm(j) = flm(j) + (vt(i,j) - vc(i,j))
              fls(j) = fls(j) +
     1                    (vt(i,j) - vc(i,j))*(ss(i,j)-sd(i,j))
            ENDIF
          ENDIF
C         upper layer
          IF (vt(i,j)+vc(i,j) .GT. 0.0) THEN
            IF (ss(i,j)+sd(i,j) .GT. scrit) THEN
              flm(j) = flm(j) + (vt(i,j) + vc(i,j))
              fls(j) = fls(j) +
     1                    (vt(i,j) + vc(i,j))*(ss(i,j)+sd(i,j))
            ENDIF
          ENDIF
 1010   CONTINUE
        flm(j) = flm(j)*iconst
        fls(j) = fls(j)*iconst
 1000 CONTINUE

      RETURN
      END
