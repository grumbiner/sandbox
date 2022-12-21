C*************************************************----------++++++++++!!
      SUBROUTINE STEXT(xmax, ymax, u, v, s, t, q)
C     Subroutine to extrapolate s (rho1 - rho2) and t (rho1+rho2),
C       where rho1 is the density of the upper layer and rho2 is
C       the density of the lower layer.

      INTEGER xmax, ymax

      REAL u(0:xmax+1, 0:ymax), v(0:xmax+1, 0:ymax),
     1     s(0:xmax+1, 0:ymax), t(0:xmax+1, 0:ymax),
     2     q(0:xmax+1, 0:ymax)


      COMMON /params/ deltat, deltax, e, r, epsiln, sigma,
     1                xbox, ycoast, tend, debug, outden

      REAL deltat, deltax, e, r, epsiln, sigma, tend
      INTEGER xbox, ycoast, outden
      LOGICAL debug


C     Local variables
      INTEGER i, j
C     WARNing! the dimension of these arrays is hard-coded.
C     It should be large enough for now (xmax=45, ymax=42), but
C       this must be changed if the domain is enlarged.
      REAL sforce(1:45, 0:42), tforce(1:45, 0:42)

      SAVE

C     Fill the forcing matrices
C*************************************************----------++++++++++!!
      DO 1000 j = 1, ymax
        DO 1010 i = 1, xmax

          IF ( (j .EQ. 1 .AND. i .LE. xbox) .OR. (j .EQ. ycoast
     1           .AND. i .GE. xbox ) ) THEN
C           Apply no salinity flux at a horizontal boundary.
            tforce(i, j) = t(i, j+1)/deltat
            sforce(i, j) = s(i, j+1)/deltat

           ELSE IF(j .LT. ycoast .AND. (i .EQ. 1 .OR. i .EQ. xbox))
     1      THEN
C           Apply no salinity flux at the vert. bndys.
             IF (i .EQ. 1) THEN
C             Boundary to left of point.
              tforce(i, j) = t(i+1, j)/deltat
              sforce(i, j) = s(i+1, j)/deltat
             ELSE
C             Boundary to right of point.
              tforce(i, j) = t(i-1, j)/deltat
              sforce(i, j) = s(i-1, j)/deltat
             ENDIF

           ELSE IF(j .EQ. ymax) THEN
C           No flux through top of region.
            tforce(i, j) = t(i, j-1)/deltat
            sforce(i, j) = s(i, j-1)/deltat

           ELSE IF(j .LT. ycoast .AND. i .GT. xbox) THEN
C           On the continent
            tforce(i, j) = 0.0
            sforce(i, j) = 0.0

           ELSE
C           In the main part of the sea.
            tforce(i, j) = t(i, j)/deltat
C*************************************************----------++++++++++!!
     1        -(u(i+1, j)*s(i+1, j)-u(i-1, j)*s(i-1, j))/(deltax*2.)
     2        -(v(i, j+1)*s(i, j+1)-v(i, j-1)*s(i, j-1))/(deltax*2.)
     3        +epsiln*( t(i+1, j)+t(i-1, j)+t(i, j+1)+t(i, j-1)
     3                   -4.*t(i,j) ) / deltax**2
     4        +q(i,j)


            sforce (i, j) = s(i, j)/deltat
     1        -u(i, j)*(t(i+1 ,j)-t(i-1, j))/(deltax*2.)
     2        -v(i, j)*(t(i, j+1)-t(i, j-1))/(deltax*2.)
     3        +epsiln*( s(i+1,j)+s(i-1,j)+s(i,j+1)+s(i,j-1)
     3                    -4.*s(i, j) )/deltax**2
     4        +q(i,j)
     5        -sigma*s(i, j)

          ENDIF

 1010   CONTINUE
 1000 CONTINUE

C      Now evaluate t, s at time n+1
       DO 2000 j = 1, ymax
         DO 2010 i = 1, xmax

           t(i, j) = tforce(i, j)*deltat
           s(i, j) = sforce(i, j)*deltat

 2010   CONTINUE
 2000 CONTINUE

C     Apply the periodicity condition.
      DO 3000 j = ycoast, ymax

        t(0,      j) = t(xmax, j)
        t(xmax+1, j) = t(1,    j)
        s(0,      j) = s(xmax, j)
        s(xmax+1, j) = s(1,    j)

 3000 CONTINUE

      RETURN
      END
