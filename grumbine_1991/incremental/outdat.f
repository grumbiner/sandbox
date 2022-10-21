C*************************************************----------++++++++++!!
      SUBROUTINE OUTDAT(xmax, ymax, u, v, s, t, q, tstep)

C     This subroutine is supposed to compute the upwelling velocity,
C       density of the surface layer, density of the lower layer,
C       and flux through the mouth of the bay.

      INTEGER xmax, ymax

      REAL  u(0:xmax+1, 0:ymax), v(0:xmax+1, 0:ymax),
     1      s(0:xmax+1, 0:ymax), t(0:xmax+1, 0:ymax),
     2      q(0:xmax+1, 0:ymax)


      COMMON /params/ deltat, deltax, e, r, epsiln, sigma,
     1                xbox, ybox, tend, debug, outden

      REAL deltat, deltax, e, r, epsiln, sigma, tend
      INTEGER xbox, ybox, outden
      LOGICAL debug

C     Local variables:
C       w is the upwelling velocity
C       rho1 is the density of the upper layer.
C       rho2 is the density of the lower layer.
C       flux is the flux of water across the continental shelf break.
C     local computation of w, rho1, rho2 removed 7-23-86. BG.
C     REAL w(0:xmax+1, 0:ymax)
C     REAL rho1(0:xmax+1, 0:ymax), rho2(0:xmax+1, 0:ymax)
      REAL flux

      INTEGER tstep
      INTEGER i, j, yshelf

      SAVE /params/

C     Find w, rho1, rho2 in the interior points.
C     DO 1000 j = 1, ymax-1
C       DO 1010 i = 1, xmax

C         w(i, j)     = (u(i+1,j)-u(i-1,j)+v(i,j+1)-v(i,j-1))/
C    1                   (2.*deltax)
C         rho1 (i, j) =  t(i, j) + s(i, j)
C         rho2 (i, j) =  t(i, j) - s(i, j)

C1010   CONTINUE
C1000 CONTINUE

C     Find w, rho1, rho2 along y=ymax.
C     DO 1020 i = 1, xmax
C       w(i, ymax) = (u(i+1, ymax) - u(i-1, ymax)) / (2.*deltax)
C       rho1(i, ymax) = t(i, ymax) + s(i, ymax)
C       rho2(i, ymax) = t(i, ymax) - s(i, ymax)
C1020 CONTINUE

C     Find w, rho1, rho2 along x=0, xmax+1.
C     DO 1030 j = 1, ymax-1
C       w(0, j)    =  (u(1, j)-u(xmax-1, j) + v(0, j+1) - v(0, j-1))/
C    1                 (2.*deltax)
C       w(xmax+1,j) = (u(2,j)-u(xmax,j) + v(xmax+1,j+1)-v(xmax+1,j-1))/
C    1                 (2.*deltax)
C       rho1(0, j)      = t(0, j) + s(0, j)
C       rho2(0, j)      = t(0, j) - s(0, j)
C       rho1(xmax+1, j) = t(xmax, j) + s(xmax, j)
C       rho2(xmax+1, j) = t(xmax+1, j) - t(xmax+1, j)
C1030 CONTINUE

C     Finish the two points in the corners y=ymax, x= 0, xmax+1.
C     w(0   , ymax) = (u(1, ymax)- u(xmax-1, ymax))/ (2.*deltax)
C     w(xmax+1, ymax) = (u(2, ymax)-u(xmax, ymax))/ (2.*deltax)
C     rho1(0, ymax)   = t(0, ymax) + s(0, ymax)
C     rho2(0, ymax)   = t(0, ymax) - s(0, ymax)
C     rho1(xmax+1, ymax) = t(xmax+1, ymax) + s(xmax+1, ymax)
C     rho2(xmax+1, ymax) = t(xmax+1, ymax) - s(xmax+1, ymax)

C     Find the flux
      yshelf = INT (5.0*ymax/14.0 + .5)
      flux   = 0.0
      DO 1100 i = 1, xbox
        flux = flux + v(i, yshelf)
 1100 CONTINUE

C     Put the time step in front of the data.
C     Use unformatted output for everything other than the flux.  This i
C       faster than formatted I/O.
C     WRITE (10) tstep
C     WRITE (11) tstep
C     WRITE (12) tstep
      WRITE (13) tstep
      WRITE (14) tstep
      WRITE (15) tstep
      WRITE (16,9001) tstep
      WRITE (17) tstep
      WRITE (18) tstep

C     Set u(4*xmax/5, ymax/3) to 0.4 to give a known length vector for t
C       streamline velocity fields.
      u(4*xmax/5, ymax/3) = 0.4

C     Now write out the results.
C       Output using only the name of the matrix.  This is faster than
C         using element-by-element reference
C       WRITE (10) w
C       WRITE (11) rho1
C       WRITE (12) rho2
        WRITE (13) u
        WRITE (14) v
        WRITE (15) q
        WRITE (17) s
        WRITE (18) t

      WRITE (16,9003) flux

      RETURN

      ENTRY OUTSET(xmax, ymax, u, v, s, t, q, tstep)
C     This entry is to open all files that need to be opened, and
C       put header information at the beginning of each data file.
      OPEN (4,  FILE = 'INPUT' , STATUS = 'OLD')
C     OPEN (10, FILE = 'W.O'   , FORM = 'UNFORMATTED', STATUS = 'NEW')
C     OPEN (11, FILE = 'RHO1.O', FORM = 'UNFORMATTED', STATUS = 'NEW')
C     OPEN (12, FILE = 'RHO2.O', FORM = 'UNFORMATTED', STATUS = 'NEW')
      OPEN (13, FILE = 'U.O'   , FORM = 'UNFORMATTED', STATUS = 'NEW')
      OPEN (14, FILE = 'V.O'   , FORM = 'UNFORMATTED', STATUS = 'NEW')
      OPEN (15, FILE = 'Q.O'   , FORM = 'UNFORMATTED', STATUS = 'NEW')
      OPEN (16, FILE = 'FLUX.O', STATUS = 'NEW')
      OPEN (17, FILE = 'S.O'   , FORM = 'UNFORMATTED', STATUS = 'NEW')
      OPEN (18, FILE = 'T.O'   , FORM = 'UNFORMATTED', STATUS = 'NEW')

C     Initialize the data matrices.
C     Only initialize j=0.  The rest will be computed.
C     DO 3010 i = 0, xmax+1

C       w   (i, 0) = 0.0
C       rho1(i, 0) = 0.0
C       rho2(i, 0) = 0.0

C3010 CONTINUE

      RETURN

      ENTRY OUTEND(xmax, ymax, u, v, s, t, q, tstep)
C     This entry is to close all files and do any needed cleanup.

      CLOSE (4,  STATUS = 'KEEP')
C     CLOSE (10, STATUS = 'KEEP')
C     CLOSE (11, STATUS = 'KEEP')
C     CLOSE (12, STATUS = 'KEEP')
      CLOSE (13, STATUS = 'KEEP')
      CLOSE (14, STATUS = 'KEEP')
      CLOSE (15, STATUS = 'KEEP')
      CLOSE (16, STATUS = 'KEEP')
      CLOSE (17, STATUS = 'KEEP')
      CLOSE (18, STATUS = 'KEEP')

      RETURN

 9001 FORMAT (I6)

 9003 FORMAT (6E11.4)

      END
