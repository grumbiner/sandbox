      PROGRAM Kill74
C     This program is designed to duplicate the program used in
C       Killworth's 1974 paper, with the capability to generalize
C       the model.

      INTEGER xmax, ymax
      PARAMETER ( xmax = 45 )
      PARAMETER ( ymax = 42 )

C*************************************************----------++++++++++!!

C     Common block /Main/ contains the arrays which hold the physical
C       variables of interest. u is the eastward velocity,
C       v is the northward velocity, t = rho1+ rho2, s=rho1 - rho2,
C       where rho1 is the density of the upper layer, and rho2 is the
C       density of the lower layer.
C       q is related to the freezing rate of ice.

      REAL u(0:xmax+1, 0:ymax), v(0:xmax+1, 0:ymax),
     1     s(0:xmax+1, 0:ymax), t(0:xmax+1, 0:ymax),
     2     q(0:xmax+1, 0:ymax)


C     Common /params/ has the various parameters that control the
C       execution of the program, and the various physical constants.
C     Deltat is the time step, Deltax is the grid spacing (same in
C       both the x and y directions), E is as defined in the paper,
C       R is a Rossby number (defined in the paper), Epsilon is
C       related to the diffusion of salinity, Sigma, xbox is the
C       value of the right-hand side of the weddell sea, ycoast
C       is the y coordinate of the coast outside the bay, tend is
C       the end time of the extrapolation in years.

      COMMON /params/ deltat, deltax, e, r, epsiln, sigma,
     1                xbox, ycoast, tend, debug, outden

      REAL deltat, deltax, e, r, epsiln, sigma, tend
      INTEGER xbox, ycoast, outden
      LOGICAL debug

C     Local variables
      INTEGER tstep

C     Variables for timing information.
      INTEGER clock, temp
      INTEGER uvstim, uvtim, sttim, qtim, outtim

C*************************************************----------++++++++++!!
C     Now begin the actual execution of the program.
C     INIT initializes u, v, s, t, and /params/.
C     OUTSET  initializes the I/O.
C     UVSET initializes the extrapolation for u and v.
C     QSET initializes the extrapolation of Q, reads in some param-
C          eters used locally to the routine.
C     CALL OUTDAT writes out the initial conditions.

C     Initialize the timing variables.
      uvstim = 0
      uvtim  = 0
      sttim  = 0
      qtim   = 0
      outtim = 0

      tstep = 0

      CALL OUTSET(u, v, s, t, q, tstep)
      CALL INIT(u, v, s, t)
      temp = clock(0)
      CALL UVSET(u, v, s, t)
      PRINT *, 'Time spent in initializing uv =',(clock(0)-temp)/1.e6
      CALL QSET(q, tstep, deltat )
      CALL OUTDAT(u, v, s, t, q, 0 )

C     This is the actual extrapolation loop.
C     UVEXT extrapolates u and v to the next time step.
C     STEXT extrapolates s and t to the next time step.
C     QEXT  evaluates q for the next time step.
C     OUTDAT prints out w (upwelling velocity), rho1 (density of the
C            upper layer), and rho2 (density of the lower layer).

C*************************************************----------++++++++++!!
      DO 1000 tstep = 1, INT( tend/ deltat)

        temp = clock(0)
        CALL UVEXT(u, v, s, t)
C       IF (debug) WRITE (*,9001) tstep
        uvtim = uvtim + clock(0) - temp

        temp = clock(0)
        CALL STEXT(u, v, s, t, q)
C       IF (debug) WRITE (*,9002) tstep
        sttim = sttim + clock (0) - temp

        temp = clock(0)
        CALL QEXT(q, tstep, deltat)
C       IF (debug) WRITE (*,9003) tstep
        qtim = qtim + clock (0) - temp

        temp = clock(0)
        IF (MOD( tstep, outden) .EQ. 0)
     1      CALL OUTDAT(u, v, s, t, q, tstep)
        outtim = outtim + clock (0) - temp

 1000 CONTINUE

      PRINT *,'Time spent in u-v extrapolation:', uvtim/1.E6

      PRINT *,'Time spent in s-t extrapolation:', sttim/1.E6

      PRINT *,'Time spent in q extrapolation:  ', qtim/1.E6

      PRINT *,'Time spent on output:           ', outtim/1.E6
C     OUTEND closes all the files and does any clean up that is needed.
      CALL OUTEND(u, v, s, t, q, tstep)

 9001 FORMAT (' Finished uvext, tstep=',I6)

 9002 FORMAT (' Finished stext, tstep=',I6)

 9003 FORMAT (' Finished qext, tstep=',I6)

      END
C*************************************************----------++++++++++!!
C*************************************************----------++++++++++!!
      SUBROUTINE INIT(u, v, s, t)

      INTEGER xmax, ymax
      PARAMETER (xmax = 45)
      PARAMETER (ymax = 42)

      REAL   u(0:xmax+1, 0:ymax), v(0:xmax+1, 0:ymax),
     2       s(0:xmax+1, 0:ymax), t(0:xmax+1, 0:ymax)


      COMMON /params/ deltat, deltax, e, r, epsiln, sigma,
     1                xbox, ybox, tend, debug, outden

      REAL deltat, deltax, e, r, epsiln, sigma, tend
      INTEGER xbox, ybox, outden
      LOGICAL debug


C     Local variables
      INTEGER i, j
      REAL sinit
C     Params for nonzero initialization.
      INTEGER n, m
      REAL pi
C     Read in parameters.
      READ (4,9001) deltat
      READ (4,9001) deltax
      READ (4,9001) e
      READ (4,9001) r
      READ (4,9001) epsiln
      READ (4,9001) sigma
      READ (4,9002) xbox
      READ (4,9002) ybox
      READ (4,9001) tend
      READ (4,9001) sinit
      READ (4,9003) debug
      READ (4,9002) outden

      WRITE (6,9001) deltat
      WRITE (6,9001) deltax
      WRITE (6,9001) e
      WRITE (6,9001) r
      WRITE (6,9001) epsiln
      WRITE (6,9001) sigma
      WRITE (6,9002) xbox
      WRITE (6,9002) ybox
      WRITE (6,9001) tend
      WRITE (6,9001) sinit
      WRITE (6,9003) debug
      WRITE (6,9002) outden

      DO 1000 j = 0, ymax
        DO 1010 i = 0, xmax + 1

          IF (i .GT. xbox .AND. j .LT. ybox) THEN
            s(i, j) = 0.0
           ELSE
C           not on continent
            s(i, j) = sinit
          ENDIF

          t(i, j) = 0.0
          u(i, j) = 0.0
          v(i, j) = 0.0

 1010   CONTINUE
 1000 CONTINUE

C     This is the place to initialize selected points to non-zero values
C     pi = 3.14159236
C     n = 6
C     m = 0
C     DO 9998 j = 1, ymax
C       DO 9999 i = 0, xmax+1
C         u(i, j) = 1.0*sin(n*pi*i/xbox)*(exp(n*pi*1.4*j/ymax)-
C    1                 exp(-n*pi*1.4*j/ymax))/ (exp(n*pi*1.4)-
C    2                 exp(-n*pi*1.4) )
C         v(i, j) = 0.0
C9999   CONTINUE
C9998 CONTINUE


 9001 FORMAT (F8.4)

 9002 FORMAT (I5)

 9003 FORMAT (L10)

      RETURN
      END
C*************************************************----------++++++++++!!
C*************************************************----------++++++++++!!
      SUBROUTINE OUTDAT(u, v, s, t, q, tstep)

C     This subroutine is supposed to compute the upwelling velocity,
C       density of the surface layer, density of the lower layer,
C       and flux through the mouth of the bay.

      INTEGER xmax, ymax
      PARAMETER (xmax = 45)
      PARAMETER (ymax = 42)

      REAL  u(0:xmax+1, 0:ymax), v(0:xmax+1, 0:ymax),
     1      s(0:xmax+1, 0:ymax), t(0:xmax+1, 0:ymax),
     2      q(0:xmax+1, 0:ymax)


      COMMON /params/ deltat, deltax, e, r, epsiln, sigma,
     1                xbox, ybox, tend, debug, outden

      REAL deltat, deltax, e, r, epsiln, sigma, tend
      INTEGER xbox, ybox, outden
      LOGICAL debug

C     Local variables:
C       flux is the flux of water across the continental shelf break.
C     local computation of w, rho1, rho2 removed 7-23-86. BG.
      REAL flux

      INTEGER tstep
      INTEGER i, j, yshelf

      SAVE /params/

C     Find the flux
      yshelf = INT (5.0*ymax/14.0 + .5)
      flux   = 0.0
      DO 1100 i = 1, xbox
        flux = flux + v(i, yshelf)
 1100 CONTINUE

C     Put the time step in front of the data.
C     Use unformatted output for everything other than the flux.  This i
C       faster than formatted I/O.
      WRITE (13) tstep
      WRITE (14) tstep
C     WRITE (15) tstep
      WRITE (17) tstep
      WRITE (18) tstep

C     Set u(4*xmax/5, ymax/3) to 0.25 to give a known length vector for
C       streamline velocity fields.
      u(4*xmax/5, ymax/3) = 0.25

C     Now write out the results.
C       Output using only the name of the matrix.  This is faster than
C         using element-by-element reference
        WRITE (13) u
        WRITE (14) v
C       WRITE (15) q
        WRITE (17) s
        WRITE (18) t

      WRITE (16,9003) FLOAT(tstep), flux

      RETURN

      ENTRY OUTSET(u, v, s, t, q, tstep)

C     This entry is to open all files that need to be opened, and
C       put header information at the beginning of each data file.
      OPEN (4,  FILE = 'INPUT' , STATUS = 'OLD')
      OPEN (13, FILE = 'U.O'   , FORM = 'UNFORMATTED', STATUS = 'NEW')
      OPEN (14, FILE = 'V.O'   , FORM = 'UNFORMATTED', STATUS = 'NEW')
C     OPEN (15, FILE = 'Q.O'   , FORM = 'UNFORMATTED', STATUS = 'NEW')
      OPEN (16, FILE = 'FLUX.O', STATUS = 'NEW')
      OPEN (17, FILE = 'S.O'   , FORM = 'UNFORMATTED', STATUS = 'NEW')
      OPEN (18, FILE = 'T.O'   , FORM = 'UNFORMATTED', STATUS = 'NEW')


      RETURN

      ENTRY OUTEND(u, v, s, t, q, tstep)

C     This entry is to close all files and do any needed cleanup.

      CLOSE (4,  STATUS = 'KEEP')
      CLOSE (13, STATUS = 'KEEP')
      CLOSE (14, STATUS = 'KEEP')
C     CLOSE (15, STATUS = 'KEEP')
      CLOSE (16, STATUS = 'KEEP')
      CLOSE (17, STATUS = 'KEEP')
      CLOSE (18, STATUS = 'KEEP')

      RETURN

 9001 FORMAT (I6)

 9003 FORMAT (2G11.4)

      END
C*************************************************----------++++++++++!!
C*************************************************----------++++++++++!!
      SUBROUTINE QEXT( q, tstep, deltat )

C     The purpose of this routine is to determine the rate of ice
C       formation over the domain of the model.  This is then used
C       as an input of salinity to the water, due to fractionation
C       in the freezing process.

      INTEGER xmax, ymax
      PARAMETER (xmax = 45)
      PARAMETER (ymax = 42)

      REAL q(0:xmax+1, 0:ymax)


C     Local variables
      INTEGER loy, tstep, endwin, sumst, sumend
      INTEGER tofyr, i, j, yshelf
      REAL deltat, qmax

      SAVE

C     Begin the calculation.
      tofyr = MOD( tstep, loy)

      IF (tofyr .LE. endwin) THEN

C       It is the winter time.
        DO 1000 j = 0, ymax
          DO 1010 i = 0, xmax + 1
            q(i, j) = qmax*(-FLOAT(j)/FLOAT(yshelf) + 1.0 )**2
            IF ( j .GT. yshelf ) q(i,j) = 0.0
 1010     CONTINUE
 1000   CONTINUE

       ELSE
C       It is not winter
        DO 1100 j = 0, ymax
          DO 1110 i = 0, xmax + 1
            q(i, j) = 0.0
 1110     CONTINUE
 1100   CONTINUE

      ENDIF

      RETURN

      ENTRY QSET( q, tstep, deltat )
C     Entry pt. for initialization of routine.

      READ (4,9001) endwin
      READ (4,9001) sumst
      READ (4,9001) sumend
      READ (4,9001) yshelf
      READ (4,9002) qmax

      WRITE (*,9001) endwin
      WRITE (*,9001) sumst
      WRITE (*,9001) sumend
      WRITE (*,9001) yshelf
      WRITE (*,9002) qmax

      loy = INT( 1./ deltat )

 9001 FORMAT (I5)

 9002 FORMAT (F10.5)

      RETURN
      END
C*************************************************----------++++++++++!!
C*************************************************----------++++++++++!!
      SUBROUTINE STEXT(u, v, s, t, q)
C     Subroutine to extrapolate s (rho1 - rho2) and t (rho1+rho2),
C       where rho1 is the density of the upper layer and rho2 is
C       the density of the lower layer.

      INTEGER xmax, ymax
      PARAMETER (xmax = 45)
      PARAMETER (ymax = 42)

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
      REAL sforce(1:xmax, 0:ymax), tforce(1:xmax, 0:ymax)

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
C*************************************************----------++++++++++!!
C*************************************************----------++++++++++!!
      SUBROUTINE UVEXT( u, v, s, t )
C     This subroutine extrapolates u and v to the next time step
C       using fully implicit differencing on u, v, and fully explicit
C       differencing on s, t.

      INTEGER xmax, ymax
      PARAMETER (xmax = 45)
      PARAMETER (ymax = 42)

      REAL  u(0:xmax+1, 0:ymax), v(0:xmax+1, 0:ymax),
     1      s(0:xmax+1, 0:ymax), t(0:xmax+1, 0:ymax)


      COMMON /params/ deltat, deltax, e, r, epsiln, sigma,
     1                xbox, ycoast, tend, debug, outden

      REAL deltat, deltax, e, r, epsiln, sigma, tend
      INTEGER xbox, ycoast, outden
      LOGICAL debug


C     Matrices to compute u and v at the new time step.
      REAL uvabd(6*xmax+1, 2*xmax*ymax),
     1                force(2*xmax*ymax)

C     Variables for the LINPACK routines.
      INTEGER IPVT(2*xmax*ymax), INFO


C     Local variables
      REAL rdt, edxdx, halfdx
      INTEGER i, j, row

      SAVE uvabd, IPVT, INFO, rdt, edxdx, halfdx, /params/

C$EMA  uvabd

C     Begin the extrapolation.
      row  = -1

      DO 1000 j = 1, ymax
        DO 1010 i = 1, xmax

          row = row + 2

          IF ( (j .EQ. 1  .AND. i .LE. xbox) .OR. (j .EQ. ycoast
     1           .AND. i .GE. xbox) )  THEN
C         Apply no slip condition at horizontal bndy.
          force(row)   = 0.0
          force(row+1) = 0.0

         ELSE IF (j .LT. ycoast .AND. (i .EQ. 1 .OR. i .EQ. xbox))
     1    THEN
C         Apply no slip at a vertical boundary.
          force(row)   = 0.0
          force(row+1) = 0.0

         ELSE IF (j .EQ. ymax) THEN
C         Apply no flux in y direction here.
          force(row)   = rdt*u(i,j)+ (t(i+1,j)-t(i-1,j))*halfdx
          force(row+1) = rdt * v(i, j)
          IF (i .EQ. xmax) THEN
C           Experimentally remove the velocity at the previous step.
C           The idea here is that I don't think I can compute the full
C           laplacian at xmax, ymax.  In doing this I do allow there to
C           be a velocity at the corner of the domain, but it has no mem
C           for previous values.
            force(row  ) = (t(i+1,j)-t(i-1,j))*halfdx
            force(row+1) = 0.0
          ENDIF
C*************************************************----------++++++++++!!

         ELSE IF (j .LT. ycoast .AND. i .GT. xbox) THEN
C         You are on the continent.
          force(row)   = 0.0
          force(row+1) = 0.0

         ELSE
C         Main body of the sea.
          force(row)   = rdt* u(i,j) +(t(i+1,j)-t(i-1,j))*halfdx
          force(row+1) = rdt* v(i,j) +(t(i,j+1)-t(i,j-1))*halfdx

        ENDIF

 1010   CONTINUE
 1000 CONTINUE

C     Now call the routine to do the back substitution.
C     SGBSL is the LINPACK single precision routine for back substitutio
C       in banded matrices.
C     IF (debug) WRITE (6,9002)
      CALL SGBSL (uvabd, 6*xmax+1, 2*xmax*ymax, 2*xmax, 2*xmax,
     1              IPVT, force, 0)

C     Fill u and v with the appropriate values.
      row = -1
      DO 2000 j = 1, ymax
        DO 2010 i = 1, xmax

          row = row + 2
          u(i,j) = force(row)
          v(i,j) = force(row+1)

 2010   CONTINUE
 2000 CONTINUE

C     Apply the periodicity condition.
      DO 3000 j = 0, ymax
        IF (j .GE. ycoast) THEN
          u(0,     j) = u(xmax, j)
          u(xmax+1,j) = u(1,    j)
          v(0,     j) = v(xmax, j)
          v(xmax+1,j) = v(1,    j)
         ELSE
          u(0,     j) = 0.0
          u(xmax+1,j) = 0.0
          v(0,     j) = 0.0
          v(xmax+1,j) = 0.0
        ENDIF
 3000 CONTINUE

 9002 FORMAT (' calling back substitution')

      RETURN

      ENTRY UVSET(u, v, s, t)
C     Entry to initialize some constants, and get the LU decomposition
C       of the UV matrix.  Save the factorization to speed the back
C       substitution.

C     First, fill uvabd with 0's.
      DO 5000 j = 1, 2*xmax*ymax
        DO 5010 i = 1, 6*xmax+1
          uvabd(i,j) = 0.0
 5010   CONTINUE
 5000 CONTINUE

C     Compute some common constants.
      rdt    = r/deltat
      edxdx  = e/deltax**2
      halfdx = 1./(4.*deltax)

C     Now fill the coefficients matrix
      row = -1
      DO 6000 j = 1, ymax
        DO 6010 i = 1, xmax
        row = row + 2
        IF ( j .EQ. 1 .AND. i .LE. xbox .OR. (j .EQ. ycoast .AND.
     1         i .GE. xbox) ) THEN
C         Apply flux at at a horizontal boundary.
C           The constant is arbitrary (since the forcing is 0), so
C           choose 1.
          uvabd(4*xmax+1, row  ) = 1.0
          uvabd(4*xmax+1, row+1) = 1.0

         ELSE IF (j .LT. ycoast .AND. (i .EQ. 1 .OR. i .EQ. xbox))
     1    THEN
C         Apply no slip at a vertical boundary.
C           Constant is arbitrary, so use 1.
          uvabd(4*xmax+1, row  ) = 1.0
          uvabd(4*xmax+1, row+1) = 1.0

         ELSE IF (j .eq. ymax) THEN
          IF (i .NE. xmax) THEN
C           No flux in the y direction.
C           Equations for u
            uvabd(4*xmax+1, row  ) = rdt + 3.*edxdx
            uvabd(4*xmax  , row+1) = 1.0
            uvabd(4*xmax-1, row+2) = -edxdx
            uvabd(4*xmax+3, row-2) = -edxdx
            uvabd(6*xmax+1, row-2*xmax) = -edxdx
C           now the equations for v.
            uvabd(4*xmax+1, row+1) = rdt + 3.*edxdx
            uvabd(4*xmax+2, row  ) = -1.0
            uvabd(4*xmax-1, row+3) = -edxdx
            uvabd(4*xmax+3, row-1) = -edxdx
            uvabd(6*xmax+1, row+1-2*xmax) = -edxdx
           ELSE
C           use second deriv in x = 0 to keep from overflowing the array
C             this only occurs at one point, so there shouldn't be much
C             from this point.
            uvabd(4*xmax+1, row  ) = rdt + 3.*edxdx
            uvabd(4*xmax  , row+1) = 1.0
            uvabd(4*xmax+3, row-2) = -edxdx
            uvabd(6*xmax+1, row-2*xmax) = -edxdx
            uvabd(4*xmax+1, row+1) = rdt + 3.*edxdx
            uvabd(4*xmax+2, row  ) = 1.0
            uvabd(4*xmax+3, row-1) = -edxdx
            uvabd(6*xmax+1, row+1-2*xmax) = -edxdx
          ENDIF
         ELSE IF (j .LT. ycoast .AND. i .GT. xbox) THEN
C         On the continent.  Coefficients are arbitrary, since there
C           is no forcing, so use 1.
          uvabd(4*xmax+1, row  ) = 1.0
          uvabd(4*xmax+1, row+1) = 1.0

         ELSE
C         Not at a boundary
C         Equations for u.
          uvabd(4*xmax+1, row  ) = rdt + 4.*edxdx
          uvabd(4*xmax  , row+1) = 1.0
          uvabd(4*xmax-1, row+2) = -edxdx
          uvabd(4*xmax+3, row-2) = -edxdx
          uvabd(2*xmax+1, row+2*xmax) = -edxdx
          uvabd(6*xmax+1, row-2*xmax) = -edxdx
C         equations for v.
          uvabd(4*xmax+1, row+1) = rdt + 4.*edxdx
          uvabd(4*xmax+2, row  ) = -1.0
          uvabd(4*xmax-1, row+3) = -edxdx
          uvabd(4*xmax+3, row-1) = -edxdx
          uvabd(2*xmax+1, row+1+2*xmax) = -edxdx
          uvabd(6*xmax+1, row+1-2*xmax) = -edxdx

        ENDIF

 6010   CONTINUE
 6000 CONTINUE

C     Do the factorization.
C     SGBFA is the LINPACK routine for factoring a banded matrix,
C       single precision.
      CALL SGBFA( uvabd, 6*xmax+1, 2*xmax*ymax, 2*xmax, 2*xmax,
     1            IPVT, INFO)
      IF (INFO .NE. 0) STOP 'unusable decompostion'

      RETURN
      END
C*************************************************----------++++++++++!!
