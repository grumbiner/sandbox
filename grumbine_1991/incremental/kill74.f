C*************************************************----------++++++++++!!
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

      COMMON /main/ u(0:xmax+1, 0:ymax), v(0:xmax+1, 0:ymax),
     1              s(0:xmax+1, 0:ymax), t(0:xmax+1, 0:ymax),
     2              q(0:xmax+1, 0:ymax)

      REAL u, v, s, t, q


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
      INTEGER time, temp
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

      CALL OUTSET
      CALL INIT
      temp = time()
      CALL UVSET
      PRINT *, 'Time spent in initializing uv =', time() - temp
      CALL QSET( deltat )
      CALL OUTDAT( 0 )

C     This is the actual extrapolation loop.
C     UVEXT extrapolates u and v to the next time step.
C     STEXT extrapolates s and t to the next time step.
C     QEXT  evaluates q for the next time step.
C     OUTDAT prints out w (upwelling velocity), rho1 (density of the
C            upper layer), and rho2 (density of the lower layer).

C*************************************************----------++++++++++!!
      DO 1000 tstep = 1, INT( tend/ deltat)

        temp = time()
        CALL UVEXT
C       IF (debug) WRITE (6,9001) tstep
        uvtim = time() - temp

        temp = time()
        CALL STEXT
C       IF (debug) WRITE (6,9002) tstep
        sttim = time() - temp

        temp = time()
        CALL QEXT( tstep, deltat)
C       IF (debug) WRITE (6,9003) tstep
        qtim = time() - temp

        temp = time()
        IF (MOD( tstep, outden) .EQ. 0) CALL OUTDAT(tstep)
        outtim = time() - temp

 1000 CONTINUE

      PRINT *,'Time spent in u-v extrapolation:', uvtim

      PRINT *,'Time spent in s-t extrapolation:', sttim

      PRINT *,'Time spent in q extrapolation:  ', qtim

      PRINT *,'Time spent on output:           ', outtim
C     OUTEND closes all the files and does any clean up that is needed.
      CALL OUTEND

 9001 FORMAT (' Finished uvext, tstep=',I6)

 9002 FORMAT (' Finished stext, tstep=',I6)

 9003 FORMAT (' Finished qext, tstep=',I6)

      END
