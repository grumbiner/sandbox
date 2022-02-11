      PROGRAM KILL74
C     THIS PROGRAM IS DESIGNED TO DUPLICATE THE PROGRAM USED IN
C       KILLWORTH'S 1974 PAPER, WITH THE CAPABILITY TO GENERALIZE
C       THE MODEL.

      INTEGER XMAX, YMAX
      PARAMETER ( XMAX = 20 )
      PARAMETER ( YMAX = 18 )

C*************************************************----------++++++++++!!

C     COMMON BLOCK /MAIN/ CONTAINS THE ARRAYS WHICH HOLD THE PHYSICAL
C       VARIABLES OF INTEREST. U IS THE EASTWARD VELOCITY,
C       V IS THE NORTHWARD VELOCITY, T = RHO1+ RHO2, S=RHO1 - RHO2,
C       WHERE RHO1 IS THE DENSITY OF THE UPPER LAYER, AND RHO2 IS THE
C       DENSITY OF THE LOWER LAYER.
C       Q IS RELATED TO THE FREEZING RATE OF ICE.

      REAL U(0:XMAX+1, 0:YMAX), V(0:XMAX+1, 0:YMAX),
     1     S(0:XMAX+1, 0:YMAX), T(0:XMAX+1, 0:YMAX),
     2     Q(0:XMAX+1, 0:YMAX)


C     COMMON /PARAMS/ HAS THE VARIOUS PARAMETERS THAT CONTROL THE
C       EXECUTION OF THE PROGRAM, AND THE VARIOUS PHYSICAL CONSTANTS.
C     DELTAT IS THE TIME STEP, DELTAX IS THE GRID SPACING (SAME IN
C       BOTH THE X AND Y DIRECTIONS), E IS AS DEFINED IN THE PAPER,
C       R IS A ROSSBY NUMBER (DEFINED IN THE PAPER), EPSILON IS
C       RELATED TO THE DIFFUSION OF SALINITY, SIGMA, XBOX IS THE
C       VALUE OF THE RIGHT-HAND SIDE OF THE WEDDELL SEA, YCOAST
C       IS THE Y COORDINATE OF THE COAST OUTSIDE THE BAY, TEND IS
C       THE END TIME OF THE EXTRAPOLATION IN YEARS.

      COMMON /PARAMS/ DELTAT, DELTAX, E, R, EPSILN, SIGMA,
     1                XBOX, YCOAST, TEND, DEBUG, OUTDEN

      REAL DELTAT, DELTAX, E, R, EPSILN, SIGMA, TEND
      INTEGER XBOX, YCOAST, OUTDEN
      LOGICAL DEBUG

C     LOCAL VARIABLES
      INTEGER TSTEP

CH    VARIABLES FOR TIMING INFORMATION.
CH    INTEGER CLOCK, TEMP
CH    INTEGER UVSTIM, UVTIM, STTIM, QTIM, OUTTIM

C*************************************************----------++++++++++!!
C     NOW BEGIN THE ACTUAL EXECUTION OF THE PROGRAM.
C     INIT INITIALIZES U, V, S, T, AND /PARAMS/.
C     OUTSET  INITIALIZES THE I/O.
C     UVSET INITIALIZES THE EXTRAPOLATION FOR U AND V.
C     QSET INITIALIZES THE EXTRAPOLATION OF Q, READS IN SOME PARAM-
C          ETERS USED LOCALLY TO THE ROUTINE.
C     CALL OUTDAT WRITES OUT THE INITIAL CONDITIONS.

CH    INITIALIZE THE TIMING VARIABLES.
CH    UVSTIM = 0
CH    UVTIM  = 0
CH    STTIM  = 0
CH    QTIM   = 0
CH    OUTTIM = 0

      TSTEP = 0

      CALL OUTSET(XMAX, YMAX, U, V, S, T, Q, TSTEP)
      CALL INIT(XMAX, YMAX, U, V, S, T)
CH    TEMP = CLOCK(0)
      CALL UVSET( U, V, S, T)
CH    PRINT *, 'TIME SPENT IN INITIALIZING UV =',(CLOCK(0)-TEMP)/1.E6
      CALL QSET   ( XMAX, YMAX, Q, TSTEP, DELTAT )
      CALL OUTDAT ( XMAX, YMAX, U, V, S, T, Q, 0 )

C     THIS IS THE ACTUAL EXTRAPOLATION LOOP.
C     UVEXT EXTRAPOLATES U AND V TO THE NEXT TIME STEP.
C     STEXT EXTRAPOLATES S AND T TO THE NEXT TIME STEP.
C     QEXT  EVALUATES Q FOR THE NEXT TIME STEP.
C     OUTDAT PRINTS OUT W (UPWELLING VELOCITY), RHO1 (DENSITY OF THE
C            UPPER LAYER), AND RHO2 (DENSITY OF THE LOWER LAYER).

C*************************************************----------++++++++++!!
      DO 1000 TSTEP = 1, INT( TEND/ DELTAT)

CH      TEMP = CLOCK(0)
        CALL UVEXT( U, V, S, T)
C       IF (DEBUG) WRITE (*,9001) TSTEP
CH      UVTIM = UVTIM + CLOCK(0) - TEMP
CH      TEMP = CLOCK(0)
        CALL STEXT(XMAX, YMAX, U, V, S, T, Q, TSTEP)
C       IF (DEBUG) WRITE (*,9002) TSTEP
CH      STTIM = STTIM + CLOCK (0) - TEMP

CH      TEMP = CLOCK(0)
        CALL QEXT(XMAX, YMAX, Q, TSTEP, DELTAT)
C       IF (DEBUG) WRITE (*,9003) TSTEP
CH      QTIM = QTIM + CLOCK (0) - TEMP

CH      TEMP = CLOCK(0)
        IF (MOD( TSTEP, OUTDEN) .EQ. 0)
     1      CALL OUTDAT(XMAX, YMAX, U, V, S, T, Q, TSTEP)
CH      OUTTIM = OUTTIM + CLOCK (0) - TEMP

 1000 CONTINUE

CH    PRINT *,'TIME SPENT IN U-V EXTRAPOLATION:', UVTIM/1.E6

CH    PRINT *,'TIME SPENT IN S-T EXTRAPOLATION:', STTIM/1.E6

CH    PRINT *,'TIME SPENT IN Q EXTRAPOLATION:  ', QTIM/1.E6

CH    PRINT *,'TIME SPENT ON OUTPUT:           ', OUTTIM/1.E6
C     OUTEND CLOSES ALL THE FILES AND DOES ANY CLEAN UP THAT IS NEEDED.
      CALL OUTEND(XMAX, YMAX, U, V, S, T, Q, TSTEP)

CH 9001 FORMAT (' FINISHED UVEXT, TSTEP=',I6)

CH 9002 FORMAT (' FINISHED STEXT, TSTEP=',I6)

CH 9003 FORMAT (' FINISHED QEXT, TSTEP=',I6)

      END
C*************************************************----------++++++++++!!
      SUBROUTINE INIT(XMAX, YMAX, U, V, S, T)

      INTEGER XMAX, YMAX

      REAL
     1      U(0:XMAX+1, 0:YMAX), V(0:XMAX+1, 0:YMAX),
     2      S(0:XMAX+1, 0:YMAX), T(0:XMAX+1, 0:YMAX)


      COMMON /PARAMS/ DELTAT, DELTAX, E, R, EPSILN, SIGMA,
     1                XBOX, YBOX, TEND, DEBUG, OUTDEN

      REAL DELTAT, DELTAX, E, R, EPSILN, SIGMA, TEND
      INTEGER XBOX, YBOX, OUTDEN
      LOGICAL DEBUG, RESTRT


C     LOCAL VARIABLES
      INTEGER I, J
      REAL SINIT
C     PARAMS FOR NONZERO INITIALIZATION.
      INTEGER N, M
      REAL PI

C     READ IN PARAMETERS.
      READ (4,9001) DELTAT
      READ (4,9001) DELTAX
      READ (4,9001) E
      READ (4,9001) R
      READ (4,9001) EPSILN
      READ (4,9001) SIGMA
      READ (4,9002) XBOX
      READ (4,9002) YBOX
      READ (4,9001) TEND
      READ (4,9001) SINIT
      READ (4,9003) DEBUG
      READ (4,9002) OUTDEN
      READ (4,9003) RESTRT

      PRINT *,'VARIABLES FOR INIT'
      WRITE (6,9001) DELTAT
      WRITE (6,9001) DELTAX
      WRITE (6,9001) E
      WRITE (6,9001) R
      WRITE (6,9001) EPSILN
      WRITE (6,9001) SIGMA
      WRITE (6,9002) XBOX
      WRITE (6,9002) YBOX
      WRITE (6,9001) TEND
      WRITE (6,9001) SINIT
      WRITE (6,9003) DEBUG
      WRITE (6,9002) OUTDEN
      WRITE (6,9003) RESTRT

      IF (.NOT. RESTRT) THEN
      DO 1000 J = 0, YMAX
        DO 1010 I = 0, XMAX + 1

          IF (I .GT. XBOX .AND. J .LT. YBOX) THEN
            S(I, J) = 0.0
           ELSE
C           NOT ON CONTINENT
            S(I, J) = SINIT
          ENDIF

          T(I, J) = 0.0
          U(I, J) = 0.0
          V(I, J) = 0.0

 1010   CONTINUE
 1000 CONTINUE

       ELSE
        READ (13) U
        READ (14) V
        READ (17) S
        READ (18) T
      ENDIF

C     THIS IS THE PLACE TO INITIALIZE SELECTED POINTS TO NON-ZERO VALUES
C     PI = 3.14159236
C     N = 6
C     M = 0
C     DO 9998 J = 1, YMAX
C       DO 9999 I = 0, XMAX+1
C         U(I, J) = 1.0*SIN(N*PI*I/XBOX)*(EXP(N*PI*1.4*J/YMAX)-
C    1                 EXP(-N*PI*1.4*J/YMAX))/ (EXP(N*PI*1.4)-
C    2                 EXP(-N*PI*1.4) )
C         V(I, J) = 0.0
C9999   CONTINUE
C9998 CONTINUE


 9001 FORMAT (F8.4)

 9002 FORMAT (I5)

 9003 FORMAT (L10)

      RETURN
      END
C*************************************************----------++++++++++!!
      SUBROUTINE OUTDAT(XMAX, YMAX, U, V, S, T, Q, TSTEP)

C     THIS SUBROUTINE IS SUPPOSED TO COMPUTE THE UPWELLING VELOCITY,
C       DENSITY OF THE SURFACE LAYER, DENSITY OF THE LOWER LAYER,
C       AND FLUX THROUGH THE MOUTH OF THE BAY.

      INTEGER XMAX, YMAX

      REAL  U(0:XMAX+1, 0:YMAX), V(0:XMAX+1, 0:YMAX),
     1      S(0:XMAX+1, 0:YMAX), T(0:XMAX+1, 0:YMAX),
     2      Q(0:XMAX+1, 0:YMAX)


      COMMON /PARAMS/ DELTAT, DELTAX, E, R, EPSILN, SIGMA,
     1                XBOX, YBOX, TEND, DEBUG, OUTDEN

      REAL DELTAT, DELTAX, E, R, EPSILN, SIGMA, TEND
      INTEGER XBOX, YBOX, OUTDEN
      LOGICAL DEBUG

C     LOCAL VARIABLES:
C       W IS THE UPWELLING VELOCITY
C       RHO1 IS THE DENSITY OF THE UPPER LAYER.
C       RHO2 IS THE DENSITY OF THE LOWER LAYER.
C       FLUX IS THE FLUX OF WATER ACROSS THE CONTINENTAL SHELF BREAK.
C     LOCAL COMPUTATION OF W, RHO1, RHO2 REMOVED 7-23-86. BG.
C     REAL W(0:XMAX+1, 0:YMAX)
C     REAL RHO1(0:XMAX+1, 0:YMAX), RHO2(0:XMAX+1, 0:YMAX)
      REAL FLUX

      INTEGER TSTEP
      INTEGER I, YSHELF

      SAVE /PARAMS/

C     FIND W, RHO1, RHO2 IN THE INTERIOR POINTS.
C     DO 1000 J = 1, YMAX-1
C       DO 1010 I = 1, XMAX

C         W(I, J)     = (U(I+1,J)-U(I-1,J)+V(I,J+1)-V(I,J-1))/
C    1                   (2.*DELTAX)
C         RHO1 (I, J) =  T(I, J) + S(I, J)
C         RHO2 (I, J) =  T(I, J) - S(I, J)

C1010   CONTINUE
C1000 CONTINUE

C     FIND W, RHO1, RHO2 ALONG Y=YMAX.
C     DO 1020 I = 1, XMAX
C       W(I, YMAX) = (U(I+1, YMAX) - U(I-1, YMAX)) / (2.*DELTAX)
C       RHO1(I, YMAX) = T(I, YMAX) + S(I, YMAX)
C       RHO2(I, YMAX) = T(I, YMAX) - S(I, YMAX)
C1020 CONTINUE

C     FIND W, RHO1, RHO2 ALONG X=0, XMAX+1.
C     DO 1030 J = 1, YMAX-1
C       W(0, J)    =  (U(1, J)-U(XMAX-1, J) + V(0, J+1) - V(0, J-1))/
C    1                 (2.*DELTAX)
C       W(XMAX+1,J) = (U(2,J)-U(XMAX,J) + V(XMAX+1,J+1)-V(XMAX+1,J-1))/
C    1                 (2.*DELTAX)
C       RHO1(0, J)      = T(0, J) + S(0, J)
C       RHO2(0, J)      = T(0, J) - S(0, J)
C       RHO1(XMAX+1, J) = T(XMAX, J) + S(XMAX, J)
C       RHO2(XMAX+1, J) = T(XMAX+1, J) - T(XMAX+1, J)
C1030 CONTINUE

C     FINISH THE TWO POINTS IN THE CORNERS Y=YMAX, X= 0, XMAX+1.
C     W(0   , YMAX) = (U(1, YMAX)- U(XMAX-1, YMAX))/ (2.*DELTAX)
C     W(XMAX+1, YMAX) = (U(2, YMAX)-U(XMAX, YMAX))/ (2.*DELTAX)
C     RHO1(0, YMAX)   = T(0, YMAX) + S(0, YMAX)
C     RHO2(0, YMAX)   = T(0, YMAX) - S(0, YMAX)
C     RHO1(XMAX+1, YMAX) = T(XMAX+1, YMAX) + S(XMAX+1, YMAX)
C     RHO2(XMAX+1, YMAX) = T(XMAX+1, YMAX) - S(XMAX+1, YMAX)

C     FIND THE FLUX
      YSHELF = INT (5.0*YMAX/14.0 + .5)
      FLUX   = 0.0
      DO 1100 I = 1, XBOX
        FLUX = FLUX + V(I, YSHELF)
 1100 CONTINUE

C     PUT THE TIME STEP IN FRONT OF THE DATA.
C     USE UNFORMATTED OUTPUT FOR EVERYTHING OTHER THAN THE FLUX.  THIS I
C       FASTER THAN FORMATTED I/O.
C     WRITE (10) TSTEP
C     WRITE (11) TSTEP
C     WRITE (12) TSTEP
      WRITE (13) TSTEP
      WRITE (14) TSTEP
      WRITE (15) TSTEP
      WRITE (16,9001) TSTEP
      WRITE (17) TSTEP
      WRITE (18) TSTEP

C     SET U(4*XMAX/5, YMAX/3) TO 0.4 TO GIVE A KNOWN LENGTH VECTOR FOR T
C       STREAMLINE VELOCITY FIELDS.
      U(4*XMAX/5, YMAX/3) = 0.4

C     NOW WRITE OUT THE RESULTS.
C       OUTPUT USING ONLY THE NAME OF THE MATRIX.  THIS IS FASTER THAN
C         USING ELEMENT-BY-ELEMENT REFERENCE
C       WRITE (10) W
C       WRITE (11) RHO1
C       WRITE (12) RHO2
        WRITE (13) U
        WRITE (14) V
        WRITE (15) Q
        WRITE (17) S
        WRITE (18) T

      WRITE (16,9003) FLUX

      RETURN

      ENTRY OUTSET(XMAX, YMAX, U, V, S, T, Q, TSTEP)
C     THIS ENTRY IS TO OPEN ALL FILES THAT NEED TO BE OPENED, AND
C       PUT HEADER INFORMATION AT THE BEGINNING OF EACH DATA FILE.
      OPEN (4,  FILE = 'INPUT' , STATUS = 'OLD')
C     OPEN (10, FILE = 'W'   , FORM = 'UNFORMATTED', STATUS = 'NEW')
C     OPEN (11, FILE = 'RHO1', FORM = 'UNFORMATTED', STATUS = 'NEW')
C     OPEN (12, FILE = 'RHO2', FORM = 'UNFORMATTED', STATUS = 'NEW')
      OPEN (13, FILE = 'U'   , FORM = 'UNFORMATTED', STATUS='UNKNOWN')
      OPEN (14, FILE = 'V'   , FORM = 'UNFORMATTED', STATUS='UNKNOWN')
      OPEN (15, FILE = 'Q'   , FORM = 'UNFORMATTED', STATUS='UNKNOWN')
      OPEN (16, FILE = 'FLUX', STATUS='UNKNOWN')
      OPEN (17, FILE = 'S'   , FORM = 'UNFORMATTED', STATUS='UNKNOWN')
      OPEN (18, FILE = 'T'   , FORM = 'UNFORMATTED', STATUS='UNKNOWN')

C     INITIALIZE THE DATA MATRICES.
C     ONLY INITIALIZE J=0.  THE REST WILL BE COMPUTED.
C     DO 3010 I = 0, XMAX+1

C       W   (I, 0) = 0.0
C       RHO1(I, 0) = 0.0
C       RHO2(I, 0) = 0.0

C3010 CONTINUE

      RETURN

      ENTRY OUTEND(XMAX, YMAX, U, V, S, T, Q, TSTEP)
C     THIS ENTRY IS TO CLOSE ALL FILES AND DO ANY NEEDED CLEANUP.

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
C*************************************************----------++++++++++!!
      SUBROUTINE UVEXT( U, V, S, T )
C     THIS SUBROUTINE EXTRAPOLATES U AND V TO THE NEXT TIME STEP
C       USING FULLY IMPLICIT DIFFERENCING ON U, V, AND FULLY EXPLICIT
C       DIFFERENCING ON S, T.

      INTEGER XMAX, YMAX
      PARAMETER (XMAX = 20)
      PARAMETER (YMAX = 18)

      REAL  U(0:XMAX+1, 0:YMAX), V(0:XMAX+1, 0:YMAX),
     1      S(0:XMAX+1, 0:YMAX), T(0:XMAX+1, 0:YMAX)


      COMMON /PARAMS/ DELTAT, DELTAX, E, R, EPSILN, SIGMA,
     1                XBOX, YCOAST, TEND, DEBUG, OUTDEN

      REAL DELTAT, DELTAX, E, R, EPSILN, SIGMA, TEND
      INTEGER XBOX, YCOAST, OUTDEN
      LOGICAL DEBUG


C     MATRICES TO COMPUTE U AND V AT THE NEW TIME STEP.
      REAL UVABD(6*XMAX+1, 2*XMAX*YMAX),
     1                FORCE(2*XMAX*YMAX)

C     VARIABLES FOR THE LINPACK ROUTINES.
      INTEGER IPVT(2*XMAX*YMAX), INFO


C     LOCAL VARIABLES
      REAL RDT, EDXDX, HALFDX
      INTEGER I, J, ROW

      SAVE UVABD, IPVT, INFO, RDT, EDXDX, HALFDX, /PARAMS/

CEMA  UVABD

C     BEGIN THE EXTRAPOLATION.
      ROW  = -1

      DO 1000 J = 1, YMAX
        DO 1010 I = 1, XMAX

          ROW = ROW + 2

          IF ( (J .EQ. 1  .AND. I .LE. XBOX) .OR. (J .EQ. YCOAST
     1           .AND. I .GE. XBOX) )  THEN
C         APPLY NO SLIP CONDITION AT HORIZONTAL BNDY.
          FORCE(ROW)   = 0.0
          FORCE(ROW+1) = 0.0

         ELSE IF (J .LT. YCOAST .AND. (I .EQ. 1 .OR. I .EQ. XBOX))
     1    THEN
C         APPLY NO SLIP AT A VERTICAL BOUNDARY.
          FORCE(ROW)   = 0.0
          FORCE(ROW+1) = 0.0

         ELSE IF (J .EQ. YMAX) THEN
C         APPLY NO FLUX IN Y DIRECTION HERE.
          FORCE(ROW)   = RDT*U(I,J)+ (T(I+1,J)-T(I-1,J))*HALFDX
          FORCE(ROW+1) = RDT * V(I, J)
          IF (I .EQ. XMAX) THEN
C           EXPERIMENTALLY REMOVE THE VELOCITY AT THE PREVIOUS STEP.
C           THE IDEA HERE IS THAT I DON'T THINK I CAN COMPUTE THE FULL
C           LAPLACIAN AT XMAX, YMAX.  IN DOING THIS I DO ALLOW THERE TO
C           BE A VELOCITY AT THE CORNER OF THE DOMAIN, BUT IT HAS NO MEM
C           FOR PREVIOUS VALUES.
            FORCE(ROW  ) = (T(I+1,J)-T(I-1,J))*HALFDX
            FORCE(ROW+1) = 0.0
          ENDIF
C*************************************************----------++++++++++!!

         ELSE IF (J .LT. YCOAST .AND. I .GT. XBOX) THEN
C         YOU ARE ON THE CONTINENT.
          FORCE(ROW)   = 0.0
          FORCE(ROW+1) = 0.0

         ELSE
C         MAIN BODY OF THE SEA.
          FORCE(ROW)   = RDT* U(I,J) +(T(I+1,J)-T(I-1,J))*HALFDX
          FORCE(ROW+1) = RDT* V(I,J) +(T(I,J+1)-T(I,J-1))*HALFDX

        ENDIF

 1010   CONTINUE
 1000 CONTINUE

C     NOW CALL THE ROUTINE TO DO THE BACK SUBSTITUTION.
C     SGBSL IS THE LINPACK SINGLE PRECISION ROUTINE FOR BACK SUBSTITUTIO
C       IN BANDED MATRICES.
CD    IF (DEBUG) WRITE (6,9002)
      CALL SGBSL (UVABD, 6*XMAX+1, 2*XMAX*YMAX, 2*XMAX, 2*XMAX,
     1              IPVT, FORCE, 0)

C     FILL U AND V WITH THE APPROPRIATE VALUES.
      ROW = -1
      DO 2000 J = 1, YMAX
        DO 2010 I = 1, XMAX

          ROW = ROW + 2
          U(I,J) = FORCE(ROW)
          V(I,J) = FORCE(ROW+1)

 2010   CONTINUE
 2000 CONTINUE

C     APPLY THE PERIODICITY CONDITION.
      DO 3000 J = 0, YMAX
        IF (J .GE. YCOAST) THEN
          U(0,     J) = U(XMAX, J)
          U(XMAX+1,J) = U(1,    J)
          V(0,     J) = V(XMAX, J)
          V(XMAX+1,J) = V(1,    J)
         ELSE
          U(0,     J) = 0.0
          U(XMAX+1,J) = 0.0
          V(0,     J) = 0.0
          V(XMAX+1,J) = 0.0
        ENDIF
 3000 CONTINUE

CD 9002 FORMAT (' CALLING BACK SUBSTITUTION')

      RETURN

      ENTRY UVSET(U, V, S, T)
C     ENTRY TO INITIALIZE SOME CONSTANTS, AND GET THE LU DECOMPOSITION
C       OF THE UV MATRIX.  SAVE THE FACTORIZATION TO SPEED THE BACK
C       SUBSTITUTION.

C     FIRST, FILL UVABD WITH 0'S.
      DO 5000 J = 1, 2*XMAX*YMAX
        DO 5010 I = 1, 6*XMAX+1
          UVABD(I,J) = 0.0
 5010   CONTINUE
 5000 CONTINUE

C     COMPUTE SOME COMMON CONSTANTS.
      RDT    = R/DELTAT
      EDXDX  = E/DELTAX**2
      HALFDX = 1./(4.*DELTAX)

C     NOW FILL THE COEFFICIENTS MATRIX
      ROW = -1
      DO 6000 J = 1, YMAX
        DO 6010 I = 1, XMAX
        ROW = ROW + 2
        IF ( J .EQ. 1 .AND. I .LE. XBOX .OR. (J .EQ. YCOAST .AND.
     1         I .GE. XBOX) ) THEN
C         APPLY FLUX AT AT A HORIZONTAL BOUNDARY.
C           THE CONSTANT IS ARBITRARY (SINCE THE FORCING IS 0), SO
C           CHOOSE 1.
          UVABD(4*XMAX+1, ROW  ) = 1.0
          UVABD(4*XMAX+1, ROW+1) = 1.0

         ELSE IF (J .LT. YCOAST .AND. (I .EQ. 1 .OR. I .EQ. XBOX))
     1    THEN
C         APPLY NO SLIP AT A VERTICAL BOUNDARY.
C           CONSTANT IS ARBITRARY, SO USE 1.
          UVABD(4*XMAX+1, ROW  ) = 1.0
          UVABD(4*XMAX+1, ROW+1) = 1.0

         ELSE IF (J .EQ. YMAX) THEN
          IF (I .NE. XMAX) THEN
C           NO FLUX IN THE Y DIRECTION.
C           EQUATIONS FOR U
            UVABD(4*XMAX+1, ROW  ) = RDT + 3.*EDXDX
            UVABD(4*XMAX  , ROW+1) = 1.0
            UVABD(4*XMAX-1, ROW+2) = -EDXDX
            UVABD(4*XMAX+3, ROW-2) = -EDXDX
            UVABD(6*XMAX+1, ROW-2*XMAX) = -EDXDX
C           NOW THE EQUATIONS FOR V.
            UVABD(4*XMAX+1, ROW+1) = RDT + 3.*EDXDX
            UVABD(4*XMAX+2, ROW  ) = -1.0
            UVABD(4*XMAX-1, ROW+3) = -EDXDX
            UVABD(4*XMAX+3, ROW-1) = -EDXDX
            UVABD(6*XMAX+1, ROW+1-2*XMAX) = -EDXDX
           ELSE
C           USE SECOND DERIV IN X = 0 TO KEEP FROM OVERFLOWING THE ARRAY
C             THIS ONLY OCCURS AT ONE POINT, SO THERE SHOULDN'T BE MUCH
C             FROM THIS POINT.
            UVABD(4*XMAX+1, ROW  ) = RDT + 3.*EDXDX
            UVABD(4*XMAX  , ROW+1) = 1.0
            UVABD(4*XMAX+3, ROW-2) = -EDXDX
            UVABD(6*XMAX+1, ROW-2*XMAX) = -EDXDX
            UVABD(4*XMAX+1, ROW+1) = RDT + 3.*EDXDX
            UVABD(4*XMAX+2, ROW  ) = 1.0
            UVABD(4*XMAX+3, ROW-1) = -EDXDX
            UVABD(6*XMAX+1, ROW+1-2*XMAX) = -EDXDX
          ENDIF
         ELSE IF (J .LT. YCOAST .AND. I .GT. XBOX) THEN
C         ON THE CONTINENT.  COEFFICIENTS ARE ARBITRARY, SINCE THERE
C           IS NO FORCING, SO USE 1.
          UVABD(4*XMAX+1, ROW  ) = 1.0
          UVABD(4*XMAX+1, ROW+1) = 1.0

         ELSE
C         NOT AT A BOUNDARY
C         EQUATIONS FOR U.
          UVABD(4*XMAX+1, ROW  ) = RDT + 4.*EDXDX
          UVABD(4*XMAX  , ROW+1) = 1.0
          UVABD(4*XMAX-1, ROW+2) = -EDXDX
          UVABD(4*XMAX+3, ROW-2) = -EDXDX
          UVABD(2*XMAX+1, ROW+2*XMAX) = -EDXDX
          UVABD(6*XMAX+1, ROW-2*XMAX) = -EDXDX
C         EQUATIONS FOR V.
          UVABD(4*XMAX+1, ROW+1) = RDT + 4.*EDXDX
          UVABD(4*XMAX+2, ROW  ) = -1.0
          UVABD(4*XMAX-1, ROW+3) = -EDXDX
          UVABD(4*XMAX+3, ROW-1) = -EDXDX
          UVABD(2*XMAX+1, ROW+1+2*XMAX) = -EDXDX
          UVABD(6*XMAX+1, ROW+1-2*XMAX) = -EDXDX

        ENDIF

 6010   CONTINUE
 6000 CONTINUE

C     DO THE FACTORIZATION.
C     SGBFA IS THE LINPACK ROUTINE FOR FACTORING A BANDED MATRIX,
C       SINGLE PRECISION.
      CALL SGBFA( UVABD, 6*XMAX+1, 2*XMAX*YMAX, 2*XMAX, 2*XMAX,
     1            IPVT, INFO)
      IF (INFO .NE. 0) STOP 'UNUSABLE DECOMPOSTION'

      RETURN
      END
C*************************************************----------++++++++++!!
      SUBROUTINE STEXT(XMAX, YMAX, U, V, S, T, Q, TSTEP)
C     SUBROUTINE TO EXTRAPOLATE S (RHO1 - RHO2) AND T (RHO1+RHO2),
C       WHERE RHO1 IS THE DENSITY OF THE UPPER LAYER AND RHO2 IS
C       THE DENSITY OF THE LOWER LAYER.

      INTEGER XMAX, YMAX

      REAL U(0:XMAX+1, 0:YMAX), V(0:XMAX+1, 0:YMAX),
     1     S(0:XMAX+1, 0:YMAX), T(0:XMAX+1, 0:YMAX),
     2     Q(0:XMAX+1, 0:YMAX)


      COMMON /PARAMS/ DELTAT, DELTAX, E, R, EPSILN, SIGMA,
     1                XBOX, YCOAST, TEND, DEBUG, OUTDEN

      REAL DELTAT, DELTAX, E, R, EPSILN, SIGMA, TEND
      INTEGER XBOX, YCOAST, OUTDEN, TSTEP
      LOGICAL DEBUG


C     LOCAL VARIABLES
      INTEGER I, J
C     WARNING! THE DIMENSION OF THESE ARRAYS IS HARD-CODED.
C     IT SHOULD BE LARGE ENOUGH FOR NOW (XMAX=45, YMAX=42), BUT
C       THIS MUST BE CHANGED IF THE DOMAIN IS ENLARGED.
      REAL SFORCE(1:20, 0:18), TFORCE(1:20, 0:18)

      SAVE

C     FILL THE FORCING MATRICES
C*************************************************----------++++++++++!!
      DO 1000 J = 1, YMAX
        DO 1010 I = 1, XMAX

          IF ( (J .EQ. 1 .AND. I .LE. XBOX) .OR. (J .EQ. YCOAST
     1           .AND. I .GE. XBOX ) ) THEN
C           APPLY NO SALINITY FLUX AT A HORIZONTAL BOUNDARY.
            TFORCE(I, J) = T(I, J+1)/DELTAT
            SFORCE(I, J) = S(I, J+1)/DELTAT

           ELSE IF(J .LT. YCOAST .AND. (I .EQ. 1 .OR. I .EQ. XBOX))
     1      THEN
C           APPLY NO SALINITY FLUX AT THE VERT. BNDYS.
             IF (I .EQ. 1) THEN
C             BOUNDARY TO LEFT OF POINT.
              TFORCE(I, J) = T(I+1, J)/DELTAT
              SFORCE(I, J) = S(I+1, J)/DELTAT
             ELSE
C             BOUNDARY TO RIGHT OF POINT.
              TFORCE(I, J) = T(I-1, J)/DELTAT
              SFORCE(I, J) = S(I-1, J)/DELTAT
             ENDIF

           ELSE IF(J .EQ. YMAX) THEN
C           NO FLUX THROUGH TOP OF REGION.
            TFORCE(I, J) = T(I, J-1)/DELTAT
            SFORCE(I, J) = S(I, J-1)/DELTAT

           ELSE IF(J .LT. YCOAST .AND. I .GT. XBOX) THEN
C           ON THE CONTINENT
            TFORCE(I, J) = 0.0
            SFORCE(I, J) = 0.0

           ELSE
C           IN THE MAIN PART OF THE SEA.
            TFORCE(I, J) = T(I, J)/DELTAT
C*************************************************----------++++++++++!!
     1        -(U(I+1, J)*S(I+1, J)-U(I-1, J)*S(I-1, J))/(DELTAX*2.)
     2        -(V(I, J+1)*S(I, J+1)-V(I, J-1)*S(I, J-1))/(DELTAX*2.)
     3        +EPSILN*( T(I+1, J)+T(I-1, J)+T(I, J+1)+T(I, J-1)
     3                   -4.*T(I,J) ) / DELTAX**2
     4        +Q(I,J)


            SFORCE (I, J) = S(I, J)/DELTAT
     1        -U(I, J)*(T(I+1 ,J)-T(I-1, J))/(DELTAX*2.)
     2        -V(I, J)*(T(I, J+1)-T(I, J-1))/(DELTAX*2.)
     3        +EPSILN*( S(I+1,J)+S(I-1,J)+S(I,J+1)+S(I,J-1)
     3                    -4.*S(I, J) )/DELTAX**2
     4        +Q(I,J)
     5        -SIGMA*S(I, J)

          ENDIF

 1010   CONTINUE
 1000 CONTINUE

C      NOW EVALUATE T, S AT TIME N+1
C      PERFORM CONVECTIVE ADJUSTMENT AS PER K74 PAPER.  7-23-87 BG
       NCONV = 0
       DO 2000 J = 1, YMAX
         DO 2010 I = 1, XMAX

           T(I, J) = TFORCE(I, J)*DELTAT
           S(I, J) = SFORCE(I, J)*DELTAT
           IF ( S(I, J) .GT. 0.0) THEN
C            PRINT *, 'CONVECTIVE OVERTURNING AT TIME STEP', TSTEP
             NCONV = NCONV + 1
             S(I, J) = 0.0
           ENDIF
           IF (NCONV .NE. 0) PRINT *, NCONV,'CELLS OVERTURNED AT TSTEP',
     1       TSTEP

 2010   CONTINUE
 2000 CONTINUE

C     APPLY THE PERIODICITY CONDITION.
      DO 3000 J = YCOAST, YMAX

        T(0,      J) = T(XMAX, J)
        T(XMAX+1, J) = T(1,    J)
        S(0,      J) = S(XMAX, J)
        S(XMAX+1, J) = S(1,    J)

 3000 CONTINUE

      RETURN
      END
C*************************************************----------++++++++++!!
      SUBROUTINE QEXT( XMAX, YMAX, Q, TSTEP, DELTAT )

C     THE PURPOSE OF THIS ROUTINE IS TO DETERMINE THE RATE OF ICE
C       FORMATION OVER THE DOMAIN OF THE MODEL.  THIS IS THEN USED
C       AS AN INPUT OF SALINITY TO THE WATER, DUE TO FRACTIONATION
C       IN THE FREEZING PROCESS.

      INTEGER XMAX, YMAX, TSTEP
      REAL DELTAT
      REAL Q(0:XMAX+1, 0:YMAX)


C     LOCAL VARIABLES
      INTEGER LOY, ENDWIN, SUMST, SUMEND
      INTEGER TOFYR, I, J, YSHELF
      REAL QMAX

      SAVE

C     BEGIN THE CALCULATION.
      TOFYR = MOD( TSTEP, LOY)

      IF (TOFYR .LE. ENDWIN) THEN

C       IT IS THE WINTER TIME.
        DO 1000 J = 0, YMAX
          DO 1010 I = 0, XMAX + 1
            Q(I, J) = QMAX*(-FLOAT(J)/FLOAT(YSHELF) + 1.0 )**2
            IF ( J .GT. YSHELF ) Q(I,J) = 0.0
 1010     CONTINUE
 1000   CONTINUE

       ELSE
C       IT IS NOT WINTER
        DO 1100 J = 0, YMAX
          DO 1110 I = 0, XMAX + 1
            Q(I, J) = 0.0
 1110     CONTINUE
 1100   CONTINUE

      ENDIF

      RETURN

      ENTRY QSET( XMAX, YMAX, Q, TSTEP, DELTAT )
C     ENTRY PT. FOR INITIALIZATION OF ROUTINE.

      READ (4,9001) ENDWIN
      READ (4,9001) SUMST
      READ (4,9001) SUMEND
      READ (4,9001) YSHELF
      READ (4,9002) QMAX

      PRINT *,'VARIABLES FOR QEXT'
      WRITE (*,9001) ENDWIN
      WRITE (*,9001) SUMST
      WRITE (*,9001) SUMEND
      WRITE (*,9001) YSHELF
      WRITE (*,9002) QMAX

      LOY = INT( 1./ DELTAT )

 9001 FORMAT (I5)

 9002 FORMAT (F10.5)

      RETURN
      END