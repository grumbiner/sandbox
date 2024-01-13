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
       DO 2000 J = 1, YMAX
         DO 2010 I = 1, XMAX

           T(I, J) = TFORCE(I, J)*DELTAT
           S(I, J) = SFORCE(I, J)*DELTAT
           IF ( S(I, J) .GT. 0.0) THEN
             PRINT *, 'CONVECTIVE OVERTURNING AT TIME STEP', TSTEP
             S(I, J) = 0.0
           ENDIF

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
