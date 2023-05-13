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
