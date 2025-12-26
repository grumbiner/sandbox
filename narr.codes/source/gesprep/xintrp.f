      FUNCTION xintrp(Y,X,XBAR,N)
      IMPLICIT REAL (A-H,O-Z)
      INCLUDE "parmanl"
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    XINTRP      VERTICAL INTERPOLATION FUNCTION (AIKEN)
C   PRGMMR: DEAVEN           ORG: W/NMC22    DATE: 85-01-31
C
C ABSTRACT: PROGRAM TO PERFORM VERTICAL INTERPOLATION FROM A PROFILE
C   OF VALUES (Y) AT LOG PRESSURES (X) TO A PARTICULAR POINT (XBAR)
C   USING AN ITERATIVE AIKEN SCHEME WHICH IS VARIABLE ORDER IN LN P.
C
C PROGRAM HISTORY LOG:
C   85-01-31  DEAVEN
C   88-07-28  DIMEGO      DOCBLOCK & HALF PRECISION
C   89-03-28  DEAVEN      MODIFIED TO BE AT MOST QUADRATIC
C
C USAGE:    WXYZ = XINTRP(Y,X,XBAR,N)
C   INPUT ARGUMENT LIST:
C     Y        - ARRAY CONTAINING SOURCE VALUES
C     X        - ARRAY OF LEVELS AT WHICH WE HAVE Y
C     XBAR     - LEVEL VALUE AT WHICH WE WANT A VALUE
C     N        - NUMBER OF LEVELS IN ARRAYS X AND Y
C
C   OUTPUT ARGUMENT LIST:
C     XINTRP   - FUNCTION VALUE IS INTERPOLATED VALUE OF Y AT XBAR
C
C REMARKS: SENSITIVE TO UNEQUAL SPACING IN X
C
C ATTRIBUTES:
C   LANGUAGE: STANDARD FORTRAN
C   MACHINE:
C
C$$$
        DIMENSION Y(1),X(1),R(50),P(50,50)
        DO 333 I = 1,N
          R(I) = X(I) - XBAR
          P(I,1) = Y(I)
333     CONTINUE
        RMIN = 9. E+10
        DO 10 I = 1,N
          IF(ABS(R(I)).GT.RMIN)GO TO 10
            RMIN = ABS(R(I))
            IS = I
10      CONTINUE
        IF(RMIN.EQ.0.0)GO TO 99
        IF(IS.LT.2) IS = 2
        IF(IS.GE.N) IS = N-1
        JS = IS
        JE = IS +1
c       IF(JE.GT.N) JE = N
        IF(JS.LT.3) GO TO 55
        DO 334 I = 1,N
          P(I,JS-1) = Y(I)
334     CONTINUE
55      CONTINUE
c       DO 1 J = JS,JE
c       DO 1 I = J,JE
c1      P(I,J) = (P(J-1,J-1)*R(I) - R(J-1)*P(I,J-1))/(X(I)-X(J-1))
c   J=JS and I=JS
        J=JS
        I=JS
        P(I,J) = (P(J-1,J-1)*R(I) - R(J-1)*P(I,J-1))/(X(I)-X(J-1))
c   J=JS and I=JE
        J=JS
        I=JE
        P(I,J) = (P(J-1,J-1)*R(I) - R(J-1)*P(I,J-1))/(X(I)-X(J-1))
c   J=JE and I=JE
        J=JE
        I=JE
        P(I,J) = (P(J-1,J-1)*R(I) - R(J-1)*P(I,J-1))/(X(I)-X(J-1))
        XINTRP = P(JE,JE)
        RETURN
99      XINTRP = Y(IS)
        RETURN
        END
