       SUBROUTINE ICSEVU(KRAT, Y, NX, C, IC, S, M, IER) 
C    IMSL ROUTINE NAME   - ICSEVU 
C    COMPUTER            - CDC/SINGLE 
C    LATEST REVISION     - JANUARY 1, 1978
C    PURPOSE             - EVALUATION OF A CUBIC SPLINE 
C    USAGE               - CALL ICSEVU(X,Y,NX,C,IC,U,S,M,IER) 
C      X,U ARRAYS REPLACED BY STATEMENT 
C      FUNCTIONS ALBERT LUNDE 8/29/79 
C      X(I)=(I-1)*KRAT
C      U(J)=J-1 
C      REMOVE CALLS TO IMSL ERROR MESSAGE SUBS
C                          WARNING ERROR
C                            IER = 33, U(I) IS LESS THAN X(1).
C                            IER = 34, U(I) IS GREATER THAN X(NX).
C    COPYRIGHT           - 1978 BY IMSL, INC. ALL RIGHTS RESERVED.
C                                   SPECIFICATIONS FOR ARGUMENTS
       INTEGER KRAT,NX,IC,M,IER 
       REAL Y(NX),C(IC,3),S(M)
C                                   SPECIFICATIONS FOR LOCAL VARIABLES
       INTEGER            I, JER, KER, NXM1, K
       REAL               D, DD, ZERO 
       DATA               I/1/, ZERO/0.0/ 
       X(I) = FLOAT((I-1)*KRAT) 
       U(J) = FLOAT(J-1)
C                                   FIRST EXECUTABLE STATEMENT
       JER = 0
       KER = 0
       IF (M .LE. 0) GO TO 9005 
       NXM1 = NX-1
       IF (I .GT. NXM1) I = 1 
C                                   EVALUATE SPLINE AT M POINTS 
       DO 40 K = 1, M 
C                                   FIND THE PROPER INTERVAL
          D = U(K)-X(I) 
          IF (D) 5, 25, 15
    5     IF (I .EQ. 1) GO TO 30
          I = I-1 
          D = U(K)-X(I) 
          IF (D) 5, 25, 20
   10     I = I+1 
          D = DD
   15     IF (I .GE. NX) GO TO 35 
          DD = U(K)-X(I+1)
          IF (DD .GE. ZERO) GO TO 10
          IF (D .EQ. ZERO) GO TO 25 
C                                   PERFORM EVALUATION
   20     S(K) = ((C(I, 3)*D+C(I, 2))*D+C(I, 1))*D+Y(I) 
          GO TO 40
   25     S(K) = Y(I) 
          GO TO 40
C                                   WARNING - U(I) .LT. X(1)
   30     JER = 33
          GO TO 20
C                                   IF U(I) .GT. X(NX) - WARNING
   35     IF (DD .GT. ZERO) KER = 34
          D = U(K)-X(NXM1)
          I = NXM1
          GO TO 20
   40  CONTINUE 
       IER = MAX0(JER, KER) 
C      REMOVE CALLS TO IMSL ERROR MESSAGE SUBS
  
 9005  RETURN 
       END
