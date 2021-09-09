       SUBROUTINE IQHSCU(KRAT, Y, NX, C, IC, IER) 
C    IMSL ROUTINE NAME   - IQHSCU 
C    COMPUTER            - CDC/SINGLE 
C    LATEST REVISION     - JANUARY 1, 1978
C    PURPOSE             - ONE-DIMENSIONAL QUASI-CUBIC HERMITE
C                            INTERPOLATION
C    USAGE               - CALL IQHSCU (X, Y, NX, C, IC, IER) 
C      X ARRAY REPLACED BY STATEMENT FUNCTION ALBERT LUNDE 10/3/79
C      X(I)=(I-1)*KRAT
C      REMOVE CALLS TO IMSL ERROR MESSAGE SUBS
C                 IER    - ERROR PARAMETER. (OUTPUT)
C                          TERMINAL ERROR 
C                            IER = 129, IC IS LESS THAN NX. 
C                            IER = 130, NX IS LESS THAN 4.
C                            IER = 131, INPUT ABSCISSAE ARE NOT ORDERED 
C                              SO THAT X(1) .LT. X(2) ... .LT. X(NX). 
C    PRECISION/HARDWARE  - SINGLE AND DOUBLE/H32
C                        - SINGLE/H36,H48,H60 
C    REMARKS      THE COEFFICIENTS OF THE INTERPOLATING POLYNOMIAL
C                 ARE CONTAINED IN THE FIRST NX-1 ROWS OF THE MATRIX C. 
C                 THE ONE REMAINING ROW OF THE MATRIX C IS USED AS A
C                 WORK STORAGE AREA.
C    COPYRIGHT           - 1978 BY IMSL, INC. ALL RIGHTS RESERVED.
C                                   SPECIFICATIONS FOR ARGUMENTS
       INTEGER KRAT,NX,IC,IER 
       REAL Y(NX),C(IC,3) 
C                                   SPECIFICATIONS FOR LOCAL VARIABLES
       INTEGER            I,N0
       REAL               T1,T2,B,RM1,RM2,RM3,RM4,HALF,ONE,ZERO 
       DATA               HALF/0.50/,ONE/1.0/,ZERO/0.0/ 
       X(I) = FLOAT((I-1)*KRAT) 
C                                   FIRST EXECUTABLE STATEMENT
       IF (IC .GE. NX) GO TO 5
C                                   TERMINAL - IC LESS THAN NX
       IER = 129
       GO TO 9000 
    5  IF (NX .GT. 3) GO TO 10
C                                   TERMINAL - TOO FEW DATA POINTS
       IER = 130
       GO TO 9000 
   10  DO 15 I=2,NX 
          IF (X(I) .GT. X(I-1)) GO TO 15
C                               TERMINAL - X NOT MONOTONE INCREASING
          IER = 131 
          GO TO 9000
   15  CONTINUE 
       IER = 0
       RM3 = (Y(2)-Y(1))/(X(2)-X(1))
       T1 = RM3-(Y(2)-Y(3))/(X(2)-X(3)) 
       RM2 = RM3+T1 
       RM1 = RM2+T1 
C                                   NOW GET THE SLOPES
       N0 = NX-2
       DO 40 I = 1,NX 
C     I1 AND I2 INTRIDUCED TO AVIOD ERRORS WITH FTN5 OPT = 2. 
      I1 = I+1
      I2 = I+2
          IF (I .GT. N0) GO TO 20 
          RM4 = (Y(I2)-Y(I1))/(X(I2)-X(I1)) 
          GO TO 25
   20     RM4 = RM3-RM2+RM3 
   25     T1 = ABS(RM4-RM3) 
          T2 = ABS(RM2-RM1) 
          B = T1+T2 
          IF (B .NE. ZERO) GO TO 30 
C                                   IF DENOMINATOR IS ZERO, GET AVERAGE 
          C(I,1) = HALF*(RM2+RM3) 
          GO TO 35
   30     C(I,1) = (T1*RM2+T2*RM3)/B
   35     RM1 = RM2 
          RM2 = RM3 
          RM3 = RM4 
   40  CONTINUE 
       N0 = NX-1
C                                   COMPUTE THE COEFFICIENTS FOR THE
C                                     NX-1 INTERVALS
       DO 45 I = 1,N0 
C     I1 INTRODUCED TO AVOID ERRORS WITH FTN5 OPT = 2 
      I1 = I+1
          T1 = ONE/(X(I1)-X(I)) 
          T2 = (Y(I1)-Y(I))*T1
          B = (C(I,1)+C(I1,1)-T2-T2)*T1 
          C(I,3) = B*T1 
          C(I,2) = -B+(T2-C(I,1))*T1
   45  CONTINUE 
 9000  CONTINUE 
C      REMOVE CALLS TO IMSL ERROR MESSAGE SUBS
       RETURN 
       END
