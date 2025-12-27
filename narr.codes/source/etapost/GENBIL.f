      SUBROUTINE GENBIL(EGFUL,IMOT,JMOT)
C
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .     
C SUBPROGRAM:    GENBIL      COMPUTE BILINEAR INTRP WEIGHTS
C   PRGRMMR: TREADON         ORG: W/NP2      DATE: 92-12-23       
C     
C ABSTRACT:
C     THIS ROUTINE IS BASED ON CODE SUBROUTINE INTERP FOUND
C     IN DAVID PLUMMER'S ETAPACKC.  GIVEN THE E-GRID AND
C     OUTPUT GRID BILINEAR INTERPOLATION WEIGHTS ARE COMPUTED
C     AND SAVED COMMON BLOCK LLGRDS ARRAYS.  THE PASSED FILLED
C     E-GRID ARRAY, EGFUL CAN CONTAIN ANY FIELD.  IT IS USED
C     TO DETERMINE THE EXTENT KNOW DATA VALUES IN THE E-GRID.
C     THOSE GRID POINTS ON THE E-GRID WHICH EQUAL OR EXCEED
C     THE SPECIAL VALUE, SPVC, ARE TREATED DIFFERENTLY IN THE
C     INTERPOLATION.  FOR MOST APPLICATIONS THIS CHECK IS NOT
C     NECESSARY.
C   .     
C     
C PROGRAM HISTORY LOG:
C   ??-??-??  DAVID PLUMMER - SUBROUTINE INTERP IN ETAPACKC
C   92-12-23  RUSS TREADON - EXTRACTED CODE FROM INTERP AND 
C                               GENERALIZED TO HANDLE VARIOUS  
C                               INPUT AND OUTPUT GRIDS
C     
C USAGE:    CALL GENBIL(EGFUL,IMOT,JMOT)
C   INPUT ARGUMENT LIST:
C     EGFUL    - FILLED E-GRID DEFINING EXTENT OF E-GRID.  
C     IMOT     - FIRST DIMENSION OF OUTPUT GRID.
C     JMOT     - SECOND DIMENSION OF OUTPUT GRID.
C   OUTPUT ARGUMENT LIST: 
C     
C   OUTPUT FILES:
C     STDOUT  - RUN TIME STANDARD OUT.
C     
C   SUBPROGRAMS CALLED:
C     UTILITIES:
C       NONE
C     LIBRARY:
C       COMMON   - LLGRDS
C                  IOUNIT
C     
C   ATTRIBUTES:
C     LANGUAGE: FORTRAN
C     MACHINE : CRAY C-90
C$$$  
C
C
C     INCLUDE DECLARED GRID DIMENSIONS.
      INCLUDE "parmeta"
      INCLUDE "parmout"
      PARAMETER (IMJM=IM*JM-JM/2,IMT=2*IM-1,JMT=JM)
C     
C     DECLARE VARIABLES.
      LOGICAL LM0N0,LM1N0,LM0N1,LM1N1
      INTEGER IMOT,JMOT
      REAL EGFUL(IMT,JMT)
C     
C     INCLUDE COMMON.
      INCLUDE "LLGRDS.comm"
      INCLUDE "IOUNIT.comm"
C
C     SET TOLERANCE LIMITS.
C     
      DATA SPVC,SMALL /1.E20,1.E-4/
C     
C******************************************************************
C     START GENBIL HERE.
C     
C     COMPUTE INTERPOLATION WEIGHTS.
C     
      WRITE(STDOUT,*)'GENBIL:  GENERATE BILIN WEIGHTS'
      MAXI = IMT
      MAXJ = JMT
      DO 90 J = 1,JMOT
         DO 90 I = 1,IMOT
            XX = EGRDI(I,J)
            YY = EGRDJ(I,J)
            M = XX                                                           
            N = YY 
            IEGRD(I,J) = M
            JEGRD(I,J) = N
C
            IF ( ( M.LE.0   ).OR.( N.LE.0   ) ) THEN
               WIJ(I,J)   = 0.
               WIPJ(I,J)  = 0.
               WIJP(I,J)  = 0.
               WIPJP(I,J) = 0.
               IWGT(I,J)  = 0
               GOTO 75
            ENDIF
C
            IF ( (XX.GT.MAXI).OR.(YY.GT.MAXJ) ) THEN
               WIJ(I,J)   = 0.
               WIPJ(I,J)  = 0.
               WIJP(I,J)  = 0.
               WIPJP(I,J) = 0.
               IWGT(I,J)  = 0
               GOTO 75
            ENDIF
C
            DX  = (XX - M)                                                    
            DY  = (YY - N)                                                    
            DX1 = 1.0 - DX                                                   
            DY1 = 1.0 - DY                                                   
            IF ( (ABS(DX ).LT.SMALL).AND.(ABS(DY ).LT.SMALL) ) THEN
               W00 = 1.
               W10 = 0.
               W01 = 0.
               W11 = 0.
               WIJ(I,J)   = W00
               WIPJ(I,J)  = W10
               WIJP(I,J)  = W01
               WIPJP(I,J) = W11
               IWGT(I,J)  = 1
               GOTO 75
CX               DAT = EGFUL(M,N)
            ELSEIF ( (ABS(DX ).LT.SMALL).AND.
     X              (ABS(DY1).LT.SMALL) ) THEN
               W00 = 0.
               W10 = 0.
               W01 = 1.
               W11 = 0.
               WIJ(I,J)   = W00
               WIPJ(I,J)  = W10
               WIJP(I,J)  = W01
               WIPJP(I,J) = W11
               IWGT(I,J)  = 1
               GOTO 75
CX               DAT = EGFUL(M,N+1)
            ELSEIF ( (ABS(DX1).LT.SMALL).AND.
     X              (ABS(DY ).LT.SMALL) )  THEN
               W00 = 0.
               W10 = 1.
               W01 = 0.
               W11 = 0.
               WIJ(I,J)   = W00
               WIPJ(I,J)  = W10
               WIJP(I,J)  = W01
               WIPJP(I,J) = W11
               IWGT(I,J)  = 1
               GOTO 75
CX               DAT = EGFUL(M+1,N)
            ELSEIF ( (ABS(DX1).LT.SMALL).AND.
     X              (ABS(DY1).LT.SMALL) )  THEN
               W00 = 0.
               W10 = 0.
               W01 = 0.
               W11 = 1.
               WIJ(I,J)   = W00
               WIPJ(I,J)  = W10
               WIJP(I,J)  = W01
               WIPJP(I,J) = W11
               IWGT(I,J)  = 1
               GOTO 75
CX               DAT = EGFUL(M+1,N+1)
            END IF                                                           
C
C	    DONE WITH SPECIAL CASES.  NOW FOR GENERAL CASES.
C
            LM0N0 = (EGFUL(M  ,N  ).GE.SPVC)                                  
            LM1N0 = (EGFUL(M+1,N  ).GE.SPVC)                                  
            LM0N1 = (EGFUL(M  ,N+1).GE.SPVC)                                  
            LM1N1 = (EGFUL(M+1,N+1).GE.SPVC)                                  
            IF ( (.NOT.LM0N0).AND.(.NOT.LM1N0).AND.
     X           (.NOT.LM0N1).AND.(.NOT.LM1N1) ) THEN
               W00 = (1.-DY)*(1.-DX)
               W10 = (1.-DY)*DX
               W01 = DY*(1.-DX)
               W11 = DY*DX
               WIJ(I,J)   = W00
               WIPJ(I,J)  = W10
               WIJP(I,J)  = W01
               WIPJP(I,J) = W11
               IWGT(I,J)  = 1
CX               DAT = (1.0-DY)*((1.0-DX)*EGFUL(M,N) +
CX                     DX*EGFUL(M+1,N)) +  
CX                     DY*((1.0-DX)*EGFUL(M,N+1) +
CX                     DX*EGFUL(M+1,N+1))    
               GOTO 75
            ELSEIF (LM0N0.AND.LM1N0.AND.LM0N1.AND.LM1N1) THEN              
               WIJ(I,J)   = 0.
               WIPJ(I,J)  = 0.
               WIJP(I,J)  = 0.
               WIPJP(I,J) = 0.
               IWGT(I,J)  = 0
               GOTO 75
CX               DAT = SPVAL                                                   
            ELSE                                                             
               IM0N0 = 1                                                     
               IF (LM0N0)  IM0N0 = 0                                         
               IM1N0 = 1                                                     
               IF (LM1N0)  IM1N0 = 0                                         
               IM0N1 = 1                                                     
               IF (LM0N1)  IM0N1 = 0                                         
               IM1N1 = 1                                                     
               IF (LM1N1)  IM1N1 = 0                                         
               RXX = ((SQRT(2.0)/4.0)*(IM0N0+IM1N0+IM0N1+IM1N1))**2      
               R00 = (     DX *     DX)  + (     DY *     DY)                
               R10 = ((1.0-DX)*(1.0-DX)) + (     DY *     DY)                
               R01 = (     DX *     DX)  + ((1.0-DY)*(1.0-DY))               
               R11 = ((1.0-DX)*(1.0-DX)) + ((1.0-DY)*(1.0-DY))               
               W00 = IM0N0 * MAX(0.0,(RXX - R00)) / (RXX + R00)              
               W10 = IM1N0 * MAX(0.0,(RXX - R10)) / (RXX + R10)              
               W01 = IM0N1 * MAX(0.0,(RXX - R01)) / (RXX + R01)              
               W11 = IM1N1 * MAX(0.0,(RXX - R11)) / (RXX + R11)              
               WSUM = W00 + W01 + W10 + W11                                  
               IF (WSUM .NE. 0.0)  THEN                                      
                  WIJ(I,J)   = W00/WSUM
                  WIPJ(I,J)  = W10/WSUM
                  WIJP(I,J)  = W01/WSUM
                  WIPJP(I,J) = W11/WSUM
                  IWGT(I,J)  = 1
                  GOTO 75
CX                DAT = (W00*EGFUL(M,N)+W10*EGFUL(M+1,N) +
CX                  W01*EGFUL(M,N+1)+W11*EGFUL(M+1,N+1)) / WSUM 
               ELSE                                                          
                  WIJ(I,J)   = 0.
                  WIPJ(I,J)  = 0.
                  WIJP(I,J)  = 0.
                  WIPJP(I,J) = 0.
                  IWGT(I,J)  = 0
                  GOTO 75
CX                  DAT = SPVAL                                                
               END IF
            END IF       
            WRITE(STDOUT,*)'GENWGT:  SHOULD NOT REACH THIS LINE'
 75         CONTINUE
 90   CONTINUE
C     
C     END OF ROUTINE.
C     
      RETURN
      END
