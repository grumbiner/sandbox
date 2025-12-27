      FUNCTION DIR(X,Y)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .     
C SUBPROGRAM:    DIR         CMPTE VEC DIR FROM U-V
C   PRGRMMR: TREADON         ORG: W/NMC2     DATE: 93-05-12       
C     
C ABSTRACT:  
C     GIVEN U AND V WIND COMPONENTS, THIS FUNCTION CALCULATES
C     THE VECTOR WIND DIRECTION (IN DEGREES)
C   .     
C     
C PROGRAM HISTORY LOG:
C   ??-??-??  CHRIS PETERS ??
C   93-05-12  RUSS TREADON - ADDED DOCBLOC
C     
C USAGE:   WDIR = DIR(X,Y)
C   INPUT ARGUMENT LIST:
C     X        - U WIND COMPONENT
C     Y        - V WIND COMPONENT
C
C   OUTPUT ARGUMENT LIST: 
C     DIR      - VECTOR WIND DIRECTION IN DEGREES
C     
C   OUTPUT FILES:
C     NONE
C     
C   SUBPROGRAMS CALLED:
C     UTILITIES:
C       NONE
C     LIBRARY:
C       NONE
C     
C   ATTRIBUTES:
C     LANGUAGE: FORTRAN
C     MACHINE : CRAY Y-MP
C$$$  
C
C***************************************************************************
C     START FUNCTION DIR HERE.
C
       IF(X.EQ.0.0)THEN
         IF(Y.GT.0.0)THEN
           DIR=90.
         ELSEIF(X.EQ.0.0)THEN
           DIR=360.
         ELSE
           DIR=270.
         ENDIF
       ELSE
         DIR=ATAN(Y/X)*180./3.14159
         IF(X.LT.0.0)THEN
           DIR=DIR+180.
         ENDIF
         IF(DIR.LT.0.0)THEN
           DIR=DIR+360.
         ENDIF
       ENDIF
       RETURN
       END
