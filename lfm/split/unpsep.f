      SUBROUTINE UNPSEP(THE,P,PIIN,TGES,TR,NS)
C$$$  SUBPROGRAM  DOCUMENTATION  BLOCK
C
C SUBPROGRAM: UNPSEP         FINDS TEMPERATURE OF MOIST ADIABAT
C   AUTHOR: DENNIS DEAVEN    ORG: W/NMC23    DATE: 21 JUN 83
C
C ABSTRACT: CALCULATES (BY ITERATION) THE TEMPERATURE OF MOIST
C   ADIABAT GIVEN: PSEUDO-EQUIVALENT POTENTIAL TEMPERATURE AND
C   PRESSURE OF THE PARCEL.  ASSUMES SATURATION AT CALCULATED P&T.
C
C USAGE:  CALL UNPSEP(THE,P,PIIN,TGES,TR,NS)
C
C - - - - - - - - - I N P U T   V A R I A B L E S  - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     THE         PSEUDO-EQUIVALENT POT. TEMPERATURE (K)    ARGUMENT
C     P           PRESSURE OF PARCEL (MB)                   ARGUMENT
C     PIIN        EXNER FUNCTION (P/1000)**R/CP OF PARCEL   ARGUMENT
C     TGES        FIRST GUESS TEMPERATURE (C)               ARGUMENT
C 
C - - - - - - - - - O U T P U T   V A R I A B L E S - - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     TR          TEMPERATURE OF MOIST ADIABAT (C)          ARGUMENT
C     NS          ITERATION COUNT (LIMITED TO 100)          ARGUMENT
C                 WHEN EQUAL TO 200 INDICATES NONCONVERGENCE
C 
C - - - - - - - - - S U B P R O G R A M S   C A L L E D - - - - - - - -
C
C     NAME(S)                                               LIBRARY
C     -------                                               -------
C     ABS,SIGN                                              FORTLIB
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C ATTRIBUTES:
C   LANGUAGE: SiliconGraphics 3.5 FORTRAN 77
C   MACHINE:  SiliconGraphics IRIS-4D/25, 35, INDIGO
C$$$
      IMPLICIT REAL (A-H,O-Z)
      SAVE
C
      SV(T) = 6.1078E0 * EXP(17.2694E0*T/(T+237.3E0))
      EEP(T,SVA,P) = EXP((596.73E0-0.601E0*T)*
     1   ((0.62197E0*SVA)/(P-SVA))/
     2   (0.24E0*(T+273.16E0)))
C
      DATA CON/0.5E0/
C
      NS   = 0
      T    = TGES
      DTT  = 10.0E0
      A    = 0.0E0
  800 CONTINUE
        SVA   = SV(T)
        NS    = NS+1
        IF (NS.GT.100) GO TO 870
        AFORM = A
        A     = (T+273.16E0)*PIIN*EEP(T,SVA,P)-THE
        IF (ABS(A).LT.CON) GO TO 870
        DTT   = 0.5E0*DTT
        IF ((A*AFORM).LT.0.0E0) DTT = -DTT
        TP    = T + DTT
        SVA   = SV(TP)
        AP    = (TP+273.16E0)*PIIN*EEP(TP,SVA,P)-THE
        IF (ABS(AP).LT.CON) GO TO 869
C   USE NEWTON-RAPHSON METHOD TO FIND NEXT ESTIMATE.
C   DTT IS DISTANCE FROM T TO NEXT ESTIMATE.
C   TEST MAGNITUDE OF DENOMINATOR BEFORE COMPUTING DTT.
      XDEN = A-AP
      IF (XDEN.NE.0.0E0) GO TO 400
      NS   = 200
      GO TO 870
  400 CONTINUE
      DTT  = A * DTT / XDEN
      IF (ABS(DTT).LT.0.01E0) DTT = SIGN(0.01E0,DTT)
      T    = T + DTT
      T    = MIN(T,50.0E0)
      GO TO 800
  869 CONTINUE
        T  = TP
  870 CONTINUE
        TR = T
      RETURN
      END
