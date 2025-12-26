       FUNCTION ABSH2O(U)
C$$$   SUBPROGRAM  DOCUMENTATION  BLOCK
C
C SUBPROGRAM: ABSH2O         COMPUTES ENERGY ABSORBED BY H2O
C   AUTHOR: DENNIS DEAVEN    ORG: W/NMC23    DATE: 21 JUN 83
C
C ABSTRACT: CALCULATES THE ENERGY ABSORBED BY WATER VAPOR. APPROXIMATES
C   THE ABSORPTION CURVE WITH SIX STRAIGHT LINES (MANABE).
C
C USAGE:  XX = ABSH2O(U)
C
C - - - - - - - - - I N P U T   V A R I A B L E S  - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     U           E-3 / COS(ZENITH) * P WEIGHTED WS         ARGUMENT
C                 (SEE TEND4 FOR COMPUTATION OF U)
C
C - - - - - - - - - O U T P U T   V A R I A B L E S - - - - - - - - - -
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     ABSH2O      ENERGY ABSORBED BY U                      ARGUMENT
C 
C - - - - - - - - - S U B P R O G R A M S   C A L L E D - - - - - - - -
C     NAME(S)                                               LIBRARY
C     -------                                               -------
C     LOG10                                                 FORTLIB
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C ATTRIBUTES:
C   LANGUAGE: SiliconGraphics 3.5 FORTRAN 77
C   MACHINE:  SiliconGraphics IRIS-4D/25, 35, INDIGO
C$$$
C
      IMPLICIT REAL (A-H,O-Z)
      SAVE
C
      A = 0.E0
      IF (U) 55, 55, 5
    5 X = LOG10(U)
      IF (X) 15, 10, 10
   10 A =.15E0* X + .2E0
      GO TO 55
   15 IF (X +1.E0) 25, 20, 20
   20 A =.1E0* X + .2E0
      GO TO 55
   25 IF (X +2.E0) 35, 30, 30
   30 A =.05E0* X + .15E0
      GO TO 55
   35 IF (X +3.E0) 45, 40, 40
   40 A =.04E0* X + .13E0
      GO TO 55
   45 IF (X +5.E0) 55, 50, 50
   50 A =.005E0* X + .025E0
   55 ABSH2O = A
      RETURN
      END
