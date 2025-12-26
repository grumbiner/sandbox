       SUBROUTINE DOTSMO(A1,A2,A3,L,XAA,ALP)
C$$$   SUBPROGRAM  DOCUMENTATION  BLOCK
C
C SUBPROGRAM: DOTSMO         ROBERT TIME FILTER
C   AUTHOR: DENNIS DEAVEN    ORG: W/NMC23    DATE: 23 JUN 83
C ABSTRACT: APPLIES ROBERT TIME FILTER TO ENTIRE LFM DOMAIN
C
C USAGE:  CALL DOTSMO(A1,A2,A3,L,XAA,ALP)
C
C - - - - - - - - - I N P U T   V A R I A B L E S  - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C
C     A1          ANY FORECAST VARIABLE AT TAU-1 LEVEL      ARGUMENT
C     A2                 DITTO             TAU   LEVEL      ARGUMENT
C     A3                 DITTO             TAU+1 LEVEL      ARGUMENT
C     L           LENGTH OF VECTORS A1,A2,A3 (LI X LJ X LK) ARGUMENT
C     XAA         COEFFICIENT FOR TAU LEVEL                 ARGUMENT
C     ALP               DITTO     TAU-1 AND TAU+1 LEVELS    ARGUMENT
C 
C - - - - - - - - - O U T P U T   V A R I A B L E S - - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     A2          TAU LEVEL AFTER TIME FILTERING            ARGUMENT
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C ATTRIBUTES:
C   LANGUAGE: SiliconGraphics 3.5 FORTRAN 77
C   MACHINE:  SiliconGraphics IRIS-4D/25, 35, INDIGO
C
C$$$
       IMPLICIT REAL (A-H,O-Z)
       REAL A1(*),A2(*),A3(*)
       SAVE
C
       DO 10 I = 1,L
         A2(I) = A2(I) * XAA + ALP * (A1(I) + A3(I))
10     CONTINUE
C
       RETURN
       END
