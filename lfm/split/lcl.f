      SUBROUTINE  LCL(THETA,SATMIX,PLCL,TLCL,NS)
C$$$  SUBPROGRAM  DOCUMENTATION  BLOCK
C
C SUBPROGRAM: LCL            FINDS LIFTED CONDENSATION LEVEL (P&T)
C   AUTHOR: DENNIS DEAVEN    ORG: W/NMC23    DATE: 21 JUN 83
C
C ABSTRACT: COMPUTES (BY ITERAION) THE PRESSURE AND TEMPERATURE OF
C   THE LIFTED CONDENSATION LEVEL GIVEN THE POTENTIAL TEMPERATURE
C   AND MIXING RATIO OF A PARCEL.
C
C USAGE:  CALL LCL(THETA,SATMIX,PLCL,TLCL,NS)
C
C - - - - - - - - - I N P U T   V A R I A B L E S  - - - - - - - - -
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     THETA       POTENTIAL TEMPERATURE OF THE PARCEL (K)   ARGUMENT
C     SATMIX      MIXING RATIO OF THE PARCEL (G/G)          ARGUMENT
C
C - - - - - - - - - O U T P U T   V A R I A B L E S - - - - - - - - - -
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     PLCL        LIFTED CONDENSATION PRESS. (MB)           ARGUMENT
C     TLCL             DITTO          TEMP.  (K)            ARGUMENT
C     NS          ITERATION COUNT (LIMITED TO 20)           ARGUMENT
C 
C - - - - - - - - - S U B P R O G R A M S   C A L L E D - - - - - - - -
C     NAME(S)                                               LIBRARY
C     -------                                               -------
C     LOG10,REAL,ABS                                        FORTLIBC
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C ATTRIBUTES:
C   LANGUAGE: SiliconGraphics 3.5 FORTRAN 77
C   MACHINE:  SiliconGraphics IRIS-4D/25, 35, INDIGO
C$$$
      IMPLICIT REAL (A-H,O-Z)
      SAVE
C
C  FINDS BY ITERATION THE (P,T)COORDINATES OF THE LIFTING CONDENSATION
C  LEVEL,GIVEN THE POTENTIAL TEMPERATURE AND MIXING RATIO.
C  PLCL IN MB, TLCL IN DEGREES CELSIUS.
      NS = 0
C  PITL1 IS FIRST ESTIMATE OF PIT, USING CIT = 0.5
      PITL1 = (REAL(289.95E0/THETA)**3.5)*1000.0E0
C     START OF ITERATION LOOP
 10   CONTINUE
      NS = NS+1
      IF (NS.GT.20) GO TO 15
      CIT=LOG10((PITL1*SATMIX)/(6.11E0*(0.622E0+SATMIX)))
C    2047.5 = 7.5 * 273.0
      PIT  =(REAL((CIT*(-35.7E0)+2047.5E0)/(THETA*(7.5E0-CIT)))
     A     **3.5)*1000.0E0
      IF (ABS(PIT-PITL1)-1.) 15,11,11
   11 PITL1=PIT
      GO TO 10
C     END OF ITERATION LOOP
   15 PLCL=PIT
      TLCL=(CIT*237.3E0)/(7.5E0-CIT)
      RETURN
      END
