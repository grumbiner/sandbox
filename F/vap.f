      PROGRAM vapor
      REAL C, T, V
      
      DO T = -20, 120
        C = (T - 32) / 1.8
        PRINT *,T,' ',w3fa09(C+273.15)
      ENDDO

      STOP
      END
        
      REAL FUNCTION W3FA09 (TK)
C$$$  SUBROUTINE DOCUMENTATION BLOCK ***
C
C SUBR: W3FA09   - TEMPERATURE TO SATURATION VAPOR PRESSURE
C   AUTHOR: CHASE, P.          ORG: W345          DATE:    OCT 78
C   UPDATE: JONES, R.E.        ORG: W342          DATE: 26 JUN 84
C
C ABSTRACT: COMPUTES SATURATION VAPOR PRESSURE IN KILOPASCALS GIVEN
C   TEMPERATAURE IN KELVINS.
C
C PROGRAM HISTORY LOG:
C   78-10-01  P.CHASE
C   84-06-26  R.E.JONES  CHANGE TO IBM VS FORTRAN
C   84-06-26  R.E.JONES  CHANGE TO MICROSOFT FORTRAN 4.10
C   90-06-08  R.E.JONES  CHANGE TO SUN FORTRAN 1.3
C   91-03-29  R.E.JONES  CONVERT TO SiliconGraphic FORTRAN
C   93-03-29  R.E.JONES  ADD SAVE STATEMENT
C   95-09-25  R.E.JONES  CHANGE TK TO CRAY 64 BIT REAL, CHANGE DOUBLE
C                        PRECISION TO CRAY 64 BIT REAL.
C
C USAGE:  VP = W3FA09 (TK)
C
C   INPUT:
C     'TK' - REAL*8 TEMPERATURE IN KELVINS. IF TK < 223.16, THE VALUE
C            223.16 WILL BE USED.  IF TK > 323.16, THE VALUE 323.16
C            WILL BE USED AS THE ARGUMENT. 'TK' ITSELF IS UNCHANGED.
C   OUTPUT:
C     'VP' - SATURATION VAPOR PRESSURE IN KILOPASCALS.
C            0.0063558 < VP < 12.3395
C
C   NOTES: W3FA09 MAY BE DECLARED REAL*8 SO THAT A REAL*8 VALUE IS
C     RETURNED, BUT NO INCREASE IN ACCURACY IS IMPLIED.
C
C   EXIT STATES: NONE
C
C   EXTERNAL REFERENCES: NONE
C
C ATTRIBUTES:
C   LANGUAGE: CRAY CFT77 FORTRAN
C   MACHINE:  CRAY C916/256, J916/2048
C
C$$$
C
C THE CHEBYSHEV COEFFICIENTS ARE IN ARRAY C, LOW-ORDER TERM FIRST.
C
      REAL   C(9)
      REAL   ARG,H0,H1,H2
C
      SAVE
C
      DATA  C     /
     & 0.313732865927E+01, 0.510038215244E+01, 0.277816535655E+01,
     & 0.102673379933E+01, 0.254577145215E+00, 0.396055201295E-01,
     & 0.292209288468E-02,-0.119497199712E-03,-0.352745603496E-04/
C
C SCALE TK TO RANGE -2, +2 FOR SERIES EVALUATION.  INITIALIZE TERMS.
C
      ARG = -1.09264E1+4.0E-2*AMAX1(223.16,AMIN1(323.16,TK))
      H0  = 0.0
      H1  = 0.0
C
C EVALUATE CHEBYSHEV POLYNOMIAL
C
      DO 10 I=1,9
        H2 = H1
        H1 = H0
        H0 = ARG * H1 - H2 + C(10-I)
 10   CONTINUE
      W3FA09 = 0.5 * (C(1) - H2 + H0)
      RETURN
      END
