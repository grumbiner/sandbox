C###########################################################	
C***************************************************************************
C
C Name: RONN
C
C Language: FORTRAN77                         Type - SUBROUTINE 
C
C Version: 1.0          Date: 11-17-97        Author: V. Krasnopolsky 
C
C ----------------------------------
C
      FUNCTION RONN(X)
C
C ----------------------------------
C
C Description:   This NN calculates the water density RO
C ------------   given the temperature (T), salinity (S), and depth (Z)
C                Bias(i) = 0.0      RMS = 0.05
C                MIN err = -0.15     MAX err(i) = 0.15
C                -------------------------------------
C   NN WAS TRAIND FOR -4 =< T =< 33. , 29 =< S =< 39            
C
C***************************************************************************
C
        INTEGER HID,OUT
        PARAMETER (IN = 3, HID = 4)
C
C  Arguments:
C  ---------
C  INPUT:
C            X(1) = T - temperature (in C)
C            X(2) = S - salinity (in psu)
C            X(3) = Z - depth (in m)
C
       DIMENSION X(IN)
C
C   OUTPUT:
C            RO - water density (in kg/(m^3))
C
C  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C  Internal variables:
C  -------------------
C
C        IN - NUMBER OF NN INPUTS
C
C        HID - NUMBER OF HIDDEN NODES
C
C        W1 - INPUT WEIGHTS 
C
C        W2 - HIDDEN WEIGHTS
C
C        B1 - HIDDEN BIASES
C
C        B2 - OUTPUT BIAS
C
       DIMENSION W1(IN,HID),W2(HID),B1(HID)
C
C        A, B - OUTPUT TRANSFORMATION COEFFICIENTS
C
C###########################################################################
C
C
       DATA ((W1(I,J),J = 1,HID),I = 1,IN)
     & /
     &     0.0359693,
     &    -0.0557354,
     &     0.0821887,
     &     0.0206442,
     &     0.0339594,
     &   -0.00476632,
     &    0.00647752,
     &    -0.0400266,
     &   6.71088e-07,
     &   5.75833e-06,
     &   1.89477e-06,
     &  -1.77325e-07/
       DATA (W2(I),I = 1,HID)
     & /
     &      0.821298,
     &      0.713425,
     &      -1.76321,
     &      -1.32240/
       DATA (B1(I), I=1,HID)
     & /
     &      -1.51879,
     &       1.14567,
     &      -3.82086,
     &       2.46307/
       DATA B2
     & /
     &     -0.574046/
       DATA A
     & /
     &       22.5577/
       DATA B
     & /
     &       19.0461/
C
C - START NEURAL NETWORK
C
C  - INITIALIZE X3
C
         X3 = 0.
C
C  - INITIALIZE X2
C
          DO I = 1, HID
            X2 = 0.
            DO J = 1,IN
               X2 = X2 + X(J) * W1(J,I)
            END DO
            X2 = X2 + B1(I)
            O2 = TANH(X2)
            X3 = X3 + W2(I)*O2
          END DO
C
          X3 = X3 + B2
C
C --- CALCULATE O3
C
          O3 = TANH(X3)
          RONN = A * O3 + B - 20.
C
       RETURN
C
       END
