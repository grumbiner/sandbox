C
C ##########################################################################
C***************************************************************************
C
C Name: SALNN
C
C Language: FORTRAN77                         Type - SUBROUTINE 
C
C Version: 1.0          Date: 11-15-97        Author: V. Krasnopolsky 
C
C ----------------------------------
C
      FUNCTION SALNN(X)
C
C ----------------------------------
C
C Description:   This NN calculates the water salinity S (in psu)
C ------------   given the temperature (T), density - 1000. (RO), and depth (Z)
C                Bias(i) = 0.00      RMS = 0.15
C                MIN err = -0.4     MAX err(i) = 0.4
C                NN trained for  -2. =< T =< 33. C,  
C                and 29 =< S =< 39 psu
C
C***************************************************************************
C
        INTEGER HID
        PARAMETER (IN = 3, HID = 5)
C
C  Arguments:
C  ---------
C  INPUT:
C            X(1) = T - temperature (in C)
C            X(2) = RO - water density - 1000. (in kg/(m^3)) 
C            X(3) = Z - depth (in m)
C
       DIMENSION X(IN)
C
C   OUTPUT:
C            SALNN - salinity (in psu)
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
     &     -0.143034,
     &    -0.0277412,
     &     -0.465934,
     &      0.116133,
     &      0.226048,
     &     -0.165078,
     &     0.0416976,
     &     -0.221965,
     &      0.207854,
     &      0.182610,
     &   1.98005e-05,
     &  -3.44453e-06,
     &  -6.22486e-06,
     &   4.27837e-06,
     &  -1.34547e-05/
       DATA (W2(I),I = 1,HID)
     & /
     &     -0.531174,
     &       1.87099,
     &      -3.00296,
     &      0.417169,
     &      0.761534/
       DATA (B1(I), I=1,HID)
     & /
     &       5.91283,
     &      -1.17582,
     &       18.3848,
     &      -5.31182,
     &      -9.29538/
       DATA B2
     & /
     &       4.18037/
       DATA A
     & /
     &       8.25000/
       DATA B
     & /
     &       33.9500/
C
C - START NEURAL NETWORK
C
C
        X(2) = X(2) + 20.
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
          SALNN = A * O3 + B
      
C
       RETURN
C
       END
