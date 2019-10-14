C***************************************************************************
C
C Name: DANN
C
C Language: FORTRAN77                         Type - SUBROUTINE
C
C Version: 2.0          Date: 02-10-1998        Author: V. Krasnopolsky
C
C ----------------------------------
C
      FUNCTION DANN(T,S,Z)
C
C ----------------------------------
C Description:   This NN calculates the seawater density anomaly  = 
C ------------   = density - 1000.; given the temperature (T),
C			    salinity (S), and depth (Z)
C
C                Bias(i) = 0.00      RMS = 0.06
C                MIN err = -0.6      MAX err(i) = 0.6
C
C
C
C***************************************************************************
C
        INTEGER HID
        PARAMETER (IN = 3, HID = 7)
C
C  Arguments:
C  ---------
C  INPUT:
C            T - temperature (in C):  -2. C < T < 35. C,
C            S - salinity (in psu):  5. psu < S < 38. ps
C            Z - depth (in m):  0. m < Z < 5700. m
C
       DIMENSION X(IN)
C
C   OUTPUT:
C            DANN - seawater density anomaly = 
C				= density- 1000. (in kg/(m^3)
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
     &    -0.0447101,
     &     0.0300369,
     &    0.00922823,
     &     0.0339611,
     &   0.000650965,
     &    0.00377705,
     &    0.00401528,
     &    0.00879792,
     &    -0.0531459,
     &    -0.0131810,
     &   0.000857682,
     &    0.00813362,
     &    0.00791417,
     &    -0.0218669,
     &   7.58094e-05,
     &  -1.26525e-05,
     &   1.26904e-05,
     &   3.86479e-06,
     &   6.33793e-05,
     &   8.39456e-05,
     &  -0.000142057/
       DATA (W2(I),I = 1,HID)
     & /
     &    -0.0933599,
     &     0.0329983,
     &     -0.648872,
     &     -0.346991,
     &       2.01406,
     &       1.03772,
     &      0.500001/
       DATA (B1(I), I=1,HID)
     & /
     &      0.708841,
     &      0.788995,
     &      0.133519,
     &      -1.08981,
     &     -0.628186,
     &      0.262733,
     &      0.827411/
       DATA B2
     & /
     &     -0.236657/
       DATA A
     & /
     &       49.0333/
       DATA B
     & /
     &       27.1599/
C
C - START NEURAL NETWORK
C
C  - INITIALIZE X3
C
         X(1) = T
         X(2) = S
         X(3) = Z
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
          DANN = A * O3 + B
C
       RETURN
C
       END
C
C ##########################################################################

C***************************************************************************
C
C Name: SALNN
C
C Language: FORTRAN77                         Type - SUBROUTINE
C
C Version: 1.0          Date: 02-10-98        Author: V. Krasnopolsky
C
C ----------------------------------
C
      FUNCTION SALNN(T,DA,Z)
C
C ----------------------------------
C
C Description:   This NN calculates the water salinity S (in psu)
C ------------   given the temperature (T), density (RO), and depth (Z)
C                Bias(i) = 0.00      RMS = 0.10
C                MIN err = -0.3     MAX err(i) = 0.8
C                NN trained for  -2. =< T =< 35. C,
C                -5 kg/m^3 < RO < 58 kg/m^3
C                and 0 =< Z =< 5700 m
C
C***************************************************************************
C
        INTEGER HID
        PARAMETER (IN = 3, HID = 5)
C
C  Arguments:
C  ---------
C  INPUT:
C            T - temperature (in C)
C            DA - seawater density anomaly = density - 1000. (in kg/(m^3))
C            Z - depth (in m)
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
     &    -0.0176163,
     &     0.0269645,
     &    -0.0144142,
     &     0.0239288,
     &     0.0134635,
     &    0.00267583,
     &    0.00956947,
     &    -0.0567171,
     &    0.00293752,
     &     0.0547924,
     &   0.000138711,
     &   4.85006e-05,
     &   0.000242940,
     &   1.13283e-05,
     &  -0.000231146/
       DATA (W2(I),I = 1,HID)
     & /
     &      0.116928,
     &     -0.682048,
     &      -1.42439,
     &      0.499842,
     &       1.35671/
       DATA (B1(I), I=1,HID)
     & /
     &     -0.599732,
     &      0.285202,
     &       2.33110,
     &      -1.15167,
     &     0.0701640/
       DATA B2
     & /
     &      0.963899/
       DATA A
     & /
     &       27.5000/
       DATA B
     & /
     &       21.5000/
C
C - START NEURAL NETWORK
C
C
        X(1) = T
        X(2) = DA
        X(3) = Z
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
C
C ####################################################################
