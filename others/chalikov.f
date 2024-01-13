C Dmitry Chalikov author 17 December 1997
C Subroutine QC calculates actual values of salinity based on
C static stability considerations
C Subroute calls equation of state RONN 
C and inverse equation of state SALNN
C Both are based on NN approximation of exact eqs of state
C*********************************T*S*PROFILES*QUALITY*CONTROL*********
C INPUT:
C L - total number of levels (=34 for OTIS)
C LA - actual number of levels 
C Z(L) - array of depths (positive, growing)
C T(L) - array of temperature
C S(L) - array of salinity
C GAP - missing value (for example, GAP=-999.)
C OUTPUT:
C T - returned array of T
C S - returned array of S
C______________________________________________________________________
      subroutine QCTS(L, LA, Z, T, S, GAP)
      real T(L),S(L),Z(L),ZI(L),TI(L),SI(L),R(L)
      real RON(3),SAL(3)
Checkup stability and T_S correction
      do k=LA,2,-1
      RON(1)=T(k-1)
      RON(2)=S(k-1)
      RON(3)=Z(k-1)
      R(k-1)=RONN(RON)
      RON(1)=T(k)
      RON(2)=S(k)
      RON(3)=Z(k)
      R(k)=RONN(RON)
      if (R(k).lt.R(k-1)) then
        R(k-1)=R(k)
        SAL(1)=T(k-1)
        SAL(2)=R(k-1)
        SAL(3)=Z(k-1) 
        S(k-1)=SALNN(SAL)
CD        NPC=NPC+1
      endif
      enddo !k
C      enddo !it
      return
      end
       
C********************************PRECISE*STATE*EQUATION****************
      function RO(T,S,Z) 
      G=9.8
      A=999.842594
     &      +6.793952e-2*T
     &      -9.09529e-3*T**2
     &      +1.001685e-4*T**3
     &      -1.120083e-6*T**4
     &      +6.56332e-9*T**5
      B=     8.24493e-1-4.0899e-3*T+7.6438e-5*T**2
     &      -8.2467e-7*T**3+5.3875e-9*T**4  
      C=-5.72466e-3+1.0227e-4*T-1.6546e-6*T**3
      D=4.8314e-4
      E=19652.21+148.4206*T-2.327105*T**2+1.360477e-2*T**3
     &-5.155288e-5*T**4 
      F=54.6746-.603459*T+1.09987e-2*T**2-6.167e-5*T**3           
      G=7.944e-2+1.6483e-2*T-5.3009e-4*T**2
      H=3.239908+1.43713e-3*T+1.16092e-4*T**2-5.77905e-7*T**3
      I=2.2838e-3-1.0981e-5*T-1.6078e-6*T**2
      J=1.91075e-4
      M=8.50935e-5-6.12293e-6*T+5.2787e-8*T**2
      N=-9.9348e-7+2.0816e-8*T+9.1697e-10*T**2
      R0=A+B*S+C*S**1.5+D*S**2
      P=1.e-5*R0*G*Z
      RK=E+F*S+G*S**1.5+(H+I*S+J*S**1.5)*P
      RO=R0/(1.-P/RK)-1020.
      return
      end
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
C
C****************************************STATE*EQUATION*EXACT**********
C EXACT EQUATION OF STATE
      function ROE(T,S,Z) 
      G=9.8
      R00=1020.
      A=999.842594
     &      +6.793952e-2*T
     &      -9.09529e-3*T**2
     &      +1.001685e-4*T**3
     &      -1.120083e-6*T**4
     &      +6.56332e-9*T**5
      B=     8.24493e-1-4.0899e-3*T+7.6438e-5*T**2
     &      -8.2467e-7*T**3+5.3875e-9*T**4  
      C=-5.72466e-3+1.0227e-4*T-1.6546e-6*T**3
      D=4.8314e-4
      E=19652.21+148.4206*T-2.327105*T**2+1.360477e-2*T**3
     &-5.155288e-5*T**4 
      F=54.6746-.603459*T+1.09987e-2*T**2-6.167e-5*T**3           
      G=7.944e-2+1.6483e-2*T-5.3009e-4*T**2
      H=3.239908+1.43713e-3*T+1.16092e-4*T**2-5.77905e-7*T**3
      I=2.2838e-3-1.0981e-5*T-1.6078e-6*T**2
      J=1.91075e-4
      M=8.50935e-5-6.12293e-6*T+5.2787e-8*T**2
      N=-9.9348e-7+2.0816e-8*T+9.1697e-10*T**2
      R0=A+B*S+C*S**1.5+D*S**2
      P=1.e-5*R0*G*Z
      RK=E+F*S+G*S**1.5+(H+I*S+J*S**1.5)*P
      ROE=R0/(1.-P/RK)-R00
      return
      end



