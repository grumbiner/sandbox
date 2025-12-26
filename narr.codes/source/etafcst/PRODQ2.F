C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      SUBROUTINE PRODQ2(LMHK,DTQ2,USTAR,GM,GH,EL,Q2)
C     ******************************************************************
C     *                                                                *
C     *  LEVEL 2.5 Q2 PRODUCTION/DISSIPATION                           *
C     *                                                                *
C     ******************************************************************
C-----------------------------------------------------------------------
      INCLUDE "parmeta"
# include "sp.h"
C-----------------------------------------------------------------------
                             P A R A M E T E R
     &(LM1=LM-1)
C-----------------------------------------------------------------------
                             P A R A M E T E R
     &(EPSQ2=0.2,EPSL=0.32,EPSTRB=1.E-24,EPS1=1.E-12,EPS2=0.)
C-----------------------------------------------------------------------
                             P A R A M E T E R
C-----------------------------------------------------------------------
     &(G=9.8,BETA=1./270.,BTG=BETA*G
     &,PRT=1.0,GAM1=0.2222222222222222222
     &,A1=0.659888514560862645,A2=0.6574209922667784586
     &,B1=11.87799326209552761,B2=7.226971804046074028
     &,C1=0.000830955950095854396)
C-----------------------------------------------------------------------
CUN     &(G=9.8,BETA=1./270.,BTG=BETA*G
CUN     &,PRT=1.0,GAM1=0.2222222222222222222
CUN     &,A1=0.3310949523016403346,A2=0.8273378704055731278
CUN     &,B1=5.959709141429526024,B2=3.626088092074591135
CUN     &,C1=-0.3330651924968952113)
C-----------------------------------------------------------------------
CMY     &(G=9.8,BETA=1./270.,BTG=BETA*G
CMY     &,PRT=0.8,GAM1=0.2222222222222222222
CMY     &,A1=0.9222222350809054114,A2=0.7350190142719400952
CMY     &,B1=16.60000023145629741,B2=10.10000014082581951
CMY     &,C1=0.0805318118080613468)
C-----------------------------------------------------------------------
                             P A R A M E T E R
     &(RB1=1./B1
C--------------COEFFICIENTS OF THE TERMS IN THE NUMERATOR---------------
     &,ANMM=-3.*A1*A2*(3.*A2+3.*B2*C1+18.*A1*C1-B2)*BTG
     &,ANMH=-9.*A1*A2*A2*BTG*BTG
     &,BNMM=    A1*(1.-3.*C1)
     &,BNMH=   -A2*BTG
C--------------COEFFICIENTS OF THE TERMS IN THE DENOMINATOR-------------
     &,ADNM=18.*A1*A1*A2*(B2-3.*A2)*BTG
     &,ADNH= 9.*A1*A2*A2*(12.*A1+3.*B2)*BTG*BTG
     &,BDNM= 6.*A1*A1
     &,BDNH= 3.*A2*(7.*A1+B2)*BTG
C--------------COEFFICIENTS OF THE EQUILIBRIUM EQUATION-----------------
     &,AEQM= 3.*A1*A2*B1*(3.*A2+3.*B2*C1+18.*A1*C1-B2)*BTG
     &     +18.*A1*A1*A2*(B2-3.*A2)*BTG
     &,AEQH= 9.*A1*A2*A2*B1*BTG*BTG+9.*A1*A2*A2*(12.*A1+3.*B2)*BTG*BTG
     &,BEQM=-A1*B1*(1.-3.*C1)+6.*A1*A1
     &,BEQH= A2*B1*BTG+3.*A2*(7.*A1+B2)*BTG
C--------------FORBIDDEN TURBULENCE AREA--------------------------------
     &,REQU=-AEQH/AEQM*1.02,EPSGH=1.E-9)
C-----------------------------------------------------------------------
                             D I M E N S I O N
     & Q2    (LM)
                             D I M E N S I O N
     & GM    (LM1),GH    (LM1),EL    (LM1)
C-----------------------------------------------------------------------
C***********************************************************************
      LMHM=LMHK-1
C
          DO 150 L=1,LMHM
      GML=GM(L)
      GHL=GH(L)
C--------------COEFFICIENTS OF THE EQUILIBRIUM EQUATION-----------------
      AEQU=(AEQM*GML+AEQH*GHL)*GHL
      BEQU= BEQM*GML+BEQH*GHL
C--------------EQUILIBRIUM SOLUTION FOR L/Q-----------------------------
      EQOL2=-0.5*BEQU+SQRT(BEQU*BEQU*0.25-AEQU)
C--------------IS THERE PRODUCTION/DISSIPATION ?------------------------
          IF((GML+GHL*GHL.LE.EPSTRB           )
     &   .OR.(GHL.GE.EPSGH.AND.GML/GHL.LE.REQU)
     &   .OR.(EQOL2.LE.EPS2)                  )    THEN
C--------------NO TURBULENCE--------------------------------------------
      Q2(L)=EPSQ2
      EL(L)=EPSL
C--------------END OF THE NO TURBULENCE BRANCH--------------------------
          ELSE
C--------------COEFFICIENTS OF THE TERMS IN THE NUMERATOR---------------
      ANUM=(ANMM*GML+ANMH*GHL)*GHL
      BNUM= BNMM*GML+BNMH*GHL
C--------------COEFFICIENTS OF THE TERMS IN THE DENOMINATOR-------------
      ADEN=(ADNM*GML+ADNH*GHL)*GHL
      BDEN= BDNM*GML+BDNH*GHL
      CDEN= 1.
C--------------COEFFICIENTS OF THE NUMERATOR OF THE LINEARIZED EQ.------
      ARHS=-(ANUM*BDEN-BNUM*ADEN)*2.
      BRHS=- ANUM*4.
      CRHS=- BNUM*2.
C--------------INITIAL VALUE OF L/Q-------------------------------------
      DLOQ1=EL(L)/SQRT(Q2(L))
C--------------FIRST ITERATION FOR L/Q, RHS=0---------------------------
      ELOQ21=1./EQOL2
      ELOQ11=SQRT(ELOQ21)
      ELOQ31=ELOQ21*ELOQ11
      ELOQ41=ELOQ21*ELOQ21
      ELOQ51=ELOQ21*ELOQ31
C--------------1./DENOMINATOR-------------------------------------------
      RDEN1=1./(ADEN*ELOQ41+BDEN*ELOQ21+CDEN)
C--------------D(RHS)/D(L/Q)--------------------------------------------
      RHSP1= (ARHS*ELOQ51+BRHS*ELOQ31+CRHS*ELOQ11)*RDEN1*RDEN1
C--------------FIRST-GUESS SOLUTION-------------------------------------   
      ELOQ12=ELOQ11+(DLOQ1-ELOQ11)*EXP(RHSP1*DTQ2)
C-----------------------------------------------------------------------
      ELOQ12=AMAX1(ELOQ12,EPS1)
C--------------SECOND ITERATION FOR L/Q---------------------------------
      ELOQ22=ELOQ12*ELOQ12
      ELOQ32=ELOQ22*ELOQ12
      ELOQ42=ELOQ22*ELOQ22
      ELOQ52=ELOQ22*ELOQ32
C--------------1./DENOMINATOR-------------------------------------------
      RDEN2=1./(ADEN*ELOQ42+BDEN*ELOQ22+CDEN)
C-----------------------------------------------------------------------
      RHS2 =-(ANUM*ELOQ42+BNUM*ELOQ22)*RDEN2+RB1
      RHSP2= (ARHS*ELOQ52+BRHS*ELOQ32+CRHS*ELOQ12)*RDEN2*RDEN2
      RHST2=RHS2/RHSP2
C--------------CORRECTED SOLUTION---------------------------------------   
      ELOQ13=ELOQ12-RHST2+(RHST2+DLOQ1-ELOQ12)*EXP(RHSP2*DTQ2)
C-----------------------------------------------------------------------
      ELOQ13=AMAX1(ELOQ13,EPS1)
C--------------TWO ITERATIONS IS ENOUGH IN MOST CASES ...---------------
      ELOQN=ELOQ13
C-----------------------------------------------------------------------
      IF(ELOQN.GT.EPS1)THEN
        Q2(L)=EL(L)*EL(L)/(ELOQN*ELOQN)
        Q2(L)=AMAX1(Q2(L),EPSQ2)
      ELSE
        Q2(L)=EPSQ2
      ENDIF
C--------------END OF TURBULENT BRANCH----------------------------------
          ENDIF
C--------------END OF PRODUCTION/DISSIPATION LOOP-----------------------
  150 CONTINUE
C--------------LOWER BOUNDARY CONDITION FOR Q2--------------------------
      Q2(LMHK)=AMAX1(B1**(2./3.)*USTAR*USTAR,EPSQ2)
C-----------------------------------------------------------------------
                           RETURN
			   END
