C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      SUBROUTINE MIXLEN(LMHK,LPBL,HPBL,U,V,T,Q,Q2,APE,Z,GM,GH,EL)
C     ******************************************************************
C     *                                                                *
C     *                   LEVEL 2.5 MIXING LENGTH                      *
C     *                                                                *
C     ******************************************************************
c
c     July 1997: Modified to restore averaging of layer values of l, 
c     ELL, a la Mesinger 1993, Res. Act. Atmos. Ocean. Mod., No. 18,
c     4.36-4.38;
c
c     A problem removed which may have led to the Òabove PBLÓ scheme
c     for preliminary EL to be used inadvertently at times and places
c     where the PBL scheme was supposed to have been used
c
C-----------------------------------------------------------------------
      INCLUDE "parmeta"
      INCLUDE "mpp.h"
#include "sp.h"
C-----------------------------------------------------------------------
                             P A R A M E T E R
     &(LP1=LM+1,LM1=LM-1)
C-----------------------------------------------------------------------
                             P A R A M E T E R
     &(EPSQ2=0.2,EPSL=0.32,EPSRU=1.E-12,EPSRS=1.E-12
     &,FH=1.01,ALPH=.20,VKRM=.40,ELFC=0.23    ,EL0MAX=1000.)
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
C--------------COEFFICIENTS OF THE TERMS IN THE DENOMINATOR-------------
                             P A R A M E T E R
     &(ADNM=18.*A1*A1*A2*(B2-3.*A2)*BTG
     &,ADNH= 9.*A1*A2*A2*(12.*A1+3.*B2)*BTG*BTG
     &,BDNM= 6.*A1*A1
     &,BDNH= 3.*A2*(7.*A1+B2)*BTG
C--------------FREE TERM IN THE EQUILIBRIUM EQUATION FOR (L/Q)**2-------
     &,AEQM= 3.*A1*A2*B1*(3.*A2+3.*B2*C1+18.*A1*C1-B2)*BTG
     &     +18.*A1*A1*A2*(B2-3.*A2)*BTG
     &,AEQH= 9.*A1*A2*A2*B1*BTG*BTG+9.*A1*A2*A2*(12.*A1+3.*B2)*BTG*BTG
C--------------FORBIDDEN TURBULENCE AREA--------------------------------
     &,REQU=-AEQH/AEQM*1.02,EPSGH=1.E-9,EPSGM=REQU*EPSGH
C--------------NEAR ISOTROPY FOR SHEAR TURBULENCE, WW/Q2 LOWER LIMIT----
     &,UBRYL=(18.*REQU*A1*A1*A2*B2*C1*BTG+9.*A1*A2*A2*B2*BTG*BTG)
     &      /(REQU*ADNM+ADNH)
     &,UBRY=(1.+EPSRS)*UBRYL
     &,UBRY3=3.*UBRY
     &,AUBM=54.*A1*A1*A2*B2*C1*BTG -ADNM*UBRY3
     &,AUBH=27.*A1*A2*A2*B2*BTG*BTG-ADNH*UBRY3
     &,BUBM=18.*A1*A1*C1           -BDNM*UBRY3
     &,BUBH=(9.*A1*A2+3.*A2*B2)*BTG-BDNH*UBRY3
     &,CUBR=1.                     -     UBRY3,RCUBR=1./CUBR)
C-----------------------------------------------------------------------
                             D I M E N S I O N
     & U     (LM),V     (LM)
     &,T     (LM),Q     (LM),Q2    (LM)
                             D I M E N S I O N
     & GM    (LM1),GH    (LM1),EL    (LM1)
     &,APE   (LM )
     &,Z     (LP1)
C-----------------------------------------------------------------------
                             D I M E N S I O N
     & ELM   (LM1),ELL   (LM )
     &,Q1    (LM ),THV   (LM )
C-----------------------------------------------------------------------
C***********************************************************************
      LMHM=LMHK-1
      LMHP=LMHK+1
C--------------FIND THE HEIGHT OF THE PBL-------------------------------
      LPBL=LMHK
          DO 100 IVI=1,LMHK
      L=LMHK-IVI
      IF(Q2(L).LE.EPSQ2*FH)THEN
CVVVVVVVVVVVVVV NOT NECESSARY IF DRIVEN BY TURBL VVVVVVVVVVVVVVVVVVVVVVV
C        Q2(L)=EPSQ2
CAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
        LPBL=L
        GO TO 110
      ENDIF
 100  CONTINUE
      LPBL=1
C--------------THE HEIGHT OF THE PBL------------------------------------
 110  HPBL=Z(LPBL)-Z(LMHP)
C-----------------------------------------------------------------------
         DO 120 L=1,LMHK
      Q1(L)=0.
      THV(L)=(0.608*Q(L)+1.)*T(L)*APE(L)
 120  CONTINUE
C-----------------------------------------------------------------------
          DO 130 L=LPBL,LMHK
      Q1(L)=SQRT(Q2(L))
 130  CONTINUE
C-----------------------------------------------------------------------
      SZQ=0.
      SQ =0.
          DO 140 L=1,LMHM
      QDZL=(Q1(L)+Q1(L+1))*(Z(L+1)-Z(L+2))
      SZQ=(Z(L+1)+Z(L+2)-Z(LMHP)-Z(LMHP))*QDZL+SZQ
      SQ=QDZL+SQ
      RDZ=2./(Z(L)-Z(L+2))
      GML=((U(L)-U(L+1))**2+(V(L)-V(L+1))**2)*RDZ*RDZ
      GM(L)=AMAX1(GML,EPSGM)
      GHL=(THV(L)-THV(L+1))*RDZ
      IF(ABS(GHL).LE.EPSGH)    GHL=EPSGH
      GH(L)=GHL
 140  CONTINUE
C--------------COMPUTATION OF ASYMPTOTIC L IN BLACKADAR FORMULA---------
      EL0=AMIN1(ALPH*SZQ*0.5/SQ,EL0MAX)
C-----------------------------------------------------------------------
          DO 150 L=1,LMHM
      GML=GM(L)
      GHL=GH(L)
      IF(GHL.GE.EPSGH)THEN
        IF(GML/GHL.LE.REQU)THEN
          ELM(L)=EPSL
      ELSE
          AUBR=(AUBM*GML+AUBH*GHL)*GHL
          BUBR= BUBM*GML+BUBH*GHL
          QOL2ST=(-0.5*BUBR+SQRT(BUBR*BUBR*0.25-AUBR*CUBR))*RCUBR
          QOL2ST=AMAX1(QOL2ST,1.E-8)
          ELOQ2X=1./QOL2ST
          ELM(L)=AMAX1(SQRT(ELOQ2X*Q2(L)),EPSL)
        ENDIF
      ELSE
        ADEN=(ADNM*GML+ADNH*GHL)*GHL
        BDEN= BDNM*GML+BDNH*GHL
        QOL2UN= -0.5*BDEN+SQRT(BDEN*BDEN*0.25-ADEN)
        ELOQ2X=1./((1.+EPSRU)*QOL2UN)
        ELM(L)=AMAX1(SQRT(ELOQ2X*Q2(L)),EPSL)
      ENDIF
 150  CONTINUE
C-----------------------------------------------------------------------
c     LPBLM=LPBL-1
c         DO 160 L=1,LPBLM
c     EL(L)=AMIN1((Z(L)-Z(L+2))*ELFC,ELM(L))
c160  CONTINUE
c         DO 165 L=LPBL,LMHM
c     VKRMZ=(Z(L+1)-Z(LMHP))*VKRM
c165  EL(L)=AMIN1(VKRMZ/(VKRMZ/EL0+1.),ELM(L))
C-----------------------------------------------------------------------
c     Note: LPBL is the EL value of L of the interface starting with the
c     lowest interface and going up for the first time having Q2 .le.
c     a specified value
c
          DO 160 L=1,LPBL
      ELL(L)=     (Z(L)-Z(L+1))*ELFC
 160      CONTINUE    
          IF(LPBL.LT.LMHK)THEN
      LPBLP=LPBL+1
          DO 165 L=LPBLP,LMHK
      VKRMZ=(0.5*(Z(L)+Z(L+1))-Z(LMHP))*VKRM
      ELL(L)=     VKRMZ/(VKRMZ/EL0+1.)
 165      CONTINUE
          ENDIF
C-----------------------------------------------------------------------
          DO 260 L=1,LMHM
      EL(L)=AMIN1(0.5*(ELL(L)+ELL(L+1)),ELM(L))
 260      CONTINUE
C-----------------------------------------------------------------------
                             RETURN
                             END
