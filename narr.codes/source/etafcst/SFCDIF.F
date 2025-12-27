C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      SUBROUTINE SFCDIF(LMHK,SM,THS,QS,UZ0,VZ0,THZ0,QZ0
     &,                 USTAR,WSTAR,Z0,ZEFF,AKMS,AKHS,HPBL,CT
     &,                 U10,V10,TH02,TH10,Q02,Q10
     &,                 TH30,Q30,U30,V30
     &,                 ULM,VLM,T,Q,APE,Z,PD,PT,TLM)
C     ******************************************************************
C     *                                                                *
C     *                        SURFACE LAYER                           *
C     *                                                                *
C     ******************************************************************
C     Ammended to use the "Effective roughness" of Mason (1986, see
C     Georgelin et al., MWR July 1994), by FM, RW, June 1995
C-----------------------------------------------------------------------
      INCLUDE "parmeta"
      INCLUDE "mpp.h"
#include "sp.h"
C-----------------------------------------------------------------------
                             P A R A M E T E R
     &(LP1=LM+1)
C-----------------------------------------------------------------------
                             P A R A M E T E R
     &(WWST=1.2,WWST2=WWST*WWST,G=9.8,USTFC=0.018/G
     &,VKRM=0.40,RIC=0.183,RFC=0.191,FHNEU=0.8
     &,RRIC=1.0/RIC,RFAC=RIC/(FHNEU*RFC*RFC),EXCM=0.001
     &,BETA=1./270.,BTG=BETA*G
     &,ELFC=VKRM*BTG,CNV=0.608*G/BTG
     &,WOLD=.15,WNEW=1.-WOLD,ITRMX=05
     &,PIHF=3.14159265/2.,PIFR=3.14159265/4.
C-----------------------------------------------------------------------
     &,EPSU2=1.E-4,EPSUST=0.07,EPSIT=1.E-4,EPSA=1.E-8
     &,ZTMIN=-5.,ZTMAX=1.
C-----------------------------------------------------------------------
     &,SMALL=0.35, GLKBS=30.0,GLKBR=10.0,GRRS=GLKBR/GLKBS
     &,CZIV=SMALL*GLKBS
     &,VISC=1.5E-5, TVISC=2.1E-5, QVISC=2.1E-5
     &,RVISC=1./VISC,RTVISC=1./TVISC,RQVISC=1./QVISC
     &,SQPR=0.84,SQSC=0.84,ZQRZT=SQSC/SQPR
     &,USTR=0.225,USTC=0.7
     &,FZU1=CZIV*VISC,FZT1=RVISC *TVISC*SQPR,   FZQ1=RTVISC*QVISC*ZQRZT
     &,               FZT2=CZIV*GRRS*TVISC*SQPR,FZQ2=RTVISC*QVISC*ZQRZT
C-----------------------------------------------------------------------
     &,ZTFC=1.0
c     &,CZIL=.1000,SQVISC=258.2,ZILFC=-CZIL*VKRM*SQVISC
     &,CZIL=.2000,SQVISC=258.2,ZILFC=-CZIL*VKRM*SQVISC
     &,PQ0=379.90516,A2=17.2693882,A3=273.16,A4=35.86
     &,CAPA=0.28589641E0,H1M5=1.E-5)
C-----------------------------------------------------------------------
                             D I M E N S I O N
     & T     (LM),Q     (LM)
                             D I M E N S I O N
     & APE   (LM)
     &,Z     (LP1)
C-ZEFF-ZEFF-ZEFF-ZEFF
     &,ZEFF  (4)
C-ZEFF-ZEFF-ZEFF-ZEFF
C-----------------------------------------------------------------------
      PSLMU(ZZ)=-0.96*ALOG(1.0-4.5*ZZ)
      PSLMS(ZZ)=ZZ*RRIC-2.076*(1.-1./(ZZ+1.))
      PSLHU(ZZ)=-0.96*ALOG(1.0-4.5*ZZ)
      PSLHS(ZZ)=ZZ*RFAC-2.076*(1.-1./(ZZ+1.))
C
      PSPMU(XX)=-2.*ALOG((XX+1.)*0.5)-ALOG((XX*XX+1.)*0.5)+2.*ATAN(XX)
     &          -PIHF
      PSPMS(YY)=5.*YY
      PSPHU(XX)=-2.*ALOG((XX*XX+1.)*0.5)
      PSPHS(YY)=5.*YY
C***********************************************************************
      LMHP=LMHK+1
C
      THLM=T(LMHK)*APE(LMHK)
      QLM=Q(LMHK)
C-----------------------------------------------------------------------
      Z0=(1.-SM)*Z0+SM*AMAX1(USTFC*USTAR*USTAR,1.59E-5)
C--------------VISCOUS SUBLAYER-----------------------------------------
      IF(SM.GT.0.5.AND.USTAR.LT.USTC)THEN
C-----------------------------------------------------------------------
        IF(USTAR.LT.USTR)THEN
C
          ZU=FZU1*SQRT(SQRT(Z0*USTAR*RVISC))/USTAR
          WGHT=AKMS*ZU*RVISC
          RWGH=WGHT/(WGHT+1.)
          UZ0=(ULM*RWGH+UZ0)*0.5
          VZ0=(VLM*RWGH+VZ0)*0.5
C
          ZT=FZT1*ZU
          WGHT=AKHS*ZT*RTVISC
          THZ0=((WGHT*THLM+THS)/(WGHT+1.)+THZ0)*0.5
C
          ZQ=FZQ1*ZT
          WGHT=AKHS*ZQ*RQVISC
          QZ0 =((WGHT*QLM+QS)/(WGHT+1.)+QZ0)*0.5
C
        ENDIF
        IF(USTAR.GE.USTR.AND.USTAR.LT.USTC)THEN
C
          ZU=Z0
          UZ0=0.
          VZ0=0.
C
          ZT=FZT2*SQRT(SQRT(Z0*USTAR*RVISC))/USTAR
          WGHT=AKHS*ZT*RTVISC
          THZ0=((WGHT*THLM+THS)/(WGHT+1.)+THZ0)*0.5
C
          ZQ=FZQ2*ZT
          WGHT=AKHS*ZQ*RQVISC
          QZ0 =((WGHT*QLM+QS)/(WGHT+1.)+QZ0)*0.5
        ENDIF
C-----------------------------------------------------------------------
      ELSE
C-----------------------------------------------------------------------
        ZU=Z0
C-ZEFF-ZEFF-ZEFF-ZEFF
              IF(SM.LE.0.5)THEN
         IF(ULM.EQ.0.) ULM=EPSU2
         ALPHA=ABS(ATAN(VLM/ULM)+PIHF-EPSA)
         X=ALPHA/PIFR
         ML=1+X
         MH=1+MOD(ML,4)
         WLOW=X-ML+1
         ZU=WLOW*ZEFF(ML)+(1.-WLOW)*ZEFF(MH)
              ENDIF
C-ZEFF-ZEFF-ZEFF-ZEFF
        UZ0=0.
        VZ0=0.
C
        ZT=Z0
        THZ0=THS
C
        ZQ=Z0
        QZ0=QS
C-----------------------------------------------------------------------
      ENDIF
C-----------------------------------------------------------------------
      ZSL=(Z(LMHK)-Z(LMHP))*0.5
C-ZEFF-ZEFF-ZEFF-ZEFF
            ZU=AMIN1(ZU,0.5*ZSL)
C-ZEFF-ZEFF-ZEFF-ZEFF
      RDZ=1./ZSL
      CXCH=EXCM*RDZ
C-----------------------------------------------------------------------
      IF(SM.GT.0.5)THEN
        DTHV=(0.608*QLM+1.)*THLM-(0.608*QZ0+1.)*THZ0
      ELSE
        DTHV=(QLM-QZ0)*CNV+THLM-THZ0
        ZT=Z0*ZTFC
      ENDIF
C
      DU2=AMAX1((ULM-UZ0)**2+(VLM-VZ0)**2,EPSU2)
C-----------------------------------------------------------------------
      RIB=BTG*DTHV*ZSL/DU2
C--------------BELJARS CORRECTION OF USTAR------------------------------
      BTGH=BTG*HPBL
      WSTAR2=WWST2*ABS(BTGH*AKHS*DTHV)**(2./3.)
      USTAR=AMAX1(SQRT(AKMS*SQRT(DU2+WSTAR2)),EPSUST)
C--------------ZILITINKEVITCH FIX FOR ZT--------------------------------
      IF(SM.LT.0.5)ZT=EXP(ZILFC*SQRT(USTAR*Z0))*Z0
C-----------------------------------------------------------------------
      IF(SM.GT.0.5.AND.RIB.GE.RIC)THEN
C-----------------------------------------------------------------------
        AKMS=AMAX1( VISC*RDZ,CXCH)
        AKHS=AMAX1(TVISC*RDZ,CXCH)
C-----------------------------------------------------------------------
      ELSE
C-----------------------------------------------------------------------
        ZSLU=ZSL+ZU
        ZSLT=ZSL+ZT
C
        RLOGU=ALOG(ZSLU/ZU)
        RLOGT=ALOG(ZSLT/ZT)
C
        RLMO=ELFC*AKHS*DTHV/USTAR**3
C--------------SEA POINTS FIRST ... ------------------------------------
        IF(SM.GT.0.5)THEN
              DO 100 ITR=1,ITRMX
C--------------1./MONIN-OBUKKHOV LENGTH-SCALE---------------------------
          ZETALT=AMAX1(ZSLT*RLMO,ZTMIN)
          RLMO=ZETALT/ZSLT
          ZETALU=ZSLU*RLMO
C
          ZETAU=ZU*RLMO
          ZETAT=ZT*RLMO
C--------------LL FUNCTIONS OVER SEA------------------------------------
          IF(RLMO.LT.0.)THEN
            PSMZ=PSLMU(ZETAU)
            SIMM=       PSLMU(ZETALU)-PSMZ+RLOGU
            PSHZ=PSLHU(ZETAT)
            SIMH=FHNEU*(PSLHU(ZETALT)-PSHZ+RLOGT)
          ELSE
            PSMZ=PSLMS(ZETAU)
            SIMM=       PSLMS(ZETALU)-PSMZ+RLOGU
            PSHZ=PSLHS(ZETAT)
            SIMH=FHNEU*(PSLHS(ZETALT)-PSHZ+RLOGT)
          ENDIF
C--------------BELJAARS CORRECTION FOR USTAR----------------------------
          USTAR=AMAX1(SQRT(AKMS*SQRT(DU2+WSTAR2)),EPSUST)
C-----------------------------------------------------------------------
          USTARK=USTAR*VKRM
          AKMS=AMAX1(USTARK/SIMM,CXCH)
          AKHS=AMAX1(USTARK/SIMH,CXCH)
C-----------------------------------------------------------------------
          WSTAR2=WWST2*ABS(BTGH*AKHS*DTHV)**(2./3.)
          RLMN=ELFC*AKHS*DTHV/USTAR**3
C-----------------------------------------------------------------------
          RLMP=RLMO
          RLMA=RLMO*WOLD+RLMN*WNEW
C-----------------------------------------------------------------------
C          IF(ABS((RLMN-RLMO)/RLMA).LT.EPSIT)    GO TO 110
C-----------------------------------------------------------------------
          RLMO=RLMA
C-----------------------------------------------------------------------
  100     CONTINUE
C-----------------------------------------------------------------------
  110     CONTINUE
C--------------END OF SEA POINT PROCESSING------------------------------
        ELSE
C--------------NOW LAND POINTS ...--------------------------------------
              DO 200 ITR=1,ITRMX
C--------------1./MONIN-OBUKKHOV LENGTH-SCALE---------------------------
          ZETALT=AMAX1(ZSLT*RLMO,ZTMIN)
          RLMO=ZETALT/ZSLT
          ZETALU=ZSLU*RLMO
C
          ZETAU=ZU*RLMO
          ZETAT=ZT*RLMO
C--------------PAULSON 1970 FUNCTIONS OVER LAND W RAD. SKIN T-----------
          IF(RLMO.LT.0.)THEN
            XLU4=1.-16.*ZETALU
            XLT4=1.-16.*ZETALT
            XU4 =1.-16.*ZETAU
            XT4 =1.-16.*ZETAT
C
            XLU=SQRT(SQRT(XLU4))
            XLT=SQRT(SQRT(XLT4))
            XU =SQRT(SQRT(XU4))
            XT =SQRT(SQRT(XT4))
C
            PSMZ=PSPMU(XU)
            SIMM=PSPMU(XLU)-PSMZ+RLOGU
            PSHZ=PSPHU(XT)
            SIMH=PSPHU(XLT)-PSHZ+RLOGT
          ELSE
            ZETAU=AMIN1(ZETAU,ZTMAX)
            ZETAT=AMIN1(ZETAT,ZTMAX)
            ZETALU=AMIN1(ZETALU,ZTMAX)
            ZETALT=AMIN1(ZETALT,ZTMAX)
            PSMZ=PSPMS(ZETAU)
            SIMM=PSPMS(ZETALU)-PSMZ+RLOGU
            PSHZ=PSPHS(ZETAT)
            SIMH=PSPHS(ZETALT)-PSHZ+RLOGT
          ENDIF
C--------------BELJAARS CORRECTION FOR USTAR----------------------------
          USTAR=AMAX1(SQRT(AKMS*SQRT(DU2+WSTAR2)),EPSUST)
C--------------ZILITINKEVITCH FIX FOR ZT--------------------------------
          ZT=EXP(ZILFC*SQRT(USTAR*Z0))*Z0
          ZSLT=ZSL+ZT
          RLOGT=ALOG(ZSLT/ZT)
C-----------------------------------------------------------------------
          USTARK=USTAR*VKRM
          AKMS=AMAX1(USTARK/SIMM,CXCH)
          AKHS=AMAX1(USTARK/SIMH,CXCH)
C-----------------------------------------------------------------------
          WSTAR2=WWST2*ABS(BTGH*AKHS*DTHV)**(2./3.)
          RLMN=ELFC*AKHS*DTHV/USTAR**3
C-----------------------------------------------------------------------
          RLMP=RLMO
          RLMA=RLMO*WOLD+RLMN*WNEW
C-----------------------------------------------------------------------
C          IF(ABS((RLMN-RLMO)/RLMA).LT.EPSIT)    GO TO 210
C-----------------------------------------------------------------------
          RLMO=RLMA
C-----------------------------------------------------------------------
  200     CONTINUE
C-----------------------------------------------------------------------
  210     CONTINUE
C--------------END OF LAND POINT PROCESSING AND SEA-LAND BRANCHING------
        ENDIF
C--------------END OF TURBULENCE-NO TURBULENCE BRANCHING----------------
      ENDIF
C--------------COUNTERGRADIENT FIX--------------------------------------
C      HV=-AKHS*DTHV
C      IF(HV.GT.0.)THEN
C        FCT=-10.*(BTG)**(-1./3.)
C        CT=FCT*(HV/(HPBL*HPBL))**(2./3.)
C      ELSE
        CT=0.
C      ENDIF
C--------------DIAGNOSTIC BLOCK-----------------------------------------
      WSTAR=SQRT(WSTAR2)/WWST
C
      UMFLX=AKMS*(ULM -UZ0 )
      VMFLX=AKMS*(VLM -VZ0 )
      HSFLX=AKHS*(THLM-THZ0)
      HLFLX=AKHS*(QLM -QZ0 )
C-----------------------------------------------------------------------
      IF(SM.GT.0.5.AND.RIB.GE.RIC)THEN
C-----------------------------------------------------------------------
        AKMS10=AMAX1( VISC/10.,CXCH)
        AKMS30=AMAX1( VISC/30.,CXCH)
        AKHS02=AMAX1(TVISC/02.,CXCH)
        AKHS10=AMAX1(TVISC/10.,CXCH)
        AKHS30=AMAX1(TVISC/30.,CXCH)
C-----------------------------------------------------------------------
      ELSE
C-----------------------------------------------------------------------
        ZU10=ZU+10.
        ZU30=ZU+30.
        ZT02=ZT+02.
        ZT10=ZT+10.
        ZT30=ZT+30.
C
        RLNU10=ALOG(ZU10/ZU)
        RLNU30=ALOG(ZU30/ZU)
        RLNT02=ALOG(ZT02/ZT)
        RLNT10=ALOG(ZT10/ZT)
        RLNT30=ALOG(ZT30/ZT)
C
        ZTAU10=ZU10*RLMP
        ZTAU30=ZU30*RLMP
        ZTAT02=ZT02*RLMP
        ZTAT10=ZT10*RLMP
        ZTAT30=ZT30*RLMP
C--------------LL FUNCTIONS OVER SEA------------------------------------
        IF(SM.GT.0.5)THEN
C-----------------------------------------------------------------------
          IF(RLMP.LT.0.)THEN
            SIMM10=       PSLMU(ZTAU10)-PSMZ+RLNU10
            SIMM30=       PSLMU(ZTAU30)-PSMZ+RLNU30
            SIMH02=FHNEU*(PSLHU(ZTAT02)-PSHZ+RLNT02)
            SIMH10=FHNEU*(PSLHU(ZTAT10)-PSHZ+RLNT10)
            SIMH30=FHNEU*(PSLHU(ZTAT30)-PSHZ+RLNT30)
          ELSE
            SIMM10=       PSLMS(ZTAU10)-PSMZ+RLNU10
            SIMM30=       PSLMS(ZTAU30)-PSMZ+RLNU30
            SIMH02=FHNEU*(PSLHS(ZTAT02)-PSHZ+RLNT02)
            SIMH10=FHNEU*(PSLHS(ZTAT10)-PSHZ+RLNT10)
            SIMH30=FHNEU*(PSLHS(ZTAT30)-PSHZ+RLNT30)
          ENDIF
C--------------PAULSON 1970 FUNCTIONS OVER LAND W RAD. SKIN T-----------
        ELSE
C-----------------------------------------------------------------------
          IF(RLMP.LT.0.)THEN
            XLU104=1.-16.*ZTAU10
            XLU304=1.-16.*ZTAU30
            XLT024=1.-16.*ZTAT02
            XLT104=1.-16.*ZTAT10
            XLT304=1.-16.*ZTAT30
C
            XLU10=SQRT(SQRT(XLU104))
            XLU30=SQRT(SQRT(XLU304))
            XLT02=SQRT(SQRT(XLT024))
            XLT10=SQRT(SQRT(XLT104))
            XLT30=SQRT(SQRT(XLT304))
C
            SIMM10=PSPMU(XLU10)-PSMZ+RLNU10
            SIMM30=PSPMU(XLU30)-PSMZ+RLNU30
            SIMH02=PSPHU(XLT02)-PSHZ+RLNT02
            SIMH10=PSPHU(XLT10)-PSHZ+RLNT10
            SIMH30=PSPHU(XLT30)-PSHZ+RLNT30
          ELSE
            ZTAU10=AMIN1(ZTAU10,ZTMAX)
            ZTAU30=AMIN1(ZTAU30,ZTMAX)
            ZTAT02=AMIN1(ZTAT02,ZTMAX)
            ZTAT10=AMIN1(ZTAT10,ZTMAX)
            ZTAT30=AMIN1(ZTAT30,ZTMAX)
C
            SIMM10=PSPMS(ZTAU10)-PSMZ+RLNU10
            SIMM30=PSPMS(ZTAU30)-PSMZ+RLNU30
            SIMH02=PSPHS(ZTAT02)-PSHZ+RLNT02
            SIMH10=PSPHS(ZTAT10)-PSHZ+RLNT10
            SIMH30=PSPHS(ZTAT30)-PSHZ+RLNT30
          ENDIF
C-----------------------------------------------------------------------
        ENDIF
C-----------------------------------------------------------------------
        AKMS10=AMAX1(USTARK/SIMM10,CXCH)
        AKMS30=AMAX1(USTARK/SIMM30,CXCH)
        AKHS02=AMAX1(USTARK/SIMH02,CXCH)
        AKHS10=AMAX1(USTARK/SIMH10,CXCH)
        AKHS30=AMAX1(USTARK/SIMH30,CXCH)
C-----------------------------------------------------------------------
      ENDIF
C-----------------------------------------------------------------------
      U10 =UMFLX/AKMS10+UZ0
      V10 =VMFLX/AKMS10+VZ0
      U30 =UMFLX/AKMS30+UZ0
      V30 =VMFLX/AKMS30+VZ0
      TH02=HSFLX/AKHS02+THZ0
      TH10=HSFLX/AKHS10+THZ0
      TH30=HSFLX/AKHS30+THZ0

C GSM  changed this section in response to problem with 2-m
C     dew point occasionally being greater than 2-m temperature
C     and similar problem at 10-m.   Now, a saturation Q is
C     calculated at each level, and the Q is constrained to
C     be no higher than the saturation value. 

      PDS=PD+PT
      TERM1=-0.068283/TLM
      PSHLTR=PDS*EXP(TERM1)
      T02=TH02*(PSHLTR*H1M5)**CAPA
      QSAT2 = PQ0/PSHLTR*EXP(A2*(T02-A3)/(T02-A4))
      Q02 =HLFLX/AKHS02+QZ0
      IF (Q02.LT.0.) THEN
        IF (QLM .GT. 0.) THEN
          Q02=QLM
        ELSE
          Q02=0.0001
        ENDIF
      ENDIF
      IF (Q02.GT.QSAT2)THEN
        Q02 = QSAT2
      ENDIF

      T10=TH10*(PSHLTR*H1M5)**CAPA
      QSAT10 = PQ0/PSHLTR*EXP(A2*(T10-A3)/(T10-A4))
C Corrected to use pressure at 10m. (DJ)
      P10=PDS*EXP(-10.0*G/(287.04*TLM))
      T10=TH10*(P10*H1M5)**CAPA
      QSAT10 = PQ0/P10*EXP(A2*(T10-A3)/(T10-A4))
C
      Q10 =HLFLX/AKHS10+QZ0
      IF (Q10.LT.0.) THEN
        IF (QLM .GT. 0.) THEN
          Q10=QLM
        ELSE
          Q10=0.0001
        ENDIF
      ENDIF
      IF (Q10.GT.QSAT10)THEN
        Q10 = QSAT10
      ENDIF

      P30=PDS*EXP(-30.0*G/(287.04*TLM))
      T30=TH30*(P30*H1M5)**CAPA
      QSAT30 = PQ0/P30*EXP(A2*(T30-A3)/(T30-A4))
      Q30 =HLFLX/AKHS30+QZ0
      IF (Q30.LT.0.) THEN
        IF (QLM .GT. 0.) THEN
          Q30=QLM
        ELSE
          Q30=0.0001
        ENDIF
      ENDIF
      IF (Q30.GT.QSAT30)THEN
        Q30 = QSAT30
      ENDIF

c new calculation of 10-m winds
C-----------------------------------------------------
      U10E=U10
      V10E=V10
      U30E=U30
      V30E=V30
C-----------------------------------------------------
      IF(SM.LT.0.5)  THEN
c choose the equivalent z0 here:
czj        ZU=0.01
ckm     zu=zu*0.1
C
        zuuz=amin1(zu*0.50,0.10)
        zu=amax1(zu*0.10,zuuz)
        ZU10=ZU+10.
        ZU30=ZU+30.
        RLNU10=ALOG(ZU10/ZU)
        RLNU30=ALOG(ZU30/ZU)
        ZTAU=ZU*RLMP
        ZTAU10=ZU10*RLMP
        ZTAU30=ZU30*RLMP
c--------------------------------------------------------
        IF(RLMP.LT.0)THEN
          XLU104=1.-16.*ZTAU10
          XLU304=1.-16.*ZTAU30
          XU104 =1.-16.*ZTAU
          XLU10=SQRT(SQRT(XLU104))
          XLU30=SQRT(SQRT(XLU304))
          XU10 =SQRT(SQRT(XU104))
          SIMM10=PSPMU(XLU10)-PSPMU(XU10)+RLNU10
          SIMM30=PSPMU(XLU30)-PSPMU(XU10)+RLNU30
        ELSE
          ZTAU10=AMIN1(ZTAU10,ZTMAX)
          ZTAU30=AMIN1(ZTAU30,ZTMAX)
          SIMM10=PSPMS(ZTAU10)-PSPMS(ZTAU)+RLNU10
          SIMM30=PSPMS(ZTAU30)-PSPMS(ZTAU)+RLNU30
        ENDIF
c-----------------------------------------------------
        EKMS10=AMAX1(USTARK/SIMM10,CXCH)
        EKMS30=AMAX1(USTARK/SIMM30,CXCH)
        U10E=UMFLX/EKMS10+UZ0
        V10E=VMFLX/EKMS10+VZ0
        U30E=UMFLX/EKMS30+UZ0
        V30E=VMFLX/EKMS30+VZ0
      ENDIF
c----------------------------------------------------------------
      U10=U10E
      V10=V10E
      U30=U30E
      V30=V30E
C
C-----------------------------------------------------------------------
                           RETURN
                           END
