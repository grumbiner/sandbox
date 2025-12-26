C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
                             SUBROUTINE RADTN
C     ******************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .     
C SUBPROGRAM:    RADTN       THE OUTER RADIATION DRIVER
C   PRGRMMR: BLACK           ORG: W/NP22     DATE: 93-12-??
C     
C ABSTRACT:
C     RADTN PRIMARILY SERVES TO SET UP THE ARRAYS NEEDED AS INPUT
C     FOR RADFS (THE INNER RADIATION DRIVER).  GROUPS OF MODEL COLUMNS
C     ARE SENT TO RADFS BUT FIRST THEY ARE "LIFTED" SO THAT THE LOWEST
C     LAYER ABOVE THE GROUND HAS A VERTICAL INDEX VALUE OF LM NOT LMH.
C     THIS ROUTINE IS CALLED AS OFTEN AS DESIRED (EVERY 1 TO 2 HOURS)
C     FOR BOTH THE SHORT AND LONGWAVE EFFECTS.  THE RESULTING TEMPER-
C     ATURE TENDENCIES, TOTAL DOWNWARD AND SHORTWAVE UPWARD FLUXES ARE
C     COLLECTED.
C     THE INITIAL GROUND POTENTIAL TEMPERATURE IS ALSO COMPUTED HERE
C     AND IS SIMPLY AN ADIABATIC EXTRAPOLATION FROM THE LOWEST MID-
C     LAYER VALUE ABOVE THE GROUND.  
C     
C PROGRAM HISTORY LOG:
C   87-09-??  BLACK      - ORIGINATOR
C   92-10-??  BALDWIN    - VARIOUS CLOUD EFFECTS WERE INCLUDED
C                          WHICH WERE ALREADY IN THE MRF
C   93-11-??  ZHAO       - TIED TO UPDATED GFDL RADIATION SCHEME
C                          USING MODEL-PREDICTED CLOUD
C   95-03-25  BLACK      - CONVERSION FROM 1-D TO 2-D IN HORIZONTAL
C   95-04-13  BLACK      - PARALLELIZED THE LARGE LOOP STEPPING
C                          THROUGH THE DOMAIN THAT CALLS RADFS
C   95-10-10  ZHAO       - i) THE CALCULATION OF CLOUD FRACTION WAS
C                             CHANGED TO USE BOTH CLOUD WATER/ICE 
C                             MIXING RATIO AND RELATIVE HUMIDITY
C                             (RANDALL, 1994);
C                          ii) THE CLOUD INPUTS WERE CHANGED TO USE
C                              CLOUD FRACTION IN EACH MODEL LAYER
C                              AFTER Y.T. HOU (1995).
C   96-06-03  ZHAO       - SNOW ALBEDO IS CHANGED ACCORDING TO
C                          SUGGESTIONS FROM KEN MITCHELL AND FEI CHEN
C   96-07-23  ZHAO       - ADD CALL TO SOLARD TO CALCULATE THE NON-
C                          DIMENSIONAL SUN-EARTH DISTANCE R1 WHICH
C                          WILL BE USED IN RADFS TO COMPUTE SOLAR
C                          CONSTANT SOLC ON EACH DAY
C   96-07-26  BLACK      - ADDED OZONE COMPUTATIONS
C   97-05-19  ZHAO       - DIAGNOSTIC CLOUDS (LOW, MIDDLE, AND HIGH)
C                          ARE MODIFIED TO USE THE MAXIMUM OF
C                          CONVECTIVE AND STRATIFORM. THIS WILL REPLACE
C                          THE PREVIOUS SCHEME WHICH USES ONLY CONVECTIVE
C                          CLOUDS AT CONVECTIVE POINTS. THIS WILL
C                          AFFECT CFRACL, CFRACM, CFRACH, AND WILL
C                          AFFECT THE TOTAL CLOUD FRACTION CALCULATION
C                          IN THE POST PROCESSORS.
C   98-??-??  TUCCILLO   - ADDED PARALLELISM FOR CLASS VIII
C   98-10-27  BLACK      - PARALLELISM INTO NEWEST VERSION
C 
C
C     
C USAGE: CALL RADTN FROM MAIN PROGRAM EBU
C   INPUT ARGUMENT LIST:
C     NONE     
C  
C   OUTPUT ARGUMENT LIST: 
C     NONE
C     
C   OUTPUT FILES:
C     NONE
C     
C   SUBPROGRAMS CALLED:
C  
C     UNIQUE:
C        ZENITH
C        RADFS
C        GRADFS
C        SOLARD
C        O3CLIM
C        OZON2D
C  
C     LIBRARY:
C        NONE
C  
C   COMMON BLOCKS: CTLBLK
C                  LOOPS
C                  MASKS
C                  DYNAMD
C                  PHYS
C                  VRBLS
C                  PVRBLS
C                  CLDWTR
C                  CNVCLD
C                  INDX
C                  ACMCLD
C                  ACMRDS
C                  ACMRDL
C   
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE : IBM SP
C$$$  
C     ******************************************************************
C     *   Note: Convective clouds are added in this subroutine         *
C     *         for use in the eta model in which model-predicted      *
C     *         clouds are not used in the convective precipitation    *
C     *         processes.                                             *
C     *         For use with the version of the eta model in which     *
C     *         the model-predicted clouds are linked into the model's *
C     *         convective precipitation processes, just set:          *
C     *                    CNCLD=.FALSE.                               *
C     *                                     Qingyun  Zhao  12-9-94     *
C     ******************************************************************
C-----------------------------------------------------------------------
      INCLUDE "parmeta"
      INCLUDE "parm.tbl"
      INCLUDE "parmsoil"
      INCLUDE "mpp.h"
#include "sp.h"
C-----------------------------------------------------------------------
                             P A R A M E T E R
     & (CAPA=0.28589641,RTD=57.2957795
     &, WA=.10,WG=1.-WA,KSMUD=0)
C-------------------------CLOUD----------------------------------------
                             P A R A M E T E R
     & (A1=610.78,A2=17.2693882,A3=273.16,A4=35.86              
c     &, PQ0=379.90516,SNOALB=0.55) 
     &, PQ0=379.90516) 
C-------------------------CLOUD----------------------------------------
                             P A R A M E T E R
     & (IMJM=IM*JM-JM/2,JAM=6+2*(JM-10),LM1=LM-1,LP1=LM+1)
C
                             P A R A M E T E R
     & (SLPM=1.01325E5,EPSQ1=1.E-5,EPSQ=2.E-12,EPSO3=1.E-10,HPINC=1.E1
     &, CLDRH0=0.80,TRESH=1.00,RNRM=1./(TRESH-CLDRH0)
     &, CLDRH2=0.90,TRESH2=1.00,RNRM2=1./(TRESH2-CLDRH2)
     &, CLAPSE=-0.0005,CLPSE=-0.0006,DCLPS=-0.0001
     &, CM1=2937.4,CM2=4.9283,CM3=23.5518,EPS=0.622,PBOT=10000.0 
     &, STBOL=5.67E-8,PI2=2.*3.14159265,RLAG=14.8125)
C
                             P A R A M E T E R
     & (NB=12)
C-----------------------------------------------------------------------
                             P A R A M E T E R
     & (K15=SELECTED_REAL_KIND(15))
C
                             R E A L
     & (KIND=K15) PROD,DDX,EEX
C-----------------------------------------------------------------------
                             L O G I C A L
     & RUN,FIRST,RESTRT,SIGMA,CALL1,SHORT,LONG
     &,BCLD(IDIM1:IDIM2),BTEMP1(IDIM1:IDIM2)
     &,BITX,BITY,BITZ,BITW,BIT1,BIT2,BITC,BITS,BITCP1,BITSP1
C-------------------------CONVECTION------------------------------------
                             L O G I C A L
     & CNCLD
C-------------------------CONVECTION------------------------------------
      INCLUDE "CTLBLK.comm"
C-----------------------------------------------------------------------
      INCLUDE "LOOPS.comm"
C-----------------------------------------------------------------------
      INCLUDE "MASKS.comm"
C-----------------------------------------------------------------------
      INCLUDE "DYNAMD.comm"
C-----------------------------------------------------------------------
      INCLUDE "PHYS.comm"
C-----------------------------------------------------------------------
      INCLUDE "VRBLS.comm"
C-----------------------------------------------------------------------
      INCLUDE "PVRBLS.comm"
C-----------------------------------------------------------------------
      INCLUDE "SOIL.comm"
C-----------------------------------------------------------------------
      INCLUDE "CLDWTR.comm"
C-----------------------------------------------------------------------
      INCLUDE "CNVCLD.comm"
C-----------------------------------------------------------------------
      INCLUDE "INDX.comm"
C-----------------------------------------------------------------------
      INCLUDE "ACMCLD.comm"
C----------------------------------------------------------------------
      INCLUDE "ACMRDL.comm"
C----------------------------------------------------------------------
      INCLUDE "ACMRDS.comm"
C-----------------------------------------------------------------------
      COMMON /CUINIT/ CURAD
      LOGICAL CURAD
C-----------------------------------------------------------------------
                             C O M M O N
     & /SWRSAV/ABCFF(NB),PWTS(NB),CFCO2,CFO3,REFLO3,RRAYAV
                             C O M M O N
     & /RD1TIM/K400,CTHK(3),LTOP(3),PTOPC(4),TAUCV(3),R1
     &, LVL(IDIM1:IDIM2,JDIM1:JDIM2)
C-----------------------------------------------------------------------
                             D I M E N S I O N
     &  TENDK (LM),CLDAMT(0:LM)
     &, PSFC  (IDIM1:IDIM2),TSKN (IDIM1:IDIM2)
c     &, ALBEDO(IDIM1:IDIM2),XLAT(IDIM1:IDIM2),COSZ (IDIM1:IDIM2)
     &, ALBDO (IDIM1:IDIM2),XLAT(IDIM1:IDIM2),COSZ (IDIM1:IDIM2)
     &, CLDCFR(IDIM1:IDIM2,3),MBOT(IDIM1:IDIM2,3)
     &, CLDF  (IDIM1:IDIM2,LP1),SLMSK (IDIM1:IDIM2)
     &, TENDS (IDIM1:IDIM2,LM),TENDL (IDIM1:IDIM2,LM)
C
     &, PMID  (IDIM1:IDIM2,LM),TMID  (IDIM1:IDIM2,LM)
     &, QMID  (IDIM1:IDIM2,LM),THMID(IDIM1:IDIM2,LM)
     &, OZN   (IDIM1:IDIM2,LM),POZN  (IDIM1:IDIM2,LM)
     &, MTOP(IDIM1:IDIM2,3),ICVB(IDIM1:IDIM2), ICVT(IDIM1:IDIM2)
     &, CV(IDIM1:IDIM2),SV(IDIM1:IDIM2)
C
     &, FLWUP (IDIM1:IDIM2),FSWDN (IDIM1:IDIM2),FSWUP (IDIM1:IDIM2)
     &, FSWDNS(IDIM1:IDIM2),FSWUPS(IDIM1:IDIM2)
     &, FLWDNS(IDIM1:IDIM2),FLWUPS(IDIM1:IDIM2)
     &, PDSL  (IDIM1:IDIM2,JDIM1:JDIM2)
     &, FNE(IDIM1:IDIM2,JDIM1:JDIM2),FSE(IDIM1:IDIM2,JDIM1:JDIM2)
     &, TL (IDIM1:IDIM2,JDIM1:JDIM2)
                             D I M E N S I O N
     & PBOTL(IDIM1:IDIM2,JDIM1:JDIM2), PTOPL(IDIM1:IDIM2,JDIM1:JDIM2)
     &,PBOTM(IDIM1:IDIM2,JDIM1:JDIM2), PTOPM(IDIM1:IDIM2,JDIM1:JDIM2)
     &,PBOTH(IDIM1:IDIM2,JDIM1:JDIM2), PTOPH(IDIM1:IDIM2,JDIM1:JDIM2)
     &,TOT  (IDIM1:IDIM2,JDIM1:JDIM2)
                             D I M E N S I O N
     & CC(9),PPT(9)
                             D I M E N S I O N
     &  PINT(IDIM1:IDIM2,LP1),PHALF(LP1),CSTR(IDIM1:IDIM2)
     &, EMIS(IDIM1:IDIM2,LP1), TAUC(IDIM1:IDIM2)
     &, CVB(IDIM1:IDIM2),CVT(IDIM1:IDIM2),TAUDAR(IDIM1:IDIM2)
                             D I M E N S I O N
     & CAMT(IDIM1:IDIM2,LP1),NCLDS(IDIM1:IDIM2)
     &,ITYP(IDIM1:IDIM2,LP1),KTOP(IDIM1:IDIM2,LP1)
     &,KBTM(IDIM1:IDIM2,LP1),RRCL(IDIM1:IDIM2,NB,LP1)
     &,TTCL(IDIM1:IDIM2,NB,LP1),KCLD(IDIM1:IDIM2)
C--------------------CLOUD----------------------------------------------
                             D I M E N S I O N                          
     & CCR(IDIM1:IDIM2,LM),IW(IDIM1:IDIM2,LM),CSMID(IDIM1:IDIM2,LM)
     &,WMID(IDIM1:IDIM2,LM),HMID(IDIM1:IDIM2,LM)
     &,BMID(IDIM1:IDIM2),UMID(IDIM1:IDIM2)
     &,CCMID(IDIM1:IDIM2,LM)
C--------------------CLOUD----------------------------------------------
                             D A T A
     1  PLOMD/64200./,PMDHI/35000./,PHITP/15000./,P400/40000./
     2, PLBTM/105000./
                             D A T A
     1  NFILE/14/
        DATA CC/0.,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8/
        DATA PPT/.14,.31,.70,1.6,3.4,7.7,17.,38.,85./
C----------------------------------------------------------------------
      UTIM=1.
      CNCLD=.TRUE.
C***  
C***  ASSIGN THE PRESSURES FOR CLOUD DOMAIN BOUNDARIES
C***
        PTOPC(1)=PLBTM
        PTOPC(2)=PLOMD
        PTOPC(3)=PMDHI
        PTOPC(4)=PHITP
C***
C***  FIND THE 'SEA LEVEL PRESSURE'.
C***
      DO J=MYJS,MYJE
      DO I=MYIS,MYIE
        PDSL(I,J)=RES(I,J)*PD(I,J)
      ENDDO
      ENDDO
C**********************************************************************
C***  THE FOLLOWING CODE IS EXECUTED EACH TIME THE RADIATION IS CALLED.
C**********************************************************************
C----------------------CONVECTION--------------------------------------
C  NRADPP IS THE NUMBER OF TIME STEPS TO ACCUMULATE CONVECTIVE PRECIP
C     FOR RADIATION
C   NOTE: THIS WILL NOT WORK IF NRADS AND NRADL ARE DIFFERENT UNLESS
C         THEY ARE INTEGER MULTIPLES OF EACH OTHER
C  CLSTP IS THE NUMBER OF HOURS OF THE ACCUMULATION PERIOD
C
      NTSPH=NINT(3600./DT)
      NRADPP=MIN(NRADS,NRADL)
      CLSTP=1.0*NRADPP/NTSPH
C----------------------CONVECTION--------------------------------------
C***
C***  STATE WHETHER THE SHORT OR LONGWAVE COMPUTATIONS ARE TO BE DONE.
C***
      SHORT=.FALSE.
      LONG=.FALSE.
      IF(MOD(NTSD,NRADS).EQ.1.OR.RESTRT)SHORT=.TRUE.
      IF(MOD(NTSD,NRADL).EQ.1.OR.RESTRT)LONG=.TRUE.
      ITIMSW=0
      ITIMLW=0
      IF(SHORT)ITIMSW=1
      IF(LONG) ITIMLW=1
C-----------------------------------------------------------------------
C***
C***  FLAG FOR RESETTING CUPPT,HTOP,HBOT IN CHKOUT
C***
      IF (MOD(NTSD,NRADPP).EQ.1) CURAD=.TRUE.
C***
C***  FIND THE MEAN COSINE OF THE SOLAR ZENITH ANGLE 
C***  BETWEEN THE CURRENT TIME AND THE NEXT TIME RADIATION IS
C***  CALLED.  ONLY AVERAGE IF THE SUN IS ABOVE THE HORIZON.
C***
      TIME=(NTSD-1)*DT
      CALL ZENITH(TIME,DAYI,HOUR)
      JD=INT(DAYI+0.50)
      ADDL=0.
      IF(MOD(IDAT(3),4).EQ.0)ADDL=1.
      RANG=PI2*(DAYI-RLAG)/(365.25+ADDL)
      RSIN1=SIN(RANG)
      RCOS1=COS(RANG)
      RCOS2=COS(2.*RANG)
      IF(SHORT)THEN
!$omp parallel do private(i,j)
        DO J=MYJS,MYJE
        DO I=MYIS,MYIE
          CZMEAN(I,J)=0.
          TOT(I,J)=0.
        ENDDO
        ENDDO
C
        DO II=0,NRADS,NPHS
          TIMES=(NTSD-1)*DT+II*DT
          CALL ZENITH(TIMES,DAYI,HOUR)
!$omp parallel do private(i,j)
          DO J=MYJS,MYJE
          DO I=MYIS,MYIE
            IF(CZEN(I,J).GT.0.)THEN
              CZMEAN(I,J)=CZMEAN(I,J)+CZEN(I,J)
              TOT(I,J)=TOT(I,J)+1.
            ENDIF
          ENDDO
          ENDDO
        ENDDO
!$omp parallel do private(i,j)
        DO J=MYJS,MYJE
        DO I=MYIS,MYIE
          IF(TOT(I,J).GT.0.)CZMEAN(I,J)=CZMEAN(I,J)/TOT(I,J)
        ENDDO
        ENDDO
      ENDIF
C
C2345678901234567890123456789012345678901234567890123456789012345678901
!$omp parallel do
c!$omp& private (aa,albedo,apes,bb,bcld,bit1,bit2,bitc,bitcp1,bits,
!$omp& private (aa,albdo,apes,bb,bcld,bit1,bit2,bitc,bitcp1,bits,
!$omp&          bitsp1,bitw,bitx,bity,bitz,bmid,btemp1,camt,
!$omp&          cc1,cc2,ccmid,ccr,cfravg,cl1,cl2,
!$omp&          cldamt,cldcfr,cldmax,clpfil,cosz,cr1,
!$omp&          csmid,cstr,cv,cwmkl,dd,ddp,delp,denom,
!$omp&          dpcl,dthdp,ee,emis,exner,fctra,
!$omp&          fctrb,ff,fiq,fiw,flwdns,flwup,flwups,fswdn,fswdns,
!$omp&          fswup,fswups,gg,hh,hmid,i,icvb,icvt,ir,ityp,
!$omp&          iw,iwkl,j,kbt1,kbt2,kbtm,kcld,kntlyr,
!$omp&          kth1,kth2,ktop,ktop1,l,l400,lbase,lin,ll,
!$omp&          llbot,lltop,lml,ltrop,lvlij,malvl,mbot,mtop,
!$omp&          n,nband,nbtm,nc,ncld,nclds,nktp,nlvl,
!$omp&          nmod,ozn,p1,p2,pdslij,pint,
!$omp&          pmid,pmod,pozn,pp,prs1,prs2,psfc,qc,qi,qint,qkl,
!$omp&          qmid,qsum,qw,rqkl,rrcl,slmsk,snofac,sv,
!$omp&          tauc,taudar,tcld,tendl,tends,thmid,
!$omp&          tkl,tmid,tmt0,tmt15,tskn,ttcl,
!$omp&          u00kl,umid,us,wmid,xlat)
c
C********************************************************************
C***  THIS IS THE BEGINNING OF THE PRIMARY LOOP THROUGH THE DOMAIN
C********************************************************************
C                        *********************
                         DO 700 J = MYJS, MYJE
C                        *********************
C
      DO 125 L=1,LM
      DO I=MYIS,MYIE
        IR=IRAD(I)
        TMID(I,L)=T(I,J,1)
        QMID(I,L)=EPSQ
        CSMID(I,L)=0.
        WMID(I,L)=0.
        CCMID(I,L)=0.
        IW(I,L)=0.
        CCR(I,L)=0.
        HMID(I,L)=0.
        OZN(I,L)=EPSO3
        TENDS(I,L)=0.
        TENDL(I,L)=0.
      ENDDO
  125 CONTINUE
C
      DO 140 N=1,3
      DO I=MYIS,MYIE
        CLDCFR(I,N)=0.
        MTOP(I,N)=0
        MBOT(I,N)=0
      ENDDO
  140 CONTINUE
C***
C***  FILL IN WORKING ARRAYS WHERE VALUES AT L=LM ARE THOSE THAT
C***  ARE ACTUALLY AT ETA LEVEL L=LMH.
C***
      DO 200 I=MYIS,MYIE
      IR=IRAD(I)
      LML=LMH(I,J)
      LVLIJ=LVL(I,J)
ctlb  BMID(I)=HBM2(IR,J)
      BMID(I)=HBM2(I,J)
      UMID(I)=U00(I,J)
C
      DO L=1,LML
        PMID(I,L+LVLIJ)=AETA(L)*PDSL(I,J)+PT
        PINT(I,L+LVLIJ+1)=ETAD(L+1)*PDSL(I,J)+PT
        EXNER=(1.E5/PMID(I,L+LVLIJ))**CAPA
        TMID(I,L+LVLIJ)=T(I,J,L)
        THMID(I,L+LVLIJ)=T(I,J,L)*EXNER
        QMID(I,L+LVLIJ)=Q(I,J,L)
        WMID(I,L+LVLIJ)=CWM(I,J,L)
        HMID(I,L+LVLIJ)=HTM(I,J,L)
      ENDDO
C***
C***  FILL IN ARTIFICIAL VALUES ABOVE THE TOP OF THE DOMAIN.
C***  PRESSURE DEPTHS OF THESE LAYERS IS 1 HPA.
C***  TEMPERATURES ABOVE ARE ALREADY ISOTHERMAL WITH (TRUE) LAYER 1.
C***
      IF(LVLIJ.GT.0)THEN
        KNTLYR=0
C
        DO L=LVLIJ,1,-1
          KNTLYR=KNTLYR+1
          PMID(I,L)=PT-REAL(2*KNTLYR-1)*0.5*HPINC
          PINT(I,L+1)=PMID(I,L)+0.5*HPINC
          EXNER=(1.E5/PMID(I,L))**CAPA
          THMID(I,L)=TMID(I,L)*EXNER
        ENDDO
      ENDIF
C
      IF(LVLIJ.EQ.0) THEN
         PINT(I,1)=PT
      ELSE
         PINT(I,1)=PMID(I,1)-0.5*HPINC
      ENDIF
  200 CONTINUE
C***
C***  FILL IN THE SURFACE PRESSURE, SKIN TEMPERATURE, GEODETIC LATITUDE,
C***  ZENITH ANGLE, SEA MASK, AND ALBEDO.  THE SKIN TEMPERATURE IS
C***  NEGATIVE OVER WATER.
C***
      DO 250 I=MYIS,MYIE
      PSFC(I)=PD(I,J)+PT
      APES=(PSFC(I)*1.E-5)**CAPA
      TSKN(I)=THS(I,J)*APES*(1.-2.*SM(I,J))
      if(mype.eq.13.and.i.eq.40.and.j.eq.77) then
        print*,'apes,sm(i,j),ths(i,j)=',apes,sm(i,j),ths(i,j)
      endif
      SLMSK(I)=SM(I,J)
C
C ----------------------------------------------------------------------
c turn off snow albedo calculation since it is now calculated in SFLX.
c      SNO(I,J)=AMAX1(SNO(I,J),0.)
c      SNOFAC=AMIN1(SNO(I,J)/0.02, 1.0)
c      ALBEDO(I)=ALB(I,J)+(1.0-VEGFRC(I,J))*SNOFAC*(SNOALB-ALB(I,J))
      ALBDO(I)=ALBEDO(I,J)
C
      XLAT(I)=GLAT(I,J)*RTD
      COSZ(I)=CZMEAN(I,J)
  250 CONTINUE
C-----------------------------------------------------------------------
C*******************STRATIFORM CLOUD SECTION***************************
C-----------------------------------------------------------------------
C  CALCULATE STRATIFORM CLOUD COVERAGE AT EACH MODEL GRID POINT WHICH 
C  WILL BE USED IN THE MODEL RADIATION PARAMETERIZATION SCHEME.
C-----------------------------------------------------------------------
      US=1.                                                             
      CCLIMIT=1.0E-3                                                    
      CLIMIT =1.0E-20                                                   
C------------------QW, QI AND QINT--------------------------------------
      DO 280 I=MYIS,MYIE
      LML=LMH(I,J)
      LVLIJ=LVL(I,J)
C
      DO 275 L=1,LML
      LL=L+LVLIJ
      HH=HMID(I,LL)*BMID(I)
      TKL=TMID(I,LL)                                                   
      QKL=QMID(I,LL)                                                   
      CWMKL=WMID(I,LL)                                                 
      TMT0=(TKL-273.16)*HH                                              
      TMT15=AMIN1(TMT0,-15.)*HH                                         
      AI=0.008855
      BI=1.
C
      IF(TMT0.LT.-20.)THEN
        AI=0.007225
        BI=0.9674
      ENDIF 
C
      PP=PMID(I,LL)
      QW=HH*PQ0/PP*EXP(HH*A2*(TKL-A3)/(TKL-A4))                  
      QI=QW*(BI+AI*AMIN1(TMT0,0.))                               
      QINT=QW*(1.-0.00032*TMT15*(TMT15+15.))                        
      IF(TMT0.LE.-40.) QINT=QI                                    
C-------------------ICE-WATER ID NUMBER IW------------------------------
      U00KL=UMID(I)+UL(L)*(0.95-UMID(I))*UTIM
      IF(TMT0.LT.-15.0)THEN
        FIQ=QKL-U00KL*QI    
        IF(FIQ.GT.0..OR.CWMKL.GT.CLIMIT)THEN                    
          IW(I,LL)=1                                                 
        ELSE                                                           
          IW(I,LL)=0                                                 
        ENDIF                                                         
      ENDIF
C
      IF(TMT0.GE.0.)THEN
        IW(I,LL)=0                                                    
      ENDIF
C
      IF(TMT0.LT.0.0.AND.TMT0.GE.-15.0)THEN
        IW(I,LL)=0
        IF(IW(I,LL-1).EQ.1.AND.CWMKL.GT.CLIMIT) IW(I,LL)=1 
      ENDIF
C
      IWKL=IW(I,LL)                                                    
C
C----------------THE SATURATION SPECIFIC HUMIDITY------------------------
C
      FIW=FLOAT(IWKL)                                                   
      QC=(1.-FIW)*QINT+FIW*QI                                 
C
C----------------THE RELATIVE HUMIDITY----------------------------------
C
      IF(QC.LE.EPSQ1.OR.QKL.LE.EPSQ1)THEN  
        RQKL=0.
      ELSE                                                             
        RQKL=QKL/QC                                                 
      ENDIF                                                             
C
C----------------CLOUD COVER RATIO CCR----------------------------------
C
      IF(RQKL.GE.0.9999)THEN                                      
        CCR(I,LL)=AMIN1(US,RQKL) 
      ELSE                                                            
        ARG=-1000.*CWMKL/(US-RQKL)
        ARG=AMAX1(ARG,-25.)
        CCR(I,LL)= RQKL*(1.-EXP(ARG))
      ENDIF                                                             
      CSMID(I,LL)=AMIN1(US,CCR(I,LL))
C----------------------------------------------------------------------
  275 CONTINUE
  280 CONTINUE
C----------------------------------------------------------------------
C**********************************************************************
C NOW CHECK THE CLOUDS PRODUCED ABOVE TO MAKE SURE THEY ARE GOOD
C ENOUGH FOR RADIATION CALCULATIONS
C**********************************************************************
C***
C***  NO STRATIFORM CLOUDS FOR THIS TYPE
C***
      DO 350 I=MYIS,MYIE
C
      LML=LMH(I,J)
      LVLIJ=LVL(I,J)
C***
C***  ZERO OUT CLDAMT IF LAND AND BELOW PBOT ABOVE GROUND
C***
      IF(SM(I,J).LT.0.5)THEN
        DO L=1,LML
          LL=LML-L+1+LVLIJ
          DDP=PSFC(I)-PMID(I,LL)
          IF(DDP.GE.PBOT) GO TO 290
          CSMID(I,LL)=0.
        ENDDO
  290   CONTINUE
      ENDIF
C***
C***  CHECK FOR OCEAN STRATUS (LOW CLOUD)
C***  LOOK ONLY OVER OCEAN AND ONLY IF AN INVERSION (DTHDP.LE.-0.05)
C***  IS PRESENT WITH AT LEAST 2 CLOUD FREE LAYERS ABOVE IT
C***
      IF(SM(I,J).GT.0.5)THEN
C
C***  FIND BASE OF INVERSION
C
        LBASE=LM
        DO L=1,LML-1
          LL=LML-L+1+LVLIJ
          DTHDP=(THMID(I,LL-1)-THMID(I,LL))
     &          /(PMID(I,LL-1)-PMID(I,LL))
          IF(DTHDP.LE.CLAPSE)THEN
            LBASE=LL
            GO TO 300
          ENDIF
        ENDDO
  300   CONTINUE
C
C***  CHECK 2 LAYERS ABOVE LBASE FOR DRYNESS
C
        IF(CSMID(I,LBASE-1).LE.0..AND.CSMID(I,LBASE-2).LE.0.
     1                               .AND.LBASE.LT.LM)THEN
          IF(DTHDP.GT.CLPSE)THEN
            CLPFIL=1.-((CLPSE-DTHDP)/DCLPS)
          ELSE
            CLPFIL=1.
          ENDIF
C
          DO L=1,LML
            LL=LML-L+1+LVLIJ
            DDP=PSFC(I)-PMID(I,LL)
            IF(DDP.GE.PBOT) GO TO 310
            CSMID(I,LL)=CSMID(I,LL)*CLPFIL
          ENDDO
  310     CONTINUE
C
C***  IF NO INVERSION OR IF CLDS EXIST IN EITHER OF THE 2 LAYERS ABOVE 
C***  INVERSION, ZERO OUT CLOUD BELOW PBOT
C
        ELSE
          DO L=1,LML
            LL=LML-L+1+LVLIJ
            DDP=PSFC(I)-PMID(I,LL)
            IF(DDP.GE.PBOT) GO TO 320
            CSMID(I,LL)=0.
          ENDDO
  320     CONTINUE
C
        ENDIF
C------------
      ENDIF
C------------
C***
C***  REMOVE HIGH CLOUDS ABOVE THE TROPOPAUSE
C***
      L400=LM
      DO L=1,LML
        LL=LML-L+1+LVLIJ
        IF(PMID(I,LL).LE.40000.0)THEN
          L400=LL
          GO TO 330
        ENDIF
      ENDDO
  330 CONTINUE
C
      LTROP=LM
      DO LL=L400,2,-1
        DTHDP=(THMID(I,LL-1)-THMID(I,LL))
     1         /(PMID(I,LL-1)-PMID(I,LL))
C
        IF(DTHDP.LT.-0.0025.OR.QMID(I,LL).LE.EPSQ1)THEN
          LTROP=LL
          GOTO 340
        ENDIF
      ENDDO
  340 IF(LTROP.LT.LM)THEN
        DO LL=LTROP,1,-1
          CSMID(I,LL)=0.
        ENDDO
      ENDIF
  350 CONTINUE
C
C*********************************************************************
C*****************END OF STRATIFORM CLOUD SECTION*****************
C------------------------CONVECTION--------------------------------   
C***
C***  CONVECTIVE CLOUD SECTION
C***
C*** THIS PART WAS MODIFIED TO COMPUTE CONVECTIVE CLOUDS AT EACH
C*** MODEL LAYER BASED ON CONVECTIVE PRECIPITATION RATES. CURRENTLY,
C*** CLOUDS ARE SET TO 0.75*CV(I) BELOW 400MB
C*** AND    0.90*CV(I) ABOVE 400MB TO ACCOUNT FOR CIRRUS CAP
C***                                       Q.ZHAO   95-3-22
C
C***
C*** NON-PRECIPITATING CLOUD FRACTION OF 20 PERCENT IS ADDED AT
C*** AT POINTS WHERE THE SHALLOW AND DEEP CONVECTIONS ACCUR.
C***                                        Q. ZHAO  97-5-2
C
C   COMPUTE THE CONVECTIVE CLOUD COVER FOR RADIATION
C
C-----------------------------------------------------------------
      IF(CNCLD)THEN
C-----------------------------------------------------------------
        DO 375 I=MYIS,MYIE
        IF(HBOT(I,J)-HTOP(I,J).GT.1.0)THEN
C       IF(HTOP(I,J).LT.HBOT(I,J))THEN
          SV(I)=0.0
        ELSE
          SV(I)=0.0
        ENDIF
C
        PMOD=CUPPT(I,J)*24.0*1000.0/CLSTP
        NMOD=0
C
        DO NC=1,9
          IF(PMOD.GT.PPT(NC)) NMOD=NC
        ENDDO
C
C***  CLOUD TOPS AND BOTTOMS COME FROM CUCNVC
C***  ADD LVL TO BE CONSISTENT WITH OTHER WORKING ARRAYS
C
        IF(NMOD.EQ.0)THEN
          CV(I)=0.
        ELSEIF(NMOD.EQ.9)THEN
          CV(I)=CC(9)
        ELSE
          CC1=CC(NMOD)
          CC2=CC(NMOD+1)
          P1=PPT(NMOD)
          P2=PPT(NMOD+1)
          CV(I)=CC1+(CC2-CC1)*(PMOD-P1)/(P2-P1)
        ENDIF
C
        CV(I)=AMAX1(SV(I),CV(I))
        CV(I)=AMIN1(1.0,CV(I))
C
        IF(CV(I).EQ.0.0)THEN
          ICVT(I)=0
          ICVB(I)=0
        ELSE
          ICVT(I)=INT(HTOP(I,J)+0.50)+LVL(I,J)
          ICVB(I)=INT(HBOT(I,J)+0.50)+LVL(I,J)
        ENDIF
  375   CONTINUE
C***
C***  MAKE SURE CLOUDS ARE DEEP ENOUGH
C***
        DO I=MYIS,MYIE
          BCLD(I)=CV(I).GT.0..AND.
     1           (ICVB(I)-ICVT(I)).GE.1
          BTEMP1(I)=BCLD(I)
        ENDDO
C***
C*** COMPUTE CONVECTIVE CLOUD FRACTION
C***
        DO 390 I=MYIS,MYIE
        IF(BCLD(I)) THEN
          LML=LMH(I,J)
          LVLIJ=LVL(I,J)
C
          DO L=1,LML
            LL=L+LVLIJ
            IF(LL.GT.ICVB(I).OR.LL.LT.ICVT(I))THEN
              CCMID(I,LL)=0.
            ELSE
              CCMID(I,LL)=CV(I)
            ENDIF
            CCMID(I,LL)=AMIN1(1.0,CCMID(I,LL))
          ENDDO
        ENDIF
  390   CONTINUE
C***
C***  REMOVE HIGH CLOUDS ABOVE THE TROPOPAUSE
C***
        L400=LM
        DO 425 I=MYIS,MYIE
        LML=LMH(I,J)
        LVLIJ=LVL(I,J)
C
        DO L = 1, LML
          LL=LML-L+1+LVLIJ
          IF(PMID(I,LL).LE.40000.0)THEN
            L400=LL
            GO TO 400
          ENDIF
        ENDDO
  400   CONTINUE
C
        LTROP=LM
        DO LL=L400,2,-1
          DTHDP=(THMID(I,LL-1)-THMID(I,LL))
     1         /(PMID(I,LL-1)-PMID(I,LL))
          IF(DTHDP.LT.-0.0025.OR.QMID(I,LL).LE.EPSQ1)THEN
            LTROP=LL
            GOTO 410
          ENDIF
        ENDDO
  410   IF(LTROP.LT.LM)THEN
         DO LL=LTROP,1,-1
           CCMID(I,LL)=0.
         ENDDO
        ENDIF
  425   CONTINUE
C***
C-----------------------------------------------------------------
      ENDIF
C------------------------CONVECTION--------------------------------
C*****************END OF CONVECTIVE CLOUD SECTION*****************
C*********************************************************************
C***
C***  DETERMINE THE FRACTIONAL CLOUD COVERAGE FOR HIGH, MID
C***  AND LOW OF CLOUDS FROM THE CLOUD COVERAGE AT EACH LEVEL
C***
C***  NOTE: THIS IS FOR DIAGNOSTICS ONLY!!!
C***
C***
      DO 500 I=MYIS,MYIE
C
      CSTR(I)=0.0
C
      DO L=0,LM
        CLDAMT(L)=0.
      ENDDO
C  
C***  NOW GOES LOW, MIDDLE, HIGH
C
      DO 480 NLVL=1,3
      CLDMAX=0.
      MALVL=LM
      LLTOP=LTOP(NLVL)+LVL(I,J)
C***
C***  GO TO THE NEXT CLOUD LAYER IF THE TOP OF THE CLOUD-TYPE IN
C***  QUESTION IS BELOW GROUND OR IS IN THE LOWEST LAYER ABOVE GROUND.
C***
      IF(LLTOP.GE.LM)GO TO 480
C
      IF(NLVL.GT.1)THEN
        LLBOT=LTOP(NLVL-1)-1+LVL(I,J)
        LLBOT=MIN(LLBOT,LM1)
      ELSE
        LLBOT=LM1
      ENDIF
C
      DO 435 L=LLTOP,LLBOT
      CLDAMT(L)=AMAX1(CSMID(I,L),CCMID(I,L))
      IF(CLDAMT(L).GT.CLDMAX)THEN
        MALVL=L
        CLDMAX=CLDAMT(L)
      ENDIF
  435 CONTINUE
C*********************************************************************
C NOW, CALCULATE THE TOTAL CLOUD FRACTION IN THIS PRESSURE DOMAIN
C USING THE METHOD DEVELOPED BY Y.H., K.A.C. AND A.K. (NOV., 1992).
C IN THIS METHOD, IT IS ASSUMED THAT SEPERATED CLOUD LAYERS ARE
C RADOMLY OVERLAPPED AND ADJACENT CLOUD LAYERS ARE MAXIMUM OVERLAPPED.
C VERTICAL LOCATION OF EACH TYPE OF CLOUD IS DETERMINED BY THE THICKEST
C CONTINUING CLOUD LAYERS IN THE DOMAIN.
C*********************************************************************
      CL1=0.0
      CL2=0.0
      KBT1=LLBOT
      KBT2=LLBOT
      KTH1=0
      KTH2=0
C
      DO 450 LL=LLTOP,LLBOT
      L=LLBOT-LL+LLTOP
      BIT1=.FALSE.
      CR1=CLDAMT(L)
      BITX=(PINT(I,L).GE.PTOPC(NLVL+1)).AND.
     1     (PINT(I,L).LT.PTOPC(NLVL)).AND.
     2     (CLDAMT(L).GT.0.0)
      BIT1=BIT1.OR.BITX
      IF(.NOT.BIT1)GO TO 450
C***
C***  BITY=T: FIRST CLOUD LAYER; BITZ=T:CONSECUTIVE CLOUD LAYER
C***  NOTE:  WE ASSUME THAT THE THICKNESS OF EACH CLOUD LAYER IN THE
C***         DOMAIN IS LESS THAN 200 MB TO AVOID TOO MUCH COOLING OR
C***         HEATING. SO WE SET CTHK(NLVL)=200*E2. BUT THIS LIMIT MAY
C***         WORK WELL FOR CONVECTIVE CLOUDS. MODIFICATION MAY BE
C***         NEEDED IN THE FUTURE.
C***
      BITY=BITX.AND.(KTH2.LE.0)
      BITZ=BITX.AND.(KTH2.GT.0)
C
      IF(BITY)THEN
        KBT2=L
        KTH2=1
      ENDIF
C
      IF(BITZ)THEN
        KTOP1=KBT2-KTH2+1
        DPCL=PMID(I,KBT2)-PMID(I,KTOP1)
        IF(DPCL.LT.CTHK(NLVL))THEN
          KTH2=KTH2+1
        ELSE
          KBT2=KBT2-1
        ENDIF
      ENDIF
      IF(BITX)CL2=AMAX1(CL2,CR1)
C***
C*** AT THE DOMAIN BOUNDARY OR SEPARATED CLD LAYERS, RANDOM OVERLAP.
C*** CHOOSE THE THICKEST OR THE LARGEST FRACTION AMT AS THE CLD
C*** LAYER IN THAT DOMAIN.
C***
      BIT2=.FALSE.
      BITY=BITX.AND.(CLDAMT(L-1).LE.0.0.OR.
     1     PINT(I,L-1).LT.PTOPC(NLVL+1))
      BITZ=BITY.AND.CL1.GT.0.0
      BITW=BITY.AND.CL1.LE.0.0
      BIT2=BIT2.OR.BITY
      IF(.NOT.BIT2)GO TO 450
C
      IF(BITZ)THEN
        KBT1=INT((CL1*KBT1+CL2*KBT2)/(CL1+CL2))
        KTH1=INT((CL1*KTH1+CL2*KTH2)/(CL1+CL2))+1
        CL1=CL1+CL2-CL1*CL2
      ENDIF
C
      IF(BITW)THEN
        KBT1=KBT2
        KTH1=KTH2
        CL1=CL2
      ENDIF
C
      IF(BITY)THEN
        KBT2=LLBOT
        KTH2=0
        CL2=0.0
      ENDIF
  450 CONTINUE
C***
      CLDCFR(I,NLVL)=AMIN1(1.0,CL1)
      MTOP(I,NLVL)=MIN(KBT1,KBT1-KTH1+1)
      MBOT(I,NLVL)=KBT1
  480 CONTINUE
  500 CONTINUE
C***
C***  SET THE UN-NEEDED TAUDAR TO ONE
C***
      DO I=MYIS,MYIE
        TAUDAR(I)=1.0
      ENDDO
C----------------------------------------------------------------------
C NOW, CALCULATE THE CLOUD RADIATIVE PROPERTIES AFTER DAVIS (1982),
C HARSHVARDHAN ET AL (1987) AND Y.H., K.A.C. AND A.K. (1993).
C 
C UPDATE: THE FOLLOWING PARTS ARE MODIFIED, AFTER Y.T.H. (1994), TO 
C         CALCULATE THE RADIATIVE PROPERTIES OF CLOUDS ON EACH MODEL
C         LAYER. BOTH CONVECTIVE AND STRATIFORM CLOUDS ARE USED
C         IN THIS CALCULATIONS.
C
C                                     QINGYUN ZHAO   95-3-22
C
C----------------------------------------------------------------------
C
C***
C*** INITIALIZE ARRAYS FOR USES LATER
C***

      DO 600 I=MYIS,MYIE
      LML=LMH(I,J)
      LVLIJ=LVL(I,J)
C
C***
C*** NOTE: LAYER=1 IS THE SURFACE, AND LAYER=2 IS THE FIRST CLOUD
C***       LAYER ABOVE THE SURFACE AND SO ON.
C***
      EMIS(I,1)=1.0
      KTOP(I,1)=LP1
      KBTM(I,1)=LP1
      CAMT(I,1)=1.0
      ITYP(I,1)=0
      KCLD(I)=2
C
      DO NBAND=1,NB
        RRCL(I,NBAND,1)=0.0
        TTCL(I,NBAND,1)=1.0
      ENDDO
C
      DO 510 L=2,LP1
      ITYP(I,L)=0
      CAMT(I,L)=0.0
      KTOP(I,L)=1
      KBTM(I,L)=1
      EMIS(I,L)=0.0
C
      DO NBAND=1,NB
        RRCL(I,NBAND,L)=0.0
        TTCL(I,NBAND,L)=1.0
      ENDDO
  510 CONTINUE
C***
C*** NOW CALCULATE THE AMOUNT, TOP, BOTTOM AND TYPE OF EACH CLOUD LAYER
C*** CLOUD TYPE=1: STRATIFORM CLOUD
C***       TYPE=2: CONVECTIVE CLOUD
C*** WHEN BOTH CONVECTIVE AND STRATIFORM CLOUDS EXIST AT THE SAME POINT,
C*** SELECT CONVECTIVE CLOUD (TYPE=2),IN OTHER WORDS, CONVECTIVE CLOUDS
C*** HAVE THE HIGHER PRIORITY THAN STRATIFORM CLOUDS.
C*** CLOUD LAYERS ARE SEPARATED BY:
C***       1. NO-CLOUD LAYER
C***       2. DIFFERENT CLOUD TYPE
C*** NOTE: THERE IS ONLY ONE CONVECTIVE CLOUD LAYER IN ONE COLUMN.
C*** KTOP AND KBTM ARE THE TOP AND BOTTOM OF EACH CLOUD LAYER IN TERMS O
C*** ETA MODEL LEVEL.
C***
      DO 540 L=2,LML
      LL=LML-L+1+LVLIJ
      BITC=CCMID(I,LL).GT.0.1
      BITS=CSMID(I,LL).GT.0.1
      BITCP1=CCMID(I,LL+1).GT.0.1
      BITSP1=CSMID(I,LL+1).GT.0.1
      BIT1=BITS.OR.BITC
C-------------------
      IF(BIT1)THEN
C-------------------
        IF(ITYP(I,KCLD(I)).EQ.0)THEN
          CAMT(I,KCLD(I))=CSMID(I,LL)
          ITYP(I,KCLD(I))=1
          KBTM(I,KCLD(I))=LL
C
          IF(BITC)THEN
            CAMT(I,KCLD(I))=CCMID(I,LL)
            ITYP(I,KCLD(I))=2
          ENDIF 
        ELSE
          IF(BITC)THEN
            IF(BITCP1)THEN
              CAMT(I,KCLD(I))=AMAX1(CAMT(I,KCLD(I)),CCMID(I,LL))
            ELSE
              KCLD(I)=KCLD(I)+1
              CAMT(I,KCLD(I))=CCMID(I,LL)
              ITYP(I,KCLD(I))=2
              KTOP(I,KCLD(I)-1)=LL+1
              KBTM(I,KCLD(I))=LL
            ENDIF
          ELSE
            IF(BITCP1)THEN
              KCLD(I)=KCLD(I)+1
              CAMT(I,KCLD(I))=CSMID(I,LL)
              ITYP(I,KCLD(I))=1
              KTOP(I,KCLD(I)-1)=LL+1
              KBTM(I,KCLD(I))=LL
            ELSE
              CAMT(I,KCLD(I))=AMAX1(CAMT(I,KCLD(I)),CSMID(I,LL))
            ENDIF
          ENDIF 
        ENDIF 
C-------------------
      ELSE
C-------------------
        IF(BITCP1.OR.BITSP1)THEN
          KCLD(I)=KCLD(I)+1
          KTOP(I,KCLD(I)-1)=LL+1
          ITYP(I,KCLD(I))=0
          CAMT(I,KCLD(I))=0.0
        ENDIF
C-------------------
      ENDIF
C-------------------
  540 CONTINUE
C***
C*** THE REAL NUMBER OF CLOUD LAYERS IS (THE FIRST IS THE GROUNG;
C*** THE LAST IS THE SKY):
C***
      NCLDS(I)=KCLD(I)-2
      NCLD=NCLDS(I)
C***
C***  NOW CALCULATE CLOUD RADIATIVE PROPERTIES
C***
      IF(NCLD.GE.1)THEN
C***
C*** NOTE: THE FOLLOWING CALCULATIONS, THE UNIT FOR PRESSURE IS MB!!!
C***
        DO 580 NC=2,NCLD+1
C
        TAUC(I)=0.0
        QSUM=0.0
        NKTP=LP1
        NBTM=0
        BITX=CAMT(I,NC).GT.0.1
        NKTP=MIN(NKTP,KTOP(I,NC))
        NBTM=MAX(NBTM,KBTM(I,NC))
C
        DO 560 LL=NKTP,NBTM
        IF(LL.GE.KTOP(I,NC).AND.LL.LE.KBTM(I,NC).AND.BITX)THEN
          PRS1=PINT(I,LL)*0.01
          PRS2=PINT(I,LL+1)*0.01
          DELP=PRS2-PRS1
          TCLD=TMID(I,LL)-273.16
          QSUM=QSUM+QMID(I,LL)*DELP*(PRS1+PRS2)
     1         /(120.1612*SQRT(TMID(I,LL)))
C***
C*** FOR CONVECTIVE CLOUD OR STARTIFORM CLOUD WITH TOP ABOVE 500MB
C***
          IF(ITYP(I,NC).EQ.2
     1           .OR.PINT(I,KTOP(I,NC)).LE.PTOPC(3))THEN
            IF(TCLD.LE.-10.0)THEN
              TAUC(I)=TAUC(I)+DELP*AMAX1(0.1E-3,
     1                 2.0E-6*(TCLD+82.5)**2)
            ELSE
     	      TAUC(I)=TAUC(I)+DELP*AMIN1(0.08,6.949E-3*TCLD+0.1)
            ENDIF
          ELSE
C***
C***  FOR LOW AND MID STRATIFORM CLOUDS
C***
            IF(TCLD.LE.-20.0)THEN
              TAUC(I)=TAUC(I)+DELP*AMAX1(0.1E-3,2.56E-5*
     1               (TCLD+82.5)**2)
            ELSE
              TAUC(I)=TAUC(I)+DELP*0.1
            ENDIF
          ENDIF
        ENDIF
  560   CONTINUE
C
        IF(BITX)EMIS(I,NC)=1.0-EXP(-0.75*TAUC(I))
        IF(QSUM.GE.EPSQ1)THEN
C
          DO 570 NBAND=1,NB
          IF(BITX)THEN
            PROD=ABCFF(NBAND)*QSUM
            DDX=TAUC(I)/(TAUC(I)+PROD)
            EEX=1.0-DDX
            IF(ABS(EEX).GE.1.E-8)THEN
              DD=DDX
              EE=EEX
              FF=1.0-DD*0.85
              AA=MIN(50.0,SQRT(3.0*EE*FF)*TAUC(I))
              AA=EXP(-AA)
              BB=FF/EE
              GG=SQRT(BB)
              DD=(GG+1.0)*(GG+1.0)-(GG-1.0)*(GG-1.0)*AA*AA
              RRCL(I,NBAND,NC)=MAX(0.1E-5,(BB-1.0)*(1.0-AA*AA)/DD)
              TTCL(I,NBAND,NC)=AMAX1(0.1E-5,4.0*GG*AA/DD)
            ENDIF
          ENDIF
  570     CONTINUE
        ENDIF
  580   CONTINUE
C
      ENDIF
C
  600 CONTINUE
C*********************************************************************
C******************  COMPUTE OZONE AT MIDLAYERS  *********************
C*********************************************************************
C
C***  MODIFY PRESSURES SO THAT THE ENTIRE COLUMN OF OZONE (TO 0 MB)
C***  IS INCLUDED IN THE MODEL COLUMN EVEN WHEN PT > 0 MB
C***
      DO L=1,LM
      DO I=MYIS,MYIE
        DENOM=1./(PINT(I,LP1)-PINT(I,1))
        FCTRA=PINT(I,LP1)*DENOM
        FCTRB=-PINT(I,1)*PINT(I,LP1)*DENOM
        POZN(I,L)=PMID(I,L)*FCTRA+FCTRB
      ENDDO
      ENDDO
C
      CALL OZON2D(LM,POZN,XLAT,RSIN1,RCOS1,RCOS2,OZN)
C
C***  
C***  NOW THE VARIABLES REQUIRED BY RADFS HAVE BEEN CALCULATED.
C***
C----------------------------------------------------------------------
C***
C***  CALL THE GFDL RADIATION DRIVER
C***
C***
      CALL RADFS
c     1     (PSFC,PMID,PINT,QMID,TMID,OZN,TSKN,SLMSK,ALBEDO,XLAT
     1     (PSFC,PMID,PINT,QMID,TMID,OZN,TSKN,SLMSK,ALBDO,XLAT
     2,     CAMT,ITYP,KTOP,KBTM,NCLDS,EMIS,RRCL,TTCL 
     3,     COSZ,TAUDAR,1
     4,     1,0 
     5,     ETAD,AETA,ITIMSW,ITIMLW,JD,R1,HOUR,TENDS,TENDL
     6,     FLWUP,FSWUP,FSWDN,FSWDNS,FSWUPS,FLWDNS,FLWUPS)
C----------------------------------------------------------------------
      DO 650 I=MYIS,MYIE
      PDSLIJ=PDSL(I,J)
      PMOD=CUPPT(I,J)*24.0*1000.0/CLSTP
      CFRACL(I,J)=CLDCFR(I,1)
      CFRACM(I,J)=CLDCFR(I,2)
      CFRACH(I,J)=CLDCFR(I,3)
C     
C***  ARRAYS ACFRST AND ACFRCV ACCUMULATE AVERAGE STRATIFORM AND
C***  CONVECTIVE CLOUD FRACTIONS, RESPECTIVELY.  THIS INFORMATION
C***  IS PASSED TO THE POST PROCESSOR VIA COMMON BLOCK ACMCLD.
C
      CFRAVG=AMAX1(CFRACL(I,J),AMAX1(CFRACM(I,J),CFRACH(I,J)))
      IF(CNCLD)THEN
        IF(PMOD.LE.PPT(1))THEN
          ACFRST(I,J)=ACFRST(I,J)+CFRAVG
          NCFRST(I,J)=NCFRST(I,J)+1
        ELSE
          ACFRCV(I,J)=ACFRCV(I,J)+CFRAVG
          NCFRCV(I,J)=NCFRCV(I,J)+1
        ENDIF
      ELSE
        ACFRST(I,J)=ACFRST(I,J)+CFRAVG
        NCFRST(I,J)=NCFRST(I,J)+1
      ENDIF
  650 CONTINUE
C***
C***  COLLECT ATMOSPHERIC TEMPERATURE TENDENCIES DUE TO RADIATION.
C***  ALSO COLLECT THE TOTAL SW AND INCOMING LW RADIATION (W/M**2)
C***  AND CONVERT TO FORM NEEDED FOR PREDICTION OF THS IN SURFCE.
C***
      DO 660 I=MYIS,MYIE
      DO L=1,LM
        LL=LVL(I,J)+L
        IF(SHORT)RSWTT(I,J,L)=TENDS(I,LL)
        IF(LONG) RLWTT(I,J,L)=TENDL(I,LL)
c       if(mype.eq.13.and.i.eq.40.and.j.eq.77) then
c        print*,'l,ll,tendl(i,ll),rlwtt(i,j,l)=',
c    *    l,ll,tendl(i,ll),rlwtt(i,j,l)
c       endif
        IF(LL.EQ.LM)GO TO 660
      ENDDO
  660 CONTINUE
C***
C***  SUM THE LW INCOMING AND SW RADIATION (W/M**2) FOR RADIN.
C***
      DO 675 I=MYIS,MYIE
      IF(LONG)THEN
        SIGT4(I,J)=STBOL*TMID(I,LM)*TMID(I,LM)*
     1             TMID(I,LM)*TMID(I,LM)
      ENDIF
C     
C***  ACCUMULATE VARIOUS LW AND SW RADIATIVE FLUXES FOR POST
C***  PROCESSOR.  PASSED VIA COMMON ACMRDL AND ACMRDS.
C
      IF(LONG)THEN
        RLWIN(I,J) =FLWDNS(I)
        RLWOUT(I,J)=FLWUPS(I)
        RLWTOA(I,J)=FLWUP(I)
      ENDIF
      IF(SHORT)THEN
        RSWIN(I,J) =FSWDNS(I)
        RSWOUT(I,J)=FSWUPS(I)
        RSWTOA(I,J)=FSWUP(I)
      ENDIF
  675 CONTINUE
C***
C***  THIS ROW IS FINISHED. GO TO NEXT
C***
C                        *********************
  700                          CONTINUE
C                        *********************
C----------------------------------------------------------------------
C***
C***  CALLS TO RADIATION THIS TIME STEP ARE COMPLETE.
C***
C----------------------------------------------------------------------
C----------------------------------------------------------------------
C***
C***  HORIZONTAL SMOOTHING OF TEMPERATURE TENDENCIES
C***
C----------------------------------------------------------------------
      IF(SHORT) THEN
        DO 800 L=1,LM
        CALL ZERO2(TL)
        CALL ZERO2(FNE)
        CALL ZERO2(FSE)
C
        IF(KSMUD.GE.1)THEN
          DO 750 KS=1,KSMUD
C
          DO J=MYJS,MYJE
          DO I=MYIS,MYIE
            TL(I,J)=RSWTT(I,J,L)*HTM(I,J,L)
          ENDDO
          ENDDO
C
          DO J=MYJS,MYJE
          DO I=MYIS,MYIE
            FNE(I,J)=(TL(I+IHE(J),J+1)-TL(I,J))
     1               *HTM(I,J,L)*HTM(I+IHE(J),J+1,L)
          ENDDO
          ENDDO
C
          DO J=MYJS1,MYJE
          DO I=MYIS,MYIE
            FSE(I,J)=(TL(I+IHE(J),J-1)-TL(I,J))
     1               *HTM(I+IHE(J),J-1,L)*HTM(I,J,L)
          ENDDO
          ENDDO
C
          DO J=MYJS2,MYJE2
          DO I=MYIS,MYIE
            TL(I,J)=(FNE(I,J)-FNE(I+IHW(J),J-1)
     1              +FSE(I,J)-FSE(I+IHW(J),J+1))
     2              *HBM2(I,J)*0.125+TL(I,J)
          ENDDO
          ENDDO
C
          DO J=MYJS,MYJE
          DO I=MYIS,MYIE
            RSWTT(I,J,L)=TL(I,J)
          ENDDO
          ENDDO
C
  750     CONTINUE
        ENDIF
C
  800   CONTINUE
      ENDIF
C----------------------------------------------------------------------
C
      IF(LONG)THEN
C
        DO 900 L=1,LM
        CALL ZERO2(TL)
        CALL ZERO2(FNE)
        CALL ZERO2(FSE)
C
        IF(KSMUD.GE.1)THEN
          DO 850 KS=1,KSMUD
C
          DO J=MYJS,MYJE
          DO I=MYIS,MYIE
            TL(I,J)=RLWTT(I,J,L)*HTM(I,J,L)
          ENDDO
          ENDDO
C
          DO J=MYJS,MYJE1
          DO I=MYIS,MYIE
            FNE(I,J)=(TL(I+IHE(J),J+1)-TL(I,J))
     1               *HTM(I,J,L)*HTM(I+IHE(J),J+1,L)
          ENDDO
          ENDDO
C
          DO J=MYJS1,MYJE
          DO I=MYIS,MYIE
            FSE(I,J)=(TL(I+IHE(J),J-1)-TL(I,J))
     1               *HTM(I+IHE(J),J-1,L)*HTM(I,J,L)
          ENDDO
          ENDDO
C
          DO J=MYJS2,MYJE2
          DO I=MYIS,MYIE
            TL(I,J)=(FNE(I,J)-FNE(I+IHW(J),J-1)
     1              +FSE(I,J)-FSE(I+IHW(J),J+1))
     2              *HBM2(I,J)*0.125+TL(I,J)
          ENDDO
          ENDDO
C
          DO J=MYJS,MYJE
          DO I=MYIS,MYIE
            RLWTT(I,J,L)=TL(I,J)
          ENDDO
          ENDDO
C
  850     CONTINUE
        ENDIF
  900   CONTINUE
      ENDIF
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
                              RETURN
                              END
