           SUBROUTINE SURFCE(APE,ZINT,CKLQ)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .     
C SUBPROGRAM:    SURFCE      CALCULATE SURFACE CONDITIONS
C   PRGRMMR: JANJIC  ORG: W/NP22     DATE: 95-03-23
C     
C ABSTRACT:
C   THIS ROUTINE IS THE DRIVER FOR COMPUTATION OF GROUND
C   CONDITIONS.  FOR GCIP, ACCUMULATOR AND OTHER
C   INSTANTANEOUS HOLDING ARRAYS ARE INCLUDED. 
C
C PROGRAM HISTORY LOG:
C   95-03-23  JANJIC - ORIGINATOR
C   95-03-28  BLACK  - CONVERSION FROM 1-D TO 2-D IN HORIZONTAL
C   96-03-29  BLACK  - REMOVED SCRCH COMMON
C     
C USAGE:    CALL SURFCE FROM SUBROUTINE TURBL
C   INPUT ARGUMENT LIST:
C     APE  - EXNER FUNCTION 
C     ZINT - INTERFACE HEIGHTS
C     CKLQ - MASK VALUE
C
C   OUTPUT ARGUMENT LIST: 
C     NONE
C     
C   OUTPUT FILES:
C     NONE
C     
C   SUBPROGRAMS CALLED:
C     SFLX
C
C     UTILITIES:
C       NONE
C     LIBRARY:
C       COMMON   - CTLBLK
C                  LOOPS
C                  MASKS
C                  PHYS
C                  VRBLS
C                  PVRBLS
C                  SOIL
C                  ACMSFC
C                  ACMPRE
C                  ACMRDS
C                  ACMRDL
C                  OPTIONS
C
C     
C   ATTRIBUTES:
C     LANGUAGE: FORTRAN 90
C     MACHINE : IBM SP
C$$$  
C     
C     SET LOCAL PARAMETERS.
C-----------------------------------------------------------------------
                          P A R A M E T E R
     & (EPSWET=.001
     &, PQ0=379.90516,SEAFC=.98,TRESH=.95
     &, A2=17.2693882,A3=273.16,A4=35.86
     &, T0=273.16,T1=274.16,CAPA=0.28589641
     &, CP=1004.6,STBOL=5.67E-8,R=287.04,ROW=1.E3
     &, ELWV=2.50E6,ELIV=2.834E6,ELIW=.334E6)
C
                          P A R A M E T E R
     & (A23M4=A2*(A3-A4),PQ0SEA=PQ0*SEAFC,PQ0C=PQ0*TRESH
     &, RLIVWV=ELIV/ELWV,ROWLIW=ROW*ELIW,ROWLIV=ROW*ELIV)
C-----------------------------------------------------------------------
C***  INCLUDE GLOBAL PARAMETERS.
C-----------------------------------------------------------------------
      INCLUDE "parmeta"
      INCLUDE "parm.tbl"
      INCLUDE "parmsoil"
      INCLUDE "mpp.h"
#include "sp.h"
C-----------------------------------------------------------------------
C***  SET LOCAL PARAMETERS DEPENDENT ON GLOBAL PARAMETERS.
C-----------------------------------------------------------------------
                          P A R A M E T E R
     & (LP1=LM+1,JAM=6+2*(JM-10))
C-----------------------------------------------------------------------
                          L O G I C A L
     & RUN,FIRST,RESTRT,SIGMA
C-----------------------------------------------------------------------
                          D I M E N S I O N
     & ZLM   (idim1:idim2,jdim1:jdim2)
     &,PS    (idim1:idim2,jdim1:jdim2),APES  (idim1:idim2,jdim1:jdim2)
     &,ETALM (idim1:idim2,jdim1:jdim2),PLM   (idim1:idim2,jdim1:jdim2)
     &,APELM (idim1:idim2,jdim1:jdim2),RDSIN (idim1:idim2,jdim1:jdim2)
     &,TLM   (idim1:idim2,jdim1:jdim2),THLM  (idim1:idim2,jdim1:jdim2)
     &,QLM   (idim1:idim2,jdim1:jdim2),QLMS  (idim1:idim2,jdim1:jdim2)
     &,DQSDT (idim1:idim2,jdim1:jdim2)
     &,CKLQ  (idim1:idim2,jdim1:jdim2)
     &,FFS   (idim1:idim2,jdim1:jdim2),QFC1  (idim1:idim2,jdim1:jdim2)
     &,APE   (idim1:idim2,jdim1:jdim2,LM)
     &,ZINT  (idim1:idim2,jdim1:jdim2,LP1)
C-----------------------------------------------------------------------
                          D I M E N S I O N
c Ek 18 Jan 2000 - add SH2OK array
     & SMCK  (NSOIL),STCK  (NSOIL), SH2OK  (NSOIL)
C-----------------------------------------------------------------------
C
c Ek 10 Feb 2000 - add declarations
C DECLARATIONS
C
      LOGICAL LFIRST
      LOGICAL LFIRSTa
C
      INTEGER ICE
      INTEGER ISLTPK
      INTEGER IVGTPK
      INTEGER NSOIL
      INTEGER ISPTPK
C
      REAL ALB
      REAL ALB2D
      REAL ALBASE
      REAL ALBEDO
      REAL DQSDTK
      REAL DTK
      REAL CHK
      REAL CMCK
      REAL ELFLX
      REAL GFLX
      REAL HFLX
      REAL LWDN
Cdule      REAL MXSNAL
      REAL PLFLX
      REAL PRCP
      REAL PTU
      REAL Q1K
      REAL Q2K
      REAL Q2SAT
      REAL RNOF1K
      REAL RNOF2K
      REAL SFCPRS
      REAL SFCSPD
      REAL SFCTH2
      REAL SFCTMP
      REAL SH2OK
      REAL SI
      REAL SLDPTH
      REAL SMCK
      REAL SMELTK
      REAL SNDENS
      REAL SNO
      REAL SNOALB
      REAL SNODPK
      REAL SNOWH
      REAL SOILQM
      REAL SOILQW
      REAL SOLDN
      REAL STCK
      REAL T1K
      REAL TBOT
      REAL VGFRCK
      REAL Z
C-----------------------------------------------------------------------
C***  INCLUDE COMMON BLOCKS.
C***     COMMON BLOCKS SOIL, ACMPRE, ACMSFC WERE ADDED FOR GCIP.
C***     COMMON BLOCK OPTIONS WAS ADDED FOR THE POST.
C-----------------------------------------------------------------------
      INCLUDE "CTLBLK.comm"
C-----------------------------------------------------------------------
      INCLUDE "LOOPS.comm"
C-----------------------------------------------------------------------
      INCLUDE "MASKS.comm"
C-----------------------------------------------------------------------
      INCLUDE "PHYS.comm"
C-----------------------------------------------------------------------
      INCLUDE "VRBLS.comm"
C-----------------------------------------------------------------------
      INCLUDE "PVRBLS.comm"
C-----------------------------------------------------------------------
      INCLUDE "SOIL.comm"
C-----------------------------------------------------------------------
      INCLUDE "ACMPRE.comm"
C-----------------------------------------------------------------------
      INCLUDE "ACMSFC.comm"
C-----------------------------------------------------------------------
      INCLUDE "ACMRDS.comm"
C-----------------------------------------------------------------------
      INCLUDE "ACMRDL.comm"
C-----------------------------------------------------------------------
      INCLUDE "OPTIONS.comm"
C-----------------------------------------------------------------------
                          C H A R A C T E R 
     &  WORD*80
      DATA    LFIRST /.TRUE./
C     DATA    LFIRST /.FALSE./
      DATA    LFIRSTa /.TRUE./
C***********************************************************************
C                         START SURFCE HERE
C
C***  INITIALIZE SOME WORKING ARRAYS
C
      CALL ZERO2(QLM)
      CALL ZERO2(QLM)
      CALL ZERO2(QLMS)
C***
C***  SET CONSTANTS CALCULATED HERE FOR CLARITY.
C***
      FDTLIW=DTQ2/ROWLIW
      FDTLIV=DTQ2/ROWLIV
      FDTW=DTQ2/2.5E9
C***
C***  SET NOAH LSM CONSTANTS AND TIME INDEPENDENT VARIABLES
C***  INITIALIZE NOAH LSM HISTORICAL VARIABLES
C***
C-----------------------------------------------------------------------
      IF(NTSD.LT.NPHS)THEN
!$omp parallel do private(i,j)
        DO 50 J=MYJS,MYJE
        DO 50 I=MYIS,MYIE
        PS(I,J)=PD(I,J)+PT
        APES(I,J)=(1.E5/PS(I,J))**CAPA
        PCTSNO(I,J)=-999.0

C
C ----------------------------------------------------------------------
C Set default values for sea-ice or ocean states
C open ocean, SM=1
C sea-ice, SM=0, SICE=1
C land, SM=0, SICE=0
C*** SET ZERO-VALUE FOR SOME OUTPUT DIAGNOSTIC ARRAYS
        IF(SM(I,J).LT.0.5)THEN
          IF(SICE(I,J).GT.0.5) THEN
C***        SEA-ICE CASE
            SMSTAV(I,J)=1.0
            SMSTOT(I,J)=1.0
            SSROFF(I,J)=0.0
            BGROFF(I,J)=0.0
            CMC(I,J)=0.0
            DO NS=1,NSOIL
              SMC(I,J,NS)=1.0
              SH2O(I,J,NS)=1.0
            ENDDO
          ENDIF
        ELSE
C***       Water Case
          SMSTAV(I,J)=1.0
          SMSTOT(I,J)=1.0
          SSROFF(I,J)=0.0
          BGROFF(I,J)=0.0
          SOILTB(I,J)=280.99
          GRNFLX(I,J)=0.
          SUBSHX(I,J)=0.0
          ACSNOW(I,J)=0.0
          ACSNOM(I,J)=0.0
          SNOPCX(I,J)=0.0
          CMC(I,J)=0.0
          SNO(I,J)=0.0
c add SI (snow depth), ^^^SNO=snow water equivalent
          SI(I,J)=0.0
          DO NS=1,NSOIL
            SMC(I,J,NS)=1.0
            SH2O(I,J,NS)=1.0
            STC(I,J,NS)=273.16
          ENDDO
        ENDIF
C
   50   CONTINUE
      ENDIF
C-----------------------------------------------------------------------
C***
C***  SET LOWEST MODEL LAYER VARIABLES.
C***
!$omp parallel do private(i,j,llmh)
      DO 100 J=MYJS2,MYJE2
      DO 100 I=MYIS,MYIE
      LLMH=LMH(I,J)
      ETALM(I,J)=AETA(LLMH)
      APELM(I,J)=APE(I,J,LLMH)
      TLM(I,J)=T(I,J,LLMH)
      QLM(I,J)=Q(I,J,LLMH)
      ZLM(I,J)=(ZINT(I,J,LLMH)-ZINT(I,J,LLMH+1))*0.50
  100 CONTINUE
C
!$omp parallel do private(i,j)
      DO 110 J=MYJS2,MYJE2
      DO 110 I=MYIS,MYIE
      PS(I,J)=PD(I,J)+PT
      APES(I,J)=(1.E5/PS(I,J))**CAPA
      PLM(I,J)=ETALM(I,J)*PD(I,J)*RES(I,J)+PT
      QLMS(I,J)=((1.-SM(I,J))*PQ0+SM(I,J)*PQ0SEA)
     1           /PLM(I,J)*EXP(A2*(TLM(I,J)-A3)/(TLM(I,J)-A4))
      DQSDT(I,J)=QLMS(I,J)*A23M4/(TLM(I,J)-A4)**2
      FFSK=AKHS(I,J)*PLM(I,J)*HBM2(I,J)/((QLM(I,J)*.608+1.)*TLM(I,J)*R)
      QFC1(I,J)=APES(I,J)*FFSK*ELWV
      FFS(I,J)=FFSK*CP
  110 CONTINUE
C-----------------------------------------------------------------------
!$omp parallel do private(i,j,factrs,factrl,tlmh)
      DO 120 J=MYJS2,MYJE2
      DO 120 I=MYIS,MYIE
C***
C***  COMPUTE RADIN AND RDSIN FOR THIS TIMESTEP
C***  CZEN IS IN PHYS COMMON AND IS CURRENT FROM CALL TO RDTEMP
C***
      IF(CZMEAN(I,J).GT.0.)THEN
        FACTRS=CZEN(I,J)/CZMEAN(I,J)
      ELSE
        FACTRS=0.
      ENDIF
C
      IF(SIGT4(I,J).GT.0.)THEN
        TLMH=TLM(I,J)
        FACTRL=STBOL*TLMH*TLMH*TLMH*TLMH/SIGT4(I,J)
      ELSE
        FACTRL=0.
      ENDIF
C
c Ek 10 feb 2000 - RADIN no longer needed in SFLX (via FK)
c now use RADIN array for incoming longwave
c perhaps change the name later to e.g. RDLIN for consistency
c      RADIN(I,J)=((RSWIN(I,J)-RSWOUT(I,J))*FACTRS+
c     &            RLWIN(I,J)*FACTRL)*HBM2(I,J)
c     if(i.eq.25.and.j.eq.31.and.mype.eq.1) then
c       print*,'RLWIN,FACTRL,HBM2=',rlwin(i,j),factrl,hbm2(i,j)
c     endif
      RADIN(I,J)= RLWIN(I,J)*FACTRL*HBM2(I,J)
      RDSIN(I,J)= RSWIN(I,J)*FACTRS*HBM2(I,J)
C***
C***  DIAGNOSTIC RADIATION ACCUMULATION
C***
      ASWIN (I,J)=ASWIN (I,J)+RSWIN (I,J)*HBM2(I,J)*FACTRS
      ASWOUT(I,J)=ASWOUT(I,J)-RSWOUT(I,J)*HBM2(I,J)*FACTRS
      ASWTOA(I,J)=ASWTOA(I,J)+RSWTOA(I,J)*HBM2(I,J)*FACTRS
      ALWIN (I,J)=ALWIN (I,J)+RLWIN (I,J)*HBM2(I,J)*FACTRL
      ALWOUT(I,J)=ALWOUT(I,J)-RADOT (I,J)*HBM2(I,J)
      ALWTOA(I,J)=ALWTOA(I,J)+RLWTOA(I,J)*HBM2(I,J)
C***
C***  CHECK FOR SATURATION AT THE LOWEST MODEL LEVEL
C***
      IF((QLM(I,J).GE.QLMS(I,J)*TRESH).AND.(QLM(I,J).LT.QZ0(I,J)))THEN
        CKLQ(I,J)=0.
      ELSE
        CKLQ(I,J)=HBM2(I,J)
      ENDIF
 120  CONTINUE
C-----------------------------------------------------------------------
C***
C***  THS, THLM, CHEATING WET FOR PROFS
C***
!$omp parallel do private(i,j)
      DO 130 J=MYJS2,MYJE2
      DO 130 I=MYIS,MYIE
      THLM(I,J)=TLM(I,J)*APELM(I,J)
      QFC1(I,J)=QFC1(I,J)*CKLQ(I,J)
  130 CONTINUE
C
c Ek 10 feb 2000 - update these private statements
c add new variables:  alb2d,snoalb,alb,ISPTPK
c remove old variables no longer needed:  
C
!!$omp parallel do
!!$omp&  private(chk,chkff,cmck,dqsdtk,dtk,elflx,fk,gflx)
!!$omp&  private (hflx,i,ice,isltpk,ivgtpk,j,ns,plflx,prcp)
!!$omp&  private (q1k,q2k,q2sat,rnof1k,rnof2k,satflg,scheck)
!!$omp&  private (sfcprs,sfcth2,sfctmp,smck,smeltk,snodpk)
!!$omp&  private (soilqm,soilqw,soldn,stck,t1k,tbot,vgfrck,z)
c Ek 10 feb 2000 - private statements
c add new variables
c remove old variables no longer needed later
!!$omp&  private (lwdn,sh2ok,alb2d,snoalb,alb,ISPTPK,snowh)
C
C Ek 18 jan 2000 - temporarily set ISPTPK=1 (2-D fixed field: x,y)
c comes from ISLSCP data set 2-d fixed field
        ISPTPK=1
C ----------------------------------------------------------------------
C Begin main 'workhorse' loop over entire model domain
C ----------------------------------------------------------------------
      DO 160 J=MYJS2,MYJE2
      DO 155 I=MYIS,MYIE
C ----------------------------------------------------------------------
      IF(HBM2(I,J).LT.0.5)GO TO 155
c Check to see that when ocean, ALBASE=ALBEDO=0.06,MXSNAL=0,
      IF (SM(I,J) .GT. 0.5) THEN
          IF ((ALBASE(I,J) .NE. 0.0) .OR.
     .       (ALBEDO(I,J) .LT. 0.059) .OR.
     .       (ALBEDO(I,J) .GT. 0.061) .OR.
     .       (MXSNAL(I,J) .NE. 0.0) ) THEN	    
            WRITE(6,*)'ALBo:I,J,MYPE,ICE,SNODPK,ALB,ALB2D,SNOALB=',
     .      I,J,MYPE,ICE,SNO(I,J),ALBASE(I,J),ALBEDO(I,J),MXSNAL(I,J)        
 	    IERR1=1
          ENDIF
      ENDIF
C ----------------------------------------------------------------------
      IF(SM(I,J).GT.0.5)THEN
        THS(I,J)=SST(I,J)*APES(I,J)
        QS(I,J)=HBM2(I,J)*PQ0SEA/PS(I,J)
     1         *EXP(A2*(THS(I,J)-A3*APES(I,J))/(THS(I,J)-A4*APES(I,J)))
      ENDIF
C ----------------------------------------------------------------------
C Land or sea-ice
C ----------------------------------------------------------------------
C***
C***  LOADING AND UNLOADING NOAH LSM LAND SOIL VARIABLES
C***
      IF(SM(I,J).LT.0.5)THEN
c        ICE=INT(SICE(I,J)+0.3)
        ICE=NINT(SICE(I,J))
c Ek 10 feb 2000 - SATFLG no longer needed in SFLX
c        SATFLG=CKLQ(I,J)
C
       DTK=DTQ2
        Z=ZLM(I,J)
c Ek 10 feb 2000 - FK no longer needed in SFLX
c        FK=RADIN(I,J)
c Ek 18 jan 2000 - add longwave radiation calc needed for call SFLX
        LWDN=RADIN(I,J)
        SOLDN=RDSIN(I,J)
        SFCPRS=PLM(I,J)
        PRCP=PREC(I,J)*ROW/DTQ2
        Q2K=QLM(I,J)
        Q2SAT=QLMS(I,J)
C ----------------------------------------------------------------------
C Q2K may slightly exceed Q2SAT in some cases due to atmospheric physics
C parameterizations previously called
        IF (Q2K .GT. Q2SAT) Q2K=Q2SAT
        DQSDTK=DQSDT(I,J)
        TBOT=TG(I,J)
        CHK=AKHS(I,J)
        CHKFF=FFS(I,J)
        IVGTPK=IVGTYP(I,J)
        ISLTPK=ISLTYP(I,J)
C  MEB  PREVENT ROUTINES IN SFLX FROM GOING OUT OF BOUNDS
        IF (IVGTPK.EQ.0) IVGTPK=13
        IF (ISLTPK.EQ.0) ISLTPK=9
C  MEB  PREVENT ROUTINES IN SFLX FROM GOING OUT OF BOUNDS
        VGFRCK=VEGFRC(I,J)
        Q1K=QS(I,J)
        SFCTMP=THLM(I,J)/APELM(I,J)
        SFCTH2=THLM(I,J)/APES(I,J)
        T1K=THS(I,J)/APES(I,J)
        CMCK=CMC(I,J)
        SNODPK=SNO(I,J)
c use 2-d prognostic field of snowdepth, SI(x,y) for
C SNOWH (local snowdepth variable)
C Ek 17 Jan 2001
        SNOWH=SI(I,J)
C
        DO 140 NS=1,NSOIL
          SMCK(NS)=SMC(I,J,NS)
c         print*,'i,j,ns,smc(i,j,ns),smck(ns)=',
c    *       i,j,ns,smc(i,j,ns),smck(ns)
C use 3-d prognostic field of liquid soil moisture, SH2O(x,y,4) for
C SH2OK(NS) (local liquid soil moisture variable)
C Ek 11 Jan 2001
          SH2OK(NS)=SH2O(I,J,NS)
c          print*,'mype,i,j,ns,sh2ok=',mype,i,j,ns,sh2ok(ns)
C
          STCK(NS)=STC(I,J,NS)
c         if(mype.eq.13) then
c          print*,'i,j,ns,stc(i,j,ns)=',i,j,ns,stc(i,j,ns) 
c         endif
  140   CONTINUE
C
c OLD CALL SFLX
C-----------------------------------------------------------------------
c        CALL SFLX
c     &     (ICE   ,SATFLG,DTK   ,Z, NSOIL, NROOT, SLDPTH
c     &,     FK    ,SOLDN ,SFCPRS,PRCP  ,SFCTMP,SFCTH2
c     &,     Q2K   ,Q2SAT ,DQSDTK,TBOT  ,CHK,   CHKFF
c     &,     IVGTPK,ISLTPK,VGFRCK
c     &,     PLFLX ,ELFLX ,HFLX  ,GFLX  ,RNOF1K,RNOF2K
c     &,     Q1K   ,SMELTK,T1K   ,CMCK  ,SMCK  ,STCK  ,SNODPK
c     &,     SOILQW,SOILQM )
C-----------------------------------------------------------------------
C
C Ek 18 jan 2000 - temporarily set ISPTPK=1 (2-D fixed field: x,y)
c comes from ISLSCP data set 2-d fixed field
c        ISPTPK=1
C SNOALB (fixed value, max snow albedo) from MXSNAL
c via 2-d fixed field from David Robinson
        SNOALB=MXSNAL(I,J)
C ALB (fixed value, snow-free albedo) from ALBASE
C via 2-d fixed field from Matthews
        ALB=ALBASE(I,J)
C Set dynamic albedo from the dynamic albedo 2-d array, which is updated
C only for the land in SFLX, not for sea-ice, so we must 'pass through'
C ALB2D=0.60 for sea-ice.
C turn this off, and instead, do it within SFLX
c        ALB2D=ALBEDO(I,J)
C
C ----------------------------------------------------------------------
c Initial range check for variables/parameters for entire I,J domain for
C first timestep (when LFIRST=true).  Set LFIRST=false after end of the
C 155/160 loop.

      IF (LFIRST) THEN
c      IF ( (LFIRST)            .OR.
c     .     (NTSD .EQ.       2) .OR.
c     .     (NTSD .EQ.       3) .OR.
c     .     (NTSD .EQ. NTSTM/2) .OR.
c     .     (NTSD .EQ.   NTSTM) ) THEN
C ----------------------------------------------------------------------
C land OR sea-ice checks
C land checks first
        IF (ICE .LT. 0.5) THEN
C ----------------------------------------------------------------------
C albedo checks
C ALB = ALBASE(I,J)  = snow free albedo
C   min = 0.11 (Matthews data base)
C   max = 0.80 (Matthews data base)
C ALBEDO(I,J) = dynamic albedo (=ALBASE when SNODPK=0)
C  (=ALB2D on return from SFLX)
C SNOALB = MAXSNAL(I,J) = maximum snow albedo
C   min = 0.21 (Robinson data base)
C   max = 0.75 (Robinson data base)
          IF ( (ALB         .GT.(SNOALB+0.051)) .OR.
     .         (ALB         .GT.(ALBEDO(I,J)+0.051)) .OR.
     .         (ALBEDO(I,J) .GT.(SNOALB+0.051)     ) ) THEN
            write(6,*)'ALBl1:I,J,MYPE,ICE,SNODPK,ALB,ALB2D,SNOALB=',
     .        I,J,MYPE,ICE,SNODPK,ALB,ALBEDO(I,J),SNOALB         
           IERR1=1
          ENDIF
          IF ( (ALB    .LT. 0.10) .OR.
     .         (ALB    .GT. 0.81) .OR.
     .         (SNOALB .LT. 0.20) .OR.
     .         (SNOALB .GT. 0.81) ) THEN
            write(6,*)'ALBl2:I,J,MYPE,ICE,SNODPK,ALB,ALB2D,SNOALB=',
     .        I,J,MYPE,ICE,SNODPK,ALB,ALBEDO(I,J),SNOALB         
           IERR1=1
          ENDIF
C ----------------------------------------------------------------------
C Veg,soil,slope type, veg fraction, No. soil layers checks
          IF ( ( (IVGTPK .LT. 1) .OR.  (IVGTPK .GT. 13) ) .OR.
     .         ( (ISLTPK .LT. 1) .OR.  (ISLTPK .GT.  9) ) .OR.
     .                       (ISPTPK .NE. 1)              .OR.
     .         ( (VGFRCK .LT. 0.) .OR. (VGFRCK .GT. 1.) ) .OR.
     .                       (NSOIL .NE. 4)               ) THEN
           WRITE(6,*)'LANDSFC:I,J,MYPE,IVGTPK,ISLTPK,ISPTPK,VGFRCK=',
     .        I,J,MYPE,IVGTPK,ISLTPK,ISPTPK,VGFRCK
             IERR1=1
          ENDIF
C ----------------------------------------------------------------------
          DO index=1,NSOIL
c         do index=1,nsoil
C ----------------------------------------------------------------------
C debug
              indexp = min(index+1,nsoil)
              indexm = max(index-1,1)
C ----------------------------------------------------------------------
C Soil temp (STC) range check
c            IF ((STCK(index) .LT. 223.15) .OR.
            IF ((STCK(index) .LT. 200.00) .OR.
     .          (STCK(index) .GT. 323.15)) THEN
              write(6,*)'STCl:INDEX,I,J,MYPE,STC=',
     .          index,I,J,MYPE,STCK(index)
               IERR1=2
            ENDIF
C ----------------------------------------------------------------------
C Total soil moisture (SMC) check
            IF ( (SMCK(index) .LT. 0.02) .OR.
     .           (SMCK(index) .GT. 0.468) ) THEN
              write(6,*)'SMC:INDEX,I,J,MYPE,STCK,SMC,SH2O=',
     .        index,I,J,MYPE,STCK(index),SMCK(index),SH2OK(index)
               IERR1=1
            ENDIF
C ----------------------------------------------------------------------
C Liquid soil moisture<=total soil moisture (SH2O<=SMC) maximum check
            IF ( (SH2OK(index) .LT. 0.02) .OR.
     .           (SH2OK(index) .GT. SMCK(index)) ) THEN
              write(6,*)'aSH2Ol1:INDEX,I,J,MYPE,ICE,STCK,SMC,SH2O=',
     .        index,I,J,MYPE,ICE,STCK(index),SMCK(index),SH2OK(index)
               IERR1=1
            ENDIF
C ----------------------------------------------------------------------
C Note SH2O, SMC when STC > +1.5C
  	    IF (STCK(index) .GT. T0+1.5) THEN
  	      IF (SMCK(index)-SH2OK(index) .GT. 0.03) THEN
  		write(6,*)'SH2Ol2a:INDEX,I,J,MYPE,STCK,SMC,SH2O=',
     .  	  index,I,J,MYPE,STCK(index),SMCK(index),SH2OK(index)
          write(6,*)'SH2Ol2a:I,J,MYPE,N-1,N,N+1,
     .    STCK(N-1),STCK(N),STCK(N+1), =',
     .    I,J,MYPE,indexm,index,indexp,
     .    STCK(indexm),STCK(index),STCK(indexp)
 	      IERR1=1
                ENDIF
  	    ELSEIF (STCK(index) .LT. T0-0.5) THEN
C Note SH2O, SMC when STC < -0.5C
  	      IF (SMCK(index)-SH2OK(index) .LT. 0.005) THEN
  		write(6,*)'SH2Ol2b:INDEX,I,J,MYPE,STCK,SMC,SH2O=',
     .  	  index,I,J,MYPE,STCK(index),SMCK(index),SH2OK(index)
          write(6,*)'SH2Ol2b:I,J,MYPE,N-1,N,N+1,
     .    STCK(N-1),STCK(N),STCK(N+1), =',
     .    I,J,MYPE,indexm,index,indexp,
     .    STCK(indexm),STCK(index),STCK(indexp)
 	      IERR1=1
  	      ENDIF
  	    ENDIF
C ----------------------------------------------------------------------
c         print*,'index at the end of loop=',i,j,index
          END DO
C ----------------------------------------------------------------------
C Soil column bottom temp (TBOT) check
c	  IF ((TBOT .LT. 223.15) .OR. (TBOT .GT. 323.15)) THEN 
	  IF ((TBOT .LT. 200.00) .OR. (TBOT .GT. 323.15)) THEN 
            write(6,*)'TBOTl:INDEX,I,J,MYPE,TBOT=',
     .        index,I,J,MYPE,TBOT
               IERR2=1
          ENDIF
C ----------------------------------------------------------------------
C sfc/skin temp (T1K) check
c 	  IF ((T1K .LT. 223.15) .OR. (T1K .GT. 323.15)) THEN 
 	 IF ((T1K .LT. 200.00) .OR. (T1K .GT. 323.15)) THEN 
             write(6,*)'T1:INDEX,I,J,MYPE,T1=',
     .        index,I,J,MYPE,T1K
               IERR2=1
          ENDIF
C ----------------------------------------------------------------------
C Canopy water content (CMC) check
	  IF ((CMCK .LT. 0.) .OR. (CMCK .GT. 0.5E-3)) THEN 
            write(6,*)'CMC:INDEX,I,J,MYPE,CMC=',
     .        index,I,J,MYPE,CMCK
c              IERR1=1
          ENDIF
C ----------------------------------------------------------------------
C Snow water equivalent, snow depth check
 	  IF (((SNODPK .GT. 0.) .AND. (SNOWH .LT. 1.0E-09)) .OR.
     .        ((SNODPK .LT. 1.0E-09) .AND. (SNOWH .GT. 0.)) .OR.
     .         (SNODPK .GT. SNOWH)) THEN
 	    WRITE(6,*)'SNOW:I,J,MYPE,SNODPK,SNOWH=',
     .        I,J,MYPE,SNODPK,SNOWH
 	       IERR1=1
           ENDIF
C ----------------------------------------------------------------------
C Snow density check
	  IF (SNODPK .GT. 0.) THEN
            SNDENS=SNODPK/SNOWH
            IF (SNDENS .LT. 0.05) THEN
	      WRITE(6,*)'SNDENS<5%:I,J,MYPE,SNODPK,SNOWH,SNDENS=',
     .          I,J,MYPE,SNODPK,SNOWH,SNDENS
              IERR1=1
            ENDIF
            IF (SNDENS .GT. 0.42) THEN
	      WRITE(6,*)'SNDENS>42%:I,J,MYPE,SNODPK,SNOWH,SNDENS=',
     .          I,J,MYPE,SNODPK,SNOWH,SNDENS
              IERR1=1
            ENDIF
          ENDIF
C ----------------------------------------------------------------------
C end land checks
C ----------------------------------------------------------------------
        ELSE
C ----------------------------------------------------------------------
C sea-ice checks next
C ----------------------------------------------------------------------
C sea-ice bottom temp (TBOT) check
          IF ( (TBOT .LT. 271.159) .OR.
     .         (TBOT .GT. 271.161) ) THEN
            WRITE(6,*)'TBOTi:INDEX,I,J,MYPE,TBOT=',
     .        index,I,J,MYPE,TBOT
          ENDIF
C ----------------------------------------------------------------------
C sea-ice temp with depth (STC) range check
          DO index=1,4
c            IF ((STCK(index) .LT. 223.15) .OR.
            IF ((STCK(index) .LT. 200.00) .OR.
c     .          (STCK(index) .GT. 323.15)) THEN
     .          (STCK(index) .GT. 274.15)) THEN
              write(6,*)'STCi:INDEX,I,J,MYPE,STC=',
     .          index,I,J,MYPE,STCK(index)
c              IERR1=2
            ENDIF
          END DO
C ----------------------------------------------------------------------
C sfc/skin temp (T1K) check
c	  IF ((T1K .LT. 223.15) .OR. (T1K .GT. 323.15)) THEN 
	  IF ((T1K .LT. 200.00) .OR. (T1K .GT. 323.15)) THEN 
            write(6,*)'T1:INDEX,I,J,MYPE,ICE,T1=',
     .        index,I,J,MYPE,ICE,T1K
              IERR2=1
          ENDIF
C ----------------------------------------------------------------------
C Snow water equivalent, snow depth check
	  IF (((SNODPK .GT. 0.) .AND. (SNOWH .LT. 1.0E-09)) .OR.
     .        ((SNODPK .LT. 1.0E-09) .AND. (SNOWH .GT. 0.)) .OR.
     .         (SNODPK .GT. SNOWH)) THEN
	    WRITE(6,*)'SNOW:I,J,MYPE,SNODPK,SNOWH=',
     .        I,J,MYPE,SNODPK,SNOWH
	       IERR1=1
          ENDIF
C ----------------------------------------------------------------------
C Snow density check
	  IF (SNODPK .GT. 0.) THEN
            SNDENS=SNODPK/SNOWH
            IF (SNDENS .LT. 0.05) THEN
	      WRITE(6,*)'SNDENS<5%:I,J,MYPE,SNODPK,SNOWH,SNDENS=',
     .          I,J,MYPE,SNODPK,SNOWH,SNDENS
              IERR1=1
            ENDIF
            IF (SNDENS .GT. 0.42) THEN
	      WRITE(6,*)'SNDENS>42%:I,J,MYPE,SNODPK,SNOWH,SNDENS=',
     .          I,J,MYPE,SNODPK,SNOWH,SNDENS
              IERR1=1
            ENDIF
          ENDIF
C ----------------------------------------------------------------------
c Check to see that when sea-ice, ALBASE=ALBEDO=0.65,MXSNAL=0,
          IF ( (ALB         .NE. 0.0) .OR.
     .         (ALBEDO(I,J) .LT. 0.64) .OR.
     .         (ALBEDO(I,J) .GT. 0.66) .OR.
     .         (SNOALB      .NE. 0.0) ) THEN         
	    WRITE(6,*)'ALBi:I,J,MYPE,ICE,SNODPK,ALB,ALB2D,SNOALB=',
     .        I,J,MYPE,ICE,SNODPK,ALB,ALBEDO(I,J),SNOALB         
              IERR1=1
          ENDIF
C ----------------------------------------------------------------------
c Check to see that when sea-ice, SH2O=SMC=1.0
          DO index=1,4
            IF ( (SMCK(index) .NE.          1.0) .OR.
     .           (SMCK(index) .NE. SH2OK(index)) ) THEN
              write(6,*)'SMCi:INDEX,I,J,MYPE,ICE,STCK,SMC,SH2O=',
     .          index,I,J,MYPE,ICE,STCK(index),SMCK(index),SH2OK(index)
              IERR1=1
            ENDIF
          END DO
C ----------------------------------------------------------------------
C check to see that veg type=soil type=veg frac=0
	  IF ( (IVGTYP(I,J) .NE. 0 ) .OR.
     .         (ISLTYP(I,J) .NE. 0 ) .OR.
     .         (VGFRCK .GT. 0.) ) THEN
	    WRITE(6,*)'IVGTYPi:I,J,MYPE,IVGTYP,ISLTYP,VGFRCK=',
     .        I,J,MYPE,IVGTYP(I,J),ISLTYP(I,J),VGFRCK
	    WRITE(6,*)'IVGTYPi:I,J,MYPE,SNODPK,ALB,ALBEDO,SNOALB=',
     .        I,J,MYPE,SNODPK,ALB,ALBEDO(I,J),SNOALB
	  ENDIF
C ----------------------------------------------------------------------
C end sea-ice checks
C end of separate land AND sea-ice checks
C ----------------------------------------------------------------------
        ENDIF
C ----------------------------------------------------------------------
C both land AND sea-ice checks
C Snow water equivalent, snow depth check
	  IF (((SNODPK .GT. 0.) .AND. (SNOWH .LE. 0.)) .OR.
     .        ((SNODPK .LE. 0.) .AND. (SNOWH .GT. 0.)) .OR.
     .         (SNODPK .GT. SNOWH)) THEN
	    WRITE(6,*)'SNOW:I,J,MYPE,SNODPK,SNOWH=',
     .        I,J,MYPE,SNODPK,SNOWH
c	       IERR1=1
          ENDIF
C ----------------------------------------------------------------------
C Snow density check
	  IF (SNODPK .GT. 0.) THEN
            SNDENS=SNODPK/SNOWH
            IF (SNDENS .LT. 0.05) THEN
	      WRITE(6,*)'SNDENS<5%:I,J,MYPE,SNODPK,SNOWH,SNDENS=',
     .          I,J,MYPE,SNODPK,SNOWH,SNDENS
c              IERR1=1
            ENDIF
            IF (SNDENS .GT. 0.42) THEN
	      WRITE(6,*)'SNDENS>42%:I,J,MYPE,SNODPK,SNOWH,SNDENS=',
     .          I,J,MYPE,SNODPK,SNOWH,SNDENS
c              IERR1=1
            ENDIF
          ENDIF
C ----------------------------------------------------------------------
C sfc/skin temp (T1K) check
c	  IF ((T1K .LT. 223.15) .OR. (T1K .GT. 323.15)) THEN 
	  IF ((T1K .LT. 200.00) .OR. (T1K .GT. 323.15)) THEN 
            write(6,*)'T1:INDEX,I,J,MYPE,T1=',
     .        index,I,J,MYPE,T1K
c              IERR2=1
          ENDIF
C ----------------------------------------------------------------------
c check to see that 223.15K (-50C) =< SFCTMP,SFCTH2 <= 323.15K (+50C) 
C SFCTMP = lowest model level temp
C SFCTH2 = lowest model level pot temp
        IF ( ( (SFCTMP .LT. 223.15) .OR. (SFCTMP .GT. 323.15) ) .OR.
     .       ( (SFCTH2 .LT. 223.15) .OR. (SFCTH2 .GT. 323.15) ) ) THEN
	  WRITE(6,*)'SFCTMP:I,J,MYPE,SFCTMP,SFCTH2=',
     .      I,J,MYPE,SFCTMP,SFCTH2
        ENDIF
C ----------------------------------------------------------------------
c check to see that 0W/m2 =< LWDN <= 500W/m2
c check to see that 0W/m2 =< SOLDN <= 1200W/m2
C LWDN  = downward longwave radiation
C SOLDN = downward solar radiation
        IF ( ( (LWDN  .LT. 0.) .OR. (LWDN  .GT.  500.) ) .OR.
     .      ( (SOLDN .LT. 0.) .OR. (SOLDN .GT. 1200.) ) ) THEN
          WRITE(6,*)'LWSOLDN:I,J,MYPE,LWDN,SOLDN=',
     .      I,J,MYPE,LWDN,SOLDN
        ENDIF
C ----------------------------------------------------------------------
c check to see that 0g/kg < Q2K,Q2SAT <= 40g/kg
c check to see that Q2K <= Q2SAT
C Q2K   = lowest model level spec hum
C Q2SAT = lowest model level sat spec hum
c        IF ( ( (Q2K   .LE. 0.) .OR. (Q2K   .GT. 0.04) ) .OR.
c     .       ( (Q2SAT .LE. 0.) .OR. (Q2SAT .GT. 0.04) ) .OR.
        IF ( ( (Q2K   .LE. 0.) .OR. (Q2K   .GT. 0.05) ) .OR.
     .       ( (Q2SAT .LE. 0.) .OR. (Q2SAT .GT. 0.05) ) .OR.
     .       (Q2K .GT. Q2SAT) ) THEN
	  WRITE(6,*)'Q2:I,J,MYPE,Q2K,Q2SAT=',
     .      I,J,MYPE,Q2K,Q2SAT
        ENDIF
C ----------------------------------------------------------------------
c check to see that 600mb =< SFCPRS <= 1050mb
C SFCPRS = surface pressure (Pa)
       IF ( (SFCPRS .LE. 60000.) .OR. (SFCPRS .GT. 105000.) ) THEN
         WRITE(6,*)'SFCPRS:I,J,MYPE,SFCPRS=',
     .     I,J,MYPE,SFCPRS
       ENDIF
C ----------------------------------------------------------------------
c check to see that 0 =< PRCP <= 0.04 KG M-2 S-1 (=0.04mm/s = 144mm/hr)
C PRCP = precip rate (KG M-2 S-1)
       IF ( (PRCP .LT. 0.) .OR. (PRCP .GT. 0.04) ) THEN
         WRITE(6,*)'PRCP:I,J,MYPE,PRCP=',
     .     I,J,MYPE,PRCP
       ENDIF
C ----------------------------------------------------------------------
c check to see that 0m/s < CHK <= 0.1m/s 
C CHK = sfc heat exchange coeff (m/s)
       IF ( (CHK .LE. 0.) .OR. (CHK .GT. 0.1) ) THEN
	 WRITE(6,*)'CH:I,J,MYPE,CHK=',
     .     I,J,MYPE,CHK
       ENDIF
C ----------------------------------------------------------------------
       IF (IERR2 .EQ. 1) THEN
          WRITE(6,*) 'RANGE CHECK IN SURFCE: EXTREME VALUES BEFORE SFLX'
       ENDIF
       IF (IERR1 .EQ. 1) THEN
c 	  WRITE(6,*) 'RANGE CHECK FAILURE IN SURFCE - STOP'
c	  STOP
       ENDIF
C ----------------------------------------------------------------------
C End of initial (logical LFIRST) range check for variables/parameters.
      ENDIF
C ----------------------------------------------------------------------
C Ek 18 jan 2000 - NEW CALL SFLX
        CALL SFLX
     I    (ICE,DTK,Z,NSOIL,SLDPTH,
     I    LWDN,SOLDN,SFCPRS,PRCP,SFCTMP,SFCTH2,Q2K,SFCSPD,Q2SAT,DQSDTK,
     I    IVGTPK,ISLTPK,ISPTPK,
     I    VGFRCK,PTU,TBOT,ALB,SNOALB,
     2    CMCK,T1K,STCK,SMCK,SH2OK,SNOWH,SNODPK,ALB2D,CHK,CMK,
     O    PLFLX,ELFLX,HFLX,GFLX,RNOF1K,RNOF2K,Q1K,SMELTK,
     O    SOILQW,SOILQM,DUM1,DUM2,DUM3,DUM4)
C-----------------------------------------------------------------------
      IF (LFIRSTa) THEN
c      IF ( (LFIRST)            .OR.
c     .     (NTSD .EQ.       2) .OR.
c     .     (NTSD .EQ.       3) .OR.
c     .     (NTSD .EQ. NTSTM/2) .OR.
c     .     (NTSD .EQ.   NTSTM) ) THEN
C ----------------------------------------------------------------------
C land OR sea-ice checks
C land checks first
        IF (ICE .LT. 0.5) THEN
C ----------------------------------------------------------------------
C albedo checks
C ALB = ALBASE(I,J)  = snow free albedo
C   min = 0.11 (Matthews data base)
C   max = 0.80 (Matthews data base)
C ALBEDO(I,J) = dynamic albedo (=ALBASE when SNODPK=0)
C  (=ALB2D on return from SFLX)
C SNOALB = MAXSNAL(I,J) = maximum snow albedo
C   min = 0.21 (Robinson data base)
C   max = 0.75 (Robinson data base)
          IF ( (ALB         .GT.(SNOALB+0.051)) .OR.
     .         (ALB         .GT.(ALBEDO(I,J)+0.051)) .OR.
     .         (ALBEDO(I,J) .GT.(SNOALB+0.051)     ) ) THEN
	    write(6,*)'ALBl1:I,J,MYPE,ICE,SNODPK,ALB,ALB2D,SNOALB=',
     .        I,J,MYPE,ICE,SNODPK,ALB,ALBEDO(I,J),SNOALB         
          IERR1=1
          ENDIF
          IF ( (ALB    .LT. 0.10) .OR.
     .         (ALB    .GT. 0.81) .OR.
     .         (SNOALB .LT. 0.20) .OR.
     .         (SNOALB .GT. 0.81) ) THEN
	    write(6,*)'ALBl2:I,J,MYPE,ICE,SNODPK,ALB,ALB2D,SNOALB=',
     .        I,J,MYPE,ICE,SNODPK,ALB,ALBEDO(I,J),SNOALB         
          IERR1=1
          ENDIF
C ----------------------------------------------------------------------
C Veg,soil,slope type, veg fraction, No. soil layers checks
	  IF ( ( (IVGTPK .LT. 1) .OR.  (IVGTPK .GT. 13) ) .OR.
     .         ( (ISLTPK .LT. 1) .OR.  (ISLTPK .GT.  9) ) .OR.
     .                       (ISPTPK .NE. 1)              .OR.
     .         ( (VGFRCK .LT. 0.) .OR. (VGFRCK .GT. 1.) ) .OR.
     .                       (NSOIL .NE. 4)               ) THEN
	    WRITE(6,*)'LANDSFC:I,J,MYPE,IVGTPK,ISLTPK,ISPTPK,VGFRCK=',
     .        I,J,MYPE,IVGTPK,ISLTPK,ISPTPK,VGFRCK
            IERR1=1
	  ENDIF
C ----------------------------------------------------------------------
          DO index=1,NSOIL
C ----------------------------------------------------------------------
C debug
            indexp = min(index+1,nsoil)
            indexm = max(index-1,1)
C ----------------------------------------------------------------------
C Soil temp (STC) range check
c            IF ((STCK(index) .LT. 223.15) .OR.
            IF ((STCK(index) .LT. 200.00) .OR.
     .          (STCK(index) .GT. 323.15)) THEN
              write(6,*)'STCl:INDEX,I,J,MYPE,STC=',
     .          index,I,J,MYPE,STCK(index)
              IERR1=2
            ENDIF
C ----------------------------------------------------------------------
C Total soil moisture (SMC) check
            IF ( (SMCK(index) .LT. 0.02) .OR.
     .           (SMCK(index) .GT. 0.468) ) THEN
              write(6,*)'SMC:INDEX,I,J,MYPE,STCK,SMC,SH2O=',
     .          index,I,J,MYPE,STCK(index),SMCK(index),SH2OK(index)
              IERR1=1
            ENDIF
C ----------------------------------------------------------------------
C Liquid soil moisture<=total soil moisture (SH2O<=SMC) maximum check
            IF ( (SH2OK(index) .LT. 0.02) .OR.
     .           (SH2OK(index) .GT. SMCK(index)) ) THEN
              write(6,*)'bSH2Ol1:INDEX,I,J,MYPE,ICE,STCK,SMC,SH2O=',
     .          index,I,J,MYPE,STCK(index),SMCK(index),SH2OK(index)
              IERR1=1
            ENDIF
C ----------------------------------------------------------------------
C Note SH2O, SMC when STC > +1.5C
	    IF (STCK(index) .GT. T0+1.5) THEN
	      IF (SMCK(index)-SH2OK(index) .GT. 0.005) THEN
		write(6,*)'SH2Ol2a:INDEX,I,J,MYPE,STCK,SMC,SH2O=',
     .  	  index,I,J,MYPE,STCK(index),SMCK(index),SH2OK(index)
        write(6,*)'SH2Ol2a:I,J,MYPE,N-1,N,N+1,
     .    STCK(N-1),STCK(N),STCK(N+1), =',
     .    I,J,MYPE,indexm,index,indexp,
     .    STCK(indexm),STCK(index),STCK(indexp)
	      IERR1=1
              ENDIF
	    ELSEIF (STCK(index) .LT. T0-0.5) THEN
C Note SH2O, SMC when STC < -0.5C
	      IF (SMCK(index)-SH2OK(index) .LT. 0.005) THEN
		write(6,*)'SH2Ol2b:INDEX,I,J,MYPE,STCK,SMC,SH2O=',
     .  	  index,I,J,MYPE,STCK(index),SMCK(index),SH2OK(index)
        write(6,*)'SH2Ol2b:I,J,MYPE,N-1,N,N+1,
     .    STCK(N-1),STCK(N),STCK(N+1), =',
     .    I,J,MYPE,indexm,index,indexp,
     .    STCK(indexm),STCK(index),STCK(indexp)
	      IERR1=1
	      ENDIF
	    ENDIF
C ----------------------------------------------------------------------
          END DO
C ----------------------------------------------------------------------
C Soil column bottom temp (TBOT) check
c	  IF ((TBOT .LT. 223.15) .OR. (TBOT .GT. 323.15)) THEN 
	  IF ((TBOT .LT. 200.00) .OR. (TBOT .GT. 323.15)) THEN 
            write(6,*)'TBOTl:INDEX,I,J,MYPE,TBOT=',
     .        index,I,J,MYPE,TBOT
              IERR2=1
          ENDIF
C ----------------------------------------------------------------------
C sfc/skin temp (T1K) check
c	  IF ((T1K .LT. 223.15) .OR. (T1K .GT. 323.15)) THEN 
	  IF ((T1K .LT. 200.00) .OR. (T1K .GT. 323.15)) THEN 
            write(6,*)'T1:INDEX,I,J,MYPE,T1=',
     .        index,I,J,MYPE,T1K
              IERR2=1
          ENDIF
C ----------------------------------------------------------------------
C Canopy water content (CMC) check
	  IF ((CMCK .LT. 0.) .OR. (CMCK .GT. 0.5E-3)) THEN 
            write(6,*)'CMC:INDEX,I,J,MYPE,CMC=',
     .        index,I,J,MYPE,CMCK
              IERR1=1
          ENDIF
C ----------------------------------------------------------------------
C Snow water equivalent, snow depth check
	  IF (((SNODPK .GT. 0.) .AND. (SNOWH .LT. 1.0E-09)) .OR.
     .        ((SNODPK .LT. 1.0E-09) .AND. (SNOWH .GT. 0.)) .OR.
     .         (SNODPK .GT. SNOWH)) THEN
	    WRITE(6,*)'SNOW:I,J,MYPE,SNODPK,SNOWH=',
     .        I,J,MYPE,SNODPK,SNOWH
	       IERR1=1
          ENDIF
C ----------------------------------------------------------------------
C Snow density check
	  IF (SNODPK .GT. 0.) THEN
            SNDENS=SNODPK/SNOWH
            IF (SNDENS .LT. 0.05) THEN
	      WRITE(6,*)'SNDENS<5%:I,J,MYPE,SNODPK,SNOWH,SNDENS=',
     .          I,J,MYPE,SNODPK,SNOWH,SNDENS
              IERR1=1
            ENDIF
            IF (SNDENS .GT. 0.42) THEN
	      WRITE(6,*)'SNDENS>42%:I,J,MYPE,SNODPK,SNOWH,SNDENS=',
     .          I,J,MYPE,SNODPK,SNOWH,SNDENS
              IERR1=1
            ENDIF
          ENDIF
C ----------------------------------------------------------------------
C end land checks
C ----------------------------------------------------------------------
        ELSE
C ----------------------------------------------------------------------
C sea-ice checks next
C ----------------------------------------------------------------------
C sea-ice bottom temp (TBOT) check
          IF ( (TBOT .LT. 271.159) .OR.
     .         (TBOT .GT. 271.161) ) THEN
            WRITE(6,*)'TBOTi:INDEX,I,J,MYPE,TBOT=',
     .        index,I,J,MYPE,TBOT
          ENDIF
C ----------------------------------------------------------------------
C sea-ice temp with depth (STC) range check
          DO index=1,4
c            IF ((STCK(index) .LT. 223.15) .OR.
            IF ((STCK(index) .LT. 200.00) .OR.
c     .          (STCK(index) .GT. 323.15)) THEN
     .          (STCK(index) .GT. 274.15)) THEN
              write(6,*)'STCi:INDEX,I,J,MYPE,STC=',
     .          index,I,J,MYPE,STCK(index)
              IERR1=2
            ENDIF
          END DO
C ----------------------------------------------------------------------
C sfc/skin temp (T1K) check
c	  IF ((T1K .LT. 223.15) .OR. (T1K .GT. 323.15)) THEN 
	  IF ((T1K .LT. 200.00) .OR. (T1K .GT. 323.15)) THEN 
            write(6,*)'T1:INDEX,I,J,MYPE,ICE,T1=',
     .        index,I,J,MYPE,ICE,T1K
              IERR2=1
          ENDIF
C ----------------------------------------------------------------------
C Snow water equivalent, snow depth check
	  IF (((SNODPK .GT. 0.) .AND. (SNOWH .LT. 1.0E-09)) .OR.
     .        ((SNODPK .LT. 1.0E-09) .AND. (SNOWH .GT. 0.)) .OR.
     .         (SNODPK .GT. SNOWH)) THEN
	    WRITE(6,*)'SNOW:I,J,MYPE,SNODPK,SNOWH=',
     .        I,J,MYPE,SNODPK,SNOWH
	       IERR1=1
          ENDIF
C ----------------------------------------------------------------------
C Snow density check
	  IF (SNODPK .GT. 0.) THEN
            SNDENS=SNODPK/SNOWH
            IF (SNDENS .LT. 0.05) THEN
	      WRITE(6,*)'SNDENS<5%:I,J,MYPE,SNODPK,SNOWH,SNDENS=',
     .          I,J,MYPE,SNODPK,SNOWH,SNDENS
              IERR1=1
            ENDIF
            IF (SNDENS .GT. 0.42) THEN
	      WRITE(6,*)'SNDENS>42%:I,J,MYPE,SNODPK,SNOWH,SNDENS=',
     .          I,J,MYPE,SNODPK,SNOWH,SNDENS
              IERR1=1
            ENDIF
          ENDIF
C ----------------------------------------------------------------------
c Check to see that when sea-ice, ALBASE=ALBEDO=0.65,MXSNAL=0,
          IF ( (ALB         .NE. 0.0 ) .OR.
     .         (ALBEDO(I,J) .LT. 0.64) .OR.
     .         (ALBEDO(I,J) .GT. 0.66) .OR.
     .         (SNOALB      .NE. 0.0 ) ) THEN         
	    WRITE(6,*)'ALBi:I,J,MYPE,ICE,SNODPK,ALB,ALB2D,SNOALB=',
     .        I,J,MYPE,ICE,SNODPK,ALB,ALBEDO(I,J),SNOALB         
              IERR1=1
          ENDIF
C ----------------------------------------------------------------------
c Check to see that when sea-ice, SH2O=SMC=1.0
          DO index=1,4
            IF ( (SMCK(index) .NE.          1.0) .OR.
     .           (SMCK(index) .NE. SH2OK(index)) ) THEN
              write(6,*)'SMCi:INDEX,I,J,MYPE,ICE,STCK,SMC,SH2O=',
     .          index,I,J,MYPE,ICE,STCK(index),SMCK(index),SH2OK(index)
              IERR1=1
            ENDIF
          END DO
C ----------------------------------------------------------------------
C check to see that veg type=soil type=veg frac=0
	  IF ( (IVGTYP(I,J) .NE. 0 ) .OR.
     .         (ISLTYP(I,J) .NE. 0 ) .OR.
     .         (VGFRCK .GT. 0.) ) THEN
	    WRITE(6,*)'IVGTYPi:I,J,MYPE,IVGTYP,ISLTYP,VGFRCK=',
     .        I,J,MYPE,IVGTYP(I,J),ISLTYP(I,J),VGFRCK
	    WRITE(6,*)'IVGTYPi:I,J,MYPE,SNODPK,ALB,ALBEDO,SNOALB=',
     .        I,J,MYPE,SNODPK,ALB,ALBEDO(I,J),SNOALB
	  ENDIF
C ----------------------------------------------------------------------
C end sea-ice checks
C end of separate land AND sea-ice checks
C ----------------------------------------------------------------------
        ENDIF
C ----------------------------------------------------------------------
C both land AND sea-ice checks
C Snow water equivalent, snow depth check
	  IF (((SNODPK .GT. 0.) .AND. (SNOWH .LE. 0.)) .OR.
     .        ((SNODPK .LE. 0.) .AND. (SNOWH .GT. 0.)) .OR.
     .         (SNODPK .GT. SNOWH)) THEN
	    WRITE(6,*)'SNOW:I,J,MYPE,SNODPK,SNOWH=',
     .        I,J,MYPE,SNODPK,SNOWH
	       IERR1=1
          ENDIF
C ----------------------------------------------------------------------
C Snow density check
	  IF (SNODPK .GT. 0.) THEN
            SNDENS=SNODPK/SNOWH
            IF (SNDENS .LT. 0.05) THEN
	      WRITE(6,*)'SNDENS<5%:I,J,MYPE,SNODPK,SNOWH,SNDENS=',
     .          I,J,MYPE,SNODPK,SNOWH,SNDENS
              IERR1=1
            ENDIF
            IF (SNDENS .GT. 0.42) THEN
	      WRITE(6,*)'SNDENS>42%:I,J,MYPE,SNODPK,SNOWH,SNDENS=',
     .          I,J,MYPE,SNODPK,SNOWH,SNDENS
              IERR1=1
            ENDIF
          ENDIF
C ----------------------------------------------------------------------
C sfc/skin temp (T1K) check
c	  IF ((T1K .LT. 223.15) .OR. (T1K .GT. 323.15)) THEN 
	  IF ((T1K .LT. 200.00) .OR. (T1K .GT. 323.15)) THEN 
            write(6,*)'T1:INDEX,I,J,MYPE,T1=',
     .        index,I,J,MYPE,T1K
              IERR2=1
          ENDIF
C ----------------------------------------------------------------------
c check to see that 223.15K (-50C) =< SFCTMP,SFCTH2 <= 323.15K (+50C) 
C SFCTMP = lowest model level temp
C SFCTH2 = lowest model level pot temp
        IF ( ( (SFCTMP .LT. 223.15) .OR. (SFCTMP .GT. 323.15) ) .OR.
     .       ( (SFCTH2 .LT. 223.15) .OR. (SFCTH2 .GT. 323.15) ) ) THEN
	  WRITE(6,*)'SFCTMP:I,J,MYPE,SFCTMP,SFCTH2=',
     .      I,J,MYPE,SFCTMP,SFCTH2
        ENDIF
C ----------------------------------------------------------------------
c check to see that 0W/m2 =< LWDN <= 500W/m2
c check to see that 0W/m2 =< SOLDN <= 1200W/m2
C LWDN  = downward longwave radiation
C SOLDN = downward solar radiation
        IF ( ( (LWDN  .LT. 0.) .OR. (LWDN  .GT.  500.) ) .OR.
     .      ( (SOLDN .LT. 0.) .OR. (SOLDN .GT. 1200.) ) ) THEN
          WRITE(6,*)'LWSOLDN:I,J,MYPE,LWDN,SOLDN=',
     .      I,J,MYPE,LWDN,SOLDN
        ENDIF
C ----------------------------------------------------------------------
c check to see that 0g/kg < Q2K,Q2SAT <= 40g/kg
c check to see that Q2K <= Q2SAT
C Q2K   = lowest model level spec hum
C Q2SAT = lowest model level sat spec hum
c        IF ( ( (Q2K   .LE. 0.) .OR. (Q2K   .GT. 0.04) ) .OR.
c     .       ( (Q2SAT .LE. 0.) .OR. (Q2SAT .GT. 0.04) ) .OR.
        IF ( ( (Q2K   .LE. 0.) .OR. (Q2K   .GT. 0.05) ) .OR.
     .       ( (Q2SAT .LE. 0.) .OR. (Q2SAT .GT. 0.05) ) .OR.
     .       (Q2K .GT. Q2SAT) ) THEN
	  WRITE(6,*)'Q2:I,J,MYPE,Q2K,Q2SAT=',
     .      I,J,MYPE,Q2K,Q2SAT
        ENDIF
C ----------------------------------------------------------------------
c check to see that 600mb =< SFCPRS <= 1050mb
C SFCPRS = surface pressure (Pa)
       IF ( (SFCPRS .LE. 60000.) .OR. (SFCPRS .GT. 105000.) ) THEN
         WRITE(6,*)'SFCPRS:I,J,MYPE,SFCPRS=',
     .     I,J,MYPE,SFCPRS
       ENDIF
C ----------------------------------------------------------------------
c check to see that 0 =< PRCP <= 0.04 KG M-2 S-1 (=0.04mm/s = 144mm/hr)
C PRCP = precip rate (KG M-2 S-1)
       IF ( (PRCP .LT. 0.) .OR. (PRCP .GT. 0.04) ) THEN
         WRITE(6,*)'PRCP:I,J,MYPE,PRCP=',
     .     I,J,MYPE,PRCP
       ENDIF
C ----------------------------------------------------------------------
c check to see that 0m/s < CHK <= 0.1m/s 
C CHK = sfc heat exchange coeff (m/s)
       IF ( (CHK .LE. 0.) .OR. (CHK .GT. 0.1) ) THEN
	 WRITE(6,*)'CH:I,J,MYPE,CHK=',
     .     I,J,MYPE,CHK
       ENDIF
C ----------------------------------------------------------------------
        IF (IERR2 .EQ. 1) THEN
          WRITE(6,*) 'RANGE CHECK IN SURFCE: EXTREME VALUES AFTER SFLX'
        ENDIF
	IF (IERR1 .EQ. 1) THEN
c	  WRITE(6,*) 'RANGE CHECK FAILURE IN SURFCE - STOP'
c	  STOP
	ENDIF
C ----------------------------------------------------------------------
C End of initial (logical LFIRST) range check for variables/parameters.
      ENDIF
C ----------------------------------------------------------------------

      SCHECK=Z*CHK
      IF(SCHECK.LE.1.3E-3)THEN
        PLFLX=0.
        ELFLX=0.
      ENDIF
C***
C***  GCIP DIAGNOSTICS & MODIFICATION OF QFC1 OVER SNOW
C***
        SSROFF(I,J)=SSROFF(I,J)+RNOF1K*DTQ2
        BGROFF(I,J)=BGROFF(I,J)+RNOF2K*DTQ2
        SMSTAV(I,J)=SOILQW
        SOILTB(I,J)=TBOT
        SFCEXC(I,J)=CHK
        GRNFLX(I,J)=GFLX
        IF(SNO (I,J).GT.0..OR.SICE(I,J).GT.0.5)THEN
          QFC1(I,J)=QFC1(I,J)*RLIVWV
        ENDIF
        IF(SNO(I,J).GT.0.)THEN
          ACSNOM(I,J)=ACSNOM(I,J)+SMELTK
          SNOPCX(I,J)=SNOPCX(I,J)-SMELTK/FDTLIW
        ENDIF
        POTEVP(I,J)=POTEVP(I,J)+PLFLX*FDTW
        POTFLX(I,J)=POTFLX(I,J)-PLFLX
        SUBSHX(I,J)=SUBSHX(I,J)+GFLX
C***
C***  ETA MODEL LOWER BOUNDARY CONDITIONS
C***
C       THS(I,J)=THLM(I,J)+HFLX*APES(I,J)/FFS(I,J)
        THS(I,J)=T1K*APES(I,J)
        IF(QFC1(I,J).GT.0.)
     1    QS(I,J)=QLM(I,J)+ELFLX*APES(I,J)/QFC1(I,J)
C***
C***  HISTORICAL VARIABLES
C***
c dynamic albedo, ALBEDO, to be passed to RADTN.f
        ALBEDO(I,J)=ALB2D
        SNO(I,J)=SNODPK
c snow depth, SI
        SI(I,J)=SNOWH
        CMC(I,J)=CMCK
        SMSTOT(I,J)=SOILQM
        DO 150 NS=1,NSOIL
        SMC(I,J,NS)=SMCK(NS)
c SH2O array (liquid soil moisture)
        SH2O(I,J,NS)=SH2OK(NS)
        STC(I,J,NS)=STCK(NS)
  150   CONTINUE
      ENDIF
C
  155 CONTINUE
  160 CONTINUE
C ----------------------------------------------------------------------
C Set LFIRST=false so that there are not variable/parameter range checks
C for the next 155/160 loop
      LFIRSTa = .FALSE.
      LFIRST = .FALSE.
C
C***  VARIABLES TWBS AND QWBS COMPUTED HERE FOR GCIP.
C***  ACCUMULATE SURFACE HEAT FLUXES HERE.
C***  FOR GCIP ACCUMULATE ACTUAL AND POTENTIAL EVAPORATION.
C***  FOR GCIP ACCUMULATE TOTAL SNOW MELT AND
C***  THE ASSOCIATED NET HEAT FLUX.
C
!$omp parallel do private(i,j)
      DO 200 J=MYJS2,MYJE2
      DO 200 I=MYIS,MYIE
      TWBS(I,J)=(THLM(I,J)-THS(I,J)*(1.-SM(I,J))-THZ0(I,J)*SM(I,J))
     1       *FFS (I,J)/APES(I,J)
      QWBS(I,J)=(QLM (I,J)-QS (I,J)*(1.-SM(I,J))-QZ0 (I,J)*SM(I,J))
     1       *QFC1(I,J)/APES(I,J)
      SFCSHX(I,J)=SFCSHX(I,J)+TWBS(I,J)
      SFCLHX(I,J)=SFCLHX(I,J)+QWBS(I,J)
      SFCEVP(I,J)=SFCEVP(I,J)-QWBS(I,J)*FDTW
      POTEVP(I,J)=POTEVP(I,J)-QWBS(I,J)*SM(I,J)*FDTW
      POTFLX(I,J)=POTFLX(I,J)+QWBS(I,J)*SM(I,J)
C
C***  IF COLD ENOUGH, IT SNOWS (IN NOAH LSM)...
C***  FOR GCIP ACCUMULATE TOTAL SNOWFALL.
C
      IF(THLM(I,J)/APELM(I,J).LE.T0.AND.SICE(I,J)+SM(I,J).LT.0.5)THEN
        ACSNOW(I,J)=ACSNOW(I,J)+PREC(I,J)
C***
C***  ... OTHERWISE IT RAINS.
C***
      ELSE
        ACCLIQ(I,J)=ACCLIQ(I,J)+PREC(I,J)
      ENDIF
C
      PREC(I,J)=0.
  200 CONTINUE
C***
C***  LONGWAVE OUTGOING RADIATION
C***
!$omp parallel do private(i,j,tsfc,tsfc2)
      DO 210 J=MYJS2,MYJE2
      DO 210 I=MYIS,MYIE
      TSFC=THS(I,J)/APES(I,J)
      TSFC2=TSFC*TSFC
      RADOT(I,J)=HBM2(I,J)*EPSR(I,J)*STBOL*TSFC2*TSFC2
  210 CONTINUE
C
C-----------------------------------------------------------------------
C
C   INCREMENT TIME STEP COUNTERS FOR USE IN COMPUTING TIME AVE VALUES
C
      APHTIM = APHTIM + 1.
      ARDSW  = ARDSW  + 1.
      ARDLW  = ARDLW  + 1.
      ASRFC  = ASRFC  + 1.
C-----------------------------------------------------------------------
                             RETURN
                             END
C
      BLOCK DATA OPT
      COMMON /OPTIONS/ SPVAL,IBESSL,KSB,IOFFS,IFLAG,SATDEL
      DATA SPVAL  / 99999  /
      DATA IBESSL /   0    /
      DATA KSB    /   3    /
      DATA IOFFS  /   2    /
      DATA IFLAG  /   0    /
      DATA SATDEL / 0.05   /
      END BLOCK DATA OPT
