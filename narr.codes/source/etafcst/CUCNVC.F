                              SUBROUTINE CUCNVC
C     ******************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .     
C SUBPROGRAM:    CUCNVC      CONVECTIVE PRECIPITATION PARAMETERIZATION
C   PRGRMMR: JANJIC          ORG: W/NP2      DATE: 93-11-02       
C     
C ABSTRACT:
C     CUCNVC CALCULATES THE SUB-GRID SCALE CONVECTION INCLUDING 
C     DEEP AND SHALLOW CONVECTIVE CLOUDS FOLLOWING THE SCHEME DESCRIBED
C     BY JANJIC (1994) BUT WITH SIGNIFICANT MODIFICATIONS.  IN ADDITION,
C     THE LATENT HEAT RELEASE AND MOISTURE CHANGE DUE TO PRECIPITATING
C     AND NON-PRECIPITATING CLOUDS ARE COMPUTED.
C     
C     
C PROGRAM HISTORY LOG:
C   87-09-??  JANJIC     - ORIGINATOR
C   90-11-21  JANJIC     - TWO SETS OF DSP PROFILES (FAST AND SLOW)
C                          REPLACE THE ORIGINAL ONE SET
C   95-03-25  BLACK      - CONVERSION FROM 1-D TO 2-D IN HORIZONTAL
C   96-03-28  BLACK      - ADDED EXTERNAL EDGE
C   98-11-02  BLACK      - MODIFIED FOR DISTRIBUTED MEMORY
C     
C USAGE: CALL CUCNVC FROM MAIN PROGRAM EBU
C
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
C        TTBLEX
C  
C     LIBRARY:
C        NONE
C  
C   COMMON BLOCKS: CTLBLK
C                  LOOPS
C                  MASKS
C                  PHYS
C                  VRBLS
C                  CNVCLD
C                  PVRBLS
C                  ACMCLH
C                  INDX
C   
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE : IBM SP
C$$$  
C     ******************************************************************
C     *                                                                *
C     *  REFERENCES:                                                   *
C     *                                                                *
C     *  JANJIC, Z.I., 1994:  THE STEP-MOUNTAIN ETA COORDINATE MODEL:  *
C     *    FURTHER DEVELOPMENTS OF THE CONVECTION, VISCOUS SUBLAYER    *
C     *    AND TURBULENCE CLOSURE SCHEMES.  MONTHLY WEATHER REVIEW,    *
C     *    VOL. 122, 927-945.                                          *
C     *                                                                *
C     ******************************************************************
C *** WARNING: THIS SUBROUTINE WILL NOT WORK IF LM.LT.12;
C              MUST BE CALLED IN THE SAME STEP WITH PROFQ2 BECAUSE PROFQ
C              DEFINES APE;
C----------------------------------------------------------------------
      INCLUDE "cuparm"
C----------------------------------------------------------------------
      INCLUDE "parmeta"
      INCLUDE "parm.tbl"
      INCLUDE "mpp.h"
C----------------------------------------------------------------------
                             P A R A M E T E R
CVVVVVVVVVV INSTABILITY FOR TOO LARGE LSH VVVVVVVVVVVVVVVVVVVVVVVVVVVVV
     & (KSMUD=0,NROW= 0)
C    & (KSMUD=0,NROW= 5)
CAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
                             P A R A M E T E R
     & (IMJM=IM*JM-JM/2,JAM=6+2*(JM-10)
     &, IMJM_LOC=IDIM2*JDIM2
CVVVVVVVVVV INSTABILITY FOR TOO LARGE LSH VVVVVVVVVVVVVVVVVVVVVVVVVVVVV
C    &, LP1=LM+1,LM1=LM-1,LNO=1,LSH=LM/3-1,LSHU=LM/2-1,LQM=LM/5,KBM=3
C    &, LP1=LM+1,LM1=LM-1,LNO=1,LSH=LM/3  ,LSHU=LM/2-1,LQM=LM/5,KBM=3
C    &, LP1=LM+1,LM1=LM-1,LNO=3,LSH=LM/3  ,LSHU=LM/2-1,LQM=LM/5,KBM=3
C    &, LP1=LM+1,LM1=LM-1,LNO=2,LSH=LM/3-1,LSHU=LM/2-1,LQM=LM/5,KBM=3
C    &, LP1=LM+1,LM1=LM-1,LNO=2,LSH=LM/3-2,LSHU=LM/2-1,LQM=LM/5,KBM=3
     &, LP1=LM+1,LM1=LM-1)
CAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
C----------------------------------------------------------------------
                             L O G I C A L
     & RUN,FIRST,RESTRT,SIGMA
C----------------------------------------------------------------------
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
      INCLUDE "CNVCLD.comm"
C-----------------------------------------------------------------------
      INCLUDE "PVRBLS.comm"
C-----------------------------------------------------------------------
      INCLUDE "ACMCLH.comm"
C-----------------------------------------------------------------------
      INCLUDE "INDX.comm"
CYL---------------------------------------------------------------------
      INCLUDE "PPTASM.comm"
CYL---------------------------------------------------------------------
                             D I M E N S I O N
     & TREFK (LM),QREFK (LM),PK    (LM),APEK  (LM),TK    (LM)
     &,THSK  (LM),PSK   (LM),APESK (LM),QK    (LM),THERK (LM)
     &,THVREF(LM),THEVRF(LM),THVMOD(LM),DIFT  (LM),DIFQ  (LM)
     &,QSATK (LM),FPK   (LM)
     &,NTOPD (LM),NBOTD (LM),NTOPS (LM),NBOTS (LM)
     &,NDPTHD(LM),NDPTHS(LM)
C
                             D I M E N S I O N
     & LTOP  (IDIM1:IDIM2,JDIM1:JDIM2),LBOT  (IDIM1:IDIM2,JDIM1:JDIM2)
     &,PTOP  (IDIM1:IDIM2,JDIM1:JDIM2),PBOT  (IDIM1:IDIM2,JDIM1:JDIM2)
     &,IPTB  (IDIM1:IDIM2,JDIM1:JDIM2),ITHTB (IDIM1:IDIM2,JDIM1:JDIM2)
     &,PDSL  (IDIM1:IDIM2,JDIM1:JDIM2),APEBT (IDIM1:IDIM2,JDIM1:JDIM2)
     &,TBT   (IDIM1:IDIM2,JDIM1:JDIM2),Q2BT  (IDIM1:IDIM2,JDIM1:JDIM2)
     &,QQ    (IDIM1:IDIM2,JDIM1:JDIM2),PP    (IDIM1:IDIM2,JDIM1:JDIM2)
     &,PSP   (IDIM1:IDIM2,JDIM1:JDIM2),THBT  (IDIM1:IDIM2,JDIM1:JDIM2)
     &,THESP (IDIM1:IDIM2,JDIM1:JDIM2),P     (IDIM1:IDIM2,JDIM1:JDIM2)
     &,BTH   (IDIM1:IDIM2,JDIM1:JDIM2),STH   (IDIM1:IDIM2,JDIM1:JDIM2)
     &,T00   (IDIM1:IDIM2,JDIM1:JDIM2),T10   (IDIM1:IDIM2,JDIM1:JDIM2)
     &,T01   (IDIM1:IDIM2,JDIM1:JDIM2),T11   (IDIM1:IDIM2,JDIM1:JDIM2)
     &,WF1   (IDIM1:IDIM2,JDIM1:JDIM2),WF2   (IDIM1:IDIM2,JDIM1:JDIM2)
     &,WF3   (IDIM1:IDIM2,JDIM1:JDIM2),WF4   (IDIM1:IDIM2,JDIM1:JDIM2)
     &,PRECOL(IDIM1:IDIM2,JDIM1:JDIM2)
C
     &,IBUOY (IMJM_LOC),JBUOY (IMJM_LOC)
     &,IDEEP (IMJM_LOC),JDEEP (IMJM_LOC)
     &,ISHAL (IMJM_LOC),JSHAL (IMJM_LOC)
     &,ILRES (IMJM_LOC),JLRES (IMJM_LOC)
     &,IHRES (IMJM_LOC),JHRES (IMJM_LOC)
C
     &,APE   (IDIM1:IDIM2,JDIM1:JDIM2,LM)
     &,TREF  (IDIM1:IDIM2,JDIM1:JDIM2,LM)
     &,TMOD  (IDIM1:IDIM2,JDIM1:JDIM2,LM)
     &,QMOD  (IDIM1:IDIM2,JDIM1:JDIM2,LM)
C
                             D I M E N S I O N
     & DSPB  (IDIM1:IDIM2,JDIM1:JDIM2),DSP0  (IDIM1:IDIM2,JDIM1:JDIM2)
     &,DSPT  (IDIM1:IDIM2,JDIM1:JDIM2)
     &,TL    (IDIM1:IDIM2,JDIM1:JDIM2),QL    (IDIM1:IDIM2,JDIM1:JDIM2)
     &,TNE   (IDIM1:IDIM2,JDIM1:JDIM2),TSE   (IDIM1:IDIM2,JDIM1:JDIM2)
     &,QNE   (IDIM1:IDIM2,JDIM1:JDIM2),QSE   (IDIM1:IDIM2,JDIM1:JDIM2)
C-----------------------------------------------------------------------
      CALL ZERO2(DSP0)
      CALL ZERO2(DSPB)
      CALL ZERO2(DSPT)
      CALL ZERO2(PSP)
C----------------------------------------------------------------------
      AVCNVC=AVCNVC+1.
      ACUTIM=ACUTIM+1.
      DTCNVC=NCNVC*DT
      RDTCNVC=1./DTCNVC
      TAUK=DTCNVC/TREL
c possible future change for shallow convection
c      TAUKSC=DTCNVC/(5.*TREL)   
C-----------------------------------------------------------------------
C************************** DIAGNOSTICS ********************************
C-----------------------------------------------------------------------
      DO L=1,LM
        NTOPD (L)=0
        NBOTD (L)=0
        NTOPS (L)=0
        NBOTS (L)=0
        NDPTHS(L)=0
        NDPTHD(L)=0
      ENDDO
C***********************************************************************
C---------------------------PREPARATIONS--------------------------------
C-----------------------------------------------------------------------
!$omp parallel do 
      DO 120 J=MYJS,MYJE
      DO 120 I=MYIS,MYIE
      LBOT (I,J)=LMH(I,J)
      THESP(I,J)=0.
      PDSL (I,J)=RES(I,J)*PD(I,J)
      PRECOL(I,J)=0.
      TREF(I,J,1)=T(I,J,1)
  120 CONTINUE
C-----------------------------------------------------------------------
C--------------PADDING SPECIFIC HUMIDITY IF TOO SMALL-------------------
C                  RESTORE APE TO SCRATCH ARRAY
C-----------------------------------------------------------------------
!$omp parallel do private(apests)
      DO 130 L=1,LM
      DO J=MYJS,MYJE
      DO I=MYIS,MYIE
        APESTS=PDSL(I,J)*AETA(L)+PT
        APE(I,J,L)=(1.E5/APESTS)**CAPA
        IF(Q(I,J,L).LT.EPSQ)Q(I,J,L)=HTM(I,J,L)*EPSQ
      ENDDO
      ENDDO
  130 CONTINUE
C-----------------------------------------------------------------------
C--------------SEARCH FOR MAXIMUM BUOYANCY LEVEL------------------------
C-----------------------------------------------------------------------
                             DO 170 KB=1,LM
C-----------------------------------------------------------------------
C--------------TRIAL MAXIMUM BUOYANCY LEVEL VARIABLES-------------------
C-----------------------------------------------------------------------
!$omp parallel do 
!$omp&  private(apesp,bq,bqs00k,bqs10k,iq,it,ittb,ittbk,iqtb,
!$omp&          lmhk,p00k,p01k,p10k,p11k,pkl,pp1,psfck,qbt,qq1,
!$omp&          sq,sqs00k,sqs10k,tpsp,tq,tth,tthbt,tthes)
      DO 155 J=MYJS2,MYJE2
      DO 150 I=MYIS1,MYIE1
C
      PKL=AETA(KB)*PDSL(I,J)+PT
      LMHK=LMH(I,J)
      PSFCK=AETA(LMHK)*PDSL(I,J)+PT
c now searching over a scaled depth in finding the parcel
c   with the max theta-e instead of the old 130 mb
c     IF(PKL.LT.PSFCK-PBM)GO TO 150
c     IF(KB.GT.LMHK)GO TO 150
      IF(KB.LE.LMHK .AND. PKL.GE.0.80*PSFCK) THEN
      QBT=Q(I,J,KB)
      TTHBT=T(I,J,KB)*APE(I,J,KB)
      TTH=(TTHBT-THL)*RDTH
      QQ1=TTH-AINT(TTH)
      ITTB=INT(TTH)+1
C--------------KEEPING INDICES WITHIN THE TABLE-------------------------
      IF(ITTB.LT.1)THEN
        ITTB=1
        QQ1=0.
      ENDIF
      IF(ITTB.GE.JTB)THEN
        ITTB=JTB-1
        QQ1=0.
      ENDIF
      CONTINUE
C--------------BASE AND SCALING FACTOR FOR SPEC. HUMIDITY---------------
      ITTBK=ITTB
      BQS00K=QS0(ITTBK)
      SQS00K=SQS(ITTBK)
      BQS10K=QS0(ITTBK+1)
      SQS10K=SQS(ITTBK+1)
C--------------SCALING SPEC. HUMIDITY & TABLE INDEX---------------------
      BQ  = (BQS10K-BQS00K)*QQ1+BQS00K
      SQ  = (SQS10K-SQS00K)*QQ1+SQS00K
      TQ  = (QBT-BQ)/SQ*RDQ
      PP1  =TQ - AINT(TQ)
      IQTB=INT(TQ)+1
C--------------KEEPING INDICES WITHIN THE TABLE-------------------------
      IF(IQTB.LT.1)THEN
        IQTB=1
        PP1=0.
      ENDIF
      IF(IQTB.GE.ITB)THEN
        IQTB=ITB-1
        PP1=0.
      ENDIF
C--------------SATURATION PRESSURE AT FOUR SURROUNDING TABLE PTS.-------
      IQ=IQTB
      IT=ITTB
      P00K=PTBL(IQ  ,IT  )
      P10K=PTBL(IQ+1,IT  )
      P01K=PTBL(IQ  ,IT+1)
      P11K=PTBL(IQ+1,IT+1)
C--------------SATURATION POINT VARIABLES AT THE BOTTOM-----------------
      TPSP=P00K+(P10K-P00K)*PP1+(P01K-P00K)*QQ1
     1        +(P00K-P10K-P01K+P11K)*PP1*QQ1
      APESP=(1.E5/TPSP)**CAPA
      TTHES=TTHBT*EXP(ELOCP*QBT*APESP/TTHBT)
C--------------CHECK FOR MAXIMUM BUOYANCY-------------------------------
       IF(TTHES.GT.THESP(I,J))THEN
        PSP  (I,J)=TPSP
        THBT (I,J)=TTHBT
        THESP(I,J)=TTHES
       ENDIF
      ENDIF
  150 CONTINUE
  155 CONTINUE
C-----------------------------------------------------------------------
  170 CONTINUE
C-----------------------------------------------------------------------
C---------CHOOSE CLOUD BASE AS MODEL LEVEL JUST BELOW PSP--------------
C-----------------------------------------------------------------------
      DO 240 L=1,LM1
      AETAL=AETA(L)
C
!$omp parallel do 
      DO J=MYJS2,MYJE2
      DO I=MYIS,MYIE
        P(I,J)=PDSL(I,J)*AETAL+PT
        IF(P(I,J).LT.PSP(I,J).AND.P(I,J).GE.PQM)LBOT(I,J)=L+1
      ENDDO
      ENDDO
C
  240 CONTINUE
C***
C*** WARNING: LBOT MUST NOT BE GT LMH(I,J)-1 IN SHALLOW CONVECTION
C*** MAKE SURE CLOUD BASE IS AT LEAST PONE ABOVE THE SURFACE
C***
!$omp parallel do private(lmhij,psfck)
      DO 250 J=MYJS2,MYJE2
      DO 250 I=MYIS,MYIE
      LMHIJ=LMH(I,J)
      PBOT(I,J)=AETA(LBOT(I,J))*PDSL(I,J)+PT
      PSFCK=AETA(LMHIJ)*PDSL(I,J)+PT
      IF(PBOT(I,J).GE.PSFCK-PONE.OR.LBOT(I,J).GE.LMHIJ)THEN
C
        DO L=1,LMHIJ-1
        P(I,J)=AETA(L)*PDSL(I,J)+PT
        IF(P(I,J).LT.PSFCK-PONE)LBOT(I,J)=L
        ENDDO
C
        PBOT(I,J)=AETA(LBOT(I,J))*PDSL(I,J)+PT
      ENDIF 
  250 CONTINUE
C--------------CLOUD TOP COMPUTATION------------------------------------
!$omp parallel do 
      DO J=MYJS,MYJE
      DO I=MYIS,MYIE
        LTOP(I,J)=LBOT(I,J)
        PTOP(I,J)=PBOT(I,J)
      ENDDO
      ENDDO
C-----------------------------------------------------------------------
!$omp parallel do 
!$omp&  private(ihres,ilres,iptb,ithtb,jhres,jlres,
!$omp&          knumh,knuml,pp,presk,qq)
C
      DO 290 L=LM,1,-1
C
C--------------SCALING PRESSURE & TT TABLE INDEX------------------------
      KNUML=0
      KNUMH=0
C
      DO 270 J=MYJS2,MYJE2
      DO 270 I=MYIS1,MYIE1
      PRESK=PDSL(I,J)*AETA(L)+PT
      IF(PRESK.LT.PLQ)THEN
        KNUML=KNUML+1
        ILRES(KNUML)=I
        JLRES(KNUML)=J
      ELSE
        KNUMH=KNUMH+1
        IHRES(KNUMH)=I
        JHRES(KNUMH)=J
      ENDIF
 270  CONTINUE
C***
C***  COMPUTE PARCEL TEMPERATURE ALONG MOIST ADIABAT FOR PRESSURE<PL
C**
      IF(KNUML.GT.0)THEN
        CALL TTBLEX(TREF(IDIM1,JDIM1,L),TTBL,ITB,JTB,KNUML
     1,             ILRES,JLRES,PDSL,AETA(L),HTM(IDIM1,JDIM1,L)
     2,             PT,PL,QQ(IDIM1,JDIM1),PP(IDIM1,JDIM1)
     3,             RDP,THE0,STHE,RDTHE
     4,             THESP(IDIM1,JDIM1),IPTB(IDIM1,JDIM1)
     5,             ITHTB(IDIM1,JDIM1))
      ENDIF
C***
C***  COMPUTE PARCEL TEMPERATURE ALONG MOIST ADIABAT FOR PRESSURE>PL
C**
      IF(KNUMH.GT.0)THEN
        CALL TTBLEX(TREF(IDIM1,JDIM1,L),TTBLQ,ITBQ,JTBQ,KNUMH
     1,             IHRES,JHRES,PDSL,AETA(L),HTM(IDIM1,JDIM1,L)
     2,             PT,PLQ,QQ(IDIM1,JDIM1),PP(IDIM1,JDIM1)
     3,             RDPQ,THE0Q,STHEQ,RDTHEQ
     4,             THESP(IDIM1,JDIM1),IPTB(IDIM1,JDIM1)
     5,             ITHTB(IDIM1,JDIM1))
      ENDIF
 290  CONTINUE
C--------------BUOYANCY CHECK-------------------------------------------
      DO 295 L=LM,1,-1
!$omp parallel do 
      DO J=MYJS2,MYJE2
      DO I=MYIS1,MYIE1
        IF(TREF(I,J,L).GT.T(I,J,L)-DTTOP)LTOP(I,J)=L
      ENDDO
      ENDDO
C-----------------------------------------------------------------------
  295 CONTINUE
C-----------------CLOUD TOP PRESSURE------------------------------------
!$omp parallel do 
      DO J=MYJS2,MYJE2
      DO I=MYIS1,MYIE1
        PTOP(I,J)=AETA(LTOP(I,J))*PDSL(I,J)+PT
      ENDDO
      ENDDO
C-----------------------------------------------------------------------
C--------------DEFINE AND SMOOTH DSPS AND CLDEFI------------------------
C ************ UNIFIED OR SEPARATE LAND/SEA CONV ***********************
C-----------------------------------------------------------------------
C
      IF(UNIS)THEN
!$omp parallel do private(efi)
        DO J=MYJS,MYJE
        DO I=MYIS,MYIE
          EFI=CLDEFI(I,J)
          DSPB(I,J)=(EFI-EFIMN)*SLOPBS+DSPBSS
          DSP0(I,J)=(EFI-EFIMN)*SLOP0S+DSP0SS
          DSPT(I,J)=(EFI-EFIMN)*SLOPTS+DSPTSS
        ENDDO
        ENDDO
      ELSEIF(.NOT.UNIL)THEN
!$omp parallel do private(efi)
        DO J=MYJS,MYJE
        DO I=MYIS,MYIE
          EFI=CLDEFI(I,J)
          DSPB(I,J)=((EFI-EFIMN)*SLOPBS+DSPBSS)*SM(I,J)
     1             +((EFI-EFIMN)*SLOPBL+DSPBSL)*(1.-SM(I,J))
          DSP0(I,J)=((EFI-EFIMN)*SLOP0S+DSP0SS)*SM(I,J)
     1             +((EFI-EFIMN)*SLOP0L+DSP0SL)*(1.-SM(I,J))
          DSPT(I,J)=((EFI-EFIMN)*SLOPTS+DSPTSS)*SM(I,J)
     1             +((EFI-EFIMN)*SLOPTL+DSPTSL)*(1.-SM(I,J))
        ENDDO
        ENDDO
      ELSE
!$omp parallel do private(efi)
        DO J=MYJS,MYJE
        DO I=MYIS,MYIE
          EFI=CLDEFI(I,J)
          DSPB(I,J)=((EFI-EFIMN)*SLOPBL+DSPBSL)
          DSP0(I,J)=((EFI-EFIMN)*SLOP0L+DSP0SL)
          DSPT(I,J)=((EFI-EFIMN)*SLOPTL+DSPTSL) 
        ENDDO
        ENDDO
      ENDIF
C
C--------------EXTENDING SEA STRUCTURES INLAND ALONG COASTLINE----------
      IF(NROW.GT.0.AND..NOT.UNIS.AND..NOT.UNIL)THEN
!$omp parallel do 
        DO J=MYJS,MYJE
        DO I=MYIS,MYIE
          WF1(I,J)=0.
          WF2(I,J)=0.
          WF3(I,J)=0.
          WF4(I,J)=0.
        ENDDO
        ENDDO
C
        KROW=NROW
C
        DO 350 KOUNT=1,KROW
!$omp parallel do 
        DO 345 J=MYJS2,MYJE2
        DO 345 I=MYIS1,MYIE1
        WF1(I,J)=(DSPB(I+IHW(J),J-1)+DSPB(I+IHE(J),J-1)
     1           +DSPB(I+IHW(J),J+1)+DSPB(I+IHE(J),J+1)+4.*DSPB(I,J))
     2           *HBM2(I,J)*0.125
        WF2(I,J)=(DSP0(I+IHW(J),J-1)+DSP0(I+IHE(J),J-1)
     1           +DSP0(I+IHW(J),J+1)+DSP0(I+IHE(J),J+1)+4.*DSP0(I,J))
     2           *HBM2(I,J)*0.125
        WF3(I,J)=(DSPT(I+IHW(J),J-1)+DSPT(I+IHE(J),J-1)
     1           +DSPT(I+IHW(J),J+1)+DSPT(I+IHE(J),J+1)+4.*DSPT(I,J))
     2           *HBM2(I,J)*0.125
        WF4(I,J)=(CLDEFI(I+IHW(J),J-1)+CLDEFI(I+IHE(J),J-1)
     1     +CLDEFI(I+IHW(J),J+1)+CLDEFI(I+IHE(J),J+1)+4.*CLDEFI(I,J))
     2     *HBM2(I,J)*0.125
  345   CONTINUE 
C
!$omp parallel do private(rsmk,smk)
        DO J=MYJS,MYJE
        DO I=MYIS,MYIE
          SMK =SM(I,J)
          RSMK=1.-SMK
          DSPB  (I,J)=DSPB  (I,J)*SMK+WF1(I,J)*RSMK
          DSP0  (I,J)=DSP0  (I,J)*SMK+WF2(I,J)*RSMK
          DSPT  (I,J)=DSPT  (I,J)*SMK+WF3(I,J)*RSMK 
          CLDEFI(I,J)=CLDEFI(I,J)*SMK+WF4(I,J)*RSMK
        ENDDO
        ENDDO
C
  350   CONTINUE
C-----------------------------------------------------------------------
      ENDIF
C--------------INITIALIZE CHANGES OF T AND Q DUE TO CONVECTION----------
!$omp parallel do 
      DO 360 L=1,LM
      DO J=MYJS,MYJE
      DO I=MYIS,MYIE
        TMOD(I,J,L)=0.
        QMOD(I,J,L)=0.
      ENDDO
      ENDDO
  360 CONTINUE
C--------------CLEAN UP AND GATHER DEEP CONVECTION POINTS---------------
!$omp parallel do 
      DO 380 J=MYJS2,MYJE2
      DO 380 I=MYIS1,MYIE1
      IF(LTOP(I,J).GE.LBOT(I,J))THEN
        LBOT(I,J)=0
        LTOP(I,J)=LBOT(I,J)
        PTOP(I,J)=PBOT(I,J)
      ENDIF
      IF(HBM2(I,J).LT.0.90)THEN
        LBOT(I,J)=0
        LTOP(I,J)=LBOT(I,J)
        PTOP(I,J)=PBOT(I,J)
      ENDIF
      IF(PTOP(I,J).GT.PBOT(I,J)-PNO.OR.LTOP(I,J).GT.LBOT(I,J)-2)
     1 CLDEFI(I,J)=AVGEFI*SM(I,J)+STEFI*(1.-SM(I,J))
  380 CONTINUE
C
      KHDEEP=0
      PSHNEW=20000.
      DO J=MYJS2,MYJE2
       DO I=MYIS1,MYIE1
        PSFCIJ=PD(I,J)+PT
c depth of cloud required to make the point a deep convection point
c  is now a scaled value of the PSFC instead of 290 mb everywhere 
        DEPMIN=PSHNEW*PSFCIJ*1.E-5
        DEPTH=PBOT(I,J)-PTOP(I,J)
        IF(DEPTH .GE. DEPMIN) THEN
c        IF(PTOP(I,J).LT.PBOT(I,J)-PSH)THEN
          KHDEEP=KHDEEP+1
          IDEEP(KHDEEP)=I
          JDEEP(KHDEEP)=J
         ENDIF
       ENDDO
      ENDDO
C************* HORIZONTAL LOOP FOR DEEP CONVECTION *********************
!$omp parallel do
!$omp&  private(apek,apekl,apekxx,apekxy,apesk,avrgt,avtgtl,dentpy,
!$omp&          depmin,depth,depwl,dhdt,difq,difql,dift,diftl,drheat,
!$omp&          drhdp,dsp,dsp0k,dspbk,dsptk,dthem,efi,fefi,hcorr,
!$omp&          i,j,l0,l0m1,lb,lbm1,lbtk,lcor,lqm,lshu,ltp1,ltp2,
!$omp&          ltpk,ltsh,pbtk,pk,pk0,pkb,pkl,pkt,preck,psfcij,psk,
!$omp&          pthrs,ptpk,qk,qkl,qrefk,qsatk,rdp0t,rhh,rhl,rhmax,
!$omp&          sumde,sumdp,therk,therkx,therky,thsk,thskl,tk,tkl,
!$omp&          trefk,trefkx,tskl)
C***********************************************************************
      DO 600 N=1,KHDEEP
C***********************************************************************
      I=IDEEP(N)
      J=JDEEP(N)
      PSFCIJ=PD(I,J)+PT
      LTPK=LTOP(I,J)
      LBTK=LBOT(I,J)
CDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCD
CDCDCDCDCDCDC  DEEP CONVECTION   DCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCD
CDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCD
      LB   =LBTK
      EFI  =CLDEFI(I,J)
      DSPBK=DSPB  (I,J)
      DSP0K=DSP0  (I,J)
      DSPTK=DSPT  (I,J)
C--------------INITIALIZE VARIABLES IN THE CONVECTIVE COLUMN------------
C***
C***  ONE SHOULD NOTE THAT THE VALUES ASSIGNED TO THE ARRAY TREFK
C***  IN THE 410 LOOP ARE REALLY ONLY RELEVANT IN ANCHORING THE
C***  REFERENCE TEMPERATURE PROFILE AT LEVEL LB.  WHEN BUILDING THE
C***  REFERENCE PROFILE FROM CLOUD BASE, THEN ASSIGNING THE
C***  AMBIENT TEMPERATURE TO TREFK IS ACCEPTABLE.  HOWEVER, WHEN
C***  BUILDING THE REFERENCE PROFILE FROM SOME OTHER LEVEL (SUCH AS
C***  ONE LEVEL ABOVE THE GROUND), THEN TREFK SHOULD BE FILLED WITH
C***  THE TEMPERATURES IN TREF(I,J,L) WHICH ARE THE TEMPERATURES OF
C***  THE MOIST ADIABAT THROUGH CLOUD BASE.  BY THE TIME THE LINE 
C***  NUMBERED 450 HAS BEEN REACHED, TREFK ACTUALLY DOES HOLD THE
C***  REFERENCE TEMPERATURE PROFILE.
C***
      DO 410 L=1,LM
      DIFT  (L)=0.
      DIFQ  (L)=0.
      TKL      =T(I,J,L)
      TK    (L)=TKL
      TREFK (L)=TKL
      QKL      =Q(I,J,L)
      QK    (L)=QKL
      QREFK (L)=QKL
      PKL      =AETA(L)*PDSL(I,J)+PT
      PK    (L)=PKL
      PSK   (L)=PKL
      APEKL    =APE(I,J,L)
      APEK  (L)=APEKL
      THERK (L)=TREF(I,J,L)*APEKL
C 410 THVMOD(L)=(QKL*0.608+1.)*TKL*APEKL
  410     CONTINUE
C--------------DEEP CONVECTION REFERENCE TEMPERATURE PROFILE------------
      LTP1=LTPK+1
      LBM1=LB-1
      PKB=PK(LB)
      PKT=PK(LTPK)
C--------------TEMPERATURE REFERENCE PROFILE BELOW FREEZING LEVEL-------
      L0=LB
      PK0=PK(LB)
      TREFKX=TREFK(LB)
      THERKX=THERK(LB)
      APEKXX=APEK(LB)
      THERKY=THERK(LBM1)
      APEKXY=APEK(LBM1)
C
      DO 420 L=LBM1,LTPK,-1
      IF(T(I,J,L+1).LT.TFRZ)GO TO 430
      STABDL=STABD
      TREFKX=((THERKY-THERKX)*STABDL
     1        +TREFKX*APEKXX)/APEKXY
      TREFK(L)=TREFKX
      APEKXX=APEKXY
      THERKX=THERKY
      APEKXY=APEK(L-1)
      THERKY=THERK(L-1)
      L0=L
      PK0=PK(L0)
  420 CONTINUE
C--------------FREEZING LEVEL AT OR ABOVE THE CLOUD TOP-----------------
      L0M1=L0-1
      GO TO 450
C--------------TEMPERATURE REFERENCE PROFILE ABOVE FREEZING LEVEL-------
  430 L0M1=L0-1
      RDP0T=1./(PK0-PKT)
      DTHEM=THERK(L0)-TREFK(L0)*APEK(L0)
C
      DO L=LTPK,L0M1
        TREFK(L)=(THERK(L)-(PK(L)-PKT)*DTHEM*RDP0T)/APEK(L)
      ENDDO
C-----------------------------------------------------------------------
C--------------DEEP CONVECTION REFERENCE HUMIDITY PROFILE---------------
C-----------------------------------------------------------------------
c  reference profile had been too dry in the case where the cloud
c   base was close to the freezing level - this is now changed
  450 DEPTH=PFRZ*PSFCIJ*1.E-5
      DEPWL=PKB-PK0
      DO 460 L=LTPK,LB
C-----------------------------------------------------------------------
C--------------SATURATION PRESSURE DIFFERENCE---------------------------
C-----------------------------------------------------------------------
c     IF(PKB-PK0.GE.PFRZ)THEN
      IF(DEPWL .GE. DEPTH) THEN
        IF(L.LT.L0)THEN
          DSP=((PK0-PK(L))*DSPTK+(PK(L)-PKT)*DSP0K)/(PK0-PKT)
        ELSE
          DSP=((PKB-PK(L))*DSP0K+(PK(L)-PK0)*DSPBK)/(PKB-PK0)
        ENDIF
      ELSE
c       DSP=DSPC
        DSP=DSP0K
        IF(L.LT.L0)
     1 DSP=( (PK0-PK(L))*DSPTK+(PK(L)-PKT)*DSP0K)/(PK0-PKT)
      ENDIF

C--------------HUMIDITY PROFILE-----------------------------------------
      IF(PK(L).GT.PQM)THEN
        PSK(L)=PK(L)+DSP
        APESK(L)=(1.E5/PSK(L))**CAPA
        THSK(L)=TREFK(L)*APEK(L)
        QREFK(L)=PQ0/PSK(L)*EXP(A2*(THSK(L)-A3*APESK(L))
     1                            /(THSK(L)-A4*APESK(L)))
      ELSE
        QREFK(L)=Q(I,J,L)
      ENDIF
  460 CONTINUE
C--------------ENTHALPY CONSERVATION INTEGRAL--------------------------
                             DO 520 ITER=1,2
C-----------------------------------------------------------------------
      SUMDE=0.
      SUMDP=0.
C
      DO L=LTPK,LB
        SUMDE=((TK(L)-TREFK(L))*CP+(QK(L)-QREFK(L))*ELWV)*DETA(L)
     1        +SUMDE
        SUMDP=SUMDP+DETA(L)
      ENDDO
C
      HCORR=SUMDE/(SUMDP-DETA(LTPK))
      LCOR=LTPK+1
C-----------------------FIND LQM----------------------------------------
      DO L=1,LB
        IF(PK(L).LE.PQM)LQM=L
      ENDDO
C--------------ABOVE LQM CORRECT TEMPERATURE ONLY-----------------------
      IF(LCOR.LE.LQM)THEN
        DO L=LCOR,LQM
          TREFK(L)=TREFK(L)+HCORR*RCP
        ENDDO
        LCOR=LQM+1
      ENDIF
C--------------BELOW LQM CORRECT BOTH TEMPERATURE AND MOISTURE----------
C
      DO 510 L=LCOR,LB
      TSKL=TREFK(L)*APEK(L)/APESK(L)
      DHDT=QREFK(L)*A23M4L/(TSKL-A4)**2+CP
      TREFK(L)=HCORR/DHDT+TREFK(L)
      THSKL=TREFK(L)*APEK(L)
      QREFK(L)=PQ0/PSK(L)*EXP(A2*(THSKL-A3*APESK(L))
     1                          /(THSKL-A4*APESK(L)))
  510 CONTINUE
C-----------------------------------------------------------------------
  520 CONTINUE
C--------------HEATING, MOISTENING, PRECIPITATION-----------------------
      DENTPY=0.
      AVRGT =0.
      PRECK =0.
C
      DO 530 L=LTPK,LB
      TKL    =TK(L)
      DIFTL  =(TREFK(L)-TKL  )*TAUK
      DIFQL  =(QREFK(L)-QK(L))*TAUK
      AVRGTL =(TKL+TKL+DIFTL)
      DENTPY =(DIFTL*CP+DIFQL*ELWV)*DETA(L)/AVRGTL+DENTPY
      AVRGT  =AVRGTL*DETA(L)+AVRGT
      PRECK  =DETA(L)*DIFTL+PRECK
      DIFT(L)=DIFTL
      DIFQ(L)=DIFQL
  530 CONTINUE
C
      DENTPY=DENTPY+DENTPY
      AVRGT =AVRGT/(SUMDP+SUMDP)
C-------------SWAP IF ENTROPY AND/OR PRECIP .LT. 0 ...------------------
      IF(DENTPY.LT.EPSNTP.OR.PRECK.LT.0.)THEN
        IF(OCT90)THEN
          CLDEFI(I,J)=EFIMN
        ELSE
          CLDEFI(I,J)=EFIMN*SM(I,J)+STEFI*(1.-SM(I,J))
        ENDIF
C
C--------------SEARCH FOR SHALLOW CLOUD TOP-----------------------------
        LBTK=LBOT(I,J)
        LTSH=LBTK
        LBM1=LBTK-1
        PBTK=PK(LBTK)
c use new threshold for cloud depth
c        PTPK=PBTK-PSH
        PSFCIJ=PD(I,J)+PT
        DEPMIN=PSHNEW*PSFCIJ*1.E-5
        PTPK=PBTK-DEPMIN
C-------------CLOUD TOP IS THE LEVEL JUST BELOW PBTK-PSH----------------
        DO L=1,LM
          IF(PK(L).LE.PTPK)LTPK=L+1
        ENDDO
        PTPK=PK(LTPK)
C--------------HIGHEST LEVEL ALLOWED IS LEVEL JUST BELOW PSHU-----------
        IF(PTPK.LE.PSHU)THEN
          DO L=1,LM
            IF(PK(L).LE.PSHU)LSHU=L+1
          ENDDO
          LTPK=LSHU
          PTPK=PK(LTPK)
        ENDIF
C
        LTP1=LTPK+1
        LTP2=LTPK+2
C-----------------------------------------------------------------------
        DO L=LTPK,LBTK
          QSATK(L)=PQ0/PK(L)*EXP(A2*(TK(L)-A3)/(TK(L)-A4))
        ENDDO
C-----------------------------------------------------------------------
        RHH=QK(LTPK)/QSATK(LTPK)
        RHMAX=0.
C
        DO 570 L=LTP1,LBM1
        RHL=QK(L)/QSATK(L)
        DRHDP=(RHH-RHL)/(PK(L-1)-PK(L))
        IF(DRHDP.GT.RHMAX)THEN
          LTSH=L-1
          RHMAX=DRHDP
        ENDIF
        RHH=RHL
  570   CONTINUE
C
        LTOP(I,J)=LTSH
C---------------CLOUD MUST BE AT LEAST TWO LAYERS THICK-----------------
        IF(LBOT(I,J)-LTOP(I,J).LT.2)LTOP(I,J)=LBOT(I,J)-2
C
        PTOP(I,J)=PK(LTOP(I,J))
        GO TO 600
      ENDIF
C-----------------------------------------------------------------------
C--------------... DEEP CONVECTION OTHERWISE----------------------------
C-----------------------------------------------------------------------
      DRHEAT=(PRECK*SM(I,J)+AMAX1(EPSP,PRECK)*(1.-SM(I,J)))
     1       *CP/AVRGT
      EFI=EFIFC*DENTPY/DRHEAT
C
C************** UNIFIED OR SEPARATE LAND/SEA CONV. **************
C
      IF(OCT90)THEN
        IF(UNIS)THEN
          EFI=CLDEFI(I,J)*FCB+EFI*FCC
        ELSEIF(.NOT.UNIL)THEN
          EFI=(CLDEFI(I,J)*FCB+EFI*FCC)*SM(I,J)+1.-SM(I,J)
        ELSE
          EFI=1.
        ENDIF
      ELSE
        EFI=CLDEFI(I,J)*FCB+EFI*FCC
      ENDIF
C
      IF(EFI.GT.1.   )    EFI=1.
      IF(EFI.LT.EFIMN)    EFI=EFIMN
      IF(PRECK.EQ.0.)     EFI=1.
      CLDEFI(I,J)=EFI
C
      FEFI=EFMNT+SLOPE*(EFI-EFIMN)
C     FEFI=AMAX1(EFI,EFMNT)
C
      PRECK=PRECK*FEFI
C
C--------------UPDATE PRECIPITATION, TEMPERATURE & MOISTURE-------------
C
      PRECOL(I,J)=PDSL(I,J)*PRECK*CPRLG
      PREC  (I,J)=PDSL(I,J)*PRECK*CPRLG+PREC  (I,J)
      CUPREC(I,J)=PDSL(I,J)*PRECK*CPRLG+CUPREC(I,J)
      ACPREC(I,J)=PDSL(I,J)*PRECK*CPRLG+ACPREC(I,J)
      CUPPT(I,J) =PDSL(I,J)*PRECK*CPRLG+CUPPT(I,J)
C
      DO L=LTPK,LB
        TMOD(I,J,L)=DIFT(L)*FEFI
        QMOD(I,J,L)=DIFQ(L)*FEFI
CYL
        TLAT(I,J,L)=DIFT(L)*FEFI + TLAT(I,J,L)
CYL
      ENDDO
C
CDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCD
CDCDCDCDCDCDC          END OF DEEP CONVECTION            DCDCDCDCDCDCDCD
CDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCD
C
C-----------------------------------------------------------------------
  600 CONTINUE
C-----------------------------------------------------------------------
      NDEEP=0
C
      DO 620 J=MYJS2,MYJE2
      DO 620 I=MYIS,MYIE
      LTPK=LTOP(I,J)
      LBTK=LBOT(I,J)
      LB  =LMH(I,J)-1
      PSFCIJ=PD(I,J)+PT
      DEPMIN=PSHNEW*PSFCIJ*1.E-5
      IF(PTOP(I,J).LT.PBOT(I,J)-DEPMIN)THEN
        NDEEP=NDEEP+1
        NDEPTH=LB-LTPK
        NTOPD (LTPK  )=NTOPD (LTPK  )+1
        NBOTD (LB    )=NBOTD (LB    )+1
        NDPTHD(NDEPTH)=NDPTHD(NDEPTH)+1
      ENDIF
  620 CONTINUE
      NNEG=KHDEEP-NDEEP
C
C--------------GATHER SHALLOW CONVECTION POINTS-------------------------
C
      KHSHAL=0
      NDSTN =0
      NDSTP =0
C
      DO 630 J=MYJS2,MYJE2
      DO 630 I=MYIS,MYIE
      IF(PTOP(I,J).GT.PBOT(I,J)-PNO.OR.
     1   LTOP(I,J).GT.LBOT(I,J)-2)GO TO 630
      PSFCIJ=PD(I,J)+PT
      DEPMIN=PSHNEW*PSFCIJ*1.E-5
      IF(PTOP(I,J)+1..GE.PBOT(I,J)-DEPMIN)THEN
        KHSHAL=KHSHAL+1
        ISHAL(KHSHAL)=I
        JSHAL(KHSHAL)=J
      ENDIF
  630 CONTINUE
C
C************* HORIZONTAL LOOP FOR SHALLOW CONVECTION ******************
!$omp parallel do 
!$omp&  private(apek,apekl,apekxx,apekxy,bqk,bqs00k,bqs10k,den,dentpy,
!$omp&           dpkl,dpmix,dqref,dst,dstq,dtdeta,fpk,fptk,i,iq,it,j,
!$omp&           lbm1,lbtk,ltp1,ltpk,otsum,part1,part2,part3,pk,pkl,
!$omp&           pkxxxx,pkxxxy,potsum,ppk,psum,ptpk,pz0,qk,qkl,qnew,
!$omp&           qotsum,qqk,qrefk,qrfkl,qrftp,qsatk,qsum,rdpsum,rtbar,
!$omp&           smix,sqk,sqs00k,sqs10k,sumdp,sumdt,tcorr,thvmkl,
!$omp&           thvref,tk,tkl,tqk,trefk,trefkx,trfkl,tthk)
C
      DO 800 N=1,KHSHAL
C***********************************************************************
      I=ISHAL(N)
      J=JSHAL(N)
C-----------------------------------------------------------------------
CSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCS
CSCSCSCSCSCSC         SHALLOW CONVECTION          CSCSCSCSCSCSCSCSCSCSCS
CSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCS
C-----------------------------------------------------------------------
      PZ0=PD(I,J)+PT
      LLMH=LMH(I,J)
C
      DO 650 L=1,LLMH
      TKL      =T  (I,J,L)
      TK   (L) =TKL
      TREFK(L) =TKL
      QKL      =Q  (I,J,L)
      QK   (L) =QKL
      QREFK(L) =QKL
      PKL      =AETA(L)*PDSL(I,J)+PT
      PK   (L) =PKL
      QSATK(L) =PQ0/PK(L)*EXP(A2*(TK(L)-A3)/(TK(L)-A4))
      APEKL    =APE(I,J,L)
CVVVVVVVVVVVV CHOOSE THE PRESSURE FUNCTION VVVVVVVVVVVVVVVVVVVVVVVVVVVVV
C         FPK  (L) =ALOG(PKL)
C         FPK  (L) =PKL
C         FPK  (L) =-1./PKL
CAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
      APEK (L) =APEKL
      THVMKL   =TKL*APEKL*(QKL*0.608+1.)
C     THVMOD(L)=THVMKL
      THVREF(L)=THVMKL
  650 CONTINUE
C--------------SHALLOW CLOUD TOP-----------------------------
      LBTK=LBOT(I,J)
      LBM1=LBTK-1
      PTPK=PTOP(I,J)
      LTP1=LTOP(I,J)
      LTPK=LTOP(I,J)-1
C-----------------------------------------------------------------------
      IF(PTOP(I,J).GT.PBOT(I,J)-PNO.OR.LTOP(I,J).GT.LBOT(I,J)-2)THEN
        LBOT(I,J)=0
        LTOP(I,J)=LBOT(I,J)
        PTOP(I,J)=PBOT(I,J)
        GO TO 800
      ENDIF
C--------------SCALING POTENTIAL TEMPERATURE & TABLE INDEX AT TOP-------
C
C     THTPK=T(I,J,LTP1)*APE(I,J,LTP1)
      THTPK=T(I,J,LTPK)*APE(I,J,LTPK)
C
      TTHK =(THTPK-THL)*RDTH
      QQK  =TTHK-AINT(TTHK)
      IT   =INT(TTHK)+1
      IF(IT.LT.1)THEN
        IT=1
        QQK=0.
      ENDIF
      IF(IT.GE.JTB)THEN
        IT=JTB-1
        QQK=0.
      ENDIF
C--------------BASE AND SCALING FACTOR FOR SPEC. HUMIDITY AT TOP--------
      BQS00K=QS0(IT)
      SQS00K=SQS(IT)
      BQS10K=QS0(IT+1)
      SQS10K=SQS(IT+1)
C--------------SCALING SPEC. HUMIDITY & TABLE INDEX AT TOP--------------
      BQK=(BQS10K-BQS00K)*QQK+BQS00K
      SQK=(SQS10K-SQS00K)*QQK+SQS00K
C
C     TQK=(Q(I,J,LTP1)-BQK)/SQK*RDQ
      TQK=(Q(I,J,LTPK)-BQK)/SQK*RDQ
C
      PPK=TQK-AINT(TQK)
      IQ =INT(TQK)+1
      IF(IQ.LT.1)THEN
        IQ=1
        PPK=0.
      ENDIF
      IF(IQ.GE.ITB)THEN
        IQ=ITB-1
        PPK=0.
      ENDIF
C--------------CLOUD TOP SATURATION POINT PRESSURE----------------------
      PART1=(PTBL(IQ+1,IT)-PTBL(IQ,IT))*PPK
      PART2=(PTBL(IQ,IT+1)-PTBL(IQ,IT))*QQK
      PART3=(PTBL(IQ  ,IT  )-PTBL(IQ+1,IT  )
     1      -PTBL(IQ  ,IT+1)+PTBL(IQ+1,IT+1))*PPK*QQK
      PTPK=PTBL(IQ,IT)+PART1+PART2+PART3
C-----------------------------------------------------------------------
      DPMIX=PTPK-PSP(I,J)
      IF(ABS(DPMIX).LT.3000.)DPMIX=-3000.
C--------------TEMPERATURE PROFILE SLOPE--------------------------------
      SMIX=(THTPK-THBT(I,J))/DPMIX*STABS
C
      TREFKX=TREFK(LBTK+1)
      PKXXXX=PK(LBTK+1)
      PKXXXY=PK(LBTK)
      APEKXX=APEK(LBTK+1)
      APEKXY=APEK(LBTK)
C
      DO 670 L=LBTK,LTP1,-1
      TREFKX=((PKXXXY-PKXXXX)*SMIX
     1        +TREFKX*APEKXX)/APEKXY
      TREFK(L)=TREFKX
      APEKXX=APEKXY
      PKXXXX=PKXXXY
      APEKXY=APEK(L-1)
      PKXXXY=PK(L-1)
  670 CONTINUE
C--------------TEMPERATURE REFERENCE PROFILE CORRECTION-----------------
      SUMDT=0.
      SUMDP=0.
C
      DO L=LTP1,LBTK
        SUMDT=(TK(L)-TREFK(L))*DETA(L)+SUMDT
        SUMDP=SUMDP+DETA(L)
      ENDDO
C
      RDPSUM=1./SUMDP
      FPK(LBTK)=TREFK(LBTK)
C
      TCORR=SUMDT*RDPSUM
C
      DO L=LTP1,LBTK
C     TCORR=SUMDT/(SUMDP-DETA(LBTK))
C     DO L=LTP1,LBM1
C
        TRFKL   =TREFK(L)+TCORR
        TREFK(L)=TRFKL
        FPK  (L)=TRFKL
      ENDDO
C--------------HUMIDITY PROFILE EQUATIONS-------------------------------
      PSUM  =0.
      QSUM  =0.
      POTSUM=0.
      QOTSUM=0.
      OTSUM =0.
      DST   =0.
      FPTK  =FPK(LTP1)
C
      DO 700 L=LTP1,LBTK
      DPKL  =FPK(L)-FPTK
      PSUM  =DPKL *DETA(L)+PSUM
      QSUM  =QK(L)*DETA(L)+QSUM
      RTBAR =2./(TREFK(L)+TK(L))
      OTSUM =DETA(L)*RTBAR+OTSUM
      POTSUM=DPKL   *RTBAR*DETA(L)+POTSUM
      QOTSUM=QK(L)  *RTBAR*DETA(L)+QOTSUM
      DST   =(TREFK(L)-TK(L))*RTBAR*DETA(L)+DST
  700 CONTINUE
C
      PSUM  =PSUM*RDPSUM
      QSUM  =QSUM*RDPSUM
      ROTSUM=1./OTSUM
      POTSUM=POTSUM*ROTSUM
      QOTSUM=QOTSUM*ROTSUM
      DST   =DST   *ROTSUM*CP/ELWV
C--------------ENSURE POSITIVE ENTROPY CHANGE---------------------------
      IF(DST.GT.0.)THEN
C
C       DSTQ=DST*EPSUP
        LBOT(I,J)=0
        LTOP(I,J)=LBOT(I,J)
        PTOP(I,J)=PBOT(I,J)
cccc    NDSTP=NDSTP+1
        GO TO 800
C
      ELSE
        DSTQ=DST*EPSDN
      ENDIF
C--------------CHECK FOR ISOTHERMAL ATMOSPHERE--------------------------
      DEN=POTSUM-PSUM
C
      IF(-DEN/PSUM.LT.5.E-5)THEN
        LBOT(I,J)=0
        LTOP(I,J)=LBOT(I,J)
        PTOP(I,J)=PBOT(I,J)
        GO TO 800
C
C--------------SLOPE OF THE REFERENCE HUMIDITY PROFILE------------------
      ELSE
        DQREF=(QOTSUM-DSTQ-QSUM)/DEN
      ENDIF
C------------ HUMIDITY DOES NOT INCREASE WITH HEIGHT--------------------
      IF(DQREF.LT.0.)THEN
        LBOT(I,J)=0
        LTOP(I,J)=LBOT(I,J)
        PTOP(I,J)=PBOT(I,J)
        GO TO 800
      ENDIF
C--------------HUMIDITY AT THE CLOUD TOP--------------------------------
      QRFTP=QSUM-DQREF*PSUM
C--------------HUMIDITY PROFILE-----------------------------------------
C
      DO 720 L=LTP1,LBTK
      QRFKL=(FPK(L)-FPTK)*DQREF+QRFTP
C
C--------------SUPERSATURATION OR NEGATIVE Q NOT ALLOWED----------------
C
      QNEW =(QRFKL-QK(L))*TAUK+QK(L)
      IF(QNEW.GT.QSATK(L)*STRESH.OR.QNEW.LT.0.)THEN
        LBOT(I,J)=0
        LTOP(I,J)=LBOT(I,J)
        PTOP(I,J)=PBOT(I,J)
        GO TO 800
      ENDIF
C
C-----------------------------------------------------------------------
      THVREF(L)=TREFK(L)*APEK(L)*(QRFKL*0.608+1.)
      QREFK(L)=QRFKL
  720 CONTINUE
C--------------ELIMINATE IMPOSSIBLE SLOPES (BETTS, DTHETA/DQ)-----------
      DO 730 L=LTP1,LBTK
      DTDETA=(THVREF(L-1)-THVREF(L))/(AETA(L)-AETA(L-1))
      IF(DTDETA.LT.EPSTH)THEN
        LBOT(I,J)=0
        LTOP(I,J)=LBOT(I,J)
        PTOP(I,J)=PBOT(I,J)
        GO TO 800
      ENDIF
  730 CONTINUE
C*************************** DIAGNOSTICS ******************************
cccc  IF(DST.GT.0.)THEN
cccc    NDSTP=NDSTP+1
cccc  ELSE
cccc    NDSTN=NDSTN+1
cccc  ENDIF
C**********************************************************************
      DENTPY=0.
C
      DO L=LTP1,LBTK
        DENTPY=((TREFK(L)-TK(L))*CP+(QREFK(L)-QK(L))*ELWV)
     1         /(TK(L)+TREFK(L))*DETA(L)+DENTPY
      ENDDO
C
C-----------------------------------------------------------------------
C--------------RELAXATION TOWARDS REFERENCE PROFILES--------------------
C-----------------------------------------------------------------------
C
C
      DO 750 L=LTP1,LBTK
      TMOD(I,J,L)=(TREFK(L)-TK(L))*TAUK
      QMOD(I,J,L)=(QREFK(L)-QK(L))*TAUK
  750 CONTINUE
C-----------------------------------------------------------------------
CSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCS
CSCSCSCSCSCSC         END OF SHALLOW CONVECTION        SCSCSCSCSCSCSCSCS
CSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCS
C-----------------------------------------------------------------------
  800                        CONTINUE
C-----------------------------------------------------------------------
C
C************************** DIAGNOSTICS ********************************
      NSHAL=0
C
      DO 820 J=MYJS2,MYJE2
      DO 820 I=MYIS,MYIE
      LTPK=LTOP(I,J)
      LBTK=LBOT(I,J)
      PTPK=PTOP(I,J)
      PBTK=PBOT(I,J)
      IF(PTPK.GT.PBTK-PNO.OR.LTPK.GT.LBTK-2)GO TO 820
      PSFCIJ=PD(I,J)+PT
      DEPMIN=PSHNEW*PSFCIJ*1.E-5
C      IF(PTPK.GE.PBTK-PSH)THEN
      IF(PTPK.GE.PBTK-DEPMIN)THEN
        NSHAL=NSHAL+1
        NTOPS(LTPK)=NTOPS(LTPK)+1
        NBOTS(LBTK)=NBOTS(LBTK)+1
        NDEPTH=LBTK-LTPK
        NDPTHS(NDEPTH)=NDPTHS(NDEPTH)+1
      ENDIF
  820 CONTINUE
      NEGDS=KHSHAL-NSHAL
C***********************************************************************
C
C--------------SMOOTHING TEMPERATURE & HUMIDITY CORRECTIONS-------------
      IF(KSMUD.EQ.0)THEN
!$omp parallel do 
C
        DO 900 L=1,LM
C***
C***  UPDATE THE FUNDAMENTAL PROGNOSTIC ARRAYS
C***
        DO 830 J=MYJS,MYJE
        DO 830 I=MYIS,MYIE
        T(I,J,L)=T(I,J,L)+TMOD(I,J,L)
        Q(I,J,L)=Q(I,J,L)+QMOD(I,J,L)
C***
C***  ACCUMULATE LATENT HEATING DUE TO CONVECTION.
C***  SCALE BY THE RECIPROCAL OF THE PERIOD AT WHICH THIS ROUTINE
C***  IS CALLED.  THIS PERIOD IS THE CONVECTION TIMESTEP.
C***
        TCUCN(I,J,L)=TCUCN(I,J,L)+TMOD(I,J,L)*RDTCNVC
 830    CONTINUE
 900    CONTINUE
      ELSE
!$omp parallel do private(ql,qne,qse,tl,tne,tse)
C
        DO 910 L=1,LM
C
        CALL ZERO2(QL)
        CALL ZERO2(QNE)
        CALL ZERO2(QSE)
        CALL ZERO2(TL)
        CALL ZERO2(TNE)
        CALL ZERO2(TSE)
C-----------------------------------------------------------------------
        DO J=MYJS,MYJE
        DO I=MYIS,MYIE
          TL(I,J)=TMOD(I,J,L)
          QL(I,J)=QMOD(I,J,L)
        ENDDO
        ENDDO
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
        NSMUD=KSMUD
C
        DO 870 KS=1,NSMUD
C
        DO J=MYJS,MYJE1
        DO I=MYIS,MYIE
        TNE(I,J)=(TL(I+IHE(J),J+1)-TL(I,J))
     1           *HTM(I,J,L)*HTM(I+IHE(J),J+1,L)
        QNE(I,J)=(QL(I+IHE(J),J+1)-QL(I,J))
     1           *HTM(I,J,L)*HTM(I+IHE(J),J+1,L)
        ENDDO
        ENDDO
C
        DO J=MYJS1,MYJE
        DO I=MYIS,MYIE
        TSE(I,J)=(TL(I+IHE(J),J-1)-TL(I,J))
     1           *HTM(I+IHE(J),J-1,L)*HTM(I,J,L)
        QSE(I,J)=(QL(I+IHE(J),J-1)-QL(I,J))
     1           *HTM(I+IHE(J),J-1,L)*HTM(I,J,L)
        ENDDO
        ENDDO
C
        DO J=MYJS2,MYJE2
        DO I=MYIS,MYIE
        TL(I,J)=(TNE(I,J)-TNE(I+IHW(J),J-1)+TSE(I,J)-TSE(I+IHW(J),J+1))
     1          *HBM2(I,J)*0.125+TL(I,J)
        QL(I,J)=(QNE(I,J)-QNE(I+IHW(J),J-1)+QSE(I,J)-QSE(I+IHW(J),J+1))
     1          *HBM2(I,J)*0.125+QL(I,J)
        ENDDO
        ENDDO
C
  870   CONTINUE
C-----------------------------------------------------------------------
C***
C***  UPDATE THE FUNDAMENTAL PROGNOSTIC ARRAYS
C***
        DO J=MYJS,MYJE
        DO I=MYIS,MYIE
          T(I,J,L)=T(I,J,L)+TL(I,J)
          Q(I,J,L)=Q(I,J,L)+QL(I,J)
        ENDDO
        ENDDO
C***
C***  ACCUMULATE LATENT HEATING DUE TO CONVECTION.
C***  SCALE BY THE RECIPROCAL OF THE PERIOD AT WHICH THIS ROUTINE
C***  IS CALLED.  THIS PERIOD IS THE CONVECTION TIMESTEP.
C***
        DO J=MYJS,MYJE
        DO I=MYIS,MYIE
          TCUCN(I,J,L)=TCUCN(I,J,L)+TL(I,J)*RDTCNVC
        ENDDO
        ENDDO
C
  910   CONTINUE
C
      ENDIF
C--------------SAVE CLOUD TOP AND BOTTOM FOR RADIATION------------------
!$omp parallel do 
      DO J=MYJS,MYJE
      DO I=MYIS,MYIE
        IF (LTOP(I,J).GT.0 .AND. LTOP(I,J).LT.LBOT(I,J)) THEN
          CUtop=FLOAT(LTOP(I,J))
          CUbot=FLOAT(LBOT(I,J))
          HTOP(I,J)=MIN(CUtop,HTOP(I,J))
          HBOT(I,J)=MAX(CUbot,HBOT(I,J))
          CNVTOP(I,J)=MIN(CUtop,CNVTOP(I,J))
          CNVBOT(I,J)=MAX(CUbot,CNVBOT(I,J))
        ENDIF
      ENDDO
      ENDDO
C-----------------------------------------------------------------------
C************************* DIAGNOSTICS *********************************
C
C     WRITE(LIST,950)NTSD,NSHAL,NDEEP,NNEG,NEGDS,NDSTN,NDSTP
C         DO 940 L=1,LM
C     WRITE(LIST,952)L
C     WRITE(LIST,954)NBOTS(L),NTOPS(L),NDPTHS(L)
C    1,              NBOTD(L),NTOPD(L),NDPTHD(L)
C 940 CONTINUE
C 950 FORMAT(' NTSD=',I3,I8,' SHALLOW, ',I4,' DEEP, ',
C    1 I4,' NEG., ',I4,' NEG. SHALL.,',I4,' DST.LT.0, ',I4,' DST.GT.0')
C 952 FORMAT(' LAYER (FROM TOP),',I2)
C 954 FORMAT('     NBOTS=',I4,'     NTOPS=',I4,'     NDPTHS=',I4,
C    1       '     NBOTD=',I4,'     NTOPD=',I4,'     NDPTHD=',I4)
C***********************************************************************
                             RETURN
                             END
