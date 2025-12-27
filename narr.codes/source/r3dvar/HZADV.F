C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& 
                             SUBROUTINE HZADV 
C     ******************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .     
C SUBPROGRAM:    HZADV       HORIZONTAL ADVECTION
C   PRGRMMR: JANJIC          ORG: W/NP22     DATE: 93-10-28       
C     
C ABSTRACT:
C     HZADV CALCULATES THE CONTRIBUTION OF THE HORIZONTAL ADVECTION
C     TO THE TENDENCIES OF TEMPERATURE, WIND COMPONENTS, AND
C     TURBULENT KINETIC ENERGY AND THEN UPDATES THOSE VARIABLES.
C     THE JANJIC ADVECTION SCHEME FOR THE ARAKAWA E GRID IS USED
C     FOR ALL VARIABLES INSIDE THE FIFTH ROW.  AN UPSTREAM SCHEME
C     IS USED ON ALL VARIABLES IN THE THIRD, FOURTH, AND FIFTH
C     OUTERMOST ROWS.  A MODIFIED EULER-BACKWARD TIME SCHEME (HEUN)
C     IS USED.  UNDERGROUND WINDS MUST BE EQUAL TO ZERO SINCE THEY
C     ARE USED EXPLICITLY WITHOUT THE VELOCITY MASK IN THE FLUX
C     CALCULATIONS.
C     
C PROGRAM HISTORY LOG:
C   87-06-??  JANJIC     - ORIGINATOR
C   95-03-25  BLACK      - CONVERSION FROM 1-D TO 2-D IN HORIZONTAL
C   96-03-28  BLACK      - ADDED EXTERNAL EDGE
C   98-10-30  BLACK      - MODIFIED FOR DISTRIBUTED MEMORY
C     
C USAGE: CALL HZADV FROM MAIN PROGRAM EBU
C   INPUT ARGUMENT LIST:
C       NONE     
C  
C   OUTPUT ARGUMENT LIST: 
C     NONE
C     
C   OUTPUT FILES:
C     NONE
C     
C   SUBPROGRAMS CALLED:
C  
C     UNIQUE: NONE
C  
C     LIBRARY: NONE
C  
C   COMMON BLOCKS: CTLBLK
C                  LOOPS
C                  MASKS
C                  DYNAM
C                  VRBLS
C                  CONTIN
C                  PVRBLS
C                  INDX
C   
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE : IBM SP
C$$$  
C***********************************************************************
!                            P A R A M E T E R
!    & (TLC=2.*0.703972477)
C-----------------------------------------------------------------------
      INCLUDE "PARMETA.comm"
      INCLUDE "mpp.h"
!#include "sp.h"
C-----------------------------------------------------------------------
!                            P A R A M E T E R
!    & (IM1=IM-1,JAM=6+2*(JM-10)
!    &, IMJM=IM*JM-JM/2,LP1=LM+1
!    &, JAMD=(JAM*2-10)*3)
C-----------------------------------------------------------------------
                             L O G I C A L
     & RUN,FIRST,RESTRT,ITER2,SIGMA
C----------------------------------------------------------------------
      INCLUDE "CTLBLK.comm"
C-----------------------------------------------------------------------
      INCLUDE "LOOPS.comm"
C-----------------------------------------------------------------------
      INCLUDE "MASKS.comm"
C-----------------------------------------------------------------------
      INCLUDE "DYNAM.comm"
C-----------------------------------------------------------------------
      INCLUDE "VRBLS.comm"
C-----------------------------------------------------------------------
      INCLUDE "CONTIN.comm"
C-----------------------------------------------------------------------
      INCLUDE "PVRBLS.comm"
      INCLUDE "CLDWTR.comm"
C-----------------------------------------------------------------------
      INCLUDE "INDX.comm"
C-----------------------------------------------------------------------
                             D I M E N S I O N
     & HM    (IDIM1:IDIM2,JDIM1:JDIM2),VM    (IDIM1:IDIM2,JDIM1:JDIM2)
     &,RDPD  (IDIM1:IDIM2,JDIM1:JDIM2)
     &,ADPDX (IDIM1:IDIM2,JDIM1:JDIM2),ADPDY (IDIM1:IDIM2,JDIM1:JDIM2)
     &,RDPDX (IDIM1:IDIM2,JDIM1:JDIM2),RDPDY (IDIM1:IDIM2,JDIM1:JDIM2)
     &,ADT   (IDIM1:IDIM2,JDIM1:JDIM2)
     &,ADU   (IDIM1:IDIM2,JDIM1:JDIM2),ADV   (IDIM1:IDIM2,JDIM1:JDIM2)
     &,ADQ2M (IDIM1:IDIM2,JDIM1:JDIM2),ADQ2L (IDIM1:IDIM2,JDIM1:JDIM2)
     &,Q2MNS (IDIM1:IDIM2,JDIM1:JDIM2),Q2LNS (IDIM1:IDIM2,JDIM1:JDIM2)
     &,UDY   (IDIM1:IDIM2,JDIM1:JDIM2),VDX   (IDIM1:IDIM2,JDIM1:JDIM2)
C
                             D I M E N S I O N
     & DPDE  (IDIM1:IDIM2,JDIM1:JDIM2)
     &,TEMPA (IDIM1:IDIM2,JDIM1:JDIM2),TEMPB (IDIM1:IDIM2,JDIM1:JDIM2)
     &,TST   (IDIM1:IDIM2,JDIM1:JDIM2)
     &,UST   (IDIM1:IDIM2,JDIM1:JDIM2),VST   (IDIM1:IDIM2,JDIM1:JDIM2)
     &,Q2M   (IDIM1:IDIM2,JDIM1:JDIM2),Q2L   (IDIM1:IDIM2,JDIM1:JDIM2)
     &,TEW   (IDIM1:IDIM2,JDIM1:JDIM2),TNS   (IDIM1:IDIM2,JDIM1:JDIM2)
     &,Q2MEW (IDIM1:IDIM2,JDIM1:JDIM2),Q2LEW (IDIM1:IDIM2,JDIM1:JDIM2)
C
                             D I M E N S I O N
     & TNE   (IDIM1:IDIM2,JDIM1:JDIM2),TSE   (IDIM1:IDIM2,JDIM1:JDIM2)
     &,Q2MNE (IDIM1:IDIM2,JDIM1:JDIM2),Q2MSE (IDIM1:IDIM2,JDIM1:JDIM2)
     &,Q2LNE (IDIM1:IDIM2,JDIM1:JDIM2),Q2LSE (IDIM1:IDIM2,JDIM1:JDIM2)
     &,UEW   (IDIM1:IDIM2,JDIM1:JDIM2),UNS   (IDIM1:IDIM2,JDIM1:JDIM2)
     &,VEW   (IDIM1:IDIM2,JDIM1:JDIM2),VNS   (IDIM1:IDIM2,JDIM1:JDIM2)
     &,UNE   (IDIM1:IDIM2,JDIM1:JDIM2),USE   (IDIM1:IDIM2,JDIM1:JDIM2)
     &,VNE   (IDIM1:IDIM2,JDIM1:JDIM2),VSE   (IDIM1:IDIM2,JDIM1:JDIM2)
     &,FEW   (IDIM1:IDIM2,JDIM1:JDIM2),FNS   (IDIM1:IDIM2,JDIM1:JDIM2)
     &,FNE   (IDIM1:IDIM2,JDIM1:JDIM2),FSE   (IDIM1:IDIM2,JDIM1:JDIM2)
C
                             D I M E N S I O N
     & ADQ2HL(IDIM1:IDIM2,JDIM1:JDIM2,LM)
     &,Q2ML(IDIM1:IDIM2,JDIM1:JDIM2,LM+1)
C
      DIMENSION ARRAY0(JAMD)  
      DIMENSION ARRAY1(JAMD) 
      DIMENSION ARRAY2(JAMD)   
      DIMENSION ARRAY3(JAMD)  
      DIMENSION KHHAS(JAMD)  
      DIMENSION IHLAS(JAMD) 
      DIMENSION JHLAS(JAMD)
      DIMENSION KVHAS(JAMD)    
      DIMENSION IVLAS(JAMD)   
      DIMENSION JVLAS(JAMD)  
      DIMENSION ISPA(JAMD)  
      DIMENSION ISQA(JAMD) 
c
      LOGICAL UPSTRM,LJRA(JAM)
C--------------------------------------------------------------------
C***********************************************************************
C-----------------------------------------------------------------------
      TLC=2.*0.703972477
C
C***  FIGURE OUT IF WE ARE IN THE UPSTREAM REGION
C
      UPSTRM=.FALSE.
      IF(MYPE.LE.INPES-1)UPSTRM=.TRUE.
      IF(MYPE.GE.NPES-INPES)UPSTRM=.TRUE.
      IF(MOD(MYPE,INPES).EQ.0)UPSTRM=.TRUE.
      IF(MOD(MYPE+1,INPES).EQ.0)UPSTRM=.TRUE.
C
      JAKONE=0 
C
      DO 25 JA=1,JAM
      IHL=IHLA(JA)
      IHH=IHHA(JA)
      J=JRA(JA)
      LJRA(JA)=.FALSE.
C
      IF(J.GE.MY_JS_GLB-JBPAD2.AND.J.LE.MY_JE_GLB+JTPAD2)THEN
        LJRA(JA)=.TRUE.
        DO I=IHL,IHH
          IF(I.GE.MY_IS_GLB-ILPAD2.AND.I.LE.MY_IE_GLB+IRPAD2)THEN
            JAKONE=JAKONE+1
            KHHAS(JAKONE)=JA
            IHLAS(JAKONE)=I
            JHLAS(JAKONE)=J
          ENDIF
        ENDDO
      ENDIF
C
   25 CONTINUE
C
      JAKTWO=0
      DO 50 JA=1,JAM
        IVL=IVLA(JA)
        IVH=IVHA(JA)
        J=JRA(JA)
C
      DO 50 I=IVL,IVH
      IF(I.GE.MY_IS_GLB-ILPAD2.AND.I.LE.MY_IE_GLB+IRPAD2.AND.
     1   J.GE.MY_JS_GLB-JBPAD2.AND.J.LE.MY_JE_GLB+JTPAD2)THEN
        JAKTWO=JAKTWO+1
        KVHAS(JAKTWO)=JA
        IVLAS(JAKTWO)=I
        JVLAS(JAKTWO)=J
      ENDIF
   50 CONTINUE
C
C
      DO 70 J=MYJS_P5,MYJE_P5
      DO 70 I=MYIS_P4,MYIE_P4
        Q2ML(I,J,1)=0.
   70 CONTINUE
C
! !$omp parallel do 
      DO 80 L=2,LM+1 
      DO 80 J=MYJS_P5,MYJE_P5
      DO 80 I=MYIS_P4,MYIE_P4
        Q2ML(I,J,L)=Q2(I,J,L-1)
   80 CONTINUE
C***********************************************************************
! !$omp parallel do
! !$omp& private(adpdx,adpdy,adq,adq2l,adq2m,adt,adu,adv,
! !$omp&         array0,array1,array2,array3,dpde,f0,f1,f2,f3,
! !$omp&         few,fne,fns,fse,hm,i,ifp,ifq,ihh,ihl,ipq,isp,
! !$omp&         ispa,isq,isqa,iter2,ix,iy,j,ja,jak,l,pp,q2l,
! !$omp&         q2lew,q2lne,q2lns,q2lse,q2m,q2mew,q2mne,q2mns,
! !$omp&         q2mse,qew,qne,qns,qp,qse,qst,rdpd,rdpdx,rdpdy,
! !$omp&         tempa,tempb,tew,tne,tns,tse,tst,tta,ttb,udy,uew,une,
! !$omp&         uns,use,ust,vdx,vew,vm,vne,vns,vse,vst)
C***********************************************************************
                             DO 500 L=1,LM
C***********************************************************************
      CALL ZERO2(ADT)
      CALL ZERO2(ADU)
      CALL ZERO2(ADV)
      CALL ZERO2(ADQ2M)
      CALL ZERO2(ADQ2L)
      CALL ZERO2(DPDE)
      CALL ZERO2(FEW)
      CALL ZERO2(FNE)
      CALL ZERO2(FNS)
      CALL ZERO2(FSE)
      CALL ZERO2(Q2L)
      CALL ZERO2(Q2LEW)
      CALL ZERO2(Q2LNE)
      CALL ZERO2(Q2LSE)
      CALL ZERO2(Q2M)
      CALL ZERO2(Q2MEW)
      CALL ZERO2(Q2MNE)
      CALL ZERO2(Q2MSE)
      CALL ZERO2(RDPD)
      CALL ZERO2(TEMPA)
      CALL ZERO2(TEMPB)
      CALL ZERO2(TEW)
      CALL ZERO2(TNE)
      CALL ZERO2(TNS)
      CALL ZERO2(TSE)
      CALL ZERO2(TST)
      CALL ZERO2(UDY)
      CALL ZERO2(UEW)
      CALL ZERO2(UNE)
      CALL ZERO2(UNS)
      CALL ZERO2(USE)
      CALL ZERO2(UST)
      CALL ZERO2(VEW)
      CALL ZERO2(VNE)
      CALL ZERO2(VNS)
      CALL ZERO2(VSE)
      CALL ZERO2(VST)
      CALL ZERO2(VM)
C***********************************************************************
                             ITER2=.FALSE.
C-----------------------------------------------------------------------
      DO J=MYJS_P4,MYJE_P4
      DO I=MYIS_P4,MYIE_P4
c       Q2M(I,J)=0.
        Q2M(I,J)=Q2ML(I,J,L)
      ENDDO
      ENDDO
C
      DO 110 J=MYJS_P5,MYJE_P5
      DO 110 I=MYIS_P4,MYIE_P4
      HM(I,J)=HTM(I,J,L)*HBM2(I,J)
      DPDE(I,J)=PDSL(I,J)*DETA(L)
      RDPD(I,J)=1./DPDE(I,J)
      UST(I,J)=U(I,J,L)
      VST(I,J)=V(I,J,L)
      TST(I,J)=T(I,J,L)
      Q2L(I,J)=Q2ML(I,J,L+1)
  110 CONTINUE
C-----------------------------------------------------------------------
      DO 120 J=MYJS1_P4,MYJE1_P4
      DO 120 I=MYIS_P4,MYIE_P4
      VM(I,J)=VTM(I,J,L)*VBM2(I,J)
      ADPDX(I,J)=DPDE(I+IVW(J),J)+DPDE(I+IVE(J),J)
      ADPDY(I,J)=DPDE(I,J-1)+DPDE(I,J+1)
      RDPDX(I,J)=1./ADPDX(I,J)
      RDPDY(I,J)=1./ADPDY(I,J)
  120 CONTINUE
C--------------MASS FLUXES AND MASS POINTS ADVECTION COMPONENTS---------
C***
C***  THE NS AND EW FLUXES IN THE FOLLOWING LOOP ARE ON V POINTS
C***
  125 DO 130 J=MYJS1_P4,MYJE1_P4
      DO 130 I=MYIS_P4,MYIE_P4
      UDY(I,J)=UST(I,J)*DY
      FEW(I,J)=UDY(I,J)*ADPDX(I,J)
      TEW(I,J)=FEW(I,J)*(TST(I+IVE(J),J)-TST(I+IVW(J),J))
      Q2MEW(I,J)=FEW(I,J)*(Q2M(I+IVE(J),J)-Q2M(I+IVW(J),J))
      Q2LEW(I,J)=FEW(I,J)*(Q2L(I+IVE(J),J)-Q2L(I+IVW(J),J))
      VDX(I,J)=VST(I,J)*DX(I,J)
      FNS(I,J)=VDX(I,J)*ADPDY(I,J)
      TNS(I,J)=FNS(I,J)*(TST(I,J+1)-TST(I,J-1))
      Q2MNS(I,J)=FNS(I,J)*(Q2M(I,J+1)-Q2M(I,J-1))
      Q2LNS(I,J)=FNS(I,J)*(Q2L(I,J+1)-Q2L(I,J-1))
  130 CONTINUE
C--------------DIAGONAL FLUXES AND DIAGONALLY AVERAGED WIND-------------
C***
C***  THE NE AND SE FLUXES ARE ON H POINTS
C***  (ACTUALLY JUST TO THE NE AND SE OF EACH H POINT)
C***
      DO 145 J=MYJS2_P4,MYJE2_P4
      DO 145 I=MYIS_P4,MYIE_P4
      TEMPA(I,J)=UDY(I,J)+VDX(I,J)
      TEMPB(I,J)=UDY(I,J)-VDX(I,J)
  145 CONTINUE
C
      DO 150 J=MYJS2_P4,MYJE2_P4
      DO 150 I=MYIS_P4,MYIE_P4
      FNE(I,J)=(TEMPA(I+IHE(J),J)+TEMPA(I,J+1))
     1         *(DPDE(I,J)+DPDE(I+IHE(J),J+1))
      TNE(I,J)=FNE(I,J)*(TST(I+IHE(J),J+1)-TST(I,J))
      Q2MNE(I,J)=FNE(I,J)*(Q2M(I+IHE(J),J+1)-Q2M(I,J))
      Q2LNE(I,J)=FNE(I,J)*(Q2L(I+IHE(J),J+1)-Q2L(I,J))
      FSE(I,J)=(TEMPB(I+IHE(J),J)+TEMPB(I,J-1))
     1         *(DPDE(I,J)+DPDE(I+IHE(J),J-1))
      TSE(I,J)=FSE(I,J)*(TST(I+IHE(J),J-1)-TST(I,J))
      Q2MSE(I,J)=FSE(I,J)*(Q2M(I+IHE(J),J-1)-Q2M(I,J))
      Q2LSE(I,J)=FSE(I,J)*(Q2L(I+IHE(J),J-1)-Q2L(I,J))
  150 CONTINUE
C--------------THERMODYNAMIC EQUATION & MOISTURE------------------------
C***
C***  THE AD ARRAYS IN THE 170 LOOP ARE ON H POINTS
C***
      DO 170 J=MYJS5_P2,MYJE5_P2
      DO 170 I=MYIS_P2,MYIE_P2
      ADT(I,J)=(TEW(I+IHW(J),J)+TEW(I+IHE(J),J)+TNS(I,J-1)+TNS(I,J+1)
     1         +TNE(I+IHW(J),J-1)+TNE(I,J)+TSE(I,J)+TSE(I+IHW(J),J+1))
     2         *RDPD(I,J)*FAD(I,J)
      ADQ2M(I,J)=(Q2MEW(I+IHW(J),J)+Q2MEW(I+IHE(J),J)
     1           +Q2MNS(I,J-1)+Q2MNS(I,J+1)
     2           +Q2MNE(I+IHW(J),J-1)+Q2MNE(I,J)
     3           +Q2MSE(I,J)+Q2MSE(I+IHW(J),J+1))
     4           *RDPD(I,J)*FAD(I,J)
      ADQ2L(I,J)=(Q2LEW(I+IHW(J),J)+Q2LEW(I+IHE(J),J)
     1           +Q2LNS(I,J-1)+Q2LNS(I,J+1)
     2           +Q2LNE(I+IHW(J),J-1)+Q2LNE(I,J)
     3           +Q2LSE(I,J)+Q2LSE(I+IHW(J),J+1))
     4           *RDPD(I,J)*FAD(I,J)
  170 CONTINUE
C-----------------------------------------------------------------------
C--------------UPSTREAM ADVECTION OF T, Q AND Q2------------------------
C-----------------------------------------------------------------------
      IF(UPSTRM)THEN
        DO 171 JAK=1,JAKONE
        JA=KHHAS(JAK)
        I =IHLAS(JAK)
        J =JHLAS(JAK)
        IX=I-MY_IS_GLB+1
        JX=J-MY_JS_GLB+1
        TTA=EMT(JA)*(UST(IX,JX-1)+UST(IX+IHW(JX),JX)
     1              +UST(IX+IHE(JX),JX)+UST(IX,JX+1))
        TTB=ENT    *(VST(IX,JX-1)+VST(IX+IHW(JX),JX)
     1              +VST(IX+IHE(JX),JX)+VST(IX,JX+1))
        PP=-TTA-TTB
        QP= TTA-TTB
C
        IF(PP.LT.0.)THEN
          ISPA(JAK)=-1
        ELSE
          ISPA(JAK)= 1
        ENDIF
C
        IF(QP.LT.0.)THEN
          ISQA(JAK)=-1
        ELSE
          ISQA(JAK)= 1
        ENDIF
C
        PP=ABS(PP)
        QP=ABS(QP)
        ARRAY3(JAK)=PP*QP
        ARRAY0(JAK)=ARRAY3(JAK)-PP-QP
        ARRAY1(JAK)=PP-ARRAY3(JAK)
        ARRAY2(JAK)=QP-ARRAY3(JAK)
  171   CONTINUE
C
        JAK=0
        DO 173 JA=1,JAM
        IHL=IHLA(JA)
        IHH=IHHA(JA)
        J=JRA(JA)
        IF(.NOT.LJRA(JA))GO TO 173
C
        DO I=IHL,IHH
        IF(I.GE.MY_IS_GLB-ILPAD2.AND.I.LE.MY_IE_GLB+IRPAD2)THEN
          JAK=JAK+1
          ISP=ISPA(JAK)
          ISQ=ISQA(JAK)
          IFP=(ISP-1)/2
          IFQ=(-ISQ-1)/2
          IPQ=(ISP-ISQ)/2
C
          IX=I-MY_IS_GLB+1
          JX=J-MY_JS_GLB+1
C
          IF(HTM(IX+IHE(JX)+IFP,JX+ISP,L)*HTM(IX+IHE(JX)+IFQ,JX+ISQ,L)
     1      *HTM(IX+IPQ,JX+ISP+ISQ,L).GT.0.1)GO TO 172
C
          IF(HTM(IX+IHE(JX)+IFP,JX+ISP,L)+HTM(IX+IHE(JX)+IFQ,JX+ISQ,L)
     1      +HTM(IX+IPQ,JX+ISP+ISQ,L).LT.0.1)THEN
C
            TST(IX+IHE(JX)+IFP,JX+ISP)=TST(IX,JX)
            TST(IX+IHE(JX)+IFQ,JX+ISQ)=TST(IX,JX)
            TST(IX+IPQ,JX+ISP+ISQ)    =TST(IX,JX)
C
          ELSEIF
     1     (HTM(IX+IHE(JX)+IFP,JX+ISP,L)+HTM(IX+IPQ,JX+ISP+ISQ,L)
     2      .LT.0.99)THEN
C
            TST(IX+IHE(JX)+IFP,JX+ISP)=TST(IX,JX)
            TST(IX+IPQ,JX+ISP+ISQ)    =TST(IX+IHE(JX)+IFQ,JX+ISQ)
C
          ELSEIF
     1     (HTM(IX+IHE(JX)+IFQ,JX+ISQ,L)+HTM(IX+IPQ,JX+ISP+ISQ,L)
     2      .LT.0.99)THEN
C
            TST(IX+IHE(JX)+IFQ,JX+ISQ)=TST(IX,JX)
            TST(IX+IPQ,JX+ISP+ISQ)    =TST(IX+IHE(JX)+IFP,JX+ISP)
C
          ELSEIF
     1     (HTM(IX+IHE(JX)+IFP,JX+ISP,L)+HTM(IX+IHE(JX)+IFQ,JX+ISQ,L)
     2      .LT.0.99)THEN
            TST(IX+IHE(JX)+IFP,JX+ISP)=
     1                    0.5*(TST(IX,JX)+TST(IX+IPQ,JX+ISP+ISQ))
            TST(IX+IHE(JX)+IFQ,JX+ISQ)=TST(IX+IHE(JX)+IFP,JX+ISP)
C
          ELSEIF(HTM(IX+IHE(JX)+IFP,JX+ISP,L).LT.0.99)THEN
            TST(IX+IHE(JX)+IFP,JX+ISP)=
     1        TST(IX,JX)+TST(IX+IPQ,JX+ISP+ISQ)
     2                  -TST(IX+IHE(JX)+IFQ,JX+ISQ)
C
          ELSEIF(HTM(IX+IHE(JX)+IFQ,JX+ISQ,L).LT.0.99)THEN
            TST(IX+IHE(JX)+IFQ,JX+ISQ)=
     1        TST(IX,JX)+TST(IX+IPQ,JX+ISP+ISQ)
     2                  -TST(IX+IHE(JX)+IFP,JX+ISP)
C
          ELSE
            TST(IX+IPQ,JX+ISP+ISQ)=
     1        TST(IX+IHE(JX)+IFP,JX+ISP)
     2       +TST(IX+IHE(JX)+IFQ,JX+ISQ)-TST(IX,JX)
C
          ENDIF
C
  172     CONTINUE
C
          F0=ARRAY0(JAK)
          F1=ARRAY1(JAK)
          F2=ARRAY2(JAK)
          F3=ARRAY3(JAK)
          ADT(IX,JX)=F0*TST(IX,JX)+F1*TST(IX+IHE(JX)+IFP,JX+ISP)
     1                            +F2*TST(IX+IHE(JX)+IFQ,JX+ISQ)
     2                            +F3*TST(IX+IPQ,JX+ISP+ISQ)
        ENDIF
C
        ENDDO
  173   CONTINUE
C
        DO 175 JAK=1,JAKONE
        I=IHLAS(JAK)
        J=JHLAS(JAK)
C
        IX=I-MY_IS_GLB+1
        JX=J-MY_JS_GLB+1
C
        ISP=ISPA(JAK)
        ISQ=ISQA(JAK)
        IFP=(ISP-1)/2
        IFQ=(-ISQ-1)/2
        IPQ=(ISP-ISQ)/2
        F0=ARRAY0(JAK)
        F1=ARRAY1(JAK)
        F2=ARRAY2(JAK)
        F3=ARRAY3(JAK)
        ADQ2M(IX,JX)=F0*Q2M(IX,JX)+F1*Q2M(IX+IHE(JX)+IFP,JX+ISP)
     1                            +F2*Q2M(IX+IHE(JX)+IFQ,JX+ISQ)
     2                            +F3*Q2M(IX+IPQ,JX+ISP+ISQ)
        ADQ2L(IX,JX)=F0*Q2L(IX,JX)+F1*Q2L(IX+IHE(JX)+IFP,JX+ISP)
     1                            +F2*Q2L(IX+IHE(JX)+IFQ,JX+ISQ)
     2                            +F3*Q2L(IX+IPQ,JX+ISP+ISQ)
 175    CONTINUE
c
      ENDIF
C***
C***  END OF THIS UPSTREAM REGION
C***
C--------------CALCULATION OF MOMENTUM ADVECTION COMPONENTS-------------
C***
C***  THE FOLLOWING EW AND NS ARRAYS ARE ON H POINTS
C***
      DO 180 J=MYJS4_P4,MYJE4_P4
      DO 180 I=MYIS_P4,MYIE_P4
      UEW(I,J)=(FEW(I+IHW(J),J)+FEW(I+IHE(J),J))
     1         *(UST(I+IHE(J),J)-UST(I+IHW(J),J))
      UNS(I,J)=(FNS(I+IHW(J),J)+FNS(I+IHE(J),J))
     1         *(UST(I,J+1)-UST(I,J-1))
      VEW(I,J)=(FEW(I,J-1)+FEW(I,J+1))
     1         *(VST(I+IHE(J),J)-VST(I+IHW(J),J))
      VNS(I,J)=(FNS(I,J-1)+FNS(I,J+1))*(VST(I,J+1)-VST(I,J-1))
C***
C***  THE FOLLOWING NE AND SE ARRAYS ARE TIED TO V POINTS
C***
      UNE(I,J)=(FNE(I+IVW(J),J)+FNE(I+IVE(J),J))
     1         *(UST(I+IVE(J),J+1)-UST(I,J))
      USE(I,J)=(FSE(I+IVW(J),J)+FSE(I+IVE(J),J))
     1         *(UST(I+IVE(J),J-1)-UST(I,J))
      VNE(I,J)=(FNE(I,J-1)+FNE(I,J+1))*(VST(I+IVE(J),J+1)-VST(I,J))
      VSE(I,J)=(FSE(I,J-1)+FSE(I,J+1))*(VST(I+IVE(J),J-1)-VST(I,J))
  180 CONTINUE
C--------------EQUATION OF MOTION---------------------------------------
C***
C***  ADU AND ADV ARE ON V POINTS
C***
      DO 200 J=MYJS5_P2,MYJE5_P2
      DO 200 I=MYIS_P2,MYIE_P2
      ADU(I,J)=(UEW(I+IVW(J),J)+UEW(I+IVE(J),J)+UNS(I,J-1)+UNS(I,J+1)
     1       +UNE(I+IVW(J),J-1)+UNE(I,J)+USE(I,J)+USE(I+IVW(J),J+1))
     2       *RDPDX(I,J)*FAD(I+IVW(J),J)
      ADV(I,J)=(VEW(I+IVW(J),J)+VEW(I+IVE(J),J)+VNS(I,J-1)+VNS(I,J+1)
     1       +VNE(I+IVW(J),J-1)+VNE(I,J)+VSE(I,J)+VSE(I+IVW(J),J+1))
     2       *RDPDY(I,J)*FAD(I+IVW(J),J)
  200 CONTINUE
C
C--------------UPSTREAM ADVECTION OF VELOCITY COMPONENTS----------------
C
      IF(UPSTRM)THEN
        DO 205 JAK=1,JAKTWO
        JA=KVHAS(JAK)
        I=IVLAS(JAK)
        J=JVLAS(JAK)
C
        IX=I-MY_IS_GLB+1
        JX=J-MY_JS_GLB+1
C
        TTA=EM(JA)*UST(IX,JX)
        TTB=EN    *VST(IX,JX)
        PP=-TTA-TTB
        QP=TTA-TTB
C
        IF(PP.LT.0.)THEN
          ISP=-1
        ELSE
          ISP= 1
        ENDIF
C
        IF(QP.LT.0.)THEN
          ISQ=-1
        ELSE
          ISQ= 1
        ENDIF
C
        IFP=(ISP-1)/2
        IFQ=(-ISQ-1)/2
        IPQ=(ISP-ISQ)/2
        PP=ABS(PP)
        QP=ABS(QP)
        F3=PP*QP
        F0=F3-PP-QP
        F1=PP-F3
        F2=QP-F3
        ADU(IX,JX)=F0*UST(IX,JX)+F1*UST(IX+IVE(JX)+IFP,JX+ISP)
     1                          +F2*UST(IX+IVE(JX)+IFQ,JX+ISQ)
     2                          +F3*UST(IX+IPQ,JX+ISP+ISQ)
        ADV(IX,JX)=F0*VST(IX,JX)+F1*VST(IX+IVE(JX)+IFP,JX+ISP)
     1                          +F2*VST(IX+IVE(JX)+IFQ,JX+ISQ)
     2                          +F3*VST(IX+IPQ,JX+ISP+ISQ)
  205   CONTINUE
      ENDIF
C***
C***  END OF THIS UPSTREAM REGION
C***
C-----------------------------------------------------------------------
      IF(ITER2)GO TO 235
C-----------------------------------------------------------------------
      DO 220 J=MYJS2_P2,MYJE2_P2
      DO 220 I=MYIS1_P2,MYIE1_P2
      TST(I,J)=ADT  (I,J)*(HM(I,J)*TLC)+TST(I,J)
      Q2M(I,J)=ADQ2M(I,J)*(HM(I,J)*TLC)+Q2M(I,J)
      Q2L(I,J)=ADQ2L(I,J)*(HM(I,J)*TLC)+Q2L(I,J)
  220 CONTINUE
C
      DO 230 J=MYJS2_P2,MYJE2_P2
      DO 230 I=MYIS1_P2,MYIE1_P2
      UST(I,J)=ADU(I,J)*VM(I,J)*TLC+UST(I,J)
      VST(I,J)=ADV(I,J)*VM(I,J)*TLC+VST(I,J)
  230 CONTINUE
C-----------------------------------------------------------------------
      ITER2=.TRUE.
      GO TO 125
C-----------------------------------------------------------------------
  235 DO 240 J=MYJS2,MYJE2
      DO 240 I=MYIS1,MYIE1
      T(I,J,L)=ADT(I,J)*(2.0*HM(I,J))+T(I,J,L)
  240 CONTINUE
C
      DO 250 J=MYJS2,MYJE2
      DO 250 I=MYIS1,MYIE1
      U(I,J,L)=ADU(I,J)*(2.0*VM(I,J))+U(I,J,L)
      V(I,J,L)=ADV(I,J)*(2.0*VM(I,J))+V(I,J,L)
  250 CONTINUE
C-----------------------------------------------------------------------
      IF(L.EQ.1)THEN  
        DO 260 J=MYJS2,MYJE2
        DO 260 I=MYIS1,MYIE1
        ADQ2HL(I,J,1)=ADQ2L(I,J)
  260   CONTINUE
      ELSE
        DO 270 J=MYJS2,MYJE2
        DO 270 I=MYIS1,MYIE1
        ADQ2HL(I,J,L)=ADQ2L(I,J)
        Q2(I,J,L-1)=ADQ2M(I,J)*HM(I,J)+Q2(I,J,L-1)
  270   CONTINUE
      ENDIF
C***********************************************************************
  500                        CONTINUE
C***********************************************************************
! !$omp parallel do private(hm)
      DO 600 L=2,LM
      DO J=MYJS2,MYJE2
      DO I=MYIS1,MYIE1
        HM(I,J)=HTM(I,J,L)*HBM2(I,J)
        Q2(I,J,L-1)=ADQ2HL(I,J,L-1)*HM(I,J)+Q2(I,J,L-1)
      ENDDO
      ENDDO
  600 CONTINUE
C-----------------------------------------------------------------------
                             RETURN
                             END
