       SUBROUTINE SANDF(Z,D,H,ZS,DF,HF,DEPTH,JCAPIN)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    SANDF       SLOW+FAST TO SLOW-AND-FAST TRANSFORM
C   PRGMMR: PARRISH          ORG: W/NMC22    DATE: 88-06-13
C
C ABSTRACT: GO FROM FULL VARIABLES TO TEMPERTON SLOW-AND-FAST
C   DECOMPOSITION.
C
C PROGRAM HISTORY LOG:
C   88-06-13  PARRISH
C
C USAGE:    CALL SANDF(Z,D,H,ZS,DF,HF,DEPTH)
C   INPUT ARGUMENT LIST:
C     Z        - SLOW + FAST VORTICITY
C     D        - DIVERGENCE.
C     H        - SLOW + FAST HEIGHT
C     DEPTH    - SCALE HEIGHT.
C
C   OUTPUT ARGUMENT LIST:
C     ZS       - SLOW COMPONENT OF VORTICITY.
C     DF       - FAST DIVERGENCE (NOT CHANGED FROM INPUT--SLOW DIV=0)
C     HF       - FAST HEIGHT COMPONENT
C
C REMARKS: MUST INCLUDE BLOCK DATA BLOCKSP02 AND DEFINITION OF
C           PL1 VARIABLES BEFORE COMPILING.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN200
C   MACHINE:  CYBER
C
C$$$
         include "myparam"
C--------
         real Z(1),D(1),H(1),ZS(1),DF(1),HF(1)
C--------
         REAL RC0INV
         LOGICAL BWORK(INCOEFS)
C--------
         COMMON/DAVEBUFF/ COF(INCOEFS,INUMCOEF),GRD(INLOLA,INUMGRID)
         real COF,GRD
         REAL RCF(INCOEFS,2)
         EQUIVALENCE (COF,RCF)
C--------
         COMMON/FIXSP02/JCAP,NCOEFS,NFOP,JFOP,
     *      ISCALZD,IUNSCLZD,
     *      IBSHAT,IBVHAT,IBVIHAT,
     *      IBOP,ICOP,IFOP,ICINVOP,
     *      IRFOP,IRCINVOP,
     *      BFOP(INCOEFS),BONES(INCOEFS)
         LOGICAL BFOP,BONES
C--------
C-------- CHECK TO SEE IF INITIALIZATION OF INTERNAL VARIABLES NEEDED
         IF(JCAP.NE.JCAPIN) CALL SETSP02(JCAPIN)
C--------
C-------- MAKE SURE ZONAL PART OF COEFS HAS PROPER FORM
C--------
         do 10002 i=1,ncoefs
         Z(i)=COF(i,IBVHAT)*Z(i)
         D(i)=COF(i,IBVIHAT)*D(i)
         H(i)=COF(i,IBSHAT)*H(i)
10002    continue
         CALL getcoef(IRF,1)
         CALL getcoef(IRZ,1)
         CALL getcoef(IRWORK,1)
         CALL getcoef(IRC,1)
         CALL getcoef(IRB,1)
         CALL getcoef(IRR,1)
         RC0INV=6.3712E6*7.2921E-5/SQRT(9.8062*DEPTH)
         JCAP1=JCAP+1
C--------
C-------- OBTAIN RHS OF EQUATION TO SOLVE FOR FAST HEIGHT
C--------
         do 10004 i=1,ncoefs
         RCF(i,IRZ)=Z(i)
10004    continue
         CALL FOPMULT(RCF(1,IRZ),RCF(1,IRF),RCF(1,IRFOP),NCOEFS,NFOP,
     *                 JFOP,RCF(1,IRWORK),BFOP,BONES,BWORK)
         do 10006 i=1,ncoefs
         RCF(i,IRF)=RC0INV*RCF(i,IRCINVOP)
     *              *RCF(i,IRF)+H(i)
10006    continue
C--------
C-------- SOLVE TRIDIAGONAL MATRIX EQUATION FOR FAST HEIGHT
C--------
         CALL SOLVTRID(IRF,IRZ,IRWORK,IRC,IRB,IRR,RC0INV)
         do 10008 i=1,ncoefs
         HF(i)=RCF(i,IRF)
10008    continue
C--------
C-------- COMPUTE FAST COMPONENT OF VORTICITY
C--------
         do 10010 i=1,ncoefs
         RCF(i,IRZ)=RC0INV
     *       *RCF(i,IRCINVOP)*RCF(i,IRF)
10010    continue
         CALL FOPMULT(RCF(1,IRZ),RCF(1,IRF),RCF(1,IRFOP),NCOEFS,NFOP,
     *        JFOP,RCF(1,IRWORK),BFOP,BONES,BWORK)
C-------- NOW GET SLOW COMPONENT OF VORTICITY
         do 10012 i=1,ncoefs
         ZS(i)=Z(i)-RCF(i,IRF)
10012    continue
C-------- FINALLY DIVERGENCE
         do 10014 i=1,ncoefs
         DF(i)=D(i)
10014    continue
         CALL freecoef(IRF,1)
         CALL freecoef(IRZ,1)
         CALL freecoef(IRWORK,1)
         CALL freecoef(IRC,1)
         CALL freecoef(IRB,1)
         CALL freecoef(IRR,1)
C--------
C-------- MAKE SURE ZONAL PART OF COEFS HAS PROPER FORM
C--------
         do 10016 i=1,ncoefs
         ZS(i)=COF(i,IBVHAT)*ZS(i)
         DF(i)=COF(i,IBVIHAT)*DF(i)
         HF(i)=COF(i,IBSHAT)*HF(i)
10016    continue
       RETURN
       END
