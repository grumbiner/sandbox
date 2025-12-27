       SUBROUTINE SOLVTRID(IRF,IRZ,IRWORK,IRC,IRB,IRR,RC0INV)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    SOLVTRID    SOLVE TRIDIAG TEMPERTON MATRIX
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
C-------- SET UP TRIDIAGONAL MATRIX TO INVERT
C--------
         JCAP1=JCAP+1
         do 10002 i=1,ncoefs
         RCF(i,IRC)=0.
10002    continue
         NM1VECT=2*JCAP1
         NVECT=NM1VECT-2
         NM1=1
         N=NM1+NM1VECT
         do 10004 i=1,ncoefs
         RCF(i,IRB)=RCF(i,IRFOP)*RCF(i,IRFOP)
10004    continue
         DO 100 M=2,JCAP1
         do 10006 i=1,nvect
           RCF(N+i-1,IRC)=RCF(N+i-1,IRFOP)*RC0INV*RC0INV
     *                      *RCF(NM1+i-1,IRCINVOP)
10006    continue
           NM1=N
           N=N+NVECT
           NVECT=NVECT-2
100      CONTINUE
         NVECT=2*JCAP1
         NP1VECT=NVECT-2
         N=1
         NP1=N+NVECT
         DO 200 M=1,JCAP
         do 10008 i=1,np1vect
           RCF(N+i-1,IRC)=RCF(N+i-1,IRC)
     *          *RCF(NP1+i-1,IRCINVOP)*RCF(NP1+i-1,IRFOP)
           RCF(N+i-1,IRB)=RCF(N+i-1,IRB)
     *          +RCF(NP1+i-1,IRFOP)*RCF(NP1+i-1,IRFOP)
10008    continue
           N=NP1
           NP1=NP1+NP1VECT
           NP1VECT=NP1VECT-2
200      CONTINUE
         do 10010 i=1,ncoefs
         RCF(i,IRB)=RCF(i,IRCINVOP)
     *                    *RCF(i,IRCINVOP)*RC0INV*RC0INV
     *         *RCF(i,IRB)+1.
10010    continue
C--------
C-------- DOWNWARD SWEEP
C--------
         NM2VECT=2*JCAP1
         NM1VECT=NM2VECT-2
         NVECT=NM1VECT-2
         NM2=1
         NM1=NM2+NM2VECT
         N=NM1+NM1VECT
         DO 300 M=3,JCAP1
           do 10012 i=1,nvect
           RCF(i,IRR)=RCF(NM1+i-1,IRC)/RCF(NM2+i-1,IRB)
           RCF(N+i-1,IRF)=RCF(N+i-1,IRF)
     *                  -RCF(i,IRR)*RCF(NM2+i-1,IRF)
           RCF(N+i-1,IRB)=RCF(N+i-1,IRB)
     *                  -RCF(i,IRR)*RCF(NM1+i-1,IRC)
10012      continue
           NM2=NM1
           NM1=N
           N=N+NVECT
           NVECT=NVECT-2
300      CONTINUE
C--------
C-------- SOLUTION PHASE
C--------
         RCF(NCOEFS,IRF)=RCF(NCOEFS,IRF)/RCF(NCOEFS,IRB)
         RCF(NCOEFS-1,IRF)=RCF(NCOEFS-1,IRF)/RCF(NCOEFS-1,IRB)
         RCF(NCOEFS-2,IRF)=RCF(NCOEFS-2,IRF)/RCF(NCOEFS-2,IRB)
         RCF(NCOEFS-3,IRF)=RCF(NCOEFS-3,IRF)/RCF(NCOEFS-3,IRB)
         RCF(NCOEFS-4,IRF)=RCF(NCOEFS-4,IRF)/RCF(NCOEFS-4,IRB)
         RCF(NCOEFS-5,IRF)=RCF(NCOEFS-5,IRF)/RCF(NCOEFS-5,IRB)
C--------
         NP2=NCOEFS-1
         NP2VECT=2
         NP1VECT=NP2VECT+2
         NVECT=NP1VECT+2
         NP1=NP2-NP1VECT
         N=NP1-NVECT
         DO 400 M=JCAP1-2,1,0-1
           do 10014 i=1,np2vect
           RCF(N+i-1,IRF)=RCF(N+i-1,IRF)
     *            -RCF(NP1+i-1,IRC)*RCF(NP2+i-1,IRF)
10014      continue
           do 10016 i=1,nvect
           RCF(N+i-1,IRF)=RCF(N+i-1,IRF)/RCF(N+i-1,IRB)
10016      continue
           NP2=NP1
           NP1=N
           NP2VECT=NP1VECT
           NP1VECT=NVECT
           NVECT=NVECT+2
           N=N-NVECT
400      CONTINUE
       RETURN
       END
