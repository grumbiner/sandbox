       SUBROUTINE GETTOPS(RFOP,RCINVOP,SCALZD,UNSCLZD,FOP,BOP,COP,
     *                    CINVOP,BSHAT,BVHAT,BVIHAT,JCAP)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    GETTOPS     GENERATE SOME OF TEMPERTON OPERATORS
C   PRGMMR: PARRISH          ORG: W/NMC22    DATE: 88-06-15
C
C ABSTRACT: GENERATE TRANSFORM SPACE FORM OF VARIOUS OPERATORS
C   USED IN SLOW-FAST PACKAGE.
C
C PROGRAM HISTORY LOG:
C   88-06-15  PARRISH
C
C USAGE:    CALL GETTOPS(RFOP,RCINVOP,SCALZD,UNSCLZD,FOP,CINVOP)
C   OUTPUT ARGUMENT LIST:
C       real VARIABLES:
C     RFOP     - FULL PRECISION COPY OF "F" OPERATOR
C     RCINVOP  - FULL PRECISION COPY OF INVERSE OF "C" OPERATOR
C     SCALZD   - SCALING FOR VORTICITY AND DIVERGENCE
C     UNSCLZD  - INVERSE OF SCALZD
C     FOP      - real VERSION OF "F" OPERATOR
C     BOP      - real VERSION OF "B" OPERATOR
C     COP      - real VERSION OF "C" OPERATOR
C     CINVOP   - real COPY OF INVERSE OF "C" OPERATOR
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN77
C   MACHINE:  CYBER
C
C$$$
         include "myparam"
         REAL RFOP(1),RCINVOP(1)
         real SCALZD(1),UNSCLZD(1),FOP(1),CINVOP(1)
         real BOP(1),COP(1)
         real BSHAT(1),BVHAT(1),BVIHAT(1)
C--------
         COMMON/DAVEBUFF/ COF(INCOEFS,INUMCOEF),GRD(INLOLA,INUMGRID)
         real COF,GRD
         REAL RCF(INCOEFS,2)
         EQUIVALENCE (COF,RCF)
C--------
         CALL getcoef(IARL,1)
         CALL getcoef(IARN,1)
         CALL getcoef(IRF,1)
         IAD=1
         DO 200 M=0,JCAP
           DO 100 L=0,JCAP-M
             N=M+L
             RCF(IAD,IARL)=L
             RCF(IAD+1,IARL)=L
             RCF(IAD,IARN)=N
             RCF(IAD+1,IARN)=N
             IAD=IAD+2
100        CONTINUE
200      CONTINUE
         ITOT=IAD-1
         NV=(JCAP+1)*(JCAP+2)
         N3=NV-2*JCAP-2
         I3=2*JCAP+3
         do 10002 i=1,nv
         RCF(i,IRF)=RCF(i,IARN)*(RCF(i,IARN)+1.)
         COP(i)=SQRT(RCF(i,IRF))
         UNSCLZD(i)=SQRT(RCF(i,IRF))/6.3712E6
10002    continue
         RCF(1,IRF)=0.
         RCF(2,IRF)=0.
         do 10004 i=3,nv
         RCF(i,IRF)=1./RCF(i,IRF)
10004    continue
         do 10006 i=1,nv
         RCINVOP(i)=SQRT(RCF(i,IRF))
         CINVOP(i)=RCINVOP(i)
         BOP(i)=2.*RCF(i,IARL)*RCF(i,IRF)
         SCALZD(i)=6.3712E6*CINVOP(i)
10006    continue
        do 10008 i=1,n3
         RCF(i3+i-1,IRF)=(RCF(i3+i-1,IARN)-RCF(i3+i-1,IARL))
     *            *(RCF(i3+i-1,IARN)+RCF(i3+i-1,IARL))/
     *        ((2.*RCF(i3+i-1,IARN)-1.)
     *        *(2.*RCF(i3+i-1,IARN)+1.))
         RCF(i3+i-1,IRF)=(RCF(i3+i-1,IARN)+1.)
     *       *(RCF(i3+i-1,IARN)-1.)*RCF(i3+i-1,IRF)/
     *           (RCF(i3+i-1,IARN)*RCF(i3+i-1,IARN))
         RCF(i3+i-1,IRF)=SQRT(RCF(i3+i-1,IRF))
10008    continue
         do 10010 i=1,2*jcap+2
         RFOP(i)=0.
10010    continue
         do 10012 i=1,n3
         RFOP(I3+i-1)=2.*RCF(I3+i-1,IRF)
10012    continue
         do 10014 i=1,nv
         FOP(i)=RFOP(i)
10014    continue
         CALL freecoef(IARL,1)
         CALL freecoef(IARN,1)
         CALL freecoef(IRF,1)
         do 10016 i=1,nv
         BSHAT(i)=0.
         BVHAT(i)=0.
         BVIHAT(i)=0.
10016    continue
         IAD=1
         DO 900 MP1=1,JCAP+1
           BSHAT(IAD)=1.
           BSHAT(IAD+1)=0.
           BVHAT(IAD)=1.
           BVHAT(IAD+1)=0.
           BVIHAT(IAD)=0.
           BVIHAT(IAD+1)=1.
           IAD=IAD+2
           IF(MP1.LE.JCAP) THEN
             DO 800 LP1=2,JCAP+2-MP1
               BSHAT(IAD)=1.
               BSHAT(IAD+1)=1.
               BVHAT(IAD)=1.
               BVHAT(IAD+1)=1.
               BVIHAT(IAD)=1.
               BVIHAT(IAD+1)=1.
               IAD=IAD+2
800          CONTINUE
           END IF
900      CONTINUE
         BVHAT(1)=0.
         BVIHAT(2)=0.
       RETURN
       END
