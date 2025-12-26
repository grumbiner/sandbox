       SUBROUTINE FOPMULT(Z,FZ,FOP,NCOEFS,NFOP,IFOP,
     *              WORK,BFOP,BONES,BWORK)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    FOPMULT     APPLY OPERATOR "F" FROM TEMPERTON SCHEME
C   PRGMMR: PARRISH          ORG: W/NMC22    DATE: 88-06-13
C
C ABSTRACT: APPLY OPERATOR "F" USED IN TEMPERTON SLOW-FAST SEPERATION
C
C PROGRAM HISTORY LOG:
C   88-06-13  PARRISH
C
C USAGE:    CALL FOPMULT(Z,FZ,FOP,NCOEFS,NFOP,IFOP,WORK,
C                          BFOP,BONES,BWORK)
C   INPUT ARGUMENT LIST:
C     Z        - INPUT COEFFICIENTS
C     FOP      - ELEMENTS OF "F" OPERATOR
C     NCOEFS   - NUMBER OF COEFFICIENTS
C     NFOP     - USED WITH FOP
C     IFOP     - USED WITH FOP
C     BFOP     - (LOGICAL ARRAY) USED WITH FOP
C     BONES    - (LOGICAL ARRAY) USED WITH FOP
C
C   OUTPUT ARGUMENT LIST:
C     FZ       - Z MULTIPLIED BY "F"
C     WORK     - WORK ARRAY
C     BWORK    - (LOGICAL ARRAY) WORK ARRAY
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN200
C   MACHINE:  CYBER
C
C$$$
         include "myparam"
C--------
         real Z(NCOEFS),FZ(NCOEFS),FOP(NCOEFS),WORK(NCOEFS)
         LOGICAL BFOP(NCOEFS),BONES(NCOEFS),BWORK(NCOEFS)
C--------
         ii=0
         do 10002 i=1,ncoefs
           if(bfop(i)) then
             ii=ii+1
             work(ifop+ii-1)=z(i)
           end if
10002    continue
         do 10004 i=1,ifop
         FZ(i)=0.
10004    continue
         do 10006 i=1,nfop
         FZ(IFOP+i-1)=WORK(IFOP+i-1)*FOP(IFOP+i-1)
         WORK(IFOP+i-1)=FOP(IFOP+i-1)*Z(IFOP+i-1)
10006    continue
         ii1=0
         ii2=0
         ii3=0
         do 10008 i=1,ncoefs
           if(bones(i).and..not.bfop(i)) then
             ii3=ii3+1
             ii1=ii1+1
             fz(ii3)=fz(ii1)
           end if
           if(.not.bones(i).and.bfop(i)) then
             ii3=ii3+1
             ii2=ii2+1
             fz(ii3)=work(ifop+ii2-1)
           end if
           if(bones(i).and.bfop(i)) then
             ii3=ii3+1
             ii1=ii1+1
             ii2=ii2+1
             fz(ii3)=fz(ii1)+work(ifop+ii2-1)
           end if
10008    continue
       RETURN
       END
