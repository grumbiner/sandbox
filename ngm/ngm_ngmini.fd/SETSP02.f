       SUBROUTINE SETSP02(JCAPIN)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    SETSP02     SET CONSTS FOR SLOW-FAST PACKAGE
C   PRGMMR: PARRISH          ORG: W/NMC22    DATE: 88-06-13
C
C ABSTRACT: SET UP ALL INTERNAL VARIABLES REQUIRED FOR SLOW-FAST
C   DECOMPOSITION USED BY TEMPERTON.
C
C PROGRAM HISTORY LOG:
C   88-06-13  PARRISH
C
C USAGE:    CALL SETSP02
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
         IF(JCAP.NE.0) THEN
           CALL FREECOEF(ISCALZD,1)
           CALL FREECOEF(IUNSCLZD,1)
           CALL FREECOEF(IBSHAT,1)
           CALL FREECOEF(IBVHAT,1)
           CALL FREECOEF(IBVIHAT,1)
           CALL FREECOEF(IBOP,1)
           CALL FREECOEF(ICOP,1)
           CALL FREECOEF(IFOP,1)
           CALL FREECOEF(ICINVOP,1)
           CALL freecoef(IRFOP,1)
           CALL freecoef(IRCINVOP,1)
         END IF
         CALL GETCOEF(ISCALZD,1)
         CALL GETCOEF(IUNSCLZD,1)
         CALL GETCOEF(IBSHAT,1)
         CALL GETCOEF(IBVHAT,1)
         CALL GETCOEF(IBVIHAT,1)
         CALL GETCOEF(IBOP,1)
         CALL GETCOEF(ICOP,1)
         CALL GETCOEF(IFOP,1)
         CALL GETCOEF(ICINVOP,1)
         CALL getcoef(IRFOP,1)
         CALL getcoef(IRCINVOP,1)
         JCAP=MIN(IJCAPDP,JCAPIN)
         NCOEFS=(JCAP+1)*(JCAP+2)
         do 10002 i=1,ncoefs
         BONES(i)=.true.
10002    continue
         IB=1
         DO 100 M=0,JCAP
           LEN1=2*JCAP+2-2*M
           LEN2=LEN1-2
           do 10004 i=1,len2
             bfop(ib+i-1)=.true.
10004      continue
           do 10006 i=len2+1,len1
             bfop(ib+i-1)=.false.
10006      continue
           IB=IB+LEN1
100      CONTINUE
         JFOP=2*JCAP+3
         NFOP=NCOEFS-JFOP+1
         CALL GETTOPS(RCF(1,IRFOP),RCF(1,IRCINVOP),COF(1,ISCALZD),
     *       COF(1,IUNSCLZD),COF(1,IFOP),COF(1,IBOP),COF(1,ICOP),
     *       COF(1,ICINVOP),COF(1,IBSHAT),COF(1,IBVHAT),COF(1,IBVIHAT),
     *                 JCAP)
       RETURN
       END
