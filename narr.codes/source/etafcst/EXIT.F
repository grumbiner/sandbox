      SUBROUTINE EXITT(NAME)
C--------------------------------------------------------------
      INCLUDE "parmeta"
      INCLUDE "parm.tbl"
      INCLUDE "cuparm"
      INCLUDE "parmsoil"
      INCLUDE "mpp.h"
      INCLUDE "mpif.h"
C--------------------------------------------------------------
      PARAMETER (JAM=6+2*(JM-10),LB=2*IM+JM-3
     1,          LM1=LM-1,LP1=LM+1,IMT=2*IM-1)
C--------------------------------------------------------------
      CHARACTER NAME*(*)
C--------------------------------------------------------------
C
C     INCLUDE COMMON BLOCKS.
C
      INCLUDE "CTLBLK.comm"
      INCLUDE "LOOPS.comm"
      INCLUDE "MASKS.comm"
      INCLUDE "DYNAM.comm"
      INCLUDE "PHYS2.comm"
      INCLUDE "MAPOT1.comm"
      INCLUDE "VRBLS.comm"
      INCLUDE "CONTIN.comm"
      INCLUDE "PVRBLS.comm"
      INCLUDE "BOCO.comm"
      INCLUDE "ACMCLH.comm"
      INCLUDE "ACMCLD.comm"
      INCLUDE "ACMPRE.comm"
      INCLUDE "ACMRDL.comm"
      INCLUDE "ACMRDS.comm"
      INCLUDE "ACMSFC.comm"
      INCLUDE "CLDWTR.comm"
      INCLUDE "CNVCLD.comm"
      INCLUDE "SOIL.comm"
      INCLUDE "INDX.comm"
      INCLUDE "Z0EFFT.comm"
      INCLUDE "TEMPV.comm"
C--------------------------------------------------------------
87645 FORMAT(' EXIT ',A)
      IEND=MYIE
      IF(MOD(MYPE+1,INPES).EQ.0)IEND=MYIE-1
      DO 87654 L=1,LM-1
      DO 87653 J=MYJS,MYJE
      DO 87653 I=MYIS,IEND
      IF(HTM(I,J,L).GT.0.5)THEN
        IF(T(I,J,L).LT.150..OR.T(I,J,L).GT.355.0.OR.
     &     T(I,J,L).NE.T(I,J,L))THEN
          WRITE(6,87645)NAME
          WRITE(6,87650)I,J,L,T(I,J,L),MYPE
87650     FORMAT(' I=',I3,' J=',I3,' L=',I2,' T=',E12.5
     1,          ' MYPE=',I3)
          STOP666
       ELSEIF(Q(I,J,L).LT.-1.E-4.OR.Q(I,J,L).GT.30.E-3.OR.
     &     Q(I,J,L).NE.Q(I,J,L))THEN
         WRITE(6,87645)NAME
         WRITE(6,87651)I,J,L,Q(I,J,L),MYPE
87651     FORMAT(' I=',I3,' J=',I3,' L=',I2,' Q=',E12.5
     1,          ' MYPE=',I3)
         STOP666
       ELSEIF(Q2(I,J,L).GT.250..OR.Q2(I,J,L).NE.Q2(I,J,L))THEN
         WRITE(6,87645)NAME
         WRITE(6,87751)I,J,L,Q2(I,J,L),MYPE
87751     FORMAT(' I=',I3,' J=',I3,' L=',I2,' Q2=',E12.5
     1,          ' MYPE=',I3)
         STOP666
        ELSEIF(CWM(I,J,L).GT.2.E-2.OR.CWM(I,J,L).NE.CWM(I,J,L))THEN
c       elseif(cwm(i,j,l).gt.0.1) then
          WRITE(6,87645)NAME
          WRITE(6,87752)I,J,L,CWM(I,J,L),MYPE
87752     FORMAT(' I=',I3,' J=',I3,' L=',I2,' CWM=',E12.5
     1,          ' MYPE=',I3)
          STOP666
        ENDIF
      ENDIF
87653 CONTINUE
87654 CONTINUE
      DO 87664 L=1,LM
      DO 87663 J=MYJS,MYJE
      DO 87663 I=MYIS,IEND
      IF(ABS(U(I,J,L)).GT.125..OR.ABS(V(I,J,L)).GT.125.)THEN
        WRITE(6,87645)NAME
        WRITE(6,87662)I,J,L,U(I,J,L),V(I,J,L),MYPE
87662   FORMAT(' I=',I3,' J=',I3,' L=',I2,' U=',E12.5,' V=',E12.5
     1,          ' MYPE=',I3)
        STOP666
      ENDIF
87663 CONTINUE
87664 CONTINUE
C
      RETURN
      END
