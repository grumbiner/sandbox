C
      SUBROUTINE SFTRIG(TRIGS,N,MODE)
C     Mark Iredell 7 April 1994
C
C
C*    IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION TRIGS(1)
C
C*    PI=2.D0*ASIN(1.D0)
      PI=2.  *ASIN(1.  )
      IMODE=IABS(MODE)
      NN=N
      IF(IMODE.GT.1.AND.IMODE.LT.6) NN=N/2
      DEL=(PI+PI)/DFLOAT(NN)
      L=NN+NN
      DO 10 I=1,L,2
C*    ANGLE=0.5D0*DFLOAT(I-1)*DEL
      ANGLE=0.5  *DFLOAT(I-1)*DEL
      TRIGS(I)=COS(ANGLE)
      TRIGS(I+1)=SIN(ANGLE)
   10 CONTINUE
      IF(IMODE.EQ.1) RETURN
      IF(IMODE.EQ.8) RETURN
C*    DEL=0.5D0*DEL
      DEL=0.5  *DEL
      NH=(NN+1)/2
      L=NH+NH
      LA=NN+NN
      DO 20 I=1,L,2
C*    ANGLE=0.5D0*DFLOAT(I-1)*DEL
      ANGLE=0.5  *DFLOAT(I-1)*DEL
      TRIGS(LA+I)=COS(ANGLE)
      TRIGS(LA+I+1)=SIN(ANGLE)
   20 CONTINUE
      IF(IMODE.LE.3) RETURN
C*    DEL=0.5D0*DEL
      DEL=0.5  *DEL
      LA=LA+NN
      IF(MODE.EQ.5) GO TO 40
      DO 30 I=2,NN
      ANGLE=DFLOAT(I-1)*DEL
C*    TRIGS(LA+I)=2.D0*SIN(ANGLE)
      TRIGS(LA+I)=2.  *SIN(ANGLE)
   30 CONTINUE
      RETURN
   40 CONTINUE
C*    DEL=0.5D0*DEL
      DEL=0.5  *DEL
      DO 50 I=2,N
      ANGLE=DFLOAT(I-1)*DEL
      TRIGS(LA+I)=SIN(ANGLE)
   50 CONTINUE
      RETURN
C
      END
