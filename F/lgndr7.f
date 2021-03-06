C     Mark Iredell routien from 7 April 1994
      SUBROUTINE LGNDR7(COA,MFP,ALP,DALP,IROMB)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION ALP(1),DALP(1)
C
C
      SIA=SQRT(1.D0-COA*COA)
      LM=2
      LMD=1
      ALP(1)=SQRT(0.5D0)
      F1M=SQRT(1.5D0)
      ALP(2)=F1M*COA
      DALP(1)=0.D0
      DO 1 M1=1,MFP
      M=M1-1
      AM=M
      A2M=M+M
      RE1=SQRT(A2M+3.D0)
      E1=1.D0/RE1
      IF(M.EQ.0) GO TO 3
      F2M=F1M*SIA/SQRT(A2M)
      F1M=F2M*RE1
      LM=LM+1
      ALP(LM)=F2M
      IF(IROMB.EQ.1) GO TO 2
      IF(M1.NE.MFP) GO TO 2
      LMD=LMD+1
      DALP(LMD)=AM*E1*F1M*COA
      GO TO 1
    2 LM=LM+1
      ALP(LM)=F1M*COA
      LMD=LMD+1
      DALP(LMD)=AM*E1*ALP(LM)
    3 M2=M+2
      MFPX=MFP
      IF(IROMB.EQ.1) MFPX=MFP+M
      DO 4 N=M2,MFPX
      AN=N
      AN2=N*N
      E2=SQRT((4.D0*AN2-1.D0)/(AN2-AM*AM))
      LM=LM+1
      ALP(LM)=E2*(COA*ALP(LM-1)-E1*ALP(LM-2))
      E2=1.D0/E2
      LMD=LMD+1
      DALP(LMD)=(AN-1.D0)*E2*ALP(LM)-AN*E1*ALP(LM-2)
      E1=E2
    4 CONTINUE
      LM=LM-1
    1 CONTINUE
C
      RETURN
      END
