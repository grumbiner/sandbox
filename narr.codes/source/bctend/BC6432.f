                             SUBROUTINE BC6432(INFIL)
C     ******************************************************************
C     *                                                                *
C     *  CONVERTS THE 64-BIT BOUNDARY VALUES FROM PTETABC TO 32-BIT    *
C     *  WORDS AND DOES THE REFORMATING OF DATA                        *
C     *                                                                *
C     ******************************************************************
         include "parmeta.res"
                             P A R A M E T E R
     & (IMT=IM*2-1,JMT=JM/2+1)
                             P A R A M E T E R
     & (LB=2*JMT+IMT-3
     &, LP1=LM+1,LM1=LM-1
     &, IMJM=IM*JM-JM/2)
C-----------------------------------------------------------------------
                             R E A L
     & PDB2  (LB,2)
     &,TB2   (LB,LM,2),QB2   (LB,LM,2),UB2   (LB,LM,2),VB2   (LB,LM,2)
C-----------------------------------------------------------------------
                             D I M E N S I O N
     & PDBA  (LB)
     &,TBA   (LB,LM),QBA   (LB,LM),UBA   (LB,LM),VBA   (LB,LM)
C-----------------------------------------------------------------------
                             C O M M O N /BCHFLDS/
     & PDBA  ,TBA   ,QBA   ,UBA   ,VBA
C-----------------------------------------------------------------------
      READ (INFIL,end=99131) PDB2,TB2,QB2,UB2,VB2
C-----------------------------------------------------------------------
      DO N=1,LB
         PDBA(N)=PDB2(N,1)
      ENDDO
C-----------------------------------------------------------------------
      DO L=1,LM
        DO N=1,LB
         TBA(N,L)=TB2(N,L,1)
         QBA(N,L)=QB2(N,L,1)
         UBA(N,L)=UB2(N,L,1)
         VBA(N,L)=VB2(N,L,1)
        ENDDO
      ENDDO
C-----------------------------------------------------------------------
99131 continue
      RETURN
      END
