      SUBROUTINE WRTCOM
C---------------------------------------------------------------
      INCLUDE "parmeta"
      PARAMETER (IMJM=IM*JM-JM/2,LP1=LM+1)
      PARAMETER (LB=322,JAM=268,ITB=76,JTB=134,ITBQ=152,JTBQ=440)
      PARAMETER (NSOIL=2)
      PARAMETER (NWBC=98532,NWCW=529058,NWCV=51608,NWDY=9+129786
     1,          NWPH=21+333225
     2,          NWPV=735414,NWSO=219334,NWSR=12902,NWVR=1999810)
C
      INCLUDE "BOCO.comm"
      INCLUDE "CLDWTR.chk"
      INCLUDE "CNVCLDK.comm"
      INCLUDE "DYNAM.chk"
      INCLUDE "PHYS2.chk"
      INCLUDE "PVRBLSK.comm"
      INCLUDE "SOILK.comm"
      INCLUDE "VRBLSK.comm"
C
      REAL DYA(NWDY),PH(NWPH-1),PDBA(NWBC),CWMA(NWCW)
      EQUIVALENCE (DYC,DYA(1)),(PT2C,PH(1)),(PDB(1,1),PDBA(1))
     1,           (CWMC(1,1),CWMA(1))
C---------------------------------------------------------------
      WRITE(50)(PDBA(N),N=1,NWBC)
C
      WRITE(51)(CWMA(N),N=1,NWCW)
C
      WRITE(52)(CUPPTX(N),N=1,NWCV)
C
      WRITE(53)(DYA(N),N=1,NWDY)
C
      WRITE(54)KTMC,(PH(N),N=1,NWPH-1)
C
      WRITE(55)(Z0X(N),N=1,NWPV)
C
      WRITE(56)(SOILT1X(N),N=1,NWSO)
C
      WRITE(57)(USTARX(N),N=1,NWSR)
C
      WRITE(58)(PDX(N),N=1,NWVR)
C
      RETURN 
      END
