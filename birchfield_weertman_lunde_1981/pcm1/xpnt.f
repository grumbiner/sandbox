      SUBROUTINE XPNT 
      COMMON/LARGE/NFLD,JFLD(12,2)
      COMMON/MINOR/NMINOR,KFLD(26)
      L=0 
      DO 10 J=1,2 
      DO 10 I=1,NFLD
      L=L+1 
  10  JFLD(I,J)=L 
      DO 20 I=1,NMINOR
      L=L+1 
  20  KFLD(I)=L 
      RETURN
      END 
