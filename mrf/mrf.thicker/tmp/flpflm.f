CFPP$ NOCONCUR R
      SUBROUTINE FLPFLM(FLP,FLM,ANL)
C.................................................................
C  FOR LOOPA
C
      PARAMETER(LOTA=3* 28 +1* 28 +1)
C
       DIMENSION
     X FLP(2, 63 ,LOTA),FLM(2, 63 ,LOTA),
     X ANL( 386 ,LOTA)
C
C.................................................................
C
C     DO K=1,4* 28 +1
      DO K=1,LOTA
      DO  LL=1, 63
C DO N.HEMI
       FLP(1,LL,K)=ANL(2*(LL-1)+1,K)+
     1             ANL(2*(LL-1)+1+ 192 ,K)
       FLP(2,LL,K)=ANL(2*(LL-1)+2,K)+
     1             ANL(2*(LL-1)+2+ 192 ,K)
C
C DO S.HEMI
C
       FLM(1,LL,K)=ANL(2*(LL-1)+1,K)-
     1             ANL(2*(LL-1)+1+ 192 ,K)
       FLM(2,LL,K)=ANL(2*(LL-1)+2,K)-
     1             ANL(2*(LL-1)+2+ 192 ,K)
      ENDDO
      ENDDO
C.................................................................
      RETURN
      END
