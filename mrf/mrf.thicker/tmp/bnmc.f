      SUBROUTINE BNMC(KM,SI,SL,CD,CI,CQ,CQL)
      PARAMETER(RD= 2.8705E+2 ,CP= 1.0046E+3 ,ROCP=RD/CP)
      DIMENSION SI(KM+1),SL(KM)
      DIMENSION CD(KM,KM+1),CI(KM+1,KM),CQ(KM+1,KM),CQL(KM,KM)
      DIMENSION DEL(KM),SLK(KM),ALFA(2:KM),BETA(KM-1)
 
      DO K=1,KM
        DEL(K)=SI(K+1)-SI(K)
        SLK(K)=SL(K)**ROCP
      ENDDO
      DO K=2,KM
        ALFA(K)=0.5*(1.-SLK(K-1)/SLK(K))/ROCP
      ENDDO
      DO K=1,KM-1
        BETA(K)=0.5*(SLK(K+1)/SLK(K)-1.)/ROCP
      ENDDO
 
      DO KD=1,KM
        DO KI=1,KM+1
          CD(KD,KI)=0
        ENDDO
      ENDDO
      DO KD=1,KM
        CD(KD,KD)=-1/DEL(KD)
        CD(KD,KD+1)=1/DEL(KD)
      ENDDO
 
      DO KI=1,KM+1
        DO KD=1,KM
          CI(KI,KD)=0
        ENDDO
      ENDDO
      DO KI=2,KM
        CI(KI,KI-1)=0.5
        CI(KI,KI)=0.5
      ENDDO
 
      DO KI=1,KM+1
        DO KD=KI,KM
          CQ(KI,KD)=0
        ENDDO
        DO KD=1,KI-1
          CQ(KI,KD)=DEL(KD)
        ENDDO
      ENDDO
 
      CQL(1,1)=DEL(1)-BETA(1)*SI(2)
      DO KT=2,KM-1
        CQL(1,KT)=DEL(KT)-ALFA(KT)*SI(KT)-BETA(KT)*SI(KT+1)
      ENDDO
      CQL(1,KM)=DEL(KM)-ALFA(KM)*SI(KM)
      DO KZ=2,KM
        CQL(KZ,1)=CQL(1,1)+BETA(1)
        DO KT=2,KZ-1
          CQL(KZ,KT)=CQL(1,KT)+ALFA(KT)+BETA(KT)
        ENDDO
        CQL(KZ,KZ)=CQL(1,KZ)+ALFA(KZ)
        DO KT=KZ+1,KM
          CQL(KZ,KT)=CQL(1,KT)
        ENDDO
      ENDDO
 
      RETURN
      END
