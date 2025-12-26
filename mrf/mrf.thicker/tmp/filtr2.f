      SUBROUTINE FILTR2(TEM,TE,Y,DIM,DI,X,ZEM,ZE,W,RM,RQ,RT,FILTA)
      DIMENSION
     1  TE( 4033 , 28 ), DI( 4033 , 28 ), ZE( 4033 , 28 ),
     1 TEM( 4033 , 28 ),DIM( 4033 , 28 ),ZEM( 4033 , 28 ),
     1   Y( 4033 , 28 ),  X( 4033 , 28 ),  W( 4033 , 28 ),
     1  RQ( 4033 , 28 ), RT( 4033 , 28 ), RM( 4033 , 28 )
      FILTB = (1.-FILTA)* 0.5
      DO 4900 K=1, 28
      DO 4900 J=1, 4032
      TE(J,K)=Y(J,K)
      DI(J,K)=X(J,K)
      ZE(J,K)=W(J,K)
      TEM(J,K)=TEM(J,K)+FILTB*TE(J,K)
      DIM(J,K)=DIM(J,K)+FILTB*DI(J,K)
      ZEM(J,K)=ZEM(J,K)+FILTB*ZE(J,K)
4900  CONTINUE
      DO 4901 K=1, 28
      DO 4901 J=1, 4032
      RQ(J,K)=RT(J,K)
      RM(J,K)=RM(J,K)+FILTB*RQ(J,K)
4901  CONTINUE
      RETURN
      END
