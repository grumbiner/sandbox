      SUBROUTINE DAMPUX(DIV,VOR,TEM,RMX,DELTIM,FDAMP,ARN,SPDMAX)
      DIMENSION FDAMP( 4032 ),ARN( 4032 ),SPDMAX( 28 ),
     1 RMX( 4033 , 28 ),
     3 VOR( 4033 , 28 ), DIV( 4033 , 28 ),TEM( 4033 , 28 )
      COMMON/COMBIT/NDEX( 4032 ),SNNP1( 4032 )
      COMMON/COMBIT/LAB(4),IFIN,ICEN,IGEN,ICEN2,IENST,IENSI,RUNID,USRID
      CHARACTER*8 LAB
C.................................................................
      DO 410 J=1, 4032
      ARN(J)=SNNP1(J)+0.25 E 0
      ARN(J)= SQRT(ARN(J))
      ARN(J)=ARN(J)-0.5 E 0
  410 CONTINUE
      ALFA=2.5 E 0
      BETA= 6.3712E+6 *1.009 E 0/DELTIM
      ALFADT=ALFA*DELTIM/ 6.3712E+6
C.................................................
      DO 80 K=1, 28
      RNCRIT=BETA/SPDMAX(K)
      COEF=ALFADT*SPDMAX(K)
      DO 420 J=1, 4032
      IF (ARN(J).GT.RNCRIT) THEN
      DIV(J,K) =DIV(J,K)/(1.+(ARN(J)-RNCRIT)*COEF)
      VOR(J,K) =VOR(J,K)/(1.+(ARN(J)-RNCRIT)*COEF)
      TEM(J,K) =TEM(J,K)/(1.+(ARN(J)-RNCRIT)*COEF)
      END IF
  420 CONTINUE
      DO 422 IT=1, 1
      IS=(IT-1)* 28
      L=IS+K
      DO 421 J=1, 4032
      IF (ARN(J).GT.RNCRIT) THEN
      RMX(J,L) =RMX(J,L)/(1.+(ARN(J)-RNCRIT)*COEF)
      END IF
  421 CONTINUE
  422 CONTINUE
   80 CONTINUE
      RETURN
      END
