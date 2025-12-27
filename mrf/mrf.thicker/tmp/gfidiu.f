CFPP$ NOCONCUR R
      SUBROUTINE GFIDIU(DG,TG,ZG,UG,VG,RQG,DPHI,DLAM,
     1 RCL,DEL,RDEL2,CI,P1,P2,H1,H2,TOV,SPDMAX,
     2 DTDF,DTDL,DRDF,DRDL,DUDL,DVDL,DUDF,DVDF,
     2 DQDT,DTDT,DRDT,DUDT,DVDT)
C
C INPUT VARIABLES
C
      DIMENSION
     1  DG( 386 , 28 ),TG( 386 , 28 ), ZG( 386 , 28 ),
     2  UG( 386 , 28 ),VG( 386 , 28 ),RQG( 386 , 28 , 1 ),
     3          DPHI( 386 ),DLAM( 386 )
C
      DIMENSION
     1  DTDF( 386 , 28 ),DTDL( 386 , 28 ),
     1  DRDF( 386 , 28 , 1 ),DRDL( 386 , 28 , 1 ),
     1  DUDL( 386 , 28 ),DVDL( 386 , 28 ),
     1  DUDF( 386 , 28 ),DVDF( 386 , 28 )
C OUTPUT VARIABLES
C
      DIMENSION SPDMAX( 28 ),
     1  DUDT( 386 , 28 ),DVDT( 386 , 28 ),
     1  DTDT( 386 , 28 ),DRDT( 386 , 28 , 1 ),
     1  DQDT( 386 )
C
C CONSTANT ARRAYS
C
       DIMENSION
     1 DEL( 28 ),RDEL2( 28 ),
     2 CI( 29 ),TOV( 28 ),
     3 P1( 28 ),P2( 28 ),H1( 28 ),H2( 28 )
C
C LOCAL VARIABLES
C
      DIMENSION
     1     CG ( 386 , 28 ), DB( 386 , 28 ),CB( 386 , 28 ),
     2     DOT( 386 , 29 ),DUP( 386 , 28 ),DVP( 386 , 28 ),
     3     DUM( 386 , 28  ),DVM( 386 , 28 ), EK( 386 , 28 ),
     4     RMU( 28  ),RNU( 28 ),RHO( 28 ),SI( 29 ),
     5      X1( 28  ), X2( 28 ), X3( 28 ),X4( 28 )
C
      RK=  2.8705E+2  / 1.0046E+3
      SINRA=SQRT(1.-1./RCL)
      FNOR=2.* 7.2921E-5 *SINRA
      FSOU=-FNOR
      SINRA=SINRA/ 6.3712E+6
C
      SI(1)=1.0
      DO 4 K=1, 28
      SI(K+1)=SI(K)-DEL(K)
4     CONTINUE
C
      DO 1 K=1, 27
      RHO(K)=ALOG(SI(K)/SI(K+1))
1     CONTINUE
      RHO( 28 )=0.
C
      DO 2 K=1, 28
      RMU(K)=1.-SI(K+1)*RHO(K)/DEL(K)
2     CONTINUE
C
      DO 3 K=1, 27
      RNU(K+1)=-1.+SI(K)*RHO(K)/DEL(K)
3     CONTINUE
      RNU(1)=0.
C
      DO 20 K=1, 28
      X1(K)=RMU(K)*(1.-RK*RNU(K))/(RMU(K)+RNU(K))
      X2(K)=1.-X1(K)
      X3(K)=(1.+RK*RMU(K))/(1.-RK*RNU(K))
      X4(K)=1./X3(K)
20    CONTINUE
C
      DO 1234 K=1, 28
      SPDMAX(K)=0.
1234  CONTINUE
      RCL2=.5 E 0*RCL
C
      DO 140 K=1, 28
      DO 140 J=1, 384
      EK(J,K)=(UG(J,K)*UG(J,K)+VG(J,K)*VG(J,K))*RCL
  140 CONTINUE
C
      DO 10 K=1, 28
      DO 10 J=1, 384
      IF (EK(J,K) .GT. SPDMAX(K))  SPDMAX(K)=EK(J,K)
   10 CONTINUE
C
C     COMPUTE C=V(TRUE)*DEL(LN(PS)).DIVIDE BY COS FOR DEL, COS FOR V
C
      DO 150 J=1, 384
      DPHI(J)=DPHI(J)*RCL
      DLAM(J)=DLAM(J)*RCL
  150 CONTINUE
      DO 180 K=1, 28
      DO 180 J=1, 384
      CG(J,K)=UG(J,K)*DLAM(J)+VG(J,K)*DPHI(J)
  180 CONTINUE
C
      DO 190 J=1, 384
      DB(J,1)=DEL(1)*DG(J,1)
      CB(J,1)=DEL(1)*CG(J,1)
  190 CONTINUE
      DO 210 K=1, 27
      DO 210 J=1, 384
      DB(J,K+1)=DB(J,K)+DEL(K+1)*DG(J,K+1)
      CB(J,K+1)=CB(J,K)+DEL(K+1)*CG(J,K+1)
  210 CONTINUE
C
C   STORE INTEGRAL OF CG IN DLAX
C
      DO 220 J=1, 384
      DQDT(J)= -CB(J, 28 )
  220 CONTINUE
C
C   SIGMA DOT COMPUTED ONLY AT INTERIOR INTERFACES.
C
      DO 230 J=1, 384
      DOT(J,1)=0. E 0
      DVM(J,1)=0. E 0
      DUM(J,1)=0. E 0
      DOT(J, 29 )=0. E 0
      DVP(J, 28  )=0. E 0
      DUP(J, 28  )=0. E 0
  230 CONTINUE
C
      DO 240 K=1, 27
      DO 240 J=1, 384
      DOT(J,K+1)=DOT(J,K)+
     1                 DEL(K)*(DB(J, 28 )+CB(J, 28 )-
     2                 DG(J,K)-CG(J,K))
  240 CONTINUE
C
C
C
      DO 260 K=1, 27
      DO 260 J=1, 384
      DVP(J,K  )=VG(J,K+1)-VG(J,K)
      DUP(J,K  )=UG(J,K+1)-UG(J,K)
      DVM(J,K+1)=VG(J,K+1)-VG(J,K)
      DUM(J,K+1)=UG(J,K+1)-UG(J,K)
  260 CONTINUE
      DO J=1, 384
       DPHI(J)=DPHI(J)/RCL
       DLAM(J)=DLAM(J)/RCL
      ENDDO
      DO K=1, 28
       DO J=1, 384
        DUDT(J,K)=-UG(J,K)*DUDL(J,K)-VG(J,K)*DUDF(J,K)
     1 -RDEL2(K)*(DOT(J,K+1)*DUP(J,K)+DOT(J,K)*DUM(J,K))
     2 - 2.8705E+2 *TG(J,K)*DLAM(J)
C
        DVDT(J,K)=-UG(J,K)*DVDL(J,K)-VG(J,K)*DVDF(J,K)
     1 -RDEL2(K)*(DOT(J,K+1)*DVP(J,K)+DOT(J,K)*DVM(J,K))
     2 - 2.8705E+2 *TG(J,K)*DPHI(J)
       ENDDO
      ENDDO
C
      DO K=1, 28
       DO J=1, 192
        DUDT(J,K)=DUDT(J,K)+VG(J,K)*FNOR
        DUDT(J+ 192 ,K)=DUDT(J+ 192 ,K)+VG(J+ 192 ,K)*FSOU
 
        DVDT(J,K)=DVDT(J,K)-UG(J,K)*FNOR
     1 -SINRA*EK(J,K)
        DVDT(J+ 192 ,K)=DVDT(J+ 192 ,K)-UG(J+ 192 ,K)*FSOU
     1 +SINRA*EK(J+ 192 ,K)
       ENDDO
      ENDDO
      DO K=1, 28
       DO J=1, 384
        DUDT(J,K)=DUDT(J,K)*RCL
        DVDT(J,K)=DVDT(J,K)*RCL
       ENDDO
      ENDDO
C
C
      DO 280 K=1, 27
      DO 280 J=1, 384
CECMWF:
      DUP(J,K  )=TG(J,K+1)+TOV(K+1)-TG(J,K)-TOV(K)+2.*RK*RNU(K+1)*
     .(TG(J,K)+TOV(K))
CECMWF:
      DUM(J,K+1)=TG(J,K+1)+TOV(K+1)-TG(J,K)-TOV(K)+2.*RK*RMU(K+1)*
     .(TG(J,K+1)+TOV(K+1))
  280 CONTINUE
C
C
      DO K=1, 28
       DO J=1, 384
C       DTDT(J,K)=
        DTDT(J,K)=-UG(J,K)*DTDL(J,K)-VG(J,K)*DTDF(J,K)
     1 -RDEL2(K)*(DOT(J,K+1)*DUP(J,K)+DOT(J,K)*DUM(J,K))
       ENDDO
      ENDDO
C
      DO K=1, 28
       DO J=1, 384
        DTDT(J,K)=DTDT(J,K)
     1  +RK*(TOV(K)+TG(J,K))*(CG(J,K)-CB(J, 28 )-DB(J, 28 ))
       ENDDO
      ENDDO
C
      DO 330 N=1, 1
      DO 300 K=1, 27
      DO 300 J=1, 384
      DUP(J,K  )=RQG(J,K+1,N)-RQG(J,K,N)
      DUM(J,K+1)=RQG(J,K+1,N)-RQG(J,K,N)
  300 CONTINUE
      DO 310 J=1, 384
      DUP(J, 28 )=0. E 0
  310 CONTINUE
      DO 320 K=1, 28
      DO 320 J=1, 384
      DRDT(J,K,N)=-UG(J,K)*DRDL(J,K,N)-VG(J,K)*DRDF(J,K,N)
     1 -RDEL2(K)*(DOT(J,K+1)*DUP(J,K)+DOT(J,K)*DUM(J,K))
  320 CONTINUE
  330 CONTINUE
C
      RETURN
      END
