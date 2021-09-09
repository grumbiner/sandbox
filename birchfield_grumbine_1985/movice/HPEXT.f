      SUBROUTINE HPEXT
C 
      INTEGER MXSB
      PARAMETER (MXSB  = 160) 
  
C     COMMON BLOCK ISCOM
      COMMON/ISCOM/HGT(0:MXSB,0:1), H, HP, HTOPO(0:MXSB)
  
      COMMON /ISETC/ TIMELP, TIMSTP, GRDSTP,
     2    TRIPS, RNETAC, POLFLX, MSNAUT, MAPRIM, OUTDAT,
     3    DELTAX, DELTAT, DHPDT(0:MXSB), HPCALL, ISSB, SNLXIN 
  
      REAL HGT, TIMELP
      REAL TIMSTP, GRDSTP, SNLXIN 
      REAL RNETAC, POLFLX, MSNAUT, MAPRIM 
      REAL DELTAX, DELTAT, DHPDT
      LOGICAL OUTDAT
      INTEGER H, HP, TRIPS, HPCALL, ISSB
  
C     ARRAYS FOR THE TRANSFORM ROUTINE
C     HK CONTAINS THE FOURIER TRANSFORM COMPONENTS OF THE ICE 
C         SHEET THICKNESS.
C     HPK (H'K) HOLDS THE FOURIER TRANSFORM COMPONENTS OF THE 
C         BEDROCK DISPLACEMENT
C     TR  IS USED AS TEMPORARY STORAGE TO HOLD THE THICKNESS FOR
C         THE TRANSFORM ROUTINES
C     HPE IS ?
C     HKOLD IS THE VALUE OF HK ON THE PREVIOUS CALL.  THIS IS USED
C       IN EVALUATING THE ELASTIC CONTRIBUTION TO THE RESPONSE. 
      COMMON /FTCOM/HK(-MXSB-2:MXSB+1), HPK(-MXSB-2:MXSB+1) 
      REAL TR(-MXSB-2:MXSB+1), HPE(-MXSB-2:MXSB+1)
      REAL HK, HPK
      REAL HKOLD(-MXSB-2:MXSB+1)
  
C     VARIABLES FOR THE EXTRAPOLATION 
C     PHYSICAL TERMS
      REAL D, MU, ETA, RHOI, G,PI, K,L
      REAL RHOA, RHOL, EPSA, EPSL, TAU
C     COMPUTED CONSTANTS
      REAL CHSQV, CHVSHV, V, VSQ, DET 
      REAL MUETA, RHOG
      REAL RHOR 
  
C     A   HOLDS THE SPECTRAL COMPONENTS OF THE HORIZONTAL DIS-
C            PLACEMENT OF THE ASTHENOSPHERE.
C     B   IS RELATED TO THE SPECTRAL COMPONENTS OF THE VERTICAL DIS-
C            PLACEMENT OF THE BEDROCK.
      REAL A(-MXSB-2:0,2),B(-MXSB-2:0,2)
      REAL A11(-MXSB-1:0),A12(-MXSB-1:0),A13(-MXSB-1:0) 
      REAL A14(-MXSB-1:0) 
      REAL B21(-MXSB-1:0),B22(-MXSB-1:0),B23(-MXSB-1:0) 
      REAL B24(-MXSB-1:0) 
C     I   IS A GENERIC LOOP CONTROL VARIABLE
C     ISB IS THE FIRST POINT AS YOU MOVE S-N WHERE THE BEDROCK IS 
C           DISPLACED 
      INTEGER I, ISB
C     ELAST TELLS WHETHER YOU WANT THE CONTRIBUTION FROM THE ELASTIC
C       RESPONSE. 
      LOGICAL ELAST, ONELAY 
  
  
C     RECOMPUTE THE B TERMS SO THEY REPRESENT THE CURRENT SITUATION 
C       IN THE BEDROCK.  MUST DO THIS BECAUSE HPEXT IS NOT CALLED 
C       EVERY TIME STEP, AND THE B VALUES ARE ONLY EXTRAPOLATED 
C       ONE STEP INTO THE FUTURE. 
      TR(-MXSB-2) = 0.0 
      TR(0) = 0.0 
      DO 520 I = 1,MXSB+1 
        TR(I)  = HGT(I-1, HP) -HTOPO(I-1) 
        TR(-I) = -TR(I) 
 520  CONTINUE
      CALL FTSC (TR, HPK,1.E-8) 
      DO 530 I = -MXSB-1,0
        B(I,1) = -4.*PI*MU*(I+MXSB+2)*HPK(I)/L
 530  CONTINUE
      B(-MXSB-2,1) = 0.0
C 
C     ENTER THICKNESS INTO THE MATRIX TO BE TRANSFORMED.
      TR(-MXSB-2) = 0.0 
      TR(0) = 0.0 
      DO 100 I = 1,MXSB+1 
        TR(I)  = HGT(I-1, H)+HGT(I-1, HP) 
        TR(-I) = -TR(I) 
 100  CONTINUE
  
C     FIND THE TRANSFORM OF THICKNESS.
      CALL FTSC(TR, HK, 1.E-8)
  
C     CALCULATE DEL A, DEL B, NEW A, NEW B, H'K 
      B(-MXSB-2,1)=0.0
      DO 200 I=-MXSB-1,0
        A(I,2)=A11(I)*A(I,1)+A12(I)*B(I,1)-A13(I)*HK(I) 
              IF( ELAST ) A(I,2)=A(I,2)-A14(I)*(HK(I)-HKOLD(I)) 
        B(I,2)=B21(I)*A(I,1)+B22(I)*B(I,1)-B23(I)*HK(I) 
              IF( ELAST ) B(I,2)= B(I,2)-B24(I)*(HK(I)-HKOLD(I))
        A(I,1)=A(I,1)+A(I,2)*DELTAT*HPCALL
        B(I,1)=B(I,1)+B(I,2)*DELTAT*HPCALL
        HPK(I)=-B(I,1)*L/(4*PI*MU*(I+MXSB+2)) 
 200  CONTINUE
  
C     FILL OUT THE REST OF THE HPK MATRIX.
      HPK(-MXSB-2)=0.0
      DO 300 I=1,MXSB+1 
        HPK(I) = -HPK(-I) 
 300  CONTINUE
  
C     CALCULATE THE INVERSE TRANSFORM OF H'K
      CALL IFTSC(HPE, HPK,1.E-8)
  
C     PUT HK INTO HKOLD TO PREPARE FOR THE NEXT CALL
C     PUT HP INTO HGT(I, HP) FOR ITERATION LOOP 
      HKOLD(-MXSB-2)=0.0
      HKOLD(0)      =HK(0)
      DO 400 I=1,MXSB+1 
        HKOLD(I)   = HK(I)
        HKOLD(-I)  = HK(-I) 
        DHPDT(I-1) = (HPE(I)-HGT(I-1, HP))/HPCALL 
        HGT(I-1, HP)= HGT(I-1, HP)+DHPDT(I-1) 
 400  CONTINUE
C 
C     DO EXTRAPOLATION FOR THE REGION AHEAD OF THE ICE SHEET. 
C     DO EXTRAPOLATION ONLY IF SPECIFIED BY ONELAY
      IF (.NOT. ONELAY) RETURN
      ISB=MXSB+1
 500  CONTINUE
      IF (TR(ISB).LE.0.0.AND.ISB.GT.1) THEN 
        ISB=ISB-1 
C       SUBTRACT THE CHANGE THAT WAS PREDICTED BY THE TWO LAYER METHOD
C       IN THE REGION IN FRONT OF THE ICE SHEET.  USE THE ONE LAYER 
C       VISCOUS METHOD INSTEAD. 
        HGT(ISB, HP)=HGT(ISB, HP)-DHPDT(ISB)
        DHPDT(ISB)=TIMSTP*(RHOI*HGT(ISB, H)-(RHOR-RHOI)*HGT(ISB, HP))/
     1    (RHOR*TAU)
        HGT(ISB, HP) = HGT(ISB, HP) +DHPDT(ISB) 
        GO TO 500 
      ENDIF 
  
      RETURN
  
      ENTRY HPEXIN
C     SET UP THE CONSTANT MATRICES
      READ (*,9003) D 
      D     = 1000.*D 
      READ (*,9003) MU
      READ (*,9003) ETA 
      RHOI   = 917.0
      READ (*,9003) RHOA
      READ (*,9003) RHOL
 9003 FORMAT (E20.12) 
      RHOR    = .5*(RHOA+RHOL)
      READ (*,9003) TAU 
      G     = 9.80
      PI    = 3.1415926535898 
      L     = (2*MXSB+4)*DELTAX 
      MUETA = MU/ETA
      RHOG  = RHOI*G
      READ (*,9005) ELAST 
      READ (*,9005) ONELAY
  
      DO 1000 I=-MXSB-1,0 
  
        K      = 2.*PI*(I+MXSB+2)/L 
        V      = K*D
        EPSA   = RHOA*G/(2*MU*K)
        EPSL   = RHOL*G/(2*MU*K)
        VSQ    = V*V
        CHVSHV = .5*SINH(2.*V)
        CHSQV  = COSH(V)**2 
C 
        DET    = 1./(VSQ+CHSQV) 
        A11(I) = -DET*MUETA*(CHVSHV+V+(EPSA-EPSL)*VSQ*CHSQV)
        A12(I) = DET*MUETA*(VSQ-(EPSA-EPSL)*(V*CHSQV-VSQ*CHVSHV)) 
        A13(I) = DET*MUETA*RHOG*(EPSA-EPSL)*(V*CHVSHV-VSQ*CHSQV)
        A14(I) = -DET*RHOG*VSQ/DELTAT 
        B21(I) = DET*MUETA*(VSQ-(EPSA-EPSL)*(V*CHSQV+VSQ*CHVSHV)) 
        B22(I) = -DET*MUETA*(CHVSHV-V+EPSA*(CHSQV-VSQ*(CHSQV-1.)) 
     1            +EPSL*VSQ*CHSQV)
        B23(I) = MUETA*RHOG*(1.+DET*(EPSA-EPSL)*(CHVSHV*(1.-VSQ)-V))
        B24(I) = DET*RHOG*(CHVSHV-V)/DELTAT 
  
        A(I,1)= 0.0 
        B(I,1)= 0.0 
  
 1000 CONTINUE
  
      DO 1100 I=-MXSB-2,MXSB+1
        HKOLD(I)=0.0
 1100 CONTINUE
      DO 1101 I=0,MXSB
        DHPDT(I)=0.0
 1101 CONTINUE
      HK(-MXSB-2)   = 0.0 
      A(-MXSB-2,1)  = 0.0 
      B(-MXSB-2,1)  = 0.0 
      HPK(-MXSB-2)  = 0.0 
      HPE(-MXSB-2)  =  0.0
  
 9005 FORMAT (L14)
  
      RETURN
  
      END 
