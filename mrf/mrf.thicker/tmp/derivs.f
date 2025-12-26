CFPP$ NOCONCUR R
      SUBROUTINE DERIVS(SYN,DYN,RCS2)
C.................................................................
C SYN(1, 0* 28 +0* 28 +1)  ZE
C SYN(1, 1* 28 +0* 28 +1)  DI
C SYN(1, 2* 28 +0* 28 +1)  TE
C SYN(1, 3* 28 +0* 28 +1)  RQ
C SYN(1, 3* 28 +1* 28 +1)  DPDLAM
C SYN(1, 3* 28 +1* 28 +2)  DPDPHI
C SYN(1, 3* 28 +1* 28 +3)  ULN
C SYN(1, 4* 28 +1* 28 +3)  VLN
C.................................................................
C DYN(1, 0* 28 +0* 28 +1)  D(T)/D(PHI)
C DYN(1, 1* 28 +0* 28 +1)  D(RQ)/D(PHI)
C DYN(1, 1* 28 +1* 28 +1)  D(T)/D(LAM)
C DYN(1, 2* 28 +1* 28 +1)  D(RQ)/D(LAM)
C DYN(1, 2* 28 +2* 28 +1)  D(U)/D(LAM)
C DYN(1, 3* 28 +2* 28 +1)  D(V)/D(LAM)
C DYN(1, 4* 28 +2* 28 +1)  D(U)/D(PHI)
C DYN(1, 5* 28 +2* 28 +1)  D(V)/D(PHI)
C.................................................................
      PARAMETER(LOTS    =5* 28 +1* 28 +2,LOTST=2* 28 +1,
     &          KSZ     =0* 28 +0* 28 +1,
     &          KSD     =1* 28 +0* 28 +1,
     &          KST     =2* 28 +0* 28 +1,
     &          KSR     =3* 28 +0* 28 +1,
     &          KSPLAM  =3* 28 +1* 28 +1,
     &          KSPPHI  =3* 28 +1* 28 +2,KSTB=3* 28 +1* 28 +2,
     &          KSU     =3* 28 +1* 28 +3,
     &          KSV     =4* 28 +1* 28 +3)
      PARAMETER(LOTD    =6* 28 +2* 28 ,
     &          KDTPHI  =0* 28 +0* 28 +1,
     &          KDRPHI  =1* 28 +0* 28 +1,
     &          KDTLAM  =1* 28 +1* 28 +1,
     &          KDRLAM  =2* 28 +1* 28 +1,
     &          KDULAM  =2* 28 +2* 28 +1,
     &          KDVLAM  =3* 28 +2* 28 +1,
     &          KDUPHI  =4* 28 +2* 28 +1,
     &          KDVPHI  =5* 28 +2* 28 +1)
C...
       DIMENSION RL( 63 ),RLCS2( 63 ),
     1 SYN( 386 ,LOTS),DYN( 386 ,LOTD)
C.................................................................
        DO  LL=1, 63
          RL(LL)=FLOAT(LL-1)/ 6.3712E+6
        ENDDO
C
      DO I=1, 63
       RLCS2(I)=RL(I)*RCS2
      ENDDO
C
C   CALCULATE T RQ U V ZONAL DERIVS. BY MULTIPLICATION WITH I*L
C   NOTE RLCS2=RCS2*L/ 6.3712E+6
C
      DO K=1, 28
       DO I=1, 63
C   D(T)/D(LAM)
        DYN(2*I-1,KDTLAM-1+K)= -SYN(2*I  ,KST-1+K)*RLCS2(I)
        DYN(2*I  ,KDTLAM-1+K)=  SYN(2*I-1,KST-1+K)*RLCS2(I)
        DYN( 192 +2*I-1,KDTLAM-1+K)= -SYN( 192 +2*I  ,KST-1+K)*RLCS2(I)
        DYN( 192 +2*I  ,KDTLAM-1+K)=  SYN( 192 +2*I-1,KST-1+K)*RLCS2(I)
C   D(U)/D(LAM)
        DYN(2*I-1,KDULAM-1+K)= -SYN(2*I  ,KSU-1+K)*RLCS2(I)
        DYN(2*I  ,KDULAM-1+K)=  SYN(2*I-1,KSU-1+K)*RLCS2(I)
        DYN( 192 +2*I-1,KDULAM-1+K)= -SYN( 192 +2*I  ,KSU-1+K)*RLCS2(I)
        DYN( 192 +2*I  ,KDULAM-1+K)=  SYN( 192 +2*I-1,KSU-1+K)*RLCS2(I)
C   D(V)/D(LAM)
        DYN(2*I-1,KDVLAM-1+K)= -SYN(2*I  ,KSV-1+K)*RLCS2(I)
        DYN(2*I  ,KDVLAM-1+K)=  SYN(2*I-1,KSV-1+K)*RLCS2(I)
        DYN( 192 +2*I-1,KDVLAM-1+K)= -SYN( 192 +2*I  ,KSV-1+K)*RLCS2(I)
        DYN( 192 +2*I  ,KDVLAM-1+K)=  SYN( 192 +2*I-1,KSV-1+K)*RLCS2(I)
       ENDDO
      ENDDO
C
      DO K=1, 28
       DO I=1, 63
C   D(RQ)/D(LAM)
        DYN(2*I-1,KDRLAM-1+K)= -SYN(2*I  ,KSR-1+K)*RLCS2(I)
        DYN(2*I  ,KDRLAM-1+K)=  SYN(2*I-1,KSR-1+K)*RLCS2(I)
        DYN( 192 +2*I-1,KDRLAM-1+K)= -SYN( 192 +2*I  ,KSR-1+K)*RLCS2(I)
        DYN( 192 +2*I  ,KDRLAM-1+K)=  SYN( 192 +2*I-1,KSR-1+K)*RLCS2(I)
       ENDDO
      ENDDO
C
      CALL FTI 192 (SYN,DUMMY,(5* 28 + 28 +2)*2,1)
C
C   D(T)/D(PHI)  D(RQ)/D(PHI) IN S. HEMI.
      DO K=1, 28
       DO I=1, 192
        DYN(I+ 192 ,KDTPHI-1+K)=-DYN(I+ 192 ,KDTPHI-1+K)
       ENDDO
      ENDDO
      DO K=1, 28
       DO I=1, 192
        DYN(I+ 192 ,KDRPHI-1+K)=-DYN(I+ 192 ,KDRPHI-1+K)
       ENDDO
      ENDDO
 
C   SYNTHESIZE TEMP.    MERIDIONAL AND ZONAL DERIVATIVES
C   SYNTHESIZE MOISTURE MERIDIONAL AND ZONAL DERIVATIVES
C   SYNTHESIZE U AND V            ZONAL      DERIVATIVES
C
      CALL FTI 192 (DYN,DUMMY,(4* 28 +2* 28 )*2,1)
C
C CALCULATE GRID MERIDIONAL DERIVATIVES OF U AND V.
C
C  COS*D(U)/D(THETA)= D(V)/D(LAM)-A*ZETA*COS**2
C  COS*D(V)/D(THETA)=-D(U)/D(LAM)+A*DIVR*COS**2
C
      DO K=1, 28
       DO J=1, 384
        DYN(J,KDUPHI-1+K)= DYN(J,KDVLAM-1+K)-SYN(J,KSZ-1+K)
        DYN(J,KDVPHI-1+K)=-DYN(J,KDULAM-1+K)+SYN(J,KSD-1+K)
       ENDDO
      ENDDO
C
      RETURN
      END
