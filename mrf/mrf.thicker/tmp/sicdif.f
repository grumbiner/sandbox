C-----------------------------------------------------------------------
      SUBROUTINE SICDIF(D,T,Q,X,Y,Z,U,V)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    SICDIF      SEMI-IMPLICIT TIME INTEGRATION.
C   PRGMMR: JOSEPH SELA      ORG: W/NMC23    DATE: 89-03-15
C
C ABSTRACT: INTEGRATES DIVERGENCE, TEMPERATURE AND LOG SURFACE PRESSURE
C           SEMI-IMPLICITLY IN TIME.
C
C PROGRAM HISTORY LOG:
C   89-03-15  JOSEPH SELA
C   93-03-15  MARK IREDELL   LINEAR MATRICES PASSED IN COMMON
C
C USAGE:    CALL SICDIF(D,T,Q,X,Y,Z,U,V)
C   INPUT ARGUMENT LIST:
C     D        - DIVERGENCE AT TIME T-DT
C     T        - TEMPERATURE AT TIME T-DT
C     Q        - LN(PSFC) AT TIME T-DT
C     X        - DIVERGENCE NONLINEAR TENDENCY AT TIME T
C     Y        - TEMPERATURE NONLINEAR TENDENCY AT TIME T
C     Z        - LN(PSFC) NONLINEAR TENDENCY AT TIME T
C
C   OUTPUT ARGUMENT LIST:
C     X        - DIVERGENCE AT TIME T+DT
C     Y        - TEMPERATURE AT TIME T+DT
C     Z        - LN(PSFC) AT TIME T+DT
C     U        - WORK ARRAY
C     V        - WORK ARRAY
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77.
C   MACHINE:  CRAY YMP.
C
C$$$
      PARAMETER(KM= 28 ,JCAP= 62 ,LNT2= 4032 ,LNT22= 4033 )
      REAL D(LNT22,KM),T(LNT22,KM),Q(LNT22)
      REAL X(LNT22,KM),Y(LNT22,KM),Z(LNT22),U(LNT22,KM),V(LNT22,KM)
      COMMON/COMBIT/NDEX( 4032 ),SNNP1( 4032 )
      COMMON/COMBIT/LAB(4),IFIN,ICEN,IGEN,ICEN2,IENST,IENSI,RUNID,USRID
      CHARACTER*8 LAB
      COMMON/COMSIC/ DT,GVDT(KM),SVDT(KM),AMDT(KM,KM),BMDT(KM,KM),
     1               DM(KM,KM,0:JCAP)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  EXPLICITLY INTEGRATE LNPS AND TEMPERATURE HALFWAY IN TIME.
      DO I=1,LNT2
        Z(I)=Q(I)+DT*Z(I)
      ENDDO
      DO K=1,KM
        DO I=1,LNT2
          Y(I,K)=T(I,K)+DT*Y(I,K)
        ENDDO
      ENDDO
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  COMPUTE LINEAR DEPENDENCE OF DIVERGENCE ON LNPS AND TEMPERATURE.
C  EXPLICITLY INTEGRATE DIVERGENCE HALFWAY INCLUDING LINEAR TERMS.
CMIC$ DO ALL AUTOSCOPE
      DO K=1,KM
        DO I=1,LNT2
          V(I,K)=0.
        ENDDO
CFPP$ UNROLL L
        DO J=1,KM
          DO I=1,LNT2
            V(I,K)=V(I,K)+AMDT(K,J)*Y(I,J)
          ENDDO
        ENDDO
        DO I=1,LNT2
          U(I,K)=D(I,K)+DT*X(I,K)+SNNP1(I)*(V(I,K)+GVDT(K)*Z(I))
        ENDDO
      ENDDO
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  SOLVE HELMHOLZ EQUATION FOR SEMI-IMPLICIT DIVERGENCE.
CMIC$ DO ALL AUTOSCOPE
      DO K=1,KM
        DO I=1,LNT2
          V(I,K)=0.
        ENDDO
CFPP$ UNROLL L
        DO J=1,KM
          DO I=1,LNT2,2
            N=NDEX(I)
            V(I,K)=V(I,K)+DM(K,J,N)*U(I,J)
            V(I+1,K)=V(I+1,K)+DM(K,J,N)*U(I+1,J)
          ENDDO
        ENDDO
      ENDDO
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  BACK SOLVE FOR LNPS.
CFPP$ UNROLL L
      DO J=1,KM
        DO I=1,LNT2
          Z(I)=Z(I)+SVDT(J)*V(I,J)
        ENDDO
      ENDDO
      DO I=1,LNT2
        Z(I)=2*Z(I)-Q(I)
      ENDDO
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  BACK SOLVE FOR TEMPERATURE AND DIVERGENCE.
CMIC$ DO ALL AUTOSCOPE
      DO K=1,KM
CFPP$ UNROLL L
        DO J=1,KM
          DO I=1,LNT2
            Y(I,K)=Y(I,K)+BMDT(K,J)*V(I,J)
          ENDDO
        ENDDO
        DO I=1,LNT2
          Y(I,K)=2*Y(I,K)-T(I,K)
          X(I,K)=2*V(I,K)-D(I,K)
        ENDDO
      ENDDO
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      RETURN
      END
