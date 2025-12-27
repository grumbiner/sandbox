C-----------------------------------------------------------------------
      SUBROUTINE IMPADJ(D,T,Q,X,Y,Z,U,V)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    IMPADJ      IMPLICIT ADJUSTMENT OF PHYSICS TENDENCIES.
C   PRGMMR: MARK IREDELL     ORG: W/NMC23    DATE: 91-03-15
C
C ABSTRACT: EXTENDS THE SEMI-IMPLICIT TIME INTEGRATION TO INCLUDE
C           THE PHYICAL FORCING TERMS COMPUTED IN GLOOPB.
C
C PROGRAM HISTORY LOG:
C   91-03-15  MARK IREDELL
C   93-03-15  MARK IREDELL   CHANGE ARGUMENT LIST
C
C USAGE:    CALL IMPADJ(D,T,Q,X,Y,Z,U,V)
C   INPUT ARGUMENT LIST:
C     D        - DIVERGENCE BEFORE ADJUSTMENT
C     T        - TEMPERATURE BEFORE ADJUSTMENT
C     Q        - LN(PSFC) BEFORE ADJUSTMENT
C     X        - DIVERGENCE TENDENCY ADJUSTMENT
C     Y        - TEMPERATURE TENDENCY ADJUSTMENT
C     Z        - LN(PSFC) TENDENCY ADJUSTMENT
C
C   OUTPUT ARGUMENT LIST:
C     D        - DIVERGENCE ADJUSTED
C     T        - TEMPERATURE ADJUSTED
C     Q        - LN(PSFC) ADJUSTED
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
C  COMPUTE LINEAR DEPENDENCE OF DIVERGENCE ON LNPS AND TEMPERATURE.
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
          U(I,K)=X(I,K)+SNNP1(I)*(V(I,K)+GVDT(K)*Z(I))
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
          Q(I)=Q(I)+SVDT(J)*V(I,J)
        ENDDO
      ENDDO
      DO I=1,LNT2
        Q(I)=Q(I)+Z(I)
      ENDDO
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  BACK SOLVE FOR TEMPERATURE AND DIVERGENCE.
CMIC$ DO ALL AUTOSCOPE
      DO K=1,KM
CFPP$ UNROLL L
        DO J=1,KM
          DO I=1,LNT2
            T(I,K)=T(I,K)+BMDT(K,J)*V(I,J)
          ENDDO
        ENDDO
        DO I=1,LNT2
          T(I,K)=T(I,K)+Y(I,K)
          D(I,K)=D(I,K)+V(I,K)
        ENDDO
      ENDDO
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      RETURN
      END
