      SUBROUTINE CLMAX
     I (DETA,PDSL,HTM,Q2,ZINT,SM,HGT,LMH,IM,JM,LM,LP1
     O, EL0
     W, SQZ,SQ,RQ2L,RQ2H)
CFPP$ NOCONCUR R
C
C     CALCULATES THE FREE-ATMOSPHERE ASYMPTOTE OF THE TURBULENCE LENGTH
C     SCALE (L-INF IN THE BLACKADAR's FORMULA) FROM THE DISTRIBUTION
C     OF THE TURBULENT ENERGY (see MY82)
C
C     EXTRACTED FROM EXISTING CODE BY L. LOBOCKI, JULY 28, 1992
C
C     INPUT:
C     ------
C
C     DETA (LM)        - VECTOR OF THE ETA COORDINATE STEPS
C     PDSL (IM,JM)     - VECTORIZED ARRAY OF SURFACE PRESSURE
C     HTM  (IM,JM,LM)  - HEIGHT TOPOGRAPHY MASK ARRAY
C     Q2   (IM,JM,LM)  - TWICE THE TURBULENT ENERGY FIELD
C     ZINT (IM,JM,LP1) - ETA INTERFACES HEIGHT FIELD
C     SM   (IM,JM)     - SEA MASK
C     HGT  (IM,JM)     - SURFACE ELEVATION ARRAY
C     LMH  (IM,JM)     - TOPOGRAPHY INDEXES ARRAY
C     IM,JM            - ARRAY DIMENSIONS FOR HORIZONTAL GRIDS
C                        IN A VECTORIZED FORM
C     KHL00            - INDEX POINTING TO THE BEGIN OF INTERNAL H-POINTS
C                        IN HORIZONTAL GRIDS
C     KHH00            - INDEX POINTING TO THE END OF INTERNAL H-POINTS
C                        IN HORIZONTAL GRIDS
C     LM               - ARRAY DIMENSION FOR VERTICAL GRIDS
C     LP1              = LM+1
C
C
C     OUTPUT:
C     -------
C
C     EL0 (IM,JM)      - ARRAY OF RESULTING ASYMPTOTIC MIXING LENGTHS
C
C
C     SCRATCH AREAS:
C     --------------
C
C     SQZ(IM,JM),SQ(IM,JM),RQ2L(IM,JM),RQ2H(IM,JM)
C
C
C     RELEVANT CONSTANTS:
C     -------------------
C
C     PROPORTIONALITY CONSTANT BETWEEN ASYMPTOTIC MIXING LENGTH AND THE
C     S.D. OF Q DISTRIBUTION, FOR LAND AND SEA AREAS, CORRESPONDINGLY:
      PARAMETER (ALPHAL=0.2, ALPHAS=0.2)
C
C     ASYMPTOTIC MIXING LENGTH LIMITATIONS:
      PARAMETER (EL0M=300.0, ELMIN=11.0)
C
C     MINIMAL VALUE OF TURBULENT ENERGY:
      PARAMETER (EPSQ2=1.0E-4)
C
C     ------------------------------------------------------------------
C
                         D I M E N S I O N
     & DETA(LM),PDSL(IM,JM),HTM(IM,JM,LM),Q2(IM,JM,LM)
     &,ZINT(IM,JM,LP1),SM(IM,JM),HGT(IM,JM),EL0(IM,JM),LMH(IM,JM)
     &,SQZ(IM,JM),SQ(IM,JM),RQ2L(IM,JM),RQ2H(IM,JM)
C     ------------------------------------------------------------------
C
      INCLUDE "CTLBLK.comm"
C
!$omp  parallel do
      DO J=JSTA,JEND
      DO I=1,IM
        SQZ(I,J)=0.0
        SQ(I,J)=0.0
        RQ2H(I,J)=0.0
      ENDDO
      ENDDO
C
      DO 220 L=1,LM
!$omp  parallel do
!$omp& private(dp,rq2m)
      DO J=JSTA,JEND
      DO I=1,IM
        IF(Q2(I,J,L).LE.EPSQ2) THEN
          RQ2L(I,J)=0.0
        ELSE
          RQ2L(I,J)=SQRT(Q2(I,J,L))
        ENDIF
C
C         -----------------------------------------------------------------
C         THIS PART OF THE CODE IS LEFT FOR TESTING OTHER PARAMETERIZATION
C         SCHEMES 
C
C         IF (L.GE.LMH(I,J)) GOTO 215
c         RQ2L(I,J)=SQRT(Q2(I,J,L))
c         IF(Q2(I,J,L).LT.0.0)THEN
c           write(3,*)'NEGATIVE Q2 AT (I,J,L)=(',I,',',J,',',L,'): ',
c                     Q2(I,J,L)
c           STOP
c         ENDIF
C         -----------------------------------------------------------------
C
          DP=DETA(L)*PDSL(I,J)*HTM(I,J,L)
C***
C***      SUM OF Q2 AT BOTH LOWER & UPPER SURFACES:
C***
          RQ2M=(RQ2H(I,J)+RQ2L(I,J))
C***
C***      INTEGRAL OF Q*Z OVER DP
C***
          SQZ(I,J)=((ZINT(I,J,L)+ZINT(I,J,L+1))*0.5-HGT(I,J))*RQ2M*DP
     1              +SQZ(I,J)
C***
C***      INTEGRAL OF Q OVER DP:
C***
          SQ(I,J)=RQ2M*DP+SQ(I,J)
          RQ2H(I,J)=RQ2L(I,J)
      ENDDO
      ENDDO
c215  CONTINUE
 220  CONTINUE
C***
C***    CLIPPING & APPLYING DIFFERENT VALUES OF THE PROPORTIONALITY 
C***    CONSTANT ALPHA FOR THE LAND AND SEA AREA:
C***
!$omp  parallel do
      DO J=JSTA,JEND
      DO I=1,IM
        EL0(I,J)= AMAX1(AMIN1(
     1    ((SM(I,J)*ALPHAS+(1.0-SM(I,J))*ALPHAL)*SQZ(I,J)
     2     /(SQ(I,J)+EPSQ2)),
     3    EL0M),ELMIN)
      ENDDO
      ENDDO
C
      RETURN
      END

