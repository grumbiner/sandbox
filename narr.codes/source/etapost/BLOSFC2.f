      SUBROUTINE BLOSFC2
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .     
C SUBPROGRAM:    BLOSFC2     SETS BELOW SURFACE VALUES
C   PRGRMMR: TREADON         ORG: W/NP2      DATE: 93-05-07       
C     
C ABSTRACT:  THIS ROUTINE SETS BELOW GROUND Q, U, V, 
C     AND OMEGA.  FOR U, V, AND OMEGA WE SIMPLY FILL 
C     BELOW GROUND ARRAY ELEMENTS WITH VALUES FROM 
C     THE FIRST ATMOSPHERIC ETA LAYER (FAL).  FOR Q 
C     WE FIRST COMPUTE THE FAL RELATIVE HUMIDITY.  USING
C     THE GIVEN TEMPERATURE AND PRESSURE WE USE THIS 
C     FAL RH FIELD TO COMPUTE BELOW SURFACE Q WHICH 
C     MAINTAINS THE FAL RH.
C   .     
C     
C PROGRAM HISTORY LOG:
C   93-01-27  RUSS TREADON
C   93-05-07  RUSS TREADON - ADDED DOCBLOC
C   96-03-07  MIKE BALDWIN - SPEED UP CODE 
C   98-06-08  T BLACK      - CONVERSION FROM 1-D TO 2-D
C   98-08-17  MIKE BALDWIN - COMPUTE RH OVER ICE
C   98-12-22  MIKE BALDWIN - BACK OUT RH OVER ICE
C   00-01-03  JIM TUCCILLO - MPI VERSION         
C     
C USAGE:    CALL BLOSFC2
C   INPUT ARGUMENT LIST:
C     NONE     
C
C   OUTPUT ARGUMENT LIST: 
C     NONE
C     
C   OUTPUT FILES:
C     NONE
C     
C   SUBPROGRAMS CALLED:
C     UTILITIES:
C       NONE
C     LIBRARY:
C       COMMON   - MAPOT
C                  VRBLS
C                  LOOPS
C                  EXTRA
C                  OMGAOT
C                  MASKS
C     
C   ATTRIBUTES:
C     LANGUAGE: FORTRAN 90
C     MACHINE : CRAY C-90
C$$$  
C     
C
C     INCLUDE PARAMETER STATEMENTS.  SET LOCAL PARAMETERS.
      INCLUDE "parmeta"
      INCLUDE "params"
      PARAMETER (DPBND=60.E2,ISMTHP=2,ISMTHT=2,ISMTHQ=2,ISMTHR=2,
     &  CLIMIT=1.E-20)
C     
C     DECLARE VARIABLES.
      REAL PBND(IM,JM),QBND(IM,JM),RHBND(IM,JM),TBND(IM,JM)
      REAL PSUM(IM,JM),ICEB(IM,JM),IWM1(IM,JM)
C     
C     INCLUDE COMMON BLOCKS
      INCLUDE "MAPOT.comm"
      INCLUDE "VRBLS.comm"
      INCLUDE "CLDWTR.comm"
      INCLUDE "LOOPS.comm"
      INCLUDE "EXTRA.comm"
      INCLUDE "OMGAOT.comm"
      INCLUDE "MASKS.comm"
      INCLUDE "CTLBLK.comm"
C     
C********************************************************************
C     START BLOSFC HERE.
C     
C     SET BELOW GROUND OMEGA.
!$omp  parallel do
      DO L = 1,LM
         DO J=JSTA,JEND
         DO I=1,IM
           LLMH = LMH(I,J)
           IF(L.GT.LLMH) OMGA(I,J,L) = OMGA(I,J,LLMH)
         ENDDO
         ENDDO
         CALL EXCH(OMGA(1,1,L))
      ENDDO
C     
C     SET BELOW GROUND U AND V WIND COMPONENTS.
!$omp  parallel do
!$omp& private(llmv)
      DO L = 1,LM
         DO J=JSTA,JEND
         DO I=1,IM
           LLMV = LMV(I,J)
           IF (L.GT.LLMV) THEN
             U(I,J,L) = U(I,J,LLMV)
             V(I,J,L) = V(I,J,LLMV)
           ENDIF
         ENDDO
         ENDDO
         CALL EXCH(U(1,1,L))
         CALL EXCH(V(1,1,L))
      ENDDO
C
C     LOOP OVER HORIZONTAL.  AT EACH MASS POINT COMPUTE 
C     LAYER MEAN P, T, AND Q IN A DPBND THICK BOUNDARY
C     LAYER FROM THE SURFACE UP.
C
!$omp  parallel do
      DO J=JSTA,JEND
      DO I=1,IM 
        PBND(I,J)= PD(I,J) + PT - 0.5*DPBND
        PSUM(I,J)= D00
        TBND(I,J)= D00
        QBND(I,J)= D00
        ICEB(I,J)= D00
        IWM1(I,J)= D00
      ENDDO
      ENDDO
!$omp  parallel do
!$omp& private(dp,iwm1,pbot,pm,ptop,riw)
      DO L = 1,LM
        DO J=JSTA,JEND
        DO I=1,IM
          PM = D50*(PINT(I,J,L)+PINT(I,J,L+1))
          PTOP = PBND(I,J)-DPBND*0.5
          PBOT = PBND(I,J)+DPBND*0.5
C   COMPUTE IW
          RIW=0.
          IF(CWM(I,J,L).GT.CLIMIT) THEN
             IF(T(I,J,L).LT.258.15)THEN
               RIW=1.
             ELSEIF(T(I,J,L).GE.273.15)THEN
               RIW=0.
             ELSE
               IF(IWM1(I,J).EQ.1.0)RIW=1.
             ENDIF
          ELSE
             RIW=0.
          ENDIF
          IWM1(I,J)=RIW
C   COMPUTE IW
          IF (PM.GT.PTOP.AND.PM.LE.PBOT) THEN
             DP = PINT(I,J,L+1)-PINT(I,J,L)
             PSUM(I,J) = PSUM(I,J) + DP
             TBND(I,J) = TBND(I,J) + T(I,J,L)*DP
             QBND(I,J) = QBND(I,J) + Q(I,J,L)*DP
             ICEB(I,J) = ICEB(I,J) + RIW*DP
          ENDIF
        ENDDO
        ENDDO
      ENDDO
C
      DO J=JSTA,JEND
      DO I=1,IM
         IF (PSUM(I,J).NE.0.) THEN
            RPSUM   = 1./PSUM(I,J)
            TBND(I,J) = TBND(I,J)*RPSUM
            QBND(I,J) = QBND(I,J)*RPSUM
            ICEB(I,J) = ICEB(I,J)*RPSUM
            IF (ICEB(I,J).LT.0.5) ICEB(I,J)=0.
         ELSE
            LLMH=LMH(I,J)
            TBND(I,J) = T(I,J,LLMH)
            QBND(I,J) = Q(I,J,LLMH)
            ICEB(I,J) = IWM1(I,J)
         ENDIF
      ENDDO
      ENDDO
C     USE BOUNDARY LAYER PRESSURE, TEMPERATURE, AND SPECIFIC
C     HUMIDITY ARRAYS TO COMPUTE BOUNDARY LAYER RELATIVE
C     HUMIDITY
      CALL P2FILT(ISMTHP,HBM2,PBND)
      CALL P2FILT(ISMTHT,HBM2,TBND)
      CALL P2FILT(ISMTHQ,HBM2,QBND)
      CALL BOUNDL(QBND,H1M12,H99999,IM,JM)
      CALL CALRH2(PBND,TBND,QBND,ICEB,RHBND,IM,JM)
      CALL P2FILT(ISMTHR,HBM2,RHBND)
C     
C     SET BELOW GROUND Q TO PRESERVE BOUNDARY LAYER
C     RELATIVE HUMIDITY.
C     
!$omp  parallel do
!$omp& private(ai,bi,llmh,pm,qi,qint,qs,qw,tm,tmt0,tmt15)
      DO L = 1,LM
        DO J=JSTA,JEND
        DO I=1,IM
          LLMH=LMH(I,J)
          IF(L.GT.LLMH)THEN
            PM=D50*(PINT(I,J,L)+PINT(I,J,L+1))
            TM=T(I,J,L)
C     
            TMT0=TM-273.16
            TMT15=AMIN1(TMT0,-15.)
            AI=0.008855
            BI=1.
            IF(TMT0.LT.-20.)THEN
              AI=0.007225
              BI=0.9674
            ENDIF
            QW=PQ0/PM*EXP(A2*(TM-A3)/(TM-A4))
            QI=QW*(BI+AI*AMIN1(TMT0,0.))
            QINT=QW*(1.-0.00032*TMT15*(TMT15+15.))
            IF(TMT0.LT.-15.)THEN
               QS=QI
            ELSEIF(TMT0.GE.0.)THEN
               QS=QINT
            ELSE
               IF(ICEB(I,J).GT.0.0) THEN
                 QS=QI
               ELSE
                 QS=QINT
               ENDIF
            ENDIF
CMEB 12/22/98 SWITCH TO RH VS WATER NO MATTER WHAT
C             DELETE THIS LINE TO SWITCH BACK TO RH VS ICE
            QS=QW
CMEB 12/22/98 SWITCH TO RH VS WATER NO MATTER WHAT
C
C
            Q(I,J,L)=RHBND(I,J)*QS
            Q(I,J,L)=AMAX1(H1M12,Q(I,J,L))
          ENDIF
        ENDDO
        ENDDO
        CALL EXCH(Q(1,1,L))
      ENDDO
C     
C     END OF ROUTINE
C     
      RETURN
      END
