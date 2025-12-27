      SUBROUTINE BNDLYR2(PBND,TBND,QBND,RHBND,UBND,VBND,
     X     OMGBND,PWTBND,LVLBND)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .     
C SUBPROGRAM:    BNDLYR2     COMPUTES CONSTANT MASS MEAN FIELDS
C   PRGRMMR: TREADON         ORG: W/NP2      DATE: 93-01-29
C     
C ABSTRACT:  THIS ROUTINE COMPUTES CONSTANT MASS (BOUNDARY LAYER)
C   FIELDS.  THE FIELDS ARE A MEAN OVER LAYERS PARAMETER DPBND
C   (PASCALS) THICK.  THERE ARE NBND CONSTANT MASS LAYERS, EACH
C   DPBND THICK STARTING FROM THE SURFACE UP.  COMPUTED BOUNDARY 
C   LAYER FIELDS ARE PRESSURE, TEMPERATURE, SPECIFIC HUMIDITY,
C   RELATIVE HUMIDITY, U AND V WINDS, VERTICAL VELOCITY,
C   AND PRECIPITABLE WATER.  GIVEN THESE FUNDAMENTAL VARIABLES
C   OTHER FIELDS MAY BE COMPUTED.
C
C   ***WARNING*** IF YOU CHANGE PARAMETER NBND IN THIS ROUTINE 
C                 DON'T FOREGET TO CHANGE IT ALSO IN THE CALLING
C                 SUBPROGRAM, MISCLN.
C   .     
C     
C PROGRAM HISTORY LOG:
C   93-01-29  RUSS TREADON
C   93-05-07  RUSS TREADON - ADDED DOC BLOCK AND MORE COMMENTS.
C   93-06-19  RUSS TREADON - ADDED LVLBND TO PARAMETER LIST.
C   96-03-07  MIKE BALDWIN - CHANGE PWTR CALC TO INCLUDE CLD WTR
C                            SPEED UP CODE
C   98-06-16  T BLACK      - CONVERSION FROM 1-D TO 2-D
C   98-08-18  MIKE BALDWIN - CHANGE QSBND TO RHBND IN CALL,
C                            COMPUTE RH OVER ICE
C   98-12-22  MIKE BALDWIN - BACK OUT RH OVER ICE
C   00-01-04  JIM TUCCILLO - MPI VERSION 
C     
C     USAGE:    CALL BNDLYR2(PBND,TBND,QBND,RHBND,UBND,VBND,
C                            OMGBND,PWTBND,LVLBND)
C           
C   INPUT ARGUMENT LIST:
C     NONE     
C
C   OUTPUT ARGUMENT LIST: 
C     PBND     - LAYER MEAN PRESSURE IN NBND BOUNDARY LAYERS (NBL).
C     TBND     - LAYER MEAN TEMPERATURE IN NBL.
C     QBND     - LAYER MEAN SPECIFIC HUMIDITY IN NBL.
C     RHBND    - LAYER MEAN RELATIVE HUM. (QBND/QSBND) IN  NBL.
C     UBND     - LAYER MEAN U WIND COMPONENT IN NBL.
C     VBND     - LAYER MEAN V WIND COMPONENT IN NBL.
C     OMGBND   - LAYER MEAN VERTICAL VELOCITY IN NBL.
C     PWTBND   - LAYER PRECIPITABLE WATER IN NBL.
C     LVLBND   - ETA LAYER AT MIDPOINT OF NBL.
C     
C   OUTPUT FILES:
C     NONE
C     
C   SUBPROGRAMS CALLED:
C     UTILITIES:
C       H2V
C
C     LIBRARY:
C       COMMON   - LOOPS
C                  VRBLS
C                  MAPOT
C                  EXTRA
C                  MASKS
C                  PVRBLS
C                  OMGAOT
C                  OPTIONS
C                  INDX
C     
C   ATTRIBUTES:
C     LANGUAGE: FORTRAN 90
C     MACHINE : CRAY C-90
C$$$  
C     
C
C     INCLUDE GLOBAL PARAMETERS.  SET LOCAL PARAMETERS.
C        DPBND:  DEPTH OF BOUNDARY LAYER (IN PASCALS).
C     
      INCLUDE "parmeta"
      INCLUDE "params"
C
C
C
      PARAMETER (DPBND=30.E2,RHOWAT=1.E3,rrhowt=1./rhowat)
      PARAMETER (NBND=6)
C     
C     DECLARE VARIABLES.
C     
      INTEGER LVLBND(IM,JM,NBND)
      REAL PBND(IM,JM,NBND),TBND(IM,JM,NBND),QBND(IM,JM,NBND)
      REAL UBND(IM,JM,NBND),VBND(IM,JM,NBND),OMGBND(IM,JM,NBND)
      REAL PWTBND(IM,JM,NBND),PBINT(IM,JM,NBND+1)
      REAL QSBND(IM,JM,NBND),RHBND(IM,JM,NBND),IWL(IM,JM),IWM1
      REAL PSUM(IM,JM,NBND),PVSUM(IM,JM,NBND),NSUM(IM,JM,NBND)
C     
C     INCLUDE COMMONS.
      INCLUDE "LOOPS.comm"
      INCLUDE "VRBLS.comm"
      INCLUDE "MAPOT.comm"
      INCLUDE "EXTRA.comm"
      INCLUDE "MASKS.comm"
      INCLUDE "PVRBLS.comm"
      INCLUDE "CLDWTR.comm"
      INCLUDE "OMGAOT.comm"
      INCLUDE "OPTIONS.comm"
      INCLUDE "INDX.comm"
      INCLUDE "CTLBLK.comm"
C
C*****************************************************************************
C     START BNDLYR HERE
        CLIMIT =1.0E-20
C
C     LOOP OVER HORIZONTAL GRID.  AT EACH MASS POINT COMPUTE
C     PRESSURE AT THE INTERFACE OF EACH BOUNDARY LAYER.
C     
!$omp  parallel do
      DO J=JSTA,JEND
      DO I=1,IM
        PBINT(I,J,1)=PD(I,J)+PT
      ENDDO
      ENDDO
C
      DO LBND=2,NBND+1
!$omp  parallel do
        DO J=JSTA,JEND
        DO I=1,IM
          PBINT(I,J,LBND)=PBINT(I,J,LBND-1)-DPBND
        ENDDO
        ENDDO
      ENDDO
C     
C     LOOP OVER HORIZONTAL.  AT EACH MASS POINT COMPUTE 
C     MASS WEIGHTED LAYER MEAN P, T, Q, U, V, OMEGA, 
C     WAND PRECIPITABLE WATER IN EACH BOUNDARY LAYER FROM THE SURFACE UP.
C     
!$omp  parallel do
!$omp& private(ai,bi,dp,iwl,iwm1,pm,qi,qint,qsat,qw,tmt0,tmt15)
      DO LBND=1,NBND
        DO J=JSTA,JEND
        DO I=1,IM
          PBND(I,J,LBND)   = D00
          TBND(I,J,LBND)   = D00
          QBND(I,J,LBND)   = D00
          QSBND(I,J,LBND)  = D00
          RHBND(I,J,LBND)  = D00
          UBND(I,J,LBND)   = D00
          VBND(I,J,LBND)   = D00
          OMGBND(I,J,LBND) = D00
          LVLBND(I,J,LBND) = 0
          NSUM(I,J,LBND)   = 0
          PSUM(I,J,LBND)   = D00
          PVSUM(I,J,LBND)  = D00
          PWTBND(I,J,LBND) = D00
        ENDDO
        ENDDO
C
        IWL=0.
        DO L=1,LM
          DO J=JSTA,JEND
          DO I=1,IM
C
C   COMPUTE IW
C
           IF (L.LE.LMH(I,J)) THEN
            IWM1=IWL(I,J)
            IF(CWM(I,J,L).GT.CLIMIT) THEN
             IF(T(I,J,L).LT.258.15)THEN
               IWL(I,J)=1.
             ELSEIF(T(I,J,L).GE.273.15)THEN
               IWL(I,J)=0.
             ELSE
               IF(IWM1.EQ.1.0)IWL(I,J)=1.
             ENDIF
            ELSE
             IWL(I,J)=0.
            ENDIF
           ENDIF
C
            PM=D50*(PINT(I,J,L)+PINT(I,J,L+1))
            IF((PBINT(I,J,LBND).GE.PM).AND.
     1         (PBINT(I,J,LBND+1).LE.PM)) THEN
              DP     = PINT(I,J,L+1)-PINT(I,J,L)
              PSUM(I,J,LBND)  =PSUM(I,J,LBND)+DP
              NSUM(I,J,LBND)  =NSUM(I,J,LBND)+1
              LVLBND(I,J,LBND)=LVLBND(I,J,LBND)+L
              TBND(I,J,LBND)  =TBND(I,J,LBND)+T(I,J,L)*DP
              QBND(I,J,LBND)  =QBND(I,J,LBND)+Q(I,J,L)*DP
              OMGBND(I,J,LBND)=OMGBND(I,J,LBND)+OMGA(I,J,L)*DP
              PWTBND(I,J,LBND)=PWTBND(I,J,LBND)
     1                        +(Q(I,J,L)+CWM(I,J,L))*DP*GI
C
              TMT0=T(I,J,L)-273.16
              TMT15=AMIN1(TMT0,-15.)
              AI=0.008855
              BI=1.
              IF(TMT0.LT.-20.)THEN
                AI=0.007225
                BI=0.9674
              ENDIF
              QW=PQ0/PM
     1          *EXP(A2*(T(I,J,L)-A3)/(T(I,J,L)-A4))
              QI=QW*(BI+AI*AMIN1(TMT0,0.))
              QINT=QW*(1.-0.00032*TMT15*(TMT15+15.))
              IF(TMT0.LT.-15.)THEN
                  QSAT=QI
              ELSEIF(TMT0.GE.0.)THEN
                  QSAT=QINT
              ELSE
                IF(IWL(I,J).GT.0.0) THEN
                  QSAT=QI
                ELSE
                  QSAT=QINT
                ENDIF
              ENDIF
CMEB 12/22/98 SWITCH TO RH VS WATER NO MATTER WHAT
C             DELETE THIS LINE TO SWITCH BACK TO RH VS ICE
              QSAT=QW
CMEB 12/22/98 SWITCH TO RH VS WATER NO MATTER WHAT
              QSBND(I,J,LBND)=QSBND(I,J,LBND)+QSAT*DP
            ENDIF
          ENDDO
          ENDDO
        ENDDO
C
C         NEED TO MAKE SURE THAT PINT HALOS EXIST
C
        DO L = 1, LP1
           CALL EXCH(PINT(1,1,L))
        end do
C
        DO L=1,LM
C
          DO J=JSTA_M,JEND_M
          DO I=2,IM-1
            IE=I+IVE(J)
            IW=I+IVW(J)
            PV1=0.25*(PINT(IW,J,L)+PINT(IE,J,L)
     1               +PINT(I,J+1,L)+PINT(I,J-1,L))
            PV2=0.25*(PINT(IW,J,L+1)+PINT(IE,J,L+1)
     1               +PINT(I,J+1,L+1)+PINT(I,J-1,L+1))
            DP=PV2-PV1
            PMV=0.5*(PV1+PV2)
            IF((PBINT(IW,J,LBND).GE.PMV).AND.
     X         (PBINT(IW,J,LBND+1).LE.PMV)) THEN
              PVSUM(I,J,LBND)=PVSUM(I,J,LBND)+DP
              UBND(I,J,LBND)=UBND(I,J,LBND)+U(I,J,L)*DP
              VBND(I,J,LBND)=VBND(I,J,LBND)+V(I,J,L)*DP
            ENDIF
C
          ENDDO
          ENDDO
        ENDDO
      ENDDO
C
!$omp  parallel do
!$omp& private(rpsum)
      DO LBND=1,NBND
        DO J=JSTA,JEND
        DO I=1,IM
          IF(PSUM(I,J,LBND).NE.0.)THEN
            RPSUM           = 1./PSUM(I,J,LBND)
            LVLBND(I,J,LBND)= LVLBND(I,J,LBND)/NSUM(I,J,LBND)
            PBND(I,J,LBND)  = (PBINT(I,J,LBND)+PBINT(I,J,LBND+1))*0.5
            TBND(I,J,LBND)  = TBND(I,J,LBND)*RPSUM
            QBND(I,J,LBND)  = QBND(I,J,LBND)*RPSUM
            QSBND(I,J,LBND) = QSBND(I,J,LBND)*RPSUM
            OMGBND(I,J,LBND)= OMGBND(I,J,LBND)*RPSUM
          ENDIF
        ENDDO
        ENDDO
C
        DO J=JSTA_M,JEND_M
        DO I=2,IM-1
          IF(PVSUM(I,J,LBND).NE.0.)THEN
            RPVSUM      =1./PVSUM(I,J,LBND)
            UBND(I,J,LBND)=UBND(I,J,LBND)*RPVSUM
            VBND(I,J,LBND)=VBND(I,J,LBND)*RPVSUM
          ENDIF
        ENDDO
        ENDDO
      ENDDO
C
C  IF NO ETA MID LAYER PRESSURES FELL WITHIN A BND LYR,
C   FIND THE CLOSEST LAYER TO THE BND LYR AND ASSIGN THE VALUES THERE
C
!$omp  parallel do
!$omp& private(ai,bi,delp,delpv,dp,ie,iw,l,lv,pm,pmin,pminv,pmv,
!$omp&         qi,qint,qsat,qw,tmt0,tmt15)
      DO LBND=1,NBND
        DO J=JSTA,JEND
        DO I=1,IM
          IF(PSUM(I,J,LBND).EQ.0.)THEN
            L=LM
            PMIN=9999999.
            PBND(I,J,LBND)=(PBINT(I,J,LBND)+PBINT(I,J,LBND+1))*0.5
C
            DO LL=1,LM
              PM=D50*(PINT(I,J,LL)+PINT(I,J,LL+1))
              DELP=ABS(PM-PBND(I,J,LBND))
              IF(DELP.LT.PMIN)THEN
                PMIN=DELP
                L=LL
              ENDIF
            ENDDO
C
            DP=PINT(I,J,L+1)-PINT(I,J,L)
            PM=D50*(PINT(I,J,L)+PINT(I,J,L+1))
            LVLBND(I,J,LBND)=L
            TBND(I,J,LBND)=T(I,J,L)
            QBND(I,J,LBND)=Q(I,J,L)
C
            TMT0=T(I,J,L)-273.16
            TMT15=AMIN1(TMT0,-15.)
            AI=0.008855
            BI=1.
            IF(TMT0.LT.-20.)THEN
              AI=0.007225
              BI=0.9674
            ENDIF
            QW=PQ0/PM
     1          *EXP(A2*(T(I,J,L)-A3)/(T(I,J,L)-A4))
            QI=QW*(BI+AI*AMIN1(TMT0,0.))
            QINT=QW*(1.-0.00032*TMT15*(TMT15+15.))
            IF(TMT0.LT.-15.)THEN
                QSAT=QI
            ELSEIF(TMT0.GE.0.)THEN
                QSAT=QINT
            ELSE
              IF(IWL(I,J).GT.0.0) THEN
                QSAT=QI
              ELSE
                QSAT=QINT
              ENDIF
            ENDIF
CMEB 12/22/98 SWITCH TO RH VS WATER NO MATTER WHAT
C             DELETE THIS LINE TO SWITCH BACK TO RH VS ICE
              QSAT=QW
CMEB 12/22/98 SWITCH TO RH VS WATER NO MATTER WHAT
            QSBND(I,J,LBND)=QSAT
            OMGBND(I,J,LBND)=OMGA(I,J,L)
            PWTBND(I,J,LBND)=(Q(I,J,L)+CWM(I,J,L))*DP*GI
          ENDIF
C
C   RH, BOUNDS CHECK
C
          RHBND(I,J,LBND)=QBND(I,J,LBND)/QSBND(I,J,LBND)
          IF (RHBND(I,J,LBND).GT.1.0) THEN
            RHBND(I,J,LBND)=1.0
            QBND(I,J,LBND)=RHBND(I,J,LBND)*QSBND(I,J,LBND)
          ENDIF
          IF (RHBND(I,J,LBND).LT.0.01) THEN
            RHBND(I,J,LBND)=0.01
            QBND(I,J,LBND)=RHBND(I,J,LBND)*QSBND(I,J,LBND)
          ENDIF
        ENDDO
        ENDDO
C
        DO J=JSTA_M,JEND_M
        DO I=2,IM-1
          IF(PVSUM(I,J,LBND).EQ.0.)THEN
            LV=LM
            PMINV=9999999.
            IE=I+IVE(J)
            IW=I+IVW(J)
C
C           PINT HALOS UPDATED ALREADY
C
            DO LL=1,LM
              PMV=0.125*(PINT(IW,J,LL)+PINT(IE,J,LL)+
     1              PINT(I,J+1,LL)+PINT(I,J-1,LL)+
     2              PINT(IW,J,LL+1)+PINT(IE,J,LL+1)+
     3              PINT(I,J+1,LL+1)+PINT(I,J-1,LL+1))
              DELPV=ABS(PMV-PBND(I,J,LBND))
              IF(DELPV.LT.PMINV)THEN
                PMINV=DELPV
                LV=LL
              ENDIF
            ENDDO
C
            UBND(I,J,LBND)=U(I,J,LV)
            VBND(I,J,LBND)=V(I,J,LV)
          ENDIF
        ENDDO
        ENDDO
      ENDDO
C
C     END OF ROUTINE
C     
      RETURN
      END
C
