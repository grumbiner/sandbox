      SUBROUTINE TRPAUS(PTROP,TTROP,ZTROP,UTROP,VTROP,SHTROP)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .     
C SUBPROGRAM:    TRPAUS      COMPUTE TROPOPAUSE DATA.
C   PRGRMMR: TREADON         ORG: W/NP2      DATE: 92-12-22       
C     
C ABSTRACT:  
C     THIS ROUTINE COMPUTES TROPOPAUSE DATA.  AT EACH MASS
C     POINT A SURFACE UP SEARCH IS MADE FOR THE FIRST 
C     OCCURRENCE OF A THREE LAYER MEAN LAPSE RATE LESS THAN
C     OR EQUAL TO A CRITICAL LAPSE RATE.  THIS CRITCAL LAPSE
C     RATE IS 2DEG/KM.  THIS IS IN ACCORD WITH THE WMO
C     DEFINITION OF A TROPOPAUSE.  A MAXIMUM TROPOPAUSE
C     PRESSURE OF 500MB IS ENFORCED.  ONC THE TROPOPAUSE
C     IS LOCATED IN A COLUMN, PRESSURE, TEMPERATURE, U
C     AND V WINDS, AND VERTICAL WIND SHEAR ARE COMPUTED.
C   .     
C     
C PROGRAM HISTORY LOG:
C   92-12-22  RUSS TREADON
C   97-03-06  GEOFF MANIKIN - CHANGED CRITERIA FOR DETERMINING
C                            THE TROPOPAUSE AND ADDED HEIGHT
C   98-06-15  T BLACK       - CONVERSION FROM 1-D TO 2-D
C   00-01-04  JIM TUCCILLO  - MPI VERSION
C     
C USAGE:    CALL TRPAUS(PTROP,TTROP,ZTROP,UTROP,VTROP,SHTROP)
C   INPUT ARGUMENT LIST:
C     NONE     
C
C   OUTPUT ARGUMENT LIST: 
C     PTROP    - TROPOPAUSE PRESSURE.
C     TTROP    - TROPOPAUSE TEMPERATURE.
C     ZTROP    - TROPOPAUSE HEIGHT
C     UTROP    - TROPOPAUSE U WIND COMPONENT.
C     VTROP    - TROPOPAUSE V WIND COMPONENT.
C     SHTROP   - VERTICAL WIND SHEAR AT TROPOPAUSE.
C     
C   OUTPUT FILES:
C     NONE
C     
C   SUBPROGRAMS CALLED:
C     UTILITIES:
C       H2V     
C
C     LIBRARY:
C       COMMON   - VRBLS
C                  LOOPS
C                  EXTRA
C                  OPTIONS
C                  MASKS
C                  INDX
C     
C   ATTRIBUTES:
C     LANGUAGE: FORTRAN
C     MACHINE : CRAY C-90
C$$$  
C     
C     
C     INCLUDE ETA GRID DIMENSIONS.  SET/DERIVE PARAMETERS.
C
      INCLUDE "parmeta"
      INCLUDE "params"
C
C     
C     PARAMTER CRTLAP SPECIFIES THE CRITICAL LAPSE RATE
C     (IN K/M) IDENTIFYING THE TROPOPAUSE.  WE START 
C     LOOKING FOR THE TROPOPAUSE ABOVE PRESSURE LEVEL
C     PSTART (IN PASALS).
      PARAMETER (CRTLAP=0.002E0, PSTART=5.0E4)
C     
C     DECLARE VARIABLES.
C     
      REAL PTROP(IM,JM),TTROP(IM,JM),ZTROP(IM,JM),UTROP(IM,JM)
      REAL VTROP(IM,JM),SHTROP(IM,JM),EGRIDU(IM,JM),EGRIDV(IM,JM)
      REAL TLAPSE(LM),DZ2(LM),DELT2(LM),TLAPSE2(LM)
C     
C     INCLUDE COMMON BLOCKS.
      INCLUDE "VRBLS.comm"
      INCLUDE "LOOPS.comm"
      INCLUDE "EXTRA.comm"
      INCLUDE "MASKS.comm"
      INCLUDE "OPTIONS.comm"
      INCLUDE "INDX.comm"
      INCLUDE "CTLBLK.comm"
C     
C*****************************************************************************
C     START TRPAUS HERE.
C     
C     LOOP OVER THE HORIZONTAL GRID.
C    
      DO J=JSTA,JEND
      DO I=1,IM
         PTROP(I,J)  = SPVAL
         TTROP(I,J)  = SPVAL
         ZTROP(I,J)  = SPVAL
         UTROP(I,J)  = SPVAL
         VTROP(I,J)  = SPVAL
         SHTROP(I,J) = SPVAL
         EGRIDU(I,J) = D00
         EGRIDV(I,J) = D00
      ENDDO
      ENDDO
C
!$omp  parallel do
!$omp& private(delt,delt2,dz,dz2,ie,iw,l,llmh,pm,rsqdif,
!$omp&         tlapse,tlapse2,u0,u0l,uh,uh0,ul,
!$omp&         v0,v0l,vh,vh0)
      DO 20 J=JSTA_M,JEND_M
      DO 20 I=2,IM-1
C     
C        COMPUTE THE TEMPERATURE LAPSE RATE (-DT/DZ) BETWEEN ETA 
C        LAYERS MOVING UP FROM THE GROUND.  THE FIRST ETA LAYER
C        ABOVE PRESSURE "PSTART" IN WHICH THE LAPSE RATE IS LESS
C        THAN THE CRITCAL LAPSE RATE IS LABELED THE TROPOPAUSE.
C
        LLMH=LMH(I,J)
C
        DO 10 L=LLMH-1,2,-1
        PM     = PINT(I,J,L)
        DELT   = T(I,J,L-1)-T(I,J,L)
        DZ     = D50*(ZINT(I,J,L-1)-ZINT(I,J,L+1))
        TLAPSE(L) = -DELT/DZ
C
        IF ((TLAPSE(L).LT.CRTLAP).AND.(PM.LT.PSTART)) THEN 
          IF (L .EQ. 2 .AND. TLAPSE(L) .LT. CRTLAP) GOTO15
          DZ2(L+1) = 0.
C
          DO 17 LL=L,3,-1
          DZ2(LL) = 0.
          DELT2(LL) = 0.
          TLAPSE2(LL) = 0.
          DZ2(LL) = (2./3.)*(ZINT(I,J,LL-2)-ZINT(I,J,L+1))
          IF ((DZ2(LL) .GT. 2000.) .AND.
     1        (DZ2(LL+1) .GT. 2000.)) GO TO 15
          DELT2(LL) = T(I,J,LL-2)-T(I,J,L)
          TLAPSE2(LL) = -DELT2(LL)/DZ2(LL)
C
          IF (TLAPSE2(LL) .GT. CRTLAP) THEN
            GOTO 10
          ENDIF
C
   17     CONTINUE 
        ELSE
          GOTO 10 
        ENDIF 
C
   15   PTROP(I,J)  = D50*(PINT(I,J,L)+PINT(I,J,L+1))
        TTROP(I,J)  = T(I,J,L)
        ZTROP(I,J)= HTM(I,J,L+1)*T(I,J,L+1)*
     X               (Q(I,J,L+1)*D608+H1)*ROG*
     X               (LOG(PINT(I,J,L+1))-LOG(PTROP(I,J)))
     X               +ZINT(I,J,L+1)
C
        IE=I+IHE(J)
        IW=I+IHW(J)
        UH        = D25*(U(I,J-1,L-1)+U(IW,J,L-1)+
     X                   U(IE,J,L-1) +U(I,J+1,L-1))
        U0        = D25*(U(I,J-1,L  )+U(IW,J,L  )+
     X                   U(IE,J,L  )+U(I,J+1,L  ))
        UL        = D25*(U(I,J-1,L+1)+U(IW,J,L+1)+
     X                   U(IE,J,L+1)+U(I,J+1,L+1))
        UH0       = D50*(UH+U0)
        U0L       = D50*(U0+UL)
        VH        = D25*(V(I,J-1,L-1)+V(IW,J,L-1)+
     X                   V(IE,J,L-1)+V(I,J+1,L-1))
        V0        = D25*(V(I,J-1,L  )+V(IW,J,L  )+
     X                   V(IE,J,L  )+V(I,J+1 ,L  ))
        VL        = D25*(V(I,J-1,L+1)+V(IW,J,L+1)+
     X                   V(IE,J,L+1)+V(I,J+1,L+1))
        VH0       = D50*(VH+V0)
        V0L       = D50*(V0+VL)
        EGRIDU(I,J) = U0
        EGRIDV(I,J) = V0
        DZ        = ZINT(I,J,L)-ZINT(I,J,L+1)
        RSQDIF    = SQRT((UH0-U0L)**2+(VH0-V0L)**2)
        SHTROP(I,J) = RSQDIF/DZ
        GOTO 20
   10   CONTINUE

CX         WRITE(88,*)'REACHED TOP FOR K,P,TLAPSE:  ',K,PM,TLAPSE

        DZ       = D50*(ZINT(I,J,1)-ZINT(I,J,3))
        PTROP(I,J) = D50*(PINT(I,J,2)+PINT(I,J,3))
        TTROP(I,J) = T(I,J,2)
        ZTROP(I,J)= HTM(I,J,3)*T(I,J,3)*(Q(I,J,3)*D608+H1)*ROG*
     X          (LOG(PINT(I,J,3))-LOG(PTROP(I,J)))+ZINT(I,J,3)
        UH        = D25*(U(I,J-1,2)+U(IW,J,2)+
     X                   U(IE,J,2)+U(I,J+1,2))
        VH        = D25*(V(I,J-1,2)+V(IW,J,2)+
     X                   V(IE,J,2)+V(I,J+1,2))
        UL        = D25*(U(I,J-1,3  )+U(IW,J,3  )+
     X                   U(IE,J,3  )+U(I,J+1,3  ))
        VL        = D25*(V(I,J-1,3  )+V(IW,J,3  )+
     X                   V(IE,J,3  )+V(I,J+1,3  ))
        EGRIDU(I,J) = UH
        EGRIDV(I,J) = VH
        RSQDIF      = SQRT((UH-UL)**2+(VH-VL)**2)
        SHTROP(I,J) = RSQDIF/DZ

CX        WRITE(82,1010)I,J,L,PTROP(I,J)*D01,TTROP(I,J),
CX     X       EGRIDU(I,J),EGRIDV(I,J),SHTROP(I,J)
C     
   20 CONTINUE

C     CALCULATE U-V AT V POINTS.
      CALL H2V(EGRIDU,EGRIDV,UTROP,VTROP)

C     
C     END OF ROUTINE.
C     
      RETURN
      END
