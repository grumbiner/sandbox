      SUBROUTINE CALMXW(MXWP,MXWZ,MXWU,MXWV)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .     
C SUBPROGRAM:    CALMXW      COMPUTE MAX WIND LEVEL 
C   PRGRMMR: MANIKIN        ORG: W/NP2   DATE: 97-03-04       
C     
C ABSTRACT:  
C     THIS ROUTINE COMPUTES MAX WIND LEVEL.  AT EACH POINT,
C   IT FINDS THE MAX WIND ABOVE 500 MB AND DETERMINES THE
C   PRESSURE AND HEIGHT AT THAT LEVEL.
C     
C     
C PROGRAM HISTORY LOG:
C   97-03-04 GEOFF MANIKIN
C   98-06-15 T BLACK - CONVERSION FROM 1-D TO 2-D
C   00-01-02 JIM TUCCILLO - MPI VERSION
C     
C USAGE:    CALL  CALMXW(MXWP,MXWZ,MXWU,MXWV)
C   INPUT ARGUMENT LIST:
C     NONE     
C
C   OUTPUT ARGUMENT LIST: 
C     MXWP    - PRESSURE LEVEL OF THE MAX WIND
C     MXWZ    - HEIGHT OF THE MAX WIND
C     MXWU    - U COMPONENT OF THE ACTUAL MAX WIND 
C     MXWV    - V COMPONENT OF THE ACTUAL MAX WIND
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
C     LANGUAGE: FORTRAN 90
C     MACHINE : CRAY C-90
C$$$  
C     
C     
C     INCLUDE ETA GRID DIMENSIONS.  SET/DERIVE PARAMETERS.
C
      INCLUDE "parmeta"
      INCLUDE "params"
C
      INCLUDE "MASKS.comm"
      INCLUDE "VRBLS.comm"
      INCLUDE "OPTIONS.comm"
      INCLUDE "LOOPS.comm"
      INCLUDE "EXTRA.comm"
      INCLUDE "INDX.comm"
      INCLUDE "CTLBLK.comm"
C     
C     DECLARE VARIABLES.
C     
      REAL MXWP(IM,JM),MXWZ(IM,JM),MXWU(IM,JM),MXWV(IM,JM),MXWW 
C     
C     
C*****************************************************************************
C     START CALMXW HERE.
C     
C     LOOP OVER THE GRID.
C    
      CRITP=5.0E4
C
      DO J=JSTA,JEND
      DO I=1,IM
        MXWU(I,J) = SPVAL
        MXWV(I,J) = SPVAL
        MXWP(I,J) = SPVAL
        MXWZ(I,J) = SPVAL 
      ENDDO
      ENDDO
C
C     ASSUME THAT U AND V HAVE UPDATED HALOS
C
!$omp  parallel do
!$omp& private(ie,iw,mxww,u0,v0,wind)
      DO 20 J=JSTA_M,JEND_M
      DO 20 I=2,IM-1
      IE=I+IHE(J)
      IW=I+IHW(J)
      MXWW  = -1000.
      LLMH=LMH(I,J)
C
      DO 10 L= LLMH-1,1,-1
         U0 = D25*(U(I,J-1,L)+U(IW,J,L)+
     X             U(IE,J,L)+U(I,J+1,L))
         V0 = D25*(V(I,J-1,L)+V(IW,J,L)+
     X             V(IE,J,L)+V(I,J+1,L))
         WIND = SQRT(U0**2 + V0**2)

C  MAX WIND LEVEL MUST BE ABOVE THE 500 MB 
C    ***NOTE, HOWEVER, THAT THE CHECK FOR 500 IS WITH THE
C       INTERFACE PRESSURE.  THEREFORE, WHEN THE PRESSURE OF
C       THE ACTUAL LEVEL OF THE MAX WIND IT COMPUTED, IT MAY
C       END UP WITH A PRESSURE SLIGHTLY GREATER THAN 500

         IF (WIND .GT. MXWW .and. PINT(I,J,L) .LT. CRITP) THEN
           MXWU(I,J) = U0
           MXWV(I,J) = V0
           MXWW = WIND 
           MXWP(I,J) = (PINT(I,J,L) + PINT(I,J,L+1)) * 0.5
           MXWZ(I,J)=HTM(I,J,L+1)*T(I,J,L+1)*(Q(I,J,L+1)*D608+H1)*ROG*
     X               (LOG(PINT(I,J,L+1))-LOG(MXWP(I,J)))+ZINT(I,J,L+1)
         ENDIF
   10 CONTINUE
   20 CONTINUE

C     END OF ROUTINE.
C     
      RETURN
      END
