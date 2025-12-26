      SUBROUTINE FDLVL(ITYPE,TFD,UFD,VFD)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .     
C SUBPROGRAM:    FDLVL       COMPUTES FD LEVEL T, U, V
C   PRGRMMR: TREADON         ORG: W/NP2      DATE: 92-12-22       
C     
C ABSTRACT:
C     THIS ROUTINE COMPUTES TEMPERATURE, U WIND COMPONENT,
C     AND V WIND COMPONENT ON THE NFD=6 FD LEVELS.  THE
C     HEIGHT OF THESE LEVELS (IN METERS) IS GIVEN IN THE 
C     DATA STATEMENT BELOW.  THE ALGORITHM PROCEEDS AS 
C     FOLLOWS. (AGL IN PARENTHESES)
C     
C     AT EACH MASS POINT MOVE UP VERTICALLY FROM THE LM-TH (LOWEST
C     ATMOSPHERIC) ETA LAYER.  FIND THE ETA LAYERS WHOSE 
C     HEIGHT (ABOVE GROUND) BOUNDS THE TARGET FD LEVEL HEIGHT.
C     VERTICALLY INTERPOLATE TO GET TEMPERATURE AT THIS FD
C     LEVEL.  AVERAGE THE FOUR SURROUNDING WINDS
C     TO GET A MASS POINT WIND.  VERTICALLY INTERPOLATE THESE
C     MASS POINT WINDS TO THE TARGET FD LEVEL.  CONTINUE THIS
C     PROCESS UNTIL ALL NFD=6 FD LEVELS HAVE BEEN PROCESSED.
C     MOVE ON TO THE NEXT MASS POINT.  
C     
C     AVERAGING THE FOUR ABOVE GROUND WINDS TO THE MASS POINT
C     WAS FOUND TO SMOOTH THE FIELD AND REDUCE THE OCCURRENCE
C     OF POINT PEAK WINDS FAR IN EXCESS OF THE WINDS AT 
C     ADJACENT POINTS.
C   .     
C     
C PROGRAM HISTORY LOG:
C   92-12-22  RUSS TREADON
C   93-11-23  RUSS TREADON - CORRECTED ROUTINE TO COMPUTE
C             FD LEVELS WITH REPECT TO MEAN SEA LEVEL.
C   94-01-04  MICHAEL BALDWIN - INCLUDE OPTIONS FOR COMPUTING
C                               EITHER AGL OR MSL
C   98-06-15  T BLACK - CONVERSION FROM 1-D TO 2-D
C   00-01-04  JIM TUCCILLO - MPI VERSION            
C     
C USAGE:    CALL FDLVL(ITYPE,TFD,UFD,VFD)
C   INPUT ARGUMENT LIST:
C     ITYPE    - FLAG THAT DETERMINES WHETHER MSL (1) OR AGL (2)
C                   LEVELS ARE USED.
C
C   OUTPUT ARGUMENT LIST: 
C     TFD      - TEMPERATURE (K) ON FD LEVELS.
C     UFD      - U WIND (M/S) ON FD LEVELS.
C     VFD      - V WIND (M/S) ON FD LEVELS.
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
C                  MASKS
C                  OPTIONS
C                  INDX
C     
C   ATTRIBUTES:
C     LANGUAGE: FORTRAN
C     MACHINE : CRAY C-90
C$$$  
C     
C
C
C     
C     SET NUMBER OF FD LEVELS.
      PARAMETER (NFD=6)
C     
C     INCLUDE PARAMETERS.
      INCLUDE "parmeta"
      INCLUDE "params"
C     
C     DECLARE VARIABLES
C     
      INTEGER LVL(NFD)
      REAL DZABV(NFD), HTFD(NFD)
      REAL TFD(IM,JM,NFD),UFD(IM,JM,NFD)
      REAL VFD(IM,JM,NFD),EGRIDU(IM,JM,NFD),EGRIDV(IM,JM,NFD)
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
C     SET FD LEVEL HEIGHTS IN METERS.
      DATA HTFD  / 914.E0,1524.E0,1829.E0,
     X     2134.E0,2743.E0,3658.E0/
C     
C****************************************************************
C     START FDLVL HERE
C     
C     INITIALIZE ARRAYS.
C     
!$omp  parallel do
      DO 10 IFD = 1,NFD
      DO J=JSTA,JEND
      DO I=1,IM
         TFD(I,J,IFD)    = SPVAL
         UFD(I,J,IFD)    = SPVAL
         VFD(I,J,IFD)    = SPVAL
         EGRIDU(I,J,IFD) = D00
         EGRIDV(I,J,IFD) = D00
      ENDDO
      ENDDO
 10   CONTINUE
C
C     MSL FD LEVELS
C
      IF (ITYPE.EQ.1) THEN
C     
C     LOOP OVER HORIZONTAL GRID.
C
C     ASSUME THAT U, V, T, AND LMV HAVE UPDATED HALOS
C     
      DO 50 J=JSTA_M,JEND_M
      DO 50 I=2,IM-1
         IFD = 1
C     
C        LOCATE VERTICAL INDICES OF T,U,V, LEVEL JUST 
C        ABOVE EACH FD LEVEL.
C
         DO 20 L = LM,1,-1
            HTTUV = D50*(ZINT(I,J,L)+ZINT(I,J,L+1))
            IF (HTTUV.GT.HTFD(IFD)) THEN
               LVL(IFD)   = L
               DZABV(IFD) = HTTUV-HTFD(IFD)
               IFD        = IFD + 1
               IF (IFD.GT.NFD) GOTO 30
            ENDIF
 20         CONTINUE
C     
C        COMPUTE T, U, AND V AT FD LEVELS.
C
 30      CONTINUE
C
         IE=I+IHE(J)         
         IW=I+IHW(J)         
         DO 40 IFD = 1,NFD
            L = LVL(IFD)
            IF (L.LT.LM) THEN
               DZ   = D50*(ZINT(I,J,L)-ZINT(I,J,L+2))
               RDZ  = 1./DZ
               DELT = T(I,J,L)-T(I,J,L+1)
               UH=0.25*(U(IE,J,L)+U(IW,J,L)
     1                 +U(I,J+1,L)+U(I,J-1,L))
               VH=0.25*(V(IE,J,L)+V(IW,J,L)
     1                 +V(I,J+1,L)+V(I,J-1,L))
               UL=0.25*(U(IE,J,L+1)+U(IW,J,L+1)
     1                 +U(I,J+1,L+1)+U(I,J-1,L+1))
               VL=0.25*(V(IE,J,L+1)+V(IW,J,L+1)
     1                 +V(I,J+1,L+1)+V(I,J-1,L+1))
C
               DELU = UH - UL
               DELV = VH - VL
               TFD(I,J,IFD) = T(I,J,L) - DELT*RDZ*DZABV(IFD)
               EGRIDU(I,J,IFD) = UH  - DELU*RDZ*DZABV(IFD)
               EGRIDV(I,J,IFD) = VH  - DELV*RDZ*DZABV(IFD)
            ELSE
               TFD(I,J,IFD) = T(I,J,L)
               EGRIDU(I,J,IFD)=0.25*(U(IE,J,L)+U(IW,J,L)
     1                              +U(I,J+1,L)+U(I,J-1,L))
               EGRIDV(I,J,IFD)=0.25*(V(IE,J,L)+V(IW,J,L)
     1                              +V(I,J+1,L)+V(I,J-1,L))
            ENDIF
 40      CONTINUE
C     
C     COMPUTE FD LEVEL T, U, AND V AT NEXT K.
C
 50   CONTINUE
C     END OF MSL FD LEVELS
      ELSE
C
C     AGL FD LEVELS 
C
C     
C     LOOP OVER HORIZONTAL GRID.
C     
      DO 250 J=JSTA_M,JEND_M
      DO 250 I=2,IM-1
         HTSFC = FIS(I,J)*GI
         LLMH  = LMH(I,J)
         IFD   = 1
C     
C        LOCATE VERTICAL INDICES OF T,U,V, LEVEL JUST 
C        ABOVE EACH FD LEVEL.
C
         DO 220 L = LLMH,1,-1
            HTTUV = D50*(ZINT(I,J,L)+ZINT(I,J,L+1))
            HTABV = HTTUV-HTSFC
            IF (HTABV.GT.HTFD(IFD)) THEN
               LVL(IFD)   = L
               DZABV(IFD) = HTABV-HTFD(IFD)
               IFD        = IFD + 1
               IF (IFD.GT.NFD) GOTO 230
            ENDIF
 220        CONTINUE
C     
C        COMPUTE T, U, AND V AT FD LEVELS.
C
 230     CONTINUE
C
         IE=I+IHE(J)       
         IW=I+IHW(J)       
         DO 240 IFD = 1,NFD
            L = LVL(IFD)
            IF (L.LT.LM) THEN
               DZ   = D50*(ZINT(I,J,L)-ZINT(I,J,L+2))
               RDZ  = 1./DZ
               DELT = T(I,J,L)-T(I,J,L+1)
C
               LOFF = L-LLMH
               UH=0.25*(U(IE,J,LMV(IE,J)+LOFF)+U(IW,J,LMV(IW,J)+LOFF)
     1             +U(I,J+1,LMV(I,J+1)+LOFF)+U(I,J-1,LMV(I,J-1)+LOFF))
               VH=0.25*(V(IE,J,LMV(IE,J)+LOFF)+V(IW,J,LMV(IW,J)+LOFF)
     1             +V(I,J+1,LMV(I,J+1)+LOFF)+V(I,J-1,LMV(I,J-1)+LOFF))
               
               LOFF   = L+1-LLMH
               UL=0.25*(U(IW,J,LMV(IW,J)+LOFF)+U(IE,J,LMV(IE,J)+LOFF)
     1             +U(I,J+1,LMV(I,J+1)+LOFF)+U(I,J-1,LMV(I,J-1)+LOFF))
               VL=0.25*(V(IW,J,LMV(IW,J)+LOFF)+V(IE,J,LMV(IE,J)+LOFF)
     1             +V(I,J+1,LMV(I,J+1)+LOFF)+V(I,J-1,LMV(I,J-1)+LOFF))
C
               DELU = UH - UL
               DELV = VH - VL
               TFD(I,J,IFD) = T(I,J,L) - DELT*RDZ*DZABV(IFD)
               EGRIDU(I,J,IFD) = UH  - DELU*RDZ*DZABV(IFD)
               EGRIDV(I,J,IFD) = VH  - DELV*RDZ*DZABV(IFD)
            ELSE
               TFD(I,J,IFD) = T(I,J,L)
               LOFF   = L-LLMH
               UH=0.25*(U(IE,J,LMV(IE,J)+LOFF)+U(IW,J,LMV(IW,J)+LOFF)
     1             +U(I,J+1,LMV(I,J+1)+LOFF)+U(I,J-1,LMV(I,J-1)+LOFF))
               VH=0.25*(V(IE,J,LMV(IE,J)+LOFF)+V(IW,J,LMV(IW,J)+LOFF)
     1             +V(I,J+1,LMV(I,J+1)+LOFF)+V(I,J-1,LMV(I,J-1)+LOFF))
               EGRIDU(I,J,IFD) = UH
               EGRIDV(I,J,IFD) = VH
            ENDIF
 240     CONTINUE
C     
C     COMPUTE FD LEVEL T, U, AND V AT NEXT K.
C
 250  CONTINUE
C     END OF AGL FD LEVELS
      ENDIF
C
C     LOOP TO COMPUTE U-V AT VELOCITY POINTS.
C
      DO IFD = 1,NFD
         CALL H2V(EGRIDU(1,1,IFD),EGRIDV(1,1,IFD),
     X        UFD(1,1,IFD),VFD(1,1,IFD))
      END DO
C
C     END OF ROUTINE.
C
      RETURN
      END
