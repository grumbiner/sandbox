      SUBROUTINE ETALL(HLAT,HLON,VLAT,VLON)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM: ETALL         COMPUTE EARTH LATITUDE & LONIGTUDE OF
C                           ETA GRID POINTS
C   PRGMMR: ROGERS          ORG: W/NMC22    DATE: 90-06-13
C
C ABSTRACT: COMPUTES THE EARTH LATITUDE AND LONGITUDE OF ETA GRID
C   POINTS (BOTH H AND V POINTS)
C
C PROGRAM HISTORY LOG:
C   90-06-13  E.ROGERS
C   98-06-09  M.BALDWIN - CONVERT TO 2-D CODE
C
C USAGE:    CALL ETALL(HLAT,HLON,VLAT,VLON)
C   INPUT ARGUMENT LIST:
C     NONE
C
C   OUTPUT ARGUMENT LIST:
C     HLAT     - LATITUDE OF H GRID POINTS IN RADIANS (NEG=S)
C     HLON     - LONGITUDE OF H GRID POINTS IN RADIANS (E)
C     VLAT     - LATITUDE OF V GRID POINTS IN RADIANS (NEG=S)
C     VLON     - LONGITUDE OF V GRID POINTS IN RADIANS (E)
C
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  CRAY
C-----------------------------------------------------------------------
      INCLUDE "parmeta.h"
C--------------HORIZONTAL GRID CONSTANTS - in griddef include file -----
      INCLUDE "griddef"
C-----------------------------------------------------------------------
                             D I M E N S I O N
     & IDAT  (3)
C
     &,GLATH (IM,JM),GLONH (IM,JM),GLATV (IM,JM),GLONV (IM,JM)
C
     &,HLAT (IM,JM),HLON (IM,JM),VLAT (IM,JM),VLON (IM,JM)
C--------------LOGICAL FILE NAMES---------------------------------------
                             D A T A
     &  LIST  /06/
C--------------UNIVERSAL CONSTANTS--------------------------------------
                             D A T A
     & PI/3.141592654/
C-----------------------------------------------------------------------
 1000 FORMAT(' I J=',2I4,' GLAT=',E12.5,' GLON=',E12.5,' ELAT=',E12.5
     &,' ELON=',E12.5,' WLON = ',E12.5)
C----------------------------------------------------------------------
C--------------DERIVED GEOMETRICAL CONSTANTS----------------------------
C----------------------------------------------------------------------
      DTR = PI / 180.0
      TPH0 = TPH0D * DTR
      WB = WBD * DTR
      SB = SBD * DTR
      DLM = DLMD * DTR
      DPH = DPHD * DTR
      TDLM = DLM + DLM
      TDPH = DPH + DPH
C
      STPH0 = SIN(TPH0)
      CTPH0 = COS(TPH0)
C
C-----------------------------------------------------------------------
C---COMPUTE GEOGRAPHIC LAT AND LONG OF ETA GRID POINTS (H & V POINTS)---
C-----------------------------------------------------------------------
      TPHH = SB - DPH
      DO 200 J = 1, JM
C
         TLMH = WB - TDLM + MOD(J+1,2) * DLM
         TPHH = TPHH + DPH
         TLMV = WB - TDLM + MOD(J,2) * DLM
         TPHV = TPHH
         STPH = SIN(TPHH)
         CTPH = COS(TPHH)
         STPV = SIN(TPHV)
         CTPV = COS(TPHV)
C----------------------------------------------------------------------
C---------- COMPUTE EARTH LATITUDE/LONGITUDE OF H POINTS --------------
C----------------------------------------------------------------------
         DO 201 I = 1,IM
c          tlmhold=tlmh
           TLMH = TLMH + TDLM
c          if(j.eq.287) then
c           print*,'i,j=',i,j
c           print*,'tlmh,tlmhold,tdlm=',tlmh,tlmhold,tdlm
c          endif
           SPHH = CTPH0 * STPH + STPH0 * CTPH * COS(TLMH)
           if(sphh.ge.1.0) sphh=0.99999
           GLATH(I,J) = ASIN(SPHH)
c          if(i.eq.119.and.j.eq.387) then
c          if(j.eq.287) then
c           print*,'i,j=',i,j
c           print*,'tdlm=',tdlm
c           print*,'tlmh,cos(tlmh),tlmhold=',tlmh,cos(tlmh),tlmhold
c           print*,'ctph0,stph,stph0,ctph=',
c    *        ctph0,stph,stph0,ctph
c           print*,'sphh=',sphh
c          endif
           CLMH = CTPH * COS(TLMH) / (COS(GLATH(I,J)) * CTPH0)
     1               - TAN(GLATH(I,J)) * TAN(TPH0)
c          if(i.eq.119.and.j.eq.387) then
c           print*,'ctph=',ctph
c           print*,'tlmh,cos(tlmh)=',tlmh,cos(tlmh)
c           print*,'glath(i,j),cos(glath(i,j))=',
c    *       glath(i,j),cos(glath(i,j))
c           print*,'ctph0=',ctph0
c           print*,'tan(glath(i,j))=',tan(glath(i,j))
c           print*,'tph0,tan(tph0)=',tph0,tan(tph0)
c           print*,'stph,stph0,ctph=',stph,stph0,ctph
c           print*,'sphh,asin(sphh)=',sphh,asin(sphh)
c          endif
           IF(CLMH .GT. 1.) CLMH = 1.0
           IF(CLMH .LT. -1.) CLMH = -1.0
           FACTH = 1.
           IF(TLMH .GT. 0.) FACTH = -1.
C          WRITE(6,88888) I,J, CLMH
C8888      FORMAT(2X,2I6,1X,E12.5)
           GLONH(I,J) = -TLM0D * DTR + FACTH * ACOS(CLMH)
c          if(i.eq.119.and.j.eq.387) then
c            print*,'tlm0d,dtr,facth,clmh,acos(clmh)=',
c    *        tlm0d,dtr,facth,clmh,acos(clmh)
c          endif
C          IF(I .EQ. 1) THEN
C           WRITE(LIST,99995) I,J,GLATH(I,J),GLONH(I,J)
C9995       FORMAT(2X,2(I6,1X),2(E12.5,1X))
C          END IF
C
C    CONVERT INTO DEGREES AND EAST LONGITUDE
C
           HLAT(I,J) = GLATH(I,J) / DTR
           HLON(I,J) = 360.0 - GLONH(I,J) / DTR
c          if(i.eq.119.and.j.eq.387) then
c           print*,'glonh(119,387)=',glonh(119,387)
c          endif
           IF(HLON(I,J) .GT. 360.) HLON(I,J) = HLON(I,J) - 360.
  201    CONTINUE
C----------------------------------------------------------------------
C---------- COMPUTE EARTH LATITUDE/LONGITUDE OF V POINTS --------------
C----------------------------------------------------------------------
         DO 202 I = 1,IM
           TLMV = TLMV + TDLM
           SPHV = CTPH0 * STPV + STPH0 * CTPV * COS(TLMV)
           GLATV(I,J) = ASIN(SPHV)
           CLMV = CTPV * COS(TLMV) / (COS(GLATV(I,J)) * CTPH0)
     1          - TAN(GLATV(I,J)) * TAN(TPH0)
           IF(CLMV .GT. 1.) CLMV = 1.
           IF(CLMV .LT. -1.) CLMV = -1.
           FACTV = 1.
           IF(TLMV .GT. 0.) FACTV = -1.
           GLONV(I,J) = -TLM0D * DTR + FACTV * ACOS(CLMV)
C          IF(I.EQ.1) THEN
C           WRITE(LIST,99995) I,J,GLATV(I,J),GLONV(I,J)
C          END IF
C
C    CONVERT INTO DEGREES AND EAST LONGITUDE
C
           VLAT(I,J) = GLATV(I,J) / DTR
           VLON(I,J) = 360.0 - GLONV(I,J) / DTR
           IF(VLON(I,J) .GT. 360.) VLON(I,J) = VLON(I,J) - 360.
  202    CONTINUE
  200 CONTINUE
C
C     DO 210 J = 1,JM
c        WRITE(LIST,88888) J,HLAT(1,J),HLON(1,J),VLAT(1,J),VLON(1,J)
C8888    FORMAT(2X,I5,1X,4(E12.5,1X))
C      END IF
C 210 CONTINUE
      RETURN
      END
