C
      SUBROUTINE INTSFC(HLAT,HLON,SST,SNO,SGICE,SSTE,snoe,SM)
C
C SUBPROGRAM: INTSFC         READS SURFACE GRIDS FROM GES FILE
C                            AND INTERPOLATES TO THE ETA GRID
C   PRGMMR: E.ROGERS         ORG: W/NMC22    DATE: 90-07-10
C
C ABSTRACT: READS THE SURFACE FILE PRODUCED BY THE GLOBAL
C           SYSTEM (ON LAT/LONG GRID FROM FRONT END JOB SFCINIGB)
C           AND INTERPOLATES THE FIELDS NEEDED BY THE ETA MODEL
C           (SEA SURFACE TEMPERATURE, SNOW DEPTH, SOIL MOISTURE)
C           TO THE ETA GRID.
C
C SUBPROGRAM HISTORY LOG:
C   90-08-18  E ROGERS
C   98-11-30  E ROGERS : REANALYSIS VERSION NEEDS TO PROCESS SEA ICE
C             ASSUMES ICE IS ON 0.5 DEG LAT/LON GRID
C
C USAGE     CALL INTSFC(HLAT,HLON,SST,SNO,SOIL,wet,SSTE,snoe)
C   INPUT ARGUMENT LIST:
C     HLAT     - LATITUDE OF E-GRID H POINTS
C     HLON     - LONGITUDE OF E-GRID H POINTS
C     SST      - SST FIELD ON LAT /LON GRID
C     SNO      - SNOW MASK ON LAT /LON GRID
C     SGICE    - SEA ICE ON LAT/LON GRID
C
C   OUTPUT ARGUMENT LIST:
C     SSTE     - SEA SURFACE TEMPERATURE INTERPOLATED TO E-GRID
C     snoe     - SNOW (1) OR NO SNOW (0) MASK
C
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE: CRAY
C
C-----------------------------------------------------------------------
C
C
           include "parmeta.h"
C-----------------------------------------------------------------------
                            D I M E N S I O N
     &  SST(360,180),SNO(181,91),SGICE(720,360)
     &, HLAT(IM,JM),HLON(IM,JM),SICEE(IM,JM),SM(IM,JM)
     &, SSTE(IM,JM),SNOE(IM,JM),OLSNOE(IM,JM)
C
C--------------LOGICAL FILE NAMES---------------------------------------
                             D A T A
     &  LIST  /06/
C----------------------------------------------------------------------
      DO J = 1, JM
      DO I = 1, IM
C----------------------------------------------------------------------
C---------OBTAIN CLOSEST ANALYSIS GRID POINT TO ETA GRID POINT --------
C----------------------------------------------------------------------
        xetah = hlon(i,j) + 0.5
        yetah = 90.5 - hlat(i,j) 
        IETAH = INT(XETAH)
        JETAH = INT(YETAH)
C-----------------------------------------------------------------------
C------------COMPUTE THE WEIGHTING FACTORS FOR THE INTERPOLATION--------
C-----------------------------------------------------------------------
C------------------ WEIGHTING FACTORS FOR H POINTS----------------------
C-----------------------------------------------------------------------
C  do sst (1 deg lat/lon)
c
        XRH = xetah - ietah
        YRH = yetah - jetah
        W1H = (1.0 - XRH) * (1.0 - YRH)
        W2H = XRH * (1.0 - YRH)
        W3H = (1.0 - XRH) * YRH
        W4H = XRH * YRH
        ietahp1 = ietah + 1
        if(ietahp1.gt.360) ietahp1 = 1
C
C do snow (2 deg lat/lon)
C
        xetas = 0.5 * hlon(i,j) + 1.0
        yetas = 0.5 *(90.0 - hlat(i,j)) + 1
        IETAs = INT(XETAs)
        JETAs = INT(YETAs)
        XRs = xetas - ietas
        YRs = yetas - jetas
        W1s = (1.0 - XRs) * (1.0 - YRs)
        W2s = XRs * (1.0 - YRs)
        W3s = (1.0 - XRs) * YRs
        W4s = XRs * YRs
        ietasp1 = ietas + 1
        if(ietasp1.gt.180) ietasp1 = 1
c
C do ice (0.5 deg lat/lon)
c
        xetai = (hlon(i,j) - 0.25) / 0.5 + 1
        yetai = (89.75 - hlat(i,j)) / 0.5 + 1
        IETAi = INT(XETAi)
        JETAi = INT(YETAi)
        XRi = xetai - ietai
        YRi = yetai - jetai
        W1i = (1.0 - XRi) * (1.0 - YRi)
        W2i = XRi * (1.0 - YRi)
        W3i = (1.0 - XRi) * YRi
        W4i = XRi * YRi
        ietaip1 = ietai + 1
        if(ietaip1.gt.720) ietaip1 = 1
C-----------------------------------------------------------------------
C------------------ PERFORM BILINEAR INTERPOLATION----------------------
C-----------------------------------------------------------------------
        SSTE(I,J) = (W1H * SST(IETAH,JETAH)
     1            +  W2H * SST(ietahp1,JETAH)
     2            +  W3H * SST(IETAH,JETAH+1)
     3            +  W4H * SST(IETAHp1,JETAH+1))
        SNOE(I,J)  = (W1s * SNO(IETAs,JETAs)
     1              +  W2s * SNO(IETAsp1,JETAs)
     2              +  W3s * SNO(IETAs,JETAs+1)
     3              +  W4s * SNO(IETAsp1,JETAs+1))
        SICEE(I,J)  = (W1i * SGICE(IETAi,JETAi)
     1              +  W2i * SGICE(IETAip1,JETAi)
     2              +  W3i * SGICE(IETAi,JETAi+1)
     3              +  W4i * SGICE(IETAip1,JETAi+1))

C-----------------------------------------------------------------------
C---------CONVERT SNOW DEPTH ON ETA GRID INTO SNOW/NO SNOW MASK---------
C-----------------------------------------------------------------------
        IF(SNOE(I,J) .LT. 0.45) THEN
          snoe(I,J) = 0.0
        ELSE
          snoe(I,J) = 1.0
        END IF
C
C   IF INTERPOLATED ICE CONCENTRATION > 0.5 ASSUME WE HAVE SEA ICE AND RESET
C   SNOE = 1.0
C
        IF(SICEE(I,J) .GE. 0.5 .AND. SM(I,J) .GT. 0.9) THEN
          OLSNOE (I,J) = SNOE(I,J)
          SNOE(I,J) = 1.0
          print *,' we have sea ice at i,j = ',i,j,sicee(i,j),
     1      olsnoe(i,j),snoe(i,j),hlat(i,j),hlon(i,j),sm(i,j)
        ENDIF
C
        IF(MOD(I,25).EQ.0 .AND. MOD(J,25).EQ.0) THEN
         WRITE(6,100) I,J,HLAT(I,J),HLON(I,J),SSTE(I,J),SNOE(I,J),
     1    OLSNOE(I,J),SICEE(I,J),SM(I,J)
 100     FORMAT(2X,2I6,7(E12.5,1X))
        END IF
      ENDDO
      ENDDO
C
      CALL PRINTETA(SNOE,SM)
      RETURN
      END
