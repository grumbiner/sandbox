C
      SUBROUTINE INTERP(NR,HLAT,HLON,VLAT,VLON,LTOTAL,REF)
c     SUBROUTINE INTERP(NR,ARRAY,HLAT,HLON,VLAT,VLON,ARRAYE,LTOTAL,REF)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM: INTERP        GENERAL INTERPOLATOR FOR LAT/LONG GRID TO
C                           ETA GRID
C   PRGMMR: ROGERS          ORG: W/NMC22    DATE: 90-06-13
C
C ABSTRACT: INTERPOLATES A GIVEN LAT/LONG GRID ONTO THE ETA GRID
C
C PROGRAM HISTORY LOG:
C   90-06-13  E.ROGERS
C   98-06-08  M.BALDWIN - CONVERT TO 2-D CODE
C
C USAGE     CALL INTERP(NR,ARRAY,HLAT,HLON,VLAT,VLON,ARRAYE)
C   INPUT ARGUMENT LIST:
C     NR       - VARIABLE COUNTER
C     ARRAY    - INPUT LAT/LONG ARRAY
C     HLAT     - LATITUDE OF HEIGHT POINTS ON ETA GRID
C     HLON     - LONGITUDE OF HEIGHT POINTS ON ETA GRID
C     VLAT     - LATITUDE OF WIND POINTS ON ETA GRID
C     VLON     - LONGITUDE OF WIND POINTS ON ETA GRID
c     LTOTAL   - NUMBER OF LEVELS TO PERFORM THE INTERPOLATION UPON
C
C   OUTPUT ARGUMENT LIST:
C     ARRAYE   - OUTPUT ARRAY ON ETA GRID
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  CRAY
C
C-----------------------------------------------------------------------
      INCLUDE "parmeta.h"
C-----------------------------------------------------------------------
                            D I M E N S I O N
     &  REF(IM,JM)
c    &  ARRAY(IMAX,JMAX,LMAX),ARRAYE(IM,JM,LMAX),REF(IM,JM)
     &, HLAT(IM,JM),HLON(IM,JM),VLAT(IM,JM),VLON(IM,JM)
          COMMON /GRIDS/
     & PSTAR(IMAX,JMAX),ZSTAR(IMAX,JMAX),TSTAR(IMAX,JMAX)
     &,ETAREF(IMAX,JMAX),ERI1(IMAX,JMAX)
     &,DETA(LM),ETA(LMP1),ETAL(LM),ZETA(LMP1)
       LOGICAL*1 L12,L13,L14,L23,L24,L34,L1,L2,L3,L4
C
       COMMON /BIGGBL/ AL(IMAX,JMAX,LMAX),ARRAYE(IM,JM,LMAX)
C
C--------------LOGICAL FILE NAMES---------------------------------------
                             D A T A
     &  LIST  /06/
C----------------------------------------------------------------------
C----------------------------------------------------------------------
       IUSE4 = 0
       IUSET = 0
       IUSE3 = 0
       IUSE2 = 0
       IUSE1 = 0
       IUSEONE = 0
C
      DO 220 L = 1,LTOTAL
C
      DO J=1,JM
      DO I=1,IM
       L12 = .FALSE.
       L13 = .FALSE.
       L14 = .FALSE.
       L23 = .FALSE.
       L24 = .FALSE.
       L34 = .FALSE.
       L1 = .FALSE.
       L2 = .FALSE.
       L3 = .FALSE.
       L4 = .FALSE.
C----------------------------------------------------------------------
C---------SUBROUTINE LL2IJ REQUIRES LONGITUDE IN DEGREES W-------------
C----------------------------------------------------------------------
        WLONH = 360.0 - HLON(I,J)
c       if(i.eq.119.and.j.eq.387) then
c        print*,'hlon(119,387)=',hlon(i,j)
c       endif
         IF(XLONW.LE.180.0.AND.WLONH.GT.180.0) WLONH = WLONH - 360.0
        WLONV = 360.0 - VLON(I,J)
         IF(XLONW.LE.180.0.AND.WLONV.GT.180.0) WLONV = WLONV - 360.0
C----------------------------------------------------------------------
C---------OBTAIN CLOSEST ANALYSIS GRID POINT TO ETA GRID POINT --------
C----------------------------------------------------------------------
c       CALL LL2IJ(HLAT(I,J),WLONH,XETAH,YETAH)
c       CALL LL2IJ(VLAT(I,J),WLONV,XETAV,YETAV)
c       if(i.eq.119.and.j.eq.387) then
c        print*,'xlonw,wlonh,dlon=',xlonw,wlonh,dlon
c       endif
        XETAH = (XLONW - WLONH) / DLON + 1.0
        YETAH = (HLAT(I,J) - YLATS) / DLAT + 1.0
        XETAV = (XLONW - WLONV) / DLON + 1.0
        YETAV = (VLAT(I,J) - YLATS) / DLAT + 1.0
        IETAH = INT(XETAH)
        JETAH = INT(YETAH)
        IETAV = INT(XETAV)
        JETAV = INT(YETAV)
        IF(NR.EQ.3.OR.NR.EQ.4) THEN
         IF(IETAV.LT.1.OR.JETAV.LT.1) THEN
c           print *,"ietav,jetav,wlonv,vlat(i,j),i,j  ",ietav,jetav
c    &               ,wlonv,vlat(i,j),i,j
            IETAV = MAX(IETAV,1)
            JETAV = MAX(JETAV,1)
         END IF
         IF(IETAV.GT.IMAX-1.OR.JETAV.GT.JMAX-1) THEN
c           print *,"ietav,jetav,wlonv,vlat(i,j),i,j  ",ietav,jetav
c    &               ,wlonv,vlat(i,j),i,j
            IETAV = MIN(IETAV,IMAX-1)
            JETAV = MIN(JETAV,JMAX-1)
         END IF
        ELSE
         IF(IETAH.LT.1.OR.JETAH.LT.1) THEN
c           print *,"ietah,jetah,wlonh,hlat(i,j),i,j  ",ietah,jetah
c    &               ,wlonh,hlat(i,j),i,j
            IETAH = MAX(IETAH,1)
            JETAH = MAX(JETAH,1)
         END IF
         IF(IETAH.GT.IMAX-1.OR.JETAH.GT.JMAX-1) THEN
c           print *,"ietah,jetah,wlonh,hlat(i,j),i,j  ",ietah,jetah
c    &               ,wlonh,hlat(i,j),i,j
            IETAH = MIN(IETAH,IMAX-1)
            JETAH = MIN(JETAH,JMAX-1)
         END IF
        END IF
c       RIETAH = FLOAT(IETAH)
c       RJETAH = FLOAT(JETAH)
c       RIETAV = FLOAT(IETAV)
c       RJETAV = FLOAT(JETAV)
c       CALL IJ2LL(RIETAH,RJETAH,XNLTAH,ELONAH)
c       CALL IJ2LL(RIETAV,RJETAV,XNLTAV,ELONAV)
C-----------------------------------------------------------------------
C------------COMPUTE THE WEIGHTING FACTORS FOR THE INTERPOLATION--------
C-----------------------------------------------------------------------
C------------------ WEIGHTING FACTORS FOR H POINTS----------------------
C-----------------------------------------------------------------------
      XRH = XETAH - IETAH
      YRH = YETAH - JETAH
c     if(ietah.eq.439.and.jetah.eq.189) then
c      print*,'xetah,yetah,xrh,yrh=',xetah,yetah,xrh,yrh
c     endif
      W1H = (1.0 - XRH) * (1.0 - YRH)
      W2H = XRH * (1.0 - YRH)
      W3H = (1.0 - XRH) * YRH
      W4H = XRH * YRH
C-----------------------------------------------------------------------
C------------------ WEIGHTING FACTORS FOR V POINTS----------------------
C-----------------------------------------------------------------------
      XRV = XETAV - IETAV
      YRV = YETAV - JETAV
      W1V = (1.0 - XRV) * (1.0 - YRV)
      W2V = XRV * (1.0 - YRV)
      W3V = (1.0 - XRV) * YRV
      W4V = XRV * YRV
C-----------------------------------------------------------------------
C------------------ PERFORM BILINEAR INTERPOLATION----------------------
C-----------------------------------------------------------------------
C
        IF(NR.EQ.3.OR.NR.EQ.4) THEN
          ARRAYE(I,J,L) = (W1V * AL(IETAV,JETAV,L)
     1                +  W2V * AL(IETAV+1,JETAV,L)
     2                +  W3V * AL(IETAV,JETAV+1,L)
     3                +  W4V * AL(IETAV+1,JETAV+1,L))
C
         ELSE
C
          IF(LTOTAL.NE.LMAX+5) THEN
             IF(REF(I,J).EQ.ETAREF(IETAH,JETAH)) L1 = .TRUE.
             IF(REF(I,J).EQ.ETAREF(IETAH+1,JETAH)) L2 = .TRUE.
             IF(REF(I,J).EQ.ETAREF(IETAH,JETAH+1)) L3 = .TRUE.
             IF(REF(I,J).EQ.ETAREF(IETAH+1,JETAH+1)) L4 = .TRUE.
             IF(.NOT.L1.OR..NOT.L2.OR..NOT.L3.OR..NOT.L4) THEN
                IUSET = IUSET + 1
                IF(.NOT.L1) W1H = 0.0
                IF(.NOT.L2) W2H = 0.0
                IF(.NOT.L3) W3H = 0.0
                IF(.NOT.L4) W4H = 0.0
                SSS = W1H + W2H + W3H + W4H
                IF (SSS.EQ.0.0) THEN
                     SSS = 1.0
                     W1H = 1.0
                     IUSEONE = IUSEONE + 1
c              write(6,*)"one only at i,j,i,j ",ietah,jetah,i,j
                END IF
                W1H = W1H / SSS
                W2H = W2H / SSS
                W3H = W3H / SSS
                W4H = W4H / SSS
                IF(W1H.NE.0.0) IUSE1 = IUSE1 + 1
                IF(W2H.NE.0.0) IUSE2 = IUSE2 + 1
                IF(W3H.NE.0.0) IUSE3 = IUSE3 + 1
                IF(W4H.NE.0.0) IUSE4 = IUSE4 + 1
          END IF
          END IF
          ARRAYE(I,J,L) = (W1H * AL(IETAH,JETAH,L)
     1                +  W2H * AL(IETAH+1,JETAH,L)
     2                +  W3H * AL(IETAH,JETAH+1,L)
     3                +  W4H * AL(IETAH+1,JETAH+1,L))
c       if(i.eq.119.and.j.eq.387) then
c        print*,'l,w1h,w2h,w3h,w4h=',l,w1h,w2h,w3h,w4h
c        print*,'ietah,jetah=',ietah,jetah
c        print*,'AL(IETAH,JETAH,L)=',AL(IETAH,JETAH,L)
c        print*,'AL(IETAH+1,JETAH,L)=',AL(IETAH+1,JETAH,L)
c        print*,'AL(IETAH,JETAH+1,L)=',AL(IETAH,JETAH+1,L)
c        print*,'AL(IETAH+1,JETAH+1,L)=',AL(IETAH+1,JETAH+1,L)
c       endif
        END IF
      ENDDO
      ENDDO
 220  CONTINUE
c       print *,"use1,iuse2,iuse3,iuse4 ",iuse1,iuse2,iuse3,iuse4
c       print *,"total points is ",iuset
c       print *," one only points is ",iuseone
      RETURN
      END
