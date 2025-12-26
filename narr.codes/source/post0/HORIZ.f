      SUBROUTINE HORIZ(KBIMX,NBCEX,L
     1,                IMI,JMI,TPH0DO,TLM0DO,TPH0DI,TLM0DI
     2,                IOUTHB,JOUTHB,IOUTVB,JOUTVB
     3,                VLATI,VLONI
     4,                HTM,VTM,HWGTS,VWGTS
     5,                T,Q,U,V,Q2,CWM
     6,                TBO,QBO,UBO,VBO,Q2BO,CWMBO)
C
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .
C   SUBROUTINE:  HORIZ       HORIZ INTERP TO NEST BCs
C   PRGRMMR: BLACK           ORG: W/NP22     DATE: 99-05-25
C
C ABSTRACT:  THIS ROUTINE DOES THE HORIZONTAL INTERPOLATION
C            OF MOST PROGNOSTIC VARIABLES TO THIS NEST'S BOUNDARY
C            USING BILINEAR INTERPOLATION
C
C PROGRAM HISTORY LOG:
C   99-05-25  T BLACK - ORIGINATOR
C
C USAGE:  CALL HORIZ FROM SUBROUTINE BCEX
C
C   INPUT ARGUMENT LIST:
C      KBIMX - THE MAXIMUM LENGTH OF THE BOUNDARY ARRAY
C      NBCEX - THE NUMBER OF GRIDS
C          L - THE MODEL LEVEL BEING CONSIDERED
C        IMI - THE ARRAY OF IM's FOR THE NESTS
C        JMI - THE ARRAY OF JM's FOR THE NESTS
C     TPH0DO - THE CENTRAL LATITUDE OF THE PARENT GRID IN DEGREES
C     TLM0DO - THE CENTRAL LONGITUDE OF THE PARENTL GRID IN DEGREES
C     TPH0DI - THE CENTRAL LATITUDE OF THE NEST GRIDS IN DEGREES
C     TLM0DI - THE CENTRAL LONGITUDE OF THE NEST GRIDS IN DEGREES
C     IOUTHB - THE I VALUE ON THE PARENT GRID TO THE LEFT OF
C              THE NEST BOUNDARY H POINT
C     JOUTHB - THE J VALUE ON THE PARENT GRID TO THE LEFT OF
C              THE NEST BOUNDARY H POINT
C     IOUTVB - THE I VALUE ON THE PARENT GRID TO THE LEFT OF
C              THE NEST BOUNDARY V POINT
C     JOUTVB - THE J VALUE ON THE PARENT GRID TO THE LEFT OF
C              THE NEST BOUNDARY V POINT
C      VLATI - THE GEODETIC LATITUDE OF THE NEST BOUNDARY V's
C      VLONI - THE GEODETIC LONGITUDE OF THE NEST BOUNDARY V's
C        HTM - THE TOPOGRAPHY HEIGHT MASK OF THE PARENT DOMAIN
C        VTM - THE VELOCITY MASK OF THE PARENT DOMAIN
C      HWGTS - THE H POINT INTERPOLATION WEIGHTS
C      VWGTS - THE V POINT INTERPOLATION WEIGHTS
C          T - THE TEMPERATURE 
C          Q - THE SPECIFIC HUMIDITY 
C          U - THE U COMPONENT
C          V - THE V COMPONENT 
C         Q2 - THE TURBULENT KINETIC ENERGY 
C        CWM - THE CLOUD WATER/ICE 
C
C   OUTPUT ARGUMENT LIST:
C        TBO - T ON THE NEST'S BOUNDARY
C        QBO - Q ON THE NEST'S BOUNDARY
C        UBO - U ON THE NEST'S BOUNDARY
C        VBO - V ON THE NEST'S BOUNDARY
C       Q2BO - Q2 ON THE NEST'S BOUNDARY
C      CWMBO - CWM ON THE NEST'S BOUNDARY
C
C   SUBPROGRAMS CALLED:
C     UNIQUE:
C             TLL    - COMPUTES TRANSFORMED (ROTATED)
C                      LATITUDE/LONGITUDE
C             ROTLLE - ROTATES GEODETIC WINDS TO TRANSFORMED GRID
C
C----------------------------------------------------------------------
      INCLUDE "parmeta"
C----------------------------------------------------------------------
                              P A R A M E T E R
     & (PI=3.141592654,D2R=PI/180.,R2D=1./D2R)
C----------------------------------------------------------------------
                              R E A L
     & TBO(KBIMX,LM,NBCEX),QBO(KBIMX,LM,NBCEX)
     &,UBO(KBIMX,LM,NBCEX),VBO(KBIMX,LM,NBCEX)
     &,Q2BO(KBIMX,LM,NBCEX),CWMBO(KBIMX,LM,NBCEX)
C----------------------------------------------------------------------
                              R E A L
     & VLATI(KBIMX,NBCEX),VLONI(KBIMX,NBCEX)
     &,HWGTS(KBIMX,4,NBCEX),VWGTS(KBIMX,4,NBCEX)
     &,TVLONI(KBIMX),TVLATI(KBIMX)
     &,UERTH(KBIMX),VERTH(KBIMX)
     &,COSALP(KBIMX),SINALP(KBIMX)
C
     &,HTM(IM,JM),VTM(IM,JM)
     &,T(IM,JM),Q(IM,JM),U(IM,JM),V(IM,JM)
     &,Q2(IM,JM),CWM(IM,JM)
C
     &,IHE(JM),IVE(JM)
C
                              R E A L
     & TPH0DI(9),TLM0DI(9)
C----------------------------------------------------------------------
                              I N T E G E R
     & KOUTHB(KBIMX),KOUTVB(KBIMX)
     &,IOUTHB(KBIMX,NBCEX),JOUTHB(KBIMX,NBCEX)
     &,IOUTVB(KBIMX,NBCEX),JOUTVB(KBIMX,NBCEX)
C
     &,IMI(9),JMI(9)
C----------------------------------------------------------------------
C**********************************************************************
C----------------------------------------------------------------------
C***
C***  CALCULATE THE I-INDEX EAST-WEST INCREMENTS
C***
      DO J=1,JM
        IHE(J)=MOD(J+1,2)
        IVE(J)=MOD(J,2)
      ENDDO
C----------------------------------------------------------------------
C***
C***  LOOP OVER ALL NESTS
C***
      DO 200 NB=1,NBCEX
C
      KBI=2*IMI(NB)+JMI(NB)-3
C----------------------------------------------------------------------
C***
C***  COMPUTE ROTATION ANGLE FOR WINDS (TRANSFORMED GRID (OUTER) TO
C***  EARTH LAT/LON AT INNER GRID POINTS)
C***
      ERLAM0=(360.-TLM0DO)*D2R
      ERPHI0=TPH0DO*D2R
      ERL0=ERLAM0/D2R
      CPHI0=COS(ERPHI0)
      SPHI0=SIN(ERPHI0)
      CALL TLL(VLONI(1,NB),VLATI(1,NB),KBI,ERL0,D2R,CPHI0,SPHI0
     1,        TVLONI,TVLATI)
C
      DO K=1,KBI
        ALPHA=ASIN((SPHI0*SIN(TVLONI(K)*D2R))
     1                /(COS(VLATI(K,NB)*D2R)))
        COSALP(K)=COS(ALPHA)
        SINALP(K)=SIN(ALPHA)
      ENDDO
C----------------------------------------------------------------------
C***
C***  PERFORM INTERPOLATION FROM OUTER GRID TO INNER GRID 
C***  BOUNDARY POINTS
C***
C***
      DO K=1,KBI
C***
C***  BILINEAR INTERPOLATION
C***
        IH=IOUTHB(K,NB)
        JH=JOUTHB(K,NB)
        I1=IH
        J1=JH
        I2=IH+1
        J2=JH
        I3=IH+IHE(JH)
        J3=JH-1
        I4=IH+IHE(JH)
        J4=JH+1
C
        H1=HTM(I1,J1)*HWGTS(K,1,NB)
        H2=HTM(I2,J2)*HWGTS(K,2,NB)
        H3=HTM(I3,J3)*HWGTS(K,3,NB)
        H4=HTM(I4,J4)*HWGTS(K,4,NB)
        HTOT=H1+H2+H3+H4
        IF(HTOT.GT.0.)THEN
          RSUMH=1./(H1+H2+H3+H4)
        ELSE
          RSUMH=1.
        ENDIF
C
        TBO(K,L,NB)=(T(I1,J1)*H1
     1              +T(I2,J2)*H2
     2              +T(I3,J3)*H3
     3              +T(I4,J4)*H4)*RSUMH
        QBO(K,L,NB)=(Q(I1,J1)*H1
     1              +Q(I2,J2)*H2
     2              +Q(I3,J3)*H3
     3              +Q(I4,J4)*H4)*RSUMH
        Q2BO(K,L,NB)=(Q2(I1,J1)*H1
     1               +Q2(I2,J2)*H2
     2               +Q2(I3,J3)*H3
     3               +Q2(I4,J4)*H4)*RSUMH
        CWMBO(K,L,NB)=(CWM(I1,J1)*H1
     1                +CWM(I2,J2)*H2
     2                +CWM(I3,J3)*H3
     3                +CWM(I4,J4)*H4)*RSUMH
C***
C***  THE V POINTS
C***
        IV=IOUTVB(K,NB)
        JV=JOUTVB(K,NB)
C
        I1=IV
        J1=JV
        I2=IV+1
        J2=JV
        I3=IV+IVE(JV)
        J3=JV-1
        I4=IV+IVE(JV)
        J4=JV+1
C       
        V1=VTM(I1,J1)*VWGTS(K,1,NB)
        V2=VTM(I2,J2)*VWGTS(K,2,NB)
        V3=VTM(I3,J3)*VWGTS(K,3,NB)
        V4=VTM(I4,J4)*VWGTS(K,4,NB)
        VTOT=V1+V2+V3+V4
        IF(VTOT.GT.0.)THEN
          RSUMV=1./(V1+V2+V3+V4)
        ELSE
          RSUMV=1.
        ENDIF
C
        UBO(K,L,NB)=(U(I1,J1)*V1
     1              +U(I2,J2)*V2
     2              +U(I3,J3)*V3
     3              +U(I4,J4)*V4)*RSUMV
        VBO(K,L,NB)=(V(I1,J1)*V1
     1              +V(I2,J2)*V2
     2              +V(I3,J3)*V3
     3              +V(I4,J4)*V4)*RSUMV
      ENDDO
C----------------------------------------------------------------------
C***
C***  BEGIN ROTATION OF WIND COMPONENTS.
C***  FIRST, ROTATE FROM OUTER GRID COORD TO EARTH COORDINATES.
C***
      DO K=1,KBI
        UERTH(K)=UBO(K,L,NB)*COSALP(K)+VBO(K,L,NB)*SINALP(K)
        VERTH(K)=VBO(K,L,NB)*COSALP(K)-UBO(K,L,NB)*SINALP(K)
      ENDDO
C***
C***  ROTATE FROM EARTH COORDINATES TO THE TARGET GRID
C***
      CALL ROTLLE(UERTH,VERTH,VLATI(1,NB),VLONI(1,NB)
     1,           TPH0DI(NB),TLM0DI(NB),KBI
     2,           UBO(1,L,NB),VBO(1,L,NB))
C
  200 CONTINUE
C----------------------------------------------------------------------
      RETURN
      END
C----------------------------------------------------------------------
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
                          SUBROUTINE TLL
     1 (ALMD,APHD,KB,TLMOD,DTR,CTPH0,STPH0,TLM,TPH)
C---------------------------------------------------------------------
      REAL ALMD(KB),APHD(KB),TLM(KB),TPH(KB)
C---------------------------------------------------------------------
      DO K=1,KB
        RELM=(ALMD(K)-TLMOD)*DTR
        SRLM=SIN(RELM)
        CRLM=COS(RELM)
        APH=APHD(K)*DTR
        SPH=SIN(APH)
        CPH=COS(APH)
        CC=CPH*CRLM
        ANUM=CPH*SRLM
        DENOM=CTPH0*CC+STPH0*SPH
        TLM(K)=-ATAN2(ANUM,DENOM)/DTR
        TPH(K)=ASIN(CTPH0*SPH-STPH0*CC)/DTR
      ENDDO
C
      RETURN
      END
C----------------------------------------------------------------------
      SUBROUTINE ROTLLE(U,V,VLAT,VLON,TPH0DI,TLM0DI,KBI,UOUT,VOUT)
C
C SUBPROGRAM: ROTLLE        ROTATE WINDS ON LAT/LONG GRID TO E-GRID
C   PRGMMR: T.BLACK         ORG: W/NP22     DATE: ??-??-??
C
C ABSTRACT: ROTATES WINDS ON THE LAT/LONG GRID TO THE ETA MODEL GRID
C
C PROGRAM HISTORY LOG:
C   ??-??-??  T. BLACK
C
C USAGE     CALL ROTLLE(U,V,VLAT,VLON)
C   INPUT ARGUMENT LIST:
C     U        - LAT/LONG U-COMPONENT
C     V        - LAT/LONG V-COMPONENT
C     VLAT     - LATITUDE OF E-GRID V POINTS (DEGREES)
C     VLON     - LONGITUDE OF E-GRID V POINTS (DEGREES)
C
C   OUTPUT ARGUMENT LIST:
C     U        - ETA GRID U-COMPONENT
C     V        - ETA GRID V-COMPONENT
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  IBM SP
C
C***
C*** ROTATE THE LAT-LON WINDS TO THE E-GRID
C***
C
C    N O T E : INPUT LAT/LONG MUST BE CONVERTED TO RADIANS !!!
C
C----------------------------------------------------------------------
      INCLUDE "parmeta"
C----------------------------------------------------------------------
                         P A R A M E T E R
     1 (PI=3.141592654,D2R=PI/180.)
C----------------------------------------------------------------------
                         D I M E N S I O N
     1 U(KBI),V(KBI),VLAT(KBI),VLON(KBI)
     2,UOUT(KBI),VOUT(KBI),CRAY(KBI),DRAY(KBI)
C----------------------------------------------------------------------
      ORPHI0=TPH0DI*D2R
      ORLAM0=(360.+TLM0DI)*D2R
      SPHI0=SIN(ORPHI0)
      CPHI0=COS(ORPHI0)
C
      DO K=1,KBI
        TLAT=VLAT(K)*D2R
        TLON=-VLON(K)*D2R
        RELM=TLON-ORLAM0
        SRLM=SIN(RELM)
        CRLM=COS(RELM)
        SPH=SIN(TLAT)
        CPH=COS(TLAT)
        CC=CPH*CRLM
        TPH=ASIN(CPHI0*SPH-SPHI0*CC)
        RCTPH=1./COS(TPH)
        CRAY(K)=SPHI0*SRLM*RCTPH
        DRAY(K)=(CPHI0*CPH+SPHI0*SPH*CRLM)*RCTPH
      ENDDO
C
      DO K=1,KBI
        RU=DRAY(K)*U(K)-CRAY(K)*V(K)
        RV=CRAY(K)*U(K)+DRAY(K)*V(K)
        UOUT(K)=RU
        VOUT(K)=RV
      ENDDO
C----------------------------------------------------------------------
      RETURN
      END
