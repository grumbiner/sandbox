       SUBROUTINE GRD2ETA(ZE,SPE,ZSE,SME,SSTE,
     1                    SNOE,EREFSE,IDATE,SIGMA,REF,ETOP)
C$$$
C   PROGRAM: MRF2ETA2        INTERPOLATES ANALYSIS STRIPS
C                            FROM THE LAT/LONG ANALYSIS GRID
C                            TO THE E-GRID
C   PRGMMR: E.ROGERS         ORG: W/NMC22    DATE: 90-06-14
C
C ABSTRACT: INTERPOLATES ANALYSIS STRIPS FROM THE LAT/LONG GRID TO
C           THE E-GRID. THIS VERSION RUNS FOR S. LORD'S PACIFIC RUNS
C
C PROGRAM HISTORY LOG:
C   86-03-31  G DIMEGO WROTE ORIGINAL STRIP READER FOR ROI
C   90-06-14  E ROGERS MODIFIED ORIGINAL CODE TO READ FOR ETA GRID
C   90-08-18  E ROGERS MODIFIED ORIGINAL CODE TO USE GES FILE RATHER
C             THEN MRF SURFACE FILE FOR WET, SST, SNO
C   98-05-29  T BLACK - SOME OPTIMIZATION
C   98-06-08  M BALDWIN - CONVERT TO 2-D CODE
C   98-12-09  E ROGERS - ADD LOGIC TO PROCESS NCEP REANALYSIS SURFACE
C             (SNOW/SST?SEA ICE) DATA
C
C INPUT FILES
C   UNIT 11 - ANALYSIS STRIPS ON LAT/LONG GRID
C   UNIT 12 - SURFACE FIELDS FROM GLOBAL SYSTEM
C
C OUTPUT FILES
C   UNIT 03 - PRINT TO SAVRS
C   UNIT 51 - VARIABLES ON E-GRID
C   UNIT 52 - DATE RECORD
C
C SUBROUTINES CALLED
C   UNIQUE: LL2IJ, IJ2LL, ETALL, INTERP, FLIP, ROTLLE
C           INTSFC
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE: CRAY
C
C-----------------------------------------------------------------------
      integer,parameter::real_32=selected_real_kind(6,30)
      INCLUDE "parmeta.h"
      parameter(lussti=34,lusstg=35,jf=1000000)
      parameter(lusnoi=36,lusnog=37)
      parameter(luicoi=38,luicog=39)
C-----------------------------------------------------------------------
                             R E A L
     & MINLAT
C-----------------------------------------------------------------------
                             L O G I C A L 
     & SIGMA
C-----------------------------------------------------------------------
                             D I M E N S I O N
c    & TVE(IM,JM,LMAX),UE(IM,JM,LMAX),VE(IM,JM,LMAX),QE(IM,JM,LMAX)
     & ZE(IM,JM,LMAX1),REF(IM,JM)
     &,ZSE(IM,JM),SPE(IM,JM),SME(IM,JM),SSTE(IM,JM)
     &,SST(360,180),SNO(720,361),SGICE(720,360)
c    &,SST(360,180),SNO(181,91),SGICE(720,360)
c    &,ALE(IM,JM,LMAX),ZSE(IM,JM),SPE(IM,JM),SME(IM,JM),SSTE(IM,JM)
c    &,SNOE(IM,JM),STRIP(IMAX,LMAX,NVBL),AL(IMAX,JMAX,LMAX)
     &,SNOE(IM,JM)
     &,IDATE(4),SL(LMAX),SI(LMAX1),TSE(IM,JM),EREFSE(IM,JM)
     &,DUM(IM,JM),SM(IM,JM)
C
        COMMON /GBLATM/TVE(IM,JM,LMAX),UE(IM,JM,LMAX),VE(IM,JM,LMAX)
     1,     QE(IM,JM,LMAX)
        COMMON /BIGGBL/ AL(IMAX,JMAX,LMAX),ALE(IM,JM,LMAX)

C-----------------------------------------------------------------------
       REAL(REAL_32),ALLOCATABLE,DIMENSION(:,:,:)::STRIP
C
       COMMON /LATLONS/
     & HLAT(IM,JM),HLON(IM,JM),VLAT(IM,JM),VLON(IM,JM)
C-----------------------------------------------------------------------
       COMMON /GRIDS/
     & PSTAR(IMAX,JMAX),ZSTAR(IMAX,JMAX),TSTAR(IMAX,JMAX)
     &,ETAREF(IMAX,JMAX),ERI1(IMAX,JMAX)
     &,DETA(LM),ETA(LMP1),ETAL(LM),ZETA(LMP1)
C-----------------------------------------------------------------------
       integer jpds(200),jgds(200),kpds(200),kgds(200)
       logical logb(jf)
C-----------------------------------------------------------------------
                             D A T A
     & LIST/03/, LSTP/11/, LSFC/12/, LOROG/14/, LOUT/51/, LDATE/52/
C----------------------------------------------------------------------
C------------COMPUTE THE EARTH LAT/LONG OF ALL ETA GRID POINTS---------
C----------------------------------------------------------------------
      CALL ETALL(HLAT,HLON,VLAT,VLON)
C
      MINLAT=90.0
      DO J = 1, JM
        DO I = 1, IM
          MINLAT = MIN(HLAT(I,J), MINLAT)
        ENDDO
      ENDDO
C
c SET UP ETA VALUES AND REFERENCE HEIGHTS
C
      G = 9.80
      R = 287.04
      CP = 1004.6
      P00 = 1.E5
      GAMMA = 0.0065
      PRF0 = 101325.0
      T0 = 288.0
      GORG = G / (R * GAMMA)
      RGOG = 1.0 / GORG
      ROG = R / G
      CPOG = CP / G
      CAPA = R / CP
      PT= etop * 100.0
      PETAT = PT
      DETA2 = 0.0
      ALPT = ALOG(PT)
C
C------- DEPTH OF ETA LAYERS ----------------------------------------
C
c     INCLUDE "deta"
      READ(16) DETA
      REWIND 16
      ETA(1) = 0.
C
      DO 272 L = 1, LMM1
         ETA(L+1) = ETA(L) + DETA(L)
 272  CONTINUE
C
      ETA(LMP1) = 1.0
      DETA(LM) = 1.0 - ETA(LM)
C
      DO 901 L = 1, LM
         ETAL(L) = 0.5 * (ETA(L) + ETA(L+1))
 901  CONTINUE
C
C SET UP REFERENCE STEPS AT EACH ETA INTERFACE
C
C
      DO L=1,LMP1
        ZETA(L)=T0*(1.-((PT+ETA(L)*(PRF0-PT))/PRF0)**RGOG)
     1          /GAMMA
      ENDDO
C
C   READ REF
C
      REWIND LOROG
      READ(LOROG)DUM,SM
      READ(LOROG)REF
      REWIND LOROG
C
      IF(SIGMA)THEN
        DO J=1,JM
        DO I=1,IM
          REF(I,J)=1.
        ENDDO
        ENDDO
      ENDIF
C
C-----------------------------------------------------------------------
C-----READ IN THE ANALYSIS STRIPS AND PULL OFF ONE FIELD AT A TIME------
C-----------------------------------------------------------------------
C---------THE VALUES OF NREAD COORESPOND TO FIELDS AS FOLLOWS:----------
C-----------------------------------------------------------------------
C                      NREAD = 1 ---> Z
C                      NREAD = 2 ---> T
C                      NREAD = 3 ---> U
C                      NREAD = 4 ---> V
C                      NREAD = 5 ---> Q
C                      NREAD = 6 ---> PMID
C
C                      NREAD = 5, L = 1 ---> ZSE
C                      NREAD = 5, L = 2 ---> SPE
C-----------------------------------------------------------------------
C
C   READ IN SURFACE VALUES PLUS ETAREF AND INDEX
C
      REWIND 13
      READ(13)
      READ(13) PSTAR
      READ(13) ZSTAR
      READ(13) TSTAR 
      READ(13) ETAREF
      READ(13) ERI1
C
      DO 3382 J = 1,JMAX
      ERLAT = (J - 1) * DLAT + YLATS
      DO 3382 I = 1,IMAX
        DO 3381 JJ= 1,JM
        DO 3381 II= 1,IM
c         ERDIFF = ETAREF(I,J)-REF(II,JJ)
c         IF(I.EQ.285 .AND. J.EQ.6) THEN
c            PRINT *,II,JJ,ETAREF(I,J),REF(II,JJ),ERDIFF
c         ENDIF
          IF(ABS(ETAREF(I,J)-REF(II,JJ)).LT.1.E-6)THEN
            ETAREF(I,J)=REF(II,JJ)
            GO TO 3382
          ENDIF
 3381   CONTINUE
c       IF(ERLAT .LT. MINLAT) THEN
c         WRITE(6,*)"LAT/LON POINT OUTSIDE OF E-GRID ",I,J,ERLAT,MINLAT
c       ELSE
c         WRITE(6,*)"OH OH WE GOT TROUBLE AT I,J,II,JJ",I,J,II,JJ
c       ENDIF
 3382 CONTINUE
C
      CALL INTERPS(1,pstar,HLAT,HLON,VLAT,VLON,spe,1,ref)
      CALL INTERPS(1,zstar,HLAT,HLON,VLAT,VLON,zse,1,ref)
      CALL INTERPS(1,tstar,HLAT,HLON,VLAT,VLON,tse,1,ref)
      CALL INTERPS(1,etaref,HLAT,HLON,VLAT,VLON,erefse,1,ref)
C
      ALLOCATE(STRIP(IMAX,LMAX,NVBL))
      DO 500 NREAD = 1, 5
        WRITE(6,321) NREAD
  321   FORMAT(2X,' NREAD = ',I2)
        REWIND LSTP
        READ(LSTP) HOUR, IDATE, SI, SL
C
C  CHECK TO SEE IF WE HAVE A 4-DIGIT YEAR; IF NOT ADD 1900
C  SINCE REANALYSIS SIGMA FILE HAD A 2-DIGIT YEAR UNTIL MARCH 1997
C
        IF(IDATE(4) .LT. 1900) THEN
          IYR4D = IDATE(4) + 1900
          IDATE(4) = IYR4D
        ENDIF
C
        WRITE(6,322) HOUR,IDATE
  322   FORMAT(2X,F6.0,2X,4I6)
c       print*,'imax,jmax=',imax,jmax
        DO 550 NJ = 1, JMAX
          READ(LSTP) STRIP
c         if(nread.eq.5) then
c           do i=115,123
c            print*,'i,strip(i,387,2)=',i,strip(i,387,2)
c           enddo
c         endif
          DO 600 NL = 1, LMAX
          DO 600 NI = 1, IMAX
C          IF(MOD(NI,240) .EQ. 0 .AND. MOD(NJ,60) .EQ. 0
C    1           .AND. MOD(NL,9) .EQ. 0) THEN
C           WRITE(6,549) NREAD,NJ,NL,NI
C 549       FORMAT(2X,'NREAD= ',I2,' NJ= ',I4,' NL = ',I3,' NI =',I3)
C          END IF
           AL(NI,NJ,NL) = STRIP(NI,NL,NREAD)
c          if(nread.eq.5.and.nl.eq.2.and.nj.ge.367) then
c           print*,'i,j,al(i,j,l)='
c    *       ,ni,nj,al(ni,nj,2)
c          endif
  600     CONTINUE
  550   CONTINUE
C----------------------------------------------------------------------
C-----------------------FLIP ARRAY AL----------------------------------
C----------------------------------------------------------------------
        CALL FLIP(LMAX)
c       CALL FLIP(AL,LMAX)
C----------------------------------------------------------------------
C---------CALL THE INTERPOLATION SUBROUTINE----------------------------
C----------------------------------------------------------------------
        CALL INTERP(NREAD,HLAT,HLON,VLAT,VLON,lmax,ref)
c       CALL INTERP(NREAD,AL,HLAT,HLON,VLAT,VLON,lmax,ref)
        DO J = 1, JM
         DO I = 1, IM
          IF(MOD(I,10).EQ.0 .AND. MOD(J,10).EQ.0) THEN
           WRITE(6,78889) NREAD,I,J,ALE(I,J,9),ALE(I,J,18)
78889      FORMAT(2X,3(I5,1X),2(E12.5,1X))
          END IF
         ENDDO
        ENDDO
C----------------------------------------------------------------------
C---------PUT ALE INTO SPECIFIC ARRAYS---------------------------------
C----------------------------------------------------------------------
      IF(NREAD.EQ.1)THEN
        DO L=1,LMAX
        DO J=1,JM
        DO I=1,IM
          ZE(I,J,L)=ALE(I,J,L)
        ENDDO
        ENDDO
        ENDDO
      ELSEIF(NREAD.EQ.2)THEN
        DO L=1,LMAX
        DO J=1,JM
        DO I=1,IM
          TVE(I,J,L)=ALE(I,J,L)
        ENDDO
        ENDDO
        ENDDO
      ELSEIF(NREAD.EQ.3)THEN
        DO L=1,LMAX
        DO J=1,JM
        DO I=1,IM
          UE(I,J,L)=ALE(I,J,L)
        ENDDO
        ENDDO
        ENDDO
      ELSEIF(NREAD.EQ.4)THEN
        DO L=1,LMAX
        DO J=1,JM
        DO I=1,IM
          VE(I,J,L)=ALE(I,J,L)
        ENDDO
        ENDDO
        ENDDO
      ELSEIF(NREAD.EQ.5)THEN
        DO J=1,JM
        DO I=1,IM
          SPE(I,J)=ALE(I,J,2)
        ENDDO
        ENDDO
        DO L=LDQ2,LMAX
        DO J=1,JM
        DO I=1,IM
          QE(I,J,L)=ALE(I,J,L)
        ENDDO
        ENDDO
        ENDDO
      ENDIF
C----------------------------------------------------------------------
C-------------------ZERO OUT UPPER LEVEL Q-----------------------------
C----------------------------------------------------------------------
        IF(NREAD .EQ. 5) THEN
         DO 300 L = 1, LDQ2
         DO 300 J = 1, JM
         DO 300 I = 1, IM
           QE(I,J,L) = 0.0
  300    CONTINUE
        END IF
  500 CONTINUE
      DEALLOCATE(STRIP)
C----------------------------------------------------------------------
C------------------------CALCULATE THE VIRTUAL TEMPERATURE--------------------
C----------------------------------------------------------------------
c     DO 400 L = 1, LMAX
c     DO 400 J = 1, JM
c     DO 400 I = 1, IM
c       TVE(I,J,L) = TVE(I,J,L) * (1.0 + (0.608 * QE(I,J,L)))
c 400 CONTINUE
C----------------------------------------------------------------------
C-----REORIENT WIND COMPONENTS FROM GEODETIC LAT/LONG TO ETA GRID------
C-----------------------------------------------------------------------
      CALL ROTLLE(UE,VE,VLAT,VLON)
C-----------------------------------------------------------------------
C---------------WRITE ETA GRID FIELDS TO UNIT 23------------------------
C-----------------------------------------------------------------------
      DO J = 1, JM
      DO I = 1, IM
        ZE(I,J,LMAX1) = 0.0
      ENDDO
      ENDDO
C     REWIND LOUT
C     WRITE(LOUT) ZE,TVE,UE,VE,QE,SPE,ZSE,SME,SSTE,SNOE,erefse
C-----------------------------------------------------------------------
C***
C***  DIAGNOSTIC PRINTS
C***
      DO J = 1, JM
      DO I = 1, IM
       IF(I.EQ.119.AND.J.EQ.387) THEN
        WRITE(6,94857) I,J
94857   FORMAT(2X,' K(SIGMA) = ',2I6)
       DO L = 1, LMAX
        WRITE(6,59647) L,TVE(I,J,L),QE(I,J,L),UE(I,J,L),VE(I,J,L),
     &             ZE(I,J,L),SPE(I,J),ZSE(I,J)
59647   FORMAT(1X,I3,7(E12.5,1X))
       ENDDO
       END IF
      ENDDO
      ENDDO
      WRITE(LDATE) IDATE
C----------------------------------------------------------------------
      RETURN
      END
