      SUBROUTINE SFCGDAS
C     ******************************************************************
                             P A R A M E T E R
     & (D00=0.0,D5=5.E-1,D01=1.00E-2,H1=1.0,HM1=-1.0
     &, H90=90.0,H360=360.0,EPS=1.E-10)
C----------------------------------------------------------------------
      integer,parameter::real_32=selected_real_kind(6,30)
      INCLUDE "parmeta.res"
      INCLUDE "parmsoil"
C----------------------------------------------------------------------
                             P A R A M E T E R
     & (ITB=076,JTB=134,ITBQ=152,JTBQ=440)
                             P A R A M E T E R
     & (IM2=IM-2,JM1=JM-1,JM2=JM-2,JM3=JM-3,JM4=JM-4,JM5=JM-5
     &, IMJM=IM*JM-JM/2,JAM=6+2*(JM-10)
     &, KHN=IM-1,KHS=-IM
     &, KNE=IM    ,KNW=IM-1 ,KSW=-IM    ,KSE=-IM+1
     &, KHL00=1                    ,KHH00=IM*JM-JM/2
     &, LP1=LM+1)
c    &, LP1=LM+1,nglats=190,nglons=384)
C-----------------------------------------------------------------------
                             C O M M O N  /PTETA/
     & IDAT(3),PT ,DETA  (LM),AETA  (LM),ETA   (LP1),DFL   (LP1)
C
     &,RES   (IM,JM),FIS   (IM,JM),ALBEDO(IM,JM)
     &,SNO   (IM,JM),SST   (IM,JM),SI    (IM,JM)
     &,SM    (IM,JM),LMH   (IM,JM),LMV   (IM,JM)
     &,SMC(IM,JM,NSOIL),STC(IM,JM,NSOIL),CMC(IM,JM),SH2O(IM,JM,NSOIL)
C-----------------------------------------------------------------------
                             C O M M O N /MASKS/
     & HBM2  (IM,JM),VBM2  (IM,JM),VBM3  (IM,JM),SICE (IM,JM)
C
     &,HTM   (IM,JM,LM),VTM   (IM,JM,LM)
C-----------------------------------------------------------------------
       COMMON /LATLONS/
     & HLAT(IM,JM),HLON(IM,JM),VLAT(IM,JM),VLON(IM,JM)
C-----------------------------------------------------------------------
       COMMON /TGRND/ TG(IM,JM)
C
                             D I M E N S I O N
     &  IGDATE(4)
C
       REAL(REAL_32),ALLOCATABLE,DIMENSION(:)::GLATS,SLAT,WLAT
       REAL(REAL_32),ALLOCATABLE,DIMENSION(:,:)::GGTG,GGSLI,GGRID1
       REAL(REAL_32),ALLOCATABLE,DIMENSION(:,:,:)::GGSMC2,GGSTC2
C
       CHARACTER*32 GLABEL
C-----------------------------------------------------------------------
      DPR=180./3.141592654
C
      READ (33) 
      READ (33) GFHR,IGDATE,NGLONS,NGLATS,NUM
      print *,' nglats,nglons = ',gfhr,igdate,nglats,nglons,num
C
      ALLOCATE(GGRID1(NGLONS,NGLATS))
      ALLOCATE(GLATS(NGLATS))
      ALLOCATE(SLAT(NGLATS))
      ALLOCATE(WLAT(NGLATS))
      ALLOCATE(GGSMC2(NGLONS,NGLATS,2))
      ALLOCATE(GGSTC2(NGLONS,NGLATS,2))
      ALLOCATE(GGTG(NGLONS,NGLATS))
      ALLOCATE(GGSLI(NGLONS,NGLATS))
C
C READ GLOBAL GAUSSIAN FIELDS FROM THE BGES FILE AND INTERPOLATE THEM TO
C THE ETA GRID
C FIRST CALCULATE THE GAUSSIAN LATITUDES
C
      CALL SPLAT(4,NGLATS,SLAT,WLAT)
      DO J = 1, NGLATS
        GLATS(J) = DPR*ASIN(SLAT(J))
        print *,j,glats(j)
      ENDDO 
C
C NOW READ IN THE FIELDS, NOTE WE NEED THE GLOBAL SEA,LAND ICE MASK
C TO DO A PROPER INTERPOLATION.
C FIRST TWO RECORDS NOW READ IN ECONVK
c     OPEN(33,FILE='fort.33',
c    &     FORM='UNFORMATTED',STATUS='OLD',IOSTAT=IRET)
C
      print *,'ok after gfhr in sfcanl'
C  BYPASS THE SURFACE TEMPERATURE RECORD
      READ (33) GGRID1
      print *,'ok after rec 1 in sfcanl'
C  READ AND SAVE THE TWO-LAYER SOIL MOISTURE RECORDS
      READ (33) GGSMC2
      print *,'ok after rec 2 in sfcanl'
C  BYPASS THE SNOW DEPTH RECORD
      READ (33) GGRID1
      print *,'ok after rec 3 in sfcanl'
C  READ AND STORE THE THE SOIL TEMPERATURES (TWO-LAYERS)
      READ (33) GGSTC2
      print *,'ok after rec 4 in sfcanl'
C  READ AND STORE THE DEEP LAYER SOIL TEMPERATURE
      READ (33) GGTG
      print *,'ok after rec 5 in sfcanl'
C  BYPASS 5 MORE RECORDS AND THEN READ AND STORE THE SEA,LAND ICE RECORD
C  BYPASSING Z0,CONV. CLD FRACT.,CONV. CLD BASE,CONV CLD TOP, AND ALBEDO
      DO KRD = 1,6
      READ (33) GGSLI
      END DO
      print *,'ok after rec 6-12 in sfcanl'
C
      CALL GAUTOETA(GLATS,NGLATS,NGLONS,HLAT,HLON,SM,SICE,GGSLI,
     &              GGSMC2(1,1,1),SMC(1,1,1))
      CALL GAUTOETA(GLATS,NGLATS,NGLONS,HLAT,HLON,SM,SICE,GGSLI,
     &              GGSTC2(1,1,1),STC(1,1,1))
      CALL GAUTOICE(GLATS,NGLATS,NGLONS,HLAT,HLON,SM,SICE,GGSLI,
     &              GGSTC2(1,1,1),STC(1,1,1))
      DO KK=2,NSOIL
       CALL GAUTOETA(GLATS,NGLATS,NGLONS,HLAT,HLON,SM,SICE,GGSLI,
     &              GGSMC2(1,1,2),SMC(1,1,KK))
       CALL GAUTOETA(GLATS,NGLATS,NGLONS,HLAT,HLON,SM,SICE,GGSLI,
     &              GGSTC2(1,1,2),STC(1,1,KK))
       CALL GAUTOICE(GLATS,NGLATS,NGLONS,HLAT,HLON,SM,SICE,GGSLI,
     &              GGSTC2(1,1,2),STC(1,1,KK))
      ENDDO
C
C NOW SET LOWER BOUNDARY CONDITION ON SUB-SURFACE
C TEMPERATURE, TREAT UNDER SEA-ICE AFTER CALL
      CALL GAUTOETA(GLATS,NGLATS,NGLONS,HLAT,HLON,SM,SICE,GGSLI,
     &              GGTG,TG)
C
      DEALLOCATE(GGRID1)
      DEALLOCATE(SLAT)
      DEALLOCATE(WLAT)
      DEALLOCATE(GLATS)
      DEALLOCATE(GGSMC2)
      DEALLOCATE(GGSTC2)
      DEALLOCATE(GGTG)
      DEALLOCATE(GGSLI)
C-----------------------------------------------------------------------
                             RETURN
                             END
