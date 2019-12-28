C
      SUBROUTINE DATAPK_AVG(IM,JM,KB,IMJM,KB1,gbname)
C     ===========================================
C
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    DATAPK_AVG  PACK AVG DATA IN GRIB    
C   PRGMMR: RIVIN            ORG: W/NMC21    DATE: 00-11-01
C
C ABSTRACT: CALLS ROUTINES TO PACK AVG DATA IN GRIB.
C
C PROGRAM HISTORY LOG:
C
C USAGE:    CALL DATAPK_AVG(IM,JM,KB,IMJM,KB1,gbname)
C   INPUT ARGUMENT LIST:
C     FT21F001 - yymmddhh.avg
C
C   OUTPUT ARGUMENT LIST:      
C
C   SUBPROGRAMS CALLED
C     UNIQUE:    
C      GENPDS
C      GENGDS
C      GENBDS
C      IN2
C      IN0
C      FULLYR
C      FIXDAT
C
C ATTRIBUTES:
C   LANGUAGE: IBM 370 VS FORTRAN
C   MACHINE:  NAS, CRAY C-90, IBM SP
C
C$$$
C
      DIMENSION Z(KB),ZZ(KB),ELB(IM,JM),TB(IM,JM,KB),SB(IM,JM,KB)
      DIMENSION H(IM,JM),FSM(IM,JM),DUM(IM,JM),DVM(IM,JM),ALON(IM,JM)
      DIMENSION ALAT(IM,JM)
      DIMENSION UAB(IM,JM),VAB(IM,JM),UB(IM,JM,KB),VB(IM,JM,KB)
      DIMENSION W(IM,JM,KB)
      DIMENSION HU(IM,JM), HV(IM,JM), ULEV(IM,JM), VLEV(IM,JM),
     *          DUM1(IM,JM), DVM1(IM,JM)      
      REAL Q2(IM,JM,KB),L(IM,JM,KB)                            
      
      DIMENSION ES(IM,JM),TS(IM,JM,KB),SS(IM,JM,KB),UAS(IM,JM),
     &           VAS(IM,JM),
     &          US(IM,JM,KB),VS(IM,JM,KB)
      CHARACTER*64 HEADER                                       

      DIMENSION UCLIP(IM,JM), VCLIP(IM,JM)
      DIMENSION ELMAX(IM,JM),ELMIN(IM,JM)

      DIMENSION ELB1(IMJM),UB1(IMJM),VB1(IMJM),RBMAP(IMJM),RUMAP(IMJM)
      DIMENSION H1(IMJM),ALAT1(IMJM),ALON1(IMJM),SMASK(IMJM)
      DIMENSION RVMAP(IMJM),RNMAP(IMJM)
      DIMENSION TRAW(IMJM),SRAW(IMJM),URAW(IMJM),VRAW(IMJM),TKE(IMJM)
      DIMENSION WRAW(IMJM)
      DIMENSION ES1(IMJM),UAS1(IMJM),VAS1(IMJM),TSRAW(IMJM),SSRAW(IMJM)
      DIMENSION USRAW(IMJM),VSRAW(IMJM)
      REAL LRAW(IMJM)
      REAL hh,dd,mm,yy
      CHARACTER*1 PDS(28)
      CHARACTER*80 gbname, GRNAME
      INTEGER IGDS(18)
      LOGICAL FIRSTIME,LEND,MEND,FRST2,KEND
C234567890---------2---------3---------4---------5---------6---------7-}
      IU = 53
      LUN2 = 52
      FIRSTIME=.TRUE.          
      FRST2=.TRUE.               
      LAVG=0
      GRNAME='ecofsgrd.grb'                     
      ZLEV=1.0
      N = 0
      KZ = 1                            
      LOOP = 0

      CALL IN0 (22,IHHF,IDDF,IMMF,IYYF,IFT,IDGRD,
     &                 Z,H,FSM,DUM,DVM,ALON,ALAT,IM,JM,KB)
      CLOSE(22)        
      WRITE (*,*) 'GRID DATA READ'
      WRITE (*,*) 'Z:'
      WRITE (*,*) (Z(K),K=1,KB)


      DO K=1,KB-1
        ZZ(K)=(Z(K)+Z(K+1))/2.0
      ENDDO  
C     Bathymetry needs to be interpolated to velocity poins      
            
      DO J=1,JM
        DO I=2,IM
          HU(I,J)=(H(I,J)+H(I-1,J))/2.0
        ENDDO
      ENDDO  
      DO I=1,IM
        DO J=2,JM
          HV(I,J)=(H(I,J)+H(I,J-1))/2.0
        ENDDO
      ENDDO  



      CALL FULLYR (IYYF)           
      CALL FIXDAT (IYYF,IMMF,IDDF,IHHF, IYYREF,IMMREF,IDDREF,IHHREF)
      WRITE (*,*) IYYREF,IMMREF,IDDREF,IHHREF
c      CALL UNFULLYR (IYYREF)
      IH=IHHREF
      IDY=IDDREF
      IMY=IMMREF
      IY=IYYREF
      
C
C INITIALIZE VARIABLE STORAGE ARRAYS
C
      DO 250 I=1,IMJM
        ELB1(I)=0.0
        UB1(I)=0.0
        VB1(I)=0.0
        TRAW(I)=0.0
        SRAW(I)=0.0
        URAW(I)=0.0
        VRAW(I)=0.0                                   
        WRAW(I)=0.0
        ALON1(I)=0.0
        ALAT1(I)=0.0
        H1(I)=0.0                                           
        SMASK(I)=0.0
 250  CONTINUE               


C---------------------AVERAGED FIELDS-----------------------------

      
 1010 CONTINUE
        

      CALL IN2 (21,ES,TS,SS,UAS,VAS,US,VS,IM,JM,KB,IFT,IAVG,NC,MEND)
      WRITE (*,*) 'PROCESSING FORECAST TIME: ',IFT
      IF (MEND) GOTO 1500

      II=1
      DO 1300 J = 1,JM
       DO 1300 I = 1, IM
       ES1(II)=ES(I,J)
       RBMAP(II) = FSM(I,J)
       II = II + 1
 1300 CONTINUE   

      LAVG=3
      KZ = 1
C
C AVG ELEVATION
C
      KPD = 82
      LVL = 1
      CALL GENPDS(IY,IMY,IDY,IH,IFT,KPD,LVL,PDS,KZ,Z,KB,IMJM,ES1,
     & RBMAP,LAVG,IAVG,NC)
      CALL GENGDS(IGDS,IM,JM)
      CALL GENBDS(PDS,IGDS,ES1,IU,IMJM,FIRSTIME,RBMAP,GBNAME,2)
      FIRSTIME=.FALSE.
C
C AVG VELOCITIES - BAROTROPIC
C


      II=1
      DO 1301 J = 1,JM
       DO 1301 I = 1,IM
       UAS1(II)=UAS(I,J)
       RUMAP(II) = DUM(I,J)
       II = II + 1
 1301 CONTINUE

      II=1
      DO 1302 J = 1,JM
       DO 1302 I = 1,IM
       VAS1(II)=VAS(I,J)
       RVMAP(II) = DVM(I,J)
       II = II + 1
 1302 CONTINUE


      KPD = 49
      LVL = 201
      CALL GENPDS(IY,IMY,IDY,IH,IFT,KPD,LVL,PDS,KZ,Z,KB,IM*JM,UAS1,
     &            RUMAP,LAVG,IAVG,NC)
      CALL GENGDS(IGDS,IM,JM)
      CALL GENBDS(PDS,IGDS,UAS1,IU,IM*JM,FIRSTIME,RUMAP,GBNAME,2)
C
      KPD = 50
      LVL = 201
      CALL GENPDS(IY,IMY,IDY,IH,IFT,KPD,LVL,PDS,KZ,Z,KB,IM*JM,VAS1,
     &            RVMAP,LAVG,IAVG,NC)
      CALL GENGDS(IGDS,IM,JM)
      CALL GENBDS(PDS,IGDS,VAS1,IU,IM*JM,FIRSTIME,RVMAP,GBNAME,2)
C
      DO 450 KZ = 1,KB1
        II = 1
        DO 460 J = 1,JM
         DO 460 I = 1,IM
          TSRAW(II) = TS(I,J,KZ) + 273.16
          SSRAW(II) = SS(I,J,KZ)/1000.
          II = II + 1
 460    CONTINUE
        CALL MNMX (TSRAW,IMJM,RN,RX,IN,IX)

        II = 1
        DO 470 J=1,JM
         DO 470 I=1,IM
          USRAW(II) = US(I,J,KZ)
          II = II + 1
 470    CONTINUE
        II = 1
        DO 480 J=1,JM
         DO 480 I=1,IM
          VSRAW(II) = VS(I,J,KZ)
          II = II + 1
 480    CONTINUE

C       FIELDS ON SIGMA SURFACES

        IF (KZ.LE.9) THEN 
          LVL = 128
        ELSE
          LVL = 108
        ENDIF
C
C AVG TEMPERATURE
C                  
        KPD = 80
        CALL GENPDS(IY,IMY,IDY,IH,IFT,KPD,LVL,PDS,KZ,Z,KB,IMJM,TSRAW,
     #  RBMAP,LAVG,IAVG,NC)
        CALL GENGDS(IGDS,IM,JM)
        CALL GENBDS(PDS,IGDS,TSRAW,IU,IMJM,FIRSTIME,RBMAP,GBNAME,2)
C
C AVG SALINITY
C
        KPD = 88
        CALL GENPDS(IY,IMY,IDY,IH,IFT,KPD,LVL,PDS,KZ,Z,KB,IMJM,SSRAW,
     #  RBMAP,LAVG,IAVG,NC)
        CALL GENGDS(IGDS,IM,JM)
        CALL GENBDS(PDS,IGDS,SSRAW,IU,IMJM,FIRSTIME,RBMAP,GBNAME,2)
C
C AVG U INT MODE
C
        KPD = 49
        CALL GENPDS(IY,IMY,IDY,IH,IFT,KPD,LVL,PDS,KZ,Z,KB,IM*JM,
     #  USRAW,RUMAP,LAVG,IAVG,NC)
        CALL GENGDS(IGDS,IM,JM)
        CALL GENBDS(PDS,IGDS,USRAW,IU,IM*JM,FIRSTIME,RUMAP,GBNAME,2)
C
C AVG V INT MODE
C
        KPD = 50
        CALL GENPDS(IY,IMY,IDY,IH,IFT,KPD,LVL,PDS,KZ,Z,KB,IM*JM,
     #  VSRAW,RVMAP,LAVG,IAVG,NC)
        CALL GENGDS(IGDS,IM,JM)
        CALL GENBDS(PDS,IGDS,VSRAW,IU,IM*JM,FIRSTIME,RVMAP,GBNAME,2)
  450 CONTINUE        
  


C     NOW GET AVERAGED VELOCITIES AT A CONSTANT DEPTH

      CALL SIGTOZ(ZZ,HU,US,ULEV,ZLEV,DUM,DUM1,IM,JM,KB)
      CALL SIGTOZ(ZZ,HV,VS,VLEV,ZLEV,DVM,DVM1,IM,JM,KB)
 
      II = 1
      DO J=1,JM
        DO I=2,IM
          URAW(II) = ULEV(I,J)
          RUMAP(II) = DUM1(I,J)
          II = II + 1
        ENDDO
      ENDDO   
 
      KPD = 49
      LVL = 160  
      ZSAVD=Z(1)              
      Z(1)=ZLEV
      
      CALL GENPDS(IY,IMY,IDY,IH,IFT,KPD,LVL,PDS,KZ,Z,KB,IM*JM,
     &            URAW,RUMAP,LAVG,IAVG,NC)
      CALL GENGDS(IGDS,IM,JM)
      CALL GENBDS(PDS,IGDS,URAW,IU,IM*JM,FIRSTIME,RUMAP,GBNAME,2)

      II = 1
      DO J=2,JM
        DO I=1,IM
          VRAW(II) = VLEV(I,J)
          RVMAP(II) = DVM1(I,J)
          II = II + 1
        ENDDO
      ENDDO    
      
      KPD = 50
      LVL = 160
      CALL GENPDS(IY,IMY,IDY,IH,IFT,KPD,LVL,PDS,KZ,Z,KB,IM*JM,
     &             VRAW,RVMAP,LAVG,IAVG,NC)
      CALL GENGDS(IGDS,IM,JM)
      CALL GENBDS(PDS,IGDS,VRAW,IU,IM*JM,FIRSTIME,RVMAP,GBNAME,2)

      Z(1)=ZSAVD
      GOTO 1010
 1500 CONTINUE      


C             MAX/MIN sea elevations

      
 2010 CONTINUE
        CALL IN3 (23,IFT,
     &            ELMAX,ELMIN,IDGRD,IM,JM,KB,NAVAC,N_HRS,
     &            KEND)
        IF (KEND) GOTO 2500      
        
        II=1
        DO 2300 J = 1,JM
         DO 2300 I = 1, IM
         ES1(II)=ELMAX(I,J)
         II = II + 1
 2300   CONTINUE   

        LAVG=4
        KZ = 1
C
C MAX & MIN SEA ELEVATION
C
        KPD = 131
        LVL = 1
        CALL GENPDS(IY,IMY,IDY,IH,IFT,KPD,LVL,PDS,KZ,Z,KB,IMJM,ES1,
     &              RBMAP,LAVG,IAVG,NAVAC)
        PDS(4)=CHAR(128)
        CALL GENGDS(IGDS,IM,JM)
        CALL GENBDS(PDS,IGDS,ES1,IU,IMJM,FIRSTIME,RBMAP,GBNAME,128)
        
        II=1
        DO 2301 J = 1,JM
         DO 2301 I = 1, IM
         ES1(II)=ELMIN(I,J)
         II = II + 1
 2301   CONTINUE   
        
        KPD = 132
        LVL = 1
        CALL GENPDS(IY,IMY,IDY,IH,IFT,KPD,LVL,PDS,KZ,Z,KB,IMJM,ES1,
     &              RBMAP,LAVG,IAVG,NAVAC)
        CALL GENGDS(IGDS,IM,JM)
        CALL GENBDS(PDS,IGDS,ES1,IU,IMJM,FIRSTIME,RBMAP,GBNAME,128)
        
      GOTO 2010        
 2500 CONTINUE 
 

      STOP
      END
