C     
      SUBROUTINE DATAPK_SFC(IM,JM,KB,IMJM,KB1,gbname)
C
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    DATAPK_AVG  PACK SFC DATA IN GRIB
C   PRGMMR: RIVIN            ORG: W/NMC21    DATE: 00-11-01
C
C ABSTRACT: CALLS ROUTINES TO PACK SFC DATA IN GRIB.
C
C PROGRAM HISTORY LOG:
C
C USAGE:    CALL DATAPK_SFC(IM,JM,KB,IMJM,KB1,gbname)
C   INPUT ARGUMENT LIST:
C     FT21F001 - yymmddhh.2D 
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

C ---------------------  HOURLY 2D FIELDS --------------------  
      READ(21) VERSTR
 3010 CONTINUE      
      CALL IN4 (21,IFT,ELB,ULEV,VLEV,ZLEV,DUM1,DVM1,LEND,IM,JM)
      IF (LEND) GOTO 3500
                            
      II=1
      DO 3300 J = 1,JM
       DO 3300 I = 1, IM
       ELB1(II) = ELB(I,J)
       RBMAP(II) = FSM(I,J)
       II = II + 1
 3300 CONTINUE   
                                                                       
C     ELEVATIONS      
      KPD = 82
      LVL = 1                      
      LAVG=0
      write (*,*) 'INST. ELEVATIONS'
      write (*,*) IY,IMY,IDY,IH,IFT
      CALL GENPDS(IY,IMY,IDY,IH,IFT,KPD,LVL,PDS,KZ,Z,KB,IMJM,ELB1,
     &              RBMAP,LAVG,IAVG,NC)     
      CALL GENGDS(IGDS,IM,JM)
      CALL GENBDS(PDS,IGDS,ELB1,IU,IMJM,FIRSTIME,RBMAP,GBNAME,2)
      FIRSTIME=.FALSE.      
      II = 1
      DO J=1,JM
        DO I=1,IM
          URAW(II) = ULEV(I,J)
          RUMAP(II) = DUM1(I,J)
          II = II + 1
        ENDDO
      ENDDO   
           
      KPD = 49
      LVL = 160                
      Z(1)=ZLEV
      CALL GENPDS(IY,IMY,IDY,IH,IFT,KPD,LVL,PDS,KZ,Z,KB,IM*JM,
     &            URAW,RUMAP,LAVG,IAVG,NC)
      CALL GENGDS(IGDS,IM,JM)
      CALL GENBDS(PDS,IGDS,URAW,IU,IM*JM,FIRSTIME,RUMAP,GBNAME,2)

      II = 1
      DO J=1,JM
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
      GOTO 3010
 3500 CONTINUE      


      STOP
      END
