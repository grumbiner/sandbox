C
      SUBROUTINE DATAPK_INST(IM,JM,KB,IMJM,KB1,gbname)
C
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    DATAPK_INST PACK INST DATA IN GRIB
C   PRGMMR: RIVIN            ORG: W/NMC21    DATE: 00-11-01
C
C ABSTRACT: CALLS ROUTINES TO PACK INST DATA IN GRIB.
C
C PROGRAM HISTORY LOG:
C
C USAGE:    CALL DATAPK_INST(IM,JM,KB,IMJM,KB1,gbname)
C   INPUT ARGUMENT LIST:
C     FT21F001 - yymmddhh.3D
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
     *          TMPLEV(IM,JM),SALLEV(IM,JM),
     *          DUM1(IM,JM),DVM1(IM,JM),DTM1(IM,JM),DSM1(IM,JM)      
      REAL Q2(IM,JM,KB),L(IM,JM,KB)                            
      
      DIMENSION ES(IM,JM),TS(IM,JM,KB),SS(IM,JM,KB),UAS(IM,JM),
     &           VAS(IM,JM),
     &          US(IM,JM,KB),VS(IM,JM,KB)
      CHARACTER*64 HEADER                                       

      DIMENSION UCLIP(IM,JM), VCLIP(IM,JM)
      DIMENSION ELMAX(IM,JM),ELMIN(IM,JM)

      DIMENSION ELB1(IMJM),UB1(IMJM),VB1(IMJM),RBMAP(IMJM),RUMAP(IMJM)
      DIMENSION H1(IMJM),ALAT1(IMJM),ALON1(IMJM),SMASK(IMJM)
      DIMENSION RVMAP(IMJM),RNMAP(IMJM),RTMAP(IMJM),RSMAP(IMJM)
      DIMENSION TRAW(IMJM),SRAW(IMJM),URAW(IMJM),VRAW(IMJM),TKE(IMJM)
      DIMENSION WRAW(IMJM)
      DIMENSION ES1(IMJM),UAS1(IMJM),VAS1(IMJM),TSRAW(IMJM),SSRAW(IMJM)
      DIMENSION USRAW(IMJM),VSRAW(IMJM),ZLEV(25)
      REAL LRAW(IMJM)
      REAL hh,dd,mm,yy
      CHARACTER*1 PDS(28)
      CHARACTER*80 gbname, GRNAME
      INTEGER IGDS(18)
      LOGICAL FIRSTIME,LEND,MEND,FRST2,KEND
      DATA ZLEV/1.0,2.5,5.0,7.5,10.0,12.5,17.5,25.0,32.5,40.0,50.0,
     &  62.5,75.0,100.0,125.0,150.0,200.0,300.0,400.0,500.0,600.0,
     &  700.0,800.0,900.0,1000.0 /
C234567890---------2---------3---------4---------5---------6---------7-}
      IU = 53
      LUN2 = 52
      FIRSTIME=.TRUE.          
      FRST2=.TRUE.               
      LAVG=0
      GRNAME='ecofsgrd.grb'                     
c      ZLEV=1.0
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
C READ IN CFS MODEL VARIABLES FROM BINARY OUTPUT FILE
C             

  10  CONTINUE      !   LOOP ENTRY POINT
      LOOP=LOOP+1    
      
      
      CALL IN1 (21,
     &                IHHF,IDDF,IMMF,IYYF,IFT,
     &                ELB,TB,SB,UAB,VAB,UB,VB,Q2,L,W,
     &                IM,JM,KB,LEND)
                 
      CALL FULLYR (IYYF)           
      CALL FIXDAT (IYYF,IMMF,IDDF,IHHF, IYYREF,IMMREF,IDDREF,IHHREF)
c      CALL UNFULLYR (IYYREF)
      
     
      WRITE (*,*) 'LEVELS: ',Z

      IF (LEND) GOTO 500




      N = N + 1
C     WRITE(80) hh,dd,mm,yy
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


C
      TMAX=TB(1,1,1)
      ELMX=ELB(1,1)
      TMIN=TB(1,1,1)
      ELMN=ELB(1,1)
      DO I = 1,IM
       DO J = 1,JM
        IF (TB(I,J,1).GT.TMAX) TMAX=TB(I,J,1)
        IF (ELB(I,J).GT.ELMX) ELMX=ELB(I,J)
        IF (TB(I,J,1).LT.TMIN) TMIN=TB(I,J,1)
        IF (ELB(I,J).LT.ELMN) ELMN=ELB(I,J)
       END DO
      END DO
      
      IH=IHHREF
      IDY=IDDREF
      IMY=IMMREF
      IY=IYYREF
      WRITE(6,*) 'RECORD NUMBER READ = ',N

      WRITE(6,*) 'YR = ',IY,' MO= ',IMY,' DY= ',IDY,' HR= ',IH,
     &           ' IFT=',IFT

C     WRITE(6,*) 'TMAX= ',TMAX,'TMIN= ',TMIN
C     WRITE(6,*) 'ELMX= ',ELMX,'ELMN= ',ELMN
      II = 1
      OPEN (52,FILE='SMASK.TXT')
      DO J=1,JM
        WRITE (52,'(1X,181I1)') (NINT(FSM(I,J)),I=1,IM)
      ENDDO
      CLOSE (52)  
      DO 300 J = 1,JM
       DO 300 I = 1, IM
       ELB1(II) = ELB(I,J)
       RBMAP(II) = FSM(I,J)
       H1(II)=H(I,J)
       ALON1(II)=ALON(I,J)
       ALAT1(II)=ALAT(I,J)
       SMASK(II)=1.0-FSM(I,J)          
       RNMAP(II)=1.0          ! NULL MASK FOR GRIBCODING THE MASK ITSELF
       II = II + 1
 300  CONTINUE   
      IF (LOOP.EQ.1) THEN
 
C       ------------------ CONSTANT FIELDS -------------------- 
C       ENCODE OCEAN DEPTH, GEOGRAPHIC COORDINATES AND SEA MASK
 
C       OCEAN DEPTH
        KPD = 8
        LVL = 201
        IAVG=0
        CALL GENPDS(IY,IMY,IDY,IH,0,KPD,LVL,PDS,KZ,Z,KB,IMJM,H1,
     &              RBMAP,LAVG,IAVG,NC)     
        CALL GENGDS(IGDS,IM,JM)
        CALL GENBDS(PDS,IGDS,H1,LUN2,IMJM,FRST2,RBMAP,GRNAME,2)
        write (*,*) 'ocean depth'        
        FRST2=.FALSE.

C       LATITUDE
        KPD = 176
        LVL = 201      
        CALL GENPDS(IY,IMY,IDY,IH,0,KPD,LVL,PDS,KZ,Z,KB,IMJM,ALAT1,
     &              RBMAP,LAVG,IAVG,NC)     
        CALL GENGDS(IGDS,IM,JM)
        CALL GENBDS(PDS,IGDS,ALAT1,LUN2,IMJM,FRST2,RNMAP,GRNAME,2)
        write (*,*) 'latitude'        

C       LONGITUDE
        KPD = 177
        LVL = 201      
        CALL GENPDS(IY,IMY,IDY,IH,0,KPD,LVL,PDS,KZ,Z,KB,IMJM,ALON1,
     &              RBMAP,LAVG,IAVG,NC)     
        CALL GENGDS(IGDS,IM,JM)
        CALL GENBDS(PDS,IGDS,ALON1,LUN2,IMJM,FRST2,RNMAP,GRNAME,2)
        write (*,*) 'longitude'        

C       LAND MASK (LAND=1, SEA=0)
        KPD = 81
        LVL = 1      
        CALL GENPDS(IY,IMY,IDY,IH,0,KPD,LVL,PDS,KZ,Z,KB,IMJM,SMASK,
     &              RBMAP,LAVG,IAVG,NC)     
        CALL GENGDS(IGDS,IM,JM)
        CALL GENBDS(PDS,IGDS,SMASK,LUN2,IMJM,FRST2,RNMAP,GRNAME,2)
        write (*,*) 'land mask'        
      ENDIF
      
                                       
      write (*,*) 'DONE WITH CONSTANT FIELDS'
               
C     ------------------ VARIABLE FIELDS ------------------               

C     ELEVATIONS      
      KPD = 82
      LVL = 1
      CALL GENPDS(IY,IMY,IDY,IH,IFT,KPD,LVL,PDS,KZ,Z,KB,IMJM,ELB1,
     &              RBMAP,LAVG,IAVG,NC)     
      CALL GENGDS(IGDS,IM,JM)
      CALL GENBDS(PDS,IGDS,ELB1,IU,IMJM,FIRSTIME,RBMAP,GBNAME,2)
      write (*,*) 'ELEVATIONS'

C     VELOCITIES - BAROTROPIC
      FIRSTIME=.FALSE.

      II=1
      DO 301 J = 1,JM
       DO 301 I = 1,IM
       UB1(II) = UAB(I,J)
       RUMAP(II) = DUM(I,J)
       II = II + 1
 301  CONTINUE
      KPD = 49
      LVL = 201
      CALL GENPDS(IY,IMY,IDY,IH,IFT,KPD,LVL,PDS,KZ,Z,KB,IM*JM,UB1,
     &            RUMAP,LAVG,IAVG,NC)
      CALL GENGDS(IGDS,IM,JM)
      CALL GENBDS(PDS,IGDS,UB1,IU,IM*JM,FIRSTIME,RUMAP,GBNAME,2)
      WRITE (*,*) 'UAB WRITTEN'


C     CALL PRNCRN ('FSM     ',FSM,IM,JM,1,10,JM-50,JM-40)
C     CALL PRNCRN ('DUM     ',DUM,IM,JM,1,10,JM-50,JM-40)
C     CALL PRNCRN ('DVM     ',DVM,IM,JM,1,10,JM-50,JM-40)


      II=1
      DO 302 J = 1,JM
       DO 302 I = 1,IM
       VB1(II) = VAB(I,J)
       RVMAP(II) = DVM(I,J)
       II = II + 1
 302  CONTINUE
      KPD = 50
      LVL = 201
      CALL GENPDS(IY,IMY,IDY,IH,IFT,KPD,LVL,PDS,KZ,Z,KB,IM*JM,VB1,
     &            RVMAP,LAVG,IAVG,NC)
      CALL GENGDS(IGDS,IM,JM)
      CALL GENBDS(PDS,IGDS,VB1,IU,IM*JM,FIRSTIME,RVMAP,GBNAME,2)


      DO 400 KZ = 1,KB1
        II = 1
        DO 310 J = 1,JM
         DO 310 I = 1,IM
          TRAW(II) = TB(I,J,KZ) + 273.16
          SRAW(II) = SB(I,J,KZ)/1000.
          TKE(II) = Q2(I,J,KZ)/2.
          LRAW(II) = L(I,J,KZ)
          WRAW(II) = W(I,J,KZ)
          II = II + 1
 310    CONTINUE
        CALL MNMX (WRAW,IMJM,RN,RX,IN,IX)
        WRITE(*,9007)RN,RX,IN,IX
 9007   FORMAT('  MIN=', E12.5,' MAX=',E12.5,' IMIN=',I5,
     &             ' IMAX=',I5)

        II = 1
        DO 320 J=1,JM
         DO 320 I=1,IM
          URAW(II) = UB(I,J,KZ)
          II = II + 1
 320    CONTINUE
        II = 1
        DO 330 J=1,JM
         DO 330 I=1,IM
          VRAW(II) = VB(I,J,KZ)
          II = II + 1
 330    CONTINUE

C       FIELDS ON SIGMA SURFACES

        IF (KZ.LE.9) THEN 
          LVL = 128
        ELSE
          LVL = 108
        ENDIF

C       TEMPERATURE
        KPD = 80
        CALL GENPDS(IY,IMY,IDY,IH,IFT,KPD,LVL,PDS,KZ,Z,KB,IMJM,TRAW,
     &              RBMAP,LAVG,IAVG,NC)
        CALL GENGDS(IGDS,IM,JM)
        CALL GENBDS(PDS,IGDS,TRAW,IU,IMJM,FIRSTIME,RBMAP,GBNAME,2)
C       SALINITY
        KPD = 88
        CALL GENPDS(IY,IMY,IDY,IH,IFT,KPD,LVL,PDS,KZ,Z,KB,IMJM,SRAW,
     &              RBMAP,LAVG,IAVG,NC)
        CALL GENGDS(IGDS,IM,JM)
        CALL GENBDS(PDS,IGDS,SRAW,IU,IMJM,FIRSTIME,RBMAP,GBNAME,2)
C       TKE
        KPD = 158
        CALL GENPDS(IY,IMY,IDY,IH,IFT,KPD,LVL,PDS,KZ,Z,KB,IMJM,TKE,
     &              RBMAP,LAVG,IAVG,NC)
        CALL GENGDS(IGDS,IM,JM)
        CALL GENBDS(PDS,IGDS,TKE,IU,IMJM,FIRSTIME,RBMAP,GBNAME,2)
        IF (KZ.GT.1) THEN
C       MR LENGTH   - CODE AS "BLACKADAR" SINCE THERE IS NO GENERIC ONE 
C                     IN THE NCEP GRIB DESCRIPTION AS OF 3/1/94
          KPD = 226
          CALL GENPDS(IY,IMY,IDY,IH,IFT,KPD,LVL,PDS,KZ,Z,KB,IMJM,LRAW,
     &              RBMAP,LAVG,IAVG,NC)
          CALL GENGDS(IGDS,IM,JM)
          CALL GENBDS(PDS,IGDS,LRAW,IU,IMJM,FIRSTIME,RBMAP,GBNAME,2)
        ENDIF
C       U INT MODE
        KPD = 49
        CALL GENPDS(IY,IMY,IDY,IH,IFT,KPD,LVL,PDS,KZ,Z,KB,IM*JM,
     &              URAW,RUMAP,LAVG,IAVG,NC)
        CALL GENGDS(IGDS,IM,JM)
        CALL GENBDS(PDS,IGDS,URAW,IU,IM*JM,FIRSTIME,RUMAP,GBNAME,2)
C       V INT MODE
        KPD = 50
        CALL GENPDS(IY,IMY,IDY,IH,IFT,KPD,LVL,PDS,KZ,Z,KB,IM*JM,
     &              VRAW,RVMAP,LAVG,IAVG,NC)
        CALL GENGDS(IGDS,IM,JM)
        CALL GENBDS(PDS,IGDS,VRAW,IU,IM*JM,FIRSTIME,RVMAP,GBNAME,2)
C       VERT VELOCITY
        KPD = 40
        CALL GENPDS(IY,IMY,IDY,IH,IFT,KPD,LVL,PDS,KZ,Z,KB,IMJM,WRAW,
     &              RBMAP,LAVG,IAVG,NC)
        CALL GENGDS(IGDS,IM,JM)
        CALL GENBDS(PDS,IGDS,WRAW,IU,IMJM,FIRSTIME,RBMAP,GBNAME,2)
 400  CONTINUE 
C
C     *********************************
C
C     FIELDS ON CONSTANT DEPTH SURFACES
C
C     *********************************
C
 
C     INPUT:          
C       ZZ(KB)      - vector of sigma values at which the field is given
C       HU(IM,JM)   - bottom depth
C       U(IM,JM,KB) - the field
C       ZLEV        - the required depth to interpolate to
C       IM,JM,KB    - array dimensions
C       FSM(IM,JM)  - model sea mask (common to all sigma surfaces)
C     OUTPUT:
C       ULEV(IM,JM) - target interpolated field
C       FSM1(IM,JM) - sea mask at a given depth
C       
C     Calc. midlayer heights
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
                                    
c    added on Feb 7 1997:
c    the sigtoz interpolation for salinity and temp
c    also added 10 levels and hence the loop.
c    for the Z levels check the data statement for ZLEV.
c                                   
      do iji=1,25
      ZZLEV=ZLEV(iji)
      CALL SIGTOZ(ZZ,HU,UB,ULEV,ZZLEV,DUM,DUM1,IM,JM,KB)
      CALL SIGTOZ(ZZ,HV,VB,VLEV,ZZLEV,DVM,DVM1,IM,JM,KB)
      CALL SIGTOZ(ZZ,H,TB,TMPLEV,ZZLEV,FSM,DTM1,IM,JM,KB)
      CALL SIGTOZ(ZZ,H,SB,SALLEV,ZZLEV,FSM,DSM1,IM,JM,KB)
 
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
      Z(1)=ZLEV(iji)
      
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

      II = 1
      DO J=2,JM
        DO I=1,IM
          TRAW(II) = TMPLEV(I,J) + 273.16
          RTMAP(II) = DTM1(I,J)
          II = II + 1
        ENDDO
      ENDDO   

      KPD = 80
      LVL = 160
      CALL GENPDS(IY,IMY,IDY,IH,IFT,KPD,LVL,PDS,KZ,Z,KB,IM*JM,
     &             TRAW,RTMAP,LAVG,IAVG,NC)
      CALL GENGDS(IGDS,IM,JM)
      CALL GENBDS(PDS,IGDS,TRAW,IU,IM*JM,FIRSTIME,RTMAP,GBNAME,2)

      II = 1
      DO J=2,JM
        DO I=1,IM
          SRAW(II) = SALLEV(I,J)/1000.0
          RSMAP(II) = DSM1(I,J)
          II = II + 1
        ENDDO
      ENDDO   

      KPD = 88
      LVL = 160
      CALL GENPDS(IY,IMY,IDY,IH,IFT,KPD,LVL,PDS,KZ,Z,KB,IM*JM,
     &             SRAW,RSMAP,LAVG,IAVG,NC)
      CALL GENGDS(IGDS,IM,JM)
      CALL GENBDS(PDS,IGDS,SRAW,IU,IM*JM,FIRSTIME,RSMAP,GBNAME,2)
 
      enddo
      GO TO 10
 500  CONTINUE


      RETURN
      END
