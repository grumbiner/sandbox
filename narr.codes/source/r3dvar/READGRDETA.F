      SUBROUTINE READGRDETA
C
C     THIS SUBROUTINE READS IN THE AIR FORCE SNOWDEPTH ON THE 32-KM ETA
C     GRID AND DISTRIBUTES TO ALL THE NODES TO COMPARE WITH THE EDAS
C     SNOW TO BE READ IN VIA READ_RESTRT2.
C
C     IT WILL ALSO READ IN THE SEA-ICE, THE SSTs, THE ALBEDO, AND
C     THE GREENNESS FRACTION.
C
C     PERRY SHAFRAN - 27 NOVEMBER 2002
C
      INCLUDE "mpif.h"
      INCLUDE "PARMETA.comm"
      INCLUDE "PARMTBL.comm"
      INCLUDE "parmsoil"
      INCLUDE "mpp.h"

!                          P A R A M E T E R
!    & (LP1=LM+1,JAM=6+2*(JM-10))

      INCLUDE "PVRBLS.comm"
      INCLUDE "CTLBLK.comm"
      INCLUDE "PHYS.comm"
      INCLUDE "SOIL.comm"
      INCLUDE "MASKS.comm"

      DIMENSION HGT(IM,JM)
      INTEGER IYR
      LOGICAL*1 BIT(IM,JM)
      INTEGER JPDS(25), JGDS(22), KGDS(22), KPDS(25)

      INTEGER VEGTYP

      INTEGER MAX_VEGTYP               !!! from REDPRM (in SFLX.F)
      PARAMETER (MAX_VEGTYP   = 30)    !!! from REDPRM (in SFLX.F)
      REAL    SNUPX(MAX_VEGTYP)
      REAL    SNUP
      DATA SNUPX  /0.040, 0.040, 0.040, 0.040, 0.040, 0.040,
     *             0.020, 0.020, 0.020, 0.020, 0.013, 0.020,
     *             0.013, 0.000, 0.000, 0.000, 0.000, 0.000,
     *             0.000, 0.000, 0.000, 0.000, 0.000, 0.000,
     *             0.000, 0.000, 0.000, 0.000, 0.000, 0.000/

      REAL SALP, SALP_DATA             !!! from REDPRM (in SFLX.F)
c      DATA SALP_DATA /2.6/             !!! from REDPRM (in SFLX.F)
      DATA SALP_DATA /4.0/             !!! from REDPRM (in SFLX.F)
C--------------SURFACE DATA---------------------------------------------
C from CNSTS.f ( GRDETA )
C
      REAL TI0
                             D A T A
     & TI0/271.16/

      LOGICAL L12Z
      NAMELIST /UPSNOW/ L12Z

      DATA LOROG/41/
C
C***********************************************************************
C     START READGRDETA HERE.
C
      IF (MYPE.EQ.0) THEN
         WRITE(*,*) " ************************************** "
         WRITE(*,*) " READGRDETA ", IDAT,IHRST
         WRITE(*,*) " ************************************** "
         WRITE(0,*) " ************************************** "
         WRITE(0,*) " READGRDETA ", IDAT,IHRST
         WRITE(0,*) " ************************************** "
      END IF
C
      SALP   = SALP_DATA
!
!   get eta model guess year, month, day, hour
        call eta_ymdh(ietay,ietam,ietad,ietah)
!
C
C Namelist UPSNOW has .TRUE. for 12UTC and .FALSE. otherwise
C
      READ(10,UPSNOW)
      IF (MYPE.EQ.0) THEN
      WRITE(*,UPSNOW)
      WRITE(0,UPSNOW)
      END IF
C
C Read the truly-fixed land-mask into array SM from the Eta "orography" file for the
C (0.0=landmass,1.0=sea, both unfrozen & frozen sea).
C Note: SM mask convention will be changed below after SICE is read and applied.
C
      IF(MYPE.EQ.0) THEN
        REWIND LOROG
        READ(LOROG)HGT,TEMP1
      ENDIF
      CALL DSTRB(TEMP1,SM,1,1,1)
C
C
C Read the daily sea-ice analysis into array SICE, on the native Eta grid:
C SICE (1.0 = sea ice, 0.0 = open sea or land mass).
C
      IF(MYPE.EQ.0) THEN
        CALL BAOPENR(43,'fort.43',IRET)
        IF (IRET.NE.0) THEN
           WRITE(0,*)'BAOPENR CAN NOT OPEN UNIT 43 IRET=',IRET
           CALL MPI_ABORT(MPI_COMM_WORLD,1,IERR)
           STOP
        END IF
        WRITE(0,*) 'BAOPENR ON UNIT 43', ' IRET=', IRET
C
        JPDS=-1


        WRITE(0,*) 'ietay=',ietay
        IF(ietay.GE.2000) THEN
          IYR=ietay-2000
        ELSE
          IYR=ietay-1900
        ENDIF
        WRITE(0,*) 'IYR=',IYR
        JPDS(8)=IYR
        JPDS(9)=ietam
        JPDS(10)=ietad
        CALL GETGB(43,0,IM*JM,0,JPDS,JGDS,KF,K,KPDS,KGDS,BIT,TEMP1,IRET)
        WRITE(0,11) IRET,KF,KPDS(5),
     &         (KPDS(21)*100+KPDS(8))/100-1, MOD(KPDS(8),100),KPDS(9),
     &          KPDS(10)
11      FORMAT('IRET=',I3,' KF=', I6,' FLD=', I3,
     &     2X,'SEA ICE',1X, 4i2.2)
        IF (IRET.NE.0) THEN
           WRITE(0,*)"JPDS(8),JPDS(9),JPDS(10)",JPDS(8),JPDS(9),JPDS(10)
           CALL MPI_ABORT(MPI_COMM_WORLD,1,IERR)
           STOP
        END IF
      ENDIF
C
C DISTRIBUTE SEA ICE TO ALL THE NODES
C
      CALL DSTRB(TEMP1,SICE,1,1,1)


C
C Read the daily SST analysis into array SST, on the native Eta grid:
C SST (value 271.16 or greater everywhere, including sea, sea-ice, and land mass.)
C
      IF(MYPE.EQ.0) THEN
        CALL BAOPENR(44,'fort.44',IRET)
        IF (IRET.NE.0) THEN
           WRITE(0,*)'BAOPENR CAN NOT OPEN UNIT 44 IRET=',IRET
           CALL MPI_ABORT(MPI_COMM_WORLD,1,IERR)
           STOP
        END IF
        WRITE(0,*) 'BAOPENR ON UNIT 44', ' IRET=', IRET
C
        JPDS=-1
        JPDS(8)=IYR
        JPDS(9)=ietam
        JPDS(10)=ietad
        CALL GETGB(44,0,IM*JM,0,JPDS,JGDS,KF,K,KPDS,KGDS,BIT,TEMP1,IRET)
        WRITE(0,12) IRET,KF,KPDS(5),
     &         (KPDS(21)*100+KPDS(8))/100-1, MOD(KPDS(8),100),KPDS(9),
     &          KPDS(10)
12      FORMAT('IRET=',I3,' KF=', I6,' FLD=', I3,
     &     2X,'SST',1X, 4i2.2)
        IF (IRET.NE.0) THEN
           WRITE(0,*)"JPDS(8),JPDS(9),JPDS(10)",JPDS(8),JPDS(9),JPDS(10)
           CALL MPI_ABORT(MPI_COMM_WORLD,1,IERR)
           STOP
        END IF
      ENDIF
C
C DISTRIBUTE SST TO ALL THE NODES
C
      CALL DSTRB(TEMP1,SST,1,1,1)
C
C Read the daily vegetation greenness fraction (0-1), on the native Eta grid:
C VEGFRC (positive number over land mass, zero over non-land mass)
C
      IF(MYPE.EQ.0) THEN
        CALL BAOPENR(46,'fort.46',IRET)
        IF (IRET.NE.0) THEN
           WRITE(0,*)'BAOPENR CAN NOT OPEN UNIT 46 IRET=',IRET
           CALL MPI_ABORT(MPI_COMM_WORLD,1,IERR)
           STOP
        END IF
        WRITE(0,*) 'BAOPENR ON UNIT 46', ' IRET=', IRET
C
        JPDS=-1
C       JPDS(8)=IYR
        JPDS(9)=ietam
        JPDS(10)=ietad
        IF (JPDS(9).EQ.2.AND.JPDS(10).EQ.29) JPDS(10)=28   ! dusan (2003/05/06)
        CALL GETGB(46,0,IM*JM,0,JPDS,JGDS,KF,K,KPDS,KGDS,BIT,TEMP1,IRET)
        WRITE(0,14) IRET,KF,KPDS(5),
     &         (KPDS(21)*100+KPDS(8))/100-1, MOD(KPDS(8),100),KPDS(9),
     &          KPDS(10)
14      FORMAT('IRET=',I3,' KF=', I6,' FLD=', I3,
     &     2X,'GREENNESS FRACTION',1X, 4I2.2)
        IF (IRET.NE.0) THEN
           WRITE(0,*)"JPDS(8),JPDS(9),JPDS(10)",JPDS(8),JPDS(9),JPDS(10)
           CALL MPI_ABORT(MPI_COMM_WORLD,1,IERR)
           STOP
        END IF
      ENDIF
C
C DISTRIBUTE VEGFRC TO ALL THE NODES
C
      CALL DSTRB(TEMP1,VEGFRC,1,1,1)
C
C
C Read in daily field of snow-free albedo fraction (0-1), on the native Eta grid:
C ALBASE (positive number over land mass, zero over non-land mass).
C
      IF(MYPE.EQ.0) THEN
        CALL BAOPENR(45,'fort.45',IRET)
        IF (IRET.NE.0) THEN
           WRITE(0,*)'BAOPENR CAN NOT OPEN UNIT 45 IRET=',IRET
           CALL MPI_ABORT(MPI_COMM_WORLD,1,IERR)
           STOP
        END IF
        WRITE(0,*) 'BAOPENR ON UNIT 45', ' IRET=', IRET
C
        JPDS=-1
C       JPDS(8)=IYR
        JPDS(9)=ietam
        JPDS(10)=ietad
        IF (JPDS(9).EQ.2.AND.JPDS(10).EQ.29) JPDS(10)=28   ! dusan (2003/05/06)
        CALL GETGB(45,0,IM*JM,0,JPDS,JGDS,KF,K,KPDS,KGDS,BIT,TEMP1,IRET)
        WRITE(0,13) IRET,KF,KPDS(5),
     &         (KPDS(21)*100+KPDS(8))/100-1, MOD(KPDS(8),100),KPDS(9),
     &          KPDS(10)
13      FORMAT('IRET=',I3,' KF=', I6,' FLD=', I3,
     &     2X,'ALBEDO',1X, 4I2.2)
        IF (IRET.NE.0) THEN
           WRITE(0,*)"JPDS(8),JPDS(9),JPDS(10)",JPDS(8),JPDS(9),JPDS(10)
           CALL MPI_ABORT(MPI_COMM_WORLD,1,IERR)
           STOP
        END IF
      ENDIF
C
C DISTRIBUTE ALBASE TO ALL THE NODES
C
      CALL DSTRB(TEMP1,ALBASE,1,1,1)
C
      IF (L12Z) THEN
         IF (MYPE.EQ.0) THEN
         WRITE(*,*) " ************************************** "
         WRITE(*,*) " READGRDETA L12Z ", L12Z, IDAT,IHRST
         WRITE(*,*) " ************************************** "
         WRITE(0,*) " ************************************** "
         WRITE(0,*) " READGRDETA L12Z ", L12Z, IDAT,IHRST
         WRITE(0,*) " ************************************** "
         END IF
C
C Read the daily snow depth analysis, on the native Eta grid:
C SI (snow depth in meters over land mass, 0.0 = zero snow over land or non-land).
C
      IF(MYPE.EQ.0) THEN
        CALL BAOPENR(42,'fort.42',IRET)
        IF (IRET.NE.0) THEN
           WRITE(0,*)'BAOPENR CAN NOT OPEN UNIT 42 IRET=',IRET
           CALL MPI_ABORT(MPI_COMM_WORLD,1,IERR)
           STOP
        END IF
        WRITE(0,*) 'BAOPENR ON UNIT42', ' IRET=', IRET
C
C READ AF SNOW
C
        JPDS=-1
        IF(ietay.GE.2000) THEN
          IYR=ietay-2000
        ELSE
          IYR=ietay-1900
        ENDIF
        JPDS(8)=IYR
        JPDS(9)=ietam
        JPDS(10)=ietad
        CALL GETGB(42,0,IM*JM,0,JPDS,JGDS,KF,K,KPDS,KGDS,BIT,TEMP1,IRET)
        WRITE(0,10) IRET,KF,KPDS(5),
     &         (KPDS(21)*100+KPDS(8))/100-1, MOD(KPDS(8),100),KPDS(9),
     &          KPDS(10)
10      FORMAT('IRET=',I3,' KF=', I6,' FLD=', I3,
     &     2X,'AF SNOW',1X, 4I2.2)
        IF (IRET.NE.0) THEN
           WRITE(0,*)"JPDS(8),JPDS(9),JPDS(10)",JPDS(8),JPDS(9),JPDS(10)
           CALL MPI_ABORT(MPI_COMM_WORLD,1,IERR)
           STOP
        END IF
      ENDIF
C
C DISTRIBUTE AFSI TO ALL THE NODES
C
      CALL DSTRB(TEMP1,AFSI,1,1,1)

      ENDIF    !!! IF (L12Z) THEN

C
C     UPDATE LOGIC
C
C     Now loop over the entire Eta native grid (landmass and non-land mass) and
C     separately treat the following three surface types:
C     sea ice (frozen sea),
C     open sea (unfrozen sea),
C     land mass (snow and albedo update). 

      DO J=JDIM1,JDIM2
      DO I=IDIM1,IDIM2
C
C NOTE: AT THIS POINT SM IS LANDMASS MASK, I.E.
C  (LANDMASS: SM=0, NON-LANDMASS, SM=1)
C
      IF (SM(I,J) .GT. 0.5) THEN
C
C NON LAND (OPEN SEA OR SEA ICE)
C
        IF (SICE(I,J) .GT. 0.5) THEN
C
C THIS IS SEA-ICE
C
C ASIDE: SEE FINAL COMMENT LINES AT BOTTOM OF THIS DOUBLE 
C DOUBLE DO-LOOP FOR DISCUSSION OF SEA-ICE SKIN TEMPERATURE
C TREATMENT IMPLIED BY THE LOGIC HERE
C 
           SM(I,J)        = 0.0
C           SICE(I,J)      = 1.0 (this stays as already read-in)

           ALBASE(I,J)    = 0.0
           VEGFRC(I,J)    = 0.0
C
           EPSR(I,J)      = 0.97
C
           SI(I,J)        = 0.10
           SNO(I,J)       = SI(I,J) * 0.10   !! RR assumes 1:10 ratio
C
           ALBEDO(I,J)    = 0.65   ! albedo over sea-ice (65%)
C
           CMC(I,J)       = 0.0

C if this point was sea-ice previous cycle let temp. profile cycle,
C otherwise define initial sea-ice pack temperature profile
           IF ( TG(I,J) .GT. 271.16) THEN   ! this point was open-sea previous cycle
           TG(I,J)        = TI0    ! TI0 = 271.16
           DO NS=1,NSOIL
              STC(I,J,NS)    = 269.16
              SMC(I,J,NS)    = 1.0
              SH2O(I,J,NS)   = 1.0
           ENDDO
           END IF
C                                                
        ELSE
C
C THIS IS OPEN SEA (NO SEA ICE)
C
C            SST(I,J)      = as already readin (Dusan:how does this make its way to THS?)
C
C            SM(I,J)       = 1.0 (this stays as already read-in)
C            SICE(I,J)     = 0.0 (this stays as already read-in)
C
            ALBASE(I,J)   = 0.0
            VEGFRC(I,J)   = 0.0
C
            EPSR(I,J)     = 0.97
C
            SI(I,J)       = 0.0
            SNO(I,J)      = 0.0
C
            ALBEDO(I,J)   = 0.06
C
            CMC(I,J)      = 0.0
            TG(I,J)       = 280.99
            DO NS=1,NSOIL
               STC(I,J,NS)   = 273.16
               SMC(I,J,NS)   = 1.0
               SH2O(I,J,NS)  = 1.0
            ENDDO
        ENDIF
C
      ELSE
C
C THIS IS LAND MASS
C
C            THS(I,J)        =     SKIN TEMP (THS/APES) CYCLED IN RESTART FILE
C
C            SM(I,J)         =    0.0 (this stays as already read-in)
C            SICE(I,J)       =    0.0 (this stays as already read-in)
C
C            ALBASE(I,J)    = (this stays as already read-in)
C            VEGFRC(I,J)    = (this stays as already read-in)
C
            EPSR(I,J) = 1.0
C
            IF (L12Z) THEN
C
C UPDATE SNOW AND ALBEDO ON 12Z CYCLE ONLY
C
C
C FIXME  what for undefined values !!!!!!!!!!!!!!!!!!
C
        IF (AFSI(I,J).LT.900.0) THEN

        IF (AFSI(I,J).EQ.0.0) THEN
          SI(I,J)=0.0
          SNO(I,J)=0.0
        ELSE
          IF (SI(I,J).EQ.0.0) THEN
            SI(I,J)=0.5*AFSI(I,J)
            SNO(I,J)=0.1*SI(I,J)
          ELSE
            IF (SI(I,J).GE.0.5*AFSI(I,J) .AND.
     &          SI(I,J).LE.2.0*AFSI(I,J)) THEN
              SIINC=0.0
            ELSE
              SIP=SI(I,J)
              SDENSP=SNO(I,J)/SI(I,J)
              IF (SI(I,J).LT.0.5*AFSI(I,J) ) THEN
                SIINC=SI(I,J)-0.5*AFSI(I,J)
                SI(I,J)=0.5*AFSI(I,J)
              ELSE IF (SI(I,J).GT.2.0*AFSI(I,J)) THEN
                SIINC=SI(I,J)-2.0*AFSI(I,J)
                SI(I,J)=2.0*AFSI(I,J)
              END IF
              SIRATIO=SI(I,J)/SIP
              IF (SIRATIO.LT.1.0) THEN
                SDENSN=(1.0-SIRATIO)*0.1 + SIRATIO*SDENSP
              ELSE IF (SIRATIO.GE.1.0 .AND. SIRATIO.LT.2.0) THEN
                SDENSN=SDENSP+(SIRATIO-1.0)/(2.0-1.0)*(0.1-SDENSP)
              ELSE
                SDENSN=0.1
              END IF
              SNO(I,J)=SDENSN*SI(I,J)
            END IF
          END IF
        END IF

        IF (SNO(I,J).NE.0.0 .AND. SI(I,J).EQ.0.0) then
           WRITE(*,*) " I,J, SNO(I,J), SI(I,J)",
     &                  I,J, SNO(I,J), SI(I,J)
           WRITE(0,*) " I,J, SNO(I,J), SI(I,J)",
     &                  I,J, SNO(I,J), SI(I,J)
           CALL MPI_ABORT(MPI_COMM_WORLD,1,IERR)
           STOP
        END IF

        ENDIF

C  UPDATE ALBEDO 
        IF ((SNO(I,J) .EQ. 0.0) .OR.
     .           (ALBASE(I,J) .GE. MXSNAL(I,J) ) ) THEN
           ALBEDO(I,J) = ALBASE(I,J)
        ELSE
           VEGTYP = IVGTYP(I,J)
           SNUP = SNUPX(VEGTYP)

C MODIFY ALBEDO IF SNOWCOVER:
C BELOW SNOWDEPTH THRESHOLD...
           IF (SNO(I,J) .LT. SNUP) THEN
               RSNOW = SNO(I,J)/SNUP
               SNOFAC = 1. - ( EXP(-SALP*RSNOW) - RSNOW*EXP(-SALP))
               ALBEDO(I,J)=ALBASE(I,J)+SNOFAC*(MXSNAL(I,J)-ALBASE(I,J))
C ABOVE SNOWDEPTH THRESHOLD...
           ELSE
               ALBEDO(I,J) = MXSNAL(I,J)
           ENDIF

        ENDIF

        END IF  !!! IF (L12Z) THEN
C
C  AS WE ARE OVER LAND HERE, WE RECALL THAT
C            TIK    = CYCLED VIA RESTART FILE (VIA THS, T1K=THS/APES)
C            CMC     = CYCLED VIA RESTART FILE
C            TG     = CYCLED VIA RESTART FILE
C            STC    = CYCLED VIA RESTART FILE    
C            SMC    = CYCLED VIA RESTART FILE
C            SH2O    = CYCLED VIA RESTART FILE
C
      ENDIF
C
      ENDDO
      ENDDO
C
C SEA-ICE SKIN TEMPERATURE TREATMENT IMPLIED BY LOGIC IN 
C DOUBLE DO LOOP ABOVE
C
C BY DESIGN, IF NEW SEA ICE JUST EMERGED AT THIS POINT 
C (I.E. SICE=0 PREVIOUS CYCLE), THE SKIN TEMPERATURE WILL START
C WITH SST FROM PREVIOUS CYLE (WHICH IS OK, AS THIS SST VALUE IS
C LIKELY NEAR SEA ICE FREEZING POINT) AND THEN THIS TEMPERATURE
C WILL COMMENCE CONTINOUSLY CYCLING VIA RESTART FILE
C ACCORDING TO SURFACE ENERGY BUDGET PHYSICS IN ROUTINE SFLX,
C UNTIL THIS POINT LATER BECOMES ICE FREE AGAIN, AT WHICH TIME
C THE SST AGAIN DEFINES THE SURFACE TEMPERATURE.  THE SFC SKIN
C TEMP OVER OPEN SEA, SEA-ICE, AND LAND MASS (INCLUDING SNOW) IS
C EMBODIED IN THE "THS" ARRAY IN THE RESTART FILE.  THS IS
C ACTUALLY THE POTENTIAL SURFACE SKIN TEMPERATURE, WHICH IS
C CONVERTED TO ACTUAL SKIN TEMPERATURE WHEN NEEDED 
C (E.G. IN ROUTINE SURFCE, WHERE SKIN TEMP T1K=THS/APES, WHERE
C APES IS EXNER'S FUNCTION, INCLUDING SURFACE PRESSURE AND R/CP)
C
      RETURN
      END
