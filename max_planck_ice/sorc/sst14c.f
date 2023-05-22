      PROGRAM sst14
C     Read in the 14 km sst's (1/8 degree) being produced by
C       Chris Peters for the ETA model, for use by the 
C       Great Lakes ice model.

      IMPLICIT none
      INCLUDE "icegrid.glk"

      INTEGER nx, ny
      PARAMETER (nx = 1041)
      PARAMETER (ny =  441)
      INTEGER onx, ony
      PARAMETER (onx = 50*8+1)
      PARAMETER (ony = 30*8+1)

      REAL sst(nx, ny)
      REAL sstout(onx, ony)
      REAL sst2out(LP, MP)
      LOGICAL lbm(onx, ony)

      REAL firstlat, firstlon, dlat1, dlon1
      PARAMETER (firstlat = 10.0)
      PARAMETER (firstlon = -165.0)
      PARAMETER (dlat1 = 1./8.)
      PARAMETER (dlon1 = 1./8.)

      INTEGER i, j, k, li 
      CHARACTER grib((100 + 28 + nx*ny*(8+1))/8 )
      INTEGER lgrib, ierr
      INTEGER yy, mon, dd
      CHARACTER*6 tag

C     Read in the whole grid
      DO 1000 i = 1, nx
        READ (10) (sst(i,j),j=1,ny)
 1000 CONTINUE

C     Pull out a 50*30 degree subset (95 W to 45 W, 35 N to 65 N)
      DO 2000 j = 1, ony
        li = j + (35. - firstlat) / dlat1
        DO 2100 i = 1, onx
          k = i + (-95. - firstlon ) / dlon1
          sstout(i,j) = sst(k, li)
 2100   CONTINUE
 2000 CONTINUE

C     Write out a grib file
      READ (*, 9001) tag
 9001 FORMAT (A6)
      READ (tag, 9002) yy, mon, dd
 9002 FORMAT (I2, I2, I2)
CD      WRITE (11) sstout

      CALL gribit(sstout, lbm, 0, onx, ony, 16, 0.0, 28, 1, 7, 0, 0, 80, 
     1     102, 0, 0, yy, mon, dd, 0, 1,
     2     0, 0, 10, 0, 0, 2, 
C     Last argument is power in multiplying (data)*10**x prior to gribbing.
     3     35., -95., 
     4     35.+30., -95. + 50., 
     5     dlon1, dlat1, 0, -10., grib, lgrib, ierr) 

      PRINT *,'ierr = ',ierr
      PRINT *,'lgrib = ',lgrib
      IF (ierr .EQ. 0) THEN
        CALL WRYTE(12, lgrib, grib)
      ENDIF

      CALL terph(sstout, sst2out, .FALSE.)
      CALL gribit(sst2out, lbm, 0, LP, MP, 16, 0.0, 28, 1, 7, 0, 0, 80, 
     1     102, 0, 0, yy, mon, dd, 0, 1,
     2     0, 0, 10, 0, 0, 2, 
C     Last argument is power in multiplying (data)*10**x prior to gribbing.
     3     latmin, lonmin,
     4     latmin+M*dlat, lonmin+L*dlon,
     5     dlon, dlat, 0, -10., grib, lgrib, ierr) 

      PRINT *,'ierr = ',ierr
      PRINT *,'lgrib = ',lgrib
      IF (ierr .EQ. 0) THEN
        CALL WRYTE(13, lgrib, grib)
      ENDIF


     
      STOP
      END
C-----------------------------------------------------------------------
      SUBROUTINE GRIBIT(F,LBM,IDRT,IM,JM,MXBIT,COLAT1,
     &                  ILPDS,IPTV,ICEN,IGEN,IBMS,IPU,ITL,IL1,IL2,
     &                  IYR,IMO,IDY,IHR,IFTU,IP1,IP2,ITR,INA,INM,IDS,
     &                  XLAT1,XLON1,XLAT2,XLON2,DELX,DELY,ORTRU,PROJ,
     &                  GRIB,LGRIB,IERR)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    GRIBIT      CREATE GRIB MESSAGE
C   PRGMMR: IREDELL          ORG: W/NMC23    DATE: 92-10-31
C   Modified by R. Grumbine       W/NMC21    DATE: 95-06-29
C
C ABSTRACT: CREATE A GRIB MESSAGE FROM A FULL FIELD.
C   AT PRESENT, ONLY GLOBAL LATLON GRIDS AND GAUSSIAN GRIDS ARE ALLOWED.
C
C PROGRAM HISTORY LOG:
C   92-10-31  IREDELL
C   94-05-04  JUANG (FOR GSM AND RSM USE)
C   95-06-29  Grumbine (arbitrary lat-long grids)
C
C USAGE:    CALL GRIBIT(F,LBM,IDRT,IM,JM,MXBIT,COLAT1,
C    &                  ILPDS,IPTV,ICEN,IGEN,IBMS,IPU,ITL,IL1,IL2,
C    &                  IYR,IMO,IDY,IHR,IFTU,IP1,IP2,ITR,INA,INM,IDS,
C    &                  XLAT1,XLON1,XLAT2,XLON2,DELX,DELY,ORTRU,PROJ,
C    &                  GRIB,LGRIB,IERR)
C   INPUT ARGUMENT LIST:
C     F        - REAL (IM*JM) FIELD DATA TO PACK INTO GRIB MESSAGE
C     LBM      - LOGICAL (IM*JM) BITMAP TO USE IF IBMS=1
C     IDRT     - INTEGER DATA REPRESENTATION TYPE
C                (0 FOR LATLON OR 4 FOR GAUSSIAN)
C     IM       - INTEGER LONGITUDINAL DIMENSION
C     JM       - INTEGER LATITUDINAL DIMENSION
C     MXBIT    - INTEGER MAXIMUM NUMBER OF BITS TO USE (0 FOR NO LIMIT)
C     COLAT1   - REAL FIRST COLATITUDE OF GAUSSIAN GRID IF IDRT=4
C     ILPDS    - INTEGER LENGTH OF THE PDS (USUALLY 28)
C     IPTV     - INTEGER PARAMETER TABLE VERSION (USUALLY 1)
C     ICEN     - INTEGER FORECAST CENTER (USUALLY 7)
C     IGEN     - INTEGER MODEL GENERATING CODE
C     IBMS     - INTEGER BITMAP FLAG (0 FOR NO BITMAP)
C     IPU      - INTEGER PARAMETER AND UNIT INDICATOR
C     ITL      - INTEGER TYPE OF LEVEL INDICATOR
C     IL1      - INTEGER FIRST LEVEL VALUE (0 FOR SINGLE LEVEL)
C     IL2      - INTEGER SECOND LEVEL VALUE
C    &                  IYR,IMO,IDY,IHR,IFTU,IP1,IP2,ITR,INA,INM,IDS,
C    &                  GRIB,LGRIB,IERR)
C     IYR      - INTEGER YEAR
C     IMO      - INTEGER MONTH
C     IDY      - INTEGER DAY
C     IHR      - INTEGER HOUR
C     IFTU     - INTEGER FORECAST TIME UNIT (1 FOR HOUR)
C     IP1      - INTEGER FIRST TIME PERIOD
C     IP2      - INTEGER SECOND TIME PERIOD (0 FOR SINGLE PERIOD)
C     ITR      - INTEGER TIME RANGE INDICATOR (10 FOR SINGLE PERIOD)
C     INA      - INTEGER NUMBER INCLUDED IN AVERAGE
C     INM      - INTEGER NUMBER MISSING FROM AVERAGE
C     IDS      - INTEGER DECIMAL SCALING
C    &                  XLAT1,XLON1,DELX,DELY,ORTRU,PROJ,
C     XLAT1    - FIRST POINT OF REGIOANL LATITUDE
C     XLON1    - FIRST POINT OF REGIONAL LONGITUDE
C     XLAT2    - LAST POINT OF REGIOANL LATITUDE
C     XLON2    - LAST POINT OF REGIONAL LONGITUDE
C     DELX     - DX IN METER ON 60N FOR REGIONAL
C     DELY     - DY IN METER ON 60N FOR REGIONAL
C     PROJ     - POLAR PROJECTION FLAG 0 FOR NORTH 128 FOR SOUTH
C     ORTRU   - ORIENTATION OF LONGITUDE FOR POLAR PROJECTION
C                OR TRUTH OF LATITUDE FOR MERCATER PROJECTION
C
C   OUTPUT ARGUMENT LIST:
C     GRIB     - CHARACTER (LGRIB) GRIB MESSAGE
C     LGRIB    - INTEGER LENGTH OF GRIB MESSAGE
C                (NO MORE THAN 100+ILPDS+IM*JM*(MXBIT+1)/8)
C     IERR     - INTEGER ERROR CODE (0 FOR SUCCESS)
C
C SUBPROGRAMS CALLED:
C   GTBITS     - COMPUTE NUMBER OF BITS AND ROUND DATA APPROPRIATELY
C   W3FI72     - ENGRIB DATA INTO A GRIB1 MESSAGE
C
C ATTRIBUTES:
C   LANGUAGE: CRAY FORTRAN
C
C$$$
      REAL F(IM*JM)
      LOGICAL LBM(IM*JM)
      CHARACTER GRIB(*)
      INTEGER IBM(IM*JM*IBMS+1-IBMS),IPDS(25),IGDS(18),IBDS(9)
      REAL FR(IM*JM)
      CHARACTER PDS(ILPDS)
      NF=IM*JM
      IF(IDRT.EQ.0) THEN
        IF(IM.EQ.144.AND.JM.EQ.73) THEN
          IGRID=2
        ELSEIF(IM.EQ.360.AND.JM.EQ.181) THEN
          IGRID=3
        ELSE
          IGRID=255
        ENDIF
        IRESFL=128
        ISCAN=0
CO        LAT1=NINT(90.E3)
CO        LON1=0
CO        LATI=NINT(180.E3/(JM-1))
CO        LONI=NINT(360.E3/IM)
        LAT1 = NINT(XLAT1*1000.)
        LON1 = NINT(XLON1*1000.)
        LATI = NINT( (XLAT2-XLAT1)/(JM-1) * 1000.)
        LONI = NINT( (XLON2-XLON1)/(IM-1) * 1000.)  
        IGDS09=-LAT1
        IGDS10=-LONI
        IGDS11=LATI
        IGDS12=LONI
        IGDS13=ISCAN
      ELSEIF(IDRT.EQ.4) THEN
        IGRID=255
        IRESFL=128
        ISCAN=0
        LAT1=NINT(90.E3-180.E3/ACOS(-1.)*COLAT1)
        LON1=0
        LATI=JM/2
        LONI=NINT(360.E3/IM)
        IGDS09=-LAT1
        IGDS10=-LONI
        IGDS11=LATI
        IGDS12=LONI
        IGDS13=ISCAN
      ELSEIF(IDRT.EQ.5) THEN	! POLAR PROJECTION
        IGRID=255
        LAT1=NINT(180.E3/ACOS(-1.) * XLAT1)
        LON1=NINT(180.E3/ACOS(-1.) * XLON1)
        IRESFL=0
        IGDS09=NINT(ORTRU*1.E3)
        IGDS10=DELX  
        IGDS11=DELY
        IF( NINT(PROJ).EQ.1  ) IGDS12=0		! NORTH POLAR PROJ
        IF( NINT(PROJ).EQ.-1 ) IGDS12=128	! SOUTH POLAT PROJ
        ISCAN=64
        IGDS13=ISCAN
      ELSEIF(IDRT.EQ.1) THEN	! MERCATER PROJECTION
        IGRID=255
        LAT1=NINT(180.E3/ACOS(-1.) * XLAT1)
        LON1=NINT(180.E3/ACOS(-1.) * XLON1)
        IRESFL=0
        IGDS09=NINT(180.E3/ACOS(-1.) * XLAT2)
        IGDS10=NINT(180.E3/ACOS(-1.) * XLON2)
        IGDS11=DELX
        IGDS12=DELY
        IGDS13=NINT(ORTRU*1.E3)
        ISCAN=64
        IGDS14=ISCAN
      ELSE
        IERR=40
        RETURN
      ENDIF
      IPDS(01)=ILPDS	! LENGTH OF PDS
      IPDS(02)=IPTV	! PARAMETER TABLE VERSION ID
      IPDS(03)=ICEN	! CENTER ID
      IPDS(04)=IGEN	! GENERATING MODEL ID
      IPDS(05)=IGRID	! GRID ID
      IPDS(06)=1	! GDS FLAG
      IPDS(07)=IBMS 	! BMS FLAG
      IPDS(08)=IPU	! PARAMETER UNIT ID
      IPDS(09)=ITL	! TYPE OF LEVEL ID
      IPDS(10)=IL1	! LEVEL 1 OR 0
      IPDS(11)=IL2	! LEVEL 2
      IPDS(12)=IYR	! YEAR
      IPDS(13)=IMO	! MONTH
      IPDS(14)=IDY	! DAY
      IPDS(15)=IHR	! HOUR
      IPDS(16)=0	! MINUTE
      IPDS(17)=IFTU	! FORECAST TIME UNIT ID
      IPDS(18)=IP1  	! TIME PERIOD 1
      IPDS(19)=IP2	! TIME PERIOD 2 OR 0
      IPDS(20)=ITR	! TIME RANGE INDICATOR
      IPDS(21)=INA	! NUMBER IN AVERAGE
      IPDS(22)=INM	! NUMBER MISSING
      IPDS(23)=20	! CENTURY
      IPDS(24)=0	! RESERVED
      IPDS(25)=IDS	! DECIMAL SCALING
      IGDS(01)=0	! NUMBER OF VERTICAL COORDS
      IGDS(02)=255	! VERTICAL COORD FLAG
      IGDS(03)=IDRT	! DATA REPRESENTATION TYPE
      IGDS(04)=IM	! EAST-WEST POINTS
      IGDS(05)=JM	! NORTH-SOUTH POINTS
      IGDS(06)=LAT1	! LATITUDE OF ORIGIN
      IGDS(07)=LON1	! LONGITUDE OF ORIGIN
      IGDS(08)=IRESFL	! RESOLUTION FLAG
      IGDS(09)=IGDS09	! LATITUDE OF END OR ORIENTATION
      IGDS(10)=IGDS10	! LONGITUDE OF END OR DX IN METER ON 60N
      IGDS(11)=IGDS11	! LAT INCREMENT OR GAUSSIAN LATS OR DY IN METER ON 60N
      IGDS(12)=IGDS12	! LONGITUDE INCREMENT OR PROJECTION
      IGDS(13)=IGDS13	! SCANNING MODE OR LAT OF INTERCUT ON EARTH FOR MERCATER
      IGDS(14)=IGDS14	! NOT USED OR SCANNING MODE FOR MERCATER
      IGDS(15)=0	! NOT USED 
      IGDS(16)=0	! NOT USED
      IGDS(17)=0	! NOT USED
      IGDS(18)=0	! NOT USED
      IBDS(1:9)=0	! BDS FLAGS
      NBM=NF
      IF(IBMS.NE.0) THEN
        NBM=0
        DO I=1,NF
          IF(LBM(I)) THEN
            IBM(I)=1
            NBM=NBM+1
          ELSE
            IBM(I)=0
          ENDIF
        ENDDO
        IF(NBM.EQ.NF) IPDS(7)=0
      ENDIF
      IF(NBM.EQ.0) THEN
        DO I=1,NF
          FR(I)=0.
        ENDDO
        NBIT=0
      ELSE
        CALL GTBITS(IPDS(7),IDS,NF,IBM,F,FR,FMIN,FMAX,NBIT)
        IF(MXBIT.GT.0) NBIT=MIN(NBIT,MXBIT)
      ENDIF
      CALL W3FI72(0,FR,0,NBIT,0,IPDS,PDS,
     &            1,255,IGDS,0,0,IBM,NF,IBDS,
     &            NFO,GRIB,LGRIB,IERR)
      RETURN
      END
