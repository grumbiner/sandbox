      PROGRAM engrib_hycom
      IMPLICIT none

      INCLUDE "grib_parms.h"

C  Will move to proper include file
      INTEGER im, jm
      PARAMETER (im = 1200)
      PARAMETER (jm = 1684)
      INTEGER nlevels
      PARAMETER (nlevels = 26)
      
C Includes for bacio:
      INCLUDE "locale.inc"
      INCLUDE "clib.inc"
      INTEGER fdes, newpos, nactual, start, bacio

C gribit parms
      INTEGER griblen
      PARAMETER (griblen = (100 + 28 + IM*JM*(8+1)) / 8 )
      CHARACTER grib(griblen) !message
      LOGICAL lbm(im, jm)     !bit mask true -> use pt
      INTEGER ibms            !1 for use bit mask, 0 else
      INTEGER cen, yy, mmm, dd, runhour, verfhr
      INTEGER parmno, level
      INTEGER lgrib, ierr     !returned by gribit
      INTEGER mxbit
      INTEGER il1, il2, iftu, itl1
      INTEGER itr, ip1, ip2, ids, inm, ina
      INTEGER igen, icen, ilpds, itl, gridno, idrt
      REAL xlat1, xlon1, xlat2, xlon2, delx, dely, colat1
      INTEGER proj, ortru
      INTEGER iptv
C-----------

C     Dummy test
      REAL ssh(im, jm)
      CHARACTER*90 fname
      REAL pi
      INTEGER i, j
      INTEGER identity
C-------------
C     Center and grib table identification
      ilpds = 28  !pds length
      icen  =  7  !center
      igen  = 120 !generating process XXX


C     Model run identification
      yy = 2005
      mmm = 7
      dd  = 20
      runhour = 00
     
C     Lat long bounding boxes
      pi = ABS(ACOS(-1.))
        xlat1 = -32 
        xlon1 = -110 
        xlat2 = 75  
        xlon2 = 30 
      delx = (xlon2 - xlon1)/float(im - 1)
      dely = (xlat2 - xlat1)/float(jm - 1)
C     Hycom grib-related specs:
      idrt   = 6
      colat1 = 0   ! dummy when gribbing hycom
      gridno = 255 ! marker for non-grib projection

C     Parameter identification and packing material
C      mxbit  = 12 !maximum # bits to use, 0 -> infinit
C      parmno = 91 !sea ice
C      iptv  =  1  !parameter table version
C      ids    = 2  !decimal digits precision 
      mxbit = 0
      verfhr = 0
      itl = 102
      il1 = 0
      il2 = 0
      iftu = 1
      ip1  = 0
      ip2  = 0
      itr  = 10
      ina  = 0
      inm  = 0
      ibms  =  0  !1 -> use bit mask file

      
C  Dummy set up
      DO i = 1, im
      DO j = 1, jm
        ssh(i,j) = 0.0
      ENDDO
      ENDDO

      ssh(1,1) = 1.33
      ssh(im,jm) = -2.36
C     Create a test bit mask
      lbm = .false.
      DO i = im/2, im
      DO j = jm/2, jm
        lbm(i,j) = .true.
      ENDDO
      ENDDO
      ibms = 1

C  Start using the include file parms
      WRITE (fname, 9002)
 9002 FORMAT("fort.51")
      ierr = bacio(BAOPEN_WONLY , start, newpos,
     1                SIZEOF_CHARACTER, 0, nactual, fdes, fname,
     2                grib)


      DO identity = 1, 13
        IDS = -LOG10(precision(identity))
        parmno = parms(identity)
        iptv   = tables(identity)
        IF (identity .EQ. 12 .OR. identity .EQ. 13) THEN
          itl = 201  ! ocean column-average
        ELSE
          itl = 102  ! msl
        ENDIF
          

C Hycom version
        CALL gribit(ssh, lbm, idrt, im, jm, mxbit, colat1, 
     &                  ilpds, iptv, icen,igen,ibms,parmno,ITL,IL1,IL2,
     &                  yy, mmm, dd,runhour,
     &                  IFTU,IP1,IP2,ITR,INA,INM,IDS,
     &                  xlat1,xlon1,xlat2,xlon2,delx,dely,ORTRU,PROJ,
     &                  gridno,
     &                  grib,lgrib,ierr)
        IF (identity .EQ. 1) THEN
C         repeat call because of a weirdness regarding rads vs. degrees
        CALL gribit(ssh, lbm, idrt, im, jm, mxbit, colat1,
     &                  ilpds, iptv, icen,igen,ibms,parmno,ITL,IL1,IL2,
     &                  yy, mmm, dd,runhour,
     &                  IFTU,IP1,IP2,ITR,INA,INM,IDS,
     &                  xlat1,xlon1,xlat2,xlon2,delx,dely,ORTRU,PROJ,
     &                  gridno,
     &                  grib,lgrib,ierr)
        ENDIF

      IF (ierr .NE. 0) THEN
        PRINT *,'error ',ierr,' while trying to construct grib message '
      ELSE
        ierr = bacio(BAWRITE, start, newpos,
     1                SIZEOF_CHARACTER, lgrib, nactual, fdes, fname,
     2                grib)
        start = newpos
      ENDIF

      ENDDO

      DO identity = 14, nparms
        IDS = -LOG10(precision(identity))
        parmno = parms(identity)
        iptv   = tables(identity)
        itl = 160 ! 160 for z levels

        DO i = 1, nlevels
          il1 = 1
          il2 = i * 15
C Hycom version
        CALL gribit(ssh, lbm, idrt, im, jm, mxbit, colat1,
     &                  ilpds, iptv, icen,igen,ibms,parmno,ITL,IL1,IL2,
     &                  yy, mmm, dd,runhour,
     &                  IFTU,IP1,IP2,ITR,INA,INM,IDS,
     &                  xlat1,xlon1,xlat2,xlon2,delx,dely,ORTRU,PROJ,
     &                  gridno,
     &                  grib,lgrib,ierr)

      IF (ierr .NE. 0) THEN
        PRINT *,'error ',ierr,' while trying to construct grib message '
      ELSE
        ierr = bacio(BAWRITE, start, newpos,
     1                SIZEOF_CHARACTER, lgrib, nactual, fdes, fname,
     2                grib)
        start = newpos
      ENDIF

      ENDDO
      ENDDO

        ierr = bacio(BACLOSE, start, newpos,
     1                SIZEOF_CHARACTER, 0, nactual, fdes, fname,
     2                grib)
        
      STOP
      END
C-----------------------------------------------------------------------
      SUBROUTINE GRIBIT(F,LBM,IDRT,IM,JM,MXBIT,COLAT1,
     &                  ILPDS,IPTV,ICEN,IGEN,IBMS,IPU,ITL,IL1,IL2,
     &                  IYR,IMO,IDY,IHR,IFTU,IP1,IP2,ITR,INA,INM,IDS,
     &                  XLAT1,XLON1,XLAT2,XLON2,DELX,DELY,ORTRU,PROJ,
     &                  GRIDNO,
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
C   97-06-04  Grumbine (send grid number as argument)
C   98-07-21  Grumbine Y2K fix on century usage
C   05-07-20  Grumbine variant for hycom native grids
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
C                  -- 0 FOR LATLON 
C                  -- 1 FOR Mercator
C                  -- 4 FOR GAUSSIAN
C                  -- 5 FOR Polar Stereographic
C                  -- 6 FOR Hycom native
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
C     IYR      - INTEGER YEAR -- 4 digits 7/21/1998
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
C     GRIDNO  - NCEP Grib table Grib number.
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
CD      IMPLICIT none
C     Declare argument list:
      INTEGER ILPDS, IPTV, ICEN, IGEN, IBMS, IPU, ITL, IL1, IL2
      INTEGER IYR, IMO, IDY, IHR, IFTU, IP1, IP2, ITR, INA, INM, IDS
      REAL XLAT1, XLON1, XLAT2, XLON2, DELX, DELY, ORTRU, PROJ
      REAL colat1
      INTEGER iresfl, iscan, nbit, igrid, mxbit, ierr, lgrib
 
C$$$
      REAL F(IM*JM)
      LOGICAL LBM(IM*JM)
      CHARACTER GRIB(*)
      INTEGER IBM(IM*JM*IBMS+1-IBMS),IPDS(25),IGDS(18),IBDS(9)
      REAL FR(IM*JM)
      CHARACTER PDS(ILPDS)
      INTEGER GRIDNO
C     Declare local variables:
      INTEGER igds10, igds11, igds12, igds13, igds14, igds09
      INTEGER i
      INTEGER lat1, lon1, lati, loni


      NF=IM*JM
      IF(IDRT.EQ.0) THEN
        IF(IM.EQ.144.AND.JM.EQ.73) THEN
          IGRID=2
        ELSEIF(IM.EQ.360.AND.JM.EQ.181) THEN
          IGRID=3
        ELSE
          IGRID=GRIDNO
        ENDIF
        IRESFL=128
        ISCAN=0
CO        LAT1=NINT(90.E3)
CO        LON1=0
CO        LATI=NINT(180.E3/(JM-1))
CO        LONI=NINT(360.E3/IM)
        LAT1 = NINT(XLAT1*1000.)
        LON1 = NINT(XLON1*1000.)
        lati = NINT( (XLAT2-XLAT1)/(JM-1) * 1000.)
        loni = NINT( (XLON2-XLON1)/(IM-1) * 1000.)  
        IGDS09=-LAT1
        IGDS10=-LON1  !was LONI
        IGDS11=lati
        IGDS12=loni
        IGDS13=ISCAN
      ELSEIF(IDRT.EQ.4) THEN
        IGRID=GRIDNO
        IRESFL=128
        ISCAN=0
        LAT1=NINT(90.E3-180.E3/ACOS(-1.)*COLAT1)
        LON1=0
        lati=JM/2
        loni=NINT(360.E3/IM)
        IGDS09=-LAT1
        IGDS10=-LON1
        IGDS11=lati
        IGDS12=loni
        IGDS13=ISCAN
      ELSEIF(IDRT.EQ.1) THEN    ! MERCATOR PROJECTION
        IGRID=GRIDNO
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
      ELSEIF(IDRT.EQ.5) THEN    ! POLAR PROJECTION
        IGRID=GRIDNO
        LAT1=NINT(180.E3/ACOS(-1.) * XLAT1)
        LON1=NINT(180.E3/ACOS(-1.) * XLON1)
        IRESFL=0
        IGDS09=NINT(ORTRU*1.E3)
        IGDS10=DELX  
        IGDS11=DELY
        IF( NINT(PROJ).EQ.1  ) IGDS12=0        ! NORTH POLAR PROJ
        IF( NINT(PROJ).EQ.-1 ) IGDS12=128    ! SOUTH POLAT PROJ
        ISCAN=64
        IGDS13=ISCAN
      ELSEIF (IDRT .EQ. 6) THEN   ! Hycom native grid
        IGRID=GRIDNO
        LAT1=NINT(180.E3/ACOS(-1.) * XLAT1)
        LON1=NINT(180.E3/ACOS(-1.) * XLON1)
        IRESFL = 128  ! XXX
        IGDS09 = NINT(180.E3/ACOS(-1.) * XLAT2)
        IGDS10 = NINT(180.E3/ACOS(-1.) * XLON2)
        lati = NINT( (XLAT2-XLAT1)/(JM-1) * 1000. *180./ACOS(-1.)) ! regular-equivalent spacing
        loni = NINT( (XLON2-XLON1)/(IM-1) * 1000. *180./ACOS(-1.))
        IGDS11=lati
        IGDS12=loni
        ISCAN = 0
        IGDS13 = ISCAN  ! XXX scanning mode
        IDRT   = 0      ! XXX override on exit, for sake of w3fi72
      ELSE
        IERR=40
        RETURN
      ENDIF


      IPDS(01)=ILPDS   ! LENGTH OF PDS
      IPDS(02)=IPTV    ! PARAMETER TABLE VERSION ID
      IPDS(03)=ICEN    ! CENTER ID
      IPDS(04)=IGEN    ! GENERATING MODEL ID
      IPDS(05)=IGRID   ! GRID ID
      IPDS(06)=1       ! GDS FLAG
      IPDS(07)=IBMS    ! BMS FLAG
      IPDS(08)=IPU     ! PARAMETER UNIT ID
      IPDS(09)=ITL     ! TYPE OF LEVEL ID
      IPDS(10)=IL1     ! LEVEL 1 OR 0
      IPDS(11)=IL2     ! LEVEL 2
      IPDS(23)=1 + (IYR-1)/100            ! CENTURY
      IPDS(12)=IYR - 100*(IPDS(23) - 1) ! YEAR
      IPDS(13)=IMO    ! MONTH
      IPDS(14)=IDY    ! DAY
      IPDS(15)=IHR    ! HOUR
      IPDS(16)=0      ! MINUTE
      IPDS(17)=IFTU   ! FORECAST TIME UNIT ID
      IPDS(18)=IP1    ! TIME PERIOD 1
      IPDS(19)=IP2    ! TIME PERIOD 2 OR 0
      IPDS(20)=ITR    ! TIME RANGE INDICATOR
      IPDS(21)=INA    ! NUMBER IN AVERAGE
      IPDS(22)=INM    ! NUMBER MISSING
      IPDS(24)=0      ! RESERVED
      IPDS(25)=IDS    ! DECIMAL SCALING

      IGDS(01)=0      ! NUMBER OF VERTICAL COORDS
      IGDS(02)=255    ! VERTICAL COORD FLAG
      IGDS(03)=IDRT   ! DATA REPRESENTATION TYPE
      IGDS(04)=IM     ! EAST-WEST POINTS
      IGDS(05)=JM     ! NORTH-SOUTH POINTS
      IGDS(06)=LAT1   ! LATITUDE OF ORIGIN
      IGDS(07)=LON1   ! LONGITUDE OF ORIGIN
      IGDS(08)=IRESFL    ! RESOLUTION FLAG
      IGDS(09)=IGDS09    ! LATITUDE OF END OR ORIENTATION
      IGDS(10)=IGDS10    ! LONGITUDE OF END OR DX IN METER ON 60N
      IGDS(11)=IGDS11    ! LAT INCREMENT OR GAUSSIAN LATS OR DY IN METER ON 60N
      IGDS(12)=IGDS12    ! LONGITUDE INCREMENT OR PROJECTION
      IGDS(13)=IGDS13    ! SCANNING MODE OR LAT OF INTERCUT ON EARTH FOR MERCATER
      IGDS(14)=IGDS14    ! NOT USED OR SCANNING MODE FOR MERCATER
      IGDS(15)=0    ! NOT USED 
      IGDS(16)=0    ! NOT USED
      IGDS(17)=0    ! NOT USED
      IGDS(18)=0    ! NOT USED
      IBDS(1:9)=0   ! BDS FLAGS
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
        PRINT *,'back from gtbits, nbit = ',NBIT,' mxbit = ',mxbit
        IF(MXBIT.GT.0) NBIT=MIN(NBIT,MXBIT)
      ENDIF

      CALL W3FI72(0,FR,0,NBIT,0,IPDS,PDS,
     &            1,255,IGDS,0,0,IBM,NF,IBDS,
     &            NFO,GRIB,LGRIB,IERR)
      RETURN
      END
