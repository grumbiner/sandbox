      PROGRAM mk
C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C MAIN PROGRAM: ICE2GRIB     ENGRIB A 0.5 DEGREE LAT-LONG CHAR ARRAY 
C   PRGMMR: GRUMBINE         ORG: NP21        DATE: 1999-05-06
C
C ABSTRACT: READ IN 1 BYTE VALUES FROM A 0.5 DEGREE LAT-LONG GRID
C    AND WRITE THEM OUT AS A GRIB FIELD.
C
C PROGRAM HISTORY LOG:
C    97-06-24 ROBERT GRUMBINE
C    98-01-27 Robert Grumbine    Modify to write out a WMO file
C    98-07-21 Robert Grumbine    Y2K and F90 changes
C    98-07-21 Robert Grumbine    Fix grid number to match appendix K
C    98-11-18 Robert Grumbine    Drop W3FQ02 in favor of utcdat
C
C USAGE:
C  INPUT FILES:
C     FTNF06 - STANDARD INPUT - DATE INFORMATION
C     FTNF11 - CHARACTER ARRAY TO ENGRIB
C  OUTPUT FILES:
C     FTNF51 - OUTPUT GRIB FILE
C     FTNF52 - OUTPUT WMO Encoded Grib Bulletin
C 
C  SUBPROGRAMS CALLED:
C      GRIBIT, WMOOUT, MAKWMO, QUEDES, TRANST 
C    LIBRARY:
C      W3LIB: WRYTE, W3FI72, GTBITS, utcdat, W3FI92, W3AI19
C
C  EXIT STATES:
C    COND = 0  - SUCCESSFUL RUN
C
C  REMARKS:
C
C ATTRIBUTES:
C   LANGUAGE: CRAY FORTRAN
C   MACHINE: CRAY4
C
C$$$
C     Engrib character maps already on lat-long grids.  
C     Robert Grumbine 4 June 1997.

      IMPLICIT none

      INTEGER nx, ny
      REAL dxlat, dylat
      PARAMETER (dxlat = 0.5)
      PARAMETER (dylat = 0.5)
      PARAMETER (nx = 360 / dxlat)
      PARAMETER (ny = 180 / dylat)

      INTEGER gridno
      PARAMETER (gridno = 235) !NCEP grid number of this grid
      REAL outmap(nx, ny)
      LOGICAL lbm(nx, ny)

      INTEGER griblen
      PARAMETER (griblen = (100 + 28 + nx * ny * (8+1)) /8 )

      CHARACTER*1 cmap(nx, ny)
      CHARACTER grib( griblen )
      INTEGER lgrib, ierr

C     Definitions for the WMO section
      INTEGER byteint, wmolen, wmounit
      CHARACTER*6 BULHEAD
      CHARACTER*4 KW
      PARAMETER (wmolen = 1280)
      PARAMETER (byteint = 8)
      PARAMETER (wmounit = 52)
      PARAMETER (BULHEAD = "OEXA88")
      PARAMETER (KW      = "KWBM")
      INTEGER linelen, nlines
      PARAMETER (linelen = wmolen/byteint)
      PARAMETER (nlines  = (griblen + 80 + 21) / wmolen + 2)
      INTEGER lwork(linelen, nlines)


C     Local Utility variables      
      INTEGER i, j, cen, yy, mm, dd
      CHARACTER*8 tag

      READ (11) cmap
      READ (*, 9001) tag
 9001 FORMAT (A8)
      READ (tag, 9002) cen, yy, mm, dd
 9002 FORMAT (I2, I2, I2, I2)
      yy = yy + 100 * cen          ! Pass 4 digit year to gribit

      CALL W3TAGB('ICE2GRIB',1999,0126,0058,'NP21   ')

      DO 1000 j = 1, ny
        DO 1100 i = 1, nx
          outmap(i,j) = FLOAT(ICHAR(cmap(i,j))) / 100.
 1100   CONTINUE
 1000 CONTINUE

      CALL gribit(outmap, lbm, 0, nx, ny, 8, 0.0, 28, 1, 7, 0, 0, 91, 
     1     102, 0, 0, yy, mm, dd, 0, 1,
     2     0, 0, 10, 0, 0, 2, 
C     Last argument is power in multiplying (data)*10**x prior to gribbing.
     3     90.-dylat/2.,         dxlat/2., 
     4     90.-dylat/2+(ny-1)*dylat, dxlat/2.+dxlat*(nx-1), 
     5     dxlat, dylat, 0, -10., gridno, grib, lgrib, ierr) 

      IF (ierr .EQ. 0) THEN
        CALL WRYTE(51, lgrib, grib)
        CALL wmoout(BULHEAD, KW, yy, mm, dd, 0, lwork, linelen, nlines,
     1                 grib, lgrib, wmounit)
       ELSE
        PRINT *,'Error ',ierr,' constructing grib message in mkllglob'
      ENDIF
     
      CALL W3TAGE('ICE2GRIB')
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
C$$$
      REAL F(IM*JM)
      LOGICAL LBM(IM*JM)
      CHARACTER GRIB(*)
      INTEGER IBM(IM*JM*IBMS+1-IBMS),IPDS(25),IGDS(18),IBDS(9)
      REAL FR(IM*JM)
      CHARACTER PDS(ILPDS)
      INTEGER GRIDNO

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
        LATI = NINT( (XLAT2-XLAT1)/(JM-1) * 1000.)
        LONI = NINT( (XLON2-XLON1)/(IM-1) * 1000.)  
        IGDS09=-LAT1
        IGDS10=-LON1  !was LONI
        IGDS11=LATI
        IGDS12=LONI
        IGDS13=ISCAN
      ELSEIF(IDRT.EQ.4) THEN
        IGRID=GRIDNO
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
      ELSEIF(IDRT.EQ.1) THEN    ! MERCATER PROJECTION
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
      IGDS(13)=IGDS13   ! SCANNING MODE OR LAT OF INTERCUT ON EARTH FOR MERCATER
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
        IF(MXBIT.GT.0) NBIT=MIN(NBIT,MXBIT)
      ENDIF
      CALL W3FI72(0,FR,0,NBIT,0,IPDS,PDS,
     &            1,255,IGDS,0,0,IBM,NF,IBDS,
     &            NFO,GRIB,LGRIB,IERR)
      RETURN
      END
      SUBROUTINE MAKWMO (BULHED,IDS,HEADER,kwbx)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM: MAKWMO         FORMAT THE WMO HEADER
C   PRGMMR: FARLEY           ORG: W/NMC42    DATE: 84-07-06
C
C ABSTRACT: FORMS THE WMO HEADER FOR A GIVEN BULLETIN.  THE WMO
C   HEADER IS IN ASCII AND THE SKELETON IS FILLED IN BY CONVERTING
C   THE BULLETIN HEADER (TTAAII) TO ASCII AND EXTRACTING THE DAY
C   OF MONTH AND FORECAST HOUR FROM THE 7TH WORD OF THE ID AND
C   CONVERTING THEM TO ASCII VALUES ALSO.
C
C PROGRAM HISTORY LOG:
C   84-07-06  FARLEY      ORIGINAL AUTHOR
c   96-01-01  gerald      modified to run on cray for gakgrib
C   98-09-29  Grumbine    Modified for F-77 compliance -- not equivalencing
C                           character and non-character data
C
C USAGE:    CALL MAKWMO(BULHED,IDS,HEADER)
C   INPUT ARGUMENT LIST:
C     BULHED   -  TTAAII BULLETIN HEADER                    FT10
C     IDs      -  yr mon day hr
C
C   OUTPUT ARGUMENT LIST:      (INCLUDING WORK ARRAYS)
C     HEADER   -  COMPLETE WMO HEADER IN ASCII
C
C ATTRIBUTES:
C   LANGUAGE: CRAY CFT77 FORTRAN
C   MACHINE:  CRAY C916/256, J916/2048
C
C$$$
C
      IMPLICIT none

      INTEGER       IDS(6)
CFarley       INTEGER       INTIH  (2)
CFarley       INTEGER       WMO    (3)
C
      CHARACTER * 6 ASCII
      CHARACTER * 6 BULHED
      CHARACTER * 1 HEADER (*)
      CHARACTER * 1 HOLD1  (6)
      CHARACTER * 1 INTI   (10)
      CHARACTER * 1 ITIME  (8)
      CHARACTER * 1 WMOHDR (21)
      character * 4 kwbx
C
      INTEGER IDAY, IHOUR
      INTEGER I, NONES, NTENS
      SAVE
C
      EQUIVALENCE (ASCII, HOLD1(1))
CFarley      EQUIVALENCE (INTI(1),INTIH(1))
CFarley original      EQUIVALENCE (WMO(1),WMOHDR(1))
C
CFarley       DATA  INTIH /Z'3031323334353637',Z'3839202020202020'/
CFarley original      DATA  WMO   /Z'202020202020204B',Z'5742432020202020',
CFarley original     X             Z'30300D0D0A202020'/
C
C--------------------------------------------------------------------
C
C$            1.     CREATE WMO HEADER.
C
C$            1.1    CONVERT BULHED FROM EBCDIC TO ASCII.
C
C  Grumbine: Use standard F-77 
       WMOHDR(1)  = CHAR(32) ! Z'20')
       WMOHDR(2)  = CHAR(32) ! Z'20')
       WMOHDR(3)  = CHAR(32) ! Z'20')
       WMOHDR(4)  = CHAR(32) ! Z'20')
       WMOHDR(5)  = CHAR(32) ! Z'20')
       WMOHDR(6)  = CHAR(32) ! Z'20')
       WMOHDR(7)  = CHAR(32) ! Z'20')
       WMOHDR(8)  = CHAR(75) ! Z'4B')
       WMOHDR(9)  = CHAR(87) ! Z'57')
       WMOHDR(10) = CHAR(66) ! Z'42')
       WMOHDR(11) = CHAR(67) ! Z'43')
       WMOHDR(12) = CHAR(32) ! Z'20')
       WMOHDR(13) = CHAR(32) ! Z'20')
       WMOHDR(14) = CHAR(32) ! Z'20')
       WMOHDR(15) = CHAR(32) ! Z'20')
       WMOHDR(16) = CHAR(32) ! Z'20')
       WMOHDR(17) = CHAR(48) ! Z'30')
       WMOHDR(18) = CHAR(48) ! Z'30')
       WMOHDR(19) = CHAR(13) ! Z'0D')
       WMOHDR(20) = CHAR(13) ! Z'0D')
       WMOHDR(21) = CHAR(10) ! Z'0A')
       INTI(1) = CHAR(48) ! Z'30')
       INTI(2) = CHAR(49) ! Z'31')
       INTI(3) = CHAR(50) ! Z'32')
       INTI(4) = CHAR(51) ! Z'33')
       INTI(5) = CHAR(52) ! Z'34')
       INTI(6) = CHAR(53) ! Z'35')
       INTI(7) = CHAR(54) ! Z'36')
       INTI(8) = CHAR(55) ! Z'37')
       INTI(9) = CHAR(56) ! Z'38')
       INTI(10) = CHAR(57) ! Z'39')
       INTI(11) = CHAR(32) ! Z'20')
       INTI(12) = CHAR(32) ! Z'20')
       INTI(13) = CHAR(32) ! Z'20')
       INTI(14) = CHAR(32) ! Z'20')
       INTI(15) = CHAR(32) ! Z'20')
       INTI(16) = CHAR(32) ! Z'20')
    
      ASCII = BULHED
cccccccall w3ai38(ascii,6)
      DO 100 I = 1,6
        WMOHDR(I) = HOLD1(I)
  100 CONTINUE
c
c      move kwbx into wmoheader
c
        do 101 i = 1,4
         wmohdr(i+ 7) = kwbx(I:I)
  101   continue
C
C$            1.2    PICK OFF THE DAY OF MONTH (YY)
C$                   AND CONVERT TO ASCII.
C
CD      IDAY       = ICHAR(IDS(3))
      IDAY       = IDS(3)
      NTENS      = IDAY * .10
      NONES      = IDAY - (NTENS * 10)
      WMOHDR(13) = INTI(NTENS + 1)
      WMOHDR(14) = INTI(NONES + 1)
C
C$            1.3    PICK OFF THE HOUR(GG) AND CONVERT TO ASCII.
C
CD      IHOUR      = ICHAR(IDS(4))
      IHOUR      = IDS(4)
      NTENS      = IHOUR * .10
      NONES      = IHOUR - (NTENS * 10)
      WMOHDR(15) = INTI(NTENS + 1)
      WMOHDR(16) = INTI(NONES + 1)
C
C--------------------------------------------------------------------
C
C$            2.     MOVE WMOHDR TO OUTPUT FIELD.
C
      DO 200 I = 1,21
        HEADER(I) = WMOHDR(I)
  200 CONTINUE
C
CD      write(6,60)(wmohdr(i),i=1,16),bulhed,iday,ihour
CD   60    format(1x,' wmohdr ',16a1,1x,a6,1x,2i4)
      RETURN
      END
C
      SUBROUTINE QUEDES(QUEUE,BULHED,ITOT,kwbx)
C
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    QUEDES      QUEUE DESCRIPTOR
C   PRGMMR: FARLEY           ORG: W/NMC21   DATE: 86-12-10
C
C ABSTRACT: AN 80-BYTE MESSAGES THAT CONTAINS INFORMATION NEEDED BY
C   COMMUNICATIONS PEOPLE.
C
C PROGRAM HISTORY LOG:
C   86-12-10  FARLEY      ORIGINAL AUTHOR
C   93-11-01  GERALD      MODIFIED FOR GULF OF ALASKA WAVE MODEL
C   99-05-05  Grumbine    Modified and standardized for sea ice products 
C
C USAGE:    CALL QUEDES(QUEUE,BULHED,ITOT)
C   INPUT ARGUMENT LIST:
C     BULHED   -  BULLETIN HEADER CHARACTER * 6
C
C   OUTPUT ARGUMENT LIST:  NONE
C     QUEUE    -  QUEUE DESCRIPTOR CHARACTER * 1
C     ITOT     -  TOTAL LENGTH OF BULLETIN QUEDES + (GRIB - 7777)
C
C ATTRIBUTES:
C   LANGUAGE: F77, rigorously standard
C   MACHINE:  any
C
C$$$
C
       CHARACTER*6 BULHED
       character*4 kwbx
       CHARACTER*1 QUEUE(80)
       INTEGER   KARY(7)
       INTEGER   ITIME(8)
C
C...... GET SYSTEM CLOCK TIME
        CALL w3utcdat(ITIME)
C

       KARY(1) = ITIME(3)
       KARY(2) = ITIME(5)
       KARY(3) = ITIME(6)
       KARY(4) = 0
       KARY(5) = 0
       KARY(6) = 0
       KARY(7) = 21 + ITOT

       JERR = 0
       CALL W3FI92 (QUEUE,BULHED,KARY,kwbx,JERR)
       IF (JERR .NE. 0) PRINT *,' w3fi92 err ',jerr

       RETURN
       END
      SUBROUTINE TRANST (KOUT,KBUF,WMOHDR,QUEUE,ITOT,LTOT, 
     1                   lwork, linelen, nlines)
C
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    TRANST      WRITE GRIB MESSAGE TO TRAN FILE.
C   PRGMMR: FARLEY           ORG: W/NMC42   DATE: 86-12-10
C
C ABSTRACT: FORMS WAVE MESSAGES INTO 1280 BYTE PHYSICAL RECORDS AND
C   ADDS THEM TO THE OUTPUT TRANSMISSION.
C
C PROGRAM HISTORY LOG:
C   86-12-10  FARLEY      ORIGINAL AUTHOR
C   93-07-01  GERALD      MODIFIED TO TRANSMIT REGIONAL GULF OF
C                         ALASKA REGIONAL WAVE MODEL OUTPUT.
C   98-01-28  Grumbine    Generalized for use with ice products
C
C USAGE:    CALL TRANST(KOUT,KBUF,WMOHDR,QUEUE,ITOT,LTOT, lwork,
C            linelen, nlines)
C   INPUT ARGUMENT LIST:
C     KOUT     -  UNIT NUMBER OF TRANSMISSION FILE
C     KBUF     -  TRANSMISSION WORK ARRAY
C     WMOHDR   -  WMO HEADER
C     QUEUE    -  QUEUE DESCRIPTOR
C     ITOT     -  LENGTH OF GRIB MESSAGE  (GRIB - 7777)
C     lwork    -  Integer working space array
C     linelen  -  Number of integers equivalent to the length of a 
C                  line in a WMO message
C     nlines   -  Number of lines in the message
C
C   OUTPUT ARGUMENT LIST:  NONE
C     LTOT     -  TOTAL LENGTH OF BULLETIN QUEDES + WMOHDR +
C                 (GRIB - 7777)
C
C   OUTPUT FILES:
C     FTXXF001 - WMO-encoded grib message.  XX is kout
C
C ATTRIBUTES:
C   LANGUAGE: Fortran 90, Standard
C   MACHINE:  Cray
C
C$$$
C

      IMPLICIT none

      INTEGER       KOUT, ITOT, LTOT
      INTEGER linelen, nlines
      INTEGER       lwork(linelen,nlines)
      INTEGER byteint
      PARAMETER (byteint = 8)
C
      CHARACTER*1 KBUF(ITOT)
      CHARACTER*1 QUEUE (*)
      CHARACTER*1 WMOHDR(*)
C
      INTEGER i, j, LEFT, NEXT, IPUT
C

      NEXT = 0
      CALL W3AI19 (QUEUE ,     80, LWORK, byteint*linelen*nlines, NEXT)
      IF (NEXT .LE. 0) THEN
        PRINT *,'Error in W3AI19, next = ',NEXT
        STOP
      ENDIF

      CALL W3AI19 (WMOHDR,     21, LWORK, byteint*linelen*nlines, NEXT)
      IF (NEXT .LE. 0) THEN
        PRINT *,'Error in W3AI19, next = ',NEXT
        STOP
      ENDIF

      CALL W3AI19 (KBUF, ITOT, LWORK, byteint*linelen*nlines, NEXT)
      IF (NEXT .LE. 0) THEN
        PRINT *,'Error in W3AI19, next = ',NEXT
        STOP
      ENDIF

      LTOT = 80 + 21 + ITOT

      IPUT = LTOT / (linelen*byteint)
      LEFT = MOD(LTOT,(linelen*byteint))
      IF (LEFT .GT. 0) IPUT = IPUT + 1

      DO 100 j = 1,IPUT
        write(kout)(LWORK(i,j),i=1,linelen)
  100 CONTINUE
      CLOSE (kout,STATUS='KEEP')

      RETURN
      END
      SUBROUTINE wmoout(BULHEAD, KW, yy, mm, dd, hh, 
     1                  lwork, linelen, nlines, grib, lgrib, wmounit)
   
      IMPLICIT none

      CHARACTER*6 BULHEAD
      CHARACTER*4 KW
      INTEGER yy, mm, dd, hh
      INTEGER linelen, nlines, lwork(linelen, nlines)
      INTEGER lgrib, wmounit
      CHARACTER*1 grib(lgrib) 

      INTEGER IDS(6)
      CHARACTER*1 HEADER(21)
      CHARACTER*1 QUEUE(80)
      INTEGER mlen, mfin

      IDS(1) = yy
      IDS(2) = mm
      IDS(3) = dd
      IDS(4) = hh
      IDS(5) = 0
      IDS(6) = 0
      mlen = lgrib

      CALL MAKWMO(BULHEAD, IDS, HEADER, KW)
      CALL QUEDES(QUEUE, BULHEAD, mlen, KW)
      CALL TRANST(wmounit, grib, HEADER, QUEUE, mlen, mfin,
     1            lwork, linelen, nlines)

      RETURN
      END
