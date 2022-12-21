!------------------------------------------------------------------------------
!
! NAME:
!       AMSR2_89A_write_bufr
!
! PURPOSE:
!       This is a subroutine for writing the AMSR2_89A BUFR file.
!
! CATEGORY:
!       Subroutine
!
! CALLING SEQUENCE:
!       CALL AMSR2_89A_write_bufr(BUFR_FileName,BUFR_Table_FileName,ORBN,AggregateBeginningTime)
!
! INPUTS:
!      BUFR_FileName     - The name for the output BUFR file
!      BUFR_Table_FileName  - The name of the BUFR table
!      ORBN  - The orbit number
!      AggregateBeginningTime - Granule beginning time. It is used to get leap seconds
!
! KNOWN BUGS AND LIMITATIONS:
!       None known.
!
! CALLS:
!    OPENBF
!    MAXOUT
!    OPENMB
!    UFBSEQ
!    WRITCP
!    CLOSMG
!    CLOSBF
!
! MODULES:
!    type_kinds
!    errormsg_module
!    common_parameters
!
! MODIFICATION HISTORY:
!       Written by:  Yi Song (10/15/2015)
!                    IMSG 
!                    Yi.Song@noaa.gov
!
!------------------------------------------------------------------------------
!

   SUBROUTINE AMSR2_89A_write_bufr(amsr2_1b_rec,BUFR_FileName,BUFR_Table_FileName,ORBN,AggregateBeginningTime)

!
! Declare modules used
!

   USE type_kinds
   USE errormsg_module
   USE common_parameters
   USE amsr2_1b_module
   USE array_index_interface

   IMPLICIT NONE

!
!  Variable declarations
!

   TYPE (amsr2_1b_record) :: amsr2_1b_rec
   CHARACTER(250) :: BUFR_FileName
   CHARACTER(250) :: BUFR_Table_FileName
   CHARACTER(250) :: AggregateBeginningTime
   INTEGER(LONG) ::  ORBN, LEAPSECONDS

   INTEGER(LONG) :: LUNDX       ! BUFR table unit number
   INTEGER(LONG) :: LUBFR       ! BUFR file unit number
   INTEGER(LONG), PARAMETER :: ARRDIM = 32 
!   INTEGER(LONG), PARAMETER :: ARRDIM = 81 
   REAL(DOUBLE) :: ARR(ARRDIM)
   INTEGER(LONG) :: NX
   REAL(DOUBLE), PARAMETER :: BMISS = 10e10
   INTEGER(LONG) :: IDATE
   INTEGER(LONG) :: IRET

   INTEGER(LONG) :: Open_Status, Close_Status

   INTEGER(LONG) :: Scan 
   INTEGER(LONG) :: CrossTrack 
   INTEGER(LONG) :: AlongTrack 

   INTEGER(LONG) :: P2D_Index, P2AD_Index
   INTEGER(LONG) :: P3D_Index
   INTEGER(LONG) :: CH_2D_Index

   INTEGER(LONG), PARAMETER :: SATID = 122      ! GCOM-W1 
   INTEGER(LONG), PARAMETER :: CENTID = 160     ! 007=ncep and 160=nesdis
   INTEGER(LONG), PARAMETER :: SUB_CENTID = 0   ! 0=ORA(STAR)
   INTEGER(LONG), PARAMETER :: AMSR2_ID = 478   ! 478=AMSR2, 620=CrIS, 621=ATMS, 616=VIIRS
   INTEGER(LONG), PARAMETER :: SATCLS = 122     ! GCOM

   CHARACTER(8)  :: SUBSET

   REAL(DOUBLE) :: Time, UTC_Time
   INTEGER(LONG) :: Year
   INTEGER(LONG) :: Month
   INTEGER(LONG) :: Day
   INTEGER(LONG) :: Hour
   INTEGER(LONG) :: Minute
   INTEGER(LONG) :: Second
   INTEGER(LONG) :: Ascend, POS, CHNM, ODD_NUM
   INTEGER(DLONG), PARAMETER :: OFFSET_1970_to_1993 = 725846400_DLONG

   REAL(DOUBLE) :: SCCF(2), MSecond, Sat_Height
   REAL(SINGLE) :: VIIRSQ, ALFR, ACQF, ANPO 
   CHARACTER*8	cval
   REAL*8          rval
   EQUIVALENCE	( cval, rval )
!
! intermediate variables
!

   INTEGER(LONG) :: NUM_CHAN, NUM_PIXEL, MISSING
   INTEGER(LONG) :: Get_Lun
   LOGICAL :: MISSING_ALL

!
!  Begin the program.
!

   PRINT*,'Starting AMSR2_89A_write_bufr'
!
!  deal with all the data are missing situation
!
   MISSING_ALL=.TRUE.
   DO AlongTrack = 1,amsr2_1b_rec%AlongTrack
   DO CrossTrack=1,amsr2_1b_rec%CrossTrack2
   P2D_Index=Array_Index(CrossTrack,amsr2_1b_rec%CrossTrack2,AlongTrack,amsr2_1b_rec%AlongTrack)
    IF (ABS(amsr2_1b_rec%Latitude_for_89A(P2D_Index)) < 90.0 ) MISSING_ALL=.FALSE.
    IF (ABS(amsr2_1b_rec%Longitude_for_89A(P2D_Index)) < 180.0 ) MISSING_ALL=.FALSE.
   ENDDO
   ENDDO
   IF (MISSING_ALL) THEN
    print *, 'ALL the AMSR2_89A are missing values in the input HDF5 file'
    CALL EXIT (1)
   ENDIF

   PRINT*,'ORBN=', ORBN
print *, "AggregateBeginningTime=", AggregateBeginningTime

!
!  Initialize some local variables
!
   DATA SCCF/89.0e9, 89.0e9/

   SUBSET = 'NC021248'

!
!  Open the BUFR file.
!

   LUBFR = Get_Lun();
   OPEN (UNIT=LUBFR, File = BUFR_FileName, FORM="UNFORMATTED", Status="UNKNOWN", &
         IOSTAT=Open_Status)

   IF(Open_Status /= 0)THEN
      CALL error_messaging('AMSR2_89A_write_bufr', &
      'Failed to open output BUFR file', &
      FATAL)
   ENDIF

!
!  Open the BUFR table.
!

   LUNDX = Get_Lun();

   OPEN (UNIT=LUNDX, File = BUFR_Table_FileName, Status="OLD", &
         IOSTAT=Open_Status)

   IF(Open_Status /= 0)THEN
      CALL error_messaging('AMSR2_89A_write_bufr', &
      'Failed to open BUFR table file', &
      FATAL)
   ENDIF

!
!  Set some values in the BUFR file to write over the default values.  The are
!  the originating center, subcenter, and the WMO master BUFR table version.
!

   CALL PKVS01('OGCE',160)
   CALL PKVS01('GSES',0)
   CALL PKVS01('MTV',21)

!
!  Associated the BUFR file and table and identify them to the
!  BUFRLIB software.
!

   CALL OPENBF(LUBFR,'NODX',LUNDX)

!
!  The first call forces all output messages to be internally converted to BUFR 
!  edition 4 before being written out and the second call sets the international 
!  subcategory identifier to a value of 5 (CrIS), 6 (ATMS), 7 (VIIRS), 6 (SST), !  7 (AMSR2_1b propose to add microwave scanning radiometer under 21 Radiance (satellite measured). 
!

   CALL PKVS01('BEN', 4)
   CALL PKVS01('MSBTI', 7)

!   CALL MAXOUT(0)
   CALL MAXOUT(50000)

!
!  Get the leapseconds
!
   READ(AggregateBeginningTime,'(i4,i2,i2,i2,i2,i5)')Year,Month,Day,Hour,Minute,Second
   MSecond=Second/1000.0
   Second=floor(MSecond)
   CALL convert_to_seconds (Year,Month,Day,Hour,Minute,Second,UTC_Time)
   LEAPSECONDS= amsr2_1b_rec%Scan_Time(1)*amsr2_1b_rec%Scan_Time_SF &
              + OFFSET_1970_to_1993 - UTC_Time*1.0_DOUBLE 

print *, "LEAPSECONDS= ", LEAPSECONDS, " Seconds"

!
!  Loop through all the AMSR2_89A GRANUEL.
!
 
   NUM_PIXEL=0
   MISSING=0
TIME_CHECK:   DO AlongTrack = 1,amsr2_1b_rec%AlongTrack
         Time=(amsr2_1b_rec%Scan_Time(AlongTrack)*amsr2_1b_rec%Scan_Time_SF &
              + OFFSET_1970_to_1993 - LEAPSECONDS)*1.0_DOUBLE
         MSecond=Time-floor(Time)

         IF(Time > 0)THEN

            CALL seconds_to_calendar (Time, Year, Month, Day, Hour, Minute, Second)
         ELSE
         cycle TIME_CHECK 
         ENDIF

         MSecond=Second + MSecond
         IDATE = Year*1000000+Month*10000+Day*100+Hour
!print *, "Time=", Year, Month, Day, Hour, Minute, Second

MISSING_CHECK: DO CrossTrack=1,amsr2_1b_rec%CrossTrack2
        P2D_Index=Array_Index(CrossTrack,amsr2_1b_rec%CrossTrack2,AlongTrack,amsr2_1b_rec%AlongTrack)
        ODD_NUM=FLOOR((CrossTrack/2.0)+0.9) 
!print *, "ODD_NUM=", ODD_NUM
        P2AD_Index=Array_Index(ODD_NUM,amsr2_1b_rec%CrossTrack1,AlongTrack,amsr2_1b_rec%AlongTrack)

!
!  Reorder and reformat the data for the BUFR file
!

         ARR = BMISS

!
!  Assignment list for NC021203
!

         ARR(1) = SATID             ! COMMON C  001007  SATELLITE IDENTIFIER
         ARR(2) = CENTID            ! COMMON C  001033  IDENTIFICATION OF ORIGINATING CENTER
         ARR(3) = SUB_CENTID        ! COMMON C  001034  IDENTIFICATION OF ORIGINATING Sub-CENTER
         ARR(4) = AMSR2_ID           ! COMMON C  002019  SATELLITE INSTRUMENTS
         ARR(5) = SATCLS            ! CODE TAB  002020  SATELLITE CLASSIFICATION
         ARR(6) = Year              ! YEAR      004001  YEAR
         ARR(7) = Month             ! MONTH     004002  MONTH
         ARR(8) = Day               ! DAY       004003  DAY
         ARR(9) = Hour              ! HOUR      004004  HOUR
         ARR(10) = Minute            ! MINUTE    004005  MINUTE
         ARR(11) = MSecond           ! SECOND    004006  SECOND
         ARR(12) = ORBN            ! NUMERIC   005040  Orbit number  
         ARR(13) = AlongTrack         ! NUMERIC   005041  SCAN LINE NUMBER
         ARR(14) = CrossTrack       ! NUMERIC   005043  FIELD OF VIEW
         ARR(15) = amsr2_1b_rec%Latitude_for_89A(P2D_Index)*amsr2_1b_rec%Latitude_89A_SF ! 005001 Latitude (high accuracy) 
         ARR(16) = amsr2_1b_rec%Longitude_for_89A(P2D_Index)*amsr2_1b_rec%Longitude_89A_SF ! 006001 Longitude (high accuracy) 
         ARR(17) = amsr2_1b_rec%Sun_Azimuth(P2AD_Index)*amsr2_1b_rec%Sun_Azimuth_SF ! 005022 Solar azimuth 
         IF (amsr2_1b_rec%Sun_Azimuth(P2AD_Index) <0) then
         ARR(17) = 360.0 + amsr2_1b_rec%Sun_Azimuth(P2AD_Index)*amsr2_1b_rec%Sun_Azimuth_SF ! 005022 Solar azimuth 
         ENDIF 
         ARR(18) = amsr2_1b_rec%Sun_Elevation(P2AD_Index)*amsr2_1b_rec%Sun_Elevation_SF ! 007022 Solar elevation
         ARR(19) = amsr2_1b_rec%Earth_Incidence(P2AD_Index)*amsr2_1b_rec%Earth_Incidence_SF ! 025081 Incidence angle

         ARR(20) = amsr2_1b_rec%Earth_Azimuth(P2AD_Index)*amsr2_1b_rec%Earth_Azimuth_SF ! 025082 Azimuth angle 
         IF (amsr2_1b_rec%Earth_Azimuth(P2AD_Index) <0) then
         ARR(20) = 360.0 + amsr2_1b_rec%Earth_Azimuth(P2AD_Index)*amsr2_1b_rec%Earth_Azimuth_SF ! 025082 Azimuth angle 
         ENDIF 

         P3D_Index=Array_Index(473, amsr2_1b_rec%Sector3, AlongTrack,amsr2_1b_rec%AlongTrack)
         ACQF=0
         IF (amsr2_1b_rec%Scan_Data_Quality(P3D_Index) > 0) ACQF=2**17  ! 033032 Channel quality flags for ATOVS
         P3D_Index=Array_Index(474, amsr2_1b_rec%Sector3, AlongTrack,amsr2_1b_rec%AlongTrack)
         IF (amsr2_1b_rec%Scan_Data_Quality(P3D_Index) >0 ) ACQF=2**17  ! 033032 Channel quality flags for ATOVS
         P3D_Index=Array_Index(475, amsr2_1b_rec%Sector3, AlongTrack,amsr2_1b_rec%AlongTrack)
         IF (amsr2_1b_rec%Scan_Data_Quality(P3D_Index) >0 ) ACQF=2**17  ! 033032 Channel quality flags for ATOVS
         P3D_Index=Array_Index(476, amsr2_1b_rec%Sector3, AlongTrack,amsr2_1b_rec%AlongTrack)
         IF (amsr2_1b_rec%Scan_Data_Quality(P3D_Index) >0 ) ACQF=2**17  ! 033032 Channel quality flags for ATOVS
         ARR(21)=ACQF

!         cval='NORMAL'
         cval='A-horn'
         ARR(22)=rval

         NX=22
!       set up 89AGHz_H
         CHNM=1
         NX=NX+1
         ARR(NX)=SCCF(CHNM)             ! 002153 Satellite channel center frequency 
         CH_2D_Index=Array_Index(CrossTrack,amsr2_1b_rec%CrossTrack2,AlongTrack,amsr2_1b_rec%AlongTrack,CHNM,amsr2_1b_rec%Sector3)
         ALFR = (100.0 - amsr2_1b_rec%Land_Ocean_Flag_89(CH_2D_Index)*amsr2_1b_rec%Land_Ocean_89_SF)/100.0 ! 021166 Land fraction
         NX=NX+1
         ARR(NX)=ALFR
         POS=0
         VIIRSQ=(2**10)*IBITS(amsr2_1b_rec%Pixel_Data_Quality_89(P2D_Index),POS,1)
         NX=NX+1
         ARR(NX)=VIIRSQ
         ANPO=0
         NX=NX+1
         ARR(NX)=ANPO
         NX=NX+1
         ARR(NX)=amsr2_1b_rec%BT_890GHzA_H(P2D_Index)*amsr2_1b_rec%BT_890GHzA_H_SF

!       set up 89AGHz_V
         CHNM=1
         NX=NX+1
         ARR(NX)=SCCF(CHNM)             ! 002153 Satellite channel center frequency 
         CH_2D_Index=Array_Index(CrossTrack,amsr2_1b_rec%CrossTrack2,AlongTrack,amsr2_1b_rec%AlongTrack,CHNM,amsr2_1b_rec%Sector1)
         ALFR = (100.0 - amsr2_1b_rec%Land_Ocean_Flag_89(CH_2D_Index)*amsr2_1b_rec%Land_Ocean_89_SF)/100.0 ! 021166 Land fraction
         NX=NX+1
         ARR(NX)=ALFR
         POS=1
         VIIRSQ=(2**10)*IBITS(amsr2_1b_rec%Pixel_Data_Quality_89(P2D_Index),POS,1)
         NX=NX+1
         ARR(NX)=VIIRSQ
         ANPO=1
         NX=NX+1
         ARR(NX)=ANPO
         NX=NX+1
         ARR(NX)=amsr2_1b_rec%BT_890GHzA_V(P2D_Index)*amsr2_1b_rec%BT_890GHzA_V_SF

!print *, "NX=", NX, "AMSR2_89A=", ARR(26), ARR(31), ARR(36), ARR(41), ARR(46), ARR(51), ARR(56), ARR(61), ARR(66), ARR(71), ARR(76), ARR(81)
!print *, "NX=", NX, cval, rval
!
!  Reassign the missing values we use (-9999) to those used for BUFR (10e10).
!
         WHERE(INT(ARR) == BAD_INT)  ARR = BMISS

!
!  Open a BUFR message
!
         NUM_CHAN=2
         CALL OPENMB(LUBFR,SUBSET,IDATE)
         CALL DRFINI(LUBFR,NUM_CHAN,1,'{AMSRCH}')
         CALL UFBSEQ(LUBFR,ARR,ARRDIM,1,IRET,SUBSET)
         CALL PKVS01('MINU',Minute)
         CALL PKVS01('SECO',Second)
         CALL WRITCP(LUBFR)

!
!  If we've obtained our predetermined max number of messages per record then 
!  write what we have.
!

   NUM_PIXEL=NUM_PIXEL+1
   ENDDO MISSING_CHECK ! Cycling through AMSR2_89A FOVs
  ENDDO TIME_CHECK ! Cycling through AMSR2_89A Scan 

   print *, "NUM_PIXEL=", NUM_PIXEL
   print *, "MISSING=", MISSING
   PRINT*,'Closing BUFR file.'

   CALL CLOSBF(LUBFR)

   CLOSE(LUBFR, IOSTAT = Close_Status)
   IF (Close_Status /= 0) THEN
      CALL error_messaging('AMSR2_89A_write_bufr', &
      'Failed TO CLOSE BUFR File', &
      FATAL)
   ENDIF

   CLOSE(LUNDX, IOSTAT = Close_Status)
   IF (Close_Status /= 0) THEN
      CALL error_messaging('AMSR2_89A_write_bufr', &
      'Failed TO CLOSE BUFR Table', &
      FATAL)
   ENDIF

   PRINT*,'Finishing AMSR2_89A_write_bufr'

   RETURN

   END SUBROUTINE AMSR2_89A_write_bufr

