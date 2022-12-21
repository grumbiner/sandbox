!------------------------------------------------------------------------------
!
! NAME:
!       AMSR2_1B_hdf5_to_bufr
!
! PURPOSE:
!       This is a subroutine for the NOAA Product Reformatter (NPR).  It reads 
!       the AMSR2 1B in HDF5 and converts it to BUFR.
!
! CATEGORY:
!       Subroutine
!
! CALLING SEQUENCE:
!       CALL AMSR2_1B_hdf5_to_bufr(Unit_Number_Resource_File)
!
!
! INPUTS:
!      Unit_Number_Resource_File  - The file route number 
!
! OUTPUTS:
!      None
!
! KNOWN BUGS AND LIMITATIONS:
!       None known.
!
! CALLS:
!    error_messaging
!
! MODULES:
!    type_kinds
!    errormsg_module
!    common_parameters
!    amsr2_1b_module
!    hdf5_defines
!    read_hdf5_interface
!    AMSR2_1B_write_bufr_interface
!
! SIDE EFFECTS:
!       None known
!
! RESTRICTIONS:
!       None
!
! PROCEDURE:
!
! EXAMPLE:
!
! MODIFICATION HISTORY:
!        Written by: Yi Song (09/30/2013)
!                    IMSG
!                    Yi.Song@noaa.gov
!
!
!
!------------------------------------------------------------------------------
!

   SUBROUTINE AMSR2_1B_hdf5_to_bufr (Unit_Number_Resource_File)

!
! Declare modules used
!

   USE type_kinds
   USE errormsg_module
   USE common_parameters
   USE HDF5
   USE hdf5_interface
   USE amsr2_1b_module

   IMPLICIT NONE

!
!  Variable declarations
!

   TYPE (amsr2_1b_record) :: amsr2_1b_rec
   INTEGER(LONG) :: Which_Dimension
   INTEGER(LONG) :: Number_of_Dimensions
   INTEGER(LONG) :: File_Status
   INTEGER(LONG) :: Unit_Number_Resource_File
   INTEGER(LONG) :: Read_Status, Close_Status, Open_Status
   INTEGER(LONG) :: ORBN

   CHARACTER(250) :: BUFR_FileName
   CHARACTER(250) :: BUFR89A_FileName
   CHARACTER(250) :: BUFR89B_FileName
   CHARACTER(250) :: Input_FileName
   CHARACTER(250) :: Output_FileName
   CHARACTER(250) :: BUFR_Table_FileName
   CHARACTER(250) :: AggregateBeginningTime

!
!  Fuction delcarations
!

   INTEGER(LONG) :: Get_netCDF_Dimensions
   INTEGER(LONG) :: Get_Lun

!
!  Start the program
!

   PRINT*,'Starting AMSR2_1B_hdf5_to_bufr'

!  Read the input file name.
!

   READ(Unit_Number_Resource_File, FMT='(a)',IOSTAT=Read_Status) &
     Input_FileName 

   IF(Read_Status /= 0)THEN
      CALL error_messaging('AMSR2_1B_hdf5_to_bufr', &
      'Failed to read npr.filenames', FATAL) 
   ENDIF
     amsr2_1b_rec%FileName=TRIM(Input_FileName)

!
!  Read the output file name.
!

   READ(Unit_Number_Resource_File, FMT='(a)',IOSTAT=Read_Status) &
     Output_FileName

   IF(Read_Status /= 0)THEN
      CALL error_messaging('AMSR2_1B_hdf5_to_bufr', &
      'Failed to read npr.filenames', &
      FATAL)
   ENDIF
         BUFR_FileName = TRIM(Output_FileName)
print *, "BUFR_FileName=", BUFR_FileName

   READ(Unit_Number_Resource_File, FMT='(a)',IOSTAT=Read_Status) &
     Output_FileName

   IF(Read_Status /= 0)THEN
      CALL error_messaging('AMSR2_1B_hdf5_to_bufr', &
      'Failed to read npr.filenames', &
      FATAL)
   ENDIF
         BUFR89A_FileName = TRIM(Output_FileName)
print *, "BUFR89A_FileName=", BUFR89A_FileName

   READ(Unit_Number_Resource_File, FMT='(a)',IOSTAT=Read_Status) &
     Output_FileName

   IF(Read_Status /= 0)THEN
      CALL error_messaging('AMSR2_1B_hdf5_to_bufr', &
      'Failed to read npr.filenames', &
      FATAL)
   ENDIF
         BUFR89B_FileName = TRIM(Output_FileName)
print *, "BUFR89B_FileName=", BUFR89A_FileName

!
!  Read the BUFR table name
!

         READ(Unit_Number_Resource_File, FMT='(a)',IOSTAT=Read_Status) &
           BUFR_Table_FileName

         IF(Read_Status /= 0)THEN
            CALL error_messaging('AMSR2_1B_hdf5_to_bufr', &
            'Failed to read npr.filenames', &
            FATAL)
         ENDIF

   READ(Unit_Number_Resource_File, FMT='(I5)',IOSTAT=Read_Status) &
     ORBN

   IF(Read_Status /= 0)THEN
      CALL error_messaging('AMSR2_1b_hdf5_to_bufr', &
      'Failed to read npr.filenames', &
      FATAL)
   ENDIF

   READ(Unit_Number_Resource_File, FMT='(a)',IOSTAT=Read_Status) AggregateBeginningTime
print *, "AggregateBeginningTime=", AggregateBeginningTime

   IF(Read_Status /= 0)THEN
      CALL error_messaging('AMSR2_1b_hdf5_to_bufr', &
      'Failed to read npr.filenames', &
      FATAL)
   ENDIF

   !
   !  Close the resource file.
   !

   CLOSE(Unit_Number_Resource_File, IOSTAT = Close_Status)

   IF (Close_Status /= 0) THEN
      CALL error_messaging('AMSR2_1B_hdf5_to_bufr', &
      'Failed TO CLOSE npr.filenames.', &
      FATAL)
   ENDIF

   !
   !  Check to see if we have any input/output files
   !

   File_Status = LEN_TRIM (amsr2_1b_rec%FileName)
   IF (File_Status <= 0 .OR. File_Status >= 240) THEN
      CALL error_messaging('main_AMSR2_1b_preprocessor', &
      'The input netCDF file name is a zero-length string.', &
      FATAL)
   ENDIF

   !
   ! Set HDF5 group names
   !

   amsr2_1b_rec%group = ""
!   amsr2_1b_rec%group = "/"

   !
   !  We need to get dimensions of the arrays so we can allocate space for
   !  the input data structure.
   !
   !
   !  Read the dimesion from Brightness Temperature (10.7GHz,H)
   !

   Which_Dimension = 1
   CALL get_hdf5_dimensions(amsr2_1b_rec%FileName, &
   TRIM(amsr2_1b_rec%group)//"Brightness Temperature (10.7GHz,H)", Which_Dimension, amsr2_1b_rec%CrossTrack1)

print *, "amsr2_1b_rec%CrossTrack1=", amsr2_1b_rec%CrossTrack1

   Which_Dimension = 2 
   CALL get_hdf5_dimensions(amsr2_1b_rec%FileName, &
   TRIM(amsr2_1b_rec%group)//"Brightness Temperature (10.7GHz,H)", Which_Dimension, amsr2_1b_rec%AlongTrack)
print *, "amsr2_1b_rec%AlongTrack=", amsr2_1b_rec%AlongTrack

   Which_Dimension = 1
   CALL get_hdf5_dimensions(amsr2_1b_rec%FileName, &
   TRIM(amsr2_1b_rec%group)//"Latitude of Observation Point for 89A", Which_Dimension, amsr2_1b_rec%CrossTrack2)
print *, "amsr2_1b_rec%CrossTrack2=", amsr2_1b_rec%CrossTrack2

   Which_Dimension = 3
   CALL get_hdf5_dimensions(amsr2_1b_rec%FileName, &
   TRIM(amsr2_1b_rec%group)//"Land_Ocean Flag 6 to 36", Which_Dimension, amsr2_1b_rec%Sector1)
print *, "amsr2_1b_rec%Sector1=", amsr2_1b_rec%Sector1

   Which_Dimension = 3
   CALL get_hdf5_dimensions(amsr2_1b_rec%FileName, &
   TRIM(amsr2_1b_rec%group)//"Land_Ocean Flag 89", Which_Dimension, amsr2_1b_rec%Sector2)
print *, "amsr2_1b_rec%Sector2=", amsr2_1b_rec%Sector2

   Which_Dimension = 1
   CALL get_hdf5_dimensions(amsr2_1b_rec%FileName, &
   TRIM(amsr2_1b_rec%group)//"Scan Data Quality", Which_Dimension, amsr2_1b_rec%Sector3)
print *, "amsr2_1b_rec%Sector3=", amsr2_1b_rec%Sector3

      amsr2_1b_rec%Allocate_Status = 0
      CALL amsr2_1b_allocate(amsr2_1b_rec)
      IF(amsr2_1b_rec%Allocate_Status /= 0)THEN
         CALL error_messaging('main_AMSR2_1b_preprocessor', &
         'Failed to allocate memory for amsr2_1b_rec.', FATAL)
      ENDIF

!
!  Initialize the nucaps_all and preprocessor data
!
      CALL amsr2_1b_init(amsr2_1b_rec)

!
!  Read the netCDF file.
!

      ! Read the input netCDF data file
      CALL amsr2_1b_read_hdf5(amsr2_1b_rec)
      CALL amsr2_1b_attr_read_hdf5(amsr2_1b_rec)

!
!  Write the BUFR file.
!
print *, "AggregateBeginningTime=", AggregateBeginningTime
   CALL AMSR2_1b_write_bufr(amsr2_1b_rec, BUFR_FileName,BUFR_Table_FileName, ORBN, AggregateBeginningTime)
   CALL AMSR2_89A_write_bufr(amsr2_1b_rec, BUFR89A_FileName,BUFR_Table_FileName, ORBN, AggregateBeginningTime)
   CALL AMSR2_89B_write_bufr(amsr2_1b_rec, BUFR89B_FileName,BUFR_Table_FileName, ORBN, AggregateBeginningTime)
!
!  Deallocate data structures.
!

      amsr2_1b_rec%Deallocate_Status = 0
      CALL amsr2_1b_deallocate (amsr2_1b_rec)
      IF(amsr2_1b_rec%Deallocate_Status /= 0)THEN
         CALL error_messaging('main_AMSR2_1b_preprocessor', &
         'Failed to deallocate memory for amsr2_1b_rec.', FATAL)
      ENDIF

   PRINT*,'Finishing AMSR2_1B_hdf5_to_bufr'

   RETURN

END Subroutine AMSR2_1B_hdf5_to_bufr
