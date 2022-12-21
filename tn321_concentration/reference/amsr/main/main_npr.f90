!------------------------------------------------------------------------------
!
! (C) PSGS
!
!
! NAME:
!       main_npr
!
! PURPOSE:
!       This is a main program for the NOAA Product Reformatter (NPR).  It currently
!       converts between netCDF and BUFR for different product types.  This will be
!       expanded to do GRIB2 later on.
!
! CATEGORY:
!       Main Program
!
! CALLING SEQUENCE:
!       ./ main_npr
!
! INPUTS:
!       npr.filenames -- a resource file containing the direction of conversion, product 
!                        type, and the names of the input and output files.
!
! OUTPUTS:
!       None
!
! KNOWN BUGS AND LIMITATIONS:
!       None known.
!
! CALLS:
!
!
! MODULES:
!
!
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
!       Written by:  Thomas King (01/27/2009)
!                    PSGS
!                    Fairfax, VA
!                    Thomas.S.King@noaa.gov
!
!       Modified by: Yi Song (09/30/2009)
!                    RTI 
!                    Yi.Song@noaa.gov
!
!
!------------------------------------------------------------------------------
!

Program main_npr

!
!  The Modules used in the code
!

   USE type_kinds
   USE errormsg_module

   IMPLICIT NONE

!
!  Variable Definitions
!

   INTEGER(LONG) :: Which_Dimension
   INTEGER(LONG) :: Number_of_Dimensions
   INTEGER(LONG) :: Unit_Number_Resource_File
   INTEGER(LONG) :: Open_Status
   INTEGER(LONG) :: Read_Status
   INTEGER(LONG) :: File_Status
   INTEGER(LONG) :: Close_Status
   INTEGER(LONG) :: Status

   CHARACTER(250) :: Conversion_Scheme
   CHARACTER(250) :: Product_Type
   CHARACTER(250) :: BUFR_FileName
   CHARACTER(250) :: Input_FileName
   CHARACTER(250) :: Output_FileName
   CHARACTER(250) :: BUFR_Table_FileName
   CHARACTER(250) :: Choice 

!
!  Declare for some functions.
!

   INTEGER(LONG) :: Get_Lun
   REAL(DOUBLE) :: cpu_t1,cpu_t2
!
!  Begin the program.
!

!
!  Open the input resource file.
!

   PRINT*,'main_npr is now starting.'
      call cpu_time(cpu_t1)
   Unit_Number_Resource_File = Get_Lun();
   OPEN (UNIT=Unit_Number_Resource_File, File = "npr.filenames", &
         Status="OLD", ACTION = "READ", IOSTAT=Open_Status)

   IF(Open_Status /= 0)THEN
      CALL error_messaging('main_npr', &
      'Failed to open npr.filenames', &
      FATAL)
   ENDIF

!
!  Read the input resource file.
!

   READ(Unit_Number_Resource_File,FMT='(a)',IOSTAT=Read_Status)Product_Type

   IF(Read_Status /= 0)THEN
      CALL error_messaging('main_npr', &
      'Failed to read Product_Type from npr.filenames', &
      FATAL)
   ENDIF

   Product_Type = ADJUSTL(Product_Type)

   READ(Unit_Number_Resource_File,FMT='(a)',IOSTAT=Read_Status)Conversion_Scheme

   IF(Read_Status /= 0)THEN
      CALL error_messaging('main_npr', &
      'Failed to read Conversion_Scheme from npr.filenames', &
      FATAL)
   ENDIF
   
   Conversion_Scheme = ADJUSTL(Conversion_Scheme)

!
!  Now, we figure out what to do with the information we have.
!

   SELECT CASE (TRIM(Product_Type))

   CASE("AMSR2-1B")
      SELECT CASE (TRIM(Conversion_Scheme))
      CASE("NC2BUFR")
         CALL AMSR2_1B_hdf5_to_bufr(Unit_Number_Resource_File)
      CASE("BUFR2NC")
!         CALL AMSR2_1B_bufr_to_netcdf(Unit_Number_Resource_File)
      CASE DEFAULT
         CALL error_messaging('main_npr', &
         'None of the input Conversion Types match known definitions', &
         FATAL)
      END SELECT
   CASE DEFAULT

      CALL error_messaging('main_npr', &
      'None of the input Product Types match known definitions', &
      FATAL)

   END SELECT

!
!  Close the resource file.
!

   CLOSE (UNIT=Unit_Number_Resource_File, IOSTAT=Close_Status)

   IF(Close_Status /= 0)THEN
      CALL error_messaging('main_npr', &
      'Failed to close npr.filenames', &
      FATAL)
   ENDIF

   PRINT*,'main_npr is now finished.'
   call cpu_time(cpu_t2)
   print *, "CPU time for the whole program is:", cpu_t2-cpu_t1
END PROGRAM

