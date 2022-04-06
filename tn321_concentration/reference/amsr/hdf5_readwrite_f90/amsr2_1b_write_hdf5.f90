!------------------------------------------------------------------------------
!
! (C) IMSG  
!
!
! NAME:
!    amsr2_1b_write_hdf5
!
! CALLING SEQUENCE:
!    Call amsr2_1b_write_hdf5 (amsr2_1b_Rec)
!
! INPUTS:
!    amsr2_1b_Rec
!
! OUTPUTS:
!    amsr2_1b_Rec
!
! HISTORY:
!     Created by: Yi Song, IMSG, 2013 
!                 Yi.Song@noaa.gov
!     Program is generated by the code generator.
!
!
!------------------------------------------------------------------------------

SUBROUTINE amsr2_1b_write_hdf5 (amsr2_1b_Rec)

   !
   ! Declare modules used
   !

   USE HDF5
   USE errormsg_module
   USE common_parameters
   USE amsr2_1b_Module
   USE hdf5_interface

   IMPLICIT NONE

   !
   !  Variable declarations
   !

   TYPE (amsr2_1b_record) :: amsr2_1b_Rec

   INTEGER :: error ! Error flag
   INTEGER(HID_T) :: file_id
   INTEGER(HSIZE_T) :: dims(MAX_HDF_DIMS)

   !
   !  Initialize HDF5 FORTRAN interface.
   CALL h5open_f(error)
   IF(error == -1)THEN
       CALL error_messaging('amsr2_1b_write_hdf5', &
       'Error Initialize FORTRAN INTERFACE', FATAL)
   ENDIF
   !
   !  Open an existing HDF5 data file.
   ! H5F_ACC_RDWR_F -- Read&Write, H5F_ACC_RDWR_F -- REadand Write
   CALL h5fopen_f (amsr2_1b_Rec%FileName, H5F_ACC_RDWR_F, file_id, error)
   IF(error == -1)THEN
       CALL error_messaging('amsr2_1b_write_hdf5', &
       'Error OPEN ' // amsr2_1b_Rec%FileName, FATAL)
   ENDIF
   !

   ! Write HDF5 dataset
   dims = 1

   !
   !  Read the Brightness Temperature (10.7GHz,H)
   !
   CALL write_hdf5(file_id, TRIM(amsr2_1b_Rec%group)//"Brightness Temperature (10.7GHz,H)", &
   amsr2_1b_Rec%BT_107GHz_H, dims)

   !
   !  Read the Brightness Temperature (10.7GHz,V)
   !
   CALL write_hdf5(file_id, TRIM(amsr2_1b_Rec%group)//"Brightness Temperature (10.7GHz,V)", &
   amsr2_1b_Rec%BT_107GHz_V, dims)

   !
   !  Read the Brightness Temperature (18.7GHz,H)
   !
   CALL write_hdf5(file_id, TRIM(amsr2_1b_Rec%group)//"Brightness Temperature (18.7GHz,H)", &
   amsr2_1b_Rec%BT_187GHz_H, dims)

   !
   !  Read the Brightness Temperature (18.7GHz,V)
   !
   CALL write_hdf5(file_id, TRIM(amsr2_1b_Rec%group)//"Brightness Temperature (18.7GHz,V)", &
   amsr2_1b_Rec%BT_187GHz_V, dims)

   !
   !  Read the Brightness Temperature (23.8GHz,H)
   !
   CALL write_hdf5(file_id, TRIM(amsr2_1b_Rec%group)//"Brightness Temperature (23.8GHz,H)", &
   amsr2_1b_Rec%BT_238GHz_H, dims)

   !
   !  Read the Brightness Temperature (23.8GHz,V)
   !
   CALL write_hdf5(file_id, TRIM(amsr2_1b_Rec%group)//"Brightness Temperature (23.8GHz,V)", &
   amsr2_1b_Rec%BT_238GHz_V, dims)

   !
   !  Read the Brightness Temperature (36.5GHz,H)
   !
   CALL write_hdf5(file_id, TRIM(amsr2_1b_Rec%group)//"Brightness Temperature (36.5GHz,H)", &
   amsr2_1b_Rec%BT_365GHz_H, dims)

   !
   !  Read the Brightness Temperature (36.5GHz,V)
   !
   CALL write_hdf5(file_id, TRIM(amsr2_1b_Rec%group)//"Brightness Temperature (36.5GHz,V)", &
   amsr2_1b_Rec%BT_365GHz_V, dims)

   !
   !  Read the Brightness Temperature (6.9GHz,H)
   !
   CALL write_hdf5(file_id, TRIM(amsr2_1b_Rec%group)//"Brightness Temperature (6.9GHz,H)", &
   amsr2_1b_Rec%BT_69GHz_H, dims)

   !
   !  Read the Brightness Temperature (6.9GHz,V)
   !
   CALL write_hdf5(file_id, TRIM(amsr2_1b_Rec%group)//"Brightness Temperature (6.9GHz,V)", &
   amsr2_1b_Rec%BT_69GHz_V, dims)

   !
   !  Read the Brightness Temperature (7.3GHz,H)
   !
   CALL write_hdf5(file_id, TRIM(amsr2_1b_Rec%group)//"Brightness Temperature (7.3GHz,H)", &
   amsr2_1b_Rec%BT_73GHz_H, dims)

   !
   !  Read the Brightness Temperature (7.3GHz,V)
   !
   CALL write_hdf5(file_id, TRIM(amsr2_1b_Rec%group)//"Brightness Temperature (7.3GHz,V)", &
   amsr2_1b_Rec%BT_73GHz_V, dims)

   !
   !  Read the Earth Azimuth
   !
   CALL write_hdf5(file_id, TRIM(amsr2_1b_Rec%group)//"Earth Azimuth", &
   amsr2_1b_Rec%Earth_Azimuth, dims)

   !
   !  Read the Earth Incidence
   !
   CALL write_hdf5(file_id, TRIM(amsr2_1b_Rec%group)//"Earth Incidence", &
   amsr2_1b_Rec%Earth_Incidence, dims)

   !
   !  Read the Sun Azimuth
   !
   CALL write_hdf5(file_id, TRIM(amsr2_1b_Rec%group)//"Sun Azimuth", &
   amsr2_1b_Rec%Sun_Azimuth, dims)

   !
   !  Read the Sun Elevation
   !
   CALL write_hdf5(file_id, TRIM(amsr2_1b_Rec%group)//"Sun Elevation", &
   amsr2_1b_Rec%Sun_Elevation, dims)

   !
   !  Read the Brightness Temperature (89.0GHz-A,H)
   !
   CALL write_hdf5(file_id, TRIM(amsr2_1b_Rec%group)//"Brightness Temperature (89.0GHz-A,H)", &
   amsr2_1b_Rec%BT_890GHzA_H, dims)

   !
   !  Read the Brightness Temperature (89.0GHz-A,V)
   !
   CALL write_hdf5(file_id, TRIM(amsr2_1b_Rec%group)//"Brightness Temperature (89.0GHz-A,V)", &
   amsr2_1b_Rec%BT_890GHzA_V, dims)

   !
   !  Read the Brightness Temperature (89.0GHz-B,H)
   !
   CALL write_hdf5(file_id, TRIM(amsr2_1b_Rec%group)//"Brightness Temperature (89.0GHz-B,H)", &
   amsr2_1b_Rec%BT_890GHzB_H, dims)

   !
   !  Read the Brightness Temperature (89.0GHz-B,V)
   !
   CALL write_hdf5(file_id, TRIM(amsr2_1b_Rec%group)//"Brightness Temperature (89.0GHz-B,V)", &
   amsr2_1b_Rec%BT_890GHzB_V, dims)

   !
   !  Read the Latitude of Observation Point for 89A
   !
   CALL write_hdf5(file_id, TRIM(amsr2_1b_Rec%group)//"Latitude of Observation Point for 89A", &
   amsr2_1b_Rec%Latitude_for_89A, dims)

   !
   !  Read the Latitude of Observation Point for 89B
   !
   CALL write_hdf5(file_id, TRIM(amsr2_1b_Rec%group)//"Latitude of Observation Point for 89B", &
   amsr2_1b_Rec%Latitude_for_89B, dims)

   !
   !  Read the Longitude of Observation Point for 89A
   !
   CALL write_hdf5(file_id, TRIM(amsr2_1b_Rec%group)//"Longitude of Observation Point for 89A", &
   amsr2_1b_Rec%Longitude_for_89A, dims)

   !
   !  Read the Longitude of Observation Point for 89B
   !
   CALL write_hdf5(file_id, TRIM(amsr2_1b_Rec%group)//"Longitude of Observation Point for 89B", &
   amsr2_1b_Rec%Longitude_for_89B, dims)

   !
   !  Read the Pixel Data Quality 6 to 36
   !
   CALL write_hdf5(file_id, TRIM(amsr2_1b_Rec%group)//"Pixel Data Quality 6 to 36", &
   amsr2_1b_Rec%Pixel_Data_Quality_6_to_36, dims)

   !
   !  Read the Pixel Data Quality 89
   !
   CALL write_hdf5(file_id, TRIM(amsr2_1b_Rec%group)//"Pixel Data Quality 89", &
   amsr2_1b_Rec%Pixel_Data_Quality_89, dims)

   !
   !  Read the Land_Ocean Flag 6 to 36
   !
   CALL write_hdf5(file_id, TRIM(amsr2_1b_Rec%group)//"Land_Ocean Flag 6 to 36", &
   amsr2_1b_Rec%Land_Ocean_Flag_6_to_36, dims)

   !
   !  Read the Land_Ocean Flag 89
   !
   CALL write_hdf5(file_id, TRIM(amsr2_1b_Rec%group)//"Land_Ocean Flag 89", &
   amsr2_1b_Rec%Land_Ocean_Flag_89, dims)

   !
   !  Read the Scan Data Quality
   !
   CALL write_hdf5(file_id, TRIM(amsr2_1b_Rec%group)//"Scan Data Quality", &
   amsr2_1b_Rec%Scan_Data_Quality, dims)

   !
   !  Read the Scan Time
   !
   CALL write_hdf5(file_id, TRIM(amsr2_1b_Rec%group)//"Scan Time", &
   amsr2_1b_Rec%Scan_Time, dims)


   ! Close the HDF5 data file.
   CALL h5fclose_f(file_id, error)
   ! Close HDF5 FORTRAN interface.
   CALL h5close_f(error)

   RETURN

END Subroutine amsr2_1b_write_hdf5
