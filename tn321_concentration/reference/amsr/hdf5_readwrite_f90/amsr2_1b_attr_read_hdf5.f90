!------------------------------------------------------------------------------
!
! NAME:
!    amsr2_1b_attr_read_hdf5
!
! CALLING SEQUENCE:
!    Call amsr2_1b_attr_read_hdf5 (amsr2_1b_Rec)
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
!
!------------------------------------------------------------------------------

SUBROUTINE amsr2_1b_attr_read_hdf5 (amsr2_1b_Rec)

   !
   ! Declare modules used 
   !

   USE HDF5
   USE errormsg_module
   USE common_parameters
   USE amsr2_1b_Module
   USE hdf5_attribute_interface

   IMPLICIT NONE

   !
   !  Variable declarations
   !

   TYPE (amsr2_1b_record) :: amsr2_1b_Rec

   INTEGER :: error ! Error flag
   INTEGER(HID_T) :: file_id

   !
   !  Initialize HDF5 FORTRAN interface.
   CALL h5open_f(error)
   IF(error == -1)THEN
       CALL error_messaging('amsr2_1b_attr_read_hdf5', &
       'Error Initialize FORTRAN INTERFACE', FATAL)
   ENDIF
   !
   !  Open an existing HDF5 data file.
   ! H5F_ACC_RDWR_F -- Read&Write, H5F_ACC_RDONLY_F -- ReadOnly
   CALL h5fopen_f (trim(amsr2_1b_Rec%FileName), H5F_ACC_RDONLY_F, file_id, error)
   IF(error == -1)THEN
       CALL error_messaging('amsr2_1b_attr_read_hdf5', &
       'Error OPEN ' // amsr2_1b_Rec%FileName, FATAL)
   ENDIF
   !


   ! Read HDF5 dataset

   !
   !  Read the Brightness Temperature (10.7GHz,H)
   !
   CALL read_hdf5_attribute(file_id, TRIM(amsr2_1b_Rec%group)//"Brightness Temperature (10.7GHz,H)", &
   "SCALE FACTOR", amsr2_1b_Rec%BT_107GHz_H_SF)
   !
   !  Read the Brightness Temperature (10.7GHz,V)
   !
   CALL read_hdf5_attribute(file_id, TRIM(amsr2_1b_Rec%group)//"Brightness Temperature (10.7GHz,V)", &
   "SCALE FACTOR", amsr2_1b_Rec%BT_107GHz_V_SF)
   !
   !  Read the Brightness Temperature (18.7GHz,H)
   !
   CALL read_hdf5_attribute(file_id, TRIM(amsr2_1b_Rec%group)//"Brightness Temperature (18.7GHz,H)", &
   "SCALE FACTOR", amsr2_1b_Rec%BT_187GHz_H_SF)

   !
   !  Read the Brightness Temperature (18.7GHz,V)
   !
   CALL read_hdf5_attribute(file_id, TRIM(amsr2_1b_Rec%group)//"Brightness Temperature (18.7GHz,V)", &
   "SCALE FACTOR", amsr2_1b_Rec%BT_187GHz_V_SF)
   !
   !  Read the Brightness Temperature (23.8GHz,H)
   !
   CALL read_hdf5_attribute(file_id, TRIM(amsr2_1b_Rec%group)//"Brightness Temperature (23.8GHz,H)", &
   "SCALE FACTOR", amsr2_1b_Rec%BT_238GHz_H_SF)

   !
   !  Read the Brightness Temperature (23.8GHz,V)
   !
   CALL read_hdf5_attribute(file_id, TRIM(amsr2_1b_Rec%group)//"Brightness Temperature (23.8GHz,V)", &
   "SCALE FACTOR", amsr2_1b_Rec%BT_238GHz_V_SF)

   !
   !  Read the Brightness Temperature (36.5GHz,H)
   !
   CALL read_hdf5_attribute(file_id, TRIM(amsr2_1b_Rec%group)//"Brightness Temperature (36.5GHz,H)", &
   "SCALE FACTOR", amsr2_1b_Rec%BT_365GHz_H_SF)

   !
   !  Read the Brightness Temperature (36.5GHz,V)
   !
   CALL read_hdf5_attribute(file_id, TRIM(amsr2_1b_Rec%group)//"Brightness Temperature (36.5GHz,V)", &
   "SCALE FACTOR", amsr2_1b_Rec%BT_365GHz_V_SF)

   !
   !  Read the Brightness Temperature (6.9GHz,H)
   !
   CALL read_hdf5_attribute(file_id, TRIM(amsr2_1b_Rec%group)//"Brightness Temperature (6.9GHz,H)", &
   "SCALE FACTOR", amsr2_1b_Rec%BT_69GHz_H_SF)

   !
   !  Read the Brightness Temperature (6.9GHz,V)
   !
   CALL read_hdf5_attribute(file_id, TRIM(amsr2_1b_Rec%group)//"Brightness Temperature (6.9GHz,V)", &
   "SCALE FACTOR", amsr2_1b_Rec%BT_69GHz_V_SF)

   !
   !  Read the Brightness Temperature (7.3GHz,H)
   !
   CALL read_hdf5_attribute(file_id, TRIM(amsr2_1b_Rec%group)//"Brightness Temperature (7.3GHz,H)", &
   "SCALE FACTOR", amsr2_1b_Rec%BT_73GHz_H_SF)

   !
   !  Read the Brightness Temperature (7.3GHz,V)
   !
   CALL read_hdf5_attribute(file_id, TRIM(amsr2_1b_Rec%group)//"Brightness Temperature (7.3GHz,V)", &
   "SCALE FACTOR", amsr2_1b_Rec%BT_73GHz_V_SF)

   !
   !  Read the Earth Azimuth
   !
   CALL read_hdf5_attribute(file_id, TRIM(amsr2_1b_Rec%group)//"Earth Azimuth", &
   "SCALE FACTOR", amsr2_1b_Rec%Earth_Azimuth_SF)

   !
   !  Read the Earth Incidence
   !
   CALL read_hdf5_attribute(file_id, TRIM(amsr2_1b_Rec%group)//"Earth Incidence", &
   "SCALE FACTOR", amsr2_1b_Rec%Earth_Incidence_SF)

   !
   !  Read the Sun Azimuth
   !
   CALL read_hdf5_attribute(file_id, TRIM(amsr2_1b_Rec%group)//"Sun Azimuth", &
   "SCALE FACTOR", amsr2_1b_Rec%Sun_Azimuth_SF)

   !
   !  Read the Sun Elevation
   !
   CALL read_hdf5_attribute(file_id, TRIM(amsr2_1b_Rec%group)//"Sun Elevation", &
   "SCALE FACTOR", amsr2_1b_Rec%Sun_Elevation_SF)

   !
   !  Read the Brightness Temperature (89.0GHz-A,H)
   !
   CALL read_hdf5_attribute(file_id, TRIM(amsr2_1b_Rec%group)//"Brightness Temperature (89.0GHz-A,H)", &
   "SCALE FACTOR", amsr2_1b_Rec%BT_890GHzA_H_SF)

   !
   !  Read the Brightness Temperature (89.0GHz-A,V)
   !
   CALL read_hdf5_attribute(file_id, TRIM(amsr2_1b_Rec%group)//"Brightness Temperature (89.0GHz-A,V)", &
   "SCALE FACTOR", amsr2_1b_Rec%BT_890GHzA_V_SF)

   !
   !  Read the Brightness Temperature (89.0GHz-B,H)
   !
   CALL read_hdf5_attribute(file_id, TRIM(amsr2_1b_Rec%group)//"Brightness Temperature (89.0GHz-B,H)", &
   "SCALE FACTOR", amsr2_1b_Rec%BT_890GHzB_H_SF)

   !
   !  Read the Brightness Temperature (89.0GHz-B,V)
   !
   CALL read_hdf5_attribute(file_id, TRIM(amsr2_1b_Rec%group)//"Brightness Temperature (89.0GHz-B,V)", &
   "SCALE FACTOR", amsr2_1b_Rec%BT_890GHzB_V_SF)

   !
   !  Read the Latitude of Observation Point for 89A
   !
   CALL read_hdf5_attribute(file_id, TRIM(amsr2_1b_Rec%group)//"Latitude of Observation Point for 89A", &
   "SCALE FACTOR", amsr2_1b_Rec%Latitude_89A_SF)

   !
   !  Read the Latitude of Observation Point for 89B
   !
   CALL read_hdf5_attribute(file_id, TRIM(amsr2_1b_Rec%group)//"Latitude of Observation Point for 89B", &
   "SCALE FACTOR", amsr2_1b_Rec%Latitude_89B_SF)

   !
   !  Read the Longitude of Observation Point for 89A
   !
   CALL read_hdf5_attribute(file_id, TRIM(amsr2_1b_Rec%group)//"Longitude of Observation Point for 89A", &
   "SCALE FACTOR", amsr2_1b_Rec%Longitude_89A_SF)

   !
   !  Read the Longitude of Observation Point for 89B
   !
   CALL read_hdf5_attribute(file_id, TRIM(amsr2_1b_Rec%group)//"Longitude of Observation Point for 89B", &
   "SCALE FACTOR", amsr2_1b_Rec%Longitude_89B_SF)

   !
   !  Read the Land_Ocean Flag 6 to 36
   !
   CALL read_hdf5_attribute(file_id, TRIM(amsr2_1b_Rec%group)//"Land_Ocean Flag 6 to 36", &
   "SCALE FACTOR", amsr2_1b_Rec%Land_Ocean_6_SF)

   !
   !  Read the Land_Ocean Flag 89
   !
   CALL read_hdf5_attribute(file_id, TRIM(amsr2_1b_Rec%group)//"Land_Ocean Flag 89", &
   "SCALE FACTOR", amsr2_1b_Rec%Land_Ocean_89_SF)

   !
   !  Read the Scan Time
   !
   CALL read_hdf5_attribute(file_id, TRIM(amsr2_1b_Rec%group)//"Scan Time", &
   "SCALE FACTOR", amsr2_1b_Rec%Scan_Time_SF)

   ! Close the HDF5 data file.
   CALL h5fclose_f(file_id, error)
   ! Close HDF5 FORTRAN interface.
   CALL h5close_f(error)
   
   RETURN

END Subroutine amsr2_1b_attr_read_hdf5
