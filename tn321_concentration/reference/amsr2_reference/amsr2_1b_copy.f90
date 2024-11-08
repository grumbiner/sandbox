!------------------------------------------------------------------------------
!
! (C) IMSG  
!
!
! NAME:
!    amsr2_1b_copy
!
! FUNCTION:
!    TO BE ADDED
!
! DESCRIPTION:
!    TO BE ADDED
!
! REFERENCE:
!    TO BE ADDED
!
! CALLING SEQUENCE:
!    Call amsr2_1b_copy (amsr2_1b_Rec_Src,amsr2_1b_Rec_Des)
!
! INPUTS:
!    amsr2_1b_Rec_Src
!    amsr2_1b_Rec_Des
!
! OUTPUTS:
!    amsr2_1b_Rec_Des
!
! DEPENDENCIES:
!    TO BE ADDED
!
! SIDE EFFECTS:
!    TO BE ADDED
!
! HISTORY:
!     Created by: Yi Song, IMSG, 2013 
!                 Yi.Song@noaa.gov
!     Program is generated by the code generator,
!         which is written by Qingzhao Guo, AIT
!
! TARGET PROCESSOR:
!    TO BE ADDED
!
!
!------------------------------------------------------------------------------

SUBROUTINE amsr2_1b_copy (amsr2_1b_Rec_Src,amsr2_1b_Rec_Des)

   !
   ! Declare modules used 
   !

   USE type_kinds
   USE errormsg_module
   USE common_parameters
   USE amsr2_1b_Module

   IMPLICIT NONE

   TYPE (amsr2_1b_record) :: amsr2_1b_Rec_Src
   TYPE (amsr2_1b_record) :: amsr2_1b_Rec_Des

   !
   !  Start the processing
   !

   !
   !  Copy scalar variable values
   !

   amsr2_1b_Rec_Des%HDF5FileName = amsr2_1b_Rec_Src%HDF5FileName

   amsr2_1b_Rec_Des%FileName = amsr2_1b_Rec_Src%FileName

   amsr2_1b_Rec_Des%group = amsr2_1b_Rec_Src%group

   amsr2_1b_Rec_Des%Allocate_Status = amsr2_1b_Rec_Src%Allocate_Status

   amsr2_1b_Rec_Des%Deallocate_Status = amsr2_1b_Rec_Src%Deallocate_Status

   amsr2_1b_Rec_Des%AlongTrack = amsr2_1b_Rec_Src%AlongTrack

   amsr2_1b_Rec_Des%CrossTrack1 = amsr2_1b_Rec_Src%CrossTrack1

   amsr2_1b_Rec_Des%CrossTrack2 = amsr2_1b_Rec_Src%CrossTrack2

   amsr2_1b_Rec_Des%Sector1 = amsr2_1b_Rec_Src%Sector1

   amsr2_1b_Rec_Des%Sector2 = amsr2_1b_Rec_Src%Sector2

   amsr2_1b_Rec_Des%Sector3 = amsr2_1b_Rec_Src%Sector3


   !
   !  Copy array variable values
   !

   amsr2_1b_Rec_Des%BT_107GHz_H = amsr2_1b_Rec_Src%BT_107GHz_H

   amsr2_1b_Rec_Des%BT_107GHz_V = amsr2_1b_Rec_Src%BT_107GHz_V

   amsr2_1b_Rec_Des%BT_187GHz_H = amsr2_1b_Rec_Src%BT_187GHz_H

   amsr2_1b_Rec_Des%BT_187GHz_V = amsr2_1b_Rec_Src%BT_187GHz_V

   amsr2_1b_Rec_Des%BT_238GHz_H = amsr2_1b_Rec_Src%BT_238GHz_H

   amsr2_1b_Rec_Des%BT_238GHz_V = amsr2_1b_Rec_Src%BT_238GHz_V

   amsr2_1b_Rec_Des%BT_365GHz_H = amsr2_1b_Rec_Src%BT_365GHz_H

   amsr2_1b_Rec_Des%BT_365GHz_V = amsr2_1b_Rec_Src%BT_365GHz_V

   amsr2_1b_Rec_Des%BT_69GHz_H = amsr2_1b_Rec_Src%BT_69GHz_H

   amsr2_1b_Rec_Des%BT_69GHz_V = amsr2_1b_Rec_Src%BT_69GHz_V

   amsr2_1b_Rec_Des%BT_73GHz_H = amsr2_1b_Rec_Src%BT_73GHz_H

   amsr2_1b_Rec_Des%BT_73GHz_V = amsr2_1b_Rec_Src%BT_73GHz_V

   amsr2_1b_Rec_Des%Earth_Azimuth = amsr2_1b_Rec_Src%Earth_Azimuth

   amsr2_1b_Rec_Des%Earth_Incidence = amsr2_1b_Rec_Src%Earth_Incidence

   amsr2_1b_Rec_Des%Sun_Azimuth = amsr2_1b_Rec_Src%Sun_Azimuth

   amsr2_1b_Rec_Des%Sun_Elevation = amsr2_1b_Rec_Src%Sun_Elevation

   amsr2_1b_Rec_Des%BT_890GHzA_H = amsr2_1b_Rec_Src%BT_890GHzA_H

   amsr2_1b_Rec_Des%BT_890GHzA_V = amsr2_1b_Rec_Src%BT_890GHzA_V

   amsr2_1b_Rec_Des%BT_890GHzB_H = amsr2_1b_Rec_Src%BT_890GHzB_H

   amsr2_1b_Rec_Des%BT_890GHzB_V = amsr2_1b_Rec_Src%BT_890GHzB_V

   amsr2_1b_Rec_Des%Latitude_for_89A = amsr2_1b_Rec_Src%Latitude_for_89A

   amsr2_1b_Rec_Des%Latitude_for_89B = amsr2_1b_Rec_Src%Latitude_for_89B

   amsr2_1b_Rec_Des%Longitude_for_89A = amsr2_1b_Rec_Src%Longitude_for_89A

   amsr2_1b_Rec_Des%Longitude_for_89B = amsr2_1b_Rec_Src%Longitude_for_89B

   amsr2_1b_Rec_Des%Pixel_Data_Quality_6_to_36 = amsr2_1b_Rec_Src%Pixel_Data_Quality_6_to_36

   amsr2_1b_Rec_Des%Pixel_Data_Quality_89 = amsr2_1b_Rec_Src%Pixel_Data_Quality_89

   amsr2_1b_Rec_Des%Land_Ocean_Flag_6_to_36 = amsr2_1b_Rec_Src%Land_Ocean_Flag_6_to_36

   amsr2_1b_Rec_Des%Land_Ocean_Flag_89 = amsr2_1b_Rec_Src%Land_Ocean_Flag_89

   amsr2_1b_Rec_Des%Scan_Data_Quality = amsr2_1b_Rec_Src%Scan_Data_Quality

   amsr2_1b_Rec_Des%Scan_Time = amsr2_1b_Rec_Src%Scan_Time



   RETURN

END Subroutine amsr2_1b_copy
