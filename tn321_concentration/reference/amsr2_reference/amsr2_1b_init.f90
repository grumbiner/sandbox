!------------------------------------------------------------------------------
!
! NAME:
!    amsr2_1b_init
!
! CALLING SEQUENCE:
!    Call amsr2_1b_init (amsr2_1b_Rec)
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
!     Program is generated by the code generator,
!         which is written by Qingzhao Guo, AIT
!
!------------------------------------------------------------------------------

SUBROUTINE amsr2_1b_init (amsr2_1b_Rec)

   !
   ! Declare modules used
   !

   USE type_kinds
   USE errormsg_module
   USE common_parameters
   USE amsr2_1b_Module

   IMPLICIT NONE

   TYPE (amsr2_1b_record) :: amsr2_1b_Rec


   amsr2_1b_Rec%BT_107GHz_H = BAD_INT

   amsr2_1b_Rec%BT_107GHz_V = BAD_INT

   amsr2_1b_Rec%BT_187GHz_H = BAD_INT

   amsr2_1b_Rec%BT_187GHz_V = BAD_INT

   amsr2_1b_Rec%BT_238GHz_H = BAD_INT

   amsr2_1b_Rec%BT_238GHz_V = BAD_INT

   amsr2_1b_Rec%BT_365GHz_H = BAD_INT

   amsr2_1b_Rec%BT_365GHz_V = BAD_INT

   amsr2_1b_Rec%BT_69GHz_H = BAD_INT

   amsr2_1b_Rec%BT_69GHz_V = BAD_INT

   amsr2_1b_Rec%BT_73GHz_H = BAD_INT

   amsr2_1b_Rec%BT_73GHz_V = BAD_INT

   amsr2_1b_Rec%Earth_Azimuth = BAD_INT

   amsr2_1b_Rec%Earth_Incidence = BAD_INT

   amsr2_1b_Rec%Sun_Azimuth = BAD_INT

   amsr2_1b_Rec%Sun_Elevation = BAD_INT

   amsr2_1b_Rec%BT_890GHzA_H = BAD_INT

   amsr2_1b_Rec%BT_890GHzA_V = BAD_INT

   amsr2_1b_Rec%BT_890GHzB_H = BAD_INT

   amsr2_1b_Rec%BT_890GHzB_V = BAD_INT

   amsr2_1b_Rec%Latitude_for_89A = BAD_REAL

   amsr2_1b_Rec%Latitude_for_89B = BAD_REAL

   amsr2_1b_Rec%Longitude_for_89A = BAD_REAL

   amsr2_1b_Rec%Longitude_for_89B = BAD_REAL

   amsr2_1b_Rec%Pixel_Data_Quality_6_to_36 = BAD_INT

   amsr2_1b_Rec%Pixel_Data_Quality_89 = BAD_INT

   amsr2_1b_Rec%Land_Ocean_Flag_6_to_36 = BAD_INT

   amsr2_1b_Rec%Land_Ocean_Flag_89 = BAD_INT

   amsr2_1b_Rec%Scan_Data_Quality = BAD_INT

   amsr2_1b_Rec%Scan_Time = BAD_REAL


   RETURN

END Subroutine amsr2_1b_init
