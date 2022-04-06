!------------------------------------------------------------------------------
!
! (C) IMSG  
!
!
! NAME:
!    amsr2_1b_deallocate
!
! CALLING SEQUENCE:
!    Call amsr2_1b_deallocate (amsr2_1b_Rec)
!
! INPUTS:
!    amsr2_1b_Rec
!
! OUTPUTS:
!    amsr2_1b_Rec
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

SUBROUTINE amsr2_1b_deallocate (amsr2_1b_Rec)

   !
   ! Declare modules used --
   !

   USE type_kinds
   USE errormsg_module
   USE amsr2_1b_Module

   IMPLICIT NONE

   TYPE (amsr2_1b_record) :: amsr2_1b_Rec

   !
   !  Variable declarations
   !

   INTEGER(LONG) :: Deallocate_Status

   !
   !  Start the processing
   !


   !
   !  Deallocate memory for BT_107GHz_H
   !

   DEALLOCATE (amsr2_1b_Rec%BT_107GHz_H, STAT = Deallocate_Status)

   IF(Deallocate_Status /= 0)THEN
      PRINT*,'ERROR: amsr2_1b_deallocate Could NOT deallocate space for ', &
      'amsr2_1b_Rec%BT_107GHz_H'
      amsr2_1b_Rec%Deallocate_Status = 1
      RETURN
   ENDIF

   !
   !  Deallocate memory for BT_107GHz_V
   !

   DEALLOCATE (amsr2_1b_Rec%BT_107GHz_V, STAT = Deallocate_Status)

   IF(Deallocate_Status /= 0)THEN
      PRINT*,'ERROR: amsr2_1b_deallocate Could NOT deallocate space for ', &
      'amsr2_1b_Rec%BT_107GHz_V'
      amsr2_1b_Rec%Deallocate_Status = 1
      RETURN
   ENDIF

   !
   !  Deallocate memory for BT_187GHz_H
   !

   DEALLOCATE (amsr2_1b_Rec%BT_187GHz_H, STAT = Deallocate_Status)

   IF(Deallocate_Status /= 0)THEN
      PRINT*,'ERROR: amsr2_1b_deallocate Could NOT deallocate space for ', &
      'amsr2_1b_Rec%BT_187GHz_H'
      amsr2_1b_Rec%Deallocate_Status = 1
      RETURN
   ENDIF

   !
   !  Deallocate memory for BT_187GHz_V
   !

   DEALLOCATE (amsr2_1b_Rec%BT_187GHz_V, STAT = Deallocate_Status)

   IF(Deallocate_Status /= 0)THEN
      PRINT*,'ERROR: amsr2_1b_deallocate Could NOT deallocate space for ', &
      'amsr2_1b_Rec%BT_187GHz_V'
      amsr2_1b_Rec%Deallocate_Status = 1
      RETURN
   ENDIF

   !
   !  Deallocate memory for BT_238GHz_H
   !

   DEALLOCATE (amsr2_1b_Rec%BT_238GHz_H, STAT = Deallocate_Status)

   IF(Deallocate_Status /= 0)THEN
      PRINT*,'ERROR: amsr2_1b_deallocate Could NOT deallocate space for ', &
      'amsr2_1b_Rec%BT_238GHz_H'
      amsr2_1b_Rec%Deallocate_Status = 1
      RETURN
   ENDIF

   !
   !  Deallocate memory for BT_238GHz_V
   !

   DEALLOCATE (amsr2_1b_Rec%BT_238GHz_V, STAT = Deallocate_Status)

   IF(Deallocate_Status /= 0)THEN
      PRINT*,'ERROR: amsr2_1b_deallocate Could NOT deallocate space for ', &
      'amsr2_1b_Rec%BT_238GHz_V'
      amsr2_1b_Rec%Deallocate_Status = 1
      RETURN
   ENDIF

   !
   !  Deallocate memory for BT_365GHz_H
   !

   DEALLOCATE (amsr2_1b_Rec%BT_365GHz_H, STAT = Deallocate_Status)

   IF(Deallocate_Status /= 0)THEN
      PRINT*,'ERROR: amsr2_1b_deallocate Could NOT deallocate space for ', &
      'amsr2_1b_Rec%BT_365GHz_H'
      amsr2_1b_Rec%Deallocate_Status = 1
      RETURN
   ENDIF

   !
   !  Deallocate memory for BT_365GHz_V
   !

   DEALLOCATE (amsr2_1b_Rec%BT_365GHz_V, STAT = Deallocate_Status)

   IF(Deallocate_Status /= 0)THEN
      PRINT*,'ERROR: amsr2_1b_deallocate Could NOT deallocate space for ', &
      'amsr2_1b_Rec%BT_365GHz_V'
      amsr2_1b_Rec%Deallocate_Status = 1
      RETURN
   ENDIF

   !
   !  Deallocate memory for BT_69GHz_H
   !

   DEALLOCATE (amsr2_1b_Rec%BT_69GHz_H, STAT = Deallocate_Status)

   IF(Deallocate_Status /= 0)THEN
      PRINT*,'ERROR: amsr2_1b_deallocate Could NOT deallocate space for ', &
      'amsr2_1b_Rec%BT_69GHz_H'
      amsr2_1b_Rec%Deallocate_Status = 1
      RETURN
   ENDIF

   !
   !  Deallocate memory for BT_69GHz_V
   !

   DEALLOCATE (amsr2_1b_Rec%BT_69GHz_V, STAT = Deallocate_Status)

   IF(Deallocate_Status /= 0)THEN
      PRINT*,'ERROR: amsr2_1b_deallocate Could NOT deallocate space for ', &
      'amsr2_1b_Rec%BT_69GHz_V'
      amsr2_1b_Rec%Deallocate_Status = 1
      RETURN
   ENDIF

   !
   !  Deallocate memory for BT_73GHz_H
   !

   DEALLOCATE (amsr2_1b_Rec%BT_73GHz_H, STAT = Deallocate_Status)

   IF(Deallocate_Status /= 0)THEN
      PRINT*,'ERROR: amsr2_1b_deallocate Could NOT deallocate space for ', &
      'amsr2_1b_Rec%BT_73GHz_H'
      amsr2_1b_Rec%Deallocate_Status = 1
      RETURN
   ENDIF

   !
   !  Deallocate memory for BT_73GHz_V
   !

   DEALLOCATE (amsr2_1b_Rec%BT_73GHz_V, STAT = Deallocate_Status)

   IF(Deallocate_Status /= 0)THEN
      PRINT*,'ERROR: amsr2_1b_deallocate Could NOT deallocate space for ', &
      'amsr2_1b_Rec%BT_73GHz_V'
      amsr2_1b_Rec%Deallocate_Status = 1
      RETURN
   ENDIF

   !
   !  Deallocate memory for Earth_Azimuth
   !

   DEALLOCATE (amsr2_1b_Rec%Earth_Azimuth, STAT = Deallocate_Status)

   IF(Deallocate_Status /= 0)THEN
      PRINT*,'ERROR: amsr2_1b_deallocate Could NOT deallocate space for ', &
      'amsr2_1b_Rec%Earth_Azimuth'
      amsr2_1b_Rec%Deallocate_Status = 1
      RETURN
   ENDIF

   !
   !  Deallocate memory for Earth_Incidence
   !

   DEALLOCATE (amsr2_1b_Rec%Earth_Incidence, STAT = Deallocate_Status)

   IF(Deallocate_Status /= 0)THEN
      PRINT*,'ERROR: amsr2_1b_deallocate Could NOT deallocate space for ', &
      'amsr2_1b_Rec%Earth_Incidence'
      amsr2_1b_Rec%Deallocate_Status = 1
      RETURN
   ENDIF

   !
   !  Deallocate memory for Sun_Azimuth
   !

   DEALLOCATE (amsr2_1b_Rec%Sun_Azimuth, STAT = Deallocate_Status)

   IF(Deallocate_Status /= 0)THEN
      PRINT*,'ERROR: amsr2_1b_deallocate Could NOT deallocate space for ', &
      'amsr2_1b_Rec%Sun_Azimuth'
      amsr2_1b_Rec%Deallocate_Status = 1
      RETURN
   ENDIF

   !
   !  Deallocate memory for Sun_Elevation
   !

   DEALLOCATE (amsr2_1b_Rec%Sun_Elevation, STAT = Deallocate_Status)

   IF(Deallocate_Status /= 0)THEN
      PRINT*,'ERROR: amsr2_1b_deallocate Could NOT deallocate space for ', &
      'amsr2_1b_Rec%Sun_Elevation'
      amsr2_1b_Rec%Deallocate_Status = 1
      RETURN
   ENDIF

   !
   !  Deallocate memory for BT_890GHzA_H
   !

   DEALLOCATE (amsr2_1b_Rec%BT_890GHzA_H, STAT = Deallocate_Status)

   IF(Deallocate_Status /= 0)THEN
      PRINT*,'ERROR: amsr2_1b_deallocate Could NOT deallocate space for ', &
      'amsr2_1b_Rec%BT_890GHzA_H'
      amsr2_1b_Rec%Deallocate_Status = 1
      RETURN
   ENDIF

   !
   !  Deallocate memory for BT_890GHzA_V
   !

   DEALLOCATE (amsr2_1b_Rec%BT_890GHzA_V, STAT = Deallocate_Status)

   IF(Deallocate_Status /= 0)THEN
      PRINT*,'ERROR: amsr2_1b_deallocate Could NOT deallocate space for ', &
      'amsr2_1b_Rec%BT_890GHzA_V'
      amsr2_1b_Rec%Deallocate_Status = 1
      RETURN
   ENDIF

   !
   !  Deallocate memory for BT_890GHzB_H
   !

   DEALLOCATE (amsr2_1b_Rec%BT_890GHzB_H, STAT = Deallocate_Status)

   IF(Deallocate_Status /= 0)THEN
      PRINT*,'ERROR: amsr2_1b_deallocate Could NOT deallocate space for ', &
      'amsr2_1b_Rec%BT_890GHzB_H'
      amsr2_1b_Rec%Deallocate_Status = 1
      RETURN
   ENDIF

   !
   !  Deallocate memory for BT_890GHzB_V
   !

   DEALLOCATE (amsr2_1b_Rec%BT_890GHzB_V, STAT = Deallocate_Status)

   IF(Deallocate_Status /= 0)THEN
      PRINT*,'ERROR: amsr2_1b_deallocate Could NOT deallocate space for ', &
      'amsr2_1b_Rec%BT_890GHzB_V'
      amsr2_1b_Rec%Deallocate_Status = 1
      RETURN
   ENDIF

   !
   !  Deallocate memory for Latitude_for_89A
   !

   DEALLOCATE (amsr2_1b_Rec%Latitude_for_89A, STAT = Deallocate_Status)

   IF(Deallocate_Status /= 0)THEN
      PRINT*,'ERROR: amsr2_1b_deallocate Could NOT deallocate space for ', &
      'amsr2_1b_Rec%Latitude_for_89A'
      amsr2_1b_Rec%Deallocate_Status = 1
      RETURN
   ENDIF

   !
   !  Deallocate memory for Latitude_for_89B
   !

   DEALLOCATE (amsr2_1b_Rec%Latitude_for_89B, STAT = Deallocate_Status)

   IF(Deallocate_Status /= 0)THEN
      PRINT*,'ERROR: amsr2_1b_deallocate Could NOT deallocate space for ', &
      'amsr2_1b_Rec%Latitude_for_89B'
      amsr2_1b_Rec%Deallocate_Status = 1
      RETURN
   ENDIF

   !
   !  Deallocate memory for Longitude_for_89A
   !

   DEALLOCATE (amsr2_1b_Rec%Longitude_for_89A, STAT = Deallocate_Status)

   IF(Deallocate_Status /= 0)THEN
      PRINT*,'ERROR: amsr2_1b_deallocate Could NOT deallocate space for ', &
      'amsr2_1b_Rec%Longitude_for_89A'
      amsr2_1b_Rec%Deallocate_Status = 1
      RETURN
   ENDIF

   !
   !  Deallocate memory for Longitude_for_89B
   !

   DEALLOCATE (amsr2_1b_Rec%Longitude_for_89B, STAT = Deallocate_Status)

   IF(Deallocate_Status /= 0)THEN
      PRINT*,'ERROR: amsr2_1b_deallocate Could NOT deallocate space for ', &
      'amsr2_1b_Rec%Longitude_for_89B'
      amsr2_1b_Rec%Deallocate_Status = 1
      RETURN
   ENDIF

   !
   !  Deallocate memory for Pixel_Data_Quality_6_to_36
   !

   DEALLOCATE (amsr2_1b_Rec%Pixel_Data_Quality_6_to_36, STAT = Deallocate_Status)

   IF(Deallocate_Status /= 0)THEN
      PRINT*,'ERROR: amsr2_1b_deallocate Could NOT deallocate space for ', &
      'amsr2_1b_Rec%Pixel_Data_Quality_6_to_36'
      amsr2_1b_Rec%Deallocate_Status = 1
      RETURN
   ENDIF

   !
   !  Deallocate memory for Pixel_Data_Quality_89
   !

   DEALLOCATE (amsr2_1b_Rec%Pixel_Data_Quality_89, STAT = Deallocate_Status)

   IF(Deallocate_Status /= 0)THEN
      PRINT*,'ERROR: amsr2_1b_deallocate Could NOT deallocate space for ', &
      'amsr2_1b_Rec%Pixel_Data_Quality_89'
      amsr2_1b_Rec%Deallocate_Status = 1
      RETURN
   ENDIF

   !
   !  Deallocate memory for Land_Ocean_Flag_6_to_36
   !

   DEALLOCATE (amsr2_1b_Rec%Land_Ocean_Flag_6_to_36, STAT = Deallocate_Status)

   IF(Deallocate_Status /= 0)THEN
      PRINT*,'ERROR: amsr2_1b_deallocate Could NOT deallocate space for ', &
      'amsr2_1b_Rec%Land_Ocean_Flag_6_to_36'
      amsr2_1b_Rec%Deallocate_Status = 1
      RETURN
   ENDIF

   !
   !  Deallocate memory for Land_Ocean_Flag_89
   !

   DEALLOCATE (amsr2_1b_Rec%Land_Ocean_Flag_89, STAT = Deallocate_Status)

   IF(Deallocate_Status /= 0)THEN
      PRINT*,'ERROR: amsr2_1b_deallocate Could NOT deallocate space for ', &
      'amsr2_1b_Rec%Land_Ocean_Flag_89'
      amsr2_1b_Rec%Deallocate_Status = 1
      RETURN
   ENDIF

   !
   !  Deallocate memory for Scan_Data_Quality
   !

   DEALLOCATE (amsr2_1b_Rec%Scan_Data_Quality, STAT = Deallocate_Status)

   IF(Deallocate_Status /= 0)THEN
      PRINT*,'ERROR: amsr2_1b_deallocate Could NOT deallocate space for ', &
      'amsr2_1b_Rec%Scan_Data_Quality'
      amsr2_1b_Rec%Deallocate_Status = 1
      RETURN
   ENDIF

   !
   !  Deallocate memory for Scan_Time
   !

   DEALLOCATE (amsr2_1b_Rec%Scan_Time, STAT = Deallocate_Status)

   IF(Deallocate_Status /= 0)THEN
      PRINT*,'ERROR: amsr2_1b_deallocate Could NOT deallocate space for ', &
      'amsr2_1b_Rec%Scan_Time'
      amsr2_1b_Rec%Deallocate_Status = 1
      RETURN
   ENDIF


   RETURN

END Subroutine amsr2_1b_deallocate
