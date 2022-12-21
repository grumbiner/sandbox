!------------------------------------------------------------------------------
!
! QSS Group
!
!
! NAME:
!       Get_Lun
!
! PURPOSE:
!       This function will find an unused logical unit number.  This
!       is used to make sure that there are no duplicates in the 
!       logical unit numbers.
!
! CATEGORY:
!       Function
!
! CALLING SEQUENCE:
!       LUN = Get_Lun()
!
! INPUTS:
!       None
!
! OUTPUTS:
!       Returns an unused Logical Unit Number
!
! CALLS:
!       None
!
! MODULES:
!       None
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
!       Written by:   Walter Wolf  (01/14/00)
!                     QSS Group
!                     Lanham, MD
!                     Walter.Wolf@noaa.gov
!
!------------------------------------------------------------------------------
!


FUNCTION Get_Lun() RESULT(Get_Lun_Result)

  USE type_kinds
  IMPLICIT NONE

!
!  Variables to be used
!

   LOGICAL       :: File_Open
   INTEGER(LONG) :: Get_Lun_Result
   INTEGER(LONG) :: Logical_Unit_Number

!
! -- Initialize logical unit number and file_open
!

   Logical_Unit_Number = 9
   File_Open = .TRUE.

!
! -- Start open loop for lun search
!

   DO WHILE ( File_Open )

!
!   ..Increment logical unit number
!

      Logical_Unit_Number = Logical_Unit_Number + 1

!
!   ..Check if file is open
!

      INQUIRE (Logical_Unit_Number, OPENED=File_Open)

   END DO

! -- Found free lun

   Get_Lun_Result = Logical_Unit_Number

   RETURN
END
