!------------------------------------------------------------------------------
!
! (C) QSS Group, Inc 2000
!
!
! NAME:
!       error_messaging
!
! PURPOSE:
!       Routine to write out messages if errors occur during processing.
!
! CATEGORY:
!       General
!
! CALLING SEQUENCE:
!       CALL error_messaging( boolean_test, output_string )
!
! INPUTS:
!       Routine_Name: Name of routine where message originated
!       Message: Text of message
!       Level: Message level (0=notificiation, 1=warning, 2=fatal,program stops)
!
! OUTPUTS:
!       If the logical test is true, the contents of output_string are output
!         to the screen.
!
! CALLS:
!       None
!
! MODULES:
!       type_kinds:      Module containing definitions for kinds of variable
!                        types
!
! SIDE EFFECTS:
!       Program execution is halted if the logical test is true.
!
! RESTRICTIONS:
!       None
!
! EXAMPLE:
!       PROGRAM test_error_messaging
!         USE type_kinds
!         IMPLICIT NONE
!         REAL( Single ) :: a
!         EXTERNAL error_messaging
!         a = 11.0
!         CALL error_messaging( ( a > 10.0 ), 'A is greater than 10.0' )
!       END PROGRAM test_error_messaging
!
!       Executing this code will produce the screen output:
!
!         A is greater than 10.0
!
!
! MODIFICATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC, 10-Feb-1997
!
!  $Date: 1998/07/21 18:47:17 $
!  $Id: error_messaging.f90,v 1.1 1998/07/21 18:47:17 walterw Exp walterw $
!  $Log: error_messaging.f90,v $
!  Revision 1.1  1998/07/21 18:47:17  walterw
!  Initial revision
!
!  Revision 1.2  1998/04/26 21:25:00  paulv
!  Documentation update.
!
!  Revision 1.1  1998/03/27 22:45:56  paulv
!  Initial revision
!
!
!
!------------------------------------------------------------------------------


SUBROUTINE error_messaging( Routine_Name, Message, Level )


!------------------------------------------------------------------------------
!                          -- Declare modules used --
!------------------------------------------------------------------------------
 
   USE type_kinds

!------------------------------------------------------------------------------
!                           -- Type declarations --
!------------------------------------------------------------------------------
 
   IMPLICIT NONE

! ---------
! Arguments
! ---------

   INTEGER(LONG), INTENT(IN) :: Level
   CHARACTER( * ), INTENT(IN) :: Routine_Name, Message
 

!##############################################################################
!                     ## BEGIN EXECUTABLE CODE ##
!##############################################################################

 
   SELECT CASE (Level)

      CASE (0)
         WRITE (*, '(a, a, a, a)') &
                "NOTICE: ", Message, " in program ", Routine_Name

      CASE (1)
         WRITE (*, '(a, a, a, a)') &
                "WARNING: ", Message, " in program ", Routine_Name

      CASE (2)
         WRITE (*, '(a, a, a, a)') &
                "FATAL ERROR: ", Message, " in program ", Routine_Name

         CALL EXIT(1) 
 
   END SELECT

   RETURN

END SUBROUTINE error_messaging

