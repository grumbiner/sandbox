program esmf_application

  ! modules
  use ESMF
  
  implicit none
  
  ! local variables
  integer:: rc
  
  call ESMF_Initialize(rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  print *, "Hello ESMF World"

  call ESMF_Finalize()
  
end program
