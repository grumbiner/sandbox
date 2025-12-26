 subroutine stop1
 call mpi_finalize(ierror)
 stop
 return
 end

 subroutine stop2(ierror_code)
 include 'mpif.h'
      include "my_comm.h"
 write(6,*)'****STOP2****  ABORTING EXECUTION w/code=',ierror_code
 call mpi_abort(my_comm,ierror_code,ierror)
 stop
 return
 end
