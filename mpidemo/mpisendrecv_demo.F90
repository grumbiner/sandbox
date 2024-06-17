PROGRAM demo
  IMPLICIT none
  INCLUDE "mpif.h"
  INTEGER i,j, procno, itag, istatus(MPI_STATUS_SIZE), ierr, nproc, myproc, bufsize
! on my desk, max bufsize ~16031 (then **2)
  PARAMETER (bufsize = 16031 )
  REAL  :: sendbuf(bufsize,bufsize) , recvbuf(bufsize,bufsize)

  CALL MPI_INIT(ierr)
  CALL MPI_COMM_SIZE(MPI_COMM_WORLD, nproc, ierr)
  CALL MPI_COMM_RANK(MPI_COMM_WORLD, myproc, ierr)
  PRINT *,'nproc = ',nproc,' while my number is ',myproc
  itag = myproc

  IF (nproc .EQ. 1) THEN
    PRINT *,'solo execution'
    STOP
  ENDIF
  DO i = 1, bufsize
  DO j = 1, bufsize
    sendbuf(i,j) = i
  ENDDO
  ENDDO

  IF (myproc .EQ. 0) THEN
!send pieces to the other processors as needed
    DO procno = 1, nproc-1
      PRINT *,'sending to processor number ',procno
      PRINT *,'args ',bufsize, MPI_REAL, procno, itag, MPI_COMM_WORLD, ierr
      CALL MPI_SEND(sendbuf, bufsize*bufsize, MPI_REAL, procno, itag, MPI_COMM_WORLD, ierr)
      PRINT *,'sent to procno, ierr ',procno, ierr
    ENDDO
  ELSE
!receive mypatch information
    PRINT *,myproc,' trying to recieve '
    itag = MPI_ANY_TAG !tag must either match sender, or be 'any'
    CALL MPI_RECV(recvbuf, bufsize*bufsize, MPI_REAL, 0, itag, MPI_COMM_WORLD, istatus, ierr)
    PRINT *,'recieved by myproc, ierr ',istatus, myproc, ierr
    PRINT *,'element 72 = ',recvbuf(72,72)

  ENDIF

  CALL MPI_FINALIZE(ierr)

END PROGRAM
