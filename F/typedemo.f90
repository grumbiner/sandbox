PROGRAM demo

    IMPLICIT NONE
    INCLUDE "mpif.h"

    INTEGER :: message_size
    INTEGER i_status(MPI_STATUS_SIZE) , reps
    INTEGER  num_of_tasks, my_task_id 

    TYPE meteo_primitive
      REAL uair, vair, t2m, q2m, slp
    END TYPE meteo_primitive

    TYPE(meteo_primitive), allocatable :: in_msg(:) , out_msg(:)
    TYPE(meteo_primitive) :: prim

    INTEGER  i, n , i_type, typesize
    INTEGER  ierror , master, worker
    DOUBLE PRECISION tstart, tend

!/********************************/
!/*  Initialization		*/
!/********************************/
    CALL MPI_INIT( ierror )
    CALL MPI_Comm_size (MPI_COMM_WORLD, num_of_tasks, ierror)
    CALL MPI_Comm_rank (MPI_COMM_WORLD, my_task_id, ierror)

!    PRINT *,'tasks, procno ',num_of_tasks, my_task_id
    master = 0
    worker = 1
    i_type = 1
    reps = 1024
    prim%uair = 7.07
    prim%vair = 7.07
    prim%t2m  = 260.0
    prim%q2m  = 0.0
    prim%slp  = 1013.25e3
    typesize = sizeof(prim) ! not standard, but common
    PRINT *,'type size = ',typesize

    DO i = 0, 64
!    DO i = 10, 20
      message_size = 2048 + 2**11 * i
!      message_size = 2**i
      ALLOCATE(in_msg(message_size), out_msg(message_size))
      in_msg = prim

      tstart = MPI_Wtime(ierror)

    if (my_task_id .eq. master) then
    do n = 1, reps
      !/* round-trip timing test */
      !/* send message to worker - message type set to 1.  If  */
      !/* return code is less than zero quit */
      CALL MPI_Send(out_msg, message_size, typesize, worker, i_type, MPI_COMM_WORLD, ierror)
      CALL MPI_Recv( in_msg, message_size, typesize, worker, i_type, MPI_COMM_WORLD, i_status, ierror)
    end do
   else if (my_task_id .eq. worker)  then
     do n = 1, reps
         CALL MPI_Recv(in_msg , message_size,  typesize, master, i_type, MPI_COMM_WORLD, i_status, ierror)
         CALL MPI_Send(out_msg, message_size,  typesize, master, i_type, MPI_COMM_WORLD, ierror)
     end do
   end if

    tend = MPI_Wtime(ierror)
    WRITE(*,*) message_size, my_task_id, tend-tstart, &
        FLOAT(reps)*FLOAT(message_size)*FLOAT(typesize)/(tend-tstart) / 1024.0 / 1024.0

    DEALLOCATE(in_msg, out_msg)
  END DO 
  tend = MPI_WTICK(ierror)
  WRITE (*,*) "time tick = ", tend

  CALL MPI_FINALIZE( ierror )

  stop
end
