subroutine getmaxmin2(gridmin,gridmax,grid,imeta,jmeta)

!-------- obtain max and min value of a 2-dim distributed eta grid field

  include 'mpif.h'
      include "my_comm.h"
  
  real(4) grid(imeta,jmeta)

  integer(4),allocatable::ishifth(:),ishiftv(:)

  call mpi_comm_rank(my_comm,mype,ierr)

!   get ishifth,ishiftv, which define which points are dummy points (on east global boundary only)

  allocate(ishifth(jmeta)) ; allocate(ishiftv(jmeta))
  call getshift(ishifth,ishiftv)
  call get_etalims(myis2,myie2,myjs2,myje2)

  gridminloc=huge(gridminloc)
  gridmaxloc=-huge(gridminloc)
  do j=myjs2,myje2
   do i=myis2,myie2-ishifth(j)
    gridminloc=min(grid(i,j),gridminloc)
    gridmaxloc=max(grid(i,j),gridmaxloc)
   end do
  end do
  call mpi_barrier(my_comm,ierr)
  call mpi_allreduce(gridmaxloc,gridmax,1,mpi_real,mpi_max,my_comm,ierr)
  call mpi_allreduce(gridminloc,gridmin,1,mpi_real,mpi_min,my_comm,ierr)

return
end subroutine getmaxmin2
