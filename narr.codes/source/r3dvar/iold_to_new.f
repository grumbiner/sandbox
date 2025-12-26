subroutine iold_to_new(ifold,o2n,ifnew,ims_old,ime_old,jms_old,jme_old, &
                      ips_new,ipe_new,jps_new,jpe_new)

  implicit none
  include 'mpif.h'
      include "my_comm.h"
  include 'old_2_new_type.h'

  integer(4) ims_old,ime_old,jms_old,jme_old
  integer(4) ips_new,ipe_new,jps_new,jpe_new
  integer(4) ifold(ims_old:ime_old,jms_old:jme_old)
  integer(4) ifnew(ips_new:ipe_new,jps_new:jpe_new)
  type(old_2_new_cons) o2n

  integer(4),allocatable::ibufsend(:),ibufrecv(:)

  integer(4) i,ierr,npes,npoints_recv,npoints_send

  npes=o2n%npes_old
  npoints_send=o2n%ndsend(npes)
  npoints_recv=o2n%ndrecv(npes)
  allocate(ibufsend(npoints_send))
  allocate(ibufrecv(npoints_recv))
  if(npoints_send.gt.0) then
   do i=1,npoints_send
    ibufsend(i)=ifold(o2n%isend(i),o2n%jsend(i))
   end do
  end if
  call mpi_alltoallv(ibufsend,o2n%nsend,o2n%ndsend,mpi_integer4, &
                       ibufrecv,o2n%nrecv,o2n%ndrecv,mpi_integer4,my_comm,ierr)
  deallocate(ibufsend)
  if(npoints_recv.gt.0) then
   do i=1,npoints_recv
    ifnew(o2n%irecv(i),o2n%jrecv(i))=ibufrecv(i)
   end do
  end if
  deallocate(ibufrecv)

return
end subroutine iold_to_new
