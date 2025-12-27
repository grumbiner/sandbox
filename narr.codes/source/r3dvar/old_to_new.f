subroutine old_to_new(fold,o2n,fnew,ims_old,ime_old,jms_old,jme_old, &
                      ips_new,ipe_new,jps_new,jpe_new)

  implicit none
  include 'mpif.h'
      include "my_comm.h"
  include 'old_2_new_type.h'

  integer(4) ims_old,ime_old,jms_old,jme_old
  integer(4) ips_new,ipe_new,jps_new,jpe_new
  real(4) fold(ims_old:ime_old,jms_old:jme_old)
  real(4) fnew(ips_new:ipe_new,jps_new:jpe_new)
  type(old_2_new_cons) o2n

  real(4),allocatable::bufsend(:),bufrecv(:)

  integer(4) i,ierr,npes,npoints_recv,npoints_send

!         if(o2n%mype.eq.0)   write(0,*)' at 1 in old_to_new'
  npes=o2n%npes_old
  npoints_send=o2n%ndsend(npes)
  npoints_recv=o2n%ndrecv(npes)
  allocate(bufsend(npoints_send))
  allocate(bufrecv(npoints_recv))
          !rite(0,*)' in old_to_new, mype,ips,e,jps,e_old=',o2n%mype,ims_old,ime_old,jms_old,jme_old
          !rite(0,*)' in old_to_new, mype,ips,e,jps,e_old=',o2n%mype,ims_old,ime_old,jms_old,jme_old
        !rite(0,*)' in old_to_new, mype,fold(ims_old,jms_old)=',o2n%mype,fold(ims_old,jms_old)
        !rite(0,*)' in old_to_new, mype,fold(ime_old,jms_old)=',o2n%mype,fold(ime_old,jms_old)
        !rite(0,*)' in old_to_new, mype,fold(ims_old,jme_old)=',o2n%mype,fold(ims_old,jme_old)
        !rite(0,*)' in old_to_new, mype,fold(ime_old,jme_old)=',o2n%mype,fold(ime_old,jme_old)
        !rite(0,*)' in old_to_new, mype,npes,npoints_send,npoings_recv=', &
                     !           o2n%mype,o2n%npes_old,npoints_send,npoints_recv
  if(npoints_send.gt.0) then
             !rite(0,*)' in old_to_new, mype,max,min(02n%isend=',o2n%mype,maxval(o2n%isend),minval(o2n%isend)
             !rite(0,*)' in old_to_new, mype,max,min(02n%jsend=',o2n%mype,maxval(o2n%jsend),minval(o2n%jsend)
   do i=1,npoints_send
    bufsend(i)=fold(o2n%isend(i),o2n%jsend(i))
   end do
  end if
!         if(o2n%mype.eq.0)   write(0,*)' at 2 in old_to_new'
                   !rite(0,*)' in old_to_new, mype,nsend=',o2n%mype,o2n%nsend
                   !rite(0,*)' in old_to_new, mype,ndsend=',o2n%mype,o2n%ndsend
                   !rite(0,*)' in old_to_new, mype,nrecv=',o2n%mype,o2n%nrecv
                   !rite(0,*)' in old_to_new, mype,ndrecv=',o2n%mype,o2n%ndrecv
  call mpi_alltoallv(bufsend,o2n%nsend,o2n%ndsend,mpi_real4, &
                       bufrecv,o2n%nrecv,o2n%ndrecv,mpi_real4,my_comm,ierr)
!         if(o2n%mype.eq.0)   write(0,*)' at 3 in old_to_new'
  deallocate(bufsend)
  if(npoints_recv.gt.0) then
             !rite(0,*)' in old_to_new, mype,max,min(02n%irecv=',o2n%mype,maxval(o2n%irecv),minval(o2n%irecv)
             !rite(0,*)' in old_to_new, mype,max,min(02n%jrecv=',o2n%mype,maxval(o2n%jrecv),minval(o2n%jrecv)
   do i=1,npoints_recv
    fnew(o2n%irecv(i),o2n%jrecv(i))=bufrecv(i)
   end do
  end if
  deallocate(bufrecv)
!         if(o2n%mype.eq.0)   write(0,*)' at 4 in old_to_new'

return
end subroutine old_to_new
