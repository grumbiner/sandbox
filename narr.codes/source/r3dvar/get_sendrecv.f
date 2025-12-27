subroutine get_sendrecv(o2n,ips,ipe,jps,jpe,npes, &
          inpes_out,jnpes_out,idone_pes, &
            ids,ide,jds,jde, &
          pe_of_injn_out,in_of_i_out,jn_of_j_out, &
       nsend,ndsend,nrecv,ndrecv)

!    obtain send and recv info for alltoallv calls which create new partition from old partition

  implicit none

  include 'mpif.h'
      include "my_comm.h"
  include 'old_2_new_type.h'

  type(old_2_new_cons) o2n

  integer(4),intent(in)::ips,ipe,jps,jpe     !  old grid dimensions 
  integer(4),intent(in)::npes                !  number of pes in existing communicator
  integer(4),intent(in)::inpes_out,jnpes_out ! dimensions of x,y for nwe communicator
  integer(4),intent(in)::idone_pes           !  number of pieces already written out
  integer(4),intent(in)::ids,ide,jds,jde     !  global domain definition (fixed)
  integer(4),intent(in)::pe_of_injn_out(inpes_out,jnpes_out) 
  integer(4),intent(in)::in_of_i_out(ids:ide)
  integer(4),intent(in)::jn_of_j_out(jds:jde)
  integer(4),intent(out)::nsend(0:npes-1),ndsend(0:npes)
  integer(4),intent(out)::nrecv(0:npes-1),ndrecv(0:npes)

  integer(4),allocatable::idest(:),indx(:),iwork(:)
  integer(4) i,idestpe,ierr,j,mbuf,mpe,numsendmax

  numsendmax=(jpe-jps+1)*(ipe-ips+1)
  allocate(o2n%isend(numsendmax))
  allocate(o2n%jsend(numsendmax))
  allocate(idest(numsendmax))
  nsend=0
  mbuf=0
  do j=jps,jpe
   do i=ips,ipe
    idestpe=pe_of_injn_out(in_of_i_out(i),jn_of_j_out(j))-idone_pes
    if(idestpe.ge.0.and.idestpe.le.npes-1) then
     mbuf=mbuf+1
     nsend(idestpe)=nsend(idestpe)+1
     idest(mbuf)=idestpe
     o2n%isend(mbuf)=i
     o2n%jsend(mbuf)=j
    end if
   end do
  end do

!   sort destination pe numbers from smallest to largest

  allocate(indx(max(mbuf,1)))
  if(mbuf.gt.0) then
   call indexxi4(mbuf,idest,indx)
  end if

  allocate(iwork(max(mbuf,1)))
  if(mbuf.gt.0) then

!   use sort index to reorder isend,jsend

   do i=1,mbuf
    iwork(i)=o2n%isend(indx(i))
   end do
   do i=1,mbuf
    o2n%isend(i)=iwork(i)
   end do
   do i=1,mbuf
    iwork(i)=o2n%jsend(indx(i))
   end do
   do i=1,mbuf
    o2n%jsend(i)=iwork(i)
   end do
  end if
  deallocate(indx)
  deallocate(iwork)
  deallocate(idest)

!  now get remaining info necessary for using alltoallv command

  ndsend(0)=0
  do mpe=1,npes
   ndsend(mpe)=ndsend(mpe-1)+nsend(mpe-1)
  end do
  call mpi_alltoall(nsend,1,mpi_integer, &
         nrecv,1,mpi_integer,my_comm,ierr)
  ndrecv(0)=0
  do mpe=1,npes
   ndrecv(mpe)=ndrecv(mpe-1)+nrecv(mpe-1)
  end do
  allocate(o2n%irecv(max(1,ndrecv(npes))))
  allocate(o2n%jrecv(max(1,ndrecv(npes))))
  call mpi_alltoallv(o2n%isend,nsend,ndsend,mpi_integer2, &
                    o2n%irecv,nrecv,ndrecv,mpi_integer2,my_comm,ierr)
  call mpi_alltoallv(o2n%jsend,nsend,ndsend,mpi_integer2, &
                    o2n%jrecv,nrecv,ndrecv,mpi_integer2,my_comm,ierr)

return
end subroutine get_sendrecv
