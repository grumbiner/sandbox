subroutine var_3d_locs(rlon,rlat,rpres,mdata,ivar,varlats,varpres,nvarlats,nvarpres,npes)

  include 'mpif.h'
      include "my_comm.h"

  real(4) rlon(max(1,mdata)),rlat(max(1,mdata)),rpres(max(1,mdata)),varlats(nvarlats),varpres(nvarpres)
  integer(4) ivar(nvarlats,nvarpres)

  real(4),allocatable::spres(:)
  integer(4),allocatable::nearest0(:)
  real(4),allocatable::delmin0(:)

  call mpi_comm_rank(my_comm,mype,ierr)
  allocate(nearest0(0:npes-1))
  allocate(delmin0(0:npes-1))

  allocate(spres(max(1,mdata)))
  if(mdata.gt.0) then
   do i=1,mdata
    spres(i)=exp(rpres(i))
   end do
  end if
  do k=1,nvarpres
   do j=1,nvarlats
    nearest=0
    delmin=huge(dellat)
    if(mdata.gt.0) then
     do i=1,mdata
      del=(rlat(i)-varlats(j))**2+(spres(i)-varpres(k))**2
      if(del.le.delmin) then
       nearest=i
       delmin=del
      end if
     end do
    end if
    call mpi_gather(nearest,1,mpi_integer,nearest0,1,mpi_integer,0,my_comm,ierr)
    call mpi_gather(delmin,1,mpi_real,delmin0,1,mpi_real,0,my_comm,ierr)
    if(mype.eq.0) then
     minpe=-1
     delminmin=huge(delminmin)
     do i=0,npes-1
      if(delmin0(i).le.delminmin) then
       minpe=i
       delminmin=delmin0(i)
      end if
     end do
     do i=0,npes-1
      if(i.ne.minpe) then
       nearest0(i)=0
      end if
     end do
    end if
    call mpi_scatter(nearest0,1,mpi_integer,ivar(j,k),1,mpi_integer,0,my_comm,ierr)

!   if(ivar(j,k).ne.0) then
!    print *,' in var_3d_locs, mype,ivar,varlat,oblat,varpres,obpres=',mype,ivar(j,k),varlats(j), &
!                         rlat(ivar(j,k)),varpres(k),spres(ivar(j,k))
!   end if

   end do
  end do

return
end subroutine var_3d_locs
