subroutine var_2d_locs(rlon,rlat,mdata,ivar,varlats,nvarlats,npes)

  include 'mpif.h'
      include "my_comm.h"

  real(4) rlon(max(1,mdata)),rlat(max(1,mdata)),varlats(nvarlats)
  integer(4) ivar(nvarlats)

  integer(4),allocatable::nearest0(:)
  real(4),allocatable::delmin0(:)

  call mpi_comm_rank(my_comm,mype,ierr)
  allocate(nearest0(0:npes-1))
  allocate(delmin0(0:npes-1))

  do j=1,nvarlats
   nearest=0
   delmin=huge(dellat)
   if(mdata.gt.0) then
    do i=1,mdata
     del=(rlat(i)-varlats(j))**2
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
   call mpi_scatter(nearest0,1,mpi_integer,ivar(j),1,mpi_integer,0,my_comm,ierr)

!  if(ivar(j).ne.0) then
!   print *,' in var_2d_locs, mype,ivar,varlat,oblat=',mype,ivar(j),varlats(j),rlat(ivar(j))
!  end if

  end do

return
end subroutine var_2d_locs
