subroutine isonormal2d(filter, &
             ids, ide, jds, jde, kds, kde, &         ! domain indices
             ips, ipe, jps, jpe, kps, kpe, &         ! patch indices
             ims, ime, jms, jme, kms, kme, &         ! memory indices
      inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j ) ! processor info

!   normalize amplitude of 2-d isotropic recursive filter

  include 'filtertype.h'
  include 'mpif.h'
      include "my_comm.h"

  type(filter_cons) filter(14)
  INTEGER(4), INTENT(IN) :: pe_of_injn(inpes,jnpes),in_of_i(ids:ide),jn_of_j(jds:jde)

  real(4),allocatable::rnorm(:,:),xline(:),xline0(:,:),xnorm(:),ynorm(:)

  allocate(rnorm(ims:ime,jms:jme))
  rnorm=0.
  do j=max(ids,jds),min(ide,jde)
   if(j.ge.jps.and.j.le.jpe.and.j.ge.ips.and.j.le.ipe) rnorm(j,j)=1.
  end do 
  ixyz=1
  call raf1d(rnorm,filter,ixyz, &
             ids, ide, jds, jde, kds, kde, &         ! domain indices
             ips, ipe, jps, jpe, kps, kpe, &         ! patch indices
             ims, ime, jms, jme, kms, kme, &         ! memory indices
      inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j ) ! processor info
  call ad_raf1d(rnorm,filter,ixyz, &
             ids, ide, jds, jde, kds, kde, &         ! domain indices
             ips, ipe, jps, jpe, kps, kpe, &         ! patch indices
             ims, ime, jms, jme, kms, kme, &         ! memory indices
      inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j ) ! processor info

  nxyh=1+min(jde-jds,ide-ids)/2
  allocate(xline(nxyh))
  i=0
  xline=-1.
  do j=max(ids,jds),min(ide,jde)
   i=i+1
   if(i.le.nxyh.and.j.ge.jps.and.j.le.jpe.and.j.ge.ips.and.j.le.ipe) xline(i)=rnorm(j,j)
  end do
  allocate(xline0(nxyh,npes))
  call mpi_gather(xline,nxyh,mpi_real4,xline0,nxyh,mpi_real4,0,my_comm,ierr)
  if(mype.eq.0) then
   do j=1,npes
    do i=1,nxyh
     if(xline0(i,j).ne.-1.) xline(i)=xline0(i,j)
    end do
   end do
!       do i=1,nxyh
!         print *,' i,xline(i)=',i,xline(i)
!       end do
  end if
  call mpi_bcast(xline,nxyh,mpi_real4,0,my_comm,ierr)

  allocate(xnorm(ids:ide))
  xnorm=-1.
  do i=ids,ids+nxyh-1
   xnorm(i)=xline(i-ids+1)
  end do
  do i=ide-nxyh+1,ide
   xnorm(i)=xline(ide-i+1)
  end do
  do i=ids+1,ide
   if(xnorm(i).eq.-1.) xnorm(i)=xnorm(i-1)
  end do
!    if(mype.eq.0) then
!     do i=ids,ide
!            print *,' i,xnorm(i)=',i,xnorm(i)
!     end do
!    end if
  allocate(ynorm(jds:jde))
  ynorm=-1.
  do j=jds,jds+nxyh-1
   ynorm(j)=xline(j-jds+1)
  end do
  do j=jde-nxyh+1,jde
   ynorm(j)=xline(jde-j+1)
  end do
  do j=jds+1,jde
   if(ynorm(j).eq.-1.) ynorm(j)=ynorm(j-1)
  end do
!    if(mype.eq.0) then
!     do j=jds,jde
!            print *,' j,ynorm(y)=',j,ynorm(j)
!     end do
!    end if
  xnorm=1./sqrt(xnorm)
  ynorm=1./sqrt(ynorm)
  do j=jps,jpe
   do i=ips,ipe
    filter(1)%amp(i,j,1)=xnorm(i)*ynorm(j)
   end do
  end do

!  check to see how we did
   
! i=ide+1
! j=jde+1
! do jj=1,20
!  i=i-1
!  j=j-1
!  rnorm=0.
!  if(j.ge.jps.and.j.le.jpe.and.i.ge.ips.and.i.le.ipe) rnorm(i,j)=1.
!  call rf2d(rnorm,filter_2, &
!            ids, ide, jds, jde, &         ! domain indices
!            ips, ipe, jps, jpe, &         ! patch indices
!            ims, ime, jms, jme, &         ! memory indices
!     inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j ) ! processor info
!  call ad_rf2d(rnorm,filter_2, &
!            ids, ide, jds, jde, &         ! domain indices
!            ips, ipe, jps, jpe, &         ! patch indices
!            ims, ime, jms, jme, &         ! memory indices
!     inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j ) ! processor info
!  if(j.ge.jps.and.j.le.jpe.and.i.ge.ips.and.i.le.ipe) print *,' in isonormal2d, i,j,rnorm(i,j)=', &
!                                i,j,rnorm(i,j)
! end do

  deallocate(xnorm)
  deallocate(ynorm)
  deallocate(rnorm)
  deallocate(xline)
  deallocate(xline0)

return
end subroutine isonormal2d
