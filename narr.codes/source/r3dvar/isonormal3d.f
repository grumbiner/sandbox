subroutine isonormal3d(filter, &
             ids, ide, jds, jde, kds, kde, &                          ! domain indices
             ips, ipe, jps, jpe, kps, kpe, &                          ! patch indices
             ims, ime, jms, jme, kms, kme, &                          ! memory indices
             inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j ) ! processor info

!   normalize amplitude of 3-d isotropic recursive filter

  INCLUDE 'filtertype.h'
  INCLUDE 'mpif.h'
      include "my_comm.h"

  type(filter_cons) filter(14)
  INTEGER(4), INTENT(IN) :: pe_of_injn(inpes,jnpes),in_of_i(ids:ide),jn_of_j(jds:jde)

  real(4),allocatable::rnorm(:,:,:),xline(:,:),xline0(:,:,:),xnorm(:,:),ynorm(:,:)
  real(4),allocatable::pline(:),pline0(:,:)

  allocate(rnorm(ims:ime,jms:jme,kms:kme))
  rnorm=0.
  do k=kms,kme
   do j=max(ids,jds),min(ide,jde)
    if(j.ge.jps.and.j.le.jpe.and.j.ge.ips.and.j.le.ipe) rnorm(j,j,k)=1.
   end do
  end do
  ixyz=1
  call raf1d(rnorm,filter,ixyz, &
             ids, ide, jds, jde, kds, kde, &                          ! domain indices
             ips, ipe, jps, jpe, kps, kpe, &                          ! patch indices
             ims, ime, jms, jme, kms, kme, &                          ! memory indices
             inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j ) ! processor info

              if(mype.eq.0) write(0,*)' at 3 in normalize_raf'
  call ad_raf1d(rnorm,filter,ixyz, &
             ids, ide, jds, jde, kds, kde, &                          ! domain indices
             ips, ipe, jps, jpe, kps, kpe, &                          ! patch indices
             ims, ime, jms, jme, kms, kme, &                          ! memory indices
             inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j ) ! processor info
     do k=kms,kme
      do j=max(ids,jds),min(ide,jde)
       if(j.ge.jps.and.j.le.jpe.and.j.ge.ips.and.j.le.ipe) then
        if(j.eq.(jpe+jps)/2.and.mype.eq.0) then
           diagnostic_len=sqrt(rnorm(j,j,k)/(2.*rnorm(j,j,k)-rnorm(j+1,j,k)-rnorm(j-1,j,k)))
           print *,' diagnostic_xlen for k=',k,' = ',diagnostic_len
        end if
       end if
      end do
     end do

  nxyh=1+min(jde-jds,ide-ids)/2
  allocate(xline(nxyh,kms:kme))
  xline=-1.
  do k=kms,kme
   i=0
   do j=max(ids,jds),min(ide,jde)
    i=i+1
    if(i.le.nxyh.and.j.ge.jps.and.j.le.jpe.and.j.ge.ips.and.j.le.ipe) xline(i,k)=rnorm(j,j,k)
   end do
  end do
  allocate(xline0(nxyh,kms:kme,npes))
  call mpi_gather(xline,nxyh*(kme-kms+1),mpi_real4,xline0,nxyh*(kme-kms+1),mpi_real4,0,my_comm,ierr)
  if(mype.eq.0) then
   do j=1,npes
    do k=kms,kme
     do i=1,nxyh
      if(xline0(i,k,j).ne.-1.) xline(i,k)=xline0(i,k,j)
     end do
    end do
   end do
!      do k=kms,kme
!       do i=1,nxyh
!         print *,' i,xline(i,k)=',i,xline(i,k)
!       end do
!      end do
  end if
  call mpi_bcast(xline,nxyh*(kme-kms+1),mpi_real4,0,my_comm,ierr)

  allocate(xnorm(ids:ide,kms:kme))
  xnorm=-1.
  do k=kms,kme
   do i=ids,ids+nxyh-1
    xnorm(i,k)=xline(i-ids+1,k)
   end do
   do i=ide-nxyh+1,ide
    xnorm(i,k)=xline(ide-i+1,k)
   end do
   do i=ids+1,ide
    if(xnorm(i,k).eq.-1.) xnorm(i,k)=xnorm(i-1,k)
   end do
  end do
!    if(mype.eq.0) then
!     do i=ids,ide
!            print *,' i,xnorm(i,1)=',i,xnorm(i,1)
!     end do
!    end if
  allocate(ynorm(jds:jde,kms:kme))
  ynorm=-1.
  do k=kms,kme
   do j=jds,jds+nxyh-1
    ynorm(j,k)=xline(j-jds+1,k)
   end do
   do j=jde-nxyh+1,jde
    ynorm(j,k)=xline(jde-j+1,k)
   end do
   do j=jds+1,jde
    if(ynorm(j,k).eq.-1.) ynorm(j,k)=ynorm(j-1,k)
   end do
  end do
!    if(mype.eq.0) then
!     do j=jds,jde
!            print *,' j,ynorm(j,1)=',j,ynorm(j,1)
!     end do
!    end if
  xnorm=1./sqrt(xnorm)
  ynorm=1./sqrt(ynorm)

!   now get vertical normalization

  rnorm=0.
  k=kms-1
  do j=jds,jde
   do i=ids,ide
    k=k+1
    if(k.le.kme.and.i.ge.ips.and.i.le.ipe.and.j.ge.jps.and.j.le.jpe) rnorm(i,j,k)=1.
   end do
  end do
  ixyz=3
  call raf1d(rnorm,filter,ixyz, &
             ids, ide, jds, jde, kds, kde, &                          ! domain indices
             ips, ipe, jps, jpe, kps, kpe, &                          ! patch indices
             ims, ime, jms, jme, kms, kme, &                          ! memory indices
             inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j ) ! processor info
  call ad_raf1d(rnorm,filter,ixyz, &
             ids, ide, jds, jde, kds, kde, &                          ! domain indices
             ips, ipe, jps, jpe, kps, kpe, &                          ! patch indices
             ims, ime, jms, jme, kms, kme, &                          ! memory indices
             inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j ) ! processor info

  allocate(pline(kms:kme))
  pline=-1.
  k=kms-1
  do j=jds,jde
   do i=ids,ide
    k=k+1
    if(k.le.kme.and.i.ge.ips.and.i.le.ipe.and.j.ge.jps.and.j.le.jpe) pline(k)=rnorm(i,j,k)
   end do
  end do
  allocate(pline0(kms:kme,npes))
  call mpi_gather(pline,kme-kms+1,mpi_real4,pline0,kme-kms+1,mpi_real4,0,my_comm,ierr)
  if(mype.eq.0) then
   do j=1,npes
    do k=kms,kme
     if(pline0(k,j).ne.-1.) pline(k)=pline0(k,j)
    end do
   end do
!      do k=kms,kme
!         print *,' k,pline(k)=',k,pline(k)
!      end do
  end if
  call mpi_bcast(pline,kme-kms+1,mpi_real4,0,my_comm,ierr)
  pline=1./sqrt(pline)

  do k=kms,kme
   do j=jps,jpe
    do i=ips,ipe
     filter(1)%amp(i,j,k)=xnorm(i,k)*ynorm(j,k)*pline(k)
    end do
   end do
  end do

!  check to see how we did

! i=ide+1
! j=jde+1
! k=kde+1
! do jj=1,20
!  i=i-1
!  j=j-1
!  k=k-1
!  rnorm=0.
!  if(j.ge.jps.and.j.le.jpe.and.i.ge.ips.and.i.le.ipe) rnorm(i,j,k)=1.
!  call rf3d(rnorm,filter_3, &
!            ids, ide, jds, jde, kds, kde, &                          ! domain indices
!            ips, ipe, jps, jpe, kps, kpe, &                          ! patch indices
!            ims, ime, jms, jme, kms, kme, &                          ! memory indices
!            inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j ) ! processor info
!  call ad_rf3d(rnorm,filter_3, &
!            ids, ide, jds, jde, kds, kde, &                          ! domain indices
!            ips, ipe, jps, jpe, kps, kpe, &                          ! patch indices
!            ims, ime, jms, jme, kms, kme, &                          ! memory indices
!            inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j ) ! processor info
!  if(j.ge.jps.and.j.le.jpe.and.i.ge.ips.and.i.le.ipe) print *,' in isonormal3d, i,j,k,rnorm(i,j,k)=', &
!                                i,j,k,rnorm(i,j,k)
! end do

  deallocate(xnorm)
  deallocate(ynorm)
  deallocate(rnorm)
  deallocate(xline)
  deallocate(xline0)
  deallocate(pline)
  deallocate(pline0)

return
end subroutine isonormal3d
