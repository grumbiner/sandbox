subroutine get3berrq(qg,iplot,jplot,kplot,aspect,amp,isofilter_3, &
                    nhaloc,npass,no_interp,binom,nsmooth,nsmooth_shapiro,ifilt_ord, &
             ids, ide, jds, jde, kds, kde, &                          ! domain indices
             ips, ipe, jps, jpe, kps, kpe, &                          ! patch indices
             ims, ime, jms, jme, kms, kme, &                          ! memory indices
             inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j ) ! processor info
!--------
!-------- produce 3-d filter parameters from input error, correlation
!--------  lengths, which are currently only a function of vertical.
! <--   isofilter_3: output filter constants for this variable
!--
!--
         include 'mpif.h'
      include "my_comm.h"
         include 'filtertype.h'

  real(4) qg(ims:ime,jms:jme,kms:kme)
  integer(4) iplot(20,20),jplot(20,20),kplot(20,20)
  logical binom
  INTEGER(4), INTENT(IN) :: pe_of_injn(inpes,jnpes),in_of_i(ids:ide),jn_of_j(jds:jde)
         type(filter_cons) isofilter_3(14)
!--------
         real(4) aspect(7,ips:ipe,jps:jpe,kps:kpe),amp(ips:ipe,jps:jpe,kps:kpe)
         real(4),allocatable:: xyzvol(:,:,:),qcor(:,:,:)

  logical anormal,oldf

  anormal=.true.
  oldf=.false.

!   set up 3-d aspect tensor

                  if(mype.eq.0) write(0,*)' at 9 in get3berrq'
   allocate(xyzvol(ips:ipe,jps:jpe,kps:kpe))
   xyzvol=1.
   call regular_init_filt(isofilter_3,nhaloc, &
             ids, ide, jds, jde, kds, kde, &                          ! domain indices
             ips, ipe, jps, jpe, kps, kpe, &                          ! patch indices
             ims, ime, jms, jme, kms, kme, &                          ! memory indices
             inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j ) ! processor info
   call init_raf(aspect,npass,no_interp,binom, &
          nsmooth,nsmooth_shapiro,ifilt_ord,isofilter_3,xyzvol,anormal,oldf, &
             ids, ide, jds, jde, kds, kde, &                          ! domain indices
             ips, ipe, jps, jpe, kps, kpe, &                          ! patch indices
             ims, ime, jms, jme, kms, kme, &                          ! memory indices
             inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j ) ! processor info

   deallocate(xyzvol)

!      make test plots to look at correlation functions

! allocate(qcor(ims:ime,jms:jme,kms:kme))
! qcor=0.
! do jj=1,20
!  do ii=1,20
!   i=iplot(ii,jj)
!   j=jplot(ii,jj)
!   k=kplot(ii,jj)
!   if(i.ge.ips.and.i.le.ipe.and.j.ge.jps.and.j.le.jpe.and.k.ge.kps.and.k.le.kpe) qcor(i,j,k)=1.
!  end do
! end do
!call regular_raf(qcor,isofilter_3)
!call regular_ad_raf(qcor,isofilter_3)
!call outgradq(qg,qcor, &
!                ids, ide, jds, jde, kds, kde, &         ! domain indices
!                ips, ipe, jps, jpe, kps, kpe, &         ! patch indices
!                ims, ime, jms, jme, kms, kme, &         ! memory indices
!                inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j ) ! processor info

!--------
!-------- now get final estimate of amplitude
!--------
         do k=kps,kpe
                 eiijjmin=huge(eiijjmin)
                 eiijjmax=-huge(eiijjmax)
          do j=jps,jpe
           do i=ips,ipe
            isofilter_3(1)%amp(i,j,k)=isofilter_3(1)%amp(i,j,k)*amp(i,j,k)
                 eiijjmin=min(amp(i,j,k),eiijjmin)
                 eiijjmax=max(amp(i,j,k),eiijjmax)
           end do
          end do
              call mpi_reduce(eiijjmax,eiijjmaxall,1,mpi_real,mpi_max,0,my_comm,ierr)
              call mpi_reduce(eiijjmin,eiijjminall,1,mpi_real,mpi_min,0,my_comm,ierr)
              if(mype.eq.0) print *,' in get3berrq, k,eiijjmax,min=',k,eiijjmaxall,eiijjminall
         end do


       return
       end
