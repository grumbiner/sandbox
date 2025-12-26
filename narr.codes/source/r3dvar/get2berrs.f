subroutine get2berrs(e,lat1,lat2,wgts_bightp,iwgts_bightp,rlenxy,isofilter_2, &
                     nhaloc,npass,no_interp,binom,nsmooth,nsmooth_shapiro,ifilt_ord, &
                     nxc,nyc,myxs,myys, &
             ids, ide, jds, jde, kds, kde, &                          ! domain indices
             ips, ipe, jps, jpe, kps, kpe, &                          ! patch indices
             ims, ime, jms, jme, kms, kme, &                          ! memory indices
             inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j ) ! processor info

!-------- same as get3berrs, but for 2--d fields
!--------
!  -->    e,rlenxy:  error amplitude, and correlation length   
! <--     filtercons: output filter constants for this variable
!  -->    nxc,nyc: grid dimensions
!--------
  include 'mpif.h'
      include "my_comm.h"
  include 'filtertype.h'

  logical binom
  INTEGER(4), INTENT(IN) :: pe_of_injn(inpes,jnpes),in_of_i(ids:ide),jn_of_j(jds:jde)
         real(4) e(lat1:lat2)
         real(4) wgts_bightp(2,nxc,nyc)
         integer(4) iwgts_bightp(2,nxc,nyc)
  type(filter_cons) isofilter_2(14)
!--------
         real(4),allocatable::aspect(:,:,:),xyzvol(:,:)

  logical anormal,oldf

  anormal=.false.
  oldf=.true.

!  compute filter parameters

   allocate(aspect(7,ips:ipe,jps:jpe))
   allocate(xyzvol(ips:ipe,jps:jpe))
   xyzvol=1.
   
   do j=jps,jpe
    do i=ips,ipe
     aspect(1,i,j)=rlenxy**2
     aspect(2,i,j)=rlenxy**2
     aspect(3,i,j)=1.
     aspect(4:7,i,j)=0.
    end do
   end do
   kds2=1 ; kde2=1
   kps2=1 ; kpe2=1
   kms2=1 ; kme2=1
   call regular_init_filt(isofilter_2,nhaloc, &
             ids, ide, jds, jde, kds2, kde2, &                          ! domain indices
             ips, ipe, jps, jpe, kps2, kpe2, &                          ! patch indices
             ims, ime, jms, jme, kms2, kme2, &                          ! memory indices
             inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j ) ! processor info
   call init_raf(aspect,npass,no_interp,binom, &
              nsmooth,nsmooth_shapiro,ifilt_ord,isofilter_2,xyzvol,anormal,oldf, &
             ids, ide, jds, jde, kds2, kde2, &                          ! domain indices
             ips, ipe, jps, jpe, kps2, kpe2, &                          ! patch indices
             ims, ime, jms, jme, kms2, kme2, &                          ! memory indices
             inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j ) ! processor info

   deallocate(aspect)
   deallocate(xyzvol)

!     normalize

   call isonormal2d(isofilter_2, &
             ids, ide, jds, jde, kds2, kde2, &                          ! domain indices
             ips, ipe, jps, jpe, kps2, kpe2, &                          ! patch indices
             ims, ime, jms, jme, kms2, kme2, &                          ! memory indices
             inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j ) ! processor info

!--------
!-------- now get final estimate of amplitude
!--------
              eiijjmax=-huge(eiijjmax)
              eiijjmin=huge(eiijjmin)
              ampmax=-huge(ampmax)
              ampmin=huge(ampmin)
         jj=myys-1
         do j=jps,jpe
          jj=jj+1
          ii=myxs-1
          do i=ips,ipe
           ii=ii+1
                  ampmax=max(isofilter_2(1)%amp(i,j,1),ampmax)
                  ampmin=min(isofilter_2(1)%amp(i,j,1),ampmin)
           eiijj=e(iwgts_bightp(1,ii,jj))*wgts_bightp(1,ii,jj)+ &
                  e(iwgts_bightp(2,ii,jj))*wgts_bightp(2,ii,jj)
           isofilter_2(1)%amp(i,j,1)=isofilter_2(1)%amp(i,j,1)*eiijj
                 eiijjmax=max(eiijj,eiijjmax)
                 eiijjmin=min(eiijj,eiijjmin)
          end do
         end do
                call mpi_reduce(eiijjmax,eiijjmaxall,1,mpi_real,mpi_max,0,my_comm,ierr)
                call mpi_reduce(eiijjmin,eiijjminall,1,mpi_real,mpi_min,0,my_comm,ierr)
                if(mype.eq.0) print *,' in get2berrs, eiijjmax,min=',eiijjmaxall,eiijjminall
                if(mype.eq.0) print *,' in get2berrs, eiijjmax,min2=',eiijjmaxall**2,eiijjminall**2
                call mpi_reduce(ampmax,ampmaxall,1,mpi_real,mpi_max,0,my_comm,ierr)
                call mpi_reduce(ampmin,ampminall,1,mpi_real,mpi_min,0,my_comm,ierr)
                if(mype.eq.0) print *,' in get2berrs, ampmax,min=',ampmaxall,ampminall
                if(mype.eq.0) print *,' in get2berrs, ampmax,min2=',ampmaxall**2,ampminall**2

       return
       end
