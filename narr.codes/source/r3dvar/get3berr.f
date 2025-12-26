subroutine get3berr(e,lat1,lat2,wgts_bightp,iwgts_bightp,rlenxy,rlenp,isofilter_3, &
                    nhaloc,npass,no_interp,binom,nsmooth,nsmooth_shapiro,ifilt_ord, &
                    nxc,nyc,lmetaex,myxsc,myysc, &
             ids, ide, jds, jde, kds, kde, &                          ! domain indices
             ips, ipe, jps, jpe, kps, kpe, &                          ! patch indices
             ims, ime, jms, jme, kms, kme, &                          ! memory indices
             inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j ) ! processor info
!--------
!-------- produce 3-d filter parameters from input error, correlation
!--------  lengths, which are currently only a function of vertical.
!  -->  e:    errors
!  -->  rlenxy: horizontal correlation lengths (grid units)
!  -->  rlenp:  vertical correlation lengths (grid units)
! <--   filtercons: output filter constants for this variable
!  -->  nxc,nyc,lmetaex:  grid dimensions
!--
!--
         include 'mpif.h'
      include "my_comm.h"
         include 'filtertype.h'

  logical binom
  INTEGER(4), INTENT(IN) :: pe_of_injn(inpes,jnpes),in_of_i(ids:ide),jn_of_j(jds:jde)
         real(4) e(lmetaex,lat1:lat2),rlenxy(lmetaex),rlenp(lmetaex)
         real(4) wgts_bightp(2,nxc,nyc)
         integer(4) iwgts_bightp(2,nxc,nyc)
         type(filter_cons) isofilter_3(14)
!--------
         real(4),allocatable::aspect(:,:,:,:),xyzvol(:,:,:)

  logical anormal,oldf

  anormal=.false.
  oldf=.true.

!   set up 3-d aspect tensor

                  if(mype.eq.0) write(0,*)' at 9 in get3berr'
   allocate(aspect(7,ips:ipe,jps:jpe,kps:kpe))
   allocate(xyzvol(ips:ipe,jps:jpe,kps:kpe))
   xyzvol=1.
   do k=kps,kpe
        if(mype.eq.0) print *,' before isonormal3d, rlenxy for k=',k,' = ',rlenxy(k)
    do j=jps,jpe
     do i=ips,ipe
      aspect(1,i,j,k)=rlenxy(k)**2
      aspect(2,i,j,k)=rlenxy(k)**2
      aspect(3,i,j,k)=rlenp(k)**2
      aspect(4:7,i,j,k)=0.
     end do
    end do
   end do
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
   deallocate(aspect) ; deallocate(xyzvol)

!     normalize

   call isonormal3d(isofilter_3, &
             ids, ide, jds, jde, kds, kde, &                          ! domain indices
             ips, ipe, jps, jpe, kps, kpe, &                          ! patch indices
             ims, ime, jms, jme, kms, kme, &                          ! memory indices
             inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j ) ! processor info

!--------
!-------- now get final estimate of amplitude
!--------
         do k=kps,kpe
              eiijjmax=-huge(eiijjmax)
              eiijjmin=huge(eiijjmin)
          jj=myysc-1
          do j=jps,jpe
           jj=jj+1
           ii=myxsc-1
           do i=ips,ipe
            ii=ii+1
            eiijj=e(k,iwgts_bightp(1,ii,jj))*wgts_bightp(1,ii,jj)+ &
                  e(k,iwgts_bightp(2,ii,jj))*wgts_bightp(2,ii,jj)
            isofilter_3(1)%amp(i,j,k)=isofilter_3(1)%amp(i,j,k)*eiijj
                 eiijjmax=max(eiijj,eiijjmax)
                 eiijjmin=min(eiijj,eiijjmin)
           end do
          end do
              call mpi_reduce(eiijjmax,eiijjmaxall,1,mpi_real,mpi_max,0,my_comm,ierr)
              call mpi_reduce(eiijjmin,eiijjminall,1,mpi_real,mpi_min,0,my_comm,ierr)
              if(mype.eq.0) print *,' in get3berr, k,eiijjmax,min=',k,eiijjmaxall,eiijjminall
              if(mype.eq.0) print *,' in get3berr, k,eiijjmax,min2=',k,eiijjmaxall**2,eiijjminall**2
         end do


       return
       end
