subroutine getbigeeta(bigeetah,bigeetavx,bigeetavy,ibigeetah,ibigeetav, &
                      xeta,yeta,xetam,yetam,imeta,jmeta,iorddata,lbig2data, &
                    rxcglb,rycglb,nxcglb,nycglb,nxc,nyc,myis2,myie2,myjs2,myje2,mype)

!   compute interpolation constants for operator bigeeta, which interpolates from
!    the coarse analysis grid to the full eta grid

  include 'mpif.h'
      include "my_comm.h"

  real(4) bigeetah(imeta,jmeta,lbig2data),bigeetavx(imeta,jmeta,lbig2data)
  real(4) bigeetavy(imeta,jmeta,lbig2data)
  integer(4) ibigeetah(imeta,jmeta,lbig2data),ibigeetav(imeta,jmeta,lbig2data)
  real(4) xeta(imeta,jmeta+1),yeta(imeta,jmeta+1)
  real(4) xetam(imeta,jmeta+1),yetam(imeta,jmeta+1)
  real(4) rxcglb(nxcglb),rycglb(nycglb)

  real(4),allocatable::rxcmglb(:),rycmglb(:)
  integer(4),allocatable::iflag(:,:)

!     compute horizontal h-grid interpolation constants

   allocate(iflag(imeta,jmeta))

   nf=1 ; ndx=0 ; ndy=0 ; ndxx=0
   ndxy=0 ; ndyy=0
   lhalf=0

   call simpin2f(bigeetah,bigeetah,bigeetah,bigeetah,bigeetah,bigeetah,ibigeetah, &
                 iflag,xeta,yeta,imeta*jmeta,iorddata,lhalf,lbig2data, &
                 rxcglb,rycglb,nxcglb,nycglb,nf,ndx,ndy,ndxx,ndxy,ndyy)
   call glb2loc_iwgts(ibigeetah,imeta*jmeta,lbig2data,nxcglb,nycglb,nxc,nyc,2)
     iflagmax=maxval(iflag(myis2:myie2,myjs2:myje2))
     iflagmin=minval(iflag(myis2:myie2,myjs2:myje2))
     call mpi_reduce(iflagmax,iflagmaxall,1,mpi_integer,mpi_max,0,my_comm,ierror)
     call mpi_reduce(iflagmin,iflagminall,1,mpi_integer,mpi_min,0,my_comm,ierror)
     if(mype.eq.0) print *,' max,min iflag for bigeetah in getbigeeta =',iflagmaxall,iflagminall

!     compute horizontal interpolation constants for v-grid

   ndxx=0
   ndxy=0 ; ndyy=0
   nfv=0 ; ndxv=1 ; ndyv=1
   deg2rad=atan(1.)/45.
   escale=conmc('rerth$')*deg2rad
   allocate(rxcmglb(nxcglb)) ; allocate(rycmglb(nycglb))
   rxcmglb=rxcglb*escale ; rycmglb=rycglb*escale

   lhalf=0
   call simpin2f(bigeetavx,bigeetavx,bigeetavy,bigeetavy,bigeetavy,bigeetavy,ibigeetav, &
                 iflag,xetam,yetam,imeta*jmeta,iorddata,lhalf,lbig2data, &
                 rxcmglb,rycmglb,nxcglb,nycglb,nfv,ndxv,ndyv,ndxx,ndxy,ndyy)
   do j=myjs2,myje2
    coslati=1./cos(deg2rad*yeta(1,j))
    do m=1,lbig2data
     do i=myis2,myie2
      bigeetavx(i,j,m)=coslati*bigeetavx(i,j,m)
     end do
    end do
   end do
   call glb2loc_iwgts(ibigeetav,imeta*jmeta,lbig2data,nxcglb,nycglb,nxc,nyc,2)
     iflagmax=maxval(iflag(myis2:myie2,myjs2:myje2))
     iflagmin=minval(iflag(myis2:myie2,myjs2:myje2))
     call mpi_reduce(iflagmax,iflagmaxall,1,mpi_integer,mpi_max,0,my_comm,ierror)
     call mpi_reduce(iflagmin,iflagminall,1,mpi_integer,mpi_min,0,my_comm,ierror)
     if(mype.eq.0) print *,' max,min iflag for bigeetav in getbigeeta =',iflagmaxall,iflagminall
   deallocate(rxcmglb) ; deallocate(rycmglb)
   deallocate(iflag)

return
end subroutine getbigeeta
