subroutine coast_ice_check(lndsea,isflg,icst,sm,sice,sice_missing,sno,rlong,rlatg,rlate,ndata, &
         iord,lbig2,lhalf,imeta,jmeta,wbglb,dlon,sbglb,dlat,istaghglb,imetaglb,jmetaglb)

!    set land-sea and ice/snow flags based on proximity to eta-grid land and snow/ice

! <--> lndsea:  set equal to 1 if in proximity to land point
! <--> isflg:   set equal to 1 if in proximity of sea ice or snow
! <--> icst:    set equal to 1 if any change in land-sea-sno-ice over one grid box
!  --> sm,sice,sno:  fields on eta h-grid, used to determine land/sea/snow/ice
!  --> rlong,rlatg:  eta grid lat,lon of observations (degrees)
!  --> ndata:       number of observations
!  --> iord:        interpolation order
!  --> lbig2:       number of interpolating points
!  --> lhalf:       extra interpolation parameter
!  --> imeta,jmeta: eta grid dimensions
!  --> wb,dlon,sb,dlat,istagh:  eta grid descriptors


!                                   sm   sice   sno
!         land, no ice, no snow      0     0     0
!         land, snow or ice          0     0    >0
!         sea, no ice                1     0     0
!         sea, ice                   0     1     0

  include 'mpif.h'
      include "my_comm.h"

  integer(4) lndsea(ndata),isflg(ndata),icst(ndata)
  real(4) sm(imeta*jmeta),sice(imeta*jmeta),sno(imeta*jmeta)
  real(4) rlong(ndata),rlatg(ndata),rlate(ndata)
  logical sice_missing

  real(4),allocatable::wgtsh(:,:)
  integer(4),allocatable::iwgtsh(:,:),iflagh(:)

  call mpi_comm_rank(my_comm,mype,ierr)
  if(ndata.gt.0) then
   allocate(wgtsh(ndata,lbig2)) ; allocate(iwgtsh(ndata,lbig2))
   allocate(iflagh(ndata))
   call st_simpin2f(wgtsh,iwgtsh,iflagh,rlong,rlatg,ndata,iord,lhalf,lbig2, &
           wbglb,dlon,sbglb,dlat,imetaglb,jmetaglb,istaghglb)
   call st_glb2loc_iwgts(iwgtsh,ndata,lbig2,imetaglb,jmetaglb,imeta,jmeta)
   deallocate(wgtsh)
   nout=0
   do i=1,ndata
!              print *,' at 1 in coast_ice_check, i,iflagh,lndsea=',iflagh(i),lndsea(i)
!              print *,' sm(iwgt-20--iwgt+20)=',(sm(iwgtsh(i,1)+k),k=-20,20)
!              print *,' sice(iwgt-20--iwgt+20)=',(sice(iwgtsh(i,1)+k),k=-20,20)
!              print *,' sno(iwgt-20--iwgt+20)=',(sno(iwgtsh(i,1)+k),k=-20,20)
    if(iflagh(i).gt.0) then
     if(lndsea(i).ne.1) then
      seasum=0.
      do k=1,lbig2
       seasum=seasum+sm(iwgtsh(i,k))
!                print *,' at 2 in coast_ice_check, i,k,seasum,sm=',i,k,seasum,sm(iwgtsh(i,k))
      end do
!                   print *,' at 3 in coast_ice_check, lbig2,seasum=',lbig2,seasum
      if(seasum.lt.lbig2-.5) lndsea(i)=1
!                   print *,' at 4 in coast_ice_check, i,lndsea(i)=',i,lndsea(i)
     end if
     if(isflg(i).ne.1) then
      snosum=0. ; sumice=0.
      do k=1,lbig2
       snosum=max(snosum,sno(iwgtsh(i,k)))
       sumice=sumice+sice(iwgtsh(i,k))
      end do
      if(sice_missing.and.rlate(i).gt.60.) sumice=lbig2
      if(snosum.gt.0.001 .or. sumice.gt.0.) isflg(i)=1
     end if
     itest0=nint(sm(iwgtsh(i,1))+2.*sice(iwgtsh(i,1)))
     do k=2,lbig2
      itest=nint(sm(iwgtsh(i,k))+2.*sice(iwgtsh(i,k)))
      if(itest.ne.itest0) then
       icst(i)=1
       exit
      end if
     end do
    else
     nout=nout+1
    end if
   end do
   deallocate(iwgtsh) ; deallocate(iflagh)
   if(nout.gt.0) print *,' PROBLEM IN COAST_ICE_CHECK, mype,nout=',mype,nout
  end if

return
end subroutine coast_ice_check
