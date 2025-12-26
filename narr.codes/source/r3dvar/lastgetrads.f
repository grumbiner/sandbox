subroutine lastgetrads(yorad,eyorad00,xbarbrad,bighradh,bighradv,ibighradh,mraddata, &
        icxrad,predrad,npred,lbig2ges,lmetaex,rad_dat,mrad_dat,mype,ivarrad,varlats, &
         nvarlats,jpch,jpchus,npes)

!-------- final processing of data before analysis iterations.
!--------
!-------- 1.  read all obs information into memory
!-------- 2.  get scriptK, the interpolation operator (integration in vertical)

  include 'mpif.h'
         include "my_comm.h"
  include 'types.h'

  real(4) yorad(max(1,mraddata)),xbarbrad(max(1,mraddata)),bighradh(lbig2ges,max(1,mraddata))
  real(4) bighradv(2*lmetaex,max(1,mraddata)),eyorad00(max(1,mraddata))
  integer(4) ibighradh(lbig2ges,max(1,mraddata))
  type(rad_obs) rad_dat(max(1,mrad_dat))
  integer(4) icxrad(2,max(1,mraddata))
  real(4) predrad(npred+jpchus-1,max(1,mraddata))
  integer(4) ivarrad(nvarlats,jpch)
  real(4) varlats(nvarlats)

  real(4),allocatable::radlat(:),radlon(:)

  allocate(radlat(max(1,mraddata)))
  allocate(radlon(max(1,mraddata)))
  ij=0
  bighradv=0.
     kpbotmax=1
     kpbotmin=lmetaex
  if(mraddata.gt.0) then
   do i=1,mrad_dat
    nthis=rad_dat(i)%ncc
    nsig=rad_dat(i)%nsig
    do j=1,nthis
     ij=ij+1
     do m=1,lbig2ges
      bighradh(m,ij)=rad_dat(i)%wgts(m)
      ibighradh(m,ij)=rad_dat(i)%iwgts(m)
     end do
     kpbot=rad_dat(i)%kpbot
         kpbotmax=max(kpbot,kpbotmax)
         kpbotmin=min(kpbot,kpbotmin)
     do k=kpbot,lmetaex
      kr=lmetaex+1-k
      bighradv(kr,ij)=rad_dat(i)%htlto(k,j)
      bighradv(lmetaex+kr,ij)=rad_dat(i)%htlto(nsig+k,j)
     end do

!------- finally include remaining information

     yorad(ij)=rad_dat(i)%obsbt(j)
     xbarbrad(ij)=rad_dat(i)%gesbt(j)
     eyorad00(ij)=1./sqrt(rad_dat(i)%var(j))
     icxrad(1,ij)=rad_dat(i)%icx(j)
     icxrad(2,ij)=j
     predrad(:npred+nthis-1,ij)=rad_dat(i)%pred(:npred+nthis-1)
     radlon(ij)=rad_dat(i)%lon
     radlat(ij)=rad_dat(i)%lat
    end do
   end do
  end if

  if(mraddata.gt.0) then
      if(mype.eq.0)  &
         print *,' at 3 in lastgetrads, mype,ij,mraddata,kpbotmax,min,nsig,lmetaex=',mype,ij,mraddata, &
                                                  kpbotmax,kpbotmin,nsig,lmetaex
     if(ij.ne.mraddata) then
       print *,' problem in lastgetrads'
       stop
     end if
  end if

! call var_2d_rad_locs(radlon,radlat,icxrad,mraddata,ivarrad,varlats,nvarlats,npes)
  deallocate(radlon)
  deallocate(radlat)

return
end subroutine lastgetrads
