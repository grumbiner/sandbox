subroutine getradges(isz,obstype,sfct,w10ges,tgese,qgese,pgese,pgesei, &
          kpgesbot,ozgese,zsfcges,veg_type,veg_frac,soil_type,soil_temp, &
          soil_moi,sno,tvp,qvp,pvp,pvpi,poz,zz,ts5,wsp10,vegtype5,vegf5,soiltype5, &
          soilt5,soilm5,snow5, &
         rlon,rlat,ndata, &
        iordges,lbig2ges,lhalfges,imeta,jmeta,nsig, &
        iwgtsout,wgtsout,kpvpbot, &
            wbglb,dlon,sbglb,dlat,istaghglb,istagvglb,imetaglb,jmetaglb)

!-------- obtain guess 3-d field at observed x,y,p for 
!--------    a group of wind components

! <--  ts5:  guess skin temperature at observation locations
! <--  wsp10: guess 10m wind speed at observation locations
! <--  tvp:  guess virtual temperature profile at observation locations
! <--  qvp:  guess specific humidity profile at observation locations
! <--  pvp:  guess mid-layer pressures at observation locations
! <--  pvpi: guess interface pressures at observation locations
! <--  poz:  guess ozone at observaton locations
! <--  zz:   guess surface elevation at observation locations
!  --> sfct: ges skin temperature
!  --> w10ges: ges 10m wind (on eta v grid)
!  --> tgese:  ges virtual temperature
!  --> qgese:  ges specific humidity
!  --> pgese:  ges pressure at each grid point
!  --> pgesei: ges pressure at each interface point
!  --> ozgese: ges ozone
!  --> zsfcges: ges surface elevation (m)
!  --> rlon,rlat:  observation coodinates (on rotated eta grid, degrees)
!  --> ndata:      number of observations
!  --> iordges: order of interpolation
!  --> lbig2ges: number of interpolating points
!  --> lhalfges: 2nd interpolation order parameter
!  --> imeta,jmeta,nsig:  dimensions of guess grid
!  --> wb,dlon,sb,dlat:   info for describing horizontal eta-grid
!  --> istagh,istagv:     info on starting point for h, v grids

!-------  note: guess value of 1.e20 is returned for every observation
!-------    location which falls outside the guess domain (this includes
!-------               points below model terrain)

  include 'mpif.h'
      include "my_comm.h"

       character(10) obstype
  real(8) ts5(ndata),wsp10(ndata),tvp(nsig,ndata),qvp(nsig,ndata),pvp(nsig,ndata)
  real(8) soilt5(ndata),vegf5(ndata),vegtype5(ndata),soilm5(ndata),soiltype5(ndata)
  real(8) snow5(ndata)
  integer(4) kpvpbot(ndata)
  real(8) pvpi(nsig+1,ndata),poz(nsig,ndata),zz(ndata)
  real(4) sfct(imeta*jmeta),w10ges(imeta*jmeta)
  real(4) tgese(imeta*jmeta,nsig),qgese(imeta*jmeta,nsig)
  real(4) pgese(imeta*jmeta,nsig),pgesei(imeta*jmeta,nsig+1)
  integer(4) kpgesbot(imeta*jmeta)
  real(4) ozgese(imeta*jmeta,nsig),zsfcges(imeta*jmeta)
  real(4),dimension(imeta*jmeta)::veg_type,veg_frac,soil_type,soil_temp,soil_moi,sno
  real(4) rlon(ndata),rlat(ndata)
  integer(4) iwgtsout(lbig2ges,ndata)
  real(4) wgtsout(lbig2ges,ndata)

  real(4),allocatable::wgtsh(:,:),wgtsv(:,:)
  integer(4),allocatable::iwgtsh(:,:),iwgtsv(:,:),iflagh(:),iflagv(:)

  rkappa=conmc('rd$')/conmc('cp$')
  special_value=1.e20
  test_value=.98*special_value
  call mpi_comm_rank(my_comm,mype,ierr)

!-------- get horizontal interpolation weights on h grid

  if(ndata.gt.0) then
   allocate(wgtsh(ndata,lbig2ges)) ; allocate(iwgtsh(ndata,lbig2ges))
   allocate(iflagh(ndata))
   call st_simpin2f(wgtsh,iwgtsh,iflagh,rlon,rlat,ndata,iordges,lhalfges,lbig2ges, &
                   wbglb,dlon,sbglb,dlat,imetaglb,jmetaglb,istaghglb)
   call st_glb2loc_iwgts(iwgtsh,ndata,lbig2ges,imetaglb,jmetaglb,imeta,jmeta)
   allocate(wgtsv(ndata,lbig2ges)) ; allocate(iwgtsv(ndata,lbig2ges))
   allocate(iflagv(ndata))
   call st_simpin2f(wgtsv,iwgtsv,iflagv,rlon,rlat,ndata,iordges,lhalfges,lbig2ges, &
                    wbglb,dlon,sbglb,dlat,imetaglb,jmetaglb,istagvglb)
   call st_glb2loc_iwgts(iwgtsv,ndata,lbig2ges,imetaglb,jmetaglb,imeta,jmeta)
   do i=1,ndata
    do m=1,lbig2ges
     wgtsout(m,i)=wgtsh(i,m)
     iwgtsout(m,i)=iwgtsh(i,m)
    end do
   end do
   ts5=special_value
   wsp10=special_value
   vegtype5=special_value
   vegf5=special_value
   soiltype5=special_value
   soilt5=special_value
   soilm5=special_value
   snow5=special_value
   tvp=special_value
   qvp=special_value
   pvp=special_value
   pvpi=special_value
   poz=special_value
   zz=special_value
   nout=0
   do i=1,ndata
    if(iflagh(i).gt.0.and.iflagv(i).gt.0) then
     ts5(i)=0.
     wsp10(i)=0.
     vegtype5(i)=0.
     vegf5(i)=0.
     soiltype5(i)=0.
     soilt5(i)=0.
     soilm5(i)=0.
     snow5(i)=0.
     tvp(:,i)=0.
     qvp(:,i)=0.
     pvp(:,i)=0.
     pvpi(:,i)=0.
     poz(:,i)=0.
     zz(i)=0.
     kpvpbot(i)=1
     do kk=1,lbig2ges
      kpvpbot(i)=max(kpvpbot(i),kpgesbot(iwgtsh(i,kk)))
      ts5(i)=ts5(i)+wgtsh(i,kk)*sfct(iwgtsh(i,kk))
      wsp10(i)=wsp10(i)+wgtsv(i,kk)*w10ges(iwgtsv(i,kk))
      vegtype5(i)=vegtype5(i)+wgtsv(i,kk)*veg_type(iwgtsv(i,kk))
      vegf5(i)=vegf5(i)+wgtsv(i,kk)*veg_frac(iwgtsv(i,kk))
      soiltype5(i)=soiltype5(i)+wgtsv(i,kk)*soil_type(iwgtsv(i,kk))
      soilt5(i)=soilt5(i)+wgtsv(i,kk)*soil_temp(iwgtsv(i,kk))
      soilm5(i)=soilm5(i)+wgtsv(i,kk)*soil_moi(iwgtsv(i,kk))
      snow5(i)=snow5(i)+wgtsv(i,kk)*sno(iwgtsv(i,kk))
      tvp(:,i)=tvp(:,i)+wgtsh(i,kk)*tgese(iwgtsh(i,kk),:)
      qvp(:,i)=qvp(:,i)+wgtsh(i,kk)*qgese(iwgtsh(i,kk),:)
      pvp(:,i)=pvp(:,i)+wgtsh(i,kk)*pgese(iwgtsh(i,kk),:)
      pvpi(:,i)=pvpi(:,i)+wgtsh(i,kk)*pgesei(iwgtsh(i,kk),:)
      poz(:,i)=poz(:,i)+wgtsh(i,kk)*ozgese(iwgtsh(i,kk),:)
      zz(i)=zz(i)+wgtsh(i,kk)*zsfcges(iwgtsh(i,kk))
     end do

!    must convert skin temperature from potential to sensible temperature
     ts5sens=ts5(i)*(.001*pvpi(1,i))**rkappa
!        call w3fa03(pvpi(1,i),stdz,stdt,stdth)
!        print ('('' mype,etc='',i4,7f12.2)'),mype,pvpi(1,i),zz(i),ts5sens,ts5(i),stdz,stdt,stdth
     ts5(i)=ts5sens

    else
     nout=nout+1
     print *,' in getradges, i,mype,iflagh,iflagv,rlong,rlatg=',i,mype,iflagh(i),iflagv(i),rlon(i),rlat(i)
    end if
   end do
   if(nout.gt.0) print *,' PROBLEM IN GETRADGES, mype,nout,isz,obstype=',mype,nout,isz,obstype

   deallocate(wgtsh) ; deallocate(iwgtsh)
   deallocate(iflagh)
   deallocate(wgtsv) ; deallocate(iwgtsv)
   deallocate(iflagv)
  end if

return
end subroutine getradges
