subroutine getwges(wges,rlon,rlat,rpres,welev,wstaid,delta,epsilnw,ndata, &
            iordges,lbig2ges,lbig3ges,lhalfges, &
            rletaobs,wgtsv,iwgtsv, &
            imeta,jmeta,lmetaex,pdres01,ugges,vgges, &
            etamex,ptop,lmh,wtype,wobs,hgges,etaiex,etheta,werr,wbglb,dlon,sbglb,dlat, &
            istaghglb,istagvglb,imetaglb,jmetaglb)

!-------- obtain guess 3-d field at observed x,y,p for 
!--------    a group of wind components

!--   wges:   output guess interpolated to obs locations
!--   rlon,rlat:  grid lon, lat of obs location (degrees)
!--   rpres:  log pressure (mb) for each obs
!--   delta:   cos(angle of wind component to x-axis) for each obs
!--   epsilnw: sin(angle of wind component to x-axis) for each obs
!--   ndata:  number of obs
!--   nx,ny,np:  dimensions of guess grid
!--   rx,ry:     horizontal grid coordinates
!--   rlogpges:  pressure of each point on guess grid
!--   ugges,vgges:  guess wind components, wobs=delta*ugges+epsilnw*vgges

!-------  note: guess value of 1.e20 is returned for every observation
!-------    location which falls outside the guess domain (this includes
!-------               points below model terrain)

  include 'mpif.h'
      include "my_comm.h"

  real(4) wges(ndata),rlon(ndata),rlat(ndata),rpres(ndata),welev(ndata)
  character(1) wstaid(8,ndata)
  real(4) delta(ndata),epsilnw(ndata),ugges(imeta*jmeta*lmetaex),vgges(imeta*jmeta*lmetaex)
  real(4) hgges(imeta*jmeta*(lmetaex+1)),pdres01(imeta*jmeta)
  real(4) etamex(lmetaex)
  integer(4) lmh(imeta*jmeta)
  real(4) wtype(ndata),wobs(ndata),etaiex(lmetaex+1),etheta(ndata),werr(ndata)
  real(4) wgtsv(ndata,lbig3ges),rletaobs(ndata)
  integer(4) iwgtsv(ndata,lbig3ges)

  character(1) dollar
  data dollar/'$'/
  real(4),allocatable::rletam(:),rlpdobs(:),pdres01obs(:)
  real(4),allocatable::hthis(:),pthis(:),welevc(:)
  real(4),allocatable::wgtsh(:,:)
  integer(4),allocatable::iwgtsh(:,:),iflagh(:),iflagv(:)
  character(1) staida8(8),staidb8(8)
  character(8) staida,staidb
  equivalence(staida,staida8(1))
  equivalence(staidb,staidb8(1))
  real(8) delhbar8,delhrms8
  real(8) delhbarall8,delhrmsall8

  special_value=1.e20
  test_value=.98*special_value
  call mpi_comm_rank(my_comm,mype,ierr)

       werrmax=-huge(werrmax)
       werrmin=huge(werrmin)
       do i=1,ndata
        werrmax=max(werr(i),werrmax)
        werrmin=min(werr(i),werrmin)
       end do
!        print *,' mype,werrmax,min just in getwges=',mype,werrmax,werrmin


!-------- obtain pressure elevation to use in place of missing elevations
!--------    (used to adjust error)

  iprint=0
  msgelev=0
  if(ndata.gt.0) then
   allocate(welevc(ndata))
   welevc=welev
   do i=1,ndata
    if(welev(i).gt.1.e6.and.nint(wtype(i)).lt.2270) then
     msgelev=msgelev+1
     pobs0=exp(rpres(i))
     call w3fa03(pobs0,welevc(i),dumt,dumtheta)
     iprint=iprint+1
!    if(iprint.le.50) print *,' in getwges, wtype,pobs,preselevobs=',wtype(i),pobs0,welevc(i)
    end if
   end do
  end if
  call mpi_barrier(my_comm,ierror)
  call mpi_reduce(msgelev,msgelevall,1,mpi_integer,mpi_sum,0,my_comm,ierror)
  if(mype.eq.0) print *,' in getwges, number obs with missing elevation=',msgelevall

  allocate(rletam(lmetaex))
  rletam=log(etamex)

!-------- find minimum guess pressure

  call getmaxmin2(peta1min,petagmax,pdres01,imeta,jmeta)
  peta1min=etamex(1)*peta1min+ptop
  petagmax=etamex(lmetaex)*petagmax+ptop
  if(mype.eq.0) then
   print '('' in getwges, peta1min='',es14.5)',peta1min
   print '('' in getwges, petag,max='',es14.5)',petagmax
  end if

!-------- add 125mb to petagmax, so we include more data underground

  petagmax=petagmax+125.

!-------- get horizontal interpolation weights on h grid

  if(ndata.gt.0) then
   allocate(wgtsh(ndata,lbig2ges)) ; allocate(iwgtsh(ndata,lbig2ges))
   allocate(iflagh(ndata))
   call st_simpin2f(wgtsh,iwgtsh,iflagh,rlon,rlat,ndata,iordges,lhalfges,lbig2ges, &
                   wbglb,dlon,sbglb,dlat,imetaglb,jmetaglb,istaghglb)
   call st_glb2loc_iwgts(iwgtsh,ndata,lbig2ges,imetaglb,jmetaglb,imeta,jmeta)
   do i=1,ndata
    if(iflagh(i).gt.0) then
     do kk=1,lbig2ges
      khthis=min(lmetaex+1,lmh(iwgtsh(i,kk))+1)
      ihggesthis=iwgtsh(i,kk)+(khthis-1)*imeta*jmeta
      if(hgges(ihggesthis).gt.test_value) iflagh(i)=0
     end do
    end if
   end do
!-------- interpolate pressure variable to obs points so we
!-------- can convert vertical obs coordinate to eta grid units

   allocate(pdres01obs(ndata))
   pdres01obs=petagmax-ptop
   do i=1,ndata
    if(iflagh(i).gt.0) then
     pdres01obs(i)=0.
     do kk=1,lbig2ges
      pdres01obs(i)=pdres01obs(i)+wgtsh(i,kk)*pdres01(iwgtsh(i,kk))
     end do
    end if
   end do
  end if

  allocate(hthis(lmetaex+1))
  allocate(pthis(lmetaex+1))

!-------- for surface obs, replace obs pressure with guess pressure
!--------  at obs elevation (only if pressure was missing--indicated
!--------                      by $-sign in 6-th character of staid)

  numsfcw=0
  sfcpmax=-huge(sfcpmax)
  sfcpmin=huge(sfcpmin)
  elevmax=-huge(elevmax)
  elevmin=huge(elevmin)
  rp0max=-huge(rp0max)
  rp0min=huge(rp0min)
  kabovemax=0
  kabovemin=1000
  numdollar=0
  if(ndata.gt.0) then
   do i=1,ndata
    if(iflagh(i).gt.0) then
     iwtype=nint(wtype(i))
     if(iwtype.ge.280.and.iwtype.le.289) then
      numsfcw=numsfcw+1
      if(wstaid(6,i).eq.dollar) then

!------------ if here, then pressure is std atm, so we can recover sta elev

       numdollar=numdollar+1
       pobsstd=exp(rpres(i))
       call w3fa03(pobsstd,welevc(i),dumtemp,dumtheta)
       elevmax=max(welevc(i),elevmax)
       elevmin=min(welevc(i),elevmin)
       rp0max=max(rpres(i),rp0max)
       rp0min=min(rpres(i),rp0min)

!----------- form column of heights, pressures for this location

       do k=1,lmetaex+1
        hthis(k)=0.
        do kk=1,lbig2ges
         hthis(k)=hthis(k)+wgtsh(i,kk)*hgges(iwgtsh(i,kk)+(k-1)*imeta*jmeta)
        end do
        pthis(k)=etaiex(k)*pdres01obs(i)+ptop
       end do

!----------- find interpolation interval

       do k=2,lmetaex+1
        kbelow=k
        if(welevc(i).gt.hthis(kbelow)) go to 120
       end do
120    continue
       kabove=kbelow-1
       kabovemax=max(kabove,kabovemax)
       kabovemin=min(kabove,kabovemin)
       pinterp=((hthis(kabove)-welevc(i))*pthis(kbelow) &
            -(hthis(kbelow)-welevc(i))*pthis(kabove)) &
            /(hthis(kabove)-hthis(kbelow))
       sfcpmax=max(pinterp,sfcpmax)
       sfcpmin=min(pinterp,sfcpmin)
       rpres(i)=log(pinterp)
      end if
     end if
    end if
   end do
  end if
  call mpi_reduce(numsfcw,numsfcwall,1,mpi_integer,mpi_sum,0,my_comm,ierror)
  call mpi_reduce(numdollar,numdollarall,1,mpi_integer,mpi_sum,0,my_comm,ierror)
  call mpi_reduce(sfcpmax,sfcpmaxall,1,mpi_real,mpi_max,0,my_comm,ierror)
  call mpi_reduce(sfcpmin,sfcpminall,1,mpi_real,mpi_min,0,my_comm,ierror)
  call mpi_reduce(elevmax,elevmaxall,1,mpi_real,mpi_max,0,my_comm,ierror)
  call mpi_reduce(elevmin,elevminall,1,mpi_real,mpi_min,0,my_comm,ierror)
  call mpi_reduce(kabovemax,kabovemaxall,1,mpi_integer,mpi_max,0,my_comm,ierror)
  call mpi_reduce(kabovemin,kaboveminall,1,mpi_integer,mpi_min,0,my_comm,ierror)
  call mpi_reduce(rp0max,rp0maxall,1,mpi_real,mpi_max,0,my_comm,ierror)
  call mpi_reduce(rp0min,rp0minall,1,mpi_real,mpi_min,0,my_comm,ierror)
  if(mype.eq.0) then
         print *,' in getwges, number of surface obs=',numsfcwall
         print *,' numdollar=',numdollarall
         print '('' interpolated max, min sfc obs pres='',2es14.5)',sfcpmaxall,sfcpminall
         print '('' max,min sfc obs elevations='',2es14.5)',elevmaxall,elevminall
         print *,' kabovemax,min=',kabovemaxall,kaboveminall
         print '('' rp0max,min='',2es14.5)',rp0maxall,rp0minall
  end if

!-------- for radar obs (type 227), get guess pressure from observed height

  numradar=0
  radarpmax=-huge(radarpmax)
  radarpmin=huge(radarpmin)
  radarelevmax=-huge(radarelevmax)
  radarelevmin=huge(radarelevmin)
  kabovemax=0
  kabovemin=1000
  if(ndata.gt.0) then
   do i=1,ndata
    if(iflagh(i).gt.0) then
     iwtype=nint(wtype(i))
     if(iwtype.ge.2270) then
      numradar=numradar+1

      radarelevmax=max(welevc(i),radarelevmax)
             if(welevc(i).gt.100000.) print *,' in getwges, mype,i,wtype,elev=',mype,i,wtype(i),welevc(i)
      radarelevmin=min(welevc(i),radarelevmin)

!----------- form column of heights, pressures for this location

      do k=1,lmetaex+1
       hthis(k)=0.
       do kk=1,lbig2ges
        hthis(k)=hthis(k)+wgtsh(i,kk)*hgges(iwgtsh(i,kk)+(k-1)*imeta*jmeta)
       end do
       pthis(k)=etaiex(k)*pdres01obs(i)+ptop
      end do

!----------- find interpolation interval

      do k=2,lmetaex+1
       kbelow=k
       if(welevc(i).gt.hthis(kbelow)) go to 130
      end do
130   continue
      kabove=kbelow-1
      kabovemax=max(kabove,kabovemax)
      kabovemin=min(kabove,kabovemin)
      pinterp=((hthis(kabove)-welevc(i))*pthis(kbelow) &
           -(hthis(kbelow)-welevc(i))*pthis(kabove)) &
           /(hthis(kabove)-hthis(kbelow))
      radarpmax=max(pinterp,radarpmax)
      radarpmin=min(pinterp,radarpmin)
      rpres(i)=log(pinterp)
     end if
    end if
   end do
  end if

  deallocate(pthis)
  deallocate(hthis)
  call mpi_reduce(numradar,numradarall,1,mpi_integer,mpi_sum,0,my_comm,ierror)
  call mpi_reduce(radarpmax,radarpmaxall,1,mpi_real,mpi_max,0,my_comm,ierror)
  call mpi_reduce(radarpmin,radarpminall,1,mpi_real,mpi_min,0,my_comm,ierror)
  call mpi_reduce(radarelevmax,radarelevmaxall,1,mpi_real,mpi_max,0,my_comm,ierror)
  call mpi_reduce(radarelevmin,radarelevminall,1,mpi_real,mpi_min,0,my_comm,ierror)
  call mpi_reduce(kabovemax,kabovemaxall,1,mpi_integer,mpi_max,0,my_comm,ierror)
  call mpi_reduce(kabovemin,kaboveminall,1,mpi_integer,mpi_min,0,my_comm,ierror)
  if(mype.eq.0) then
         print *,' in getwges, number of radar radial winds=',numradarall
         print '('' interpolated max, min radar obs pres='',2es14.5)',radarpmaxall,radarpminall
         print '('' max,min radar obs elevations='',2es14.5)',radarelevmaxall,radarelevminall
         print *,' radar kabovemax,min=',kabovemaxall,kaboveminall
  end if

!-------- now exclude observations whose pressure is too far above or below model domain

  if(ndata.gt.0) then
   allocate(rlpdobs(ndata))
   rlpdobs=log(peta1min-ptop)
   do i=1,ndata
    if(iflagh(i).gt.0) then
     pobs=exp(rpres(i))
     if(pobs.gt.peta1min.and.pobs.lt.petagmax) then
      rlpdobs(i)=log(abs(pobs-ptop))
     else
      iflagh(i)=0
     end if
    end if
   end do
   rletaobs=rletam(1)
   do i=1,ndata
    if(iflagh(i).gt.0) rletaobs(i)=rlpdobs(i)-log(pdres01obs(i))
   end do
   deallocate(rlpdobs)
   deallocate(pdres01obs)

   allocate(iflagv(ndata))
   call st_simpin3f(wgtsv,iwgtsv,iflagv,rlon,rlat,rletaobs,ndata,iordges,lhalfges,lbig3ges, &
                    wbglb,dlon,sbglb,dlat,rletam,imetaglb,jmetaglb,lmetaex,istagvglb)
   call st_glb2loc_iwgts(iwgtsv,ndata,lbig3ges,imetaglb,jmetaglb,imeta,jmeta)

!     for observations below the surface, do 2-d interpolation from lowest eta level

   do i=1,ndata
    if(iflagh(i).gt.0.and.iflagv(i).eq.0.and.exp(rpres(i)).gt.500.) then
     do k=1,lbig2ges
      wgtsv(i,k)=wgtsh(i,k)
      iwgtsv(i,k)=iwgtsh(i,k)+(lmetaex-1)*imeta*jmeta
     end do
     do k=lbig2ges+1,lbig3ges
      wgtsv(i,k)=0.
      iwgtsv(i,k)=iwgtsv(i,1)
     end do
     iflagv(i)=1
    end if
   end do

   do i=1,ndata
    if(iflagv(i).gt.0) then
     do kk=1,lbig3ges
      if(ugges(iwgtsv(i,kk)).gt.test_value.or. &
         vgges(iwgtsv(i,kk)).gt.test_value) iflagv(i)=0
     end do
    end if
   end do

   iflagh=iflagv*iflagh
   deallocate(iflagv)
  end if
  deallocate(rletam)

!-------- now do 3-d interpolation

  numabove=0
  numbelow=0
  numbelow200=0
  wgesmin=huge(wgesmin)
  wgesmax=-huge(wgesmax)
  werrmax=-huge(werrmax)
  werrmin=huge(werrmin)
  werrmax0=-huge(werrmax0)
  werrmin0=huge(werrmin0)
  delhbar8=0._8
  delhrms8=0._8
  icount=0
  if(ndata.gt.0) then
   wges=1.e20
   do i=1,ndata
    if(iflagh(i).gt.0) then
     hmodel=0.
     do kk=1,lbig2ges
      khthis=min(lmetaex+1,lmh(iwgtsh(i,kk))+1)
      hmodel=hmodel+wgtsh(i,kk)*hgges(iwgtsh(i,kk)+(khthis-1)*imeta*jmeta)
     end do
     delh=welevc(i)-hmodel
           if(delh.ge.-1000.) then
     if(delh.ge.0.) numabove=numabove+1
     if(delh.lt.0.) numbelow=numbelow+1
      icount=icount+1
     uges=0. ; vges=0.
     do kk=1,lbig3ges
      uges=uges+wgtsv(i,kk)*ugges(iwgtsv(i,kk))
      vges=vges+wgtsv(i,kk)*vgges(iwgtsv(i,kk))
     end do
     wges(i)=delta(i)*uges+epsilnw(i)*vges
     if(nint(wtype(i)).eq.2001) then
      print *,' GUESS VALUE AT SINGLE TEST POINT FOLLOWS:'
      print *,' GUESS VALUE AT SINGLE TEST POINT FOLLOWS:'
      print *,' GUESS VALUE AT SINGLE TEST POINT FOLLOWS:'
      print *,' GUESS VALUE AT SINGLE TEST POINT FOLLOWS:'
      print *,' one point obs test, wges set to zero'
      print *,' for one point with full wobs value, set one_type=2002'
      print *,'   and use following value of wges as reference for obs value:'
      print *,'      wges=',wges(i)
      wges(i)=0.
     end if

!----------- increase obs error as function of distance
!----------- of obs elevation below model surface elevation
!-------       (set to 100 percent change per 500m)
     werrmax0=max(werr(i),werrmax0)
     werrmin0=min(werr(i),werrmin0)
     werr(i)=werr(i)*max(1.,1.-delh/250.)
     if(delh.lt.-200.) numbelow200=numbelow200+1
     werrmax=max(werr(i),werrmax)
     werrmin=min(werr(i),werrmin)
     delhbar8=delhbar8+1._8*delh
     delhrms8=delhrms8+(1._8*delh)**2
           end if
    end if
   end do
   deallocate(wgtsh) ; deallocate(iwgtsh)
   deallocate(welevc)
   do i=1,ndata
    if(wges(i).lt.1.e19) then
     wgesmax=max(wges(i),wgesmax)
     wgesmin=min(wges(i),wgesmin)
    end if
   end do
  end if
  call mpi_barrier(my_comm,ierror)
  call mpi_reduce(delhbar8,delhbarall8,1,mpi_real8,mpi_sum,0,my_comm,ierror)
  call mpi_reduce(delhrms8,delhrmsall8,1,mpi_real8,mpi_sum,0,my_comm,ierror)
  call mpi_reduce(icount,icountall,1,mpi_integer,mpi_sum,0,my_comm,ierror)
  call mpi_reduce(wgesmax,wgesmaxall,1,mpi_real,mpi_max,0,my_comm,ierror)
  call mpi_reduce(wgesmin,wgesminall,1,mpi_real,mpi_min,0,my_comm,ierror)
  call mpi_reduce(numabove,numaboveall,1,mpi_integer,mpi_sum,0,my_comm,ierror)
  call mpi_reduce(numbelow,numbelowall,1,mpi_integer,mpi_sum,0,my_comm,ierror)
  call mpi_reduce(numbelow200,numbelow200all,1,mpi_integer,mpi_sum,0,my_comm,ierror)
  call mpi_reduce(werrmax0,werrmax0all,1,mpi_real,mpi_max,0,my_comm,ierror)
  call mpi_reduce(werrmin0,werrmin0all,1,mpi_real,mpi_min,0,my_comm,ierror)
  call mpi_reduce(werrmax,werrmaxall,1,mpi_real,mpi_max,0,my_comm,ierror)
  call mpi_reduce(werrmin,werrminall,1,mpi_real,mpi_min,0,my_comm,ierror)
  if(mype.eq.0) then
   delhbarall8=delhbarall8/max(1,icountall)
   delhrmsall8=sqrt(delhrmsall8/max(1,icountall))
      print *,' in getwges, icount=',icountall
      print '('' in getwges, wgesmax,min='',2es14.5)',wgesmaxall,wgesminall
      print '('' in getwges, delhbar,delhrms='',2es14.5)',delhbarall8,delhrmsall8
      print *,' in getwges, numabove,below=',numaboveall,numbelowall
      print *,' in getwges, numbelow200=',numbelow200all
      print '('' in getwges, uncorrected werrmax,min='',2es14.5)',werrmax0all,werrmin0all
      print '('' in getwges, corrected werrmax,min='',2es14.5)',werrmaxall,werrminall
  end if

!--------  assign ssm/i wind direction as guess direction

  numssmi=0
  icount=0
  isingle=0
  ibad=0
  if(ndata.gt.1) then 
   iflagh=0
   do i=1,ndata-1
    if(nint(wtype(i)).eq.283.and.iflagh(i).eq.0) then
     iflagh(i)=1
     ipair=1
     do j=i+1,ndata
      if(nint(wtype(j)).eq.283.and.iflagh(j).eq.0) then
       staida8(:)=wstaid(:,i)
       staidb8(:)=wstaid(:,j)
       errlon=abs(rlon(i)-rlon(j))
       errlat=abs(rlat(i)-rlat(j))
       if(errlon.lt..005.and.errlat.lt..005.and.staida.eq.staidb) then
        iflagh(j)=1
        ipair=0
        numssmi=numssmi+1
        if(wges(i).lt.1.e19.and.wges(j).lt.1.e19) then
         icount=icount+1
         wobs0=wobs(i)
         wobs1=wobs(j)
         wges0=wges(i)
         wges1=wges(j)
!        if(icount.le.300) print *,' ssmi wobs0,wobs1=',wobs0,wobs1
         obsspd2=wobs0**2+wobs1**2
         gesspd2=wges0**2+wges1**2
         ratio2=obsspd2/max(.01,gesspd2)
         if(ratio2.gt..01.and.ratio2.lt.100.) then
          ratio=sqrt(ratio2)
          wobs(i)=wges(i)*ratio
          wobs(j)=wges(j)*ratio
         else
          ibad=ibad+1
          wges(i)=1.e20
          wges(j)=1.e20
         end if
        else
         wges(i)=1.e20
         wges(j)=1.e20
        end if
       end if
      end if
      if(ipair.eq.0) exit
     end do
     isingle=isingle+ipair
    end if
   end do
   deallocate(iflagh)
  end if
  call mpi_barrier(my_comm,ierror)
  call mpi_reduce(numssmi,numssmiall,1,mpi_integer,mpi_sum,0,my_comm,ierror)
  call mpi_reduce(icount,icountall,1,mpi_integer,mpi_sum,0,my_comm,ierror)
  call mpi_reduce(ibad,ibadall,1,mpi_integer,mpi_sum,0,my_comm,ierror)
  call mpi_reduce(isingle,isingleall,1,mpi_integer,mpi_sum,0,my_comm,ierror)
            
  if(mype.eq.0) then
   print *,' process ssm/i winds, obs direction is ges direction'
   print *,' numssmi=',numssmiall
   print *,' number of paired up ssmi obs=',icountall
   print *,' number of ssmi obs that fail gross check=',ibadall
   print *,' number of ssmi obs not in pairs=',isingleall
  end if

return
end subroutine getwges
