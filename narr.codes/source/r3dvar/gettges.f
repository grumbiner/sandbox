subroutine gettges(tges,qges,rlon,rlat,rpres,telev,tstaid,ndata,iordges,lbig2ges,lbig3ges,lhalfges, &
             rletaobs,wgts3,iwgts3, &
             imeta,jmeta,lmetaex,pdres01,tgges,qgges, &
             lmh,etamex,ptop,ttype,hgges,etai,terr,tobs, &
             wbglb,dlon,sbglb,dlat,istaghglb,imetaglb,jmetaglb)

!-------- obtain guess 3-d field at observed x,y,p for 
!--------    a group of temperatures


!-------- do conversion from elevation to pressure for sfc obs
!--------   (convert to guess pressure of station elevation for obs
!--------     with missing surface pressure)
!--------  make error proportional to local uncertainty of surface
!--------  elevation (presumably large when difference between obs
!--------  and model terrain is large and/or difference between
!--------  height of adajacent local steps is large)

!--   tges:   output guess interpolated to obs locations
!--   rlon,rlat:  grid lon, lat of obs location (degrees)
!--   rpres:  log pressure (mb) for each obs
!--   ndata:  number of obs
!--   nx,ny,np:  dimensions of guess grid
!--   rx,ry:     horizontal grid coordinates
!--   rlogpges:  pressure of each point on guess grid
!--   tgges:  guess temperature

!--------   tges set to 1.e20 for every
!-------    location which falls outside the guess domain (this includes
!-------               points below model terrain)

  include 'mpif.h'
      include "my_comm.h"

  real(4) tges(ndata),rlon(ndata),rlat(ndata),rpres(ndata)
  real(4) telev(ndata)
  character*1 tstaid(8,ndata)
  real(4) qges(ndata),qgges(imeta*jmeta*lmetaex)
  real(4) tgges(imeta*jmeta*lmetaex),pdres01(imeta*jmeta)
  real(4) hgges(imeta*jmeta*(lmetaex+1)),etai(lmetaex+1)
  real(4) terr(ndata)
  integer(4) lmh(imeta*jmeta)
  real(4) etamex(lmetaex),ttype(ndata),tobs(ndata)
  real(4) wgts3(max(1,ndata),lbig3ges)
  integer(4) iwgts3(max(1,ndata),lbig3ges)
  real(4) rletaobs(max(1,ndata))

  character*1 dollar
  data dollar/'$'/
  real(4) rletam(lmetaex)
  real(4),allocatable::rlpdobs(:)
  real(4),allocatable::pdres01obs(:),televc(:),wgts(:,:)
  integer(4),allocatable::iwgts(:,:),iflag(:),iflag3(:)
  real(4) hthis(lmetaex+1),pthis(lmetaex+1)
  real(8) delhbar8,delhrms8
  real(8) delhbarall8,delhrmsall8

  special_value=1.e20
  test_value=.98*special_value
  call mpi_comm_rank(my_comm,mype,ierr)

!-------- obtain pressure elevation to use in place of missing elevations
!--------    (used to adjust error)

  iprint=0
  msgelev=0
  if(ndata.gt.0) then
   allocate(televc(ndata))
   televc=telev
   do i=1,ndata
    if(telev(i).gt.1.e6) then
     msgelev=msgelev+1
     pobs0=exp(rpres(i))
     call w3fa03(pobs0,televc(i),dumt,dumtheta)
     iprint=iprint+1
!    if(iprint.le.50) print *,' in gettges, ttype,pobs,preselevobs=',ttype(i),pobs0,televc(i)
    end if
   end do
  end if
  call mpi_reduce(msgelev,msgelevall,1,mpi_integer,mpi_sum,0,my_comm,ierror)
  if(mype.eq.0) print *,' in gettges, number obs with missing elevation=',msgelevall

  rletam=log(etamex)
!-------- find minimum guess pressure

  call getmaxmin2(peta1min,petagmax,pdres01,imeta,jmeta)
  peta1min=etamex(1)*peta1min+ptop
  petagmax=etamex(lmetaex)*petagmax+ptop
  if(mype.eq.0) then
   print '('' in gettges, peta1min='',es14.5)',peta1min
   print '('' in gettges, petagmax='',es14.5)',petagmax
  end if

!-------- add 125mb to max guess pressure, since we allow data
!--------  up to 125mb below ground

         petagmax=petagmax+125.

!-------- get horizontal interpolation weights, addresses

  if(ndata.gt.0) then
   allocate(wgts(ndata,lbig2ges)) ; allocate(iwgts(ndata,lbig2ges))
   allocate(iflag(ndata))

   call st_simpin2f(wgts,iwgts,iflag,rlon,rlat,ndata,iordges,lhalfges,lbig2ges, &
                   wbglb,dlon,sbglb,dlat,imetaglb,jmetaglb,istaghglb)
   call st_glb2loc_iwgts(iwgts,ndata,lbig2ges,imetaglb,jmetaglb,imeta,jmeta)
   do i=1,ndata
    if(iflag(i).gt.0) then
     do kk=1,lbig2ges
      khthis=min(lmetaex+1,lmh(iwgts(i,kk))+1)
      ihggesthis=iwgts(i,kk)+(khthis-1)*imeta*jmeta
      if(hgges(ihggesthis).gt.test_value) iflag(i)=0
     end do
    end if
   end do
!-------- interpolate pressure variable to obs points so we
!-------- can convert vertical obs coordinate to eta grid units

   allocate(pdres01obs(ndata))
   pdres01obs=petagmax-ptop
   do i=1,ndata
    if(iflag(i).gt.0) then
     pdres01obs(i)=0.
     do kk=1,lbig2ges
      pdres01obs(i)=pdres01obs(i)+wgts(i,kk)*pdres01(iwgts(i,kk))
     end do
    end if
   end do
  end if

!-------- for surface obs, replace obs pressure with guess pressure
!--------  at obs elevation (only if pressure was missing--indicated
!--------                      by $-sign in 6-th character of staid)

  numsfct=0
  sfcpmax=-huge(sfcpmax)
  sfcpmin=huge(sfcpmax)
  elevmax=-huge(elevmax)
  elevmin=huge(elevmin)
  rp0max=-huge(rp0max)
  rp0min=huge(rp0min)
  kabovemax=0
  kabovemin=1000
  numdollar=0
  errmax=0.
  limprint=0
  if(ndata.gt.0) then
   do i=1,ndata
    if(iflag(i).gt.0) then
     ittype=nint(ttype(i))
     if(ittype.ge.180.and.ittype.le.189) then
      numsfct=numsfct+1
      if(tstaid(6,i).eq.dollar) then

!------------ if here, then pressure is std atm, so we can recover sta elev
!------------  then compare with given sta elev so we can decide if
!------------  this is really right pressure

       numdollar=numdollar+1
       pobsstd=exp(rpres(i))
       call w3fa03(pobsstd,televchk,dumtemp,dumtheta)
              televc(i)=televchk
       errmax=max(abs(televchk-telev(i)),errmax)
!      elevmax=max(telev(i),elevmax)
!      elevmin=min(telev(i),elevmin)
       elevmax=max(televc(i),elevmax)
       elevmin=min(televc(i),elevmin)
       rp0max=max(rpres(i),rp0max)
       rp0min=min(rpres(i),rp0min)

!---------- form column of heights, pressures for this location

       do k=1,lmetaex+1
        hthis(k)=0.
        do kk=1,lbig2ges
         hthis(k)=hthis(k)+wgts(i,kk)*hgges(iwgts(i,kk)+(k-1)*imeta*jmeta)
        end do
        pthis(k)=etai(k)*pdres01obs(i)+ptop
       end do

!----------- find interpolation interval

       do k=2,lmetaex+1
        kbelow=k
        if(televc(i).gt.hthis(kbelow)) go to 120
!       if(telev(i).gt.hthis(kbelow)) go to 120
       end do
120    continue
       kabove=kbelow-1
            kabovemax=max(kabove,kabovemax)
            kabovemin=min(kabove,kabovemin)
       pold=exp(rpres(i))
       pinterp=((hthis(kabove)-televc(i))*pthis(kbelow) &
                  -(hthis(kbelow)-televc(i))*pthis(kabove)) &
                  /(hthis(kabove)-hthis(kbelow))
       sfcpmax=max(pinterp,sfcpmax)
       sfcpmin=min(pinterp,sfcpmin)
       rpres(i)=log(pinterp)
       pdiff=pinterp-pold
       limprint=limprint+1
!      if(limprint.le.100) write(6,125)kabove,televchk,telev(i),pold,pinterp,pdiff
!25    format(' kabove,telev1,2,pold,pinterp,pdiff=',i3,5f7.1)
      end if
     end if
    end if
   end do
  end if
  call mpi_reduce(numsfct,numsfctall,1,mpi_integer,mpi_sum,0,my_comm,ierror)
  call mpi_reduce(numdollar,numdollarall,1,mpi_integer,mpi_sum,0,my_comm,ierror)
  call mpi_reduce(sfcpmax,sfcpmaxall,1,mpi_real,mpi_max,0,my_comm,ierror)
  call mpi_reduce(sfcpmin,sfcpminall,1,mpi_real,mpi_min,0,my_comm,ierror)
  call mpi_reduce(errmax,errmaxall,1,mpi_real,mpi_max,0,my_comm,ierror)
  call mpi_reduce(elevmax,elevmaxall,1,mpi_real,mpi_max,0,my_comm,ierror)
  call mpi_reduce(elevmin,elevminall,1,mpi_real,mpi_min,0,my_comm,ierror)
  call mpi_reduce(kabovemax,kabovemaxall,1,mpi_integer,mpi_max,0,my_comm,ierror)
  call mpi_reduce(kabovemin,kaboveminall,1,mpi_integer,mpi_min,0,my_comm,ierror)
  call mpi_reduce(rp0max,rp0maxall,1,mpi_real,mpi_max,0,my_comm,ierror)
  call mpi_reduce(rp0min,rp0minall,1,mpi_real,mpi_min,0,my_comm,ierror)
  if(mype.eq.0) then
   print *,' in gettges, number of surface obs=',numsfctall
   print *,' numdollar=',numdollarall
   print '('' interpolated max, min sfc obs pres='',2es14.5)',sfcpmaxall,sfcpminall
   print '('' errmax='',es14.5)',errmaxall
   print '('' max,min sfc obs elevations='',2es14.5)',elevmaxall,elevminall
   print *,' kabovemax,min=',kabovemaxall,kaboveminall
   print '('' rp0max,min='',2es14.5)',rp0maxall,rp0minall
  end if

!-------- now exclude observations whose pressure is too far above or below model domain

  if(ndata.gt.0) then
   allocate(rlpdobs(ndata))
   rlpdobs=log(peta1min-ptop)
   do i=1,ndata
    if(iflag(i).gt.0) then
     pobs=exp(rpres(i))
     if(pobs.gt.peta1min.and.pobs.lt.petagmax) then
      rlpdobs(i)=log(pobs-ptop)
     else
      iflag(i)=0
     end if
    end if
   end do
   rletaobs=rletam(1)
   do i=1,ndata
    if(iflag(i).gt.0) rletaobs(i)=rlpdobs(i)-log(pdres01obs(i))
   end do
   deallocate(rlpdobs) ; deallocate(pdres01obs)

   allocate(iflag3(ndata))
   call st_simpin3f(wgts3,iwgts3,iflag3,rlon,rlat,rletaobs,ndata,iordges,lhalfges,lbig3ges, &
                   wbglb,dlon,sbglb,dlat,rletam,imetaglb,jmetaglb,lmetaex,istaghglb)
   call st_glb2loc_iwgts(iwgts3,ndata,lbig3ges,imetaglb,jmetaglb,imeta,jmeta)

!     for observations below the surface, do 2-d interpolation from lowest eta level

   do i=1,ndata
    if(iflag(i).gt.0.and.iflag3(i).eq.0.and.exp(rpres(i)).gt.500.) then
     do k=1,lbig2ges
      wgts3(i,k)=wgts(i,k)
      iwgts3(i,k)=iwgts(i,k)+(lmetaex-1)*imeta*jmeta
     end do
     do k=lbig2ges+1,lbig3ges
      wgts3(i,k)=0.
      iwgts3(i,k)=iwgts3(i,1)
     end do
     iflag3(i)=1
    end if
   end do

   iflag=iflag3*iflag
   deallocate(iflag3)
  end if

!-------- now do 3-d interpolation

  icount=0
  numabove=0
  numbelow=0
  numbelow200=0
  terrmax=-huge(terrmax)
  terrmin=huge(terrmax)
  terrmax0=-huge(terrmax0)
  terrmin0=huge(terrmin0)
  tgesmax=-huge(tgesmax)
  tgesmin=huge(tgesmax)
  qgesmax=-huge(tgesmax)
  qgesmin=huge(tgesmax)
  delhbar8=0._8
  delhrms8=0._8
  lim180=0
  if(ndata.gt.0) then
   tges=1.e20
   qges=1.e20
   do i=1,ndata
    if(iflag(i).gt.0) then
     hmodel=0.
     do kk=1,lbig2ges
      khthis=min(lmetaex+1,lmh(iwgts(i,kk))+1)
      hmodel=hmodel+wgts(i,kk)*hgges(iwgts(i,kk)+(khthis-1)*imeta*jmeta)
     end do
     delh=televc(i)-hmodel
             if(delh.ge.-1000.) then
     if(delh.ge.0.) numabove=numabove+1
     if(delh.lt.0.) numbelow=numbelow+1
     icount=icount+1
     tges(i)=0. ; qges(i)=0.
     do kk=1,lbig3ges
      tges(i)=tges(i)+wgts3(i,kk)*tgges(iwgts3(i,kk))
      qges(i)=qges(i)+wgts3(i,kk)*qgges(iwgts3(i,kk))
     end do
     if(nint(ttype(i)).eq.1003) then
      print *,' GUESS VALUE AT SINGLE TEST POINT FOLLOWS:'
      print *,' GUESS VALUE AT SINGLE TEST POINT FOLLOWS:'
      print *,' GUESS VALUE AT SINGLE TEST POINT FOLLOWS:'
      print *,' GUESS VALUE AT SINGLE TEST POINT FOLLOWS:'
      print *,' one point obs test, tges set to zero'
      print *,' for one point with full tobs value, set one_type=1004'
      print *,'   and use following value of tges as reference for obs value:'
      print *,'      tges=',tges(i)
      tges(i)=0.
     end if
     tgesmax=max(tges(i),tgesmax)
     tgesmin=min(tges(i),tgesmin)
     qgesmax=max(qges(i),qgesmax)
     qgesmin=min(qges(i),qgesmin)

!----------- increase obs error as function of distance
!----------- of obs elevation below model surface elevation
!-------       (set to 100 percent change per 500m)
     terrmax0=max(terr(i),terrmax0)
     terrmin0=min(terr(i),terrmin0)
     terr(i)=terr(i)*max(1.,1.-delh/250.)
     if(delh.lt.-200.) numbelow200=numbelow200+1
     terrmax=max(terr(i),terrmax)
     terrmin=min(terr(i),terrmin)
     delhbar8=delhbar8+1._8*delh
     delhrms8=delhrms8+(1._8*delh)**2
            end if
    end if
   end do
   deallocate(televc)
   deallocate(wgts) ; deallocate(iwgts)
   deallocate(iflag)
  end if
  call mpi_reduce(delhbar8,delhbarall8,1,mpi_real8,mpi_sum,0,my_comm,ierror)
  call mpi_reduce(delhrms8,delhrmsall8,1,mpi_real8,mpi_sum,0,my_comm,ierror)
  call mpi_reduce(icount,icountall,1,mpi_integer,mpi_sum,0,my_comm,ierror)
  call mpi_reduce(tgesmax,tgesmaxall,1,mpi_real,mpi_max,0,my_comm,ierror)
  call mpi_reduce(tgesmin,tgesminall,1,mpi_real,mpi_min,0,my_comm,ierror)
  call mpi_reduce(qgesmax,qgesmaxall,1,mpi_real,mpi_max,0,my_comm,ierror)
  call mpi_reduce(qgesmin,qgesminall,1,mpi_real,mpi_min,0,my_comm,ierror)
  call mpi_reduce(numabove,numaboveall,1,mpi_integer,mpi_sum,0,my_comm,ierror)
  call mpi_reduce(numbelow,numbelowall,1,mpi_integer,mpi_sum,0,my_comm,ierror)
  call mpi_reduce(numbelow200,numbelow200all,1,mpi_integer,mpi_sum,0,my_comm,ierror)
  call mpi_reduce(terrmax0,terrmax0all,1,mpi_real,mpi_max,0,my_comm,ierror)
  call mpi_reduce(terrmin0,terrmin0all,1,mpi_real,mpi_min,0,my_comm,ierror)
  call mpi_reduce(terrmax,terrmaxall,1,mpi_real,mpi_max,0,my_comm,ierror)
  call mpi_reduce(terrmin,terrminall,1,mpi_real,mpi_min,0,my_comm,ierror)
  if(mype.eq.0) then
      print *,' icount=',icountall
      print '('' tgesmax,min='',2es14.5)',tgesmaxall,tgesminall
      print '('' qgesmax,min='',2es14.5)',qgesmaxall,qgesminall
      delhbarall8=delhbarall8/max(1,icountall)
      delhrmsall8=sqrt(delhrmsall8/max(1,icountall))
      print '('' in gettges, delhbar,delhrms='',2es14.5)',delhbarall8,delhrmsall8
      print *,' in gettges, numabove,below=',numaboveall,numbelowall
      print *,' in gettges, numbelow200=',numbelow200all
      print '('' in gettges, uncorrected terrmax,min='',2es14.5)',terrmax0all,terrmin0all
      print '('' in gettges, corrected terrmax,min='',2es14.5)',terrmaxall,terrminall
  end if

return
end subroutine gettges
