subroutine getqges(qges,qsatges,rlon,rlat,rpres,qelev,qstaid,qtobs,ndata, &
       iordges,lbig2ges,lbig3ges,lhalfges, &
       rletaobs,wgts3,iwgts3, &
       imeta,jmeta,lmetaex,pdres01,tgges,qgges, &
       lmh,etamex,ptop,qtype,hgges,etaiex,qerr,qobs, &
       wbglb,dlon,sbglb,dlat,istaghglb,imetaglb,jmetaglb)

!-------- obtain guess 3-d field at observed x,y,p for 
!--------    a group of moisture values--also get sat humidity at obs points

!-------- do conversion from elevation to pressure for sfc obs
!--------   (convert to guess pressure of station elevation for obs
!--------     with missing surface pressure)
!--------  make error proportional to local uncertainty of surface
!--------  elevation (presumably large when difference between obs
!--------  and model terrain is large and/or difference between
!--------  height of adajacent local steps is large)

!--   qges:   output guess interpolated to obs locations
!--   rlon,rlat:  grid lon, lat of obs location (degrees)
!--   rpres:  log pressure (mb) for each obs
!--   ndata:  number of obs
!--   nx,ny,np:  dimensions of guess grid
!--   rx,ry:     horizontal grid coordinates
!--   rlogpges:  pressure of each point on guess grid
!--   tgges:  guess temperature
!--   qgges:  guess moisture

!--------   qges set to 1.e20 for every
!-------    location which falls outside the guess domain (this includes
!-------               points below model terrain)

  include 'mpif.h'
      include "my_comm.h"

  real(4) qges(max(1,ndata)),rlon(max(1,ndata)),rlat(max(1,ndata))
  real(4) rpres(max(1,ndata)),qsatges(max(1,ndata)),qtype(max(1,ndata))
  character*1 qstaid(8,max(1,ndata))
  real(4) qtobs(max(1,ndata)),qelev(max(1,ndata))
  real(4) tgges(imeta*jmeta*lmetaex),pdres01(imeta*jmeta)
  real(4) qgges(imeta*jmeta*lmetaex),hgges(imeta*jmeta*(lmetaex+1))
  integer(4) lmh(imeta*jmeta)
  real(4) etamex(lmetaex),etaiex(lmetaex+1),qobs(max(1,ndata)),qerr(max(1,ndata))
  real(4) wgts3(max(1,ndata),lbig3ges)
  integer(4) iwgts3(max(1,ndata),lbig3ges)
  real(4) rletaobs(max(1,ndata))

  character*1 dollar
  data dollar/'$'/
  real(4) rletam(lmetaex)
  real(4),allocatable::tges(:)
  real(4),allocatable::rlpdobs(:)
  real(4),allocatable::pdres01obs(:)
  real(4),allocatable::pobs0(:),pobs(:),qsatobs0(:)
  real(4),allocatable::qsatobs(:),qelevc(:),wgts(:,:)
  integer(4),allocatable::iwgts(:,:),iflag(:),iflag3(:)
  real(4) hthis(lmetaex+1),pthis(lmetaex+1)

  special_value=1.e20
  test_value=.98*special_value
  call mpi_comm_rank(my_comm,mype,ierr)

!-------- obtain pressure elevation to use in place of missing elevations
!--------    (used to adjust error)

  iprint=0
  msgelev=0
  if(ndata.gt.0) then
   allocate(qelevc(max(1,ndata)))
   qelevc=qelev
   do i=1,ndata
    if(qelev(i).gt.1.e6) then
     msgelev=msgelev+1
     pobs1=exp(rpres(i))
     call w3fa03(pobs1,qelevc(i),dumt,dumtheta)
     iprint=iprint+1
!    if(iprint.le.50) print *,' in getqges, qtype,pobs,preselevobs=',qtype(i),pobs1,qelevc(i)
    end if
   end do
  end if
  call mpi_barrier(my_comm,ierror)
  call mpi_reduce(msgelev,msgelevall,1,mpi_integer,mpi_sum,0,my_comm,ierror)
  if(mype.eq.0) print *,' in getqges, number obs with missing elevation=',msgelevall

  rletam=log(etamex)

!-------- find minimum guess pressure

  call getmaxmin2(peta1min,petagmax,pdres01,imeta,jmeta)
  peta1min=etamex(1)*peta1min+ptop
  petagmax=etamex(lmetaex)*petagmax+ptop
  if(mype.eq.0) then
   print '('' in getqges, peta1min='',es14.5)',peta1min
   print '('' in getqges, petag,max='',es14.5)',petagmax
  end if

!-------- add 125mb to max guess pressure, since we allow data
!--------  up to 125mb below ground

         petagmax=petagmax+125.

!-------- get horizontal interpolation weights, addresses

  if(ndata.gt.0) then
   allocate(wgts(max(1,ndata),lbig2ges)) ; allocate(iwgts(max(1,ndata),lbig2ges))
   allocate(iflag(max(1,ndata)))

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

   allocate(pdres01obs(max(1,ndata)))
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
!--------  at obs elevation.

  numsfcq=0
  sfcpmax=-huge(sfcpmax)
  sfcpmin=huge(sfcpmax)
  if(ndata.gt.0) then
   allocate(pobs0(max(1,ndata)))
   pobs0=exp(rpres)
   do i=1,ndata
    if(iflag(i).gt.0) then
     iqtype=nint(qtype(i))
     if(iqtype.ge.180.and.iqtype.le.189) then
      numsfcq=numsfcq+1

!---------- form column of heights, pressures for this location

      do k=1,lmetaex+1
       hthis(k)=0.
       do kk=1,lbig2ges
        hthis(k)=hthis(k)+wgts(i,kk)*hgges(iwgts(i,kk)+(k-1)*imeta*jmeta)
       end do
       pthis(k)=etaiex(k)*pdres01obs(i)+ptop
      end do

!----------- find interpolation interval

      do k=2,lmetaex+1
       kbelow=k
       if(qelevc(i).gt.hthis(kbelow)) go to 120
      end do
120   continue
      kabove=kbelow-1
      pold=exp(rpres(i))
      pinterp=((hthis(kabove)-qelevc(i))*pthis(kbelow) &
           -(hthis(kbelow)-qelevc(i))*pthis(kabove)) &
           /(hthis(kabove)-hthis(kbelow))
      sfcpmax=max(pinterp,sfcpmax)
      sfcpmin=min(pinterp,sfcpmin)
      rpres(i)=log(pinterp)
      pdiff=pinterp-pold
!     write(6,125)kabove,qelev(i),pold,pinterp,pdiff
!25   format(' kabove,qelev,pold,pinterp,pdiff=',i3,4f7.1)
     end if
    end if
   end do
  end if
  call mpi_barrier(my_comm,ierror)
  call mpi_reduce(numsfcq,numsfcqall,1,mpi_integer,mpi_sum,0,my_comm,ierror)
  call mpi_reduce(sfcpmax,sfcpmaxall,1,mpi_real,mpi_max,0,my_comm,ierror)
  call mpi_reduce(sfcpmin,sfcpminall,1,mpi_real,mpi_min,0,my_comm,ierror)
  if(mype.eq.0) then
   print *,' in getqges, number of surface obs=',numsfcqall
   print '('' interpolated max, min sfc obs pres='',2es14.5)',sfcpmaxall,sfcpminall
  end if

!-------- now exclude observations whose pressure is too far above or below model domain

  msgsfp=0
  changemax=0.
  if(ndata.gt.0) then
   allocate(rlpdobs(max(1,ndata)))
   rlpdobs=log(peta1min-ptop)
   allocate(pobs(max(1,ndata)))
   do i=1,ndata
    if(iflag(i).gt.0) then
     pobs(i)=exp(rpres(i))
     if(pobs(i).gt.peta1min.and.pobs(i).lt.petagmax) then
      rlpdobs(i)=log(pobs(i)-ptop)
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
   allocate(iflag3(max(1,ndata)))

   call st_simpin3f(wgts3,iwgts3,iflag3,rlon,rlat,rletaobs,ndata,iordges,lhalfges,lbig3ges, &
                   wbglb,dlon,sbglb,dlat,rletam,imetaglb,jmetaglb,lmetaex,istaghglb)
   call st_glb2loc_iwgts(iwgts3,ndata,lbig3ges,imetaglb,jmetaglb,imeta,jmeta)

!     for observations below the surface, do 2-d interpolation from lowest eta level

   do i=1,ndata
    if(iflag(i).gt.0.and.iflag3(i).eq.0.and.pobs(i).gt.500.) then
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

!-------- look for observations where specific humidity was 
!-------- created from relative humidity using std atm pressure
!-------- and observed temperature (sfc obs with missing station pres)

   allocate(qsatobs0(max(1,ndata)))
   call genqsat(qtobs,qsatobs0,ndata,pobs0)
   allocate(qsatobs(max(1,ndata)))
   call genqsat(qtobs,qsatobs,ndata,pobs)
!     print *,' qnew qobs diff qsatobs qsatobs0 pobs pobs0 diff'
   do i=1,ndata
    if(iflag(i).gt.0) then
     iqtype=nint(qtype(i))
     if(iqtype.ge.180.and.iqtype.le.189.and.qstaid(6,i).eq.dollar) then
      msgsfp=msgsfp+1
       rh=qobs(i)/qsatobs0(i)
      qnew=rh*qsatobs(i)
      changemax=max(abs(qnew-qobs(i)/qsatobs(i)),changemax)
      diff=qnew-qobs(i)
      diffp=pobs(i)-pobs0(i)
!     write(6,135)qnew,qobs(i),diff,qsatobs(i),qsatobs0(i),pobs(i),pobs0(i),diffp
!35   format(1h ,5f9.5,3f9.1)
      qobs(i)=qnew
     end if
    end if
   end do
   deallocate(qsatobs) ; deallocate(qsatobs0)
   deallocate(pobs0)
   changemax=100.*changemax
  end if
  call mpi_barrier(my_comm,ierror)
  call mpi_reduce(msgsfp,msgsfpall,1,mpi_integer,mpi_sum,0,my_comm,ierror)
  call mpi_reduce(changemax,changemaxall,1,mpi_real,mpi_max,0,my_comm,ierror)
  if(mype.eq.0) then
   print *,' number of sfc moisture obs with missing sfp=',msgsfpall
   print *,' max change (percent of satq) in moisture, going from'
   print '(''  stdatm pres to guess pres='',es14.5)',changemaxall
  end if

!-------- now do 3-d interpolation

  icount=0
  numabove=0
  numbelow=0
  numbelow200=0
  qerrmax=-huge(qerrmax)
  qerrmin=huge(qerrmin)
  qerrmax0=-huge(qerrmax0)
  qerrmin0=huge(qerrmin0)
  tgesmax=-huge(tgesmax)
  tgesmin=huge(tgesmax)
  qgesmax=-huge(tgesmax)
  qgesmin=huge(tgesmax)
  delhbar=0.
  delhrms=0.
  delhmax=-huge(delhmax)
  delhmin=huge(delhmin)
  if(ndata.gt.0) then
   allocate(tges(max(1,ndata)))
   tges=1.e20
   qges=1.e20
   do i=1,ndata
    if(iflag(i).gt.0) then
     hmodel=0.
     do kk=1,lbig2ges
      khthis=min(lmetaex+1,lmh(iwgts(i,kk))+1)
      hmodel=hmodel+wgts(i,kk)*hgges(iwgts(i,kk)+(khthis-1)*imeta*jmeta)
     end do
     delh=qelevc(i)-hmodel
             if(delh.ge.-1000.) then
     if(delh.ge.0.) numabove=numabove+1
     if(delh.lt.0.) numbelow=numbelow+1
     icount=icount+1
     tges(i)=0. ; qges(i)=0.
     do kk=1,lbig3ges
      tges(i)=tges(i)+wgts3(i,kk)*tgges(iwgts3(i,kk))
      qges(i)=qges(i)+wgts3(i,kk)*qgges(iwgts3(i,kk))
     end do
     if(nint(qtype(i)).eq.1005) then
      print *,' GUESS VALUE AT SINGLE TEST POINT FOLLOWS:'
      print *,' GUESS VALUE AT SINGLE TEST POINT FOLLOWS:'
      print *,' GUESS VALUE AT SINGLE TEST POINT FOLLOWS:'
      print *,' GUESS VALUE AT SINGLE TEST POINT FOLLOWS:'
      print *,' one point obs test, qges set to zero'
      print *,' for one point with full qobs value, set one_type=1005'
      print *,'   and use following value of qges as reference for obs value:'
      print *,'      qges=',qges(i)
      qges(i)=0.
     end if
     qgesmax=max(qges(i),qgesmax)
     qgesmin=min(qges(i),qgesmin)
     tgesmax=max(tges(i),tgesmax)
     tgesmin=min(tges(i),tgesmin)

!----------- increase obs error as function of distance
!----------- of obs elevation below model surface elevation
!-------       (set to 100 percent change per 500m)
     qerrmax0=max(qerr(i),qerrmax0)
     qerrmin0=min(qerr(i),qerrmin0)
     qerr(i)=qerr(i)*max(1.,1.-delh/250.)
     if(delh.lt.-200.) numbelow200=numbelow200+1
     qerrmax=max(qerr(i),qerrmax)
     qerrmin=min(qerr(i),qerrmin)
     delhbar=delhbar+delh
     delhrms=delhrms+delh**2
     delhmax=max(delh,delhmax)
     delhmin=min(delh,delhmin)
            end if
    end if
   end do
   deallocate(qelevc)
   deallocate(wgts) ; deallocate(iwgts)
   deallocate(iflag)
  end if
  qggesmax=maxval(qgges,qgges.lt.test_value)
  qggesmin=minval(qgges)
  numnegqges=0
  if(ndata.gt.0) then
           do i=1,ndata
            if(qges(i).lt.0.) then
             numnegqges=numnegqges+1
            end if
           end do
  end if
  call mpi_barrier(my_comm,ierror)
  call mpi_reduce(delhbar,delhbarall,1,mpi_real,mpi_sum,0,my_comm,ierror)
  call mpi_reduce(delhrms,delhrmsall,1,mpi_real,mpi_sum,0,my_comm,ierror)
  call mpi_reduce(icount,icountall,1,mpi_integer,mpi_sum,0,my_comm,ierror)
  call mpi_reduce(tgesmax,tgesmaxall,1,mpi_real,mpi_max,0,my_comm,ierror)
  call mpi_reduce(tgesmin,tgesminall,1,mpi_real,mpi_min,0,my_comm,ierror)
  call mpi_reduce(qgesmax,qgesmaxall,1,mpi_real,mpi_max,0,my_comm,ierror)
  call mpi_reduce(qgesmin,qgesminall,1,mpi_real,mpi_min,0,my_comm,ierror)
  call mpi_reduce(numabove,numaboveall,1,mpi_integer,mpi_sum,0,my_comm,ierror)
  call mpi_reduce(numbelow,numbelowall,1,mpi_integer,mpi_sum,0,my_comm,ierror)
  call mpi_reduce(numbelow200,numbelow200all,1,mpi_integer,mpi_sum,0,my_comm,ierror)
  call mpi_reduce(qerrmax0,qerrmax0all,1,mpi_real,mpi_max,0,my_comm,ierror)
  call mpi_reduce(qerrmin0,qerrmin0all,1,mpi_real,mpi_min,0,my_comm,ierror)
  call mpi_reduce(qerrmax,qerrmaxall,1,mpi_real,mpi_max,0,my_comm,ierror)
  call mpi_reduce(qerrmin,qerrminall,1,mpi_real,mpi_min,0,my_comm,ierror)
  call mpi_reduce(qggesmax,qggesmaxall,1,mpi_real,mpi_max,0,my_comm,ierror)
  call mpi_reduce(qggesmin,qggesminall,1,mpi_real,mpi_min,0,my_comm,ierror)
  call mpi_reduce(numnegqges,numnegqgesall,1,mpi_integer,mpi_sum,0,my_comm,ierror)
  if(mype.eq.0) then
      print *,' icount=',icountall
      delhbarall=delhbarall/max(1,icountall)
      delhrmsall=sqrt(delhrmsall/max(1,icountall))
      print '('' in getqges, qgesmax,min='',2es14.5)',qgesmaxall,qgesminall
      print '('' in getqges, tgesmax,min='',2es14.5)',tgesmaxall,tgesminall
      print '('' in getqges, delhbar,delhrms='',2es14.5)',delhbarall,delhrmsall
      print *,' in getqges, numabove,below=',numaboveall,numbelowall
      print *,' in getqges, numbelow200=',numbelow200all
      print '('' in getqges, uncorrected qerrmax,min='',2es14.5)',qerrmax0all,qerrmin0all
      print '('' in getqges, corrected qerrmax,min='',2es14.5)',qerrmaxall,qerrminall
      print '('' max,min qgges='',2es14.5)',qggesmaxall,qggesminall
      print *,' numnegqges=',numnegqgesall
  end if
  if(ndata.gt.0) then
   call genqsat(tges,qsatges,ndata,pobs)

   deallocate(tges)
   deallocate(pobs)
  end if

return
end subroutine getqges
