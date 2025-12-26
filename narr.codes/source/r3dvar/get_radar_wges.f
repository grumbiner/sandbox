subroutine get_radar_wges(wges,staelev,kbeambot,kbeamtop,rlon,rlat,welev,wrange,wstaid,delta,epsilnw,ndata, &
         iordges,lbig2ges,lhalfges,wgtsv,iwgtsv,imeta,jmeta,lmetaex,ugges,vgges, &
         lmh,lmv,wtype,wobs,hgges,etheta,werr,wbglb,dlon,sbglb,dlat,istaghglb,istagvglb,imetaglb,jmetaglb, &
         rlone,rlate,wtime,beamdepth,ireason)

!-------- special processing for radar wind data, which attempts to account for beam width in vertical
!--------  this should allow obs to be used out to the maximum range of the radar

!--   wges:   output guess interpolated to obs locations using non-linear forward model over depth of beam
!--   kbeambot,kbeamtop:  vertical indices of eta levels that just contain the radar beam above and below
!--   rlon,rlat:  grid lon, lat of obs location (degrees)
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

  real(4) wges(ndata),rlon(ndata),rlat(ndata),welev(ndata),wrange(ndata)
  real(4) staelev(ndata)
  integer(4) kbeambot(ndata),kbeamtop(ndata)
  character(8) wstaid(ndata)
  real(4) delta(ndata),epsilnw(ndata),ugges(imeta*jmeta*lmetaex),vgges(imeta*jmeta*lmetaex)
  real(4) hgges(imeta*jmeta*(lmetaex+1))
  integer(4) lmh(imeta*jmeta)
  integer(4) lmv(imeta*jmeta)
  real(4) wtype(ndata),wobs(ndata),etheta(ndata),werr(ndata)
  real(4) wgtsv(ndata,lbig2ges)
  integer(4) iwgtsv(ndata,lbig2ges)
  real(4) rlone(ndata),rlate(ndata),wtime(ndata),beamdepth(ndata)
  integer(4) ireason(ndata)

  real(4),allocatable::hthis(:),hthism(:),uprofile(:)
  integer(4),allocatable::iflagh(:),iflagv(:),kbotlim(:)
  real(4),allocatable::wgtsh(:,:),delelev(:)
  integer(4),allocatable::iwgtsh(:,:)
  character(40)filename

  special_value=1.e20
  test_value=.98*special_value
  call mpi_comm_rank(my_comm,mype,ierr)
         if(mype.eq.0) write(0,*)' at 1 in get_radar_wges'

  if(ndata.gt.0) then
   ireason=-999
   allocate(iflagh(ndata))
   allocate(iflagv(ndata))
   allocate(wgtsh(ndata,lbig2ges))
   allocate(iwgtsh(ndata,lbig2ges))
   allocate(delelev(ndata))
  end if
!-------- get horizontal interpolation weights on h and v grids

  delelmax=0.
  if(ndata.gt.0) then
   delelev=0.
   call st_simpin2f(wgtsh,iwgtsh,iflagh,rlon,rlat,ndata,iordges,lhalfges,lbig2ges, &
                   wbglb,dlon,sbglb,dlat,imetaglb,jmetaglb,istaghglb)
   call st_glb2loc_iwgts(iwgtsh,ndata,lbig2ges,imetaglb,jmetaglb,imeta,jmeta)
   call st_simpin2f(wgtsv,iwgtsv,iflagv,rlon,rlat,ndata,iordges,lhalfges,lbig2ges, &
                   wbglb,dlon,sbglb,dlat,imetaglb,jmetaglb,istagvglb)
   call st_glb2loc_iwgts(iwgtsv,ndata,lbig2ges,imetaglb,jmetaglb,imeta,jmeta)
   do i=1,ndata
    iwtype=nint(wtype(i))
    if(iwtype.lt.2270) iflagh(i)=0
   end do
   do i=1,ndata
    if(iflagh(i).gt.0) then
     do kk=1,lbig2ges
      khthis=min(lmetaex+1,lmh(iwgtsh(i,kk))+1)
      ihggesthis=iwgtsh(i,kk)+(khthis-1)*imeta*jmeta
      if(hgges(ihggesthis).gt.test_value) then
       iflagh(i)=0
      else
       delelev(i)=max(abs(staelev(i)-hgges(ihggesthis)),delelev(i))
       delelmax=max(delelev(i),delelmax)
      end if
     end do
    end if
   end do
   iflagh=iflagh*iflagv
   iflagv=iflagh
  end if

         if(mype.eq.0) write(0,*)' at 2 in get_radar_wges'
  allocate(hthis(lmetaex+1))
  allocate(hthism(lmetaex))

!-------- for radar obs (type 227), get guess vertical range from estimated beam spread in vertical

  numradar=0
  kbeamdiffmax=0
  kbeamdiffmin=lmetaex+1
  allocate(kbotlim(max(1,ndata)))
  if(ndata.gt.0) then
   kbotlim=0
   do i=1,ndata
    if(iflagh(i).gt.0) then
     iwtype=nint(wtype(i))
     numradar=numradar+1

     addelev=max(.5*delelev(i),10.0*wrange(i))
     beamdepth(i)=2.*addelev
     elevtop=welev(i)+addelev          !  this is based on 100ft/Nm = 16.5m/km beam spread     
     elevbot=welev(i)-addelev          !  for .95 deg beam angle (multiplied by 1.2 to allow
                                        !   for propagation uncertainty)     
                                        !  also, a minimum uncertainty based on difference between
                                        !  model surface elevation and actual radar elevation
          if(mod(i,100).eq.0.and.mype.eq.30)  &
               print *,' elevtop,elevbot,welev,wrange=',elevtop,elevbot,welev(i),wrange(i)

!----------- form column of heights for this location

     kbotlim(i)=lmetaex+1
     do k=1,lmetaex+1
      hthis(k)=0.
      do kk=1,lbig2ges
       hthis(k)=hthis(k)+wgtsh(i,kk)*hgges(iwgtsh(i,kk)+(k-1)*imeta*jmeta)
       kbotlim(i)=min(min(lmetaex,lmh(iwgtsh(i,kk))),kbotlim(i))
      end do
     end do

!        get height estimate at center of each layer (where wind variable is located)

     do k=1,lmetaex
      hthism(k)=.5*(hthis(k)+hthis(k+1))
     end do

!   obtain  kbeambot, kbeamtop

     do k=1,lmetaex
      if(hthism(k).lt.elevtop) then
       kbeamtop(i)=k-1
       exit
      end if
     end do
     do k=lmetaex,1,-1
      if(hthis(k).gt.elevbot) then
       kbeambot(i)=k+1
       exit
      end if
     end do
         kbeamdiffmax=max(kbeambot(i)-kbeamtop(i),kbeamdiffmax)
         kbeamdiffmin=min(kbeambot(i)-kbeamtop(i),kbeamdiffmin)
    end if
   end do
  end if

         if(mype.eq.0) write(0,*)' at 3 in get_radar_wges'

  deallocate(hthis)

!-------- now exclude observations whose pressure is too far above or below model domain

  numbottoss=0
  numtoptoss=0
  numzero=0
  if(ndata.gt.0) then
   do i=1,ndata
    if(iflagh(i).gt.0) then
     if(kbeambot(i).gt.kbotlim(i)) then
      iflagh(i)=0
      numbottoss=numbottoss+1
      ireason(i)=8
     end if
     if(kbeamtop(i).lt.1) then
      iflagh(i)=0
      numtoptoss=numtoptoss+1
      ireason(i)=9
     end if
     if(wobs(i).eq.0.) then
      iflagh(i)=0
      numzero=numzero+1
      ireason(i)=10
     end if
    end if
   end do
  end if
         if(mype.eq.0) write(0,*)' at 4 in get_radar_wges'
  call mpi_reduce(numbottoss,numbottossall,1,mpi_integer,mpi_sum,0,my_comm,ierror)
  call mpi_reduce(numtoptoss,numtoptossall,1,mpi_integer,mpi_sum,0,my_comm,ierror)
  call mpi_reduce(numzero,numzeroall,1,mpi_integer,mpi_sum,0,my_comm,ierror)
  call mpi_reduce(numradar,numradarall,1,mpi_integer,mpi_sum,0,my_comm,ierror)
  call mpi_reduce(kbeamdiffmax,kbeamdiffmaxall,1,mpi_integer,mpi_max,0,my_comm,ierror)
  call mpi_reduce(kbeamdiffmin,kbeamdiffminall,1,mpi_integer,mpi_min,0,my_comm,ierror)
         if(mype.eq.0) write(0,*)' at 5 in get_radar_wges'
  if(mype.eq.0) print *,' in get_radar_wges, numbottoss,numtoptoss,numradar=', &
                 numbottossall,numtoptossall,numradarall
  if(mype.eq.0) print *,' kbeamdiffmax=',kbeamdiffmaxall
  if(mype.eq.0) print *,' kbeamdiffmin=',kbeamdiffminall
  if(mype.eq.0) print *,' numzero=',numzeroall

!       next get wges

  allocate(uprofile(lmetaex+1))
  numequal=0
  numnotequal=0
  count=0.
  rmsfit=0.
  if(ndata.gt.0) then
   do i=1,ndata
    if(wtype(i).gt.2269.5) wges(i)=1.e20
   end do
   do i=1,ndata
    if(iflagh(i).gt.0) then
    
!     first interpolate to get vertical profile of uges

     do k=kbeamtop(i),kbeambot(i)
      uprofile(k)=0.
      ugloc=0.
      vgloc=0.
      do kk=1,lbig2ges
       ugloc=ugloc+wgtsv(i,kk)*ugges(iwgtsv(i,kk)+(k-1)*imeta*jmeta)
       vgloc=vgloc+wgtsv(i,kk)*vgges(iwgtsv(i,kk)+(k-1)*imeta*jmeta)
      end do
      uprofile(k)=delta(i)*ugloc+epsilnw(i)*vgloc
     end do

!            now run forward model to get desired simulated wind

     call forward_radar(wges(i),wobs(i),uprofile,kbeambot(i),kbeamtop(i))
             rmsfit=rmsfit+(wges(i)-wobs(i))**2
             count=count+1.
             if(wges(i).eq.wobs(i)) then
              numequal=numequal+1
             else
              numnotequal=numnotequal+1
             end if
    end if
   end do
  end if
  call mpi_reduce(rmsfit,rmsfitall,1,mpi_real,mpi_sum,0,my_comm,ierror)
  call mpi_reduce(count,countall,1,mpi_real,mpi_sum,0,my_comm,ierror)
  if(mype.eq.0) then
   if (countall/=0) then            !!!!dule
   rmsfit=sqrt(rmsfitall/countall)
   end if                           !!!!dule
   print *,' in get_radar_wges, rmsfit,countall=',rmsfit,countall
  end if
  call mpi_reduce(numequal,numequalall,1,mpi_integer,mpi_sum,0,my_comm,ierror)
  call mpi_reduce(numnotequal,numnotequalall,1,mpi_integer,mpi_sum,0,my_comm,ierror)
  if(mype.eq.0) print *,' in get_radar_wges, numequal=',numequalall
  if(mype.eq.0) print *,' in get_radar_wges, numnotequal=',numnotequalall
  call mpi_reduce(delelmax,delelmax0,1,mpi_real,mpi_max,0,my_comm,ierror)
  if(mype.eq.0) print *,' in get_radar_wges, delelmax=',delelmax0
  if(ndata.gt.0) then
   deallocate(iflagh)
   deallocate(iflagv)
   deallocate(wgtsh)
   deallocate(iwgtsh)
   deallocate(delelev)
  end if
  deallocate(hthism)
  deallocate(kbotlim)
  deallocate(uprofile)
return
end subroutine get_radar_wges
subroutine forward_radar(wout,wobs,wges,kbot,ktop)

!   forward model for radar winds that allows for beam spread in crude manner

  real(4) wges(*)

! real(4) p(ktop:kbot)

!     bign=kbot-ktop+1
!     bigninv=1./bign
!     ubar=0.
!     pbar=0.
!     upbar=0.
!     ppbar=0.
!     pmid=.5*(ktop+kbot)
!     do k=ktop,kbot
!      p(k)=k-pmid
!     end do

  umaxmax=-huge(umaxmax)
  uminmin=huge(uminmin)
  do k=ktop,kbot
   umaxmax=max(umaxmax,wges(k))
   uminmin=min(uminmin,wges(k))
!      ubar=ubar+wges(k)
!      pbar=pbar+p(k)
!      upbar=upbar+wges(k)*p(k)
!      ppbar=ppbar+p(k)*p(k)
  end do
!     ubar=ubar*bigninv
!     pbar=pbar*bigninv
!     upbar=upbar*bigninv
!     ppbar=ppbar*bigninv
!     a=(upbar-ubar*pbar)/(ppbar-pbar*pbar)
!     b=(ubar*ppbar-upbar*pbar)/(ppbar-pbar*pbar)
!     ubot=a*p(kbot)+b
!     utop=a*p(ktop)+b
!     umin=min(ubot,utop)
!     umax=max(ubot,utop)
!     if(wobs.lt.umin) wout=umin
!     if(wobs.gt.umax) wout=umax
  wout=wobs
  if(wobs.lt.uminmin) wout=uminmin
  if(wobs.gt.umaxmax) wout=umaxmax

return
end subroutine forward_radar
