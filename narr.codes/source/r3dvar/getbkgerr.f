subroutine getbkgerr(bight_psi,bighp_psi,wgts_bightp,iwgts_bightp, &
             et,eq,epsi,echi,ep, &
             rlenxyt,rlenxyq,rlenxypsi,rlenxychi,rlenxyp, &
             rlenpt,rlenpq,rlenppsi,rlenpchi, &
             bscalep,bscalet,bscaleq,bscalepsi,bscalechi, &
             nlath,lat1,lat2,rxc,ryc,erlat0,nxc,nyc,lmeta,dlatc,ptop,etai,etam,mype)

  include 'mpif.h'
      include "my_comm.h"

  real(4) bight_psi(lmeta,lmeta,lat1:lat2),bighp_psi(lmeta,lat1:lat2),wgts_bightp(2,nxc,nyc)
  integer(4) iwgts_bightp(2,nxc,nyc)
  real(4) et(lmeta,lat1:lat2),eq(lmeta,lat1:lat2)
  real(4) epsi(lmeta,lat1:lat2),echi(lmeta,lat1:lat2)
  real(4) ep(lat1:lat2)
  real(4) rlenxyt(lmeta),rlenxyq(lmeta),rlenxypsi(lmeta),rlenxychi(lmeta)
  real(4) rlenpt(lmeta),rlenpq(lmeta),rlenppsi(lmeta),rlenpchi(lmeta)
  real(4) etai(lmeta+1),etam(lmeta)
  real(4) rxc(nxc),ryc(nyc)

  real(4),allocatable::qtai(:),qtam(:),rlats(:),wgts(:)
  real(4),allocatable::bt_vert(:,:,:),bd2t_vert(:,:,:)
  real(4),allocatable::bpsi_vert(:,:,:),bvort_vert(:,:,:)
  real(4),allocatable::bchi_vert(:,:,:),bdiv_vert(:,:,:)
  real(4),allocatable::bq_vert(:,:,:),bd2q_vert(:,:,:)
  real(4),allocatable::bpeta(:),bd2peta(:)
  integer(4),allocatable::ixi(:)
  real(4),allocatable::tl(:,:,:),alocal(:),blocal(:)
  real(4),allocatable::rlatthis(:,:)
  real(4),allocatable::wgts_loc(:,:,:)
  integer(4),allocatable::iwgts_loc(:,:,:),iflag(:,:)

  allocate(qtai(lmeta+1))
  allocate(qtam(lmeta))
  allocate(rlats(2*nlath))
  allocate(wgts(2*nlath))
  allocate(bt_vert(lmeta,lmeta,lat1:lat2))
  allocate(bd2t_vert(lmeta,lmeta,lat1:lat2))
  allocate(bpsi_vert(lmeta,lmeta,lat1:lat2))
  allocate(bvort_vert(lmeta,lmeta,lat1:lat2))
  allocate(bchi_vert(lmeta,lmeta,lat1:lat2))
  allocate(bdiv_vert(lmeta,lmeta,lat1:lat2))
  allocate(bq_vert(lmeta,lmeta,lat1:lat2))
  allocate(bd2q_vert(lmeta,lmeta,lat1:lat2))
  allocate(bpeta(lat1:lat2))
  allocate(bd2peta(lat1:lat2))
  allocate(ixi(0:1))
  allocate(tl(2,2,2*(lat2-lat1+1)))
  allocate(alocal(2*(lat2-lat1+1)))
  allocate(blocal(2*(lat2-lat1+1)))
  allocate(rlatthis(nxc,nyc))
  allocate(wgts_loc(nxc,nyc,2))
  allocate(iwgts_loc(nxc,nyc,2))
  allocate(iflag(nxc,nyc))


  rewind 9
  read(9)
  read(9)qtop,zmin,qtai,qtam,bight_psi,bighp_psi,rlats,wgts, &
          bt_vert,bd2t_vert, &
          bpsi_vert,bvort_vert, &
          bchi_vert,bdiv_vert, &
          bq_vert,bd2q_vert, &
          bpeta,bd2peta
  rewind 9

!     check to see if input file is correct:

  ierr=0
  if(qtop.ne.ptop) ierr=1
  do k=1,lmeta+1
   if(qtai(k).ne.etai(k)) ierr=1
  end do
  do k=1,lmeta
   if(qtam(k).ne.etam(k)) ierr=1
  end do
  if(ierr.ne.0) then
    write(0,*)' BAD OR WRONG BACKGROUND ERROR FILE--eta edvar fails'
    stop
  end if

!   get variances first

  do j=lat1,lat2
   do k=1,lmeta
    et(k,j)=sqrt(bt_vert(k,k,j))
    eq(k,j)=sqrt(bq_vert(k,k,j))
    epsi(k,j)=sqrt(bpsi_vert(k,k,j))
    echi(k,j)=sqrt(bchi_vert(k,k,j))
   end do
   ep(j)=sqrt(bpeta(j))
  end do

  deg2rad=atan(1.)/45.
     if(mype.eq.0) then
      do l=0,90,10
       rlat0=l
       dlmin=180.
       do j=lat1,lat2
        dl=abs(rlat0-rlats(j)/deg2rad)
        if(dl.lt.dlmin) then
         j0=j ; dlmin=dl
        end if
       end do
       var_p=ep(j0)**2
       print *,' j0,lat1,lat2,lat,var_p=',j0,lat1,lat2,rlat0,var_p
      end do

     end if
     if(mype.eq.0) then
      do k=1,lmeta
       do l=0,90,10
        rlat0=l
        dlmin=180.
        do j=lat1,lat2
         dl=abs(rlat0-rlats(j)/deg2rad)
         if(dl.lt.dlmin) then
          j0=j ; dlmin=dl
         end if
        end do
        var_t=et(k,j0)**2
        var_q=eq(k,j0)**2
        var_psi=epsi(k,j0)**2
        var_chi=echi(k,j0)**2
        print *,' k,lat,vart,q,psi,chi=',k,rlat0,var_t,var_q,var_psi,var_chi
       end do
      end do
     end if

    
!   next get horizontal correlation lengths

  sum=0.
  rlenxyt=0.
  rlenxyq=0.
  rlenxypsi=0.
  rlenxychi=0.
  rlenxyp=0.
  do j=lat1,lat2
   sum=sum+wgts(j)
   do k=1,lmeta
    rlenxyt(k)=rlenxyt(k)+wgts(j)*(8.*bt_vert(k,k,j)/bd2t_vert(k,k,j))**.25
    rlenxyq(k)=rlenxyq(k)+wgts(j)*(8.*bq_vert(k,k,j)/bd2q_vert(k,k,j))**.25
    rlenxypsi(k)=rlenxypsi(k)+wgts(j)*(8.*bpsi_vert(k,k,j)/bvort_vert(k,k,j))**.25
    rlenxychi(k)=rlenxychi(k)+wgts(j)*(8.*bchi_vert(k,k,j)/bdiv_vert(k,k,j))**.25
   end do
   rlenxyp=rlenxyp+wgts(j)*(8.*bpeta(j)/bd2peta(j))**.25
  end do
  escale=conmc('rerth$')*deg2rad
                                         !    conversion factor meters to grid units--
  convert=1./(dlatc*escale)              !           (strictly valid only at 
                                         !   center of domain, since convergence of longitudes ignored)
  fixcon=1.
  rlenxyt=fixcon*rlenxyt/sum           
  rlenxyq=fixcon*rlenxyq/sum
  rlenxypsi=fixcon*rlenxypsi/sum
  rlenxychi=fixcon*rlenxychi/sum
  rlenxyp=fixcon*rlenxyp/sum
    if(mype.eq.0)   then
      print *,' rlenxyp(km)=',.001*rlenxyp
      do k=1,lmeta
       papprox=etam(k)*(1013.-ptop)+ptop
       print *,' p,rlenxyt,q,psi,chi(km)=',papprox, &
                          .001*rlenxyt(k),.001*rlenxyq(k),.001*rlenxypsi(k),.001*rlenxychi(k)
      end do
    end if
  rlenxyt=convert*rlenxyt
  rlenxyq=convert*rlenxyq
  rlenxypsi=convert*rlenxypsi
  rlenxychi=convert*rlenxychi
  rlenxyp=convert*rlenxyp
    if(mype.eq.0)   then
      print *,' rlenxyp(gu)=',rlenxyp
      do k=1,lmeta
       papprox=etam(k)*(1013.-ptop)+ptop
       print *,' p,rlenxyt,q,psi,chi(gu)=',papprox,rlenxyt(k),rlenxyq(k),rlenxypsi(k),rlenxychi(k)
      end do
    end if

!  now get vertical correlations

!   first divide out variance

  do j=lat1,lat2
   do k=1,lmeta
    do m=1,lmeta
     bt_vert(k,m,j)=bt_vert(k,m,j)/(et(k,j)*et(m,j))
     bq_vert(k,m,j)=bq_vert(k,m,j)/(eq(k,j)*eq(m,j))
     bpsi_vert(k,m,j)=bpsi_vert(k,m,j)/(epsi(k,j)*epsi(m,j))
     bchi_vert(k,m,j)=bchi_vert(k,m,j)/(echi(k,j)*echi(m,j))
    end do
   end do
  end do

!        rescale variance by external multiplying factors

  et=bscalet*et
  eq=bscaleq*eq
  epsi=bscalepsi*epsi
  echi=bscalechi*echi
  ep=bscalep*ep
     if(mype.eq.0) then
      do l=0,90,10
       rlat0=l
       dlmin=180.
       do j=lat1,lat2
        dl=abs(rlat0-rlats(j)/deg2rad)
        if(dl.lt.dlmin) then
         j0=j ; dlmin=dl
        end if
       end do
       var_p=ep(j0)**2
       print *,' j0,lat1,lat2,lat,var_p=',j0,lat1,lat2,rlat0,var_p
      end do

     end if
     if(mype.eq.0) then
      do k=1,lmeta
       do l=0,90,10
        rlat0=l
        dlmin=180.
        do j=lat1,lat2
         dl=abs(rlat0-rlats(j)/deg2rad)
         if(dl.lt.dlmin) then
          j0=j ; dlmin=dl
         end if
        end do
        var_t=et(k,j0)**2
        var_q=eq(k,j0)**2
        var_psi=epsi(k,j0)**2
        var_chi=echi(k,j0)**2
        print *,' k,lat,vart,q,psi,chi=',k,rlat0,var_t,var_q,var_psi,var_chi
       end do
      end do
     end if

!    now get correlation lengths

  rlenpt=0.
  rlenpq=0.
  rlenppsi=0.
  rlenpchi=0.
  do j=lat1,lat2
   do k=1,lmeta
    kp=k+1
    km=k-1
    if(km.eq.0) km=2
    if(kp.eq.lmeta+1) kp=lmeta-1
    rlenpt(k)=rlenpt(k)+wgts(j)*sqrt(bt_vert(k,k,j)/(2.*bt_vert(k,k,j)-bt_vert(km,k,j)-bt_vert(kp,k,j)))
    rlenpq(k)=rlenpq(k)+wgts(j)*sqrt(bq_vert(k,k,j)/(2.*bq_vert(k,k,j)-bq_vert(km,k,j)-bq_vert(kp,k,j)))
    rlenppsi(k)=rlenppsi(k)+wgts(j)*sqrt(bpsi_vert(k,k,j)/ &
                       (2.*bpsi_vert(k,k,j)-bpsi_vert(km,k,j)-bpsi_vert(kp,k,j)))
    rlenpchi(k)=rlenpchi(k)+wgts(j)*sqrt(bchi_vert(k,k,j)/ &
                       (2.*bchi_vert(k,k,j)-bchi_vert(km,k,j)-bchi_vert(kp,k,j)))
   end do
  end do
  rlenpt=fixcon*rlenpt/sum
  rlenpq=fixcon*rlenpq/sum
  rlenppsi=fixcon*rlenppsi/sum
  rlenpchi=fixcon*rlenpchi/sum
    if(mype.eq.0)   then
      do k=1,lmeta
       papprox=etam(k)*(1013.-ptop)+ptop
       print *,' p,rlenpt,q,psi,chi(gu)=',papprox,rlenpt(k),rlenpq(k),rlenppsi(k),rlenpchi(k)
      end do
    end if
!-----------  above are in grid units already

!------------- finally get interpolation weights for interpolating latidude dependent stuff to 
!-------------- analysis grid

  iord=1
  lbig=iord+1
  call simpin1_init(ixi,tl,alocal,blocal,iord,lbig,rlats(lat1),lat2-lat1+1)
  clat0=cos(deg2rad*erlat0)
  slat0=sin(deg2rad*erlat0)
  tomega=2.*conmc('omega$')
  do j=1,nyc
   coslat=cos(ryc(j)*deg2rad)
   sinlat=sin(ryc(j)*deg2rad)
   do i=1,nxc
    slatthis=abs(clat0*sinlat+slat0*coslat*cos(rxc(i)*deg2rad))
    rlatthis(i,j)=asin(min(.99999,slatthis))
   end do
  end do
  call simpin1(wgts_loc,wgts_loc,wgts_loc,iwgts_loc, &
         iflag,rlatthis,nxc*nyc,iord,lbig,rlats(lat1),lat2-lat1+1,1,0,0,ixi,tl,alocal,blocal)

     numextrap=0
  do j=1,nyc
   do i=1,nxc
    if(iflag(i,j).ne.0) then
     wgts_bightp(:,i,j)=wgts_loc(i,j,:)
     iwgts_bightp(:,i,j)=iwgts_loc(i,j,:)
    else
     numextrap=numextrap+1
     wgts_bightp(1,i,j)=1.
     wgts_bightp(2,i,j)=0.
     if(iwgts_loc(i,j,1).lt.4) then
      iwgts_bightp(:,i,j)=1
     else
      iwgts_bightp(:,i,j)=lat2-lat1+1
     end if
    end if
            if(mype.eq.0.and.mod(i,10).eq.0.and.mod(j,10).eq.0) &
               print *,' i,j,iwgts,wgts=',i,j,iwgts_bightp(1,i,j),iwgts_bightp(2,i,j), &
                                      wgts_bightp(1,i,j),wgts_bightp(2,i,j)
   end do
  end do
       call mpi_reduce(numextrap,numextrap0,1,mpi_integer4,mpi_sum,0,my_comm,ierr)
       if(mype.eq.0) print *,' number of points extrapolated=',numextrap0

!  adjust iwgts based on starting address of lat1 instead of 1
     
  do j=1,nyc
   do i=1,nxc
    iwgts_bightp(1,i,j)=iwgts_bightp(1,i,j)+lat1-1
    iwgts_bightp(2,i,j)=iwgts_bightp(2,i,j)+lat1-1
   end do
  end do

  deallocate(qtai)
  deallocate(qtam)
  deallocate(rlats)
  deallocate(wgts)
  deallocate(bt_vert)
  deallocate(bd2t_vert)
  deallocate(bpsi_vert)
  deallocate(bvort_vert)
  deallocate(bchi_vert)
  deallocate(bdiv_vert)
  deallocate(bq_vert)
  deallocate(bd2q_vert)
  deallocate(bpeta)
  deallocate(bd2peta)
  deallocate(ixi)
  deallocate(tl)
  deallocate(alocal)
  deallocate(blocal)
  deallocate(rlatthis)
  deallocate(wgts_loc)
  deallocate(iwgts_loc)
  deallocate(iflag)

return
end subroutine getbkgerr
