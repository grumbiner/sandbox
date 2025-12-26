subroutine getpwges(pwges,rlon,rlat,presb,prest,ndata,iordges,lbig2ges,lhalfges, &
              iwgts,bighpw, &
              imeta,jmeta,lmetaex,pdres01,qgges,lmh,ptop,pwtype,etaiex,wbglb,dlon,sbglb,dlat, &
              istaghglb,imetaglb,jmetaglb)

!-------- obtain guess 3-d field at observed x,y,p for 
!--------    a group of moisture values.  then add up to get
!--------    guess precipitable water.
!--------  height of adajacent local steps is large)

!--   pwges:   output guess interpolated to obs locations
!--   rlon,rlat:  grid lon, lat of obs location (degrees)
!--   presb,prest:  on input, sigma integration limits, on output
!--                      corresponding pressure integration limits)
!--   ndata:  number of obs
!--   iordges:  order of interpolation (probably should always be =1 (linear) 
!                                         because of step mountains)
!--   lbig2ges:  number of interpolating points (=3 for iordges=1)
!--   imeta,jmeta,lmetaex:  dimensions of guess eta grid
!--   pdres01:            pressure thickness for eta grid
!--   qgges:  guess moisture
!--   lmh:    terrain index array
!--   ptop:   top pressure
!--   pwtype:  obs type
!--   etaiex:    eta interface coordinate
!--   wb:      starting longitude of eta grid (degrees)
!--   dlon:    longitude grid increment of eta grid (degrees)
!--   sb:      starting latitude of eta grid (degrees)
!--   dlat:    latitude grid increment of eta grid (degrees)

!--------   pwges set to 1.e20 for every
!-------    location which falls outside the guess domain

  include 'mpif.h'
      include "my_comm.h"

  real(4) pwges(max(1,ndata)),rlon(max(1,ndata)),rlat(max(1,ndata)),presb(max(1,ndata)),prest(max(1,ndata))
  real(4) pdres01(imeta*jmeta),qgges(imeta*jmeta,lmetaex)
  integer(4) lmh(imeta*jmeta)
  real(4) pwtype(max(1,ndata)),etaiex(lmetaex+1)
  integer(4) iwgts(max(1,ndata),lbig2ges)
  real(4) bighpw(lmetaex,lbig2ges,max(1,ndata))

  real(4),allocatable::wgts(:,:)
  real(4),allocatable::prestout(:),presbout(:)
  integer(4),allocatable::iflag(:)

  special_value=1.e20
  test_value=.98*special_value
  call mpi_comm_rank(my_comm,mype,ierr)

!--------
!-------- find minimum guess pressure

  call getmaxmin2(peta1min,petagmax,pdres01,imeta,jmeta)
  peta1min=etaiex(1)*peta1min+ptop
  petagmax=etaiex(lmetaex+1)*petagmax+ptop
  if(mype.eq.0) then
   print '('' in getpwges, peta1min='',es14.5)',peta1min
   print '('' in getpwges, petag,max='',es14.5)',petagmax
  end if

!-------- get horizontal interpolation weights, addresses

  icount=0
  pwgesmax=-huge(pwgesmax)
  pwgesmin=huge(pwgesmax)
  allocate(iflag(max(1,ndata)))
  allocate(prestout(max(1,ndata))) ; allocate(presbout(max(1,ndata)))
  allocate(wgts(max(1,ndata),lbig2ges))
  if(ndata.gt.0) then
   
   call st_simpin2f(wgts,iwgts,iflag,rlon,rlat,ndata,iordges,lhalfges,lbig2ges, &
                   wbglb,dlon,sbglb,dlat,imetaglb,jmetaglb,istaghglb)
   call st_glb2loc_iwgts(iwgts,ndata,lbig2ges,imetaglb,jmetaglb,imeta,jmeta)
   do i=1,ndata
    if(iflag(i).gt.0) then
     do kk=1,lbig2ges
      if(pdres01(iwgts(i,kk)).gt.test_value) iflag(i)=0
     end do
    end if
   end do

!-------- now do 2-d interpolation of moisture and vertical integration

   prestout=0. ; presbout=0.
   pwges=1.e20

   bighpw=0.
   do i=1,ndata
    if(iflag(i).gt.0) then
     pwges(i)=0.
     icount=icount+1
     do kk=1,lbig2ges
      khmax=lmh(iwgts(i,kk))
!---------- convert integration limits from sigma to pressure
      psfc=ptop+etaiex(khmax+1)*pdres01(iwgts(i,kk))
      prestg=max(ptop,prest(i)*psfc)
      presbg=presb(i)*psfc
      pwgesg=0.
      do k=1,khmax
       pbelow=ptop+etaiex(k+1)*pdres01(iwgts(i,kk))
       pabove=ptop+etaiex(k)*pdres01(iwgts(i,kk))
       pabove=max(pabove,prestg)
       pbelow=min(pbelow,presbg)
       pdiff=max(0.,pbelow-pabove)
       bighpw(k,kk,i)=wgts(i,kk)*pdiff*100./9.8
       pwges(i)=pwges(i)+bighpw(k,kk,i)*qgges(iwgts(i,kk),k)
      end do
      prestout(i)=prestout(i)+wgts(i,kk)*prestg
      presbout(i)=presbout(i)+wgts(i,kk)*presbg
     end do
     if(nint(pwtype(i)).eq.1007) then
      print *,' GUESS VALUE AT SINGLE TEST POINT FOLLOWS:'
      print *,' GUESS VALUE AT SINGLE TEST POINT FOLLOWS:'
      print *,' GUESS VALUE AT SINGLE TEST POINT FOLLOWS:'
      print *,' GUESS VALUE AT SINGLE TEST POINT FOLLOWS:'
      print *,' one point obs test, pwges set to zero'
      print *,' for one point with full pwobs value, set one_type=1008'
      print *,'   and use following value of pwges as reference for obs value:'
      print *,'      pwges=',pwges(i)
      print *,'        for further interest, presb,prest=',presbout(i),prestout(i)
      pwges(i)=0.
     end if
        pwgesmax=max(pwges(i),pwgesmax)
        pwgesmin=min(pwges(i),pwgesmin)
    end if
   end do
  end if
  call mpi_reduce(icount,icountall,1,mpi_integer,mpi_sum,0,my_comm,ierror)
  call mpi_reduce(pwgesmax,pwgesmaxall,1,mpi_real,mpi_max,0,my_comm,ierror)
  call mpi_reduce(pwgesmin,pwgesminall,1,mpi_real,mpi_min,0,my_comm,ierror)
  if(mype.eq.0) then
   print *,' in getpwges, icount=',icountall
   print '('' in getpwges, pwgesmax,min='',2es14.5)',pwgesmaxall,pwgesminall
  end if

  deallocate(wgts)
  deallocate(iflag)
  deallocate(prestout) ; deallocate(presbout)

return
end subroutine getpwges
