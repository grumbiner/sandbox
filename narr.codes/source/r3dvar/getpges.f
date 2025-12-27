subroutine getpges(pges,petaobs,rlon,rlat,rpres,ptobs,pelev,pstaid,ndata, &
              iordges,lbig2ges,lhalfges,wgts2,iwgts2,imeta,jmeta,pdres01,zsfcges,tsfcges,res, &
              tlapseges,ptop,perr,wbglb,dlon,sbglb,dlat,istaghglb,imetaglb,jmetaglb,mype,icode)


!-------- obtain guess peta at observed x,y,p for 
!--------    surface pressures and/or geopotential heights

!--   pges:   output guess peta interpolated to obs locations
!--   petaobs: output adjusted obs peta.
!--   rlon,rlat:  grid lon, lat of obs location (degrees)
!--   rpres:  input log pressure (mb) for each obs
!--   ptobs:  input obs temp
!--   pelev:  input obs elevation
!--   ndata:  input number of obs
!--   imeta,jmeta:  input dimensions of guess grid
!--   wgts2:  output horizontal interpolation weights
!--   iwgts2: output horizontal interpolation indices

!--------   pges set to 1.e20 for every
!-------    location which falls outside the guess domain (this includes
!-------               points below model terrain)

  include 'mpif.h'
         include "my_comm.h"

  real(4) pges(max(1,ndata)),rlon(max(1,ndata)),rlat(max(1,ndata))
  real(4) petaobs(max(1,ndata))
  real(4) rpres(max(1,ndata)),ptobs(max(1,ndata)),pelev(max(1,ndata))
  character*1 pstaid(8,max(1,ndata))
  real(4) wgts2(max(1,ndata),lbig2ges)
  integer(4) iwgts2(max(1,ndata),lbig2ges)
  real(4) pdres01(imeta*jmeta)
  real(4) zsfcges(imeta*jmeta)
  real(4) tsfcges(imeta*jmeta)
  real(4) res(imeta*jmeta)
  real(4) tlapseges(imeta*jmeta)
  real(4) perr(max(1,ndata))
  integer(4) icode(max(1,ndata))

  integer(4),allocatable::iflag(:)

  character*1 dollar
  data dollar/'$'/

  allocate(iflag(max(1,ndata)))

!   initialize output guess and adjusted observed peta

  icode=0
  pges=0.
  petaobs=0.

!-------- if elevation, or temperature missing, or stdatm pressure, don't use
!                      (stdatm pressure indicated by $-sign in 6-th character of staid)

  msgelev=0
  msgtobs=0
  numdollar=0
  if(ndata.gt.0) then
   do i=1,ndata
    if(pstaid(6,i).eq.dollar) then
     numdollar=numdollar+1
     pges(i)=1.e20
     icode(i)=6
    end if
    if(pelev(i).gt.1.e6) then
     msgelev=msgelev+1
     pges(i)=1.e20
     icode(i)=4
    end if
    if(ptobs(i).lt.150..or.ptobs(i).gt.350.) then
     msgtobs=msgtobs+1
    end if
   end do
  end if
  call mpi_reduce(msgelev,msgelevall,1,mpi_integer,mpi_sum,0,my_comm,ierror)
  call mpi_reduce(msgtobs,msgtobsall,1,mpi_integer,mpi_sum,0,my_comm,ierror)
  call mpi_reduce(numdollar,numdollarall,1,mpi_integer,mpi_sum,0,my_comm,ierror)
  if(mype.eq.0) print *,' in getpges, number sfcp obs with missing elevation=',msgelevall
  if(mype.eq.0) print *,' in getpges, number sfcp obs with missing temperature=',msgtobsall
  if(mype.eq.0) print *,' in getpges, number sfcp obs with missing pressure=',numdollarall

!-------- get horizontal interpolation weights, addresses

  numoutside=0
  tlapsegmax=-huge(gorm)
  tlapsegmin=huge(gorm)
  if(ndata.gt.0) then
   call st_simpin2f(wgts2,iwgts2,iflag,rlon,rlat,ndata,iordges,lhalfges,lbig2ges, &
                   wbglb,dlon,sbglb,dlat,imetaglb,jmetaglb,istaghglb)
   call st_glb2loc_iwgts(iwgts2,ndata,lbig2ges,imetaglb,jmetaglb,imeta,jmeta)

   
!              only keep obs inside domain

   do i=1,ndata
    if(iflag(i).eq.0) then
     numoutside=numoutside+1
    else
     pges(i)=0.
    end if
   end do

!    obtain adjusted petaobs at each interpolating point, and interpolate
!     petages,petaobs to obs location.
!      also increase obs err by 1mb/200m of difference between obs elev and model elev.

   gorm=9.8076/287.16
   do k=1,lbig2ges
    do i=1,ndata
     if(pges(i).lt.1.e19) then
      ii=iwgts2(i,k)
      delz=pelev(i)-zsfcges(ii)
      if(ptobs(i).lt.150..or.ptobs(i).gt.350.) then
       tbar=tsfcges(ii)+tlapseges(ii)*delz     !  tlapseges is average lapse rate of guess over
       errmult=.008                            !    1000 meters above the surface  deg/meter
            tlapsegmax=max(tlapseges(ii),tlapsegmax)
            tlapsegmin=min(tlapseges(ii),tlapsegmin)
      else
       tbar=.5*(tsfcges(ii)+ptobs(i))
       errmult=.005
      end if
      dlogp=gorm*delz/tbar
      adjusted_lpobs=rpres(i)+dlogp
      this_petaobs=res(ii)*(exp(adjusted_lpobs)-ptop)
      pges(i)=pges(i)+wgts2(i,k)*pdres01(ii)
      petaobs(i)=petaobs(i)+wgts2(i,k)*this_petaobs
      perr(i)=perr(i)+wgts2(i,k)*abs(delz)*errmult
     end if
    end do
   end do
  end if
      call mpi_reduce(tlapsegmax,tlapsegmaxall,1,mpi_real,mpi_max,0,my_comm,ierr)
      call mpi_reduce(tlapsegmin,tlapsegminall,1,mpi_real,mpi_min,0,my_comm,ierr)
      if(mype.eq.0) print *,' in getpges, tlapsegmax,min=',tlapsegmaxall,tlapsegminall
  deallocate(iflag)

return
end subroutine getpges
