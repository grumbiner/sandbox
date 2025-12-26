subroutine gen_eps_bayes( &
      epsbart,epsbarw,epsbarq,epsbarpw,epsbarps,epsbarrad,epspred,epst,epsq,epsw,epsp, &
      mtdata,mwdata,mqdata,mpwdata,mpdata,mraddata, &
      npred,nxc,nyc,lmetaex,userad,myxsc,myxec,myysc,myyec,jpch)

!--- generate bold_eps "white noise" vector

  include 'mpif.h'
      include "my_comm.h"

  logical userad
  real(4) epsbart(max(1,mtdata)),epsbarw(max(1,mwdata)),epsbarq(max(1,mqdata))
  real(4) epsbarpw(max(1,mpwdata)),epsbarps(max(1,mpdata)),epsbarrad(max(1,mraddata))
  real(4) epspred(jpch*npred)
  real(4) epst(nxc,nyc,lmetaex),epsq(nxc,nyc,lmetaex)
  real(4) epsw(nxc,nyc,lmetaex,2),epsp(nxc,nyc)

  call mpi_comm_rank(my_comm,mype,ierr)
  call greed(mtdata+mwdata+mqdata+mpwdata+mpdata+mraddata)

  if(mtdata.gt.0) then
   do i=1,mtdata
    epsbart(i)=gauss()
   end do
  end if
  if(mwdata.gt.0) then
   do i=1,mwdata
    epsbarw(i)=gauss()
   end do
  end if
  if(mqdata.gt.0) then
   do i=1,mqdata
    epsbarq(i)=gauss()
   end do
  end if
  if(mpwdata.gt.0) then
   do i=1,mpwdata
    epsbarpw(i)=gauss()
   end do
  end if
  if(mpdata.gt.0) then
   do i=1,mpdata
    epsbarps(i)=gauss()
   end do
  end if
  if(mraddata.gt.0) then
   do i=1,mraddata
    epsbarrad(i)=gauss()
   end do
  end if

  do k=1,lmetaex
   do j=myysc,myyec
    do i=myxsc,myxec
     epst(i,j,k)=gauss()
     epsq(i,j,k)=gauss()
     epsw(i,j,k,1)=gauss()
     epsw(i,j,k,2)=gauss()
    end do
   end do
  end do
  do j=myysc,myyec
   do i=myxsc,myxec
    epsp(i,j)=gauss()
   end do
  end do
  do i=1,jpch*npred
   epspred(i)=gauss()
  end do
     
return
end subroutine gen_eps_bayes
