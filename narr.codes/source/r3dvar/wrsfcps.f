subroutine wrsfcps(perr,plon,plat,plong,platg,pobs,pelev,hges, &
               hsfc,etaobssfc,bighhh,ibighhh, &
                   ptobs,ptime,pzqm,ptype,pstaid,iplabel,mpdata,lbig2ges)

!   transfer height data from common

  include 'types.h'
  include "PARMETA.comm"
  include "mpp.h"
  include "mpif.h"
      include "my_comm.h"
  include "r3dv_data.comm"

  real(4) perr(*),plon(*),plat(*)
  real(4) plong(*),platg(*)
  real(4) pobs(*),pelev(*),hges(*),ptobs(*)
  real(4) hsfc(*),etaobssfc(*)
  real(4) bighhh(lbig2ges,*)
  integer(4) ibighhh(lbig2ges,*)
  real(4) ptime(*),ptype(*),pzqm(*)
  integer(8) iplabel(*)
  character(8) pstaid(*)

!------

  call mpi_comm_rank(my_comm,mype,ierr)
! write(0,*)' wrsfcps--mype,mpdata: ',mype,mpdata
  npdatacom=mpdata
  if(mpdata.gt.0) then
   do i=1,mpdata
    all_loc_data(iadpdata(i))%type=ptype(i)
    all_loc_data(iadpdata(i))%staid=pstaid(i)
    all_loc_data(iadpdata(i))%error=perr(i)
    all_loc_data(iadpdata(i))%lon=plon(i)
    all_loc_data(iadpdata(i))%lat=plat(i)
    all_loc_data(iadpdata(i))%pressure=pobs(i)
    all_loc_data(iadpdata(i))%elevobs=pelev(i)
    all_loc_data(iadpdata(i))%tobs=ptobs(i)
    all_loc_data(iadpdata(i))%time=ptime(i)
    all_loc_data(iadpdata(i))%elevges=hges(i)
    all_loc_data(iadpdata(i))%bigh(lbig2ges+1)=hsfc(i)
    all_loc_data(iadpdata(i))%bigh(lbig2ges+2)=etaobssfc(i)
    all_loc_data(iadpdata(i))%bigh(1:lbig2ges)=bighhh(1:lbig2ges,i)
    all_loc_data(iadpdata(i))%ibigh(1:lbig2ges)=ibighhh(1:lbig2ges,i)
    all_loc_data(iadpdata(i))%long=plong(i)
    all_loc_data(iadpdata(i))%latg=platg(i)
    all_loc_data(iadpdata(i))%zqm=pzqm(i)
    all_loc_data(iadpdata(i))%label=iplabel(i)
   end do
  end if

return
end subroutine wrsfcps
