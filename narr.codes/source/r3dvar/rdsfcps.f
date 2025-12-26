subroutine rdsfcps(perr,plon,plat,plong,platg,pobs,pelev,hges, &
               hsfc,etaobssfc,bighhh,ibighhh, &
                   ptobs,ptime,pzqm,ptype,pstaid,iplabel,npdata,lbig2ges)

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

  if(npdata.gt.0) then
   do i=1,npdata
    ptype(i)=all_loc_data(iadpdata(i))%type
    pstaid(i)=all_loc_data(iadpdata(i))%staid
    perr(i)=all_loc_data(iadpdata(i))%error
    plon(i)=all_loc_data(iadpdata(i))%lon
    plat(i)=all_loc_data(iadpdata(i))%lat
    pobs(i)=all_loc_data(iadpdata(i))%pressure
    pelev(i)=all_loc_data(iadpdata(i))%elevobs
    ptobs(i)=all_loc_data(iadpdata(i))%tobs
    ptime(i)=all_loc_data(iadpdata(i))%time
    hges(i)=all_loc_data(iadpdata(i))%elevges
    hsfc(i)=all_loc_data(iadpdata(i))%bigh(lbig2ges+1)
    etaobssfc(i)=all_loc_data(iadpdata(i))%bigh(lbig2ges+2)
    bighhh(1:lbig2ges,i)=all_loc_data(iadpdata(i))%bigh(1:lbig2ges)
    ibighhh(1:lbig2ges,i)=all_loc_data(iadpdata(i))%ibigh(1:lbig2ges)
    plong(i)=all_loc_data(iadpdata(i))%long
    platg(i)=all_loc_data(iadpdata(i))%latg
    pzqm(i)=all_loc_data(iadpdata(i))%zqm
    iplabel(i)=all_loc_data(iadpdata(i))%label
   end do
  end if

return
end
