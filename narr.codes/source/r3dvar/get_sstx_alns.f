subroutine get_sstx_alns(sstx,alns,rlatg,rlong,iord,lbig2,lhalf,imetaglb,jmetaglb, &
               wbglb,dlon,sbglb,dlat,istaghglb)

!    obtain sst interpolated to obs location rlatg,rlong
!       also sum of land points at interpolating locations (alns)

  real(4),pointer::save_full_sst(:),save_full_sm(:)
  common/full_sst_sm/save_full_sst,save_full_sm

  real(4) wgtsh(lbig2)
  integer(4) iwgtsh(lbig2)

  ndata=1
  call st_simpin2f(wgtsh,iwgtsh,iflagh,rlong,rlatg,ndata,iord,lhalf,lbig2, &
           wbglb,dlon,sbglb,dlat,imetaglb,jmetaglb,istaghglb)
  sstx=999999.
  alns=lbig2
  if(iflagh.gt.0) then
   sstx=0.
   alns=0.
   do k=1,lbig2
    alns=alns+1.-save_full_sm(iwgtsh(k))
    sstx=sstx+save_full_sst(iwgtsh(k))*wgtsh(k)
   end do
  end if

return
end subroutine get_sstx_alns
