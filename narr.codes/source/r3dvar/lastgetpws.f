subroutine lastgetpws(yopw,eyopw0,xbarbpw,bighpw,ibighpw,mpwdata,lbig2ges,lmetaex,pwdata,pwstaid)

  real(4) yopw(max(1,mpwdata))
  real(4) eyopw0(max(1,mpwdata))
  real(4) xbarbpw(max(1,mpwdata))
  real(4) bighpw(lmetaex,lbig2ges,max(1,mpwdata))
  integer(4) ibighpw(lbig2ges,max(1,mpwdata))
  real(4) pwdata(max(1,mpwdata),6)
  character(8) pwstaid(max(1,mpwdata))

  real(4),pointer::yopw_com(:),eyopw0_com(:),xbarbpw_com(:)
  real(4),pointer::bighpw_com(:,:,:),pwdata_com(:,:)
  character(8),pointer::pwstaid_com(:)
  integer(4),pointer::ibighpw_com(:,:)
  common/r3dv_pwdata/ibighpw_com,yopw_com,eyopw0_com,xbarbpw_com, &
                     bighpw_com,pwdata_com,pwstaid_com

  

  if(mpwdata.gt.0) then
   do i=1,mpwdata
    yopw(i)=yopw_com(i)
    eyopw0(i)=eyopw0_com(i)
    xbarbpw(i)=xbarbpw_com(i)
    do kk=1,lbig2ges
     ibighpw(kk,i)=ibighpw_com(kk,i)
     do k=1,lmetaex
      bighpw(k,kk,i)=bighpw_com(k,kk,i)
     end do
    end do
    do k=1,6
     pwdata(i,k)=pwdata_com(i,k)
    end do
    pwstaid(i)=pwstaid_com(i)
   end do
  end if
!     print *,' inside lastgetpws, max,min(ibighpw)=',maxval(ibighpw),minval(ibighpw)

! deallocate(yopw_com)
! deallocate(eyopw0_com)
! deallocate(xbarbpw_com)
! deallocate(bighpw_com)
! deallocate(ibighpw_com)
! deallocate(pwdata_com)
! deallocate(pwstaid_com)

return
end subroutine lastgetpws
