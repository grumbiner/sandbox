subroutine slow_plus_fast(t,p,psi, &
                          lat1,lat2,bight_psi,bighp_psi,wgts_bightp,iwgts_bightp,nxc,nyc,lmetaex)


  real(4) t(nxc,nyc,lmetaex),p(nxc,nyc),psi(nxc,nyc,lmetaex)
  real(4) bight_psi(lmetaex,lmetaex,lat1:lat2),bighp_psi(lmetaex,lat1:lat2)
  real(4) wgts_bightp(2,nxc,nyc)
  integer(4) iwgts_bightp(2,nxc,nyc)

  do k=1,lmetaex
   do kk=1,lmetaex
    do j=1,nyc
     do i=1,nxc
      bight_psi_this=bight_psi(k,kk,iwgts_bightp(1,i,j))*wgts_bightp(1,i,j)+ &
                    bight_psi(k,kk,iwgts_bightp(2,i,j))*wgts_bightp(2,i,j)
      t(i,j,k)=t(i,j,k)+bight_psi_this*psi(i,j,kk)
     end do
    end do
   end do
  end do
  do kk=1,lmetaex
   do j=1,nyc
    do i=1,nxc
     bighp_psi_this=bighp_psi(kk,iwgts_bightp(1,i,j))*wgts_bightp(1,i,j)+ &
                    bighp_psi(kk,iwgts_bightp(2,i,j))*wgts_bightp(2,i,j)
     p(i,j)=p(i,j)+bighp_psi_this*psi(i,j,kk)
    end do
   end do
  end do

return
end subroutine slow_plus_fast
