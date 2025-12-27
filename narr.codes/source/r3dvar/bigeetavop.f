subroutine bigeetavop(psichi,uveta,imeta,jmeta,lmetaex,lbig2data,nxc,nyc,myis2,myie2,myjs2,myje2, &
                         bigeetav,ibigeetav)

  real(4) psichi(nxc*nyc,lmetaex,2),uveta(imeta,jmeta,lmetaex,2)
  real(4) bigeetav(imeta,jmeta,lbig2data,2)
  integer(4) ibigeetav(imeta,jmeta,lbig2data)
  
  call refresh_grid(psichi,nxc,nyc,2*lmetaex,2)
  uveta=0.
  do k=1,lmetaex
   do j=myjs2,myje2
    do m=1,lbig2data
     do i=myis2,myie2
      uveta(i,j,k,1)=uveta(i,j,k,1)-bigeetav(i,j,m,2)*psichi(ibigeetav(i,j,m),k,1) &
                             +bigeetav(i,j,m,1)*psichi(ibigeetav(i,j,m),k,2)
      uveta(i,j,k,2)=uveta(i,j,k,2)+bigeetav(i,j,m,2)*psichi(ibigeetav(i,j,m),k,2) &
                             +bigeetav(i,j,m,1)*psichi(ibigeetav(i,j,m),k,1)
     end do
    end do
   end do
  end do
  call exch(uveta,2*lmetaex,5,5)

return
end subroutine bigeetavop
