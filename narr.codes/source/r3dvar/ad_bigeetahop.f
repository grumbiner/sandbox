subroutine ad_bigeetahop(g,geta,imeta,jmeta,lmetaex,lbig2data,nxc,nyc,myis2,myie2,myjs2,myje2, &
                         bigeetah,ibigeetah)

  real(4) g(nxc*nyc,lmetaex),geta(imeta,jmeta,lmetaex)
  real(4) bigeetah(imeta,jmeta,lbig2data)
  integer(4) ibigeetah(imeta,jmeta,lbig2data)
  
  g=0.
  do k=1,lmetaex
   do j=myjs2,myje2
    do m=1,lbig2data
     do i=myis2,myie2
      g(ibigeetah(i,j,m),k)=g(ibigeetah(i,j,m),k)+bigeetah(i,j,m)*geta(i,j,k)
     end do
    end do
   end do
  end do
  call ad_refresh_grid(g,nxc,nyc,lmetaex,2)
  call refresh_grid(g,nxc,nyc,lmetaex,2)

return
end subroutine ad_bigeetahop
