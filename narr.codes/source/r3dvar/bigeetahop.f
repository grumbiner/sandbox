subroutine bigeetahop(g,geta,imeta,jmeta,lmetaex,lbig2data,nxc,nyc,myis2,myie2,myjs2,myje2, &
                         bigeetah,ibigeetah)

  real(4) g(nxc*nyc,lmetaex),geta(imeta,jmeta,lmetaex)
  real(4) bigeetah(imeta,jmeta,lbig2data)
  integer(4) ibigeetah(imeta,jmeta,lbig2data)
  
  call refresh_grid(g,nxc,nyc,lmetaex,2)
  geta=0.
  do k=1,lmetaex
   do j=myjs2,myje2
    do m=1,lbig2data
     do i=myis2,myie2
      geta(i,j,k)=geta(i,j,k)+bigeetah(i,j,m)*g(ibigeetah(i,j,m),k)
     end do
    end do
   end do
  end do
  call exch(geta,lmetaex,5,5)

return
end subroutine bigeetahop
