subroutine bighetahop(g,geta,imeta,jmeta,lmetaex,lbig2ges,nxc,nyc,bighetah,ibighetah)

  real(4) g(nxc,nyc,lmetaex),geta(imeta*jmeta,lmetaex)
  real(4) bighetah(nxc,nyc,lbig2ges)
  integer(4) ibighetah(nxc,nyc,lbig2ges)
  
  g=0.
  do k=1,lmetaex
   do j=1,nyc
    do m=1,lbig2ges
     do i=1,nxc
      g(i,j,k)=g(i,j,k)+bighetah(i,j,m)*geta(ibighetah(i,j,m),k)
     end do
    end do
   end do
  end do
  call refresh_grid(g,nxc,nyc,lmetaex,2)

return
end subroutine bighetahop
