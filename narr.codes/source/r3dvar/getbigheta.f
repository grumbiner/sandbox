subroutine getbigheta(bighetah,bighetav,ibighetah,ibighetav,rxc,ryc,nxc,nyc, &
     iordges,lhalfges,lbig2ges,wbglb,dlon,sbglb,dlat,imeta,jmeta,imetaglb,jmetaglb,mype)

!      obtain interpolation constants to go from full eta grid to coarse grid

  real(4) bighetah(nxc*nyc,lbig2ges),bighetav(nxc*nyc,lbig2ges)
  integer(4) ibighetah(nxc*nyc,lbig2ges),ibighetav(nxc*nyc,lbig2ges)
  real(4) rxc(nxc),ryc(nyc)

  real(4),allocatable::rxc2(:,:),ryc2(:,:)
  integer(4),allocatable::iflag(:)

         if(mype.eq.0) write(0,*)' at 1 in getbigheta'
  allocate(rxc2(nxc,nyc))
  allocate(ryc2(nxc,nyc))
         if(mype.eq.0) write(0,*)' at 2 in getbigheta'
  do j=1,nyc
   do i=1,nxc
    rxc2(i,j)=rxc(i)
    ryc2(i,j)=ryc(j)
   end do
  end do
         if(mype.eq.0) write(0,*)' at 3 in getbigheta'
  allocate(iflag(nxc*nyc))
         if(mype.eq.0) write(0,*)' at 4 in getbigheta'
  istaghglb=0
  call st_simpin2f(bighetah,ibighetah,iflag,rxc2,ryc2,nxc*nyc,iordges,lhalfges,lbig2ges, &
               wbglb,dlon,sbglb,dlat,imetaglb,jmetaglb,istaghglb)
         if(mype.eq.0) write(0,*)' at 5 in getbigheta'
  call st_glb2loc_iwgts(ibighetah,nxc*nyc,lbig2ges,imetaglb,jmetaglb,imeta,jmeta)
         if(mype.eq.0) write(0,*)' at 6 in getbigheta'
  istagvglb=1
  call st_simpin2f(bighetav,ibighetav,iflag,rxc2,ryc2,nxc*nyc,iordges,lhalfges,lbig2ges, &
               wbglb,dlon,sbglb,dlat,imetaglb,jmetaglb,istagvglb)
         if(mype.eq.0) write(0,*)' at 7 in getbigheta'
  call st_glb2loc_iwgts(ibighetav,nxc*nyc,lbig2ges,imetaglb,jmetaglb,imeta,jmeta)
         if(mype.eq.0) write(0,*)' at 8 in getbigheta'

  deallocate(rxc2) ; deallocate(ryc2) ; deallocate(iflag)

return
end subroutine getbigheta
