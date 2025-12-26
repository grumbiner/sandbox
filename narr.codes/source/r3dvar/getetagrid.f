subroutine getetagrid(xeta,yeta,xetam,yetam,imeta,jmeta,lmeta)

!     obtain lats and lons of eta h grid (degrees) and physical coordinates of eta v grid (meters)

  real(4) xeta(imeta,jmeta+1),yeta(imeta,jmeta+1)
  real(4) xetam(imeta,jmeta+1),yetam(imeta,jmeta+1)

  real(4) etam(lmeta),etai(lmeta+1)
  real(8) wb8,sb8

  call getetacons(lmeta,imeta,jmeta,erlon0,erlat0,dlon0,dlat0,ptop, &
         wb8,sb8,wbglb,sbglb,istagh,istagv,etai,etam)

!   note:  wb,sb are coordinates of sw corner of local domain,
!               imeta,jmeta are dimensions of local domain, and
!                istagh,istagv define sw corner point
!        istagh=0, sw corner is h-point
!              =1, sw corner is v-point
!        istagv=0, sw corner is v-point
!              =1, sw corner is h-point

!------- define lats, lons of eta h-grid (degrees)

   do j=1+istagh,jmeta+1,2
    do i=1,imeta
     call get_ijglb_eta(i,j,iglb,jglb)
     xeta(i,j)=wbglb+2.*(iglb-1)*dlon0
     yeta(i,j)=sbglb+(jglb-1)*dlat0
    end do
   end do
   do j=2-istagh,jmeta+1,2
    do i=1,imeta
     call get_ijglb_eta(i,j,iglb,jglb)
     xeta(i,j)=wbglb+(2*iglb-1)*dlon0
     yeta(i,j)=sbglb+(jglb-1)*dlat0
    end do
   end do

!------- define lats, lons of eta v-grid (meters)

   deg2rad=atan(1.)/45.
   escale=conmc('rerth$')*deg2rad
   do j=2-istagv,jmeta+1,2
    do i=1,imeta
     call get_ijglb_eta(i,j,iglb,jglb)
     xetam(i,j)=wbglb+(2*iglb-1)*dlon0
     yetam(i,j)=sbglb+(jglb-1)*dlat0
    end do
   end do
   do j=1+istagv,jmeta+1,2
    do i=1,imeta
     call get_ijglb_eta(i,j,iglb,jglb)
     xetam(i,j)=wbglb+2.*(iglb-1)*dlon0
     yetam(i,j)=sbglb+(jglb-1)*dlat0
    end do
   end do
   xetam=xetam*escale ; yetam=yetam*escale

return
end subroutine getetagrid
