subroutine verifygrid(inetacons,imeta,jmeta,lmeta,rx,ry,nx,ny,erlat0,erlon0)
!--------
!-------- verify that grid-coordinates for eta-egrid are correct
!--------
         real(4),allocatable::glatin(:,:),glonin(:,:)
         dimension rx(nx),ry(ny)
!-------
         print *,' entering verifygrid, imeta,jmeta=',imeta,jmeta
         print *,' lmeta=',lmeta
         print *,' nx,ny=',nx,ny
         print *,' erlat0,erlon0=',erlat0,erlon0
         d2r=atan(1.)/45.
         r2d=45./atan(1.)
         rewind inetacons
         do l=1,8+2*lmeta+11
          read(inetacons)
         end do
         allocate(glatin(imeta,jmeta))
         read(inetacons)ptop,glatin
         ptop=.01*ptop
         print *,' ptop=',ptop
         allocate(glonin(imeta,jmeta))
         read(inetacons)glonin
         close(inetacons)
!-----------
!----------to start with, just look at 1st row
!----------
         erlonmax=0.
         erlatmax=0.
         j=0
         do jme=1,ny
          j=j+1
          if(j.gt.jmeta) go to 100
!---------------------------even points
          do i=1,imeta
           ime=i
           rlatme=r2d*asin(cos(erlat0*d2r)*sin(ry(jme)*d2r) &
               +sin(erlat0*d2r)*cos(ry(jme)*d2r)*cos(rx(ime)*d2r))
           slonme=cos(ry(jme)*d2r)*sin(rx(ime)*d2r)/cos(rlatme*d2r)
           clonme=(-sin(erlat0*d2r)*sin(ry(jme)*d2r) &
               +cos(erlat0*d2r)*cos(ry(jme)*d2r)*cos(rx(ime)*d2r)) &
                          /cos(rlatme*d2r)
           rlonme=r2d*atan2(slonme,clonme)+erlon0
           glatshouldbe=glatin(i,j)*r2d
           glonshouldbe=-glonin(i,j)*r2d
           erlonmax=max(abs(glonshouldbe-rlonme),erlonmax)
           erlatmax=max(abs(glatshouldbe-rlatme),erlatmax)
          end do
          j=j+1
          if(j.gt.jmeta) go to 100
!---------------------------odd points
          do i=1,imeta-1
           ime=i
           ryodd=.5*(ry(jme)+ry(jme+1))
           rxodd=.5*(rx(ime)+rx(ime+1))
           rlatme=r2d*asin(cos(erlat0*d2r)*sin(ryodd*d2r) &
               +sin(erlat0*d2r)*cos(ryodd*d2r)*cos(rxodd*d2r))
           slonme=cos(ryodd*d2r)*sin(rxodd*d2r)/cos(rlatme*d2r)
           clonme=(-sin(erlat0*d2r)*sin(ryodd*d2r) &
               +cos(erlat0*d2r)*cos(ryodd*d2r)*cos(rxodd*d2r)) &
                          /cos(rlatme*d2r)
           rlonme=r2d*atan2(slonme,clonme)+erlon0
           glatshouldbe=glatin(i,j)*r2d
           glonshouldbe=-glonin(i,j)*r2d
           erlonmax=max(abs(glonshouldbe-rlonme),erlonmax)
           erlatmax=max(abs(glatshouldbe-rlatme),erlatmax)
          end do
         end do
100      continue
         dlon=.5*(rx(2)-rx(1))
         dlat=.5*(ry(2)-ry(1))
         print *,' erlonmax,erlatmax=',erlonmax,erlatmax
         print *,' dlon=',dlon
         print *,' dlat=',dlat
       return
       end
