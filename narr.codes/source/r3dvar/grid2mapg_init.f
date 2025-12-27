       subroutine grid2mapg_init(nx,ny,rx,ry, &
             nlonmap,nlatmap,rlonmap0,rlatmap0,erlon0,erlat0, &
             dlonmap,dlatmap,iordmap,lbigmap,deltags,epsilongs, &
             deltagu,epsilongu,deltagv,epsilongv,wgts,iwgts,iflag)
!--------
!        initialize stuff for grid2mapg
!--------
         dimension deltags(nlonmap*nlatmap)
         dimension epsilongs(nlonmap*nlatmap)
         dimension deltagu(nlonmap*nlatmap)
         dimension epsilongu(nlonmap*nlatmap)
         dimension deltagv(nlonmap*nlatmap)
         dimension epsilongv(nlonmap*nlatmap)
         dimension wgts(nlonmap*nlatmap*lbigmap)
         dimension iwgts(nlonmap*nlatmap*lbigmap)
         dimension iflag(nlonmap*nlatmap)
!--------
         real(4),allocatable::glon(:,:),glat(:,:),gx(:),gy(:)
         real(4),allocatable::etheta(:)
!--------
         dg2rad=atan(1.)/45.
         cerlat0=cos(erlat0*dg2rad)
         serlat0=sin(erlat0*dg2rad)
!--------
!-------- get grid-units for output grid in terms of input grid
!--------
         allocate(glon(nlonmap,nlatmap))
         allocate(glat(nlonmap,nlatmap))
         do i=1,nlonmap
          do j=1,nlatmap
           glon(i,j)=rlonmap0+(i-1.)*dlonmap
           glat(i,j)=rlatmap0+(j-1.)*dlatmap
          end do
         end do
         allocate(gx(nlonmap*nlatmap)) ; allocate(gy(nlonmap*nlatmap))
         call tllv(glon,glat,erlon0,dg2rad,cerlat0,serlat0,gx,gy,nlonmap*nlatmap)
         deltags=1.
         epsilongs=0.
         allocate(etheta(nlonmap*nlatmap))
         etheta=0.
         call getdelepsv(etheta,glon,glat,erlon0,dg2rad, &
             cerlat0,serlat0,gx,gy,deltagu,epsilongu,nlonmap*nlatmap)
         etheta=90.
         call getdelepsv(etheta,glon,glat,erlon0,dg2rad, &
             cerlat0,serlat0,gx,gy,deltagv,epsilongv,nlonmap*nlatmap)
         deallocate(glon) ; deallocate(glat)
         deallocate(etheta)
         nf=1 ; ndx=0 ; ndy=0 ; ndxx=0 ; ndxy=0 ; ndyy=0
         lhalf=0
         call simpin2f(wgts,wgts,wgts,wgts,wgts,wgts,iwgts,iflag,gx,gy,nlonmap*nlatmap, &
                      iordmap,lhalf,lbigmap,rx,ry,nx,ny,nf,ndx,ndy,ndxx,ndxy,ndyy)
         deallocate(gx) ; deallocate(gy)
       return
       end
