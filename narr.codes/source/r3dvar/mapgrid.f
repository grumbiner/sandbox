subroutine mapgrid(rx,ry,nx,ny,rlonmap0,rlatmap0,nlonmap,nlatmap, &
                      erlon0,erlat0,dlonmap,dlatmap)
!--------
!-------- define region in earth coordinates to interpolate output
!-------- to for display using grads
!--------
         dimension rx(nx),ry(ny)
!--------
         real(4),allocatable::glon(:,:),glat(:,:),gx(:,:),gy(:,:)
         integer(1),allocatable::itest(:,:)
!--------
         dg2rad=atan(1.)/45.
         cerlat0=cos(erlat0*dg2rad)
         serlat0=sin(erlat0*dg2rad)
         dlat=1.
         dlon=1.
         rlonmin=erlon0-180.
         rlatmin=-89.
         allocate(glon(361,179))
         allocate(glat(361,179))
         do i=1,361
          do j=1,179
           glon(i,j)=rlonmin+(i-1.)*dlon
           glat(i,j)=rlatmin+(j-1.)*dlat
          end do
         end do
         allocate(gx(361,179))
         allocate(gy(361,179))
         call tllv(glon,glat,erlon0,dg2rad,cerlat0,serlat0,gx,gy,361*179)
!! following only works for equally spaced coordinates rx,ry, which is the case here
          dx=rx(2)-rx(1)
          dy=ry(2)-ry(1)
         gx=1.+(gx-rx(1))/dx
         gy=1.+(gy-ry(1))/dy
!        call gdcrdp(gx,361*179,rx,nx)         !  these replaced by
!        call gdcrdp(gy,361*179,ry,ny)         !  above computation
         allocate(itest(361,179))
         itest=0
         do j=1,179
          do i=1,361
           if(gx(i,j).gt.1.01.and.gx(i,j).lt.nx-1.01.and. &
              gy(i,j).gt.1.01.and.gy(i,j).lt.ny-1.01) itest(i,j)=1
          end do
         end do
         deallocate(gx) ; deallocate(gy)
         ileft=1
         iright=361
         itop=179
         ibot=1
         do loop=1,361
          numbot=0
          numtop=0
          do i=ileft,iright
           numbot=numbot+itest(i,ibot)
           numtop=numtop+itest(i,itop)
          end do
          numright=0
          numleft=0
          do i=ibot,itop
           numleft=numleft+itest(ileft,i)
           numright=numright+itest(iright,i)
          end do
          if(numbot*numtop*numleft*numright.gt.0) go to 200
          if(numbot.eq.0) ibot=ibot+1
          if(numtop.eq.0) itop=itop-1
          if(numright.eq.0) iright=iright-1
          if(numleft.eq.0) ileft=ileft+1
         end do
200      continue
         print *,' in mapgrid, ileft,iright=',ileft,iright
         print *,'      ibot,itop=',ibot,itop
         rlatmap0=glat(ileft,ibot)
         rlonmap0=glon(ileft,ibot)
         rlatmap1=glat(ileft,itop)
         rlonmap1=glon(iright,ibot)
!        rlatmap0=max(15.,rlatmap0)
!        rlatmap1=min(60.,rlatmap1)
!        rlonmap0=max(-140.,rlonmap0)
!        rlonmap1=min(-50.,rlonmap1)
         print *,'      rlonmap0,1=',rlonmap0,rlonmap1
         print *,' rlatmap0,1=',rlatmap0,rlatmap1
         nlonmap=nint(1.+(rlonmap1-rlonmap0)/dlonmap)
         nlatmap=nint(1.+(rlatmap1-rlatmap0)/dlatmap)
         print *,'   nlatmap,nlonmap=',nlatmap,nlonmap
         print *,'   erlat0,erlon0=',erlat0,erlon0

  deallocate(glon)
  deallocate(glat)
  deallocate(itest)

       return
       end
