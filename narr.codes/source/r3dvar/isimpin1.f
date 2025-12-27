subroutine isimpin1(wgts,iflag,rin,sin,nin,iord,lbig,x1grid,n1grid, &
                    ixi,tl,alocal,blocal,wgtsi0,iwgtsi0)

  ! compute integration weights for 1-d integration

  ! --> rin:      coordinates of starting limit of integration
  ! --> sin:      coordinates of ending limit of integration
  ! --> nin:       number of integrations to perform
  ! --> iord:      order of integration (1=linear, 2=quadratic, etc)
  ! --> lbig:      number of interpolating points (=iord+1)
  ! --> x1grid:    coordinates of interpolator grid
  !                         (can be monotonic increasing or decreasing)
  ! --> n1grid:    dimension of interpolator grid

  ! <-- wgts:     integration weights for function ( wgts(n1grid,nin) )

  ! <-- iflag:    flag for each interpolatee (iflag(nin))
  !                 =0, then one or both integration limits too close to edge (or outside) of domain.
  !                               In this case, wgts is set to zero.
  !                     or, sin is equal or to left of rin, in which case also, integral is assumed 0
  !                     so wgts again set to zero.
  !                 =1, then weights computed.
  ! --> ixi,tl:   constants precomputed by
  ! --> alocal:       isimpin1_init--depend
  ! --> blocal:       only on
  ! --> wgtsi0:       iord  and
  ! --> iwgtsi0:      x1grid

  dimension rin(nin),sin(nin),x1grid(n1grid)
  dimension wgts(n1grid,nin)
  dimension ixi(0:iord)
  dimension tl(lbig,lbig,n1grid,2),alocal(n1grid,2),blocal(n1grid,2)
  dimension wgtsi0(lbig,n1grid,2),iwgtsi0(lbig,n1grid,2)
  dimension iflag(nin)

  dimension dx1gridi(-3:n1grid+3)
  dimension i1refr(nin),i1refs(nin)
  dimension ix1signr(nin),ix1signs(nin)
  dimension iwgtsr(lbig,nin),iwgtss(lbig,nin)
  integer, allocatable, dimension(:) :: ix1grid,iwmax(:),iwmin(:)
  
  ntl=max(64,nin/lbig)

  !  find and mark all points that are outside interval of interpolation

  iximx=maxval(ixi) ; iximn=minval(ixi)
  iximax=max(abs(iximx),abs(iximn))
! print *,' iximx,mn=',iximx,iximn
! print *,' iximax=',iximax
! print *,' ixi=',ixi
  nminleft=abs(iximn)+1 ; nmaxright=n1grid-iximax
  if(iord.eq.1) then
   xboundleft=x1grid(1)
   xboundright=x1grid(n1grid)
  else
   xboundleft=x1grid(1+iximax)-.49999*(x1grid(1+iximax)-x1grid(iximax))
   xboundright=x1grid(n1grid-iximax)+.49999*(x1grid(n1grid-iximax+1)-x1grid(n1grid-iximax))
  end if
!   print *, ' xboundleft=',xboundleft
!   print *, ' xboundright=',xboundright
!   do i=1,iximax+1
!    print *, ' x1grid(',i,')=',x1grid(i)
!   end do
!   do i=n1grid,n1grid-iximax,-1
!    print *,' x1grid(',i,')=',x1grid(i)
!   end do
  if(x1grid(n1grid).gt.x1grid(1)) then
   iflag=1
   do n=1,nin
    if(rin(n).le.xboundleft.or.rin(n).ge.xboundright) iflag(n)=0
    if(sin(n).le.xboundleft.or.sin(n).ge.xboundright) iflag(n)=0
    if(rin(n).ge.sin(n)) iflag(n)=0
   end do
  end if
  if(x1grid(n1grid).lt.x1grid(1)) then
   iflag=1
   do n=1,nin
    if(rin(n).ge.xboundleft.or.rin(n).le.xboundright) iflag(n)=0
    if(sin(n).ge.xboundleft.or.sin(n).le.xboundright) iflag(n)=0
    if(rin(n).le.sin(n)) iflag(n)=0
   end do
  end if

  !  set up uniform fine grid to use in finding interpolation coordinates
 
  dxmax=-huge(x) ; dxmin=huge(x)
  do i=1,n1grid-1
   dxthis=x1grid(i+1)-x1grid(i)
   dx1gridi(i)=1./dxthis
   dxmax=max(dxthis,dxmax) ; dxmin=min(dxthis,dxmin)
  end do
  dx1gridi(-3:0)=dx1gridi(1)
  dx1gridi(n1grid:n1grid+3)=dx1gridi(n1grid-1)
  if(dxmax*dxmin.le.0.) then
   print *,' INTERPOLATION GRID NOT MONOTONIC IN SIMPIN1'
   stop
  end if
  dxminmin=min(abs(dxmax),abs(dxmin))
  dxfine=sign(dxminmin,dxmax)
  dxfinei=1./dxfine
  n1fine=1+ceiling((x1grid(n1grid)-x1grid(1))/dxfine)
!      print *,' in simpin1, n1grid,n1fine=',n1grid,n1fine
  allocate (ix1grid(n1fine))
  ii=1
  do i=1,n1fine
   x1ref=x1grid(1)+(i-1)*dxfine
   if(dxfinei*(x1ref-x1grid(ii+1)).ge.-.001) ii=min(ii+1,n1grid-1)
   ix1grid(i)=ii
!       print *,' x1fine,x1grid=',x1ref,x1grid(ix1grid(i))
  end do
 
  ! now get i1ref, index of interval containing point
 
  i1refr=-1
  do n=1,nin
   i1fine=1+nint((rin(n)-x1grid(1))*dxfinei)
   i1fine=max(1,min(i1fine,n1fine))
   i1ref0=max(4,min(ix1grid(i1fine),n1grid-4))
   dxa=(rin(n)-x1grid(i1ref0-3))*dx1gridi(i1ref0-3)
   dxb=(rin(n)-x1grid(i1ref0-2))*dx1gridi(i1ref0-2)
   dxc=(rin(n)-x1grid(i1ref0-1))*dx1gridi(i1ref0-1)
   dxd=(rin(n)-x1grid(i1ref0  ))*dx1gridi(i1ref0  )
   dxe=(rin(n)-x1grid(i1ref0+1))*dx1gridi(i1ref0+1)
   dxf=(rin(n)-x1grid(i1ref0+2))*dx1gridi(i1ref0+2)
   dxg=(rin(n)-x1grid(i1ref0+3))*dx1gridi(i1ref0+3)
   if(dxa.le.1.) i1refr(n)=i1ref0-3
   if(dxb.ge.0..and.dxb.le.1.) i1refr(n)=i1ref0-2
   if(dxc.ge.0..and.dxc.le.1.) i1refr(n)=i1ref0-1
   if(dxd.ge.0..and.dxd.le.1.) i1refr(n)=i1ref0
   if(dxe.ge.0..and.dxe.le.1.) i1refr(n)=i1ref0+1
   if(dxf.ge.0..and.dxf.le.1.) i1refr(n)=i1ref0+2
   if(dxg.ge.0.) i1refr(n)=i1ref0+3
   i1refr(n)=max(nminleft,min(i1refr(n),nmaxright))
  end do
  i1refs=-1
  do n=1,nin
   i1fine=1+nint((sin(n)-x1grid(1))*dxfinei)
   i1fine=max(1,min(i1fine,n1fine))
   i1ref0=max(4,min(ix1grid(i1fine),n1grid-4))
   dxa=(sin(n)-x1grid(i1ref0-3))*dx1gridi(i1ref0-3)
   dxb=(sin(n)-x1grid(i1ref0-2))*dx1gridi(i1ref0-2)
   dxc=(sin(n)-x1grid(i1ref0-1))*dx1gridi(i1ref0-1)
   dxd=(sin(n)-x1grid(i1ref0  ))*dx1gridi(i1ref0  )
   dxe=(sin(n)-x1grid(i1ref0+1))*dx1gridi(i1ref0+1)
   dxf=(sin(n)-x1grid(i1ref0+2))*dx1gridi(i1ref0+2)
   dxg=(sin(n)-x1grid(i1ref0+3))*dx1gridi(i1ref0+3)
   if(dxa.le.1.) i1refs(n)=i1ref0-3
   if(dxb.ge.0..and.dxb.le.1.) i1refs(n)=i1ref0-2
   if(dxc.ge.0..and.dxc.le.1.) i1refs(n)=i1ref0-1
   if(dxd.ge.0..and.dxd.le.1.) i1refs(n)=i1ref0
   if(dxe.ge.0..and.dxe.le.1.) i1refs(n)=i1ref0+1
   if(dxf.ge.0..and.dxf.le.1.) i1refs(n)=i1ref0+2
   if(dxg.ge.0.) i1refs(n)=i1ref0+3
   i1refs(n)=max(nminleft,min(i1refs(n),nmaxright))
  end do
  deallocate (ix1grid)
!          print *,' max,min(i1refr)=',maxval(i1refr),minval(i1refr)
!          print *,' max,min(i1refs)=',maxval(i1refs),minval(i1refs)


  !             i1ref now has index of interval containing point
  !               adjust to be closest one to interpolating point
  ix1signr=1
  do n=1,nin
   dxa=(rin(n)-x1grid(i1refr(n)))*dx1gridi(i1refr(n))
   if(dxa.gt..5.and.i1refr(n).le.nmaxright) then
    i1refr(n)=i1refr(n)+1
    ix1signr(n)=-1
   end if
  end do
  ix1signs=1
  do n=1,nin
   dxa=(sin(n)-x1grid(i1refs(n)))*dx1gridi(i1refs(n))
   if(dxa.gt..5.and.i1refs(n).le.nmaxright) then
    i1refs(n)=i1refs(n)+1
    ix1signs(n)=-1
   end if
  end do

  ! get interpolation indices

  l=0
  allocate(iwmax(nin)) ; allocate(iwmin(nin))
  iwmax=0
  iwmin=n1grid
  do i=0,iord
   l=l+1
   do n=1,nin
    iwgtsr(l,n)=i1refr(n)+ix1signr(n)*ixi(i)
    iwgtss(l,n)=i1refs(n)+ix1signs(n)*ixi(i)
    iwmax(n)=max(iwgtsr(l,n),iwgtss(l,n),iwmax(n))
    iwmin(n)=min(iwgtsr(l,n),iwgtss(l,n),iwmin(n))
    iwgtsr(l,n)=max(1,min(iwgtsr(l,n),n1grid))
    iwgtss(l,n)=max(1,min(iwgtss(l,n),n1grid))
   end do
  end do
  do n=1,nin
   if(iwmax(n).gt.n1grid.or.iwmin(n).lt.1) iflag(n)=0
  end do
  deallocate(iwmax) ; deallocate(iwmin)
  
!   print *,' max,min(iwgtsr)=',maxval(iwgtsr),minval(iwgtsr)
!   print *,' max,min(iwgtss)=',maxval(iwgtss),minval(iwgtss)
!   print *,' both of above should be >= 1 and <= ',n1grid 
    
 
!  now add up integration weights

  wgts=0.
  do n=1,nin
   if(iflag(n).gt.0) then

!     start with left limit

    r=rin(n)
    if(ix1signr(n).eq.1) then
     iflip=1
     s=x1grid(i1refr(n))+.5*(x1grid(i1refr(n)+1)-x1grid(i1refr(n)))
    else
     iflip=2
     s=x1grid(i1refr(n))
    end if
    if(x1grid(n1grid).gt.x1grid(1)) s=min(s,sin(n))
    if(x1grid(n1grid).lt.x1grid(1)) s=max(s,sin(n))
    rp=alocal(i1refr(n),iflip)*(r-x1grid(i1refr(n)))+blocal(i1refr(n),iflip)
    sp=alocal(i1refr(n),iflip)*(s-x1grid(i1refr(n)))+blocal(i1refr(n),iflip)
    l=0
    do i=0,iord
     l=l+1
     tlint=(sp**l-rp**l)/(l*alocal(i1refr(n),iflip))
     do k=1,iord+1
      wgts(iwgtsr(k,n),n)=wgts(iwgtsr(k,n),n)+tl(l,k,i1refr(n),iflip)*tlint
     end do
    end do
    if(s.ne.sin(n)) then   !  s = sin, then we are done

! next intermediate intervals, if any

     if(ix1signr(n).eq.1.and.ix1signs(n).eq.1) then
      ileftp=i1refr(n)+1
      irightp=i1refs(n)-1
      ileftn=i1refr(n)+1
      irightn=i1refs(n)
     end if
     if(ix1signr(n).eq.1.and.ix1signs(n).eq.-1) then
      ileftp=i1refr(n)+1
      irightp=i1refs(n)-1
      ileftn=i1refr(n)+1
      irightn=i1refs(n)-1
     end if
     if(ix1signr(n).eq.-1.and.ix1signs(n).eq.1) then
      ileftp=i1refr(n)
      irightp=i1refs(n)-1
      ileftn=i1refr(n)+1
      irightn=i1refs(n)
     end if
     if(ix1signr(n).eq.-1.and.ix1signs(n).eq.-1) then
      ileftp=i1refr(n)
      irightp=i1refs(n)-1
      ileftn=i1refr(n)+1
      irightn=i1refs(n)-1
     end if
     if(ileftp.le.irightp) then
      do j=ileftp,irightp
       do k=1,iord+1
        wgts(iwgtsi0(k,j,1),n)=wgts(iwgtsi0(k,j,1),n)+wgtsi0(k,j,1)
       end do
      end do
     end if
     if(ileftn.le.irightn) then
      do j=ileftn,irightn
       do k=1,iord+1
        wgts(iwgtsi0(k,j,2),n)=wgts(iwgtsi0(k,j,2),n)+wgtsi0(k,j,2)
       end do
      end do
     end if

! finally finish with right limit

     s=sin(n)
     if(ix1signs(n).eq.1) then
      iflip=1
      r=x1grid(i1refs(n))
     else
      iflip=2
      r=x1grid(i1refs(n)-1)+.5*(x1grid(i1refs(n))-x1grid(i1refs(n)-1))
     end if
     rp=alocal(i1refs(n),iflip)*(r-x1grid(i1refs(n)))+blocal(i1refs(n),iflip)
     sp=alocal(i1refs(n),iflip)*(s-x1grid(i1refs(n)))+blocal(i1refs(n),iflip)
     l=0
     do i=0,iord
      l=l+1
      tlint=(sp**l-rp**l)/(l*alocal(i1refs(n),iflip))
      do k=1,iord+1
       wgts(iwgtss(k,n),n)=wgts(iwgtss(k,n),n)+tl(l,k,i1refs(n),iflip)*tlint
      end do
     end do

    end if
   end if
  end do

return
end subroutine isimpin1
