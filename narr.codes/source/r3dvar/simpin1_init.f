subroutine simpin1_init(ixi,tlout,alocalout,blocalout,iord,lbig,x1grid,n1grid)

  ! compute all taylor matrices ever needed for 1-d interpolation on grid x1grid

  ! <-- ixi:            local coordinate order counter
  ! <-- tl:             output taylor matrices
  ! <-- alocal,blocal:  used to transform to local coordinates around each grid-point
  ! --> iord:           order of interpolation (1=linear, 2=quadratic, etc)
  ! --> lbig:           number of interpolating points (=iord+1)
  ! --> x1grid:         coordinates of interpolator grid
  !                         (can be monotonic increasing or decreasing)
  ! --> n1grid:         dimension of interpolator grid

  integer(4) ixi(0:iord)
  real(4) tlout(lbig,lbig,2*n1grid),alocalout(2*n1grid),blocalout(2*n1grid),x1grid(n1grid)
  
  real(4) x1in(2*n1grid)
  dimension dx1gridi(-3:n1grid+3)
  dimension tl(max(64,2*n1grid/lbig),lbig,lbig)
  dimension i1ref(2*n1grid)
  dimension ix1sign(2*n1grid)
  dimension x1temp(2*n1grid),x1p(2*n1grid)
  dimension iflag(2*n1grid)
  dimension alocal(max(64,2*n1grid/lbig)),blocal(max(64,2*n1grid/lbig))
  dimension xmaxlocal(max(64,2*n1grid/lbig)),xminlocal(max(64,2*n1grid/lbig))
  integer, allocatable, dimension(:) :: ix1grid
  
  nin=2*n1grid
  do n=1,n1grid
   nm=max(1,n-1)
   np=min(n1grid,n+1)
   delx1=x1grid(np)-x1grid(np-1)
   x1in(n)=x1grid(n)+.1*delx1
   delx1=x1grid(nm+1)-x1grid(nm)
   x1in(n1grid+n)=x1grid(n)-.1*delx1
  end do
   
  ntl=max(64,nin/lbig)
  ! set ixi, the coordinate order counter
 
  do i=0,iord
   ih=(i+1)/2
   if(2*ih.eq.i) then
    ixi(i)=-ih
   else
    ixi(i)=ih
   end if
!  write(6,100)i,ixi(i)
!  100 format(' xi(',i2,')=',f10.0)
!  print'('' xi('',i2,'')='',i10)',i,ixi(i)
  end do

  !  find and mark all points that are outside interval of interpolation

  iximx=maxval(ixi) ; iximn=minval(ixi)
  iximax=max(abs(iximx),abs(iximn))
  nminleft=abs(iximn)+1 ; nmaxright=n1grid-iximax
  if(iord.eq.1) then
   xboundleft=x1grid(1)
   xboundright=x1grid(n1grid)
  else
   xboundleft=x1grid(1+iximax)-.49999*(x1grid(1+iximax)-x1grid(iximax))
   xboundright=x1grid(n1grid-iximax)+.49999*(x1grid(n1grid-iximax+1)-x1grid(n1grid-iximax))
  end if
  if(x1grid(n1grid).gt.x1grid(1)) then
   iflag=1
   do n=1,nin
    if(x1in(n).le.xboundleft.or.x1in(n).ge.xboundright) iflag(n)=0
   end do
  end if
  if(x1grid(n1grid).lt.x1grid(1)) then
   iflag=1
   do n=1,nin
    if(x1in(n).ge.xboundleft.or.x1in(n).le.xboundright) iflag(n)=0
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
 
  rlow=-epsilon(rlow)
  rhigh=1.+epsilon(rhigh)
  do n=1,nin
   i1fine=1+nint((x1in(n)-x1grid(1))*dxfinei)
   i1fine=max(1,min(i1fine,n1fine))
   i1ref0=max(4,min(ix1grid(i1fine),n1grid-4))
   dxa=(x1in(n)-x1grid(i1ref0-3))*dx1gridi(i1ref0-3)
   dxb=(x1in(n)-x1grid(i1ref0-2))*dx1gridi(i1ref0-2)
   dxc=(x1in(n)-x1grid(i1ref0-1))*dx1gridi(i1ref0-1)
   dxd=(x1in(n)-x1grid(i1ref0  ))*dx1gridi(i1ref0  )
   dxe=(x1in(n)-x1grid(i1ref0+1))*dx1gridi(i1ref0+1)
   dxf=(x1in(n)-x1grid(i1ref0+2))*dx1gridi(i1ref0+2)
   dxg=(x1in(n)-x1grid(i1ref0+3))*dx1gridi(i1ref0+3)
   if(dxa.le.rhigh) i1ref(n)=i1ref0-3
   if(dxb.ge.rlow.and.dxb.le.rhigh) i1ref(n)=i1ref0-2
   if(dxc.ge.rlow.and.dxc.le.rhigh) i1ref(n)=i1ref0-1
   if(dxd.ge.rlow.and.dxd.le.rhigh) i1ref(n)=i1ref0
   if(dxe.ge.rlow.and.dxe.le.rhigh) i1ref(n)=i1ref0+1
   if(dxf.ge.rlow.and.dxf.le.rhigh) i1ref(n)=i1ref0+2
   if(dxg.ge.rlow) i1ref(n)=i1ref0+3
   i1ref(n)=max(nminleft,min(i1ref(n),nmaxright))
  end do
  deallocate (ix1grid)

  !             i1ref now has index of interval containing point
  !               adjust to be closest one to interpolating point
  ix1sign=1
  do n=1,nin
   dxa=(x1in(n)-x1grid(i1ref(n)))*dx1gridi(i1ref(n))
   if(dxa.gt..5) then
    i1ref(n)=min(i1ref(n)+1,nmaxright)
    ix1sign(n)=-1
   end if
  end do

  ! get taylor matrices and invert
  
  nstart=1
  nend=min(nin,ntl)
  do while (nstart.le.nend)
   nthis=nend-nstart+1
 
!  compute limits of local x coordinate

   xmaxlocal=-huge(x) ; xminlocal=huge(x)
   do i=0,iord
    do n=nstart,nend
     if(iflag(n).gt.0) then
      nn=n-nstart+1
      xlocal=x1grid(i1ref(n)+ix1sign(n)*ixi(i))-x1grid(i1ref(n))
      xmaxlocal(nn)=max(xmaxlocal(nn),xlocal)
      xminlocal(nn)=min(xminlocal(nn),xlocal)
     end if
    end do
   end do
   alocal=0.
   blocal=0.
   do n=nstart,nend
    if(iflag(n).gt.0) then 
     nn=n-nstart+1
     alocal(nn)=2./(xmaxlocal(nn)-xminlocal(nn))
     blocal(nn)=-1.-2.*xminlocal(nn)/(xmaxlocal(nn)-xminlocal(nn))
     alocalout(n)=alocal(nn)
     blocalout(n)=blocal(nn)
    end if
   end do

   tl=0.
   do i=1,lbig
    tl(1:nthis,i,i)=1.
   end do
   l=0
   do i=0,iord
    l=l+1
    x1temp(nstart:nend)=0.
    do n=nstart,nend
     nn=n-nstart+1
     x1temp(n)=alocal(nn)*(x1grid(i1ref(n)+ix1sign(n)*ixi(i))-x1grid(i1ref(n))) &
                 +blocal(nn)
    end do
    lp=0
    x1p(nstart:nend)=1.
    do ip=0,iord
     lp=lp+1
     do n=nstart,nend
      if(iflag(n).gt.0) tl(n-nstart+1,l,lp)=x1p(n)
     end do
     x1p(nstart:nend)=x1p(nstart:nend)*x1temp(nstart:nend)
    end do
   end do
 
   call vinvmm(tl,tl,lbig,lbig,lbig,nthis,ntl)

   do n=nstart,nend
    nn=n-nstart+1
    do j=1,lbig
     do i=1,lbig
      tlout(i,j,n)=tl(nn,i,j)
     end do
    end do
   end do
 
   nstart=nstart+ntl
   nend=min(nend+ntl,nin)
 
  end do

return
end subroutine simpin1_init
