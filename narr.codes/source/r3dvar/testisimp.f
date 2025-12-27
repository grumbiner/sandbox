subroutine testisimp

  !  run a test of isimpin1, one-dimensional lagrangian integration.

  integer(4),allocatable::ixi(:),iwgtsi0(:,:),iflag(:),iflagmax(:)
  real(4),allocatable::x1grid(:),tl(:,:,:),alocal(:),blocal(:),wgtsi0(:,:)
  real(4),allocatable::rin(:),sin(:),testu(:),testt(:),xtest(:),wgts(:,:)


  n1grid=11
  allocate(x1grid(n1grid))
  allocate(alocal(2*n1grid))
  allocate(blocal(2*n1grid))

  ntest=101
  allocate(rin(ntest)) ; allocate(sin(ntest)) ; allocate(iflag(ntest))
  allocate(wgts(n1grid,ntest))
  allocate(testu(n1grid))
  allocate(testt(ntest))
  allocate(xtest(ntest))
  allocate(iflagmax(ntest))
 do isign=-1,1,2
  do i=1,n1grid
   x1grid(i)=isign*(-1.+2.*(i-1.)/(n1grid-1.))
    print *,' x1grid(',i,')=',x1grid(i)
  end do
  do i=1,ntest
   xtest(i)=-1.+2.*(i-1.)/(ntest-1.)
  end do
  do iord=1,5
   iordp=iord+1
   iflagmax=0
   lbig=iord+1
   allocate(ixi(0:iord))
   allocate(iwgtsi0(lbig,2*n1grid))
   allocate(tl(lbig,lbig,2*n1grid))
   allocate(wgtsi0(lbig,2*n1grid))
   call isimpin1_init(ixi,tl,alocal,blocal,wgtsi0,iwgtsi0,iord,lbig,x1grid,n1grid)
   do i=1,n1grid
    xthis=x1grid(i)
    if(iordp.eq.1) then
     testu(i)=1.
    else
     um=0. ; u0=1. ; tm=1. ; t0=xthis
     do j=1,iordp-1
      tp=2.*xthis*t0-tm
      up=2.*xthis*u0-um+2.*t0
      tm=t0 ; t0=tp ; um=u0 ; u0=up
     end do
     testu(i)=up
    end if
   end do
   do i=1,ntest
    xthis=xtest(i)
    if(iordp.eq.1) then
     testt(i)=xthis
    else
     tm=1. ; t0=xthis
     do j=1,iordp-1
      tp=2.*xthis*t0-tm
      tm=t0 ; t0=tp
     end do
     testt(i)=tp
    end if
   end do
   errmax=0.
   exactmax=0.
   do ks=1,ntest
    sin(1:ntest)=xtest(ks)
    do kr=1,ntest
     rin(kr)=xtest(kr)
    end do
    call isimpin1(wgts,iflag,rin,sin,ntest,iord,lbig,x1grid,n1grid, &
                ixi,tl,alocal,blocal,wgtsi0,iwgtsi0)
    do kr=1,ntest
     iflagmax(kr)=max(iflag(kr),iflagmax(kr))
     exactint=0. ; compint=0.
     if(iflag(kr).eq.1) then
      exactint=testt(ks)-testt(kr)
      do m=1,n1grid
       compint=compint+wgts(m,kr)*testu(m)
      end do
      errmax=max(errmax,abs(compint-exactint))
      exactmax=max(exactmax,abs(exactint))
     end if
!         if(abs(compint-exactint).gt.1.e-4) &
!              print *,' iord,iflag,rin,sin,exactint,compint=', &
!                   iord,iflag(kr),rin(kr),sin(kr),exactint,compint
    end do
   end do
   print *,' iord,errmax,exactmax=',iord,errmax,exactmax
   print *,' iflagmax=',iflagmax
   deallocate(ixi) ; deallocate(iwgtsi0) ; deallocate(tl) ; deallocate(wgtsi0)
  end do
 end do

return
end subroutine testisimp
    
