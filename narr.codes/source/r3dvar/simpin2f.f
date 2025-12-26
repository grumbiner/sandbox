subroutine simpin2f(wgts,wgtsx1,wgtsx2,wgtsx11,wgtsx12,wgtsx22,iwgts, &
                   iflag,x1in0,x2in0,nin,iord,lhalf,lbig,x1grid0,x2grid0, &
                   n1grid,n2grid,nf,ndx1,ndx2,ndx11,ndx12,ndx22)

! ????????????????????????????????????????????????
! ???????? tested only for value and 1st derivative in each direction, and
! ????????? only for uniform monotonic increasing grid in both coordinate directions
! ????????? also, test not rigorous, but only reproduced results of analysis code
! ?????????   with simpin2 replaced by simpin2f.
! ????????????????????????????????????????????????

  ! compute interpolation weights for 2-d simplex interpolation 
  !             (j. purser scheme)

  !     FAST version, computes in unit-grid space.  derivatives are
  !     obtained, using the chain rule, and simpin1 is used to get the
  !     appropriate modification factors necessary to get correct derivatives.

  ! --> x1in0,x2in0: coordinates of interpolatees
  ! --> nin:       number of interpolatees
  ! --> iord:      order of interpolation (1=linear, 2=quadratic, etc)
  ! --> lhalf:     = 0, then doing diamond interpolation
  !                = 1, then doing traditional square interpolation, which goes 
  !                         part way to the next higher order, because of cross terms
  ! --> lbig:      number of interpolating points
  !                  if lhalf = 0, lbig = (iord*(iord+3)+2)/2
  !                  if lhalf = 1, lbig = (iord+1)**2
  ! --> x1grid0,x2grid0: coordinates of interpolator grid
  !                         (can be monotonic increasing or decreasing)
  ! --> n1grid,n2grid: dimensions of interpolator grid
  ! --> nf:    =1, then return interpolation weights for function f
  ! --> ndx1:  =1, then return interpolation weights for df/dx1
  ! --> ndx2:  =1, then return interpolation weights for df/dx2
  ! --> ndx11: =1, then return interpolation weights for d2f/(dx1*dx1)
  ! --> ndx12: =1, then return interpolation weights for d2f/(dx1*dx2)
  ! --> ndx22: =1, then return interpolation weights for d2f/(dx2*dx2)
  
  ! <-- wgts:     interpolation weights for function ( wgts(nin,lbig) )
  ! <-- wgtsx1:   interpolation weights for df/dx1 ( wgtsx1(nin,lbig) )
  ! <-- wgtsx2:   interpolation weights for df/dx2 ( wgtsx2(nin,lbig) )
  ! <-- wgtsx11:  interpolation weights for d2f/(dx1*dx1)
  ! <-- wgtsx12:  interpolation weights for d2f/(dx1*dx2)
  ! <-- wgtsx22:  interpolation weights for d2f/(dx2*dx2)
  !                 note: if any of these 6 are not asked for, they
  !                       can be dummy arguments
  
  ! <-- iwgts:    absolute grid addresses ( iwgts(nin,lbig) )
  !                 note 1: addresses computed assuming grid field is dimensioned
  !                         grid(n1grid,n2grid)
  !                 note2:  for points too close to edge or outside domain,
  !                          iwgts is set to 1, and wgts is set to 0
  !                           so there is no impact from those points.

  ! <-- iflag:    flag for each interpolatee (iflag(nin))
  !                    =0, then point too close to edge of domain, no weights
  !                                                        computed
  !                    =1, then weights computed   

  dimension x1in0(nin),x2in0(nin),x1grid0(n1grid),x2grid0(n2grid)
  dimension wgts(nin,lbig),iwgts(nin,lbig)
  dimension wgtsx1(nin,lbig),wgtsx2(nin,lbig)
  dimension wgtsx11(nin,lbig),wgtsx12(nin,lbig),wgtsx22(nin,lbig)
  dimension iflag(nin)

  intent(in)    ::x1in0,x2in0,nin,iord,lbig,x1grid0,x2grid0,n1grid,n2grid
  intent(in)    ::nf,ndx1,ndx2,ndx11,ndx12,ndx22
  intent(out)   ::wgts,wgtsx1,wgtsx2,wgtsx11,wgtsx12,wgtsx22,iwgts,iflag

  dimension x1grid(n1grid),x2grid(n2grid)
  real(4),allocatable::x1in(:),x2in(:)
  real(4),allocatable::wgt1(:,:),wgt1x1(:,:),wgt1x11(:,:)
  integer(4),allocatable::iwgt1(:,:),iflag1(:)
  real(4),allocatable::wgt2(:,:),wgt2x2(:,:),wgt2x22(:,:)
  integer(4),allocatable::iwgt2(:,:),iflag2(:)
  real(4),allocatable::du1dx1(:),du1dx11(:)
  real(4),allocatable::du2dx2(:),du2dx22(:)
  real(4),allocatable::wgtsu1(:,:),wgtsu2(:,:)
  dimension tl(4,lbig,lbig)
  dimension ixi(0:iord)
  dimension i1ref(nin),i2ref(nin)
  dimension ix1sign(nin),ix2sign(nin)
  dimension x1p(nin),x12p(nin)
  dimension ix1temp(nin),ix2temp(nin)
  dimension z0(nin,lbig)
  integer(1),allocatable::itype(:)
  integer(4),allocatable::ixi1(:)
  real(4),allocatable::tl1(:,:,:),alocal1(:),blocal1(:)
  
  ! set ixi, the coordinate order counter

  mhalf=1-lhalf
!         write(0,*)' at 1 in simpin2f, lhalf,mhalf,iord,lbig=',lhalf,mhalf,iord,lbig
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

!   convert coordinates to grid units, using simpin1

  allocate(iwgt1(nin,iord+1)) ; allocate(wgt1(nin,iord+1))
  allocate(wgt1x1(nin,iord+1)) ; allocate(wgt1x11(nin,iord+1))
  allocate(iflag1(nin))
  mf1=1
  mdx1=max(ndx1,ndx11,ndx12)
  mdx11=ndx11
  lbig1=iord+1
  allocate(ixi1(0:iord))
  allocate(tl1(lbig1,lbig1,2*n1grid))
  allocate(alocal1(2*n1grid))
  allocate(blocal1(2*n1grid))
  call simpin1_init(ixi1,tl1,alocal1,blocal1,iord,lbig1,x1grid0,n1grid)
  call simpin1(wgt1,wgt1x1,wgt1x11,iwgt1,iflag1,x1in0,nin,iord,iord+1,x1grid0,n1grid,mf1,mdx1,mdx11, &
                    ixi1,tl1,alocal1,blocal1)
  deallocate(ixi1) ; deallocate(tl1) ; deallocate(alocal1) ; deallocate(blocal1)
  do i=1,n1grid
   x1grid(i)=i
  end do
  allocate(x1in(nin))
  x1in=0.
  do k=1,iord+1
   do i=1,nin
    x1in(i)=x1in(i)+wgt1(i,k)*x1grid(iwgt1(i,k))
   end do
  end do

!  obtain coordinate derivatives as needed

  if(mdx1.eq.1) then
   allocate(du1dx1(nin))
   du1dx1=0.
   do k=1,iord+1
    do i=1,nin
     if(iflag1(i).gt.0) du1dx1(i)=du1dx1(i)+wgt1x1(i,k)*x1grid(iwgt1(i,k))
    end do
   end do
  end if
  if(mdx11.eq.1) then
   allocate(du1dx11(nin))
   du1dx11=0.
   do k=1,iord+1
    do i=1,nin
     if(iflag1(i).gt.0) du1dx11(i)=du1dx11(i)+wgt1x11(i,k)*x1grid(iwgt1(i,k))
    end do
   end do
  end if
  deallocate(iwgt1) ; deallocate(wgt1) ; deallocate(wgt1x1) ; deallocate(wgt1x11)

  allocate(iwgt2(nin,iord+1)) ; allocate(wgt2(nin,iord+1))
  allocate(wgt2x2(nin,iord+1)) ; allocate(wgt2x22(nin,iord+1))
  allocate(iflag2(nin))
  mf2=1
  mdx2=max(ndx2,ndx22,ndx12)
  mdx22=ndx22
  allocate(ixi1(0:iord))
  allocate(tl1(lbig1,lbig1,2*n2grid))
  allocate(alocal1(2*n2grid))
  allocate(blocal1(2*n2grid))
  call simpin1_init(ixi1,tl1,alocal1,blocal1,iord,lbig1,x2grid0,n2grid)
  call simpin1(wgt2,wgt2x2,wgt2x22,iwgt2,iflag2,x2in0,nin,iord,iord+1,x2grid0,n2grid,mf2,mdx2,mdx22, &
                    ixi1,tl1,alocal1,blocal1)
  deallocate(ixi1) ; deallocate(tl1) ; deallocate(alocal1) ; deallocate(blocal1)
  do i=1,n2grid
   x2grid(i)=i
  end do
  allocate(x2in(nin))
  x2in=0.
  do k=1,iord+1
   do i=1,nin
    x2in(i)=x2in(i)+wgt2(i,k)*x2grid(iwgt2(i,k))
   end do
  end do

!  obtain coordinate derivatives as needed

  if(mdx2.eq.1) then
   allocate(du2dx2(nin))
   du2dx2=0.
   do k=1,iord+1
    do i=1,nin
     if(iflag2(i).gt.0) du2dx2(i)=du2dx2(i)+wgt2x2(i,k)*x2grid(iwgt2(i,k))
    end do
   end do
  end if
  if(mdx22.eq.1) then
   allocate(du2dx22(nin))
   du2dx22=0.
   do k=1,iord+1
    do i=1,nin
     if(iflag2(i).gt.0) du2dx22(i)=du2dx22(i)+wgt2x22(i,k)*x2grid(iwgt2(i,k))
    end do
   end do
  end if
  deallocate(iwgt2) ; deallocate(wgt2) ; deallocate(wgt2x2) ; deallocate(wgt2x22)


  !  get i1ref, i2ref, indices of nearest grid point to each interpolatee

  ix1sign=1
  ix2sign=1
  iximx=maxval(ixi) ; iximn=minval(ixi)
  iximax=max(abs(iximx),abs(iximn))
  nminleft=abs(iximn)+1 ; nmax1right=n1grid-iximax
  nmax2right=n2grid-iximax
  do i=1,nin
   i1ref(i)=nint(x1in(i))
   i1ref(i)=max(nminleft,min(i1ref(i),nmax1right))
   if(x1in(i)-i1ref(i).lt.0._4) ix1sign(i)=-1
   i2ref(i)=nint(x2in(i))
   i2ref(i)=max(nminleft,min(i2ref(i),nmax2right))
   if(x2in(i)-i2ref(i).lt.0._4) ix2sign(i)=-1
  end do

   

! get interpolation indices

!         write(0,*)' at 2 in simpin2f, lhalf,mhalf,iord,lbig=',lhalf,mhalf,iord,lbig
  l=0
  iflag=1
  do i=0,iord
   ix1temp=0
   do n=1,nin
    if(i1ref(n).le.0) then
     iflag(n)=0
    else
     ix1temp(n)=i1ref(n)+ix1sign(n)*ixi(i)
     if(ix1temp(n).lt.1.or.ix1temp(n).gt.n1grid) iflag(n)=0
    end if
   end do
   do j=0,iord-i*mhalf
    l=l+1
    ix2temp=0
    do n=1,nin
     if(i2ref(n).le.0) iflag(n)=0
     ix2temp(n)=i2ref(n)+ix2sign(n)*ixi(j)
     if(ix2temp(n).lt.1.or.ix2temp(n).gt.n2grid) iflag(n)=0
     iwgts(n,l)=(ix2temp(n)-1)*n1grid+ix1temp(n)
    end do
   end do
  end do
  do n=1,nin
   iflag(n)=min(iflag(n),iflag1(n),iflag2(n))
  end do
  deallocate(iflag1) ; deallocate(iflag2)
  do l=1,lbig
   do n=1,nin
    if(iflag(n).eq.0) then
     iwgts(n,l)=1
    end if
   end do
  end do

!         write(0,*)' at 3 in simpin2f, lhalf,mhalf,iord,lbig=',lhalf,mhalf,iord,lbig

! get taylor matrices and invert
  
  ix10=n1grid/2
  ix20=n2grid/2
  n=0
  do ix1sign0=-1,1,2
   do ix2sign0=-1,1,2
    n=n+1
    l=0
    do i=0,iord
     x1temp=x1grid(ix10+ix1sign0*ixi(i))-x1grid(ix10)
     do j=0,iord-i*mhalf
      l=l+1
      x2temp=x2grid(ix20+ix2sign0*ixi(j))-x2grid(ix20)
      lp=0
      x1p0=1.
      do ip=0,iord
       x12p0=x1p0
       do jp=0,iord-ip*mhalf
        lp=lp+1
        tl(n,l,lp)=x12p0
        x12p0=x12p0*x2temp
       end do
       x1p0=x1p0*x1temp
      end do
     end do
    end do
   end do
  end do
  call vinvmm(tl,tl,lbig,lbig,lbig,4,4)

!         write(0,*)' at 4 in simpin2f, lhalf,mhalf,iord,lbig=',lhalf,mhalf,iord,lbig
  ! get type number of each point

  allocate(itype(nin))
  do n=1,nin
   if(ix1sign(n).eq.-1.and.ix2sign(n).eq.-1) itype(n)=1
   if(ix1sign(n).eq.-1.and.ix2sign(n).eq. 1) itype(n)=2
   if(ix1sign(n).eq. 1.and.ix2sign(n).eq.-1) itype(n)=3
   if(ix1sign(n).eq. 1.and.ix2sign(n).eq. 1) itype(n)=4
  end do

  ! get taylor vector(s)

!         write(0,*)' at 5 in simpin2f, lhalf,mhalf,iord,lbig=',lhalf,mhalf,iord,lbig
  if(nf.eq.1) then  ! no derivative
   lp=0
   x1p=1.
   do ip=0,iord
    x12p=x1p
    do jp=0,iord-ip*mhalf
     lp=lp+1
     z0(1:nin,lp)=x12p(1:nin)
     x12p(1:nin)=x12p(1:nin)*  &
                 (x2in(1:nin)-x2grid(max(1,i2ref(1:nin))))
    end do
    x1p(1:nin)=x1p(1:nin)*  &
                 (x1in(1:nin)-x1grid(max(1,i1ref(1:nin))))
   end do

   ! get interpolation weights

   do k=1,lbig
    wgts(1:nin,k)=0.
    do j=1,lbig
     do n=1,nin
      if(iflag(n).gt.0) wgts(n,k)=wgts(n,k)+z0(n,j)*tl(itype(n),j,k)
     end do
    end do
   end do

  end if

  if(ndx1.eq.1.or.ndx11.eq.1) then     ! 1st derivative in 1st direction
   allocate(wgtsu1(nin,lbig))
   lp=0
   x1p=1.
   do ip=0,iord
    x12p=ip*x1p
    do jp=0,iord-ip*mhalf
     lp=lp+1
     z0(1:nin,lp)=x12p(1:nin)
     x12p(1:nin)=x12p(1:nin)*  &
                 (x2in(1:nin)-x2grid(max(1,i2ref(1:nin))))
    end do
    if(ip.gt.0) &
      x1p(1:nin)=x1p(1:nin)*  &
                 (x1in(1:nin)-x1grid(max(1,i1ref(1:nin))))
   end do

   ! get interpolation weights

   do k=1,lbig
    wgtsu1(1:nin,k)=0.
    do j=1,lbig
     do n=1,nin
      if(iflag(n).gt.0) wgtsu1(n,k)=wgtsu1(n,k)+z0(n,j)*tl(itype(n),j,k)
     end do
    end do
   end do

   ! now need to multiply by du1/dx1, the derivative of unit grid wrt actual grid
   !   in order to get correct derivative

   if(ndx1.eq.1) then
    do k=1,lbig
     do i=1,nin
      wgtsx1(i,k)=du1dx1(i)*wgtsu1(i,k)
     end do
    end do
   end if
  end if

  if(ndx2.eq.1.or.ndx22.eq.1) then     ! 1st derivative in 2nd direction
   allocate(wgtsu2(nin,lbig))
   lp=0
   x1p=1.
   do ip=0,iord
    x12p=x1p
    do jp=0,iord-ip*mhalf
     lp=lp+1
     z0(1:nin,lp)=jp*x12p(1:nin)
     if(jp.gt.0) &
       x12p(1:nin)=x12p(1:nin)*  &
                 (x2in(1:nin)-x2grid(max(1,i2ref(1:nin))))
    end do
    x1p(1:nin)=x1p(1:nin)*  &
                 (x1in(1:nin)-x1grid(max(1,i1ref(1:nin))))
   end do

   ! get interpolation weights

   do k=1,lbig
    wgtsu2(1:nin,k)=0.
    do j=1,lbig
     do n=1,nin
      if(iflag(n).gt.0) wgtsu2(n,k)=wgtsu2(n,k)+z0(n,j)*tl(itype(n),j,k)
     end do
    end do
   end do

   ! now need to multiply by du2/dx2, the derivative of unit grid wrt actual grid
   !   in order to get correct derivative

   if(ndx2.eq.1) then
    do k=1,lbig
     do i=1,nin
      wgtsx2(i,k)=du2dx2(i)*wgtsu2(i,k)
     end do
    end do
   end if
  end if


  if(ndx11.eq.1) then     ! 2nd derivative in 1st direction
   lp=0
   x1p=1.
   do ip=0,iord
    x12p=ip*(ip-1)*x1p
    do jp=0,iord-ip*mhalf
     lp=lp+1
     z0(1:nin,lp)=x12p(1:nin)
     x12p(1:nin)=x12p(1:nin)*  &
                  (x2in(1:nin)-x2grid(max(1,i2ref(1:nin))))
    end do
    if(ip.gt.1) &
     x1p(1:nin)=x1p(1:nin)*  &
                  (x1in(1:nin)-x1grid(max(1,i1ref(1:nin))))
   end do

   ! get interpolation weights

   do k=1,lbig
    wgtsx11(1:nin,k)=0.
    do j=1,lbig
     do n=1,nin
      if(iflag(n).gt.0) wgtsx11(n,k)=wgtsx11(n,k)+z0(n,j)*tl(itype(n),j,k)
     end do
    end do
   end do

   ! now need to do a little computation to get final derivatives

   do k=1,lbig
    do i=1,nin
     wgtsx11(i,k)=wgtsx11(i,k)*du1dx1(i)**2+wgtsu1(i,k)*du1dx11(i)
    end do
   end do
  end if

  if(ndx12.eq.1) then     ! 1st derivative in both directions
   lp=0
   x1p=1.
   do ip=0,iord
    x12p=ip*x1p
    do jp=0,iord-ip*mhalf
     lp=lp+1
     z0(1:nin,lp)=jp*x12p(1:nin)
     if(jp.gt.0) &
     x12p(1:nin)=x12p(1:nin)*  &
                  (x2in(1:nin)-x2grid(max(1,i2ref(1:nin))))
    end do
    if(ip.gt.0) &
     x1p(1:nin)=x1p(1:nin)*  &
                  (x1in(1:nin)-x1grid(max(1,i1ref(1:nin))))
   end do

   ! get interpolation weights

   do k=1,lbig
    wgtsx12(1:nin,k)=0.
    do j=1,lbig
     do n=1,nin
      if(iflag(n).gt.0) wgtsx12(n,k)=wgtsx12(n,k)+z0(n,j)*tl(itype(n),j,k)
     end do
    end do
   end do

   ! now get final derivatives

   do k=1,lbig
    do i=1,nin
     wgtsx12(i,k)=wgtsx12(i,k)*du1dx1(i)*du2dx2(i)
    end do
   end do
  end if

  if(ndx22.eq.1) then     ! 2nd derivative in 2nd direction
   lp=0
   x1p=1.
   do ip=0,iord
    x12p=x1p
    do jp=0,iord-ip*mhalf
     lp=lp+1
     z0(1:nin,lp)=jp*(jp-1)*x12p(1:nin)
     if(jp.gt.1) &
       x12p(1:nin)=x12p(1:nin)*  &
                 (x2in(1:nin)-x2grid(max(1,i2ref(1:nin))))
    end do
    x1p(1:nin)=x1p(1:nin)*  &
                 (x1in(1:nin)-x1grid(max(1,i1ref(1:nin))))
   end do

   ! get interpolation weights

   do k=1,lbig
    wgtsx22(1:nin,k)=0.
    do j=1,lbig
     do n=1,nin
      if(iflag(n).gt.0) wgtsx22(n,k)=wgtsx22(n,k)+z0(n,j)*tl(itype(n),j,k)
     end do
    end do
   end do

   ! now need to do a little computation to get final derivatives

   do k=1,lbig
    do i=1,nin
     wgtsx22(i,k)=wgtsx22(i,k)*du2dx2(i)**2+wgtsu2(i,k)*du2dx22(i)
    end do
   end do
  end if

  deallocate(x1in)
  if(mdx1.eq.1) then
   deallocate(du1dx1)
  end if
  if(mdx11.eq.1) then
   deallocate(du1dx11)
  end if
  deallocate(x2in)
  if(mdx2.eq.1) then
   deallocate(du2dx2)
  end if
  if(mdx22.eq.1) then
   deallocate(du2dx22)
  end if
  deallocate(itype)
  if(ndx1.eq.1.or.ndx11.eq.1) then
   deallocate(wgtsu1)
  end if
  if(ndx2.eq.1.or.ndx22.eq.1) then
   deallocate(wgtsu2)
  end if

return
end subroutine simpin2f
