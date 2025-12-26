subroutine simpin1(wgts,wgtsx1,wgtsx11,iwgts, &
           iflag,x1in,nin,iord,lbig,x1grid,n1grid,nf,ndx1,ndx11,ixi,tl,alocal,blocal)

  ! compute interpolation weights for 1-d interpolation

  ! --> x1in:      coordinates of interpolatees
  ! --> nin:       number of interpolatees
  ! --> iord:      order of interpolation (1=linear, 2=quadratic, etc)
  ! --> lbig:      number of interpolating points (=iord+1)
  ! --> x1grid:    coordinates of interpolator grid
  !                         (can be monotonic increasing or decreasing)
  ! --> n1grid:    dimension of interpolator grid
  ! --> nf:    =1, then return interpolation weights for function f
  ! --> ndx1:  =1, then return interpolation weights for df/dx1
  ! --> ndx11: =1, then return interpolation weights for d2f/(dx1*dx1)

  ! <-- wgts:     interpolation weights for function ( wgts(nin,lbig) )
  ! <-- wgtsx1:   interpolation weights for df/dx1
  ! <-- wgtsx11:  interpolation weights for d2f/(dx1*dx1)
  !                 note: if any of these 3 are not asked for, they
  !                       can be dummy arguments

  ! <-- iwgts:    absolute grid addresses ( iwgts(nin,lbig) )

  ! <-- iflag:    flag for each interpolatee (iflag(nin))
  !                    =0, then point too close to edge of domain, but weights
  !                                                   are computed anyway, incase
  !                                    it is considered OK to extrapolate.
  !                    =1, then weights computed

  dimension x1in(nin),x1grid(n1grid)
  dimension wgts(nin,lbig),iwgts(nin,lbig)
  dimension wgtsx1(nin,lbig),wgtsx11(nin,lbig)
  dimension ixi(0:iord)
  dimension tl(lbig,lbig,n1grid,2),alocal(n1grid,2),blocal(n1grid,2)

  dimension dx1gridi(-3:n1grid+3)
  dimension i1ref(nin)
  dimension ix1sign(nin)
  dimension x1temp(nin),x1p(nin)
  dimension ix1temp(nin)
  dimension iflag(nin)
  dimension z0(max(64,nin/lbig),lbig)
  integer, allocatable, dimension(:) :: ix1grid
  
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
 
  i1ref=-1
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
   if(dxa.le.1.) i1ref(n)=i1ref0-3
   if(dxb.ge.0..and.dxb.le.1.) i1ref(n)=i1ref0-2
   if(dxc.ge.0..and.dxc.le.1.) i1ref(n)=i1ref0-1
   if(dxd.ge.0..and.dxd.le.1.) i1ref(n)=i1ref0
   if(dxe.ge.0..and.dxe.le.1.) i1ref(n)=i1ref0+1
   if(dxf.ge.0..and.dxf.le.1.) i1ref(n)=i1ref0+2
   if(dxg.ge.0.) i1ref(n)=i1ref0+3
   i1ref(n)=max(nminleft,min(i1ref(n),nmaxright))
  end do
  deallocate (ix1grid)
!          print *,' max,min(i1ref)=',maxval(i1ref),minval(i1ref)


  !             i1ref now has index of interval containing point
  !               adjust to be closest one to interpolating point
  ix1sign=1
  do n=1,nin
   dxa=(x1in(n)-x1grid(i1ref(n)))*dx1gridi(i1ref(n))
   if(dxa.gt..5.and.i1ref(n).lt.nmaxright) then
    i1ref(n)=i1ref(n)+1
    ix1sign(n)=-1
   end if
  end do

  ! get interpolation indices

  l=0
  do i=0,iord
   ix1temp=0
   l=l+1
   do n=1,nin
    ix1temp(n)=i1ref(n)+ix1sign(n)*ixi(i)
    iwgts(n,l)=ix1temp(n)
   end do
!   print *,' max,min(ix1temp)=',maxval(ix1temp),minval(ix1temp)
  end do
!   print *,' max,min(iwgts)=',maxval(iwgts),minval(iwgts)
!   print *,' both of above should be >= 1 and <= ',n1grid 
    
 
      ! check that we got all points
!     dx1max=-huge(x)
!     dx1min=huge(x)
!     numinside=0
!     numgtp5=0
!     numoutside=0
!     halfpeps=.5+epsilon(x)
!     do n=1,nin
!      if(iflag(n).ne.0) then
!       numinside=numinside+1
!       dx1top=x1in(n)-x1grid(i1ref(n))
!       if(dx1top*dx1gridi(i1ref(n)).lt.0.) then
!        dx1this=(x1in(n)-x1grid(i1ref(n)-1)) &
!                  *dx1gridi(i1ref(n)-1)-1.
!       else
!        dx1this=dx1top*dx1gridi(i1ref(n))
!       end if
!       if(abs(dx1this).gt.halfpeps) then
!        numgtp5=numgtp5+1
!       end if
!       dx1max=max(dx1this,dx1max)
!       dx1min=min(dx1this,dx1min)
!      else
!       numoutside=numoutside+1
!      end if
!     end do
!          print *,' numinside,outside=',numinside,numoutside
!              print *,' numgtp5=',numgtp5
!         print *,' dx1min,max=',dx1min,dx1max

  ! get taylor matrices and invert
  
  nstart=1
  nend=min(nin,ntl)
  do while (nstart.le.nend)
   nthis=nend-nstart+1
 
   if(nf.eq.1) then    ! get weights for interpolating function
 
    ! get taylor vector(s)
 
    lp=0
    x1p(nstart:nend)=1.
    do ip=0,iord
     lp=lp+1
     z0(1:nthis,lp)=x1p(nstart:nend)
     do n=1,nthis
      nn=n+nstart-1
      iflip=1
      if(ix1sign(nn).lt.0) iflip=2
      x1p(nn)=x1p(nn)*(alocal(i1ref(nn),iflip)*  &
                 (x1in(nn)-x1grid(max(1,i1ref(nn)))) &
            +blocal(i1ref(nn),iflip))
     end do
    end do

    ! get interpolation weights
  
    do k=1,lbig
     wgts(nstart:nend,k)=0.
     do j=1,lbig
      do n=1,nthis
       nn=n+nstart-1
       iflip=1
       if(ix1sign(nn).lt.0) iflip=2
       wgts(nn,k)=wgts(nn,k)+z0(n,j)*tl(j,k,i1ref(nn),iflip)
      end do
     end do
    end do

   end if

   if(ndx1.eq.1) then    ! get weights for df/dx1
 
    ! get taylor vector(s)
 
    lp=0
    x1p(nstart:nend)=1.
    do ip=0,iord
     lp=lp+1
     do n=1,nthis
      nn=n+nstart-1
      iflip=1
      if(ix1sign(nn).lt.0) iflip=2
      z0(n,lp)=ip*x1p(nn)*alocal(i1ref(nn),iflip)
     end do
     if(ip.gt.0) then
      do n=1,nthis
       nn=n+nstart-1
       iflip=1
       if(ix1sign(nn).lt.0) iflip=2
       x1p(nn)=x1p(nn)*(alocal(i1ref(nn),iflip)*  &
                 (x1in(nn)-x1grid(max(1,i1ref(nn)))) &
            +blocal(i1ref(nn),iflip))
      end do
     end if
    end do

    ! get interpolation weights
  
    do k=1,lbig
     wgtsx1(nstart:nend,k)=0.
     do j=1,lbig
      do n=1,nthis
       nn=n+nstart-1
       iflip=1
       if(ix1sign(nn).lt.0) iflip=2
       wgtsx1(nn,k)=wgtsx1(nn,k)+z0(n,j)*tl(j,k,i1ref(nn),iflip)
      end do
     end do
    end do

   end if

   if(ndx11.eq.1) then    ! get weights for d2f/(dx1*dx1)
 
    ! get taylor vector(s)
 
    lp=0
    x1p(nstart:nend)=1.
    do ip=0,iord
     lp=lp+1
     do n=1,nthis
      nn=n+nstart-1
      iflip=1
      if(ix1sign(nn).lt.0) iflip=2
      z0(n,lp)=ip*(ip-1)*x1p(nn)*alocal(i1ref(nn),iflip)**2
     end do
     if(ip.gt.1) then
      do n=1,nthis
       nn=n+nstart-1
       iflip=1
       if(ix1sign(nn).lt.0) iflip=2
       x1p(nn)=x1p(nn)*(alocal(i1ref(nn),iflip)*  &
                 (x1in(nn)-x1grid(max(1,i1ref(nn)))) &
            +blocal(i1ref(nn),iflip))
      end do
     end if
    end do

    ! get interpolation weights
  
    do k=1,lbig
     wgtsx11(nstart:nend,k)=0.
     do j=1,lbig
      do n=1,nthis
       nn=n+nstart-1
       iflip=1
       if(ix1sign(nn).lt.0) iflip=2
       wgtsx11(nn,k)=wgtsx11(nn,k)+z0(n,j)*tl(j,k,i1ref(nn),iflip)
      end do
     end do
    end do

   end if
 
   nstart=nstart+ntl
   nend=min(nend+ntl,nin)
 
  end do

return
end subroutine simpin1

subroutine vinvmm(b,a,m,nb,na,ninv,ninv0)

  ! vector version of purserlib routine invmm
  ! invert a collection of matrices, possibly in place (a=b), using
  !   the l-u decomposition method

  ! (<)-->   b:     input matrices
  !   <--(>) a:     output inverses of b (can have a=b in calling program)
  !    -->   m:     order of matrices
  !    -->   nb:    leading dimension of b
  !    -->   na:    leading dimension of a
  !    -->   ninv:  number of matrices to invert
  !    -->   ninv0: maximum number of matrices

  dimension a(ninv0,na,*),b(ninv0,nb,*)

  intent(in)    ::m,nb,na,ninv,ninv0
  intent(inout) ::b,a 

  dimension ipiv(ninv,m),s(ninv),d(ninv)

  do j=1,m
   do i=1,m
    a(1:ninv,i,j)=b(1:ninv,i,j)
   end do
  end do
  call vlufm(a,ipiv,d,m,na,ninv,ninv0,s)

  !  invert u in place:

  do i=1,m
   a(1:ninv,i,i)=1./a(1:ninv,i,i)
  end do
  do i=1,m-1
   do j=i+1,m
    s=0.
    do k=i,j-1
     s(1:ninv)=s(1:ninv)-a(1:ninv,i,k)*a(1:ninv,k,j)
    end do
    a(1:ninv,i,j)=a(1:ninv,j,j)*s(1:ninv)
   end do
  end do

  !  invert l in place assuming implicitly diagonal elements of unity

  do j=1,m-1
   do i=j+1,m
    s(1:ninv)=-a(1:ninv,i,j)
    do k=j+1,i-1
     s(1:ninv)=s(1:ninv)-a(1:ninv,i,k)*a(1:ninv,k,j)
    end do
    a(1:ninv,i,j)=s(1:ninv)
   end do
  end do

  !  form the product of u**-1 and l**-1 in place

  do j=1,m-1
   do i=1,j
    s(1:ninv)=a(1:ninv,i,j)
    do k=j+1,m
     s(1:ninv)=s(1:ninv)+a(1:ninv,i,k)*a(1:ninv,k,j)
    end do
    a(1:ninv,i,j)=s(1:ninv)
   end do
   do i=j+1,m
    s=0.
    do k=i,m
     s(1:ninv)=s(1:ninv)+a(1:ninv,i,k)*a(1:ninv,k,j)
    end do
    a(1:ninv,i,j)=s(1:ninv)
   end do
  end do

  !  permute columns according to ipiv

  do j=m-1,1,-1
   do i=1,m
    do n=1,ninv
     s(n)=a(n,i,j)
     a(n,i,j)=a(n,i,ipiv(n,j))
     a(n,i,ipiv(n,j))=s(n)
    end do
   end do
  end do

return
end subroutine vinvmm

subroutine vlufm(a,ipiv,d,m,na,ninv,ninv0,s)

  ! vector version of purserlib routine lufm
  ! pivot routine used by matrix inversion routine vinvmm

  ! <-->  a:     on input, original matrices, on output, permuted matrices
  ! <--   ipiv:  record of permutations
  ! <--   d:     signs of determinants of matrices
  !  -->  m:     order of matrices
  !  -->  na:    leading dimension of matrices
  !  -->  ninv:  number of matrices to invert
  !  -->  ninv0: maximum number of matrices to invert
  !  --   s:     work space of dimension ninv

  dimension a(ninv0,na,*),ipiv(ninv,m),d(ninv),s(ninv)

  intent(in)    ::m,na,ninv,ninv0
  intent(out)   ::ipiv,d
  intent(inout) ::a,s

  dimension ajj(ninv)

  d=1.
  ipiv(1:ninv,m)=m
  do j=1,m-1
   jp=j+1
   ajj(1:ninv)=abs(a(1:ninv,j,j))
   ipiv(1:ninv,j)=j
   do i=jp,m
    do n=1,ninv
     aa=abs(a(n,i,j))
     if(aa.gt.ajj(n))then
      ipiv(n,j)=i
      ajj(n)=aa
     end if
    end do
   end do

   !  swap rows, recording changed sign of determinant

   do n=1,ninv
    if(ipiv(n,j).ne.j) d(n)=-d(n)
   end do
   do k=1,m
    do n=1,ninv
     s(n)=a(n,j,k)
     a(n,j,k)=a(n,ipiv(n,j),k)
     a(n,ipiv(n,j),k)=s(n)
    end do
   end do

   do n=1,ninv
    ajj(n)=a(n,j,j)
    if(ajj(n).eq.0.)then
     jm=j-1
!    write(6,100)jm
!    100 format(' failure in lufact:',/,' matrix singular, rank=',i3)
     print'('' failure in lufact:'',/,'' matrix singular, rank='',i3)',jm
                           stop
    endif
   end do
   ajj=1./ajj
   do i=jp,m
    a(1:ninv,i,j)=ajj(1:ninv)*a(1:ninv,i,j)
    do k=jp,m
     a(1:ninv,i,k)=a(1:ninv,i,k)-a(1:ninv,i,j)*a(1:ninv,j,k)
    end do
   end do

  end do

return
end subroutine vlufm
