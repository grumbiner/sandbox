subroutine st_simpin3f(wgts,iwgts,iflag,x1in,x2in,x3in,nin,iord,lhalf,lbig, &
      x1grid_start,dx1grid,x2grid_start,dx2grid,x3grid,n1grid,n2grid,n3grid,istag1)

  !  compute interpolation weights for 3-d simplex interpolation on a horizontally staggered 
  !     grid with uniform spacing in each horizontal direction, and arbitrary spacing in vertical.
  !          note:  derivative capability to be added later

  !  <--  wgts:  interpolation weights for function ( wgts(nin,lbig) )
  !  <--  iwgts: absolute grid addresses ( iwgts(nin,lbig) )
  !                   (assumes grid fields are dimensioned grid(n1grid,n2grid), so 
  !                     absolute address of grid(i,j) is i +(j-1)*n1grid  )
  !  <--  iflag: flag for each interpolatee
  !                   =0, then point too close to edge or outside of domain, 
  !                             wgt = 0, and iwgt = 1 for these points
  !                   =1, then weights computed
  !   --> x1in,x2in,x3in:  coordinates of interpolatees (1st two are horizontal, 3rd is vertical)
  !   --> nin:        number of interpolatees
  !   --> iord:       order of interpolation (1=linear, 2=quadratic, etc)
  !   --> lhalf:   = 0, then doing most efficient interpolation (no higher order cross terms)
  !                = 1, then doing traditional square interpolation, which goes
  !                         part way to the next higher order, because of cross terms
  !   --> lbig:       number of interpolating points
  !                 for 2-dim, lbig = (iord*(iord+3)+2)/2
  !                 for 3-dim, lbig = (iord*(iord*(iord+6)+11)+6)/6
  !   --> x1grid_start: starting value of unstaggered x1 rows
  !   --> dx1grid:  grid increment in x1 direction (x1 points are spaced 2*dx1grid apart)
  !   --> x2grid_start: starting value of x2 coordinate
  !   --> dx2grid:  grid increment in x2 direction (x2 points are spaced dx2grid apart)
  !   --> x3grid:   vertical grid coordinate
  !   --> n1grid:   number of points in x1 direction (horizontal)
  !   --> n2grid:   number of points in x2 direction (horizontal)
  !   --> n3grid:   number of points in x3 direction (vertical)
  !   --> istag1:   tells how staggered grid starts
  !                  =0, then 1st row starts at x1grid_start and has n1grid points
  !                  =1, then 1st row starts at x1grid_start+dx1grid and has n1grid-1 points

!   example:  istag1=0

! x2max  __       .   .   .   .   # used = n1grid     |
!                   .   .   .   x # used = n1grid-1   |
!         ^       .   .   .   .        .              |
!         |         .   .   .   x      .              |--> # rows = n2grid
!     x2grid      .   .   .   .        .              |
!                _  .   .   .   x                     |
!       dx2grid <_.   .   .   .   # used = n1grid     |
!                   .   .   .   x # used = n1grid-1   |
!                 .   .   .   .   # used = n1grid     |
!                   .   .   .   x # used = n1grid-1   |
! x2grid_start _  .   .   .   .   # used = n1grid     |
!                 | |
!                  v            |
!                dx1grid     dummy point
!                   
!                 |        x1grid--> 
!             x1grid_start




!   example:  istag1=1

! x2max  __       .   .   .   .   # used = n1grid     |
!                   .   .   .   x # used = n1grid-1   |
!         ^       .   .   .   .         .             |
!         |         .   .   .   x       .             |--> # rows = n2grid
!     x2grid      .   .   .   .         .             |
!                _  .   .   .   x                     |
!       dx2grid <_.   .   .   .   # used = n1grid     |
!                   .   .   .   x # used = n1grid-1   |
!                 .   .   .   .   # used = n1grid     |
! x2grid_start _    .   .   .   x # used = n1grid-1   |
!                 | |
!                  v            |
!                dx1grid     dummy point
!                   
!                 |        x1grid--> 
!             x1grid_start


  real(4) wgts(nin,lbig)
  integer(4) iwgts(nin,lbig),iflag(nin)
  real(4) x1in(nin),x2in(nin),x3in(nin)

  integer(1),allocatable::ishift(:)
  integer(2),allocatable::ix1pall(:,:),ix2pall(:,:)
  real(4),allocatable::x1pin(:),x2pin(:),x1pgrid(:),x2pgrid(:)

  !  compute rotated integer coordinates of grid

  !            definition of rotated grid:

  !                     i1p = k1add + (2*i1-1+is(i2)+i2)/2

  !                     i2p = k2add + (-2*i1+1-is(i2)+i2)/2

  !               where k1add, k2add are determined so min(i1p)=min(i2p)=1
  !                   and is(i2) = mod(istag1+i2-1,2) is staggering constant

  allocate(ishift(n2grid))
  do i2=1,n2grid
   ishift(i2)=mod(i2-1,2)
  end do
  if(istag1.eq.1) ishift=-ishift

  allocate(ix1pall(n1grid,n2grid))
  allocate(ix2pall(n1grid,n2grid))

  do i2=1,n2grid
   do i1=1,n1grid
    ix1pall(i1,i2)=(2*i1-1+ishift(i2)+i2)/2
    ix2pall(i1,i2)=(-2*i1+1-ishift(i2)+i2)/2
   end do
  end do
  k1add=1-minval(ix1pall) ; k2add=1-minval(ix2pall)
!     print *,' 1st max,min(ix1pall) = ',maxval(ix1pall),minval(ix1pall)
!     print *,' 1st max,min(ix2pall) = ',maxval(ix2pall),minval(ix2pall)
  ix1pall=ix1pall+k1add ; ix2pall=ix2pall+k2add
!     print *,' 2nd max,min(ix1pall) = ',maxval(ix1pall),minval(ix1pall)
!     print *,' 2nd max,min(ix2pall) = ',maxval(ix2pall),minval(ix2pall)

  ! set up rotated grid coordinates and get dimensions

  n1pgrid=maxval(ix1pall) ; n2pgrid=maxval(ix2pall)
  deallocate(ix1pall) ; deallocate(ix2pall)
!    print *,' n1pgrid,n2pgrid=',n1pgrid,n2pgrid
  allocate(x1pgrid(n1pgrid)) ; allocate(x2pgrid(n2pgrid))
  do i1=1,n1pgrid
   x1pgrid(i1)=i1
  end do
  do i2=1,n2pgrid
   x2pgrid(i2)=i2
  end do

  ! rotate interpolatee coordinates

  allocate(x1pin(nin)) ; allocate(x2pin(nin))
  
  do i=1,nin
   x1s=1.+(x1in(i)-x1grid_start-istag1*dx1grid)/dx1grid
   x2s=1.+(x2in(i)-x2grid_start)/dx2grid
   x1pin(i)=k1add+.5*(x1s+x2s)
   x2pin(i)=k2add+.5*(-x1s+x2s)
  end do


  ! get interpolation weights

  call simpin3f(wgts,wgts,wgts,wgts,wgts,wgts,wgts,wgts,wgts,wgts,iwgts, &
        iflag,x1pin,x2pin,x3in,nin,iord,lhalf,lbig, &
                x1pgrid,x2pgrid,x3grid,n1pgrid,n2pgrid,n3grid, &
              1_4,0_4,0_4,0_4,0_4,0_4,0_4,0_4,0_4,0_4)
  deallocate(x1pin) ; deallocate(x2pin) ; deallocate(x1pgrid) ; deallocate(x2pgrid)

  ! convert addresses from rotated grid to original staggered grid

          i3max=-huge(i3max) ; i3min=huge(i3min)
  do k=1,lbig
   do i=1,nin
    if(iflag(i).ne.0) then
     i3=iwgts(i,k)/(n1pgrid*n2pgrid)
     iwgtsres=iwgts(i,k)-n1pgrid*n2pgrid*i3
     i3=i3+1
          i3max=max(i3,i3max) ; i3min=min(i3,i3min)
     i2p=iwgtsres/n1pgrid
     i1p=iwgtsres-n1pgrid*i2p
     i2p=i2p+1
     i2=i1p+i2p-k1add-k2add
     if(i2.ge.1.and.i2.le.n2grid) then
      i1=(i1p-i2p-k1add+k2add-ishift(i2)+1)/2
      if(i1.ge.1.and.i1+ishift(i2).le.n1grid) then
       iwgts(i,k)=i1+(i2-1)*n1grid+(i3-1)*(n1grid*n2grid)
      else
       iwgts(i,k)=1
       wgts(i,k)=0.
       iflag(i)=0
      end if
     else
      iwgts(i,k)=1
      wgts(i,k)=0.
      iflag(i)=0
     end if
    else
     iwgts(i,k)=1
     wgts(i,k)=0.
    end if
   end do
  end do
!          print *,' in st_simpin3f, i3max,min=',i3max,i3min
!          print *,'   above should not exceed 1 and ',n3grid

!     nprint=0
!     do i=1,nin
!      if(iflag(i).ne.0) then
!       nprint=nprint+1
!       do k=1,lbig
!        i3=iwgts(i,k)/(n1grid*n2grid)
!        iwgtsres=iwgts(i,k)-n1grid*n2grid*i3
!        i3=i3+1
!        i2=iwgtsres/n1grid
!        i1=iwgtsres-n1grid*i2
!        i2=i2+1
!        i1=2*i1-1+ishift(i2)
!        print *,' i1,i2,i3,wgt=',i1,i2,i3,wgts(i,k)
!       end do
!       x1s=1.+(x1in(i)-x1grid_start-istag1*dx1grid)/dx1grid
!       x2s=1.+(x2in(i)-x2grid_start)/dx2grid
!       print *,' x1s,x2s,x3=',x1s,x2s,x3in(i)
!       if(nprint.gt.5) exit
!      end if
!     end do

  !  check wgts (should add to 1)

      nbad=0
      do i=1,nin
       sum=0.
       do k=1,lbig
        sum=sum+wgts(i,k)
       end do
       if(sum.lt..99999) then
        do k=1,lbig
         wgts(i,k)=0.
         iwgts(i,k)=1
        end do
        if(iflag(i).ne.0) nbad=nbad+1
        iflag(i)=0
       end if
      end do

!        if(nbad.ne.0) write(0,*)' IN ST_SIMPIN3F_TST, NBAD=',nbad

return
end subroutine st_simpin3f
