subroutine retrieve_xstats(sigtheta1_in,wspd2_in,rldq3_in,v,x)

!  get variance, correlation lengths 

  real(4),pointer::sigtheta1(:),wspd2(:),rldq3(:),d1(:),d2(:),d3(:),del1i(:),del2i(:),del3i(:)
  real(4),pointer::qlens(:,:,:),stdqs(:,:,:)
  common/aniso_xstats/n1,n2,n3,sigtheta1,wspd2,rldq3,qlens,stdqs,d1,d2,d3
  common/aniso_xstats/ del1i,del2i,del3i

  integer(4) ia(1)

!  obtain indices, interpolation constants

  d1(1:n1)=sigtheta1(1:n1)-sigtheta1_in
  ia=minloc(d1(1:n1),d1(1:n1).gt.0.)
  i1=max(1,min(n1-1,ia(1)))
  i1p=i1+1
  w1p=d1(i1)*del1i(i1)
  w1p=max(0.,min(1.,w1p))
  w1=1.-w1p

  d2(1:n2)=wspd2(1:n2)-wspd2_in
  ia=minloc(d2(1:n2),d2(1:n2).gt.0.)
  i2=max(1,min(n2-1,ia(1)))
  i2p=i2+1
  w2p=d2(i2)*del2i(i2)
  w2p=max(0.,min(1.,w2p))
  w2=1.-w2p

  d3(1:n3)=rldq3(1:n3)-rldq3_in
  ia=minloc(d3(1:n3),d3(1:n3).gt.0.)
  i3=max(1,min(n3-1,ia(1)))
  i3p=i3+1
  w3p=d3(i3)*del3i(i3)
  w3p=max(0.,min(1.,w3p))
  w3=1.-w3p

!   interpolate in first coordinate:

  x00=w1*qlens(i1,i2 ,i3 )+w1p*qlens(i1p,i2 ,i3 )
  x0p=w1*qlens(i1,i2 ,i3p)+w1p*qlens(i1p,i2 ,i3p)
  xp0=w1*qlens(i1,i2p,i3 )+w1p*qlens(i1p,i2p,i3 )
  xpp=w1*qlens(i1,i2p,i3p)+w1p*qlens(i1p,i2p,i3p)

  v00=w1*stdqs(i1,i2 ,i3 )+w1p*stdqs(i1p,i2 ,i3 )
  v0p=w1*stdqs(i1,i2 ,i3p)+w1p*stdqs(i1p,i2 ,i3p)
  vp0=w1*stdqs(i1,i2p,i3 )+w1p*stdqs(i1p,i2p,i3 )
  vpp=w1*stdqs(i1,i2p,i3p)+w1p*stdqs(i1p,i2p,i3p)

!   interpolate in second coordinate:

  x0=w2*x00+w2p*xp0
  xp=w2*x0p+w2p*xpp

  v0=w2*v00+w2p*vp0
  vp=w2*v0p+w2p*vpp

  
!   interpolate in third coordinate:

  x=w3*x0+w3p*xp
  v=w3*v0+w3p*vp

return
end subroutine retrieve_xstats

subroutine retrieve_xstats0(instats)

!  read in variance, correlation length tables

  real(4),pointer::sigtheta1(:),wspd2(:),rldq3(:),d1(:),d2(:),d3(:),del1i(:),del2i(:),del3i(:)
  real(4),pointer::qlens(:,:,:),stdqs(:,:,:)
  common/aniso_xstats/n1,n2,n3,sigtheta1,wspd2,rldq3,qlens,stdqs,d1,d2,d3
  common/aniso_xstats/ del1i,del2i,del3i

  rewind instats
  read(instats,err=100,end=100)n1,n2,n3
  go to 200
100 continue
    print *,' PROBLEM READING VARIANCE, CORRELATION TABLES:'
    stop 99
200 continue
  print *,' read variance, correlation length tables, n1,n2,n3=',n1,n2,n3

  deallocate(sigtheta1,stat=ierr)
  allocate(sigtheta1(n1))
  deallocate(wspd2,stat=ierr)
  allocate(wspd2(n2))
  deallocate(rldq3,stat=ierr)
  allocate(rldq3(n3))
  deallocate(qlens,stat=ierr)
  allocate(qlens(n1,n2,n3))
  deallocate(stdqs,stat=ierr)
  allocate(stdqs(n1,n2,n3))
  rewind instats
  read(instats,err=100,end=100)n1,n2,n3,sigtheta1,wspd2,rldq3,qlens,stdqs
  close(instats)

  deallocate(d1,stat=ierr)
  allocate(d1(n1))
  deallocate(d2,stat=ierr)
  allocate(d2(n2))
  deallocate(d3,stat=ierr)
  allocate(d3(n3))

  deallocate(del1i,stat=ierr)
  allocate(del1i(n1))
  deallocate(del2i,stat=ierr)
  allocate(del2i(n2))
  deallocate(del3i,stat=ierr)
  allocate(del3i(n3))

  do i1=1,n1-1
   del1i(i1)=1./(sigtheta1(i1+1)-sigtheta1(i1))
  end do
  del1i(n1)=del1i(n1-1)
  do i2=1,n2-1
   del2i(i2)=1./(wspd2(i2+1)-wspd2(i2))
  end do
  del2i(n2)=del2i(n2-1)
  do i3=1,n3-1
   del3i(i3)=1./(rldq3(i3+1)-rldq3(i3))
  end do
  del3i(n3)=del3i(n3-1)

return
end subroutine retrieve_xstats0
