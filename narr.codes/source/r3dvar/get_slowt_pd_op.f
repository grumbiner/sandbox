subroutine get_slowt_pd_op(bight_psi,bighpdr01_psi,nvmodes,lmeta,lmetaex,etaiex,ptop,mype)

!  compute operator which gives reasonable T and pdres01 changes for unit variance of mass variable

!  use standard atmosphere to generate operator from T,pdres01 to bigphi, the mass variable.

!  then use svd to obtain well-behaved inverse operator.

  real(4) bight_psi(lmetaex,lmetaex),bighpdr01_psi(lmetaex)
  real(4) etaiex(lmetaex+1)

  real(8) deta(lmetaex),eta(lmetaex),pbar(lmetaex),tbar(lmetaex),abar(lmetaex),bbar(lmetaex)
  real(8) d(lmetaex),e(lmetaex),f(lmetaex)
  real(8) bigst(lmetaex,lmetaex+1),bigu(lmetaex+1,lmetaex),w(lmetaex),winv(lmetaex),bigv(lmetaex,lmetaex)
  real(8) bigsminus(lmetaex+1,lmetaex),std(lmetaex+1)

  real(8) pdtbar,gascon,gij,wi,erroru,sum,errorv,wcut

  if(nvmodes.eq.0) then
   bight_psi=0.
   bighpdr01_psi=0.
   if(mype.eq.0) print *,' implicit linear balance turned off'
   return
  end if

  do k=1,lmetaex
   deta(k)=etaiex(k+1)-etaiex(k)
   eta(k)=.5_8*(etaiex(k+1)+etaiex(k))
  end do
  pdtbar=1013._8-ptop
  do k=1,lmetaex
   pbar(k)=eta(k)*pdtbar+ptop
   pbar4=pbar(k)
   call w3fa03(pbar4,hgt4,tbar4,thetabar4)
   tbar(k)=tbar4
  end do
  gascon=conmc('rd$')
  abar=gascon*tbar/pbar
  bbar=gascon*pdtbar/pbar
  d(lmetaex)=.5_8*deta(lmetaex)*bbar(lmetaex)
  e(lmetaex)=0._8
  f(lmetaex)=.5_8*deta(lmetaex)*abar(lmetaex)
  do i=1,lmetaex-1
   d(i)=.25_8*bbar(i)*(deta(i)+deta(i+1))
   e(i)=.25_8*bbar(i+1)*(deta(i)+deta(i+1))
   f(i)=.25_8*(abar(i)+abar(i+1))*(deta(i)+deta(i+1))
  end do
  bigst=0._8
  do j=1,lmeta
   do i=1,lmeta
    if(j.lt.i) gij=0._8
    if(j.eq.i) gij=.5_8*bbar(j)*deta(j)
    if(j.gt.i) gij=bbar(j)*deta(j)
    bigst(i,j)=gij
   end do
  end do
  if(lmetaex.gt.lmeta) then
   do j=lmeta+1,lmetaex
    do i=lmeta+1,lmetaex
     if(j.lt.i) gij=bbar(j)*deta(j)
     if(j.eq.i) gij=.5_8*bbar(j)*deta(j)
     if(j.gt.i) gij=0._8
     bigst(i,j)=gij
    end do
   end do
  end if

  do i=1,lmeta
   wi=abar(i)*eta(i)+.5_8*abar(i)*deta(i)
   if(i.lt.lmeta) then
    do k=i+1,lmeta
     wi=wi+abar(k)*deta(k)
    end do
   end if
   bigst(i,lmetaex+1)=wi
  end do
  if(lmetaex.gt.lmeta) then
   do i=lmeta+1,lmetaex
    wi=abar(i)*eta(i)
    if(i.gt.lmeta+1) then
     do k=lmeta+1,i-1
      wi=wi+abar(k)*deta(k)
     end do
    end if
    wi=wi+.5_8*abar(i)*deta(i)
    bigst(i,lmetaex+1)=wi
   end do
  end if

  do i=1,lmetaex+1
   do j=1,lmetaex
    bigu(i,j)=bigst(j,i)
   end do
  end do
  call svdcmp(bigu,lmetaex+1,lmetaex,lmetaex+1,lmetaex,w,bigv,ierror)
  if(mype.eq.0) print *,' return from svdcmp, ierror=',ierror
  if(mype.eq.0) then
   do i=1,nvmodes
    print *,' w(',i,')=',w(i)
   end do
  end if
 
!  check orthonormality of u,v

  do m=1,lmetaex
   do n=1,lmetaex
    erroru=0._8
    sum=0._8
    if(m.eq.n) sum=-1._8
    do k=1,lmetaex+1
     sum=sum+bigu(k,m)*bigu(k,n)
    end do
    erroru=max(erroru,abs(sum))
   end do
  end do
  if(mype.eq.0) print *,' erroru=',erroru
  do m=1,lmetaex
   do n=1,lmetaex
    errorv=0._8
    sum=0._8
    if(m.eq.n) sum=-1._8
    do k=1,lmetaex
     sum=sum+bigv(k,m)*bigv(k,n)
    end do
    errorv=max(errorv,abs(sum))
   end do
  end do
  if(mype.eq.0) print *,' errorv=',errorv
    
!--- compute bigsminus

  winv=0.
  do k=1,nvmodes
   winv(k)=1._8/w(k)
  end do
  bigsminus=0._8
  do i=1,lmetaex+1
   do j=1,lmetaex
    do k=1,lmetaex
     bigsminus(i,j)=bigsminus(i,j)+bigu(i,k)*winv(k)*bigv(j,k)
    end do
   end do
  end do
  if(mype.eq.0) print *,' variances for nvmodes=',nvmodes
  do i=1,lmetaex+1
   sum=0._8
   do k=1,lmetaex
    sum=sum+bigsminus(i,k)**2
   end do
   std(i)=sqrt(sum)
  end do
  sum=std(17)
  do i=1,lmetaex+1
   std(i)=std(i)/sum
  end do
  do i=1,lmetaex+1
   if(i.le.lmetaex) then
    if(mype.eq.0) print('('' p, std dev for t('',i3,'')='',f8.1,f8.3)'),i,pbar(i),std(i)
   else
    if(mype.eq.0) print('('' std dev for pd='',f8.3)'),std(i)
   end if
  end do
  do j=1,lmetaex
   do i=1,lmetaex
    bight_psi(i,j)=bigsminus(i,j)
   end do
   bighpdr01_psi(j)=bigsminus(lmetaex+1,j)
  end do

return
end
