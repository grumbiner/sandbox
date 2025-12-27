subroutine vinterpfc(t,tc,rpc,npc,rp,np,mype)

  !  interpolate from fine to coarse grid in one dimension, where first the fine
  !    grid must be extended to exceed the coarse grid domain

  real(4) t(np),tc(npc),rp(np),rpc(npc)

  real(4),allocatable::wgtsn(:,:)
  integer(4),allocatable::iwgtsn(:,:),iflagn(:)
  integer(4),allocatable::ixi1(:)
  real(4),allocatable::tl1(:,:,:),alocal1(:),blocal1(:)

  allocate(wgtsn(npc,2)) ; allocate(iwgtsn(npc,2)) ; allocate(iflagn(npc))
  iord1=1
  lbig1=iord1+1
  allocate(ixi1(0:iord1))
  allocate(tl1(lbig1,lbig1,2*np))
  allocate(alocal1(2*np))
  allocate(blocal1(2*np))
  call simpin1_init(ixi1,tl1,alocal1,blocal1,iord1,lbig1,rp,np)
  call simpin1(wgtsn,wgtsn,wgtsn,iwgtsn,iflagn,rpc,npc,iord1,lbig1,rp,np,1,0,0, &
                      ixi1,tl1,alocal1,blocal1)
  do i=1,npc
   tc(i)=t(1)
   if(iflagn(i).ne.0) exit
  end do
  do i=npc,1,-1
   tc(i)=t(np)
   if(iflagn(i).ne.0) exit
  end do
  do i=1,npc
   if(iflagn(i).ne.0) tc(i)=wgtsn(i,1)*t(iwgtsn(i,1))+wgtsn(i,2)*t(iwgtsn(i,2))
  end do
  if(mype.eq.0) then
   do i=1,np
    print *,' i,rp,t=',i,rp(i),t(i)
   end do
   do i=1,npc
    print *,' i,rpc,tc=',i,rpc(i),tc(i)
   end do
  end if

  deallocate(wgtsn) ; deallocate(iwgtsn) ; deallocate(iflagn)
  deallocate(ixi1)
  deallocate(tl1)
  deallocate(alocal1)
  deallocate(blocal1)

return
end
