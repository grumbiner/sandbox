subroutine gradnl_radar(grad,yo,xbarb,xbar,mdata,lmetaex,ibighwr,lbig2ges)

!   compute nonlinear gradient of Jo (radar wind contribution--no non-lin qc for now)

  real(4) grad(lmetaex,max(1,mdata)),yo(max(1,mdata)),xbarb(lmetaex,max(1,mdata)),xbar(lmetaex,max(1,mdata))
  integer(4) ibighwr(lbig2ges+2,max(1,mdata))
       !  ibighwr(lbig2ges+1,.) -- kbeamtop
       !  ibighwr(lbig2ges+2,.) -- kbeambot

  real(4) uprofile(lmetaex)

  if(mdata.gt.0) then
   do i=1,mdata
    kbeamtop=ibighwr(lbig2ges+1,i)
    kbeambot=ibighwr(lbig2ges+2,i)
    uprofile(kbeamtop:kbeambot)=xbarb(kbeamtop:kbeambot,i)+xbar(kbeamtop:kbeambot,i)
    call grad_forward_radar(grad(1,i),yo(i),uprofile,kbeambot,kbeamtop,lmetaex)
   enddo
  endif

return
end subroutine gradnl_radar
subroutine grad_forward_radar(grad,wobs,wges,kbot,ktop,lmetaex)

!  gradient of forward model for radar winds that allows for beam spread in crude manner

  real(4) grad(lmetaex),wges(lmetaex)

!     real(4) p(ktop:kbot)

!     bign=kbot-ktop+1
!     bigninv=1./bign
!     ubar=0.
!     pbar=0.
!     upbar=0.
!     ppbar=0.
!     pmid=.5*(ktop+kbot)
!     do k=ktop,kbot
!      p(k)=k-pmid
!     end do
  kminmin=0
  kmaxmax=0
  umaxmax=-huge(umaxmax)
  uminmin=huge(uminmin)
  do k=ktop,kbot
   if(wges(k).gt.umaxmax) then
    kmaxmax=k
    umaxmax=wges(k)
   end if
   if(wges(k).lt.uminmin) then
    kminmin=k
    uminmin=wges(k)
   end if
!      ubar=ubar+wges(k)
!      pbar=pbar+p(k)
!      upbar=upbar+wges(k)*p(k)
!      ppbar=ppbar+p(k)*p(k)
  end do
!     ubar=ubar*bigninv
!     pbar=pbar*bigninv
!     upbar=upbar*bigninv
!     ppbar=ppbar*bigninv
!     bigvpinv=1./(ppbar-pbar*pbar)
!     bigvpninv=bigninv*bigvpinv
!     a=(upbar-ubar*pbar)*bigvpinv
!     b=(ubar*ppbar-upbar*pbar)*bigvpinv
!     ubot=a*p(kbot)+b
!     utop=a*p(ktop)+b
!     if(utop.gt.ubot) then
!      umin=ubot
!      kmin=kbot
!      umax=utop
!      kmax=ktop
!     else
!      umin=utop
!      kmin=ktop
!      umax=ubot
!      kmax=kbot
!     end if
!     if(wobs.lt.umin) then
!      do j=ktop,kbot
!       daduj=bigvpninv*(p(j)-pbar)
!       dbduj=bigvpninv*(ppbar-pbar*p(j))
!       grad(j)=-(p(kmin)*daduj+dbduj)*(umin-wobs)
!      end do
!     end if
!     if(wobs.gt.umax) then
!      do j=ktop,kbot
!       daduj=bigvpninv*(p(j)-pbar)
!       dbduj=bigvpninv*(ppbar-pbar*p(j))
!       grad(j)=-(p(kmax)*daduj+dbduj)*(umax-wobs)
!      end do
!     end if
  grad=0.
  if(wobs.lt.uminmin) grad(kminmin)=wobs-uminmin
  if(wobs.gt.umaxmax) grad(kmaxmax)=wobs-umaxmax

return
end subroutine grad_forward_radar
