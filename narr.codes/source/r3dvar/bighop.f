subroutine bighop(qeta,pbarq,mqdata,ibighq,bighq, &
                       pbarpw,mpwdata,ibighpw,bighpw, &
                  weta,pbarw,mwdata,ibighw,bighw, &
                       pbarwr,mwrdata,ibighwr,bighwr, &
                  peta,pbarp,mpdata,ibighp,bighp, &
                  teta,pbart,mtdata,ibight,bight, &
                 ppred,pbarrad,mraddata,ibighradh,bighradh,bighradv,icxrad,predrad,npred,userad, &
                  lbig2ges,lbig3ges,imeta,jmeta,lmetaex,jpch,jpchus)

!-------- implementation of linear forward operator H

!  --> pt,etc:       input model variable vector scaled by half of background error
! <--  pbart,etc:    output model variables at obs locations scaled by obs error
!  --> mtdata,etc:   number of observations of each type
!  --> ibight,etc:   linear forward model index arrays
!  --> bight,etc:    linear forward model weights
!  --> lbig2ges,lbig3ges: number of interpolating points for 2- and 3-dim interpolation
!  --> imeta,jmeta,lmetaex:  grid dimensions

  include 'mpif.h'
         include "my_comm.h"

  real(4) qeta(imeta*jmeta,lmetaex)
  real(4) pbarq(max(1,mqdata))
  integer(4) ibighq(lbig3ges,max(1,mqdata))
  real(4) bighq(lbig3ges,max(1,mqdata))

  real(4) pbarpw(max(1,mpwdata))
  integer(4) ibighpw(lbig2ges,max(1,mpwdata))
  real(4) bighpw(lmetaex,lbig2ges,max(1,mpwdata))

  real(4) weta(imeta*jmeta*lmetaex,2)
  real(4) pbarw(max(1,mwdata))
  integer(4) ibighw(lbig3ges,max(1,mwdata))
  real(4) bighw(lbig3ges,2,max(1,mwdata))

  real(4) pbarwr(lmetaex,max(1,mwrdata))
  integer(4) ibighwr(lbig2ges+2,max(1,mwrdata))
  real(4) bighwr(lbig2ges,2,max(1,mwrdata))

  real(4) peta(imeta*jmeta)
  real(4) pbarp(max(1,mpdata))
  integer(4) ibighp(lbig2ges,max(1,mpdata))
  real(4) bighp(lbig2ges,max(1,mpdata))

  real(4) teta(imeta*jmeta,lmetaex)
  real(4) pbart(max(1,mtdata))
  integer(4) ibight(lbig3ges,max(1,mtdata))
  real(4) bight(lbig3ges,max(1,mtdata))

  logical userad
  real(4) ppred(jpch,npred)
  real(4) pbarrad(max(1,mraddata))
  integer(4) ibighradh(lbig2ges,max(1,mraddata))
  real(4) bighradh(lbig2ges,max(1,mraddata))
  real(4) bighradv(2*lmetaex,max(1,mraddata))
  integer(4) icxrad(2,max(1,mraddata))
  real(4) predrad(npred+jpchus-1,max(1,mraddata))

  call exch(qeta,lmetaex,5,5)
  call exch(weta,2*lmetaex,5,5)
  call exch(teta,lmetaex,5,5)
  call exch(peta,1,5,5)

  imjmeta=imeta*jmeta

!------------------------------------------------------------------
!----   apply linear operator H
!------------------------------------------------------------------
!---------------------------------------q
  pbarq=0.
  if(mqdata.gt.0) then
   do i=1,mqdata
    do m=1,lbig3ges
     pbarq(i)=pbarq(i)+bighq(m,i)*qeta(ibighq(m,i),1)
    end do
   end do
  end if
!---------------------------------------pw
  pbarpw=0.
  if(mpwdata.gt.0) then
   do i=1,mpwdata
    do m=1,lbig2ges
     do k=1,lmetaex
      pbarpw(i)=pbarpw(i)+bighpw(k,m,i)*qeta(ibighpw(m,i),k)
     end do
    end do
   end do
  end if
!---------------------------------------w
  pbarw=0.
  if(mwdata.gt.0) then
   do k=1,2
    do i=1,mwdata
     do m=1,lbig3ges
      pbarw(i)=pbarw(i)+bighw(m,k,i)*weta(ibighw(m,i),k)
     end do
    end do
   end do
  end if
!---------------------------------------wr
  pbarwr=0.
  if(mwrdata.gt.0) then
   do k=1,2
    do i=1,mwrdata
     kbeamtop=ibighwr(lbig2ges+1,i)
     kbeambot=ibighwr(lbig2ges+2,i)
     do kk=kbeamtop,kbeambot
      do m=1,lbig2ges
       pbarwr(kk,i)=pbarwr(kk,i)+bighwr(m,k,i)*weta(ibighwr(m,i)+(kk-1)*imjmeta,k)
      end do
     end do
    end do
   end do
  end if
!---------------------------------------p
  pbarp=0.
  if(mpdata.gt.0) then
   do i=1,mpdata
    do m=1,lbig2ges
     pbarp(i)=pbarp(i)+bighp(m,i)*peta(ibighp(m,i))
    end do
   end do
  end if
!---------------------------------------t
  pbart=0.
  if(mtdata.gt.0) then
   do i=1,mtdata
    do m=1,lbig3ges
     pbart(i)=pbart(i)+bight(m,i)*teta(ibight(m,i),1)
    end do
   end do
  end if
!---------------------------------------rad
  pbarrad=0.
  if(userad) then
!-------------------------------------- contribution from t
   if(mraddata.gt.0) then
    do i=1,mraddata
     do k=1,lmetaex
      radthis=0.
      do m=1,lbig2ges
       radthis=radthis+bighradh(m,i)*teta(ibighradh(m,i),k)
      end do
      pbarrad(i)=pbarrad(i)+bighradv(k,i)*radthis
     end do
    end do
   end if
!--------------------------------------- contribution from q
   if(mraddata.gt.0) then
    do i=1,mraddata
     do k=1,lmetaex
      radthis=0.
      do m=1,lbig2ges
       radthis=radthis+bighradh(m,i)*qeta(ibighradh(m,i),k)
      end do
      pbarrad(i)=pbarrad(i)+bighradv(k+lmetaex,i)*radthis
     end do
    end do
   end if
!--------------------------------------- contribution from bias correction
         if(mraddata.gt.0) then
          do i=1,mraddata
           ic=icxrad(1,i)
           icl=npred+icxrad(2,i)-2
           do n=1,npred-2
            pbarrad(i)=pbarrad(i)+ppred(ic,n)*predrad(n,i)
           end do
           pbarrad(i)=pbarrad(i)+(ppred(ic,npred)+ppred(ic,npred-1)*predrad(icl,i))*predrad(icl,i)
          end do
         end if
  end if

return
end subroutine bighop
