function ofn(alpha, &
              xq,xbarq,pq,pbarq,yoq,xbarbq,mqdata,qfw, &
                          xbarpw,pbarpw,yopw,xbarbpw,mpwdata,pwfw, &
              xw,xbarw,pw,pbarw,yow,xbarbw,mwdata,wfw, &
                          xbarwr,pbarwr,yowr,xbarbwr,mwrdata,ibighwr,lbig2ges, &
              xp,xbarp,pp,pbarp,yop,xbarbp,mpdata,psfw, &
              xt,xbart,pt,pbart,yot,xbarbt,mtdata,tfw, &
              xpred,xbarrad,ppred,pbarrad,yorad,xbarbrad,mraddata,jpch,npred,userad,radfw, &
                 nxc,nyc,lmetaex,myxsc,myxec,myysc,myyec,doqc)

      include 'mpif.h'
      include "my_comm.h"
      include 'qcparam.h'

!  Calculate the objective function for 'temperature'

      real(8)   ofn8,ofno8,ofnb8,prod8
      real(4)   ofn,wgross,wnotgross

      real(4)   xq(nxc*nyc*lmetaex)
      real(4)   xbarq(max(1,mqdata))
      real(4)   pq(nxc*nyc*lmetaex)
      real(4)   pbarq(max(1,mqdata))
      real(4)   yoq(max(1,mqdata))
      real(4)   xbarbq(max(1,mqdata))
      logical qfw(max(1,mqdata))

  real(4) xbarpw(max(1,mpwdata))
  real(4) pbarpw(max(1,mpwdata))
  real(4) yopw(max(1,mpwdata))
  real(4) xbarbpw(max(1,mpwdata))
  logical pwfw(max(1,mpwdata))

      real(4)   xw(nxc*nyc*lmetaex,2)
      real(4)   xbarw(max(1,mwdata))
      real(4)   pw(nxc*nyc*lmetaex,2)
      real(4)   pbarw(max(1,mwdata))
      real(4)   yow(max(1,mwdata))
      real(4)   xbarbw(max(1,mwdata))
      logical wfw(max(1,mwdata))

      real(4)   xbarwr(lmetaex,max(1,mwrdata))
      real(4)   pbarwr(lmetaex,max(1,mwrdata))
      real(4)   yowr(max(1,mwrdata))
      real(4)   xbarbwr(lmetaex,max(1,mwrdata))
      integer(4) ibighwr(lbig2ges+2,max(1,mwrdata))

      real(4) xp(nxc*nyc)
      real(4) xbarp(max(1,mpdata))
      real(4) pp(nxc*nyc)
      real(4) pbarp(max(1,mpdata))
      real(4) yop(max(1,mpdata))
      real(4) xbarbp(max(1,mpdata))
      logical psfw(max(1,mpdata))

      real(4)   xt(nxc*nyc*lmetaex)
      real(4)   xbart(max(1,mtdata))
      real(4)   pt(nxc*nyc*lmetaex)
      real(4)   pbart(max(1,mtdata))
      real(4)   yot(max(1,mtdata))
      real(4)   xbarbt(max(1,mtdata))
      logical tfw(max(1,mtdata))

  real(4) xpred(jpch,npred)
  real(4) xbarrad(max(1,mraddata))
  real(4) ppred(jpch,npred)
  real(4) pbarrad(max(1,mraddata))
  real(4) yorad(max(1,mraddata))
  real(4) xbarbrad(max(1,mraddata))
  logical userad
  logical doqc
  logical radfw(max(1,mraddata))

!-------- stuff for vertical integration in eta

      logical start
      data start/.true./
      save start
  real(4),allocatable::w2(:),w3(:,:)

                      call mpi_comm_rank(my_comm,mype,ierr)
  allocate(w2(nxc*nyc))
  allocate(w3(nxc*nyc*lmetaex,2))
      ofn=0.
      ofn8=0._8
      ofno8=0._8
      ofnb8=0._8
      cg = 1.2533/b          ! cg = sqrt(2*pi)/2b

!  Observation contribution

      if(start) then
        wnotgross = 1. - pgt
        wgross = pgt*cg
        print *,' ofn--b,wnotgrosst,wgrosst: ',b,wnotgross,wgross
        start = .false.
      endif
!       if(mype.eq.0) write(0,*)' at 1 in ofn'
      call dot_prod8nl(prod8,pgq,cg,yoq,xbarbq,xbarq,alpha,pbarq,mqdata,qfw,doqc)
      ofno8 = ofno8 + prod8
!       if(mype.eq.0) write(0,*)' at 1 in ofn'
      call dot_prod8nl(prod8,pgpw,cg,yopw,xbarbpw,xbarpw,alpha,pbarpw,mpwdata,pwfw,doqc)
      ofno8 = ofno8 + prod8
!       if(mype.eq.0) write(0,*)' at 2 in ofn, prod8=',prod8
      call dot_prod8nl(prod8,pgw,cg,yow,xbarbw,xbarw,alpha,pbarw,mwdata,wfw,doqc)
      ofno8 = ofno8 + prod8
      call dot_prod8nlwr(prod8,yowr,xbarbwr,xbarwr,alpha,pbarwr,mwrdata,lmetaex,ibighwr,lbig2ges)
      ofno8 = ofno8 + prod8
!       if(mype.eq.0) write(0,*)' at 3 in ofn, prod8=',prod8
      call dot_prod8nl(prod8,pgp,cg,yop,xbarbp,xbarp,alpha,pbarp,mpdata,psfw,doqc)
      ofno8 = ofno8 + prod8
!       if(mype.eq.0) write(0,*)' at 3.1 in ofn, prod8=',prod8
      call dot_prod8nl(prod8,pgt,cg,yot,xbarbt,xbart,alpha,pbart,mtdata,tfw,doqc)
      ofno8 = ofno8 + prod8
      if(userad) then
        call dot_prod8nl(prod8,pgrad,cg,yorad,xbarbrad,xbarrad,alpha,pbarrad,mraddata,radfw,doqc)
        ofno8 = ofno8 + prod8
!       print *,' ofn--prod8 for rad-obs: ',prod8
      endif
     
!  Background contribution

!       if(mype.eq.0) write(0,*)' at 4 in ofn, prod8=',prod8
      w3(:,1)=xq+alpha*pq
      call dot_prod8g(prod8,w3,w3,nxc,nyc,lmetaex,myxsc,myxec,myysc,myyec)
      ofnb8 = ofnb8 + 0.5_8*prod8
!       if(mype.eq.0) write(0,*)' at 4.1 in ofn, prod8=',prod8

      w3=xw+alpha*pw
      call dot_prod8g(prod8,w3,w3,nxc,nyc,lmetaex*2,myxsc,myxec,myysc,myyec)
      ofnb8 = ofnb8 + 0.5_8*prod8
!       if(mype.eq.0) write(0,*)' at 5.1 in ofn, prod8=',prod8

      w3(:,1)=xt+alpha*pt
      call dot_prod8g(prod8,w3,w3,nxc,nyc,lmetaex,myxsc,myxec,myysc,myyec)
      ofnb8 = ofnb8 + 0.5_8*prod8
!       if(mype.eq.0) write(0,*)' at 6.1 in ofn, prod8=',prod8

      w2=xp+alpha*pp
      call dot_prod8g(prod8,w2,w2,nxc,nyc,1,myxsc,myxec,myysc,myyec)
      ofnb8 = ofnb8 + 0.5_8*prod8
!       if(mype.eq.0) write(0,*)' at 7.1 in ofn, prod8=',prod8
      if(userad) then
        do n=1,npred
          do j=1,jpch
            ofnb8 = ofnb8 + 0.5_8*(1._8*(xpred(j,n)+alpha*ppred(j,n)))**2
          enddo
        enddo
      endif


      ofn8 = ofno8 + ofnb8
      ofn = ofn8

  deallocate(w2)
  deallocate(w3)

      return
      end function ofn
