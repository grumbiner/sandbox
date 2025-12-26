subroutine stepsize(bigj,alpha8, &
                    xq,xbarq,pq,pbarq,yoq,xbarbq,mqdata,qfw, &
                          xbarpw,pbarpw,yopw,xbarbpw,mpwdata,pwfw, &
                    xw,xbarw,pw,pbarw,yow,xbarbw,mwdata,wfw, &
                       xbarwr,pbarwr,yowr,xbarbwr,mwrdata,ibighwr,lbig2ges, &
                    xp,xbarp,pp,pbarp,yop,xbarbp,mpdata,psfw, &
                    xt,xbart,pt,pbart,yot,xbarbt,mtdata,tfw, &
         xpred,xbarrad,ppred,pbarrad,yorad,xbarbrad,mraddata,jpch,npred,userad,radfw, &
                       nxc,nyc,lmetaex,myxsc,myxec,myysc,myyec,doqc)

!     obtain stepsize for conjugate gradient minimization algorithm

!  <-- alpha8: output stepsize (real(8))


  include 'mpif.h'
      include "my_comm.h"

  real(8) alpha8

!------- moisture stuff

  real(4) xq(nxc*nyc*lmetaex)
  real(4) xbarq(max(1,mqdata))
  real(4) pq(nxc*nyc*lmetaex)
  real(4) pbarq(max(1,mqdata))
  real(4) yoq(max(1,mqdata))
  real(4) xbarbq(max(1,mqdata))
  logical qfw(max(1,mqdata))

!------- pw stuff

  real(4) xbarpw(max(1,mpwdata))
  real(4) pbarpw(max(1,mpwdata))
  real(4) yopw(max(1,mpwdata))
  real(4) xbarbpw(max(1,mpwdata))
  logical pwfw(max(1,mpwdata))

!------- wind stuff

  real(4) xw(nxc*nyc*lmetaex,2)
  real(4) xbarw(max(1,mwdata))
  real(4) pw(nxc*nyc*lmetaex,2)
  real(4) pbarw(max(1,mwdata))
  real(4) yow(max(1,mwdata))
  real(4) xbarbw(max(1,mwdata))
  logical wfw(max(1,mwdata))

!------- radar wind stuff

  real(4) xbarwr(lmetaex,max(1,mwrdata))
  real(4) pbarwr(lmetaex,max(1,mwrdata))
  real(4) yowr(max(1,mwrdata))
  real(4) xbarbwr(lmetaex,max(1,mwrdata))
  integer(4) ibighwr(lbig2ges+2,max(1,mwrdata))

!------- h stuff

  real(4) xp(nxc*nyc)
  real(4) xbarp(max(1,mpdata))
  real(4) pp(nxc*nyc)
  real(4) pbarp(lmetaex+1,max(1,mpdata))
  real(4) yop(max(1,mpdata))
  real(4) xbarbp(max(1,mpdata))
  logical psfw(max(1,mpdata))

!------- t stuff

  real(4) xt(nxc*nyc*lmetaex)
  real(4) xbart(max(1,mtdata))
  real(4) pt(nxc*nyc*lmetaex)
  real(4) pbart(max(1,mtdata))
  real(4) yot(max(1,mtdata))
  real(4) xbarbt(max(1,mtdata))
  logical tfw(max(1,mtdata))

!------- rad stuff

  real(4) xpred(jpch,npred)
  real(4) xbarrad(max(1,mraddata))
  real(4) ppred(jpch,npred)
  real(4) pbarrad(max(1,mraddata))
  real(4) yorad(max(1,mraddata))
  real(4) xbarbrad(max(1,mraddata))
  logical userad
  logical radfw(max(1,mraddata))

!-------- stuff for vertical integration in eta



  real(4) indx(3),alphas(3),f(3),alt(3),fnt(3)
  logical start
  logical doqc
  real(4) ofn,alpha1,alpha2,alpha3,f1,f2,f3,alpha
  integer iord


  data start /.true./
  external ofn
  save start

  call mpi_comm_rank(my_comm,mype,ierr)

!  Find alpha's bracketing the minimum

      if(start) then
        print *,' stepsize--at start for mype = ',mype
        alpha1 = 0.0
        alpha2 = .001
        alpha3 = 0.02

  call mnbrak(alpha1,alpha2,alpha3,f1,f2,f3,ofn, &
              xq,xbarq,pq,pbarq,yoq,xbarbq,mqdata,qfw, &
                          xbarpw,pbarpw,yopw,xbarbpw,mpwdata,pwfw, &
              xw,xbarw,pw,pbarw,yow,xbarbw,mwdata,wfw, &
                          xbarwr,pbarwr,yowr,xbarbwr,mwrdata,ibighwr,lbig2ges, &
              xp,xbarp,pp,pbarp,yop,xbarbp,mpdata,psfw, &
              xt,xbart,pt,pbart,yot,xbarbt,mtdata,tfw, &
         xpred,xbarrad,ppred,pbarrad,yorad,xbarbrad,mraddata,jpch,npred,userad,radfw, &
              nxc,nyc,lmetaex,myxsc,myxec,myysc,myyec,doqc)

!     reorder alpha's,f's

        alphas(1) = alpha1
        alphas(2) = alpha2
        alphas(3) = alpha3
        f(1) = f1
        f(2) = f2
        f(3) = f3
        iord = 0
        call shell(alphas,indx,3,iord)
        call sort(f,indx,3)
        alpha1 = alphas(1)
        alpha2 = alphas(2)
        alpha3 = alphas(3)
        f1 = f(1)
        f2 = f(2)
        f3 = f(3)
        start = .false.
      else
        alpha1 = 0.
        alpha2 = alpha8
        alpha3 = 2.*alpha8
      endif

!  Find alpha giving minimum to objective function

      tol = 1.0e-2
      if(mype.eq.0) write(0,*)' stepsize--before brent for mype = ',mype, &
                                       '  alpha1,2,3: ',alpha1,alpha2,alpha3
      bigj=brent(alpha1,alpha2,alpha3,ofn,tol,alpha,iter, &
                 xq,xbarq,pq,pbarq,yoq,xbarbq,mqdata,qfw, &
                          xbarpw,pbarpw,yopw,xbarbpw,mpwdata,pwfw, &
                 xw,xbarw,pw,pbarw,yow,xbarbw,mwdata,wfw, &
                          xbarwr,pbarwr,yowr,xbarbwr,mwrdata,ibighwr,lbig2ges, &
                 xp,xbarp,pp,pbarp,yop,xbarbp,mpdata,psfw, &
                 xt,xbart,pt,pbart,yot,xbarbt,mtdata,tfw, &
         xpred,xbarrad,ppred,pbarrad,yorad,xbarbrad,mraddata,jpch,npred,userad,radfw, &
                 nxc,nyc,lmetaex,myxsc,myxec,myysc,myyec,doqc)
      if(mype.eq.0) write(0,*)' stepsize--after brent for mype = ',mype
      alpha8 = alpha

return
end subroutine stepsize
