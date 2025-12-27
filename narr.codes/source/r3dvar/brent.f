      FUNCTION brent(ax,bx,cx,func,tol,xmin,iter, &
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

      INTEGER ITMAX
      integer   n
      real(4)   brent,ax,bx,cx,tol,xmin,CGOLD,ZEPS
      real(4)   func
      EXTERNAL  func

  real(4) xq(nxc*nyc*lmetaex)
  real(4) xbarq(max(1,mqdata))
  real(4) pq(nxc*nyc*lmetaex)
  real(4) pbarq(max(1,mqdata))
  real(4) yoq(max(1,mqdata))
  real(4) xbarbq(max(1,mqdata))
  logical qfw(max(1,mqdata))

  real(4) xbarpw(max(1,mpwdata))
  real(4) pbarpw(max(1,mpwdata))
  real(4) yopw(max(1,mpwdata))
  real(4) xbarbpw(max(1,mpwdata))
  logical pwfw(max(1,mpwdata))

  real(4) xw(nxc*nyc*lmetaex*2)
  real(4) xbarw(max(1,mwdata))
  real(4) pw(nxc*nyc*lmetaex*2)
  real(4) pbarw(max(1,mwdata))
  real(4) yow(max(1,mwdata))
  real(4) xbarbw(max(1,mwdata))
  logical wfw(max(1,mwdata))

  real(4) xbarwr(lmetaex,max(1,mwrdata))
  real(4) pbarwr(lmetaex,max(1,mwrdata))
  real(4) yowr(max(1,mwrdata))
  real(4) xbarbwr(lmetaex,max(1,mwrdata))
  integer(4) ibighwr(lbig2ges+2,max(1,mwrdata))

      real(4) xp(nxc*nyc)
      real(4) xbarp(max(1,mpdata))
      real(4) pp(nxc*nyc)
      real(4) pbarp(max(1,mpdata))
      real(4) yop(max(1,mpdata))
      real(4) xbarbp(max(1,mpdata))
      logical psfw(max(1,mpdata))

  real(4) xt(nxc*nyc*lmetaex)
  real(4) xbart(max(1,mtdata))
  real(4) pt(nxc*nyc*lmetaex)
  real(4) pbart(max(1,mtdata))
  real(4) yot(max(1,mtdata))
  real(4) xbarbt(max(1,mtdata))
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

      PARAMETER (ITMAX=100,CGOLD=.3819660,ZEPS=1.0e-10)


!
!  Given a function func, and given a bracketing triplet of abscissas ax,bx,cx 
!  (such that bx is between ax and cx, and f(bx) is less than both f(ax) and
!  f(cx)), this routine isolates the minimum to a fractional precision of
!  about tol using Brent's method.  The abscissa of the minimum is returned
!  as xmin, and the minimum function value as brent, the returned function
!  value.
!  Parameters: Maximum allowed number of iterations; golden ratio; and a 
!  small number that protects against trying to achieve fractional accuracy
!  for a minimum that happens to be exactly zero.
!
      INTEGER   iter
      real(4)   a,b,d,e,etemp,fu,fv,fw,fx,p,q,r,tol1,tol2,u,v,w,x,xm

      call mpi_comm_rank(my_comm,mype,ierr)

      
      a = min(ax,cx)
      b = max(ax,cx)
      v = bx
      w = v
      x = v
      e = 0.
      fx = func(x, &
                 xq,xbarq,pq,pbarq,yoq,xbarbq,mqdata,qfw, &
                          xbarpw,pbarpw,yopw,xbarbpw,mpwdata,pwfw, &
                 xw,xbarw,pw,pbarw,yow,xbarbw,mwdata,wfw, &
                          xbarwr,pbarwr,yowr,xbarbwr,mwrdata,ibighwr,lbig2ges, &
              xp,xbarp,pp,pbarp,yop,xbarbp,mpdata,psfw, &
              xt,xbart,pt,pbart,yot,xbarbt,mtdata,tfw, &
                 xpred,xbarrad,ppred,pbarrad,yorad,xbarbrad,mraddata,jpch,npred,userad,radfw, &
                 nxc,nyc,lmetaex,myxsc,myxec,myysc,myyec,doqc)
!     if(mype.eq.0) print *,' brent--x,fx: ',x,fx
      fv = fx
      fw = fx
      do iter=1,ITMAX
        xm = 0.5*(a+b)
        tol1 = tol*abs(x)+ZEPS
        tol2 = 2.*tol1
!       if(mype.eq.0) print *,' brent--',iter,abs(x-xm),tol2-.5*(b-a)
        if(abs(x-xm).le.(tol2-.5*(b-a))) goto 3
        if(abs(e).gt.tol1) then
          r = (x-w)*(fx-fv)
          q = (x-v)*(fx-fw)
          p = (x-v)*q-(x-w)*r
          q = 2.*(q-r)
!         if(mype.eq.0) print *,' brent--iter,p,q,r: ',iter,p,q,r
          if(q.gt.0.) p = -p
          q = abs(q)
          etemp = e
          e = d
          if(abs(p).ge.abs(.5*q*etemp) .or. p.le.q*(a-x) .or.           &
     &      p.ge.q*(b-x)) goto 1
          d = p/q
          u = x+d
          if(u-a.lt.tol2 .or. b-u.lt.tol2) d = sign(tol1,xm-x)
          goto 2
        endif
    1   if(x.ge.xm) then
          e = a-x
        else
          e = b-x
        endif
        d = CGOLD*e
    2   if(abs(d).ge.tol1) then
          u = x+d
        else
          u = x+sign(tol1,d)
        endif
        fu = func(u, &
                 xq,xbarq,pq,pbarq,yoq,xbarbq,mqdata,qfw, &
                          xbarpw,pbarpw,yopw,xbarbpw,mpwdata,pwfw, &
                 xw,xbarw,pw,pbarw,yow,xbarbw,mwdata,wfw, &
                          xbarwr,pbarwr,yowr,xbarbwr,mwrdata,ibighwr,lbig2ges, &
              xp,xbarp,pp,pbarp,yop,xbarbp,mpdata,psfw, &
              xt,xbart,pt,pbart,yot,xbarbt,mtdata,tfw, &
                 xpred,xbarrad,ppred,pbarrad,yorad,xbarbrad,mraddata,jpch,npred,userad,radfw, &
                 nxc,nyc,lmetaex,myxsc,myxec,myysc,myyec,doqc)
!       if(mype.eq.0) print *,' brent--u,fu: ',u,fu
        if(fu.le.fx) then
          if(u.ge.x) then
            a = x
          else
            b = x
          endif
          v = w
          fv = fw
          w = x
          fw = fx
          x = u
          fx = fu
        else
          if(u.lt.x) then
            a = u
          else
            b = u
          endif
          if(fu.le.fw .or. w.eq.x) then
            v = w
            fv = fw
            w = u
            fw = fu
          elseif(fu.le.fv .or. v.eq.x .or. v.eq.w) then
            v = u
            fv = fu
          endif
        endif
      enddo
!     print *,'brent exceeded maximum iterations'
    3 xmin = x
      brent = fx
      return
      END
